      SUBROUTINE DPUCDF(X,M,N,ALPHA,BETA,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DOUBLY-PARETO UNIFORM
C              DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING CDF FUNCTION:
C
C              F(X;ALPHA,BETA,M,N) = K(M,N)*
C
C                  [N/(M+M*N+N)]*((BETA-ALPHA/(BETA-X))**M
C                  X < ALPHA 
C
C                  [(M*N*(X-ALPHA) + N*(BETA-ALPHA)]/
C                  [(M+M*N+N)*(BETA-ALPHA)]
C                  ALPHA  <= X <= BETA
C
C                  1 - [M/(M+M*N+N)]*((BETA-ALPHA/(X-ALPHA))**N
C                  X > BETA
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE CUMULATIVE DISTRIBUTION
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE DOUBLE PRECISION SHAPE PARAMETER
C                       BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
C                       M      = THE DOUBLE PRECISION SHAPE PARAMETER
C                       N      = THE DOUBLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--CDF    = THE DOUBLE PRECISION CUMULATIVE
C                                DISTRIBUTION FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION CUMULATIVE DISTRIBUTION
C             FUNCTION VALUE CDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--ALPHA < BETA, M, N > 0.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--SINGH, VAN DORP, MAZZUCHI "A NOVEL ASYMMETRIC
C                 DISTRIBUTION WITH POWER TAILS", 
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C               --VAN DORP, SIGN, AND MAZZUCHI "THE DOUBLY-PARETO
C                 UNIFORM DISTRIBUTION WITH APPLICATIONS IN
C                 UNCERTAINTY ANALYSIS AND ECONOMETRICS",
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2899
C     ORIGINAL VERSION--OCTOBER   2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION M
      DOUBLE PRECISION N
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      CDF=0.0D0
      IF(ALPHA.GT.BETA)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,45)ALPHA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(M.LE.0.0D0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)M
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.LE.0.0D0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** ERROR--THE FIFTH ARGUMENT TO DPUCDF IS LESS THAN OR ',
     1'EQUAL TO THE FOURTH ARGUMENT.')
   22 FORMAT(
     1'***** ERROR--THE SECOND ARGUMENT TO DPUCDF IS NON-POSITIVE.')
   32 FORMAT(
     1'***** ERROR--THE THIRD ARGUMENT TO DPUCDF IS NON-POSITIVE.')
   45 FORMAT('      THE VALUE OF ALPHA         = ',G15.7)
   46 FORMAT('      THE VALUE OF BETA          = ',G15.7)
   47 FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      IF(X.LT.ALPHA)THEN
        TERM1=N/(M+M*N+N)
        TERM2=((BETA-ALPHA)/(BETA-X))**M
        CDF=TERM1*TERM2
      ELSEIF(X.GT.BETA)THEN
        TERM1=M/(M+M*N+N)
        TERM2=((BETA-ALPHA)/(X-ALPHA))**N
        CDF=1.0D0 - TERM1*TERM2
      ELSE
        TERM1=M*N*(X-ALPHA) + N*(BETA-ALPHA)
        TERM2=(M+M*N+N)*(BETA-ALPHA)
        CDF=TERM1/TERM2
      ENDIF
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE DPUPDF(X,M,N,ALPHA,BETA,PDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PROBABILITY DENSITY
C              FUNCTION VALUE FOR THE DOUBLY-PARETO UNIFORM
C              DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING PDF FUNCTION:
C
C              f(X;ALPHA,BETA,M,N) = K(M,N)*
C                  (BETA-ALPHA)**M/(BETA-X)**(M+1))  X < ALPHA
C                  1/(BETA-ALPHA)                    ALPHA <= X <= BETA
C                  (BETA-ALPHA)**N/(X-ALPHA)**(N+1)) X > BETA
C                  ALPHA < BETA; M, N > 0
C
C              WHERE
C                  K(M,N) = M*N/(M+M*N+N)
C
C     INPUT  ARGUMENTS--X      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE DOUBLE PRECISION SHAPE PARAMETER
C                       BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
C                       M      = THE DOUBLE PRECISION SHAPE PARAMETER
C                       N      = THE DOUBLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE DOUBLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PROBABILITY DENSITY
C             FUNCTION VALUE PDF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--ALPHA < BETA, M, N > 0.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--SINGH, VAN DORP, MAZZUCHI "A NOVEL ASYMMETRIC
C                 DISTRIBUTION WITH POWER TAILS", 
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C               --VAN DORP, SIGN, AND MAZZUCHI "THE DOUBLY-PARETO
C                 UNIFORM DISTRIBUTION WITH APPLICATIONS IN
C                 UNCERTAINTY ANALYSIS AND ECONOMETRICS",
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2899
C     ORIGINAL VERSION--OCTOBER   2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION M
      DOUBLE PRECISION N
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PDF=0.0D0
      IF(ALPHA.GT.BETA)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,45)ALPHA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(M.LE.0.0D0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)M
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.LE.0.0D0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   12 FORMAT(
     1'***** ERROR--THE FIFTH ARGUMENT TO DPUPDF IS LESS THAN OR ',
     1'EQUAL TO THE FOURTH ARGUMENT.')
   22 FORMAT(
     1'***** ERROR--THE SECOND ARGUMENT TO DPUPDF IS NON-POSITIVE.')
   32 FORMAT(
     1'***** ERROR--THE THIRD ARGUMENT TO DPUPDF IS NON-POSITIVE.')
   45 FORMAT('      THE VALUE OF ALPHA         = ',G15.7)
   46 FORMAT('      THE VALUE OF BETA          = ',G15.7)
   47 FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      TERM1=DLOG(M) + DLOG(N) - DLOG(M+M*N+N)
      IF(X.LT.ALPHA)THEN
        TERM2=M*DLOG(BETA-ALPHA) - (M+1.0D0)*DLOG(BETA-X)
        PDF=DEXP(TERM1 + TERM2)
      ELSEIF(X.GT.BETA)THEN
        TERM2=N*DLOG(BETA-ALPHA) - (N+1.0D0)*DLOG(X-ALPHA)
        PDF=DEXP(TERM1 + TERM2)
      ELSE
        TERM2=DLOG(BETA-ALPHA)
        PDF=DEXP(TERM1 - TERM2)
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPUPPA(INAME1,INAME2,AVAL,NPAR,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--MANY DATAPLOT COMMANDS AUTOMATICALLY CREATE PARAMETERS
C              THAT CAN BE USED IN SUBSEQUENT ANALYSIS.  THIS SUBROUTINE
C              IS A UTILITY ROUTINE FOR CREATING THOSE PARAMETERS.  THE
C              INPUT IS AN ARRAY OF PARAMETER NAMES WITH THEIR
C              ASSOCIATED VALUES.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2014/5
C     ORIGINAL VERSION--MAY       2014.
C
C---------------------------------------------------------------------
C
      DIMENSION AVAL(*)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      CHARACTER*4 INAME1(*)
      CHARACTER*4 INAME2(*)
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UPPA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPUPPA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NPAR
   53   FORMAT('NPAR = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NPAR
          WRITE(ICOUT,63)I,INAME1(I),INAME2(I),AVAL(I)
   63     FORMAT('I,INAME1(I),INAME2(I),AVAL(I) = ',I8,2A4,G15.7)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
      ENDIF
C
      IERROR='NO'
      ISUBN0='DPUP'
C
C               ***************************************
C               **  STEP 1--                         **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UPPA')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NPAR.LT.1)GOTO9000
C
      DO100I=1,NPAR
        IH=INAME1(I)
        IH2=INAME2(I)
        VALUE0=AVAL(I)
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
  100 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UPPA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPUPPA--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPUPPF(P,M,N,ALPHA,BETA,PPF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE PERCENT POINT
C              FUNCTION VALUE FOR THE DOUBLY-PARETO UNIFORM
C              DISTRIBUTION.
C              THIS DISTRIBUTION HAS THE FOLLOWING PPF FUNCTION:
C
C              G(P;ALPHA,BETA,M,N) = 
C
C                  LAMBDA1*(BETA-ALPHA) + ALPHA   0 < P < PI1
C                  LAMBDA2*(BETA-ALPHA) + ALPHA   PI1 <= P <= PI2
C                  LAMBDA3*(BETA-ALPHA) + ALPHA   PI2 < P < 1
C
C                  WHERE
C
C                  PI1 = N/(M + M*N + N)
C                  PI2 = (M*N)/(M + M*N + N)
C                  PI3 = M/(M + M*N + N)
C
C                  LAMBDA1 = 1 - (PI1/P)**(1/M)
C                  LAMBDA2 = (P - PI1)/PI2
C                  LAMBDA3 = (PI3/(1-P))**(1/N)
C
C     INPUT  ARGUMENTS--P      = THE DOUBLE PRECISION VALUE AT
C                                WHICH THE PERCENT POINT
C                                FUNCTION IS TO BE EVALUATED.
C                     --ALPHA  = THE DOUBLE PRECISION SHAPE PARAMETER
C                       BETA   = THE DOUBLE PRECISION SHAPE PARAMETER
C                       M      = THE DOUBLE PRECISION SHAPE PARAMETER
C                       N      = THE DOUBLE PRECISION SHAPE PARAMETER
C     OUTPUT ARGUMENTS--PPF    = THE DOUBLE PRECISION PERCENT POINT
C                                FUNCTION VALUE.
C     OUTPUT--THE DOUBLE PRECISION PERCENT POINT
C             FUNCTION VALUE PPF.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS. 
C     RESTRICTIONS--ALPHA < BETA, M, N > 0.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--DOUBLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--SINGH, VAN DORP, MAZZUCHI "A NOVEL ASYMMETRIC
C                 DISTRIBUTION WITH POWER TAILS", 
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C               --VAN DORP, SIGN, AND MAZZUCHI "THE DOUBLY-PARETO
C                 UNIFORM DISTRIBUTION WITH APPLICATIONS IN
C                 UNCERTAINTY ANALYSIS AND ECONOMETRICS",
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE:  301-975-2899
C     ORIGINAL VERSION--OCTOBER   2007. 
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION M
      DOUBLE PRECISION N
C
      INCLUDE 'DPCOP2.INC'
C
C---------------------------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0D0
C
      IF(P.LE.0.0D0 .OR. P.GE.1.0D0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)P
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(ALPHA.GT.BETA)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,45)ALPHA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(M.LE.0.0D0)THEN
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)M
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(N.LE.0.0D0)THEN
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    2 FORMAT(
     1'***** ERROR--THE FIRST ARGUMENT TO DPUPPF IS OUTSIDE THE ',
     1'(0,1) INTERVAL.')
   12 FORMAT(
     1'***** ERROR--THE FIFTH ARGUMENT TO DPUPPF IS LESS THAN OR ',
     1'EQUAL TO THE FOURTH ARGUMENT.')
   22 FORMAT(
     1'***** ERROR--THE SECOND ARGUMENT TO DPUPPF IS NON-POSITIVE.')
   32 FORMAT(
     1'***** ERROR--THE THIRD ARGUMENT TO DPUPPF IS NON-POSITIVE.')
   45 FORMAT('      THE VALUE OF ALPHA         = ',G15.7)
   46 FORMAT('      THE VALUE OF BETA          = ',G15.7)
   47 FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7)
C
C-----START POINT-----------------------------------------------------
C
      PI1=N/(M + M*N + N)
      PI2=(M*N)/(M + M*N + N)
      PI3=M/(M + M*N + N)
      PCUT1=PI1
      PCUT2=1.0D0 - PI3
C
      IF(P.LT.PCUT1)THEN
        ALAMB=1.0D0 - (PI1/P)**(1.0D0/M)
      ELSEIF(P.GT.PCUT2)THEN
        ALAMB=(PI3/(1.0D0 - P))**(1.0D0/N)
      ELSE
        ALAMB=(P - PI1)/PI2
      ENDIF
      PPF=ALAMB*(BETA-ALPHA) + ALPHA
C
 9000 CONTINUE
      RETURN
      END 
      SUBROUTINE DPURAN(N,AM,AN,ALPHA,BETA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE DOUBLY-PARETO UNIFORM DISTRIBUTION
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C                     --AN     = THE SINGLE PRECISION SHAPE PARAMETER
C                       AM     = THE SINGLE PRECISION SHAPE PARAMETER
C                       ALPHA  = THE SINGLE PRECISION SHAPE PARAMETER
C                       BETA   = THE SINGLE PRECISION SHAPE PARAMETER
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE DOUBLY-PARETO UNIFORM DISTRIBUTION
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN, DPUPPF
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--SINGH, VAN DORP, MAZZUCHI "A NOVEL ASYMMETRIC
C                 DISTRIBUTION WITH POWER TAILS", 
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C               --VAN DORP, SIGN, AND MAZZUCHI "THE DOUBLY-PARETO
C                 UNIFORM DISTRIBUTION WITH APPLICATIONS IN
C                 UNCERTAINTY ANALYSIS AND ECONOMETRICS",
C                 DOWNLOADED FROM VAN DORP WEB SITE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2007.10
C     ORIGINAL VERSION--OCTOBER   2007.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DPPF
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
      ELSEIF(ALPHA.GT.BETA)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,45)ALPHA
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(AM.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,22)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)M
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(AN.LE.0.0)THEN
        WRITE(ICOUT,12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF DOUBLY-PARETO',
     1' UNIFORM RADOM NUMBERS IS NON-POSITIVE')
   12 FORMAT(
     1'***** ERROR--DOUBLY-UNIFORM RANDOM NUMBERS')
   13 FORMAT(
     1'             ALPHA IS GREATER THAN OR EQUAL TO BETA.')
   22 FORMAT(
     1'             THE M PARAMETER IS NON-POSITIVE.')
   32 FORMAT(
     1'             THE N PARAMETER IS NON-POSITIVE.')
   45 FORMAT('      THE VALUE OF ALPHA         = ',G15.7)
   46 FORMAT('      THE VALUE OF BETA          = ',G15.7)
   47 FORMAT('      THE VALUE OF THE ARGUMENT  = ',G15.7)
C
C
C     GENERATE N UNIFORM (0,1) RANDOM NUMBERS;
C
      CALL UNIRAN(N,ISEED,X)
C
C     GENERATE N DOUBLY-PARETO UNIFORM RANDOM NUMBERS
C     USING THE PERCENT POINT FUNCTION TRANSFORMATION METHOD.
C
      DO100I=1,N
      P=X(I)
      CALL DPUPPF(DBLE(P),DBLE(AM),DBLE(AN),DBLE(ALPHA),
     1            DBLE(BETA),DPPF)
      X(I)=DPPF
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPUNST(YTEMP,XTEMP,NTEMP,IHLEFT,IHLEF2,XDIST,NDIST,
     1                  TEMP1,ISUBT,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--GIVEN A RESPONSE VARIABLE, Y, AND A GROUP-ID
C              VARIABLE, X, THIS ROUTINE STORES THE RESPONSE
C              VALUES FOR EACH UNIQUE VALUE OF THE GROUP-ID
C              VARIABLE.  THIS IS ESSENTIALLY THE REVERSE
C              OPERATION OF THE STACK COMMAND.
C     INPUT  ARGUMENTS--YTEMP  = A SINGLE PRECISION VECTOR CONTAINING
C                                THE RESPONSE VALUES.
C                       XTEMP  = A SINGLE PRECISION VECTOR CONTAINING
C                                THE GROUP-ID VARIABLE.
C                     --NTEMP  = THE INTEGER NUMBER OF OBSERVATIONS
C                                FOR Y AND X.
C                     --IHLEFT = CHARACTER STRINGS CONTAINING THE
C                       IHLEF2   BASE NAME FOR THE NEWLY CREATED
C                                VARIABLES.
C     OUTPUT ARGUMENTS--XDIST  = A SINGLE PRECISION VECTOR CONTAINING
C                                THE DISTINCT VALUES OF THE GROUP-ID
C                                VARIABLE.
C     OUTPUT--XDIST
C     OTHER DATAPAC   SUBROUTINES NEEDED--DPDIST.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2016.08
C     ORIGINAL VERSION--AUGUST    2016.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C
      INTEGER NTEMP
C
      REAL YTEMP(*)
      REAL XTEMP(*)
      REAL XDIST(*)
      REAL TEMP1(*)
C
      INTEGER ISUBT(*)
C
      CHARACTER*4 IHLEFT
      CHARACTER*4 IHLEF2
      CHARACTER*8 IBASNM
      CHARACTER*8 INAMET
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UNST')THEN
        WRITE(ICOUT,998)
  998   FORMAT('test')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPUNST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,NTEMP
   52   FORMAT('IBUGA3,ISUBRO,NTEMP = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        IF(NTEMP.GT.0)THEN
          DO55I=1,NTEMP
            WRITE(ICOUT,56)I,XTEMP(I),YTEMP(I)
   56       FORMAT('I,XTEMP(I),YTEMP(I) = ',I8,2X,2G15.7)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
      ENDIF
C
      NLAST=0
C
      IBASNM(1:4)=IHLEFT(1:4)
      IBASNM(5:8)=IHLEF2(1:4)
      DO90I=8,1,-1
        IF(IBASNM(I:I).NE.' ')THEN
          NLAST=I
          GOTO99
        ENDIF
   90 CONTINUE
   99 CONTINUE
      IF(NLAST.EQ.0)THEN
        IBASNM='Z'
        NLAST=1
      ENDIF
C
C     STEP 1: DETERMINE THE UNIQUE VALUES OF THE GROUP-ID
C             VARIABLE AND SORT THEM.
C
      IWRITE='OFF'
      CALL DISTIN(XTEMP,NTEMP,IWRITE,XDIST,NDIST,IBUGA3,IERROR)
      IF(NDIST.GT.99)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN UNSTACK COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)NDIST
  103   FORMAT('      THE NUMBER OF DISTINCT VALUES IN THE ',
     1         'GROUP-ID VARIABLE (',I8,') IS GREATER THAN 99.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
      IF(IERROR.EQ.'YES')GOTO9000
      CALL SORT(XDIST,NDIST,XDIST)
C
      IF(NDIST.LE.9)THEN
        IF(NLAST.EQ.8)NLAST=7
      ELSE
        IF(NLAST.GE.7)NLAST=6
      ENDIF
      IBASNM(NLAST+1:8)=' '
C
C     STEP 2: NOW LOOP THROUGH THE DISTINCT VALUES OF THE GROUP-ID
C             VARIABLE
C
      DO210I=1,NDIST
        HOLD=XDIST(I)
        K=0
        INAMET=' '
        INAMET(1:NLAST)=IBASNM(1:NLAST)
        IF(I.LE.9)THEN
          WRITE(INAMET(NLAST+1:NLAST+1),'(I1)')I
        ELSE
          WRITE(INAMET(NLAST+1:NLAST+2),'(I2)')I
        ENDIF
        DO220J=1,NTEMP
          IF(XTEMP(J).EQ.HOLD)THEN
            K=K+1
            TEMP1(K)=YTEMP(J)
          ENDIF
  220   CONTINUE
C
C       UPDATE THE NAME TABLE WITH THE NEW VARIABLE
C
        DO230J=1,K
          ISUBT(J)=1
  230   CONTINUE
C
        CALL DPADDV(INAMET(1:4),INAMET(5:8),TEMP1,K,ISUBT,
     1              IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1              IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,
     1              MAXCOL,NUMCOL,
     1              PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              IBUGA3,ISUBRO,IERROR)
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,231)INAMET,K
  231     FORMAT('VARIABLE ',A8,' CREATED WITH ',I8,' ELEMENTS.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  210 CONTINUE
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UNST')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DPUNST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9053)NDIST
 9053   FORMAT('NOUT = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NDIST.GT.0)THEN
          DO9055I=1,NDIST
            WRITE(ICOUT,9056)I,XDIST(I)
 9056       FORMAT('I,XDIST(I) = ',I8,2X,G15.7)
            CALL DPWRST('XXX','BUG ')
 9055     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPUOSM(IBUGA3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE UNIFORM ORDER STATISTIC MEDIANS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
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
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEQ
      CHARACTER*4 ILEFT
      CHARACTER*4 ILEFT2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      ISUBN1='DPUO'
      ISUBN2='SM  '
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
C
C               ***********************************************
C               **  TREAT THE UNIFORM ORDER STATISTIC MEDIANS CASE  **
C               **       1) FOR A FULL VARIABLE, OR          **
C               **       2) FOR PART OF A VARIABLE.          **
C               ***********************************************
C
      IF(IBUGA3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPUOSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGA3,IBUGQ
   52 FORMAT('IBUGA3,IBUGQ = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=3
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ****************************************************************
C               **  STEP 3--                                                   *
C               **  EXAMINE THE LEFT-HAND SIDE--                               *
C               **  IS THE PARAMETER OR VARIABLE NAME TO LEFT OF = SIGN        *
C               **  ALREADY IN THE NAME LIST?                                  *
C               **  NOTE THAT     ILEFT      IS THE NAME OF THE VARIABLE       *
C               **  ON THE LEFT.                                               *
C               **  NOTE THAT     ILISTL    IS THE LINE IN THE TABLE           *
C               **  OF THE NAME ON THE LEFT.                                   *
C               **  NOTE THAT     ICOLL    IS THE DATA COLUMN (1 TO 12)        *
C               **  FOR THE NAME OF THE LEFT.                                  *
C               ****************************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC ILEFT=IHOL(2)
CCCCC ILEFT2=IHOL2(2)
      ILEFT=IHARG(1)
      ILEFT2=IHARG2(1)
      DO310I=1,NUMNAM
      I2=I
      IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'P')GOTO329
      IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.
     1IUSE(I).EQ.'V')GOTO380
  310 CONTINUE
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)GOTO320
      GOTO330
C
  320 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,321)
  321 FORMAT('***** ERROR IN DPUOSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,322)
  322 FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,323)MAXNAM
  323 FORMAT('      NAMES HAS JUST EXCEEDED THE MAX ALLOWABLE ',
     1I8,'  .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,324)
  324 FORMAT('      SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,325)
  325 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,326)
  326 FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,327)
  327 FORMAT('      AND THEN REDEFINE (REUSE) SOME OF THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,328)
  328 FORMAT('      ALREADY-USED NAMES')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  329 CONTINUE
      ILISTL=I2
      GOTO330
C
  330 CONTINUE
      NLEFT=0
      ICOLL=NUMCOL+1
      IF(ICOLL.GT.MAXCOL)GOTO340
      GOTO390
C
  340 CONTINUE
      WRITE(ICOUT,341)
  341 FORMAT('***** ERROR IN DPUOSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,342)
  342 FORMAT('      THE NUMBER OF DATA COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,343)MAXCOL
  343 FORMAT('      HAS JUST EXCEEDED THE MAX ALLOWABLE ',I8,'  .')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,344)
  344 FORMAT('      SUGGESTED ACTION--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,345)
  345 FORMAT('      ENTER      STAT')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,346)
  346 FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,347)
  347 FORMAT('      AND THEN OVERWRITE SOME COLUMNS.   EXAMPLE--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,348)
  348 FORMAT('      IF       LET X(I) = 3.14         FAILED')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,349)
  349 FORMAT('      THEN ONE MIGHT ENTER     NAME X 7')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,350)
  350 FORMAT('      (THEREBY EQUATING THE NAME X WITH COLUMN 7')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,351)
  351 FORMAT('      FOLLOWED BY              LET X = 3.14')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,352)
  352 FORMAT('      (WHICH WILL ACTUALLY OVERWRITE COLUMN 7')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,353)
  353 FORMAT('      WITH THE NUMERIC CONSTANTS 3.14)')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  380 CONTINUE
      ILISTL=I2
      ICOLL=IVALUE(ILISTL)
      NLEFT=IN(ILISTL)
C
  390 CONTINUE
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
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO670
      DO610J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')GOTO620
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
  671 FORMAT('***** INTERNAL ERROR IN DPUOSM')
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
      IF(IWIDTH.GE.1)WRITE(ICOUT,677)(IANS(I),I=1,IWIDTH)
  677 FORMAT(80A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
  680 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO690
      WRITE(ICOUT,681)NUMARG,ILOCQ,ICASEQ
  681 FORMAT('NUMARG,ILOCQ,ICASEQ = ',I8,I8,2X,A4)
      CALL DPWRST('XXX','BUG ')
C
  690 CONTINUE
C
C               ******************************************************
C               **  STEP 7--                                        **
C               **  BRANCH TO THE APPROPRIATE SUBCASE               **
C               **  (BASED ON THE QUALIFIER);                       **
C               **  DETERMINE THE NUMBER (= NUOSM)                   **
C               **  OF UNIFORM ORDER STATISTIC MEDIANS TO BE GENERATED.
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
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO710
      IF(ICASEQ.EQ.'SUBS')GOTO720
      IF(ICASEQ.EQ.'FOR')GOTO730
C
  710 CONTINUE
      IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
      IF(NEWNAM.EQ.'YES')NIISUB=MAXN
      DO715I=1,NIISUB
      ISUB(I)=1
  715 CONTINUE
      NUOSM=NIISUB
      GOTO750
C
  720 CONTINUE
      NIISUB=MAXN
      CALL DPSUBS(NIISUB,ILOCS,NS,IBUGQ,IERROR)
      NUOSM=NS
      GOTO750
C
  730 CONTINUE
      IF(NEWNAM.EQ.'NO')NIISUB=NLEFT
      IF(NEWNAM.EQ.'YES')NIISUB=MAXN
      CALL DPFOR(NIISUB,NINEW,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NIISUB=NINEW
      NUOSM=NS
      GOTO750
C
  750 CONTINUE
C
C               ******************************************
C               **  STEP 8--                            **
C               **  GENERATE    NUOSM    UNIFORM ORDER   **
C               **  STATISTIC MEDIANS.                  **
C               **  STORE THEM TEMPORARILY IN           **
C               **  THE VECTOR Y(.).                    **
C               ******************************************
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL UNIMED(NUOSM,Y)
C
C               ***********************************************************
C               **  STEP 8--                                             **
C               **  IF CALLED FOR (THAT IS, IF IBUGA3 IS ON),      **
C               **  PRINT OUT THE INTERMEDIATE VARIABLE Y(.).            **
C               **  THIS IS USEFUL FOR DIAGNOSTIC PURPOSES               **
C               **  IN REVIEWING THE OUTPUT FROM THIS SUBROUTINE.        **
C               ***********************************************************
C
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA3.EQ.'OFF')GOTO2090
      WRITE(ICOUT,2051)
 2051 FORMAT('OUTPUT FROM MIDDLE OF DPUOSM AFTER UNIMED ',
     1'HAS BEEN CALLED--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2052)NUOSM
 2052 FORMAT('NUOSM = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUOSM.LE.0)GOTO2090
      DO2054I=1,NUOSM
      WRITE(ICOUT,2055)I,Y(I)
 2055 FORMAT('I,Y(I) = ',I8,F12.5)
      CALL DPWRST('XXX','BUG ')
 2054 CONTINUE
C
 2090 CONTINUE
C
C               ******************************************************
C               **  STEP 9--                                        **
C               **  COPY THE ORDER STATISTIC MEDIANS                **
C               **  FROM THE INTERMEDIATE VECTOR Y(.)               **
C               **  TO THE APPROPRIATE COLUMN                       **
C               **  (BASED ON THE QUALIFIER--FULL, SUBSET, OR FOR)  **
C               **  IN THE INTERNAL DATAPLOT DATA TABLE.            **
C               ******************************************************
C
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NS2=0
      DO2100I=1,NIISUB
      IJ=MAXN*(ICOLL-1)+I
      IF(ISUB(I).EQ.0)GOTO2100
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
 2100 CONTINUE
C
C               *******************************************
C               **  STEP 10--                            **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING.   **
C               *******************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
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
CCCCC IUSE(ICOLL)='V'
CCCCC IVALUE(ICOLL)=ICOLL
CCCCC VALUE(ICOLL)=ICOLL
CCCCC IN(ICOLL)=NINEW
C
      IF(NEWNAM.EQ.'YES')NUMNAM=NUMNAM+1
      IF(NEWNAM.EQ.'YES')NUMCOL=NUMCOL+1
C
      DO4100J4=1,NUMNAM
      IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)GOTO4105
      GOTO4100
 4105 CONTINUE
      IUSE(J4)='V'
      IVALUE(J4)=ICOLL
      VALUE(J4)=ICOLL
      IN(J4)=NINEW
 4100 CONTINUE
C
      IF(IPRINT.EQ.'OFF')GOTO4059
      IF(IFEEDB.EQ.'OFF')GOTO4059
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4011)ILEFT,ILEFT2,NS2
 4011 FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
C
      IJ=MAXN*(ICOLL-1)+IROW1
      IF(ICOLL.LE.MAXCOL)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,V(IJ),IROW1
 4021    FORMAT('THE FIRST           COMPUTED VALUE OF ',
     1   A4,A4,' = ',E15.7,'   (ROW ',I6,')')
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP1)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,PRED(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP2)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,RES(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP3)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,YPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP4)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,XPLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP5)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,X2PLOT(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ELSE IF(ICOLL.EQ.MAXCP6)THEN
         WRITE(ICOUT,4021)ILEFT,ILEFT2,TAGPLO(IROW1),IROW1
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IJ=MAXN*(ICOLL-1)+IROWN
      IF(NS2.NE.1)THEN
         IF(ICOLL.LE.MAXCOL)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,V(IJ),IROWN
 4031       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',
     1      A4,A4,' = ',E15.7,'   (ROW ',I6,')')
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP1)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,PRED(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP2)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,RES(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP3)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,YPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP4)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,XPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP5)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,X2PLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ELSE IF(ICOLL.EQ.MAXCP6)THEN
            WRITE(ICOUT,4031)NS2,ILEFT,ILEFT2,TAGPLO(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
         ENDIF
      ENDIF
      IF(NS2.NE.1)GOTO4090
      WRITE(ICOUT,4041)
 4041 FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4042)
 4042 FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
      CALL DPWRST('XXX','BUG ')
 4090 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4112)ILEFT,ILEFT2,ICOLL
 4112 FORMAT('THE CURRENT COLUMN FOR ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,4113)ILEFT,ILEFT2,NINEW
 4113 FORMAT('THE CURRENT LENGTH OF  ',
     1'THE VARIABLE ',A4,A4,' = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
 4059 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPUOSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IBUGA3,IBUGQ
 9013 FORMAT('IBUGA3,IBUGQ = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)NS2
 9015 FORMAT('NS2 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)NS,NIISUB,NUOSM
 9016 FORMAT('NS,NIISUB,NUOSM = ',I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,IVALU2,VALUE,IN,
CCCCC UPDATE VARIABLE LABELS.  JANUARY 2000.
     1IVARLB,
CCCCC OCTOBER 1993.  ADD IVALU2 TO ARGUMENT LIST (DELETE CAUSED
CCCCC PROBLEMS WITH MATRICES THAT FOLLOWED ON VARIABLE LIST).
CCCCC SUBROUTINE DPUPDV(IHNAME,IHNAM2,IUSE,IVALUE,VALUE,IN,
     1IVSTAR,IVSTOP,MAXNAM,NUMNAM,V,MAXN,MAXCOL,NUMCOL,
     1IBUGS2,IERROR)
C
C     PURPOSE--HANDLE THE CASE IN WHICH DATA VARIABLES HAVE
C              BEEN DELETED AND SO THE ENTIRE DATA ARRAY
C              MUST BE SHIFTED TO AVOID HOLES IN THE ARRAY.
C              UPDATE HOUSEKEEPING TABLES ACCORDINGLY.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --OCTOBER   1993. ADD IVALU2 (FIX MATRICES BUG)
C     UPDATED         --JUNE      1994. BUG FOR PARAMETERS
C     UPDATED         --OCTOBER   1997. RE-INIATILIZE TO ZERO INSTEAD
C                                       OF CPUMIN
C     UPDATED         --JANUARY   2000. SUPPORT FOR VARIABLE LABELS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IBUGS2
      CHARACTER*4 IERROR
C
      CHARACTER*40 IVARLB
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE.
      DIMENSION IVALU2(*)
      DIMENSION VALUE(*)
      DIMENSION IN(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
C
      DIMENSION IVARLB(*)
C
      DIMENSION V(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPUP'
      ISUBN2='DV  '
      IERROR='NO'
C
      ILAST=0
      ICOLOL=0
C
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPUPDV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,IERROR,MAXNAM,NUMNAM
   52   FORMAT('IBUGS2,IERROR,MAXNAM,NUMNAM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)MAXN,MAXCOL,NUMCOL
   54   FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO60I=1,NUMNAM
          WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   61     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1           I8,2X,A4,A4,2X,A4,I8,G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,62)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
   62     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1           I8,2X,2A4,6X,3I8)
          CALL DPWRST('XXX','BUG ')
   60   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO70J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,71)J,MAXN,IJ,V(IJ)
   71     FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
   70   CONTINUE
      ENDIF
C
C               *********************************************
C               **  STEP 1--                               **
C               **  UPDATE THE HOUSEKEEPING TABLES.        **
C               **  ELIMINATE ANY ENTRIES IN THESE TABLES  **
C               **  WHICH HAVE LENGTH OF VARIABLE = 0;     **
C               **  THAT IS, WHICH HAVE IN(.) = 0.         **
C               *********************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMNAM.LE.1)GOTO1129
      J=0
 1101 CONTINUE
      J=J+1
      IF(J.GT.NUMNAM)GOTO1129
      IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GOTO1100
      IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GOTO1100
      IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO1100
      IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO1100
      IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GOTO1100
      IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GOTO1100
      IF(IUSE(J).EQ.'V'.AND.IN(J).LE.0)GOTO1109
CCCCC OCTOBER 1993.  ADD FOLLOWING 2 LINES
CCCCC JUNE 1994. FOR PARAMETER, SET TO -1 TO DELETE (SOME INTERNALLY
CCCCC SET PARAMETERS DO NOT SET IN(.), WHICH CAUSED BUGS WHEN RETAIN
CCCCC OR DELETE ENTERED).
CCCCC IF(IUSE(J).EQ.'P'.AND.IN(J).LE.0)GOTO1109
      IF(IUSE(J).EQ.'P'.AND.IN(J).LT.0)GOTO1109
      IF(IUSE(J).EQ.'M'.AND.IN(J).LE.0)GOTO1109
      IF(IUSE(J).EQ.'F'.AND.IN(J).LT.0)GOTO1109
      GOTO1100
 1109 CONTINUE
C
      JP1=J+1
      IF(JP1.GT.NUMNAM)GOTO1119
      DO1110K=JP1,NUMNAM
        KM1=K-1
        IHNAME(KM1)=IHNAME(K)
        IHNAM2(KM1)=IHNAM2(K)
        IUSE(KM1)=IUSE(K)
        IVALUE(KM1)=IVALUE(K)
CCCCC   OCTOBER 1993.  ADD FOLLOWING LINE.
        IVALU2(KM1)=IVALU2(K)
        VALUE(KM1)=VALUE(K)
        IN(KM1)=IN(K)
        IVSTAR(KM1)=IVSTAR(K)
        IVSTOP(KM1)=IVSTOP(K)
        IVARLB(KM1)=IVARLB(K)
 1110 CONTINUE
 1119 CONTINUE
      NUMNAM=NUMNAM-1
      J=J-1
C
 1100 CONTINUE
      GOTO1101
 1129 CONTINUE
C
C               ************************************************
C               **  STEP 2--                                  **
C               **  DETERMINE THE LARGEST COLUMN REFERENCED.  **
C               ************************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOLMX=0
      IF(NUMNAM.LE.0)GOTO2159
      DO2150J=1,NUMNAM
        IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GOTO2150
        IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GOTO2150
        IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO2150
        IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO2150
        IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GOTO2150
        IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GOTO2150
        IF(IUSE(J).EQ.'V'.AND.IVALUE(J).GT.ICOLMX)ICOLMX=IVALUE(J)
CCCCC   OCTOBER 1993.  ADD FOLLOWING LINE
        IF(IUSE(J).EQ.'M'.AND.IVALUE(J).GT.ICOLMX)ICOLMX=IVALUE(J)
 2150 CONTINUE
 2159 CONTINUE
C
C               *******************************************************
C               **  STEP 3--                                         **
C               **  TREAT THE CASE WHERE THERE IS AT LEAST           **
C               **  1 VARIABLE IN THE DATA ARRAY WHICH MAY           **
C               **  (AT LEAST POTENTIALLY) BE SHIFTED (COMPRESSED).  **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICODE=0
      NUMCO2=NUMCOL
      IF(ICOLMX.LE.0)GOTO3900
      DO3300ICOL=1,ICOLMX
C
        IPASS=0
        IF(NUMNAM.LE.0)GOTO3900
        DO3400J=1,NUMNAM
          IF(IHNAME(J).EQ.'PRED'.AND.IHNAM2(J).EQ.'    ')GOTO3400
          IF(IHNAME(J).EQ.'RES '.AND.IHNAM2(J).EQ.'    ')GOTO3400
          IF(IHNAME(J).EQ.'YPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO3400
          IF(IHNAME(J).EQ.'XPLO'.AND.IHNAM2(J).EQ.'T   ')GOTO3400
          IF(IHNAME(J).EQ.'X2PL'.AND.IHNAM2(J).EQ.'OT  ')GOTO3400
          IF(IHNAME(J).EQ.'TAGP'.AND.IHNAM2(J).EQ.'LOT ')GOTO3400
          IF(IUSE(J).EQ.'V'.AND.IVALUE(J).EQ.ICOL)GOTO3450
CCCCC     OCTOBER 1993.  ADD FOLLOWING LINE
          IF(IUSE(J).EQ.'M'.AND.IVALUE(J).EQ.ICOL)GOTO3450
          GOTO3400
C
 3450   CONTINUE
        IPASS=IPASS+1
        IF(IPASS.EQ.1)THEN
          ICODE=ICODE+1
          IF(IVALUE(J).EQ.ICODE)GOTO3490
          ICOLOL=IVALUE(J)
C
          IMAX=MAXN
          DO3461I=1,IMAX
            IJ=MAXN*(ICODE-1)+I
CCCCC       OCTOBER 1997.  FIX FOLLOWING LINE
CCCCC       V(IJ)=CPUMIN
            V(IJ)=0.0
 3461     CONTINUE
C
          IMAX=IN(J)
          DO3462I=1,IMAX
            IJ=MAXN*(ICODE-1)+I
            IJOL=MAXN*(ICOLOL-1)+I
            V(IJ)=V(IJOL)
 3462     CONTINUE
C
          IMAX=MAXN
          DO3463I=1,IMAX
            IJOL=MAXN*(ICOLOL-1)+I
CCCCC       OCTOBER 1997.  FIX FOLLOWING LINE
CCCCC       V(IJOL)=CPUMIN
            V(IJOL)=0.0
 3463     CONTINUE
        ENDIF
C
        IVALUE(J)=ICODE
CCCCC   OCTOBER 1993.  ADD FOLLOWING LINE
        IVALU2(J)=IVALU2(J)-(ICOLOL-ICODE)
        VALUE(J)=IVALUE(J)
        IVSTAR(J)=MAXN*(ICODE-1)+1
        IVSTOP(J)=MAXN*(ICODE-1)+IN(J)
C
 3490   CONTINUE
 3400   CONTINUE
 3300 CONTINUE
 3900 CONTINUE
      NUMCOL=ICODE
C
C               *****************************************
C               **  STEP 4--                           **
C               **  TREAT THE CASE WHERE NO VARIABLES  **
C               **  REMAIN IN THE DATA ARRAY.          **
C               *****************************************
C
      ISTEPN='4'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOLMX.LE.0 .AND. NUMCO2.GT.0)THEN
        DO4200J=1,NUMCO2
          DO4300I=1,MAXN
            IJ=MAXN*(J-1)+I
CCCCC       OCTOBER 1997.  FIX FOLLOWING LINE
CCCCC       V(IJ)=CPUMIN
            V(IJ)=0.0
 4300     CONTINUE
 4200   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPUPDV--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,MAXNAM,NUMNAM
 9013   FORMAT('IERROR,MAXNAM,NUMNAM = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)MAXN,MAXCOL,NUMCOL
 9014   FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO9020I=1,NUMNAM
          WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                     IVALUE(I),VALUE(I)
 9021     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1           I8,2X,A4,A4,2X,A4,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I),IN(I),
     1                     IVSTAR(I),IVSTOP(I)
 9022     FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1           I8,2X,2A4,6X,3I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9030J=1,NUMCOL
          IJ=MAXN*(J-1)+1
          WRITE(ICOUT,9031)J,MAXN,IJ,V(IJ)
 9031     FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPUPD2(NUMADD,NFIRST,IBUGS2,IERROR)
C
C     PURPOSE--ADD NUMADD BLANK COLUMNS BEFORE COLUMN IDENTIFIED 
C              BY NFIRST.  REQUIRED BY THE MATRIX AUGMENT COMMAND.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/10
C     ORIGINAL VERSION--OCTOBER   1993.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----CHARACTER STATEMENTS FOR COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPUP'
      ISUBN2='D2  '
      IERROR='NO'
C
      IF(IBUGS2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPUPD2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGS2,IERROR
   52 FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)MAXNAM,NUMNAM
   53 FORMAT('MAXNAM,NUMNAM = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)MAXN,MAXCOL,NUMCOL
   54 FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO60I=1,NUMNAM
      WRITE(ICOUT,61)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
   61 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1I8,2X,A4,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
   62 FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1I8,2X,A4,A4,6X,I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO70J=1,NUMCOL
      IJ=MAXN*(J-1)+1
      WRITE(ICOUT,71)J,MAXN,IJ,V(IJ)
   71 FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   70 CONTINUE
      WRITE(ICOUT,76)NFIRST,NUMADD
   76 FORMAT('NFIRST,NUMADD = ',I8,2X,I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               *********************************************
C               **  STEP 1--                               **
C               **  CHECK THAT MAXIMUM NUMBER OF COLUMNS   **
C               **  WON'T BE EXCEEDED.                     **
C               *********************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NFIRST+NUMADD.LE.MAXCOL)GOTO199
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,101)
  101 FORMAT('***** ERROR FROM DPUPD2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,102)
  102 FORMAT('      ADDING ADDITIONAL COLUMNS WILL EXCEED MAXIMUM')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,103)
  103 FORMAT('      NUMER OF ALLOWED COLUMNS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,104)NUMADD
  104 FORMAT('      NUMER OF COLUMNS TO ADD = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,105)MAXCOL
  105 FORMAT('      MAXIMUM NUMER OF COLUMNS = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,106)NUMCOL
  106 FORMAT('      CURRENT NUMER OF COLUMNS = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9090
  199 CONTINUE
C
      DO200I=1,NUMNAM
        IF(NFIRST.EQ.IVALUE(I))THEN
          IINDX=I
          GOTO209
        ENDIF
 200  CONTINUE
 209  CONTINUE
C
C
C               *********************************************
C               **  STEP 2--                               **
C               **  UPDATE THE HOUSEKEEPING TABLES.        **
C               **  ELIMINATE ANY ENTRIES IN THESE TABLES  **
C               **  WHICH HAVE LENGTH OF VARIABLE = 0;     **
C               **  THAT IS, WHICH HAVE IN(.) = 0.         **
C               *********************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMADD.LE.0)GOTO9090
      IF(NFIRST.GE.NUMCOL)GOTO9090
      IF(NFIRST.LT.1)GOTO9090
      IF(NFIRST.LT.1)GOTO9090
C
      DO1110K=NUMNAM,IINDX,-1
      KM1=K+NUMADD
      IHNAME(KM1)=IHNAME(K)
      IHNAM2(KM1)=IHNAM2(K)
      IUSE(KM1)=IUSE(K)
      IF(IUSE(K).EQ.'V'.OR.IUSE(K).EQ.'M')THEN
        IVALUE(KM1)=IVALUE(K)+NUMADD
        IVALU2(KM1)=IVALU2(K)+NUMADD
      ELSE
        IVALUE(KM1)=IVALUE(K)
        IVALU2(KM1)=IVALU2(K)
      ENDIF
      VALUE(KM1)=VALUE(K)
      IN(KM1)=IN(K)
      IVSTAR(KM1)=IVSTAR(K)
      IVSTOP(KM1)=IVSTOP(K)
 1110 CONTINUE
C
      NTEMP2=IINDX+NUMADD-1
      NTEMP1=IINDX-1
      IF(NTEMP1.LT.1)NTEMP1=1
      DO1120K=NTEMP1,NTEMP2
      IHNAME(K)='    '
      IHNAM2(K)='    '
      IUSE(K)='UNKN'
      IVALUE(K)=0
      IVALU2(K)=0
      VALUE(K)=0.0
      IN(K)=0
      IVSTAR(K)=0
      IVSTOP(K)=0
 1120 CONTINUE
C
      IMAX=MAXN
      IVINC=NUMADD*IMAX
      NTEMP1=(NFIRST-1)*IMAX+1
      IF(NTEMP1.LT.1)NTEMP1=1
      NTEMP2=NUMCOL*IMAX
      DO1130K=NTEMP2,NTEMP1,-1
      V(K+IVINC)=V(K)
 1130 CONTINUE
      NTEMP2=NTEMP1-1+NUMADD*IMAX
      DO1140K=NTEMP1,NTEMP2
      V(K)=0.0
 1140 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPUPD2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IERROR
 9012 FORMAT('IBUGS2,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IINDX,MAXNAM,NUMNAM
 9013 FORMAT('IINDX,MAXNAM,NUMNAM = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)MAXN,MAXCOL,NUMCOL
 9014 FORMAT('MAXN,MAXCOL,NUMCOL = ',3I8)
      CALL DPWRST('XXX','BUG ')
      DO9020I=1,NUMNAM
      WRITE(ICOUT,9021)I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)
 9021 FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I) = ',
     1I8,2X,A4,A4,2X,A4,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)
 9022 FORMAT('I,IHNAME(I),IHNAM2(I),IN(I),IVSTAR(I),IVSTOP(I)  = ',
     1I8,2X,A4,A4,6X,I8,I8,I8)
      CALL DPWRST('XXX','BUG ')
 9020 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO9030J=1,NUMCOL
      IJ=MAXN*(J-1)+1
      WRITE(ICOUT,9031)J,MAXN,IJ,V(IJ)
 9031 FORMAT('J,MAXN,IJ,V(IJ) = ',I8,I8,I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9030 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPVECT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IVCFMT,IVCARR,IANGLU,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A VECTOR PLOT--
C              THE VECTOR CAN BE REPRESENTED IN ONE OF 3 WAYS:
C              1)  YSTART XSTART  YSTOP XSTOP
C                  (I.E., START AND END POINT OF VECTOR)
C              2)  YSTART XSTART ANGLE DIST
C                  (I.E., START POINT, ANGLE OF VECTOR AND LENGTH
C                  OF VECTOR)
C              3)  YSTART XSTART YDELTA XDELTA
C                  (I.E., START POINT, X AND Y COMPONENTS OF VECTOR)
C              THE FORMAT IS DETERMINED BY THE COMMAND:
C                  VECTOR FORMAT <POINT/ANGLE/DELTA>
C              THE ARROW HEAD CAN BE EITHER A FIXED SIZE OR A
C              VARY ACCORDING TO THE VECTOR LENGTH (THE CHAR SIZE
C              COMMAND WILL SET THE ARROW SIZE FOR THE LARGEST VECTOR).
C              THIS IS CONTROLLED WITH THE COMMAND:
C                   VECTOR ARROW <FIXED/VARIABLE>
C     EXAMPLE--VECTOR PLOT XSTART XSTOP ANGLE DISTANCE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--92/8
C     ORIGINAL VERSION--AUGUST    1992.
C     UPDATED         --SEPTEMBER 1993. BUG FIX
C     UPDATED         --AUGUST    1994. BUG FIX FOR VARIABLE CASE
C     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IVCFMT
      CHARACTER*4 IVCARR
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
C
      CHARACTER*4 IHIGH
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
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION Y3(MAXOBV)
      DIMENSION Y4(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XDIST(MAXOBV)
CCCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),Y3(1))
      EQUIVALENCE (GARBAG(IGARB4),Y4(1))
      EQUIVALENCE (GARBAG(IGARB5),XHIGH(1))
      EQUIVALENCE (GARBAG(IGARB6),XDIST(1))
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
      IFOUND='NO'
      ISUBN1='DPVE'
      ISUBN2='CT  '
      ICASPL='VECT'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************
C               **  TREAT THE VECTOR PLOT CASE  **
C               ***********************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VECT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPVECT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN = ',3(A4,2X,A4,2X,A4),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VECT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHIGH='OFF'
      IF(ICOM.EQ.'VECT')THEN
        IF(NUMARG.GE.1 .AND.
     1    (IHARG(1).EQ.'HIGH' .OR. IHARG(1).EQ.'SUBS').AND.
     1    IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IHIGH='ON'
        ELSEIF(NUMARG.GE.1 .AND. IHARG(1).EQ.'PLOT')THEN
          ILASTC=1
        ELSE
          GOTO9000
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IHIGH='ON'
        IF(NUMARG.GE.1 .AND.IHARG(1).EQ.'VECT'.AND.
     1    IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
        ELSE
          GOTO9000
        ENDIF
      ELSE
        GOTO9000
      ENDIF
C
C
C               *********************************
C               **  STEP 4--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VECT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='VECTOR PLOT'
      MINNA=4
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      IF(IHIGH.EQ.'ON')THEN
        MINNA=5
        MINNVA=5
        MAXNVA=5
      ELSE
        MINNVA=4
        MAXNVA=4
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VECT')THEN
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
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y2,Y3,Y4,Y4,XHIGH,Y4,NS,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
C
C               ********************************************************
C               **  STEP 8--                                          **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS             **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.                **
C               **  DEFINE THE VECTOR D(.) TO CREATE PAIRS OF POINTS  **
C               **  (EACH ROW WILL DEFINE A SINGLE VECTOR WITH A      **
C               **  UNIQUE D IDENTIFIER.                              **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
C               ********************************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VECT')THEN
        ISTEPN='8'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8901)NS,ICASPL,IVCFMT,IVCARR,IANGLU
 8901   FORMAT('NS,ICASPL,IVCFMT,IVCARR,IANGLU=',I5,4(1X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL DPVEC2(Y1,Y2,Y3,Y4,XHIGH,XDIST,NS,ICASPL,IHIGH,
     1            IVCFMT,IVCARR,IANGLU,MAXOBV,
     1            Y,X,D,DSIZE,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VECT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVECT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(NS.GE.1)THEN
          DO9042I=1,NS
            WRITE(ICOUT,9043)I,Y1(I),Y2(I),Y3(I),Y4(I)
 9043       FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(I) = ',I8,4G15.7)
            CALL DPWRST('XXX','BUG ')
 9042      CONTINUE
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
      SUBROUTINE DPVEC2(Y1,Y2,Y3,Y4,XHIGH,XDIST,NZ,ICASPL,IHIGH,
     1                  IVCFMT,IVCARR,IANGLU,MAXNXT,
     1                  Y,X,D,DSIZE,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS THAT WILL DEFINE
C              A VECTOR PLOT
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--92/8
C     ORIGINAL VERSION--AUGUST    1992.
C     UPDATED         --FEBRUARY  2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
      CHARACTER*4 IVCFMT
      CHARACTER*4 IVCARR
      CHARACTER*4 IANGLU
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION Y3(*)
      DIMENSION Y4(*)
      DIMENSION XHIGH(*)
      DIMENSION XDIST(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION DSIZE(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPVE'
      ISUBN2='C2  '
      IERROR='NO'
      IWRITE='OFF'
C
      PI=3.1415926
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
   31   FORMAT('***** ERROR IN VECTOR PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 1;')
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VEC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPVEC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,NZ,N2,NPLOTV,MAXNXT
   72   FORMAT('ICASPL,NZ,N2,NPLOTV,MAXNXT = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)IVCFMT,IVCARR,IANGLU,IHIGH
   73   FORMAT('IVCFMT,IVCARR,IANGLU,IHIGH=',3(A4,1X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(NZ.GE.1)THEN
          DO81I=1,NZ
           WRITE(ICOUT,82)I,Y1(I),Y2(I),Y3(I),Y4(I)
   82      FORMAT('I,Y1(I),Y2(I),Y3(I),Y4(I) = ',I8,4E12.5)
           CALL DPWRST('XXX','BUG ')
   81    CONTINUE
        ENDIF
      ENDIF
C
C               ****************************************
C               **  STEP 11--                         **
C               **  DETERMINE PLOT COORDINATES        **
C               **  HANDLE 3 CASES OF VECTOR FORMAT   **
C               **  SEPARATELY.                       **
C               **  IVCFMT = POINT                    **
C               **           Y1, Y2 = (X,Y) START POINT*
C               **           Y3, Y4 = (X,Y) STOP POINT**
C               **         = ANGLE                    **
C               **           Y1, Y2 = (X,Y) START POINT*
C               **           Y3 = ANGLE               **
C               **           Y4 = LENGTH              **
C               **         = DELTA                    **
C               **           Y1, Y2 = (X,Y) START POINT*
C               **           Y3 = X DISTANCE          **
C               **           Y4 = Y DISTANCE          **
C               ****************************************
C
CCCCC IF(NHIGH.GT.0)THEN
      IF(IHIGH.EQ.'ON')THEN
        CALL CODE(XHIGH,NZ,IWRITE,XDIST,DSIZE,MAXN,IBUGG3,IERROR)
        DO1010I=1,NZ
          XHIGH(I)=XDIST(I)
 1010   CONTINUE
      ELSE
        DO1020I=1,NZ
          XHIGH(I)=1.0
 1020   CONTINUE
      ENDIF
C
      ANZ=NZ
      AMXDST=-1.0
      IF(IVCFMT.EQ.'POIN')THEN
        J=0
        K=0
        DO1100I=1,NZ
          J=J+1
          K=K+1
          X(J)=Y2(I)
          Y(J)=Y1(I)
          D(J)=REAL(K)
          J=J+1
          X(J)=Y4(I)
          Y(J)=Y3(I)
          D(J)=REAL(K)
          DIST=(Y2(I)-Y4(I))**2 + (Y1(I)-Y3(I))**2
          IF(DIST.GT.AMXDST)AMXDST=DIST
 1100   CONTINUE
      ELSEIF(IVCFMT.EQ.'DELT')THEN
        J=0
        K=0
        DO1200I=1,NZ
          J=J+1
          K=K+1
          X(J)=Y2(I)
          Y(J)=Y1(I)
          D(J)=REAL(K)
          J=J+1
          X(J)=Y2(I)+Y4(I)
          Y(J)=Y1(I)+Y3(I)
          D(J)=REAL(K)
          DIST=Y4(I)*Y4(I)+Y3(I)*Y3(I)
          IF(DIST.GT.AMXDST)AMXDST=DIST
 1200   CONTINUE
      ELSE
        J=0
        K=0
        DO1300I=1,NZ
          J=J+1
          K=K+1
          X(J)=Y2(I)
          Y(J)=Y1(I)
          D(J)=REAL(K)
          J=J+1
          THETA=Y3(I)
          DIST=Y4(I)
          IF(DIST.GT.AMXDST)AMXDST=DIST
          IF(IANGLU.EQ.'DEGR')THETA=THETA*(PI/180.0)
          X(J)=Y2(I)+DIST*COS(THETA)
          Y(J)=Y1(I)+DIST*SIN(THETA)
          D(J)=REAL(K)
 1300   CONTINUE
      ENDIF
C
      N2=J
      NPLOTV=3
C
C     ***************************************
C     **  HANDLE FIXED OR VARIABLE SIZE    **
C     **  ARROWS.                          **
C     ***************************************
C
      IF(IVCARR.EQ.'FIXE')THEN
        DO2100I=1,N2
          DSIZE(I)=1.0
 2100   CONTINUE
      ELSE
        ICASPL='VVAR'
CCCCC   AUGUST, 1994.  BASE ON X AND Y ARRAYS, TREATED SAME
CCCCC   FOR ALL CASES.
        J1=0
        DO2200I=1,N2,2
CCCCC     I1=I
CCCCC     I2=I+1
CCCCC     J=MOD(I1,2)+1
CCCCC     IF(IVCFMT.EQ.'POIN')THEN
CCCCC       DIST=(Y2(J)-Y4(J))**2 + (Y1(J)-Y3(J))**2
CCCCC     ELSEIF(IVCFMT.EQ.'DELT')THEN
CCCCC       DIST=Y4(J)*Y4(J)+Y3(J)*Y3(J)
CCCCC     ELSE
CCCCC       DIST=Y4(J)
CCCCC     ENDIF
          DIST=(X(I)-X(I+1))**2 + (Y(I)-Y(I+1))**2
          ASIZE=DIST/AMXDST
          IF(ASIZE.GT.1.0)ASIZE=1.0
          IF(ASIZE.LE.0.05)ASIZE=0.05
CCCCC     DSIZE(I1)=ASIZE
CCCCC     DSIZE(I2)=ASIZE
          DSIZE(I)=ASIZE
          DSIZE(I+1)=ASIZE
 2200   CONTINUE
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VEC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVEC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,NZ,N2,IERROR
 9012   FORMAT('ICASPL,NZ,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N2,NPLOTV,J,K,AMXDST
 9013   FORMAT('N2,NPLOTV,J,K,AMXDST = ',4I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NZ.GE.1)THEN
          DO9021I=1,NZ
            WRITE(ICOUT,9022)I,Y1(I),Y2(I),Y3(I),Y4(I)
 9022       FORMAT('I,Y1(I),Y2(I),Y3(I) = ',I8,4E12.5)
            CALL DPWRST('XXX','BUG ')
 9021     CONTINUE
        ENDIF
        DO9035I=1,N2
          WRITE(ICOUT,9036)I,Y(I),X(I),D(I),DSIZE(I)
 9036     FORMAT('I,Y(I),X(I),D(I) = ',I8,2E15.7,F9.2,F5.2)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPVCFM(IHARG,NUMARG,
     1IDEFVF,
     1IVCFMT,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE VECTOR FORMAT
C              CAN BE <POINT/ANGLE/DELTA> (DEFAULT IS ANGLE)
C              THIS SWITCH CONTROLS HOW THE 4 ARGUMENTS TO THE
C              VECTOR PLOT COMMAND ARE INTERPERTED (2 POINTS,
C              1 POINT WITH ANGLE AND DISTANCE, 1 POINT WITH
C              X DISTANCE AND Y DISTANCE)
C
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFVF (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IVCFMT (A CHARACTER VARIABLE)
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
C     ORIGINAL VERSION--AUGUST   1992.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFVF
      CHARACTER*4 IVCFMT
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
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPVCFM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFVF
   53 FORMAT('IDEFVF = ',A4)
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
      IHOLD=IDEFVF
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(2)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IVCFMT=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IVCFMT
 1181 FORMAT('THE VECTOR FORMAT SWITCH HAS JUST BEEN SET TO ',
     1A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPVCFM')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFVF,IVCFMT
 9013 FORMAT('IDEFVF,IVCFMT = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPVCAR(IHARG,NUMARG,
CCCCC1IDEFAR,IDEFVO,
     1IDEFVA,IDEFVO,
     1IVCARR,IVCOPN,
     1IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--VECTOR ARROW <FIXED/VARIABLE>
C              VECTOR ARROW <OPEN/CLOSED>
C              <FIXED/VARIABLE> CONTROLS WHETHER THE ARROWS ON
C              THE VECTOR PLOT COMMAND ARE DRAWN AS FIXED LENGTH
C              OR SIZE SCALED RELATIVE TO THE LENGTH OF THE VECTOR.
C              <OPEN/CLOSED> CONTROLS WHETHER THE ARROW IS DRAWN
C              LIKE A TRIANGLE (CLOSED, THE DEFAULT) OR WITH THE
C              BASE OF THE TRIANGLE LEFT OFF (OPEN).
C
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFVA (A  CHARACTER VARIABLE)
C                     --IDEFVO (A  CHARACTER VARIABLE)
C                     --IBUGS2 (A  CHARACTER VARIABLE)
C     OUTPUT ARGUMENTS--IVCARR (A CHARACTER VARIABLE)
C                     --IVCOPN (A CHARACTER VARIABLE)
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
C     ORIGINAL VERSION--AUGUST   1992.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IDEFVA
      CHARACTER*4 IVCARR
CCCCC OCTOBER 1993. ADD FOLLOWING 2 LINES
      CHARACTER*4 IVCOPN
      CHARACTER*4 IDEFVO
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPVCAR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IDEFVA,IDEFVO
   53 FORMAT('IDEFVA,IDEFVO = ',A4,1X,A4)
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
      IF(NUMARG.NE.2)GOTO9000
C
      IF(IHARG(2).EQ.'AUTO')GOTO1150
      IF(IHARG(2).EQ.'DEFA')GOTO1150
      IF(IHARG(2).EQ.'FIXE')GOTO1160
      IF(IHARG(2).EQ.'VARI')GOTO1170
      IF(IHARG(2).EQ.'OPEN')GOTO1180
      IF(IHARG(2).EQ.'CLOS')GOTO1190
      GOTO1150
C
 1150 CONTINUE
      IVCARR=IDEFVA
      IVCOPN=IDEFVO
      GOTO2000
C
 1160 CONTINUE
      IVCARR='FIXE'
      GOTO2000
C
 1170 CONTINUE
      IVCARR='VARI'
      GOTO2000
C
 1180 CONTINUE
      IVCOPN='OPEN'
      GOTO2000
C
 1190 CONTINUE
      IVCOPN='CLOS'
      GOTO2000
C
 2000 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO2089
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2051)IVCARR
 2051 FORMAT('THE VECTOR ARROW SIZE WILL BE ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2052)IVCOPN
 2052 FORMAT('THE VECTOR ARROW HEAD WILL BE ',A4)
      CALL DPWRST('XXX','BUG ')
 2089 CONTINUE
      GOTO9000
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPVCAR-')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGS2,IFOUND,IERROR
 9012 FORMAT('IBUGS2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)IDEFVA,IVCARR
 9013 FORMAT('IDEFVA,IVCARR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IDEFVO,IVCOPN
 9014 FORMAT('IDEFVO,IVCOPN = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPVERT(IHARG,IARGT,ARG,NUMARG,
     1PDEFVG,
     1PTEXVG,
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE VERTICAL GAP FOR TEXT CHARACTERS.
C              THE VERTICAL GAP FOR TEXT CHARACTERS WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE PTEXVG.
C     NOTE--THE VERTICAL GAP IS IN STANDARDIZED UNITS (0.0 TO 100.0).
C     NOTE--THE VERTICAL GAP IS THE BETWEEN-LINE SPACING (DISTANCE)
C           FROM THE BOTTOM OF A CHARACTER ON ONE LINE
C           TO THE TOP OF A CHARACTER ON THE NEXT LINE.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT
C                     --ARG
C                     --NUMARG
C                     --PDEFVG
C                     --IBUGD2
C     OUTPUT ARGUMENTS--PTEXVG
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPVERT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)PDEFVG
   53 FORMAT('PDEFVG = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56 FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               ***********************************
C               **  TREAT THE VERTICAL GAP CASE  **
C               ***********************************
C
      IF(NUMARG.LE.0)GOTO1150
      IF(NUMARG.EQ.1.AND.IHARG(NUMARG).EQ.'GAP')GOTO1150
      IF(NUMARG.EQ.1.AND.IHARG(NUMARG).EQ.'SPAC')GOTO1150
      IF(NUMARG.EQ.1.AND.IHARG(NUMARG).EQ.'DIST')GOTO1150
      IF(NUMARG.EQ.1.AND.IHARG(NUMARG).EQ.'LENG')GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
C
      IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')
     1GOTO1160
C
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPVERT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR VERTICAL GAP ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE IT IS DESIRED THAT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      THE TEXT CHARACTERS HAVE A VERTICAL SPACING ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      OF 2 (WHERE THE VERTICAL SCREEN UNITS RANGE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      FROM 0 TO 100,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('           VERTICAL SPACING 5 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      PTEXVG=PDEFVG
      GOTO1180
C
 1160 CONTINUE
      PTEXVG=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE VERTICAL SPACING (FOR TEXT CHARACTERS)  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PTEXVG
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPVERT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)PTEXVG
 9013 FORMAT('PTEXVG = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPVIOL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  ICONT,IFENCE,IKDETY,IKDENP,PKDEWI,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A VIOLIN PLOT
C              A VIOLIN PLOT GENERATES A BOX PLOT.  IT THEN ADDS
C              A VERTICAL DENSITY PLOT TO EACH SIDE OF THE BOX
C              PLOT.
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
C     UPDATED         --FEBRUARY  2011. USE DPPARS AND DPPAR3 TO PERFORM
C                                       THE COMMAND PARSING
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "MULTIPLE" CASE
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR TWO GROUP-ID VARIABLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 ICONT
      CHARACTER*4 IFENCE
      CHARACTER*4 IKDETY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 IFOUN1
      CHARACTER*4 IFOUN2
      CHARACTER*4 IWRITE
C
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
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
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZD.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION X4(MAXOBV)
      DIMENSION X5(MAXOBV)
      DIMENSION X6(MAXOBV)
      DIMENSION XTEMP1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION XTEMP3(MAXOBV)
      DIMENSION XTEMP4(MAXOBV)
      DIMENSION XTEMP5(MAXOBV)
      DIMENSION XTEMP6(MAXOBV)
      DIMENSION XTEMP0(MAXOBV)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION TEMP(MAXOBV)
C
      DOUBLE PRECISION DTEMP1(MAXOBV)
      DOUBLE PRECISION DTEMP2(MAXOBV)
      DOUBLE PRECISION DTEMP3(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),X2(1))
      EQUIVALENCE (GARBAG(IGARB4),X3(1))
      EQUIVALENCE (GARBAG(IGARB5),X4(1))
      EQUIVALENCE (GARBAG(IGARB6),X5(1))
      EQUIVALENCE (GARBAG(IGARB7),X6(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB9),XTEMP1(1))
      EQUIVALENCE (GARBAG(IGAR10),XTEMP2(1))
      EQUIVALENCE (GARBAG(JGAR11),XTEMP3(1))
      EQUIVALENCE (GARBAG(JGAR12),XTEMP4(1))
      EQUIVALENCE (GARBAG(JGAR13),XTEMP5(1))
      EQUIVALENCE (GARBAG(JGAR14),XTEMP6(1))
      EQUIVALENCE (GARBAG(JGAR15),XTEMP0(1))
      EQUIVALENCE (GARBAG(JGAR16),TEMP(1))
C
      EQUIVALENCE (DGARBG(IDGAR1),DTEMP1(1))
      EQUIVALENCE (DGARBG(IDGAR2),DTEMP2(1))
      EQUIVALENCE (DGARBG(IDGAR3),DTEMP3(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPVI'
      ISUBN2='OL  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               **********************************
C               **  TREAT THE VIOLIN PLOT CASE  **
C               **********************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'VIOL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPVIOL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO
   53   FORMAT('ICONT,IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IFENCE,IKDETY,IKDENP,PKDEWI
   54   FORMAT('IFENCE,IKDETY,IDENP,PKDEWI = ',A4,2X,A4,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  EXTRACT THE COMMAND                             **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:         **
C               **    1) VIOLIN PLOT Y X1 ... X2                    **
C               **    2) MULTIPLE VIOLIN PLOT Y1 ... YK             **
C               **    3) REPLICATED VIOLIN PLOT Y X1 X2             **
C               **  THE "REPLICATION" CASE IS ACTUALLY THE DEFAULT  **
C               **  AND THE KEYWORD "REPLICATION" IS OPTIONAL.      **
C               **  HOWEVER, SUPPORT IT FOR COMPATABILITY WITH      **
C               **  OTHER COMMANDS.                                 **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'VIOL')GOTO89
      IF(ICOM.EQ.'MULT')GOTO89
      IF(ICOM.EQ.'REPL')GOTO89
      GOTO9000
C
   89 CONTINUE
      ICASPL='VIPL'
      IMULT='OFF'
      IREPL='OFF'
      ILASTC=-9999
C
      IF(ICOM.EQ.'VIOL')THEN
        IFOUN1='YES'
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
          IFOUN2='YES'
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
        ELSEIF(IHARG(I).EQ.'VIOL')THEN
          IFOUN1='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'PLOT')THEN
          IFOUN2='YES'
          ILASTC=MAX(ILASTC,I)
        ELSEIF(IHARG(I).EQ.'REPL')THEN
          IREPL='ON'
        ELSEIF(IHARG(I).EQ.'MULT')THEN
          IMULT='ON'
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
  101     FORMAT('***** ERROR IN VIOLIN PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,107)
  107     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THIS PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(ILASTC.GE.1)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'VIOL')THEN
        WRITE(ICOUT,112)ICASPL,IMULT,IREPL,ILASTC
  112   FORMAT('ICASPL,IMULT,IREPL,ILASTC = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='VIOLIN PLOT'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IF(IMULT.EQ.'ON')THEN
        IFLAGE=0
      ELSE
        IREPL='ON'
      ENDIF
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=2
C
C     NOTE: NEED TO KEEP "VIOLIN PLOT Y" AS VALID SYNTAX, SO
C           MINIMUM NUMBER OF VARIABLES IS 1 EVEN FOR REPLICATION
C           CASE.
C
      IF(IREPL.EQ.'ON')THEN
CCCCC   MINNVA=MINNVA+1
        MAXNVA=MAXNVA+5
      ELSEIF(IMULT.EQ.'ON')THEN
        MAXNVA=30
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')THEN
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
      NRESP=1
C
      NREPL=0
      IF(IMULT.EQ.'ON')THEN
        NRESP=NUMVAR
      ELSE
        NREPL=NUMVAR-NRESP
        IF(NREPL.LT.0 .OR. NREPL.GT.6)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,511)
  511     FORMAT('      FOR THE REPLICATION CASE, THE NUMBER OF ',
     1           'REPLICATION VARIABLES')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      MUST BE BETWEEN 0 AND 6;  SUCH WAS NOT THE ',
     1           'CASE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,513)NREPL
  513     FORMAT('      THE NUMBER OF REPLICATION VARIABLES = ',I5)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')THEN
        ISTEPN='6'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,601)NRESP,NREPL
  601   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 7A--                                   **
C               **  CASE 1: NO "MULTIPLE" CASE--CAN HAVE EITHER **
C               **          1, 2, OR 3 VARIABLES.  THE FIRST    **
C               **          VARIABLE IS A RESPONSE VARIABLE     **
C               **          AND THE SECOND AND THIRD VARIABLES  **
C               **          ARE REPLICATION VARIABLES (IF       **
C               **          PRESENT).  NOTE THAT THIS VERSION   **
C               **          DOES NOT ACCEPT MATRIX ARGUMENTS    **
C               **          EVEN IF ONLY A SINGLE ARGUMENT IS   **
C               **          GIVEN (YOU CAN USE THE MULTIPLE     **
C               **          OPTION IN THAT CASE).               **
C               **************************************************
C
      IF(IMULT.EQ.'OFF')THEN
        ISTEPN='7A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        ICOL=1
        CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1              INAME,IVARN1,IVARN2,IVARTY,
     1              ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1              MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1              MAXCP4,MAXCP5,MAXCP6,
     1              V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1              Y1,X1,X2,X3,X4,X5,X6,NLOCAL,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C       IF THERE ARE TWO OR MORE REPLICATION VARIABLES, COMBINE
C       THEM TO CREATE A SINGLE REPLICATION VARIABLE.
C
        IF(NUMVAR.EQ.3)THEN
          CALL CODCT2(X1,X2,NLOCAL,ICCTOF,ICCTG1,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,
     1                IBUGG3,ISUBRO,IERROR)
          DO7011I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7011     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.4)THEN
          CALL CODCT3(X1,X2,X3,NLOCAL,ICCTOF,ICCTG1,ICCTG2,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,
     1                IBUGG3,ISUBRO,IERROR)
          DO7012I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7012     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.5)THEN
          CALL CODCT4(X1,X2,X3,X4,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,
     1                IBUGG3,ISUBRO,IERROR)
          DO7013I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7013     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.6)THEN
          CALL CODCT5(X1,X2,X3,X4,X5,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,
     1                IBUGG3,ISUBRO,IERROR)
          DO7014I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7014     CONTINUE
          NUMVAR=2
        ELSEIF(NUMVAR.EQ.7)THEN
          CALL CODCT6(X1,X2,X3,X4,X5,X6,NLOCAL,
     1                ICCTOF,ICCTG1,ICCTG2,ICCTG3,ICCTG4,ICCTG5,IWRITE,
     1                XTEMP0,XTEMP1,XTEMP2,XTEMP3,XTEMP4,XTEMP5,XTEMP6,
     1                IBUGG3,ISUBRO,IERROR)
          DO7015I=1,NLOCAL
            X1(I)=XTEMP0(I)
 7015     CONTINUE
          NUMVAR=2
        ENDIF
C
C               *********************************************************
C               **  STEP 7B--                                         **
C               **  GENERATE THE VIOLIN PLOT.                         **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).     **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).     **
C               *********************************************************
C
        CALL DPVIO2(Y1,X1,NLOCAL,NUMVAR,ICASPL,ISIZE,ICONT,MAXOBV,
     1              IFENCE,IBXPWI,IKDETY,IKDENP,PKDEWI,
     1              XIDTEM,TEMP,DTEMP1,DTEMP2,DTEMP3,
     1              Y,X,D,NPLOTP,NPLOTV,ISUBRO,IBUGG3,IERROR)
C
C               ***********************************************
C               **  STEP 8A--                                **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES.     **
C               **          THESE CAN BE EITHER VARIABLE OR  **
C               **          MATRIX ARGUMENTS.                **
C               ***********************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='8A'
        IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'VIOL')
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
     1              XTEMP1,Y1,X1,NLOCAL,ICASE,
     1              IBUGG3,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        NUMVAR=2
C
C               *****************************************************
C               **  STEP 8B--                                      **
C               **  FORM THE VERTICAL AND HORIZONTAL AXIS          **
C               **  VALUES Y(.) AND X(.) FOR THE PLOT.             **
C               *****************************************************
C
        CALL DPVIO2(Y1,X1,NLOCAL,NUMVAR,ICASPL,ISIZE,ICONT,MAXOBV,
     1              IFENCE,IBXPWI,IKDETY,IKDENP,PKDEWI,
     1              XIDTEM,TEMP,DTEMP1,DTEMP2,DTEMP3,
     1              Y,X,D,NPLOTP,NPLOTV,ISUBRO,IBUGG3,IERROR)
      ENDIF
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'VIOL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVIOL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IFENCE,ISIZE
 9014   FORMAT('IFENCE,ISIZE = ',A4,I8)
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
      SUBROUTINE DPVIO2(Y,X,N,NUMV2,ICASPL,ISIZE,ICONT,MAXOBV,
     1                  IFENCE,IBXPWI,IKDETY,IKDENP,PKDEWI,
     1                  XIDTEM,TEMP,DY,FT,SMOOTH,
     1                  Y2,X2,D2,N2,NPLOTV,ISUBRO,IBUGG3,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A VIOLIN PLOT.
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
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IFENCE
      CHARACTER*4 IBXPWI
      CHARACTER*4 IKDETY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
      DIMENSION XIDTEM(*)
      DIMENSION TEMP(*)
      DOUBLE PRECISION DY(*)
      DOUBLE PRECISION FT(*)
      DOUBLE PRECISION SMOOTH(*)
C
      DOUBLE PRECISION DH
      DOUBLE PRECISION DHI
      DOUBLE PRECISION DLO
      DOUBLE PRECISION DN
      DOUBLE PRECISION DSUM
      DOUBLE PRECISION DX
      DOUBLE PRECISION DMEAN
      DOUBLE PRECISION DVAR
      DOUBLE PRECISION DSD
      DOUBLE PRECISION DYMX
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPVI'
      ISUBN2='O2  '
C
      I2=0
      ISIZE2=0
C
      AN=0.0
      SIZE=0.0
      SIZE2=0.0
      XWIDTH=0.0
      XWIDT2=0.0
      YBARI=0.0
      SDI=0.0
      YMED=0.0
C
      H=0.0
      STEP=0.0
      AINNFU=0.0
      AOUTFU=0.0
      IREV=0
      AINNFL=0.0
      AOUTFL=0.0
C
      DO 10 I=1,MAXOBV
        X2(I)=0.0
        Y2(I)=0.0
        D2(I)=0.0
 10   CONTINUE
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN VIOLIN PLOT--')
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
      HOLD=Y(1)
      DO60I=1,N
        IF(Y(I).NE.HOLD)GOTO69
   60 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,31)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)
   62 FORMAT('      ALL RESPONSE VARIABLE ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)HOLD
   63 FORMAT('      ARE IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
   69 CONTINUE
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')THEN
        WRITE(ICOUT,70)
   70   FORMAT('AT THE BEGINNING OF DPVIO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)N,ICASPL,NUMV2,ISIZE,ICONT
   71   FORMAT('N,ICASPL,NUMV2,ISIZE,ICONT = ',I8,2X,A4,I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IFENCE,IKDETY
   72   FORMAT('IFENCE,IKDETY = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO75I=1,N
          WRITE(ICOUT,76)I,Y(I),X(I)
   76     FORMAT('I, Y(I), X(I) = ',I8,2F15.7)
          CALL DPWRST('XXX','BUG ')
   75   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE NUMBER OF DISTINCT VALUES         **
C               **  FOR VARIABLE 2 (THE GROUP VARIABLE).            **
C               **  IF ALL VALUES ARE DISTINCT, THEN THIS           **
C               **  IMPLIES WE HAVE THE NO REPLICATION CASE         **
C               **  WHICH IS AN ERROR CONDITION FOR A VIOLIN PLOT.  **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMV2.EQ.1)THEN
        DO120I=1,N
          X(I)=1.0
  120   CONTINUE
        NUMSET=1
        XIDTEM(1)=X(1)
      ELSEIF(NUMV2.EQ.2)THEN
        NUMSET=0
        DO160I=1,N
          IF(NUMSET.EQ.0)GOTO165
          DO170J=1,NUMSET
            IF(X(I).EQ.XIDTEM(J))GOTO160
  170     CONTINUE
  165     CONTINUE
          NUMSET=NUMSET+1
          XIDTEM(NUMSET)=X(I)
  160   CONTINUE
        CALL SORT(XIDTEM,NUMSET,XIDTEM)
        XID1=XIDTEM(1)
        XID2=XIDTEM(NUMSET)
      ENDIF
C
      IF(NUMSET.EQ.0)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,191)
  191   FORMAT('       NUMSET = 0')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSEIF(NUMSET.EQ.N)THEN
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,192)
  192   FORMAT('       NUMSET = N')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               **********************************
C               **  STEP 2--                    **
C               **  IF NECESSARY,               **
C               **  COMPUTE AVERAGE CLASS SIZE  **
C               **********************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      AN=N
      ANUMSE=NUMSET
C
      SIZE=ISIZE
      SIZE2=SIZE
      SIZE2=AN/ANUMSE
      ISIZE2=INT(SIZE2+0.5)
C
C               ***********************************
C               **  STEP 3--                     **
C               **  COMPUTE MINIMUM CLASS WIDTH  **
C               ***********************************
C
      ISTEPN='3'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMSET.EQ.1)THEN
        XWIDTH=0.10*XIDTEM(1)
      ELSE
        XWIDTH=CPUMAX
        IMAX=NUMSET-1
        DO300I=1,IMAX
          IP1=I+1
          XWIDT2=XIDTEM(IP1)-XIDTEM(I)
          IF(XWIDT2.LT.XWIDTH)XWIDTH=XWIDT2
  300   CONTINUE
      ENDIF
C
C               **************************************
C               **  STEP 4--                        **
C               **  COMPUTE MAXIMUM SUBSAMPLE SIZE  **
C               **************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NIMAX=0
      DO400ISET=1,NUMSET
C
        K=0
        DO420I=1,N
          IF(X(I).EQ.XIDTEM(ISET))THEN
            K=K+1
            TEMP(K)=Y(I)
          ENDIF
  420   CONTINUE
        NI=K
        IF(NI.GT.NIMAX)NIMAX=NI
C
  400 CONTINUE
      ANIMAX=NIMAX
C
C               ***************************************************
C               **  STEP 5--                                     **
C               **  DETERMINE PLOT COORDINATES                   **
C               ***************************************************
C
      ISTEPN='5'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCPL=11
      J=0
      JD=0
      DO1110ISET=1,NUMSET
C
        K=0
        DO1120I=1,N
          IF(X(I).EQ.XIDTEM(ISET))THEN
            K=K+1
            TEMP(K)=Y(I)
          ENDIF
 1120   CONTINUE
        NI=K
        ANI=NI
C
        IF(NI.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1121)
 1121     FORMAT('***** INTERNAL ERROR IN DPVIO2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1122)
 1122     FORMAT('NI FOR SOME CLASS = 0')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1123)ISET,XIDTEM(ISET),NI
 1123     FORMAT('ISET,XIDTEM(ISET),NI = ',I8,E15.7,I8)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        CALL SORT(TEMP,NI,TEMP)
C
        XMID=XIDTEM(ISET)
C
        IF(IBXPWI.EQ.'FIXE')THEN
          FACTOR=1.0
        ELSE
          FACTOR=SQRT(ANI/ANIMAX)
        ENDIF
        XLEFT=XMID-(XWIDTH/4.0)*FACTOR
        XRIGHT=XMID+(XWIDTH/4.0)*FACTOR
        XLEF2=XMID-(XWIDTH/2.5)
        XRIGH2=XMID+(XWIDTH/2.5)
C
C               **********************************************
C               **  STEP 5.05--                             **
C               **  CALL DENEST ROUTINE TO COMPUTE THE      **
C               **  KERNEL DENSITY ESTIMATE.                **
C               **********************************************
C
        DO1010I=1,NI
          DY(I)=DBLE(TEMP(I))
 1010   CONTINUE
C   
        IERROR='NO'
        ICAL=0
        KFLAG=1
        CALL DSORT(DY,DY,NI,KFLAG,IERROR)
        DH=DBLE(PKDEWI)
        IF(PKDEWI.LE.0)THEN
          DN=DBLE(NI)
          DSUM=0.0D0
          DO1020I=1,NI
            DX=DY(I)
            DSUM=DSUM+DX
 1020     CONTINUE
          DMEAN=DSUM/DN
          DSUM=0.0D0
          DO1030I=1,NI
            DX=DY(I)
            DSUM=DSUM+(DX-DMEAN)**2
 1030     CONTINUE
          DVAR=DSUM/(DN-1.0D0)
          DSD=0.0D0
          IF(DVAR.GT.0.0D0)DSD=DSQRT(DVAR)
C
          P=0.25
          AN=REAL(NI)
          ANI=P*(AN+1.0)
          NI2=INT(ANI+0.1)
          A2NI=REAL(NI)
          REM=ANI-A2NI
          NIP1=NI2+1
          IF(NI2.LE.1)NI2=1
          IF(NI2.GE.NI)NI2=NI
          IF(NIP1.LE.1)NIP1=1
          IF(NIP1.GE.NI)NIP1=NI
          XPERC1=(1.0-REM)*TEMP(NI2)+REM*TEMP(NIP1)
C
          P=0.75
          ANI=P*(AN+1.0)
          NI2=INT(ANI+0.1)
          A2NI=REAL(NI2)
          REM=ANI-A2NI
          NIP1=NI2+1
          IF(NI2.LE.1)NI2=1
          IF(NI2.GE.NI)NI2=NI
          IF(NIP1.LE.1)NIP1=1
          IF(NIP1.GE.NI)NIP1=NI
          XPERC2=(1.0-REM)*TEMP(NI2)+REM*Y(NIP1)
          AIQ=(XPERC2-XPERC1)/1.34
C
          DH=0.9D0*MIN(DSD,DBLE(AIQ))*DN**(-1.0D0/5.0D0)
        ENDIF
        DLO=DY(1) - 3.0D0*DH
        DHI=DY(NI) + 3.0D0*DH
C
        CALL DENEST(DY,NI,DLO,DHI,DH,FT,SMOOTH,IKDENP,ICAL,IERROR)
C
        IF(IERROR.EQ.'YES')THEN
          WRITE(ICOUT,1041)
 1041     FORMAT('**** ERROR IN VIOLIN PLOT--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1043)ISET
 1043     FORMAT('     UNABLE TO COMPUTE DENSITY FUNCTION FOR ',
     1           'SET ',I8)
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
        DYMX=0.0D0
        DO1050I=1,IKDENP
          IF(SMOOTH(I).GT.DYMX)DYMX=SMOOTH(I)
 1050   CONTINUE
C
        JD=JD+1
        XINC=XRIGH2-XMID
        DO1060I=1,IKDENP
          J=J+1
          X2(J)=XMID + (XINC*REAL(SMOOTH(I))/REAL(DYMX))
          Y2(J)=REAL(DLO + (DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
          D2(J)=REAL(JD)
 1060   CONTINUE
        DO1065I=IKDENP-1,1,-1
          J=J+1
          X2(J)=XMID - (XINC*REAL(SMOOTH(I))/REAL(DYMX))
          Y2(J)=REAL(DLO + (DBLE(I) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
          D2(J)=REAL(JD)
 1065   CONTINUE
        J=J+1
        X2(J)=XMID + (XINC*REAL(SMOOTH(1))/REAL(DYMX))
        Y2(J)=REAL(DLO + (DBLE(1) - 0.5D0)*(DHI-DLO)/DBLE(IKDENP))
        D2(J)=REAL(JD)
C
C
C               ***************************
C               **  STEP 5.1--           **
C               **  COMPUTE THE MAXIMUM  **
C               ***************************
C
        YMAX=TEMP(NI)
C
C               ***********************************************
C               **  STEP 5.2--                               **
C               **  COMPUTE THE POINT AT THE TOP OF THE BOX  **
C               **  (THE UPPER HINGE FOR A MEDIAN BOX PLOT)  **
C               ***********************************************
C
        NI2=(NI+1)/2
        IARG1=(NI2+1)/2
        IARG2=(NI2+1)-IARG1
        IARG1R=NI-IARG1+1
        IARG2R=NI-IARG2+1
        Y75=(TEMP(IARG1R)+TEMP(IARG2R))/2.0
C
C               ***************************************
C               **  STEP 5.3--                       **
C               **  COMPUTE UPPER CONFIDENCE LIMITS  **
C               **  FOR THE MEAN                     **
C               ***************************************
C
        YUCL=Y75
C
C               *********************************
C               **  STEP 5.4--                 **
C               **  COMPUTE THE TYPICAL VALUE  **
C               **  (MEDIAN)                   **
C               *********************************
C
        N50=NI/2
        N50P1=N50+1
        IEVODD=NI-2*(NI/2)
        IF(IEVODD.EQ.0)YMED=(TEMP(N50)+TEMP(N50P1))/2.0
        IF(IEVODD.EQ.1)YMED=TEMP(N50P1)
        Y50=YMED
C
C               ****************************************************
C               **  STEP 5.5--                                    **
C               **  COMPUTE LOWER CONFIDENCE LIMITS FOR THE MEAN  **
C               ****************************************************
C
        YLCL=Y50
C
C               ****************************************************
C               **  STEP 5.6--                                    **
C               **  COMPUTE THE POINT AT THE BOTTOM OF THE BOX    **
C               **  (THE LOWER HINGE FOR A BOX VIOLIN PLOT)       **
C               ****************************************************
C
        NI2=(NI+1)/2
        IARG1=(NI2+1)/2
        IARG2=(NI2+1)-IARG1
        Y25=(TEMP(IARG1)+TEMP(IARG2))/2.0
C
C               ***************************
C               **  STEP 5.7--           **
C               **  COMPUTE THE MINIMUM  **
C               ***************************
C
        YMIN=TEMP(1)
C
C               **************************************************
C               **  STEP 5.7A--                                 **
C               **  FOR THE UPPER HALF OF THE DATA--            **
C               **  COMPUTE THE OUTER FENCE, THE INNER FENCE,   **
C               **  AND THE ADJACENT VALUE                      **
C               **************************************************
C
        H=Y75-Y25
        STEP=1.5*H
C
        AINNFU=Y75+STEP
        AOUTFU=Y75+2.0*STEP
        YADJU=Y75
        DO1155I=1,NI
          IREV=NI-I+1
          IF(TEMP(IREV).LE.AINNFU)GOTO1156
 1155   CONTINUE
        GOTO1159
 1156   CONTINUE
        YADJU=TEMP(IREV)
 1159   CONTINUE
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')THEN
          WRITE(ICOUT,1157)Y75,YADJU,TEMP(IREV),IREV
 1157     FORMAT('Y75,YADJU,TEMP(IREV),IREV = ',3E15.7,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ***************************************************
C               **  STEP 5.7B--                                  **
C               **  FOR THE LOWER HALF OF THE DATA--             **
C               **  COMPUTE THE OUTER FENCE, THE INNER FENCE,    **
C               **  AND THE ADJACENT VALUE                       **
C               ***************************************************
C
        AINNFL=Y25-STEP
        AOUTFL=Y25-2.0*STEP
        YADJL=Y25
        DO1165I=1,NI
          I2=I
          IF(TEMP(I2).GE.AINNFL)GOTO1166
 1165   CONTINUE
        GOTO1169
 1166   CONTINUE
        YADJL=TEMP(I2)
 1169   CONTINUE
C
C               *******************************************
C               **  STEP 6.1--                           **
C               **  IF IFENCE IS OFF, THEN               **
C               **  DEFINE THE CHARACTER AT THE MAXIMUM. **
C               **  IF IFENCE IS ON, THEN                **
C               **  DEFINE THE CHARACTER AT THE UPPER    **
C               **  ADJACENT VALUE;                      **
C               *******************************************
C
        IF(IFENCE.EQ.'OFF')
     1    CALL DPCHLI(ICONT,NUMCPL,YMAX,YMAX,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
        IF(IFENCE.EQ.'ON')
     1    CALL DPCHLI(ICONT,NUMCPL,YADJU,YADJU,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
C
C               ****************************************
C               **  STEP 6.2--                       **
C               **  DEFINE THE CHARACTER AT THE TOP   **
C               **  OF THE BOX                        **
C               **  (UPPER HINGE CHARACTER, IF ANY).  **
C               ****************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y75,Y75,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ***************************************************
C               **  STEP 6.3--                                   **
C               **  DEFINE THE CHARACTER IN THE BOX              **
C               **  BUT TOWARDS THE TOP OF THE BOX               **
C               ***************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YUCL,YUCL,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ***************************************
C               **  STEP 6.4--                       **
C               **  DEFINE THE CHARACTER IN THE BOX  **
C               **  NEAR THE MIDDLE                  **
C               **  (SUCH AS THE MEDIAN OR MEAN)     **
C               ***************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y50,Y50,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               *****************************************************
C               **  STEP 6.5--                                     **
C               **  DEFINE THE CHARACTER IN THE BOX                **
C               **  BUT TOWARDS THE BOX OF THE BOX                 **
C               **  (SUCH AS A LOWER CONFIDENCE LIMIT FOR THE MEAN,**
C               **  IF ANY)                                        **
C               *****************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YLCL,YLCL,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ******************************************
C               **  STEP 6.6--                          **
C               **  DEFINE THE CHARACTER AT THE BOTTOM  **
C               **  OF THE BOX                          **
C               **  (LOWER HINGE CHARACTER, IF ANY).    **
C               ******************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y25,Y25,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               *******************************************
C               **  STEP 6.7--                           **
C               **  IF IFENCE IS OFF, THEN               **
C               **  DEFINE THE CHARACTER AT THE MINIMUM. **
C               **  IF IFENCE IS ON, THEN                **
C               **  DEFINE THE CHARACTER AT THE LOWER    **
C               **  ADJACENT VALUE;                      **
C               *******************************************
C
        IF(IFENCE.EQ.'OFF')
     1    CALL DPCHLI(ICONT,NUMCPL,YMIN,YMIN,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
        IF(IFENCE.EQ.'ON')
     1    CALL DPCHLI(ICONT,NUMCPL,YADJL,YADJL,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
C
C               ***********************************************
C               **  STEP 6.8--                               **
C               **  IF IFENCE IS OFF, THEN                   **
C               **  DEFINE THE VERTICAL LINE FROM            **
C               **  THE MAXIMUM VALUE TO THE TOP OF THE BOX  **
C               **  IF IFENCE IS ON, THEN                    **
C               **  DEFINE THE VERTICAL LINE FROM            **
C               **  THE UPPER ADJACENT VALUE TO THE TOP OF   **
C               **  THE BOX                                  **
C               ***********************************************
C
        IF(IFENCE.EQ.'OFF')
     1    CALL DPCHLI(ICONT,NUMCPL,YMAX,Y75,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
        IF(IFENCE.EQ.'ON')
     1    CALL DPCHLI(ICONT,NUMCPL,YADJU,Y75,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
C
C               *******************************************************
C               **  STEP 6.9--                                       **
C               **  DEFINE THE VERTICAL LINE                         **
C               **  FROM THE TOP OF THE BOX (THE UPPER HINGE POINT)  **
C               **  TO THE POINT IN THE BOX TOWARD THE TOP           **
C               **  (SUCH AS THE UPPER CONFIDENCE LIMIT POINT)       **
C               *******************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y75,YUCL,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               **************************************************
C               **  STEP 6.10--                                 **
C               **  DEFINE THE VERTICAL LINE                    **
C               **  FROM THE POINT IN THE BOX TOWARD THE TOP    **
C               **  (SUCH AS THE UPPER CONFIDENCE LIMIT POINT)  **
C               **  TO THE POINT IN THE BOX                     **
C               **  IN THE MIDDLE                               **
C               **  (SUCH AS THE MEDIAN OR MEAN)                **
C               **************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YUCL,Y50,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               **************************************************
C               **  STEP 6.11--                                 **
C               **  DEFINE THE VERTICAL LINE                    **
C               **  FROM THE POINT IN THE BOX                   **
C               **  IN THE MIDDLE                               **
C               **  (SUCH AS THE MEDIAN OR MEAN)                **
C               **  TO THE POINT IN THE BOX TOWARD THE BOTTOM   **
C               **  (SUCH AS THE LOWER CONFIDENCE LIMIT POINT)  **
C               **************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y50,YLCL,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               *******************************************************
C               **  STEP 6.12--                                      **
C               **  DEFINE THE VERTICAL LINE                         **
C               **  FROM THE POINT IN THE BOX TOWARD THE BOTTOM      **
C               **  (SUCH AS THE LOWER CONFIDENCE LIMIT POINT)       **
C               **  TO THE BOTTOM OF THE BOX (THE LOWER HINGE POINT) **
C               *******************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YLCL,Y25,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               **********************************
C               **  STEP 6.13--                 **
C               **  IF IFENCE IS OFF, THEN      **
C               **  DEFINE THE VERTICAL LINE    **
C               **  FROM THE BOTTOM OF THE BOX  **
C               **  TO THE MINIMUM VALUE        **
C               **  IF IFENCE IS ON, THEN       **
C               **  DEFINE THE VERTICAL LINE    **
C               **  FROM THE BOTTOM OF THE BOX  **
C               **  TO THE LOWER ADJACENT VALUE **
C               **********************************
C
        IF(IFENCE.EQ.'OFF')
     1    CALL DPCHLI(ICONT,NUMCPL,Y25,YMIN,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
        IF(IFENCE.EQ.'ON')
     1    CALL DPCHLI(ICONT,NUMCPL,Y25,YADJL,XMID,XMID,J,JD,Y2,X2,D2,
     1    IERROR)
C
C               *********************************************
C               **  STEP 6.14--                            **
C               **  DEFINE THE VERTICAL LINE               **
C               **  CONSTITUTING THE LEFT SIDE OF THE BOX  **
C               **  WHICH GOES FROM THE TOP OF THE BOX     **
C               **  TO THE BOTTOM OF THE BOX               **
C               *********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y75,Y25,XLEFT,XLEFT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               **********************************************
C               **  STEP 6.15--                             **
C               **  DEFINE THE VERTICAL LINE                **
C               **  CONSTITUTING THE RIGHT SIDE OF THE BOX  **
C               **  WHICH GOES FROM THE TOP OF THE BOX      **
C               **  TO THE BOTTOM OF THE BOX                **
C               **********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y75,Y25,XRIGHT,XRIGHT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ***********************************************
C               **  STEP 6.16--                              **
C               **  DEFINE THE HORIZONTAL LINE               **
C               **  AT THE TOP OF THE BOX                    **
C               **  (RUNNING THROUGH THE UPPER HINGE POINT)  **
C               ***********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y75,Y75,XLEFT,XRIGHT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ****************************************************
C               **  STEP 6.17--                                   **
C               **  DEFINE THE HORIZONTAL LINE                    **
C               **  IN THE BOX                                    **
C               **  (RUNNING THROUGH THE UPPER CONFIDENCE LIMIT)  **
C               ****************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YUCL,YUCL,XLEFT,XRIGHT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               *********************************************
C               **  STEP 6.18--                            **
C               **  DEFINE THE HORIZONTAL LINE             **
C               **  IN THE BOX                             **
C               **  (RUNNING THROUGHT THE MEDIAN OR MEAN)  **
C               *********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y50,Y50,XLEFT,XRIGHT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ****************************************************
C               **  STEP 6.19--                                   **
C               **  DEFINE THE HORIZONTAL LINE                    **
C               **  IN THE BOX                                    **
C               **  (RUNNING THROUGH THE LOWER CONFIDENCE LIMIT)  **
C               ****************************************************
C
        CALL DPCHLI(ICONT,NUMCPL,YLCL,YLCL,XLEFT,XRIGHT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               ***********************************************
C               **  STEP 6.20--                              **
C               **  DEFINE THE HORIZONTAL LINE               **
C               **  AT THE BOTTOM OF THE BOX                 **
C               **  (RUNNING THROUGH THE LOWER HINGE POINT)  **
C               ***********************************************
C
        CALL DPCHLI(ICONT,NUMCPL,Y25,Y25,XLEFT,XRIGHT,J,JD,Y2,X2,D2,
     1              IERROR)
C
C               *******************************************************
C               **  STEP 6.20B--                                     **
C               **  IF A VIOLIN PLOT WITH NO FENCES HAS BEEN CALLED  **
C               **  FOR THEN SKIP PAST THE FINAL 4 SPECIFICATIONS.   **
C               *******************************************************
C
        IF(IFENCE.EQ.'OFF')GOTO1110
C
C               *************************************************
C               **  STEP 6.21--                                **
C               **  DEFINE THE CHARACTER FOR THE UPPER FAR OUT **
C               **  VALUES (BEYOND THE UPPER OUTER FENCE)      **
C               *************************************************
C
        YTEMP=Y25
        CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
        JD=JD-1
C
        IPASS=0
        DO1215I=1,NI
          IREV=NI-I+1
          YTEMP=TEMP(IREV)
          IF(YTEMP.LE.AOUTFU)GOTO1219
          IPASS=IPASS+1
          IF(IPASS.EQ.1)J=J-1
          CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1                IERROR)
          JD=JD-1
 1215   CONTINUE
 1219   CONTINUE
        JD=JD+1
C
C               *******************************************************
C               **  STEP 6.22--                                      **
C               **  DEFINE THE CHARACTER FOR THE UPPER NEAR OUT      **
C               **  VALUES (BETWEEN THE UPPER INNER AND OUTER FENCES)**
C               *******************************************************
C
        YTEMP=Y25
        CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
        JD=JD-1
C
        IPASS=0
        DO1225I=1,NI
          IREV=NI-I+1
          YTEMP=TEMP(IREV)
          IF(YTEMP.GE.AOUTFU)GOTO1225
          IF(YTEMP.LE.AINNFU)GOTO1229
          IPASS=IPASS+1
          IF(IPASS.EQ.1)J=J-1
          CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1                IERROR)
          JD=JD-1
 1225   CONTINUE
 1229   CONTINUE
        JD=JD+1
C
C               *******************************************************
C               **  STEP 6.23--                                      **
C               **  DEFINE THE CHARACTER FOR THE LOWER NEAR OUT      **
C               **  VALUES (BETWEEN THE LOWER INNER AND OUTER FENCES)**
C               *******************************************************
C
        YTEMP=Y25
        CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
        JD=JD-1
C
        IPASS=0
        DO1235I=1,NI
          I2=I
          YTEMP=TEMP(I2)
          IF(YTEMP.LE.AOUTFL)GOTO1235
          IF(YTEMP.GE.AINNFL)GOTO1239
          IPASS=IPASS+1
          IF(IPASS.EQ.1)J=J-1
          CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1                IERROR)
          JD=JD-1
 1235   CONTINUE
 1239   CONTINUE
        JD=JD+1
C
C               *************************************************
C               **  STEP 6.24--                                **
C               **  DEFINE THE CHARACTER FOR THE LOWER FAR OUT **
C               **  VALUES (BEYOND THE LOWER OUTER FENCE)      **
C               *************************************************
C
        YTEMP=Y25
        CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1              IERROR)
        JD=JD-1
C
        IPASS=0
        DO1245I=1,NI
          I2=I
          YTEMP=TEMP(I2)
          IF(YTEMP.GE.AOUTFL)GOTO1249
          IPASS=IPASS+1
          IF(IPASS.EQ.1)J=J-1
          CALL DPCHLI(ICONT,NUMCPL,YTEMP,YTEMP,XMID,XMID,J,JD,Y2,X2,D2,
     1                IERROR)
          JD=JD-1
 1245   CONTINUE
 1249   CONTINUE
        JD=JD+1
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'VIO2')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1251)
 1251     FORMAT('***** FROM THE MIDDLE OF DPVIO2--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1252)ANI,J,JD,XMID
 1252     FORMAT('ANI,J,JD,XMID = ',E15.7,I8,I8,E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1253)YMAX,Y75,Y50,Y25,YMIN
 1253     FORMAT('YMAX,Y75,Y50,Y25,YMIN = ',5E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1254)H,STEP,Y75,YADJU,AINNFU,AOUTFU
 1254     FORMAT('H,STEP,Y75,YADJU,AINNFU,AOUTFU = ',6E15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1255)H,STEP,Y25,YADJL,AINNFL,AOUTFL
 1255     FORMAT('H,STEP,Y25,YADJL,AINNFL,AOUTFL = ',6E15.7)
          CALL DPWRST('XXX','BUG ')
      ENDIF
C
 1110 CONTINUE
C
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
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'VIO2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVIO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,NUMSET,N2,IERROR
 9012   FORMAT('ICASPL,N,NUMSET,N2,IERROR = ',A4,3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFENCE
 9013   FORMAT('IFENCE = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NUMV2,ISIZE,SIZE,SIZE2,ISIZE2
 9014   FORMAT('NUMV2,ISIZE,SIZE,SIZE2,ISIZE2 = ',2I8,2E15.7,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)AN,XWIDT2,XWIDTH
 9015   FORMAT('AN,XWIDT2,XWIDTH = ',3E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)YMAX,Y75,Y50,Y25,YMIN
 9021   FORMAT('YMAX,Y75,Y50,Y25,YMIN = ',5E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)H,STEP,Y75,YADJU,AINNFU,AOUTFU
 9022   FORMAT('H,STEP,Y75,YADJU,AINNFU,AOUTFU = ',6E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)H,STEP,Y25,YADJL,AINNFL,AOUTFL
 9023   FORMAT('H,STEP,Y25,YADJL,AINNFL,AOUTFL = ',6E15.7)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,N2
          WRITE(ICOUT,9036)I,Y2(I),X2(I),D2(I)
 9036     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPVIS(IHARG,NUMARG,IVISSW,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE VISIBLE SWITCH IVISSW.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--IVISSW   ('ON'  OR 'OFF')
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISSWION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IVISSW
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
C
C-----COMMON----------------------------------------------------------
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
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1160
      GOTO1199
C
 1150 CONTINUE
      IVISSW='ON'
      GOTO1180
C
 1160 CONTINUE
      IVISSW='OFF'
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
 1181 FORMAT('THE VISIBLE SWITCH (AFFECTING BACKGROUND LINES ',
     1'IN 3-D PLOTS)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)IVISSW
 1182 FORMAT('           HAS JUST BEEN TURNED ',A4)
      CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPVLAB(IHARG,IHARG2,IARG,NUMARG,
     1                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,IVARLB,
     1                  IANS,IANSLC,IWIDTH,IBUGS2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE A VARIABLE LABEL FOR A VARIABLE.
C              THIS CAN BE USED IN SOME PLOTS (AND ITS USE WILL
C              PROBABLY BE EXTENDED IN THE FUTURE).
C              EXAMPLE--VARIABLE LABEL X1 PRESSURE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/11
C     ORIGINAL VERSION--NOVEMBER  1999.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IANS
      CHARACTER*4 IANSLC
      CHARACTER*40 IVARLB
      CHARACTER*4 IBUGS2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ILEFT
      CHARACTER*4 ILEFT2
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARG(*)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IN(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
C
      DIMENSION IANS(*)
      DIMENSION IANSLC(*)
      DIMENSION IVARLB(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
      ISUBN1='DPVL'
      ISUBN2='AB  '
      ILEFT='UNKN'
      ILEFT2='UNKN'
C
      ICOLL=0
      ILISTR=0
      ILISTL=0
C
C
C               *************************************
C               **  TREAT THE VARIABLE LABEL CASE  **
C               *************************************
C
C               *******************************************************
C               **  STEP 1--                                         **
C               **  CHECK THAT THE FIRST ARGUMENT IS LABEL.          **
C               **  THEN THE NEXT ARGUMENT SHOULD BE THE VARIABLE    **
C               **  NAME.                                            **
C               *******************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=2
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,
     1IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IHARG(1).NE.'LABE')THEN
        IFOUND='NO'
        GOTO9000
CCCCC ELSE
CCCCC   ISHIFT=1
CCCCC   CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
CCCCC1              IBUGS2,IERROR)
      ENDIF
C
      IF(NUMARG.LE.1)GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'ON')GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'OFF')GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'AUTO')GOTO110
      IF(NUMARG.EQ.2.AND.IHARG(2).EQ.'DEFA')GOTO110
      GOTO150
C
  110 CONTINUE
      DO120I=1,MAXNAM
      IVARLB(I)=' '
  120 CONTINUE
      ICOLL=-1
      GOTO8000
C
  150 CONTINUE
      ILEFT=IHARG(2)
      ILEFT2=IHARG2(2)
      ICOLL=IARG(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(ILEFT,ILEFT2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ICOLL=IVALUE(ILOCV)
C
      IF(NUMARG.EQ.3.AND.IHARG(3).EQ.'ON')GOTO157
      IF(NUMARG.EQ.3.AND.IHARG(3).EQ.'OFF')GOTO157
      IF(NUMARG.EQ.3.AND.IHARG(3).EQ.'AUTO')GOTO157
      IF(NUMARG.EQ.3.AND.IHARG(3).EQ.'DEFA')GOTO157
      IF(NUMARG.EQ.2)GOTO157
      GOTO159
  157 CONTINUE
      IVARLB(ICOLL)=' '
      GOTO8000
  159 CONTINUE
C
C               *****************************************************
C               **  STEP 3--                                       **
C               **  DETERMINE THE LOCATION OF THE WORD    LABEL  . **
C               *****************************************************
C
      DO160I=1,IWIDTH
        I2=I
        IP1=I+1
        IP2=I+2
        IP3=I+3
        IP4=I+4
        IF(IP4.GT.IWIDTH)GOTO169
        IF(IANS(I).EQ.'L'.AND.IANS(IP1).EQ.'A'.AND.
     1    IANS(IP2).EQ.'B'.AND.IANS(IP3).EQ.'E'.AND.
     1    (IANS(IP4).EQ.'L'.OR.IANS(IP4).EQ.' '))GOTO180
  160 CONTINUE
  169 CONTINUE
C
      WRITE(ICOUT,171)
  171 FORMAT('***** ERROR IN VARIABLE LABEL--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,172)
  172 FORMAT('      THE WORD     LABEL     NOT FOUND.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
  180 CONTINUE
      ISTOPL=IP4+1
C
C               *****************************************************
C               **  STEP 4--                                       **
C               **  SKIP THE NEXT WORD (THE VARIABLE NAME)         **
C               **  AND THEN FIND NEXT NON-BLANK CHARACTER         **
C               **  (THIS CHARACTER TILL END OF LINE EQUAL         **
C               **  = VARIABLE LABEL)                              **
C               *****************************************************
C
      ISTART=ISTOPL
      DO190I=ISTART,IWIDTH
        ISTOPL=I
        IF(IANSLC(I).EQ.' ')GOTO190
        GOTO191
  190 CONTINUE
      ISTOPL=IWIDTH+1
  191 CONTINUE
      IF(ISTOPL.GT.IWIDTH)THEN
        IVARLB(ICOLL)=' '
        GOTO8000
      ENDIF
      ISTART=ISTOPL
      DO195I=ISTART,IWIDTH
        ISTOPL=I
        IF(IANSLC(I).EQ.' ')GOTO196
  195 CONTINUE
      ISTOPL=IWIDTH+1
  196 CONTINUE
      IF(ISTOPL.GT.IWIDTH)THEN
        IVARLB(ICOLL)=' '
        GOTO8000
      ENDIF
      ISTART=ISTOPL
      DO198I=ISTART,IWIDTH
        IF(IANSLC(I).NE.' ')THEN
          ISTARS=I
          GOTO199
        ENDIF
  198 CONTINUE
      ISTARS=IWIDTH+1
  199 CONTINUE
      IF(ISTARS.GT.IWIDTH)THEN
        IVARLB(ICOLL)=' '
        GOTO8000
      ENDIF
C
      NCHAR=IWIDTH-ISTARS+1
      IF(NCHAR.GT.40)NCHAR=40
      IVARLB(ICOLL)=' '
      J=0
      DO250I=ISTARS,ISTARS+NCHAR-1
        J=J+1
        IVARLB(ICOLL)(J:J)=IANSLC(I)(1:1)
  250 CONTINUE
      GOTO8000
C
C               **********************************************
C               **  STEP 5--                                **
C               **  PRINT OUT A BRIEF MESSAGE               **
C               **  INDICATING THAT THE VARIABLE LABEL      **
C               **  HAS BEEN CARRIED OUT.                   **
C               **********************************************
C
 8000 CONTINUE
      ISTEPN='5'
      IF(IBUGS2.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(ICOLL.GE.1)THEN
          WRITE(ICOUT,611)ILEFT,ILEFT2
  611     FORMAT('VARIABLE ',A4,A4,' LABEL HAS JUST BEEN SET TO')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,613)IVARLB(ICOLL)
  613     FORMAT(A40)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,621)
  621     FORMAT('ALL VARIABLE LABELS HAVE JUST BEEN SET TO THERE ',
     1           'DEFAULT VALUES.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
C
      IF(IBUGS2.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVLAB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ILEFT,ILEFT2,ICOLL
 9012   FORMAT('ILEFT,ILEFT2,ICOLL = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO9030I=1,NUMNAM
          WRITE(ICOUT,9031)I,IUSE(I),IVALUE(I),IN(I)
 9031    FORMAT('I,IUSE(I),IVALUE(I),IN(I) = ',I8,2X,A4,2I8)
         CALL DPWRST('XXX','BUG ')
 9030   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPVRML(NPTS,NLAB,
     1                  AMEAN,ASD,N,
     1                  AMEAN2,ASD2,N2,XTEMP1,XTEMP2,
     1                  X,T,W,DTEMP1,DTEMP2,
     1                  XMLS,S2BMLS,SEML,SEMLK1,SEMLK2,
     1                  DLOWML,DHIGML,STXMU,STS2B,
     1                  SEMLBO,DLOWM2,DHIGM2,
     1                  IWRITE,
     1                  ICAPSW,ICAPTY,IOUNI5,NUMDIG,ISEED,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--IMPLEMENT VANGEL-RUKHIN APPROACH TO CONSENSUS MEANS
C     WRITTEN BY--CODE FOR VANGEL-RUKHIN PROVIDED BY MARK VANGEL.
C     PRINTING--YES
C     SUBROUTINES NEEDED--MPSUB
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/3
C     ORIGINAL VERSION--MARCH     2006. EXTRACTED FROM DPMAN2 ROUTINE
C     UPDATED         --FEBRUARY  2010. USE DPDTA1 TO PRINT
C     UPDATED         --OCTOBER   2011. SUPPORT PARAMETERIC BOOTSTRAP
C                                       OPTION
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ICAPS2
      CHARACTER*4 ICAPT2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ICASJB
      CHARACTER*4 IPRSAV
C
      REAL APPF
      REAL XML
      REAL XMLS
      REAL S2BML
      REAL S2BMLS
      REAL SEML
      REAL SEMLK1
      REAL SEMLK2
      REAL SEMLBO
      REAL XMLST
      REAL XMEAN
      REAL XSD
      REAL XMPST
      REAL STXMU
      REAL STS2B
      REAL ALPHA1
      REAL XPERC
      REAL AJUNK1
      REAL AJUNK2
      REAL AJUNK3
      REAL AJUNK4
      REAL AJUNK5
      REAL AJUNK6
C
C----------------------------------------------------------------
C
      REAL AMEAN(*)
      REAL ASD(*)
      REAL AMEAN2(*)
      REAL ASD2(*)
      REAL XTEMP1(*)
      REAL XTEMP2(*)
C
      INTEGER N(*)
      INTEGER N2(*)
C
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION T(*)
      DOUBLE PRECISION W(*)
      DOUBLE PRECISION DTEMP1(*)
      DOUBLE PRECISION DTEMP2(*)
C
      COMMON /MPCOM/ T0, T1
C
      PARAMETER (MAXROW=30)
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
C-----COMMON-----------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
      ISUBN1='DPVR'
      ISUBN2='ML  '
C
      IFLAGB=0
      IPRSAV=IPRINT
C
      IF (STS2B .GT. 0.D0) THEN
        DO 501 I=1,NLAB
          W(I) = STS2B/(STS2B +T(I))
 501    CONTINUE
      ELSE
        DO 507 I=1,NLAB
          W(I) = 1.0D0/T(I)
 507    CONTINUE
      END IF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VRML')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPVRML--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPTS,NLAB,STS2B,STXMU
   52   FORMAT('NPTS,NLAB,STS2B,STXMU = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IVRUCM,IVRBCM,T0,T1,NUMDIG
   55   FORMAT('IVRUCM,IVRBCM,T0,T1,NUMDIG = ',2(A4,2X),2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        DO65I=1,NLAB
          WRITE(ICOUT,66)I,T(I),X(I),W(I),N(I)
   66     FORMAT('I,T(I),X(I),W(I),N(I) = ',I8,3G15.7,I8)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
C
      MAXIT = 1000
      DXML   = STXMU
      DS2BML = STS2B
      CALL MPINTL(NLAB,N,X,T,DXML,DS2BML,W,MAXIT,DLIK,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      XML=REAL(DXML)
      S2BML=REAL(DS2BML)
      DXMLS=(T1-T0)*DXML + T0
      XMLS=REAL(DXMLS)
      D2BMLS=((T1-T0)**2)*DS2BML
      S2BMLS=REAL(D2BMLS)
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VRML')THEN
        WRITE(ICOUT,520)XML,XMLS,S2BML,S2BMLS
  520   FORMAT('XML,XMLS,S2BML,S2BMLS = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        DO522I=1,NLAB
          WRITE(ICOUT,526)I,T(I),W(I)
  526     FORMAT('I,T(I),W(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
  522   CONTINUE
      ENDIF
C
      DSUM1=0.0D0
      DSUM2=0.0D0
      WRITE(IOUNI5,506)
  506 FORMAT('VANGEL-RUKHIN:  W(I)     TAU(I)')
      DO509J=1,NLAB
        TAU=DS2BML/W(J) - DS2BML
        TAU=(T1-T0)**2*TAU + D2BMLS
        XJ=(T1-T0)*X(J) + T0
        DSUM1=DSUM1 +  (XJ-DXMLS)**2/(TAU**2)
        DSUM2=DSUM2 + 1.0D0/TAU
        WRITE(IOUNI5,508)W(J),TAU
  509 CONTINUE
  508 FORMAT(E15.7,1X,E15.7)
C
      STDERR=DSQRT(DSUM1)/DSUM2
      SEML=REAL(STDERR)
      SEMLK1=SEML
      SEMLK2=2.0*SEML
      CALL NORPPF(0.975,APPF)
      DLOWML=XMLS - DBLE(APPF)*STDERR
      DHIGML=XMLS + DBLE(APPF)*STDERR
C
C     2011/10: IMPLEMENT PARAMETERIC BOOTSTRAP TO OBTAIN STANDARD
C              ERROR FOR CONSENSUS MEAN ESTIMATE
C
      IF(IVRBCM.EQ.'ON')THEN
        IFLAGB=1
        ICASJB='BOOT'
        NRESAM=2000
        NUMDI9=-99
        ICNT=0
        DO1010IRESAM=1,NRESAM
          DO1030IROW=1,NLAB
            NTEMP=N(IROW)
            IF(NTEMP.LE.0)THEN
              IFLAGB=0
              GOTO1099
            ENDIF
            YMEAN=AMEAN(IROW)
            YSD=ASD(IROW)
            CALL NORRAN(NTEMP,ISEED,XTEMP1)
            DO1031IJ=1,NTEMP
              XTEMP1(IJ)=YMEAN + YSD*XTEMP1(IJ)
 1031       CONTINUE
            CALL MEAN(XTEMP1,NTEMP,IWRITE,XMEAN,IBUGA3,IERROR)
            CALL SD(XTEMP1,NTEMP,IWRITE,XSD,IBUGA3,IERROR)
            AMEAN2(IROW)=XMEAN
            ASD2(IROW)=XSD
            N2(IROW)=INT(REAL(NTEMP)+0.1)
 1030     CONTINUE
C
C         NOW COMPUTE THE VANGEL-RUKHIN ML ESTIMATE OF THE
C         CONSENSUS MEAN.
C
          T0=10000000.D0
          T1=-T0
C
          AMNX=CPUMAX
          AMXX=CPUMIN
          AMNSD=CPUMAX
          AMXSD=CPUMIN
C
          DO32250II=1,NLAB
C
            DTEMP1(II)=DBLE(AMEAN2(II))
            IF(DTEMP1(II).LT.T0) T0=DTEMP1(II)
            IF(DTEMP1(II).GT.T1) T1=DTEMP1(II)
            IF(AMEAN2(II).GT.AMXX)AMXX=AMEAN2(II)
            IF(AMEAN2(II).LT.AMNX)AMNX=AMEAN2(II)
C
            IF(N(I).LT.0)THEN
              DTEMP2(II)=DBLE(ASD2(II))**2
            ELSE
              DTEMP2(II)=DBLE(ASD2(II))**2/DBLE(N2(II))
            ENDIF
            IF(ASD2(II).GT.0.0)THEN
              IF(ASD2(II).LT.AMNSD)AMNSD=ASD2(II)
              IF(ASD2(II).GT.AMXSD)AMXSD=ASD2(II)
            ENDIF
C
32250     CONTINUE
C
          EPS=0.00001
          T0=AMNX - EPS
          T1=AMXX
          DO32270II=1,NLAB
            DTEMP1(II)=(DTEMP1(II)-T0)/(T1-T0)
            DTEMP2(II)=DTEMP2(II)/((T1-T0)**2)
32270     CONTINUE
C
          IPRINT='OFF'
          ICAPS2='OFF'
          ICAPT2='ASCI'
          CALL DPMNPL(AMEAN2,ASD2,XTEMP1,NPTS,NLAB,
     1                DTEMP1,DTEMP2,N2,
     1                XMPST,AJUNK1,AJUNK2,AJUNK3,AJUNK4,
     1                DJUNK1,DJUNK2,AJUNK5,AJUNK6,
     1                IWRITE,
     1                ICAPS2,ICAPT2,NUMDI9,
     1                ISUBRO,IBUGA3,IERROR)
          IPRINT=IPRSAV
          CALL DPVRM2(NPTS,NLAB,
     1                DTEMP1,DTEMP2,W,N2,
     1                XMLST,AJUNK5,AJUNK6,
     1                ISUBRO,IBUGA3,IERROR)
C
C         FOLLOWING TRICK IS TO TEST FOR "NaN", I.E., THE
C         ALGORITHM FAILED.
C
          IF(XMLST == XMLST) THEN
            ICNT=ICNT+1
            XTEMP2(ICNT)=XMLST
          ENDIF
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VRML')THEN
            WRITE(ICOUT,32271)IRESAM,ICNT,XMLST
32271      FORMAT('IRESAM,ICNT,XMLST = ',2I8,G15.7)
           CALL DPWRST('XXX','BUG ')
          ENDIF
C
 1010   CONTINUE
        NRESAM=ICNT
        CALL SD(XTEMP2,NRESAM,IWRITE,XSD,IBUGA3,IERROR)
        SEMLBO=XSD
        SEMLB2=2.0*XSD
        ALPHA1=100.0*0.025
        CALL PERCEN(ALPHA1,XTEMP2,NRESAM,IWRITE,XTEMP1,NRESAM,
     1              XPERC,IBUGA3,IERROR)
        DLOWM2=DBLE(XPERC)
        ALPHA1=100.0*0.975
        CALL PERCEN(ALPHA1,XTEMP2,NRESAM,IWRITE,XTEMP1,NRESAM,
     1              XPERC,IBUGA3,IERROR)
        DHIGM2=DBLE(XPERC)
      ENDIF
C
 1099 CONTINUE
C
      IF(IPRINT.EQ.'OFF')GOTO9000
      IF(IVRUCM.EQ.'OFF')GOTO4009
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
C
      ICNT=1
      ITEXT(ICNT)=' 3. Method: Vangel-Rukhin Maximum Likelihood'
      NCTEXT(ICNT)=44
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of (unscaled) Consensus Mean:'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=XMLS
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Estimate of (scaled) Consensus Mean:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=XML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Between Lab Variance (unscaled):'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=S2BMLS
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Between Lab SD (unscaled):'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=SQRT(S2BMLS)
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Between Lab Variance (scaled):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=S2BML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Standard Deviation of Consensus Mean:'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=SEML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=SEML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=2.0*SEML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Expanded Uncertainty (k =           ):'
      WRITE(ITEXT(ICNT)(31:40),'(F10.7)')APPF
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=APPF*SEML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Normal PPF of 0.975:'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=APPF
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Lower 95% (normal) Confidence Limit:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=DLOWML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Upper 95% (normal) Confidence Limit:'
      NCTEXT(ICNT)=40
      AVALUE(ICNT)=DHIGML
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='    Note: Vangel-Rukhin Maximum Likelihood'
      NCTEXT(ICNT)=42
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='          Best Usage: 6 or More Labs'
      NCTEXT(ICNT)=36
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
C     PRINT EXPLICIT WARNING IF BETWEEN LAB VARIANCE IS TO
C     SMALL (SAY ON THE ORDER OF 1E-05 OR LESS)
C
      EPS=1.0E-05
      IF(S2BML.LE.EPS)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=0
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='    WARNING: ESTIMATED BETWEEN LAB VARIANCE'
        NCTEXT(ICNT)=43
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='             IS LESS THAN 0.00001.  THE'
        NCTEXT(ICNT)=39
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='             ESTIMATED STANDARD ERROR OF THE'
        NCTEXT(ICNT)=44
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='             CONSENSUS MEAN MAY BE SUSPECT.'
        NCTEXT(ICNT)=45
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
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
C
      ITITLE=' '
      NCTITL=0
      ITITLZ=' '
      NCTITZ=0
      ITITL9=' '
      NCTIT9=0
C
 4009 CONTINUE
C
      IF(IVRBCM.EQ.'OFF' .OR. IFLAGB.EQ.0)GOTO9000
C
      ICNT=1
      ITEXT(ICNT)=' 3b. Method: Vangel-Rukhin Maximum Likelihood'
      NCTEXT(ICNT)=45
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='     with Parameteric Bootstrap Variance Estimate'
      NCTEXT(ICNT)=49
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='     Number of Bootstrap Samples:'
      NCTEXT(ICNT)=33
      AVALUE(ICNT)=REAL(NRESAM)
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Estimate of (unscaled) Consensus Mean:'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=XMLS
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Standard Uncertainty (k = 1):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=SEMLBO
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Expanded Uncertainty (k = 2):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=2.0*SEMLBO
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Lower 95% (normal) Confidence Limit:'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=DLOWM2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Upper 95% (normal) Confidence Limit:'
      NCTEXT(ICNT)=41
      AVALUE(ICNT)=DHIGM2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='     Note: Vangel-Rukhin Maximum Likelihood'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='           Best Usage: 6 or More Labs'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      NUMROW=ICNT
      DO320I=1,NUMROW
        NTOT(I)=15
  320 CONTINUE
C
      IFRST=.TRUE.
      ILAST=.TRUE.
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
CCCCC FOLLOWING CODE IS STILL EXPERIMENTAL BY MARK VANGEL.
CCCCC WE PRINT COPY OF TAU (CAN BE COMPARED TO T(I), BUT RES
CCCCC IS STILL UNDER DEVELOPMENT.
CCCCC DO320I=1,NLAB
CCCCC   A   =S2BML/(X(I) -XML)**2
CCCCC   B   =T(I)/(X(I)-XML)**2
CCCCC   D   =(X(I)-XML)**2
CCCCC   TAU =S2BML/W(I) - S2BML
CCCCC   RES = (DBLE(N(I)-1)/TAU) *(1.0D0 - T(I)/TAU)
CCCCC   WRITE TO IOUNI2?
CCCCC   WRITE (IOUNI1,322) X(I),T(I),TAU,(X(I)-XML)/(S2BML+TAU),RES
C322    FORMAT(5G15.7)
C320  CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VRML')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVRML--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPTS,NLAB
 9013   FORMAT('NPTS,NLAB = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)XMLS,S2BMLS,SEML
 9014   FORMAT('XMLS,S2BMLS,SEML = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)DLOWML,DHIGML
 9015   FORMAT('DLOWML,DHIGML = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPVRM2(NPTS,NLAB,
     1                  X,T,W,N,
     1                  XMLS,STXMU,STS2B,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--IMPLEMENT VANGEL-RUKHIN APPROACH TO CONSENSUS MEANS
C     WRITTEN BY--CODE FOR VANGEL-RUKHIN PROVIDED BY MARK VANGEL.
C     PRINTING--YES
C     SUBROUTINES NEEDED--MPSUB
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 Gaithersburg, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2006/3
C     ORIGINAL VERSION--MARCH     2006. EXTRACTED FROM DPMAN2 ROUTINE
C     UPDATED         --FEBRUARY  2010. USE DPDTA1 TO PRINT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES--------------
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      REAL STXMU
      REAL STS2B
      REAL XML
      REAL XMLS
C
C----------------------------------------------------------------
C
      INTEGER N(*)
C
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION T(*)
      DOUBLE PRECISION W(*)
C
      COMMON /MPCOM/ T0, T1
C
C-----COMMON-----------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT------------------------------------------------
C
      IERROR='NO'
C
      IF (STS2B .GT. 0.D0) THEN
        DO 501 I=1,NLAB
          W(I) = STS2B/(STS2B +T(I))
 501    CONTINUE
      ELSE
        DO 507 I=1,NLAB
          W(I) = 1.0D0/T(I)
 507    CONTINUE
      END IF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VRM2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPVRM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPTS,NLAB,STS2B,STXMU
   52   FORMAT('NPTS,NLAB,STS2B,STXMU = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)T0,T1,NUMDIG
   55   FORMAT('T0,T1,NUMDIG = ',2G15.7,I8)
        CALL DPWRST('XXX','BUG ')
        DO65I=1,NLAB
          WRITE(ICOUT,66)I,T(I),X(I),W(I),N(I)
   66     FORMAT('I,T(I),X(I),W(I),N(I) = ',I8,3G15.7,I8)
          CALL DPWRST('XXX','BUG ')
   65   CONTINUE
      ENDIF
C
      MAXIT = 1000
      DXML   = STXMU
      DS2BML = STS2B
      CALL MPINTL(NLAB,N,X,T,DXML,DS2BML,W,MAXIT,DLIK,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      XML=REAL(DXML)
      DXMLS=(T1-T0)*DXML + T0
      XMLS=REAL(DXMLS)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VRM2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVRM2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,NPTS,NLAB,XMLS
 9012   FORMAT('IERROR,NPTS,NLAB,XMLS = ',A4,2X,2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPVWAE(YTEMP,XTEMP,MAXNXT,
     1                  ICAPSW,IFORSW,IMULT,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT VAN DER WAERDEN TEST
C              NON-PARAMETRIC ONE-WAY ANOVA
C     EXAMPLE--VAN DER WAERDEN TEST Y X
C     REFERENCE--W. J, CONOVER, "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, 1999, PP. 396-406.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/10
C     ORIGINAL VERSION--OCTOBER   2004.
C     UPDATED         --JANUARY   2007. CALL LIST TO RANK
C     UPDATED         --FEBRUARY  2011. USE DPPARS
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "MULTIPLE" CASE
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
      CHARACTER*4 ICASE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHOST1
      CHARACTER*4 ISUBN0
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
      DIMENSION YTEMP(*)
      DIMENSION XTEMP(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION DTAG(MAXOBV)
      DIMENSION ARANK(MAXOBV)
      DIMENSION ANORM(MAXOBV)
      DIMENSION NRANK(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
      DIMENSION TEMP5(MAXOBV)
      DIMENSION TEMP6(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE(GARBAG(IGARB1),DTAG(1))
      EQUIVALENCE(GARBAG(IGARB2),ARANK(1))
      EQUIVALENCE(GARBAG(IGARB3),ANORM(1))
      EQUIVALENCE(GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE(GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE(GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE(GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE(GARBAG(IGARB8),TEMP4(1))
      EQUIVALENCE(GARBAG(IGARB9),TEMP5(1))
      EQUIVALENCE(GARBAG(IGAR10),TEMP6(1))
C
      INCLUDE 'DPCOZI.INC'
      EQUIVALENCE(IGARBG(IIGAR1),NRANK(1))
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPVW'
      ISUBN2='AE  '
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
C               *******************************************
C               **  TREAT THE VAN DER WAERDEN TEST CASE  **
C               *******************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'VWAE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPVWAE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   52   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IMULT,IKRUGS,MAXNXT
   55   FORMAT('IMULT,IKRUGS,MAXNXT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************
C               **  STEP 1--                   **
C               **  EXTRACT THE VARIABLE LIST  **
C               *********************************
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='VAN DER WAERDEN TEST'
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')THEN
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
C               **  GENERATE THE VAN DER WAERDEN TEST FOR THE        **
C               **  VARIOUS CASES                                    **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')
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
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')
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
C               ******************************************************
C               **  STEP 3B--
C               **  PREPARE FOR ENTRANCE INTO DPVWA2--
C               ******************************************************
C
        ISTEPN='3B'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,331)
  331     FORMAT('***** FROM DPVWAE, AS WE ARE ABOUT TO CALL DPVWA2--')
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
      CALL DPVWA2(Y,X,NLOCAL,IVARN1,IVARN2,
     1            YTEMP,DTAG,XTEMP,ANORM,ARANK,NRANK,XTEMP2,MAXNXT,
     1            TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,
     1            STATVA,STATCD,PVAL,
     1            CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,
     1            ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,
     1            IBUGA3,ISUBRO,IERROR)
C
C               *******************************************************
C               **  STEP 4A--                                        **
C               **  CASE 2: MULTIPLE RESPONSE VARIABLES.  NOTE THAT  **
C               **          LEVENE TEST, THE MULTIPLE LABS ARE       **
C               **          CONVERTED INTO A "Y X" STACKED PAIR      **
C               **          WHERE "X" IS THE LAB-ID VARIABLE.        **
C               *******************************************************
C
      ELSEIF(IMULT.EQ.'ON')THEN
        ISTEPN='4A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')
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
     1              XTEMP,Y,X,NLOCAL,ICASE,
     1              IBUGA3,ISUBRO,IFOUND,IERROR)
        NUMVAR=2
        IF(IERROR.EQ.'YES')GOTO9000
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'VWAE')THEN
          ISTEPN='4B'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,442)
  442     FORMAT('***** FROM THE MIDDLE  OF DPKRUS--')
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
        CALL DPVWA2(Y,X,NS1,IVARN1,IVARN2,
     1              YTEMP,DTAG,XTEMP,ANORM,ARANK,NRANK,XTEMP2,MAXNXT,
     1              TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,
     1              STATVA,STATCD,PVAL,
     1              CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,CUT99,CUT999,
     1              ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,
     1              IBUGA3,ISUBRO,IERROR)
C
      ENDIF
C
C               ***************************************
C               **  STEP 61--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='61'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'VWAE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISUBN0='DPVW'
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
      VALUE0=STATCD
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
      IH='CUTO'
      IH2='FF0 '
      VALUE0=CUT0
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='FF50'
      VALUE0=CUT50
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='FF75'
      VALUE0=CUT75
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='FF90'
      VALUE0=CUT90
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='FF95'
      VALUE0=CUT95
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='F975'
      VALUE0=CUT975
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='FF99'
      VALUE0=CUT99
      CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1IANS,IWIDTH,IBUGA3,IERROR)
C
      IH='CUTO'
      IH2='F999'
      VALUE0=CUT99
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'VWAE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVWAE--')
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
      SUBROUTINE DPVWA2(Y,TAG,N,IVARID,IVARI2,
     1                  YTEMP,DTAG,AMEAN,ANORM,ARANK,NRANK,
     1                  XTEMP2,MAXNXT,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,
     1                  STATVA,STATCD,PVAL,
     1                  CUT0,CUT50,CUT75,CUT90,CUT95,CUT975,
     1                  CUT99,CUT999,
     1                  ICAPSW,ICAPTY,IFORSW,IMULT,IKRUGS,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT VAN DER WAERDEN (NORMAL SCORE
C              TEST) FOR SEVERAL INDEPENDENT VARIABLES, I.E., 
C              A NON-PARAMETRIC ONE-WAY ANOVA
C     EXAMPLE--VAN DER WAERDEN TEST Y TAG
C     REFERENCE--W. J. CONOVER, "PRACTICAL NONPARAMETRIC
C                STATISTICS", THIRD EDITION, 1999, WILEY,
C                PP. 396-405.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/10
C     ORIGINAL VERSION--OCTOBER   2004.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C     UPDATED         --JANUARY   2007. CALL LIST TO RANK
C     UPDATED         --FEBRUARY  2011. USE DPDTA1 AND DPDTA4 TO PRINT
C                                       OUTPUT TABLES.  THIS ADDS RTF
C                                       SUPPORT AND SPECIFICATION OF
C                                       THE NUMBER OF DIGITS.
C     UPDATED         --FEBRUARY  2011. OPTION TO PRINT GROUP
C                                       STATISTICS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IMULT
      CHARACTER*4 IKRUGS
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
C
      CHARACTER*4 IWRITE
C
      CHARACTER*3 IATEMP
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IOP
C
      DOUBLE PRECISION DSUM1
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TAG(*)
      DIMENSION DTAG(*)
      DIMENSION ANORM(*)
      DIMENSION YTEMP(*)
      DIMENSION AMEAN(*)
      DIMENSION ARANK(*)
      DIMENSION NRANK(*)
      DIMENSION XTEMP2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
      DIMENSION TEMP5(*)
      DIMENSION TEMP6(*)
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA/
     1 0.0, 50.0, 75.0, 90.0, 95.0, 97.5, 99.0, 99.9/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPVW'
      ISUBN2='A2  '
      ISUBN0='    '
C
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VWA2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPVWA2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',A4,2X,A4,2X,I8)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.4)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN VAN DER WAERDEN TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE NUMBER OF OBSERVATIONS IS LESS THAN 4.')
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
      HOLD=TAG(1)
      DO235I=2,N
      IF(TAG(I).NE.HOLD)GOTO239
  235 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,111)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,231)HOLD
  231 FORMAT('      THE GROUP-ID VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  239 CONTINUE
C
C               *******************************
C               **  STEP 41--                **
C               **  CARRY OUT CALCULATIONS   **
C               **  FOR VAN DER WAERDEN TEST **
C               *******************************
C
      ISTEPN='41'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
C
CCCCC THE ALGORITHM FOR VAN DER WAERDEN TEST IS:
CCCCC
CCCCC   1) RANK ALL OBSERVATIONS (R)
CCCCC   2) COMPUTE: A = NORPPF(RANK/(N+1))
CCCCC   3) COMPUTE MEAN OF A FOR EACH GROUP (Abar(i))
CCCCC   4) COMPUTE VARIACE FOR FULL SAMPLE (S**2)
CCCCC   5) TEST STATISTIC IS:
CCCCC
CCCCC      T = (1/S**2)*SUM[i=1 to k][N(i)*(Abar(i)**2]
CCCCC
CCCCC THE CRITICAL VALUE IS A CHI-SQUARED DISTRIBUTION WITH
CCCCC (K-1) DEGREES OF FREEDOM
CCCCC
C
      CALL DISTIN(TAG,N,IWRITE,DTAG,NUMDIS,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
CCCCC CALL SORTC(TAG,Y,N,TAG,Y)
      CALL RANK(Y,N,IWRITE,ARANK,XTEMP2,MAXNXT,IBUGA3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      DO420I=1,N
        ATEMP=ARANK(I)/REAL(N+1)
        CALL NORPPF(ATEMP,ANORM(I))
  420 CONTINUE
      CALL VAR(ANORM,N,IWRITE,S2,IBUGA3,IERROR)
C
      AN=REAL(N)
C
      DSUM1=0.0D0
      DO460IDIS=1,NUMDIS
         J=0
         DO470I=1,N
            IF(TAG(I).EQ.DTAG(IDIS))THEN
               J=J+1
               YTEMP(J)=ANORM(I)
               TEMP5(J)=Y(I)
            ENDIF
  470    CONTINUE
         CALL MEDIAN(TEMP5,J,IWRITE,TEMP6,MAXNXT,YMED,
     1               IBUGA3,IERROR)
         CALL MEAN(TEMP5,J,IWRITE,YMEANT,IBUGA3,IERROR)
         CALL SD(TEMP5,J,IWRITE,YSD,IBUGA3,IERROR)
         TEMP1(IDIS)=J
         TEMP2(IDIS)=YMEANT
         TEMP3(IDIS)=YMED
         TEMP4(IDIS)=YSD
         NRANK(IDIS)=J
         ANR=REAL(NRANK(IDIS))
         CALL MEAN(YTEMP,NRANK(IDIS),IWRITE,YMEAN,IBUGA3,IERROR)
         AMEAN(IDIS)=YMEAN
         DSUM1=DSUM1 + DBLE(NRANK(IDIS))*DBLE(YMEAN)**2
  460 CONTINUE
C
      STATVA=REAL(DSUM1/DBLE(S2))
      NUMDF=NUMDIS-1
      CALL CHSCDF(STATVA,NUMDF,STATCD)
      PVAL=1.0 - STATCD
C
      CUT0=0.0
      CALL CHSPPF(.50,NUMDF,CUT50)
      CALL CHSPPF(.75,NUMDF,CUT75)
      CALL CHSPPF(.90,NUMDF,CUT90)
      CALL CHSPPF(.95,NUMDF,CUT95)
      CALL CHSPPF(.975,NUMDF,CUT975)
      CALL CHSPPF(.99,NUMDF,CUT99)
      CALL CHSPPF(.999,NUMDF,CUT999)
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
      WRITE(IOUNI1,501)
  501 FORMAT('     I       J  ',
     1       '|Abar(i) - Abar(j)|  ',
     1       '90% CV        ',
     1       '95% CV        ',
     1       '99% CV        ')
C
      IDF=N-NUMDIS
      ALPHAT=0.05
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT95)
      ALPHAT=0.10
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT90)
      ALPHAT=0.01
      CALL TPPF(1.0-ALPHAT/2.0,REAL(IDF),AT99)
      AFACT2=SQRT(S2*(AN-1.0-STATVA)/REAL(N-NUMDIS))
C
      DO530I=1,NUMDIS
        DO539J=1,NUMDIS
          IF(I.LT.J)THEN
            ANI=REAL(NRANK(I))
            ANJ=REAL(NRANK(J))
            ADIFF=ABS(AMEAN(I) - AMEAN(J))
            AFACT3=SQRT((1.0/ANI) + (1.0/ANJ))
            ACV90=AT90*AFACT2*AFACT3
            ACV95=AT95*AFACT2*AFACT3
            ACV99=AT99*AFACT2*AFACT3
            IATEMP='   '
            IF(ADIFF.GE.ACV90)IATEMP(1:1)='*'
            IF(ADIFF.GE.ACV95)IATEMP(2:2)='*'
            IF(ADIFF.GE.ACV99)IATEMP(3:3)='*'
            WRITE(IOUNI1,537)I,J,ADIFF,ACV90,ACV95,ACV99,IATEMP
  537       FORMAT(I6,2X,I6,2X,4E15.7,A3)
          ENDIF
  539   CONTINUE
  530 CONTINUE
C
      DO590I=1,N
        WRITE(IOUNI2,597)I,Y(I),ARANK(I),ANORM(I),AMEAN(I),NRANK(I)
  597   FORMAT(I8,4E15.7,I8)
  590 CONTINUE
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLG1,IFLG2,IFLG3,IFLG4,IFLG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               ********************************
C               **   STEP 42--                **
C               **   WRITE OUT EVERYTHING     **
C               **   FOR VAN DER WAERDEN TEST **
C               ********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
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
      ITITLE='Van Der Waerden (Normal Scores) One Factor Test'
      NCTITL=47
      ITITLZ=' '
      NCTITZ=0
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
C     IF REQUESTED, PRINT OUT GROUP INFORMATION.  SINCE NUMBER
C     OF GROUPS IS UNKNOWN (AND POTENTIALLY LARGE, PRINT EACH
C     GROUP AS A SEPARATE TABLE.
C
      IF(IKRUGS.EQ.'ON')THEN
C
        DO2160I=1,NUMDIS
C
          NUMROW=ICNT
          DO2165II=1,NUMROW
            NTOT(II)=15
 2165     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
C
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGA3,IERROR)
          ICNT=0
          ITITLE=' '
          NCTITL=0
          ITITLZ=' '
          NCTITZ=0
C
          ICNT=ICNT+1
          ITEXT(ICNT)=' '
          NCTEXT(ICNT)=1
          AVALUE(ICNT)=0.0
          IDIGIT(ICNT)=-1
C
          IF(IMULT.EQ.'ON')THEN
            ICNT=ICNT+1
            ITEXT(ICNT)='Group Variable: '
            WRITE(ITEXT(ICNT)(17:20),'(A4)')IVARID(I)(1:4)
            WRITE(ITEXT(ICNT)(21:24),'(A4)')IVARI2(I)(1:4)
            NCTEXT(ICNT)=24
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ELSE
            ICNT=ICNT+1
            ITEXT(ICNT)='Group    '
            WRITE(ITEXT(ICNT)(7:9),'(I3)')I
            NCTEXT(ICNT)=9
            AVALUE(ICNT)=0.0
            IDIGIT(ICNT)=-1
          ENDIF
          ICNT=ICNT+1
          ITEXT(ICNT)='Number of Observations:'
          NCTEXT(ICNT)=23
          AVALUE(ICNT)=TEMP1(I)
          IDIGIT(ICNT)=0
          ICNT=ICNT+1
          ITEXT(ICNT)='Mean:'
          NCTEXT(ICNT)=5
          AVALUE(ICNT)=TEMP2(I)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Median:'
          NCTEXT(ICNT)=7
          AVALUE(ICNT)=TEMP3(I)
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='SD:'
          NCTEXT(ICNT)=3
          AVALUE(ICNT)=TEMP4(I)
          IDIGIT(ICNT)=NUMDIG
 2160   CONTINUE
C
        IF(ICNT.GT.0)THEN
          NUMROW=ICNT
          DO2168II=1,NUMROW
            NTOT(II)=15
 2168     CONTINUE
C
          IFRST=.TRUE.
          ILAST=.TRUE.
C
          CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1                AVALUE,IDIGIT,
     1                NTOT,NUMROW,
     1                ICAPSW,ICAPTY,ILAST,IFRST,
     1                ISUBRO,IBUGA3,IERROR)
          ICNT=0
        ENDIF
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='H0: Samples Come From Identical Populations'
      NCTEXT(ICNT)=43
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: Samples Do Not Come From Identical Populations'
      NCTEXT(ICNT)=50
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
      ITEXT(ICNT)='Variance of Normal Scores of Ranks'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=S2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Van Der Waerden Test Statistic Value:'
      NCTEXT(ICNT)=37
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
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
     1'Percent Points of the Chi-Square Reference Distribution'
      NCTITL=55
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'VWA2')
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
      ITITLE(1:26)='Multiple Comparisons Table'
      NCTITL=26
      ITITL9=' '
      NCTIT9=0
C
      ITITL2(1,1)='I'
      NCTIT2(1,1)=1
      ITITL2(1,2)='J'
      NCTIT2(1,2)=1
      ITITL2(1,3)='|Abar(i)-Abar(j)|'
      NCTIT2(1,3)=15
      ITITL2(1,4)='90% CV'
      NCTIT2(1,4)=6
      ITITL2(1,5)='95% CV'
      NCTIT2(1,5)=6
      ITITL2(1,6)='99% CV'
      NCTIT2(1,6)=6
C
      NMAX=0
      NUMCOL=6
      DO4010I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        NTOT(I)=15
        IF(I.EQ.1 .OR. I.EQ.2)THEN
          NTOT(I)=5
          IDIGIT(I)=0
        ELSEIF(I.EQ.3)THEN
          NTOT(I)=20
        ENDIF
        NMAX=NMAX+NTOT(I)
 4010 CONTINUE
      IWHTML(1)=50
      IWHTML(2)=50
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IINC=1600
      IINC2=200
      IINC3=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC2
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(3)+IINC
      IWRTF(5)=IWRTF(4)+IINC
      IWRTF(6)=IWRTF(5)+IINC
C
      ICNT=0
      AFACT2=SQRT(S2*(AN-1.0-STATVA)/REAL(N-NUMDIS))
      DO4081I=1,NUMDIS
        DO4083J=1,NUMDIS
          IF(I.LT.J)THEN
C
            ANI=REAL(NRANK(I))
            ANJ=REAL(NRANK(J))
            ADIFF=ABS(AMEAN(I) - AMEAN(J))
            AFACT3=SQRT((1.0/ANI) + (1.0/ANJ))
            ACV90=AT90*AFACT2*AFACT3
            ACV95=AT95*AFACT2*AFACT3
            ACV99=AT99*AFACT2*AFACT3
C
            IF(ICNT.GE.MAXROW)THEN
              NUMLIN=1
              IFRST=.TRUE.
              ILAST=.TRUE.
              IFLAGS=.TRUE.
              IFLAGE=.TRUE.
              CALL DPDTA5(ITITLE,NCTITL,
     1                    ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                    MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                    IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                    IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                    ICAPSW,ICAPTY,IFRST,ILAST,
     1                    IFLAGS,IFLAGE,
     1                    ISUBRO,IBUGA3,IERROR)
              ICNT=0
            ENDIF
C
            ICNT=ICNT+1
            IVALUE(ICNT,1)=' '
            NCVALU(ICNT,1)=0
            AMAT(ICNT,1)=REAL(I)
            IVALUE(ICNT,2)=' '
            NCVALU(ICNT,2)=0
            AMAT(ICNT,2)=REAL(J)
            IVALUE(ICNT,3)=' '
            NCVALU(ICNT,3)=0
            AMAT(ICNT,3)=ADIFF
            IVALUE(ICNT,4)=' '
            NCVALU(ICNT,4)=0
            AMAT(ICNT,4)=ACV90
            IVALUE(ICNT,5)=' '
            NCVALU(ICNT,5)=0
            AMAT(ICNT,5)=ACV95
            IVALUE(ICNT,6)=' '
            NCVALU(ICNT,6)=0
            AMAT(ICNT,6)=ACV99
          ENDIF
 4083   CONTINUE
 4081 CONTINUE
C
      IF(ICNT.GE.1)THEN
        NUMLIN=1
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
       ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VWA2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPVWA2--')
        CALL DPWRST('XXX','WRIT')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I),TAG(I),ARANK(I),ANORM(I)
 9017     FORMAT('I,TAG(I),ARANK(I),ANORM(I) = ',I8,4G15.7)
          CALL DPWRST('XXX','WRIT')
 9016   CONTINUE
        DO9026I=1,NUMDIS
          WRITE(ICOUT,9027)I,AMEAN(I),NRANK(I)
 9027     FORMAT('I,AMEAN(I),NRANK(I) = ',I8,G15.7,I8)
          CALL DPWRST('XXX','WRIT')
 9026   CONTINUE
        WRITE(ICOUT,9031)STAVA,STATCD
 9031   FORMAT('STATVA,STATCD = ',2G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWCCP(ICASPL,
     1                  YLOWER,YUPPER,IOUT,KMAXM1,PEROUT,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--WRITE CONCLUSIONS
C              FROM CORRELATION PLOT COMMANDS--
C                 1) AUTOCORRELATION PLOT
C                 2) CROSS-CORRELATION PLOT
C                 3) PARTIAL AUTOCORRELATION PLOT
C              OUT TO A FILE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JUNE      1982.
C     UPDATED         --FEBRUARY  1989.  FORMATS DUE TO 2X NOS LOWER CASE CHAR
C     UPDATED         --NOVEMBER  1989.  FIX IOUNIT=0 BUG (NELSON)
C     UPDATED         --FEBRUARY  1993.  PARTIAL AUTOCORRELATION PLOT
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
CCCCC CHARACTER*4 IENDFI
CCCCC CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
      CHARACTER*4 ICONFL
      COMMON/ICONCO/ICONFL
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWC'
      ISUBN2='CP  '
      IERROR='NO'
C
CCCCC THE FOLLOWING LINE WAS FIXED NOVEMBER 1989
CCCCC IF(IBUGS2.EQ.'OFF'.AND.ISUBRO.NE.'WCCP')GOTO1199
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'WCCP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWCCP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,YLOWER,YUPPER,PEROUT
   52   FORMAT('ICASPL,YLOWER,YUPPER,PEROUT = ',A4,2X,3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IOUT,KMAXM1,ICONNU
   54   FORMAT('IOUT,KMAXM1,ICONNU = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUGS2,IFOUND,IERROR
   59   FORMAT('IBUGS2,IFOUND,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)ICONNA(1:80)
   62   FORMAT('ICONNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ICONST,ICONFO,ICONAC,ICONFO,ICONCS
   63   FORMAT('ICONST,ICONFO,ICONAC,ICONFO,ICONCS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCCP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICONFL.EQ.'CLOS')THEN
        CALL DPOPF0(ICONNU,IBUGS2,ISUBRO,IERROR)
        ICONFL='OPEN'
      ENDIF
C
      IOUNIT=ICONNU
      IFILE=ICONNA
      ISTAT=ICONST
      IFORM=ICONFO
      IACCES=ICONAC
      IPROT=ICONPR
      ICURST=ICONCS
C
      ISUBN0='WCCP'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'WCCP')THEN
        WRITE(ICOUT,1193)IOUNIT
 1193   FORMAT('IOUNIT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1194)IFILE(1:80)
 1194   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1196)ISUBN0,IERRFI
 1196   FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  CHECK TO SEE IF CONCLUSIONS FILE MAY EXIST  **
C               **************************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCCP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPWCCP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE CONCLUSIONS CANNOT BE SENT TO FILE BECAUSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      THE REQUIRED SYSTEM MASS STORAGE FILE WHICH ',
     1         'STORES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)
 1216   FORMAT('      SUCH CONCLUSIONS IS NOT AVAILABLE AT THIS ',
     1         'INSTALLATION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,ICONST
 1217   FORMAT('ISTAT,ICONST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ******************************************
C               **  STEP 30--                           **
C               **  BRANCH TO THE APPROPRIATE CASE      **
C               **  AND WRITE OUT          CONCLUSIONS  **
C               ******************************************
C
      ISTEPN='30'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCCP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASPL.EQ.'AUCO')GOTO3100
      IF(ICASPL.EQ.'CRCO')GOTO4100
CCCCC THE FOLLOWING LINE WAS ADDED FEBRUARY 1993
      IF(ICASPL.EQ.'PACO')GOTO5100
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3011)
 3011 FORMAT('***** INTERNAL ERROR IN DPWCCP ',
     1'AT BRANCH POINT 3011--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3012)
 3012 FORMAT('      ICASPL SHOULD BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3013)
 3013 FORMAT('      AUCO, CRCO, OR PACO, BUT IS NOT.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3014)ICASPL
 3014 FORMAT('      ICASPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *********************************************
C               **  STEP 31--                            **
C               **  WRITE OUT          CONCLUSIONS       **
C               **  FOR AUTOCORRELATION PLOT ANALYSIS    **
C               *******************************************
C
 3100 CONTINUE
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCCP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGS2.EQ.'ON')WRITE(ICOUT,3101)ICONNU
 3101 FORMAT('ICONNU = ',I8)
      IF(IBUGS2.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3111)
 3111 FORMAT(
     *'Conclusion from autocorrelation',
     *' plot')
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3112)
 3112 FORMAT(
     *'      Under the null hypothesis',
     *' of white noise')
      WRITE(IOUNIT,3113)
 3113 FORMAT(
     *'      (and normality), a ',
     *'2-sided 95% confidence')
      WRITE(IOUNIT,3114)
 3114 FORMAT(
     *  '      interval for the ',
     *  'autocorrelation coefficient')
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3115)YLOWER,YUPPER
 3115 FORMAT('      ',F10.2,'      to ',F10.2)
C
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3116)
 3116 FORMAT(
     *'      Under this null hypothesis,',
     *' only 5')
      WRITE(IOUNIT,3117)
 3117 FORMAT(
     *'      average) of the ',
     *'computed autocorelations')
      WRITE(IOUNIT,3118)
 3118 FORMAT(
     *'      should fall outside ',
     *'of this interval')
C
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3121)
 3121 FORMAT(
     *'      For this data set, ',
     *'it is observed')
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3122)IOUT,KMAXM1,PEROUT
 3122 FORMAT('      ',I8,
     *'     out of the ',I8,' (= ',F7.2,'%)')
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3123)
 3123 FORMAT(
     *'      of the computed ',
     *'autocorrelation coefficients ',
     *'fall')
      WRITE(IOUNIT,3124)
 3124 FORMAT('      outside of this interval.')
C
      IF(PEROUT.LE.5.0)GOTO3130
      GOTO3140
C
 3130 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3131)
 3131 FORMAT(
     *'Conclusion--based on this ',
     *'autocorrelation')
      WRITE(IOUNIT,3132)
 3132 FORMAT(
     *'            plot test, ',
     *'there is no evidence from')
      WRITE(IOUNIT,3133)
 3133 FORMAT(
     *'            this data to reject',
     *' the hypothesis')
      WRITE(IOUNIT,3134)
 3134 FORMAT('            of randomness.')
      WRITE(IOUNIT,999)
      GOTO3190
C
 3140 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3141)
 3141 FORMAT(
     *'Conclusion--based ',
     *'on this autocorrelation')
      WRITE(IOUNIT,3142)
 3142 FORMAT(
     *'            plot test, ',
     *  'there is evidence from')
      WRITE(IOUNIT,3143)
 3143 FORMAT(
     *'            this data that ',
     *'the hypothesis')
      WRITE(IOUNIT,3144)
 3144 FORMAT('            of randomness should be')
      WRITE(IOUNIT,3145)
 3145 FORMAT('            rejected.')
      WRITE(IOUNIT,999)
      GOTO3190
C
 3190 CONTINUE
      GOTO9000
C
C               *******************************************
C               **  STEP 41--                            **
C               **  WRITE OUT          CONCLUSIONS       **
C               **  FOR CROSS-CORRELATION PLOT ANALYSIS  **
C               *******************************************
C
 4100 CONTINUE
      ISTEPN='41'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCCP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      GOTO9000
C
CCCCC THE FOLLOWING SECTION WAS ADDED FEBRUARY 1993
C               *******************************************
C               **  STEP 51--                            **
C               **  WRITE OUT          CONCLUSIONS       **
C               **  FOR PARTIAL AUTOCORRELATION PLOT ANALYSIS  **
C               *******************************************
C
 5100 CONTINUE
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCCP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      GOTO9000
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'WCCP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWCCP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR,ISUBN0,IERRFI,IOUNIT
 9021   FORMAT('IERROR,ISUBN0,IERRFI,IOUNIT = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE(1:80)
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWCPP(X1,N1,ICASPL,IDATSW,
     1                  CORR,DISPAR,NUMDIS,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--WRITE CONCLUSIONS BASED ON ANALYSIS OF PPCC PLOT
C              (PROBABILITY PLOT CORRELATION COEFFICIENT PLOT)
C              OUT TO A FILE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--JUNE      1982.
C     UPDATED         --JANUARY   1986.
C     UPDATED         --FEBRUARY  1989.  FORMATS DUE TO 2X NOS LOWER CASE CHAR
C     UPDATED         --MAY       1990.  EXIT FOR IG, WALD, RIG, FL
C     UPDATED         --DECEMBER  1993.  EXIT FOR POISSON, CHIS, GEOM,
C                                        GAMMA, EV, AND GP
C     UPDATED         --APRIL     1995.  EXIT FOR LOGNORMAL, POWER
C                                        NORMAL, POWER LOGNORMAL
C     UPDATED         --DECEMBER  1995.  EXIT FOR GENERALIZED LOGISTIC
C     UPDATED         --FEBRUARY  1996.  EXIT FOR BRADFORD
C     UPDATED         --MAY       1996.  EXIT FOR RECIPROCAL
C     UPDATED         --JANUARY   1998.  EXIT FOR VON MISES
C     UPDATED         --JANUARY   1998.  EXIT FOR INVERTED GAMMA
C     UPDATED         --AUGUST    2001.  EXIT FOR 2-PARAMETER DISTRIBUTIONS
C     UPDATED         --SEPTEMBER 2001.  EXIT FOR 4 ADDITIONAL
C                                        DISTRIBUTIONS
C     UPDATED         --NOVEMBER  2001.  EXIT FOR GEOM EXTR EXPO
C     UPDATED         --MAY       2002.  EXIT FOR TWO-SIDED POWER
C     UPDATED         --NOVEMBER  2009.  DISTRIBUTIONS HAVE BEEN
C                                        RENAMED
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IDATSW
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
CCCCC CHARACTER*4 IENDFI
CCCCC CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DIMENSION X1(*)
      DIMENSION CORR(*)
      DIMENSION DISPAR(*)
C
C-----COMMON----------------------------------------------------------
C
      CHARACTER*4 ICONFL
      COMMON/ICONCO/ICONFL
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWC'
      ISUBN2='PP  '
      IERROR='NO'
C
      B1=(-999.0)
      EB1=(-999.0)
      SDB1=(-999.0)
      ZB1=(-999.0)
      B2=(-999.0)
      EB2=(-999.0)
      SDB2=(-999.0)
      ZB2=(-999.0)
      CORRMX=(-999.0)
      PARMX=(-999.0)
      CORRUN=(-999.0)
      CORRNO=(-999.0)
      RATIUN=(-999.0)
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'WCPP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWCPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)N1,NUMDIS,ICONNU,ICASPL,IDATSW
   52   FORMAT('N1,NUMDIS,ICONNU,ICASPL,IDATSW = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NUMDIS.GE.1)THEN
          DO55I=1,MIN(500,NUMDIS)
            WRITE(ICOUT,56)I,DISPAR(I),CORR(I)
   56       FORMAT('I,DISPAR(I),CORR(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
        WRITE(ICOUT,60)IBUGS2,ISUBRO,IFOUND,IERROR
   60   FORMAT('IBUGS2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)ICONNA(1:80)
   62   FORMAT('ICONNA = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ICONST,ICONFO,ICONAC,ICONFO,ICONCS
   63   FORMAT('ICONST,ICONFO,ICONAC,ICONFO,ICONCS = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************
C               **  STEP 11--           **
C               **  COPY OVER VARIABLES **
C               **************************
C
      ISTEPN='11'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICONFL.EQ.'CLOS')THEN
        CALL DPOPF0(ICONNU,IBUGS2,ISUBRO,IERROR)
        ICONFL='OPEN'
      ENDIF
C
      IOUNIT=ICONNU
      IFILE=ICONNA
      ISTAT=ICONST
      IFORM=ICONFO
      IACCES=ICONAC
      IPROT=ICONPR
      ICURST=ICONCS
C
      ISUBN0='WCPP'
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'WCPP')THEN
        WRITE(ICOUT,1193)ISUBN0,IERRFI,IOUNIT
 1193   FORMAT('ISUBN0,IERRFI,IOUNIT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1194)IFILE(1:80)
 1194   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1195)ISTAT,IFORM,IACCES,IPROT,ICURST
 1195   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  CHECK TO SEE IF CONCLUSIONS FILE MAY EXIST  **
C               **************************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTAT.EQ.'NONE')THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1211)
 1211   FORMAT('***** ERROR IN DPWCPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1212)
 1212   FORMAT('      THE CONCLUSIONS CANNOT BE SENT TO FILE BECAUSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1214)
 1214   FORMAT('      THE REQUIRED SYSTEM MASS STORAGE FILE WHICH ',
     1         'STORES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1216)
 1216   FORMAT('      SUCH CONCLUSIONS IS NOT AVAILABLE AT THIS ',
     1         'INSTALLATION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1217)ISTAT,ICONST
 1217   FORMAT('ISTAT,ICONST = ',A12,2X,A12)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               ****************************************************************
C               **  STEP 20--
C               **  MAKE PRELIMINARY CALCULTIONS--
C               **  COMPUTE MEAN, S, BIASED S,
C               **  B1, AND B2.
C               **  COMPUTE THE EXPECTED VALUE AND STANDARD DEVIATION OF B1 AND
C               **  UNDER THE NORMALITY ASSUMPTION
C               **  REFERENCE--CRAMER, PAGE 386
C               ****************************************************************
C
      ISTEPN='21'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      N=N1
      AN=N
C
      SUM1=0.0
      DO2110I=1,N
        SUM1=SUM1+X1(I)
 2110 CONTINUE
      XBAR=SUM1/AN
C
      SUM2=0.0
      SUM3=0.0
      SUM4=0.0
      DO2120I=1,N
        DEL=X1(I)-XBAR
        A2=DEL*DEL
        A3=DEL*A2
        A4=A2*A2
        SUM2=SUM2+A2
        SUM3=SUM3+A3
        SUM4=SUM4+A4
 2120 CONTINUE
      AM2=SUM2/AN
      AM3=SUM3/AN
      AM4=SUM4/AN
      S=SQRT(SUM2/(AN-1.0))
      BS=SQRT(AM2)
      B1=AM3/(BS**3)
      B2=AM4/(BS**4)
C
      EB1=0.0
      SDB1=6.0*(AN-2.0)/((AN+1.0)*(AN+3.0))
      SDB1=SQRT(SDB1)
      ZB1=(B1-EB1)/SDB1
C
      EB2=3.0-6.0/(AN+1.0)
      SDB2=24.0*AN*(AN-2.0)*(AN-3.0)/((AN+1.0)*(AN+1.0)*(AN+3.0)*(AN+5.0
     1))
      ZB2=(B2-EB2)/SDB2
C
      CORRMX=CORR(1)
      PARMX=DISPAR(1)
      DO2130I=1,NUMDIS
        IF(CORR(I).GT.CORRMX)THEN
          CORRMX=CORR(I)
          PARMX=DISPAR(I)
        ENDIF
 2130 CONTINUE
C
      IF(ICASPL.NE.'LACP')GOTO2149
      CORRUN=0.0
      CORRNO=1.0
      RATIUN=0.0
      DO2140I=1,NUMDIS
      IF(0.99.LE.DISPAR(I).AND.DISPAR(I).LE.1.01)CORRUN=CORR(I)
      IF(0.09.LE.DISPAR(I).AND.DISPAR(I).LE.0.11)CORRNO=CORR(I)
 2140 CONTINUE
      RATIUN=CORRUN/CORRNO
 2149 CONTINUE
C
C               *****************************************
C               **  STEP 30--                          **
C               **  BRANCH TO THE APPROPRIATE CASE     **
C               **  AND WRITE OUT        CONCLUSIONS   **
C               *****************************************
C
      ISTEPN='30'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3001)
 3001 FORMAT(
     *'Conclusion regarding ',
     *'distributional')
C
      IF(-3.0.LE.ZB1.AND.ZB1.LE.3.0)GOTO3011
      GOTO3019
 3011 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3012)
 3012 FORMAT(
     *'      Based on the third ',
     *'central moment')
      WRITE(IOUNIT,3013)
 3013 FORMAT(
     *'      there is no evidence ',
     *'from this data')
      WRITE(IOUNIT,3014)
 3014 FORMAT(
     *'      to reject the ',
     *'hypothesis of symmetry')
      WRITE(IOUNIT,3015)
 3015 FORMAT(
     *'      In such case, ',
     *'parsimony dictates that the')
      WRITE(IOUNIT,3016)
 3016 FORMAT(
     *'      symmetric model be ',
     *'preferable over the')
      WRITE(IOUNIT,3017)
 3017 FORMAT(
     *  '      non-symmetric model.')
 3019 CONTINUE
C
      IF(ZB1.LE.(-3.0).OR.ZB1.GT.3.0)GOTO3021
      GOTO3029
 3021 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3022)
 3022 FORMAT(
     *'      Based on the third ',
     *'central moment')
      WRITE(IOUNIT,3023)
 3023 FORMAT(
     *'      There is evidence ',
     *'from this data')
      WRITE(IOUNIT,3024)
 3024 FORMAT(
     *'      that the hypothesis of ',
     *  'symmetry')
      WRITE(IOUNIT,3025)
 3025 FORMAT('      should be rejected.')
 3029 CONTINUE
C
      IF(ICASPL.EQ.'TULA')GOTO3100
      IF(ICASPL.EQ.'TPP')GOTO3100
      IF(ICASPL.EQ.'WEIB')GOTO4100
      IF(ICASPL.EQ.'EV2 ')GOTO5100
      GOTO9000
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3031)
 3031 FORMAT('***** INTERNAL ERROR IN DPWCPP ',
     1'AT BRANCH POINT 3031--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3032)
 3032 FORMAT('      ICASPL SHOULD BE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3033)
 3033 FORMAT('      LACP, TCP, WECP, E2CP,   ETC. ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3034)
 3034 FORMAT('      BUT IS NOT.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3035)ICASPL
 3035 FORMAT('      ICASPL = ',A4)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *****************************************
C               **  STEP 31--                          **
C               **  WRITE OUT        CONCLUSIONS       **
C               **  FOR TUKEY OR T PPCC PLOT ANALYSIS  **
C               *****************************************
C
 3100 CONTINUE
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3201)
 3201 FORMAT(
     *'Conclusion regarding ',
     *'normality--')
C
      IF(-3.0.LE.ZB2.AND.ZB2.LE.3.0)GOTO3211
      GOTO3219
 3211 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3212)
 3212 FORMAT(
     *'      Based on the fourth ',
     *'central moment')
      WRITE(IOUNIT,3213)
 3213 FORMAT(
     *'      there is no evidence ',
     *'from this data')
      WRITE(IOUNIT,3214)
 3214 FORMAT(
     *'      to reject the hypothesis ',
     *'of normality')
 3219 CONTINUE
C
      IF(ZB2.LE.(-3.0).OR.ZB2.GT.3.0)GOTO3221
      GOTO3229
 3221 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3222)
 3222 FORMAT(
     *'      Based on the fourth ',
     *'central moment')
      WRITE(IOUNIT,3223)
 3223 FORMAT(
     *'      There is evidence ',
     *'from this data')
      WRITE(IOUNIT,3224)
 3224 FORMAT(
     *'      that the hypothesis ',
     *'of normality')
      WRITE(IOUNIT,3225)
 3225 FORMAT('      should be rejected.')
 3229 CONTINUE
C
      IF(0.0.LE.PARMX.AND.PARMX.LE.0.3)GOTO3231
      GOTO3239
 3231 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3232)
 3232 FORMAT(
     *'      Based on the probability ',
     *'plot')
      WRITE(IOUNIT,3233)
 3233 FORMAT(
     *'      correlation coefficient ',
     *'analysis')
      WRITE(IOUNIT,3234)
 3234 FORMAT(
     *'      indications are that ',
     *'the normal')
      WRITE(IOUNIT,3235)
 3235 FORMAT(
     *'      provides a near-optimal ',
     *'fit among')
      IF(ICASPL.EQ.'LACP')WRITE(IOUNIT,3236)
 3236 FORMAT(
     *'      various members of ',
     *'the Tukey lambda')
      IF(ICASPL.EQ.'TCP')WRITE(IOUNIT,3237)
 3237 FORMAT(
     *'      various members of the t')
      WRITE(IOUNIT,3238)
 3238 FORMAT('      distribution family.')
 3239 CONTINUE
C
      IF(0.0.LE.PARMX.AND.PARMX.LE.0.3.AND.
     1RATIUN.GE.0.95)GOTO3241
      GOTO3249
 3241 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,3242)
 3242 FORMAT(
     *'      However, there is ',
     *'also evidence')
      WRITE(IOUNIT,3243)
 3243 FORMAT(
     *'      that many distributions ',
     *'shorter-')
      WRITE(IOUNIT,3244)
 3244 FORMAT(
     *'      tailed than normal ',
     *'(e.g., uniform)')
      WRITE(IOUNIT,3245)
 3245 FORMAT(
     *'      would serve-equally-well ',
     *  'as a')
      WRITE(IOUNIT,3246)
 3246 FORMAT('      distributional model.')
 3249 CONTINUE
      GOTO7900
C
C               **************************************
C               **  STEP 41--                       **
C               **  WRITE OUT EXPERT CONCLUSIONS    **
C               **  FOR WEIBULL PPCC PLOT ANALYSIS  **
C               **************************************
C
 4100 CONTINUE
      ISTEPN='31'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      GOTO7900
C
C               ***************************************************
C               **  STEP 51--                                    **
C               **  WRITE OUT EXPERT CONCLUSIONS                 **
C               **  FOR EXTREME VALUE TYPE 2 PPCC PLOT ANALYSIS  **
C               ***************************************************
C
 5100 CONTINUE
      ISTEPN='51'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,5101)
 5101 FORMAT(
     *'Conclusion regarding extreme ',
     *'value')
C
      IF(PARMX.GT.20.0)GOTO5111
      GOTO5119
 5111 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,5112)
 5112 FORMAT(
     *'      Based on the ',
     *'probability plot')
      WRITE(IOUNIT,5113)
 5113 FORMAT(
     *'      correlation coefficient ',
     *'analysis')
      WRITE(IOUNIT,5114)
 5114 FORMAT(
     *'      indications are ',
     *'that the  ')
      WRITE(IOUNIT,5115)
 5115 FORMAT(
     *'      extreme value type ',
     *'1 distribution')
      WRITE(IOUNIT,5116)
 5116 FORMAT(
     *'      provides a near-optimal ',
     *'fit among')
      WRITE(IOUNIT,5117)
 5117 FORMAT(
     *'      various members of the ',
     *'extreme value')
      WRITE(IOUNIT,5118)
 5118 FORMAT('      distribution family.')
 5119 CONTINUE
C
      GOTO7900
C
C               **************************************************
C               **  STEP 79--                                   **
C               **  IF APPROPRIATE, PRINT OUT A COMMENT         **
C               **  REGARDING THE SMALLESS OF THE SAMPLE SIZE.  **
C               **************************************************
C
 7900 CONTINUE
      ISTEPN='79'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WCPP')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LE.30)GOTO7951
      GOTO7959
 7951 CONTINUE
      WRITE(IOUNIT,999)
      WRITE(IOUNIT,7952)
 7952 FORMAT(
     *'      Caution must be ',
     *'exercised in')
      WRITE(IOUNIT,7953)
 7953 FORMAT(
     *'      this distributional-modeling',
     *' problem')
      WRITE(IOUNIT,7954)
 7954 FORMAT(
     *'      due to the relatively ',
     *'small number')
      WRITE(IOUNIT,7955)
 7955 FORMAT('      of data points.')
 7959 CONTINUE
      WRITE(IOUNIT,999)
C
C
C               ******************
C               **   STEP 90--  **
C               **   EXIT       **
C               ******************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'WCPP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWCPP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,N1,NUMDIS,ICASPL,IDATSW
 9012   FORMAT('N,N1,NUMDIS,ICASPL,IDATSW = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        IF(NUMDIS.GE.1)THEN
          DO9015I=1,MIN(500,NUMDIS)
            WRITE(ICOUT,9016)I,DISPAR(I),CORR(I)
 9016       FORMAT('I,DISPAR(I),CORR(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
        WRITE(ICOUT,9023)AN,XBAR,S,BS
 9023   FORMAT('AN,XBAR,S,BS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)B1,EB1,SDB1,ZB1
 9024   FORMAT('B1,EB1,SDB1,ZB1 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)B2,EB2,SDB2,ZB2
 9025   FORMAT('B2,EB2,SDB2,ZB2 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)CORRMX,PARMX
 9026   FORMAT('CORRMX,PARMX = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9027)CORRUN,CORRNO,RATIUN
 9027   FORMAT('CORRUN,CORRNO,RATIUN = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IBUGS2,ISUBRO,ISUBN0,IFOUND,IERROR
 9029   FORMAT('IBUGS2,ISUBRO,ISUBB0,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)IERRFI,IOUNIT
 9031   FORMAT('IERRFOI,IOUNIT = ',A4,2X,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IFILE(1:80)
 9032   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)ISTAT,IFORM,IACCES,IPROT,ICURST
 9033   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWDST(IWD1,IWD12,ISHIFT,IWD2,IWD22,IANS,IWIDTH,
     1                  IANS2,N2,IBUGA3,IERROR)
C
C     PURPOSE--GIVEN THAT WE HAVE THE PAIR OF A4 WORDS (IWD1 AND IWD2)
C              IN IHARG(.) THAT ARE    ISHIFT    APART
C              (ISHIFT = 0, 1, 2, ...), FIND THE CORRESPONDING A1
C              HOLLERITH STRING FOR THE SECOND WORD (IWD2);
C              INCLUDE ALSO ANY CONTINUATIONS OF THE SECOND WORD.
C     NOTE--THIS SUBROUTINE IS USEFUL IN THE CONVERSION OF A WORD (AND
C           ITS CONTINUATION) INTO A CONSTANT OR AN ELEMENT OF A VECTOR.
C     NOTE--VALID VALUES OF ISHIFT FOR THIS SUBROUTINE
C           ARE 0 AND THE POSITIVE INTEGERS.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY  1979.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWD1
      CHARACTER*4 IWD12
      CHARACTER*4 IWD2
      CHARACTER*4 IWD22
      CHARACTER*4 IANS
      CHARACTER*4 IANS2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICH1
      CHARACTER*4 ICH11
      CHARACTER*4 ICH12
      CHARACTER*4 ICH2
      CHARACTER*4 ICH21
      CHARACTER*4 ICH22
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IANS(*)
      DIMENSION IANS2(*)
C
      DIMENSION ICH11(10)
      DIMENSION ICH12(10)
      DIMENSION ICH1(20)
      DIMENSION ICH21(10)
      DIMENSION ICH22(10)
      DIMENSION ICH2(20)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA MAXPAS/100/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWD'
      ISUBN2='ST  '
      IERROR='NO'
C
      NUMASC=4
      NUMAS2=2*NUMASC
C
      IPOS1=0
      IPOS2=0
      I2=0
C
      DO11I=1,10
        ICH11(I)=' '
        ICH12(I)=' '
        ICH21(I)=' '
        ICH22(I)=' '
   11 CONTINUE
      DO13I=1,20
        ICH1(I)=' '
        ICH2(I)=' '
   13 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWDST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IWD1,IWD12,ISHIFT,IWD2,IWD22
   52   FORMAT('IWD1,IWD12,ISHIFT,IWD2,IWD22 = ',2A4,2X,I8,2X,2A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IWIDTH,IBUGA3
   53   FORMAT('IWIDTH,IBUGA3 = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,MIN(100,IWIDTH))
   54   FORMAT('IANS(.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C               ************************************
C               **  STEP 2--                      **
C               **  DETERMINE THE A1-EQUIVALENT   **
C               **  OF THE A4-WORD IWD1.          **
C               **  DETERMINE THE A1-EQUIVALENT   **
C               **  OF THE A4-WORD IWD2.          **
C               ************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPXH1H(IWD1,ICH11,IEND11,IBUGA3)
      CALL DPXH1H(IWD12,ICH12,IEND12,IBUGA3)
      DO205K=1,NUMAS2
        ICH1(K)=' '
  205 CONTINUE
      L=0
      DO206K=1,NUMASC
        L=L+1
        ICH1(L)=ICH11(K)
  206 CONTINUE
      DO207K=1,NUMASC
        L=L+1
        ICH1(L)=ICH12(K)
  207 CONTINUE
      IEND1=0
      IF(IEND11.GE.1)IEND1=IEND11
      IF(IEND11.GE.NUMASC)IEND1=NUMASC
      IF(IEND12.GE.1)IEND1=NUMASC+IEND12
      IF(IEND12.GE.NUMAS2)IEND1=NUMAS2
C
      CALL DPXH1H(IWD2,ICH21,IEND21,IBUGA3)
      CALL DPXH1H(IWD22,ICH22,IEND22,IBUGA3)
      DO605K=1,NUMAS2
        ICH2(K)=' '
  605 CONTINUE
      L=0
      DO606K=1,NUMASC
        L=L+1
        ICH2(L)=ICH21(K)
  606 CONTINUE
      DO607K=1,NUMASC
        L=L+1
        ICH2(L)=ICH22(K)
  607 CONTINUE
      IEND2=0
      IF(IEND21.GE.1)IEND2=IEND21
      IF(IEND21.GE.NUMASC)IEND2=NUMASC
      IF(IEND22.GE.1)IEND2=NUMASC+IEND22
      IF(IEND22.GE.NUMAS2)IEND2=NUMAS2
C
C               ******************************************
C               **  STEP 3--                            **
C               **  SET UP A LARGE LOOP--               **
C               **  MAKE AT MOST 100 PASSES AT IANS(.)  **
C               **  TO SEARCH FOR IWD1                  **
C               **  AND FOLLOWED (ISHIFT WORDS LATER)   **
C               **  BY IWD2.                            **
C               ******************************************
C
      IF(IBUGA3.EQ.'ON')THEN
        ISTEPN='3'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,1011)IEND1,IEND2
 1011   FORMAT('IEND1,IEND2 = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)IEND11,IEND12,IEND21,IEND22
 1012   FORMAT('IEND11,IEND12,IEND21,IEND22 = ',4I8)
        CALL DPWRST('XXX','BUG ')
        DO1013II=1,MAX(IEND1,IEND2)
          WRITE(ICOUT,1015)II,ICH1(II),ICH2(II)
 1015     FORMAT('II,ICH1(II),ICH2(II) = ',I5,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
 1013   CONTINUE
      ENDIF
C
      IMINCO=1
      DO1000IPASS=1,MAXPAS
C
C               **************************************
C               **  STEP 4--                        **
C               **  LOCATE THE POSITION IN IANS(.)  **
C               **  OF THE FIRST LETTER OF THE      **
C               **  A1-EQUIVALENT OF IWD1.          **
C               **  STORE THIS POSITION IN IPOS1.   **
C               **************************************
C
        IF(IBUGA3.EQ.'ON')THEN
          ISTEPN='4'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,1091)IMINCO
 1091     FORMAT('IMINCO = ',I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO410I=IMINCO,IWIDTH
          DO420J=1,IEND1
            K=I+J-1
            IF(K.GT.IWIDTH)GOTO430
            IF(IANS(K).NE.ICH1(J))GOTO410
  420     CONTINUE
          KP1=K+1
          IF(KP1.GT.IWIDTH)GOTO430
          IF(IEND1.NE.NUMCPW.AND.IANS(KP1).NE.' ')GOTO410
          IPOS1=I
          GOTO490
  410   CONTINUE
  430   CONTINUE
C
        WRITE(ICOUT,431)
  431   FORMAT('***** INTERNAL ERROR IN DPWDST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,432)IWD1,IWD12
  432   FORMAT('      1H REPRESENTATION FOR    ',A4,A4,
     1         '   NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,433)(IANS(I),I=1,MIN(100,IWIDTH))
  433     FORMAT('      ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
  490   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,491)IPOS1,K
  491     FORMAT('IPOS1,K = ',I8,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               **************************************
C               **  STEP 5--                        **
C               **  LOCATE THE POSITION IN IANS(.)  **
C               **  OF THE FIRST LETTER OF THE      **
C               **  A1-EQUIVALENT OF THE WORD       **
C               **  ISHIFT    WORDS TO THE RIGHT    **
C               **  OF IWD1.                        **
C               **  THIS SHOULD CORRESPOND TO       **
C               **  THE WORD FOUND IN IWD2.         **
C               **  STORE THIS POSITION IN IPOS2.   **
C               **************************************
C
        ISTEPN='5'
        IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IMIN=IPOS1
        IPOS2=IMIN
        IF(ISHIFT.LE.0)GOTO590
        DO510K=1,ISHIFT
          DO520I=IMIN,IWIDTH
            I2=I
            IF(IANS(I).EQ.' ')THEN
              DO530J=I2,IWIDTH
                J2=J
                IF(IANS(J).NE.' ')THEN
                  IMIN=J2
                  IPOS2=IMIN
                  GOTO510
                ENDIF
  530         CONTINUE
              GOTO580
            ELSEIF(IANS(IMIN).NE.'='.AND.IANS(I).EQ.'=')THEN
              IMIN=I2
              IPOS2=IMIN
              GOTO510
            ELSEIF(IANS(IMIN).EQ.'='.AND.IANS(I).NE.'=')THEN
              IMIN=I2
              IPOS2=IMIN
              GOTO510
            ENDIF
  520     CONTINUE
          GOTO580
  510   CONTINUE
        GOTO590
C
  580   CONTINUE
        WRITE(ICOUT,431)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,582)ISHIFT
  582   FORMAT('      1H REPRESENTATION FOR WORD SHIFTED ',I8,' WORDS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,583)IWD1,IWD12
  583   FORMAT('      TO THE RIGHT OF ',A4,A4,' NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,433)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
  590   CONTINUE
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,591)IPOS2,I2
  591     FORMAT('IPOS2,I2 = ',I8,I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ************************************************
C               **  STEP 6--                                  **
C               **  EXTRACT THE CHARACTER STRING IN IANS(.)   **
C               **  STARTING WITH POSITION IPOS2              **
C               **  AND STOPPING WITH (BUT NOT INCLUDING)     **
C               **  THE FIRST BLANK CHARACTER.                **
C               **  STORE SUCH A STRING IN IANS2(.).          **
C               **  STORE THE LENGTH OF SUCH A STRING IN N2.  **
C               ************************************************
C
        ISTEPN='6'
        IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        J=0
        DO610I=IPOS2,IWIDTH
          IF(IANS(I).EQ.' ')GOTO620
          J=J+1
          IANS2(J)=IANS(I)
  610   CONTINUE
  620   CONTINUE
        N2=J
        IF(N2.LT.1)THEN
          WRITE(ICOUT,431)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,622)
  622     FORMAT('      LENGTH N2 OF OUTPUT STRING = 0')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,623)
  623     FORMAT('      FOR 1H REPRESENTATION OF WORD')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,624)ISHIFT
  624     FORMAT('      SHIFTED ',I8,' WORDS TO THE RIGHT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,625)IWD1,IWD12
  625     FORMAT('OF ',A4,A4,'   .')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,433)(IANS(I),I=1,MIN(100,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,691)N2,IEND2
  691     FORMAT('N2,IEND2 = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,692)(IANS2(I),I=1,MIN(100,N2))
  692     FORMAT('IANS2(.) = ',100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ****************************************
C               **  STEP 7--                          **
C               **  AS A FINAL CHECK,                 **
C               **  COMPARE THE A1-EQUIVALENT         **
C               **  OF THE A4-WORD IWD2               **
C               **  WITH THE CONTENTS                 **
C               **  OF IANS2(.)--THE FIRST IEND2      **
C               **  CHARACTERS SHOULD BE IDENTICAL.   **
C               **  IF NOT, THEN MAKE ANOTHER         **
C               **  PASS FURTHER DOWN IANS(.)         **
C               **  TO SEARCH FOR                     **
C               **  THE PAIR (IWWD1 AND IWD2)         **
C               **  AT THE SPECIFIED                  **
C               **  ISHIFT   WORDS APART.             **
C               ****************************************
C
        ISTEPN='7'
        IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(N2.GE.IEND2)THEN
          DO710I=1,IEND2
            IF(ICH2(I).NE.IANS2(I))GOTO719
  710     CONTINUE
          GOTO9000
        ENDIF
  719   CONTINUE
C
        IMINCO=IPOS1+1
        IF(IMINCO.LE.IWIDTH)GOTO1000
        WRITE(ICOUT,431)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1102)IWD1,IWD12,IWD2,IWD22
 1102   FORMAT('      1H REPRESENTATION FOR    ',A4,A4,' AND ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1103)ISHIFT
 1103   FORMAT('      (',I8,' WORDS APART) NOT FOUND.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,433)(IANS(I),I=1,IWIDTH)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
C
 1000 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
C
      ISTEPN='8'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWDST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IEND1,IEND2,IPOS1,IPOS2,N2
 9012   FORMAT('IEND1,IEND2,IPOS1,IPOS2,N2 = ',5I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(IANS2(I),I=1,MIN(100,N2))
 9014   FORMAT('IANS2(.) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IERROR,NUMASC,NUMAS2
 9015   FORMAT('IERROR,NUMASC,NUMAS2 = ',A4,2X,2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)IEND11,IEND12,IEND1,IEND21,IEND22,IEND2
 9020   FORMAT('IEND11,IEND12,IEND1,IEND21,IEND22,IEND2 = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)(ICH11(I),I=1,10)
 9021   FORMAT('(ICH11(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)(ICH12(I),I=1,10)
 9022   FORMAT('(ICH12(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)(ICH1 (I),I=1,10)
 9023   FORMAT('(ICH1 (I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)(ICH21(I),I=1,10)
 9024   FORMAT('(ICH21(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)(ICH22(I),I=1,10)
 9025   FORMAT('(ICH22(I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)(ICH2 (I),I=1,10)
 9026   FORMAT('(ICH2 (I),I=1,10) = ',10A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWEAR(TEMP,TEMP2,IBUGA3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE ADJUSTED RANKS
C              FOR DATA IN PREPARATION
C              WITH A WEIBULL PLOT ANALYSIS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
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
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --JULY      2019. MOVE CREATION OF SCRATCH STORAGE
C                                       TO DPLET
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 NEWNAM
      CHARACTER*4 NEWCOL
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IWRITE
      CHARACTER*4 IHARG3
      CHARACTER*4 IHARG4
      CHARACTER*4 IHARG5
      CHARACTER*4 IHARG6
      CHARACTER*4 ILEFT
      CHARACTER*4 ILEFT2
      CHARACTER*4 IRIGHT
      CHARACTER*4 IRIGH2
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION TEMP(*)
      DIMENSION TEMP2(*)
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
      ISUBN1='DPWE'
      ISUBN2='AR  '
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
      NS3=0
      NIISUB=(-999)
      ICOLL=(-999)
      IRIGHT='-999'
      IRIGH2='-999'
      ILOCV=(-999)
      NUMVAR=(-999)
      ICOLR=(-999)
      NIRIGH=(-999)
      ICOL2=(-999)
      NIRIG2=(-999)
      ILOCSV=(-999)
      NLEFT=(-999)
C
C               ***********************************************
C               **  TREAT THE WEIBULL ADJUSTED RANKS CASE    **
C               **       1) FOR A FULL VARIABLE, OR          **
C               **       2) FOR PART OF A VARIABLE.          **
C               ***********************************************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWEAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,IBUGQ
   52   FORMAT('IBUGA3,IBUGQ = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NEWNAM='NO'
      NEWCOL='NO'
C
C               ********************************************************
C               **  STEP 2--                                           *
C               **  EXAMINE THE LEFT-HAND SIDE--                       *
C               **  IS THE VARIABLE NAME TO LEFT OF = SIGN ALREADY IN  *
C               **  THE NAME LIST?    AS A VARIABLE?  NOTE THAT        *
C               **  ILEFT     IS THE NAME OF THE VARIABLE ON THE LEFT. *
C               **  NOTE THAT   ILISTL  IS THE LINE IN THE TABLE OF    *
C               **  THE NAME ON THE LEFT.  NOTE THAT   ICOLL  IS THE   *
C               **  DATA COLUMN (1 TO 12) FOR THE NAME OF THE LEFT.    *
C               ********************************************************
C
      ISTEPN='2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILEFT=IHARG(1)
      ILEFT2=IHARG2(1)
      DO200I=1,NUMNAM
        I2=I
        IF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'P')THEN
          ILISTL=I2
          GOTO235
        ELSEIF(ILEFT.EQ.IHNAME(I).AND.ILEFT2.EQ.IHNAM2(I).AND.
     1         IUSE(I).EQ.'V')THEN
          ILISTL=I2
          ICOLL=IVALUE(ILISTL)
          NIOLD=IN(ILISTL)
          GOTO290
        ENDIF
  200 CONTINUE
C
      NEWNAM='YES'
      ILISTL=NUMNAM+1
      IF(ILISTL.GT.MAXNAM)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,221)
  221   FORMAT('***** ERROR IN DPWEAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,222)
  222   FORMAT('      THE NUMBER OF VARIABLE AND/OR PARAMETER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,223)MAXNAM
  223   FORMAT('      NAMES HAS JUST EXCEEDED THE MAX ALLOWABLE ',
     1         I8,'  .')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,224)
  224   FORMAT('      SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,225)
  225   FORMAT('      ENTER      STATUS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,226)
  226   FORMAT('      TO FIND OUT THE FULL LIST OF USED NAMES,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,227)
  227   FORMAT('      AND THEN REDEFINE (REUSE) SOME OF THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,228)
  228   FORMAT('      ALREADY-USED NAMES')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
      ENDIF
C
  235 CONTINUE
      NIOLD=0
      ICOLL=NUMCOL+1
      IF(ICOLL.GT.MAXCOL)THEN
        WRITE(ICOUT,221)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,242)
  242   FORMAT('      THE NUMBER OF DATA COLUMNS HAS JUST EXCEEDED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,243)MAXCOL
  243   FORMAT('      THE MAX ALLOWABLE ',I8,'  .  SUGGESTED ACTION--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,245)
  245   FORMAT('      ENTER      STATUS VARIABLES')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,246)
  246   FORMAT('      TO FIND OUT THE FULL LIST OF USED COLUMNS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,247)
  247   FORMAT('      AND THEN DELETE SOME COLUMNS.   EXAMPLE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,248)
  248   FORMAT('      IF       LET X(I) = 3.14         FAILED,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,249)
  249   FORMAT('      THEN ONE MIGHT ENTER     DELETE Y1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,250)
  250   FORMAT('      (ASSUMING Y1 IS A VARAIBLE IN THE LIST)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,251)
  251   FORMAT('      FOLLOWED BY              LET X(I) = 3.14')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO19000
      ENDIF
C
  290 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,291)
  291   FORMAT('AT THE END OF STEP 2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,292)ILEFT,ILEFT2,NEWNAM,NUMNAM,ILISTL,NUMCOL,ICOLL,
     1                  NIOLD
  292   FORMAT('ILEFT,ILEFT2,NEWNAM,NUMNAM,ILISTL,NUMCOL,ICOLL,',
     1         'NIOLD = ',2A4,2X,A4,2X,5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 3--                                           *
C               **  EXAMINE THE RIGHT-HAND SIDE--                      *
C               **  HAS THE VARIABLE OR COLUMN ON THE RIGHT            *
C               **  ALREADY BEEN DEFINED?                              *
C               **  NOTE THAT     ILISTR    IS THE LINE IN THE TABLE   *
C               **  OF THE VARIABLE OR COLUMN ON THE RIGHT.            *
C               **  NOTE THAT   ICOLR   IS THE DATA COLUMN (1 TO 12)   *
C               **  FOR THE VARIABLE OR COLUMN ON THE RIGHT.           *
C               ********************************************************
C
      ISTEPN='3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ***********************************************
C               **  STEP 4--                                 **
C               **  BRANCH BETWEEN 1-VARIABLE SPECIFICATION  **
C               **  (LET Y = WEIBULL ADJUSTED RANKS X)       **
C               **  AND 2-VARIABLE SPECIFICATION             **
C               **  (LET Y = WEIBULL ADJUSTED RANKS X TAG)   **
C               ***********************************************
C
      ISTEPN='4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCV=6
C
      NUMVAR=1
      ILOCVP=ILOCV+1
      IF(ILOCVP.GT.NUMARG)GOTO1000
      IHARG5=IHARG(ILOCVP)
      IHARG6=IHARG2(ILOCVP)
      IF(IHARG5.EQ.'SUBS'.AND.IHARG6.EQ.'ET  ')GOTO1000
      IF(IHARG5.EQ.'EXCE'.AND.IHARG6.EQ.'PT  ')GOTO1000
      IF(IHARG5.EQ.'FOR '.AND.IHARG6.EQ.'    ')GOTO1000
      NUMVAR=2
      GOTO2000
C
C               *******************************************
C               **  STEP 5--                             **
C               **  TREAT THE 1-VARIABLE SPECIFICATIONS  **
C               *******************************************
C
 1000 CONTINUE
C
      ISTEPN='5'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVAR=1
C
      IRIGHT=IHARG(ILOCV)
      IRIGH2=IHARG2(ILOCV)
      DO1100I=1,NUMNAM
        I2=I
        IF(IRIGHT.EQ.IHNAME(I).AND.IRIGH2.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'V')THEN
          ILISTR=I2
          ICOLR=IVALUE(ILISTR)
          NIRIGH=IN(ILISTR)
          GOTO7000
        ELSEIF(IRIGHT.EQ.IHNAME(I).AND.IRIGH2.EQ.IHNAM2(I).AND.
     1         IUSE(I).EQ.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,221)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1102)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1154)
 1154     FORMAT('      ON THE RIGHT OF THE = SIGN WAS FOUND IN THE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1156)
 1156     FORMAT('      INTERNAL NAME LIST, BUT AS A PARAMETER,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1157)
 1157     FORMAT('      AND NOT AS A VARIABLE AS IT SHOULD BE HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1108)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1109)(IANS(II),II=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO19000
        ENDIF
 1100 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,221)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1102)
 1102 FORMAT('      THE SPECIFIED ARGUMENT (VARIABLE NAME OR COLUMN ',
     1       'NUMBER)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1104)
 1104 FORMAT('      ON THE RIGHT OF THE = SIGN WAS NOT FOUND IN THE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1106)
 1106 FORMAT('      INTERNAL NAME LIST OF AVAILABLE VARIABLE NAMES.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1107)IRIGHT,IRIGH2
 1107 FORMAT('      THE VARIABLE IN QUESTION WAS ',2A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1108)
 1108 FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1109)(IANS(I),I=1,MIN(80,IWIDTH))
 1109   FORMAT(80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO19000
C
C               ************************************************
C               **  STEP 6.2--                                **
C               **  TREAT THE 2 VARIABLE SPECIFICATION.       **
C               **  CHECK THE VALIDITY OF THE FIRST ARGUMENT  **
C               ************************************************
C
 2000 CONTINUE
C
      ISTEPN='6.2'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVAR=2
C
      IHARG3=IHARG(ILOCV)
      IHARG4=IHARG2(ILOCV)
      DO2210I=1,NUMNAM
        I2=I
        IF(IHARG3.EQ.IHNAME(I).AND.IHARG4.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'V')THEN
          ILISTR=I2
          ICOLR=IVALUE(ILISTR)
          NIRIGH=IN(ILISTR)
          GOTO2300
        ELSEIF(IHARG3.EQ.IHNAME(I).AND.IHARG4.EQ.IHNAM2(I).AND.
     1         IUSE(I).EQ.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,221)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2212)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1154)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1156)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1157)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1107)IHARG3,IHARG4
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1108)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1109)(IANS(II),II=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO19000
        ENDIF
 2210 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,221)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2212)
 2212 FORMAT('      THE SPECIFIED FIRST  ARGUMENT (VARIABLE NAME ',
     1       'OR COLUMN NUMBER)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1104)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1106)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1107)IHARG3,IHARG4
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1108)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1109)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO19000
C
C
C               *****************************************************
C               **  STEP 6.3--                                     **
C               **  CHECK THE VALIDITY OF THE SECOND ARGUMENT      **
C               *****************************************************
C
 2300 CONTINUE
C
      ISTEPN='6.3'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCVP=ILOCV+1
      IF(ILOCVP.GT.NUMARG)THEN
        WRITE(ICOUT,221)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2302)
 2302   FORMAT('      NO SECOND VARIABLE NAME OR COLUMN NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2303)
 2303   FORMAT('      WAS GIVEN AFTER THE OPERATION CALCULATION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1108)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1109)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO19000
      ENDIF
C
      IHARG5=IHARG(ILOCVP)
      IHARG6=IHARG2(ILOCVP)
      DO2310I=1,NUMNAM
        I2=I
        IF(IHARG5.EQ.IHNAME(I).AND.IHARG6.EQ.IHNAM2(I).AND.
     1     IUSE(I).EQ.'V')THEN
          ILIST2=I2
          ICOL2=IVALUE(ILIST2)
          NIRIG2=IN(ILIST2)
          GOTO2390
C
        ELSEIF(IHARG5.EQ.IHNAME(I).AND.IHARG6.EQ.IHNAM2(I).AND.
     1         IUSE(I).EQ.'P')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,221)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2312)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1154)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1156)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1157)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1107)IHARG3,IHARG4
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1108)
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1109)(IANS(II),II=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO19000
        ENDIF
 2310 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,221)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2312)
 2312 FORMAT('      THE SPECIFIED SECOND ARGUMENT (VARIABLE NAME OR ',
     1       'COLUMN NUMBER)')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1104)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1106)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1107)IRIGHT,IRIGH2
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1108)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,1109)(IANS(I),I=1,MIN(80,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO19000
C
 2390 CONTINUE
C
C               ******************************************************
C               **  STEP 6.4--                                      **
C               **  CHECK THAT THE 2 VARIABLES HAVE THE SAME        **
C               **  NUMBER OF ELEMENTS.                             **
C               ******************************************************
C

      ISTEPN='6.4'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NIRIG2.NE.NIRIGH)THEN
        WRITE(ICOUT,221)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      FOR A 2-VARIABLE MATHEMATICAL OPERATION, THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2413)
 2413   FORMAT('      NUMBER OF OBSERVATIONS IN EACH VARIABLE MUST BE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2415)
 2415   FORMAT('      THE SAME;  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2416)IHARG3,IHARG4,NIRIGH
 2416   FORMAT('           VARIABLE ',2A4,' HAS ',I8,' OBSERVATIONS;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2416)IHARG5,IHARG6,NIRIG2
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1108)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,1109)(IANS(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO19000
      ENDIF
      GOTO7000
C
C               *******************************
C               **  STEP 7--                 **
C               **  DETERMINE THE SUBCASE    **
C               **  AND BRANCH ACCORDINGLY.  **
C               *******************************
C
 7000 CONTINUE
C
      ISTEPN='7'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,7006)NINEW,NIRIGH,NIRIG2,NUMVAR
 7006   FORMAT('NINEW,NIRIGH,NIRIG2,NUMVAR = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NUMVAR.EQ.1)THEN
        IF(ILOCV.EQ.NUMARG)GOTO8000
        ILOCVP=ILOCV+1
        IF(ILOCV.LT.NUMARG.AND.IHARG(ILOCVP).EQ.'SUBS'.AND.
     1     IHARG2(ILOCVP).EQ.'ET  ')GOTO9000
        IF(ILOCV.LT.NUMARG.AND.IHARG(ILOCVP).EQ.'EXCE'.AND.
     1     IHARG2(ILOCVP).EQ.'PT  ')GOTO9000
        IF(ILOCV.LT.NUMARG.AND.IHARG(ILOCVP).EQ.'FOR '.AND.
     1     IHARG2(ILOCVP).EQ.'    ')GOTO10000
      ELSEIF(NUMVAR.EQ.2)THEN
        ILOCVP=ILOCV+1
        IF(ILOCVP.EQ.NUMARG)GOTO8000
        ILOCV2=ILOCV+2
        IF(ILOCVP.LT.NUMARG.AND.IHARG(ILOCV2).EQ.'SUBS'.AND.
     1     IHARG2(ILOCV2).EQ.'ET  ')GOTO9000
        IF(ILOCVP.LT.NUMARG.AND.IHARG(ILOCV2).EQ.'EXCE'.AND.
     1     IHARG2(ILOCV2).EQ.'PT  ')GOTO9000
        IF(ILOCVP.LT.NUMARG.AND.IHARG(ILOCV2).EQ.'FOR '.AND.
     1     IHARG2(ILOCV2).EQ.'    ')GOTO10000
      ENDIF
C
      WRITE(ICOUT,221)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,7012)
 7012 FORMAT('      ILLEGAL SYNTAX FOR LET COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1108)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1109)(IANS(I),I=1,MIN(80,IWIDTH))
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO19000
C
C               ************************************************
C               **  STEP 8--                                  **
C               **  TREAT THE FULL VARIABLE CASE.             **
C               **  EXAMPLE--LET Y = WEIBULL ADJUSTED RANKS X **
C               **  THEN JUMP TO STEP NUMBER 10 BELOW         **
C               **  FOR THE LIST UPDATING AND                 **
C               **  FOR SOME INFORMATIVE PRINTING.            **
C               ************************************************
C
 8000 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        ISTEPN='8'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,8011)NINEW,NIRIGH,NUMVAR
 8011   FORMAT('NINEW,NIRIGH,NUMVAR = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICASEQ='FULL'
      NIOLD=NIRIGH
      IF(NUMVAR.GE.2.AND.NIRIG2.GT.NIOLD)NIOLD=NIRIG2
      NINEW=NIOLD
      DO8100I=1,NINEW
        ISUB(I)=1
 8100 CONTINUE
      NIISUB=NIOLD
      GOTO11000
C
C               ********************************************************
C               **  STEP 9--                                           *
C               **  TREAT THE PARTIAL VARIABLE SUBSET CASE.            *
C               **  EXAMPLE--LET Y = WEIBULL ADJUSTED RANKS X          *
C               **                   SUBSET 2 3 5                      *
C               **  JUMP TO STEP NUMBER 11 BELOW FOR THE ACTUAL        *
C               **  MATHEMATICAL OPERATIOn, FOR THE LIST UPDATING, AND *
C               **  FOR SOME INFORMATIVE PRINTING.                     *
C               ********************************************************
C
19000 CONTINUE
      ISTEPN='9'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='SUBS'
      IF(NUMVAR.EQ.1)ILOCSV=ILOCV+2
      IF(NUMVAR.EQ.2)ILOCSV=ILOCV+3
      IHSET=IHARG(ILOCSV)
      IHSET2=IHARG2(ILOCSV)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1            IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1            ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
      IF(IERROR.EQ.'YES')GOTO19000
      NIOLD=IN(ILOC)
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
CCCCC NINEW=NS
      NINEW=NIOLD
      NIISUB=NIOLD
      GOTO11000
C
C               ********************************************************
C               **  STEP 10--                                          *
C               **  TREAT THE PARTIAL VARIABLE FOR CASE.               *
C               **  EXAMPLE--LET Y = WEIBULL ADJUSTED RANKS X          *
C               **                   FOR I = 1 2 10                    *
C               **  JUMP TO STEP NUMBER 11 BELOW FOR THE ACTUAL        *
C               **  MATHEMATICAL OPERATION, FOR THE LIST UPDATING, AND *
C               **  FOR SOME INFORMATIVE PRINTING.                     *
C               ********************************************************
C
10000 CONTINUE
      ISTEPN='10'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FOR'
      CALL DPFOR(NIOLD,NINEW,IROW1,IROWN,
     1           NLOCAL,ILOCS,NS,IBUGQ,IERROR)
      NIFOR=NINEW
      NIISUB=NINEW
      GOTO11000
C
C               ********************************************
C               **  STEP 11--                             **
C               **  GENERATE    WEIBULL   ADJUSTED RANKS. **
C               **  STORE THEM TEMPORARILY IN             **
C               **  THE VECTOR TEMP(.).                   **
C               ********************************************
C
11000 CONTINUE
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NITEMP=NINEW
      NS2=0
      DO11100I=1,NINEW
        IJ=MAXN*(ICOLR-1)+I
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,11110)I,NS2,NINEW,ISUB(I),IJ,V(IJ)
11110     FORMAT('I,NS2,NINEW,ISUB(I),IJ,V(IJ) = ',5I8,F12.5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ISUB(I).EQ.0)GOTO11100
C
        IF(NUMVAR.EQ.1)THEN
          IF(I.LE.NIRIGH)THEN
            NS2=NS2+1
            IJ=MAXN*(ICOLR-1)+I
            IF(ICOLR.LE.MAXCOL)TEMP(NS2)=1.0
            IF(ICOLR.EQ.MAXCP1)TEMP(NS2)=1.0
            IF(ICOLR.EQ.MAXCP2)TEMP(NS2)=1.0
            IF(ICOLR.EQ.MAXCP3)TEMP(NS2)=1.0
            IF(ICOLR.EQ.MAXCP4)TEMP(NS2)=1.0
            IF(ICOLR.EQ.MAXCP5)TEMP(NS2)=1.0
            IF(ICOLR.EQ.MAXCP6)TEMP(NS2)=1.0
          ENDIF
        ELSEIF(NUMVAR.EQ.2)THEN
          IF(I.LE.NIRIG2)THEN
            NS2=NS2+1
            IJ=MAXN*(ICOL2-1)+I
            IF(ICOL2.LE.MAXCOL)TEMP(NS2)=V(IJ)
            IF(ICOL2.EQ.MAXCP1)TEMP(NS2)=PRED(I)
            IF(ICOL2.EQ.MAXCP2)TEMP(NS2)=RES(I)
            IF(ICOL2.EQ.MAXCP3)TEMP(NS2)=RES(I)
            IF(ICOL2.EQ.MAXCP4)TEMP(NS2)=RES(I)
            IF(ICOL2.EQ.MAXCP5)TEMP(NS2)=RES(I)
            IF(ICOL2.EQ.MAXCP6)TEMP(NS2)=RES(I)
          ENDIF
        ENDIF
C
11100 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,11131)ICOLL,ICOLR,ICOL2,NS2,NINEW,ICASEQ
11131   FORMAT('ICOLL,ICOLR,ICOL2,NS2,NINEW,ICASEQ = ',5I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IWRITE='ON'
      IF(IPRINT.EQ.'OFF' .OR. IFEEDB.EQ.'OFF')IWRITE='OFF'
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,11132)(TEMP(I),I=1,NS2)
11132   FORMAT(F10.5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL WEIBAR(TEMP,NS2,IWRITE,TEMP2,IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11132)(TEMP2(I),I=1,NS2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 12--                                         **
C               **  IF CALLED FOR (THAT IS, IF IBUGA3 IS ON),         **
C               **  PRINT OUT THE INTERMEDIATE VARIABLE TEMP2(.).     **
C               **  THIS IS USEFUL FOR DIAGNOSTIC PURPOSES            **
C               **  IN REVIEWING THE OUTPUT FROM THIS SUBROUTINE.     **
C               ********************************************************
C
C
      IF(IBUGA3.EQ.'ON')THEN
        ISTEPN='12'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,12051)
12051   FORMAT('OUTPUT FROM MIDDLE OF DPWEAR AFTER WEIBAR ',
     1         'HAS BEEN CALLED--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12052)NS2
12052   FORMAT('NS2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NS2.GT.0)THEN
          DO12054I=1,NS2
            WRITE(ICOUT,12055)I,TEMP(I),TEMP2(I)
12055       FORMAT('I,TEMP(I),TEMP2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
12054     CONTINUE
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 13--                                       **
C               **  COPY THE WEIBULL ADJUSTED RANKS                 **
C               **  FROM THE INTERMEDIATE VECTOR TEMP2(.)           **
C               **  TO THE APPROPRIATE COLUMN                       **
C               **  (BASED ON THE QUALIFIER--FULL, SUBSET, OR FOR)  **
C               **  IN THE INTERNAL DATAPLOT DATA TABLE.            **
C               ******************************************************
C
      ISTEPN='13'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NS3=0
      DO13000I=1,NIISUB
        IJ=MAXN*(ICOLL-1)+I
        IF(ISUB(I).EQ.0)GOTO13000
        NS3=NS3+1
        IF(ICOLL.LE.MAXCOL)V(IJ)=TEMP2(NS3)
        IF(ICOLL.EQ.MAXCP1)PRED(I)=TEMP2(NS3)
        IF(ICOLL.EQ.MAXCP2)RES(I)=TEMP2(NS3)
        IF(ICOLL.EQ.MAXCP3)YPLOT(I)=TEMP2(NS3)
        IF(ICOLL.EQ.MAXCP4)XPLOT(I)=TEMP2(NS3)
        IF(ICOLL.EQ.MAXCP5)X2PLOT(I)=TEMP2(NS3)
        IF(ICOLL.EQ.MAXCP6)TAGPLO(I)=TEMP2(NS3)
        IF(NS3.EQ.1)IROW1=I
        IROWN=I
13000 CONTINUE
C
C               *******************************************
C               **  STEP 14--                            **
C               **  CARRY OUT THE LIST UPDATING AND      **
C               **  GENERATE THE INFORMATIVE PRINTING.   **
C               *******************************************
C
      ISTEPN='14'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')THEN
        IF(NEWNAM.EQ.'NO')THEN
          NINEW=NIRIGH
        ELSEIF(NEWNAM.EQ.'YES')THEN
          NINEW=IROWN
        ENDIF
      ELSEIF(ICASEQ.EQ.'SUBS')THEN
        IF(NEWNAM.EQ.'NO'.AND.NLEFT.GE.IROWN)THEN
          NINEW=NLEFT
        ELSEIF(NEWNAM.EQ.'NO'.AND.NLEFT.LT.IROWN)THEN
          NINEW=IROWN
        ELSEIF(NEWNAM.EQ.'YES')THEN
           NINEW=IROWN
        ENDIF
      ELSEIF(ICASEQ.EQ.'FOR')THEN
        IF(NEWNAM.EQ.'NO'.AND.NLEFT.GE.IROWN)THEN
          NINEW=NLEFT
        ELSEIF(NEWNAM.EQ.'NO'.AND.NLEFT.LT.IROWN)THEN
          NINEW=IROWN
        ELSEIF(NEWNAM.EQ.'YES')THEN
          NINEW=IROWN
        ENDIF
      ENDIF
C
      IHNAME(ILISTL)=ILEFT
      IHNAM2(ILISTL)=ILEFT2
      IUSE(ILISTL)='V'
      IVALUE(ILISTL)=ICOLL
      VALUE(ILISTL)=ICOLL
      IN(ILISTL)=NINEW
C
      IF(NEWNAM.EQ.'YES')THEN
        NUMNAM=NUMNAM+1
        NUMCOL=NUMCOL+1
      ENDIF
C
      DO14100J4=1,NUMNAM
        IF(IUSE(J4).EQ.'V'.AND.IVALUE(J4).EQ.ICOLL)THEN
          IUSE(J4)='V'
          IVALUE(J4)=ICOLL
          VALUE(J4)=ICOLL
          IN(J4)=NINEW
        ENDIF
14100 CONTINUE
C
      IF(IPRINT.EQ.'ON' .AND. IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14011)ILEFT,ILEFT2,NS2
14011   FORMAT('THE NUMBER OF VALUES GENERATED FOR ',
     1         'THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
C
        IJ=MAXN*(ICOLL-1)+IROW1
        IJN=MAXN*(ICOLL-1)+IROWN
        IF(ICOLL.LE.MAXCOL)THEN
          WRITE(ICOUT,14021)ILEFT,ILEFT2,V(IJ),IROW1
14021     FORMAT('THE FIRST           COMPUTED VALUE OF ',2A4,
     1           ' = ',E15.7,'   (ROW ',I6,')')
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,V(IJN),IROWN
14031       FORMAT('THE LAST (',I5,'-TH) COMPUTED VALUE OF ',2A4,
     1             ' = ',E15.7,'   (ROW ',I6,')')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP1)THEN
          WRITE(ICOUT,14021)ILEFT,ILEFT2,PRED(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,PRED(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP2)THEN
           WRITE(ICOUT,14021)ILEFT,ILEFT2,RES(IROW1),IROW1
           CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,RES(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP3)THEN
          WRITE(ICOUT,14021)ILEFT,ILEFT2,YPLOT(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,YPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP4)THEN
          WRITE(ICOUT,14021)ILEFT,ILEFT2,XPLOT(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,XPLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP5)THEN
          WRITE(ICOUT,14021)ILEFT,ILEFT2,X2PLOT(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,X2PLOT(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ELSEIF(ICOLL.EQ.MAXCP6)THEN
          WRITE(ICOUT,14021)ILEFT,ILEFT2,TAGPLO(IROW1),IROW1
          CALL DPWRST('XXX','BUG ')
          IF(NS2.NE.1)THEN
            WRITE(ICOUT,14031)NS2,ILEFT,ILEFT2,TAGPLO(IROWN),IROWN
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
C
        IF(NS2.EQ.1)THEN
          WRITE(ICOUT,14041)
14041     FORMAT('SINCE THE GENERATED SAMPLE SIZE WAS ONLY 1,')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,14042)
14042     FORMAT('THE ABOVE VALUE WAS THE SOLE VALUE COMPUTED.')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14112)ILEFT,ILEFT2,ICOLL
14112   FORMAT('THE CURRENT COLUMN FOR THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14113)ILEFT,ILEFT2,NINEW
14113   FORMAT('THE CURRENT LENGTH OF THE VARIABLE ',2A4,' = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWEAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)NS,NS2,NS3,NINEW,NIISUB
 9015   FORMAT('NS,NS2,NS3,NINEW,NIISUB = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWEB(ICOM,ICOM2,IHARG,IHARG2,NUMARG,
     1                 IANSLC,IWIDTH,IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--ACCESS THE WORLD WIDE WEB
C
C              THIS COMMAND TAKES THE FOLLOWING FORMS:
C                  WEB                - GO TO DEFAULT URL
C                  WEB <STRING>  - GO TO URL SPECIFIED BY <STRING>
C     INPUT  ARGUMENTS--IANS    (A  HOLLERITH VECTOR)
C                     --IWIDTH (AN INTEGER VARIABLE)
C                     --IBROWS  (A CHARACTER VARIABLE THAT IDENTIFIES
C                               THE BROWSER TO USE)
C                     --IURL    (A CHARACTER VARIABLE THAT IDENTIFIES
C                               THE WEB URL OF THE DATAPLOT HOME PAGE)
C     OUTPUT ARGUMENTS--IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--97/4
C     ORIGINAL VERSION--APRIL     1997.
C     UPDATED         --MARCH     1999. UPDATE A FEW ADDRESSES
C     UPDATED         --MARCH     1999. TREAT "HANDBOOK" SPECIAL
C     UPDATED         --NOVEMBER  2015. ADD "WEB SEARCH" OPTION
C     UPDATED         --APRIL     2018. ADD SOME SYNONYMS
C     UPDATED         --MARCH     2019. SET SYSTEM PERSIST
C                                       SET SYSTEM HIDDEN
C     UPDATED         --NOVEMBER  2019. SET FILE NAME QUOTE ON
C     UPDATED         --NOVEMBER  2019. SET HYPHEN WORD SEPARATOR OFF
C     UPDATED         --DECEMBER  2019. SUPPORT FOR EDGE BROWSER
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICOM
      CHARACTER*4 ICOM2
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IANSLC
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IERRO2
      CHARACTER*1 IQUOTE
      CHARACTER*500 ICALL
      CHARACTER*128 ICANS
      CHARACTER*128 ISTRIN
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 ICLESV
      CHARACTER*4 IFILQ2
      CHARACTER*4 IHYPS2
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IANSLC(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOF2.INC'
C
      CHARACTER*80 PROFIL
      CHARACTER*80 P86FIL
      CHARACTER*80 APPDAT
      CHARACTER*80 COMNAM
      CHARACTER*80 UPROFI
      CHARACTER*80 DEFPRI
      CHARACTER*20 USRNAM
      CHARACTER*20 ISHELL
      CHARACTER*4  WINBIT
      COMMON/SYSVAR/PROFIL,P86FIL,APPDAT,COMNAM,UPROFI,USRNAM,DEFPRI,
     1              WINBIT,ISHELL
      COMMON/SYSVA2/NCPROF,NCP86F,NCAPPD,NCCOMP,NCUPRO,NCUSER,NCPRIN,
     1              NCSHEL
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWE'
      ISUBN2='B   '
      IFOUND='YES'
      IERROR='NO'
      ISTRIN=' '
      ICALL=' '
      NCSTR=0
      NCURL=0
C
      CALL DPCONA(39,IQUOTE)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWEB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR,ICOM,ICOM2,IWIDTH,NUMARG
   53   FORMAT('IBUGS2,ISUBRO,IERROR,ICOM,ICOM2,IWIDTH,NUMARG = ',
     1         5(A4,2X),2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)(IANSLC(I),I=1,MIN(IWIDTH,80))
   55   FORMAT('IANS(.) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)IBROWS(1:80)
   86   FORMAT('IBROWS = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,88)IURL(1:80)
   88   FORMAT('IDPURL = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,89)IHBURL(1:80)
   89   FORMAT('IHBURL = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC IF(
CCCCC1       (IHOST1.EQ.'SUN') .OR.
CCCCC1       (IHOST1.EQ.'CRAY' .AND. IOPSY1.EQ.'UNIX') .OR.
CCCCC1       (IHOST1.EQ.'CONV') .OR.
CCCCC1       (IHOST1.EQ.'SGI ') .OR.
CCCCC1       (IHOST1.EQ.'HP-9') .OR.
CCCCC1       (IHOST1.EQ.'AIX ') .OR.
CCCCC1       (IHOST1.EQ.'LINU') .OR.
CCCCC1       (IOPSY1.EQ.'UNIX'))GOTO199
CCCCC IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')GOTO199
CC100 CONTINUE
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,111)
CC111 FORMAT('***** FROM DPWEB--WEB COMMAND CURRENTLY ONLY SUPPORTED ',
CCCCC1       'UNIX OR IBM-PC WINDOWS PLATFORMS.')
CC199 CONTINUE
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  CHECK FOR SOME SPECIAL CASES FIRST              **
C               ******************************************************
C
      NCURL=0
      IHB=0
      IF((NUMARG.GE.1.AND.IHARG(1).EQ.'SEAR') .OR.
     1   (ICOM.EQ.'????' .AND. ICOM2.EQ.'?   ') .OR.
     1    ICOM.EQ.'WS  ')THEN
C
        IFLAGW=0
        IF(ICOM.EQ.'WS  ')IFLAGW=1
        IF(ICOM.EQ.'????' .AND. ICOM2.EQ.'?   ')IFLAGW=1
C
C       2015/11: ADD SEARCH OPTION.  CURRENTLY SUPPORT THE FOLLOWING
C                SEARCH ENGINES:
C
C                  1. GOOGLE (THE DEFAULT)
C                  2. BING
C                  3. DUCKDUCKGO
C                  4. WOW (AOL SEARCH ENGINE)
C                  5. YAHOO
C
        IF(IWEBSE.EQ.'BING' .OR. IWEBSE.EQ.'GOOG' .OR.
     1     IWEBSE.EQ.'WOW' .OR. IWEBSE.EQ.'YAHO')THEN
          IF((IFLAGW.EQ.0 .AND. NUMARG.EQ.1) .OR.
     1      (IFLAGW.EQ.1 .AND. NUMARG.EQ.0))THEN
            IF(IWEBSE.EQ.'BING')THEN
              ISTRIN='https://www.bing.com/'
              NCURL=21
            ELSEIF(IWEBSE.EQ.'WOW ')THEN
              ISTRIN='http://www.wow.com/'
              NCURL=19
            ELSEIF(IWEBSE.EQ.'YAHO')THEN
              ISTRIN='https://search.yahoo.com/'
              NCURL=25
            ELSE
              ISTRIN='https://www.google.com/'
              NCURL=23
            ENDIF
          ELSE
            IF(IWEBSE.EQ.'BING')THEN
              ISTRIN='https://www.bing.com/search?q='
              NCURL=30
            ELSEIF(IWEBSE.EQ.'WOW ')THEN
              ISTRIN='http://www.wow.com/search?q='
              NCURL=28
            ELSEIF(IWEBSE.EQ.'YAHO')THEN
              ISTRIN='https://search.yahoo.com/search?q='
              NCURL=34
            ELSE
              ISTRIN='https://www.google.com/search?q='
              NCURL=32
            ENDIF
C
            IFRST=2
            IF(IFLAGW.EQ.1)IFRST=1
            DO211I=IFRST,NUMARG
              IF(I.GT.IFRST)THEN
                ISTRIN(NCURL+1:NCURL+1)='+'
                NCURL=NCURL+1
              ENDIF
              ISTRIN(NCURL+1:NCURL+4)=IHARG(I)
              ISTRIN(NCURL+5:NCURL+8)=IHARG2(I)
              NCURL=NCURL+8
              ISTRT=NCURL
              ISTOP=NCURL-8
              DO213JJ=ISTRT,ISTOP,-1
                IF(ISTRIN(JJ:JJ).NE.' ')THEN
                  NCURL=JJ
                  GOTO219
                ENDIF
  213         CONTINUE
              NCURL=NCURL-8
  219         CONTINUE
  211       CONTINUE
C
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')THEN
              WRITE(ICOUT,216)NCURL,ISTRIN(1:MIN(100,NCURL))
  216         FORMAT('AT 211: NCURL,ISTRIN = ',I5,2X,A100)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
C           IF "SET WEB SEARCH DATAPLOT" SWITCH IS SET TO ON, THEN
C           ADD "DATAPLOT" TO SEARCH STRING.
C
            IF(IWBSDP.EQ.'ON')THEN
              ISTRIN(NCURL+1:NCURL+1)='+'
              NCURL=NCURL+1
              ISTRIN(NCURL+1:NCURL+8)='DATAPLOT'
              NCURL=NCURL+8
            ENDIF
          ENDIF
        ELSEIF(IWEBSE.EQ.'DUCK')THEN
          ISTRIN='https://www.duckduckgo.com/'
          NCURL=27
          DO221I=2,NUMARG
            IF(I.GT.2)THEN
              ISTRIN(NCURL+1:NCURL+1)='+'
              NCURL=NCURL+1
            ENDIF
            ISTRIN(NCURL+1:NCURL+4)=IHARG(I)
            ISTRIN(NCURL+5:NCURL+8)=IHARG2(I)
            NCURL=NCURL+8
            ISTRT=NCURL
            ISTOP=NCURL-8
            DO223JJ=ISTRT,ISTOP,-1
              IF(ISTRIN(JJ:JJ).NE.' ')THEN
                NCURL=JJ
                GOTO229
              ENDIF
  223       CONTINUE
            NCURL=NCURL-8
  229       CONTINUE
  221     CONTINUE
        ENDIF
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'NIST')THEN
        NCURL=21
        ISTRIN='https://www.nist.gov/'
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'SED ')THEN
        NCURL=28
        ISTRIN='https://www.nist.gov/itl/sed'
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'ITL ')THEN
        NCURL=25
        ISTRIN='https://www.nist.gov/itl/'
CCCCC ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'SIMA')THEN
CCCCC   NCURL=49
CCCCC   ISTRIN='http://www.mel.nist.gov/div826/msid/sima/sima.htm'
CCCCC ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'HPCC')THEN
CCCCC   NCURL=20
CCCCC   ISTRIN='http://www.hpcc.gov/'
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'SEMA')THEN
        NCURL=24
        ISTRIN='http://www.sematech.org/'
      ELSEIF(NUMARG.GE.1.AND.
     1      (IHARG(1).EQ.'JJF ' .OR. IHARG(1).EQ.'FILL'))THEN
        NCURL=40
        ISTRIN='http:/stat.nist.gov/~filliben/index.html'
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'HECK')THEN
        NCURL=39
        ISTRIN='http:/stat.nist.gov/~heckert/index.html'
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'HAND')THEN
        IHB=1
      ELSEIF(NUMARG.GE.1.AND.IHARG(1).EQ.'DATA'.AND.
     1       IHARG2(1).EQ.'PLOT')THEN
        GOTO9000
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,291)IHB,NCURL
  291   FORMAT('IHB,NCURL=',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 2--                                        **
C               **  ADD BROWSER TO COMMAND STRING                   **
C               ******************************************************
C
      ISTEPN='52.1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2019/12: FOR MICROSOFT EDGE BROWSER, USE
C
C                  START  MICROSOFT-EDGE:<url>
C
C              CHECK FOR "EDGE" AT END OF BROWSER STRING
C
      ICALL=' '
C
      DO2000I=MAXBRO,1,-1
         NUMBRO=I
         IF(IBROWS(I:I).NE.' ')GOTO2009
 2000 CONTINUE
 2009 CONTINUE
C
      IFLAGE=0
      IF(NUMBRO.GE.4 .AND.
     1   (IBROWS(NUMBRO-4:NUMBRO-1).EQ.'EDGE' .OR.
     1    IBROWS(NUMBRO-4:NUMBRO-1).EQ.'edge'))THEN
        IFLAGE=1
        ICALL(1:21)='start microsoft-edge:'
        NCSTR=21
      ELSEIF(NUMBRO.GT.0)THEN
        ICALL(1:NUMBRO)=IBROWS(1:NUMBRO)
        NCSTR=NUMBRO+1
        ICALL(NCSTR:NCSTR)=' '
      ELSE
        IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
          ICALL(1:NCPROF)=PROFIL(1:NCPROF)
          NCSTR=NCPROF
          ICALL(NCSTR+1:32)='\Internet Explorer\iexplore.exe '
          NCSTR=NCSTR+32
        ELSEIF(IOPSY2.EQ.'MAC')THEN
          ICALL(1:4)='open'
          NCSTR=4
        ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
          ICALL(1:8)='xdg-open'
          NCSTR=8
        ENDIF
      ENDIF
C
C     CHECK IF BROWSWER SET TO INTERNET EXPLORER
C
CCCCC IBRWFL='NETS'
CCCCC IF(NUMBRO.GE.8)THEN
CCCCC   DO2025I=1,NUMBRO-7
CCCCC     IF(IBROWS(I:I+7).EQ.'IEXPLORE' .OR.
CCCCC1       IBROWS(I:I+7).EQ.'iexplore')THEN
CCCCC        IBRWFL='IEXP'
CCCCC        GOTO2028
CCCCC     ENDIF
C2025   CONTINUE
C2028   CONTINUE
CCCCC ENDIF
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  CHECK FOR URL ON COMMAND LINE.  IF NOT FOUND,   **
C               **  CHECK FOR IURL VARIABLE FROM PRIOR SET URL      **
C               ******************************************************
C
      IF(NCURL.GT.0.AND.IHB.EQ.0)GOTO3099
C
      DO3010I=1,128
        ICANS(I:I)=IANSLC(I)
 3010 CONTINUE
C
      IF(NUMARG.LT.1)THEN
        NCSTRI=0
        GOTO3019
      ENDIF
      ISTART=1
      ISTOP=IWIDTH
      IWORD=2
      IF(IFLAGW.EQ.1)IWORD=IWORD-1
      IF(IHB.EQ.1.AND.NUMARG.GT.1)THEN
        IWORD=IWORD+1
      ELSEIF(IHB.EQ.1.AND.NUMARG.LE.1)THEN
        NCSTRI=0
        GOTO3099
      ENDIF 
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')THEN
        WRITE(ICOUT,3016)ISTART,ISTOP,IWORD,IFLAGW
 3016   FORMAT('BEFORE DPEXWO: ISTART,ISTOP,IWORD,IFLAGW = ',4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IFILQ2=IFILQU
      IHYPS2=IHYPSW
      IFILQU='ON'
      IHYPSW='OFF'
      ISTRIN=' '
      NCSTRI=0
      CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,ICOL1,ICOL2,
     1            ISTRIN,NCSTRI,IBUGS2,ISUBRO,IERROR)
      IFILQU=IFILQ2
      IHYPSW=IHYPS2
C
C     2019/11: STRIP OFF BEGINNING AND ENDING QUOTES IF FOUND.
C
      IF(ISTRIN(NCSTRI:NCSTRI).EQ.'"')THEN
        ISTRIN(NCSTRI:NCSTRI)=' '
        NCSTRI=NCSTRI-1
      ENDIF
      IF(ISTRIN(1:1).EQ.'"')THEN
        ISTRIN(1:NCSTRI-1)=ISTRIN(2:NCSTRI)
        NCSTRI=NCSTRI-1
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')THEN
        WRITE(ICOUT,3017)NCSTRI,ISTRIN(1:NCSTRI)
 3017   FORMAT('AFTER DPEXWO: NCSTRI,ISTRIN = ',I8,2X,80A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 3019 CONTINUE
      IF(NCSTRI.LE.0)THEN
        DO3050I=80,1,-1
          NCSTRI=I
          IF(IURL(I:I).NE.' ')GOTO3059
 3050   CONTINUE
 3059   CONTINUE
        IF(NCSTRI.GT.0)THEN
          ISTRIN(1:NCSTRI)=IURL(1:NCSTRI)
        ELSE
          NCSTRI=21
          ISTRIN(1:NCSTRI)='https://www.nist.gov/'
        ENDIF
      ENDIF
C
 3099 CONTINUE
C
C  IF "SET NETSCAPE OLD" COMMAND WAS ENTERED, THEN USE 
C  -remote NETSCAPE OPTION.  THIS ONLY APPLIES TO UNIX PLATFORMS.
C
C  2014/10: COMMENT THIS SECTION OUT FOR NOW AS THIS APPLIES
C           TO AN OBSOLETE BROWSER.
C
CCCCC IF(IHOST1.EQ.'IBM-')THEN
CCCCC   IF(IBRWFL.EQ.'NETS')THEN
CCCCC     NCSTR=NCSTR+1
CCCCC     NCSTR2=NCSTR+3
CCCCC     ICALL(NCSTR:NCSTR2)=' -h '
CCCCC     NCSTR=NCSTR2
CCCCC   ENDIF
CCCCC   GOTO5129 
CCCCC ENDIF
C
CCCCC IF(INETSW.EQ.'OLD'.AND.IHOST1.NE.'IBM-')THEN
CCCCC   NCSTR=NCSTR+1
CCCCC   NCSTR2=NCSTR+8
CCCCC   ICALL(NCSTR:NCSTR2)=' -remote '
CCCCC   NCSTR=NCSTR2+1
CCCCC   ICALL(NCSTR:NCSTR)=IQUOTE
CCCCC   NCSTR=NCSTR+1
CCCCC   NCSTR2=NCSTR+7
CCCCC   ICALL(NCSTR:NCSTR2)='openURL('
CCCCC   NCSTR=NCSTR2
CCCCC ENDIF
C
      IF(IHB.EQ.1)THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NCURL-1
        ICALL(NCSTR:NCSTR2)=IHBURL(1:NCURL)
        NCSTR=NCSTR2
        IF(NCSTRI.GT.0)THEN
          NCSTR=NCSTR+1
          NCSTR2=NCSTR+NCSTRI-1
          ICALL(NCSTR:NCSTR2)=ISTRIN(1:NCSTRI)
          NCSTR=NCSTR2
        ENDIF
      ELSEIF(NCURL.GT.0)THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NCURL-1
        ICALL(NCSTR:NCSTR2)=ISTRIN(1:NCURL)
        NCSTR=NCSTR2
      ELSE
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NCSTRI-1
        ICALL(NCSTR:NCSTR2)=ISTRIN(1:NCSTRI)
        NCSTR=NCSTR2
      ENDIF
C
C               ****************************************************
C               **  STEP 53--                                     **
C               **  USE DPSYS2 TO MAKE A SYSTEM CALL              **
C               ****************************************************
C
      ISTEPN='53'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC IF(INETSW.EQ.'OLD'.AND.IHOST1.NE.'IBM-')THEN
CCCCC   NCSTR=NCSTR+1
CCCCC   ICALL(NCSTR:NCSTR)=')'
CCCCC   NCSTR=NCSTR+1
CCCCC   ICALL(NCSTR:NCSTR)=IQUOTE
CCCCC ENDIF
      IF(IHOST1.NE.'IBM-')THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+1
        ICALL(NCSTR:NCSTR2)=' &'
        NCSTR=NCSTR2
      ENDIF
C
CCCCC IF(INETSW.EQ.'NEW')THEN
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,5411)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   WRITE(ICOUT,999)
CCCCC   CALL DPWRST('XXX','BUG ')
CCCCC   IF(IHOST1.NE.'IBM-')THEN
CCCCC     WRITE(ICOUT,5412)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,5413)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,5414)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,999)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,5415)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,999)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC   ENDIF
CCCCC ENDIF
C5411 FORMAT('*****NOTE: IT MAY TAKE THE BROWSER A FEW MOMENTS TO ',
CCCCC1      'START UP.')
C5412 FORMAT('     IF YOU ARE USING THE NETSCAPE BROWSER, YOU CAN ',
CCCCC1       'SPEED UP SUBSEQUENT')
C5413 FORMAT('     USE OF WEB HELP BY ENTERING THE FOLLOWING DATAPLOT',
CCCCC1       ' COMMAND')
C5414 FORMAT('     (LEAVE THE BROWSER OPEN):')
C5415 FORMAT('         SET NETSCAPE OLD')
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ICLESV=ICLEWT
      ISYSPE='OFF'
      ISYSHI='ON'
      ICLEWT='OFF'
      CALL DPSYS2(ICALL,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      ICLEWT=ICLESV
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WEB ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWEB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IERRO2,IFOUND,IFLAGW
 9012   FORMAT('IERROR,IERRO2,IFOUND,IFLAGW = ',3(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9097)ICALL(1:128)
 9097   FORMAT('ICALL = ',A128)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9099)ICALL(129:256)
 9099   FORMAT('ICALL = ',A128)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWEIB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,MAXNPP,
     1                  IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1                  IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--FORM A WEIBULL PLOT
C              (USEFUL FOR RELIABILITY AND LIFE-TESTING).
C     EXAMPLE--WEIBULL PLOT Y TAG
C              WEIBULL PLOT Y
C     NOTE--NORMALLY THIS COMMAND HAS 2 ARGUMENTS--
C           ARGUMENT 1 IS THE RESPONSE VARIABLE
C           ARGUMENT 2 IS THE CENSOR-TAG VARIABLE
C           IF THE WEIBULL PLOT COMMAND HAS ONLY ONE ARGUMENT,
C           THEN IT IS ASSUMED THAT ALL OF THE DATA IS TO BE INCLUDED
C           (THAT IS, NO CENSORING).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/6
C     ORIGINAL VERSION--JUNE      1987.
C     UPDATED         --JUNE      1990. TEMPORARY ARRAYS TO GARBAGE COMMON
C     UPDATED         --APRIL     1992. DEFINE CUTOFF
C     UPDATED         --MAY       1995. ADD LINE TO EQUIVALENCE
C     UPDATED         --FEBRUARY  2011. USE DPPARS, DPPAR3
C     UPDATED         --FEBRUARY  2011. SUPPORT FOR "HIGHLIGHTED" OPTION
C     UPDATED         --JUNE      2011. SUPPORT "BRITTLE FIBER WEIBULL"
C                                       PLOT
C     UPDATED         --OCTOBER   2013. SUPPORT "FRECHET PLOT"
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
C
      CHARACTER*4 IX1TSV
      CHARACTER*4 IX2TSV
      CHARACTER*4 IY1TSV
      CHARACTER*4 IY2TSV
C
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHIGH
      CHARACTER*4 ICASE
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
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
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
      DIMENSION YS(MAXOBV)
      DIMENSION TAGC2(MAXOBV)
      DIMENSION ITAGC2(MAXOBV)
      DIMENSION WAR(MAXOBV)
      DIMENSION WMR(MAXOBV)
      DIMENSION WMRT(MAXOBV)
      DIMENSION YST(MAXOBV)
      DIMENSION XHIGH(MAXOBV)
      DIMENSION XDIST(MAXOBV)
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),YS(1))
      EQUIVALENCE (GARBAG(IGARB4),TAGC2(1))
      EQUIVALENCE (GARBAG(IGARB5),YST(1))
      EQUIVALENCE (GARBAG(IGARB6),WAR(1))
      EQUIVALENCE (GARBAG(IGARB7),WMRT(1))
      EQUIVALENCE (GARBAG(IGARB8),WMR(1))
      EQUIVALENCE (GARBAG(IGARB9),XHIGH(1))
      EQUIVALENCE (GARBAG(IGAR10),XDIST(1))
      EQUIVALENCE (IGARBG(IIGAR1),ITAGC2(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWE'
      ISUBN2='IB  '
      IFOUND='NO'
      IERROR='NO'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      IVAL=0
C
      BETA=(-999.0)
      ETA=(-999.0)
      SDBETA=(-999.0)
      SDETA=(-999.0)
      BPT1=(-999.0)
      BPT5=(-999.0)
      B1=(-999.0)
      B5=(-999.0)
      B10=(-999.0)
      B20=(-999.0)
      B50=(-999.0)
      B80=(-999.0)
      B90=(-999.0)
      B95=(-999.0)
      B99=(-999.0)
      B995=(-999.0)
      B999=(-999.0)
C
CCCCC THE FOLLOWING 4 LINES WERE ADDED   APRIL 1992 (ALAN)
      ICUTMX=NUMBPW
      IF(IHOST1.EQ.'CDC '.OR.IHOST1.EQ.'CYBE')ICUTMX=48
      IF(IHOST1.EQ.'205 ')ICUTMX=48
      CUTOFF=2**(ICUTMX-3)
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWEIB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,MAXNPP
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN,MAXNPP = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IANGLU,IBUGG2,IBUGG3,IBUGQ,ISUBRO
   54   FORMAT('IANGLU,IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IX1TSC,IX2TSC,IY1TSC,IY2TSC
   61   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IX1TSV,IX2TSV,IY1TSV,IY2TSV
   62   FORMAT('IX1TSV,IX2TSV,IY1TSV,IY2TSV = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFOUND='NO'
      IHIGH='OFF'
      ICASPL='WEIB'
      IF(ICOM.EQ.'WEIB')THEN
        IF(IHARG(1).EQ.'HIGH' .AND. IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IFOUND='YES'
          IHIGH='ON'
        ELSEIF(IHARG(1).EQ.'PLOT')THEN
          ILASTC=1
          IFOUND='YES'
        ENDIF
      ELSEIF(ICOM.EQ.'FREC')THEN
        ICASPL='FREC'
        IF(IHARG(1).EQ.'HIGH' .AND. IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IFOUND='YES'
          IHIGH='ON'
        ELSEIF(IHARG(1).EQ.'PLOT')THEN
          ILASTC=1
          IFOUND='YES'
        ENDIF
      ELSEIF(ICOM.EQ.'BRIT' .AND. IHARG(1).EQ.'FIBE' .AND.
     1       IHARG(2).EQ.'WEIB')THEN
        IF(IHARG(3).EQ.'HIGH' .AND. IHARG(4).EQ.'PLOT')THEN
          ILASTC=4
          IFOUND='YES'
          IHIGH='ON'
          ICASPL='BFWE'
        ELSEIF(IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          IFOUND='YES'
          ICASPL='BFWE'
        ENDIF
      ELSEIF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IHIGH='ON'
        IF(IHARG(1).EQ.'WEIB' .AND. IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IFOUND='YES'
        ELSEIF(IHARG(1).EQ.'FREC' .AND. IHARG(2).EQ.'PLOT')THEN
          ILASTC=2
          IFOUND='YES'
          ICASPL='FREC'
        ELSEIF(IHARG(1).EQ.'BRIT' .AND. IHARG(2).EQ.'FIBE' .AND.
     1         IHARG(3).EQ.'WEIB' .AND. IHARG(4).EQ.'PLOT')THEN
          ILASTC=4
          IFOUND='YES'
          ICASPL='BFWE'
        ENDIF
      ENDIF
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='WEIBULL PLOT'
      IF(ICASPL.EQ.'FREC')INAME='FRECHET PLOT'
      MINNA=1
      MAXNA=100
      MINN2=1
      IFLAGE=1
      IFLAGM=1
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=1
      MAXNVA=2
      IF(IHIGH.EQ.'ON')THEN
        MINNVA=2
        MAXNVA=3
      ENDIF
      IF(ICASPL.EQ.'FREC')MAXNVA=MAXNVA-1
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')THEN
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
      DO290I=1,NRIGHT(1)
        Y2(I)=1.0
        XHIGH(I)=1.0
  290 CONTINUE
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y2,XHIGH,NS,NLOCA2,NLOCA3,ICASE,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IHIGH.EQ.'ON' .AND. NUMVAR.EQ.2)THEN
        DO299I=1,NS
          XHIGH(I)=Y2(I)
          Y2(I)=1.0
  299   CONTINUE
      ENDIF
C
C               *********************************************
C               **  STEP 34--                              **
C               **  CHECK TO MAKE SURE THAT THE            **
C               **  COMBINATION OF CENSORING AND           **
C               **  SUBSETTING DOES NOT RESULT IN          **
C               **  TOO FEW DATA POINTS RESULTING          **
C               **  (AT LEAST 2)                           **
C               **  WITH WHICH TO FORM A WEIBULL PLOT.     **
C               *********************************************
C
      ISTEPN='34'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOUNT=0
      DO3400I=1,NS
        IF(Y2(I).LE.-0.000001.OR.Y2(I).GE.0.0001)ICOUNT=ICOUNT+1
 3400 CONTINUE
C
      IF(ICOUNT.LE.MINN2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3451)
 3451   FORMAT('***** ERROR IN WEIBULL PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3452)
 3452   FORMAT('      AFTER THE SPECIFIED CENSORING AND SUBSETTING ',
     1         'HAS BEEN DONE,')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3454)IVARN1(1),IVARN2(1)
 3454   FORMAT('      THE NUMBER OF OBSERVATIONS REMAINING FROM ',
     1         'VARIABLE ',A4,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3455)
 3455   FORMAT('      (FOR WHICH A WEIBULL PLOT IS TO BE FORMED)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3457)MINN2
 3457   FORMAT('      MUST BE ',I8,' OR LARGER;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3458)ICOUNT
 3458   FORMAT('      SUCH WAS NOT THE CASE HERE (ICOUNT = ',I8,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3459)
 3459   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3460)(IANS(I),I=1,MIN(80,IWIDTH))
 3460     FORMAT('      ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
C
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC JUNE, 1990.  DIMENSIONS FOR YS - YST NOW DONE IN DPWEIB
      IF(ICASPL.EQ.'WEIB')THEN
        CALL DPWEI2(Y1,Y2,XHIGH,NS,ICASPL,MAXN,IHIGH,
     1              IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1              IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1              BETA,ETA,SDBETA,SDETA,
     1              BPT1,BPT5,B1,B5,B10,B20,B50,B80,B90,B95,
     1              B99,B995,B999,
     1              YS,TAGC2,ITAGC2,WAR,WMR,WMRT,YST,XDIST,
     1              Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ELSEIF(ICASPL.EQ.'FREC')THEN
        CALL DPWEI3(Y1,XHIGH,NS,ICASPL,MAXN,IHIGH,MAXOBV,
     1              XDIST,YS,
     1              SHAPE,SCALE,SDSHAP,SDSCAL,
     1              BPT1,BPT5,B1,B5,B10,B20,B50,B80,B90,B95,
     1              B99,B995,B999,
     1              Y,X,D,NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
      ENDIF
C
C               ***************************************
C               **  STEP 51--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='51'
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'WEIB')
     1   CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO5100IPASS=1,17
        IF(ICASPL.EQ.'WEIB')THEN
          IF(IPASS.EQ.1)THEN
            IH='BETA'
            IH2='    '
          ELSEIF(IPASS.EQ.2)THEN
            IH='ETA'
            IH2='    '
          ELSEIF(IPASS.EQ.3)THEN
            IH='SDBE'
            IH2='TA  '
          ELSEIF(IPASS.EQ.4)THEN
            IH='SDET'
            IH2='A   '
          ENDIF
        ELSEIF(ICASPL.EQ.'FREC')THEN
          IF(IPASS.EQ.1)THEN
            IH='SHAP'
            IH2='E   '
          ELSEIF(IPASS.EQ.2)THEN
            IH='SCAL'
            IH2='    '
          ELSEIF(IPASS.EQ.3)THEN
            IH='SDSH'
            IH2='APE '
          ELSEIF(IPASS.EQ.4)THEN
            IH='SDSC'
            IH2='ALE '
          ENDIF
        ENDIF
C
        IF(IPASS.EQ.5)THEN
          IH='BPT1'
          IH2='    '
        ELSEIF(IPASS.EQ.6)THEN
          IH='BPT5'
          IH2='    '
        ELSEIF(IPASS.EQ.7)THEN
          IH='B1  '
          IH2='    '
        ELSEIF(IPASS.EQ.8)THEN
          IH='B5  '
          IH2='    '
        ELSEIF(IPASS.EQ.9)THEN
          IH='B10 '
          IH2='    '
        ELSEIF(IPASS.EQ.10)THEN
          IH='B20 '
          IH2='    '
        ELSEIF(IPASS.EQ.11)THEN
          IH='B50 '
        ELSEIF(IPASS.EQ.11)THEN
          IH2='    '
        ELSEIF(IPASS.EQ.12)THEN
          IH='B80 '
          IH2='    '
        ELSEIF(IPASS.EQ.13)THEN
          IH='B90 '
          IH2='    '
        ELSEIF(IPASS.EQ.14)THEN
          IH='B95 '
          IH2='    '
        ELSEIF(IPASS.EQ.15)THEN
          IH='B99 '
          IH2='    '
        ELSEIF(IPASS.EQ.16)THEN
          IH='B995'
          IH2='    '
        ELSEIF(IPASS.EQ.17)THEN
          IH='B999'
          IH2='    '
        ENDIF
C
        DO5150I=1,NUMNAM
          I2=I
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')THEN
            ILOC=I2
            GOTO5180
          ENDIF
 5150   CONTINUE
C
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,3451)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5151)MAXNAM
 5151     FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES (',
     1           I8,')')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5153)
 5153     FORMAT('      HAS JUST BEEN EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,3460)(IANS(I),I=1,MIN(80,IWIDTH))
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        NUMNAM=NUMNAM+1
        ILOC=NUMNAM
        IHNAME(ILOC)=IH
        IHNAM2(ILOC)=IH2
        IUSE(ILOC)='P'
C
 5180   CONTINUE
        IF(ICASPL.EQ.'WEIB')THEN
          IF(IPASS.EQ.1)VALUE(ILOC)=BETA
          IF(IPASS.EQ.2)VALUE(ILOC)=ETA
          IF(IPASS.EQ.3)VALUE(ILOC)=SDBETA
          IF(IPASS.EQ.4)VALUE(ILOC)=SDETA
        ELSEIF(ICASPL.EQ.'FREC')THEN
          IF(IPASS.EQ.1)VALUE(ILOC)=SHAPE
          IF(IPASS.EQ.2)VALUE(ILOC)=SCALE
          IF(IPASS.EQ.3)VALUE(ILOC)=SDSHAP
          IF(IPASS.EQ.4)VALUE(ILOC)=SDSCAL
        ENDIF
        IF(IPASS.EQ.5)VALUE(ILOC)=BPT1
        IF(IPASS.EQ.6)VALUE(ILOC)=BPT5
        IF(IPASS.EQ.7)VALUE(ILOC)=B1
        IF(IPASS.EQ.8)VALUE(ILOC)=B5
        IF(IPASS.EQ.9)VALUE(ILOC)=B10
        IF(IPASS.EQ.10)VALUE(ILOC)=B20
        IF(IPASS.EQ.11)VALUE(ILOC)=B50
        IF(IPASS.EQ.12)VALUE(ILOC)=B80
        IF(IPASS.EQ.13)VALUE(ILOC)=B90
        IF(IPASS.EQ.14)VALUE(ILOC)=B95
        IF(IPASS.EQ.15)VALUE(ILOC)=B99
        IF(IPASS.EQ.16)VALUE(ILOC)=B995
        IF(IPASS.EQ.17)VALUE(ILOC)=B999
        VAL=VALUE(ILOC)
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
        IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
        IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
        IVALUE(ILOC)=IVAL
C
 5100 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'WEIB')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWEIB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         I8,I8,I8,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)ICASPL,MAXN,NUMVAR,ICOUNT
 9014   FORMAT('ICASPL,MAXN,NUMVAR,ICOUNT = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GE.1)THEN
          DO9020I=1,NPLOTP
            WRITE(ICOUT,9021)I,Y(I),X(I),D(I)
 9021       FORMAT('I,Y(I),X(I),D(I) = ',I8,3F12.5)
            CALL DPWRST('XXX','BUG ')
 9020     CONTINUE
        ENDIF
        WRITE(ICOUT,9041)IX1TSC,IX2TSC,IY1TSC,IY2TSC
 9041   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)IX1TSV,IX2TSV,IY1TSV,IY2TSV
 9042   FORMAT('IX1TSV,IX2TSV,IY1TSV,IY2TSV = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)BETA,ETA,SDBETA,SDETA
 9043   FORMAT('BETA,ETA,SDBETA,SDETA = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        DO9050I=1,NS
          WRITE(ICOUT,9051)I,Y1(I),Y2(I),ISUB(I)
 9051     FORMAT('I,Y1(I),Y2(I),ISUB(I) = ',I8,2E15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9050   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWEI2(Y,TAGC,XHIGH,N,ICASPL,MAXN,IHIGH,
     1                  IX1TSC,IX2TSC,IY1TSC,IY2TSC,
     1                  IX1TSV,IX2TSV,IY1TSV,IY2TSV,
     1                  BETA,ETA,SDBETA,SDETA,
     1                  BPT1,BPT5,B1,B5,B10,B20,B50,B80,B90,
     1                  B95,B99,B995,B999,
     1                  YS,TAGC2,ITAGC2,WAR,WMR,WMRT,YST,XDIST,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
CCCCC JUNE, 1990. YS - YST NOW DIMENSIONED IN DPWEIB
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE
C              A WEIBULL PLOT.
C              THE PLOT WILL CONSIST OF 6 COMPONENTS--
C                  1) THE RAW DATA
C                  2) THE FITTED LINE
C                  3) THE HORIZONTAL 63.2% LINE
C                  4) THE VERTICAL   63.2% LINE
C                  5) 95% CONFIDENCE LIMITS
C                  6) 99% CONFIDENCE LIMITS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/6
C     ORIGINAL VERSION--JUNE      1987.
C     UPDATED         --FEBRUARY  1988.  (ERROR TRAP FOR NON-POSITIVE DATA)
C     UPDATED         --JUNE      1990.  SOME DIMENSIONS NOW DONE IN DPWEIB
C     UPDATED         --APRIL     1992.  YMIN/2/3/4/ TO XMIN/2/3/4/
C     UPDATED         --NOVEMBER  1992.  CHARACTER*4 ICASPL
C     UPDATED         --FEBRUARY  2011.  SUPPORT FOR HIGHLIGHT OPTION
C     UPDATED         --JUNE      2011.  SUPPORT FOR BRITTLE FIBER
C                                        WEIBULL
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IX1TSC
      CHARACTER*4 IX2TSC
      CHARACTER*4 IY1TSC
      CHARACTER*4 IY2TSC
C
      CHARACTER*4 IX1TSV
      CHARACTER*4 IX2TSV
      CHARACTER*4 IY1TSV
      CHARACTER*4 IY2TSV
C
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1992
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION TAGC(*)
      DIMENSION XHIGH(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
CCCCC JUNE, 1990.  FOLLOWING NOW DIMENSIONED IN DPWEIB
CCCCC DIMENSION YS(MAXOBV)
CCCCC DIMENSION TAGC2(MAXOBV)
CCCCC DIMENSION ITAGC2(MAXOBV)
CCCCC DIMENSION WAR(MAXOBV)
CCCCC DIMENSION WMR(MAXOBV)
CCCCC DIMENSION WMRT(MAXOBV)
CCCCC DIMENSION YST(MAXOBV)
      DIMENSION YS(*)
      DIMENSION TAGC2(*)
      DIMENSION ITAGC2(*)
      DIMENSION WAR(*)
      DIMENSION WMR(*)
      DIMENSION WMRT(*)
      DIMENSION YST(*)
      DIMENSION XDIST(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWE'
      ISUBN2='I2  '
      IERROR='NO'
      IWRITE='OFF'
C
      XMAX3=0.0
      AN=N
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWEI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO
   52   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IHIGH,MAXN,N,NPLOTV
   53   FORMAT('ICASPL,IHIGH,MAXN,N,NPLOTV = ',A4,2X,A4,3I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO60I=1,N
            WRITE(ICOUT,61)I,Y(I),TAGC(I),XHIGH(I)
   61       FORMAT('I,Y(I),TAGC(I),XHIGH(I) = ',I8,3G15.7)
            CALL DPWRST('XXX','BUG ')
   60     CONTINUE
        ENDIF
        WRITE(ICOUT,71)IX1TSC,IX2TSC,IY1TSC,IY2TSC
   71   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IX1TSV,IX2TSV,IY1TSV,IY2TSV
   72   FORMAT('IX1TSV,IX2TSV,IY1TSV,IY2TSV = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN WEIBULL PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)N
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1130I=1,N
        IF(Y(I).NE.HOLD)GOTO1139
 1130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ALL THE INPUT RESPONSE VARIABLE ELEMENTS ARE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
      DO1140I=1,N
        IF(Y(I).NE.0.0)GOTO1149
 1140 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1142)
 1142 FORMAT('      ALL INPUT TAG VARIABLE ELEMENTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1143)
 1143 FORMAT('      ARE IDENTICALLY EQUAL TO 0.0;')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1144)
 1144 FORMAT('      THUS THERE ARE NO RESPONSE VARIABLE VALUES ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1145)
 1145 FORMAT('      REMAINING UPON WHICH TO DO A WEIBULL ANALYSIS.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1149 CONTINUE
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  SORT THE DATA AND CARRY ALONG THE TAG    **
C               ***********************************************
C
      IF(IHIGH.EQ.'ON')THEN
        CALL SORTC(Y,XHIGH,N,YS,TAGC2)
        DO2010I=1,N
          XHIGH(I)=TAGC2(I)
 2010   CONTINUE
        CALL DISTIN(XHIGH,N,IWRITE,XDIST,NDIST,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ELSE
        NDIST=1
        DO2013I=1,N
          XHIGH(I)=1.0
 2013   CONTINUE
      ENDIF
C
      CALL SORTC(Y,TAGC,N,YS,TAGC2)
C
      DO2100I=1,N
       ITAGC2(I)=INT(TAGC2(I)+0.1)
 2100 CONTINUE
C
C
C               ***********************************************
C               **  STEP 22--                                **
C               **  COMPUTE WEIBULL ADUSTED RANKS            **
C               ***********************************************
C
C               -----------------------------------------------
C               SET INITIAL VALUE FOR SAVED ADJUSTED RANK.
C               SET INITIAL VALUE FOR RANK INCREMENT.
C               -----------------------------------------------
C
      SAVEAR=0.0
C
      I=0
      ANUM=(AN+1.0)-SAVEAR
      ADENOM=1+(N-I)
      RANINC=ANUM/ADENOM
C
      NVALID=0
      DO2200I=1,N
        IF(ITAGC2(I).EQ.1)THEN
C
C         -----------------------------------------------
C         TREAT THE VALID (TO BE INCLUDED) ITEM CASE.
C         COMPUTE THE ADJUSTED RANK.
C         SAVE THE ADJUSTED RANK.
C         DO NOT RECOMPUTE THE RANK INCREMENT.
C         -----------------------------------------------
C
          NVALID=NVALID+1
C
          IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI2')THEN
            WRITE(ICOUT,2211)I,YS(I),TAGC2(I),ITAGC2(I),WAR(I)
 2211       FORMAT('I,YS(I),TAGC2(I),ITAGC2(I),WAR(I) = ',I8,2G15.7,
     1             I8,G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          WAR(I)=SAVEAR+RANINC
          SAVEAR=WAR(I)
        ELSE
C
C         -----------------------------------------------
C         TREAT THE SUSPENDED (= CENSORED) ITEM CASE
C         RECOMPUTE THE RANK INCREMENT.
C         DO NOT RECOMPUTE THE SAVED ADJUSTED RANK.
C         -----------------------------------------------
C
          ANUM=(AN+1.0)-SAVEAR
          ADENOM=1+(N-I)
          RANINC=ANUM/ADENOM
        ENDIF
 2200 CONTINUE
C
C               ************************************
C               **  STEP 23--                     **
C               **  DETERMINE THE NUMBER OF       **
C               **  "GOOD"                        **
C               **  = NON-CENSORED/NON-SUSPENDED  **
C               **  DATA VALUES.                  **
C               ************************************
C
      NSUB=0
      DO2300I=1,N
        IF(ITAGC2(I).EQ.0)GOTO2300
        NSUB=NSUB+1
 2300 CONTINUE
      ANSUB=NSUB
C
C               ****************************************
C               **  STEP 24--                         **
C               **  COMPUTE WEIBULL MEDIAN RANKS      **
C               **  (FOR THE GOOD DATA ONLY)          **
C               ****************************************
C
      DO2400I=1,N
        WMR(I)=(-999.0)
        IF(ITAGC2(I).EQ.0)GOTO2400
        WMR(I)=100.0*(WAR(I)-0.3)/(AN+0.4)
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI2')THEN
          WRITE(ICOUT,2411)I,WAR(I),WMR(I)
 2411     FORMAT('I,WAR(I),WMR(I) = ',I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
 2400 CONTINUE
C
C               ****************************************
C               **  STEP 30--                         **
C               **  FIT THE DATA TO ESTIMATE          **
C               **  BETA (= SHAPE PARAMETER) AND      **
C               **  ETA  (= CHARACTERISTIC LIFE)      **
C               ****************************************
C
C               ******************************************
C               **  STEP 31--                           **
C               **  TRANSFORM THE WEIBULL MEDIAN RANKS  **
C               ******************************************
C
      DO3100I=1,N
        WMRT(I)=(-999.0)
        IF(ITAGC2(I).EQ.0)GOTO3100
        ARG1=100.0/(100.0-WMR(I))
        ARG2=LOG(ARG1)
        WMRT(I)=LOG(ARG2)
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI2')THEN
          WRITE(ICOUT,3111)I,ITAGC2(I),WMR(I),WMRT(I)
 3111     FORMAT('I,ITAGC2(I),WMR(I),WMRT(I) = ',2I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 3100 CONTINUE
C
C               ******************************************
C               **  STEP 32--                           **
C               **  TRANSFORM THE SORTED DATA           **
C               ******************************************
C
      DO3200I=1,N
        YST(I)=(-999.0)
        IF(ITAGC2(I).EQ.0)GOTO3200
        IF(YS(I).LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3212)
 3212     FORMAT('      ZERO OR NEGATIVE DATA IS NOT PERMITTED IN A')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3214)
 3214     FORMAT('      WEIBULL PLOT.  THE ILLEGAL VALUE IS ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3215)
 3215     FORMAT('      SUGGESTION--ADD A CONSTANT SO THAT ALL DATA')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3216)
 3216     FORMAT('      IS POSITIVE, AND THEN REDO THE WEIBULL PLOT.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        YST(I)=LOG(YS(I))
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI2')THEN
          WRITE(ICOUT,3221)I,ITAGC2(I),YS(I),YST(I)
 3221     FORMAT('I,ITAGC2(I),YS(I),YST(I) = ',2I8,2E15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 3200 CONTINUE
C
C               ******************************************
C               **  STEP 33--                           **
C               **  CARRY OUT THE FIT OF                **
C               **  TRANSFORMED SORTED DATA VERSUS      **
C               **  TRANSFORMED WEIBULL MEDIAN RANKS    **
C               ******************************************
C
      SUMX=0.0
      SUMY=0.0
      DO3310I=1,N
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI2')THEN
          WRITE(ICOUT,3311)I,ITAGC2(I),YST(I),WMRT(I)
 3311     FORMAT('I,ITAGC2(I),YST(I),WMRT(I) = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ITAGC2(I).EQ.0)GOTO3310
        SUMX=SUMX+WMRT(I)
        SUMY=SUMY+YST(I)
 3310 CONTINUE
      XBAR=SUMX/ANSUB
      YBAR=SUMY/ANSUB
C
      SUMXX=0.0
      SUMYY=0.0
      SUMXY=0.0
      DO3320I=1,N
        IF(ITAGC2(I).EQ.0)GOTO3320
        SUMXX=SUMXX+(WMRT(I)-XBAR)*(WMRT(I)-XBAR)
        SUMYY=SUMYY+(YST(I)-YBAR)*(YST(I)-YBAR)
        SUMXY=SUMXY+(WMRT(I)-XBAR)*(YST(I)-YBAR)
 3320 CONTINUE
      ASLOPE=0.0
      IF(SUMXX.GT.0.0)ASLOPE=SUMXY/SUMXX
      AINTER=YBAR-ASLOPE*XBAR
C
      SUMRR=0.0
      SUMX2=0.0
      DO3330I=1,N
        IF(ITAGC2(I).EQ.0)GOTO3330
        RES=YST(I)-(AINTER+ASLOPE*WMRT(I))
        SUMRR=SUMRR+RES*RES
        SUMX2=SUMX2+WMRT(I)*WMRT(I)
 3330 CONTINUE
      RESVAR=SUMRR/(AN-2.0)
      RESSD=0.0
      IF(RESVAR.GT.0.0)RESSD=SQRT(RESVAR)
      SDINTE=RESSD*SQRT(SUMX2/(AN*SUMXX))
      SDSLOP=RESSD*SQRT(1.0/SUMXX)
C
C               ****************************************
C               **  STEP 34--                         **
C               **  FORM ESTIMATES FOR                **
C               **  BETA (= SHAPE PARAMETER) AND      **
C               **  ETA  (= CHARACTERISTIC LIFE)      **
C               ****************************************
C
      IF(ASLOPE.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3332)
 3332   FORMAT('      THE FITTED SLOPE IS ZERO OR NEGATIVE WHICH WOULD')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3335)
 3335   FORMAT('      YIELD AN IMPOSSIBLE VALUE FOR BETA = 1/SLOPE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3336)ASLOPE,AINTER
 3336   FORMAT('      ASLOPE,AINTER = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3337)SUMX,SUMY,SUMXX,SUMYY,SUMXY
 3337   FORMAT('      SUMX,SUMY,SUMXX,SUMYY,SUMXY = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      BETA=1/ASLOPE
      ETA=EXP(AINTER)
      SDBETA=BETA*BETA*SDSLOP
      SDETA=ETA*SDINTE
C
C               ************************************************
C               **  STEP 35--                                 **
C               **  FORM ESTIMATES FOR                        **
C               **     BPT1= .1%   POINT OF BEST-FIT DIST.    **
C               **     BPT5= .5%   POINT OF BEST-FIT DIST.    **
C               **     B1  =  1%   POINT OF BEST-FIT DIST.    **
C               **     B5  =  5%   POINT OF BEST-FIT DIST.    **
C               **     B10 = 10%   POINT OF BEST-FIT DIST.    **
C               **     B20 = 20%   POINT OF BEST-FIT DIST.    **
C               **     B50 = 50%   POINT OF BEST-FIT DIST.    **
C               **     B80 = 80%   POINT OF BEST-FIT DIST.    **
C               **     B90 = 90%   POINT OF BEST-FIT DIST.    **
C               **     B95 = 95%   POINT OF BEST-FIT DIST.    **
C               **     B99 = 99%   POINT OF BEST-FIT DIST.    **
C               **     B995= 99.5% POINT OF BEST-FIT DIST.    **
C               **     B999= 99.9% POINT OF BEST-FIT DIST.    **
C               ************************************************
C
      P=.001
      BPT1=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.005
      BPT5=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.01
      B1=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.05
      B5=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.10
      B10=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.20
      B20=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.50
      B50=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.80
      B80=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.90
      B90=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.95
      B95=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.99
      B99=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.995
      B995=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
      P=.999
      B999=ETA*(LOG(1.0/(1.0-P)))**(1.0/BETA)
C
C               ****************************************
C               **  STEP 41--                         **
C               **  SAVE OLD SETTINGS FOR             **
C               **     HORIZONTAL AXIS PLOT SCALE     **
C               **     VERTICAL AXIS PLOT SCALE       **
C               **  CHANGE                            **
C               **     HORIZONTAL AXIS PLOT SCALE     **
C               **     TO LOG                         **
C               **  CHANGE                            **
C               **     VERTICAL AXIS PLOT SCALE       **
C               **     TO WEIBULL                     **
C               ****************************************
 
      IX1TSV=IX1TSC
      IX2TSV=IX2TSC
      IY1TSV=IY1TSC
      IY2TSV=IY2TSC
C
      IX1TSC='LOG'
      IX2TSC='LOG'
      IY1TSC='WEIB'
      IY2TSC='WEIB'
C
C               ****************************************
C               **  STEP 42--                         **
C               **  DETERMINE PLOT LIMITS FOR         **
C               **  PREDICTED LINE                    **
C               ****************************************
C
      P2=0.1
      P=P2/100.0
      ARG1=1.0/(1.0-P)
      TERM=LOG(ARG1)
      ARG2=1.0/BETA
      PPF=ETA*TERM**ARG2
      XMIN=PPF
C
      P2=99.9
      P=P2/100.0
      ARG1=1.0/(1.0-P)
      TERM=LOG(ARG1)
      ARG2=1.0/BETA
      PPF=ETA*TERM**ARG2
      XMAX=PPF
C
      XINC=(XMAX-XMIN)/100.0
C
      XMIN2=LOG10(XMIN)
CCCCC XMIN3=AINT(XMIN2)
      IF(XMIN2.GE.0.0)XMIN3=AINT(XMIN2)
      IF(XMIN2.LT.0.0)XMIN3=(-AINT(-XMIN2+1.0))
      XMIN4=10.0**XMIN3+0.001
C
      XMAX2=LOG10(XMAX)
CCCCC XMAX3=AINT(XMAX2)+1.0
      IF(XMAX2.GE.0.0)XMAX3=AINT(XMAX2)
      IF(XMAX2.LT.0.0)XMAX3=(-AINT(-XMAX2+1.0))
      XMAX3=XMAX3+1.0
      XMAX4=10.0**XMAX3-0.001
C
      X632=ETA
C
C               ****************************************
C               **  STEP 51--                         **
C               **  FORM PLOT COORDINATES             **
C               **     RAW (GOOD) DATA                **
C               **     PREDICTED LINE                 **
C               **     HORIZONTAL 63.2% LINE          **
C               **     VERTICAL   63.2% LINE          **
C               **     95% CONFIDENCE BAND            **
C               **     99% CONFIDENCE BAND            **
C               ****************************************
C
      J=0
      DO5110I=1,N
        IF(ITAGC2(I).EQ.0)GOTO5110
        J=J+1
        Y2(J)=WMR(I)
        X2(J)=YS(I)
        IF(NDIST.EQ.1)THEN
          D2(J)=1.0
        ELSE
          IINDX=1
          DO5115K=1,NDIST
            IF(XHIGH(I).EQ.XDIST(K))THEN
              IINDX=K
              GOTO5119
            ENDIF
 5115     CONTINUE
 5119     CONTINUE
          D2(J)=REAL(IINDX)
        ENDIF
 5110 CONTINUE
C
      X=XMIN-XINC
CCCCC MARCH 1996.  CHECK THAT PREDICTED VALUE IS STRICTLY POSITIVE.
CCCCC IF NOT, INCREMENT UNTIL GET POSITIVE POINT.
      DO5120I=1,10000
        X=X+XINC
        IF(X.GT.XMAX)GOTO5129
        PRED=100.0*(1.0-EXP(-((X/ETA)**BETA)))
        IF(PRED.LE.0.0)THEN
          ZINC=XINC/500.
          XJUNK=X
          DO5125LL=1,500
            XJUNK=XJUNK+ZINC
            PRED=100.0*(1.0-EXP(-((XJUNK/ETA)**BETA)))
            IF(PRED.LE.0.0)GOTO5125
            J=J+1
            Y2(J)=PRED
            X2(J)=XJUNK
            D2(J)=REAL(NDIST+1)
            GOTO5128
 5125     CONTINUE
 5128     CONTINUE
        ELSE
          J=J+1
          Y2(J)=PRED
          X2(J)=X
          D2(J)=REAL(NDIST+1)
        ENDIF
 5120 CONTINUE
 5129 CONTINUE
C
      J=J+1
      Y2(J)=63.2
      X2(J)=XMIN4
      D2(J)=REAL(NDIST+2)
      J=J+1
      Y2(J)=63.2
      X2(J)=XMAX4
      D2(J)=REAL(NDIST+2)
C
      J=J+1
      Y2(J)=99.9
      X2(J)=X632
      D2(J)=REAL(NDIST+3)
      J=J+1
      Y2(J)=0.1
      X2(J)=X632
      D2(J)=REAL(NDIST+3)
C
      N2=J
      NPLOTV=3
C
C               ****************************************
C               **  STEP 61--                         **
C               **  RESTORE OLD SETTINGS FOR          **
C               **     HORIZONTAL AXIS PLOT SCALE     **
C               **     VERTICAL AXIS PLOT SCALE       **
C               ****************************************
C
CCCCC IX1TSC=IX1TSV
CCCCC IX2TSC=IX2TSV
CCCCC IY1TSC=IY1TSV
CCCCC IY2TSC=IY2TSV
C     (THIS RESTORATION MUST BE DONE IN MAIN)
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'WEI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWEI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,MAXN,N2,IERROR
 9012   FORMAT('ICASPL,MAXN,N2,IERROR = ',A4,I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,2E15.7,F9.2)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9021)IX1TSC,IX2TSC,IY1TSC,IY2TSC
 9021   FORMAT('IX1TSC,IX2TSC,IY1TSC,IY2TSC = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IX1TSV,IX2TSV,IY1TSV,IY2TSV
 9022   FORMAT('IX1TSV,IX2TSV,IY1TSV,IY2TSV = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)AINTER,ASLOPE,SDINTE,SDSLOP
 9031   FORMAT('AINTER,ASLOPE,SDINTE,SDSLOP = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)BETA,ETA,SDBETA,SDETA
 9032   FORMAT('BETA,ETA,SDBETA,SDETA = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9034)BPT1,BPT5,B1,B5
 9034   FORMAT('BPT1,BPT5,B1,B5 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)B10,B20,B50,B80,B90
 9035   FORMAT(' B10,B20,B50,B80,B90 = ',5E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9036)B95,B99,B995,B999
 9036   FORMAT('B95,B99,B995,B999 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9037)RESSD,XINC,ETA,X632
 9037   FORMAT('RESSD,XINC,ETA,X62 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)XMIN,XMIN2,XMIN3,XMIN4
 9041   FORMAT('XMIN,XMIN2,XMIN3,XMIN4 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)XMIN,XMIN2,XMIN3,XMIN4
 9043   FORMAT('XMIN,XMIN2,XMIN3,XMIN4 = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWEI3(Y,XHIGH,N,ICASPL,MAXN,IHIGH,MAXOBV,
     1                  XDIST,XHIGHC,
     1                  SHAPE,SCALE,SDSHAP,SDSCAL,
     1                  BPT1,BPT5,B1,B5,B10,B20,B50,B80,B90,
     1                  B95,B99,B995,B999,
     1                  Y2,X2,D2,N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE A FRECHET PLOT.
C              THE PLOT WILL CONSIST OF 2 COMPONENTS--
C                  1) THE RAW DATA
C                  2) THE FITTED LINE
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2013/10
C     ORIGINAL VERSION--OCTOBER   2013.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IHIGH
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IWRITE
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION XHIGH(*)
      DIMENSION XDIST(*)
      DIMENSION XHIGHC(*)
C
      DIMENSION Y2(*)
      DIMENSION X2(*)
      DIMENSION D2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWE'
      ISUBN2='I3  '
      IERROR='NO'
      IWRITE='OFF'
C
      AN=N
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWEI3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG3,ISUBRO
   52   FORMAT('IBUGG3,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IHIGH,MAXN,N,NPLOTV
   53   FORMAT('ICASPL,IHIGH,MAXN,N,NPLOTV = ',A4,2X,A4,3I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO60I=1,N
            WRITE(ICOUT,61)I,Y(I),XHIGH(I)
   61       FORMAT('I,Y(I),XHIGH(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   60     CONTINUE
        ENDIF
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN FRECHET PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)N
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I6)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1130I=1,N
        IF(Y(I).NE.HOLD)GOTO1139
 1130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      ALL THE INPUT RESPONSE VARIABLE ELEMENTS ARE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1133)HOLD
 1133 FORMAT('      IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
      DO1140I=1,N
        IF(Y(I).LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1111)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1142)I
 1142     FORMAT('      ROW ',I8,' OF THE RESPONSE VARIABLE IS ',
     1           'NON-POSITIVE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1145)Y(I)
 1145     FORMAT('      IT HAS THE VALUE ',G15.7)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
 1140 CONTINUE
C
C               THE FRECHET PLOT IS FORMED BY:
C
C                  -LN[-LN[P(i)]] VERSUS LOG(Y(I))
C
C               WHERE THE Y(I) ARE THE SORTED DATA AND
C
C                  P(I) = (I - 0.3)/(N + 0.4)
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  SORT THE DATA AND CARRY ALONG THE TAG    **
C               ***********************************************
C
      DO2005I=1,N
        PI=(REAL(I) - 0.3)/(REAL(N) + 0.4)
        Y2(I)=-LOG(-LOG(PI))
        D2(I)=1.0
 2005 CONTINUE
C
      IF(IHIGH.EQ.'ON')THEN
        CALL SORTC(Y,XHIGH,N,X2,XDIST)
        DO2010I=1,N
          XHIGH(I)=XDIST(I)
          X2(I)=LOG(X2(I))
 2010   CONTINUE
        CALL DISTIN(XHIGH,N,IWRITE,XDIST,NDIST,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        CALL CODE(XHIGH,N,IWRITE,XHIGHC,XDIST,MAXOBV,IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        DO2020I=1,N
          D2(I)=XHIGHC(I)
 2020   CONTINUE
      ELSE
        CALL SORT(Y,N,X2)
        NDIST=1
        DO2013I=1,N
          XHIGH(I)=1.0
          X2(I)=LOG(X2(I))
 2013   CONTINUE
      ENDIF
C
C               ******************************************
C               **  STEP 33--                           **
C               **  CARRY OUT THE FIT                   **
C               ******************************************
C
      AN=REAL(N)
      SUMX=0.0
      SUMY=0.0
      DO3310I=1,N
C
        IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'WEI3')THEN
          WRITE(ICOUT,3311)I,Y2(I),X2(I)
 3311     FORMAT('I,Y2(I),X2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        SUMX=SUMX+X2(I)
        SUMY=SUMY+Y2(I)
 3310 CONTINUE
      XBAR=SUMX/AN
      YBAR=SUMY/AN
C
      SUMXX=0.0
      SUMYY=0.0
      SUMXY=0.0
      DO3320I=1,N
        SUMXX=SUMXX+(X2(I)-XBAR)*(X2(I)-XBAR)
        SUMYY=SUMYY+(Y2(I)-YBAR)*(Y2(I)-YBAR)
        SUMXY=SUMXY+(X2(I)-XBAR)*(Y2(I)-YBAR)
 3320 CONTINUE
      ASLOPE=0.0
      IF(SUMXX.GT.0.0)ASLOPE=SUMXY/SUMXX
      AINTER=YBAR-ASLOPE*XBAR
C
      SUMRR=0.0
      SUMX2=0.0
      DO3330I=1,N
        RES=Y2(I)-(AINTER+ASLOPE*X2(I))
        SUMRR=SUMRR+RES*RES
        SUMX2=SUMX2+X2(I)*X2(I)
 3330 CONTINUE
      RESVAR=SUMRR/(AN-2.0)
      RESSD=0.0
      IF(RESVAR.GT.0.0)RESSD=SQRT(RESVAR)
      SDINTE=RESSD*SQRT(SUMX2/(AN*SUMXX))
      SDSLOP=RESSD*SQRT(1.0/SUMXX)
C
C               ****************************************
C               **  STEP 34--                         **
C               **  FORM ESTIMATES FOR                **
C               **  BETA (= SHAPE PARAMETER) AND      **
C               **  ETA  (= CHARACTERISTIC LIFE)      **
C               ****************************************
C
      IF(ASLOPE.LE.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3332)
 3332   FORMAT('      THE FITTED SLOPE IS ZERO OR NEGATIVE WHICH WOULD')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3335)
 3335   FORMAT('      YIELD AN IMPOSSIBLE VALUE FOR THE SHAPE ',
     1         'PARAMETER = SLOPE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3336)ASLOPE
 3336   FORMAT('      ASLOPE = ',G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3337)SUMX,SUMY,SUMXX,SUMYY,SUMXY
 3337   FORMAT('      SUMX,SUMY,SUMXX,SUMYY,SUMXY = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      SHAPE=ASLOPE
      SCALE=EXP(AINTER/(-ASLOPE))
C
C     NOT SURE WHAT THE STANDARD DEVIATIONS SHOULD BE FOR THE
C     SHAPE/SCALE PARAMETERS, SO DON'T COMPUTE FOR NOW.
CCCCC SDSHAP=BETA*BETA*SDSLOP
CCCCC SDSCAL=ETA*SDINTE
      SDSHAP=CPUMIN
      SDSCAL=CPUMIN
C
C               ************************************************
C               **  STEP 35--                                 **
C               **  FORM ESTIMATES FOR                        **
C               **     BPT1= .1%   POINT OF BEST-FIT DIST.    **
C               **     BPT5= .5%   POINT OF BEST-FIT DIST.    **
C               **     B1  =  1%   POINT OF BEST-FIT DIST.    **
C               **     B5  =  5%   POINT OF BEST-FIT DIST.    **
C               **     B10 = 10%   POINT OF BEST-FIT DIST.    **
C               **     B20 = 20%   POINT OF BEST-FIT DIST.    **
C               **     B50 = 50%   POINT OF BEST-FIT DIST.    **
C               **     B80 = 80%   POINT OF BEST-FIT DIST.    **
C               **     B90 = 90%   POINT OF BEST-FIT DIST.    **
C               **     B95 = 95%   POINT OF BEST-FIT DIST.    **
C               **     B99 = 99%   POINT OF BEST-FIT DIST.    **
C               **     B995= 99.5% POINT OF BEST-FIT DIST.    **
C               **     B999= 99.9% POINT OF BEST-FIT DIST.    **
C               ************************************************
C
      P=.001
      BPT1=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.005
      BPT5=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.01
      B1=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.05
      B5=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.10
      B10=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.20
      B20=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.50
      B50=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.80
      B80=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.90
      B90=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.95
      B95=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.99
      B99=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.995
      B995=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
      P=.999
      B999=SCALE*(LOG(1.0/P)**(-1.0/SHAPE))
C
C               ****************************************
C               **  STEP 42--                         **
C               **  DETERMINE PLOT LIMITS FOR         **
C               **  PREDICTED LINE                    **
C               ****************************************
C
      N2=N+1
      XMIN=X2(1)
      YMIN=AINTER + ASLOPE*XMIN
      X2(N2)=XMIN
      Y2(N2)=YMIN
      D2(N2)=REAL(NDIST+1)
      N2=N2+1
      XMAX=X2(N)
      YMAX=AINTER + ASLOPE*XMAX
      X2(N2)=XMAX
      Y2(N2)=YMAX
      D2(N2)=REAL(NDIST+1)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'WEI3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWEI3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,MAXN,N2,IERROR
 9012   FORMAT('ICASPL,MAXN,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9015I=1,N2
          WRITE(ICOUT,9016)I,Y2(I),X2(I),D2(I)
 9016     FORMAT('I,Y2(I),X2(I),D2(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9015   CONTINUE
        WRITE(ICOUT,9031)AINTER,ASLOPE,SDINTE,SDSLOP
 9031   FORMAT('AINTER,ASLOPE,SDINTE,SDSLOP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)SHAPE,SCALE
 9032   FORMAT('SHAPE,SCALE = ',2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9034)BPT1,BPT5,B1,B5
 9034   FORMAT('BPT1,BPT5,B1,B5 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)B10,B20,B50,B80,B90
 9035   FORMAT(' B10,B20,B50,B80,B90 = ',5G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9036)B95,B99,B995,B999
 9036   FORMAT('B95,B99,B995,B999 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWEIG(IHARG,IHARG2,NUMARG,IDEFW1,IDEFW2,
     1IWEIG1,IWEIG2,IWEIGH,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE USER VARIABLE NAME IN WHICH
C              THE WEIGHTS FOR FITTING, PRE-FITTING, ANOVA, EC. RESIDE.
C              CHARACTERS 1 TO 4 OF THE SPECIFIED KNOT NAME
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IWEIG1;
C              CHARACTERS 5 TO 8 OF THE SPECIFIED KNOT NAME
C              WILL BE PLACED IN THE HOLLERITH VARIABLE IWEIG2.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IHARG2 (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C                     --IDEFW1 (A  HOLLERITH VARIABLE)
C                     --IDEFW2 (A  HOLLERITH VARIABLE)
C     OUTPUT ARGUMENTS--IWEIG1 (A  HOLLERITH VARIABLE)
C                     --IWEIG2 (A  HOLLERITH VARIABLE)
C                     --IWEIGH (A  HOLLERITH VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IDEFW1
      CHARACTER*4 IDEFW2
      CHARACTER*4 IWEIG1
      CHARACTER*4 IWEIG2
      CHARACTER*4 IWEIGH
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHOLD1
      CHARACTER*4 IHOLD2
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      GOTO1110
C
 1110 CONTINUE
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      GOTO1160
C
 1150 CONTINUE
      IHOLD1=IDEFW1
      IHOLD2=IDEFW2
      IWEIGH='OFF'
      GOTO1180
C
 1160 CONTINUE
      IHOLD1=IHARG(NUMARG)
      IHOLD2=IHARG2(NUMARG)
      IWEIGH='ON'
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IWEIG1=IHOLD1
      IWEIG2=IHOLD2
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IWEIG1,IWEIG2
 1181 FORMAT('THE WEIGHTS VARIABLE HAS JUST BEEN DESIGNATED AS ',
     1A4,A4)
      CALL DPWRST('XXX','BUG ')
      IF(IWEIGH.EQ.'OFF')WRITE(ICOUT,1182)
 1182 FORMAT('(THAT IS, THE EQUAL-WEIGHTS CASE IS BEING ASSUMED)')
      IF(IWEIGH.EQ.'OFF')CALL DPWRST('XXX','BUG ')
 1189 CONTINUE
      GOTO1199
C
 1199 CONTINUE
      RETURN
      END
      SUBROUTINE DPWICC(IHARG,IHARG2,IARGT,ARG,NUMARG,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,IANS,IWIDTH,
     1PWXMIN,PWXMAX,PWYMIN,PWYMAX,IBUGP2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE WINDOW CORNER COORDINATES
C              (LOWER LEFT AND UPPER RIGHT)
C              WHICH IN TURN WILL DEFINE THE SIZE AND SHAPE
C              OF THE PLOT WINDOW.
C              THE 2 PAIRS OF COORDINATES ARE CONTAINED IN THE
C              4 VARIABLES    PWXMIN,PWYMIN    AND    PWXMAX,PWYMAX
C      NOTE--THE PLOT WINDOW INCLUDES THE AREA INSIDE THE FRAME
C            AND THE AREA OUTSIDE THE FRAME.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A  FLOATING POINT VECTOR)
C                     --NUMARG
C     OUTPUT ARGUMENTS--PWXMIN = X COOR. FOR LOWER LEFT  CORNER
C                     --PWXMAX = X COOR. FOR UPPER RIGHT CORNER
C                     --PWYMIN = Y COOR. FOR LOWER LEFT  CORNER
C                     --PWYMAX = Y COOR. FOR UPPER RIGHT CORNER
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--NOVEMBER  1978.
C     UPDATED         --SEPTEMBER 1980.
C     UPDATED         --MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1996. NO ARGUMENTS EQUAL DEFAULT
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWI'
      ISUBN2='CC  '
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGP2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DPWICC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IFOUND,IERROR
   52 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)PWXMIN,PWXMAX,PWYMIN,PWYMAX
   53 FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************************
C               **  TREAT THE    WINDOW    COORDINATES    CASE  **
C               **************************************************
C
      IF(NUMARG.LE.0)GOTO1150
      GOTO1110
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
CCCCC DECEMBER 1996.  IF NO ARGUMENTS, IHARG(NUMARG) = 'COOR'
      IF(IHARG(NUMARG).EQ.'COOR')GOTO1150
      IF(NUMARG.GE.2)GOTO1175
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPCORN--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR WINDOW CORNER COORDINATES ',
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
 1126 FORMAT('      THE LOWER LEFT CORNER OF THE WINDOW')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      10% ACROSS THE PAGE AND 20% UP THE PAGE, AND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      THE UPPER RIGHT CORNER OF THE WINDOW')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      90% ACROSS THE PAGE AND 80% UP THE PAGE,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('      WINDOW CORNER COORDINATES 10 20 90 80')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('      WINDOW 10 20 90 80')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      PWXMIN=0.
      PWYMIN=0.
      PWXMAX=100.
      PWYMAX=100.
      GOTO1180
C
 1175 CONTINUE
      DO1176J=2,NUMARG
      IF(IARGT(J).EQ.'NUMB')GOTO1177
      GOTO1178
 1177 CONTINUE
      IF(J.EQ.2)PWXMIN=ARG(J)
      IF(J.EQ.3)PWYMIN=ARG(J)
      IF(J.EQ.4)PWXMAX=ARG(J)
      IF(J.EQ.5)PWYMAX=ARG(J)
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
      IF(J.EQ.2)PWXMIN=VALUE(ILOC)
      IF(J.EQ.3)PWYMIN=VALUE(ILOC)
      IF(J.EQ.4)PWXMAX=VALUE(ILOC)
      IF(J.EQ.5)PWYMAX=VALUE(ILOC)
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
 1185 FORMAT('THE WINDOW CORNER COORDINATES HAVE JUST BEEN SET ',
     1'AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1186)PWXMIN,PWYMIN
 1186 FORMAT('    (X,Y) FOR LOWER LEFT  CORNER OF WINDOW = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1187)PWXMAX,PWYMAX
 1187 FORMAT('    (X,Y) FOR UPPER RIGHT CORNER OF WINDOW = ',2E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPWICC--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)PWXMIN,PWXMAX,PWYMIN,PWYMAX
 9013 FORMAT('PWXMIN,PWXMAX,PWYMIN,PWYMAX = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPWIDT(IHARG,IARGT,ARG,NUMARG,
     1PDEFWI,
     1PTEXWI,
     1IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE WIDTH FOR TEXT CHARACTERS.
C              THE WIDTH FOR TEXT CHARACTERS WILL BE PLACED
C              IN THE FLOATING POINT VARIABLE PTEXWI.
C     NOTE--THE WIDTH IS IN STANDARDIZED UNITS (0.0 TO 100.0).
C     NOTE--THE WIDTH DOES NOT INCLUDE BETWEEN-LINE GAP.
C     INPUT  ARGUMENTS--IHARG  (A  CHARACTER VECTOR)
C                     --IARGT
C                     --ARG
C                     --NUMARG
C                     --PDEFWI
C                     --IBUGD2
C     OUTPUT ARGUMENTS--PTEXWI
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--APRIL     1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IARGT
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION IHARG(*)
      DIMENSION IARGT(*)
      DIMENSION ARG(*)
C
C-----COMMON----------------------------------------------------------
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
   51 FORMAT('***** AT THE BEGINNING OF DPWIDT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)PDEFWI
   53 FORMAT('PDEFWI = ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,NUMARG
      WRITE(ICOUT,56)I,IHARG(I),IARGT(I),ARG(I)
   56 FORMAT('I,IHARG(I),IARGT(I),ARG(I) = ',I8,2X,A4,2X,A4,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               *****************************
C               **  TREAT THE WIDTH CASE  **
C               *****************************
C
      IF(NUMARG.LE.0)GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
C
      IF(NUMARG.GE.1.AND.IARGT(NUMARG).EQ.'NUMB')
     1GOTO1160
C
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPWIDT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR WIDTH COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      TEST EXAMPLE TO DEMONSTRATE THE ',
     1'PROPER FORM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1125)
 1125 FORMAT('      SUPPOSE IT IS DESIRED THAT ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1126)
 1126 FORMAT('      THE TEXT CHARACTERS HAVE A WIDTH OF 5')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1127)
 1127 FORMAT('      (WHERE THE HORIZONTAL SCREEN UNITS RANGE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1128)
 1128 FORMAT('      FROM 0 TO 100, AND WHERE ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1129)
 1129 FORMAT('      THE BETWEEN-CHARACTER GAP IS NOT INCLUDED),')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1130)
 1130 FORMAT('      THEN THE ALLOWABLE FORM IS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('           WIDTH 5 ')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      PTEXWI=PDEFWI
      GOTO1180
C
 1160 CONTINUE
      PTEXWI=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('THE WIDTH (FOR TEXT CHARACTERS)  ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)PTEXWI
 1182 FORMAT('HAS JUST BEEN SET TO ',E15.7)
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
      WRITE(ICOUT,8111)PTEXWI
 8111 FORMAT('THE CURRENT (TEXT) WIDTH  IS ',E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)PDEFWI
 8112 FORMAT('THE DEFAULT (TEXT) WIDTH  IS ',E15.7)
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
 9011 FORMAT('***** AT THE END       OF DPWIDT--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,ISUBRO,IFOUND,IERROR
 9012 FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)PTEXWI
 9013 FORMAT('PTEXWI = ',E15.7)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPWILC(XTEMP1,XTEMP2,MAXNXT,
     1                  ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A 1-SAMPLE OR 2-SAMPLE WILCOXON SIGNED RANK TEST
C     EXAMPLE--WILCOXON SIGNED RANK TEST Y D0
C              WILCOXON SIGNED RANK TEST D0 Y
C              WILCOXON SIGNED RANK TEST Y1 Y2
C              WILCOXON SIGNED RANK TEST Y1 Y2 Y3 Y4 D0
C              WILCOXON SIGNED RANK TEST Y1 Y2 Y3 Y4 Y5
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/6
C     ORIGINAL VERSION--JUNE      1999.
C     UPDATED         --JANUARY   2007.  CALL LIST TO RANK
C     UPDATED         --MAY       2011.  USE DPPARS AND DPPAR3
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
      CHARACTER*4 ICASA2
      CHARACTER*4 ICASA3
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
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
      DIMENSION XTEMP1(*)
      DIMENSION XTEMP2(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
      DIMENSION XTEMP3(MAXOBV)
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),XTEMP3(1))
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWI'
      ISUBN2='LC  '
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
C               **  TREAT THE WILCOXON SIGNED RANK TEST CASE  **
C               ************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WILC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWILC--')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WILC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTZ=9999
      ICASAN='WILC'
      ICASA2='UNKN'
      ICASA3='TWOT'
C
C     LOOK FOR:
C
C          SIGNED RANK TEST/WILCOXON SIGNED RANK TEST
C          ONE SAMPLE (OR 1 SAMPLE)
C          TWO SAMPLE (OR 2 SAMPLE)
C          LOWER TAILED
C          UPPER TAILED
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
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'WILC' .AND. ICTMP2.EQ.'SIGN' .AND.
     1         ICTMP3.EQ.'RANK' .AND. ICTMP4.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'WILC' .AND. ICTMP2.EQ.'SIGN' .AND.
     1         ICTMP3.EQ.'RANK')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'WILC' .AND. ICTMP2.EQ.'SIGN' .AND.
     1         ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'WILC' .AND. ICTMP2.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'WILC')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I
        ELSEIF(ICTMP1.EQ.'SIGN' .AND. ICTMP2.EQ.'RANK' .AND.
     1         ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'SIGN' .AND. ICTMP2.EQ.'RANK' .AND.
     1         ICTMP3.NE.'TEST')THEN
          IFOUND='YES'
          ICASAN='WILC'
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'ONE' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='ONES'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'1' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='ONES'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'TWO' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='TWOS'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'2' .AND. ICTMP2.EQ.'SAMP')THEN
          ICASA2='TWOS'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'LOWE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='LOWE'
          ILASTZ=MAX(ILASTZ,I+1)
        ELSEIF(ICTMP1.EQ.'UPPE' .AND. ICTMP2.EQ.'TAIL')THEN
          ICASA3='UPPE'
          ILASTZ=MAX(ILASTZ,I+1)
        ENDIF
  100 CONTINUE
C
      IF(IFOUND.EQ.'NO')GOTO9000
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WILC')THEN
        WRITE(ICOUT,91)ICASAN,ICASA2,ISHIFT
   91   FORMAT('DPWILC: ICASAN,ICASA2,ISHIFT = ',
     1         2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WILC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='WILCOXON SIGNED RANK TEST'
      MINNA=1
      MAXNA=100
      MINN2=5
      IFLAGE=1
      IFLAGM=1
      MINNVA=2
      MAXNVA=MAXSPN
      IFLAGP=29
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WILC')THEN
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
C     IF FIRST OR LAST ARGUMENT IS A PARAMETER, EXTRACT D0.
C
      IF(IVARTY(1).EQ.'PARA')THEN
        ISTART=2
        ISTOP=NUMVAR
        D0=PVAR(1)
      ELSEIF(IVARTY(NUMVAR).EQ.'PARA')THEN
        ISTART=1
        ISTOP=NUMVAR-1
        D0=PVAR(NUMVAR)
      ELSE
        ISTART=1
        ISTOP=NUMVAR
        D0=0.0
      ENDIF
C
      NTEMP=ISTOP-ISTART+1
      IF(ICASA2.EQ.'UNKN')THEN
        IF(NTEMP.EQ.1)ICASA2='ONES'
        IF(NTEMP.EQ.2)ICASA2='TWOS'
      ENDIF
C
      IF(ICASA2.EQ.'TWOS' .AND. NTEMP.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,301)
  301   FORMAT('***** ERROR IN WILCOXON SIGNED RANK TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,303)
  303   FORMAT('      FOR THE TWO-SAMPLE CASE, THERE MUST BE AT LEAST')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,305)
  305   FORMAT('      TWO VARIABLES SPECIFIED.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WILC')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMVA2=1
      DO5210I=ISTART,ISTOP
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
        IF(ICASA2.EQ.'ONES')THEN
          ISTRT2=1
          ISTOP2=1
        ELSE
          ISTRT2=I+1
          ISTOP2=ISTOP
        ENDIF
C
        DO5220J=ISTRT2,ISTOP2
C
          IF(ICASA2.EQ.'TWOS')THEN
            ICOL=J
            CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1                  INAME,IVARN1,IVARN2,IVARTY,
     1                  ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVA2,
     1                  MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1                  MAXCP4,MAXCP5,MAXCP6,
     1                  V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1                  X,X,X,NS2,NLOCA2,NLOCA3,ICASE,
     1                  IBUGA3,ISUBRO,IFOUND,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
          ENDIF
C
C               *******************************************
C               **  STEP 52--                            **
C               **  PERFORM A WILCOXON SIGNED RANK TEST  **
C               *******************************************
C
          ISTEPN='52'
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WILC')THEN
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5211)
 5211       FORMAT('***** FROM DPWILC, BEFORE CALL DPWIL2--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,5212)I,J,NS1,NS2,MAXN,D0
 5212       FORMAT('I,J,NS1,NS2,MAXN,D0 = ',5I8,G15.7)
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
          CALL DPWIL2(Y,X,NS1,D0,ICASA2,ICASA3,
     1                XTEMP1,XTEMP2,XTEMP3,MAXNXT,
     1                ICAPSW,ICAPTY,IFORSW,
     1                IVARID,IVARI2,IVARI3,IVARI4,
     1                STATVA,STATV2,STATCD,
     1                PVAL2T,PVALLT,PVALUT,
     1                CTL005,CTL010,CTL025,CTL050,CTL100,
     1                CTL200,CTL500,
     1                CTU995,CTU990,CT975,CTU950,CTU900,
     1                CTU800,CTU500,
     1                CVL005,CVL010,CVL025,CVL050,CVL100,
     1                CVL200,CVL500,
     1                CVU995,CVU990,CV975,CVU950,CVU900,
     1                CVU800,CVU500,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WILC')
     1      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
          IF(ICASA2.EQ.'TWOS')THEN
            IF(NUMVAR.GT.2)THEN
              IFLAGU='FILE'
            ELSE
              IFLAGU='ON'
            ENDIF
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(I.EQ.1 .AND. J.EQ.2)IFRST=.TRUE.
            IF(I.EQ.NUMVAR .AND. J.EQ.NUMVAR)ILAST=.TRUE.
          ELSE
            IF(ISTOP-ISTART.GT.0)THEN
              IFLAGU='FILE'
            ELSE
              IFLAGU='ON'
            ENDIF
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(I.EQ.ISTART)IFRST=.TRUE.
            IF(I.EQ.ISTOP)ILAST=.TRUE.
          ENDIF
          CALL DPWIL5(ICASA2,ICASA3,
     1                STATVA,STATV2,STATCD,
     1                PVAL2T,PVALLT,PVALUT,
     1                CTL005,CTL010,CTL025,CTL050,CTL100,
     1                CTL200,CTL500,
     1                CTU995,CTU990,CT975,CTU950,CTU900,
     1                CTU800,CTU500,
     1                CVL005,CVL010,CVL025,CVL050,CVL100,
     1                CVL200,CVL500,
     1                CVU995,CVU990,CV975,CVU950,CVU900,
     1                CVU800,CVU500,
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WILC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWILC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IFOUND,IERROR
 9016   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWIL2(Y1,Y2,N1,D0,ICASAN,ICASA2,
     1                  TEMP1,TEMP2,TEMP3,MAXNXT,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  IVARID,IVARI2,IVARI3,IVARI4,
     1                  STATVA,STATV2,STATCD,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  CTL005,CTL010,CTL025,CTL050,CTL100,
     1                  CTL200,CTL500,
     1                  CTU995,CTU990,CTU975,CTU950,CTU900,
     1                  CTU800,CTU500,
     1                  CVL005,CVL010,CVL025,CVL050,CVL100,
     1                  CVL200,CVL500,
     1                  CVU995,CVU990,CVU975,CVU950,CVU900,
     1                  CVU800,CVU500,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT A 2-SAMPLE PAIRED SIGNED RANK
C              TEST
C     EXAMPLE--SIGNED RANK TEST Y1 Y2
C              SIGNED RANK TEST Y1 Y2 D0
C     SAMPLE 1 IS IN INPUT VECTOR Y1
C              (WITH N1 OBSERVATIONS).
C     SAMPLE 2 IS IN INPUT VECTOR Y2
C              (WITH N1 OBSERVATIONS).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/6
C     ORIGINAL VERSION--JUNE      1999.
C     UPDATED         --AUGUST    2002.
C     UPDATED         --JANUARY   2007. CALL LIST TO RANK
C     UPDATED         --MAY       2011. SWITCH FROM WALPOLE/MEYERS
C                                       FORMULATION TO CONOVER
C                                       IMPLEMENTATION.
C     UPDATED         --MAY       2011. USE DPDTA1, DPDTA5 TO PRINT
C                                        OUTPUT.  REFORMAT OUTPUT
C                                        SOMEWHAT AS WELL.
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
      CHARACTER*4 ICASA2
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
      DOUBLE PRECISION TPLUS
      DOUBLE PRECISION TMINUS
      DOUBLE PRECISION RSUM
      DOUBLE PRECISION RSUMSQ
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
C
      DIMENSION CV005(47)
      DIMENSION CV010(47)
      DIMENSION CV025(47)
      DIMENSION CV050(47)
      DIMENSION CV100(47)
      DIMENSION CV200(47)
      DIMENSION CV500(47)
C
      PARAMETER (NUMALP=7)
      REAL ALPHA(NUMALP)
      PARAMETER (NUMAL2=5)
      REAL ALPHA2(NUMAL2)
C
      PARAMETER(NUMCLI=5)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=30)
      CHARACTER*60 ITITLE
      CHARACTER*60 ITITLZ
      CHARACTER*60 ITITL9
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
      LOGICAL IFLAGS
      LOGICAL IFLAGE
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA ALPHA /0.50, 0.80, 0.90, 0.95, 0.975, 0.99, 0.995/
      DATA ALPHA2/0.60, 0.80, 0.90, 0.95, 0.99/
C
      DATA (CV005(I),I=1,47) /
     1                    0.0,  0.0,  0.0,  0.0,  1.0,  2.0,  4.0,
     1  6.0,  8.0, 10.0, 13.0, 16.0, 20.0, 24.0, 28.0, 33.0, 38.0,
     1 44.0, 49.0, 55.0, 62.0, 69.0, 76.0, 84.0, 92.0,101.0,110.0,
     1119.0,129.0,139.0,149.0,160.0,172.0,184.0,196.0,208.0,221.0,
     1235.0,248.0,263.0,277.0,292.0,308.0,324.0,340.0,357.0,374.0/
C
      DATA (CV010(I),I=1,47) /
     1                    0.0,  0.0,  0.0,  1.0,  2.0,  4.0,  6.0,
     1  8.0, 10.0, 13.0, 16.0, 20.0, 24.0, 28.0, 33.0, 38.0, 44.0,
     1 50.0, 56.0, 63.0, 70.0, 77.0, 85.0, 94.0,102.0,111.0,121.0,
     1131.0,141.0,152.0,163.0,175.0,187.0,199.0,212.0,225.0,239.0,
     1253.0,267.0,282.0,297.0,313.0,329.0,346.0,363.0,381.0,398.0/
C
      DATA (CV025(I),I=1,47) /
     1                    0.0,  0.0,  1.0,  3.0,  4.0,  6.0,  9.0,
     1 11.0, 14.0, 18.0, 22.0, 26.0, 30.0, 35.0, 41.0, 47.0, 53.0,
     1 59.0, 67.0, 74.0, 82.0, 90.0, 99.0,108.0,117.0,127.0,138.0,
     1148.0,160.0,171.0,183.0,196.0,209.0,222.0,236.0,250.0,265.0,
     1280.0,295.0,311.0,328.0,344.0,362.0,379.0,397.0,416.0,435.0/
C
      DATA (CV050(I),I=1,47) /
     1                    0.0,  1.0,  3.0,  4.0,  6.0,  9.0, 11.0,
     1 14.0, 18.0, 22.0, 26.0, 31.0, 36.0, 42.0, 48.0, 54.0, 61.0,
     1 68.0, 76.0, 84.0, 92.0,101.0,111.0,120.0,131.0,141.0,152.0,
     1164.0,176.0,188.0,201.0,214.0,228.0,242.0,257.0,272.0,287.0,
     1303.0,320.0,337.0,354.0,372.0,390.0,408.0,428.0,447.0,467.0/
C
      DATA (CV100(I),I=1,47) /
     1                    1.0,  3.0,  4.0,  6.0,  9.0, 11.0, 15.0,
     1 18.0, 22.0, 27.0, 32.0, 37.0, 43.0, 49.0, 56.0, 63.0, 70.0,
     1 78.0, 87.0, 95.0,105.0,114.0,125.0,135.0,146.0,158.0,170.0,
     1182.0,195.0,208.0,222.0,236.0,251.0,266.0,282.0,298.0,314.0,
     1331.0,349.0,366.0,385.0,403.0,423.0,442.0,463.0,483.0,504.0/
C
      DATA (CV200(I),I=1,47) /
     1                    3.0,  4.0,  6.0,  9.0, 12.0, 15.0, 19.0,
     1 23.0, 28.0, 33.0, 39.0, 45.0, 51.0, 58.0, 66.0, 74.0, 83.0,
     1 91.0,100.0,110.0,120.0,131.0,142.0,154.0,166.0,178.0,191.0,
     1205.0,219.0,233.0,248.0,263.0,279.0,295.0,312.0,329.0,347.0,
     1365.0,384.0,403.0,422.0,442.0,463.0,484.0,505.0,527.0,550.0/
C
      DATA (CV500(I),I=1,47) /
     1                    5.0,  7.5, 10.5, 14.0, 18.0, 22.5, 27.5,
     1 33.0, 39.0, 45.5, 52.5, 60.0, 68.0, 76.5, 85.5, 95.0,105.0,
     1115.5,126.5,138.0,150.0,162.5,175.5,189.0,203.0,217.5,232.5,
     1248.0,264.0,280.5,297.5,315.0,333.0,351.5,370.5,390.0,410.0,
     1430.5,451.5,473.0,495.0,517.5,540.5,564.0,588.0,612.5,637.5/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWI'
      ISUBN2='L2  '
C
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
      CTL005=CPUMIN
      CTL010=CPUMIN
      CTL025=CPUMIN
      CTL050=CPUMIN
      CTL100=CPUMIN
      CTL200=CPUMIN
      CTL500=CPUMIN
      CTU500=CPUMIN
      CTU800=CPUMIN
      CTU900=CPUMIN
      CTU950=CPUMIN
      CTU975=CPUMIN
      CTU990=CPUMIN
      CTU995=CPUMIN
C
      CVL005=CPUMIN
      CVL010=CPUMIN
      CVL025=CPUMIN
      CVL050=CPUMIN
      CVL100=CPUMIN
      CVL200=CPUMIN
      CVL500=CPUMIN
      CVU500=CPUMIN
      CVU800=CPUMIN
      CVU900=CPUMIN
      CVU950=CPUMIN
      CVU975=CPUMIN
      CVU990=CPUMIN
      CVU995=CPUMIN
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'WIL2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPWIL2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASAN,ICASA2
   52   FORMAT('IBUGA3,ISUBRO,ICASAN,ICASA2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,53)IVARID,IVARI2,IVARI3,IVARI4
   53   FORMAT('IVARID,IVARI2,IVARI3,IVARI4 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,55)N1,NUMDIG,D0
   55   FORMAT('N1,NUMDIG,D0 = ',2I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        IF(N1.GE.1)THEN
          DO56I=1,N1
            WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57       FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','WRIT')
   56     CONTINUE
        ENDIF
      ENDIF
C
C               ************************************
C               **   STEP 1--                     **
C               **   CALL DPWIL3 TO COMPUTE THE   **
C               **   BASIC TEST STATISTIC (FOR    **
C               **   EITHER 1-SAMPLE OR 2-SAMPLE  **
C               **   CASE).                       **
C               ************************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPWIL3(Y1,Y2,N1,D0,ICASAN,ICASA2,
     1            TEMP1,TEMP2,TEMP3,MAXNXT,
     1            STATVA,STATV2,STATCD,
     1            PVAL2T,PVALLT,PVALUT,
     1            NTEMP,NPLUS,NMINUS,NTIES,
     1            TPLUS,TMINUS,RSUM,RSUMSQ,
     1            IBUGA3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 21--                        **
C               **  COMPUTE THE CRITICAL VALUES FOR  **
C               **  VARIOUS VALUES OF ALPHA          **
C               **************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     CRITICAL POINTS FOR LARGE SAMPLE NORMAL APPROXIMATION
C
      CALL NORPPF(.005,CTL005)
      CALL NORPPF(.010,CTL010)
      CALL NORPPF(.025,CTL025)
      CALL NORPPF(.050,CTL050)
      CALL NORPPF(.100,CTL100)
      CALL NORPPF(.200,CTL200)
      CALL NORPPF(.500,CTL500)
      CALL NORPPF(.500,CTU500)
      CALL NORPPF(.800,CTU800)
      CALL NORPPF(.900,CTU900)
      CALL NORPPF(.950,CTU950)
      CALL NORPPF(.975,CTU975)
      CALL NORPPF(.990,CTU990)
      CALL NORPPF(.995,CTU995)
C
C     NOW GENERATE CRITICAL VALUES FROM TABLES IF
C     THE NUMBER OF UNTIED VALUES IS <= 50 AND THE
C     PERCENTAGE OF TIES IS < 10%.
C
      ITAB=0
      IF(NTEMP.GE.4 .AND. NTEMP.LE.50)THEN
        ITAB=1
        ACUT=REAL(NTEMP)*0.10
        NCUT=INT(ACUT+0.5)
        IF(NTIES.GT.NCUT)ITAB=0
      ENDIF
C
      IF(ITAB.EQ.1)THEN
        IINDX=NTEMP-3
        CVL005=CV005(IINDX)
        CVL010=CV010(IINDX)
        CVL025=CV025(IINDX)
        CVL050=CV050(IINDX)
        CVL100=CV100(IINDX)
        CVL200=CV200(IINDX)
        CVL500=CV500(IINDX)
        CONST=REAL(NTEMP)*REAL(NTEMP+1)/2.0
        CVU500=CVL050
        CVU800=CONST-CVL200
        CVU900=CONST-CVL100
        CVU950=CONST-CVL050
        CVU975=CONST-CVL025
        CVU990=CONST-CVL010
        CVU995=CONST-CVL005
      ENDIF
C
C               *************************************************
C               **   STEP 22--                                 **
C               **   WRITE OUT EVERYTHING                      **
C               **   FOR A WILCOXON SIGNED RANK TEST           **
C               *************************************************
C
      ISTEPN='22'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IPRINT.EQ.'OFF')GOTO9000
C
      IF(ICASAN.EQ.'ONES')THEN
        IF(ICASA2.EQ.'LOWE')THEN
          ITITLE='One Sample Lower-Tailed Wilcoxon Signed Rank Test'
          NCTITL=49
        ELSEIF(ICASA2.EQ.'UPPE')THEN
          ITITLE='One Sample Upper-Tailed Wilcoxon Signed Rank Test'
          NCTITL=49
        ELSE
          ITITLE='One Sample Two-Sided Wilcoxon Signed Rank Test'
          NCTITL=46
        ENDIF
      ELSE
        IF(ICASA2.EQ.'LOWE')THEN
          ITITLE='Two Sample Lower-Tailed Wilcoxon Signed Rank Test'
          NCTITL=49
        ELSEIF(ICASA2.EQ.'UPPE')THEN
          ITITLE='Two Sample Upper-Tailed Wilcoxon Signed Rank Test'
          NCTITL=49
        ELSE
          ITITLE='Two Sample Two-Sided Wilcoxon Signed Rank Test'
          NCTITL=46
        ENDIF
      ENDIF
      ITITLZ='(Conover Formulation)'
      NCTITZ=21
C
      ICNT=1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=0
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(ICASAN.EQ.'ONES')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Response Variable: '
        WRITE(ITEXT(ICNT)(20:23),'(A4)')IVARID(1:4)
        WRITE(ITEXT(ICNT)(24:27),'(A4)')IVARI2(1:4)
        NCTEXT(ICNT)=27
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
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
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(ICASAN.EQ.'ONES')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='H0: Mean Equal'
        NCTEXT(ICNT)=14
        AVALUE(ICNT)=D0
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Mean Not Equal'
        NCTEXT(ICNT)=18
        AVALUE(ICNT)=D0
        IDIGIT(ICNT)=NUMDIG
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='H0: Mu1 - Mu2 Equal'
        NCTEXT(ICNT)=19
        AVALUE(ICNT)=D0
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Ha: Mu1 - Mu2 Not Equal'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=D0
        IDIGIT(ICNT)=NUMDIG
      ENDIF
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
      AVALUE(ICNT)=REAL(N1)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Zero Differences (Omitted):'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=REAL(N1 - NTEMP)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Positive Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=REAL(NPLUS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Negative Differences:'
      NCTEXT(ICNT)=31
      AVALUE(ICNT)=REAL(NMINUS)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Number of Tied Ranks:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=REAL(NTIES)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of Positive Ranks:'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=TPLUS
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sum of Negative Ranks:'
      NCTEXT(ICNT)=22
      AVALUE(ICNT)=TMINUS
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      IF(ITAB.EQ.1)THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Test (Small Sample Exact):'
        NCTEXT(ICNT)=26
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Test Statistic Value:'
        NCTEXT(ICNT)=23
        AVALUE(ICNT)=STATVA
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)=' '
        NCTEXT(ICNT)=1
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test (Large Sample Approximation):'
      NCTEXT(ICNT)=34
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=STATV2
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='CDF Value:'
      NCTEXT(ICNT)=10
      AVALUE(ICNT)=STATCD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value (2-tailed test):'
      NCTEXT(ICNT)=24
      AVALUE(ICNT)=PVAL2T
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value (lower-tailed test):'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=PVALLT
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='P-Value (upper-tailed test):'
      NCTEXT(ICNT)=28
      AVALUE(ICNT)=PVALUT
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='21B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITLE='Two-Tailed Test: Normal Approximation'
      NCTITL=37
      IF(ICASAN.EQ.'ONES')THEN
        ITITL9='H0: u = d0; Ha: u <> d0'
        NCTIT9=23
      ELSE
        ITITL9='H0: u1 - u2 = d0; Ha: u1 - u2 <> d0'
        NCTIT9=35
      ENDIF
C
      DO2130J=1,NUMCLI
        DO2140I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 2140   CONTINUE
 2130 CONTINUE
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
      ITITL2(3,3)='Value (+/-)'
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
      NUMCOL=4
      DO2150I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.4)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 2150 CONTINUE
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
      DO2160J=1,NUMAL2
C
        AMAT(J,2)=STATV2
        ALPHT=(1.0 - ALPHA2(J))/2.0
        ALPHT=1.0 - ALPHT
        CALL NORPPF(ALPHT,CUTTMP)
        AMAT(J,3)=CUTTMP
        IVALUE(J,4)(1:6)='REJECT'
        IF(ABS(STATV2).LT.AMAT(J,3))THEN
          IVALUE(J,4)(1:6)='ACCEPT'
        ENDIF
        NCVALU(J,4)=6
C
        ALPHAT=100.0*ALPHA2(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 2160 CONTINUE
C
      ICNT=NUMAL2
      NUMLIN=3
      NUMCOL=4
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
C
      IF(ICASA2.EQ.'TWOT')THEN
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASA2.EQ.'LOWE')THEN
        ITITLE='Lower-Tailed Test: Normal Approximation'
        NCTITL=39
        IF(ICASAN.EQ.'ONES')THEN
          ITITL9='H0: u = d0; Ha: u < d0'
          NCTIT9=22
        ELSE
          ITITL9='H0: u1 - u2 = d0; Ha: u1 - u2 < d0'
          NCTIT9=34
        ENDIF
C
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
C
        NMAX=0
        NUMCOL=4
        DO2250I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2250   CONTINUE
C
        DO2260J=1,NUMALP
          AMAT(J,2)=STATV2
          ALPHAT=1.0 - ALPHA(J)
          CALL NORPPF(ALPHAT,ATEMP)
          AMAT(J,3)=ATEMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATV2.GE.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 2260   CONTINUE
C
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASA2.EQ.'UPPE')THEN
        ITITLE='Upper-Tailed Test: Normal Approximation'
        NCTITL=39
        IF(ICASAN.EQ.'ONES')THEN
          ITITL9='H0: u = d0; Ha: u > d0'
          NCTIT9=22
        ELSE
          ITITL9='H0: u1 - u2 = d0; Ha: u1 - u2 > d0'
          NCTIT9=35
        ENDIF
C
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (>)'
        NCTIT2(3,3)=9
C
        NMAX=0
        NUMCOL=4
        DO2350I=1,NUMCOL
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
 2350 CONTINUE
C
        DO2360J=1,NUMALP
          AMAT(J,2)=STATV2
          ALPHAT=ALPHA(J)
          CALL NORPPF(ALPHAT,ATEMP)
          AMAT(J,3)=ATEMP
          IVALUE(J,4)(1:6)='REJECT'
          IF(STATV2.LE.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 2360 CONTINUE
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
      ENDIF
C
C     NOW PERFORM THE "EXACT" TEST IF:
C
C         1) SAMPLE SIZE <= 50
C         2) LESS THAN 10% OF RANKS ARE TIES
C
      ISTEPN='31A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ITAB.NE.1)GOTO9000
C
      ITITLE='Two-Tailed Test: Exact Small Sample (Assumes No Ties)'
      NCTITL=53
      IF(ICASAN.EQ.'ONES')THEN
        ITITL9='H0: u = d0; Ha: u <> d0'
        NCTIT9=23
      ELSE
        ITITL9='H0: u1 - u2 = d0; Ha: u1 - u2 <> d0'
        NCTIT9=35
      ENDIF
C
      DO3130J=1,5
        DO3140I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 3140   CONTINUE
 3130 CONTINUE
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
      ITITL2(1,3)='Lower'
      NCTIT2(1,3)=5
      ITITL2(2,3)='Critical'
      NCTIT2(2,3)=8
      ITITL2(3,3)='Value (<)'
      NCTIT2(3,3)=9
C
      ITITL2(1,4)='Upper'
      NCTIT2(1,4)=5
      ITITL2(2,4)='Critical'
      NCTIT2(2,4)=8
      ITITL2(3,4)='Value (>)'
      NCTIT2(3,4)=9
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
      DO3150I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.5)THEN
          ITYPCO(I)='ALPH'
        ENDIF
 3150 CONTINUE
C
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
      IWRTF(5)=IWRTF(4)+IINC
C
      DO3160J=1,NUMAL2
C
        AMAT(J,2)=STATVA
        IF(J.EQ.1)THEN
          AMAT(J,3)=CVL200
          AMAT(J,4)=CVU800
        ELSEIF(J.EQ.2)THEN
          AMAT(J,3)=CVL100
          AMAT(J,4)=CVU900
        ELSEIF(J.EQ.3)THEN
          AMAT(J,3)=CVL050
          AMAT(J,4)=CVU950
        ELSEIF(J.EQ.4)THEN
          AMAT(J,3)=CVL025
          AMAT(J,4)=CVU975
        ELSEIF(J.EQ.5)THEN
          AMAT(J,3)=CVL005
          AMAT(J,4)=CVU995
        ENDIF
        IVALUE(J,5)(1:6)='ACCEPT'
        IF(STATVA.LT.AMAT(J,3))THEN
          IVALUE(J,5)(1:6)='REJECT'
        ELSEIF(STATVA.GT.AMAT(J,4))THEN
          IVALUE(J,5)(1:6)='REJECT'
        ENDIF
        NCVALU(J,5)=6
C
        ALPHAT=100.0*ALPHA2(J)
        WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
        IVALUE(J,1)(5:5)='%'
        NCVALU(J,1)=5
 3160 CONTINUE
C
      ICNT=NUMAL2
      NUMLIN=3
      IFRST=.TRUE.
      ILAST=.TRUE.
      IFLAGS=.TRUE.
      IFLAGE=.TRUE.
C
      IF(ICASA2.EQ.'TWOT')THEN
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASA2.EQ.'LOWE')THEN
        ITITLE='Lower-Tailed Test: Exact Small Sample (Assumes No Ties)'
        NCTITL=55
        IF(ICASAN.EQ.'ONES')THEN
          ITITL9='H0: u = d0; Ha: u < d0'
          NCTIT9=22
        ELSE
          ITITL9='H0: u1 - u2 = d0; Ha: u1 - u2 < d0'
          NCTIT9=34
        ENDIF
C
        ITITL2(1,3)='Lower'
        NCTIT2(1,3)=5
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (<)'
        NCTIT2(3,3)=9
C
        ITITL2(1,4)='Null'
        NCTIT2(1,4)=4
        ITITL2(2,4)='Hypothesis'
        NCTIT2(2,4)=10
        ITITL2(3,4)='Conclusion'
        NCTIT2(3,4)=10
C
        ITITL2(1,5)=' '
        NCTIT2(1,5)=0
        ITITL2(2,5)=' '
        NCTIT2(2,5)=0
        ITITL2(3,5)=' '
        NCTIT2(3,5)=0
C
        NMAX=0
        NUMCOL=4
        DO3250I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.4)THEN
            ITYPCO(I)='ALPH'
          ENDIF
 3250   CONTINUE
C
        DO3260J=1,NUMALP
C
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CVL500
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CVL200
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CVL100
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CVL050
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CVL025
          ELSEIF(J.EQ.6)THEN
            AMAT(J,3)=CVL010
          ELSEIF(J.EQ.7)THEN
            AMAT(J,3)=CVL005
          ENDIF
          IVALUE(J,4)(1:6)='ACCEPT'
          IF(STATVA.LT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='REJECT'
          ENDIF
          NCVALU(J,4)=6
C
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 3260   CONTINUE
C
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
C
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
      IF(ICASA2.EQ.'UPPE')THEN
        ITITLE='Upper-Tailed Test: Exact Small Sample (Assumes No Ties)'
        NCTITL=55
        IF(ICASAN.EQ.'ONES')THEN
          ITITL9='H0: u = d0; Ha: u > d0'
          NCTIT9=22
        ELSE
          ITITL9='H0: u1 - u2 = d0; Ha: u1 - u2 > d0'
          NCTIT9=34
        ENDIF
C
        ITITL2(1,3)='Upper'
        NCTIT2(1,3)=5
        ITITL2(2,3)='Critical'
        NCTIT2(2,3)=8
        ITITL2(3,3)='Value (>)'
        NCTIT2(3,3)=9
C
        ITITL2(1,4)='Null'
        NCTIT2(1,4)=4
        ITITL2(2,4)='Hypothesis'
        NCTIT2(2,4)=10
        ITITL2(3,4)='Conclusion'
        NCTIT2(3,4)=10
C
        ITITL2(1,5)=' '
        NCTIT2(1,5)=0
        ITITL2(2,5)=' '
        NCTIT2(2,5)=0
        ITITL2(3,5)=' '
        NCTIT2(3,5)=0
C
        NMAX=0
        NUMCOL=4
        DO3350I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.4)THEN
            ITYPCO(I)='ALPH'
          ENDIF
 3350   CONTINUE
C
        DO3360J=1,NUMALP
C
          AMAT(J,2)=STATVA
          IF(J.EQ.1)THEN
            AMAT(J,3)=CVU500
          ELSEIF(J.EQ.2)THEN
            AMAT(J,3)=CVU800
          ELSEIF(J.EQ.3)THEN
            AMAT(J,3)=CVU900
          ELSEIF(J.EQ.4)THEN
            AMAT(J,3)=CVU950
          ELSEIF(J.EQ.5)THEN
            AMAT(J,3)=CVU975
          ELSEIF(J.EQ.6)THEN
            AMAT(J,3)=CVU990
          ELSEIF(J.EQ.7)THEN
            AMAT(J,3)=CVU995
          ENDIF
          IVALUE(J,4)(1:6)='ACCEPT'
          IF(STATVA.GT.AMAT(J,3))THEN
            IVALUE(J,4)(1:6)='REJECT'
          ENDIF
          NCVALU(J,4)=6
C
          ALPHAT=100.0*ALPHA(J)
          WRITE(IVALUE(J,1)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,1)(5:5)='%'
          NCVALU(J,1)=5
 3360   CONTINUE
C
        ICNT=NUMALP
        NUMLIN=3
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
C
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'WIL2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWIL2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATV2,STATCD,PVAL2T,PVALLT,PVALUT
 9013   FORMAT('STATVA,STATV2,STATCD,PVAL2T,PVALLT,PVALUT = ',6G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWIL3(Y1,Y2,N1,D0,ICASAN,ICASA2,
     1                  TEMP1,TEMP2,TEMP3,MAXNXT,
     1                  STATVA,STATV2,STATCD,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  NTEMP,NPLUS,NMINUS,NTIES,
     1                  TPLUS,TMINUS,RSUM,RSUMSQ,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE COMPUTES THE WILCOXON SIGNED RANKED TEST
C              STATISTIC AND ASSOCIATED CDF AND P-VALUES.
C
C              THIS PART IS EXTRACTED FROM DPWIL2 IN ORDER TO
C              ALLOW IT TO BE COMPUTED FROM THE "STATISTICS" ROUTINES
C              (E.G., STATISTIC PLOT, BOOTSTRAP).
C
C              ALSO, PREVIOUS VERSIONS USED A FORM OF THE TEST
C              GIVEN BY WALPOLE AND MEYERS.  THIS VERSION SWITCHES
C              TO THE ONE USED BY CONOVER.  THE CONOVER VERSION IS
C              THE MORE COMMONLY USED AND ALSO PROVIDES MORE EXTENSIVE
C              TABLED VALUES FOR SMALL SAMPLES.
C
C              THE CDF AND P-VALUES ARE THE NORMAL APPROXIMATIONS
C              (THE TABLED CRITICAL VALUES ARE GENERATED IN DPWIL2).
C
C     EXAMPLE--SIGNED RANK TEST Y1 Y2
C              SIGNED RANK TEST Y1 Y2 D0
C              SIGNED RANK TEST Y1 D0
C     SAMPLE 1 IS IN INPUT VECTOR Y1 (WITH N1 OBSERVATIONS)
C     SAMPLE 2 IS IN INPUT VECTOR Y2 (WITH N1 OBSERVATIONS).
C     REFERENCE--CONOVER (1999), "PRACTICAL NONPARAMETRIC STATISTICS",
C                THIRD EDITION, WILEY, PP. 352 - 360.
C              --WALPOLE AND MEYERS (19xx), "ENGINEERING STATISTICS",
C                XX, PP. XX.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/5
C     ORIGINAL VERSION--MAY       2011. EXTRACTED FROM DPWIL2
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      DOUBLE PRECISION TPLUS
      DOUBLE PRECISION TMINUS
      DOUBLE PRECISION RSUM
      DOUBLE PRECISION RSUMSQ
C
C---------------------------------------------------------------------
C
      DIMENSION Y1(*)
      DIMENSION Y2(*)
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWI'
      ISUBN2='L3  '
      IERROR='NO'
      IWRITE='OFF'
C
      STATVA=CPUMIN
      STATV2=CPUMIN
      STATCD=CPUMIN
      PVAL2T=CPUMIN
      PVALLT=CPUMIN
      PVALUT=CPUMIN
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPWIL3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASA2,N1,D0
   52   FORMAT('IBUGA3,ISUBRO,ICASA2,N1,D0 = ',3(A4,2X),I8,G15.7)
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N1
          WRITE(ICOUT,57)I,Y1(I),Y2(I)
   57     FORMAT('I,Y1(I),Y2(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   56   CONTINUE
      ENDIF
C
C               ********************************************
C               **  STEP 01--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      ISTEPN='01'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N1.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR IN WILCOXON PAIRED SIGNED RANK TEST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,112)
  112   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS FOR THE ',
     1         'RESPONSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      VARIABLES MUST BE 2 OR LARGER.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,116)
  116   FORMAT('      SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,117)N1
  117   FORMAT('      THE INPUT NUMBER OF OBSERVATIONS HERE = ',I8,
     1         '.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y1(1)
      DO135I=2,N1
        IF(Y1(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE FIRST RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1       G15.7)
      CALL DPWRST('XXX','WRIT')
      IERROR='YES'
      GOTO9000
  139 CONTINUE
C
      IF(ICASAN.EQ.'TWOS')THEN
        HOLD=Y2(1)
        DO145I=2,N1
          IF(Y2(I).NE.HOLD)GOTO149
  145   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,141)HOLD
  141   FORMAT('      THE SECOND RESPONSE VARIABLE HAS ALL ELEMENTS = ',
     1         G15.7)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
  149   CONTINUE
      ENDIF
C
C               ************************************
C               **   STEP 11--                    **
C               **   BRANCH DEPENDING ON WHETHER  **
C               **   1-SAMPLE SIGNED RANK TEST OR **
C               **   2-SAMPLE SIGNED RANK TEST.   **
C               ************************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     NOTE: OMIT ANY VALUES WHERE DIFFERENCE EQUAL D0.
C
      ICNT=0
      IF(ICASAN.EQ.'ONES')THEN
        DO1110I=1,N1
          DIFF=ABS(Y1(I) - D0)
          IF(DIFF.NE.0.0)THEN
            ICNT=ICNT+1
            TEMP1(ICNT)=DIFF
            Y1(ICNT)=Y1(I)
          ENDIF
 1110   CONTINUE
      ELSE
        DO1120I=1,N1
          DIFF=ABS(Y1(I) - Y2(I) - D0)
          IF(DIFF.NE.0.0)THEN
            ICNT=ICNT+1
            TEMP1(ICNT)=DIFF
            Y1(ICNT)=Y1(I)
            Y2(ICNT)=Y2(I)
          ENDIF
 1120   CONTINUE
      ENDIF
      NTEMP=ICNT
C
      ISTEPN='11B'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL RANK(TEMP1,NTEMP,IWRITE,TEMP2,TEMP3,MAXNXT,IBUGA3,IERROR)
C
C     CHECK FOR NUMBER OF TIED RANKS.  BASICALLY, IF THE RANK IS
C     A NON-INTEGER VALUE, THIS IMPLIES THAT IT IS A TIED RANK.
C     DATAPLOT WILL ONLY PRINT THE EXACT TABLE IF THE SAMPLE SIZE
C     IS <= 50 AND LESS THAN 10% OF THE RANKS ARE TIES.
C
      NTIES=0
      NPLUS=0
      NMINUS=0
      TPLUS=0.0D0
      TMINUS=0.0D0
      RSUM=0.0D0
      RSUMSQ=0.0D0
      D0TEMP=D0
C
      IF(ICASAN.EQ.'ONES')THEN
        DO1130I=1,NTEMP
          ARANK=TEMP2(I)
          ITEMP=INT(ARANK)
          ATEMP=ARANK - REAL(ITEMP)
          IF(ABS(ATEMP).GE.0.1)NTIES=NTIES+1
          RSUMSQ=RSUMSQ + DBLE(ARANK)**2
          IF(Y1(I).GT.D0TEMP)THEN
            TPLUS=TPLUS + DBLE(ARANK)
            NPLUS=NPLUS + 1
          ELSE
            TMINUS=TMINUS + DBLE(ARANK)
            NMINUS=NMINUS + 1
          ENDIF
 1130   CONTINUE
      ELSE
        DO1140I=1,NTEMP
          ARANK=TEMP2(I)
          ITEMP=INT(ARANK)
          ATEMP=ARANK - REAL(ITEMP)
          IF(ABS(ATEMP).GE.0.1)NTIES=NTIES+1
          RSUMSQ=RSUMSQ + DBLE(ARANK)**2
          IF(Y1(I) - Y2(I).GT.D0TEMP)THEN
            TPLUS=TPLUS + DBLE(ARANK)
            NPLUS=NPLUS + 1
          ELSE
            TMINUS=TMINUS + DBLE(ARANK)
            NMINUS=NMINUS + 1
          ENDIF
 1140   CONTINUE
      ENDIF
C
      ISTEPN='11C'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      RSUM=TPLUS - TMINUS
      STATVA=TPLUS
      STATV2=RSUM/DSQRT(RSUMSQ)
      CALL NORCDF(STATV2,STATCD)
C
C     NOW COMPUTE CDF, P-VALUES
C
      Z=(RSUM+1.0D0)/DSQRT(RSUMSQ)
      CALL NORCDF(Z,PVALLT)
C
      Z=(RSUM-1.0D0)/DSQRT(RSUMSQ)
      CALL NORCDF(Z,PVALUT)
      PVALUT= 1.0 - PVALUT
C
      PVAL=MIN(PVALLT,PVALUT)
      PVAL2T=2.0*PVAL
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WIL3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWIL3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9013)STATVA,STATV2,STATCD
 9013   FORMAT('STATVA,STATV2,STATCD = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9014)PVALLT,PVALUT,PVAL2T
 9014   FORMAT('PVALLT,PVALUT,PVAL2T = ',3G15.7)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9015)NTIES,NPLUS,NMINUS
 9015   FORMAT('NTIES,NPLUS,NMINUS = ',3I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9017)TPLUS,TMINUS,RSUM,RSUMSQ
 9017   FORMAT('TPLUS,TMINUS,RSUM,RSUMSQ = ',4G15.7)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWIL5(ICASAN,ICASA2,
     1                  STATVA,STATV2,STATCD,
     1                  PVAL2T,PVALLT,PVALUT,
     1                  CTL005,CTL010,CTL025,CTL050,CTL100,
     1                  CTL200,CTL500,
     1                  CTU995,CTU990,CTU975,CTU950,CTU900,
     1                  CTU800,CTU500,
     1                  CVL005,CVL010,CVL025,CVL050,CVL100,
     1                  CVL200,CVL500,
     1                  CVU995,CVU990,CVU975,CVU950,CVU900,
     1                  CVU800,CVU500,
     1                  IFLAGU,IFRST,ILAST,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPWILC TO UPDATE VARIOUS
C              INTERNAL PARAMETERS AFTER A WILCOXON SIGNED RANK TEST.
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
C     VERSION NUMBER--2011/5
C     ORIGINAL VERSION--MAY       2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
      CHARACTER*4 ICASA2
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
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
C
      CHARACTER*4 IOP
      SAVE IOUNI1
      SAVE IOUNI2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WIL5')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWIL5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)STATVA,STATV2,STATCD,PVAL2T,PVALLT,PVALUT
   53   FORMAT('STATVA,STATV2,STATCD,PVAL2T,PVALLT,PVALUT = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ICASAN,ICASA2
   55   FORMAT('ICASAN,ICASA2 = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFLAGU.EQ.'FILE')THEN
C
        IF(IFRST)THEN
          IOP='OPEN'
          IFLAG1=1
          IFLAG2=1
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
          WRITE(IOUNI1,295)
  295     FORMAT(10X,'STATVAL',8X,'STATCDF',
     1            9X,'PVAL2T',9X,'PVALLT',9X,'PVALUT',
     1            7X,'CUTLOW50',7X,'CUTLOW20',7X,'CUTLOW10',
     1            7X,'CUTLOW05',7X,'CUTLO025',7X,'CUTLOW01',
     1            7X,'CUTLO005',
     1            7X,'CUTUPP50',7X,'CUTUPP80',7X,'CUTUPP90',
     1            7X,'CUTUPP95',7X,'CUTUP975',7X,'CUTUPP99',
     1            7X,'CUTUP995')
          WRITE(IOUNI2,296)
  296     FORMAT(10X,'STATVAL',
     1            8X,'CVLOW50',8X,'CVLOW20',8X,'CVLOW10',
     1            8X,'CVLOW05',7X,'CVLOW025',8X,'CVLOW01',
     1            7X,'CVLOW005',
     1            8X,'CVUPP50',8X,'CVUPP80',8X,'CVUPP90',
     1            8X,'CVUPP95',7X,'CVUPP975',8X,'CVUPP99',
     1            7X,'CVUPP995')
        ENDIF
        WRITE(IOUNI1,298)STATV2,STATCD,PVAL2T,PVALLT,PVALUT,
     1                   CTL500,CTL200,CTL100,CTL050,CTL025,
     1                   CTL010,CTL005,
     1                   CTU500,CTU800,CTU900,CTU950,CTU975,CTU990,
     1                   CTU995
  298   FORMAT(19E15.7)
        WRITE(IOUNI2,299)STATVA,
     1                   CVL500,CVL200,CVL100,CVL050,CVL025,
     1                   CVL010,CVL005,
     1                   CVU500,CVU800,CVU900,CVU950,CVU975,
     1                   CVU990,CVU995
  299   FORMAT(15E15.7)
      ELSEIF(IFLAGU.EQ.'ON')THEN
        IH='STAT'
        IH2='VALE'
        VALUE0=STATVA
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='VALN'
        VALUE0=STATV2
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='STAT'
        IH2='CDF '
        VALUE0=STATCD
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
        IH='PVAL'
        IH2='UEUT'
        VALUE0=PVALUT
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP50'
        VALUE0=CTU500
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='OW50'
        VALUE0=CTL500
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P50'
        VALUE0=CVU500
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W50'
        VALUE0=CVL500
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP80'
        VALUE0=CTU800
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='OW20'
        VALUE0=CTL200
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P80'
        VALUE0=CVU800
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W20'
        VALUE0=CVL200
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP90'
        VALUE0=CTU900
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='OW10'
        VALUE0=CTL100
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P90'
        VALUE0=CVU900
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W10'
        VALUE0=CVL100
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP95'
        VALUE0=CTU950
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='OW95'
        VALUE0=CTL050
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P95'
        VALUE0=CVU950
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W05'
        VALUE0=CVL050
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P975'
        VALUE0=CTU975
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='O025'
        VALUE0=CTL025
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P975'
        VALUE0=CVU975
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W975'
        VALUE0=CVL025
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='PP99'
        VALUE0=CTU990
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='OW01'
        VALUE0=CTL010
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P99'
        VALUE0=CVU990
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W01'
        VALUE0=CVL010
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTU'
        IH2='P995'
        VALUE0=CTU995
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CUTL'
        IH2='O005'
        VALUE0=CTL005
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVUP'
        IH2='P995'
        VALUE0=CVU995
        CALL DPADDP(IH,IH2,VALUE0,IHOST1,ISUBN0,
     1              IHNAME,IHNAM2,IUSE,VALUE,IVALUE,NUMNAM,MAXNAM,
     1              IANS,IWIDTH,IBUGA3,IERROR)
C
        IH='CVLO'
        IH2='W995'
        VALUE0=CVL005
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
          IFLAG2=1
          IFLAG3=0
          IFLAG4=0
          IFLAG5=0
          CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1                IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1                IBUGA3,ISUBRO,IERROR)
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WIL5')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR
  301       FORMAT('AFTER CALL DPCLFI, IERROR = ',A4)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WIL5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPWIL5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRFI(IOTERM,IOFILE,IPR2,IOUNIT,IFMFLG,
     1                  IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  ICWRIF,NCWRIF,IFWORD,IFQUOT,ICASWR,ICASW2,
     1                  IWRIRW,IWRIHE,NCWRIH,IEXCEL,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--UTILITY ROUTINE FOR DPWRIT:
C              1) DETERMINE IF WRITE TO FILE OR TERMINAL
C              2) FOR FILE, EXTRACT THE FILE NAME AND
C                 OPEN THE FILE
C              3) SET THE APPROPRIATE UNIT NUMBERS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/3
C     ORIGINAL VERSION--MARCH     2009. EXTRACT AS DISTINCT
C                                       SUBROUTINE
C     UPDATED  VERSION--JULY      2016. SUPPORT FOR HEADER FILE
C     UPDATED  VERSION--DECEMBER  2018. SUPPORT FOR WRITE1/WRITE2/
C                                       WRITE3
C     UPDATED  VERSION--FEBRUARY  2020. FOR WRITE EXCEL OPTION, FIRST
C                                       WRITE VAARIABLES TO "dpst1f.dat"
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*80 ICWRIF
      CHARACTER*80 IWRIHE
C
      CHARACTER*4 ICASWR
      CHARACTER*4 ICASW2
      CHARACTER*4 IWRIRW
      CHARACTER*4 IEXCEL
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      CHARACTER*4 IOFILE
      CHARACTER*4 IOTERM
      CHARACTER*4 IFMFLG
C
CCCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
C
CCCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4 IERRF2
      CHARACTER*4 IENDF2
      CHARACTER*4 IREWI2
      CHARACTER*240 IATEMP
C
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
CCCCC CHARACTER*200 ICANS
      CHARACTER (LEN=MAXSTR) :: ICANS
C
C---------------------------------------------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWR'
      ISUBN2='FI  '
      IFOUND='YES'
      IERROR='NO'
      IOFILE='-999'
      IOTERM='-999'
C
      IFQUOT=0
      ILAST=0
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X,1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRFI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGS2,ISUBRO,IERROR,ICASW2,IEXCEL
   54   FORMAT('IBUGS2,ISUBRO,IERROR,ICASW2,IEXCEL = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,65)(IANSLC(I),I=1,IWIDTH)
   65     FORMAT('(IANSLC(I),I=1,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(ICASW2.EQ.'0')THEN
          WRITE(ICOUT,71)IWRINU,NCWRIF
   71     FORMAT('IWRINU,NCWRIF = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,72)IWRINA
   72     FORMAT('IWRINA = ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,73)IWRIST,IWRIFO,IWRIAC,IWRIFO,IWRICS
   73     FORMAT('IWRIST,IWRIFO,IWRIAC,IWRIFO,IWRICS = ',5(A12,2X))
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASW2.EQ.'1')THEN
          WRITE(ICOUT,74)IWRIN1,NCWRIF
   74     FORMAT('IWRIN1,NCWRIF = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,75)IWRNA1
   75     FORMAT('IWRNA1 = ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,76)IWRIS1,IWRIF1,IWRIA1,IWRIF1,IWRIC1
   76     FORMAT('IWRIS1,IWRIF1,IWRIA1,IWRIF1,IWRIC1 = ',5(A12,2X))
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASW2.EQ.'2')THEN
          WRITE(ICOUT,77)IWRIN2,NCWRIF
   77     FORMAT('IWRIN2,NCWRIF = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,78)IWRNA2
   78     FORMAT('IWRNA2 = ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,79)IWRIS2,IWRIF2,IWRIA2,IWRIF2,IWRIC2
   79     FORMAT('IWRIS2,IWRIF2,IWRIA2,IWRIF2,IWRIC2 = ',5(A12,2X))
          CALL DPWRST('XXX','BUG ')
        ELSEIF(ICASW2.EQ.'3')THEN
          WRITE(ICOUT,80)IWRIN3,NCWRIF
   80     FORMAT('IWRIN3,NCWRIF = ',2I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,81)IWRNA3
   81     FORMAT('IWRNA2 = ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,82)IWRIS3,IWRIF3,IWRIA3,IWRIF3,IWRIC3
   82     FORMAT('IWRIS3,IWRIF3,IWRIA3,IWRIF3,IWRIC3 = ',5(A12,2X))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NCWRIF.GE.1)THEN
          WRITE(ICOUT,85)(ICWRIF(I:I),I=1,NCWRIF)
   85     FORMAT('(ICWRIF(I:I),I=1,NCWRIF) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  DETERMINE THE TYPE OF WRITE CASE--              **
C               **       1) TO TERMINAL;                            **
C               **       2) TO FILE;                                **
C               **  NOTE--IOTERM WILL = 'YES' ONLY IN EXPLICIT      **
C               **        TERMINAL CASE.                            **
C               **  NOTE--IOFILE WILL = 'YES' ONLY IN FILE CASE.    **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWORD=2
      IF(ICASWR.EQ.'ON')IWORD=3
      IF(IEXCEL.EQ.'ON')IWORD=3
      CALL DPFILE(IANSLC,IWIDTH,IWORD,
     1IOFILE,IBUGS2,ISUBRO,IERROR)
C
      IOTERM='NO'
      IF(IOFILE.EQ.'YES')THEN
        IF(IHARG(1).EQ.'TERM'.AND.IHARG2(1).EQ.'INAL')THEN
          IOFILE='NO'
          IOTERM='YES'
        ENDIF
      ENDIF
C
      IF(IEXCEL.EQ.'ON')THEN
        IF(IOFILE.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,211)
  211     FORMAT('      NO FILE NAME SPECIFIED ON WRITE EXCEL ',
     1           'COMMAND.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
      ENDIF
C
C               *************************************
C               **  STEP 2--                       **
C               **  IF HAVE THE FILE OUTPUT CASE-- **
C               **  COPY OVER VARIABLES            **
C               *************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2018/12: SUPPORT "WRITE1", "WRITE2", "WRITE3" CASES.  THESE
C              SUPPORT WRITING TO THREE ADDITIONAL UNITS.
C
C              THE STANDARD WRITE COMMAND OPENS THE FILE, DOES
C              THE WRITE, AND THEN CLOSES THE FILE.  YOU CAN
C              USE THE SET WRITE REWIND OFF TO SUPPRESS THE
C              CLOSING OF THE FILE.
C
C              THE NEW WRITE UNITS ESSENTIALLY WORK IN AN APPEND
C              MODE.  THAT IS, THE FILE IS ONLY OPENED IF THE
C              CURRENT STATUS IS CLOSED.  ALSO, THE CLOSE AFTER
C              THE WRITE IS SUPPRESSED.
C
C              THE PURPOSE OF THESE FILES IS TO ALLOW YOU TO
C              WRITE CERTAIN TYPES OF OUTPUT TO FILE WHILE
C              ALLOWING THE STANDARD WRITE COMMAND TO OPERATE
C              NORMALLY.  FOR EXAMPLE, IF YOU ARE DOING AN ANALYSIS
C              WITH MULTIPLE PARTS, YOU MAY WANT TO SELECTIVELY
C              WRITE CERTAIN RESULTS FROM EACH PART TO A SINGLE
C              FILE.
C
      IF(IOFILE.EQ.'YES')THEN
C
        IF(ICASW2.EQ.'1')THEN
          IOUNIT=IWRIN1
          IFILE=IWRNA1
          ISTAT=IWRIS1
          IFORM=IWRIF1
          IACCES=IWRIA1
          IPROT=IWRIP1
          ICURST=IWRIC1
        ELSEIF(ICASW2.EQ.'2')THEN
          IOUNIT=IWRIN2
          IFILE=IWRNA2
          ISTAT=IWRIS2
          IFORM=IWRIF2
          IACCES=IWRIA2
          IPROT=IWRIP2
          ICURST=IWRIC2
        ELSEIF(ICASW2.EQ.'3')THEN
          IOUNIT=IWRIN3
          IFILE=IWRNA3
          ISTAT=IWRIS3
          IFORM=IWRIF3
          IACCES=IWRIA3
          IPROT=IWRIP3
          ICURST=IWRIC3
        ELSE
          IOUNIT=IWRINU
          IFILE=IWRINA
          ISTAT=IWRIST
          IFORM=IWRIFO
          IACCES=IWRIAC
          IPROT=IWRIPR
          ICURST=IWRICS
        ENDIF
C
        ISUBN0='WRFI'
        IERRFI='NO'
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')THEN
          WRITE(ICOUT,1183)IOUNIT
 1183     FORMAT('IOUNIT = ',I8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1184)IFILE
 1184     FORMAT('IFILE = ',A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1185)ISTAT,IFORM,IACCES,IPROT,ICURST
 1185     FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST = ',
     1           A12,2X,A12,2X,A12,2X,A12,2X,A12)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1186)ISUBN0,IERRFI
 1186     FORMAT('ISUBN0,IERRFI = ',A4,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C               ***********************************************
C               **  STEP 3--                                 **
C               **  IF HAVE THE FILE OUTPUT CASE--           **
C               **  CHECK TO SEE IF THE WRITE FILE MAY EXIST **
C               ***********************************************
C
        ISTEPN='3'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(ISTAT.EQ.'NONE')THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1211)
 1211     FORMAT('***** IMPLEMENTATION ERROR IN WRITE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1212)
 1212     FORMAT('      THE DESIRED WRITING CANNOT BE CARRIED OUT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1214)
 1214     FORMAT('      BECAUSE THE INTERNAL VARIABLE    IWRIST ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1215)
 1215     FORMAT('      WHICH ALLOWS SUCH WRITING HAS BEEN SET TO ',
     1           '   NONE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1217)ISTAT,IWRIST
 1217     FORMAT('ISTAT,IWRIST = ',A12,2X,A12)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1218)
 1218     FORMAT('      ALL WRITING MUST BE DONE DIRECTLY TO THE ',
     1           'TERMINAL')
          CALL DPWRST('XXX','BUG ')
          GOTO9000
        ENDIF
C
C               *************************************
C               **  STEP 4--                       **
C               **  IF HAVE THE FILE INPUT CASE--  **
C               **  EXTRACT THE FILE NAME          **
C               *************************************
C
        ISTEPN='4'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        DO1310I=1,200
          ICANS(I:I)=IANSLC(I)
 1310   CONTINUE
C
        ISTART=1
        ISTOP=IWIDTH
        IWORD=2
        IF(ICASWR.EQ.'ON')IWORD=3
        IF(IEXCEL.EQ.'ON')IWORD=3
        CALL DPEXWO(ICANS,ISTART,ISTOP,IWORD,
     1              ICOL1,ICOL2,IFILE,NCFILE,
     1              IBUGS2,ISUBRO,IERROR)
C
        IF(NCFILE.LT.1)THEN
          IERROR='YES'
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
 1341     FORMAT('***** ERROR IN WRITE COMMAND--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1342)
 1342     FORMAT('      A USER FILE NAME IS REQUIRED IN THE WRITE ',
     1           'COMMAND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1344)
 1344     FORMAT('      (FOR EXAMPLE,    WRITE CALIB.DAT X Y Z)')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1345)
 1345     FORMAT('      BUT NONE WAS GIVEN HERE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1346)
 1346     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,1347)(IANSLC(I),I=1,MIN(100,IWIDTH))
 1347       FORMAT('      ',100A1)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          GOTO9000
        ENDIF
C
        IFWORD=0
        IFQUOT=0
        IF(IFILE(1:1).EQ.'"')THEN
          IFQUOT=1
          DO1351I=MAXFNC,1,-1
            IF(IFILE(I:I).NE.' ')THEN
              ILAST=I
              GOTO1354
            ENDIF
 1351     CONTINUE
 1354     CONTINUE
          ICOUNT=0
          ISPAC=0
          DO1356I=1,ILAST
            IF((IFILE(I:I).EQ.' '.OR.IFILE(I:I).EQ.'-') .AND.
     1         ISPAC.EQ.0)THEN
              ISPAC=1
              ICOUNT=ICOUNT+1
            ELSEIF((IFILE(I:I).NE.' '.AND.IFILE(I:I).NE.'-') .AND.
     1        ISPAC.EQ.1)THEN
              ISPAC=0
            ENDIF
 1356     CONTINUE
          IFWORD=ICOUNT
        ENDIF
C
        IF(IEXCEL.EQ.'ON')GOTO9000
C
C               *************************************
C               **  STEP 5--                       **
C               **  IF HAVE THE FILE INPUT CASE--  **
C               **  OPEN THE FILE                  **
C               *************************************
C
        ISTEPN='5'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')THEN
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,1184)IFILE
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1391)IWRICS
 1391     FORMAT('IWRICS: ',A12)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1393)ICURST
 1393     FORMAT('ICURST: ',A12)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IREWIN='ON'
        IF(ICASW2.EQ.'1' .OR. ICASW2.EQ.'2' .OR. ICASW2.EQ.'3')THEN
          IREWIN='OFF'
        ELSEIF(IWRIRW.EQ.'OFF')THEN
          IREWIN='OFF'
        ENDIF
        IFMFLG='OFF'
        IF(NCWRIF.GE.1)THEN
          IF(ICWRIF(1:5).EQ.'(UNFO'.OR.ICWRIF(1:5).EQ.'(BINA')THEN
            IFORM='UNFORMATTED'
            IFMFLG='ON'
          ENDIF
        ENDIF
C
        IF(ICURST(1:4).EQ.'CLOS')
     1    CALL DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
        IF(ICURST(1:4).EQ.'CLOS')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1341)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1396)
 1396     FORMAT('      UNABLE TO OPEN THE WRITE FILE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1397)IFILE
 1397     FORMAT('      FILE NAME: ',A80)
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
C
        IF(ICASW2.EQ.'1')THEN
          IWRIC1='OPEN'
        ELSEIF(ICASW2.EQ.'2')THEN
          IWRIC2='OPEN'
        ELSEIF(ICASW2.EQ.'3')THEN
          IWRIC3='OPEN'
        ENDIF
C
        IF(IERRFI.EQ.'YES')THEN
          IERROR='YES'
          GOTO9000
        ENDIF
C
C               *************************************
C               **  STEP 5B-                       **
C               **  IF HAVE HEADER FILE, OPEN THE  **
C               **  HEADER AND COPY CONTENTS TO    **
C               **  WRITE FILE.                    **
C               *************************************
C
        ISTEPN='5B'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IF(NCWRIH.GE.1 .AND. IWRIHE.NE.'NULL')THEN
          IOUNI2=IST1NU
          IFILE2=IWRIHE
          ISTAT2='OLD'
          IFORM2='FORMATTED'
          IACCE2='SEQUENTIAL'
          IPROT2='READONLY'
          ICURS2='CLOSED'
          ISUBN0='CAPT'
          IERRF2='NO'
C
          IREWI2='ON'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO3099
C
C         NOW LOOP THROUGH FILE (ASSUME MAXIMUM OF 1,000 LINES).
C
          DO3091I=1,1000
            IATEMP=' '
            READ(IOUNI2,3092,END=3099,ERR=3099)IATEMP
 3092       FORMAT(A240)
            ILAST=1
            DO3096J=240,1,-1
              IF(IATEMP(J:J).NE.' ')THEN
                ILAST=J
                GOTO3098
              ENDIF
 3096       CONTINUE
 3098       CONTINUE
            WRITE(IWRINU,3094,IOSTAT=IOS,ERR=3080)
     1           (IATEMP(J:J),J=1,ILAST)
 3094       FORMAT(240A1)
            GOTO 3091
C
 3080       CONTINUE
            WRITE(ICOUT,3081)IWRINU
 3081       FORMAT('****** ERROR TRYING TO WRITE TO UNIT ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3082)IOS
 3082       FORMAT('       STATUS NUMBER = ',I8)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3083)
 3083       FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',
     1             'THAT DOES NOT HAVE WRITE PERMISSION.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO 9000
C
 3091     CONTINUE
 3099     CONTINUE
C
          IENDF2='OFF'
          IREWI2='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDF2,IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
CCCCC     IF(IERRF2.EQ.'YES')GOTO9000
C
        ENDIF
C
      ENDIF
C
C               ******************************************
C               **  STEP 6--                            **
C               **  FOR THE 2 CASES--                   **
C               **      1) TERMINAL OUTPUT;             **
C               **      2) FILE OUTPUT;                 **
C               **  DEFINE THE OUTPUT WRITE UNIT NUMBER,**
C               **  AND OTHER VARIABLES NEEDED          **
C               **  FOR UPCOMING WRITES.                **
C               ******************************************
C
      ISTEPN='6'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPR2=IPR
      IF(IOFILE.EQ.'YES')THEN
        IF(ICASW2.EQ.'1')THEN
          IPR2=IWRIN1
        ELSEIF(ICASW2.EQ.'2')THEN
          IPR2=IWRIN2
        ELSEIF(ICASW2.EQ.'3')THEN
          IPR2=IWRIN3
        ELSE
          IPR2=IWRINU
        ENDIF
      ENDIF
      IF(IOTERM.EQ.'YES')IPR2=IPR
C
      IOUNIT=IPR2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRFI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWRIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR
 9015   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IPR,IPR2,IOUNIT,IOFILE,IOTERM
 9017   FORMAT('IPR,IPR2,IOUNIT,IOFILE,IOTERM = ',3I8,2(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT,IFORM,IACCES,IPROT,ICURST
 9023   FORMAT('ISTAT,IFORM,IACES,IPROT,ICURST  = ',5(A12,1X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IENDFI,IREWIN,ISUBN0,IERRFI
 9028   FORMAT('IENDFI,IREWIN,ISUBN0,IERRFI = ',4(A4,1X))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRIT(IMACRO,IMACNU,IMACCS,
     1                  IFORSW,ICWRIF,NCWRIF,
     1                  IWRIRW,
     1                  IFORWI,IFORWR,MAXNWI,
     1                  IBUGS2,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--WRITE OUT VALUES OF SPECIFIC VARIABLES, OR PARAMETERS
C              OR MODELS TO AN OUTPUT MASS STORAGE FILE
C              OR (IF NO FILE GIVEN) TO THE DEFAULT OUTPUT UNIT
C              (WHICH WILL BE THE TERMINAL).
C     ASSUMPTION--THE OUTPUT FILE ALREADY EXISTS; (THAT IS, DATAPLOT
C                 WILL AUTOMATICALLY OPEN THE FILE VIA (ON THE
C                 UNIVAC 1108), BY AN @ASG,AX ...) BUT WILL NOT
C                 AUTOMATICALLY CREATE THE FILE VIA (ON THE UNIVAC 1108),
C                 BY AN @ASG,UP ...))
C     ASSUMPTION--THE COMPUTER SYSTEM IS SUCH THAT EQUATING THE FILE
C                 NAME TO THE FORTRAN NUMERIC DESIGNATION OF 32 (OR
C                 HOWEVER THE VARIABLE    IWRINU    IS DEFINED IN INITFO)
C                 IS PERMISSABLE.
C     NOTE--OUTPUT FOR THE WRITE COMMAND MAY POTENTIALLY GO TO 3
C           DIFFERENT DESTINATIONS--
C                1) THE TERMINAL ITSELF;
C                2) A FILE;
C           DIFFERENT SYSTEMS ALLOW DIFFERENT COMBINATIONS OF THE ABOVE.
C           ALL SYSTEMS WILL ALLOW OUTPUT TO THE TERMINAL ITSELF;
C           MOST SYSTEMS WILL ALLOW OUTPUT TO A FILE;
C           TO DESIGNATE WHETHER THE LAST 2 OPTIONS
C           ARE ALLOWABLE AT A GIVEN INSTALLATION,
C           THE ANALYST SETS (IN SUBROUTINE    INITFO    AT IMPLEMENTATION TIME)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--MAY       1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --DECEMBER  1978.
C     UPDATED         --MARCH     1979.
C     UPDATED         --NOVEMBER  1980.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --MARCH     1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --SEPTEMBER 1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --JANUARY   1982.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --DECEMBER  1985.
C     UPDATED         --SEPTEMBER 1987. WRITE MATRICES
C     UPDATED         --OCTOBER   1987. FORMATTED OUTPUT
C     UPDATED         --JANUARY   1988. FORMATTED OUTPUT (PARAM.)
C     UPDATED         --DECEMBER  1988. 9,10, 11, AND 12 DEC. PLACES
C     UPDATED         --DECEMBER  1988. FORMATTED WRITE
C     UPDATED         --DECEMBER  1988. WRITE UP TO 20 PARAMETERS
C     UPDATED         --AUGUST    1992. SHIFT COLUMN HEADERS
C     UPDATED         --NOVEMBER  1995. 1) SIMPLIFY CODE
C                                       2) ALLOW MORE THAN 10 VARIABLES
C                                       3) UNFORMATTED WRITE
C     UPDATED         --JULY      1996. FORMAT STATEMENTS FOR PC
C     UPDATED         --JULY      1996. BUG FIX (FOR WRITE LINES > 80 CHARACTERS)
C     UPDATED         --SEPTEMBER 1997. PC REQUIRES "1X" IN FORMAT STATEMENTS
C     UPDATED         --OCTOBER   1997. ADD "WRITE VARIABLES ALL" OPTION
C     UPDATED         --DECEMBER  1997. MAXCOL TO 100
C     UPDATED         --JULY      2003. BUG: FILE NAME < 80
C                                       CHARACTERS, BUT COMMAND LINE
C                                       > 80 CHARACTERS
C     UPDATED         --SEPTEMBER 2003. ADD "ERR" CLAUSE FOR FORMATTED
C                                       WRITE
C     UPDATED         --SEPTEMBER 2003. ADD "WRITE HTML" OPTION
C     UPDATED         --SEPTEMBER 2003. ADD "WRITE LATEX" OPTION
C     UPDATED         --JUNE      2006. FOR STRING, MAKE LEADING
C                                       SPACE USER-SETTABLE
C     UPDATED         --APRIL     2009. REWRITE TO SIMPLIFY AND IMPROVE
C                                       CLARITY.  SPLIT OFF SOME OF
C                                       THE CODE INTO "DPWRFI" AND
C                                       "DPWRI2".  REDO THE
C                                       HTML/LATEX/RTF OUTPUT.
C     UPDATED         --JULY      2009. IF RUNNING GUI, SET TABLE
C     UPDATED         --JULY      2009. SUPPORT FOR:
C                                       SET HTML TABLE FONT
C                                       SET HTML CELL WIDTH
C     UPDATED         --MAY       2012. DON'T PRINT BLANK LINE AT END
C                                       WHEN WRITING TO A FILE
C     UPDATED         --NOVEMBER  2014. WRITE STRING/PARAMETER TO
C                                       CLIPBOARD
C     UPDATED         --DECEMBER  2014. WRITE VARIABLES TO CLIPBOARD
C     UPDATED         --JULY      2016. SUPPORT FOR INCORPORATING A
C                                       HEADER FILE
C     UPDATED         --AUGUST    2016. TRAP ERRORS ON WRITE
C     UPDATED         --JUNE      2018. WRITE GROUP LABELS
C     UPDATED         --JUNE      2018. WRITE ROW LABELS, CHARACTER DATA
C     UPDATED         --SEPTEMBER 2018. WRITE ROW CASE
C     UPDATED         --DECEMBER  2018. WRITE1/WRITE2/WRITE3
C     UPDATED         --JULY      2019. TWEAK SCRATCH STORAGE
C     UPDATED         --OCTOBER   2019. ADD "HEAD" AND "TAIL" OPTIONS
C     UPDATED         --FEBRUARY  2020. SUPPORT "CSV" OPTION
C     UPDATED         --FEBRUARY  2020. WRITE EXCEL OPTION
C     UPDATED         --FEBRUARY  2020. FOR "WRITE CLIPBOARD", CHECK
C                                       IF CLIPBOARD ARGUMENT IS
C                                       ACTUALLY A FILE NAME.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IMACRO
      CHARACTER*12 IMACCS
C
      CHARACTER*4 IFORSW
      CHARACTER*4 IHTMFL
C
      CHARACTER*80 ICWRIF
      CHARACTER*40 IFORMT
      CHARACTER*80 IFMTTA
      CHARACTER*1  IQUOTE
      CHARACTER*1  IBASLC
C
      CHARACTER*4 IWRIRW
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*8 IH
      CHARACTER*4 JVNAM1
      CHARACTER*4 JPNAM1
      CHARACTER*4 JMNAM1
      CHARACTER*4 JFNAM1
      CHARACTER*4 JUNAM1
      CHARACTER*4 JENAM1
      CHARACTER*4 JGLNA1
      CHARACTER*4 JVNAM2
      CHARACTER*4 JPNAM2
      CHARACTER*4 JMNAM2
      CHARACTER*4 JFNAM2
      CHARACTER*4 JUNAM2
      CHARACTER*4 JENAM2
      CHARACTER*4 JGLNA2
      CHARACTER*4 JVTYPE
      CHARACTER*4 JVTYP2
C
      CHARACTER*4 JMNAM3
      CHARACTER*4 JMNAM4
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOFILE
      CHARACTER*4 IOTERM
C
CCCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
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
CCCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4  IREWI2
      CHARACTER*4  IERRF2
C
      CHARACTER*4 ICASWR
      CHARACTER*4 ICASW2
      CHARACTER*4 IHMAT1
      CHARACTER*4 IHMAT2
      CHARACTER*4 IFMFLG
      CHARACTER*4 IEXCEL
      CHARACTER*4 ICSVSV
      CHARACTER*4 IOP
      CHARACTER*4 ITYPEZ
      CHARACTER*80 ISNAME
      CHARACTER*80 ISARGL
C
      CHARACTER*2400 ISTRCC
C
C---------------------------------------------------------------------
C
      INTEGER IFORWI(*)
      INTEGER IFORWR(*)
C
CCCCC SET MAXIMUM NUMBER OF VARIABLES TO PRINT.  SET TO 1,024 SINCE
CCCCC IMAGES OFTEN HAVE 2**N COLUMNS (I.E., WE CAN PRINT THE PIXEL
CCCCC VALUES FOR A 1024x1024 IMAGE).
C
CCCCC 2018/09: FOR "WRITE ROW", INCREASE MAXIMUM NUMBER OF
CCCCC          VARIABLES
C
CCCCC PARAMETER (MAXV3=1024)
      PARAMETER (MAXV3=20000)
      PARAMETER (MAXGL=20)
C
      DIMENSION JVNAM1(MAXV3)
      DIMENSION JPNAM1(MAXV3)
      DIMENSION JMNAM1(MAXV3)
      DIMENSION JFNAM1(MAXV3)
      DIMENSION JUNAM1(MAXV3)
      DIMENSION JENAM1(MAXV3)
      DIMENSION JGLNA1(MAXGL)
C
      DIMENSION JVNAM2(MAXV3)
      DIMENSION JPNAM2(MAXV3)
      DIMENSION JMNAM2(MAXV3)
      DIMENSION JFNAM2(MAXV3)
      DIMENSION JUNAM2(MAXV3)
      DIMENSION JENAM2(MAXV3)
      DIMENSION JGLNA2(MAXGL)
      DIMENSION JVTYPE(MAXV3)
      DIMENSION JVTYP2(MAXV3)
      DIMENSION NCHEAD(MAXV3)
C
      DIMENSION JMNAM3(MAXV3)
      DIMENSION JMNAM4(MAXV3)
C
      DIMENSION NIV(MAXV3)
      DIMENSION NIM(MAXV3)
      DIMENSION IVCOL2(MAXV3)
      DIMENSION PVAL(MAXV3)
      DIMENSION IFSTA2(MAXV3)
      DIMENSION IFSTO2(MAXV3)
      DIMENSION IMVAL1(MAXV3)
      DIMENSION IMVAL2(MAXV3)
      DIMENSION ZLIST(MAXV3)
      DIMENSION IZLIST(MAXV3)
      CHARACTER*25 IZCLIS(MAXV3)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOZZ.INC'
      DIMENSION XSCRT(MAXOBW)
      EQUIVALENCE (GARBAG(IGARB1),XSCRT(1))
C
      INCLUDE 'DPCOST.INC'
C
      PARAMETER (MAXHED=1024)
      INTEGER IWID99(MAXHED)
      INTEGER IDIGIT(MAXHED)
      INTEGER IDIGI2(MAXHED)
      INTEGER NTOT(MAXHED)
      CHARACTER*8 ALIGN(MAXHED)
      CHARACTER*8 VALIGN(MAXHED)
      COMMON/HTML4/IWID99,IDIGI2,ALIGN,VALIGN
      CHARACTER*60 IVAL99(MAXHED)
      INTEGER      NCTEMP(MAXHED)
C
      CHARACTER*132 ITEMPC
      CHARACTER*24 IFRMT
C
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
C
      CHARACTER*40 IHTMFZ
      COMMON/HTMC1/IHTMFZ,NCFON1
C
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      LOGICAL IFLAG3
      LOGICAL IFLAGA
      LOGICAL IFLAGB
      LOGICAL IBOLD
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWR'
      ISUBN2='IT  '
      IFOUND='YES'
      IERROR='NO'
      ICASWR='OFF'
      ICASW2='0'
      IEXCEL='OFF'
      ICSVSV=ICSVWR
C
      IFLAGH=0
      IFLAGT=0
      NTEMP=0
      NROW=0
      ICOLVJ=0
      IPRSV2=IPR
      IPRSV=IPR
C
C     CHECK FOR WRITE1, WRITE2, WRITE3
C     CHECK FOR PRINT1, PRINT2, PRINT3
C     CHECK FOR HEAD, TAILS
C
      IF(ICOM.EQ.'WRIT')THEN
        IF(ICOM2.EQ.'E1  ')ICASW2='1'
        IF(ICOM2.EQ.'E2  ')ICASW2='2'
        IF(ICOM2.EQ.'E3  ')ICASW2='3'
      ELSEIF(ICOM.EQ.'PRIN')THEN
        IF(ICOM2.EQ.'T1  ')ICASW2='1'
        IF(ICOM2.EQ.'T2  ')ICASW2='2'
        IF(ICOM2.EQ.'T3  ')ICASW2='3'
      ELSEIF(ICOM.EQ.'HEAD')THEN
        IFLAGH=1
      ELSEIF(ICOM.EQ.'TAIL')THEN
        IFLAGT=1
      ENDIF
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      ICASWR='-999'
      IOFILE='-999'
      IOTERM='-999'
C
      IF(IFORSW.EQ.'E'.OR.IFORSW.EQ.'EXP'.OR.IFORSW.EQ.'EXPO')THEN
        NUMDIG=99
      ELSEIF(IFORSW.EQ.'0')THEN
        NUMDIG=0
      ELSEIF(IFORSW.EQ.'1')THEN
        NUMDIG=1
      ELSEIF(IFORSW.EQ.'2')THEN
        NUMDIG=2
      ELSEIF(IFORSW.EQ.'3')THEN
        NUMDIG=3
      ELSEIF(IFORSW.EQ.'4')THEN
        NUMDIG=4
      ELSEIF(IFORSW.EQ.'5')THEN
        NUMDIG=5
      ELSEIF(IFORSW.EQ.'6')THEN
        NUMDIG=6
      ELSEIF(IFORSW.EQ.'7')THEN
        NUMDIG=7
      ELSEIF(IFORSW.EQ.'8')THEN
        NUMDIG=8
      ELSEIF(IFORSW.EQ.'9')THEN
        NUMDIG=9
      ELSEIF(IFORSW.EQ.'10')THEN
        NUMDIG=10
      ELSEIF(IFORSW.EQ.'11')THEN
        NUMDIG=11
      ELSEIF(IFORSW.EQ.'12')THEN
        NUMDIG=12
      ELSE
        NUMDIG=-99
      ENDIF
      IFORMT=' '
      IFMTTA=' '
      IFMFLG='OFF'
      CALL DPCONA(39,IQUOTE)
      CALL DPCONA(92,IBASLC)
C
      HALF=0.5
C
      DO40I=1,MAXV3
        ZLIST(I)=0.0
        JVNAM1(I)='    '
        JPNAM1(I)='    '
        JMNAM1(I)='    '
        JFNAM1(I)='    '
        JUNAM1(I)='    '
        JENAM1(I)='    '
        JVNAM2(I)='    '
        JPNAM2(I)='    '
        JMNAM2(I)='    '
        JFNAM2(I)='    '
        JUNAM2(I)='    '
        JENAM2(I)='    '
        JVTYPE(I)='NUME'
   40 CONTINUE
C
      MAXV2=MAXV3
      MAXP2=MAXV3
      MAXM2=MAXV3
      MAXF2=MAXV3
      MAXU2=MAXV3
      MAXE2=MAXV3
C
C               ****************************
C               **  TREAT THE WRITE CASE  **
C               ****************************
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X,1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IBUGS2,IBUGQ,IBUGS2,ISUBRO,IERROR
   54   FORMAT('IBUGS2,IBUGQ,IBUGS2,ISUBRO,IERROR = ',5(A4,2X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IMACRO,IMACNU,IMACCS
   56   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IFORSW,IWRIRW,ICSVWR,IWIDTH
   57   FORMAT('IFORSW,IWRIRW,ICSVWR,IWIDTH = ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IHARG(1).EQ.'ROW ' .AND. IHARG2(1).EQ.'    ')THEN
        ICASWR='ON '
        IHTMFL='OFF'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IPRSV=IPR
      ELSEIF((IHARG(1).EQ.'HTM '.AND.IHARG2(1).EQ.'    ') .OR.
     1   (IHARG(1).EQ.'HTML'.AND.IHARG2(1).EQ.'    '))THEN
        IHTMFL='HTML'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ELSEIF(IHARG(1).EQ.'LATE'.AND.IHARG2(1).EQ.'X   ')THEN
        IHTMFL='LATE'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
      ELSEIF(IHARG(1).EQ.'RTF '.AND.IHARG2(1).EQ.'    ')THEN
        IHTMFL='RTF '
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
      ELSEIF(IHARG(1).EQ.'EXCE'.AND.IHARG2(1).EQ.'L   ')THEN
        IEXCEL='ON'
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGS2,IERROR)
      ELSE
        IHTMFL='OFF'
      ENDIF
C
C               *******************************************************
C               **  STEP 1.1--                                       **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='1.1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.LT.1)THEN
        IERROR='YES'
        GOTO8800
      ENDIF
C
C               ******************************************************
C               **  STEP 2--                                        **
C               **  CHECK FOR TERMINAL OR FILE INPUT AND OPEN       **
C               **  FILE FOR FILE CASE.                             **
C               ******************************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     2014/11: CHECK FOR "WRITE CLIPBOARD"
C
      IF(IHARG(1).EQ.'CLIP' .AND. IHARG2(1).EQ.'BOAR')THEN
C
C       CHECK IF ARGUMENT IS A FILE NAME STARTING WITH "CLIPBOARD.
C
        IWORD=2
        IOFILE='NO'
        CALL DPFILE(IANSLC,IWIDTH,IWORD,IOFILE,IBUGS2,ISUBRO,IERROR)
        IF(IOFILE.EQ.'NO')THEN
          ISHIFT=1
          CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1                IBUGS2,IERROR)
          IOTERM='CLIP'
          IOFILE='CLIP'
          DO61I=1,NUMARG
            IHARLC(I)=IHARLC(I+1)
            IHARL2(I)=IHARL2(I+1)
   61     CONTINUE
        ELSE
          CALL DPWRFI(IOTERM,IOFILE,IPR2,IOUNIT,IFMFLG,
     1                IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                ICWRIF,NCWRIF,IFWORD,IFQUOT,ICASWR,ICASW2,
     1                IWRIRW,IWRIHE,NCWRIH,IEXCEL,
     1                IBUGS2,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO 8800
        ENDIF
C
      ELSE
        CALL DPWRFI(IOTERM,IOFILE,IPR2,IOUNIT,IFMFLG,
     1              IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1              ICWRIF,NCWRIF,IFWORD,IFQUOT,ICASWR,ICASW2,
     1              IWRIRW,IWRIHE,NCWRIH,IEXCEL,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO 8800
      ENDIF
C
C     2018/06: CHECK TO SEE IF DPZCHF.DAT EXISTS
C
      IOUNI2=IZCHNU
      IFILE2=IZCHNA
      ISTAT2=IZCHST
      IFORM2=IZCHFO
      IACCE2=IZCHAC
      IPROT2=IZCHPR
      ICURS2=IZCHCS
      IFLAGC=1
C
      ISUBN0='WRIT'
      IERRF2='NO'
      IFLAGC=1
      CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
     1            ICURS2,
     1            IREWI2,ISUBN0,IERRF2,IBUGS2,ISUBRO,IERROR)
      IF(IERRF2.EQ.'YES')THEN
        IFLAGC=0
      ELSE
        REWIND(IOUNI2)
        READ(IOUNI2,'(I8)',END=871,ERR=871)NCVAR
        IFLAGC=1
        GOTO879
C
  871   CONTINUE
        IFLAGC=0
  879   CONTINUE
      ENDIF
C
C               *************************************************
C               **  STEP 3--                                   **
C               **  CALL DPWRI2 TO DO THE FOLLOWING:           **
C               **  1) WRITE STRING (IF REQUESTED)             **
C               **  2) PARSE SUBSET/FOR CLAUSES (IF ANY)       **
C               **  3) PARSE NAMES ON COMMAND LINE AND         **
C               **     DETERMINE TYPE OF EACH NAME             **
C               **  4) CHECK FOR A VALID NUMBER OF NAMES FOR   **
C               **     EACH TYPE                               **
C               **  5) PRINT FUNCTIONS AND PARAMETERS          **
C               *************************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPWRI2(IPR2,IOUNIT,IOUNI2,IFLAGC,
     1            IFORSW,NUMDIG,ICWRIF,NCWRIF,IWRIRW,IFORFM,
     1            IOFILE,
     1            MAXV2,MAXP2,MAXM2,MAXF2,MAXU2,MAXE2,MAXGL,
     1            JVNAM1,JPNAM1,JMNAM1,JFNAM1,JUNAM1,JENAM1,
     1            JVNAM2,JPNAM2,JMNAM2,JFNAM2,JUNAM2,JENAM2,
     1            JGLNA1,JGLNA2,JVTYPE,
     1            NIV,NIM,IVCOL2,PVAL,IFSTA2,IFSTO2,IMVAL1,IMVAL2,
     1            IZLIST,
     1            NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME,NUMGL,
     1            IV,IP,IM,IF,IU,IE,IGL,
     1            IDONE,IFLAGH,IFLAGT,
     1            ICASEQ,IFWORD,IFQUOT,
     1            IBUGS2,ISUBRO,IFOUND,IERROR)
C
      IF(IOTERM.EQ.'CLIP'.AND.NUMV.EQ.0)GOTO9000
      IF(IDONE.EQ.1)GOTO8800
      IF(IERROR.EQ.'YES')GOTO 8800
C
      IF(IEXCEL.EQ.'ON')THEN
        IOP='OPEN'
        IFLG11=1
        IFLG21=0
        IFLG31=0
        IFLAG4=0
        IFLAG5=1
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        WRITE(IOUNI5,'(A80)')IFILE
        WRITE(IOUNI5,'(A8)')IEXCSH
      ENDIF
C
C     DETERMINE IF ANY VARIABLE NAMES FROM DPZCHF.DAT
C     (DON'T INCLUCE ROWLABELS IN THIS LIST).
C
      IF(IFLAGC.EQ.1)THEN
        IFLAGC=0
        DO881KK=1,NUMV
          IF(JVTYPE(KK).EQ.'CHAR')THEN
            IFLAGC=1
            GOTO883
          ENDIF
  881   CONTINUE
  883   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 4.1--                                      **
C               **  FIRST, BRANCH TO THE APPROPRIATE SUBCASE        **
C               **  (DEPENDING ON WHETHER UNQUALIFIED, SUBSET OR    **
C               **  FOR); THE DETERMINE THE LENGTH OF THE LONGEST   **
C               **  VARIABLE TO BE PRINTED OUT; THEN PRINT OUT THE  **
C               **  VARIABLES THAT WERE SPECIFIED.                  **
C               ******************************************************
C
      ISTEPN='4.1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,6011)NUMV,NIV(1),NIV(2),NIV(3)
 6011   FORMAT('NUMV,NIV(1),NIV(2),NIV(3) = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6012)IVCOL2(1),IVCOL2(2),IVCOL2(3)
 6012   FORMAT('IVCOL2(1),IVCOL2(2),IVCOL2(3) = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6013)ICASEQ,ICASWR,IPR,IPR2,IFLAGH,IFLAGT
 6013   FORMAT('ICASEQ,ICASWR,IPR,IPR2,IFLAGH,IFLAGT = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6014)JVNAM1(1),JVNAM2(1)
 6014   FORMAT('JVNAM1(1),JVNAM2(1) = ',2A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     CASE 1: WRITE LIST OF VARIABLES.
C
C             SINCE WE ARE FUNNELING THROUGH DPTABx, DPHTMx, ETC.,
C             WE NEED TO SET "IPR" TO "IPR2" (AND BE SURE TO
C             CHANGE BACK).
C
      IPRSV=IPR
      IF(IOTERM.EQ.'CLIP')THEN
        IPR=IPRSV
      ELSE
        IPR=IPR2
      ENDIF
C
      IF(NUMV.LE.0)GOTO6490
C
      IVAL1=0
      MAXNPR=NIV(1)
      DO6100IV=1,NUMV
        IF(NIV(IV).GT.MAXNPR)MAXNPR=NIV(IV)
        IF(JVTYPE(IV).EQ.'NUME')IVAL1=1
 6100 CONTINUE
      IF(MAXNPR.EQ.0 .AND. IVAL1.EQ.0)MAXNPR=MAXOBV
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,6111)MAXNPR,NS,ILOCS,ICASEQ
 6111   FORMAT('MAXNPR,NS,ILOCS,ICASEQ = ',3I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASEQ.EQ.'SUBS')THEN
        NIOLD=MAXNPR
        CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
        NQ=NIOLD
      ELSEIF(IFLAGH.EQ.1)THEN
        DO6311II=1,IHEALI
          ISUB(II)=1
 6311   CONTINUE
        IF(IHEALI.LT.MAXNPR)THEN
          DO6312II=IHEALI+1,MAXNPR
            ISUB(II)=0
 6312     CONTINUE
        ENDIF
        NQ=IHEALI
      ELSEIF(IFLAGT.EQ.1)THEN
        DO6313II=1,MAXNPR
          ISUB(II)=0
 6313   CONTINUE
        NMAX=-1
        DO66411LL=1,NUMV
C
          ICOLVJ=IVALUE(LL)
          IF(JVTYPE(LL).EQ.'NUME')THEN
            NMAXT=IN(ICOLVJ)
            IF(NMAXT.GT.NMAX)NMAX=NMAXT
          ELSEIF(JVTYPE(LL).EQ.'ROWL')THEN
            NMAXT=NMAX
            DO66413JJ=MAXOBV,1,-1
              IF(IROWLB(JJ).NE.' ')THEN
                NMAXT=JJ
                GOTO66415
              ENDIF
66413       CONTINUE
66415       CONTINUE
            IF(NMAXT.GT.NMAX)NMAX=NMAXT
          ELSEIF(JVTYPE(LL).EQ.'CHAR')THEN
C
C           IGNORE CHARACTER VARIABLES FOR NOW.
C
          ENDIF
66411   CONTINUE
        NQ=MAXNPR
        IF(NMAX.GE.1)NQ=NMAX
        ILASTT=NQ
        IFRSTT=NQ-ITAILI+1
        IF(IFRSTT.LT.1)IFRSTT=1
        DO66420II=IFRSTT,ILASTT
          ISUB(II)=1
66420   CONTINUE
      ELSEIF(ICASEQ.EQ.'FOR')THEN
          NIOLD=MAXNPR
          CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,NLOCAL,ILOCS,NS,
     1               IBUGQ,IERROR)
          NQ=NFOR
      ELSE
        DO6315I=1,MAXNPR
          ISUB(I)=1
 6315   CONTINUE
        NQ=MAXNPR
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,6113)NIOLD,NS,NQ
 6113   FORMAT('NIOLD,NS,NQ = ',3I8)
        CALL DPWRST('XXX','BUG ')
        DO6115II=1,MIN(MAXNPR,1000)
          WRITE(ICOUT,6118)II,ISUB(II)
 6118     FORMAT('II,ISUB(II) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 6115   CONTINUE
      ENDIF
C
C     2018/09: SUPPORT "WRITE ROW" CASE.  THIS OPTION IS
C              PROVIDED PRIMARILY TO REWRITE DATA FILES THAT
C              ARE GIVEN IN "ROW ORDER" IN A FORM THAT CAN
C              UTILIZE "SET READ FORMAT" OPTION.  FOR THIS
C              REASON, THIS CAPABILITY IS LIMITED TO NUMERIC
C              DATA ONLY.
C
      IF(ICASWR.EQ.'ON' .AND. IEXCEL.EQ.'OFF')THEN
        ICNT2=0
        DO66400LL=1,NUMV
          IF(JVTYPE(LL).NE.'NUME')GOTO66400
C
          IH1=JVNAM1(LL)
          IH2=JVNAM2(LL)
          DO66403KK=1,NUMNAM
            IF(IH1.EQ.IHNAME(KK) .AND. IH2.EQ.IHNAM2(KK))THEN
              IF(IUSE(KK).EQ.'V')THEN
                ICOLVJ=IVALUE(KK)
                NROW=IN(KK)
                GOTO66409
              ELSE
                GOTO66400
              ENDIF
            ENDIF
66403     CONTINUE
66409     CONTINUE
C
          ICNT=0
          DO66410II=1,NROW
            IF(ISUB(II).EQ.0)GOTO66410
            ICNT=ICNT+1
            IJ=MAXN*(ICOLVJ-1)+II
            IF(ICOLVJ.LE.MAXCOL)ZLIST(ICNT)=V(IJ)
            IF(ICOLVJ.EQ.MAXCP1)ZLIST(ICNT)=PRED(II)
            IF(ICOLVJ.EQ.MAXCP2)ZLIST(ICNT)=RES(II)
            IF(ICOLVJ.EQ.MAXCP3)ZLIST(ICNT)=YPLOT(II)
            IF(ICOLVJ.EQ.MAXCP4)ZLIST(ICNT)=XPLOT(II)
            IF(ICOLVJ.EQ.MAXCP5)ZLIST(ICNT)=X2PLOT(II)
            IF(ICOLVJ.EQ.MAXCP6)ZLIST(ICNT)=TAGPLO(II)
66410     CONTINUE
C
          IF(NCWRIF.GE.1 .AND. IFMFLG.EQ.'OFF')THEN
            WRITE(IPR,ICWRIF,ERR=66491)(ZLIST(KK),KK=1,ICNT)
          ELSE
            IFORMT=' '
            IFORMT='(    E15.7)'
            WRITE(IFORMT(2:5),'(I4)')ICNT
            WRITE(IPR,IFORMT,ERR=66491)(ZLIST(KK),KK=1,ICNT)
          ENDIF
          ICNT2=ICNT2 + 1
          GOTO66400
C
66491     CONTINUE
          IPRSV2=IPR
          IPR=IPRSV
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,66493)
66493     FORMAT('***** ERROR IN   WRITE ROW')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,66495)JVNAM1(LL),JVNAM2(LL)
66495     FORMAT('      VARIABLE ',2A4,' NOT WRITTEN.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          IPR=IPRSV2
C
66400   CONTINUE
C
        IF(IFEEDB.EQ.'ON')THEN
          IPRSV2=IPR
          IPR=IPRSV
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,66498)ICNT2
66498     FORMAT(I6,' VARIABLES WRITTEN IN ROW FORMAT.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IPR=IPRSV2
        ENDIF
        GOTO8800
      ENDIF
C
C     STEP 1: PRINT TABLE HEADER.
C
C             DO NOT PRINT HEADER IF OUTPUT TO AN EXTERNAL FILE,
C             FORMATTED WRITE SPECIFIED, OR NUMBER OF VARIABLES > 7.
C
C             2020/02: FOR "WRITE EXCEL", PRINT TABLE HEADERS WITH
C                      SEPARATING COMMAS.
C
      IFLAG=1
      MAXTMP=MAXV3
      IF(IHTMFL.EQ.'HTML')MAXTMP=15
      IF(IHTMFL.EQ.'LATE')MAXTMP=7
      IF(IHTMFL.EQ.'RTF')MAXTMP=7
      IF(IOFILE.EQ.'YES' .OR. NCWRIF.GE.1 .OR.NUMV.GT.MAXTMP)IFLAG=0
      IF(IOFILE.EQ.'CLIP')IFLAG=0
      IF(IEXCEL.EQ.'ON')IFLAG=2
C
      ISTEPN='4.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1  CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPCONA(92,IBASLC)
C
      NHEAD=NUMV
      NMAX=1
C
      DO4000I=1,NUMV
        IF(IHTMFL.EQ.'HTML')THEN
          IWID99(I)=150
          IF(ITABWD.GT.0)THEN
            IWID99(I)=ITABWD
          ELSEIF(IHTMCW.GT.0)THEN
            IWID99(I)=IHTMCW
          ENDIF
          VALIGN(I)='BOTTOM'
          ALIGN(I)='RIGHT'
          IF(JVTYPE(I).NE.'NUME')ALIGN(I)='LEFT'
        ELSEIF(IHTMFL.EQ.'LATE')THEN
          IWID99(I)=0
          VALIGN(I)='b'
          ALIGN(I)='r'
          IF(JVTYPE(I).NE.'NUME')ALIGN(I)='l'
        ELSEIF(IHTMFL.EQ.'RTF')THEN
          IF(I.EQ.1)THEN
            IWID99(I)=1650
          ELSE
            IWID99(I)=IWID99(I-1) + 1650
          ENDIF
          VALIGN(I)='b'
          ALIGN(I)='r'
          IF(JVTYPE(I).NE.'NUME')ALIGN(I)='l'
        ELSE
          IWID99(I)=0
          VALIGN(I)='b'
          ALIGN(I)='r'
          IF(JVTYPE(I).NE.'NUME')ALIGN(I)='l'
        ENDIF
        IF(JVTYPE(I).EQ.'NUME')THEN
          IDIGIT(I)=-7
          NTOT(I)=15
          IINDX=MOD(I,MAXNWI)
          IF(IINDX.EQ.0)IINDX=200
          IF(IFORWI(IINDX).NE.-99)NTOT(I)=IFORWI(I)
          IF(IFORWR(IINDX).NE.-99)THEN
            IDIGIT(I)=IFORWR(I)
          ELSEIF(NUMDIG.NE.-99)THEN
            IDIGIT(I)=NUMDIG
            IF(IDIGIT(I).EQ.99)THEN
              IDIGIT(I)=-7
            ENDIF
          ENDIF
        ELSE
          IDIGIT(I)=0
          NTOT(I)=25
        ENDIF
 4000 CONTINUE
C
C     HEADER LINE.  PRINT VARIABLE NAME OR VARIABLE LABEL.  THE
C     TABLE TITLE, IF ANY, IS IN ITABTI.  ITABBR SPECIFIES THE
C     TYPE OF BORDER (FOR NOW, BASICALLY ONLY SUPPORT "RULE" OR
C     "NONE").
C
      IF(IFLAG.EQ.1)THEN
C
        ISTEPN='4.3'
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1    CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        JNUM=0
        JCHAR=0
C
        DO4020J=1,NUMV
          IVAL99(J)=' '
          NCTEMP(J)=1
          ICOLZZ=IVCOL2(J)
          IF(ITABHD.EQ.'ON')THEN
            IVAL99(J)=' '
            NTEMP=0
            IF(JVTYPE(J).EQ.'NUME')THEN
              IF(IVARLB(ICOLZZ).EQ.' ')THEN
                IVAL99(J)(1:4)=JVNAM1(J)
                IVAL99(J)(5:8)=JVNAM2(J)
                NTEMP=8
              ELSE
                IVAL99(J)=IVARLB(ICOLZZ)
                NTEMP=40
              ENDIF
            ELSE
              IVAL99(J)(2:5)=JVNAM1(J)
              IVAL99(J)(6:9)=JVNAM2(J)
              NTEMP=9
            ENDIF
          ENDIF
          IF(NTEMP.GT.0)THEN
            DO4025JJ=NTEMP,1,-1
              IF(IVAL99(J)(JJ:JJ).NE.' ')THEN
                NCTEMP(J)=JJ
                GOTO4029
              ENDIF
 4025       CONTINUE
 4029       CONTINUE
          ENDIF
 4020   CONTINUE
C
        ITEMPC=' '
        NCHEA2=0
        IF(IHTMFL.EQ.'HTML')THEN
          IFLAG1=.TRUE.
          IFLAG2=.TRUE.
          NCFON1=NCHTM1
          IHTMFZ=IHTMFT
          CALL DPHTM1(ITABTI,NCTABT,IFLAG1,IFLAG2)
          IF(ITABBR.EQ.'ON' .OR. ITABBR.EQ.'RULE')THEN
            IFLAG1=.TRUE.
            IFLAG2=.TRUE.
          ELSE
            IFLAG1=.FALSE.
            IFLAG2=.FALSE.
          ENDIF
          IF(ITABHD.EQ.'ON')THEN
            CALL DPHTM4(IVAL99,NCTEMP,NUMV,IFLAG1,IFLAG2)
          ENDIF
C
        ELSEIF(IHTMFL.EQ.'LATE')THEN
C
C         WRITE IN LATEX FORMAT:
C
C           1) DPLAT6  - END VERBATIM MODE
C           2) DPLAT1  - START NEW TABLE, PRINT TITLE
C           3) DPLAT4  - PRINT HEADER LINE
C
          IFLAG1=.FALSE.
          IFLAG2=.FALSE.
          IFLAG3=.TRUE.
          CALL DPLAT6(IFLAG1,IFLAG2,IFLAG3)
          IFLAG1=.TRUE.
          IFLAG2=.TRUE.
          CALL DPLAT1(ITABTI,NCTABT,ITEMPC,NCHEA2,IFLAG1)
          IF(ITABBR.EQ.'ON' .OR. ITABBR.EQ.'RULE')THEN
            IFLAG1=.TRUE.
            IFLAG2=.TRUE.
          ELSE
            IFLAG1=.FALSE.
            IFLAG2=.FALSE.
          ENDIF
          IFLAG3=.TRUE.
CCCCC     IF(ITABHD.EQ.'ON')THEN
            CALL DPLAT4(IVAL99,NCTEMP,NUMV,IFLAG1,IFLAG2,IFLAG3)
CCCCC     ENDIF
C
        ELSEIF(IHTMFL.EQ.'RTF')THEN
C
          IF(NUMV.GT.5)THEN
            IPTSZ=14
            WRITE(ICOUT,8199)IBASLC,IPTSZ
 8199       FORMAT(A1,'fs',I2)
            CALL DPWRST(ICOUT,'WRIT')
          ENDIF
C
 8091     FORMAT(A1,'f',I1)
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
          ELSE
            ITEMP=0
          ENDIF 
C
          IRTFMD='OFF'
C
          CALL DPRTF1(ITABTI,NCTABT,ITEMPC,NCHEA2)
C
          IF(ITABBR.EQ.'ON' .OR. ITABBR.EQ.'RULE')THEN
            IFLAG1=.TRUE.
            IFLAG2=.TRUE.
          ELSE
            IFLAG1=.FALSE.
            IFLAG2=.FALSE.
          ENDIF
CCCCC     IF(ITABHD.EQ.'ON')THEN
            CALL DPRTF4(IVAL99,NCTEMP,NUMV,IFLAG1,IFLAG2)
CCCCC     ENDIF
        ELSE
C
CCCCC     JULY 2009: GUI IS EXPECTING HEADER LINE IN FORMAT
CCCCC                VARIABLE NAMES--Y        X
CCCCC                FOLLOWED BY A SINGLE BLANK LINE.  SO IF
CCCCC                GUI IS BEING RUN (IGUIFL='ON'), THEN PRINT
CCCCC                HEADER LINE IN THIS FORMAT.
C
          IF(IGUIFL.EQ.'ON')THEN
            IFORMT='(1X, VARIABLES-- ,  (2A4,7X))'
            IFORMT(5:5)=IQUOTE
            IFORMT(17:17)=IQUOTE
            WRITE(IFORMT(19:20),'(I2)')NUMV
            WRITE(IPR,IFORMT)(JVNAM1(I),JVNAM2(I),I=1,NUMV)
            WRITE(IPR,999)
            IFORMT=' '
            GOTO4149
          ENDIF
C
          NMAX=0
          DO4141KK=1,NUMV
            NTOT(KK)=15
            IF(JVTYPE(KK).NE.'NUME')NTOT(KK)=25
            IF(IFORWI(KK).GT.0)NTOT(KK)=IFORWI(KK)
            NMAX=NMAX+NTOT(KK)
            IF(NCTEMP(KK).GT.NTOT(KK))NCTEMP(KK)=NTOT(KK)
            IF(ALIGN(KK).EQ.'l' .AND. NCTEMP(KK).LT.NTOT(KK))THEN
              DO4146JJ=NCTEMP(KK)+1,NTOT(KK)
                IVAL99(KK)(JJ:JJ)=' '
 4146         CONTINUE
              NCTEMP(KK)=NTOT(KK)
            ELSEIF(ALIGN(KK).EQ.'c'.AND.NCTEMP(KK).LT.NTOT(KK))THEN
              IVAL99(KK)(NCTEMP(KK):NTOT(KK))=' '
              IDIFF=(NTOT(KK)-NCTEMP(KK))/2
              IF(IDIFF.GT.0)THEN
                DO4147JJ=NTOT(KK),IDIFF+1,-1
                  IVAL99(KK)(JJ:JJ)=IVAL99(KK)(JJ-IDIFF:JJ-IDIFF)
 4147           CONTINUE
                IVAL99(KK)(1:IDIFF)=' '
              ENDIF
              NCTEMP(KK)=NTOT(KK)
            ELSEIF(ALIGN(KK).EQ.'r'.AND.NCTEMP(KK).LT.NTOT(KK))THEN
              IDIFF=NTOT(KK)-NCTEMP(KK)
              DO4148JJ=NTOT(KK),IDIFF+1,-1
                IVAL99(KK)(JJ:JJ)=IVAL99(KK)(JJ-IDIFF:JJ-IDIFF)
 4148         CONTINUE
              IVAL99(KK)(1:IDIFF)=' '
              NCTEMP(KK)=NTOT(KK)
            ENDIF
 4141     CONTINUE
C
          IFLAG1=.TRUE.
          CALL DPTAB1(ITABTI,NCTABT,ITEMPC,NCHEA2,IFLAG1)
          IF(ITABBR.EQ.'ON' .OR. ITABBR.EQ.'RULE')THEN
            IFLAG1=.TRUE.
            IFLAG2=.TRUE.
          ELSE
            IFLAG1=.FALSE.
            IFLAG2=.FALSE.
          ENDIF
          IF(ITABHD.EQ.'ON')THEN
            CALL DPTAB4(IVAL99,NCTEMP,NUMV,IFLAG1,IFLAG2,NMAX)
          ENDIF
          CALL DPFLSH(IPR,IBUGS2,ISUBRO,IFOUND,IERROR)
          IFOUND='YES'
C
 4149     CONTINUE
C
        ENDIF
C
      ELSEIF(IFLAG.EQ.2)THEN
C
C       FOR EXCEL, WRITE COMMA SEPARATED LABELS TO "dpst1f.dat"
C
        IFRMT=' '
        IFRMT='(    (2A4, ,  ),2A4)'
        WRITE(IFRMT(2:5),'(I4)')NUMV-1
        WRITE(IFRMT(11:11),'(A1)')"'"
        WRITE(IFRMT(14:14),'(A1)')"'"
        WRITE(IOUNI1,IFRMT)(JVNAM1(J),JVNAM2(J),J=1,NUMV)
      ENDIF
C
C     NOW PRINT OUT THE ROWS OF THE VARIABLES.  DO NOT GENERATE
C     HTML/LATEX/RTF FORMATTED OUTPUT IF WRITING TO A FILE,
C     IF USING A "SET WRITE FORMAT" OR IF THE NUMBER OF VARIABLES > 7.
C
      ISTEPN='4.4'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITEMPC=' '
      NCTEM2=0
      J=0
      ILINE=0
      MAXLTA=40
      NUMROW=MAXNPR
      DO6380I=MAXNPR,1,-1
        IF(ISUB(I).EQ.1)THEN
          NUMROW=I
          GOTO6389
        ENDIF
 6380 CONTINUE
 6389 CONTINUE
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,6393)MAXNPR,NUMROW
 6393   FORMAT('AT 6389: MAXNPR,NUMROW = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     POSITION dpzchf.dat TO START OF DATA ROWS
C
      IF(IFLAGC.EQ.1)THEN
        REWIND(IOUNI2)
        READ(IOUNI2,'(I8)',END=8800,ERR=8800)NCVAR
        DO6390I=1,NCVAR
          READ(IOUNI2,'(A8)',END=8800,ERR=8800)IH
 6390   CONTINUE
      ELSE
         NCVAR=0
      ENDIF
C
      DO6400I=1,NUMROW
C
        II=I
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
          WRITE(ICOUT,6401)II,ISUB(II),JNUM,JCHAR
 6401     FORMAT('II,ISUB(II),JNUM,JCHAR = ',4I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ISUB(II).EQ.1)THEN
          J=J+1
C
          IFLAG1=.FALSE.
          IF(II.EQ.NUMROW)IFLAG1=.TRUE.
C
C         EXTRACT THE DATA FOR A SINGLE ROW
C
          JNUM=0
          JCHAR=0
          IF(IFLAGC.EQ.1)THEN
            IFORMT=' '
            IFORMT='(    A)'
            WRITE(IFORMT(2:5),'(I4)')NCVAR
            READ(IOUNI2,IFORMT,END=8800,ERR=8800)ISTRCC
          ENDIF
C
          DO6410LL=1,NUMV
C
            IZCLIS(LL)=' '
            NCHEAD(LL)=0
            ZLIST(LL)=CPUMIN
            IF(JVTYPE(LL).EQ.'NUME')THEN
              JNUM=JNUM+1
            ELSE
              JCHAR=JCHAR+1
            ENDIF
C
            IF(JVTYPE(LL).EQ.'NUME')THEN
              IV=LL
              ICOLVJ=IVCOL2(IV)
              IJ=MAXN*(ICOLVJ-1)+II
              IF(ICOLVJ.LE.MAXCOL)ZLIST(LL)=V(IJ)
              IF(ICOLVJ.EQ.MAXCP1)ZLIST(LL)=PRED(II)
              IF(ICOLVJ.EQ.MAXCP2)ZLIST(LL)=RES(II)
              IF(ICOLVJ.EQ.MAXCP3)ZLIST(LL)=YPLOT(II)
              IF(ICOLVJ.EQ.MAXCP4)ZLIST(LL)=XPLOT(II)
              IF(ICOLVJ.EQ.MAXCP5)ZLIST(LL)=X2PLOT(II)
              IF(ICOLVJ.EQ.MAXCP6)ZLIST(LL)=TAGPLO(II)
              JVTYP2(LL)='NUME'
            ELSEIF(JVTYPE(LL).EQ.'ROWL')THEN
              IZCLIS(LL)(1:1)=' '
              IZCLIS(LL)(2:25)=IROWLB(II)(1:24)
              JVTYP2(LL)='ALPH'
              NCHEAD(LL)=25
              DO6608KK=25,1,-1
                IF(IZCLIS(LL)(KK:KK).NE.' ')THEN
                  NCHEAD(LL)=KK
                  GOTO6609
                ENDIF
 6608         CONTINUE
 6609         CONTINUE
            ELSEIF(JVTYPE(LL).EQ.'CHAR')THEN
              JVTYP2(LL)='ALPH'
              IZCLIS(LL)=' '
              NSTRT=(IVCOL2(LL)-1)*25 + 1
              NSTOP=NSTRT+23
              IZCLIS(LL)(2:25)=ISTRCC(NSTRT:NSTOP)
              NCHEAD(LL)=25
              DO6618KK=25,1,-1
                IF(IZCLIS(LL)(KK:KK).NE.' ')THEN
                  NCHEAD(LL)=KK
                  GOTO6619
                ENDIF
 6618         CONTINUE
 6619         CONTINUE
            ENDIF
 6410     CONTINUE
C
C         HANDLE CASE WHERE A WRITE FORMAT HAS BEEN SPECIFIED
C         (FOR THIS CASE, DO NOT USE THE HTML/LATEX/RTF FORMATTING
C         CODE)
C
          IF(IOTERM.EQ.'CLIP')THEN
            DO6411LL=1,NUMV
              IPTR=(II-1)*NUMV+LL
              XSCRT(IPTR)=ZLIST(LL)
 6411       CONTINUE
            GOTO6400
          ELSEIF(NCWRIF.GE.1)THEN
            IF(IFMFLG.EQ.'ON')THEN
              DO6412LL=1,NUMV
                IPTR=(II-1)*NUMV+LL
                XSCRT(IPTR)=ZLIST(LL)
 6412         CONTINUE
              GOTO6400
            ELSE
              WRITE(IPR2,ICWRIF,ERR=6491)(ZLIST(LL),LL=1,NUMV)
              GOTO6400
C
 6491         CONTINUE
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,6493)
 6493         FORMAT('***** ERROR TRYING TO WRITE DATA TO AN ',
     1               'EXTERNAL FILE')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,6495)
 6495         FORMAT('      WHEN USING THE SET WRITE FORMAT OPTION.')
              CALL DPWRST('XXX','BUG ')
              IERROR='YES'
              GOTO8800
C
            ENDIF
          ENDIF
C
          DO6420LL=1,NUMV
            IF(ZLIST(LL).EQ.CPUMIN)THEN
              IDIGI2(LL)=-99
            ELSE
              IDIGI2(LL)=IDIGIT(LL)
            ENDIF
 6420     CONTINUE
C
          IF(IHTMFL.EQ.'HTML')THEN
            IBOLD=.FALSE.
            IF(J.EQ.1)THEN
              DO6451LL=NUMV,1,-1
                ALIGN(LL+1)=ALIGN(LL)
                VALIGN(LL+1)=VALIGN(LL)
                IWID99(LL+1)=IWID99(LL)
 6451         CONTINUE
            ENDIF
            IF(JCHAR.LE.0)THEN
              CALL DPHTM5(ITEMPC,NCTEM2,ZLIST,NUMV,IBOLD)
            ELSE
              IFLAGA=.FALSE.
              IFLAGB=.FALSE.
              CALL DPHTMY(IZCLIS,NCHEAD,ZLIST,NUMV,JVTYP2,
     1                    IFLAGA,IFLAGB)
            ENDIF
C
C           FOR LATEX, WE CANNOT EXTEND TABLES BEYOND A SINGLE
C           PAGE, SO PUT A CHECK IN.
C
          ELSEIF(IHTMFL.EQ.'LATE')THEN
            IF(JCHAR.LE.0)THEN
              CALL DPLAT5(ITEMPC,NCTEM2,ZLIST,NUMV,IFLAG1)
            ELSE
              IFLAGA=.FALSE.
              IFLAGB=.FALSE.
              CALL DPLATW(IZCLIS,NCHEAD,ZLIST,NUMV,JVTYP2,
     1                    IFLAGA,IFLAGB)
            ENDIF
            ILINE=ILINE+1
            IF(ILINE.EQ.MAXLTA .AND. J.NE.NUMROW)THEN
              ILINE=0
              IFLAG1=.TRUE.
              IFLAG2=.FALSE.
              IFLAG3=.TRUE.
              CALL DPLATZ(IFLAG1,IFLAG2,IFLAG3,NHEAD)
              IFLAG1=.FALSE.
              IFLAG2=.FALSE.
              IFLAG3=.TRUE.
              CALL DPLATY(NHEAD)
            ENDIF
          ELSEIF(IHTMFL.EQ.'RTF')THEN
            IF(JCHAR.LE.0)THEN
              IFLAG1=.FALSE.
              CALL DPRTF5(ITEMPC,NCTEM2,ZLIST,NUMV,IFLAG1)
            ELSE
              CALL DPRTFY(IZCLIS,NCHEAD,ZLIST,NUMV,JVTYP2,
     1                    IFLAGA,IFLAGB)
            ENDIF
          ELSE
            IF(IEXCEL.EQ.'ON')THEN
              ICSVWR='ON'
            ELSE
              IOUNI1=-99
            ENDIF
            IF(JCHAR.LE.0)THEN
              IFLAG1=.FALSE.
              CALL DPTAB5(ITEMPC,NCTEM2,ZLIST,NUMV,IFLAG1,NMAX,NTOT,
     1                    ICSVWR)
            ELSE
              IFLAGA=.FALSE.
              IFLAGB=.FALSE.
              CALL DPTABY(IZCLIS,NCHEAD,ZLIST,NUMV,JVTYP2,
     1                    IFLAGA,IFLAGB,NMAX,NTOT,ICSVWR,IOUNI1,
     1                    IBUGS2,IERROR)
            ENDIF
          ENDIF
        ELSE
          IF(IFLAGC.EQ.1)THEN
            READ(IOUNI2,'(A1)',END=8800,ERR=8800)ISTRCC
          ENDIF
        ENDIF
 6400 CONTINUE
C
      IF(IEXCEL.EQ.'ON')THEN
        IOP='CLOS'
        CALL DPAUFI(IOP,IFLG11,IFLG21,IFLG31,IFLAG4,IFLAG5,
     1              IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1              IBUGS2,ISUBRO,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
      ENDIF
C
      IF(IOTERM.EQ.'CLIP')THEN
        NPTS=MAXNPR*NUMV
        CALL DPCLI5(XSCRT,NUMV,MAXNPR,NPTS,IBUGS2,ISUBRO,IERROR)
        GOTO9000
      ELSEIF(IFMFLG.EQ.'ON')THEN
        NPTS=MAXNPR*NUMV
        WRITE(IPR2,ERR=6591)(XSCRT(I),I=1,NPTS)
        GOTO6490
C
 6591   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6593)
 6593   FORMAT('***** ERROR TRYING TO WRITE DATA TO AN ',
     1         'EXTERNAL FILE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6595)
 6595   FORMAT('      WHEN USING THE SET WRITE FORMAT UNFORMATTED ',
     1         'OPTION.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO8800
C
      ENDIF
C
C     NOW TERMINATE THE TABLE
C
      IF(IHTMFL.EQ.'HTML')THEN
        IFLAG1=.TRUE.
        IFLAG2=.TRUE.
        CALL DPHTM2(IFLAG1,IFLAG2,NHEAD)
      ELSEIF(IHTMFL.EQ.'LATE')THEN
        IFLAG1=.TRUE.
        IFLAG2=.TRUE.
        IFLAG3=.TRUE.
        CALL DPLATZ(IFLAG1,IFLAG2,IFLAG3,NHEAD)
      ELSEIF(IHTMFL.EQ.'RTF')THEN
C
        IF(NUMV.GT.5)THEN
          IPTSZ=IRTFPS
          WRITE(ICOUT,8199)IBASLC,IPTSZ
          CALL DPWRST(ICOUT,'WRIT')
        ENDIF
C
        IF(IRTFFF.EQ.'Courier New')THEN
          ITEMP=1
        ELSEIF(IRTFFF.EQ.'Lucida Console')THEN
          ITEMP=8
        ENDIF 
        WRITE(ICOUT,8091)IBASLC,ITEMP
        CALL DPWRST(ICOUT,'WRIT')
        CALL DPRTF6(NHEAD)
        CALL DPRTF6(NHEAD)
        IRTFMD='VERB'
      ELSE
C
C       NOTE 2012/5: ONLY PRINT THE BLANK LINE IF WRITING TO THE SCREEN
C
        IF(IOFILE.EQ.'NO')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
        ENDIF
      ENDIF
C
 6490 CONTINUE
C
      IPR=IPRSV
C
C     2020/02: NOW USE "write_excel.py" SCRIPT TO CREATE THE EXCEL
C              FILE (NOTE THAT EXCEL DOES NOT NEED TO BE INSTALLED ON
C              YOUR LOCAL PLATFORM).  HOWEVER, PYTHON 3.x MUST ALREADY BE
C              INSTALLED AND THE pandas AND xlsxwriter PACKAGES MUST
C              ALSO BE INSTALLED.
C
      IF(IEXCEL.EQ.'ON')THEN
        ITYPEZ='PYTH'
        ISNAME='write_excel.py'
        IWIDZZ=14
        ISARGL=' '
        NCARG=0
        CALL DPEXR2(ITYPEZ,ISNAME,IWIDZZ,ISARGL,NCARG,
     1              IBUGS2,ISUBRO,IFOUND,IERROR)
        IFOUND='YES'
        IF(IERROR.EQ.'NO' .AND. IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6603)
 6603     FORMAT('VARIABLES WRITTEN TO THE EXCEL FILE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6605)IFILE
 6605     FORMAT(A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        GOTO9000
      ENDIF
C
C               ********************************************************
C               **  STEP 4.2--                                        **
C               **  PRINT OUT MATRICES.  FIRST, BRANCH TO THE         **
C               **  APPROPRIATE SUBCASE (DEPENDING ON WHETHER         **
C               **  UNQUALIFIED, SUBSET OR FOR);  THEN DETERMINE THE  **
C               **  LENGTH OF THE LONGEST VARIABLE TO BE PRINTED OUT; **
C               **  THEN PRINT OUT THE VARIABLES THAT WERE SPECIFIED. **
C               ********************************************************
C
      ISTEPN='4.2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,7011)NUMM,NIM(1),NIM(2),NIM(3)
 7011   FORMAT('NUMM,NIM(1),NIM(2),NIM(3) = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7012)IMVAL1(1),IMVAL1(2),IMVAL1(3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7012)IMVAL1(1),IMVAL2(1),IMVAL1(2),IMVAL2(2)
 7012   FORMAT('IMVAL1(1),IMVAL2(1),IMVAL1(2),IMVAL2(2) = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7013)ICASEQ,IPR,IPR2
 7013   FORMAT('ICASEQ,IPR,IPR2 = ',A4,2X,I8,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,7014)JMNAM1(1),JMNAM2(1)
 7014   FORMAT('JMNAM1(1),JMNAM2(1) = ',A4,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(NUMM.GE.1)THEN
        DO7100IM=1,NUMM
C
          NR1=NIM(IM)
          NC1=IMVAL2(IM)-IMVAL1(IM)+1
C
          IF(IOFILE.EQ.'NO' .AND. NCWRIF.LT.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7111)JMNAM1(IM),JMNAM2(IM),NR1
 7111       FORMAT('        MATRIX ',A4,A4,'--     ',I8,' ROWS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,7112)NC1
 7112       FORMAT('               ',4X,4X,'--     ',I8,' COLUMNS')
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          MAXNPR=NR1
          IF(ICASEQ.EQ.'SUBS')THEN
            NIOLD=MAXNPR
            CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERROR)
            NQ=NIOLD
          ELSEIF(ICASEQ.EQ.'FOR')THEN
            NIOLD=MAXNPR
            CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1                 NLOCAL,ILOCS,NS,IBUGQ,IERROR)
            NQ=NFOR
          ELSE
            DO7315I=1,MAXNPR
              ISUB(I)=1
 7315       CONTINUE
            NQ=MAXNPR
          ENDIF
C
          IHMAT1=JMNAM1(IM)
          IHMAT2=JMNAM2(IM)
          DO7351I=1,NC1
            CALL DPAPN2(IHMAT1,IHMAT2,I,
     1                  JMNAM3(I),JMNAM4(I),IBUGS2,ISUBRO,IERROR)
 7351     CONTINUE
C
          IF(IOFILE.EQ.'NO'.AND.NCWRIF.LT.1)THEN
            WRITE(IPR2,999)
            IF(NC1.LE.5)THEN
              WRITE(IPR2,7041)(JMNAM3(I),JMNAM4(I),I=1,NC1)
 7041         FORMAT(1X,'VARIABLES--',4(2A4,7X),2A4)
            ELSE
              WRITE(IPR2,7042)(JMNAM3(I),JMNAM4(I),I=1,NC1)
 7042         FORMAT(1X,'VARIABLES--',9(2A4,4X),2A4)
            ENDIF
            WRITE(IPR2,999)
          ENDIF
C
          J=0
          DO7500I=1,MAXNPR
C
            IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
              WRITE(ICOUT,7501)I,ISUB(I)
 7501         FORMAT('I,ISUB(I) = ',2I8)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            IF(ISUB(I).EQ.0)GOTO7500
            J=J+1
C
            DO7510LL=1,NC1
              JM=LL
              ICOLVJ=IMVAL1(IM)+JM-1
              IJ=MAXN*(ICOLVJ-1)+I
              IF(ICOLVJ.LE.MAXCOL)ZLIST(LL)=V(IJ)
              IF(ICOLVJ.EQ.MAXCP1)ZLIST(LL)=PRED(I)
              IF(ICOLVJ.EQ.MAXCP2)ZLIST(LL)=RES(I)
              IF(ICOLVJ.EQ.MAXCP3)ZLIST(LL)=YPLOT(I)
              IF(ICOLVJ.EQ.MAXCP4)ZLIST(LL)=XPLOT(I)
              IF(ICOLVJ.EQ.MAXCP5)ZLIST(LL)=X2PLOT(I)
              IF(ICOLVJ.EQ.MAXCP6)ZLIST(LL)=TAGPLO(I)
 7510       CONTINUE
            IF(NCWRIF.GE.1)THEN
              IF(IFMFLG.EQ.'ON')THEN
                DO7512LL=1,NC1
                  IPTR=(I-1)*NC1+LL
                  XSCRT(IPTR)=ZLIST(LL)
 7512           CONTINUE
              ELSE
                IF(ICWRIF(1:5).EQ.'(UNFO'.OR.ICWRIF(1:5).EQ.'(BINA')THEN
                  IF(NC1.LE.5)THEN
                    WRITE(IPR2,7569,IOSTAT=IOS,ERR=8700)
     1                    (ZLIST(LL),LL=1,NC1)
                  ELSE
                    WRITE(IPR2,7589,IOSTAT=IOS,ERR=8700)
     1                   (ZLIST(LL),LL=1,NC1)
                  ENDIF
                ELSE
                  WRITE(IPR2,ICWRIF,IOSTAT=IOS,ERR=8700)
     1                  (ZLIST(LL),LL=1,NC1)
                ENDIF
              ENDIF
            ELSEIF(IFORSW.EQ.'E'.OR.IFORSW(1:3).EQ.'EXP')THEN
              IF(NC1.LE.5)THEN
                WRITE(IPR2,7569,IOSTAT=IOS,ERR=8700)(ZLIST(LL),LL=1,NC1)
              ELSE
                WRITE(IPR2,7589,IOSTAT=IOS,ERR=8700)(ZLIST(LL),LL=1,NC1)
              ENDIF
            ELSEIF(IFORSW.EQ.'0')THEN
              IF(NC1.LE.5)THEN
                WRITE(IPR2,7550,IOSTAT=IOS,ERR=8700)
     1               (INT(ZLIST(LL)+SIGN(HALF,ZLIST(LL))),LL=1,NC1)
 7550           FORMAT(1X,I10,5X,I10,5X,I10,5X,I10,5X,I10,5X)
              ELSE
                WRITE(IPR2,7570,IOSTAT=IOS,ERR=8700)
     1               (INT(ZLIST(LL)+SIGN(HALF,ZLIST(LL))),LL=1,NC1)
 7570           FORMAT(1X,I10,2X,I10,2X,I10,2X,I10,2X,I10,2X,
     1                 I10,2X,I10,2X,I10,2X,I10,2X,I10,2X)
              ENDIF
            ELSEIF(NUMDIG.GE.1.AND.NUMDIG.LE.12)THEN
              IFORMT='(  F15.  )'
              WRITE(IFORMT(2:3),'(I2)')NC1
              WRITE(IFORMT(8:9),'(I2)')NUMDIG
              WRITE(IPR2,IFORMT,IOSTAT=IOS,ERR=8700)(ZLIST(LL),LL=1,NC1)
            ELSE
              IF(NC1.LE.5)THEN
                WRITE(IPR2,7569,IOSTAT=IOS,ERR=8700)(ZLIST(LL),LL=1,NC1)
 7569           FORMAT(1X,5E15.7)
              ELSE
                WRITE(IPR2,7589,IOSTAT=IOS,ERR=8700)(ZLIST(LL),LL=1,NC1)
 7589           FORMAT(1X,10E12.4)
              ENDIF
            ENDIF
C
 7500     CONTINUE
C
          IF(IFMFLG.EQ.'ON')THEN
            NPTS=MAXNPR*NC1
            WRITE(IPR2,IOSTAT=IOS,ERR=8700)(XSCRT(I),I=1,NPTS)
          ENDIF
C
 7100   CONTINUE
      ENDIF
C
C               **************************************************
C               **  STEP 8A--                                   **
C               **  PRINT OUT THE LIST OF UNDEFINED NAMES.      **
C               **************************************************
C
      ISTEPN='8A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMU.GE.1)THEN
        WRITE(IPR2,999,IOSTAT=IOS,ERR=8700)
        WRITE(IPR2,8111,IOSTAT=IOS,ERR=8700)
 8111   FORMAT(1X,'UNDEFINED NAMES--')
        WRITE(IPR2,999,IOSTAT=IOS,ERR=8700)
        DO8120I=1,NUMU
          WRITE(IPR2,8121,IOSTAT=IOS,ERR=8700)JUNAM1(I),JUNAM2(I)
 8121     FORMAT(1X,2A4)
 8120   CONTINUE
        WRITE(IPR2,999,IOSTAT=IOS,ERR=8700)
      ENDIF
      GOTO8800
C
C     2016/08: ERROR MESSAGE IF ERROR ON WRITE STATEMENT
C
 8700 CONTINUE
      WRITE(ICOUT,8701)IPR2
 8701 FORMAT('****** ERROR TRYING TO WRITE TO UNIT ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8702)IOS
 8702 FORMAT('       STATUS NUMBER = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8703)
 8703 FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',
     1       'THAT DOES NOT HAVE WRITE PERMISSION.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO8800
C
C
C               ***************************************
C               **  STEP 88--                        **
C               **  FOR THE FILE CASE,               **
C               **  CLOSE THE FILE.                  **
C               ***************************************
C
 8800 CONTINUE
      ISTEPN='88'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IOFILE.EQ.'YES'.AND.ICURST.EQ.'OPEN'.AND.
     1   ICASW2.EQ.'0')THEN
        IENDFI='ON'
        IREWIN='ON'
        IF(IWRIRW.EQ.'ON')THEN
          CALL DPCLFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1       IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
          IWRICS='CLOSED'
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
      ICSVWR=ICSVSV
      IENDFI='OFF'
      IREWIN='ON'
      CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1            IENDFI,IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
      IZCHCS='CLOSED'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWRIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR
 9015   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)IMACRO,IMACNU,IMACCS
 9016   FORMAT('IMACRO,IMACNU,IMACCS = ',A4,I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9017)IPR,IPR2,IOUNIT
 9017   FORMAT('IPR,IPR2,IOUNIT = ',3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)IOFILE,IOTERM,IFORSW
 9018   FORMAT('IOFILE,IOTERM,IFORSW = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE
 9022   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT,IFORM,IACCES,IPROT,ICURST
 9023   FORMAT('ISTAT,IFORM,IACES,IPROT,ICURST  = ',5(A12,1X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)IENDFI,IREWIN,ISUBN0,IERRFI
 9028   FORMAT('IENDFI,IREWIN,ISUBN0,IERRFI = ',4(A4,1X))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9061)NCWRIF
 9061   FORMAT('NCWRIF = ',I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCWRIF.GE.1)THEN
          WRITE(ICOUT,9062)(ICWRIF(I:I),I=1,NCWRIF)
 9062     FORMAT('(ICWRIF(I:I),I=1,NCWRIF) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,9071)IWRIRW
 9071   FORMAT('IWRIRW = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRI2(IPR2,IOUNIT,IOUNI2,IFLAGC,
     1                  IFORSW,NUMDIG,ICWRIF,NCWRIF,IWRIRW,IFORFM,
     1                  IOFILE,
     1                  MAXV2,MAXP2,MAXM2,MAXF2,MAXU2,MAXE2,MAXGL,
     1                  JVNAM1,JPNAM1,JMNAM1,JFNAM1,JUNAM1,JENAM1,
     1                  JVNAM2,JPNAM2,JMNAM2,JFNAM2,JUNAM2,JENAM2,
     1                  JGLNA1,JGLNA2,JVTYPE,
     1                  NIV,NIM,IVCOL2,PVAL,
     1                  IFSTA2,IFSTO2,IMVAL1,IMVAL2,
     1                  IZLIST,
     1                  NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME,NUMGL,
     1                  IV,IP,IM,IF,IU,IE,IGL,
     1                  IDONE,IFLAGH,IFLAGT,
     1                  ICASEQ,IFWORD,IFQUOT,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE-UTILITY ROUTINE FOR DPWRIT
C
C             1) PRINT LITERAL STRINGS
C             2) CHECK FOR <SUBSET/EXCEPT/FOR> CLAUSE
C             3) PRINT PARAMETERS
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2009/4
C     ORIGINAL VERSION--APRIL     2009. EXTRACT AS SEPARATE SUBROUTINE
C     UPDATED         --AUGUST    2016. TRAP ERRORS ON WRITE
C     UPDATED         --JUNE      2018. SUPPORT: PRINT GROUP LABELS
C     UPDATED         --JUNE      2018. SUPPORT FOR ROW LABELS AND
C                                       CHARACTER VARIABLES
C     UPDATED         --OCTOBER   2019. SUPPORT FOR "HEAD" AND "TAIL"
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFORSW
      CHARACTER*4 IFORFM
      CHARACTER*4 IWRIRW
C
      CHARACTER*80 ICWRIF
      CHARACTER*40 IFORMT
      CHARACTER*1  IQUOTE
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IFMFLG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IH1
      CHARACTER*4 IH2
      CHARACTER*4 IH3
      CHARACTER*4 IH4
      CHARACTER*8 IH
      CHARACTER*4 JVNAM1
      CHARACTER*4 JPNAM1
      CHARACTER*4 JMNAM1
      CHARACTER*4 JFNAM1
      CHARACTER*4 JUNAM1
      CHARACTER*4 JENAM1
      CHARACTER*4 JGLNA1
      CHARACTER*4 JVNAM2
      CHARACTER*4 JPNAM2
      CHARACTER*4 JMNAM2
      CHARACTER*4 JFNAM2
      CHARACTER*4 JUNAM2
      CHARACTER*4 JENAM2
      CHARACTER*4 JGLNA2
      CHARACTER*4 JVTYPE
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IOFILE
      CHARACTER*4 IC4
      CHARACTER*1 IC1
      CHARACTER*4 ICASWR
      CHARACTER*4 IANSI
      CHARACTER*4 IANSIR
      CHARACTER*4 ICASTO
C
      CHARACTER*4096 ISTR
C
CCCCC CHARACTER*80 IFILE
CCCCC CHARACTER*12 ISTAT
CCCCC CHARACTER*12 IFORM
CCCCC CHARACTER*12 IACCES
CCCCC CHARACTER*12 IPROT
CCCCC CHARACTER*12 ICURST
CCCCC CHARACTER*4 IENDFI
CCCCC CHARACTER*4 IREWIN
CCCCC CHARACTER*4 IERRFI
C
C---------------------------------------------------------------------
C
      DIMENSION JVNAM1(*)
      DIMENSION JPNAM1(*)
      DIMENSION JMNAM1(*)
      DIMENSION JFNAM1(*)
      DIMENSION JUNAM1(*)
      DIMENSION JENAM1(*)
      DIMENSION JGLNA1(*)
C
      DIMENSION JVNAM2(*)
      DIMENSION JPNAM2(*)
      DIMENSION JMNAM2(*)
      DIMENSION JFNAM2(*)
      DIMENSION JUNAM2(*)
      DIMENSION JENAM2(*)
      DIMENSION JGLNA2(*)
      DIMENSION JVTYPE(*)
C
      DIMENSION NIV(*)
      DIMENSION NIM(*)
      DIMENSION IVCOL2(*)
      DIMENSION PVAL(*)
      DIMENSION IFSTA2(*)
      DIMENSION IFSTO2(*)
      DIMENSION IMVAL1(*)
      DIMENSION IMVAL2(*)
      DIMENSION IZLIST(*)
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
      ISUBN1='DPWR'
      ISUBN2='I2  '
      IDONE=0
      IDONE=0
      HALF=0.5
      CALL DPCONA(39,IQUOTE)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X,1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IFLAGT,IOUNIT,IOFILE,IHARLC(1),IWRIRW
   53   FORMAT('IFLAGT,IOUNIT,IOFILE,IHARLC(1),IWRIRW = ',
     1         2I5,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C    
C               ********************************************************
C               **  STEP 1--                                          **
C               **  CHECK TO SEE IF OUTPUTTING A CHARACTER STRING.    ** 
C               **  FOR EXAMPLE, WRITE "CALIBRATION ANALYSIS"         **
C               **  IF SO, THEN TREAT THIS SPECIAL CASE IMMEDIATELY.  **
C               ********************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASWR='NOST'
      IC4=IHARLC(1)
      IF(IOFILE.EQ.'YES')IC4=IHARLC(2+IFWORD)
      IC1=IC4(1:1)
      IF(IC1.EQ.'''')ICASWR='STRI'
      IF(IC1.EQ.'"')ICASWR='STRI'
C
      IF(ICASWR.EQ.'STRI')THEN
C
        IF(IFMFLG.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,111)
  111     FORMAT('***** ERROR IN WRITE COMMAND--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,112)
  112     FORMAT('      WRITING A STRING TO AN UNFORMATTED FILE IS ',
     1           'NOT PERMITTED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,113)
  113     FORMAT('      ENTER THE COMMAND:  SET WRITE FORMAT WITH NO')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,114)
  114     FORMAT('      ARGUMENTS TO RESTORE THE WRITE FILE AS A ',
     1           'FORMATTED FILE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          IDONE=1
          GOTO9000
        ENDIF
C
        NQUOT=0
        ILOCQ1=0
        ILOCQ2=0
        DO130I=1,IWIDTH
          ILOCQ1=I
          IANSI=IANSLC(I)
          IF(IFQUOT.EQ.1)THEN
            IF(IANSI(1:1).EQ.'"' .OR. IANSI(1:1).EQ.IC1)NQUOT=NQUOT+1
            IF(IANSI(1:1).EQ.IC1 .AND. NQUOT.GE.3)GOTO139
          ELSE
            IF(IANSI(1:1).EQ.IC1)GOTO139
          ENDIF
  130   CONTINUE
  139   CONTINUE
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')THEN
          WRITE(ICOUT,131)IFQUOT,NQUOT,ILOCQ1
  131     FORMAT('AT 139: IFQUOT,NQUOT,ILOCQ1 = ',3I8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ILOCQ1.GT.0)THEN
          DO140I=1,IWIDTH
            IREV=IWIDTH-I+1
            ILOCQ2=IREV
            IANSIR=IANSLC(IREV)
            IF(IANSIR(1:1).EQ.IC1)GOTO149
  140     CONTINUE
  149     CONTINUE
C
          IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')THEN
            WRITE(ICOUT,141)ILOCQ2,IPR2
  141       FORMAT('AT 149: ILOCQ2,IPR2 = ',2I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          IF(ILOCQ2.GT.0)THEN
            ISTART=ILOCQ1+1
            ISTOP=ILOCQ2-1
            IF(ISTART.GT.ISTOP)THEN
              WRITE(IPR2,999,IOSTAT=IOS,ERR=8800)
            ELSEIF(ISTART.LE.ISTOP)THEN
              IF(IOFILE.EQ.'CLIP')THEN
                ICNT=0
                DO156II=ISTART,ISTOP
                  ICNT=ICNT+1
                  ISTR(ICNT:ICNT)=IANSLC(II)(1:1)
  156           CONTINUE
                IOP=1
                ICLOSE=1
                CALL DPCLI4(ISTR,ICNT,IOP,ICLOSE,IBUGS2,ISUBRO,IERROR)
              ELSE
                IF(IFORFM.EQ.'ON')THEN
                  WRITE(IPR2,151,IOSTAT=IOS,ERR=8800)
     1                  (IANSLC(I),I=ISTART,ISTOP)
  151             FORMAT(1X,240A1)
                ELSE
                  WRITE(IPR2,152,IOSTAT=IOS,ERR=8800)
     1                  (IANSLC(I),I=ISTART,ISTOP)
  152             FORMAT(240A1)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
        IDONE=1
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 2--                           **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET; OR                    **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='2'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
C
      IF(IFLAGH.EQ.1)THEN
        ICASEQ='FOR'
      ELSEIF(IFLAGH.EQ.1)THEN
        ICASEQ='FOR'
      ENDIF
C
      ILOCQ=NUMARG+1
      IF(NUMARG.GE.1)THEN
        DO200J=1,NUMARG
          J1=J
          IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ')THEN
            ICASEQ='SUBS'
            ILOCQ=J1
            GOTO290
          ELSEIF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ')THEN
            ICASEQ='SUBS'
            ILOCQ=J1
            GOTO290
          ELSEIF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ')THEN
            ICASEQ='FOR'
            ILOCQ=J1
            GOTO290
          ENDIF
  200   CONTINUE
  290   CONTINUE
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')THEN
        WRITE(ICOUT,291)NUMARG,ILOCQ
  291   FORMAT('NUMARG,ILOCQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 3--                                        **
C               **  DETERMINE THE TYPE AND NUMBER OF ITEMS          **
C               **  TO BE PRINTED.                                  **
C               **  NUMALL = TOTAL NUMBER OF PRINT ITEMS            **
C               **           (AS DETERMINED BY INCLUDING ONLY ALL   **
C               **           BEFORE SUBS' OR 'FOR')                 **
C               **  NUMV   = NUMBER OF VARIABLES TO BE PRINTED;     **
C               **  NUMP   = NUMBER OF PARAMETERS TO BE PRINTED;    **
C               **  NUMM   = NUMBER OF MODELS TO BE PRINTED         **
C               **           (SHOULD = 0 OR 1)                      **
C               **  NUMF   = NUMBER OF FUNCTIONS TO BE PRINTED      **
C               **  NUMU   = NUMBER OF UNKNOWNS TO BE PRINTED;      **
C               **  NUME   = TOTAL NUMBER OF PRINT ITEMS            **
C               **           (SHOULD = NUMALL);                     **
C               ******************************************************
C
      ISTEPN='3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMALL=ILOCQ-1
C
      IV=0
      IP=0
      IM=0
      IF=0
      IU=0
      IE=0
      IGL=0
      JMIN=1
      IF(IOFILE.EQ.'YES')JMIN=2+IFWORD
      JMAX=ILOCQ-1
C
C     2018/06: CHECK TO SEE IF DPZCHF.DAT EXISTS
C
      IF(JMIN.LE.JMAX)THEN
        IF((IHARG(JMIN).EQ.'VARI'.AND.IHARG(JMIN+1).EQ.'ALL').OR.
     1     (IHARG(JMIN).EQ.'ALL'.AND.IHARG(JMIN+1).EQ.'VARI'))THEN
          IF(NUMCOL.LE.0)THEN
            IDONE=1
            GOTO9000
          ENDIF
          NZLIST=0
          DO310I=1,NUMCOL
            DO311J=1,NUMNAM
              IF(I.EQ.IVALUE(J).AND.IUSE(J).EQ.'V')THEN
C
                IJUNK=IVALUE(J)
                IF(NZLIST.GE.1)THEN
                  DO312LL=1,NZLIST
                    IF(IJUNK.EQ.IZLIST(LL))GOTO310
  312             CONTINUE
                  NZLIST=NZLIST+1
                  IZLIST(NZLIST)=IJUNK
                ELSE
                  NZLIST=1
                  IZLIST(NZLIST)=IJUNK
                ENDIF
C
                IV=IV+1
                IF(IV.LE.MAXV2)THEN
                  JVNAM1(IV)=IHNAME(J)
                  JVNAM2(IV)=IHNAM2(J)
                  NIV(IV)=IN(J)
                  IVCOL2(IV)=IVALUE(J)
                ENDIF
                IE=IE+1
                IF(IE.LE.MAXE2)THEN
                  JENAM1(IE)=IHNAME(J)
                  JENAM2(IE)=IHNAM2(J)
                ENDIF
              ENDIF
  311       CONTINUE
  310     CONTINUE
          GOTO390
        ENDIF
C
        IISKIP=0
        DO320J=JMIN,JMAX
C
          IF(IISKIP.EQ.1)THEN
            IISKIP=0
            GOTO320
          ENDIF
C
          IH1=IHARG(J)
          IH2=IHARG2(J)
          IF(J.LT.JMAX)THEN
            IH3=IHARG(J+1)
            IH4=IHARG2(J+1)
          ELSE
            IH3=' '
            IH4=' '
          ENDIF
C
C         *****************************************************
C         ** THE FOLLOWING CODE ALLOWS THE    TO    KEYWORD  **
C         ** TO BE ACTIVATED, AS IN                          **
C         **        WRITE FILE.EXT Y1 TO Y10                 **
C         *****************************************************
C
C         JULY 2009: THE CASE "Y1 TO Y1" SHOULD JUST SKIP THE
C                    "TO Y1" PART.
          ICASTO='OFF'
          ISKIP=0
  325     CONTINUE
          IF (IH1.EQ.'TO  ' .OR. ICASTO.EQ.'ON')THEN
            IF(ISKIP.EQ.0)THEN
              ICASTO='ON'
              JM1=J-1
              JP1=J+1
              CALL DPEXTL(IHARG(JM1),IHARG2(JM1),IHARG(JP1),IHARG2(JP1),
     1                    KNUMB,IVAL1,IVAL2,IBUGS2,ISUBRO,IERROR)
C
              IF(IVAL1.EQ.IVAL2)THEN
                IISKIP=1
                GOTO320
              ENDIF
C
              IVA1P1=IVAL1+1
              IVA2M1=IVAL2-1
              IF(IVA1P1.GT.IVA2M1)GOTO320
              IVAL=IVAL1
            ENDIF
            IVAL=IVAL+1
            IF(IVAL.GE.IVAL2)GOTO320
C
            CALL DPAPNU(IHARG(JM1),IHARG2(JM1),KNUMB,IVAL,
     1                  IH1,IH2,IBUGS2,ISUBRO,IERROR)
          ENDIF
C
C         SUPPORT:  PRINT PARAMETERS
C
          IF((IH1.EQ.'PARA'.AND.IH2.EQ.'METE') .OR.
     1       (IH1.EQ.'SCAL'.AND.IH2.EQ.'ARS ') .OR.
     1       (IH1.EQ.'CONS'.AND.IH2.EQ.'TANT')) THEN
            DO335I=1,NUMNAM
              I2=I
              IF(IUSE(I).EQ.'P')THEN
                IH1=IHNAME(I2)
                IH2=IHNAM2(I2)
                IP=IP+1
                IF(IP.GT.MAXP2)GOTO335
                JPNAM1(IP)=IH1
                JPNAM2(IP)=IH2
                PVAL(IP)=VALUE(I2)
                IE=IE+1
                IF(IE.GT.MAXE2)GOTO335
                JENAM1(IE)=IH1
                JENAM2(IE)=IH2
              ENDIF
  335       CONTINUE
            GOTO320
C
C         SUPPORT:  PRINT GROUP LABELS
C
          ELSEIF(IH1.EQ.'GROU' .AND. IH2.EQ.'P   ' .AND.
     1           IH3.EQ.'LABE' .AND. IH4(1:1).EQ.'L')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','WRIT')
C
            ICNT=0
            DO790II=1,MAXGRP
              IF(IGRPVN(II).EQ.' ')GOTO790
              ICNT=ICNT+1
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,791)II,IGRPVN(II)
  791         FORMAT('GROUP LABEL ',I5,' NAME: ',A8)
              CALL DPWRST('XXX','WRIT')
              DO792JJ=1,MAXGLA
                IF(IGRPLA(JJ,II).EQ.' ')GOTO793
                WRITE(ICOUT,794)JJ,IGRPLA(JJ,II)
  794           FORMAT('     LEVEL ',I5,': ',A40)
                CALL DPWRST('XXX','WRIT')
  792         CONTINUE
  793         CONTINUE
  790       CONTINUE
C
            IF(ICNT.EQ.0)THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,799)
  799         FORMAT('NO GROUP LABELS CURRENTLY DEFINED.')
              CALL DPWRST('XXX','WRIT')
            ENDIF
            IISKIP=1
            GOTO320
C
          ENDIF
C
          DO340I=1,NUMNAM
            I2=I
            IF(IH1.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I))THEN
              IF(IUSE(I).EQ.'V')THEN
                IV=IV+1
                IF(IV.GT.MAXV2)GOTO348
                JVNAM1(IV)=IH1
                JVNAM2(IV)=IH2
                JVTYPE(IV)='NUME'
                NIV(IV)=IN(I2)
                IVCOL2(IV)=IVALUE(I2)
                GOTO348
              ELSEIF(IUSE(I).EQ.'P')THEN
                IP=IP+1
                IF(IP.GT.MAXP2)GOTO348
                JPNAM1(IP)=IH1
                JPNAM2(IP)=IH2
                PVAL(IP)=VALUE(I2)
                GOTO348
              ELSEIF(IUSE(I).EQ.'M')THEN
                IM=IM+1
                IF(IM.GT.MAXM2)GOTO348
                JMNAM1(IM)=IH1
                JMNAM2(IM)=IH2
                IMVAL1(IM)=IVALUE(I2)
                IMVAL2(IM)=IVALU2(I2)
                NIM(IM)=IN(I2)
                GOTO348
              ELSEIF(IUSE(I).EQ.'F')THEN
                IF=IF+1
                IF(IF.GT.MAXF2)GOTO348
                JFNAM1(IF)=IH1
                JFNAM2(IF)=IH2
                IFSTA2(IF)=IVSTAR(I2)
                IFSTO2(IF)=IVSTOP(I2)
                GOTO348
              ELSE
                IU=IU+1
                IF(IU.GT.MAXU2)GOTO348
                JUNAM1(IU)=IH1
                JUNAM2(IU)=IH2
                GOTO348
              ENDIF
            ELSEIF(IH1.EQ.'ROWL'.AND.IH2.EQ.'ABEL')THEN
              IV=IV+1
              IF(IV.GT.MAXV2)GOTO348
              JVNAM1(IV)=IH1
              JVNAM2(IV)=IH2
              JVTYPE(IV)='ROWL'
              NIV(IV)=0
              DO22310KK=1,MAXOBV
                IF(IROWLB(KK).EQ.' ')THEN
                  NIV(IV)=KK-1
                  GOTO22319
                ENDIF
22310         CONTINUE
              NIV(IV)=MAXOBV
22319         CONTINUE
              IVCOL2(IV)=-1
              GOTO348
            ELSE
C
C             2018/06: SEARCH FOR
C
C                      A) GROUP LABEL NAMES
C                      B) CHARACTER VARIABLE NAMES
C
              DO350KK=1,MAXGRP
                IF(IH1.EQ.IGRPVN(KK)(1:4) .AND.
     1             IH2.EQ.IGRPVN(KK)(5:8))THEN
                  IGL=IGL+1
                  IF(IGL.GT.MAXGL)GOTO348
                  JGLNA1(IGL)=IH1
                  JGLNA2(IGL)=IH2
                  GOTO348
                ENDIF
  350         CONTINUE
C
C             2018/06: IF NAME NOT FOUND IN GROUP LABELS, CHECK FOR
C                      CHARACTER VARIABLES
C
C             2018/07: IF CHARACTER VARIABLE FOUND, CHECK FOR A NUMERIC
C                      VARIABLE OF SAME NAME (IN WHICH CASE, IGNORE AND
C                      PRINT THE NUMERIC VARIABLE)
C
              IF(IFLAGC.EQ.1)THEN
                REWIND(IOUNI2)
                READ(IOUNI2,'(I8)',END=348,ERR=348)NCVAR
                REWIND(IOUNI2)
                READ(IOUNI2,'(I1)',END=348,ERR=348)IJUNK
                DO355KK=1,NCVAR
                  READ(IOUNI2,'(A8)',END=348,ERR=348)IH
                  IF(IH1.EQ.IH(1:4) .AND. IH2.EQ.IH(5:8))THEN
C
                    DO358LL=1,NUMNAM
                      IF(IH1.EQ.IHNAME(LL) .AND. IH2.EQ.IHNAM2(LL))THEN
                        GOTO355
                      ENDIF
  358               CONTINUE
C
                    IV=IV+1
                    IF(IV.GT.MAXV2)GOTO348
                    JVNAM1(IV)=IH1
                    JVNAM2(IV)=IH2
                    JVTYPE(IV)='CHAR'
                    NIV(IV)=0
                    IVCOL2(IV)=KK
                    GOTO348
                  ENDIF
  355           CONTINUE
              ENDIF
            ENDIF
  340     CONTINUE
C
          IU=IU+1
          IF(IU.GT.MAXU2)GOTO348
          JUNAM1(IU)=IH1
          JUNAM2(IU)=IH2
C
  348     CONTINUE
          IE=IE+1
          IF(IE.GT.MAXE2)GOTO320
          JENAM1(IE)=IH1
          JENAM2(IE)=IH2
C
          IF(ICASTO.EQ.'ON')THEN
            ISKIP=1
            GOTO325
          ENDIF
C
  320   CONTINUE
C
      ENDIF
C
  390 CONTINUE
      NUMV=IV
      NUMP=IP
      NUMM=IM
      NUMF=IF
      NUMU=IU
      NUME=IE
      NUMGL=IGL
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')THEN
        WRITE(ICOUT,391)NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME
  391   FORMAT('NUMALL,NUMV,NUMP,NUMM,NUMF,NUMU,NUME = ',7I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,392)
  392   FORMAT('I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),',
     1         'JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),',
     1         'JUNAM1(I),JUNAM2(I)')
        CALL DPWRST('XXX','BUG ')
        DO395I=1,15
          WRITE(ICOUT,396)I,JVNAM1(I),JVNAM2(I),JPNAM1(I),JPNAM2(I),
     1                     JMNAM1(I),JMNAM2(I),JFNAM1(I),JFNAM2(I),
     1                     JUNAM1(I),JUNAM2(I)
  396     FORMAT(I8,5X,2A4,1X,2A4,1X,2A4,1X,2A4,1X,2A4)
          CALL DPWRST('XXX','BUG ')
  395   CONTINUE
      ENDIF
C
C               ***************************************************
C               **  STEP 4--                                     **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 500) OF VARIABLES TO BE PRINTED        **
C               **  (NOTE--THIS DOES NOT INCLUDE PARAMETERS      **
C               **  OR MODELS IN THE ABOVE COUNT--               **
C               **  ONLY VARIABLES.)                             **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 500) OF CONSTANTS TO BE PRINTED.       **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 500) OF MODELS TO BE PRINTED.          **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 500) OF FUNCTIONS TO BE PRINTED.       **
C               **  CHECK FOR A VALID NUMBER                     **
C               **  (0 TO 500) OF UNKNOWNS TO BE PRINTED.        **
C               ***************************************************
C
      IF(NUMV.LT.0 .OR. NUMV.GT.MAXV2)THEN
        WRITE(ICOUT,411)
  411   FORMAT('***** ERROR IN WRITE COMMAND--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,412)
  412   FORMAT('      FOR A WRITE, THE NUMBER OF VARIABLES (NOT ',
     1         'COUNTING PARAMETERS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,413)MAXV2
  413   FORMAT('      OR MODELS) MUST BE AT MOST ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,414)
  414   FORMAT('      SUCH WAS NOT THE CASE HERE;  THE SPECIFIED')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,415)NUMV
  415   FORMAT('      NUMBER OF VARIABLES TO BE PRINTED WAS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,416)MAXV2
  416   FORMAT('      NOTE--ONLY THE FIRST ',I8,' VARIABLES WILL BE ',
     1         'PRINTED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,417)
  417   FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,418)(IANSLC(I),I=1,MIN(80,IWIDTH))
  418     FORMAT(80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMP.LT.0 .OR. NUMP.GT.MAXP2)THEN
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,422)
  422   FORMAT('      FOR A WRITE, THE NUMBER OF PARAMETERS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,423)MAXP2
  423   FORMAT('      (CONSTANTS) MUST BE AT MOST ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,424)
  424   FORMAT('      SUCH WAS NOT THE CASE HERE;  THE SPECIFIED ',
     1         'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,425)NUMP
  425   FORMAT('      OF PARAMETERS TO BE PRINTED WAS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,426)MAXP2
  426   FORMAT('      NOTE--ONLY THE FIRST ',I8,' PARAMETERS WILL ',
     1         'BE PRINTED.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,418)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMM.LT.0 .OR. NUMM.GT.MAXM2)THEN
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,432)
  432   FORMAT('      FOR A WRITE, THE NUMBER OF MODELS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,433)MAXM2
  433   FORMAT('      MOST ',I8,'.  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,434)NUMM
  434   FORMAT('      THE SPECIFIED NUMBER OF MODELS TO BE PRINTED ',
     1         'WAS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,435)MAXM2
  435   FORMAT('      NOTE--ONLY THE FIRST ',I8,' MODELS WILL BE ',
     1         'PRINTED.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,418)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMF.LT.0 .OR. NUMF.GT.MAXF2)THEN
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,442)
  442   FORMAT('      FOR A PRINT, THE NUMBER OF FUNCTIONS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,443)MAXF2
  443   FORMAT('      MOST ',I8,'.  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,444)NUMF
  444   FORMAT('      THE SPECIFIED NUMBER OF FUNCTIONS TO BE ',
     1         'PRINTED WAS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,445)MAXF2
  445   FORMAT('      NOTE--ONLY THE FIRST ',I8,' FUNCTIONS WILL BE ',
     1         'PRINTED.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,418)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMU.LT.0 .OR. NUMU.GT.MAXU2)THEN
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,452)
  452   FORMAT('      FOR A PRINT, THE NUMBER OF UNKNOWNS MUST BE AT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,453)MAXU2
  453   FORMAT('      MOST ',I8,'.  SUCH WAS NOT THE CASE HERE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,454)NUMU
  454   FORMAT('      THE SPECIFIED NUMBER OF UNKNOWNS TO BE PRINTED ',
     1         'WAS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,455)MAXU2
  455   FORMAT('      NOTE--ONLY THE FIRST ',I8,' UNKNOWNS WILL BE ',
     1         'PRINTED.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,418)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(NUMGL.LT.0 .OR. NUMGL.GT.MAXGL)THEN
        WRITE(ICOUT,411)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,462)
  462   FORMAT('      FOR A WRITE, THE NUMBER OF GROUP LABELS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,463)MAXGL
  463   FORMAT('      MUST BE AT MOST ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,464)
  464   FORMAT('      SUCH WAS NOT THE CASE HERE;  THE SPECIFIED ',
     1         'NUMBER')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,465)NUMGL
  465   FORMAT('      OF GROUP LABELS TO BE PRINTED WAS ',I8,'.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,466)MAXGL
  466   FORMAT('      NOTE--ONLY THE FIRST ',I8,' GROUP LABELS WILL ',
     1         'BE PRINTED.')
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,418)(IANSLC(I),I=1,MIN(80,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
C               ******************************************
C               **  STEP 5A--                           **
C               **  PRINT OUT FUNCTIONS IF CALLED FOR.  **
C               ******************************************
C
      ISTEPN='5A'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ISTR=' '
      ICNT=0
      MAXCNT=2048
C
      IF(NUMF.GT.0)THEN
        IF(IOFILE.EQ.'NO' .AND. NCWRIF.LT.1)THEN
          WRITE(IPR2,999,IOSTAT=IOS,ERR=8800)
          WRITE(IPR2,501,IOSTAT=IOS,ERR=8800)
  501     FORMAT(1X,'FUNCTIONS--')
          WRITE(IPR2,999,IOSTAT=IOS,ERR=8800)
        ENDIF
        IF(IFMFLG.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,411)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,512)
  512     FORMAT('      IT IS ILLEGAL TO WRITE A FUNCTION TO AN ',
     1           'UNFORMATTED FILE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,514)
  514     FORMAT('      ENTER THE COMMAND:  SET WRITE FORMAT WITH NO')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,516)
  516     FORMAT('      ARGUMENTS TO RESTORE THE WRITE FILE AS A ',
     1           'FORMATTED FILE.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          IDONE=1
          GOTO9000
        ENDIF
C
        DO521I=1,NUMF
          JMIN=IFSTA2(I)
          JMAX=IFSTO2(I)
          IF(JMAX-JMIN.GT.115)JMAX=JMIN+115
          IF(IOFILE.EQ.'CLIP')THEN
            DO522II=JMIN,JMAX
              IF(ICNT.LT.MAXCNT)THEN
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=IFUNC(II)(1:1)
              ENDIF
  522       CONTINUE
            IF(I.LT.NUMF)THEN
              IF(ICNT.LT.MAXCNT-2)THEN
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=CHAR(13)
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=CHAR(10)
              ENDIF
            ELSE
              IF(ICNT.LT.MAXCNT-2 .AND. NUMP.GE.1)THEN
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=CHAR(13)
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=CHAR(10)
              ENDIF
            ENDIF
          ELSE
C
C           2015/08: IGNORE "SET WRITE FORMAT" FOR STRINGS
C
            IF(NCWRIF.LE.0)THEN
              WRITE(IPR2,523,IOSTAT=IOS,ERR=8800)JFNAM1(I),JFNAM2(I),
     1              (IFUNC(J),J=JMIN,JMAX)
  523         FORMAT(1X,4X,2A4,'--',115A1)
            ELSE
CCCCC         WRITE(IPR2,ICWRIF)(IFUNC(J),J=JMIN,JMAX)
              WRITE(IPR2,523,IOSTAT=IOS,ERR=8800)JFNAM1(I),JFNAM2(I),
     1              (IFUNC(J),J=JMIN,JMAX)
            ENDIF
          ENDIF
  521   CONTINUE
      ENDIF
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  PRINT OUT THE PARAMETERS AND CONSTANTS THAT     **
C               **  WERE SPECIFIED.                                 **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMP.GE.1)THEN
C
        IF(NCWRIF.GE.1 .AND. IOFILE.NE.'CLIP')THEN
          IF(IFMFLG.EQ.'ON')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,411)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,601)
  601       FORMAT('      IT IS ILLEGAL TO WRITE A PARAMETER TO AN ',
     1             'UNFORMATTED FILE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,603)
  603       FORMAT('      ENTER THE COMMAND:  SET WRITE FORMAT ')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,604)
  604       FORMAT('      WITH NO ARGUMENTS TO RESTORE THE WRITE FILE ',
     1             'AS A FORMATTED FILE.')
            CALL DPWRST('XXX','BUG ')
            IDONE=1
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(NUMP.GE.1.AND.NUMP.LE.20)THEN
            WRITE(IPR2,ICWRIF,IOSTAT=IOS,ERR=8800) (PVAL(LL),LL=1,NUMP)
          ENDIF
        ELSE
C
          IF(IOFILE.EQ.'NO')THEN
            WRITE(IPR2,999,IOSTAT=IOS,ERR=8800)
            WRITE(IPR2,611,IOSTAT=IOS,ERR=8800)
  611       FORMAT(1X,'PARAMETERS AND CONSTANTS--')
            WRITE(IPR2,999,IOSTAT=IOS,ERR=8800)
          ENDIF
C
          DO620I=1,NUMP
            Z1=PVAL(I)
            IF(IOFILE.EQ.'CLIP' .AND. ICNT.LT.MAXCNT-17)THEN
              ICNT1=ICNT+1
              ICNT2=ICNT+15
              WRITE(ISTR(ICNT1:ICNT2),'(E15.7)')Z1
              ICNT=ICNT+15
              IF(I.NE.NUMP)THEN
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=CHAR(13)
                ICNT=ICNT+1
                ISTR(ICNT:ICNT)=CHAR(10)
              ENDIF
            ELSEIF(IFORSW.EQ.'E'.OR.IFORSW.EQ.'EXP'.OR.
     1             IFORSW.EQ.'EXPO')THEN
              WRITE(IPR2,621,IOSTAT=IOS,ERR=8800)JPNAM1(I),JPNAM2(I),Z1
  621         FORMAT(1X,4X,2A4,'--',E15.7)
            ELSEIF(IFORSW.EQ.'0')THEN
              IZ1=INT(Z1+SIGN(HALF,Z1) + 0.1)
              WRITE(IPR2,622,IOSTAT=IOS,ERR=8800)JPNAM1(I),JPNAM2(I),IZ1
  622         FORMAT(1X,4X,2A4,'--',I10)
            ELSEIF(NUMDIG.GE.1.AND.NUMDIG.LE.12)THEN
              IFORMT='(4X,2A4, -- ,F15.  )'
              IFORMT(9:9)=IQUOTE
              IFORMT(12:12)=IQUOTE
              WRITE(IFORMT(18:19),'(I2)')NUMDIG
              WRITE(IPR2,IFORMT,IOSTAT=IOS,ERR=8800)JPNAM1(I),
     1              JPNAM2(I),Z1
            ELSE
              WRITE(IPR2,621,IOSTAT=IOS,ERR=8800)JPNAM1(I),JPNAM2(I),Z1
            ENDIF
  620     CONTINUE
C
        ENDIF
C
      ENDIF
C
C               ******************************************************
C               **  STEP 6.3--                                      **
C               **  PRINT OUT THE GROUP LABELS THAT WERE SPECIFIED. **
C               ******************************************************
C
      ISTEPN='6.3'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMGL.GE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
C
        IF(IFMFLG.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,411)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,631)
  631     FORMAT('      IT IS ILLEGAL TO WRITE GROUP LABELS TO AN ',
     1           'UNFORMATTED FILE.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,603)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,604)
          CALL DPWRST('XXX','BUG ')
          IDONE=1
          IERROR='YES'
          GOTO9000
        ENDIF
C
        WRITE(ICOUT,632)
  632   FORMAT('GROUP LABELS--')
        CALL DPWRST('XXX','BUG ')
C
        DO633KK=1,NUMGL
          IH1=JGLNA1(KK)
          IH2=JGLNA2(KK)
          DO634II=1,MAXGRP
            IF(IH1.EQ.IGRPVN(II)(1:4) .AND. IH2.EQ.IGRPVN(II)(5:8))THEN
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','WRIT')
              WRITE(ICOUT,638)II,IGRPVN(II)
  638         FORMAT('GROUP LABEL ',I5,' NAME: ',A8)
              CALL DPWRST('XXX','WRIT')
              DO635JJ=1,MAXGLA
                IF(IGRPLA(JJ,II).EQ.' ')GOTO637
                WRITE(ICOUT,636)JJ,IGRPLA(JJ,II)
  636           FORMAT('     LEVEL ',I5,': ',A40)
                CALL DPWRST('XXX','WRIT')
  635         CONTINUE
  637         CONTINUE
            ENDIF
  634     CONTINUE
  633   CONTINUE
C
      ENDIF
C
C
C     SEND STRINGS AND PARAMETERS TO CLIPBOARD
C
      IF(IOFILE.EQ.'CLIP' .AND. ICNT.GT.0)THEN
        IOP=1
        ICLOSE=1
        CALL DPCLI4(ISTR,ICNT,IOP,ICLOSE,IBUGS2,ISUBRO,IERROR)
      ENDIF
C
C     2016/08: ERROR MESSAGE IF ERROR ON WRITE STATEMENT
C
      GOTO9000
 8800 CONTINUE
      WRITE(ICOUT,8801)IPR2
 8801 FORMAT('****** ERROR TRYING TO WRITE TO UNIT ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8802)IOS
 8802 FORMAT('       STATUS NUMBER = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8803)
 8803 FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',
     1       'THAT DOES NOT HAVE WRITE PERMISSION.')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'WRI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWRI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR,IDONE
 9015   FORMAT('IFOUND,IERROR,IDONE = ',A4,2X,A4,2X,I4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRLA(PXMIN,PYMIN,PXMAX,PYMAX,
     1ITITTE,NCTITL,ITITCV,PTITRV,
     1IX1LTE,NCX1LA,IX1LCV,PX1LRV,
     1IX2LTE,NCX2LA,IX2LCV,PX2LRV,
     1IX3LTE,NCX3LA,IX3LCV,PX3LRV,
     1IY1LTE,NCY1LA,IY1LCV,PY1LRV,
     1IY2LTE,NCY2LA,IY2LCV,PY2LRV,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,ISEQSW,NUMSEQ)
C
C     PURPOSE--WRITE OUT LABELS, AND THE TITLE
C              ON A PLOT.
C
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     UPDATED         --FEBRUARY   1989. VERTICAL JUST. OF Y...LABEL (ALAN)
C     UPDATED         --FEBRUARY   1989. VERTICAL SIZE OF Y...LABEL (ALAN)
C     UPDATED         --OCTOBER    1999. JUSTIFICATION AND OFFSET FOR LABELS
C     UPDATED         --NOVEMBER   1999. DIRECTION AND ANGLE FOR LABELS
C     UPDATED         --OCTOBER    2018. LABEL COORDINATES
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 ITITTE
      CHARACTER*4 ITITFO
      CHARACTER*4 ITITCA
      CHARACTER*4 ITITFI
      CHARACTER*4 ITITCO
C
      CHARACTER*4 IX1LTE
      CHARACTER*4 IX1LFO
      CHARACTER*4 IX1LCA
      CHARACTER*4 IX1LFI
      CHARACTER*4 IX1LCO
      CHARACTER*4 IX1LJU
      CHARACTER*4 IX1LDI
C
      CHARACTER*4 IX2LTE
      CHARACTER*4 IX2LFO
      CHARACTER*4 IX2LCA
      CHARACTER*4 IX2LFI
      CHARACTER*4 IX2LCO
      CHARACTER*4 IX2LJU
      CHARACTER*4 IX2LDI
C
      CHARACTER*4 IX3LTE
      CHARACTER*4 IX3LFO
      CHARACTER*4 IX3LCA
      CHARACTER*4 IX3LFI
      CHARACTER*4 IX3LCO
      CHARACTER*4 IX3LJU
      CHARACTER*4 IX3LDI
C
      CHARACTER*4 IY1LTE
      CHARACTER*4 IY1LFO
      CHARACTER*4 IY1LCA
      CHARACTER*4 IY1LFI
      CHARACTER*4 IY1LCO
      CHARACTER*4 IY1LJU
      CHARACTER*4 IY1LDI
C
      CHARACTER*4 IY2LTE
      CHARACTER*4 IY2LFO
      CHARACTER*4 IY2LCA
      CHARACTER*4 IY2LFI
      CHARACTER*4 IY2LCO
      CHARACTER*4 IY2LJU
      CHARACTER*4 IY2LDI
C
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILL
      CHARACTER*4 ICOL
C
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
      CHARACTER*4 IMPSW2
C
      CHARACTER*4 ICTEXT
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IFUNC
C
      CHARACTER*1 IREPCH
C
      CHARACTER*4 ISEQSW
C
      CHARACTER*4 ITITCV
      CHARACTER*4 IX1LCV
      CHARACTER*4 IX2LCV
      CHARACTER*4 IX3LCV
      CHARACTER*4 IY1LCV
      CHARACTER*4 IY2LCV
C
      DIMENSION ITITTE(*)
      DIMENSION IX1LTE(*)
      DIMENSION IX2LTE(*)
      DIMENSION IX3LTE(*)
      DIMENSION IY1LTE(*)
      DIMENSION IY2LTE(*)
C
      DIMENSION ICTEXT(130)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
      DIMENSION ITITCV(*)
      DIMENSION PTITRV(*)
      DIMENSION IX1LCV(*)
      DIMENSION PX1LRV(*)
      DIMENSION IX2LCV(*)
      DIMENSION PX2LRV(*)
      DIMENSION IX3LCV(*)
      DIMENSION PX3LRV(*)
      DIMENSION IY1LCV(*)
      DIMENSION PY1LRV(*)
      DIMENSION IY2LCV(*)
      DIMENSION PY2LRV(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ITITFO=ITITCV(1)
      ITITCA=ITITCV(2)
      ITITFI=ITITCV(3)
      ITITCO=ITITCV(4)
      PTITHE=PTITRV(1)
      PTITWI=PTITRV(2)
      PTITVG=PTITRV(3)
      PTITHG=PTITRV(4)
      PTITTH=PTITRV(5)
      PTITDS=PTITRV(6)
C
      IX1LFO=IX1LCV(1)
      IX1LCA=IX1LCV(2)
      IX1LFI=IX1LCV(3)
      IX1LCO=IX1LCV(4)
      IX1LJU=IX1LCV(5)
      IX1LDI=IX1LCV(6)
      PX1LHE=PX1LRV(1)
      PX1LWI=PX1LRV(2)
      PX1LVG=PX1LRV(3)
      PX1LHG=PX1LRV(4)
      PX1LTH=PX1LRV(5)
      PX1LDS=PX1LRV(6)
      PX1LOF=PX1LRV(7)
      PX1LAN=PX1LRV(8)
      PX1LXC=PX1LRV(9)
      PX1LYC=PX1LRV(10)
C
      IX2LFO=IX2LCV(1)
      IX2LCA=IX2LCV(2)
      IX2LFI=IX2LCV(3)
      IX2LCO=IX2LCV(4)
      IX2LJU=IX2LCV(5)
      IX2LDI=IX2LCV(6)
      PX2LHE=PX2LRV(1)
      PX2LWI=PX2LRV(2)
      PX2LVG=PX2LRV(3)
      PX2LHG=PX2LRV(4)
      PX2LTH=PX2LRV(5)
      PX2LDS=PX2LRV(6)
      PX2LOF=PX2LRV(7)
      PX2LAN=PX2LRV(8)
      PX2LXC=PX2LRV(9)
      PX2LYC=PX2LRV(10)
C
      IX3LFO=IX3LCV(1)
      IX3LCA=IX3LCV(2)
      IX3LFI=IX3LCV(3)
      IX3LCO=IX3LCV(4)
      IX3LJU=IX3LCV(5)
      IX3LDI=IX3LCV(6)
      PX3LHE=PX3LRV(1)
      PX3LWI=PX3LRV(2)
      PX3LVG=PX3LRV(3)
      PX3LHG=PX3LRV(4)
      PX3LTH=PX3LRV(5)
      PX3LDS=PX3LRV(6)
      PX3LOF=PX3LRV(7)
      PX3LAN=PX3LRV(8)
      PX3LXC=PX3LRV(9)
      PX3LYC=PX3LRV(10)
C
      IY1LFO=IY1LCV(1)
      IY1LCA=IY1LCV(2)
      IY1LFI=IY1LCV(3)
      IY1LCO=IY1LCV(4)
      IY1LJU=IY1LCV(5)
      IY1LDI=IY1LCV(6)
      PY1LHE=PY1LRV(1)
      PY1LWI=PY1LRV(2)
      PY1LVG=PY1LRV(3)
      PY1LHG=PY1LRV(4)
      PY1LTH=PY1LRV(5)
      PY1LDS=PY1LRV(6)
      PY1LOF=PY1LRV(7)
      PY1LAN=PY1LRV(8)
      PY1LXC=PY1LRV(9)
      PY1LYC=PY1LRV(10)
C
      IY2LFO=IY2LCV(1)
      IY2LCA=IY2LCV(2)
      IY2LFI=IY2LCV(3)
      IY2LCO=IY2LCV(4)
      IY2LJU=IY2LCV(5)
      IY2LDI=IY2LCV(6)
      PY2LHE=PY2LRV(1)
      PY2LWI=PY2LRV(2)
      PY2LVG=PY2LRV(3)
      PY2LHG=PY2LRV(4)
      PY2LTH=PY2LRV(5)
      PY2LDS=PY2LRV(6)
      PY2LOF=PY2LRV(7)
      PY2LAN=PY2LRV(8)
      PY2LXC=PY2LRV(9)
      PY2LYC=PY2LRV(10)
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRLA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRLA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IMANUF,IMODEL,ISEQSW,IREPCH
   52   FORMAT('IMANUF,IMODEL,ISEQSW,IREPCH = ',3(A4,2X),A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)NUMNAM,NUMSEQ
   56   FORMAT('NUMNAM,NUMSEQ = ',2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)IBUGG4,ISUBG4,IERRG4
   59   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  WRITE OUT THE TITLE  **
C               ***************************
C
      IF(NCTITL.LE.0)GOTO1190
C
      PX1=(PXMIN+PXMAX)/2.0
      PY1=PYMAX+PTITDS
C
      NCTEXT=NCTITL
      DO1110I=1,NCTEXT
      ICTEXT(I)=ITITTE(I)
 1110 CONTINUE
C
      IFONT=ITITFO
      ICASE=ITITCA
      IJUST='CEBO'
      IDIR='HORI'
      ANGLE=0.0
      IFILL=ITITFI
      ICOL=ITITCO
C
      PHEIGH=PTITHE
      PWIDTH=PTITWI
      PVEGAP=PTITVG
      PHOGAP=PTITHG
      PTHICK=PTITTH
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
 1190 CONTINUE
C
C               *************************************************
C               **  STEP 2--                                   **
C               **  WRITE OUT THE FIRST HORIZONTAL AXIS LABEL  **
C               *************************************************
C
      IF(NCX1LA.LE.0)GOTO1290
C
      IF(PX1LXC.GE.0.0 .AND. PX1LXC.LE.100.0)THEN
        PX1=PX1LXC
      ELSE
        PX1=((PXMIN+PXMAX)/2.0)+PX1LOF
      ENDIF
      IF(PX1LYC.GE.0.0 .AND. PX1LYC.LE.100.0)THEN
        PY1=PX1LYC-PX1LHE
      ELSE
        PY1=PYMIN-PX1LDS-PX1LHE
      ENDIF
C
      NCTEXT=NCX1LA
      DO1210I=1,NCTEXT
      ICTEXT(I)=IX1LTE(I)
 1210 CONTINUE
C
      IFONT=IX1LFO
      ICASE=IX1LCA
CCCCC IJUST='CEBO'
      IJUST=IX1LJU
CCCCC IDIR='HORI'
      IDIR=IX1LDI
CCCCC ANGLE=0.0
      ANGLE=PX1LAN
      IFILL=IX1LFI
      ICOL=IX1LCO
C
      PHEIGH=PX1LHE
      PWIDTH=PX1LWI
      PVEGAP=PX1LVG
      PHOGAP=PX1LHG
      PTHICK=PX1LTH
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
 1290 CONTINUE
C
C               **************************************************
C               **  STEP 3--                                    **
C               **  WRITE OUT THE SECOND HORIZONTAL AXIS LABEL  **
C               **************************************************
C
      IF(NCX2LA.LE.0)GOTO1390
C
      IF(PX2LXC.GE.0.0 .AND. PX2LXC.LE.100.0)THEN
        PX1=PX2LXC
      ELSE
        PX1=((PXMIN+PXMAX)/2.0)+PX2LOF
      ENDIF
      IF(PX2LYC.GE.0.0 .AND. PX2LYC.LE.100.0)THEN
        PY1=PX2LYC-PX2LHE
      ELSE
        PY1=PYMIN-PX2LDS-PX2LHE
      ENDIF
C
      NCTEXT=NCX2LA
      DO1310I=1,NCTEXT
      ICTEXT(I)=IX2LTE(I)
 1310 CONTINUE
C
      IFONT=IX2LFO
      ICASE=IX2LCA
CCCCC IJUST='CEBO'
      IJUST=IX2LJU
CCCCC IDIR='HORI'
      IDIR=IX2LDI
CCCCC ANGLE=0.0
      ANGLE=PX2LAN
      IFILL=IX2LFI
      ICOL=IX2LCO
C
      PHEIGH=PX2LHE
      PWIDTH=PX2LWI
      PVEGAP=PX2LVG
      PHOGAP=PX2LHG
      PTHICK=PX2LTH
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
 1390 CONTINUE
C
C               **************************************************
C               **  STEP 4--                                    **
C               **  WRITE OUT THE THIRD  HORIZONTAL AXIS LABEL  **
C               **************************************************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRLA')THEN
        WRITE(ICOUT,1402)NCX3LA,IX3LFO
 1402   FORMAT('NCX3LA,IX3LFO = ',I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO1405I=1,NCX3LA
          WRITE(ICOUT,1407)I,IX3LTE(I)
 1407     FORMAT('I,IX3LTE(I) = ',I8,2X,A4)
          CALL DPWRST('XXX','BUG ')
 1405   CONTINUE
      ENDIF
C
      IF(NCX3LA.LE.0)GOTO1490
C
      IF(PX3LXC.GE.0.0 .AND. PX3LXC.LE.100.0)THEN
        PX1=PX3LXC
      ELSE
        PX1=((PXMIN+PXMAX)/2.0)+PX3LOF
      ENDIF
      IF(PX3LYC.GE.0.0 .AND. PX3LYC.LE.100.0)THEN
        PY1=PX3LYC-PX3LHE
      ELSE
        PY1=PYMIN-PX3LDS-PX3LHE
      ENDIF
C
      NCTEXT=NCX3LA
      DO1410I=1,NCTEXT
      ICTEXT(I)=IX3LTE(I)
 1410 CONTINUE
C
      IFONT=IX3LFO
      ICASE=IX3LCA
CCCCC IJUST='CEBO'
      IJUST=IX3LJU
CCCCC IDIR='HORI'
      IDIR=IX3LDI
CCCCC ANGLE=0.0
      ANGLE=PX3LAN
      IFILL=IX3LFI
      ICOL=IX3LCO
C
      PHEIGH=PX3LHE
      PWIDTH=PX3LWI
      PVEGAP=PX3LVG
      PHOGAP=PX3LHG
      PTHICK=PX3LTH
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
 1490 CONTINUE
C
C               ***********************************************
C               **  STEP 5--                                 **
C               **  WRITE OUT THE LEFT  VERTICAL AXIS LABEL  **
C               ***********************************************
C
      IF(NCY1LA.LE.0)GOTO1590
C
      IF(PY1LXC.GE.0.0 .AND. PY1LXC.LE.100.0)THEN
        PX1=PY1LXC-PY1LWI
      ELSE
        PX1=PXMIN-PY1LDS-PY1LWI
      ENDIF
      IF(PY1LYC.GE.0.0 .AND. PY1LYC.LE.100.0)THEN
        PY1=PY1LYC
      ELSE
        PY1=((PYMIN+PYMAX)/2.0)+PY1LOF
      ENDIF
C
      NCTEXT=NCY1LA
      DO1510I=1,NCTEXT
      ICTEXT(I)=IY1LTE(I)
 1510 CONTINUE
C
      IFONT=IY1LFO
      ICASE=IY1LCA
CCCCC THE FOLLOWING 1-LINE FIX WAS DONE                     FEBRUARY 1989
CCCCC TO USE THE CENTER FOR VERTICAL JUST. OF Y1LABEL.      FEBRUARY 1989
CCCCC THE CENTER IS THE NEEDED CHOICE FOR METAFILE DEVICES  FEBRUARY 1989
CCCCC AND FOR DEVICES THAT SUPPORT ROTATED TEXT (ALAN)      FEBRUARY 1989
CCCCC IJUST='CEBO'
CCCCC IJUST='CECE'
      IJUST=IY1LJU
CCCCC IDIR='VERT'
CCCCC ANGLE=90.0
      IDIR=IY1LDI
      ANGLE=PY1LAN
      IFILL=IY1LFI
      ICOL=IY1LCO
C
CCCCC START OF FIX  AUGUST 1989 (& FEBRUARY 1989 FOR VAX)
CCCCC KEY THE SIZE TO HORIZONTAL CHARACTER SIZE.  THIS WILL MAKE THE
CCCCC SIZE OF THE Y1LABEL THE SAME AS THE SIZE OF THE XLABEL
CCCCC UPDATE FIX: JANUARY, 1987  (& FEBRUARY 1989 FOR VAX)
CCCCC IF HARDWARE CHARACTERS, APPLY THE FIX.  IF SOFTWARE CHARACTERS,
CCCCC DO NOT APPLY THE FIX.
C
      IF(IFONT.EQ.'TEKT')GOTO1520
      PHEIGH=PY1LHE*(ANUMVP/ANUMHP)
      PWIDTH=PY1LWI*(ANUMHP/ANUMVP)
      PVEGAP=PY1LVG*(ANUMVP/ANUMHP)
      PHOGAP=PY1LHG*(ANUMHP/ANUMVP)
      GOTO1530
 1520 CONTINUE
      PHEIGH=PY1LHE
      PWIDTH=PY1LWI
      PVEGAP=PY1LVG
      PHOGAP=PY1LHG
 1530 CONTINUE
      PTHICK=PY1LTH
C
CCCCC END OF FIX
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
 1590 CONTINUE
C
C               ***********************************************
C               **  STEP 6--                                 **
C               **  WRITE OUT THE RIGHT VERTICAL AXIS LABEL  **
C               ***********************************************
C
      IF(NCY2LA.LE.0)GOTO1690
C
      IF(PY2LXC.GE.0.0 .AND. PY2LXC.LE.100.0)THEN
        PX1=PY2LXC
      ELSE
        PX1=PXMAX+PY2LDS
      ENDIF
      IF(PX1LYC.GE.0.0 .AND. PX1LYC.LE.100.0)THEN
        PY1=PY2LYC
      ELSE
        PY1=((PYMIN+PYMAX)/2.0)+PY2LOF
      ENDIF
C
      NCTEXT=NCY2LA
      DO1610I=1,NCTEXT
      ICTEXT(I)=IY2LTE(I)
 1610 CONTINUE
C
      IFONT=IY2LFO
      ICASE=IY2LCA
CCCCC THE FOLLOWING 1-LINE FIX WAS DONE                     FEBRUARY 1989
CCCCC TO USE THE CENTER FOR VERTICAL JUST. OF Y2LABEL.      FEBRUARY 1989
CCCCC THE CENTER IS THE NEEDED CHOICE FOR METAFILE DEVICES  FEBRUARY 1989
CCCCC AND FOR DEVICES THAT SUPPORT ROTATED TEXT (ALAN)      FEBRUARY 1989
CCCCC IJUST='CEBO'
CCCCC IJUST='CECE'
      IJUST=IY2LJU
CCCCC IDIR='VERT'
CCCCC ANGLE=90.0
      IDIR=IY2LDI
      ANGLE=PY2LAN
      IFILL=IY2LFI
      ICOL=IY2LCO
C
CCCCC START OF FIX   (FEBRUARY 1989 FOR VAX)
CCCCC KEY THE SIZE TO HORIZONTAL CHARACTER SIZE.  THIS WILL MAKE THE
CCCCC SIZE OF THE Y2LABEL THE SAME AS THE SIZE OF THE XLABEL
CCCCC UPDATE FIX: JANUARY, 1987 (& FEBRUARY 1989 FOR VAX)
CCCCC IF HARDWARE CHARACTERS, APPLY THE FIX.  IF SOFTWARE CHARACTERS,
CCCCC DO NOT APPLY THE FIX.
C
      IF(IFONT.EQ.'TEKT')GOTO1620
      PHEIGH=PY2LHE*(ANUMVP/ANUMHP)
      PWIDTH=PY2LWI*(ANUMHP/ANUMVP)
      PVEGAP=PY2LVG*(ANUMVP/ANUMHP)
      PHOGAP=PY2LHG*(ANUMHP/ANUMVP)
      GOTO1630
 1620 CONTINUE
      PHEIGH=PY2LHE
      PWIDTH=PY2LWI
      PVEGAP=PY2LVG
      PHOGAP=PY2LHG
 1630 CONTINUE
C
CCCCC END OF FIX
C
      PTHICK=PY2LTH
C
      IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
 1690 CONTINUE
C
C               *************************************
C               **  STEP 7--                       **
C               **  WRITE OUT THE SEQUENCE NUMBER  **
C               *************************************
C
      IF(ISEQSW.EQ.'OFF')GOTO1790
C
      PX1=PXMAX+10.0
      IF(PX1.GT.95.0)PX1=95.0
      PY1=PYMAX+5.0
      IF(PY1.GT.94.0)PY1=94.0
C
      ANUMSE=NUMSEQ
      CALL DPCONH(NUMSEQ,ANUMSE,ICTEXT,NCTEXT,IBUGG4,IERRG4)
C
      IFONT=ITITFO
      ICASE=ITITCA
      IJUST='CEBO'
      IDIR='HORI'
      ANGLE=0.0
      IFILL=ITITFI
      ICOL=ITITCO
C
      PHEIGH=PTITHE
      PWIDTH=PTITWI
      PVEGAP=PTITVG
      PHOGAP=PTITHG
      PTHICK=PTITTH
C
CCCCC IF(NCTEXT.GE.1)CALL DPREPL(ICTEXT,NCTEXT,
CCCCC1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
CCCCC1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
CCCCC1IBUGG4,IERRG4)
C
      CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1ISYMBL,ISPAC,
     1IMPSW2,AMPSCH,AMPSCW,
     1PX99,PY99)
C
C     THE FOLLOWING LINE +
C     SOME ADDITIONAL LOGIC
C     WAS MOVED TO PLOTG2 (NOV. 1986)
C
CCCCC NUMSEQ=NUMSEQ+1
C
 1790 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'WRLA')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPWRLA--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IMANUF,IMODEL
 9012 FORMAT('IMANUF,IMODEL = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NCTEXT
 9013 FORMAT('NCTEXT = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NCTEXT.GE.1.AND.NCTEXT.LE.1000)
     1WRITE(ICOUT,9014)(ICTEXT(I),I=1,NCTEXT)
 9014 FORMAT('(ICTEXT(I),I=1,NCTEXT) = ',80A1)
      IF(NCTEXT.GE.1.AND.NCTEXT.LE.1000)
     1CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)ISEQSW,NUMSEQ
 9016 FORMAT('ISEQSW,NUMSEQ = ',A4,I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9019)IBUGG4,ISUBG4,IERRG4
 9019 FORMAT('IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9021)IREPCH
 9021 FORMAT('IREPCH = ',A1)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DPWRLE(ILEGTE,ILEGST,ILEGSP,
     1                  ILEGNA,PLEGXC,PLEGYC,ILEGFO,ILEGCA,ILEGJU,
     1                  ILEGDI,ALEGAN,ILEGFI,ILEGCO,ILEGUN,
     1                  PLEGHE,PLEGWI,PLEGVG,PLEGHG,PLEGTH,NUMLEG,
     1                  PBOXXC,PBOXYC,IBOBCO,
     1                  IBOPPA,IBOBPA,PBOPTH,PBOPGA,IBOFPA,IBOFCO,
     1                  PBOFTH,PBOSHE,PBOSWI,NUMBOX,
     1                  PARRXC,PARRYC,IARRPA,IARRCO,PARRTH,
     1                  PARHLE,PARHWI,NUMARR,
     1                  PSEGXC,PSEGYC,ISEGPA,ISEGCO,PSEGTH,NUMSEG,
     1                  IMPSW2,AMPSCH,AMPSCW,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,ISYMBL,ISPAC)
C
C  BUG FIX: ADDED PLEGTH PARAMETER TO CALL LIST AUGUST, 1987
C           ALSO USE DPCOPA INCLUDE FILE TO DIMENSION
C           ALONG WITH "*" NOTATION
C
C
C     PURPOSE--WRITE OUT LEGENDS, AND BOXES, ARROWS, AND SEGMENTS
C              (IF CALLED FOR) ON A PLOT.
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     UPDATED         --JANUARY   1989.  ADD PARAMETER TO ARGUMENT LIST (ALAN)
C     UPDATED         --JANUARY   1989.  DIMENSION STATEMENTS SHOULD REFLECT
C                                        THE USE OF PARAMETER STATEMENTS
C                                        IN THE DPCOPA.INC FILE (ALAN)
C     UPDATED         --JANUARY   1989.  MODIFY CALLS TO DPDRPL (ALAN)
C     UPDATED         --JANUARY   1989.  MODIFY CALL  TO DPFIRE (ALAN)
C     UPDATED         --JANUARY   1989.  XX
C     UPDATED         --AUGUST    1992.  ELIMINATE BOX SOLID FILL
C     UPDATED         --AUGUST    1992.  ADD SHADOW TO BOX
C     UPDATED         --AUGUST    1992.  FIX ARROW COORDINATES (ALAN)
C     UPDATED         --AUGUST    1992.  FIX BOX FILL (ALAN)
C     UPDATED         --MARCH     1993.  DISTINGUISH BETWEEN BORDER
C                                        & FILL THICKNESS (ALAN)
C     UPDATED         --AUGUST    1995.  BUG FOR LEGENDS NOT ENTERED
C                                        IN NUMERICAL ORDER
C     UPDATED         --SEPTEMBER 1999.  ARGUMENT LIST FOR DPWRTE
C     UPDATED         --DECEMBER  1999.  ADD ILEGUN (ALLOW LEGENDS
C                                        TO BE DEFINED IN EITHER
C                                        SCREEN OR DATA UNITS)
C     UPDATED         --FEBRUARY  2018.  FOR ILEGUN, ALLOW SEPARATE
C                                        SEPARATE SPECIFICATION FOR
C                                        X AND Y AXIS
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*4 IFLAG
      CHARACTER*4 IPATT2
C
      CHARACTER*4 ISEGPA
      CHARACTER*4 ISEGCO
C
      CHARACTER*4 IARRPA
      CHARACTER*4 IARRCO
C
      CHARACTER*4 IBOBCO
      CHARACTER*4 IBOPPA
      CHARACTER*4 IBOBPA
      CHARACTER*4 IBOFPA
      CHARACTER*4 IBOFCO
C
      CHARACTER*4 ILEGTE
      CHARACTER*4 ILEGFO
      CHARACTER*4 ILEGCA
      CHARACTER*4 ILEGJU
      CHARACTER*4 ILEGDI
      CHARACTER*4 ILEGFI
      CHARACTER*4 ILEGCO
      CHARACTER*4 ILEGNA
      CHARACTER*4 ILEGUN
C
      CHARACTER*4 ICTEXT
C
      CHARACTER*4 IFIG
      CHARACTER*4 IPATT
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILL
      CHARACTER*4 ICOL
      CHARACTER*4 ICOLB
      CHARACTER*4 ICOLP
      CHARACTER*4 IMPSW2
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
      CHARACTER*4 IFUNC
C
      CHARACTER*1 IREPCH
C
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
CCCCC OCTOBER 1993.  ADD FOLLOWING LINE
      CHARACTER*4 ITRCSW
C
      INCLUDE 'DPCOPA.INC'
      DIMENSION PSEGXC(MAXSG,2)
      DIMENSION PSEGYC(MAXSG,2)
      DIMENSION ISEGPA(*)
      DIMENSION PSEGTH(*)
      DIMENSION ISEGCO(*)
C
      DIMENSION PARRXC(MAXAR,2)
      DIMENSION PARRYC(MAXAR,2)
      DIMENSION IARRPA(*)
      DIMENSION PARRTH(*)
      DIMENSION IARRCO(*)
      DIMENSION PARHLE(*)
      DIMENSION PARHWI(*)
C
      DIMENSION PBOXXC(MAXBX,2)
      DIMENSION PBOXYC(MAXBX,2)
C
      DIMENSION IBOBCO(*)
C
      DIMENSION IBOPPA(*)
      DIMENSION PBOPTH(*)
      DIMENSION PBOPGA(*)
CCCCC AUGUST 1992.  FOLLOWING LINE MODIFIED
CCCCC DIMENSION IBOPCO(*)
      DIMENSION IBOBPA(*)
C
      DIMENSION IBOFPA(*)
      DIMENSION PBOFTH(*)
      DIMENSION IBOFCO(*)
CCCCC THE FOLLOWING 2 LINES WERE ADDED AUGUST 1992
      DIMENSION PBOSHE(*)
      DIMENSION PBOSWI(*)
C
      DIMENSION ILEGTE(*)
      DIMENSION ILEGST(*)
      DIMENSION ILEGSP(*)
CCCCC AUGUST 1995.  ADD FOLLOWING LINE.
      DIMENSION ILEGNA(*)
      DIMENSION PLEGXC(*)
      DIMENSION PLEGYC(*)
      DIMENSION ILEGFO(*)
      DIMENSION ILEGCA(*)
      DIMENSION ILEGJU(*)
      DIMENSION ILEGDI(*)
      DIMENSION ALEGAN(*)
      DIMENSION ILEGFI(*)
      DIMENSION ILEGCO(*)
      DIMENSION ILEGUN(*)
      DIMENSION PLEGWI(*)
      DIMENSION PLEGHE(*)
      DIMENSION PLEGHG(*)
      DIMENSION PLEGVG(*)
      DIMENSION PLEGTH(*)
C
      DIMENSION ICTEXT(130)
C
      DIMENSION PX(MAXSG)
      DIMENSION PY(MAXSG)
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRLE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRLE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IMANUF,IMODEL,IBUGG4,ISUBG4,IERRG4
   52   FORMAT('IMANUF,IMODEL,IBUGG4,ISUBG4,IERRG4 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IREPCH
   61   FORMAT('IREPCH = ',A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  DRAW OUT THE LINE SEGMENTS  **
C               **********************************
C
      IF(NUMSEG.GE.1)THEN
        DO1100ISEG=1,NUMSEG
C
          PX(1)=PSEGXC(ISEG,1)
          PY(1)=PSEGYC(ISEG,1)
          PX(2)=PSEGXC(ISEG,2)
          PY(2)=PSEGYC(ISEG,2)
          NP=2
          IFIG='LINE'
          IPATT=ISEGPA(ISEG)
          ICOL=ISEGCO(ISEG)
          PTHICK=PSEGTH(ISEG)
          IF(PX(1).LT.0.0.OR.PX(1).GT.100.0)GOTO1100
          IF(PX(2).LT.0.0.OR.PX(2).GT.100.0)GOTO1100
          IF(PX(3).LT.0.0.OR.PX(3).GT.100.0)GOTO1100
          IF(PX(4).LT.0.0.OR.PX(4).GT.100.0)GOTO1100
          IF(PY(1).LT.0.0.OR.PY(1).GT.100.0)GOTO1100
          IF(PY(2).LT.0.0.OR.PY(2).GT.100.0)GOTO1100
          IF(PY(3).LT.0.0.OR.PY(3).GT.100.0)GOTO1100
          IF(PY(4).LT.0.0.OR.PY(4).GT.100.0)GOTO1100
          IFLAG='ON'
          CALL DPDRPL(PX,PY,NP,
     1                IFIG,IPATT,PTHICK,ICOL,
     1                JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
 1100   CONTINUE
      ENDIF
C
C               ***************************
C               **  STEP 2--             **
C               **  DRAW OUT THE ARROWS  **
C               ***************************
C
C  AUGUST 1992.  USE DPARR2 ROUTINE TO DRAW THE ARROW (I COULD NOT
C  GET THE POSITIONING RIGHT TRYING TO DRAW IT WITH THE POLYMARKER
C  ROUTINE.
C
      IF(NUMARR.GE.1)THEN
        DO1200IARR=1,NUMARR
C
          PX1=PARRXC(IARR,1)
          PY1=PARRYC(IARR,1)
          PX2=PARRXC(IARR,2)
          PY2=PARRYC(IARR,2)
          PX(1)=PX1
          PY(1)=PY1
          PX(2)=PX2
          PY(2)=PY2
          NP=2
          IFIG='ARRO'
          IPATT=IARRPA(IARR)
          ICOL=IARRCO(IARR)
          PTHICK=PARRTH(IARR)
          IF(PX(1).LT.0.0.OR.PX(1).GT.100.0)GOTO1200
          IF(PX(2).LT.0.0.OR.PX(2).GT.100.0)GOTO1200
          IF(PX(3).LT.0.0.OR.PX(3).GT.100.0)GOTO1200
          IF(PX(4).LT.0.0.OR.PX(4).GT.100.0)GOTO1200
          IF(PY(1).LT.0.0.OR.PY(1).GT.100.0)GOTO1200
          IF(PY(2).LT.0.0.OR.PY(2).GT.100.0)GOTO1200
          IF(PY(3).LT.0.0.OR.PY(3).GT.100.0)GOTO1200
          IF(PY(4).LT.0.0.OR.PY(4).GT.100.0)GOTO1200
          NP=1
          IFILL='ON'
          PREPTH=0.1
          PREPSP=0.1
          PHEIGH=PARHWI(IARR)
          PWIDTH=PARHLE(IARR)
          PHOGAP=0.1
          PVEGAP=0.1
          ITRCSW='ON'
          CALL DPARR3(PX1,PY1,PX2,PY2,
     1                IFIG,ITRCSW,IPATT,ICOL,PTHICK,
     1                IFILL,ICOL,ICOL,PREPTH,PREPSP,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP)
C
 1200   CONTINUE
      ENDIF
C
C               *******************************
C               **  STEP 3--                 **
C               **  FILL THE BOX BACKGROUND  **
C               *******************************
C
      IF(NUMBOX.GE.1)THEN
        DO1300IBOX=1,NUMBOX
C
          PX1=PBOXXC(IBOX,1)
          PY1=PBOXYC(IBOX,1)
          PX2=PBOXXC(IBOX,2)
          PY2=PBOXYC(IBOX,2)
          PX(1)=PX1
          PY(1)=PY1
          PX(2)=PX2
          PY(2)=PY1
          PX(3)=PX2
          PY(3)=PY2
          PX(4)=PX1
          PY(4)=PY2
          PX(5)=PX1
          PY(5)=PY1
          NP=5
          IFIG='BOX'
          IPATT=IBOFPA(IBOX)
          IF(IPATT.EQ.'OFF')GOTO1300
          IF(IPATT.EQ.'EMPT')GOTO1300
          IF(IPATT.EQ.'    ')GOTO1300
          IF(IPATT.EQ.'NONE')GOTO1300
          IF(IPATT.EQ.'BLAN')GOTO1300
          IF(IPATT.EQ.'ON')IPATT='SOLI'
          IPATT2=IBOPPA(IBOX)
          ICOLB=IBOFCO(IBOX)
          PTHICK=PBOFTH(IBOX)
          PXGAP=PBOPGA(IBOX)
          PYGAP=PBOPGA(IBOX)
          ICOLP=ICOLB
          IF(PX(1).LT.0.0.OR.PX(1).GT.100.0)GOTO1300
          IF(PX(2).LT.0.0.OR.PX(2).GT.100.0)GOTO1300
          IF(PX(3).LT.0.0.OR.PX(3).GT.100.0)GOTO1300
          IF(PX(4).LT.0.0.OR.PX(4).GT.100.0)GOTO1300
          IF(PX(5).LT.0.0.OR.PX(5).GT.100.0)GOTO1300
          IF(PY(1).LT.0.0.OR.PY(1).GT.100.0)GOTO1300
          IF(PY(2).LT.0.0.OR.PY(2).GT.100.0)GOTO1300
          IF(PY(3).LT.0.0.OR.PY(3).GT.100.0)GOTO1300
          IF(PY(4).LT.0.0.OR.PY(4).GT.100.0)GOTO1300
          IF(PY(5).LT.0.0.OR.PY(5).GT.100.0)GOTO1300
          CALL DPFIRE(PX,PY,NP,IFIG,IPATT,PTHICK,
     1                PXGAP,PYGAP,ICOLB,ICOLP,IPATT2)
C
 1300   CONTINUE
      ENDIF
C
C               ********************************
C               **  STEP 4--                  **
C               **  DRAW OUT THE BOX PATTERN  **
C               ********************************
C
CCCCC THE FOLLOWING SECTION WAS SKIPPED AROUND AUGUST 1992
CCCCC THE FOLLOWING LINE WAS ADDED AUGUST 1992
      GOTO1490
C
CCCCC IF(NUMBOX.LE.0)GOTO1490
CCCCC DO1400IBOX=1,NUMBOX
C
CCCCC PX1=PBOXXC(IBOX,1)
CCCCC PY1=PBOXYC(IBOX,1)
CCCCC PX2=PBOXXC(IBOX,2)
CCCCC PY2=PBOXYC(IBOX,2)
CCCCC PX(1)=PX1
CCCCC PY(1)=PY1
CCCCC PX(2)=PX2
CCCCC PY(2)=PY1
CCCCC PX(3)=PX2
CCCCC PY(3)=PY2
CCCCC PX(4)=PX1
CCCCC PY(4)=PY2
CCCCC PX(5)=PX1
CCCCC PY(5)=PY1
CCCCC NP=5
CCCCC IFIG='BOX'
CCCCC IPATT=IBOPPA(IBOX)
CCCCC IPATT2='SOLI'
CCCCC ICOLB=IBOBCO(IBOX)
CCCCC PTHICK=PBOPTH(IBOX)
CCCCC PXSPA=PBOPGA(IBOX)
CCCCC PYSPA=PBOPGA(IBOX)
CCCCC ICOLP=IBOPCO(IBOX)
CCCCC ICOLP=ICOLB
CCCCC IF(PX(1).LT.0.0.OR.PX(1).GT.100.0)GOTO1400
CCCCC IF(PX(2).LT.0.0.OR.PX(2).GT.100.0)GOTO1400
CCCCC IF(PX(3).LT.0.0.OR.PX(3).GT.100.0)GOTO1400
CCCCC IF(PX(4).LT.0.0.OR.PX(4).GT.100.0)GOTO1400
CCCCC IF(PX(5).LT.0.0.OR.PX(5).GT.100.0)GOTO1400
CCCCC IF(PY(1).LT.0.0.OR.PY(1).GT.100.0)GOTO1400
CCCCC IF(PY(2).LT.0.0.OR.PY(2).GT.100.0)GOTO1400
CCCCC IF(PY(3).LT.0.0.OR.PY(3).GT.100.0)GOTO1400
CCCCC IF(PY(4).LT.0.0.OR.PY(4).GT.100.0)GOTO1400
CCCCC IF(PY(5).LT.0.0.OR.PY(5).GT.100.0)GOTO1400
CCCCC CALL DPFIRE(PX,PY,NP,
CCCCC1IFIG,IPATT,PTHICK,PXSPA,PYSPA,ICOLB,ICOLP,IPATT2)
C
C1400 CONTINUE
 1490 CONTINUE
C
C               ******************************
C               **  STEP 5--                **
C               **  DRAW OUT THE BOX FRAME  **
C               ******************************
C
      IF(NUMBOX.GE.1)THEN
        DO1500IBOX=1,NUMBOX
C
          PX1=PBOXXC(IBOX,1)
          PY1=PBOXYC(IBOX,1)
          PX2=PBOXXC(IBOX,2)
          PY2=PBOXYC(IBOX,2)
          PX(1)=PX1
          PY(1)=PY1
          PX(2)=PX2
          PY(2)=PY1
          PX(3)=PX2
          PY(3)=PY2
          PX(4)=PX1
          PY(4)=PY2
          PX(5)=PX1
          PY(5)=PY1
          NP=5
          IFIG='BOX'
          IPATT=IBOBPA(IBOX)
          ICOL=IBOBCO(IBOX)
          PTHICK=PBOPTH(IBOX)
          IF(PX(1).LT.0.0.OR.PX(1).GT.100.0)GOTO1500
          IF(PX(2).LT.0.0.OR.PX(2).GT.100.0)GOTO1500
          IF(PX(3).LT.0.0.OR.PX(3).GT.100.0)GOTO1500
          IF(PX(4).LT.0.0.OR.PX(4).GT.100.0)GOTO1500
          IF(PX(5).LT.0.0.OR.PX(5).GT.100.0)GOTO1500
          IF(PY(1).LT.0.0.OR.PY(1).GT.100.0)GOTO1500
          IF(PY(2).LT.0.0.OR.PY(2).GT.100.0)GOTO1500
          IF(PY(3).LT.0.0.OR.PY(3).GT.100.0)GOTO1500
          IF(PY(4).LT.0.0.OR.PY(4).GT.100.0)GOTO1500
          IF(PY(5).LT.0.0.OR.PY(5).GT.100.0)GOTO1500
          IFLAG='ON'
          CALL DPDRPL(PX,PY,NP,
     1                IFIG,IPATT,PTHICK,ICOL,
     1                JPATT,JTHICK,PTHIC2,JCOL,IFLAG)
C
CCCCC     THE FOLLOWING 2 SECTIONS WERE ADDED AUGUST 1992
CCCCC     TO ADD A SHADOW TO THE BOX   AUGUST 1992
C
          PSH=PBOSHE(IBOX)
          PSW=PBOSWI(IBOX)
          EPSBS=0.000001
          IF(PSH.LT.EPSBS.AND.PSW.LT.EPSBS)GOTO1500
          PLEFT=PX1
          PRIGHT=PX2
          IF(PX2.LT.PX1)THEN
            PLEFT=PX2
            PRIGHT=PX1
          ENDIF
          PBOTTO=PY1
          PTOP=PY2
          IF(PY2.LT.PY1)THEN
            PBOTTO=PY2
            PTOP=PY1
          ENDIF
          PX(1)=PLEFT+PSW
          PY(1)=PBOTTO-PSH
          PX(2)=PRIGHT+PSW
          PY(2)=PBOTTO-PSH
          PX(3)=PRIGHT+PSW
          PY(3)=PBOTTO
          PX(4)=PLEFT+PSW
          PY(4)=PBOTTO
          PX(5)=PLEFT+PSW
          PY(5)=PBOTTO-PSH
          IPATT='SOLI'
          IPATT2='SOLI'
          ICOLB=IBOBCO(IBOX)
          ICOLP=ICOLB
          CALL DPFIRE(PX,PY,NP,IFIG,IPATT,PTHICK,
     1                PXSPA,PYSPA,ICOLB,ICOLP,IPATT2)
C
          PX(1)=PRIGHT
          PY(1)=PBOTTO-PSH
          PX(2)=PRIGHT+PSW
          PY(2)=PBOTTO-PSH
          PX(3)=PRIGHT+PSW
          PY(3)=PTOP-PSH
          PX(4)=PRIGHT
          PY(4)=PTOP-PSH
          PX(5)=PRIGHT
          PY(5)=PBOTTO-PSH
          CALL DPFIRE(PX,PY,NP,IFIG,IPATT,PTHICK,
     1                PXSPA,PYSPA,ICOLB,ICOLP,IPATT2)
C
 1500   CONTINUE
      ENDIF
C
C               *****************************
C               **  STEP 6--               **
C               **  WRITE OUT THE LEGENDS  **
C               *****************************
C
      IF(NUMLEG.GE.1)THEN
        DO1600ILEG=1,NUMLEG
CCCCC     AUGUST 1995.  BUG IF LEGENDS NOT ENTERED IN PROPER ORDER.
CCCCC     INDEX BY VALUE IN ILEGNA.
          READ(ILEGNA(ILEG),'(I4)')INDX
          IF(INDX.LT.1.OR.INDX.GT.100)INDX=ILEG
C
CCCCC     AUGUST 1995.  REPLACE ILEG WITH INDX IN FOLLOWING ARRAY INDICES.
          IFONT=ILEGFO(ILEG)
          ICASE=ILEGCA(ILEG)
          IJUST=ILEGJU(ILEG)
          IDIR=ILEGDI(ILEG)
          ANGLE=ALEGAN(ILEG)
          IFILL=ILEGFI(ILEG)
          ICOL=ILEGCO(ILEG)
          PHEIGH=PLEGHE(ILEG)
          PWIDTH=PLEGWI(ILEG)
          PHOGAP=PLEGHG(ILEG)
          PVEGAP=PLEGVG(ILEG)
          PX1=PLEGXC(ILEG)
          PY1=PLEGYC(ILEG)
C
C         2018/02: SUPPORT 'DD', 'DS', 'SD', AND 'SS' TO ALLOW
C                  SEPARATE SPECIFICATION FOR X AND Y AXIS
C
          IF(ILEGUN(ILEG).EQ.'DATA' .OR. ILEGUN(ILEG).EQ.'DD  ')THEN
            CALL DPCODS('X',PX1,PX1,IBUGG4,ISUBG4,IERRG4)
            CALL DPCODS('Y',PY1,PY1,IBUGG4,ISUBG4,IERRG4)
          ELSEIF(ILEGUN(ILEG).EQ.'DS  ')THEN
            CALL DPCODS('X',PX1,PX1,IBUGG4,ISUBG4,IERRG4)
          ELSEIF(ILEGUN(ILEG).EQ.'SD  ')THEN
            CALL DPCODS('Y',PY1,PY1,IBUGG4,ISUBG4,IERRG4)
          ENDIF
C
C         SEPTEMBER, 1987  SET LEGEND THICKNESS
CCCCC     PTHICK=PLEGTH(ILEG)
C
          PTHICK=PLEGTH(INDX)
C
          ISTART=ILEGST(INDX)
          ISTOP=ILEGSP(INDX)
C
          NCTEXT=ISTOP-ISTART+1
          IF(NCTEXT.LE.0)GOTO1600
          J=0
          DO1610I=ISTART,ISTOP
            J=J+1
            ICTEXT(J)=ILEGTE(I)
 1610     CONTINUE
          IF(NCTEXT.GE.1)THEN
            CALL DPREPL(ICTEXT,NCTEXT,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1                  IBUGG4,IERRG4)
          ENDIF
          IF(ILEGUN(ILEG).EQ.'SCRE' .OR. ILEGUN(ILEG).EQ.'SS  ')THEN
            IF(PX1.LT.0.0.OR.PX1.GT.100.0)GOTO1600
            IF(PY1.LT.0.0.OR.PY1.GT.100.0)GOTO1600
          ELSEIF(ILEGUN(ILEG).EQ.'SD  ')THEN
            IF(PX1.LT.0.0.OR.PX1.GT.100.0)GOTO1600
          ELSEIF(ILEGUN(ILEG).EQ.'DS  ')THEN
            IF(PY1.LT.0.0.OR.PY1.GT.100.0)GOTO1600
          ENDIF
          CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1                IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1                ISYMBL,ISPAC,
     1                IMPSW2,AMPSCH,AMPSCW,
     1                PX99,PY99)
C
 1600   CONTINUE
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGG4.EQ.'ON' .OR. ISUBG4.EQ.'WRLE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWRLE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IERRG4
 9015   FORMAT('IERRG4 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRSG(ISUBN0,TYPE,IREPCH,IMPLSW,IFLAG,ICAPNM,ICAPBX,
     1                  ILINE)
C
C     PURPOSE--WRITE OUT THE NCOUT ELEMENTS OF THE
C              CHARACTER*240 STRING ICOUT(.:.)
C              TO A GENERAL GRAPHICS DEVICE.
C     NOTE   --THIS IS A MODIFIED VERSION OF DPWRST.  IT
C              IS USED TO PRINT THE TEXT OUTPUT ON THE GRAPHICS
C              DEVICES USING THE TEXT COMMAND (CAN"T CALL DPWRST
C              DIRECTLY SINCE THIS LEADS TO RECURSION, WHICH IS
C              NOT ALLOWED.
C
C              THE VALUE OF THE VARIABLE    NCOUT
C     ICOUT AND NCOUT RESIDE IN COMMON   /TEXTOU/
C     INPUT ARGUMENTS--ICOUT (IN COMMON)
C     ISUBN0 = 6-CHARACTER NAME OF SUBROUTINE WHICH CALLED DPWRST.
C              (AND THEREBY HAVE WALKBACK INFORMATION).
C     TYPE--4 CHARACTER DEFINITION OF TYPE OF INPUT
C              1) TEXT
C              2) BUG
C              3) ERRO
C              4) LIST
C              5) HELP
C              6) WRIT (= ALWAYS WRITE EVEN IF FEEDBACK OFF)
C              7) ...
C     OUTPUT ARGUMENTS--NCOUT (DETERMINED HEREIN)
C     NOTE--ALL DATAPLOT TEXT OUTPUT IS FUNNELED THROUGH
C           THIS ONE SUBROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1993.
C     UPDATED            --SEPTEMBER 1993. ALWAYS WRITE IF TYPE = WRIT
C     UPDATED            --SEPTEMBER 1993. OMIT IBUGG4 AS BUG SWITCH
C     UPDATED            --JUNE      2002. SUPPORT FOR A
C                                          "CAPTURE GRAPHICS" OPTION.
C                                          THIS WRITES TEXT OUTPUT
C                                          TO GRAPHICS UNIT RATHER
C                                          THAN SCREEN.  IMPLEMENT
C                                          VIA "TEXT" COMMAND.
C     UPDATED            --DECEMBER  2018. CALL LIST TO DPTEXT
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
CCCCC MUST EVENTUALLY CHANGE THE FOLLOWING LINE FORM *3 TO *?
      CHARACTER*4 ISUBN0
      CHARACTER*4 TYPE
C
      CHARACTER*4 IBRANC
C
      CHARACTER*4 IFLAG
      CHARACTER*4 ICAPNM
      CHARACTER*4 ICAPBX
      CHARACTER*1 IREPCH
      CHARACTER*4 IMPLSW
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
      CHARACTER*4 IBUGXX
C
      CHARACTER*4 UNITSW
C
      PARAMETER(MAXLEN=130)
      CHARACTER*4 IANST
      CHARACTER*4 IANLCT
      CHARACTER*4 ITXTET
      DIMENSION IANST(MAXLEN)
      DIMENSION IANLCT(MAXLEN)
      DIMENSION ITXTET(MAXLEN)
C
      CHARACTER*4 ITEXCV
      DIMENSION PRV(6)
      DIMENSION PDIARV(4)
      DIMENSION ITEXCV(10)
      DIMENSION PTEXRV(5)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOTR.INC'
CCCCC JUNE 2002.  ADD FOLLOWING LINES.
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
      SAVE PXTEMP
      SAVE PYTEMP
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C
      IF(ISUBG4.EQ.'WRSG')THEN
         WRITE(IPR,999)
  999    FORMAT(1H )
         WRITE(IPR,51)
   51    FORMAT(1H ,'***** AT THE BEGINNING OF DPWRSG--')
         WRITE(IPR,52)ISUBN0
   52    FORMAT(1H ,'THE CALLING ROUTINE (ISUBN0) WAS ',A4)
         WRITE(IPR,53)TYPE
   53    FORMAT(1H ,'TYPE = ',A4)
         WRITE(IPR,55)IFEEDB,IHOST1
   55    FORMAT(1H ,'IFEEDB,IHOST1 = ',A4,2X,A4)
         WRITE(IPR,56)NCOUT,ILOUT
   56    FORMAT(1H ,'NCOUT,ILOUT = ',2I8)
         WRITE(IPR,61)
   61    FORMAT(1H ,'          123456789.123456789.123456789.123456')
         WRITE(IPR,62)ICOUT(1:40)
   62    FORMAT(1H ,'ICOUT = ',40A1)
         WRITE(IPR,63)ICOUT
   63    FORMAT(1H ,'ICOUT = ',A230)
         WRITE(IPR,65)ICAPTY
   65    FORMAT(1H ,'ICAPTY = ',A4)
      ENDIF
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  DETERMINE THE LENGTH OF THE STRING          **
C               **  (BY IGNORING BLANK CHARACTERS AT THE END)   **
C               **************************************************
C
         DO1200I=1,240
            J=240-I+1
            IF(ICOUT(J:J).NE.' ')GOTO1250
 1200    CONTINUE
         NCOUT=1
         GOTO1290
 1250    CONTINUE
         NCOUT=J
 1290    CONTINUE
C
C               ******************************
C               **  STEP 15--               **
C               **  SEND TO GRAPHICS OUTPUT **
C               **  VIA TEXT COMMAND.       **
C               ******************************
C
      IF((IFLAG.EQ.'INIT'.OR.IFLAG.EQ.'NEW').AND.ICAPBX.EQ.'ON')THEN
        PXSAVE=PXEND
        PYSAVE=PYEND
C
        NUMARG=4
        ARG(1)=PBOXXC(1,1)
        ARG(2)=PBOXYC(1,1)
        ARG(3)=PBOXXC(1,2)
        ARG(4)=PBOXYC(1,2)
        IARG(1)=INT(PBOXXC(1,1))
        IARG(2)=INT(PBOXYC(1,1))
        IARG(3)=INT(PBOXXC(1,2))
        IARG(4)=INT(PBOXYC(1,2))
        IARGT(1)='NUMB'
        IARGT(2)='NUMB'
        IARGT(3)='NUMB'
        IARGT(4)='NUMB'
C 
        IBUGXX='OFF'
        UNITSW='ABSO'
        CALL DPBX(IHARG,IARGT,ARG,NUMARG,
     1            PXSTAR,PYSTAR,
     1            PXEND,PYEND,
     1            IBOBPA,IBOBCO,PBOPTH,
     1            AREGBA,
     1            IREBLI,IREBCO,PREBTH,
     1            IBOFPA,IBOFCO,
     1            IBOFPA,IBOPPA,IBOFCO,PBOFTH,PBOPGA,
     1            PBOSHE,PBOSWI,
     1            PTEXHE,PTEXWI,PTEXVG,PTEXHG,
     1            IGRASW,IDIASW,
     1            PGRAXF,PGRAYF,PDIAXC,PDIAYC,PDIAX2,PDIAY2,
     1            PDIAHE,PDIAWI,PDIAVG,PDIAHG,
     1            NUMDEV,
     1            IDMANU,IDMODE,IDMOD2,IDMOD3,
     1            IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1            IDNVOF,IDNHOF,IDFONT,UNITSW,PDSCAL,
     1            IBUGXX,IFOUND,IERROR)
C
        PXEND=PXSAVE
        PYEND=PYSAVE
      ENDIF
C
      DO1510I=1,MAXLEN
        IANST(I)='    '
        IANLCT(I)='    '
        ITXTET(I)='    '
 1510 CONTINUE
C
      IANST(1)='T'
      IANST(2)='E'
      IANST(3)='X'
      IANST(4)='T'
      IANST(5)=' '
      IFACT=5
C
      IF(ICAPNM.EQ.'ON')THEN
        IF(ILINE.LE.9)THEN
          WRITE(IANST(6)(1:1),'(I1)')ILINE
          IFACT=6
        ELSEIF(ILINE.LE.99)THEN
          IJUNK=INT(ILINE/10)
          WRITE(IANST(6)(1:1),'(I1)')IJUNK
          IJUNK=MOD(ILINE,10)
          WRITE(IANST(7)(1:1),'(I1)')IJUNK
          IFACT=7
        ENDIF
      ENDIF
C
      DO1520I=1,NCOUT
        J=I+IFACT
        IANST(J)(1:1)=ICOUT(I:I)
        ITXTET(I)(1:1)=ICOUT(I:I)
 1520 CONTINUE
      IWDTHT=NCOUT+IFACT
      NCTEX=NCOUT
C
      DO1530I=1,MAXLEN
        IANLCT(I)=IANST(I)
 1530 CONTINUE
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IFLAG.EQ.'INIT')THEN
        PXSTAR=PXEND
        PYSTAR=PYEND
        PXTEMP=PXSTAR
        PYTEMP=PYSTAR
      ELSEIF(IFLAG.EQ.'NEW')THEN
        PXSTAR=PXTEMP
        PYSTAR=PYTEMP
      ELSE
        PXSTAR=PXEND
        PYSTAR=PYEND
      ENDIF
C
      PRV(1)=PGRAXF
      PRV(2)=PGRAYF
      PRV(3)=PDIAXC
      PRV(4)=PDIAYC
      PRV(5)=PDIAX2
      PRV(6)=PDIAY2
C
      PDIARV(1)=PDIAHE
      PDIARV(2)=PDIAWI
      PDIARV(3)=PDIAVG
      PDIARV(4)=PDIAHG
C
      ITEXCV(1)=ITEXFO
      ITEXCV(2)=ITEXCA
      ITEXCV(3)=ITEXJU
      ITEXCV(4)=ITEXDI
      ITEXCV(5)=ITEXCR
      ITEXCV(6)=ITEXLF
      ITEXCV(7)=ITEXSY
      ITEXCV(8)=ITEXSP
      ITEXCV(9)=ITEXFI
      ITEXCV(10)=ITEXCO
C
      PTEXRV(1)=PTEXHE
      PTEXRV(2)=PTEXWI
      PTEXRV(3)=PTEXVG
      PTEXRV(4)=PTEXHG
      PTEXRV(5)=PTEXTH
C
      IBUGXX='OFF'
      CALL DPTEXT(IANST,IANLCT,IWDTHT,
     1            ITXTET,NCTEX,
     1            PXSTAR,PYSTAR,PXEND,PYEND,
     1            IGRASW,IDIASW,PRV,PDIARV,
     1            ILINPA,ILINCO,PLINTH,
     1            ATEXBA,ITEBLI,ITEBCO,PTEBTH,
     1            ITEFSW,ITEFCO,
     1            ITEPTY,ITEPLI,ITEPCO,PTEPTH,PTEPSP,
     1            PTEXMR,ITEXCV,ATEXAN,PTEXRV,
     1            IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1            IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1            NUMDEV,IDMANU,IDMODE,IDMOD2,IDMOD3,
     1            IDPOWE,IDCONT,IDCOLO,IDNVPP,IDNHPP,IDUNIT,
     1            IDNVOF,IDNHOF,IDFONT,PDSCAL,
     1            IMPLSW,AMPSCH,AMPSCW,
     1            IBUGXX,IFOUND,IERROR)
CCCCC PXEND=PTEXMR
CCCCC PYEND=PYSTAR-PTEXHE-PTEXVG
C
      IF(ISUBG4.EQ.'WRSG')THEN
        WRITE(IPR,1591)
 1591   FORMAT(1H ,'***** AFTER CALL TO DPTEXT')
        WRITE(IPR,1593)IFOUND,IERROR
 1593   FORMAT(1H ,'IFOUND,IERROR,NCOUT = ',A4,2X,A4,2X,I4)
      ENDIF
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(ISUBG4.EQ.'WRSG')THEN
         WRITE(IPR,999)
         WRITE(IPR,9011)
 9011    FORMAT(1H ,'***** AT THE END       OF DPWRST--')
         WRITE(IPR,9012)ISUBN0
 9012    FORMAT(1H ,'THE CALLING ROUTINE (ISUBN0) WAS ',A3)
         WRITE(IPR,9013)TYPE
 9013    FORMAT(1H ,'TYPE = ',A4)
         WRITE(IPR,9015)IFEEDB,IHOST1
 9015    FORMAT(1H ,'IFEEDB,IHOST1 = ',A4,2X,A4)
         WRITE(IPR,9016)NCOUT,ILOUT
 9016    FORMAT(1H ,'NCOUT,ILOUT = ',2I8)
         WRITE(IPR,9021)
 9021    FORMAT(1H ,'          123456789.123456789.123456789.123456')
         WRITE(IPR,9022)ICOUT(1:40)
 9022    FORMAT(1H ,'ICOUT = ',40A1)
         WRITE(IPR,9023)ICOUT
 9023    FORMAT(1H ,'ICOUT = ',A230)
C
         WRITE(IPR,9032)IBRANC
 9032    FORMAT(1H ,'IBRANC = ',A4)
         WRITE(IPR,9034)NCOUT
 9034    FORMAT(1H ,'NCOUT = ',I8)
         IF(NCOUT.LE.0)GOTO9037
         IF(NCOUT.LE.0)GOTO9037
         DO9035I=1,NCOUT
CCCCC    IASCNE=ICHAR(ICOUT(I:I))
         CALL DPCOAN(ICOUT(I:I),IASCNE)
         WRITE(IPR,9036)I,ICOUT(I:I),IASCNE
 9036    FORMAT(1H ,'I,ICOUT(I:I),IASCNE = ',I8,2X,A1,I8)
 9035    CONTINUE
 9037    CONTINUE
         WRITE(IPR,9039)IBUGG4,ISUBG4,IERRG4
 9039    FORMAT(1H ,'IBUGG4,ISUBG4,IERRG4 = ',A4,2X,A4,2X,A4)
         IF(NUMTRA.GE.1)THEN
           CONTINUE
         ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1                  IFONT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1                  PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1                  ISYMBL,ISPAC,
     1                  IMPSW2,AMPSCH,AMPSCW,
     1                  PX99,PY99)
C
C     PURPOSE--FOR A GENERAL GRAPHICS DEVICE, WRITE A GENERAL TEXT
C              STRING WITH SPECIFIED FONT, CASE, JUSTIFICATION,
C              DIRECTION, FILL, COLOR, CHARACTER HEIGHT, WIDTH,
C              VERTICAL GAP, HORIZONTAL GAP, AND THICKNESS.
C
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     UPDATED  --JANUARY     1989. CHECK FOR UPPER & LOWER CASE SHIFTS
C                                  WHEN HARDWARE CHAR USED (ALAN)
C     UPDATED  --MARCH       1993. STRIP SP()OUT OF HARDWARE TEXT
C                                  VIA CALL TO GRSTRI.
C     UPDATED  --AUGUST      1993. CHECK FOR CASE LOWER FOR HARDWARE
C                                  TEXT
C     UPDATED  --OCTOBER     1993. UPPER, LOWER, ASIS CASE
C     UPDATED  --MAY         1995. ICTEXT BEING CHANGED CAUSES PROBLEM
C                                  WITH TEXT (WHICH LOOPS THROUGH DEVICE)
C     UPDATED  --SEPTEMBER   1999. ARGUMENT LIST TO GRWRTE
C     UPDATED  --NOVEMBER    1999. CONVERT SP() TO HARD SPACE (BUG
C                                  FOR SIMPLEX FONT)
C     UPDATED  --NOVEMBER    1999. SUPPORT CR() FOR MULTIPLE LINES
C                                  (I.E., LOOP THROUGH STRING IF
C                                  PRESENT)
C     UPDATED  --MARCH       2001. WHEN CHECK FOR SP(), NEED TO
C                                  CHECK THAT IT IS NOT IN FACT
C                                  UNSP() (WHICH TERMINATES SUPER
C                                  SCRIPTING)
C     UPDATED  --AUGUST      2012. ICTEXT IS *16 FROM DPWRTE, *4
C                                  ELSEWHERE, SO MAKE IT *(*) TO
C                                  ACCOMODATE BOTH CASES
C     UPDATED  --FEBRUARY    2019. SUPPORT FOR UP TO 10 TAB POSITIONS
C                                  (IDENTIFIED WITH TAB() )
C     UPDATED  --FEBRUARY    2019. RESTRUCTURE TO BETTER HANDLE CR() AND
C                                  TAB() SEQUENCES
C
C-----NON-COMMON VARIABLES (GRAPHICS)-----------------------------------
C
      CHARACTER*4 ICTEXT(*)
C
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILL
      CHARACTER*4 ICOL
C
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
      CHARACTER*4 IMPSW2
C
      CHARACTER*4 IPATT
      CHARACTER*4 ITYPE
C
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
      CHARACTER*4 ICASET
      CHARACTER*4 IFONTT
      CHARACTER*1 IVALC
C
CCCCC MAY 1995.  ADD FOLLOWING 3 LINES
      PARAMETER (NMAX=300)
      CHARACTER*4 ICTEX2
      CHARACTER*4 ICTEX3
      DIMENSION ICTEX2(NMAX)
      DIMENSION ICTEX3(NMAX)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)PX1,PY1,PX99,PY99
   53   FORMAT('PX1,PY1,PX99,PY99 = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NCTEXT
   55   FORMAT('NCTEXT = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)(ICTEXT(I),I=1,MIN(25,NCTEXT))
   56   FORMAT('(ICTEXT(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)IFONT,JFONT,ICASE,JCASE
   60   FORMAT('IFONT,JFONT,ICASE,JCASE = ',A4,I8,2X,A4,I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IJUST,IDIR,IFILL,ICOL
   62   FORMAT('IJUST,IDIR,IFILL,ICOL= ',3(A4,1X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)ANGLE,PTHICK,PHEIGH,PWIDTH,PVEGAP,PHOGAP
   64   FORMAT('ANGLE,PTHICK,PHEIGH,PWIDTH,PVEGAP,PHOGAP = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ISYMBL,ISPAC
   72   FORMAT('ISYMBL,ISPAC = ',A24,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,79)IBUGG4,ISUBG4,IERRG4
   79   FORMAT('IBUGG4,ISUBG4,IERRG4 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC 2019/02: CODE RESTRUCTURED TO BETTER HANDLE CR() AND TAB().
CCCCC          PERFORM CASE CONVERSION AND CONVERTING OF SP() TO
CCCCC          HARD SPACE HERE.
CCCCC
CCCCC          ALSO CHECK FOR LC(), UC().  NOTE THAT THESE
CCCCC          OVERRIDE VALUE OF CASE COMMAND.
C
      NCSAVE=NCTEXT
C
      IF(NCTEXT.LE.0)GOTO9000
C
      ICASET=ICASE
      DO111I=1,NMAX
        ICTEX2(I)=' '
        ICTEX3(I)=' '
  111 CONTINUE
C
      ISKIP=0
      J=0
      DO112I=1,NCTEXT
C
        IF(ISKIP.GT.0)THEN
          ISKIP=ISKIP-1
          GOTO112
        ENDIF
C
C       CHECK FOR:
C
C          1.  "SP()" AND CONVERT TO SINGLE SPACE
C          2.  CASE CONVERSION HERE
C          3.  LC(), UC()
C
        IF(I+3.LE.NCTEXT .AND.
     1     (ICTEXT(I).EQ.'S'.OR.ICTEXT(I).EQ.'s').AND.
     1     (ICTEXT(I+1).EQ.'P'.OR.ICTEXT(I+1).EQ.'p').AND.
     1     ICTEXT(I+2).EQ.'('.AND.
     1     ICTEXT(I+3).EQ.')')THEN
            IF(I.GE.3 .AND.
     1            (ICTEXT(I-2).EQ.'U'.OR.ICTEXT(I-2).EQ.'u').AND.
     1            (ICTEXT(I-1).EQ.'N'.OR.ICTEXT(I-1).EQ.'n'))THEN
              GOTO113
            ENDIF
C
            J=J+1
            ICTEX3(J)=' '
            ISKIP=3
            GOTO112
        ELSEIF(I+3.LE.NCTEXT .AND.
     1     (ICTEXT(I).EQ.'L'.OR.ICTEXT(I).EQ.'l').AND.
     1     (ICTEXT(I+1).EQ.'C'.OR.ICTEXT(I+1).EQ.'c').AND.
     1     ICTEXT(I+2).EQ.'('.AND.
     1     ICTEXT(I+3).EQ.')')THEN
            ICASET='LOWE'
            ISKIP=3
            GOTO112
        ELSEIF(I+3.LE.NCTEXT .AND.
     1     (ICTEXT(I).EQ.'U'.OR.ICTEXT(I).EQ.'u').AND.
     1     (ICTEXT(I+1).EQ.'C'.OR.ICTEXT(I+1).EQ.'c').AND.
     1     ICTEXT(I+2).EQ.'('.AND.
     1     ICTEXT(I+3).EQ.')')THEN
            ICASET='UPPE'
            ISKIP=3
            GOTO112
        ENDIF
C
  113   CONTINUE
C
        CALL DPCOAN(ICTEXT(I)(1:1),IVALT)
        IF(ICASET.EQ.'LOWE')THEN
          IF(IVALT.GE.65.AND.IVALT.LE.90)IVALT=IVALT+32
        ELSE IF(ICASET.EQ.'UPPE')THEN
          IF(IVALT.GE.97.AND.IVALT.LE.122)IVALT=IVALT-32
        ENDIF
        CALL DPCONA(IVALT,IVALC)
        J=J+1
        ICTEX3(J)='    '
        ICTEX3(J)(1:1)=IVALC
C
  112 CONTINUE
C
      NCTEXT=J
      DO115I=1,24
        ISYMBL(I:I)=ICTEX3(I)(1:1)
 115  CONTINUE
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
        DO116,II=1,NCTEXT
          WRITE(ICOUT,118)II,ICTEXT(II),ICTEX3(II)
  118     FORMAT('II,ICTEXT(II),ICTEX3(II) = ',I5,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
  116  CONTINUE
      ENDIF
C
      J=0
      ISKIP=0
      ICRFLG=0
      ITBFLG=0
      ISPFLG=0
      ISBFLG=0
      NLINE=0
      NTAB=0
      ICNT=0
      IFLAGN=0
C
      DO121I=1,NCTEXT
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
          WRITE(ICOUT,122)I,ISKIP,J,ICTEX3(I)
  122     FORMAT('I,ISKIP,J,ICTEX3(I) = ',3I5,2X,A4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(ISKIP.GT.0)THEN
          ISKIP=ISKIP-1
          GOTO121
        ENDIF
C
        IF(I+3.LE.NCTEXT .AND.
     1     (ICTEX3(I).EQ.'C'.OR.ICTEX3(I).EQ.'c').AND.
     1     (ICTEX3(I+1).EQ.'R'.OR.ICTEX3(I+1).EQ.'r').AND.
     1     ICTEX3(I+2).EQ.'('.AND.
     1     ICTEX3(I+3).EQ.')')THEN
             ICRFLG=1
             NLINE=NLINE+1
             ISKIP=3
             GOTO125
        ELSEIF(I+3.LE.NCTEXT .AND.
     1     (ICTEX3(I).EQ.'T'.OR.ICTEX3(I).EQ.'t').AND.
     1     (ICTEX3(I+1).EQ.'A'.OR.ICTEX3(I+1).EQ.'a').AND.
     1     (ICTEX3(I+2).EQ.'B'.OR.ICTEX3(I+2).EQ.'b').AND.
     1     ICTEX3(I+3).EQ.'('.AND.
     1     ICTEX3(I+4).EQ.')')THEN
             ITBFLG=1
             ISKIP=4
             NTAB=NTAB+1
             IF(NTAB.GT.11)NTAB=2
             IFLAGN=1
             GOTO125
        ELSEIF(I+4.LE.NCTEXT .AND.
     1     (ICTEX3(I).EQ.'S'.OR.ICTEX3(I).EQ.'s').AND.
     1     (ICTEX3(I+1).EQ.'U'.OR.ICTEX3(I+1).EQ.'u').AND.
     1     (ICTEX3(I+2).EQ.'B'.OR.ICTEX3(I+2).EQ.'b').AND.
     1     ICTEX3(I+3).EQ.'('.AND.
     1     ICTEX3(I+4).EQ.')')THEN
             ISBFLG=1
        ELSEIF(I+4.LE.NCTEXT .AND.
     1     (ICTEX3(I).EQ.'S'.OR.ICTEX3(I).EQ.'s').AND.
     1     (ICTEX3(I+1).EQ.'U'.OR.ICTEX3(I+1).EQ.'u').AND.
     1     (ICTEX3(I+2).EQ.'P'.OR.ICTEX3(I+2).EQ.'p').AND.
     1     ICTEX3(I+3).EQ.'('.AND.
     1     ICTEX3(I+4).EQ.')')THEN
             ISPFLG=1
        ENDIF
C
        J=J+1
        ICTEX2(J)=ICTEX3(I)
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
          WRITE(ICOUT,128)I,J,ICTEX3(I),ICTEX2(J)
  128     FORMAT('BEFORE 125: I,J,ICTEX3(I),ICTEX2(J) = ',
     1           2I5,2(2X,A4))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C
  125   CONTINUE
C
C       IF TAB, CARRIAGE RETURN, OR LAST CHARACTER IN LINE FOUND,
C       THEN PRINT CURRENT STRING.
C
C       NOTE THAT SOME ATTRIBUTE SETTINGS ONLY NEED TO BE DONE
C       ONCE WHILE OTHERS MAY NEED TO BE DONE FOR EACH NEW
C       LINE.
C
        IF((I.GE.NCTEXT .OR. ICRFLG.EQ.1 .OR. ITBFLG.EQ.1) .AND.
     1      J.GE.1)THEN
C
          ICNT=ICNT+1
          IFLAGT=0
          IF(I.GE.NCTEXT .AND. NTAB.GE.1)THEN
            ITBFLG=1
            NTAB=NTAB+1
          ELSEIF(ICRFLG.EQ.1 .AND. NTAB.GE.1)THEN
            ITBFLG=1
            IFLAGT=1
            NTAB=NTAB+1
          ENDIF
          IF(I.GE.NCTEXT)THEN
            IF(NLINE.GE.1)NLINE=NLINE+1
          ENDIF
          ITYPE='LINE'
C
          IF(ICNT.EQ.1)THEN
C
C               **********************************************
C               **  STEP 1--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE LINE TYPE (= SOLID) INTO A       **
C               **  NUMERIC REPRESENTATION WHICH CAN BE     **
C               **  UNDERSTOOD BY THE GRAPHICS DEVICE.      **
C               **********************************************
C
            IPATT='SOLI'
            CALL GRTRPA(ITYPE,IPATT,PXSPA,PYSPA,
     1                  JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1                  PXSPA2,PYSPA2)
C
C               ****************************************************
C               **  STEP 2--                                      **
C               **  SET THE PATTERN TYPE ON THE GRAPHICS DEVICE.  **
C               ****************************************************
C
            CALL GRSEPA(ITYPE,IPATT,PXSPA,PYSPA,
     1                  JPATT,IHORPA,IVERPA,IDUPPA,IDDOPA,
     1                  PXSPA2,PYSPA2)
C
            ITYPE='TEXT'
C
C               ****************************************************
C               **  STEP 3--                                      **
C               **  TRANSLATE THE CHARACTER REPRESENTATION OF THE **
C               **  CASE TYPE (UPPER/LOWER) INTO A NUMERIC        **
C               **  REPRESENTATION WHICH CAN BE UNDERSTOOD BY THE **
C               **  GRAPHICS DEVICE.                              **
C               ****************************************************
C
            CALL GRTRCA(ITYPE,ICASE,JCASE)
C
C               *************************************************
C               **  STEP 4--                                   **
C               **  SET THE CASE TYPE ON THE GRAPHICS DEVICE.  **
C               *************************************************
C
            CALL GRSECA(ITYPE,ICASE,JCASE)
C
C               ******************************************************
C               **  STEP 7--                                        **
C               **  TRANSLATE THE CHARACTER REPRESENTATION OF THE   **
C               **  TEXT DIRECTION INTO A NUMERIC REPRESENTATION    **
C               **  WHICH CAN BE UNDERSTOOD BY THE GRAPHICS DEVICE. **
C               ******************************************************
C
            CALL GRTRDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
C               ******************************************************
C               **  STEP 8--                                        **
C               **  SET THE TEXT DIRECTION ON THE GRAPHICS DEVICE.  **
C               ******************************************************
C
            CALL GRSEDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
C               *******************************************************
C               **  STEP 9--                                         **
C               **  TRANSLATE THE CHARACTER REPRESENTATION OF THE    **
C               **  TEXT FILL (ON/OFF) INTO A NUMERIC REPRESENTATION **
C               **  WHICH CAN BE UNDERSTOOD BY THE GRAPHICS DEVICE.  **
C               *******************************************************
C
            CALL GRTRFI(ITYPE,IFILL,JFILL)
C
C               *************************************************
C               **  STEP 10--                                  **
C               **  SET THE TEXT FILL ON THE GRAPHICS DEVICE.  **
C               *************************************************
C
            CALL GRSEFI(ITYPE,IFILL,JFILL)
C
C               ******************************************************
C               **  STEP 15--                                       **
C               **  TRANSLATE THE CHARACTER REPRESENTATION OF THE   **
C               **  TEXT THICKNESS INTO A NUMERIC REPRESENTATION    **
C               **  WHICH CAN BE UNDERSTOOD BY THE GRAPHICS DEVICE. **
C               ******************************************************
C
            CALL GRTRTH(ITYPE,PTHICK,JTHICK,PTHIC2)
C
C               *****************************************************
C               **  STEP 16--                                      **
C               **  SET THE TEXT THICKNESS ON THE GRAPHICS DEVICE. **
C               *****************************************************
C
            CALL GRSETH(ITYPE,PTHICK,JTHICK,PTHIC2)
C
          ENDIF
C
          ITYPE='TEXT'
C
C               ****************************************************
C               **  STEP 1--                                      **
C               **  FONT TYPE                                     **
C               ****************************************************
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       ITABFO(NTAB-1).NE.'NULL')THEN
            IFONTT=ITABFO(NTAB-1)
            CALL GRTRFO(ITYPE,ITABFO(NTAB-1),JFONT)
            CALL GRSEFO(ITYPE,ITABFO(NTAB-1),JFONT)
          ELSE
            IFONTT=IFONT
            CALL GRTRFO(ITYPE,IFONT,JFONT)
          ENDIF
C
C               *******************************************************
C               **  STEP 5--                                         **
C               **  TEXT JUSTIFICATION                               **
C               *******************************************************
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       ITABJU(NTAB-1).NE.'NULL')THEN
            CALL GRTRJU(ITYPE,ITABJU(NTAB-1),JJUST)
            CALL GRSEJU(ITYPE,ITABJU(NTAB-1),JJUST)
          ELSE
            CALL GRTRJU(ITYPE,IJUST,JJUST)
            CALL GRSEJU(ITYPE,IJUST,JJUST)
          ENDIF
C
C               *****************************************************
C               **  STEP 11--                                      **
C               **  TEXT COLOR                                     **
C               *****************************************************
C
          ITYPE='TEXT'
          IF(IFONT.NE.'TEKT')ITYPE='LINE'
          IF(IDIR.NE.'HORI'.AND.IDIR.NE.'VERT')ITYPE='LINE'
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       ITABCO(NTAB-1).NE.'NULL')THEN
            CALL GRTRCO(ITYPE,ITABCO(NTAB-1),JCOL)
            CALL GRSECO(ITYPE,ITABCO(NTAB-1),JCOL)
          ELSE
            CALL GRTRCO(ITYPE,ICOL,JCOL)
            CALL GRSECO(ITYPE,ICOL,JCOL)
          ENDIF
          ITYPE='TEXT'
C
C               *****************************************************
C               **  STEP 13--                                      **
C               **  TEXT SIZE                                      **
C               *****************************************************
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       PTABHE(NTAB-1).GT.0.0)THEN
            PHEIGT=PTABHE(NTAB-1)
            PWIDTT=PWIDTH
            IF(PTABWI(NTAB-1).GT.0.0)PWIDTT=PTABWI(NTAB-1)
            CALL GRTRSI(ITYPE,IFONTT,PHEIGT,PWIDTT,PVEGAP,PHOGAP,
     1                  JSIZE,
     1                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2)
            CALL GRSESI(ITYPE,IFONTT,PHEIGT,PWIDTT,PVEGAP,PHOGAP,
     1                  JSIZE,
     1                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2)
          ELSE
            PHEIGT=PHEIGH
            PWIDTT=PWIDTH
            IF(PTABWI(ITBFLG-1).NE.CPUMIN)PWIDTT=PTABWI(ITBFLG-1)
            CALL GRTRSI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                  JSIZE,
     1                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2)
            CALL GRSESI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                  JSIZE,
     1                  JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                  PHEIG2,PWIDT2,PVEGA2,PHOGA2)
          ENDIF
C
C               ***********************************************
C               **  STEP 21--                                **
C               **  DETERMINE THE LENGTH OF THE TEXT STRING  **
C               ***********************************************
C
          NCTEX2=J
          CALL GRDETL(ICTEX2,NCTEX2,
     1                IFONTT,IDIR,ANGLE,
     1                JFONT,JDIR,ANGLE2,
     1                PHEIGT,PWIDTT,PVEGAP,PHOGAP,
     1                JSIZE,
     1                JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                PXLEC,PXLECG,PYLEC,PYLECG)
C
C               **************************
C               **  STEP 22--           **
C               **  WRITE OUT THE TEXT  **
C               **************************
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       PTABHP(NTAB-1).NE.CPUMIN)THEN
            PXTEMP=PTABHP(NTAB-1)
          ELSE
            PXTEMP=PX1
          ENDIF
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       PTABVP(NTAB-1).NE.CPUMIN)THEN
            PYTEMP=PTABVP(NTAB-1)
            IF(NLINE.GE.1)THEN
              ATEMP=PHEIGH + (PVEGAP + PWRTGA)
              PYTEMP=PYTEMP - REAL(NLINE-1)*ATEMP
            ENDIF
          ELSE
            IF(I.GE.NCTEXT .OR. NLINE.LT.1)THEN
              IF(NLINE.LE.0)THEN
                PYTEMP=PY1
              ELSE
                PYTEMP=PY1
                ATEMP=PHEIGH + (PVEGAP + PWRTGA)
                PYTEMP=PY1 - REAL(NLINE-1)*ATEMP
              ENDIF
            ELSE
              IF(NLINE.LE.0)THEN
                PYTEMP=PY1
              ELSE
                ATEMP=PHEIGH + (PVEGAP + PWRTGA)
                PYTEMP=PY1 - REAL(NLINE-1)*ATEMP
              ENDIF
            ENDIF
          ENDIF
C
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       ITABUN(NTAB-1).EQ.'DATA')THEN
            CALL DPCODS('X',PXTEMP,PXTEMP,IBUGG4,ISUBG4,IERRG4)
          ENDIF
          IF(ITBFLG.EQ.1 .AND. NTAB.GE.2 .AND.
     1       ITABUV(NTAB-1).EQ.'DATA')THEN
            CALL DPCODS('Y',PYTEMP,PYTEMP,IBUGG4,ISUBG4,IERRG4)
          ENDIF
C
          IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
            WRITE(ICOUT,150)I,NCTEXT
  150       FORMAT('AT GRWRTE: I,NCTEXT = ',2I5)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,151)ICRFLG,ITBFLG,NLINE,NTAB,NCTEX2
  151       FORMAT('ICRFLG,ITBFLG,NLINE,NTAB,NCTEX2 = ',5I5)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,152)PX1,PY1,PXTEMP,PYTEMP,PX99,PY99
  152       FORMAT('PX1,PY1,PXTEMP,PYTEMP,PX99,PY99 = ',6G15.7)
            CALL DPWRST('XXX','BUG ')
            DO153JJ=1,NCTEX2
              WRITE(ICOUT,155)JJ,ICTEX2(JJ)
  155         FORMAT('JJ,ICTEX2(JJ) = ',I5,2X,A4)
              CALL DPWRST('XXX','BUG ')
  153       CONTINUE
          ENDIF
C
          CALL GRWRTE(PXTEMP,PYTEMP,ICTEX2,NCTEX2,
     1                IPATT,IFONTT,ICASE,IJUST,IDIR,ANGLE,IFILL,ICOL,
     1                JPATT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILL,JCOL,
     1                PHEIGT,PWIDTT,PVEGAP,PHOGAP,PTHICK,
     1                JSIZE,
     1                JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                JTHICK,PTHIC2,
     1                PXLEC,PXLECG,PYLEC,PYLECG,
     1                ISYMBL,ISPAC,
     1                IMPSW2,AMPSCH,AMPSCW,
     1                PX99,PY99)
C
          IF(ICRFLG.EQ.1)THEN
            IF(IFLAGT.EQ.1)NTAB=NTAB-1
            ICRFLG=0
          ENDIF
C
          IF(ITBFLG.EQ.1)THEN
            IF(IFLAGN.EQ.1)NLINE=0
            IFLAGN=0
            ITBFLG=0
          ENDIF
          J=0
C
        ENDIF
C
  121 CONTINUE
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      NCTEXT=NCSAVE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWRTE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9115)NCTEX2
 9115   FORMAT('NCTEX2 = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9116)(ICTEX2(I),I=1,MIN(NCTEX2,25))
 9116   FORMAT('(ICTEX2(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)ISYMBL,ISPAC
 9035   FORMAT('ISYMBL,ISPAC = ',A24,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWRTL(ICASPL,ICAS3D)
C
C     PURPOSE--WRITE TIC LABELS ON ALL 4 FRAME LINES.
C
C     WRITTEN BY--JAMES J. FILLIBEN
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
C     UPDATED         --JANUARY  1988. ALLOW TIC LABELS WITH NO TICS
C     UPDATED         --JANUARY  1988. ALPHABETIC TIC LABELS
C     UPDATED         --JANUARY  1988. LOG SCALE EXPONENTIAL TIC LABELS
C     UPDATED         --JANUARY  1988. LOG SCALE REAL TIC LABELS
C     UPDATED         --FEBRUARY 1988. STAR PLOT
C     UPDATED         --FEBRUARY 1989. ADDED DPCOPA.INC (ALAN)
C     UPDATED         --MARCH    1993. ADD CALLS TO GRSTRI FOR
C                                      HARDWARE TEXT.
C     UPDATED         --JULY     1997. SUPPORT EXPONENTIAL SCALE FOR
C                                      LINEAR SCALE.
C     UPDATED         --SEPTEMBER1999. ARGUMENT LIST TO GRWRTE
C     UPDATED         --NOVEMBER 1999. FOR ALPHA LABELS, GO THROUGH
C                                      DPWRTE INSTEAD OF GRWRTE
C     UPDATED         --JANUARY  2004. SUPPORT FOR:
C                                      1) ROW LABELS
C                                      2) GROUP LABELS
C                                      3) NUMERIC LABELS
C     UPDATED         --JANUARY  2006. ALLOW VARIABLE, ROWLABEL AND
C                                      GROUP LABELS TO BE
C                                      INDEXED (E.G., USE WITH
C                                      SORT BY MEAN)
C     UPDATED         --DECEMBER 2006. SUPPORT FOR TRILINEAR SCALES
C     UPDATED         --DECEMBER 2008. MAXIMUM NUMBER OF CHARACTERS
C                                      IN A GROUP LABEL NOW SETTABLE
C                                      IN DPCOPA.INC
C
C-----NON-COMMON VARIABLES (GRAPHICS)--------------------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 ICAS3D
      CHARACTER*4 IPATTT
      CHARACTER*4 ITYPE
      CHARACTER*4 IHORPA
      CHARACTER*4 IVERPA
      CHARACTER*4 IDUPPA
      CHARACTER*4 IDDOPA
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 IDIR
      CHARACTER*4 IFILLT
      CHARACTER*4 ICOL
      CHARACTER*24 ISYMBL
      CHARACTER*4 ISPAC
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHTEMP
      CHARACTER*4 IHTEM2
      CHARACTER*4 IHIND
      CHARACTER*4 IHIND2
      CHARACTER*4 IHWUSE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 IERROR
      CHARACTER*4 ICTEXT
      CHARACTER*4 ISUBN0
      CHARACTER*4 ISUBRO
      CHARACTER*4 IENDFI
      CHARACTER*4 IREWIN
      CHARACTER*1 IAJUNK
      CHARACTER*20 IFORMT
C
      CHARACTER*2048 ISTRI2
      CHARACTER*2400 ISTRCC
      CHARACTER*1 IC1
      CHARACTER*4 IC4
      CHARACTER*4 MESSAG
      CHARACTER*10 ICTEMP
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*80 IFILE2
      CHARACTER (LEN=MAXFNC) :: IFILE2
      CHARACTER*12 ISTAT2
      CHARACTER*12 IFORM2
      CHARACTER*12 IACCE2
      CHARACTER*12 IPROT2
      CHARACTER*12 ICURS2
      CHARACTER*4  IREWI2
      CHARACTER*4  IERRF2
C
      DIMENSION ICTEXT(2048)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      MESSAG='OFF'
      ISUBRO='XXXX'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      IFLAGC=0
      ICOLI=0
      IRLIND=0
      IVLIND=0
      IGLIND=0
      IROW1=0
      IGVAR=0
      IROWID=0
      ICOLL=0
      AVALU2=0.0
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWRTL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)PXMIN,PYMIN,PXMAX,PYMAX
   52   FORMAT('PXMIN,PYMIN,PXMAX,PYMAX = ',4F10.5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,ICAS3D,IBUGG4,ISUBG4,IERRG4
   53   FORMAT('ICASPL,ICAS3D,IBUGG4,ISUBG4,IERRG4 = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NX1COO,NX2COO,NY1COO,NY2COO
   54   FORMAT('NX1COO,NX2COO,NY1COO,NY2COO = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW
   55   FORMAT('IX1ZSW,IX2ZSW,IY1ZSW,IY2ZSW = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,56)IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO
   56   FORMAT('IX1ZFO,IX2ZFO,IY1ZFO,IY2ZFO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,57)IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA
   57   FORMAT('IX1ZCA,IX2ZCA,IY1ZCA,IY2ZCA = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,58)IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU
   58   FORMAT('IX1ZJU,IX2ZJU,IY1ZJU,IY2ZJU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS
   59   FORMAT('PX1ZDS,PX2ZDS,PY1ZDS,PY2ZDS = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,60)IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI
   60   FORMAT('IX1ZDI,IX2ZDI,IY1ZDI,IY2ZDI = ',A4,2X,A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,64)AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN
   64   FORMAT('AX1ZAN,AX2ZAN,AY1ZAN,AY2ZAN = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,65)IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI
   65   FORMAT('IX1ZFI,IX2ZFI,IY1ZFI,IY2ZFI = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,66)IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO
   66   FORMAT('IX1ZCO,IX2ZCO,IY1ZCO,IY2ZCO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP
   63   FORMAT('IX1ZDP,IX2ZDP,IY1ZDP,IY2ZDP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,73)PX1ZHE,PX2ZHE,PY1ZHE,PY2ZHE
   73   FORMAT('PX1ZHE,PX2ZHE,PY1ZHE,PY2ZHE = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)PX1ZWI,PX2ZWI,PY1ZWI,PY2ZWI
   74   FORMAT('PX1ZWI,PX2ZWI,PY1ZWI,PY2ZWI = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,75)PX1ZVG,PX2ZVG,PY1ZVG,PY2ZVG
   75   FORMAT('PX1ZVG,PX2ZVG,PY1ZVG,PY2ZVG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,76)PX1ZHG,PX2ZHG,PY1ZHG,PY2ZHG
   76   FORMAT('PX1ZHG,PX2ZHG,PY1ZHG,PY2ZHG = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,77)PTIZTH
   77   FORMAT('PTIZTH = ',E15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,83)IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM
   83   FORMAT('IX1ZFM,IX2ZFM,IY1ZFM,IY2ZFM = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,84)(IX1ZCN(I:I),I=1,100)
   84   FORMAT('(IX1ZCN(I:I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,85)(IX2ZCN(I:I),I=1,100)
   85   FORMAT('(IX2ZCN(I:I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,86)(IY1ZCN(I:I),I=1,100)
   86   FORMAT('(IY1ZCN(I:I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,87)(IY2ZCN(I:I),I=1,100)
   87   FORMAT('(IY2ZCN(I:I) = ',100A1)
        CALL DPWRST('XXX','BUG ')
C
        IF(NX1COO.GT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO61I=1,NX1COO
            WRITE(ICOUT,62)I,PX1COO(I),X1COOR(I)
   62       FORMAT('I,PX1COO(I),X1COOR(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
C
        IF(NX2COO.GT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO71I=1,NX2COO
           WRITE(ICOUT,72)I,PX2COO(I),X2COOR(I)
   72      FORMAT('I,PX2COO(I),X2COOR(I) = ',I8,2G15.7)
           CALL DPWRST('XXX','BUG ')
   71    CONTINUE
        ENDIF
C
        IF(NY1COO.GT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO81I=1,NY1COO
            WRITE(ICOUT,82)I,PY1COO(I),Y1COOR(I)
   82       FORMAT('I,PY1COO(I),Y1COOR(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   81     CONTINUE
        ENDIF
C
        IF(NY2COO.GT.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          DO91I=1,NY2COO
            WRITE(ICOUT,92)I,PY2COO(I),Y2COOR(I)
   92       FORMAT('I,PY2COO(I),Y2COOR(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   91     CONTINUE
        ENDIF
C
C
      ENDIF
C
      IF(ICASPL.EQ.'PIEC')GOTO9000
      IF(ICASPL.EQ.'STAR')GOTO9000
      IF(ICAS3D.EQ.'ON')GOTO9000
C
      ITYPE='LINE'
      ISYMBL=ITEXSY
      ISPAC=ITEXSP
C
C               **********************************************
C               **  STEP 1--                                **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE TIC LABEL TYPE (= SOLID)         **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
 
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8001)
 8001   FORMAT('STEP 1')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IPATTT='SOLI'
      CALL GRTRPA(ICASE,IPATTT,PXSPA,PYSPA,
     1JPATTT,IHORPA,IVERPA,IDUPPA,IDDOPA,PXSPA2,PYSPA2)
C
C               *******************************
C               **  STEP 2--                 **
C               **  SET THE PATTERN TYPE     **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
      CALL GRSEPA(ICASE,IPATTT,PXSPA,PYSPA,
     1JPATTT,IHORPA,IVERPA,IDUPPA,IDDOPA,PXSPA2,PYSPA2)
C
      ITYPE='TEXT'
C
C               **********************************************
C               **  STEP 11--                               **
C               **  TRANSLATE THE CHARACTER REPRESENTATION  **
C               **  OF THE TEXT THICKNESS                   **
C               **  INTO A NUMERIC REPRESENTATION           **
C               **  WHICH CAN BE UNDERSTOOD BY THE          **
C               **  GRAPHICS DEVICE.                        **
C               **********************************************
C
      PTHICK=PTIZTH
      CALL GRTRTH(ITYPE,PTHICK,JTHICK,PTHIC2)
C
C               *******************************
C               **  STEP 12--                **
C               **  SET THE TEXT THICKNESS   **
C               **  ON THE GRAPHICS DEVICE.  **
C               *******************************
C
      CALL GRSETH(ITYPE,PTHICK,JTHICK,PTHIC2)
C
      IF(ICASPL.EQ.'TRPL')GOTO8000
C
C               ********************************************************
C               **  STEP 13--                                         **
C               **  WRITE OUT TIC LABELS ON THE 4 AXES.  THE FIRST    **
C               **  STEP IN EACH OF THE 4 AXES IS TO TRANSLATE THE    **
C               **  CHARACTER REPRESENTATION INTO A NUMERIC           **
C               **  REPRESENTATION OF THE TEXT JUSTIFICATION WHICH    **
C               **  CAN BE UNDERSTOOD BY THE GRAPHICS DEVICE.  THE    **
C               **  SECOND STEP IS TO ACTUALLY SET THE TEXT           **
C               **  JUSTIFICATION.  THE THIRD STEP IN EACH OF THE 4   **
C               **  AXES IS TO TRANSLATE THE CHARACTER REPRESENTATION **
C               **  OF THE TEXT DIRECTION INTO A NUMERIC              **
C               **  REPRESENTATION WHICH CAN BE UNDERSTOOD BY THE     **
C               **  GRAPHICS DEVICE.  THE FOURTH STEP IS TO ACTUALLY  **
C               **  SET THE TEXT DIRECTION.  THE FIFTH STEP IS TO     **
C               **  SPECIFY REFERENCE COORDINATES FOR THE TIC LABEL.  **
C               **  THE SIXTH STEP IS TO WRITE OUT THE TIC LABEL.     **
C               ********************************************************
C
C               ******************************************************
C               **  STEP 21.1--                                     **
C               **  WRITE TIC LABELS     ON BOTTOM HORIZONTAL AXIS  **
C               ******************************************************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8002)
 8002   FORMAT('STEP 21.1')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IX1FSW.EQ.'OFF')GOTO1190
CCCCC IF(IX1TSW.EQ.'OFF')GOTO1190
      IF(IX1ZSW.EQ.'OFF')GOTO1190
      IF(NX1COO.LE.0)GOTO1190
C
      IFONT=IX1ZFO
      CALL GRTRFO(ITYPE,IFONT,JFONT)
      CALL GRSEFO(ITYPE,IFONT,JFONT)
C
      ICASE=IX1ZCA
      CALL GRTRCA(ITYPE,ICASE,JCASE)
      CALL GRSECA(ITYPE,ICASE,JCASE)
C
      IJUST=IX1ZJU
      CALL GRTRJU(ITYPE,IJUST,JJUST)
      CALL GRSEJU(ITYPE,IJUST,JJUST)
C
      IDIR=IX1ZDI
      ANGLE=AX1ZAN
      CALL GRTRDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
      CALL GRSEDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
      IFILLT=IX1ZFI
      CALL GRTRFI(ITYPE,IFILLT,JFILLT)
      CALL GRSEFI(ITYPE,IFILLT,JFILLT)
C
      ICOL=IX1ZCO
      CALL GRTRCO(ITYPE,ICOL,JCOL)
      CALL GRSECO(ITYPE,ICOL,JCOL)
C
      PHEIGH=PX1ZHE
      PWIDTH=PX1ZWI
      PVEGAP=PX1ZVG
      PHOGAP=PX1ZHG
      CALL GRTRSI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
      CALL GRSESI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
C
      PY1=PYMIN-PX1ZDS
CCCCC PY1=PY1-PHEIG2
C
      ISTART=1
CCCCC ISTOP=130
      ISTOP=2048
      NCOLMX=2048
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8003)
 8003   FORMAT('STEP 21.1: AFTER ATTRIBUTE SETTING ROUTINES')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC JANUARY 2004.  FOR VARIABLE OR GROUP LABEL CASE, NEED
CCCCC TO EXTRACT RELEVANT VARIABLE.
C
CCCCC 2019/10: FOR VARIABLE CASE, ALSO CHECK FOR CHARACTER VARIABLE
C
      IF(IX1ZFM.EQ.'VARI')THEN
C
        IFLAGC=0
        I=1
        CALL DPUPPE(IX1ZCN,ISTOP,IX1ZCN,IBUGG4,IERROR)
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11102)
11102     FORMAT('***** WARNING--FOR X1TIC MARK LABEL FORMAT ',
     1           '"VARIABLE"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11104)
11104     FORMAT('      NO VARIABLE NAME SPECIFIED ON ',
     1           'X1TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
C
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
C
C         2019/10: CHECK TO SEE IF DPZCHF.DAT EXISTS
C
          IOUNI2=IZCHNU
          IFILE2=IZCHNA
          ISTAT2=IZCHST
          IFORM2=IZCHFO
          IACCE2=IZCHAC
          IPROT2=IZCHPR
          ICURS2=IZCHCS
C
          ISUBN0='WRIT'
          IERRF2='NO'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
     1                ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO11101
          IFLAGC=1
          REWIND(IOUNI2)
          READ(IOUNI2,'(I8)',END=11201,ERR=11201)NCVAR
          DO11203KK=1,NCVAR
            READ(IOUNI2,'(2A4)',END=11201,ERR=11201)IHTEMP,IHTEM2
            IF(IHTEMP.EQ.IH .AND. IHTEM2.EQ.IH2)THEN
              IROWID=KK
              GOTO11109
            ENDIF
11203     CONTINUE
C
11201     CONTINUE
          IENDFI='OFF'
          IREWIN='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDFI,IREWIN,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IZCHCS='CLOSED'
C
11101     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11106)IH,IH2
11106     FORMAT('***** WARNING--UNABLE TO DETERMINE VARIABLE ',
     1           2A4,' FOR X1TIC MARK LABELS.')
          CALL DPWRST('XXX','BUG ')
          IF(IFLAGC.EQ.1)GOTO9000
          GOTO1190
        ENDIF
        ICOLL=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
C
11109   CONTINUE
C
C  1/2006: IF VARIABLE LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IVLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11116)IHIND,IHIND2
11116       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1            'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11117)
11117       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(VARIABLE FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1190
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IVLIND=1
        ENDIF
      ELSEIF(IX1ZFM.EQ.'GLAB')THEN
        CALL DPUPPE(IX1ZCN,ISTOP,IX1ZCN,IBUGG4,IERROR)
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11122)
11122     FORMAT('***** WARNING--FOR X1TIC MARK LABEL FORMAT ',
     1           '"GROUP LABEL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,11124)
11124     FORMAT('      NO GROUP LABEL VARIABLE NAME SPECIFIED ON ',
     1           'X1TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
          GOTO1190
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IGVAR=0
        DO11120I=1,MAXGRP
          IF(IH(1:4).EQ.IGRPVN(I)(1:4) .AND.
     1       IH2(1:4).EQ.IGRPVN(I)(5:8))THEN
            IGVAR=I
            GOTO11129
          ENDIF
11120   CONTINUE
11129   CONTINUE
C
C  1/2006: IF GROUP LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IGLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11136)IHIND,IHIND2
11136       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11137)
11137       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(GROUP LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1190
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IGLIND=1
        ENDIF
C
      ELSEIF(IX1ZFM.EQ.'ROWL')THEN
C
C  1/2006: IF ROW LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
C  4/2017: IF ROW LABEL SPECIFIED, THEN CHECK TO SEE IF
C          START AND STOP ROWS ALSO SPECIFIED.
C
        CALL DPUPPE(IX1ZCN,ISTOP,IX1ZCN,IBUGG4,IERROR)
        IRLIND=0
C
C       CHECK FOR INDEX VARIABLE
C
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          IERROR='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11138)IHIND,IHIND2
11138       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,11139)
11139       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(ROW LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1190
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IRLIND=1
        ENDIF
C
C       CHECK FOR STARTING ROW
C
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='P'
          MESSAG='NO'
          IERROR='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            IROW1=1
          ELSE
            IROW1=INT(VALUE(ILOCV)+0.01)
            IF(IROW1.LT.1)IROW1=1
            IF(IROW1.GT.MAXOBV)IROW1=1
          ENDIF
        ENDIF
C
      ENDIF
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8004)
 8004   FORMAT('STEP 21.1: BEFORE DO1100')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO1100I=1,NX1COO
C
        PX1=PX1COO(I)
        IF(IX1ZFM.EQ.'VARI')THEN
          IF(IVLIND.EQ.1)THEN
            IJ=MAXN*(ICOLI-1)+I
            IF(ICOLI.LE.MAXCOL)AVALU2=V(IJ)
            IF(ICOLI.EQ.MAXCP1)AVALU2=PRED(I)
            IF(ICOLI.EQ.MAXCP2)AVALU2=RES(I)
            IF(ICOLI.EQ.MAXCP3)AVALU2=YPLOT(I)
            IF(ICOLI.EQ.MAXCP4)AVALU2=XPLOT(I)
            IF(ICOLI.EQ.MAXCP5)AVALU2=X2PLOT(I)
            IF(ICOLI.EQ.MAXCP6)AVALU2=TAGPLO(I)
            INDX=INT(AVALU2+0.5)
            IF(INDX.LT.1 .OR. INDX.GT.NX1COO)THEN
              INDX=I
            ENDIF
          ELSE
            INDX=I
          ENDIF
C
          IF(IFLAGC.EQ.0)THEN
            IJ=MAXN*(ICOLL-1)+INDX
            IF(ICOLL.LE.MAXCOL)AVALUE=V(IJ)
            IF(ICOLL.EQ.MAXCP1)AVALUE=PRED(INDX)
            IF(ICOLL.EQ.MAXCP2)AVALUE=RES(INDX)
            IF(ICOLL.EQ.MAXCP3)AVALUE=YPLOT(INDX)
            IF(ICOLL.EQ.MAXCP4)AVALUE=XPLOT(INDX)
            IF(ICOLL.EQ.MAXCP5)AVALUE=X2PLOT(INDX)
            IF(ICOLL.EQ.MAXCP6)AVALUE=TAGPLO(INDX)
            IVALU9=INT(AVALUE+0.5)
            IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
          ELSE
            IVALU9=INDX
          ENDIF
        ELSEIF(IX1ZFM.EQ.'GLAB' .AND. IGLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(I)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(I)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(I)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(I)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(I)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(I)
          IVALU9=INT(AVALUE+0.5)
        ELSEIF(IX1ZFM.EQ.'ROWL' .AND. IRLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IJ=IJ+IROW1-1
          IF(IJ.LT.1)IJ=1
          IF(IJ.GT.MAXOBV)IJ=MAXOBV
          IJ2=I+IROW1-1
          IF(IJ2.LT.1)IJ2=1
          IF(IJ2.GT.MAXOBV)IJ2=MAXOBV
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(IJ2)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(IJ2)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(IJ2)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(IJ2)
          IVALU9=INT(AVALUE+0.5)
        ELSE
          AVALUE=X1COOR(I)
          IVALU9=INT(AVALUE+0.5)
          IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        ENDIF
C
        IF(IX1ZFM.EQ.'VARI' .AND. IFLAGC.EQ.1)GOTO11160
        IF(IX1ZFM.EQ.'ROWL')GOTO1160
        IF(IX1ZFM.EQ.'GLAB')GOTO1170
        IF(IX1ZFM.EQ.'ALPH')GOTO1150
        IF(IX1TSC.EQ.'LOG'.AND.IX1ZFM.EQ.'REAL')GOTO1120
        IF(IX1TSC.EQ.'LOG'.AND.IX1ZFM.EQ.'FIXE')GOTO1120
        IF(IX1TSC.EQ.'LOG'.AND.IX1ZFM.EQ.'DECI')GOTO1120
        IF(IX1TSC.EQ.'LOG'.AND.IX1ZFM.EQ.'INTE')GOTO1120
        IF(IX1TSC.EQ.'LOG'.AND.IX1ZFM.EQ.'EXPO')GOTO1130
        IF(IX1TSC.EQ.'LOG'.AND.IX1ZFM.EQ.'EXP')GOTO1130
CCCCC ADD FOLLOWING 2 LINES.  JULY 1997.
        IF(IX1TSC.EQ.'LINE'.AND.IX1ZFM.EQ.'EXP')GOTO1140
        IF(IX1TSC.EQ.'LINE'.AND.IX1ZFM.EQ.'EXPO')GOTO1140
        GOTO1110
C
 1110   CONTINUE
        NMDID0=IX1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1180
C
 1120   CONTINUE
CCCCC   AVALUE=X1COOR(I)
        AVALUE=10.0**AVALUE
        IVALU9=INT(AVALUE+0.5)
        IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        NMDID0=IX1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1180
C
 1130   CONTINUE
        NMDID0=IX1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        IF(NCTEXT.LE.0)GOTO1139
        DO1131J=1,NCTEXT
          JREV=NCTEXT-J+1
          J2=JREV+7
          ICTEXT(J2)=ICTEXT(JREV)
 1131   CONTINUE
        ICTEXT(1)='1   '
        ICTEXT(2)='0   '
        ICTEXT(3)='S   '
        ICTEXT(4)='U   '
        ICTEXT(5)='P   '
        ICTEXT(6)='(   '
        ICTEXT(7)=')   '
        NCTEXT=NCTEXT+7
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='U   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='N   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='S   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='P   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='(   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)=')   '
 1139   CONTINUE
        GOTO1180
C
CCCCC ADD FOLLOWING SECTION (FOR EXPONENTIAL NUMBERS ON LINEAR
CCCCC SCALE) JULY 1997
 1140   CONTINUE
        NMDID0=IX1ZDP
        ISTRI2=' '
        ICTEMP='(E15.7 )'
        NTEMP2=7
        IF(NMDID0.GE.1)NTEMP2=NMDID0
        NTEMP1=NTEMP2+8
        IF(NTEMP2.LE.9)THEN
          WRITE(ICTEMP(6:6),'(I1)')NTEMP2
        ELSE
          WRITE(ICTEMP(6:7),'(I2)')NTEMP2
        ENDIF
        WRITE(ICTEMP(3:4),'(I2)')NTEMP1
        WRITE(ISTRI2,ICTEMP)AVALUE
        DO1142KK=1,NTEMP1
          IF(ISTRI2(KK:KK).NE.' ')THEN
             NCTEXT=KK
             ICTEXT(KK)=ISTRI2(KK:KK)
          ELSE
             ICTEXT(KK)=' '
          ENDIF
 1142   CONTINUE
C
        GOTO1180
C
 1150   CONTINUE
        MESSAG='OFF'
        CALL DPEXS1(IX1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)GOTO1159
        DO1152J=1,NCSTR2
          IC1=ISTRI2(J:J)
          IC4='    '
          IC4(1:1)=IC1
          ICTEXT(J)=IC4
 1152   CONTINUE
 1159   CONTINUE
        NCTEXT=NCSTR2
        GOTO1185
C
 1160   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
CCCCC   IF(IRLIND.EQ.1)THEN
CCCCC     IF(IVALU9.GE.1 .AND. IVALU9.LE.NX1COO)INDX=IVALU9
CCCCC   ENDIF
C
        DO1161J=1,24
          ICTEXT(J)=IROWLB(INDX)(J:J)
 1161   CONTINUE
        NCTEXT=1
        DO1163J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1185
          ENDIF
 1163   CONTINUE
        GOTO1185
C
11160   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
C
        REWIND(IOUNI2)
        READ(IOUNI2,'(I8)',END=11166,ERR=11166)NCVAR
        DO11161KK=1,NCVAR
          READ(IOUNI2,'(A1)',END=11166,ERR=11166)IAJUNK
11161   CONTINUE
        IF(INDX.GT.1)THEN
          DO11162KK=1,INDX-1
            READ(IOUNI2,'(A1)',END=11166,ERR=11166)IAJUNK
11162     CONTINUE
        ENDIF
        IFORMT=' '
        IFORMT='(    A)'
        WRITE(IFORMT(2:5),'(I4)')NCVAR
        READ(IOUNI2,IFORMT,END=11166,ERR=11166)ISTRCC
        NSTRT=(IROWID-1)*25 + 1
        NSTOP=NSTRT+23
        DO11163J=1,24
          IVAL=NSTRT+J-1
          ICTEXT(J)=ISTRCC(IVAL:IVAL)
11163   CONTINUE
        NCTEXT=1
        DO11164J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1185
          ENDIF
11164   CONTINUE
        GOTO1185
C
11166   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11167)
11167   FORMAT('***** ERROR IN X1TIC MARK LABEL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11168)
11168   FORMAT('      END OF FILE OR ERROR IN READING DPZCHF.DAT FILE.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
 1170   CONTINUE
        IF(IGVAR.EQ.0)THEN
           WRITE(ICOUT,999)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1176)
 1176      FORMAT('***** WARNING--UNABLE TO DETERMINE GROUP LABEL ',
     1            'VARIABLE FOR X1TIC MARK LABELS.')
           CALL DPWRST('XXX','BUG ')
           GOTO1190
        ENDIF
C
C       JANUARY 2006.  DETERMINE THE INDEX IF REQUESTED.
C
        INDX=I
        IF(IGLIND.EQ.1)THEN
          IF(IVALU9.GE.1 .AND. IVALU9.LE.NX1COO)INDX=IVALU9
        ENDIF
C
CCCCC   DO1171J=1,24
        DO1171J=1,MAXGR2
          ICTEXT(J)=IGRPLA(INDX,IGVAR)(J:J)
 1171   CONTINUE
        NCTEXT=1
CCCCC   DO1173J=24,1,-1
        DO1173J=MAXGR2,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1185
          ENDIF
 1173   CONTINUE
        GOTO1185
C
 1180   CONTINUE
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
          WRITE(ICOUT,8101)
 8101     FORMAT('STEP 21.1: AT 1180')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
CCCCC   MARCH 1993.  STRIP OUT UC(), LC(), AND SP() FOR HARDWARE TEXT.
        IF(IFONT.EQ.'TEKT')CALL GRSTRI(ICTEXT,NCTEXT)
        IF(NCTEXT.GE.1)
     1    CALL GRDETL(ICTEXT,NCTEXT,
     1                IFONT,IDIR,ANGLE,
     1                JFONT,JDIR,ANGLE2,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                JSIZE,
     1                JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                PXLEC,PXLECG,PYLEC,PYLECG)
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
          WRITE(ICOUT,8102)
 8102     FORMAT('STEP 21.1: BEFORE CALL GRWRTE')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NCTEXT.GE.1)
     1    CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
        GOTO1100
C
 1185   CONTINUE
C
        IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
          WRITE(ICOUT,8202)
 8202     FORMAT('STEP 21.1: BEFORE CALL DPWRTE')
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NCTEXT.GE.1)
     1    CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1                IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1                ISYMBL,ISPAC,
     1                IMPSW2,AMPSCH,AMPSCW,
     1                PX99,PY99)
        GOTO1100
C
 1100 CONTINUE
 1190 CONTINUE
C
C               ******************************************************
C               **  STEP 21.2--                                     **
C               **  WRITE TIC LABELS     ON TOP    HORIZONTAL AXIS  **
C               ******************************************************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8005)
 8005   FORMAT('STEP 21.2')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IX2FSW.EQ.'OFF')GOTO1290
CCCCC IF(IX2TSW.EQ.'OFF')GOTO1290
      IF(IX2ZSW.EQ.'OFF')GOTO1290
      IF(NX2COO.LE.0)GOTO1290
C
      IFONT=IX2ZFO
      CALL GRTRFO(ITYPE,IFONT,JFONT)
      CALL GRSEFO(ITYPE,IFONT,JFONT)
C
      ICASE=IX2ZCA
      CALL GRTRCA(ITYPE,ICASE,JCASE)
      CALL GRSECA(ITYPE,ICASE,JCASE)
C
      IJUST=IX2ZJU
      CALL GRTRJU(ITYPE,IJUST,JJUST)
      CALL GRSEJU(ITYPE,IJUST,JJUST)
C
      IDIR=IX2ZDI
      ANGLE=AX2ZAN
      CALL GRTRDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
      CALL GRSEDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
      IFILLT=IX2ZFI
      CALL GRTRFI(ITYPE,IFILLT,JFILLT)
      CALL GRSEFI(ITYPE,IFILLT,JFILLT)
C
      ICOL=IX2ZCO
      CALL GRTRCO(ITYPE,ICOL,JCOL)
      CALL GRSECO(ITYPE,ICOL,JCOL)
C
      PHEIGH=PX2ZHE
      PWIDTH=PX2ZWI
      PVEGAP=PX2ZVG
      PHOGAP=PX2ZHG
      CALL GRTRSI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
      CALL GRSESI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
C
      PY1=PYMAX+PX2ZDS
C
      ISTART=1
CCCCC ISTOP=130
      ISTOP=2048
C
CCCCC JANUARY 2004.  FOR VARIABLE OR GROUP LABEL CASE, NEED
CCCCC TO EXTRACT RELEVANT VARIABLE.
C
      IF(IX2ZFM.EQ.'VARI')THEN
C
        IFLAGC=0
        I=1
        CALL DPUPPE(IX2ZCN,ISTOP,IX2ZCN,IBUGG4,IERROR)
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12102)
12102     FORMAT('***** WARNING--FOR X2TIC MARK LABEL FORMAT ',
     1           '"VARIABLE"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12104)
12104     FORMAT('      NO VARIABLE NAME SPECIFIED ON ',
     1           'X2TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IHWUSE='V   '
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
C
C         2019/10: CHECK TO SEE IF DPZCHF.DAT EXISTS
C
          IOUNI2=IZCHNU
          IFILE2=IZCHNA
          ISTAT2=IZCHST
          IFORM2=IZCHFO
          IACCE2=IZCHAC
          IPROT2=IZCHPR
          ICURS2=IZCHCS
C
          ISUBN0='WRIT'
          IERRF2='NO'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
     1                ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO12201
          IFLAGC=1
          REWIND(IOUNI2)
          READ(IOUNI2,'(I8)',END=12201,ERR=12201)NCVAR
          DO12203KK=1,NCVAR
            READ(IOUNI2,'(2A4)',END=12201,ERR=12201)IHTEMP,IHTEM2
            IF(IHTEMP.EQ.IH .AND. IHTEM2.EQ.IH2)THEN
              IROWID=KK
              GOTO12109
            ENDIF
12203     CONTINUE
C
12201     CONTINUE
          IENDFI='OFF'
          IREWIN='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDFI,IREWIN,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IZCHCS='CLOSED'
C
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12106)IH,IH2
12106     FORMAT('***** WARNING--UNABLE TO DETERMINE VARIABLE ',
     1           2A4,' FOR X2TIC MARK LABELS.')
          CALL DPWRST('XXX','BUG ')
          IF(IFLAGC.EQ.1)GOTO9000
          GOTO1290
        ENDIF
        ICOLL=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
C
12109   CONTINUE
C
C  1/2006: IF VARIABLE LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IVLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,12116)IHIND,IHIND2
12116       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1            'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,12117)
12117       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(VARIABLE FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1290
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IVLIND=1
        ENDIF
      ELSEIF(IX2ZFM.EQ.'GLAB')THEN
        CALL DPUPPE(IX2ZCN,ISTOP,IX2ZCN,IBUGG4,IERROR)
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12122)
12122     FORMAT('***** WARNING--FOR X2TIC MARK LABEL FORMAT ',
     1           '"GROUP LABEL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,12124)
12124     FORMAT('      NO GROUP LABEL VARIABLE NAME SPECIFIED ON ',
     1           'X2TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
          GOTO1290
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IGVAR=0
        DO12120I=1,MAXGRP
          IF(IH(1:4).EQ.IGRPVN(I)(1:4) .AND.
     1       IH2(1:4).EQ.IGRPVN(I)(5:8))THEN
            IGVAR=I
            GOTO12129
          ENDIF
12120   CONTINUE
12129   CONTINUE
C
C  1/2006: IF GROUP LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IGLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,12136)IHIND,IHIND2
12136       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,12137)
12137       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(GROUP LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1290
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IGLIND=1
        ENDIF
C
      ELSEIF(IX2ZFM.EQ.'ROWL')THEN
C
C  1/2006: IF ROW LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        CALL DPUPPE(IX2ZCN,ISTOP,IX2ZCN,IBUGG4,IERROR)
        IRLIND=0
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,12138)IHIND,IHIND2
12138       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,12139)
12139       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(ROW LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1290
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IRLIND=1
        ENDIF
C
C       CHECK FOR STARTING ROW
C
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='P'
          MESSAG='NO'
          IERROR='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            IROW1=1
          ELSE
            IROW1=INT(VALUE(ILOCV)+0.01)
            IF(IROW1.LT.1)IROW1=1
            IF(IROW1.GT.MAXOBV)IROW1=1
          ENDIF
        ENDIF
C
      ENDIF
C
      DO1200I=1,NX2COO
C
        PX1=PX2COO(I)
        IF(IX2ZFM.EQ.'VARI')THEN
          IF(IVLIND.EQ.1)THEN
            IJ=MAXN*(ICOLI-1)+I
            IF(ICOLI.LE.MAXCOL)AVALU2=V(IJ)
            IF(ICOLI.EQ.MAXCP1)AVALU2=PRED(I)
            IF(ICOLI.EQ.MAXCP2)AVALU2=RES(I)
            IF(ICOLI.EQ.MAXCP3)AVALU2=YPLOT(I)
            IF(ICOLI.EQ.MAXCP4)AVALU2=XPLOT(I)
            IF(ICOLI.EQ.MAXCP5)AVALU2=X2PLOT(I)
            IF(ICOLI.EQ.MAXCP6)AVALU2=TAGPLO(I)
            INDX=INT(AVALU2+0.5)
            IF(INDX.LT.1 .OR. INDX.GT.NX2COO)THEN
              INDX=I
            ENDIF
          ELSE
            INDX=I
          ENDIF
C
          IF(IFLAGC.EQ.0)THEN
            IJ=MAXN*(ICOLL-1)+INDX
            IF(ICOLL.LE.MAXCOL)AVALUE=V(IJ)
            IF(ICOLL.EQ.MAXCP1)AVALUE=PRED(INDX)
            IF(ICOLL.EQ.MAXCP2)AVALUE=RES(INDX)
            IF(ICOLL.EQ.MAXCP3)AVALUE=YPLOT(INDX)
            IF(ICOLL.EQ.MAXCP4)AVALUE=XPLOT(INDX)
            IF(ICOLL.EQ.MAXCP5)AVALUE=X2PLOT(INDX)
            IF(ICOLL.EQ.MAXCP6)AVALUE=TAGPLO(INDX)
            IVALU9=INT(AVALUE+0.5)
            IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
          ELSE
            IVALU9=INDX
          ENDIF
        ELSEIF(IX2ZFM.EQ.'GLAB' .AND. IGLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(I)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(I)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(I)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(I)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(I)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(I)
          IVALU9=INT(AVALUE+0.5)
        ELSEIF(IX2ZFM.EQ.'ROWL' .AND. IRLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IJ=IJ+IROW1-1
          IF(IJ.LT.1)IJ=1
          IF(IJ.GT.MAXOBV)IJ=MAXOBV
          IJ2=I+IROW1-1
          IF(IJ2.LT.1)IJ2=1
          IF(IJ2.GT.MAXOBV)IJ2=MAXOBV
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(IJ2)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(IJ2)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(IJ2)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(IJ2)
          IVALU9=INT(AVALUE+0.5)
        ELSE
          AVALUE=X2COOR(I)
          IVALU9=INT(AVALUE+0.5)
          IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        ENDIF
C
        IF(IX2ZFM.EQ.'VARI' .AND. IFLAGC.EQ.1)GOTO12160
        IF(IX2ZFM.EQ.'ROWL')GOTO1260
        IF(IX2ZFM.EQ.'GLAB')GOTO1270
        IF(IX2ZFM.EQ.'ALPH')GOTO1250
        IF(IX2TSC.EQ.'LOG'.AND.IX2ZFM.EQ.'REAL')GOTO1220
        IF(IX2TSC.EQ.'LOG'.AND.IX2ZFM.EQ.'FIXE')GOTO1220
        IF(IX2TSC.EQ.'LOG'.AND.IX2ZFM.EQ.'DECI')GOTO1220
        IF(IX2TSC.EQ.'LOG'.AND.IX2ZFM.EQ.'INTE')GOTO1220
        IF(IX2TSC.EQ.'LOG'.AND.IX2ZFM.EQ.'EXPO')GOTO1230
        IF(IX2TSC.EQ.'LOG'.AND.IX2ZFM.EQ.'EXP')GOTO1230
CCCCC ADD FOLLOWING 2 LINES.  JULY 1997.
        IF(IX2TSC.EQ.'LINE'.AND.IX2ZFM.EQ.'EXP')GOTO1240
        IF(IX2TSC.EQ.'LINE'.AND.IX2ZFM.EQ.'EXPO')GOTO1240
        GOTO1210
C
 1210   CONTINUE
        NMDID0=IX2ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1280
C
 1220   CONTINUE
CCCCC   AVALUE=X2COOR(I)
        AVALUE=10.0**AVALUE
        IVALU9=INT(AVALUE+0.5)
        IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        NMDID0=IX2ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1280
C
 1230   CONTINUE
        NMDID0=IX2ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        IF(NCTEXT.LE.0)GOTO1239
        DO1231J=1,NCTEXT
        JREV=NCTEXT-J+1
        J2=JREV+7
        ICTEXT(J2)=ICTEXT(JREV)
 1231   CONTINUE
        ICTEXT(1)='1   '
        ICTEXT(2)='0   '
        ICTEXT(3)='S   '
        ICTEXT(4)='U   '
        ICTEXT(5)='P   '
        ICTEXT(6)='(   '
        ICTEXT(7)=')   '
        NCTEXT=NCTEXT+7
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='U   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='N   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='S   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='P   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='(   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)=')   '
 1239   CONTINUE
        GOTO1280
C
CCCCC ADD FOLLOWING SECTION (FOR EXPONENTIAL NUMBERS ON LINEAR
CCCCC SCALE) JULY 1997
 1240   CONTINUE
        NMDID0=IX2ZDP
        ISTRI2=' '
        ICTEMP='(E15.7 )'
        NTEMP2=7
        IF(NMDID0.GE.1)NTEMP2=NMDID0
        NTEMP1=NTEMP2+8
        IF(NTEMP2.LE.9)THEN
          WRITE(ICTEMP(6:6),'(I1)')NTEMP2
        ELSE
          WRITE(ICTEMP(6:7),'(I2)')NTEMP2
        ENDIF
        WRITE(ICTEMP(3:4),'(I2)')NTEMP1
        WRITE(ISTRI2,ICTEMP)AVALUE
        DO1242KK=1,NTEMP1
          IF(ISTRI2(KK:KK).NE.' ')THEN
             NCTEXT=KK
             ICTEXT(KK)=ISTRI2(KK:KK)
          ELSE
             ICTEXT(KK)=' '
          ENDIF
 1242   CONTINUE
C
        GOTO1280
C
 1250   CONTINUE
        MESSAG='OFF'
        CALL DPEXS1(IX2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)GOTO1259
        DO1252J=1,NCSTR2
          IC1=ISTRI2(J:J)
          IC4='    '
          IC4(1:1)=IC1
          ICTEXT(J)=IC4
 1252   CONTINUE
 1259   CONTINUE
        NCTEXT=NCSTR2
CCCCC   GOTO1280
        GOTO1285
C
 1260   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
CCCCC   IF(IRLIND.EQ.1)THEN
CCCCC     IF(IVALU9.GE.1 .AND. IVALU9.LE.NX2COO)INDX=IVALU9
CCCCC   ENDIF
C
        DO1261J=1,24
          ICTEXT(J)=IROWLB(INDX)(J:J)
 1261   CONTINUE
        NCTEXT=1
        DO1263J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1285
          ENDIF
 1263   CONTINUE
        GOTO1285
C
12160   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
C
        REWIND(IOUNI2)
        READ(IOUNI2,'(I8)',END=12166,ERR=12166)NCVAR
        DO12161KK=1,NCVAR
          READ(IOUNI2,'(A1)',END=12166,ERR=12166)IAJUNK
12161   CONTINUE
        IF(INDX.GT.1)THEN
          DO12162KK=1,INDX-1
            READ(IOUNI2,'(A1)',END=12166,ERR=12166)IAJUNK
12162     CONTINUE
        ENDIF
        IFORMT=' '
        IFORMT='(    A)'
        WRITE(IFORMT(2:5),'(I4)')NCVAR
        READ(IOUNI2,IFORMT,END=12166,ERR=12166)ISTRCC
        NSTRT=(IROWID-1)*25 + 1
        NSTOP=NSTRT+23
        DO12163J=1,24
          IVAL=NSTRT+J-1
          ICTEXT(J)=ISTRCC(IVAL:IVAL)
12163   CONTINUE
        NCTEXT=1
        DO12164J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1285
          ENDIF
12164   CONTINUE
        GOTO1285
C
12166   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,12167)
12167   FORMAT('***** ERROR IN X2TIC MARK LABEL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11168)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
 1270   CONTINUE
C
C       JANUARY 2006.  DETERMINE THE INDEX IF REQUESTED.
C
        INDX=I
        IF(IGLIND.EQ.1)THEN
          IF(IVALU9.GE.1 .AND. IVALU9.LE.NX2COO)INDX=IVALU9
        ENDIF
C
        IF(IGVAR.EQ.0)THEN
           WRITE(ICOUT,999)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1276)
 1276      FORMAT('***** WARNING--UNABLE TO DETERMINE GROUP LABEL ',
     1            'VARIABLE FOR X2TIC MARK LABELS.')
           CALL DPWRST('XXX','BUG ')
           GOTO1290
        ENDIF
CCCCC   DO1271J=1,24
        DO1271J=1,MAXGR2
          ICTEXT(J)=IGRPLA(INDX,IGVAR)(J:J)
 1271   CONTINUE
        NCTEXT=1
CCCCC   DO1273J=24,1,-1
        DO1273J=MAXGR2,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1285
          ENDIF
 1273   CONTINUE
        GOTO1285
C
 1280   CONTINUE
CCCCC   MARCH 1993.  STRIP OUT UC(), LC(), AND SP() FOR HARDWARE TEXT.
        IF(IFONT.EQ.'TEKT')CALL GRSTRI(ICTEXT,NCTEXT)
        IF(NCTEXT.GE.1)
     1    CALL GRDETL(ICTEXT,NCTEXT,
     1                IFONT,IDIR,ANGLE,
     1                JFONT,JDIR,ANGLE2,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                JSIZE,
     1                JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                PXLEC,PXLECG,PYLEC,PYLECG)
C
        IF(NCTEXT.GE.1)
     1    CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1      IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1      JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1      PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1      JSIZE,
     1      JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1      PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1      JTHICK,PTHIC2,
     1      PXLEC,PXLECG,PYLEC,PYLECG,
     1      ISYMBL,ISPAC,
     1      IMPSW2,AMPSCH,AMPSCW,
     1      PX99,PY99)
        GOTO1200
C
 1285   CONTINUE
        IF(NCTEXT.GE.1)
     1    CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1                IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1                ISYMBL,ISPAC,
     1                IMPSW2,AMPSCH,AMPSCW,
     1                PX99,PY99)
        GOTO1200
C
 1200 CONTINUE
 1290 CONTINUE
C
C               ******************************************************
C               **  STEP 21.3--                                     **
C               **  WRITE TIC LABELS     ON LEFT   VERTICAL   AXIS  **
C               ******************************************************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8009)
 8009   FORMAT('STEP 21.2')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IY1FSW.EQ.'OFF')GOTO1390
CCCCC IF(IY1TSW.EQ.'OFF')GOTO1390
      IF(IY1ZSW.EQ.'OFF')GOTO1390
      IF(NY1COO.LE.0)GOTO1390
C
      IFONT=IY1ZFO
      CALL GRTRFO(ITYPE,IFONT,JFONT)
      CALL GRSEFO(ITYPE,IFONT,JFONT)
C
      ICASE=IY1ZCA
      CALL GRTRCA(ITYPE,ICASE,JCASE)
      CALL GRSECA(ITYPE,ICASE,JCASE)
C
      IJUST=IY1ZJU
      CALL GRTRJU(ITYPE,IJUST,JJUST)
      CALL GRSEJU(ITYPE,IJUST,JJUST)
C
      IDIR=IY1ZDI
      ANGLE=AY1ZAN
      CALL GRTRDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
      CALL GRSEDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
      IFILLT=IY1ZFI
      CALL GRTRFI(ITYPE,IFILLT,JFILLT)
      CALL GRSEFI(ITYPE,IFILLT,JFILLT)
C
      ICOL=IY1ZCO
      CALL GRTRCO(ITYPE,ICOL,JCOL)
      CALL GRSECO(ITYPE,ICOL,JCOL)
C
      PHEIGH=PY1ZHE
      PWIDTH=PY1ZWI
      PVEGAP=PY1ZVG
      PHOGAP=PY1ZHG
      CALL GRTRSI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
      CALL GRSESI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
C
      ISTART=1
CCCCC ISTOP=130
      ISTOP=2048
      NCOLMX=2048
C
CCCCC JANUARY 2004.  FOR VARIABLE OR GROUP LABEL CASE, NEED
CCCCC TO EXTRACT RELEVANT VARIABLE.
C
      IF(IY1ZFM.EQ.'VARI')THEN
C
        IFLAGC=0
        I=1
        CALL DPUPPE(IY1ZCN,ISTOP,IY1ZCN,IBUGG4,IERROR)
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,13102)
13102     FORMAT('***** WARNING--FOR Y1TIC MARK LABEL FORMAT ',
     1           '"VARIABLE"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,13104)
13104     FORMAT('      NO VARIABLE NAME SPECIFIED ON ',
     1           'Y1TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
C
C         2019/10: CHECK TO SEE IF DPZCHF.DAT EXISTS
C
          IOUNI2=IZCHNU
          IFILE2=IZCHNA
          ISTAT2=IZCHST
          IFORM2=IZCHFO
          IACCE2=IZCHAC
          IPROT2=IZCHPR
          ICURS2=IZCHCS
C
          ISUBN0='WRIT'
          IERRF2='NO'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
     1                ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO13101
          IFLAGC=1
          REWIND(IOUNI2)
          READ(IOUNI2,'(I8)',END=13201,ERR=13201)NCVAR
          DO13203KK=1,NCVAR
            READ(IOUNI2,'(2A4)',END=13201,ERR=13201)IHTEMP,IHTEM2
            IF(IHTEMP.EQ.IH .AND. IHTEM2.EQ.IH2)THEN
              IROWID=KK
              GOTO13109
            ENDIF
13203     CONTINUE
C
13201     CONTINUE
          IENDFI='OFF'
          IREWIN='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDFI,IREWIN,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IZCHCS='CLOSED'
C
13101     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,13106)IH,IH2
13106     FORMAT('***** WARNING--UNABLE TO DETERMINE VARIABLE ',
     1           2A4,' FOR Y1TIC MARK LABELS.')
          CALL DPWRST('XXX','BUG ')
          IF(IFLAGC.EQ.1)GOTO9000
          GOTO1390
        ENDIF
        ICOLL=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
C
13109   CONTINUE
C
C  1/2006: IF VARIABLE LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IVLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,13116)IHIND,IHIND2
13116       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1            'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,13117)
13117       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(VARIABLE FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1390
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IVLIND=1
        ENDIF
      ELSEIF(IY1ZFM.EQ.'GLAB')THEN
        CALL DPUPPE(IY1ZCN,ISTOP,IY1ZCN,IBUGG4,IERROR)
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,13122)
13122     FORMAT('***** WARNING--FOR Y1TIC MARK LABEL FORMAT ',
     1           '"GROUP LABEL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,13124)
13124     FORMAT('      NO GROUP LABEL VARIABLE NAME SPECIFIED ON ',
     1           'Y1TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
          GOTO1390
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IGVAR=0
        DO13120I=1,MAXGRP
          IF(IH(1:4).EQ.IGRPVN(I)(1:4) .AND.
     1       IH2(1:4).EQ.IGRPVN(I)(5:8))THEN
            IGVAR=I
            GOTO13129
          ENDIF
13120   CONTINUE
13129   CONTINUE
C
C  1/2006: IF GROUP LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IGLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,13136)IHIND,IHIND2
13136       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,13137)
13137       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(GROUP LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1390
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IGLIND=1
        ENDIF
C
      ELSEIF(IY1ZFM.EQ.'ROWL')THEN
C
C  1/2006: IF ROW LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        CALL DPUPPE(IY1ZCN,ISTOP,IY1ZCN,IBUGG4,IERROR)
        IRLIND=0
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,13138)IHIND,IHIND2
13138       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,13139)
13139       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(ROW LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1390
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IRLIND=1
        ENDIF
C
C       CHECK FOR STARTING ROW
C
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='P'
          MESSAG='NO'
          IERROR='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            IROW1=1
          ELSE
            IROW1=INT(VALUE(ILOCV)+0.01)
            IF(IROW1.LT.1)IROW1=1
            IF(IROW1.GT.MAXOBV)IROW1=1
          ENDIF
        ENDIF
C
      ENDIF
C
      DO1300I=1,NY1COO
C
        PX1=PXMIN-PY1ZDS
        PY1=PY1COO(I)
CCCCC   PY1=PY1-PHEIG2/2.0
        IF(IY1ZFM.EQ.'VARI')THEN
          IF(IVLIND.EQ.1)THEN
            IJ=MAXN*(ICOLI-1)+I
            IF(ICOLI.LE.MAXCOL)AVALU2=V(IJ)
            IF(ICOLI.EQ.MAXCP1)AVALU2=PRED(I)
            IF(ICOLI.EQ.MAXCP2)AVALU2=RES(I)
            IF(ICOLI.EQ.MAXCP3)AVALU2=YPLOT(I)
            IF(ICOLI.EQ.MAXCP4)AVALU2=XPLOT(I)
            IF(ICOLI.EQ.MAXCP5)AVALU2=X2PLOT(I)
            IF(ICOLI.EQ.MAXCP6)AVALU2=TAGPLO(I)
            INDX=INT(AVALU2+0.5)
            IF(INDX.LT.1 .OR. INDX.GT.NY1COO)THEN
              INDX=I
            ENDIF
          ELSE
            INDX=I
          ENDIF
          IF(IFLAGC.EQ.0)THEN
            IJ=MAXN*(ICOLL-1)+INDX
            IF(ICOLL.LE.MAXCOL)AVALUE=V(IJ)
            IF(ICOLL.EQ.MAXCP1)AVALUE=PRED(INDX)
            IF(ICOLL.EQ.MAXCP2)AVALUE=RES(INDX)
            IF(ICOLL.EQ.MAXCP3)AVALUE=YPLOT(INDX)
            IF(ICOLL.EQ.MAXCP4)AVALUE=XPLOT(INDX)
            IF(ICOLL.EQ.MAXCP5)AVALUE=X2PLOT(INDX)
            IF(ICOLL.EQ.MAXCP6)AVALUE=TAGPLO(INDX)
            IVALU9=INT(AVALUE+0.5)
            IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
          ELSE
            IVALU9=INDX
          ENDIF
        ELSEIF(IY1ZFM.EQ.'GLAB' .AND. IGLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(I)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(I)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(I)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(I)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(I)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(I)
          IVALU9=INT(AVALUE+0.5)
        ELSEIF(IY1ZFM.EQ.'ROWL' .AND. IRLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IJ=IJ+IROW1-1
          IF(IJ.LT.1)IJ=1
          IF(IJ.GT.MAXOBV)IJ=MAXOBV
          IJ2=I+IROW1-1
          IF(IJ2.LT.1)IJ2=1
          IF(IJ2.GT.MAXOBV)IJ2=MAXOBV
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(IJ2)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(IJ2)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(IJ2)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(IJ2)
          IVALU9=INT(AVALUE+0.5)
        ELSE
          AVALUE=Y1COOR(I)
          IVALU9=INT(AVALUE+0.5)
          IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        ENDIF
C
        IF(IY1ZFM.EQ.'VARI' .AND. IFLAGC.EQ.1)GOTO13160
        IF(IY1ZFM.EQ.'ROWL')GOTO1360
        IF(IY1ZFM.EQ.'GLAB')GOTO1370
        IF(IY1ZFM.EQ.'ALPH')GOTO1350
        IF(IY1TSC.EQ.'LOG'.AND.IY1ZFM.EQ.'REAL')GOTO1320
        IF(IY1TSC.EQ.'LOG'.AND.IY1ZFM.EQ.'FIXE')GOTO1320
        IF(IY1TSC.EQ.'LOG'.AND.IY1ZFM.EQ.'DECI')GOTO1320
        IF(IY1TSC.EQ.'LOG'.AND.IY1ZFM.EQ.'INTE')GOTO1320
        IF(IY1TSC.EQ.'LOG'.AND.IY1ZFM.EQ.'EXPO')GOTO1330
        IF(IY1TSC.EQ.'LOG'.AND.IY1ZFM.EQ.'EXP')GOTO1330
CCCCC   ADD FOLLOWING 2 LINES.  JULY 1997.
        IF(IY1TSC.EQ.'LINE'.AND.IY1ZFM.EQ.'EXP')GOTO1340
        IF(IY1TSC.EQ.'LINE'.AND.IY1ZFM.EQ.'EXPO')GOTO1340
        GOTO1310
C
 1310   CONTINUE
        NMDID0=IY1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1380
C
 1320   CONTINUE
CCCCC   AVALUE=Y1COOR(I)
        AVALUE=10.0**AVALUE
        IVALU9=INT(AVALUE+0.5)
        IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        NMDID0=IX1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1380
C
 1330   CONTINUE
        NMDID0=IY1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        IF(NCTEXT.LE.0)GOTO1339
        DO1331J=1,NCTEXT
          JREV=NCTEXT-J+1
          J2=JREV+7
          ICTEXT(J2)=ICTEXT(JREV)
 1331   CONTINUE
        ICTEXT(1)='1   '
        ICTEXT(2)='0   '
        ICTEXT(3)='S   '
        ICTEXT(4)='U   '
        ICTEXT(5)='P   '
        ICTEXT(6)='(   '
        ICTEXT(7)=')   '
        NCTEXT=NCTEXT+7
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='U   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='N   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='S   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='P   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='(   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)=')   '
 1339   CONTINUE
        GOTO1380
C
CCCCC   ADD FOLLOWING SECTION (FOR EXPONENTIAL NUMBERS ON LINEAR
CCCCC   SCALE) JULY 1997
 1340   CONTINUE
        NMDID0=IY1ZDP
        ISTRI2=' '
        ICTEMP='(E15.7 )'
        NTEMP2=7
        IF(NMDID0.GE.1)NTEMP2=NMDID0
        NTEMP1=NTEMP2+8
        IF(NTEMP2.LE.9)THEN
          WRITE(ICTEMP(6:6),'(I1)')NTEMP2
        ELSE
          WRITE(ICTEMP(6:7),'(I2)')NTEMP2
        ENDIF
        WRITE(ICTEMP(3:4),'(I2)')NTEMP1
        WRITE(ISTRI2,ICTEMP)AVALUE
        DO1342KK=1,NTEMP1
          IF(ISTRI2(KK:KK).NE.' ')THEN
             NCTEXT=KK
             ICTEXT(KK)=ISTRI2(KK:KK)
          ELSE
             ICTEXT(KK)=' '
          ENDIF
 1342   CONTINUE
C
        GOTO1380
C
 1350   CONTINUE
        MESSAG='OFF'
        CALL DPEXS1(IY1ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)GOTO1359
        IC4='    '
        DO1352J=1,NCSTR2
          IC4(1:1)=ISTRI2(J:J)
          ICTEXT(J)=IC4
 1352   CONTINUE
 1359   CONTINUE
        NCTEXT=NCSTR2
CCCCC   GOTO1380
        GOTO1385
C
 1360   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
CCCCC   IF(IRLIND.EQ.1)THEN
CCCCC     IF(IVALU9.GE.1 .AND. IVALU9.LE.NY1COO)INDX=IVALU9
CCCCC   ENDIF
C
        DO1361J=1,24
          ICTEXT(J)=IROWLB(INDX)(J:J)
 1361   CONTINUE
        NCTEXT=1
        DO1363J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1385
          ENDIF
 1363   CONTINUE
        GOTO1385
C
13160   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
C
        REWIND(IOUNI2)
        READ(IOUNI2,'(I8)',END=13166,ERR=13166)NCVAR
        DO13161KK=1,NCVAR
          READ(IOUNI2,'(A1)',END=13166,ERR=13166)IAJUNK
13161   CONTINUE
        IF(INDX.GT.1)THEN
          DO13162KK=1,INDX-1
            READ(IOUNI2,'(A1)',END=13166,ERR=13166)IAJUNK
13162     CONTINUE
        ENDIF
        IFORMT=' '
        IFORMT='(    A)'
        WRITE(IFORMT(2:5),'(I4)')NCVAR
        READ(IOUNI2,IFORMT,END=13166,ERR=13166)ISTRCC
        NSTRT=(IROWID-1)*25 + 1
        NSTOP=NSTRT+23
        DO13163J=1,24
          IVAL=NSTRT+J-1
          ICTEXT(J)=ISTRCC(IVAL:IVAL)
13163   CONTINUE
        NCTEXT=1
        DO13164J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1385
          ENDIF
13164   CONTINUE
        GOTO1385
C
13166   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,13167)
13167   FORMAT('***** ERROR IN Y1TIC MARK LABEL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11168)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
 1370   CONTINUE
C
C       JANUARY 2006.  DETERMINE THE INDEX IF REQUESTED.
C
        INDX=I
        IF(IGLIND.EQ.1)THEN
          IF(IVALU9.GE.1 .AND. IVALU9.LE.NY1COO)INDX=IVALU9
        ENDIF
C
        IF(IGVAR.EQ.0)THEN
           WRITE(ICOUT,999)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1376)
 1376      FORMAT('***** WARNING--UNABLE TO DETERMINE GROUP LABEL ',
     1            'VARIABLE FOR X1TIC MARK LABELS.')
           CALL DPWRST('XXX','BUG ')
           GOTO1390
        ENDIF
CCCCC   DO1371J=1,24
        DO1371J=1,MAXGR2
          ICTEXT(J)=IGRPLA(INDX,IGVAR)(J:J)
 1371   CONTINUE
        NCTEXT=1
CCCCC   DO1373J=24,1,-1
        DO1373J=MAXGR2,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1385
          ENDIF
 1373   CONTINUE
        GOTO1385
C
 1380   CONTINUE
CCCCC   MARCH 1993.  STRIP OUT UC(), LC(), AND SP() FOR HARDWARE TEXT.
        IF(IFONT.EQ.'TEKT')CALL GRSTRI(ICTEXT,NCTEXT)
        IF(NCTEXT.GE.1)
     1    CALL GRDETL(ICTEXT,NCTEXT,
     1                IFONT,IDIR,ANGLE,
     1                JFONT,JDIR,ANGLE2,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                JSIZE,
     1                JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                PXLEC,PXLECG,PYLEC,PYLECG)
C
        IF(NCTEXT.GE.1)
     1    CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1      IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1      JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1      PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1      JSIZE,
     1      JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1      PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1      JTHICK,PTHIC2,
     1      PXLEC,PXLECG,PYLEC,PYLECG,
     1      ISYMBL,ISPAC,
     1      IMPSW2,AMPSCH,AMPSCW,
     1      PX99,PY99)
        GOTO1300
C
 1385   CONTINUE
        IF(NCTEXT.GE.1)
     1    CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1      IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1      PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1      ISYMBL,ISPAC,
     1      IMPSW2,AMPSCH,AMPSCW,
     1      PX99,PY99)
            GOTO1300
C
 1300 CONTINUE
 1390 CONTINUE
C
C               ******************************************************
C               **  STEP 21.4--                                     **
C               **  WRITE TIC LABELS     ON RIGHT  VERTICAL   AXIS  **
C               ******************************************************
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,8014)
 8014   FORMAT('STEP 21.2')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IY2FSW.EQ.'OFF')GOTO1490
CCCCC IF(IY2TSW.EQ.'OFF')GOTO1490
      IF(IY2ZSW.EQ.'OFF')GOTO1490
      IF(NY2COO.LE.0)GOTO1490
C
      IFONT=IY2ZFO
      CALL GRTRFO(ITYPE,IFONT,JFONT)
      CALL GRSEFO(ITYPE,IFONT,JFONT)
C
      ICASE=IY2ZCA
      CALL GRTRCA(ITYPE,ICASE,JCASE)
      CALL GRSECA(ITYPE,ICASE,JCASE)
C
      IJUST=IY2ZJU
      CALL GRTRJU(ITYPE,IJUST,JJUST)
      CALL GRSEJU(ITYPE,IJUST,JJUST)
C
      IDIR=IY2ZDI
      ANGLE=AY2ZAN
      CALL GRTRDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
      CALL GRSEDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
      IFILLT=IY2ZFI
      CALL GRTRFI(ITYPE,IFILLT,JFILLT)
      CALL GRSEFI(ITYPE,IFILLT,JFILLT)
C
      ICOL=IY2ZCO
      CALL GRTRCO(ITYPE,ICOL,JCOL)
      CALL GRSECO(ITYPE,ICOL,JCOL)
C
      PHEIGH=PY2ZHE
      PWIDTH=PY2ZWI
      PVEGAP=PY2ZVG
      PHOGAP=PY2ZHG
      CALL GRTRSI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
      CALL GRSESI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
C
      ISTART=1
CCCCC ISTOP=130
      ISTOP=2048
C
CCCCC JANUARY 2004.  FOR VARIABLE OR GROUP LABEL CASE, NEED
CCCCC TO EXTRACT RELEVANT VARIABLE.
C
      IF(IY2ZFM.EQ.'VARI')THEN
C
        IFLAGC=0
        I=1
        CALL DPUPPE(IY2ZCN,ISTOP,IY2ZCN,IBUGG4,IERROR)
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,14102)
14102     FORMAT('***** WARNING--FOR Y2TIC MARK LABEL FORMAT ',
     1           '"VARIABLE"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,14104)
14104     FORMAT('      NO VARIABLE NAME SPECIFIED ON ',
     1           'Y2TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IHWUSE='V'
        MESSAG='NO'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
        IF(IERROR.EQ.'YES')THEN
C
C         2019/10: CHECK TO SEE IF DPZCHF.DAT EXISTS
C
          IOUNI2=IZCHNU
          IFILE2=IZCHNA
          ISTAT2=IZCHST
          IFORM2=IZCHFO
          IACCE2=IZCHAC
          IPROT2=IZCHPR
          ICURS2=IZCHCS
C
          ISUBN0='WRIT'
          IERRF2='NO'
          CALL DPOPFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,
     1                ICURS2,
     1                IREWI2,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IF(IERRF2.EQ.'YES')GOTO14101
          IFLAGC=1
          REWIND(IOUNI2)
          READ(IOUNI2,'(I8)',END=14201,ERR=14201)NCVAR
          DO14203KK=1,NCVAR
            READ(IOUNI2,'(2A4)',END=14201,ERR=14201)IHTEMP,IHTEM2
            IF(IHTEMP.EQ.IH .AND. IHTEM2.EQ.IH2)THEN
              IROWID=KK
              GOTO14109
            ENDIF
14203     CONTINUE
C
14201     CONTINUE
          IENDFI='OFF'
          IREWIN='ON'
          CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1                IENDFI,IREWIN,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
          IZCHCS='CLOSED'
C
14101     CONTINUE
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,14106)IH,IH2
14106     FORMAT('***** WARNING--UNABLE TO DETERMINE VARIABLE ',
     1           2A4,' FOR Y2TIC MARK LABELS.')
          CALL DPWRST('XXX','BUG ')
          IF(IFLAGC.EQ.1)GOTO9000
          GOTO1490
        ENDIF
        ICOLL=IVALUE(ILOCV)
        NLEFT=IN(ILOCV)
C
14109   CONTINUE
C
C  1/2006: IF VARIABLE LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IVLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,14116)IHIND,IHIND2
14116       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1            'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,14117)
14117       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(VARIABLE FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1490
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IVLIND=1
        ENDIF
      ELSEIF(IY2ZFM.EQ.'GLAB')THEN
        CALL DPUPPE(IY2ZCN,ISTOP,IY2ZCN,IBUGG4,IERROR)
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,14122)
14122     FORMAT('***** WARNING--FOR Y2TIC MARK LABEL FORMAT ',
     1           '"GROUP LABEL"')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,14124)
14124     FORMAT('      NO GROUP LABEL VARIABLE NAME SPECIFIED ON ',
     1           'Y2TIC MARK LABEL CONTENT COMMAND.')
          CALL DPWRST('XXX','BUG ')
          GOTO1490
        ELSE
          IH='    '
          IH2='    '
          IH(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)IH2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
        ENDIF
C
        IGVAR=0
        DO14120I=1,MAXGRP
          IF(IH(1:4).EQ.IGRPVN(I)(1:4) .AND.
     1       IH2(1:4).EQ.IGRPVN(I)(5:8))THEN
            IGVAR=I
            GOTO14129
          ENDIF
14120   CONTINUE
14129   CONTINUE
C
C  1/2006: IF GROUP LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        IGLIND=0
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,14136)IHIND,IHIND2
14136       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,14137)
14137       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(GROUP LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1490
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IGLIND=1
        ENDIF
C
      ELSEIF(IY2ZFM.EQ.'ROWL')THEN
C
C  1/2006: IF ROW LABEL SPECIFIED, THEN CHECK TO SEE IF
C          INDEX VARIABLE ALSO SPECIFIED.
C
        CALL DPUPPE(IY2ZCN,ISTOP,IY2ZCN,IBUGG4,IERROR)
        IRLIND=0
        I=1
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='V'
          MESSAG='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,14138)IHIND,IHIND2
14138       FORMAT('***** WARNING--UNABLE TO DETERMINE INDEX ',
     1             'VARIABLE, ',A4,A4,',')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,14139)
14139       FORMAT('      FOR TIC MARK LABEL CONTENTS ',
     1             '(ROW LABEL FORM).')
            CALL DPWRST('XXX','BUG ')
            GOTO1490
          ENDIF
          ICOLI=IVALUE(ILOCV)
          NLEFI=IN(ILOCV)
          IRLIND=1
        ENDIF
C
C       CHECK FOR STARTING ROW
C
        I=2
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.GT.0)THEN
          IHIND='    '
          IHIND2='    '
          IHIND(1:MIN(4,NCSTR2))=ISTRI2(1:MIN(4,NCSTR2))
          IF(NCSTR2.GE.5)
     1      IHIND2(1:MIN(8,NCSTR2)-4)=ISTRI2(5:MIN(8,NCSTR2))
C
          IHWUSE='P'
          MESSAG='NO'
          IERROR='NO'
          CALL CHECKN(IHIND,IHIND2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
          IF(IERROR.EQ.'YES')THEN
            IROW1=1
          ELSE
            IROW1=INT(VALUE(ILOCV)+0.01)
            IF(IROW1.LT.1)IROW1=1
            IF(IROW1.GT.MAXOBV)IROW1=1
          ENDIF
        ENDIF
C
      ENDIF
C
      DO1400I=1,NY2COO
C
        PX1=PXMAX+PY2ZDS
        PY1=PY2COO(I)
CCCCC   PY1=PY1-PHEIG2/2.0
        IF(IY2ZFM.EQ.'VARI')THEN
          IF(IVLIND.EQ.1)THEN
            IJ=MAXN*(ICOLI-1)+I
            IF(ICOLI.LE.MAXCOL)AVALU2=V(IJ)
            IF(ICOLI.EQ.MAXCP1)AVALU2=PRED(I)
            IF(ICOLI.EQ.MAXCP2)AVALU2=RES(I)
            IF(ICOLI.EQ.MAXCP3)AVALU2=YPLOT(I)
            IF(ICOLI.EQ.MAXCP4)AVALU2=XPLOT(I)
            IF(ICOLI.EQ.MAXCP5)AVALU2=X2PLOT(I)
            IF(ICOLI.EQ.MAXCP6)AVALU2=TAGPLO(I)
            INDX=INT(AVALU2+0.5)
            IF(INDX.LT.1 .OR. INDX.GT.NY2COO)THEN
              INDX=I
            ENDIF
          ELSE
            INDX=I
          ENDIF
C
          IF(IFLAGC.EQ.0)THEN
            IJ=MAXN*(ICOLL-1)+INDX
            IF(ICOLL.LE.MAXCOL)AVALUE=V(IJ)
            IF(ICOLL.EQ.MAXCP1)AVALUE=PRED(INDX)
            IF(ICOLL.EQ.MAXCP2)AVALUE=RES(INDX)
            IF(ICOLL.EQ.MAXCP3)AVALUE=YPLOT(INDX)
            IF(ICOLL.EQ.MAXCP4)AVALUE=XPLOT(INDX)
            IF(ICOLL.EQ.MAXCP5)AVALUE=X2PLOT(INDX)
            IF(ICOLL.EQ.MAXCP6)AVALUE=TAGPLO(INDX)
            IVALU9=INT(AVALUE+0.5)
            IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
          ELSE
            IVALU9=INDX
          ENDIF
        ELSEIF(IY2ZFM.EQ.'GLAB' .AND. IGLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(I)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(I)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(I)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(I)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(I)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(I)
          IVALU9=INT(AVALUE+0.5)
        ELSEIF(IY2ZFM.EQ.'ROWL' .AND. IRLIND.EQ.1)THEN
          IJ=MAXN*(ICOLI-1)+I
          IJ=IJ+IROW1-1
          IF(IJ.LT.1)IJ=1
          IF(IJ.GT.MAXOBV)IJ=MAXOBV
          IJ2=I+IROW1-1
          IF(IJ2.LT.1)IJ2=1
          IF(IJ2.GT.MAXOBV)IJ2=MAXOBV
          IF(ICOLI.LE.MAXCOL)AVALUE=V(IJ)
          IF(ICOLI.EQ.MAXCP1)AVALUE=PRED(IJ2)
          IF(ICOLI.EQ.MAXCP2)AVALUE=RES(IJ2)
          IF(ICOLI.EQ.MAXCP3)AVALUE=YPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP4)AVALUE=XPLOT(IJ2)
          IF(ICOLI.EQ.MAXCP5)AVALUE=X2PLOT(IJ2)
          IF(ICOLI.EQ.MAXCP6)AVALUE=TAGPLO(IJ2)
          IVALU9=INT(AVALUE+0.5)
        ELSE
          AVALUE=Y2COOR(I)
          IVALU9=INT(AVALUE+0.5)
          IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        ENDIF
C
        IF(IY2ZFM.EQ.'VARI' .AND. IFLAGC.EQ.1)GOTO14160
        IF(IY2ZFM.EQ.'ROWL')GOTO1460
        IF(IY2ZFM.EQ.'GLAB')GOTO1470
        IF(IY2ZFM.EQ.'ALPH')GOTO1450
        IF(IY2TSC.EQ.'LOG'.AND.IY2ZFM.EQ.'REAL')GOTO1420
        IF(IY2TSC.EQ.'LOG'.AND.IY2ZFM.EQ.'FIXE')GOTO1420
        IF(IY2TSC.EQ.'LOG'.AND.IY2ZFM.EQ.'DECI')GOTO1420
        IF(IY2TSC.EQ.'LOG'.AND.IY2ZFM.EQ.'INTE')GOTO1420
        IF(IY2TSC.EQ.'LOG'.AND.IY2ZFM.EQ.'EXPO')GOTO1430
        IF(IY2TSC.EQ.'LOG'.AND.IY2ZFM.EQ.'EXP')GOTO1430
CCCCC   ADD FOLLOWING 2 LINES.  JULY 1997.
        IF(IY2TSC.EQ.'LINE'.AND.IY2ZFM.EQ.'EXP')GOTO1440
        IF(IY2TSC.EQ.'LINE'.AND.IY2ZFM.EQ.'EXPO')GOTO1440
        GOTO1410
C
 1410   CONTINUE
        NMDID0=IY2ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1480
C
 1420   CONTINUE
CCCCC   AVALUE=Y2COOR(I)
        AVALUE=10.0**AVALUE
        IVALU9=INT(AVALUE+0.5)
        IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
        NMDID0=IX1ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        GOTO1480
C
 1430   CONTINUE
        NMDID0=IY2ZDP
        CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
        IF(NCTEXT.LE.0)GOTO1439
        DO1431J=1,NCTEXT
          JREV=NCTEXT-J+1
          J2=JREV+7
          ICTEXT(J2)=ICTEXT(JREV)
 1431   CONTINUE
        ICTEXT(1)='1   '
        ICTEXT(2)='0   '
        ICTEXT(3)='S   '
        ICTEXT(4)='U   '
        ICTEXT(5)='P   '
        ICTEXT(6)='(   '
        ICTEXT(7)=')   '
        NCTEXT=NCTEXT+7
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='U   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='N   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='S   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='P   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)='(   '
        NCTEXT=NCTEXT+1
        ICTEXT(NCTEXT)=')   '
 1439   CONTINUE
        GOTO1480
C
CCCCC   ADD FOLLOWING SECTION (FOR EXPONENTIAL NUMBERS ON LINEAR
CCCCC   SCALE) JULY 1997
 1440   CONTINUE
        NMDID0=IY2ZDP
        ISTRI2=' '
        ICTEMP='(E15.7 )'
        NTEMP2=7
        IF(NMDID0.GE.1)NTEMP2=NMDID0
        NTEMP1=NTEMP2+8
        IF(NTEMP2.LE.9)THEN
          WRITE(ICTEMP(6:6),'(I1)')NTEMP2
        ELSE
          WRITE(ICTEMP(6:7),'(I2)')NTEMP2
        ENDIF
        WRITE(ICTEMP(3:4),'(I2)')NTEMP1
        WRITE(ISTRI2,ICTEMP)AVALUE
        DO1442KK=1,NTEMP1
          IF(ISTRI2(KK:KK).NE.' ')THEN
             NCTEXT=KK
             ICTEXT(KK)=ISTRI2(KK:KK)
          ELSE
             ICTEXT(KK)=' '
          ENDIF
 1442 CONTINUE
C
        GOTO1480
C
 1450 CONTINUE
        MESSAG='OFF'
        CALL DPEXS1(IY2ZCN,NCOLMX,ISTART,ISTOP,I,MESSAG,
     1              ICOL1,ICOL2,ISTRI2,NCSTR2,
     1              IBUGG4,ISUBG4,IERRG4)
        IF(NCSTR2.LE.0)GOTO1459
        DO1452J=1,NCSTR2
        IC1=ISTRI2(J:J)
        IC4='    '
        IC4(1:1)=IC1
        ICTEXT(J)=IC4
 1452 CONTINUE
 1459 CONTINUE
        NCTEXT=NCSTR2
CCCCC GOTO1480
        GOTO1485
C
 1460   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
CCCCC   IF(IRLIND.EQ.1)THEN
CCCCC     IF(IVALU9.GE.1 .AND. IVALU9.LE.NY2COO)INDX=IVALU9
CCCCC   ENDIF
C
        DO1461J=1,24
          ICTEXT(J)=IROWLB(INDX)(J:J)
 1461   CONTINUE
        NCTEXT=1
        DO1463J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1485
          ENDIF
 1463   CONTINUE
        GOTO1485
C
14160   CONTINUE
        INDX=IVALU9
        IF(INDX.LT.1)INDX=I
C
        REWIND(IOUNI2)
        READ(IOUNI2,'(I8)',END=14166,ERR=14166)NCVAR
        DO14161KK=1,NCVAR
          READ(IOUNI2,'(A1)',END=14166,ERR=14166)IAJUNK
14161   CONTINUE
        IF(INDX.GT.1)THEN
          DO14162KK=1,INDX-1
            READ(IOUNI2,'(A1)',END=14166,ERR=14166)IAJUNK
14162     CONTINUE
        ENDIF
        IFORMT=' '
        IFORMT='(    A)'
        WRITE(IFORMT(2:5),'(I4)')NCVAR
        READ(IOUNI2,IFORMT,END=14166,ERR=14166)ISTRCC
        NSTRT=(IROWID-1)*25 + 1
        NSTOP=NSTRT+23
        DO14163J=1,24
          IVAL=NSTRT+J-1
          ICTEXT(J)=ISTRCC(IVAL:IVAL)
14163   CONTINUE
        NCTEXT=1
        DO14164J=24,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1485
          ENDIF
14164   CONTINUE
        GOTO1485
C
14166   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,14167)
14167   FORMAT('***** ERROR IN Y2TIC MARK LABEL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11168)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
C
 1470   CONTINUE
C
C       JANUARY 2006.  DETERMINE THE INDEX IF REQUESTED.
C
        INDX=I
        IF(IGLIND.EQ.1)THEN
          IF(IVALU9.GE.1 .AND. IVALU9.LE.NY2COO)INDX=IVALU9
        ENDIF
C
        IF(IGVAR.EQ.0)THEN
           WRITE(ICOUT,999)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1476)
 1476      FORMAT('***** WARNING--UNABLE TO DETERMINE GROUP LABEL ',
     1            'VARIABLE FOR X1TIC MARK LABELS.')
           CALL DPWRST('XXX','BUG ')
           GOTO1490
        ENDIF
CCCCC   DO1471J=1,24
        DO1471J=1,MAXGR2
          ICTEXT(J)=IGRPLA(INDX,IGVAR)(J:J)
 1471   CONTINUE
        NCTEXT=1
CCCCC   DO1473J=24,1,-1
        DO1473J=MAXGR2,1,-1
          IF(ICTEXT(J).NE.'    ')THEN
            NCTEXT=J
            GOTO1485
          ENDIF
 1473   CONTINUE
        GOTO1485
C
 1480   CONTINUE
CCCCC   MARCH 1993.  STRIP OUT UC(), LC(), AND SP() FOR HARDWARE TEXT.
        IF(IFONT.EQ.'TEKT')CALL GRSTRI(ICTEXT,NCTEXT)
        IF(NCTEXT.GE.1)
     1    CALL GRDETL(ICTEXT,NCTEXT,
     1                IFONT,IDIR,ANGLE,
     1                JFONT,JDIR,ANGLE2,
     1                PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1                JSIZE,
     1                JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1                PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1                PXLEC,PXLECG,PYLEC,PYLECG)
C
        IF(NCTEXT.GE.1)
     1    CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1      IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1      JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1      PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1      JSIZE,
     1      JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1      PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1      JTHICK,PTHIC2,
     1      PXLEC,PXLECG,PYLEC,PYLECG,
     1      ISYMBL,ISPAC,
     1      IMPSW2,AMPSCH,AMPSCW,
     1      PX99,PY99)
        GOTO1400
C
 1485   CONTINUE
        IF(NCTEXT.GE.1)
     1    CALL DPWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1      IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1      PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1      ISYMBL,ISPAC,
     1      IMPSW2,AMPSCH,AMPSCW,
     1      PX99,PY99)
        GOTO1400
C
 1400 CONTINUE
 1490 CONTINUE
C
      GOTO9000
C
C               ******************************************************
C               **  STEP 8--                                        **
C               **  WRITE TIC LABELS FOR TRILINEAR SCALES           **
C               ******************************************************
C
C     NOTE: FOR TRILINEAR SCALES, CURRENTLY ONLY SUPPORT LINEAR
C           SCALES.  IF TIC MARK LABELS REQUESTED, PRINT THE
C           "0" AND "1" VALUES FOR EACH OF THE 3 COMPONENTS.
C
C
 8000 CONTINUE
C
      IFONT=IX1ZFO
      CALL GRTRFO(ITYPE,IFONT,JFONT)
      CALL GRSEFO(ITYPE,IFONT,JFONT)
C
      ICASE=IX1ZCA
      CALL GRTRCA(ITYPE,ICASE,JCASE)
      CALL GRSECA(ITYPE,ICASE,JCASE)
C
      IJUST=IX1ZJU
      CALL GRTRJU(ITYPE,IJUST,JJUST)
      CALL GRSEJU(ITYPE,IJUST,JJUST)
C
      IDIR=IX1ZDI
      ANGLE=AX1ZAN
      CALL GRTRDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
      CALL GRSEDI(ITYPE,IDIR,ANGLE,JDIR,ANGLE2)
C
      IFILLT=IX1ZFI
      CALL GRTRFI(ITYPE,IFILLT,JFILLT)
      CALL GRSEFI(ITYPE,IFILLT,JFILLT)
C
      ICOL=IX1ZCO
      CALL GRTRCO(ITYPE,ICOL,JCOL)
      CALL GRSECO(ITYPE,ICOL,JCOL)
C
      PHEIGH=PX1ZHE
      PWIDTH=PX1ZWI
      PVEGAP=PX1ZVG
      PHOGAP=PX1ZHG
      CALL GRTRSI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
      CALL GRSESI(ITYPE,IFONT,PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1JSIZE,
     1JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1PHEIG2,PWIDT2,PVEGA2,PHOGA2)
C
      AMIN=0.0
      AMAX=FX1MAX
      GRDINC=(AMAX-AMIN)/REAL(NX1COO-1)
      PXRANG=PXMAX - PXMIN
      PYRANG=PYMAX - PYMIN
C
C     TIC LABELS FOR X1
C
      AVALUE=X1COOR(NX1COO)
      IVALU9=INT(AVALUE+0.5)
      IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
      NMDID0=IX1ZDP
      CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
      PX1=0.5*(PXMIN+PXMAX)
      PY1=PYMAX + PX1ZDS
      IF(NCTEXT.GE.1)THEN
        CALL GRDETL(ICTEXT,NCTEXT,
     1              IFONT,IDIR,ANGLE,
     1              JFONT,JDIR,ANGLE2,
     1              PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1              JSIZE,
     1              JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1              PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1              PXLEC,PXLECG,PYLEC,PYLECG)
C
        CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
C
      ENDIF
C
      AVALUE=0.0
      IVALU9=0
      NMDID0=IX1ZDP
      CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
      PY1=PYMIN - PX1ZDS
      IF(NCTEXT.GE.1)THEN
        CALL GRDETL(ICTEXT,NCTEXT,
     1              IFONT,IDIR,ANGLE,
     1              JFONT,JDIR,ANGLE2,
     1              PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1              JSIZE,
     1              JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1              PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1              PXLEC,PXLECG,PYLEC,PYLECG)
C
        CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
C
      ENDIF
C
C     TIC LABELS FOR X2
C
      AVALUE=X1COOR(NX1COO)
      IVALU9=INT(AVALUE+0.5)
      IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
      NMDID0=IX1ZDP
      CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
      PX1=PXMIN - PX1ZDS
      PY1=PYMIN - PHEIGH/2.0
      IF(NCTEXT.GE.1)THEN
        CALL GRDETL(ICTEXT,NCTEXT,
     1              IFONT,IDIR,ANGLE,
     1              JFONT,JDIR,ANGLE2,
     1              PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1              JSIZE,
     1              JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1              PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1              PXLEC,PXLECG,PYLEC,PYLECG)
C
        CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
C
      ENDIF
C
      AVALUE=0.0
      IVALU9=0
      NMDID0=IX1ZDP
      CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
      PX1=PXMIN + PXRANG*0.5
      PX1=PX1 + 0.5*PXRANG*(AMAX-0.5)
      PY1=PYMIN + PYRANG*(AMAX-0.5)
      PX1=PX1 + PX1ZDS
      IF(NCTEXT.GE.1)THEN
        CALL GRDETL(ICTEXT,NCTEXT,
     1              IFONT,IDIR,ANGLE,
     1              JFONT,JDIR,ANGLE2,
     1              PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1              JSIZE,
     1              JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1              PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1              PXLEC,PXLECG,PYLEC,PYLECG)
C
        CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
C
      ENDIF
C
C     TIC LABELS FOR X3
C
      AVALUE=X1COOR(NX1COO)
      IVALU9=INT(AVALUE+0.5)
      IF(AVALUE.LT.0.0)IVALU9=INT(AVALUE-0.5)
      NMDID0=IX1ZDP
      CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
      PX1=PXMAX + PX1ZDS
      PY1=PYMIN - PHEIGH/2.0
      IF(NCTEXT.GE.1)THEN
        CALL GRDETL(ICTEXT,NCTEXT,
     1              IFONT,IDIR,ANGLE,
     1              JFONT,JDIR,ANGLE2,
     1              PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1              JSIZE,
     1              JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1              PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1              PXLEC,PXLECG,PYLEC,PYLECG)
C
        CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
C
      ENDIF
C
      AVALUE=0.0
      IVALU9=0
      NMDID0=IX1ZDP
      CALL DPCON2(IVALU9,AVALUE,ICTEXT,NCTEXT,NMDID0,IBUGG4,IERRG4)
      PX1=PXMIN + 0.25*PXRANG
      PY1=PYMIN + 0.5*PYRANG
      PX1=PX1 - PX1ZDS
      IF(NCTEXT.GE.1)THEN
        CALL GRDETL(ICTEXT,NCTEXT,
     1              IFONT,IDIR,ANGLE,
     1              JFONT,JDIR,ANGLE2,
     1              PHEIGH,PWIDTH,PVEGAP,PHOGAP,
     1              JSIZE,
     1              JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1              PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1              PXLEC,PXLECG,PYLEC,PYLECG)
C
        CALL GRWRTE(PX1,PY1,ICTEXT,NCTEXT,
     1         IPATTT,IFONT,ICASE,IJUST,IDIR,ANGLE,IFILLT,ICOL,
     1         JPATTT,JFONT,JCASE,JJUST,JDIR,ANGLE2,JFILLT,JCOL,
     1         PHEIGH,PWIDTH,PVEGAP,PHOGAP,PTHICK,
     1         JSIZE,
     1         JHEIG2,JWIDT2,JVEGA2,JHOGA2,
     1         PHEIG2,PWIDT2,PVEGA2,PHOGA2,
     1         JTHICK,PTHIC2,
     1         PXLEC,PXLECG,PYLEC,PYLECG,
     1         ISYMBL,ISPAC,
     1         IMPSW2,AMPSCH,AMPSCW,
     1         PX99,PY99)
C
      ENDIF
C
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
      IF(IFLAGC.EQ.1)THEN
        IENDFI='OFF'
        IREWIN='ON'
        CALL DPCLFI(IOUNI2,IFILE2,ISTAT2,IFORM2,IACCE2,IPROT2,ICURS2,
     1              IENDFI,IREWIN,ISUBN0,IERRF2,IBUGG4,ISUBRO,IERROR)
        IZCHCS='CLOSED'
      ENDIF
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRTL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWRTL--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWSAT(YVAR,YNU,YA,N,IFLAG,UNC,
     1                  WSDF,POOLSD,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THE WELCH-SAITTERTHWAITE IS USED TO DETERMINE THE
C              "EFFECTIVE DEGREES OF FREEDOM" WHEN WE ARE COMBINING
C              UNCERTAINTIES FROM MULTIPLE SOURCES.
C
C              WE CURRENTLY SUPPORT TWO VARIANTS OF
C              WELCH-SAITTERTHWAITE.
C
C              1) FOR THE FIRST METHOD (THIS IS THE METHOD DESCRIBED ON
C                 WIKIPEDIA), WE HAVE N VARIANCES WITH THEIR CORRESPONDING
C                 DEGREES OF FREEDOM.  THE FORMULA IS THEN GIVEN AS
C
C                     NU' = (SUM[k(i)*s(i)**2])**2/
C                           SUM[(k(i)*s(i)**2)**2/nu(i)]
C
C                 WHERE
C
C                     k(i)    = 1/(nu(i)+1)
C                     nu(i)   = DEGREES OF FREEDOM FOR i-TH COMPONENT
C                     s(i)**2 = VARIANCE OF i-TH COMPONENT
C
C                 THIS FORM WILL ALSO RETURN THE POOLED STANDARD
C                 DEVIATION
C
C                     POOLSD = SQRT(SUM[i=1 to N][(NU(i)-1)*S(i)**2]/
C                                   SUM[i=1 to N][NU(i) - 1]
C
C                 THIS VERSION OF WELCH SATTERTHWAITE IS OFTEN USED TO
C                 POOL THE STANDARD DEVIATION AND DEGREES OF FREEDOM
C                 WHEN COMBING TYPE A COMPONENTS.  AN EXAMPLE IS THE
C                 TWO-SAMPLE T-TEST WITH UNEQUAL VARIANCES.
C
C                 NOTE THAT THIS CASE ASSUMES THE VARIANCES ARE
C                 INDEPENDENT.
C
C              2) THE FORMULA GIVEN IN THE GUM IS
C
C                    NU'= u**4/SUM[i=1 to N][A(i)**4*s(i)**4/nu(i)]
C
C                 WHERE
C
C                    U      = THE COMBINED STANDARD UNCERTAINTY
C                    S(i)   = THE STANDARD UNCERTAINTY FOR THE i-TH
C                             COMPONENT
C                    A(i)   = THE SENSITIVTY COMPONENT OF THE i-TH
C                             COMPONENT
C
C                 THE GUM FORMULA IS OFTEN USED WHEN WE ARE COMBINING
C                 TYPE A AND TYPE B SOURCES OF UNCERTAINTY.  THE
C                 SENSITIVITY COMPONENTS ARE DERIVED FROM PARTIAL
C                 DERIVATIVES OF THE MEASUREMENT EQUATION.  WHEN
C                 WE HAVE ADDITIVE, INDEPENDENT UNCERTAINTIES, THESE
C                 CAN BE SET TO 1.
C
C                 DEGREES OF FREEDOM FOR TYPE B COMPONENTS ARE OFTEN
C                 UNKNOWN AND OFTEN ASSUMED TO BE INFINITE.  FOR THESE
C                 CASES, IT IS RECOMMEND SIMPLY ASSIGNING A LARGE VALUE
C                 FOR THE DEGREES OF FREEDOM (E.G., 1,000 OR 10,000).
C
C     INPUT ARGUMENTS--YVAR   = THE SINGLE PRECISION VECTOR OF
C                               VARIANCES (OR STANDARD UNCERTAINTIES)
C                    --YNU    = THE SINGLE PRECISION VECTOR THAT
C                               SPECIFIES THE CORRESPONDING DEGREES
C                               OF FREEDOM
C                    --YA     = THE SINGLE PRECISION VECTOR THAT
C                               CONTAINS THE SENSITIVITY COEFFICIENTS
C                    --N      = THE INTEGER NUMBER OF OBSERVATIONS
C                               IN THE VECTOR YNU
C                    --IFLAG  = 1 => METHOD 1, YVAR CONTAINS VARIANCES
C                               2 => METHOD 2, YVAR CONTAINS STANDARD
C                                    UNCERTAINTIES
C     OUTPUT ARGUMENTS-WSDF   = THE SINGLE PRECISION SCALAR CONTAINING
C                               THE WELCH-SAITTERTHWAITE EFFECTIVE
C                               DEGREES OF FREEDOM
C                     -POOLSD = THE SINGLE PRECISION SCALAR CONTAINING
C                               THE POOLED SD BASED ON SUMMING IN
C                               QUADRATURE (METHOD 1 ONLY)
C     OTHER DATAPAC   SUBROUTINES NEEDED--NONE.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN. 
C     REFERENCES--SATTERTHWAITE (1946), "AN APPROXIMATE DISTRIBUTION OF
C                 VARIANCE COMPONENTS", BIOMETRICS BULLETIN, 2: 110-114.
C               --WELCH (1947), "THE GENERALIZATION OF "STUDENT'S"
C                 PROBLEM WHEN SEVERAL DIFFERENT POPULATION VARIANCES
C                 ARE INVOLVED", BIOMETRIKA, 34: 28-35.
C               --GUIDE TO THE EXPRESSION OF UNCERTAINTY IN MEASUREMENT,
C                 ISO, GENEVA (1993).
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     ORIGINAL VERSION--JANUARY   2017. 
C
C---------------------------------------------------------------------
C
      DIMENSION YVAR(*)
      DIMENSION YNU(*)
      DIMENSION YA(*)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      DOUBLE PRECISION DSUM1
      DOUBLE PRECISION DSUM2
      DOUBLE PRECISION DSUM3
      DOUBLE PRECISION DSUM4
      DOUBLE PRECISION DTERM1
      DOUBLE PRECISION DNU
      DOUBLE PRECISION DVI
      DOUBLE PRECISION DS2I
      DOUBLE PRECISION DSI
      DOUBLE PRECISION DAI
      DOUBLE PRECISION DKI
      DOUBLE PRECISION DNUM
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='WSAT'
      ISUBN2='    '
      IWRITE='OFF'
      IERROR='NO'
      WSDF=0.0
      POOLSD=0.0
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSAT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPWSAT--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)N,IFLAG,UNC,IBUGA3,ISUBRO
   52   FORMAT('N,IFLAG,UNC,IBUGA3,ISUBRO = ',2I8,G15.7,2(2X,A4))
        CALL DPWRST('XXX','WRIT')
        DO56I=1,N
          WRITE(ICOUT,57)I,YVAR(I),YNU(I),YA(I)
   57     FORMAT('I,YVAR(I),YNU(I),YA(I) = ',I8,3G15.7)
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSAT')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: WELCH-SATTERTHWAITE DEGREES OF FREEDOM')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF VARIANCES/DEGREES OF FREEDOM  ',
     1         'IS LESS THAN TWO.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      NUMBER OF VARIANCE/DEGREES OF FREEDOM = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ELSEIF(IFLAG.EQ.2 .AND. UNC.LT.0.0)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,107)
  107   FORMAT('      THE COMBINED UNCERTAINTY IS NON-POSITIVE.')
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      DO110I=1,N
        IF(YVAR(I).LT.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,112)I,YVAR(I)
  112     FORMAT('      ROW ',I8,' OF THE VARIANCES IS NEGATIVE ',
     1          '(',G15.7,')')
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GOTO9000
        ELSEIF(YNU(I).LE.0.0)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,101)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,114)I,YNU(I)
  114     FORMAT('      ROW ',I8,' OF THE DEGREES OF FREEDOM IS ',
     1          'NON-POSITIVE (',G15.7,')')
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GOTO9000
        ENDIF
  110 CONTINUE
C
C               ********************************************
C               **  STEP 2---                             **
C               **  COMPUTE THE EFFECTIVE DEGREES OF      **
C               **  FREEDOM AND THE POOLED SD (METHOD 1)  **
C               ********************************************
C
      IF(IFLAG.EQ.1)THEN
C
        ISTEPN='3'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSAT')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        DSUM1=0.0D0
        DSUM2=0.0D0
        DSUM3=0.0D0
        DSUM4=0.0D0
        DO210I=1,N
          DVI=DBLE(YNU(I))
          DS2I=DBLE(YVAR(I))
          DKI=1.0D0/(DVI+1.0D0)
          DSUM1=DSUM1 + DKI*DS2I
          DSUM2=DSUM2 + ((DKI*DS2I)**2)/DVI
          DSUM3=DSUM3 + (DVI-1.0D0)*DS2I
          DSUM4=DSUM4 + (DVI-1.0D0)
  210   CONTINUE
        DNUM=DSUM1**2
        DNU=DNUM/DSUM2
        WSDF=REAL(DNU)
        DTERM1=DSQRT(DSUM3/DSUM4)
        POOLSD=REAL(DTERM1)
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'WSAT')THEN
          WRITE(ICOUT,213)DSUM1,DSUM2,DSUM3
  213     FORMAT('DSUM1,DSUM2.DUSM3 = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,215)DNUM,WSDF,POOLSD
  215     FORMAT('DNUM,WSDF,POOLSD = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
C               ********************************************
C               **  STEP 3---                             **
C               **  COMPUTE THE EFFECTIVE DEGREES OF      **
C               **  FREEDOM (METHOD 2, GUM)               **
C               ********************************************
C
      ELSE
C
        ISTEPN='3'
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSAT')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        DSUM1=0.0D0
        DO301I=1,N
          DAI=DBLE(YA(I))
          DSI=DBLE(YVAR(I))
          DSUM1=DSUM1 + DAI**2*DSI**2
  301   CONTINUE
        DTERM1=DSQRT(DSUM1)
        UNC=REAL(DTERM1)
C
        DTERM1=DTERM1**4
        DSUM1=0.0D0
        DO310I=1,N
          DVI=DBLE(YNU(I))
          DSI=DBLE(YVAR(I))
          DAI=DBLE(YA(I))
          DTERM2=DAI**4*DSI**4/DVI
          DSUM1=DSUM1 + DTERM2
  310   CONTINUE
        DNU=DTERM1/DSUM1
        WSDF=REAL(DNU)
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'WSAT')THEN
          WRITE(ICOUT,313)DSUM1,DTERM1,DNU
  313     FORMAT('DSUM1,DTERM1.DNU = ',3G15.7)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
      ENDIF
C
 9000 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'WSAT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,9011)
 9011   FORMAT('**** AT THE END OF DPWSAT--')
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DPWSHA(XTEMP1,MAXNXT,ICASDI,
     1                  ICAPSW,ICASAN,IFORSW,ISEED,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT EITHER THE WILK-SHAPIRO TEST OR THE
C              JARQUE-BERA TEST FOR NORMALITY.
C     EXAMPLE--WILKS SHAPIRO NORMALITY TEST Y
C            --WILK SHAPIRO TEST Y
C            --JARQUE-BERA TEST Y
C     REFERENCE--XX, "ALGORITHM AS R94 APPL. STATIST.", (1995)
C                VOL.44, NO.4
C     REFERENCE--BRANI VIDAKOVIC (2011), "STATISTICS FOR 
C                BIOENGINEERING SCIENCES: WITH MATLAB AND WINBUGS
C                SUPPORT", SPRINGER, PP. 521-522.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORAOTRY
C                 NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS OF TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/3
C     ORIGINAL VERSION--MARCH     1999.
C     UPDATED         --OCTOBER   2003.
C     UPDATED         --MARCH     2011. USE DPPARS ROUTINE
C     UPATED          --MARCH     2011. REWRITTEN TO HANDLE MULTIPLE
C                                       RESPONSE VARIABLES, GROUP-ID
C                                       VARIABLES, OR A LAB-ID VARIABLE
C     UPATED          --JUNE      2012. JARQUE-BERA NORMALITY TEST
C     UPATED          --JULY      2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASDI
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
      CHARACTER*4 IDATSW
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IREPL
      CHARACTER*4 IMULT
      CHARACTER*4 ICTMP1
      CHARACTER*4 ICTMP2
      CHARACTER*4 ICTMP3
      CHARACTER*4 ICTMP4
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
C
      DIMENSION XTEMP1(*)
C
      DIMENSION Y1(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION XTEMP2(MAXOBV)
C
      DIMENSION XDESGN(MAXOBV,7)
      DIMENSION XIDTEM(MAXOBV)
      DIMENSION XIDTE2(MAXOBV)
      DIMENSION XIDTE3(MAXOBV)
      DIMENSION XIDTE4(MAXOBV)
      DIMENSION XIDTE5(MAXOBV)
      DIMENSION XIDTE6(MAXOBV)
C
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB4),XTEMP2(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB8),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB9),XIDTE3(1))
      EQUIVALENCE (GARBAG(IGAR10),XIDTE4(1))
      EQUIVALENCE (GARBAG(JGAR11),XIDTE5(1))
      EQUIVALENCE (GARBAG(JGAR12),XIDTE6(1))
      EQUIVALENCE (GARBAG(JGAR13),XDESGN(1,1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOS2.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      ICASAN='    '
      IREPL='OFF'
      IMULT='OFF'
      ISUBN1='DPWS'
      ISUBN2='HA  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ***********************************************
C               **  TREAT THE WILK SHAPIRO TEST     CASE     **
C               ************************************************
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSHA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWSHA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASAN,ICASDI,MAXNXT
   52   FORMAT('ICASAN,ICASDI,MAXNXT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGA2,IBUGA3,IBUGQ,ISUBRO
   53   FORMAT('IBUGA2,IBUGA3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************************
C               **  STEP 1--                                           **
C               **  EXTRACT THE COMMAND                                **
C               **  LOOK FOR ONE OF THE FOLLOWING COMMANDS:            **
C               **    1) WILK SHAPIRO TEST Y                           **
C               **    2) MULTIPLE WILK SHAPIRO TEST Y1 ... YK          **
C               **    3) REPLICATED WILK SHAPIRO TEST Y X1 ... XK      **
C               **    4) JARQUE-BERA TEST Y                            **
C               **    5) MULTIPLE JARQUE-BERA TEST Y1 ... YK           **
C               **    6) REPLICATED JARQUE-BERA TEST Y X1 ... XK       **
C               *********************************************************
C
C     NOTE: THE WORD "TEST" IS OPTIONAL.  ALSO, TREAT
C           "WILK SHAPIRO" AND "SHAPIRO WILK" AS SYNONYMS.
C
      ISTEPN='1'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILASTC=9999
      ILASTZ=9999
      IFOUND='NO'
      ICASAN='WSHA'
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
C
        IF(ICTMP1.EQ.'=')THEN
          IFOUND='NO'
          GOTO9000
        ELSEIF(ICTMP1.EQ.'WILK' .AND. ICTMP2.EQ.'SHAP' .AND.
     1         ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'WILK' .AND. ICTMP2.EQ.'SHAP' .AND.
     1         ICTMP3.EQ.'NORM' .AND. ICTMP4.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'SHAP' .AND. ICTMP2.EQ.'WILK' .AND.
     1         ICTMP3.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'JARQ' .AND. ICTMP2.EQ.'BERA' .AND.
     1         ICTMP3.EQ.'TEST')THEN
          ICASAN='JABE'
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+2
        ELSEIF(ICTMP1.EQ.'JARQ' .AND. ICTMP2.EQ.'BERA' .AND.
     1         ICTMP3.EQ.'NORM' .AND. ICTMP4.EQ.'TEST')THEN
          ICASAN='JABE'
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'SHAP' .AND. ICTMP2.EQ.'WILK' .AND.
     1         ICTMP3.EQ.'NORM' .AND. ICTMP4.EQ.'TEST')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+3
        ELSEIF(ICTMP1.EQ.'WILK' .AND. ICTMP2.EQ.'SHAP')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'SHAP' .AND. ICTMP2.EQ.'WILK')THEN
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'JARQ' .AND. ICTMP2.EQ.'BERA')THEN
          ICASAN='JABE'
          IFOUND='YES'
          ILASTC=I
          ILASTZ=I+1
        ELSEIF(ICTMP1.EQ.'REPL')THEN
          IREPL='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ELSEIF(ICTMP1.EQ.'MULT')THEN
          IMULT='ON'
          ILASTC=MIN(ILASTC,I)
          ILASTZ=MAX(ILASTZ,I)
        ENDIF
  100 CONTINUE
C
      ISHIFT=ILASTZ
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1            IBUGA2,IERROR)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')THEN
        WRITE(ICOUT,91)IMULT,IREPL,ISHIFT
   91   FORMAT('DPWSHA: IMULT,IREPL,ISHIFT = ',2(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IFOUND.EQ.'NO')GOTO9000
      IF(IMULT.EQ.'ON')THEN
        IF(IREPL.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,101)
  101     FORMAT('***** ERROR IN WILK SHAPIRO TEST--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,102)
  102     FORMAT('      YOU CANNOT SPECIFY BOTH "MULTIPLE" AND ',
     1           '"REPLICATION" FOR THE WILK SHAPIROS TEST COMMAND.')
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='WILK-SHAPIRO NORMALITY TEST'
      IF(ICASAN.EQ.'JABE') INAME='JARQUE-BERA NORMALITY TEST'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IF(IMULT.EQ.'ON')IFLAGE=0
      IFLAGM=1
      IF(IREPL.EQ.'ON')IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=-99
      MAXNVA=-99
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')THEN
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
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
          WRITE(ICOUT,101)
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')THEN
        WRITE(ICOUT,521)NRESP,NREPL
  521   FORMAT('NRESP,NREPL = ',2I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 6--                                        **
C               **  GENERATE THE WILK SHAPIROS TEST FOR THE VARIOUS **
C               **  CASES                                           **
C               ******************************************************
C
      ISTEPN='6'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ******************************************
C               **  STEP 8A--                           **
C               **  CASE 1: NO REPLICATION VARIABLES    **
C               ******************************************
C
      IF(NREPL.LT.1)THEN
        ISTEPN='8A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
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
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')THEN
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
     1                Y1,XTEMP1,XTEMP1,NLOCAL,NLOCA2,NLOCA3,ICASE,
     1                IBUGA3,ISUBRO,IFOUND,IERROR)
          IF(IERROR.EQ.'YES')GOTO9000
C
C         *****************************************************
C         **  STEP 8B--                                      **
C         *****************************************************
C
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSHA')THEN
            ISTEPN='8B'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,822)
  822       FORMAT('***** FROM THE MIDDLE  OF DPWSHA--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,823)ICASAN,NUMVAR,IDATSW,NLOCAL
  823       FORMAT('ICASAN,NUMVAR,IDATSW,NQ = ',
     1             A4,I8,2X,A4,I8)
            CALL DPWRST('XXX','BUG ')
            IF(NLOCAL.GE.1)THEN
              DO825I=1,NLOCAL
                WRITE(ICOUT,826)I,Y1(I)
  826           FORMAT('I,Y1(I) = ',I8,G15.7)
                CALL DPWRST('XXX','BUG ')
  825         CONTINUE
            ENDIF
          ENDIF
C
          IF(ICASAN.EQ.'JABE')THEN
            CALL DPJAB2(Y1,NLOCAL,
     1                  XTEMP1,XTEMP2,MAXOBV,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  STATVA,PVAL,CDF,
     1                  ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                  ISUBRO,IBUGA3,IERROR)
          ELSE
            CALL DPWSH2(Y1,NLOCAL,
     1                  XTEMP1,MAXOBV,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  STATVA,PVAL,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  ISUBRO,IBUGA3,IERROR)
          ENDIF
C
C               ***************************************
C               **  STEP 8C--                        **
C               **  COMPUTE WILK SHAPIRO     STAT    **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
          ISTEPN='8C'
          IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
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
          CALL DPWSH4(STATVA,PVAL,
     1                IFLAGU,IFRST,ILAST,ICASAN,
     1                IBUGA2,IBUGA3,ISUBRO,IERROR)
C
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
      ELSEIF(IREPL.EQ.'ON')THEN
        ISTEPN='9A'
        IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'WSHA')
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
          ICOLC=1
          IJ=MAXN*(ICOLR(ICOLC)-1)+I
          IF(ICOLR(ICOLC).LE.MAXCOL)Y1(J)=V(IJ)
          IF(ICOLR(ICOLC).EQ.MAXCP1)Y1(J)=PRED(I)
          IF(ICOLR(ICOLC).EQ.MAXCP2)Y1(J)=RES(I)
          IF(ICOLR(ICOLC).EQ.MAXCP3)Y1(J)=YPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP4)Y1(J)=XPLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP5)Y1(J)=X2PLOT(I)
          IF(ICOLR(ICOLC).EQ.MAXCP6)Y1(J)=TAGPLO(I)
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
C       **  CALL DPWSH2 TO PERFORM WILK SHAPIRO TEST.      **
C       *****************************************************
C
C
        IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSHA')THEN
          ISTEPN='9C'
          CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,941)
  941     FORMAT('***** FROM THE MIDDLE  OF DPWSHA--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,942)ICASAN,NUMVAR,IDATSW,NLOCAL,NREPL
  942     FORMAT('ICASAN,NUMVAR,IDATSW,NLOCAL,NREPL = ',
     1           A4,I8,2X,A4,2I8)
          CALL DPWRST('XXX','BUG ')
          IF(NLOCAL.GE.1)THEN
            DO945I=1,NLOCAL
              WRITE(ICOUT,946)I,Y1(I),XDESGN(I,1),XDESGN(I,2)
  946         FORMAT('I,Y1(I),XDESGN(I,1),XDESGN(I,2) = ',
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
     1             XTEMP1,XTEMP2,
     1             NUMSE1,NUMSE2,NUMSE3,NUMSE4,NUMSE5,NUMSE6,
     1             IBUGA3,ISUBRO,IERROR)
C
C       *****************************************************
C       **  STEP 9D--                                      **
C       **  NOW LOOP THROUGH THE VARIOUS REPLICATIONS      **
C       *****************************************************
C
        NPLOTP=0
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
                TEMP1(K)=Y1(I)
              ENDIF
 1130       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'JABE')THEN
                CALL DPJAB2(TEMP1,NTEMP,
     1                      XTEMP1,XTEMP2,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,CDF,
     1                      ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPWSH2(TEMP1,NTEMP,
     1                      XTEMP1,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,
     1                      ICAPSW,ICAPTY,IFORSW,
     1                      ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NUMSE1)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPWSH4(STATVA,PVAL,
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
                TEMP1(K)=Y1(I)
              ENDIF
 1290       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'JABE')THEN
                CALL DPJAB2(TEMP1,NTEMP,
     1                      XTEMP1,XTEMP2,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,CDF,
     1                      ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPWSH2(TEMP1,NTEMP,
     1                      XTEMP1,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,
     1                      ICAPSW,ICAPTY,IFORSW,
     1                      ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPWSH4(STATVA,PVAL,
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
                TEMP1(K)=Y1(I)
              ENDIF
 1390       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'JABE')THEN
                CALL DPJAB2(TEMP1,NTEMP,
     1                      XTEMP1,XTEMP2,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,CDF,
     1                      ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPWSH2(TEMP1,NTEMP,
     1                      XTEMP1,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,
     1                      ICAPSW,ICAPTY,IFORSW,
     1                      ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPWSH4(STATVA,PVAL,
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
                TEMP1(K)=Y1(I)
              ENDIF
 1490       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'JABE')THEN
                CALL DPJAB2(TEMP1,NTEMP,
     1                      XTEMP1,XTEMP2,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,CDF,
     1                      ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPWSH2(TEMP1,NTEMP,
     1                      XTEMP1,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,
     1                      ICAPSW,ICAPTY,IFORSW,
     1                      ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPWSH4(STATVA,PVAL,
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
                TEMP1(K)=Y1(I)
              ENDIF
 1590       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'JABE')THEN
                CALL DPJAB2(TEMP1,NTEMP,
     1                      XTEMP1,XTEMP2,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,CDF,
     1                      ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPWSH2(TEMP1,NTEMP,
     1                      XTEMP1,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,
     1                      ICAPSW,ICAPTY,IFORSW,
     1                      ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPWSH4(STATVA,PVAL,
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
                TEMP1(K)=Y1(I)
              ENDIF
 1690       CONTINUE
            NTEMP=K
            NCURVE=NCURVE+1
            NPLOT1=NPLOTP
            IF(NTEMP.GT.0)THEN
              IF(ICASAN.EQ.'JABE')THEN
                CALL DPJAB2(TEMP1,NTEMP,
     1                      XTEMP1,XTEMP2,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,CDF,
     1                      ICAPSW,ICAPTY,IFORSW,ISEED,IRANAL,
     1                      ISUBRO,IBUGA3,IERROR)
              ELSE
                CALL DPWSH2(TEMP1,NTEMP,
     1                      XTEMP1,MAXOBV,
     1                      PID,IVARN1,IVARN2,NREPL,
     1                      STATVA,PVAL,
     1                      ICAPSW,ICAPTY,IFORSW,
     1                      ISUBRO,IBUGA3,IERROR)
              ENDIF
            ENDIF
            NPLOT2=NPLOTP
            IFLAGU='FILE'
            IFRST=.FALSE.
            ILAST=.FALSE.
            IF(NCURVE.EQ.1)IFRST=.TRUE.
            IF(NCURVE.EQ.NTOT)ILAST=.TRUE.
            NPTEMP=NPLOT2-NPLOT1
            CALL DPWSH4(STATVA,PVAL,
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
C
      IF(IERROR.EQ.'YES')THEN
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,9001)(IANS(I),I=1,MIN(100,IWIDTH))
 9001     FORMAT(100A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSHA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWSHA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWSH2(Y,N,
     1                  XTEMP,MAXNXT,
     1                  PID,IVARID,IVARI2,NREPL,
     1                  STATVA,PVAL,
     1                  ICAPSW,ICAPTY,IFORSW,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT THE WILKS SHAPIRO TEST
C              FOR NORMALITY
C     EXAMPLE--WILKS SHAPIRO NORMALITY TEST Y
C     REFERENCE--XX, "ALGORITHM AS R94 APPL. STATIST.", (1995)
C                VOL.44, NO.4
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--99/3
C     ORIGINAL VERSION--MARCH     1999.
C     UPDATED         --OCTOBER   2003. SUPPORT FOR HTML, LATEX OUTPUT
C     UPDATED         --MARCH     2011. USE DPDTA1 AND DPDTA5 TO PRINT
C                                       TABLES
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IVARID(*)
      CHARACTER*4 IVARI2(*)
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
      DIMENSION PID(*)
C
      PARAMETER (NUMALP=7)
      REAL ALPHA(NUMALP)
C
      PARAMETER(NUMCLI=4)
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
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA ALPHA /50.0, 80.0, 90.0, 95.0, 97.5, 99.0, 99.9/
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWS'
      ISUBN2='H2  '
C
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSH2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPWSH2--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N
   52   FORMAT('IBUGA3,ISUBRO,N = ',2(A4,2X),I8)
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
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'WSH2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: WILKS-SHAPIRPO TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3.',
     1         '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.GT.5000)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** WARNING: FOR WILKS-SHAPIRPO TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE P-VALUE CALCULATION MAY NOT BE ACCURATE ',
     1         'FOR SAMPLE SIZES  GREATER THAN 5,000.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
C               ******************************
C               **  STEP 11--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR WILKS SHAPIRO       **
C               **  TEST                    **
C               ******************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'WSH2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL MEAN(Y,N,IWRITE,YMEAN,IBUGA3,IERROR)
      CALL SD(Y,N,IWRITE,YSD,IBUGA3,IERROR)
      CALL MINIM(Y,N,IWRITE,YMIN,IBUGA3,IERROR)
      CALL MAXIM(Y,N,IWRITE,YMAX,IBUGA3,IERROR)
C
      CALL DPWSH3(Y,N,XTEMP,MAXNXT,
     1            STATVA,PVAL,
     1            ISUBRO,IBUGA3,IERROR)
      CDF=1.0 - PVAL
C
C               *********************************
C               **   STEP 42--                 **
C               **   WRITE OUT EVERYTHING      **
C               **   FOR WILKS SHAPIRO TEST    **
C               *********************************
C
      ISTEPN='42'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'WSH2')
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
      ITITLE='Wilk-Shapiro Test for Normality'
      NCTITL=31
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
        DO4101I=1,NREPL
          ICNT=ICNT+1
          ITEMP=I+IADD
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
      ITEXT(ICNT)='H0: The Data Are Normally Distributed'
      NCTEXT(ICNT)=37
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      ICNT=ICNT+1
      ITEXT(ICNT)='Ha: The Data Are Not Normally Distributed'
      NCTEXT(ICNT)=41
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
      ITEXT(ICNT)='Sample Mean:'
      NCTEXT(ICNT)=12
      AVALUE(ICNT)=YMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Standard Deviation:'
      NCTEXT(ICNT)=26
      AVALUE(ICNT)=YSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Minimum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMIN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Sample Maximum:'
      NCTEXT(ICNT)=15
      AVALUE(ICNT)=YMAX
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Test Statistic Value:'
      NCTEXT(ICNT)=21
      AVALUE(ICNT)=STATVA
      IDIGIT(ICNT)=NUMDIG
CCCCC ICNT=ICNT+1
CCCCC ITEXT(ICNT)='CDF Value:'
CCCCC NCTEXT(ICNT)=10
CCCCC AVALUE(ICNT)=CDF
CCCCC IDIGIT(ICNT)=NUMDIG
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
C
      ISTEPN='42A'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSH2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSH2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ITITL9=' '
      NCTIT9=0
      ITITLE='Conclusions'
      NCTITL=11
C
      DO5030J=1,4
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
      DO5050I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        IF(I.EQ.1)NTOT(I)=12
        IF(I.EQ.3)NTOT(I)=18
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='ALPH'
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
        IWRTF(3)=IWRTF(2)+IINC2
        IWRTF(4)=IWRTF(3)+IINC
C
        DO5060J=1,NUMALP
C
          IVALUE(J,1)='Normal'
          NCVALU(J,1)=6
          IVALUE(J,4)(1:6)='REJECT'
          ALPHAT=ALPHA(J)
          ALPHAT=ALPHAT
          WRITE(IVALUE(J,2)(1:4),'(F4.1)')ALPHAT
          IVALUE(J,2)(5:5)='%'
          NCVALU(J,2)=5
          ALPHAT=1.0 - (ALPHA(J)/100.0)
          IF(PVAL.GE.ALPHAT)THEN
            IVALUE(J,4)(1:6)='ACCEPT'
          ENDIF
          NCVALU(J,4)=6
C
          IF(J.EQ.1)THEN
            IVALUE(J,3)(1:9)='(0.500,1)'
          ELSEIF(J.EQ.2)THEN
            IVALUE(J,3)(1:9)='(0.200,1)'
          ELSEIF(J.EQ.3)THEN
            IVALUE(J,3)(1:9)='(0.100,1)'
          ELSEIF(J.EQ.4)THEN
            IVALUE(J,3)(1:9)='(0.050,1)'
          ELSEIF(J.EQ.5)THEN
            IVALUE(J,3)(1:9)='(0.025,1)'
          ELSEIF(J.EQ.6)THEN
            IVALUE(J,3)(1:9)='(0.010,1)'
          ELSEIF(J.EQ.7)THEN
            IVALUE(J,3)(1:9)='(0.001,1)'
          ENDIF
          NCVALU(J,3)=9
C
 5060   CONTINUE
C
 5050 CONTINUE
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSH2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWSH2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I)
 9017     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWSH3(Y,N,
     1                  XTEMP,MAXNXT,
     1                  STATVA,PVAL,
     1                  ISUBRO,IBUGA3,IERROR)
C
C     PURPOSE--THIS ROUTINE CARRIES OUT THE WILKS SHAPIRO TEST
C              FOR NORMALITY.  EXTRACT FROM DPWSH3 IN ORDER TO
C              ALSO CALL BASIC COMPUTATION FROM CMPSTA.
C     REFERENCE--XX, "ALGORITHM AS R94 APPL. STATIST.", (1995)
C                VOL.44, NO.4
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2011/3
C     ORIGINAL VERSION--MARCH     2011. EXTRACTED FROM DPWSH2
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
      LOGICAL WGTS
C
C---------------------------------------------------------------------
C
      DIMENSION Y(*)
      DIMENSION XTEMP(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPWS'
      ISUBN2='H3  '
      IWRITE='OFF'
      IERROR='NO'
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSH3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,51)
   51   FORMAT('**** AT THE BEGINNING OF DPWSH3--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,N,MAXNXT
   52   FORMAT('IBUGA3,ISUBRO,N,MAXNXT = ',2(A4,2X),2I8)
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
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'WSH3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(N.LT.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)
  101   FORMAT('***** ERROR: WILKS-SHAPIRPO TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,102)
  102   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST 3.',
     1         '  SUCH WAS NOT THE CASE HERE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
  103   FORMAT('      SAMPLE SIZE = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(N.GT.5000)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** WARNING: FOR WILKS-SHAPIRPO TEST--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,112)
  112   FORMAT('      THE P-VALUE CALCULATION MAY NOT BE ACCURATE ',
     1         'FOR SAMPLE SIZES  GREATER THAN 5,000.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,103)N
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
      HOLD=Y(1)
      DO135I=2,N
        IF(Y(I).NE.HOLD)GOTO139
  135 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,101)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,131)HOLD
  131 FORMAT('      THE RESPONSE VARIABLE HAS ALL ELEMENTS = ',G15.7)
      CALL DPWRST('XXX','WRIT')
      GOTO9000
  139 CONTINUE
C
C               ******************************
C               **  STEP 11--               **
C               **  CARRY OUT CALCULATIONS  **
C               **  FOR WILKS SHAPIRO       **
C               **  TEST                    **
C               ******************************
C
      ISTEPN='11'
      IF(IBUGA3.EQ.'ON'.AND.ISUBRO.EQ.'WSH3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      N2=N/2
      WGTS=.FALSE.
      CALL SORT(Y,N,Y)
      STATVA=0.0
      PVAL=1.0
      CALL SWILK(WGTS,Y,N,N,N2,XTEMP,STATVA,PVAL,IFAULT)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'WSH3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPWSH3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)N,IBUGA3,IERROR
 9012   FORMAT('N,IBUGA3,IERROR = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9016I=1,N
          WRITE(ICOUT,9017)I,Y(I)
 9017     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9016   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPWSH4(STATVA,PVAL,
     1                  IFLAGU,IFRST,ILAST,ICASPL,
     1                  IBUGA2,IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--UTILITY ROUTINE USED BY DPWSHA.  THIS ROUTINE
C              UPDATES THE PARAMETERS "STATVAL" AND
C              "PVALUE" AFTER A WILK-SHAPIRO TEST.
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
      CHARACTER*4 ICASPL
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
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOHO.INC'
C
      CHARACTER*4 IOP
      SAVE IOUNI1
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSH4')THEN
        ISTEPN='1'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPWSH4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,STATVA,PVAL
   53   FORMAT('ICASPL,STATVA,PVAL = ',A4,2X,2G15.7)
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
  295     FORMAT(11X,'STATVAL',8X,'PVALUE')
        ENDIF
        WRITE(IOUNI1,299)STATVA,PVAL
  299   FORMAT(2E15.7)
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
        IF(PVAL.NE.CPUMIN)THEN
          IH='PVAL'
          IH2='UE  '
          VALUE0=PVAL
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
          IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSH4')THEN
            ISTEPN='3A'
            CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,301)IERROR
  301       FORMAT('AFTER CALL DPCLFI, IERROR = ',A4)
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
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'WSH4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DPWSH4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPW280(ISTRIN,ISTART,ISTOP,ICOL2,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--DETERMINE THE COLUMN NUMBER
C              AT THE BEGINNING OF THE SECOND WORD
C              IN THE COLUMN INTERVAL ISTART TO ISTOP
C              IN THE CHARACTER*80 VARIABLE    ISTRIN   .
C              (THIS IS USEFUL FOR EXTRACTING THE FULL SECOND WORD.)
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--DECEMBER  1985.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC CHARACTER*80 ISTRIN
      CHARACTER (LEN=*) :: ISTRIN
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPW2'
      ISUBN2='80  '
      IERROR='NO'
C
      ICOL2=(-999)
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'W280')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPW280--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR
   53   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ISTRIN(J:J),J=1,80)
   54   FORMAT('(ISTRIN(J:J),J=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISTART,ISTOP,ICOL2
   55   FORMAT('ISTART,ISTOP,ICOL2 = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE COLUMN NUMBER          **
C               **  AT THE BEGINNING OF THE SECOND WORD  **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'W280')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.GE.1.AND.ISTOP.GE.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN DPW280--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      ISTART OR ISTOP IS < 1. ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)ISTART
 1113   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)ISTOP
 1114   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1116)(ISTRIN(I:I),I=1,80)
 1116   FORMAT('      (ISTRIN(I:I),I=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1121)
 1121   FORMAT('***** ERROR IN DPW280--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      ISTART EXCEEDS ISTOP')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1123)ISTART
 1123   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1124)ISTOP
 1124   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1126)(ISTRIN(I:I),I=1,80)
 1126   FORMAT('      (ISTRIN(I:I),I=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      ICOL1=ISTOP+1
      DO1210I=ISTART,ISTOP
        I2=I
        IF(ISTRIN(I:I).EQ.' ')THEN
          ICOL1=I2
          GOTO1219
        ENDIF
 1210 CONTINUE
      ICOL1=ISTOP+1
 1219 CONTINUE
C
      ICOL2=ISTOP+1
      IF(ICOL1.GT.ISTOP)GOTO1229
      DO1220I=ICOL1,ISTOP
        I2=I
        IF(ISTRIN(I:I).NE.' ')THEN
          ICOL2=I2
          GOTO1229
        ENDIF
 1220 CONTINUE
      ICOL2=ISTOP+1
 1229 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'W280')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPW280--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ISTRIN(J:J),J=1,80)
 9014   FORMAT('(ISTRIN(J:J),J=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IERROR,ISTART,ISTOP,ICOL1,ICOL2
 9015   FORMAT('IERROR,ISTART,ISTOP,ICOL1,ICOL2 = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPW380(ISTRIN,ISTART,ISTOP,ICOL3,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--DETERMINE THE COLUMN NUMBER
C              AT THE BEGINNING OF THE THIRD WORD
C              IN THE COLUMN INTERVAL ISTART TO ISTOP
C              IN THE CHARACTER*80 VARIABLE    ISTRIN   .
C              (THIS IS USEFUL FOR EXTRACTING THE FULL THIRD WORD.)
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--JUNE      1986.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC CHARACTER*80 ISTRIN
      CHARACTER (LEN=*) :: ISTRIN
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPW3'
      ISUBN2='80  '
      IERROR='NO'
C
      ICOL2=(-999)
      ICOL2B=(-999)
      ICOL3=(-999)
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'W380')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPW380--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR
   53   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ISTRIN(J:J),J=1,80)
   54   FORMAT('(ISTRIN(J:J),J=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)ISTART,ISTOP,ICOL2,ICOL2B,ICOL3
   55   FORMAT('ISTART,ISTOP,ICOL2,ICOL2B,ICOL3 = ',5I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 1--                             **
C               **  DETERMINE THE COLUMN NUMBER          **
C               **  AT THE BEGINNING OF THE THIRD  WORD  **
C               *******************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'W380')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ISTART.LT.1 .OR. ISTOP.LT.1)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN DPW380--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      ISTART OR ISTOP IS < 1.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)ISTART
 1113   FORMAT('      ISTART  = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)ISTOP
 1114   FORMAT('      ISTOP   = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1116)(ISTRIN(I:I),I=1,80)
 1116   FORMAT('      (ISTRIN(I:I),I=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
      IF(ISTART.GT.ISTOP)THEN
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1121)
 1121   FORMAT('***** ERROR IN DPW380--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1122)
 1122   FORMAT('      ISTART EXCEEDS ISTOP')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1113)ISTART
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)ISTOP
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1116)(ISTRIN(I:I),I=1,80)
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C     FIND THE FIRST BLANK AT THE END OF WORD 1
C
      ICOL1=ISTOP+1
      DO1210I=ISTART,ISTOP
      I2=I
      IF(ISTRIN(I:I).EQ.' ')GOTO1215
 1210 CONTINUE
      ICOL1=ISTOP+1
      GOTO1219
 1215 CONTINUE
      ICOL1=I2
      GOTO1219
 1219 CONTINUE
C
C     FIND THE BEGINNING OF WORD 2
C
      ICOL2=ISTOP+1
      IF(ICOL1.GT.ISTOP)GOTO1229
      DO1220I=ICOL1,ISTOP
      I2=I
      IF(ISTRIN(I:I).NE.' ')GOTO1225
 1220 CONTINUE
      ICOL2=ISTOP+1
      GOTO1229
 1225 CONTINUE
      ICOL2=I2
      GOTO1229
 1229 CONTINUE
C
C     FIND THE FIRST BLANK AT THE END OF WORD 2
C
      ICOL2B=ISTOP+1
      IF(ICOL2.GT.ISTOP)GOTO1239
      DO1230I=ICOL2,ISTOP
      I2=I
      IF(ISTRIN(I:I).EQ.' ')GOTO1235
 1230 CONTINUE
      ICOL2B=ISTOP+1
      GOTO1239
 1235 CONTINUE
      ICOL2B=I2
      GOTO1239
 1239 CONTINUE
C
C     FIND THE BEGINNING OF WORD 3
C
      ICOL3=ISTOP+1
      IF(ICOL2B.GT.ISTOP)GOTO1249
      DO1240I=ICOL2B,ISTOP
      I2=I
      IF(ISTRIN(I:I).NE.' ')GOTO1245
 1240 CONTINUE
      ICOL3=ISTOP+1
      GOTO1249
 1245 CONTINUE
      ICOL3=I2
      GOTO1249
 1249 CONTINUE
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'W380')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPW380--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ISTRIN(J:J),J=1,80)
 9014   FORMAT('(ISTRIN(J:J),J=1,80) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IERROR,ISTART,ISTOP,ICOL1,ICOL2,ICOL2B,ICOL3
 9015   FORMAT('IERROR,ISTART,ISTOP,ICOL1,ICOL2,ICOL2B,ICOL3 = ',
     1         A4,2X,6I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPXH1H(IWD,ICH,NUMCH,IBUGA3)
C
C     PURPOSE--DECOMPOSE A WORD (TYPICALLY 4 CHARACTERS
C              BUT MORE GENERALLY NUMCPW CHARACTERS PER WORD)
C              INTO INDIVIDUAL CHARACTERS--1 CHARACTER PER WORD.
C
C     NOTE ALSO THE POSSIBLE EXISTENCE OF A6 FORMATS
C     RATHER THAN A4 FORMATS FOR THE PRINTING OF
C     CERTAIN HOLLERITH (= CHARACTER) VARIABLES.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANUARY   1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --JUNE      1981.
C     UPDATED         --JULY      1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IWD
      CHARACTER*4 ICH
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 IX1
      CHARACTER*4 IX2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION ICH(*)
C
C     NUMBPC = NUMBER OF BITS PER CHARACTER.
C     NUMCPW = NUMBER OF CHARACTERS PER WORD.
C     THESE VALUES WILL CHANGE DEPENDING
C     ON THE COMPUTER AND ARE DEFINED IN THE SUBROUTINE INITMC.
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPXH'
      ISUBN2='1H  '
C
C               ****************************************
C               **  DECOMPOSE A WORD INTO CHARACTERS  **
C               ****************************************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,91)
   91   FORMAT('***** AT THE BEGINNING OF DPXH1H--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,92)IWD
   92   FORMAT('IWD (IN A4 FORMAT)  = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,93)IWD
   93   FORMAT('IWD (IN A6 FORMAT)  = ',A6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,94)IWD
   94   FORMAT('IWD (IN A10 FORMAT) = ',A10)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,95)IBUGA3
   95   FORMAT('IBUGA3 = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUGA3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMCH=0
      DO100I=1,NUMCPW
        ICH(I)=' '
  100 CONTINUE
C
C               **************************************************************
C               **  STEP 2--                                                **
C               **  DECOMPOSE THE   TARGET WORDS INTO INDIVIDUAL CHARACTERS.**
C               **************************************************************
C
      IF(IWD.NE.' ')THEN
        IX1=IWD
        ISTAR2=0
        ILEN1=NUMBPC
        ILEN2=ILEN1
C
        DO200I=1,NUMCPW
          IX2=' '
          ISTAR1=(I-1)*NUMBPC
          CALL DPCHEX(ISTAR1,ILEN1,IX1,ISTAR2,ILEN2,IX2)
          ICH(I)=IX2
  200   CONTINUE
C
        K=0
        DO300I=1,NUMCPW
          K=K+1
          IF(ICH(I).EQ.' ')THEN
            NUMCH=K-1
            GOTO390
          ENDIF
  300   CONTINUE
        NUMCH=K
  390   CONTINUE
      ENDIF
C
C               ****************
C               **  STEP 3--  **
C               **  EXIT.     **
C               ****************
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPXH1H--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ISTAR1,ILEN1,IX1
 9012   FORMAT('ISTAR1,ILEN1,IX1 = ',I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ISTAR2,ILEN2,IX2
 9013   FORMAT('ISTAR2,ILEN2,IX2 = ',I8,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NUMCH
 9014   FORMAT('NUMCH = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)(ICH(I),I=1,MIN(120,NUMCH))
 9015   FORMAT('ICH(.) = ',120A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPYACB(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  ISUBRO,IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--GENERATE A YATES CUBE PLOT--
C              THE COMMAND HAS THE FOLLOWING FORMAT:
C                  YATES CUBE PLOT Y X1 X2 X3
C              WHERE X1, X2,  AND X3 ARE RESTRICTED TO HAVING VALUES
C              IN THE (-1,1) INTERVAL.  IF THEY HAVE 2 DISTINCT
C              LEVELS, THESE LEVELS WILL BE CONVERTED TO -1 AND 1.
C     EXAMPLE--YATES CUBE PLOT Y X1 X2 X3
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2000/1
C     ORIGINAL VERSION--JANUARY       2000.
C     UPDATED         --MARCH         2011.
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
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION YRES(MAXOBV)
      DIMENSION X1(MAXOBV)
      DIMENSION X2(MAXOBV)
      DIMENSION X3(MAXOBV)
      DIMENSION TEMP1(MAXOBV)
      DIMENSION TEMP2(MAXOBV)
      DIMENSION TEMP3(MAXOBV)
      DIMENSION TEMP4(MAXOBV)
C
      INCLUDE 'DPCOZZ.INC'
      EQUIVALENCE (GARBAG(IGARB1),YRES(1))
      EQUIVALENCE (GARBAG(IGARB2),X1(1))
      EQUIVALENCE (GARBAG(IGARB3),X2(1))
      EQUIVALENCE (GARBAG(IGARB4),X3(1))
      EQUIVALENCE (GARBAG(IGARB5),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP4(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPYA'
      ISUBN2='CB  '
      ICASPL='YCUB'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE YATES CUBE PLOT CASE    **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPYACB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGG2,IBUGG3,IBUGQ,ISUBRO
   52   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,MAXN,NS
   53   FORMAT('ICASPL,IAND1,IAND2,MAXN,NS = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2.AND.IHARG(1).EQ.'CUBE'.AND.
     1   IHARG(2).EQ.'PLOT')THEN
        IFOUND='YES'
        ILASTC=2
      ELSE
        GOTO9000
      ENDIF
      CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='YATES CUBE PLOT'
      MINNA=1
      MAXNA=100
      MINN2=1
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=4
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
     1            IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')THEN
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            YRES,X1,X2,X3,TEMP1,TEMP1,TEMP1,N,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,5001)N,ICASPL
 5001   FORMAT('N,ICASPL=',I8,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL DPYAC2(YRES,X1,X2,X3,N,ICASPL,NUMV2,
     1            TEMP1,TEMP2,TEMP3,TEMP4,
     1            Y,X,D,X3D,DSIZE,
     1            N2,NPLOTV,IBUGG3,ISUBRO,IERROR)
      NPLOTP=N2
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'YACB')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPYACB--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,N,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,N,ICASPL,IAND1,IAND2 = ',
     1         3I8,2X,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPYAC2(YRES,X1,X2,X3,N,ICASPL,NUMV2,
     1                  TEMP1,TEMP2,TEMP3,TEMP4,
     1                  Y,X,D,X3D,DSIZE,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GENERATE A PAIR OF COORDINATE VECTORS
C              THAT WILL DEFINE
C              A YATES CUBE PLOT
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2000/12
C     ORIGINAL VERSION--JANUARY   2000.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
CCCCC CHARACTER*4 ICONC
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      DIMENSION YRES(*)
      DIMENSION X1(*)
      DIMENSION X2(*)
      DIMENSION X3(*)
C
      DIMENSION TEMP1(*)
      DIMENSION TEMP2(*)
      DIMENSION TEMP3(*)
      DIMENSION TEMP4(*)
C
      DIMENSION Y(*)
      DIMENSION X(*)
      DIMENSION D(*)
      DIMENSION X3D(*)
      DIMENSION DSIZE(*)
C
      DIMENSION V1(8)
      DIMENSION V2(8)
      DIMENSION V3(8)
      DIMENSION AX1OF(8)
      DIMENSION AX2OF(8)
      DIMENSION AX3OF(8)
      DIMENSION ZX1(8)
      DIMENSION ZX2(8)
      DIMENSION ZX3(8)
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA V1 /-1.0, +1.0, -1.0, +1.0, -1.0, +1.0, -1.0, +1.0 /
      DATA V2 /-1.0, -1.0, +1.0, +1.0, -1.0, -1.0, +1.0, +1.0 /
      DATA V3 /-1.0, -1.0, -1.0, -1.0, +1.0, +1.0, +1.0, +1.0 /
      DATA AX1OF /-0.1,  0.1, -0.1,  0.1,  0.1,  0.1,  0.1,  0.1 /
      DATA AX2OF / 0.1,  0.1,  0.0,  0.0,  0.2,  0.0,  0.0,  0.0 /
      DATA AX3OF / 0.0, -0.1,  0.1, -0.1,  0.2,  0.1,  0.1,  0.0 /
      DATA ZX1   / 0.0,  1.0,  0.0,  1.0,  0.0,  1.0,  0.0,  1.0 /
      DATA ZX2   / 1.0,  1.0,  1.0,  1.0,  0.0,  0.0,  0.0,  0.0 /
      DATA ZX3   / 0.0,  0.0,  1.0,  1.0,  0.0,  0.0,  1.0,  1.0 /
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPYA'
      ISUBN2='C2  '
      IWRITE='OFF'
C
      IERROR='NO'
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
   31   FORMAT('***** ERROR IN YATES CUBE PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,32)
   32   FORMAT('      THE NUMBER OF OBSERVATIONS IS NON-POSITIVE.')
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
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'YAC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPYAC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)ICASPL,N,N2,NPLOTV,NUMV2
   72   FORMAT('ICASPL,N,N2,NPLOTV,NUMV2 = ',A4,2X,4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ****************************************
C               **  STEP 4--                          **
C               **  STEP THROUGH EACH FACTOR VARIABLE **
C               **  AND DETERMINE IF THERE ARE 2      **
C               **  DISTINCT ELEMENTS.                **
C               ****************************************
C
      IERROR='NO'
      CALL DISTIN(X1,N,IWRITE,TEMP4,N1,IBUGG3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(N1.NE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,103)
  103   FORMAT('      FIRST FACTOR VARIABLE HAS ',I8,' DISTINCT ',
     1         'VALUES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        IF(TEMP4(1).LE.TEMP4(2))THEN
          ALOW=TEMP4(1)
          AHIGH=TEMP4(2)
        ELSE
          ALOW=TEMP4(2)
          AHIGH=TEMP4(1)
        ENDIF
        DO109I=1,N
          TEMP1(I)=-1.0
          ATEMP=ABS(X1(I)-AHIGH)
          IF(ATEMP.LE.0.0005)TEMP1(I)=1.0
  109   CONTINUE
      ENDIF
C
      IERROR='NO'
      CALL DISTIN(X2,N,IWRITE,TEMP4,N2,IBUGG3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(N2.NE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,113)
  113   FORMAT('      SECOND FACTOR VARIABLE HAS ',I8,' DISTINCT ',
     1         'VALUES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        IF(TEMP4(1).LE.TEMP4(2))THEN
          ALOW=TEMP4(1)
          AHIGH=TEMP4(2)
        ELSE
          ALOW=TEMP4(2)
          AHIGH=TEMP4(1)
        ENDIF
        DO119I=1,N
          TEMP2(I)=-1.0
          ATEMP=ABS(X2(I)-AHIGH)
          IF(ATEMP.LE.0.0005)TEMP2(I)=1.0
  119   CONTINUE
      ENDIF
C
      IERROR='NO'
      CALL DISTIN(X3,N,IWRITE,TEMP4,N3,IBUGG3,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(N3.NE.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,31)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,123)
  123   FORMAT('      THIRD FACTOR VARIABLE HAS ',I8,' DISTINCT ',
     1         'VALUES.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ELSE
        IF(TEMP4(1).LE.TEMP4(2))THEN
          ALOW=TEMP4(1)
          AHIGH=TEMP4(2)
        ELSE
          ALOW=TEMP4(2)
          AHIGH=TEMP4(1)
        ENDIF
        DO129I=1,N
          TEMP3(I)=-1.0
          ATEMP=ABS(X3(I)-AHIGH)
          IF(ATEMP.LE.0.0005)TEMP3(I)=1.0
  129   CONTINUE
      ENDIF
C
C               ****************************************
C               **  STEP 5--                          **
C               **  LOOP THROUGH 8 POTENTIAL VERTICES **
C               **    -1 -1 -1                        **
C               **    -1 -1  1                        **
C               **    -1  1 -1                        **
C               **    -1  1  1                        **
C               **     1 -1 -1                        **
C               **     1 -1  1                        **
C               **     1  1 -1                        **
C               **     1  1  1                        **
C               **  AND COMPUTE PLOT POINTS           **
C               ****************************************
C
      ATOL=0.0005
      NPLOTP=0
      ITAG=0
      DO200I=1,8
        AX1=V1(I)
        AX2=V2(I)
        AX3=V3(I)
        NMTCH=0
        DO210J=1,N
          IF(AX1.EQ.TEMP1(J).AND.AX2.EQ.TEMP2(J).AND.AX3.EQ.TEMP3(J))
     1      THEN
            NMTCH=NMTCH+1
            TEMP4(NMTCH)=YRES(J)
          ENDIF
  210   CONTINUE
        IF(NMTCH.GT.0)THEN
          CALL MEAN(TEMP4,NMTCH,IWRITE,AMU,IBUGG3,IERROR)
CCCCC     ITAG=ITAG+1
          NPLOTP=NPLOTP+1
          X(NPLOTP)=ZX1(I)+AX1OF(I)
          X3D(NPLOTP)=ZX2(I)+AX2OF(I)
          Y(NPLOTP)=ZX3(I)+AX3OF(I)
          D(NPLOTP)=REAL(ITAG)
          DSIZE(NPLOTP)=AMU
        ENDIF 
  200 CONTINUE
C
C               ****************************************
C               **  STEP 5--                          **
C               **  GENERATE THE 6 FACES OF THE CUBE **
C               ****************************************
      ITAG=ITAG+1
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
C
      ITAG=ITAG+1
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
C
      ITAG=ITAG+1
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
C
      ITAG=ITAG+1
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
C
      ITAG=ITAG+1
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=0.0
      D(NPLOTP)=REAL(ITAG)
C
      ITAG=ITAG+1
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=1.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=1.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
      NPLOTP=NPLOTP+1
      X(NPLOTP)=0.0
      X3D(NPLOTP)=0.0
      Y(NPLOTP)=1.0
      D(NPLOTP)=REAL(ITAG)
C
      NPLOTV=3
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'YAC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPYAC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ICASPL,N,N2,IERROR
 9012   FORMAT('ICASPL,N,N2,IERROR = ',A4,2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        DO9035I=1,NPLOTP
          WRITE(ICOUT,9036)I,X(I),X3D(I),Y(I),D(I),DSIZE(I)
 9036     FORMAT('I,X(I),X3D(I),Y(I),D(I),DSIZE(I) = ',I5,5F10.5)
          CALL DPWRST('XXX','BUG ')
 9035   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPYACU(IHARG,IARGT,ARG,NUMARG,
     1YATCCU,YATTCU,YATRCU,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE YATES COEF/T/RESSD CUTOFF
C              THE SPECIFIED YATES COEF CUTOFF WILL BE PLACED
C              IN THE FLOATING POINT VARIABLES
C              YATCCU,YATTCU,YATRCU   RESPECTIVELY.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --IARGT  (A  HOLLERITH VECTOR)
C                     --ARG    (A FLOATING POINT VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--YATCCU (A FLOATING POINT VARIABLE)
C                       YATTCU (A FLOATING POINT VARIABLE)
C                       YATRCU (A FLOATING POINT VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/12
C     ORIGINAL VERSION--NOVEMBER  1989.
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
      IF(NUMARG.LE.1)GOTO9000
      IF(IHARG(1).EQ.'COEF')GOTO1110
      IF(IHARG(1).EQ.'T')GOTO1110
      IF(IHARG(1).EQ.'RESS')GOTO1110
      GOTO9000
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'CUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IARGT(NUMARG).EQ.'NUMB')GOTO1160
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      GOTO1120
C
 1120 CONTINUE
      IERROR='YES'
      WRITE(ICOUT,1121)
 1121 FORMAT('***** ERROR IN DPYACU--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1122)
 1122 FORMAT('      ILLEGAL FORM FOR YATES ... CUTOFF ',
     1'COMMAND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1124)
 1124 FORMAT('      EXAMPLES OF ALLOWABLE FORMS--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1131)
 1131 FORMAT('          YATES COEF  CUTOFF 10')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)
 1132 FORMAT('          YATES T     CUTOFF 1')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1133)
 1133 FORMAT('          YATES RESSD CUTOFF .5')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1134)
 1134 FORMAT('      THE DEFAULT YATES COEF  CUTOFF ',
     1'IS INFINITY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1135)
 1135 FORMAT('      THE DEFAULT YATES T     CUTOFF ',
     1'IS INFINITY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1136)
 1136 FORMAT('      THE DEFAULT YATES RESSD CUTOFF ',
     1'IS INFINITY')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 1150 CONTINUE
      HOLD=CPUMAX
      GOTO1180
C
 1160 CONTINUE
      HOLD=ARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IF(IHARG(1).EQ.'COEF')YATCCU=HOLD
      IF(IHARG(1).EQ.'T')YATTCU=HOLD
      IF(IHARG(1).EQ.'RESS')YATRCU=HOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'COEF')
     1WRITE(ICOUT,1181)YATCCU
 1181 FORMAT('THE YATES COEFFICIENT CUTOFF HAS JUST BEEN SET TO ',
     1E15.7)
      IF(IHARG(1).EQ.'COEF')
     1CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'T')
     1WRITE(ICOUT,1182)YATTCU
 1182 FORMAT('THE YATES T-VALUE CUTOFF HAS JUST BEEN SET TO ',
     1E15.7)
      IF(IHARG(1).EQ.'T')
     1CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'RESS')
     1WRITE(ICOUT,1183)
 1183 FORMAT('THE YATES RESIDUAL STANDARD DEVIATION CUTOFF ')
      IF(IHARG(1).EQ.'RESS')
     1CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'RESS')
     1WRITE(ICOUT,1184)YATRCU
 1184 FORMAT('HAS JUST BEEN SET TO ', E15.7)
      IF(IHARG(1).EQ.'RESS')
     1CALL DPWRST('XXX','BUG ')
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
C
      IF(IHARG(1).EQ.'COEF')
     1WRITE(ICOUT,8111)YATCCU
 8111 FORMAT('THE CURRENT YATES COEFFICIENT CUTOFF IS ',E15.7)
      IF(IHARG(1).EQ.'COEF')
     1CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'COEF')
     1WRITE(ICOUT,8112)
 8112 FORMAT('THE DEFAULT YATES COEFFICIENT CUTOFF IS INFINITY')
      IF(IHARG(1).EQ.'COEF')
     1CALL DPWRST('XXX','BUG ')
C
      IF(IHARG(1).EQ.'T')
     1WRITE(ICOUT,8113)YATTCU
 8113 FORMAT('THE CURRENT YATES T-VALUE CUTOFF IS ',E15.7)
      IF(IHARG(1).EQ.'T')
     1CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'T')
     1WRITE(ICOUT,8114)
 8114 FORMAT('THE DEFAULT YATES T-VALUE CUTOFF IS INFINITY')
      IF(IHARG(1).EQ.'T')
     1CALL DPWRST('XXX','BUG ')
C
      IF(IHARG(1).EQ.'RESS')
     1WRITE(ICOUT,8115)YATRCU
 8115 FORMAT('THE CURRENT YATES RES. SD. CUTOFF IS ',E15.7)
      IF(IHARG(1).EQ.'RESS')
     1CALL DPWRST('XXX','BUG ')
      IF(IHARG(1).EQ.'RESS')
     1WRITE(ICOUT,8116)
 8116 FORMAT('THE DEFAULT YATES RES. SD. CUTOFF IS INFINITY')
      IF(IHARG(1).EQ.'RESS')
     1CALL DPWRST('XXX','BUG ')
C
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPYAOU(IHARG,NUMARG,
     1IYATOS,IFOUND,IERROR)
C
C     PURPOSE--DEFINE THE YATES COEF/T/RESSD CUTOFF
C              THE SPECIFIED YATES COEF CUTOFF WILL BE PLACED
C              IN THE CHARACTER VARIABLE IYATOS.
C     INPUT  ARGUMENTS--IHARG  (A  HOLLERITH VECTOR)
C                     --NUMARG (AN INTEGER VARIABLE)
C     OUTPUT ARGUMENTS--IYATOS (A CHARACTER VARIABLE)
C                     --IFOUND ('YES' OR 'NO' )
C                     --IERROR ('YES' OR 'NO' )
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/12
C     ORIGINAL VERSION--NOVEMBER  1989.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IHARG
      CHARACTER*4 IYATOS
      CHARACTER*4 IHOLD
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
      IF(NUMARG.LE.0)GOTO9000
      IF(IHARG(1).EQ.'OUTP')GOTO1110
      GOTO9000
C
 1110 CONTINUE
      IF(IHARG(NUMARG).EQ.'OUTP')GOTO1150
      IF(IHARG(NUMARG).EQ.'ON')GOTO1150
      IF(IHARG(NUMARG).EQ.'OFF')GOTO1150
      IF(IHARG(NUMARG).EQ.'AUTO')GOTO1150
      IF(IHARG(NUMARG).EQ.'DEFA')GOTO1150
      IF(IHARG(NUMARG).EQ.'?')GOTO8100
      GOTO1160
C
 1150 CONTINUE
      IHOLD='123'
      GOTO1180
C
 1160 CONTINUE
      IHOLD=IHARG(NUMARG)
      GOTO1180
C
 1180 CONTINUE
      IFOUND='YES'
      IYATOS=IHOLD
C
      IF(IFEEDB.EQ.'OFF')GOTO1189
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)IYATOS
 1181 FORMAT('THE YATES SWITCH HAS JUST BEEN SET TO ',A4)
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
C
      WRITE(ICOUT,8111)IYATOS
 8111 FORMAT('THE CURRENT   YATES OUTPUT SWITCH IS ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8112)
 8112 FORMAT('THE DEFAULT   YATES OUTPUT SWITCH IS 123')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8113)
 8113 FORMAT('THE ALLOWABLE YATES OUTPUT SWITCH SETTINGS ARE')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8121)
 8121 FORMAT('    1     TO PRINT YATES OUTPUT SECTION 1 ONLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8122)
 8122 FORMAT('    2     TO PRINT YATES OUTPUT SECTION 2 ONLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8123)
 8123 FORMAT('    3     TO PRINT YATES OUTPUT SECTION 3 ONLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8124)
 8124 FORMAT('    12    TO PRINT YATES OUTPUT SECTIONS 1 & 2 ONLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8125)
 8125 FORMAT('    13    TO PRINT YATES OUTPUT SECTIONS 1 & 3 ONLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8126)
 8126 FORMAT('    23    TO PRINT YATES OUTPUT SECTIONS 2 & 3 ONLY')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8127)
 8127 FORMAT('    123   TO PRINT ALL 3 YATES OUTPUT SECTIONS')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPYATE(ICASAN,
     1                  ICAPSW,IFORSW,
     1                  IBUGA2,IBUGA3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CARRY OUT A YATES ANALYSIS
C              (USEFUL FOR COMPUTING THE EFFECTS IN A 2**K
C              AND A 2**(K-P) EXPERIMENT)
C     NOTE--THIS CODE ASSUMES THE DATA IS IN
C           STANDARD YATES/HUNTER/BOX ORDER.
C           FOR EXAMPLE, FOR A 2**3--
C                 - - -
C                 + - -
C                 - + -
C                 + + -
C                 - - +
C                 + - +
C                 - + +
C                 + + +
C     NOTE--IF HAVE REPLICATION, THEN THE REPLICATES
C           MAY EITHER BE IMMEDIATELY WITHIN
C           OR MAY BE IN BLOCKS AFTER.
C     EXAMPLE--YATES Y
C              YATES Y SET
C              YATES ANALYSIS Y
C              YATES ANALYSIS Y SET
C              DEX FIT Y
C              DEX FIT Y REP
C              2**K DEX FIT Y
C              2**K DEX FIT Y REP
C              + OTHER COMBINATIONS OF SYNONYMS
C     NOTE--IF THERE ARE NO REPLICATIONS IN THE DATA,
C           THEN THIS COMMAND TAKES 1 ARGUMENT.
C           IF HAVE REPLCATION,
C           THEN THIS COMMAND TAKES 2 ARGUMENTS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JUNE      1987.
C     UPDATED         --JUNE      1989.  SYNONYM = (2**K) DEX FIT
C     UPDATED         --NOVEMBER  1989.  SELECTIVE PRINTING OF COEF
C     UPDATED         --JUNE      1990.  TEMPORARY ARRAYS TO GARBAGE COMMON
C                                        MOVE SOME DPYAT2 DIMENSIONS TO DPYATE
C     UPDATED         --NOVEMBER  1991.  ALLOW 2**1 ANALYSIS
C     UPDATED         --APRIL     1992. DEFINE CUTOFF
C     UPDATED         --APRIL     1992. DELETE MAXNPP
C     UPDATED         --APRIL     1992. DELETE NPLOTP,X(.),Y(.),D(.)
C     UPDATED         --OCTOBER   2003. SUPPORT FOR HTML, LATEX OUTPUT
C     UPDATED         --APRIL     2011. USE DPPARS AND DPPAR3
C     UPDATED         --SEPTEMBER 2012. FOLD IN PHD ANALYSIS (THIS DOES
C                                       YATES AND THEN SOME EXTRA)
C     UPDATED         --JULY      2019. TWEAK SCRATCH SPACE
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASAN
C
CCCCC THE FOLLOWING LINE WAS COMMENTED OUT   APRIL 1992
CCCCC CHARACTER*4 IANGLU
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGA2
      CHARACTER*4 IBUGA3
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IPHDFL
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
CCCCC THE FOLLOWING LINE WAS ADDED   APRIL 1992 (ALAN)
      INCLUDE 'DPCOHO.INC'
CCCCC FOLLOWING LINES ADDED JUNE, 1990
      INCLUDE 'DPCOZZ.INC'
      INCLUDE 'DPCOZI.INC'
C
      DIMENSION Y1(MAXOBV)
      DIMENSION Y2(MAXOBV)
C
      DIMENSION COEF(MAXOBV)
      DIMENSION SSQCOE(MAXOBV)
      DIMENSION TCOEF(MAXOBV)
      DIMENSION RSDCOE(MAXOBV)
      DIMENSION TAGCOE(MAXOBV)
      DIMENSION TAGCO2(MAXOBV)
C
      DIMENSION REPD(MAXOBV)
      DIMENSION RSDCOC(MAXOBV)
      DIMENSION YMEAN(MAXOBV)
      DIMENSION YVAR(MAXOBV)
      DIMENSION DUMMY(MAXOBV)
      DIMENSION DUMMY2(MAXOBV)
      DIMENSION AINDEX(MAXOBV)
      DIMENSION AINDE2(MAXOBV)
      DIMENSION TEMP(MAXOBV)
C
      DIMENSION IFLAG(MAXOBV)
      DIMENSION ITAG(MAXOBV)
      DIMENSION ITAGCO(MAXOBV)
      DIMENSION PHD1(MAXOBV)
      DIMENSION PHD2(MAXOBV)
      DIMENSION PHD3(MAXOBV)
      DIMENSION PHD4(MAXOBV)
      DIMENSION PHD5(MAXOBV)
      DIMENSION RESLIN(MAXOBV)
      DIMENSION PREDLIN(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Y1(1))
      EQUIVALENCE (GARBAG(IGARB2),Y2(1))
      EQUIVALENCE (GARBAG(IGARB3),COEF(1))
      EQUIVALENCE (GARBAG(IGARB4),SSQCOE(1))
      EQUIVALENCE (GARBAG(IGARB5),TCOEF(1))
      EQUIVALENCE (GARBAG(IGARB6),RSDCOE(1))
      EQUIVALENCE (GARBAG(IGARB7),TAGCOE(1))
      EQUIVALENCE (GARBAG(IGARB8),TAGCO2(1))
      EQUIVALENCE (GARBAG(IGARB9),REPD(1))
      EQUIVALENCE (GARBAG(IGAR10),RSDCOC(1))
      EQUIVALENCE (GARBAG(JGAR11),YMEAN(1))
      EQUIVALENCE (GARBAG(JGAR12),YVAR(1))
      EQUIVALENCE (GARBAG(JGAR13),DUMMY(1))
      EQUIVALENCE (GARBAG(JGAR14),DUMMY2(1))
      EQUIVALENCE (GARBAG(JGAR15),AINDEX(1))
      EQUIVALENCE (GARBAG(JGAR16),AINDE2(1))
      EQUIVALENCE (GARBAG(JGAR17),TEMP(1))
      EQUIVALENCE (GARBAG(JGAR18),PHD1(1))
      EQUIVALENCE (GARBAG(JGAR19),PHD2(1))
      EQUIVALENCE (GARBAG(JGAR20),PHD3(1))
      EQUIVALENCE (GARBAG(IGAR11),PHD4(1))
      EQUIVALENCE (GARBAG(IGAR12),PHD5(1))
      EQUIVALENCE (GARBAG(IGAR13),RESLIN(1))
      EQUIVALENCE (GARBAG(IGAR14),PREDLIN(1))
C
      EQUIVALENCE (IGARBG(IIGAR1),IFLAG(1))
      EQUIVALENCE (IGARBG(IIGAR2),ITAG(1))
      EQUIVALENCE (IGARBG(IIGAR3),ITAGCO(1))
CCCCC END CHANGE
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
CCCCC THE FOLLOWING LINE WAS INSERTED NOVEMBER 1989
      INCLUDE 'DPCODE.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPYA'
      ISUBN2='TE  '
      IFOUND='NO'
      IERROR='NO'
      IWRITE='YES'
      IPHDFL='OFF'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
      IVAL=0
      ILASTC=0
C
CCCCC THE FOLLOWING 4 LINES WERE ADDED   APRIL 1992 (ALAN)
      ICUTMX=NUMBPW
      IF(IHOST1.EQ.'CDC '.OR.IHOST1.EQ.'CYBE')ICUTMX=48
      IF(IHOST1.EQ.'205 ')ICUTMX=48
      CUTOFF=2**(ICUTMX-3)
C
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'YATE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPYATE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)ICASAN,IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXN
   54   FORMAT('ICASAN,IBUGA2,IBUGA3,IBUGQ,ISUBRO,MAXN = ',5(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)YATCCU,YATTCU,YATRCU,IYATOS,IYATRS
   61   FORMAT('YATCCU,YATTCU,YATRCU,IYATOS,IYATRS = ',3G15.7,
     1         2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *********************************************
C               **  TREAT THE YATES ANALYSIS CASE  AND THE **
C               **            PHD   ANALYSIS CASE          **
C               *********************************************
C
C               ***************************
C               **  STEP 11--            **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='11'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'YATE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'DEX' .AND. IHARG(1).EQ.'PHD ')THEN
        IF(NUMARG.GE.2 .AND. IHARG(2).EQ.'ANAL')THEN
          ILASTC=2
          IPHDFL='ON'
        ELSEIF(NUMARG.GE.2 .AND. IHARG(2).EQ.'FIT')THEN
          ILASTC=2
          IPHDFL='ON'
        ELSE
          IPHDFL='ON'
          ILASTC=1
        ENDIF
      ELSEIF(ICOM.EQ.'PHD')THEN
        IF(NUMARG.GE.2.AND.
     1    IHARG(1).EQ.'DEX' .AND. IHARG(2).EQ.'ANAL')THEN
          IPHDFL='ON'
          ILASTC=2
        ELSEIF(NUMARG.GE.2 .AND. IHARG(1).EQ.'DEX' .AND.
     1    IHARG(2).EQ.'FIT')THEN
          IPHDFL='ON'
          ILASTC=2
        ELSEIF(NUMARG.GE.1 .AND. IHARG(1).EQ.'DEX')THEN
          IPHDFL='ON'
          ILASTC=2
        ENDIF
      ELSE
C
        IF(NUMARG.GE.1.AND.
     1    IHARG(1).EQ.'ANAL' .AND. IHARG2(1).EQ.'YSIS')THEN
          ILASTC=1
        ELSEIF(NUMARG.GE.1 .AND. IHARG(1).EQ.'FIT')THEN
          ILASTC=1
        ELSEIF(NUMARG.GE.2 .AND. IHARG(1).EQ.'DEX' .AND.
     1    IHARG(2).EQ.'FIT')THEN
          ILASTC=2
        ELSE
          ILASTC=0
        ENDIF
      ENDIF
C
      IF(ILASTC.GT.0)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
      IFOUND='YES'
      ICASAN='DEXF'
C
C               ****************************************
C               **  STEP 2--                          **
C               **  EXTRACT THE VARIABLE LIST         **
C               ****************************************
C
      ISTEPN='2'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'YATE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='YATES ANALYSIS'
      MINNA=1
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      MINNVA=1
      MAXNVA=2
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
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'YATE')THEN
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
C               ***************************************
C               **  STEP 41--                        **
C               **  CARRY OUT THE YATES ANALYSIS     **
C               ***************************************
C
      ISTEPN='41'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'YATE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      CALL DPPAR3(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Y1,Y2,Y2,NS,NLOCAL,NLOCAL,ICASE,
     1            IBUGA3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(NUMVAR.EQ.1)THEN
        DO410I=1,NS
          Y2(I)=1.0
  410   CONTINUE
      ENDIF
C
      CALL DPYAT2(Y1,Y2,NS,ICASAN,MAXN,IWRITE,
     1            YATCCU,YATTCU,YATRCU,IYATOS,IYATRS,
     1            COEF,SSQCOE,TCOEF,RSDCOE,TAGCOE,TAGCO2,NCOEF,
     1            PRESSD,PRESDF,REPSD,REPDF,REFSD,REFDF,SDCOEF,
     1            REPD,IFLAG,RSDCOC,ITAG,ITAGCO,YMEAN,
     1            YVAR,DUMMY,DUMMY2,AINDEX,AINDE2,TEMP,
     1            PHD1,PHD2,PHD3,PHD4,PHD5,RESLIN,PREDLIN,
     1            ICAPSW,ICAPTY,IFORSW,IPHDFL,
     1            IBUGA3,ISUBRO,IERROR)
C
C               ***************************************
C               **  STEP 51--                        **
C               **  UPDATE INTERNAL DATAPLOT TABLES  **
C               ***************************************
C
      ISTEPN='51'
      IF(IBUGA2.EQ.'ON'.OR.ISUBRO.EQ.'YATE')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      NPASS=7
      DO5100IPASS=1,NPASS
        IF(IPASS.EQ.1)THEN
          IH='PRES'
          IH2='SD  '
        ELSEIF(IPASS.EQ.2)THEN
          IH='PRES'
          IH2='DF  '
        ELSEIF(IPASS.EQ.3)THEN
          IH='REPS'
          IH2='D  '
        ELSEIF(IPASS.EQ.4)THEN
          IH='REPD'
          IH2='F   '
        ELSEIF(IPASS.EQ.5)THEN
          IH='REFS'
          IH2='D  '
        ELSEIF(IPASS.EQ.6)THEN
          IH='REFD'
          IH2='F   '
        ELSEIF(IPASS.EQ.7)THEN
          IH='SDCO'
          IH2='EF  '
        ENDIF
C
        DO5150I=1,NUMNAM
          I2=I
C
C         PARAMETER NAME IN NAME TABLE
C
          IF(IH.EQ.IHNAME(I).AND.IH2.EQ.IHNAM2(I).AND.
     1       IUSE(I).EQ.'P')THEN
            ILOC=I2
            GOTO5170
          ENDIF
 5150   CONTINUE
C
C       PARAMETER NAME NOT IN NAME TABLE
C
        IF(NUMNAM.GE.MAXNAM)THEN
          WRITE(ICOUT,5151)
 5151     FORMAT('***** ERROR IN YATES ANALYSIS--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5152)
 5152     FORMAT('      THE TOTAL NUMBER OF (VARIABLE + PARAMETER) ',
     1           'NAMES MUST')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5154)MAXNAM
 5154     FORMAT('      BE AT MOST ',I8,'.  SUCH WAS NOT THE CASE ',
     1           'HERE--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5155)
 5155     FORMAT('      THE MAXIMUM ALLOWABLE NUMBER OF NAMES HAS ',
     1           'JUST BEEN EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5157)
 5157     FORMAT('      SUGGESTED ACTION--ENTER     STAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5158)
 5158     FORMAT('      TO DETERMINE THE IMPORTANT (VERSUS ',
     1           'UNIMPORTANT) VARIABLES AND')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5160)
 5160     FORMAT('      PARAMETERS, AND THEN REUSE SOME OF THE NAMES.')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5162)
 5162     FORMAT('      THE ENTERED COMMAND LINE WAS AS FOLLOWS--')
          CALL DPWRST('XXX','BUG ')
          IF(IWIDTH.GE.1)THEN
            WRITE(ICOUT,5163)(IANS(I),I=1,MIN(80,IWIDTH))
 5163       FORMAT('      ',80A1)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IERROR='YES'
          GOTO9000
        ENDIF
C
        NUMNAM=NUMNAM+1
        ILOC=NUMNAM
C
 5170   CONTINUE
C
        IHNAME(ILOC)=IH
        IHNAM2(ILOC)=IH2
        IUSE(ILOC)='P'
        IF(IPASS.EQ.1)VALUE(ILOC)=PRESSD
        IF(IPASS.EQ.2)VALUE(ILOC)=PRESDF
        IF(IPASS.EQ.3)VALUE(ILOC)=REPSD
        IF(IPASS.EQ.4)VALUE(ILOC)=REPDF
        IF(IPASS.EQ.5)VALUE(ILOC)=REFSD
        IF(IPASS.EQ.6)VALUE(ILOC)=REFDF
        IF(IPASS.EQ.7)VALUE(ILOC)=SDCOEF
        VAL=VALUE(ILOC)
        IF((-CUTOFF).LE.VAL.AND.VAL.LE.CUTOFF)IVAL=INT(VAL+0.5)
        IF(VAL.GT.CUTOFF)IVAL=INT(CUTOFF)
        IF(VAL.LT.(-CUTOFF))IVAL=INT(-CUTOFF)
        IVALUE(ILOC)=IVAL
        GOTO5100
C
C
 5100 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA2.EQ.'ON' .OR. ISUBRO.EQ.'YATE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPYATE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR,ICASAN
 9012   FORMAT('IFOUND,IERROR,ICASAN = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)MAXN,NUMVAR,NLOCAL,NQ,MINN2,ICOUNT
 9014   FORMAT('MAXN,NUMVAR,NLOCAL,NQ,MINN2,ICOUNT = ',6I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9061)IHRI11,IHRI12,IHRI21,IHRI22
 9061   FORMAT('IHRI11,IHRI12,IHRI21,IHRI22 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
CCCCC   THE FOLLOWING 2 LINES WERE INSERTED NOVEMBER 1989
        WRITE(ICOUT,9071)YATCCU,YATTCU,YATRCU,IYATOS,IYATRS
 9071   FORMAT('YATCCU,YATTCU,YATRCU,IYATOS,IYATRS = ',3G15.7,
     1         2(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPYAT2(Y,REP,N,ICASPL,MAXN,IWRITE,
     1                  YATCCU,YATTCU,YATRCU,IYATOS,IYATRS,
     1                  COEF,SSQCOE,TCOEF,RSDCOE,TAGCOE,TAGCO2,NCOEF,
     1                  PRESSD,PRESDF,REPSD,REPDF,REFSD,REFDF,SDCOEF,
     1                  REPD,IFLAG,RSDCOC,ITAG,ITAGCO,YMEAN,
     1                  YVAR,DUMMY,DUMMY2,AINDEX,AINDE2,TEMP,
     1                  PHD1,PHD2,PHD3,PHD4,PHD5,RESLIN,PREDLIN,
     1                  ICAPSW,ICAPTY,IFORSW,IPHDFL,
     1                  IBUGA3,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT A YATES ANALYSIS (DEX FIT FOR 2**K DESIGNS)
C              (USEFUL FOR COMPUTING THE EFFECTS IN A 2**K
C              AND A 2**(K-P) EXPERIMENT)
C     NOTE--THIS CODE ASSUMES THE DATA IS IN
C           STANDARD YATES/HUNTER/BOX ORDER.
C           FOR EXAMPLE, FOR A 2**3--
C                 - - -
C                 + - -
C                 - + -
C                 + + -
C                 - - +
C                 + - +
C                 - + +
C                 + + +
C     NOTE--IF HAVE REPLICATION, THEN THE REPLICATES
C           MAY EITHER BE IMMEDIATELY WITHIN
C           OR MAY BE IN BLOCKS AFTER.
C     EXAMPLE--YATES Y
C              YATES Y REP
C              YATES ANALYSIS Y
C              YATES ANALYSIS Y REP
C              DEX FIT Y
C              DEX FIT Y REP
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/7
C     ORIGINAL VERSION--JULY      1987.
C     UPDATED         --JUNE      1989.  2**K DEX FIT SYNONYM
C     UPDATED         --NOVEMBER  1989.  SELECTIVE PRINTING OF COEF
C     UPDATED         --JANUARY   1990.  PRINT MEAN IN ORDERED LIST
C     UPDATED         --JUNE      1990.  MOVE SOME DIMENSIONS TO DPYATE
C     UPDATED         --OCTOBER   1991.  PRINT TO STORAGE FILE
C     UPDATED         --NOVEMBER  1991.  FIX BOMB WITH PRINT FOR 2**2
C     UPDATED         --NOVEMBER  1991.  FIX BOMB FOR 2**1
C     UPDATED         --NOVEMBER  1991.  REMOVE 2 PRINT LINES (RESSD)
C     UPDATED         --APRIL     1992.  DELETE IFOUND
C     UPDATED         --JUNE      1992.  SKIP PRINTING SECTION 2
C     UPDATED         --JUNE      1992.  FIX SD(YBAR)
C     UPDATED         --NOVEMBER  1996.  FORMAT CORRECTIONS AFTER 7400 CONTINUE
C     UPDATED         --NOVEMBER  1996.  ADD LINES AT END OF OUTPUT (7713)
C     UPDATED         --OCTOBER   2003.  SUPPORT FOR HTML, LATEX OUTPUT
C     UPDATED         --JUNE      2005.  PRINT OUTPUT TO DPST1F.DAT AND
C                                        DPST2F.DAT EVEN IF PRINTING
C                                        SWITCH IS OFF
C     UPDATED         --OCTOBER   2006.  CALL LIST TO TPPF
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IWRITE
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICAPTY
      CHARACTER*4 IFORSW
      CHARACTER*4 IPHDFL
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IREP
      CHARACTER*4 ICASE
CCCCC THE FOLLOWING LINE WAS FIXED NOVEMBER 1991
CCCCC BECAUSE THE CALLING ROUTINE (DPYATE) HAD IFLAG NOVEMBER 1991
CCCCC EQUIVALANCED TO IGARBG WHICH WAS INTEGER BUT NOVEMBER 1991
CCCCC DPYAT2 WAS TRYING TO USE IFLAG AS CHARACTER*2 NOVEMBER 1991
CCCCC CHARACTER*2 IFLAG
      CHARACTER*2 STAR
CCCCC CHARACTER*12 STAR2
C
      CHARACTER*4 IOP
C
CCCCC THE FOLLOWING LINE WAS INSERTED NOVEMBER 1989
      CHARACTER*4 IYATOS
      CHARACTER*4 IYATRS
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      PARAMETER(NUMCLI=6)
      PARAMETER(MAXLIN=3)
      PARAMETER (MAXROW=200)
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
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
C
      DIMENSION Y(*)
      DIMENSION REP(*)
C
      DIMENSION COEF(*)
      DIMENSION SSQCOE(*)
      DIMENSION TCOEF(*)
      DIMENSION RSDCOE(*)
      DIMENSION TAGCOE(*)
      DIMENSION TAGCO2(*)
C
      DIMENSION REPD(*)
      DIMENSION IFLAG(*)
      DIMENSION RSDCOC(*)
      DIMENSION ITAG(*)
      DIMENSION ITAGCO(*)
      DIMENSION YMEAN(*)
      DIMENSION YVAR(*)
      DIMENSION DUMMY(*)
      DIMENSION DUMMY2(*)
      DIMENSION AINDEX(*)
      DIMENSION AINDE2(*)
      DIMENSION TEMP(*)
C
      DIMENSION PHD1(*)
      DIMENSION PHD2(*)
      DIMENSION PHD3(*)
      DIMENSION PHD4(*)
      DIMENSION PHD5(*)
      DIMENSION PREDLIN(*)
      DIMENSION RESLIN(*)
C
      PARAMETER (MAXFAC=12)
C
      DIMENSION AMAIN(MAXFAC)
C
      DIMENSION A(MAXFAC,MAXFAC)
      DIMENSION EIGVAL(MAXFAC)
      DIMENSION EIGVA2(MAXFAC)
      DIMENSION EIGVA3(MAXFAC)
      DIMENSION EIGVEC(MAXFAC,MAXFAC)
      DIMENSION ITAG2(MAXFAC)
      DIMENSION ITAG3(MAXFAC)
      DIMENSION VJUNK(2*MAXFAC)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPYA'
      ISUBN2='T2  '
      IERROR='NO'
C
      AN=N
      CUTOFF=999999.0
      NUMDIG=7
C
      CCUTP=YATCCU
      CCUTN=(-YATCCU)
      TCUTP=YATTCU
      TCUTN=(-YATTCU)
      RCUTP=YATRCU
      RCUTN=(-YATRCU)
C
      DO10I=1,MAXFAC
        ITAG2(I)=0
        ITAG3(I)=0
        EIGVAL(I)=0.0
        EIGVA2(I)=0.0
        EIGVA3(I)=0.0
   10 CONTINUE
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPYAT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGA3,ISUBRO,ICASPL,MAXN,N
   52   FORMAT('IBUGA3,ISUBRO,ICASPL,MAXN,N = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO61I=1,N
            WRITE(ICOUT,62)I,Y(I),REP(I)
   62       FORMAT('I,Y(I),REP(I) = ',I8,2G15.7)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
        WRITE(ICOUT,71)YATCCU,YATTCU,YATRCU,IYATOS,IYATRS
   71   FORMAT('YATCCU,YATTCU,YATRCU,IYATOS,IYATRS = ',3G15.7,
     1         2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)CCUTN,CCUTP,TCUTN,TCUTP
   72   FORMAT('CCUTN,CCUTP,TCUTN,TCUTP = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,74)RCUTN,RCUTP,TAGCOE(1),TEMP(1)
   74   FORMAT('RCUTN,RCUTP,TAGCOE(1),TEMP(1) = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE INPUT ARGUMENTS FOR ERRORS  **
C               ********************************************
C
      IF(N.LE.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1111)
 1111   FORMAT('***** ERROR IN YATES ANAYLYSIS--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1112)
 1112   FORMAT('      THE NUMBER OF OBSERVATIONS MUST BE AT LEAST ',
     1         'TWO.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1114)N
 1114   FORMAT('      THE ENTERED NUMBER OF OBSERVATIONS HERE = ',I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      HOLD=Y(1)
      DO1130I=1,N
      IF(Y(I).NE.HOLD)GOTO1139
 1130 CONTINUE
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1132)HOLD
 1132 FORMAT('      THE RESPONSE VARIABLE ELEMENTS ARE ALL ',
     1       'IDENTICALLY EQUAL TO ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1139 CONTINUE
C
CCCCCC THE FOLLOWING SECTION WAS ADDED OCTOBER 1991
C               **************************************************
C               **  STEP 15--                                   **
C               **   OPEN THE STORAGE FILES                     **
C               **************************************************
C
      ISTEPN='15'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='OPEN'
      IFLAG1=1
      IFLAG2=1
      IF(IPHDFL.EQ.'ON')THEN
        IFLAG3=1
        IFLAG4=1
      ELSE
        IFLAG3=0
        IFLAG4=0
      ENDIF
      IFLAG5=0
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               **************************************************
C               **  STEP 20--                                   **
C               **  COMPUTE GRAND MEAN                          **
C               **  COMPUTE GRAND STANDARD DEVIATION            **
C               **************************************************
C
      SUM=0.0
      DO2000I=1,N
        SUM=SUM+Y(I)
 2000 CONTINUE
      GMEAN=SUM/AN
C
      SUM=0.0
      DO2020I=1,N
        SUM=SUM+(Y(I)-GMEAN)**2
 2020 CONTINUE
      GSSQ=SUM
      GVAR=GSSQ/(AN-1.0)
      GSD=0.0
      IF(GVAR.GT.0.0)GSD=SQRT(GVAR)
C
C               ****************************************************
C               **  STEP 21--                                     **
C               **  EXTRACT THE DISTINCT REPLICATION VALUES       **
C               **  IN ORDER TO                                   **
C               **  DETERMINE THE TYPE OF REPLICATION CASE--      **
C               **     1) NO REPLICATION                          **
C               **     2) REPLICATION 'WITHIN', AS IN             **
C               **        (FOR A 2**2 WITH 3 REPLICATIONS)--      **
C               **          X1  X2  REP                           **
C               **           -   +   1                            **
C               **           -   +   2                            **
C               **           -   +   3                            **
C               **                                                **
C               **           +   +   1                            **
C               **           +   +   2                            **
C               **           +   +   3                            **
C               **                                                **
C               **           -   -   1                            **
C               **           -   -   2                            **
C               **           -   -   3                            **
C               **                                                **
C               **           +   +   1                            **
C               **           +   +   2                            **
C               **           +   +   3                            **
C               **     3) REPLICATION 'BETWEEN', AS IN            **
C               **        (FOR A 2**2 WITH 3 REPLICATIONS)--      **
C               **          X1  X2  REP                           **
C               **           -   +   1                            **
C               **           +   +   1                            **
C               **           -   -   1                            **
C               **           +   +   1                            **
C               **                                                **
C               **           -   +   2                            **
C               **           +   +   2                            **
C               **           -   -   2                            **
C               **           +   +   2                            **
C               **                                                **
C               **           -   +   3                            **
C               **           +   +   3                            **
C               **           -   -   3                            **
C               **           +   +   3                            **
C               **                                                **
C               ****************************************************
C
      ISTEPN='21'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IWRITE='OFF'
      CALL DISTIN(REP,N,IWRITE,REPD,NREPD,IBUGA3,IERROR)
C
      NUMREP=NREPD
      ANUMRE=NUMREP
      IREP='NO'
      ICASE='-999'
      IF(NUMREP.GT.1)THEN
        IREP='YES'
        ICASE='BETW'
        IF(REP(2).NE.REP(1))ICASE='WITH'
      ENDIF
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')THEN
        WRITE(ICOUT,2191)REPD(1),REPD(2),REPD(3),REPD(4)
 2191   FORMAT('REPD(1),REPD(2),REPD(3),REPD(4) = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2192)NREPD,IREP,ICASE
 2192   FORMAT('NREPD,IREP,ICASE = ',I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  COMPUTE CELL MEANS                          **
C               **************************************************
C
      IF(IREP.EQ.'NO')THEN
        NMEAN=N
        ANMEAN=NMEAN
        DO2211I=1,N
          YMEAN(I)=Y(I)
 2211   CONTINUE
      ELSEIF(ICASE.EQ.'WITH')THEN
        NMEAN=N/NUMREP
        ANMEAN=NMEAN
        DO2221I=1,NMEAN
          SUM=0.0
          JMIN=NUMREP*(I-1)+1
          JMAX=NUMREP*I
          DO2222J=JMIN,JMAX
            SUM=SUM+Y(J)
 2222     CONTINUE
          YMEAN(I)=SUM/ANUMRE
 2221   CONTINUE
      ELSE
        NMEAN=N/NUMREP
        ANMEAN=NMEAN
        DO2231I=1,NMEAN
          SUM=0.0
          DO2232J=I,N,NMEAN
            SUM=SUM+Y(J)
 2232     CONTINUE
          YMEAN(I)=SUM/ANUMRE
 2231   CONTINUE
      ENDIF
C
      NCOEF=NMEAN
C
C               **************************************************
C               **  STEP 23--                                   **
C               **  IF HAVE REPLICATION,                        **
C               **  COMPUTE REPLICATION STANDARD DEVIATION      **
C               **************************************************
C
      IREPDF=0
      REPDF=0.0
      REPVAR=0.0
      REPSD=0.0
      LOFCDF=0
C
      IF(IREP.EQ.'NO')GOTO2390
      IF(ICASE.EQ.'WITH')THEN
        NMEAN=N/NUMREP
        ANMEAN=NMEAN
        SUMT=0.0
        DO2321I=1,NMEAN
          SUM=0.0
          JMIN=NUMREP*(I-1)+1
          JMAX=NUMREP*I
          DO2322J=JMIN,JMAX
            SUM=SUM+(Y(J)-YMEAN(I))**2
            SUMT=SUMT+(Y(J)-YMEAN(I))**2
 2322     CONTINUE
          YVAR(I)=SUM/(ANUMRE-1.0)
 2321   CONTINUE
        IREPDF=NMEAN
        REPDF=ANMEAN
        REPVAR=SUMT/REPDF
        REPSD=0.0
        IF(REPVAR.GT.0.0)REPSD=SQRT(REPVAR)
      ELSE
        NMEAN=N/NUMREP
        ANMEAN=NMEAN
        SUMT=0.0
        DO2331I=1,NMEAN
          SUM=0.0
          DO2332J=I,N,NMEAN
            SUM=SUM+(Y(J)-YMEAN(I))**2
            SUMT=SUMT+(Y(J)-YMEAN(I))**2
 2332     CONTINUE
          YVAR(I)=SUM/(ANUMRE-1.0)
 2331   CONTINUE
        IREPDF=NMEAN
        REPDF=ANMEAN
        REPVAR=SUMT/REPDF
        REPSD=0.0
        IF(REPVAR.GT.0.0)REPSD=SQRT(REPVAR)
      ENDIF
C
 2390 CONTINUE
      NCOEF=NMEAN
C
C               **************************************************
C               **  STEP 24--                                   **
C               **  COMPUTE EFFECTS                             **
C               **  (VIA THE YATES ALGORITHM ?)                 **
C               **************************************************
C
      DO2410I=1,NMEAN
        COEF(I)=YMEAN(I)
 2410 CONTINUE
C
      NPASS=INT((LOG10(ANMEAN)/0.30103)+0.5)
      NUMFAC=NPASS
C
      DO2420IPASS=1,NPASS
C
        DO2430I=1,NMEAN
         DUMMY(I)=COEF(I)
 2430   CONTINUE
C
        J1=0
        J2=NMEAN/2
        DO2440I=1,NMEAN,2
          IP1=I+1
          J1=J1+1
          J2=J2+1
          COEF(J1)=DUMMY(IP1)+DUMMY(I)
          COEF(J2)=DUMMY(IP1)-DUMMY(I)
 2440   CONTINUE
C
 2420 CONTINUE
C
      COEF(1)=COEF(1)/ANMEAN
      DO2450I=2,NMEAN
        COEF(I)=COEF(I)/(ANMEAN/2.0)
 2450 CONTINUE
C
C               **************************************************
C               **  STEP 24--                                   **
C               **  COMPUTE SUM OF SQUARES FOR EACH EFFECT      **
C               **  REFERENCE--HUNTER DESIGN OF EXP. COURSE,    **
C               **             VOLUME 4, PAGE 71                **
C               **************************************************
C
      SSQCOE(1)=GSSQ
      DO2500I=2,NMEAN
        SSQCOE(I)=ANMEAN*COEF(I)*COEF(I)/4.0
 2500 CONTINUE
C
C               **************************************************
C               **  STEP 26--                                   **
C               **  DEFINE IDENTIFIERS                          **
C               **************************************************
C
      J=0
      JP1=1
CCCCC TAGCOE(JP1)=0.0
      ITAGCO(JP1)=0
C
      J=1
      JP1=2
      ITAG(J)=1
CCCCC TAGCOE(JP1)=ITAG(J)
      ITAGCO(JP1)=ITAG(J)
C
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1991
      IF(NUMFAC.LE.1)GOTO2629
      DO2610IFAC=2,NUMFAC
        JMIN=2**(IFAC-1)
        JMAX=(2**IFAC)-1
        K=0
        DO2620J=JMIN,JMAX
          JP1=J+1
          IF(J.EQ.JMIN)ITAG(J)=IFAC
CCCCC     IF(J.EQ.JMIN)TAGCOE(JP1)=ITAG(J)
          IF(J.EQ.JMIN)ITAGCO(JP1)=ITAG(J)
          IF(J.EQ.JMIN.AND.IFAC.GE.10)ITAGCO(JP1)=ITAGCO(JP1)-10
          IF(J.EQ.JMIN)GOTO2620
          K=K+1
          ITAG(J)=10*ITAG(K)+IFAC
CCCCC     TAGCOE(JP1)=ITAG(J)
          ITAGCO(JP1)=ITAG(J)
          IF(IFAC.GE.10)ITAGCO(JP1)=ITAGCO(JP1)-10
 2620   CONTINUE
 2610 CONTINUE
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1991
 2629 CONTINUE
C
      TAGCO2(1)=0.0
      IF(NUMFAC.LE.0)GOTO2639
      DO2630I=2,NMEAN
        AJUNK=ITAGCO(I)
CCCCC   ATEMP=LOG10(TAGCOE(I)+0.5)
        ATEMP=LOG10(AJUNK+0.5)
        ATEMP=ATEMP+1.0
        ITEMP=INT(ATEMP+0.1)
        TAGCO2(I)=REAL(ITEMP)
 2630 CONTINUE
 2639 CONTINUE
C
C               **************************************************
C               **  STEP 27--                                   **
C               **  COMPUTE PSEUDO-REPLIC. STANDARD DEVIATION   **
C               **************************************************
C
      SUM=0.0
      SUMI=0.0
      DO2700I=1,NMEAN
        IF(TAGCO2(I).GE.2.5)SUM=SUM+SSQCOE(I)
        IF(TAGCO2(I).GE.2.5)SUMI=SUMI+1.0
C
        IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')THEN
          WRITE(ICOUT,2701)I,TAGCO2(I),COEF(I),SSQCOE(I),SUMI,SUM
 2701     FORMAT('I,TAGCO2(I),COEF(I),SSQCOE(I),SUMI,SUM = ',I8,5G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
 2700 CONTINUE
      PRESSS=SUM
      PRESDF=SUMI
      IPRESD=INT(PRESDF+0.5)
      PRESVA=0.0
      IF(PRESDF.GT.0.1)PRESVA=PRESSS/PRESDF
      PRESSD=0.0
      IF(PRESVA.GT.0.0)PRESSD=SQRT(PRESVA)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')THEN
        WRITE(ICOUT,2702)PRESSS,PRESVA,PRESDF,PRESSD
 2702   FORMAT('PRESSS,PRESVA,PRESDF,PRESSD = ',4E15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************************************
C               **  STEP 28--                                          *
C               **  COMPUTE A REFERENCE STANDARD DEVIATION             *
C               **  WHICH EQUALS                                       *
C               **     THE REPLICATION ST. DEV.  (IF HAVE REPLICATION) *
C               **     THE PSEUDO-REPLIC. ST. DEV. (IF NOT HAVE        *
C               **         REPLICATION)                                *
C               ********************************************************
C
      IREFDF=0
      IF(IREP.EQ.'NO')IREFDF=IPRESD
      IF(IREP.EQ.'YES')IREFDF=IREPDF
C
      REFVAR=0.0
      IF(IREP.EQ.'NO')REFVAR=PRESVA
C
      IF(IREP.EQ.'YES')REFVAR=REPVAR
      REFSD=0.0
      IF(REFVAR.GT.0.0)REFSD=SQRT(REFVAR)
C
C               **************************************************
C               **  STEP 29--                                   **
C               **  COMPUTE STANDARD DEV. FOR EACH COEF         **
C               **  REFERENCE--HUNTER DESIGN OF EXP. COURSE,    **
C               **             VOLUME 4, PAGE 82                **
C               **************************************************
C
      VCOER=0.0
      VCOER=2.0*(REPVAR/(AN/2.0))
      SDCOER=0.0
      IF(VCOER.GT.0.0)SDCOER=SQRT(VCOER)
C
      VCOEP=0.0
      VCOEP=2.0*(PRESVA/(AN/2.0))
      SDCOEP=0.0
      IF(VCOEP.GT.0.0)SDCOEP=SQRT(VCOEP)
C
      VCOEF=0.0
      VCOEF=2.0*(REFVAR/(AN/2.0))
      SDCOEF=0.0
      IF(VCOEF.GT.0.0)SDCOEF=SQRT(VCOEF)
C
CCCCC THE FOLLOWING 4 LINES WERE ADDED OCTOBER 1991
      VGMEAN=0.0
      VGMEAN=REFVAR/AN
      SDGMEA=0.0
      IF(VGMEAN.GT.0.0)SDGMEA=SQRT(VGMEAN)
C
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')THEN
        WRITE(ICOUT,2903)REFVAR,REFSD,VCOEF,SDCOEF
 2903   FORMAT('REFVAR,REFSD,VCOEF,SDCOEF = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **************************************************
C               **  STEP 30--                                   **
C               **  COMPUTE T VALUE FOR EACH COEF               **
C               **************************************************
C
      DO3010I=1,NMEAN
        TCOEF(I)=0.0
        IF(SDCOEF.GT.0.0)TCOEF(I)=COEF(I)/SDCOEF
        IF(SDCOEF.GT.0.0.AND.TCOEF(I).GT.CUTOFF)TCOEF(I)=CUTOFF
        IF(SDCOEF.GT.0.0.AND.TCOEF(I).LT.-CUTOFF)TCOEF(I)=(-CUTOFF)
 3010 CONTINUE
C
C               **************************************************
C               **  STEP 31--                                   **
C               **  COMPUTE A SORT INDEX BASED ON               **
C               **  THE MAGNITUDE OF THE EFFECTS                **
C               **************************************************
C
      DO3110I=1,NMEAN
        DUMMY(I)=(-ABS(COEF(I)))
        AINDEX(I)=I
 3110 CONTINUE
C
      AMIN=DUMMY(1)
      DO3120I=1,NMEAN
        IF(DUMMY(I).LT.AMIN)AMIN=DUMMY(I)
 3120 CONTINUE
      DUMMY(1)=AMIN-10.0
C
      CALL SORTC(DUMMY,AINDEX,NMEAN,DUMMY2,AINDE2)
C
C               **************************************************
C               **  STEP 32--                                   **
C               **  COMPUTE THE RESIDUAL STANDARD DEVIATION     **
C               **  THAT WOULD RESULT IF FIT EACH TERM          **
C               **  INDIVIDUALLY, AS IN                         **
C               **  RESPONSE = CONSTANT + TERM + ERROR          **
C               **************************************************
C
CCCCC DO3210I=1,NMEAN
CCCCC CALL DMV(TAGCOE(I),NMEAN,TEMP)
CCCCC COEFFI=COEF(I)
CCCCC SUM=0.0
CCCCC DO3220J=1,NMEAN
CCCCC PREDJ=GMEAN+COEFFI*TEMP(J)
CCCCC RESJ=Y(J)-PREDJ
CCCCC SUM=SUM+RESJ*RESJ
C3220 CONTINUE
CCCCC RESVI=SUM/(AN-2.0)
CCCCC RESSDI=0.0
CCCCC IF(RESVI.GT.0.0)RESSDI=SQRT(RESVI)
CCCCC RSDCOE(I)=RESSDI
C3210 CONTINUE
C
      DO3210I=1,NMEAN
CCCCC   THE FOLLOWING LINE WAS INSERTED JUNE 1992 (JJF)
        RVAR=0.0
CCCCC   IF(I.EQ.1)RVAR=SSQCOE(1)/(ANMEAN-1.0)
        IF(I.EQ.1)RVAR=SSQCOE(1)/(AN-1.0)
CCCCC   IF(I.GE.2)RVAR=(SSQCOE(1)-SSQCOE(I))/(ANMEAN-1.0-1.0)
CCCCC   THE FOLLOWING LINE WAS FIXED NOVEMBER 1991
CCCCC   IF(I.GE.2)RVAR=(SSQCOE(1)-SSQCOE(I))/(AN-1.0-1.0)
        IDENOM=N-1-1
CCCCC   THE FOLLOWING LINE WAS COMMENTED OUT & MOVED UP JUNE 1992 (JJF)
CCCCC   RVAR=0.0
        IF(I.GE.2.AND.IDENOM.GE.1)RVAR=(SSQCOE(1)-SSQCOE(I))/
     1                                 (AN-1.0-1.0)
        RSDCOE(I)=0.0
        IF(RVAR.GT.0.0)RSDCOE(I)=SQRT(RVAR)
 3210 CONTINUE
C
      DO3220I=1,NMEAN
        AI=I
        I2=INT(AINDE2(I)+0.5)
        IF(I.EQ.1)CUMSSQ=0.0
        IF(I.GE.2)CUMSSQ=CUMSSQ+SSQCOE(I2)
CCCCC   IF(I.LT.NMEAN)RVAR=(SSQCOE(1)-CUMSSQ)/(ANMEAN-AI)
        IF(I.LT.NMEAN)RVAR=(SSQCOE(1)-CUMSSQ)/(AN-AI)
        IF(I.EQ.NMEAN.AND.IREP.EQ.'YES')RVAR=(SSQCOE(1)-CUMSSQ)/(AN-AI)
        IF(I.EQ.NMEAN.AND.IREP.EQ.'NO')RVAR=0.0
        RSDCOC(I2)=0.0
        IF(RVAR.GT.0.0)RSDCOC(I2)=SQRT(RVAR)
 3220 CONTINUE
C
C               **************************************************
C               **  STEP 33--                                   **
C               **  COMPUTE 97.5 AND 99.5 PERCENT POINTS        **
C               **  COMPUTE 95% AND 99% CONFIDENCE LIMITS       **
C               **************************************************
C
      NU=IREFDF
C
      P=0.975
      CALL TPPF(P,REAL(NU),T975)
      CL95=T975*SDCOEF
C
      P=0.995
      CALL TPPF(P,REAL(NU),T995)
      CL99=T995*SDCOEF
C
C               **************************************************
C               **  STEP 34--                                   **
C               **  FLAG THOSE EFFECTS WHICH HAVE T VALUES      **
C               **  LARGER (IN MAGNITUDE) THAT T975, AND        **
C               **  LARGER (IN MAGNITUDE) THAT T995             **
C               **************************************************
C
      DO3400I=1,NMEAN
CCCCC   THE FOLLOWING 3 LINES WERE FIXED NOVEMBER 1991
CCCCC   IFLAG(I)='  '
CCCCC   IF(ABS(TCOEF(I)).GT.T975)IFLAG(I)='* '
CCCCC   IF(ABS(TCOEF(I)).GT.T995)IFLAG(I)='**'
        IFLAG(I)=0
        IF(ABS(TCOEF(I)).GT.T975)IFLAG(I)=1
        IF(ABS(TCOEF(I)).GT.T995)IFLAG(I)=2
 3400 CONTINUE
C
C               ****************************
C               **  STEP 71--             **
C               **  WRITE EVERYTHING OUT  **
C               ****************************
C
      ISTEPN='71'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC 2015/01 BUG: IF PRINTING OFF, STILL NEED TO WRITE TO
CCCCC              AUXILLARY FILES AND, OPTIONALLY, PERFORM
CCCCC              PHD ANALYSIS
C
CCCCC IF(IPRINT.EQ.'OFF')GOTO9000
      IF(IPRINT.EQ.'OFF')GOTO8000
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
      ITITLE='2**K DEX Fit'
      NCTITL=12
      ITITLZ='(Note--Data Must Be In Standard Order)'
      NCTITZ=38
C
      ICNT=0
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
      ITEXT(ICNT)='Number of Factors:'
      NCTEXT(ICNT)=23
      AVALUE(ICNT)=REAL(NUMFAC)
      IDIGIT(ICNT)=0
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
      IF(IREP.EQ.'NO')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='No Replication Case:'
        NCTEXT(ICNT)=20
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
      ELSE
        ICNT=ICNT+1
        ITEXT(ICNT)='Replication Case:'
        NCTEXT(ICNT)=17
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        IF(IYATOS.EQ.'1'.OR.IYATOS.EQ.'12'.OR.IYATOS.EQ.'13'.OR.
     1    IYATOS.EQ.'123')THEN
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Standard Deviation:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=REPSD
          IDIGIT(ICNT)=NUMDIG
          ICNT=ICNT+1
          ITEXT(ICNT)='Replication Degrees of Freedom:'
          NCTEXT(ICNT)=31
          AVALUE(ICNT)=IREPDF
          IDIGIT(ICNT)=0
        ENDIF
      ENDIF
C
      IF(IYATOS.EQ.'1'.OR.IYATOS.EQ.'12'.OR.IYATOS.EQ.'13'.OR.
     1  IYATOS.EQ.'123')THEN
        ICNT=ICNT+1
        ITEXT(ICNT)='Psuedo-Replication Standard Deviation:'
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=PRESSD
        IDIGIT(ICNT)=NUMDIG
        ICNT=ICNT+1
        ITEXT(ICNT)='Psuedo-Degrees of Freedom:'
        NCTEXT(ICNT)=26
        AVALUE(ICNT)=IPRESD
        IDIGIT(ICNT)=0
        ICNT=ICNT+1
        ITEXT(ICNT)='(The Psuedo-Replication SD Assumes All'
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='3, 4, 5, ...-Term Interactions are not'
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Real, But Are Manifestations of Random'
        NCTEXT(ICNT)=38
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
        ICNT=ICNT+1
        ITEXT(ICNT)='Error.'
        NCTEXT(ICNT)=6
        AVALUE(ICNT)=0.0
        IDIGIT(ICNT)=-1
C
        IF(IREP.EQ.'YES')THEN
           ICNT=ICNT+1
           ITEXT(ICNT)='Standard Deviation of a Coefficent:'
           NCTEXT(ICNT)=35
           AVALUE(ICNT)=SDCOER
           IDIGIT(ICNT)=NUMDIG
           ICNT=ICNT+1
           ITEXT(ICNT)='(Based On Replication Std. Deviation)'
           NCTEXT(ICNT)=37
           AVALUE(ICNT)=0.0
           IDIGIT(ICNT)=-1
        ELSE
           ICNT=ICNT+1
           ITEXT(ICNT)='Standard Deviation of a Coefficent:'
           NCTEXT(ICNT)=35
           AVALUE(ICNT)=SDCOEP
           IDIGIT(ICNT)=NUMDIG
           ICNT=ICNT+1
           ITEXT(ICNT)='(Based On Psuedo-Replication Std. Dev.)'
           NCTEXT(ICNT)=39
           AVALUE(ICNT)=0.0
           IDIGIT(ICNT)=-1
        ENDIF
      ENDIF
C
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='Grand Mean:'
      NCTEXT(ICNT)=11
      AVALUE(ICNT)=GMEAN
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='Grand Standard Deviation:'
      NCTEXT(ICNT)=25
      AVALUE(ICNT)=GSD
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
C
      ICNT=ICNT+1
      ITEXT(ICNT)='99% Confidence Limits (+-):'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=CL99
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='95% Confidence Limits (+-):'
      NCTEXT(ICNT)=27
      AVALUE(ICNT)=CL95
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='99.5% Point Of T Distribution:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=T995
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)='97.5% Point Of T Distribution:'
      NCTEXT(ICNT)=30
      AVALUE(ICNT)=T975
      IDIGIT(ICNT)=NUMDIG
      ICNT=ICNT+1
      ITEXT(ICNT)=' '
      NCTEXT(ICNT)=1
      AVALUE(ICNT)=0.0
      IDIGIT(ICNT)=-1
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
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPDTA1(ITITLE,NCTITL,ITITLZ,NCTITZ,ITEXT,NCTEXT,
     1            AVALUE,IDIGIT,
     1            NTOT,NUMROW,
     1            ICAPSW,ICAPTY,ILAST,IFRST,
     1            ISUBRO,IBUGA3,IERROR)
C
      ISTEPN='42D'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IYATOS.EQ.'3'.OR.IYATOS.EQ.'13'.OR.IYATOS.EQ.'23'.OR.
     1   IYATOS.EQ.'123')THEN
C
        ITITLE=' '
        NCTITL=0
        ITITL9=' '
        NCTIT9=0
        ICNT=0
C
        DO5030J=1,NUMCLI
          DO5040I=1,3
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
 5040     CONTINUE
 5030   CONTINUE
C
        ITITL2(3,1)='Identifier'
        NCTIT2(3,1)=10
C
        ITITL2(2,2)='Effect'
        NCTIT2(2,2)=6
        ITITL2(3,2)='Estimate'
        NCTIT2(3,2)=8
C
        ITITL2(3,3)='T Value'
        NCTIT2(3,3)=7
C
        ITITL2(1,4)='RESSD:'
        NCTIT2(1,4)=6
        ITITL2(2,4)='Mean +'
        NCTIT2(2,4)=6
        ITITL2(3,4)='Term'
        NCTIT2(3,4)=4
C
        ITITL2(1,5)='RESSD:'
        NCTIT2(1,5)=6
        ITITL2(2,5)='Mean +'
        NCTIT2(2,5)=6
        ITITL2(3,5)='Cum Terms'
        NCTIT2(3,5)=4
C
        NMAX=0
        NUMCOL=5
        DO5050I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IF(I.EQ.1 .OR. I.EQ.3)THEN
            ITYPCO(I)='ALPH'
          ENDIF
 5050   CONTINUE
C
        IWHTML(1)=125
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IINC=1800
        IINC2=1400
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC
        IWRTF(4)=IWRTF(3)+IINC
        IWRTF(5)=IWRTF(4)+IINC
C
        ITAGCO(1)=0
        TCOEF(1)=-999.99
        IF(SDGMEA.GT.0.0)TCOEF(1)=GMEAN/SDGMEA
        IFLAG(1)=0
C
        IVALUE(1,1)(1:4)='Mean'
        NCVALU(1,1)=4
        IVALUE(1,3)(1:4)=' '
        NCVALU(1,3)=1
        AMAT(1,2)=GMEAN
        AMAT(1,4)=RSDCOE(1)
        AMAT(1,5)=RSDCOE(1)
C
        ICNT=1
        DO5060I=2,NMEAN
C
          I2=INT(AINDE2(I)+0.5)
          IF(CCUTP.LT.CPUMAX.AND.
     1       CCUTN.LE.COEF(I2).AND.COEF(I2).LE.CCUTP)GOTO5060
          IF(TCUTP.LT.CPUMAX.AND.
     1       TCUTN.LE.TCOEF(I2).AND.TCOEF(I2).LE.TCUTP)GOTO5060
          IF(RCUTP.LT.CPUMAX.AND.
     1       RCUTN.LE.RSDCOC(I2).AND.RSDCOC(I2).LE.RCUTP)GOTO5060
C
          STAR='  '
          IF(IFLAG(I2).EQ.1)STAR='* '
          IF(IFLAG(I2).EQ.2)STAR='**'
          ICNT=ICNT+1
C
          AMAT(ICNT,2)=COEF(I2)
          AMAT(ICNT,4)=RSDCOE(I2)
          AMAT(ICNT,5)=RSDCOC(I2)
C
          WRITE(IVALUE(ICNT,3)(1:13),'(F13.1)')TCOEF(I2)
          ICNT2=0
          DO5070II=1,13
            IF(IVALUE(ICNT,3)(II:II).NE.' ')THEN
              ICNT2=ICNT2+1
              IVALUE(ICNT,3)(ICNT2:ICNT2)=IVALUE(ICNT,3)(II:II)
            ENDIF
 5070     CONTINUE
          IF(IFLAG(I2).EQ.1)THEN
            ICNT2=ICNT2+1
            IVALUE(ICNT,3)(ICNT2:ICNT2)='*'
            ICNT2=ICNT2+1
            IVALUE(ICNT,3)(ICNT2:ICNT2)=' '
          ELSEIF(IFLAG(I2).EQ.2)THEN
            ICNT2=ICNT2+1
            IVALUE(ICNT,3)(ICNT2:ICNT2)='*'
            ICNT2=ICNT2+1
            IVALUE(ICNT,3)(ICNT2:ICNT2)='*'
          ELSE
            ICNT2=ICNT2+1
            IVALUE(ICNT,3)(ICNT2:ICNT2)=' '
            ICNT2=ICNT2+1
            IVALUE(ICNT,3)(ICNT2:ICNT2)=' '
          ENDIF
          NCVALU(ICNT,3)=ICNT2
C
          WRITE(IVALUE(ICNT,1)(1:9),'(I9)')ITAGCO(I2)
          ICNT2=0
          DO5080II=1,9
            IF(IVALUE(ICNT,1)(II:II).NE.' ')THEN
              ICNT2=ICNT2+1
              IVALUE(ICNT,1)(ICNT2:ICNT2)=IVALUE(ICNT,1)(II:II)
            ENDIF
 5080     CONTINUE
          NCVALU(ICNT,1)=ICNT2
C
 5060   CONTINUE
C
        NUMLIN=3
        NUMCOL=5
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
C
      ENDIF
C
 8000 CONTINUE
C
      ITAGCO(1)=0
      TCOEF(1)=-999.99
      IF(SDGMEA.GT.0.0)TCOEF(1)=GMEAN/SDGMEA
      IFLAG(1)=0
      WRITE(IOUNI2,7433)ITAGCO(1),GMEAN,SDGMEA,RSDCOE(1),RSDCOE(1)
 7433 FORMAT(I9,F14.5,F13.1,'  ',F11.5,F11.5)
C
      DO7440I=2,NMEAN
        I2=INT(AINDE2(I)+0.5)
        IF(CCUTP.LT.CPUMAX.AND.
     1     CCUTN.LE.COEF(I2).AND.COEF(I2).LE.CCUTP)GOTO7440
        IF(TCUTP.LT.CPUMAX.AND.
     1     TCUTN.LE.TCOEF(I2).AND.TCOEF(I2).LE.TCUTP)GOTO7440
        IF(RCUTP.LT.CPUMAX.AND.
     1     RCUTN.LE.RSDCOC(I2).AND.RSDCOC(I2).LE.RCUTP)GOTO7440
C
        STAR='  '
        IF(IFLAG(I2).EQ.1)STAR='* '
        IF(IFLAG(I2).EQ.2)STAR='**'
        WRITE(IOUNI1,7443)ITAGCO(I2),COEF(I2),TCOEF(I2),
     1                    RSDCOE(I2),RSDCOC(I2)
        WRITE(IOUNI2,7443)ITAGCO(I2),COEF(I2),TCOEF(I2),
     1                    RSDCOE(I2),RSDCOC(I2)
 7443   FORMAT(I9,F14.5,F13.1,'  ',F11.5,F11.5)
 7440 CONTINUE
C
C               *******************************************
C               **  STEP 75--                            **
C               **  GENERATE PHD ANALYSIS IF REQUESTED   **
C               *******************************************
C
      IF(IPHDFL.EQ.'OFF')GOTO8901
      IF(NUMFAC.GT.9)GOTO8901
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8201)
C8201 FORMAT('***** WARNING--THE PHD ANALYSIS IS STILL UNDER ',
CCCCC1       'DEVELOPMENT.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8203)
C8203 FORMAT('      OUTPUT GIVEN HERE SHOULD BE CONSIDEREDS TEST ',
CCCCC1       'OUTPUT.')
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
C
      REWIND(IOUNI1)
      REWIND(IOUNI2)
C
C     FORM VECTOR OF ESTIMATED MAIN EFFECTS.
C
      IMAX=2**NUMFAC
      DO8100I=1,IMAX
         IVAL=ITAGCO(I)
         IF(0.LE.IVAL.AND.IVAL.LE.9)THEN
            IUNITS=IVAL
            IF(IUNITS.LE.0)IUNITS=10
            AMAIN(IUNITS)=COEF(I)
         ENDIF
 8100 CONTINUE
C
C     FORM PREDICTED VALUES AND RESIDUALS FOR MAIN EFFECTS MODEL
C
      DO8110I=1,N
         SUM=0.0
         DO8120J=1,NUMFAC
            CALL YATES(I,J,XIJ)
            SUM=SUM+AMAIN(J)*XIJ
 8120    CONTINUE
         SUM=0.5*SUM
         SUM=SUM+GMEAN
         PREDLIN(I)=SUM
         RESLIN(I)=Y(I)-PREDLIN(I)
 8110 CONTINUE
C
C     FORM THE MATRIX OF 2-TERM INTERACTION EFFECTS
C
      DO8200I=1,NUMFAC
        A(I,I)=0.0
 8200 CONTINUE
C
      IMAX=2**NUMFAC
      DO8210I=1,IMAX
         IVAL=ITAGCO(I)
         IF(10.LE.IVAL.AND.IVAL.LE.99)THEN
            ITENS=IVAL/10
            ITERM=10*ITENS
            IUNITS=IVAL-ITERM
            IF(ITENS.LE.0)ITENS=10
            IF(IUNITS.LE.0)IUNITS=10
            A(ITENS,IUNITS)=COEF(I)
            A(IUNITS,ITENS)=COEF(I)
         ENDIF
 8210 CONTINUE
C
C     PRINT FIRST PHD TABLE
C
      ITITLE='Dex Principal Hessian Directions (PHD) Analysis'
      NCTITL=47
      ITITL9=' '
      NCTIT9=0
      ICNT=0
C
      DO5130J=1,NUMCLI
        DO5140I=1,MAXLIN
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5140   CONTINUE
 5130 CONTINUE
C
      ITITL2(2,1)='I'
      NCTIT2(2,1)=1
C
      ITITL2(2,2)='Identifier'
      NCTIT2(2,2)=10
C
      ITITL2(1,3)='Effect'
      NCTIT2(1,3)=6
      ITITL2(2,3)='Estimate'
      NCTIT2(2,3)=8
C
      NMAX=0
      NUMCOL=3
      DO5150I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2)IDIGIT(I)=0
 5150 CONTINUE
C
      IWHTML(1)=125
      IWHTML(2)=150
      IWHTML(3)=150
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
C
      IMAX=2**NUMFAC
      DO8250I=1,IMAX
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=REAL(ITAGCO(I))
        AMAT(I,3)=COEF(I)
 8250 CONTINUE
C
      NUMLIN=2
      NUMCOL=3
      ICNT=IMAX
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
      DO8300I=1,NUMFAC
        WRITE(IOUNI1,8306)(A(I,J),J=1,NUMFAC)
 8306   FORMAT(12(1X,E15.7))
 8300 CONTINUE
C
C     PRINT PHD TABLE  3: TABLE OF 2-TERM INTERACTIONS
C
      NLOOP=1
      IF(NUMFAC.GE.7 .AND. NUMFAC.LE.12)NLOOP=2
C
      DO5310ILOOP=1,NLOOP
C
        IFAC1=(ILOOP-1)*6 + 1
        IFAC2=ILOOP*6
        IF(IFAC2.GT.NUMFAC)IFAC2=NUMFAC
        NUMCOL=IFAC2-IFAC1+1
        ITITLE='2-Term Interaction Effects Matrix'
        NCTITL=33
        ITITL9='(Factors x to xx)'
        WRITE(ITITL9(10:10),'(I1)')IFAC1
        WRITE(ITITL9(15:16),'(I2)')IFAC2
        NCTIT9=17
        ICNT=0
C
        DO5330I=1,MAXLIN
          DO5340J=1,NUMCOL
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
            IF(I.EQ.1)THEN
              IVAL=IFAC1+J-1
              WRITE(ITITL2(1,J)(1:2),'(I2)')IVAL
              NCTIT2(I,J)=2
            ENDIF
 5340     CONTINUE
 5330   CONTINUE
C
        NMAX=0
        DO5350I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IWHTML(I)=125
 5350   CONTINUE
C
        IINC=1600
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC
        IWRTF(4)=IWRTF(2)+IINC
        IWRTF(5)=IWRTF(2)+IINC
        IWRTF(6)=IWRTF(2)+IINC
C
        DO5351J=IFAC1,IFAC2
          DO5355I=1,NUMFAC
              IVAL=IFAC1+J-1
              AMAT(I,IVAL)=A(I,IVAL)
 5355     CONTINUE
 5351   CONTINUE
C
        NUMLIN=1
        ICNT=NUMFAC
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
C
 5310 CONTINUE
C
C     DETERMINE THE EIGENVALUES AND EIGENVECTORS OF THE MATRIX
C
CCCCC AUGUST 1995.  REPLACE NUMERICAL RECIPES ROUTINE WITH
CCCCC EISPACK ROUTINE.  SSIEV IS FOR SYMMETRIC CASE.
CCCCC CALL JACOBI(A,NUMFAC,MAXCOL,EIGVAL,EIGVEC,JACROT)
      IERR2=0
      IJOB=1
      DO8341JJ=1,MAXFAC
      DO8342II=1,MAXFAC
        EIGVEC(II,JJ)=A(II,JJ)
 8342 CONTINUE
 8341 CONTINUE
      CALL SSIEV(EIGVEC,MAXFAC,NUMFAC,EIGVAL,VJUNK,IJOB,IERR2)
C
C     SINCE PRINT SORTED EIGENVALUES LATER, NO NEED TO PRINT HERE
C
CCCCC WRITE(ICOUT,999)
CCCCC CALL DPWRST('XXX','BUG ')
CCCCC DO8410I=1,NUMFAC
CCCCC   WRITE(ICOUT,8411)EIGVAL(I)
C8411   FORMAT(1X,10F10.3)
CCCCC   CALL DPWRST('XXX','BUG ')
C8410 CONTINUE
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      DO8510I=1,NUMFAC
        WRITE(IOUNI3,8512)(EIGVEC(I,J),J=1,NUMFAC)
 8512   FORMAT(12(1X,E15.7))
 8510 CONTINUE
C
C     PRINT PHD TABLE  4: TABLE OF EIGENVECTORS
C
      NLOOP=1
      IF(NUMFAC.GE.7 .AND. NUMFAC.LE.12)NLOOP=2
C
      DO5410ILOOP=1,NLOOP
C
        IFAC1=(ILOOP-1)*6 + 1
        IFAC2=ILOOP*6
        IF(IFAC2.GT.NUMFAC)IFAC2=NUMFAC
        NUMCOL=IFAC2-IFAC1+1
        ITITLE='Eigenvectors'
        NCTITL=12
        ITITL9='(Factors x to xx)'
        WRITE(ITITL9(10:10),'(I1)')IFAC1
        WRITE(ITITL9(15:16),'(I2)')IFAC2
        NCTIT9=17
        ICNT=0
C
        DO5430I=1,MAXLIN
          DO5440J=1,NUMCOL
            ITITL2(I,J)=' '
            NCTIT2(I,J)=0
            IF(I.EQ.1)THEN
              IVAL=IFAC1+J-1
              WRITE(ITITL2(I,J)(1:2),'(I2)')IVAL
              NCTIT2(I,J)=2
            ENDIF
 5440     CONTINUE
 5430   CONTINUE
C
        NMAX=0
        DO5450I=1,NUMCOL
          VALIGN(I)='b'
          ALIGN(I)='r'
          NTOT(I)=15
          NMAX=NMAX+NTOT(I)
          ITYPCO(I)='NUME'
          IDIGIT(I)=NUMDIG
          IWHTML(I)=125
 5450   CONTINUE
C
        IINC=1600
        IWRTF(1)=IINC
        IWRTF(2)=IWRTF(1)+IINC
        IWRTF(3)=IWRTF(2)+IINC
        IWRTF(4)=IWRTF(2)+IINC
        IWRTF(5)=IWRTF(2)+IINC
        IWRTF(6)=IWRTF(2)+IINC
C
        DO5451J=IFAC1,IFAC2
          DO5455I=1,NUMFAC
              IVAL=IFAC1+J-1
              AMAT(I,IVAL)=EIGVEC(I,IVAL)
 5455     CONTINUE
 5451   CONTINUE
C
        NUMLIN=1
        ICNT=NUMFAC
        IFRST=.TRUE.
        ILAST=.TRUE.
        IFLAGS=.TRUE.
        IFLAGE=.TRUE.
        CALL DPDTA5(ITITLE,NCTITL,
     1              ITITL9,NCTIT9,ITITL2,NCTIT2,
     1              MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1              IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1              IDIGIT,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1              ICAPSW,ICAPTY,IFRST,ILAST,
     1              IFLAGS,IFLAGE,
     1              ISUBRO,IBUGA3,IERROR)
C
 5410 CONTINUE
C
C     DETERMINE THE 2 LARGEST (IN MAGNITUDE) EIGENVALUES
C     AND WHAT EIGENVECTORS THEY ARE ASSOCIATED WITH
C
      DO8600I=1,NUMFAC
        EIGVA2(I)=ABS(EIGVAL(I))
        EIGVA2(I)=(-EIGVA2(I))
        ITAG2(I)=I
 8600 CONTINUE
C
      CALL SORTC3(EIGVA2,ITAG2,NUMFAC,EIGVA3,ITAG3)
C
      DO8610I=1,NUMFAC
        EIGVA2(I)=(-EIGVA2(I))
        EIGVA3(I)=(-EIGVA3(I))
 8610 CONTINUE
C
C     COMPUTE PHD'S
C
      INDEX1=ITAG3(1)
      INDEX2=ITAG3(2)
      INDEX3=ITAG3(3)
      INDEX4=ITAG3(4)
CCCCC APRIL 1996.  CHANGE FOLLOWING LINE
CCCCC INDEX4=ITAG3(5)
      INDEX5=ITAG3(5)
      DO8710I=1,N
         SUM1=0.0
         SUM2=0.0
         SUM3=0.0
         SUM4=0.0
         SUM5=0.0
         DO8720J=1,NUMFAC
            CALL YATES(I,J,XIJ)
            IF(INDEX1.GT.0)SUM1=SUM1+XIJ*EIGVEC(J,INDEX1)
            IF(INDEX2.GT.0)SUM2=SUM2+XIJ*EIGVEC(J,INDEX2)
            IF(INDEX3.GT.0)SUM3=SUM3+XIJ*EIGVEC(J,INDEX3)
            IF(INDEX4.GT.0)SUM4=SUM4+XIJ*EIGVEC(J,INDEX4)
            IF(INDEX5.GT.0)SUM5=SUM5+XIJ*EIGVEC(J,INDEX5)
 8720    CONTINUE
         PHD1(I)=SUM1
         PHD2(I)=SUM2
         PHD3(I)=SUM3
         PHD4(I)=SUM4
         PHD5(I)=SUM5
 8710 CONTINUE
C
CCCCC FEBRUARY 1995.  PRINT EIGENVALUES (UNORDERED AND ORDERED) TO
CCCCC FILE DPST2F.DAT
C
      DO8800I=1,NUMFAC
        WRITE(IOUNI2,8811)EIGVAL(I),ITAG3(I),EIGVAL(ITAG3(I))
 8811   FORMAT(1X,E15.7,1X,I5,1X,E15.7)
 8800 CONTINUE
C
C     PRINT PHD TABLE 5
C
      ITITLE='Absolute Value of the Sorted Eigenvalues'
      NCTITL=40
      ITITL9=' '
      NCTIT9=0
      ICNT=0
C
      DO5180J=1,NUMCLI
        DO5190I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5190   CONTINUE
 5180 CONTINUE
C
      ITITL2(1,1)='I'
      NCTIT2(1,1)=1
C
      ITITL2(1,2)='Identifier'
      NCTIT2(1,2)=10
C
      ITITL2(1,3)='Eigenvalue'
      NCTIT2(1,3)=10
C
      NMAX=0
      NUMCOL=3
      DO5550I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1 .OR. I.EQ.2)IDIGIT(I)=0
 5550 CONTINUE
C
      IWHTML(1)=125
      IWHTML(2)=150
      IWHTML(3)=150
      IINC=1800
      IINC2=1400
      IWRTF(1)=IINC
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
C
      DO8810I=1,NUMFAC
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=REAL(ITAG3(I))
        AMAT(I,3)=EIGVA3(I)
 8810 CONTINUE
C
      NUMLIN=1
      NUMCOL=3
      ICNT=NUMFAC
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
C     PRINT PHD TABLE  6
C
      ITITLE=' '
      NCTITL=0
      ITITL9=' '
      NCTIT9=0
      ICNT=0
C
      DO5630J=1,NUMCLI
        DO5640I=1,3
          ITITL2(I,J)=' '
          NCTIT2(I,J)=0
 5640   CONTINUE
 5630 CONTINUE
C
      ITITL2(1,1)='I'
      NCTIT2(1,1)=1
      ITITL2(1,2)='Y(I)'
      NCTIT2(1,2)=4
      ITITL2(1,3)='Predicted'
      NCTIT2(1,3)=9
      ITITL2(1,4)='Residual'
      NCTIT2(1,4)=8
      ITITL2(1,5)='PHD 1'
      NCTIT2(1,5)=5
      ITITL2(1,6)='PHD 2'
      NCTIT2(1,6)=5
C
      NMAX=0
      NUMCOL=6
      DO5650I=1,NUMCOL
        VALIGN(I)='b'
        ALIGN(I)='r'
        NTOT(I)=15
        NMAX=NMAX+NTOT(I)
        ITYPCO(I)='NUME'
        IDIGIT(I)=NUMDIG
        IF(I.EQ.1)IDIGIT(I)=0
 5650 CONTINUE
C
      IWHTML(1)=100
      IWHTML(2)=150
      IWHTML(3)=150
      IWHTML(4)=150
      IWHTML(5)=150
      IWHTML(6)=150
      IINC=1800
      IINC2=1000
      IWRTF(1)=IINC2
      IWRTF(2)=IWRTF(1)+IINC
      IWRTF(3)=IWRTF(2)+IINC
      IWRTF(4)=IWRTF(2)+IINC
      IWRTF(5)=IWRTF(2)+IINC
      IWRTF(6)=IWRTF(2)+IINC
C
      DO8820I=1,N
        AMAT(I,1)=REAL(I)
        AMAT(I,2)=Y(I)
        AMAT(I,3)=PREDLIN(I)
        AMAT(I,4)=RESLIN(I)
        AMAT(I,5)=PHD1(I)
        AMAT(I,6)=PHD2(I)
 8820 CONTINUE
C
      NUMLIN=1
      NUMCOL=6
      ICNT=IMAX
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
CCCCC FEBRUARY 1995.  PRINT PRED, RES, AND FIRST 5 PHD EIGENVECTORS
CCCCC TO FILE DPST4F.DAT
C
      DO8860I=1,N
        WRITE(IOUNI4,8870)PREDLIN(I),RESLIN(I),PHD1(I),PHD2(I),
     1                    PHD3(I),PHD4(I),PHD5(I)
 8870   FORMAT(7(1X,E15.7))
        CALL DPWRST('XXX','BUG ')
 8860 CONTINUE
C
 8901 CONTINUE
C
CCCCC THE FOLLOWING WAS ADDED OCTOBER 1991
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
C
        IF(IPHDFL.EQ.'OFF')THEN
          WRITE(ICOUT,8902)
 8902     FORMAT('NOTE--TAG, COEF, TCOEF, RESSD, AND CUMULATIVE RESSD')
          CALL DPWRST('XXX','BUG ')
C
          WRITE(ICOUT,8903)
 8903     FORMAT('      WERE WRITTEN OUT TO FILES DPST1F.DAT AND ',
     1           'DPST2F.DAT')
          CALL DPWRST('XXX','BUG ')
C
          WRITE(ICOUT,8904)
 8904     FORMAT('      TO READ THESE VARIABLES BACK IN, ENTER   ')
          CALL DPWRST('XXX','BUG ')
C
          WRITE(ICOUT,8905)
 8905     FORMAT('         SKIP 0')
          CALL DPWRST('XXX','BUG ')
C
          WRITE(ICOUT,8906)
 8906     FORMAT('         READ DPST1F.DAT TAG COEF TCOEF RSD CUMRSD')
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,8891)
 8891     FORMAT('NOTE--THE MATRIX OF 2-TERM INTERACTIONS WRITTEN TO ',
     1           'FILE DPST1F.DAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8892)
 8892     FORMAT('    --THE EIGENVALUES OF THE MATRIX WRITTEN TO ',
     1           'FILE DPST3F.DAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8893)
 8893     FORMAT('    --THE EIGENVECTORS OF THE MATRIX WRITTEN TO ',
     1           'FILE DPST3F.DAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,8894)
 8894     FORMAT('    --THE PREDICTED, VALUES, RESIDUALS, AND FIRST 5 ',
     1           'PHD VECOTRS WRITTEN TO FILE DPST4F.DAT')
          CALL DPWRST('XXX','BUG ')
C
        ENDIF
      ENDIF
C
CCCCCC THE FOLLOWING SECTION WAS ADDED OCTOBER 1991
C               **************************************
C               **  STEP 89--                       **
C               **  CLOSE       THE STORAGE FILES.  **
C               **************************************
C
      ISTEPN='89'
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IOP='CLOS'
      CALL DPAUFI(IOP,IFLAG1,IFLAG2,IFLAG3,IFLAG4,IFLAG5,
     1            IOUNI1,IOUNI2,IOUNI3,IOUNI4,IOUNI5,
     1            IBUGA3,ISUBRO,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGA3.EQ.'ON'.OR.ISUBRO.EQ.'YAT2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPYAT2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR
 9012   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)N,NUMREP,IREP,ICASE
 9013   FORMAT('N,NUMREP,IREP,ICASE = ',2I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)GMEAN,GSSQ,GVAR,GSD
 9014   FORMAT('GMEAN,GSSQ,GVAR,GSD = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)PRESSD,PRESDF,REPSD,REPDF
 9015   FORMAT('PRESSD,PRESDF,REPSD,REPDF = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)REFDF,REFDF,SDCOEF
 9016   FORMAT('REFDF,REFDF,SDCOEF = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NMEAN
          WRITE(ICOUT,9022)I,YMEAN(I),COEF(I),YVAR(I)
 9022     FORMAT('I,YMEAN(I),COEF(I),YVAR(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
        WRITE(ICOUT,9031)YATCCU,YATTCU,YATRCU,IYATOS,IYATRS
 9031   FORMAT('YATCCU,YATTCU,YATRCU,IYATOS,IYATRS = ',3G15.7,
     1         2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)CCUTN,CCUTP,TCUTN,TCUTP,RCUTN,RCUTP
 9032   FORMAT('CCUTN,CCUTP,TCUTN,TCYTP,RCUTN,RCUTP = ',6G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPZSCC(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--GIVEN PROFICIENCY DATA
C
C                 ZSCORE  MATID  ROUNDID  LABID
C
C              USE MATID AND ROUNDID TO DEFINE A GRID.  IF LABID IS
C              GIVEN, THEN COMPUTE AN AVERAGE Z-SCORE FOR EACH
C              ROUND/MATERIAL COMBINATION.  THE Z-SCORE WILL BE USED
C              TO DEFINE A PLOT SYMBOL BASED ON
C
C                        Z <= -3
C                   -3 < Z <  -2
C                   -2 < Z <  +2
C                   -2 < Z <  +2
C                        Z >= +3
C
C              THE Z-SCORE IS AN ISO 13528 STANDARD Z-SCORE.  NOTE THAT
C              THIS STANDARD ALLOWS FOR SEVERAL DIFFERENT VARIATIONS OF
C              THE Z-SCORE, SO THIS COMMAND ASSUMES THE Z-SCORE IS ALREADY
C              COMPUTED (I.E., IT IS NOT COMPUTED FROM THE DATA.
C
C              AN ALTERNATIVE VERSION OF THIS PLOT IS A J-CHART (OR ZONE
C              CHART).
C
C     EXAMPLE--ISO 13528 ZSCORE PLOT Z MATID ROUNDID
C              ISO 13528 JSCORE PLOT Z MATID ROUNDID
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
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZZ.INC'
C
      REAL Z(MAXOBV)
      REAL ROUND(MAXOBV)
      REAL MATID(MAXOBV)
      REAL XIDTEM(MAXOBV)
      REAL XIDTE2(MAXOBV)
      REAL TEMP1(MAXOBV)
      REAL TEMP2(MAXOBV)
      REAL TEMP3(MAXOBV)
      REAL TEMP4(MAXOBV)
      REAL TEMP5(MAXOBV)
C
      EQUIVALENCE (GARBAG(IGARB1),Z(1))
      EQUIVALENCE (GARBAG(IGARB2),ROUND(1))
      EQUIVALENCE (GARBAG(IGARB3),MATID(1))
      EQUIVALENCE (GARBAG(IGARB4),XIDTEM(1))
      EQUIVALENCE (GARBAG(IGARB5),XIDTE2(1))
      EQUIVALENCE (GARBAG(IGARB6),TEMP1(1))
      EQUIVALENCE (GARBAG(IGARB7),TEMP2(1))
      EQUIVALENCE (GARBAG(IGARB8),TEMP3(1))
      EQUIVALENCE (GARBAG(IGARB9),TEMP4(1))
      EQUIVALENCE (GARBAG(IGAR10),TEMP5(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
      IFOUND='NO'
      ISUBN1='DPZS'
      ISUBN2='CC  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C               ****************************************
C               **  TREAT THE DEX CONTOUR PLOT CASE   **
C               ****************************************
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPZSCC--')
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(NUMARG.GE.2 .AND. ICOM.EQ.'ISO ' .AND.IHARG(1).EQ.'1352')THEN
        IF(IHARG(2).EQ.'ZSCO' .AND. IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          ICASPL='ZSCC'
        ELSEIF(IHARG(2).EQ.'JSCO' .AND. IHARG(3).EQ.'PLOT')THEN
          ILASTC=3
          ICASPL='JSCC'
        ENDIF
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
        IFOUND='YES'
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      INAME='ISO 13528 ZSCORE PLOT'
      IF(ICASPL.EQ.'JSCC')INAME='ISO 13528 JSCORE PLOT'
      MINNA=3
      MAXNA=100
      MINN2=2
      IFLAGE=1
      IFLAGM=0
      IFLAGP=0
      JMIN=1
      JMAX=NUMARG
      MINNVA=3
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')THEN
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
C               **       MATID(.)                           **
C               **       ROUND(.)                           **
C               **  CONTAINING                              **
C               **       THE Z-SCORE OF THE RESPONSE        **
C               **       THE MATERIAL-ID                    **
C               **       THE ROUND-ID                       **
C               **  RESPECTIVELY.                           **
C               **********************************************
C
      ISTEPN='33'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICOL=1
      CALL DPPAR5(ICOL,IVALUE,IVALU2,IN,MAXN,MAXOBV,
     1            INAME,IVARN1,IVARN2,IVARTY,
     1            ILIS,NRIGHT,ICOLR,ISUB,NQ,NUMVAR,
     1            MAXCOL,MAXCP1,MAXCP2,MAXCP3,
     1            MAXCP4,MAXCP5,MAXCP6,
     1            V,PRED,RES,YPLOT,XPLOT,X2PLOT,TAGPLO,
     1            Z,MATID,ROUND,XIDTEM,XIDTEM,XIDTEM,XIDTEM,NS,
     1            IBUGG3,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
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
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      CALL DPZSC2(Z,MATID,ROUND,NS,NUMVAR,ICASPL,
     1            XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1            Y,X,D,
     1            NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 9--   **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'ZSCC')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPZSCC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9014   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPZSC2(Z,MATID,ROUND,N,NUMVAR,ICASPL,
     1                  XIDTEM,XIDTE2,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,
     1                  Y,X,D,
     1                  NPLOTP,NPLOTV,IBUGG3,ISUBRO,IERROR)
C
C     PURPOSE--GIVEN PROFICIENCY DATA
C
C                 ZSCORE  MATID  ROUNDID  LABID
C
C              USE MATID AND ROUNDID TO DEFINE A GRID.  IF LABID IS
C              GIVEN, THEN COMPUTE AN AVERAGE Z-SCORE FOR EACH
C              ROUND/MATERIAL COMBINATION.  THE Z-SCORE WILL BE USED
C              TO DEFINE A PLOT SYMBOL BASED ON
C
C                        Z <= -3
C                   -3 < Z <  -2
C                   -2 < Z <  +2
C                   -2 < Z <  +2
C                        Z >= +3
C
C              THE Z-SCORE IS AN ISO 13528 STANDARD Z-SCORE.  NOTE THAT
C              THIS STANDARD ALLOWS FOR SEVERAL DIFFERENT VARIATIONS OF
C              THE Z-SCORE, SO THIS COMMAND ASSUMES THE Z-SCORE IS ALREADY
C              COMPUTED (I.E., IT IS NOT COMPUTED FROM THE DATA.
C
C              AN ALTERNATIVE VERSION OF THIS PLOT IS A J-CHART (OR ZONE
C              CHART).
C
C     REFERENCE--ISO 13528 (2005), "Statistical Methods for use in
C                proficiency testing by interlaboratory comparisons,"
C                First Edition, 2005-09-01, pp. 56-57.
C              --MICHAEL THOMPSON, STEPHEN ELLISON, ROGER WOOD (2006),
C                THE INTERNATIONAL HARMONIZED PROTOCOL FOR THE
C                PROFICIENCY TESTING OF ANALYTICAL CHEMISTRY
C                LABORATORIES,Pure Applied Chemistry, Vol. 78, No. 1,
C                pp. 145-196.
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
      CHARACTER*4 IBUGG3
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 IWRITE
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
      DOUBLE PRECISION DSUM1
C
C---------------------------------------------------------------------
C
      REAL Z(*)
      REAL MATID(*)
      REAL ROUND(*)
      REAL XIDTEM(*)
      REAL XIDTE2(*)
      REAL TEMP1(*)
      REAL TEMP2(*)
      REAL TEMP3(*)
      REAL TEMP4(*)
      REAL TEMP5(*)
C
      REAL Y(*)
      REAL X(*)
      REAL D(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPZS'
      ISUBN2='C2  '
      IWRITE='OFF'
C
      IERROR='NO'
      NPLOTP=0
      NPLOTV=3
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ZSC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,71)
   71   FORMAT('***** AT THE BEGINNING OF DPZSC2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,72)IBUGG3,ISUBRO,ICASPL,N,NUMVAR
   72   FORMAT('IBUGG3,ISUBRO,ICASPL,N,NUMVAR = ',3(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        IF(N.GT.0)THEN
          DO81I=1,N
            WRITE(ICOUT,82)I,Z(I),MATID(I),ROUND(I)
   82       FORMAT('I,Z(I),MATID(I),ROUND(I) = ',I8,3G15.7)
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
   31   FORMAT('***** ERROR IN ISO 13528 ZSCORE PLOT--')
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
C               **  DETERMINE UNIQUE VALUES OF IGINAL     **
C               **  UNITS) FOR EACH ROUND OVER ALL LABS.  **
C               ********************************************
C
      IWRITE='OFF'
      CALL DISTIN(ROUND,N,IWRITE,XIDTEM,NROUND,IBUGG3,IERROR)
      CALL SORT(XIDTEM,NROUND,XIDTEM)
      CALL DISTIN(MATID,N,IWRITE,XIDTE2,NMAT,IBUGG3,IERROR)
      CALL SORT(XIDTE2,NMAT,XIDTE2)
C
C
C               ********************************************
C               **  STEP 3--                              **
C               **  GENERATE THE PLOT COORDINATES.        **
C               ********************************************
C
      IF(ICASPL.EQ.'ZSCC')THEN
        NPLOTP=0
        DO1010J=1,NMAT
          HOLD2=XIDTE2(J)
          DO1020I=1,NROUND
            HOLD=XIDTEM(I)
            DSUM1=0.0D0
            K=0
            DO1030L=1,N
              IF(ROUND(L).EQ.HOLD .AND. MATID(L).EQ.HOLD2)THEN
                K=K+1
                DSUM1=DSUM1 + DBLE(Z(L))
              ENDIF
 1030       CONTINUE
            IF(K.GT.0)THEN
              ATEMP=REAL(DSUM1/DBLE(K))
              NPLOTP=NPLOTP+1
              Y(NPLOTP)=HOLD2
              X(NPLOTP)=HOLD
              IF(ATEMP.LE.-3.0)THEN
                D(NPLOTP)=5.0
              ELSEIF(ATEMP.GE.3.0)THEN
                D(NPLOTP)=4.0
              ELSEIF(ATEMP.LE.-2.0)THEN
                D(NPLOTP)=3.0
              ELSEIF(ATEMP.GE.2.0)THEN
                D(NPLOTP)=2.0
              ELSE
                D(NPLOTP)=1.0
              ENDIF
            ENDIF
 1020     CONTINUE
 1010   CONTINUE
C
      ELSEIF(ICASPL.EQ.'JSCC')THEN
C
C       2015/10: USE "JSCORE" ROUTINE.  EXTRACT THE Z-SCORES AND
C                ROUND-ID DATA FOR A GIVEN MATERIAL.
C
        NPLOTP=0
        DO2010J=1,NMAT
          HOLD2=XIDTE2(J)
          K=0
          DO2030L=1,N
            IF(MATID(L).EQ.HOLD2)THEN
              K=K+1
              TEMP1(K)=Z(L)
              TEMP2(K)=ROUND(L)
            ENDIF
 2030     CONTINUE
          IF(K.LE.0)GOTO2010
C
          CALL JSCORE(TEMP1,TEMP2,K,IWRITE,TEMP3,TEMP4,TEMP5,NY,
     1                IBUGG3,ISUBRO,IERROR)
          DO2040JJ=1,NY
            NPLOTP=NPLOTP+1
            Y(NPLOTP)=HOLD2
            X(NPLOTP)=TEMP3(JJ)
            ZSCORE=TEMP5(JJ)
C
            IF(ZSCORE.LE.-8.0)THEN
              D(NPLOTP)=9.0
            ELSEIF(ZSCORE.LE.-6.0)THEN
              D(NPLOTP)=7.0
            ELSEIF(ZSCORE.LE.-4.0)THEN
              D(NPLOTP)=5.0
            ELSEIF(ZSCORE.LE.-2.0)THEN
              D(NPLOTP)=3.0
            ELSEIF(ZSCORE.GE.8.0)THEN
              D(NPLOTP)=8.0
            ELSEIF(ZSCORE.GE.6.0)THEN
              D(NPLOTP)=6.0
            ELSEIF(ZSCORE.GE.4.0)THEN
              D(NPLOTP)=4.0
            ELSEIF(ZSCORE.GE.2.0)THEN
              D(NPLOTP)=2.0
            ELSE
              D(NPLOTP)=1.0
            ENDIF
 2040     CONTINUE
 2010   CONTINUE
C
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'ZSC2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPISO2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IERROR,NPLOTP,NPLOTV
 9013   FORMAT('IERROR,NPLOTP,NPLOTV = ',A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NPLOTP.GT.0)THEN
          DO9035I=1,NPLOTP
            WRITE(ICOUT,9036)I,Y(I),X(I),D(I)
 9036       FORMAT('I,Y(I),X(I),D(I) = ',I8,2G15.7,F9.2)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END

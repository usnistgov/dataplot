C     NOTE THAT THERE ARE MANY APPROACHES AND ALGORITHMS FOR
C     PERFORMING CLUSTER ANALYSIS.  THIS FILE CONTAINS ROUTINES
C     FROM THE FOLLOWING SOURCES:
C
C     1) JOHN HARTIGAN (1979), "ALGORITHM AS 136", APPLIED
C        STATISTICS, VOL. 28, NO. 1.
C
C     2) JOHN HARTIGAN (1975), "CLUSTERING ALGORITHMS",
C        WILEY.
C
C        WE ACTUALLY USE THE ROUTINES AS EXTRACTED FROM THE
C        CMLIB LIBRARY.  THESE ARE SLIGHTLY DIFFERENT THAN THE
C        ROUTINES AS GIVEN IN THE BOOK.
C
C        WE DO INCLUDE ALL THE ROUTINES.  ALSO, WE USE THE
C        K-MEANS ALGORITHM FROM APPLIED STATISTICS RATHER THAN
C        THE VERSION FROM THE BOOK (THE APPLIED STATISTICS
C        VERSION IS SOMEWHAT SIMPLER THAN THE BOOK VERSION).
C
C     3) KAUFMAN AND ROUSSEEUW (1990), "FINDING GROUPS IN
C        DATA", WILEY.
C
C        THESE ROUTINES ARE DESIGNED TO BE MORE ROBUST THAN
C        SOME OF THE STANDARD CLUSTERING ALGORITHMS.
C
C        SPECIFICALLY, WE SUPPORT THE K-MEDOIDS METHODS FROM
C        THE "PAM" AND "CLARA" ALGORITHMS.
C
C        KAUFMAN AND ROUSSEEUW SUPPORT THE FOLLOWING PROGRAMS:
C
C        1. DAISY  - FOR CREATING DISSIMILARITY MATRICES
C                    (DATAPLOT HAS COMMANDS TO DO WHAT DAISY
C                    CAN DO, SO NOTHING EXPLICITLY IMPLEMENTED
C                    FROM DAISY)
C        2. PAM    - PARTITIONING AROUND MEDOIDS BASED ON EITHER
C                    MEASUREMENT DATA OR A DISSIMILARITY MATRIX.
C                    CURRENTLY LIMITED TO A MAXIMUM OF 100 OBJECTS.
C        3. CLARA  - PARTITIONING AROUND MEDOIDS FOR THE CASE OF MORE
C                    THAN 100 OBJECTS.  ONLY APPLIED TO MEASUREMENT
C                    DATA.
C        4. FANNY  - FUZZY CLUSTERING.  NOT CURRENTLY IMPLEMENTED.
C        5. AGNES  - HIERARCHIAL CLUSTERING (OR AGGLOMERATIVE NESTING).
C        6. MONA   - HIERARCHIAL CLUSTERING FOR BINARY DATA.  NOT
C                    CURRENTLY SUPPORTED.
C
C     ALSO INCLUDE ROUTINES FROM APPLIED STATISTICS 136
C     SPECIFICALLY FOR K-MEANS CLUSTERING.
C
C     LIST OF ROUTINES:
C
C     HARTIGAN'S K-MEANS FROM APPLIED STATISTICS 136:
C
C        1. KMNS   - THE ROUTINE THAT IS CALLED FROM DATAPLOT
C        2. OPTRA  - OPTIMAL TRANSFER STAGE
C        3. QTRAN  - QUICK TRANSFER STAGE
C
C     ADDITIONAL HARTIGAN CLUSTERING CODES.  CURRENTLY WE USE
C     "MIX" FOR NORMAL MIXTURE MODELS AND SLINK FOR SINGLE
C     LINKAGE (NEAREST NEIGHBOR) CLUSTERING.  NOT ALL OF THESE
C     ROUTINES ARE CURRENTLY ACTIVELY USED.
C
C         1. MIX    - PERFORMS NORMAL MIXTURE CLUSTERING
C         2. COVOUT - USED BY MIX TO PRINT OUTPUT
C         3. INVERT - USED BY MIX TO INVERT A MATRIX
C         4. CLUMOM - USED BY MIX TO COMPUTE WEIGHTED MEANS AND
C                     STANDARD DEVIATIONS
C
C         5. MIXIND - NORMAL MIXTURE WITH SPECIFIC COVARIANCE
C                     MODEL (NOT CURRENTLY USED)
C         6. MIXOUT - PRINT OUTPUT OF MIXIND
C
C        FOLLOWING ARE NOT ACTIVELY CALLED
C
C         7. SLINK  - PERFORMS SINGLE LINKAGE CLUSTERING
C         8. BUILD  - K-MEANS FROM CMLIB
C         9. KMEANS - (CALLED BY BUILD)
C        10. SINGLE - CALLED BY BUILD/KMEANS
C        11. KOUT   - CALLED BY BUILD/KMEANS
C
C        12. SPLIT1 - SPLITTING ALGORITHM FOR CLUSTERING
C                     (NOT CURRENTLY USED)
C        13. SPLIT2 - SPLITTING ALGORITHM FOR CLUSTERING
C                     (NOT CURRENTLY USED)
C        14. CSPLIT - USED BY SPLIT1/SPLIT2
C        15. RSPLIT - USED BY SPLIT1/SPLIT2

C        16. QUICK  - PERFORMS A "QUICK" CLUSTERING (NOT
C                    CURRENTLY USED)
C
C     ROUTINES FROM KAUFFMAN AND ROUSSEEUW
C
C        1. BSWAP  (FOR CLARA AND PAM)
C        2. DYSTA  (FOR CLARA)
C        3. DYSTAP (FOR PAM)
C        4. DYSTAF (FOR FANNY)
C        5. MEET   (FOR CLARA AND PAM)
C        6. RESUL  (FOR CLARA)
C        7. SELEC  (FOR CLARA)
C        8. CSTAT  (PAM)
C        9. SUPCL  (AGNES)
C       10. AVERL  (AGNES)
C       11. BANAG  (AGNES)
C       12. SPLYT  (DIANA)
C       13. BANDY  (DIANA)
C       14. CADDY  (FANNY)
C       15. FUZZY  (FANNY)
C
      SUBROUTINE KMNS(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *    ITRAN, LIVE, ITER, WSS, IFAULT)
C
C     ALGORITHM AS 136  APPL. STATIST. (1979) VOL.28, NO.1
C
C     Divide M points in N-dimensional space into K clusters so that
C     the within cluster sum of squares is minimized.
C
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), WSS(K), DT(2)
      REAL    ZERO, ONE
C
C     Define BIG to be a very large positive number
C
      DATA BIG /1.E30/, ZERO /0.0/, ONE /1.0/
C
      IFAULT = 3
      IF (K .LE. 1 .OR. K .GE. M) RETURN
      IFAULT = 0
C
C     For each point I, find its two closest centres, IC1(I) and
C     IC2(I).     Assign it to IC1(I).
C
      DO 50 I = 1, M
      IC1(I) = 1
      IC2(I) = 2
      DO 10 IL = 1, 2
        DT(IL) = ZERO
        DO 10 J = 1, N
          DA = A(I,J) - C(IL,J)
          DT(IL) = DT(IL) + DA*DA
   10   CONTINUE
      IF (DT(1) .GT. DT(2)) THEN
        IC1(I) = 2
        IC2(I) = 1
        TEMP = DT(1)
        DT(1) = DT(2)
        DT(2) = TEMP
      END IF
      DO 50 L = 3, K
        DB = ZERO
        DO 30 J = 1, N
          DC = A(I,J) - C(L,J)
          DB = DB + DC*DC
          IF (DB .GE. DT(2)) GO TO 50
   30     CONTINUE
        IF (DB .LT. DT(1)) GO TO 40
        DT(2) = DB
        IC2(I) = L
        GO TO 50
   40     DT(2) = DT(1)
        IC2(I) = IC1(I)
        DT(1) = DB
        IC1(I) = L
   50 CONTINUE
C
C     Update cluster centres to be the average of points contained
C     within them.
C
      DO 70 L = 1, K
      NC(L) = 0
      DO 60 J = 1, N
   60   C(L,J) = ZERO
   70 CONTINUE
      DO 90 I = 1, M
      L = IC1(I)
      NC(L) = NC(L) + 1
      DO 80 J = 1, N
   80   C(L,J) = C(L,J) + A(I,J)
   90 CONTINUE
C
C     Check to see if there is any empty cluster at this stage
C
      DO 120 L = 1, K
      IF (NC(L) .EQ. 0) THEN
        IFAULT = 1
        RETURN
      END IF
      IFAULT = 0
      AA = NC(L)
      DO 110 J = 1, N
  110   C(L,J) = C(L,J) / AA
C
C     Initialize AN1, AN2, ITRAN & NCP
C     AN1(L) = NC(L) / (NC(L) - 1)
C     AN2(L) = NC(L) / (NC(L) + 1)
C     ITRAN(L) = 1 if cluster L is updated in the quick-transfer stage,
C              = 0 otherwise
C     In the optimal-transfer stage, NCP(L) stores the step at which
C     cluster L is last updated.
C     In the quick-transfer stage, NCP(L) stores the step at which
C     cluster L is last updated plus M.
C
      AN2(L) = AA / (AA + ONE)
      AN1(L) = BIG
      IF (AA .GT. ONE) AN1(L) = AA / (AA - ONE)
      ITRAN(L) = 1
      NCP(L) = -1
  120 CONTINUE
      INDX = 0
      DO 140 IJ = 1, ITER
C
C     In this stage, there is only one pass through the data.   Each
C     point is re-allocated, if necessary, to the cluster that will
C     induce the maximum reduction in within-cluster sum of squares.
C
      CALL OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *        ITRAN, LIVE, INDX)
C
C     Stop if no transfer took place in the last M optimal transfer
C     steps.
C
      IF (INDX .EQ. M) GO TO 150
C
C     Each point is tested in turn to see if it should be re-allocated
C     to the cluster to which it is most likely to be transferred,
C     IC2(I), from its present cluster, IC1(I).   Loop through the
C     data until no further change is to take place.
C
      CALL QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *       ITRAN, INDX)
C
C     If there are only two clusters, there is no need to re-enter the
C     optimal transfer stage.
C
      IF (K .EQ. 2) GO TO 150
C
C     NCP has to be set to 0 before entering OPTRA.
C
      DO 130 L = 1, K
  130   NCP(L) = 0
  140 CONTINUE
C
C     Since the specified number of iterations has been exceeded, set
C     IFAULT = 2.   This may indicate unforeseen looping.
C
      IFAULT = 2
C
C     Compute within-cluster sum of squares for each cluster.
C
  150 DO 160 L = 1, K
      WSS(L) = ZERO
      DO 160 J = 1, N
        C(L,J) = ZERO
  160 CONTINUE
      DO 170 I = 1, M
      II = IC1(I)
      DO 170 J = 1, N
        C(II,J) = C(II,J) + A(I,J)
  170 CONTINUE
      DO 190 J = 1, N
      DO 180 L = 1, K
  180   C(L,J) = C(L,J) / FLOAT(NC(L))
      DO 190 I = 1, M
        II = IC1(I)
        DA = A(I,J) - C(II,J)
        WSS(II) = WSS(II) + DA*DA

  190 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *      ITRAN, LIVE, INDX)
C
C     ALGORITHM AS 136.1  APPL. STATIST. (1979) VOL.28, NO.1
C
C     This is the optimal transfer stage.
C
C     Each point is re-allocated, if necessary, to the cluster that
C     will induce a maximum reduction in the within-cluster sum of
C     squares.
C
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), ZERO, ONE
C
C     Define BIG to be a very large positive number.
C
      DATA BIG /1.0E30/, ZERO /0.0/, ONE/1.0/
C
C     If cluster L is updated in the last quick-transfer stage, it
C     belongs to the live set throughout this stage.   Otherwise, at
C     each step, it is not in the live set if it has not been updated
C     in the last M optimal transfer steps.
C
      DO 10 L = 1, K
      IF (ITRAN(L) .EQ. 1) LIVE(L) = M + 1
   10 CONTINUE
      DO 100 I = 1, M
      INDX = INDX + 1
      L1 = IC1(I)
      L2 = IC2(I)
      LL = L2
C
C     If point I is the only member of cluster L1, no transfer.
C
      IF (NC(L1) .EQ. 1) GO TO 90
C
C     If L1 has not yet been updated in this stage, no need to
C     re-compute D(I).
C
      IF (NCP(L1) .EQ. 0) GO TO 30
      DE = ZERO
      DO 20 J = 1, N
        DF = A(I,J) - C(L1,J)
        DE = DE + DF*DF
   20   CONTINUE
      D(I) = DE * AN1(L1)
C
C     Find the cluster with minimum R2.
C
   30   DA = ZERO
      DO 40 J = 1, N
        DB = A(I,J) - C(L2,J)
        DA = DA + DB*DB
   40   CONTINUE
      R2 = DA * AN2(L2)
      DO 60 L = 1, K
C
C     If I >= LIVE(L1), then L1 is not in the live set.   If this is
C     true, we only need to consider clusters that are in the live set
C     for possible transfer of point I.   Otherwise, we need to consider
C     all possible clusters.
C
        IF (I .GE. LIVE(L1) .AND. I .GE. LIVE(L) .OR. L .EQ. L1 .OR.
     *        L .EQ. LL) GO TO 60
        RR = R2 / AN2(L)
        DC = ZERO
        DO 50 J = 1, N
          DD = A(I,J) - C(L,J)
          DC = DC + DD*DD
          IF (DC .GE. RR) GO TO 60
   50     CONTINUE
        R2 = DC * AN2(L)
        L2 = L
   60     CONTINUE
        IF (R2 .LT. D(I)) GO TO 70
C
C     If no transfer is necessary, L2 is the new IC2(I).
C
        IC2(I) = L2
        GO TO 90
C
C     Update cluster centres, LIVE, NCP, AN1 & AN2 for clusters L1 and
C     L2, and update IC1(I) & IC2(I).
C
   70     INDX = 0
        LIVE(L1) = M + I
        LIVE(L2) = M + I
        NCP(L1) = I
        NCP(L2) = I
        AL1 = NC(L1)
        ALW = AL1 - ONE
        AL2 = NC(L2)
        ALT = AL2 + ONE
        DO 80 J = 1, N
          C(L1,J) = (C(L1,J) * AL1 - A(I,J)) / ALW
          C(L2,J) = (C(L2,J) * AL2 + A(I,J)) / ALT
   80     CONTINUE
        NC(L1) = NC(L1) - 1
        NC(L2) = NC(L2) + 1
        AN2(L1) = ALW / AL1
        AN1(L1) = BIG
        IF (ALW .GT. ONE) AN1(L1) = ALW / (ALW - ONE)
        AN1(L2) = ALT / AL2
        AN2(L2) = ALT / (ALT + ONE)
        IC1(I) = L2
        IC2(I) = L1
   90   CONTINUE
      IF (INDX .EQ. M) RETURN
  100 CONTINUE
      DO 110 L = 1, K
C
C     ITRAN(L) = 0 before entering QTRAN.   Also, LIVE(L) has to be
C     decreased by M before re-entering OPTRA.
C
      ITRAN(L) = 0
      LIVE(L) = LIVE(L) - M
  110 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *    ITRAN, INDX)
C
C     ALGORITHM AS 136.2  APPL. STATIST. (1979) VOL.28, NO.1
C
C     This is the quick transfer stage.
C     IC1(I) is the cluster which point I belongs to.
C     IC2(I) is the cluster which point I is most likely to be
C         transferred to.
C     For each point I, IC1(I) & IC2(I) are switched, if necessary, to
C     reduce within-cluster sum of squares.  The cluster centres are
C     updated after each step.
C
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K)
      REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), ZERO, ONE
C
C     Define BIG to be a very large positive number
C
      DATA BIG /1.0E30/, ZERO /0.0/, ONE /1.0/
C
C     In the optimal transfer stage, NCP(L) indicates the step at which
C     cluster L is last updated.   In the quick transfer stage, NCP(L)
C     is equal to the step at which cluster L is last updated plus M.
C
      ICOUN = 0
      ISTEP = 0
   10 DO 70 I = 1, M
      ICOUN = ICOUN + 1
      ISTEP = ISTEP + 1
      L1 = IC1(I)
      L2 = IC2(I)
C
C     If point I is the only member of cluster L1, no transfer.
C
      IF (NC(L1) .EQ. 1) GO TO 60
C
C     If ISTEP > NCP(L1), no need to re-compute distance from point I to
C     cluster L1.   Note that if cluster L1 is last updated exactly M
C     steps ago, we still need to compute the distance from point I to
C     cluster L1.
C
      IF (ISTEP .GT. NCP(L1)) GO TO 30
      DA = ZERO
      DO 20 J = 1, N
        DB = A(I,J) - C(L1,J)
        DA = DA + DB*DB
   20   CONTINUE
      D(I) = DA * AN1(L1)
C
C     If ISTEP >= both NCP(L1) & NCP(L2) there will be no transfer of
C     point I at this step.
C
   30   IF (ISTEP .GE. NCP(L1) .AND. ISTEP .GE. NCP(L2)) GO TO 60
      R2 = D(I) / AN2(L2)
      DD = ZERO
      DO 40 J = 1, N
        DE = A(I,J) - C(L2,J)
        DD = DD + DE*DE
        IF (DD .GE. R2) GO TO 60
   40   CONTINUE
C
C     Update cluster centres, NCP, NC, ITRAN, AN1 & AN2 for clusters
C     L1 & L2.   Also update IC1(I) & IC2(I).   Note that if any
C     updating occurs in this stage, INDX is set back to 0.
C
      ICOUN = 0
      INDX = 0
      ITRAN(L1) = 1
      ITRAN(L2) = 1
      NCP(L1) = ISTEP + M
      NCP(L2) = ISTEP + M
      AL1 = NC(L1)
      ALW = AL1 - ONE
      AL2 = NC(L2)
      ALT = AL2 + ONE
      DO 50 J = 1, N
        C(L1,J) = (C(L1,J) * AL1 - A(I,J)) / ALW
        C(L2,J) = (C(L2,J) * AL2 + A(I,J)) / ALT
   50   CONTINUE
      NC(L1) = NC(L1) - 1
      NC(L2) = NC(L2) + 1
      AN2(L1) = ALW / AL1
      AN1(L1) = BIG
      IF (ALW .GT. ONE) AN1(L1) = ALW / (ALW - ONE)
      AN1(L2) = ALT / AL2
      AN2(L2) = ALT / (ALT + ONE)
      IC1(I) = L2
      IC2(I) = L1
C
C     If no re-allocation took place in the last M steps, return.
C
   60   IF (ICOUN .EQ. M) RETURN
   70 CONTINUE
      GO TO 10
      END
      SUBROUTINE BLOCK(MM, M, N, D, CLAB, RLAB, TITLE, KC, DMNB, NB,
     *                 IERR, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      PRINTS OUTLINES OF BLOCKS OVER A DISTANCE MATRIX
C
C   DESCRIPTION
C   -----------
C
C   1.  THERE EXISTS AN ORDERING OF THE ROWS OF THE BLOCK SUCH THAT
C       EVERY BLOCK CONSISTS OF A SET OF OBJECTS CONTIGUOUS IN THAT
C       ORDER.  THE ALGORITHM IS GIVEN ON PAGE 156 OF THE FIRST
C       REFERENCE.  THE ROW OBJECTS ARE STORED IN THE VECTOR RLAB IN
C       SUCH AN ORDER.  SIMILARLY, THE COLUMNS CAN BE ORDERED WHICH IS
C       STORED IN THE CLAB ARRAY.
C
C   2.  THIS ORDERING OF THE OBJECTS ALLOWS THE BLOCKS TO BE NAMED BY
C       GIVING THE LOCATION OF THE FIRST AND LAST ROW AND COLUMN IN THE
C       ARRAY FOR EACH BLOCK.  THE FIRST TWO COLUMNS OF THE NB ARRAY
C       STORE THE FIRST AND LAST ROWS IN EACH BLOCK AND THE THIRD AND
C       FOURTH COLUMNS STORE THE FIRST AND LAST COLUMNS IN EACH BLOCK
C
C   3.  THE FINAL BLOCK DIAGRAM PRINTS THE ROW LABELS AND THE COLUMN
C       LABELS AND THE DISTANCE MATRIX WHERE EACH VALUE IS MULTIPLIED
C       BY 10.  THE HORIZONTAL BOUNDARIES OF THE BLOCKS ARE REPRESENTED
C       BY DASHES AND THE VERTICAL BOUNDARIES BY QUOTE MARKS.  COMMAS
C       REPRESENT THE CORNERS OF THE BLOCKS.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX D.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF OBJECTS.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   D     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
C            DIMENSION MUST BE AT LEAST M (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DISTANCES.
C
C         D(I,J) = DISTANCE FROM CASE I TO CASE J
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N
C            (UNCHANGED ON OUTPUT).
C         ORDERED LABELS OF THE COLUMNS.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
C            (UNCHANGED ON OUTPUT).
C         ORDERED LABELS OF THE ROWS.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   KC    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF BLOCKS.
C
C   DMNB  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX NB.  MUST BE AT LEAST 4.
C
C   NB    REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMNB AND SECOND
C            DIMENSION MUST BE AT LEAST KC (UNCHANGED ON OUTPUT).
C         THE MATRIX DEFINING THE BOUNDARIES OF THE BLOCKS.
C
C         NB(1,I) IS 1 + THE FIRST ROW IN BLOCK I
C         NB(2,I) IS 1 + THE LAST ROW IN BLOCK I
C         NB(3,I) IS 1 + THE FIRST COLUMN IN BLOCK I
C         NB(4,I) IS 1 + THE LAST COLUMN IN BLOCK I
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   OUTPUT PARAMETER
C   ----------------
C
C   IERR  INTEGER SCALAR.
C         ERROR FLAG.
C
C         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
C
C         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
C                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
C                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
C                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
C                   RESULTS FOR THAT CLUSTER.
C
C   REFERENCES
C   ----------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 168.
C
C     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
C        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMNB, OUNIT
      DIMENSION D(MM,*), NB(DMNB,*), IA(26)
      CHARACTER*4 CLAB(*), RLAB(*), DD, AE(26)
      CHARACTER*10 TITLE
      CHARACTER*1 DASH,DITTO,COMMA,BLANK,STAR,DOT,AA(26)
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA DD/'----'/
      DATA DASH,DITTO,COMMA,BLANK,STAR,DOT/'-','''',',',' ','*','.'/
C
C     CHECK BOUNDARY ARRAY NB
C
      IF (OUNIT .LE. 0) RETURN
      DO 10 K=1,KC
         IF(NB(1,K).LT.2.OR.NB(1,K).GT.NB(2,K).OR.NB(2,K).GT.M .OR.
     *      NB(3,K).LT.2.OR.NB(3,K).GT.NB(4,K).OR.NB(4,K).GT.N) THEN
            WRITE(ICOUT,1) K
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,6) (NB(I,K)-1,I=1,4)
            CALL DPWRST('XXX','WRIT')
            IERR = 2
         ENDIF
   10 CONTINUE
    1 FORMAT(' BAD BOUNDARY IN BLOCK ',I3)
    6 FORMAT(' BOUNDARIES ARE ', 4I5)
C
      JPP=(N-2)/25+1
      DO 80 JP=1,JPP
         JLP=25*(JP-1)+1
         JUP=25*JP+1
         IF(JUP.GT.N-1) JUP=N-1
         JR=JUP-JLP+1
C
C     WRITE TITLES
C
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,2) TITLE
    2    FORMAT(' BLOCKED ARRAY ',A10)
         CALL DPWRST('XXX','WRIT')
C
C     WRITE OUT ARRAY ONE LINE AT A TIME
C
         WRITE(ICOUT,3)(CLAB(J),J=JLP,JUP)
    3    FORMAT(10X,25(1X,A4))
         CALL DPWRST('XXX','WRIT')
         DO 80 I=1,M
            I1=I-1
            DO 20 L=1,26
               AE(L)=BLANK
   20          AA(L)=BLANK
            IF (I .NE. 1) THEN
C
C     FILL IN DISTANCES
C
               DO 30 J=JLP,JUP
   30             IA(J-JLP+1)=INT(D(I1,J)*10.)
C
C     FILL IN VERTICAL BOUNDARIES
C
               DO 40 K=1,KC
                  IF(NB(2,K).GE.I.AND.NB(1,K).LE.I) THEN
                     JL=NB(3,K)-1
                     JU=NB(4,K)
                     IF(JL.GE.JLP.AND.JL.LE.JUP) AA(JL-JLP+1)=DITTO
                     IF(JU.GE.JLP.AND.JU.LE.JUP) AA(JU-JLP+1)=DITTO
                     IF(JU.EQ.JLP+JR) AA(JR+1)=DITTO
                  ENDIF
   40          CONTINUE
               WRITE(ICOUT,4) RLAB(I1),(AA(J),IA(J),J=1,JR),AA(JR+1)
    4          FORMAT(1X,A4,5X,25(A1,I4),A1)
               CALL DPWRST('XXX','WRIT')
C
C     FILL IN HORIZONTAL BOUNDARIES
C
            ENDIF
            DO 60 K=1,KC
               IF(NB(1,K).EQ.I+1.OR.NB(2,K).EQ.I) THEN
                  JL=NB(3,K)-1
                  JU=NB(4,K)
                  J1=JL-JLP+1
                  J2=JU-JLP+1
                  IF(J1.LE.0) J1=1
                  IF(J2.GT.26) J2=26
                  IF(J1.LE.26.AND.J2.GT.0) THEN
                     DO 50 J=J1,J2
                        IF(J.NE.J2) AE(J)=DD
   50                   IF(AA(J).EQ.BLANK) AA(J)=DASH
                     IF(NB(1,K).EQ.I+1) THEN
                        AA(J1)=COMMA
                        AA(J2)=COMMA
                     ENDIF
                  ENDIF
               ENDIF
   60       CONTINUE
            WRITE(ICOUT,5)(AA(J),AE(J),J=1,JR),AA(JR+1)
    5       FORMAT(10X,25(A1,A4),A1)
            CALL DPWRST('XXX','WRIT')
   70    CONTINUE
   80 CONTINUE
      RETURN
      END
      SUBROUTINE BUILD(MM, M, N, A, CLAB, RLAB, TITLE, K, ITER, XMISS,
     1                 DMSUM1, DMSUM2, SUM, IWORK, WORK, CWORK, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      BUILDS CLUSTERS BY THE K-MEANS ALGORITHM, PRINTING THE RESULTS
C      FOR ALL INTERMEDIATE ITERATIONS
C
C   DESCRIPTION
C   -----------
C
C   1.  THE VARIABLES SHOULD BE SCALED SIMILARLY (CLUSTER SUBROUTINE
C       STAND CAN BE USED TO STANDARDIZE THE VARIABLES).
C
C   2.  THE ROUTINE ITERATES FROM 1 TO THE DESIRED NUMBER OF CLUSTERS.
C       THE FIRST ITERATION STARTS WITH THE CLUSTER OF ALL CASES AND
C       COMPUTES THE SUMMARY STATISTICS FOR EACH VARIABLE AND THE
C       DISTANCES FROM EACH CASE TO THE CLUSTER CENTER WITH ALL THE
C       CALCULATIONS BEING PRINTED.  THE SECOND ITERATION DIVIDES THE
C       CLUSTER INTO TWO CLUSTERS, MOVING CASES FROM ONE TO THE OTHER
C       UNTIL EITHER NO FURTHER MOVEMENTS DECREASE THE DISTANCES
C       BETWEEN EACH CASE AND THE CENTER OF ITS ASSIGNED CLUSTER OR THE
C       MAXIMUM NUMBER OF MOVEMENTS PER ITERATION HAS BEEN REACHED.
C       FOR THE THIRD AND SUBSEQUENT ITERATIONS, THE CLUSTER WITH THE
C       LARGEST VARIANCE IS SPLIT AND ITS CASES ARE ASSIGNED TO THE
C       CLUSTER WHOSE MEAN IS THE SMALLEST DISTANCE FROM THE CASE.  THE
C       MEANS ARE THEN UPDATED AND THE PROCESS OF REASSIGNING CASES TO
C       CLUSTERS CONTINUES UNTIL NO REASSIGNMENTS ARE MADE FOR AN
C       ITERATION.
C
C   3.  THE CLUSTERS AND THEIR STATISTICS WILL BE PRINTED OUT AFTER EACH
C       ITERATION ON FORTRAN UNIT OUNIT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
C            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE VARIABLES.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE CASES.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CLUSTERS DESIRED.
C
C   ITER  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         MAXIMUM NUMBER OF MOVEMENTS ALLOWED PER ITERATION.
C
C   XMISS REAL SCALAR (UNCHANGED ON OUTPUT).
C         MISSING VALUE CODE.  IF A(I,J) = XMISS, THEN THE VALUE FOR THE
C         J-TH VARIABLE FOR THE I-TH CASE IS ASSUMED TO BE MISSING.
C
C   DMSUM1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST 7.
C
C   DMSUM2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE SECOND DIMENSION OF THE MATRIX SUM. MUST BE AT LEAST N.
C
C   SUM   REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMSUM1, WHOSE SECOND
C            DIMENSION MUST BE DMSUM2 AND WHOSE THIRD DIMENSION MUST
C            BE AT LEAST K+1.
C         WORK MATRIX.
C
C   IWORK INTEGER VECTOR DIMENSIONED AT LEAST M.
C         WORK VECTOR.
C
C   WORK  REAL VECTOR DIMENSIONED AT LEAST 2*N+2*M.
C         WORK VECTOR.
C
C   CWORK VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 84-108.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMSUM1, DMSUM2, DCLUS, OUNIT
      DIMENSION SUM(DMSUM1,DMSUM2,*), A(MM,*), WORK(*), IWORK(*)
      CHARACTER*4 CLAB(*), RLAB(*), CWORK(*)
      CHARACTER*10 TITLE
C
C     SUM(1,J,I) IS THE VALUE OF THE J-TH VARIABLE AT THE CENTER OF
C                   CLUSTER I
C     SUM(2,J,I) IS THE NUMBER OF NON-MISSING OBSERVATIONS FOR THE J-TH
C                   VARIABLE IN CLUSTER I
C     SUM(3,J,I) IS THE MEAN OF THE J-TH VARIABLE IN CLUSTER I
C     SUM(4,J,I) IS THE STANDARD DEVIATION OF THE J-TH VARIABLE IN
C                   CLUSTER I
C     SUM(5,J,I) IS THE MINIMUM OF THE J-TH VARIABLE IN CLUSTER I
C     SUM(6,J,I) IS THE MAXIMUM OF THE J-TH VARIABLE IN CLUSTER I
C     SUM(7,J,I) IS THE SUM OF SQUARED DEVIATIONS FOR THE J-TH VARIABLE
C                   FROM THE MEAN OF CLUSTER I
C
C     THE K+1-ST ROW OF SUM STORES THE SAME CALCULATIONS AS ABOVE EXCEPT
C        FOR THE ENTIRE DATA SET RATHER THAN FOR AN INDIVIDUAL CLUSTER
C
      DCLUS = 2*N + M
      DO 10 I=1,7
         DO 10 J=1,N
            DO 10 KK=1,K+1
   10          SUM(I,J,KK)=0.
C
C     LOOP ONCE FOR EACH DESIRED CLUSTER
C
      DO 130 KK=1,K
         DO 60 NC=1,ITER
            ERR=0.
            DO 20 KKK=1,KK
               DO 20 J=1,N
                  IF(NC.EQ.1.OR.SUM(1,J,KKK).NE.SUM(3,J,KKK)) ERR=1.
   20       CONTINUE
C
C     IF NO CHANGES HAVE BEEN MADE, OUTPUT THE CLUSTER
C
            IF(ERR.EQ.0.) GO TO 70
            DO 30 KKK=1,KK
               DO 30 J=1,N
                  SUM(2,J,KKK)=0.
   30       SUM(1,J,KKK)=SUM(3,J,KKK)
            DO 50 I=1,M
               DO 40 J=1,N
   40             WORK(J)=A(I,J)
               IWORK(I)=NC
C
C     FIND BEST CLUSTER FOR CASE I
C
               CALL KMEANS(N, WORK, KK, XMISS, DMSUM1, DMSUM2, SUM,
     *                     IWORK(I), WORK(DCLUS+I))
   50       CONTINUE
   60    CONTINUE
   70    IF (OUNIT .GT. 0) CALL KOUT(MM, M, N, A, CLAB, RLAB, TITLE, KK,
     *                     DMSUM1, DMSUM2, SUM, IWORK, WORK(DCLUS+1),
     *                     WORK(N+1), WORK(M+N+1), CWORK, OUNIT)
 
C
C     CREATE A NEW CLUSTER BY SPLITTING VARIABLE WITH LARGEST WITHIN-
C     CLUSTER VARIANCE AT THAT VALUE OF THAT VARIABLE AT THE CENTER
C     OF THE CLUSTER
C
         SM=0.
         DO 80 J=1,N
            DO 80 KKK=1,KK
               IF(SUM(4,J,KKK).GE.SM) THEN
                  SM=SUM(4,J,KKK)
                  KM=KKK
               ENDIF
   80    CONTINUE
         KN=KK+1
         DO 90 JJ=1,N
            SUM(2,JJ,KM)=0.
            SUM(3,JJ,KM)=0.
            SUM(2,JJ,KN)=0.
   90       SUM(3,JJ,KN)=0.
         DO 110 I=1,M
            IF(IWORK(I).EQ.KM) THEN
               DO 100 JJ=1,N
                  IF(A(I,JJ).NE.XMISS) THEN
                     IF(A(I,JJ).GE.SUM(1,JJ,KM)) THEN
                        SUM(2,JJ,KN)=SUM(2,JJ,KN)+1
                        SUM(3,JJ,KN)=SUM(3,JJ,KN)+A(I,JJ)
                     ELSE
                        SUM(2,JJ,KM)=SUM(2,JJ,KM)+1
                        SUM(3,JJ,KM)=SUM(3,JJ,KM)+A(I,JJ)
                     ENDIF
                  ENDIF
  100          CONTINUE
            ENDIF
  110    CONTINUE
         DO 120 JJ=1,N
            IF(SUM(2,JJ,KN).NE.0.)SUM(3,JJ,KN)=SUM(3,JJ,KN)/SUM(2,JJ,KN)
            IF(SUM(2,JJ,KM).NE.0.)SUM(3,JJ,KM)=SUM(3,JJ,KM)/SUM(2,JJ,KM)
  120    CONTINUE
  130 CONTINUE
      RETURN
      END
      SUBROUTINE CLUMOM(MM, M, N, A, ICLUS, W, U, DMC1, DMC2, C)
C
C  NOTE: RENAMED FOR DATAPLOT TO AVOID NAME CONFLICT.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      COMPUTES WEIGHTED MEANS AND COVARIANCES
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
C            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C   ICLUS INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE CLUSTER NUMBER.
C
C   W     REAL VECTOR DIMENSIONED AT LEAST M (UNCHANGED ON OUTPUT).
C         VECTOR OF WEIGHTS FOR THE OBJECTS.
C
C   DMC1  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX C.  MUST BE AT LEAST N.
C
C   DMC2  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE SECOND DIMENSION OF THE MATRIX C.  MUST BE AT LEAST N.
C
C   OUTPUT PARAMETERS
C   -----------------
C
C   U     REAL VECTOR DIMENSIONED AT LEAST N.
C         VECTOR OF WEIGHTED CLUSTER MEANS FOR EACH VARIABLE.
C
C   C     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMC1, WHOSE SECOND
C            DIMENSION MUST BE DMC2, AND WHOSE THIRD DIMENSION MUST BE
C            AT LEAST K.
C         C(I,J,K) IS THE IJ-TH ELEMENT OF THE COVARIANCE MATRIX FOR THE
C            K-TH CLUSTER.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 73.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMC1, DMC2
      DIMENSION C(DMC1,DMC2,*), W(*), U(*), A(MM,*)
C
      INCLUDE 'DPCOMC.INC'
C
      SP=0.
      DO 10 I=1,M
   10    SP=SP+W(I)
      IF(SP.EQ.0.) SP=R1MACH(4)
C
C     COMPUTED WEIGHTED MEANS
C
      DO 30 J=1,N
         SS=0.
         DO 20 I=1,M
   20       SS=SS+A(I,J)*W(I)
   30    U(J)=SS/SP
C
C     COMPUTED WEIGHTED COVARIANCES
C
      DO 50 J=1,N
         DO 50 K=1,J
            SS=0.
            DO 40 I=1,M
   40          SS=SS+(A(I,J)-U(J))*(A(I,K)-U(K))*W(I)
            C(J,K,ICLUS)=SS/SP
   50       C(K,J,ICLUS)=C(J,K,ICLUS)
      RETURN
      END
      SUBROUTINE COVOUT(MM, M, N, A, CLAB1, CLAB2, RLAB, TITLE, K,
     *                  DMWORK, WORK1, DMC1, DMC2, C, WORK,
     *                  ICAPTY,ICAPSW,IFORSW)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      PRINTS RESULTS OF MIX
C
C   DESCRIPTION
C   -----------
C
C   1.  SEE DESCRIPTION OF MIX FOR DESCRIPTION OF OUTPUT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   SEE SUBROUTINE MIX FOR PARAMETERS
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 127.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMWORK, P, U, PMIX, DMC1, DMC2, OUNIT
      DIMENSION A(MM,*), WORK1(DMWORK,*), C(DMC1,DMC2,*), WORK(*)
      CHARACTER*4 CLAB1(*), CLAB2(*)
      CHARACTER*8 RLAB(*)
      CHARACTER*10 TITLE
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOMC.INC'
C
      INCLUDE 'DPCOST.INC'
C
      LOGICAL IFLAGA
      LOGICAL IFLAGB
      LOGICAL IFLAG1
      LOGICAL IFLAG2
      LOGICAL IFLAG3
      INTEGER IFLAG4
      INTEGER IFLAG5
C
      PARAMETER(NUMCLI=9)
      PARAMETER(MAXLIN=3)
      PARAMETER(MAXROW=40)
      CHARACTER*65 ITITLE
      CHARACTER*60 ITITL9
      CHARACTER*4  ALIGN(NUMCLI)
      CHARACTER*4  VALIGN(NUMCLI)
      INTEGER      IDIGI2(MAXROW,NUMCLI)
      INTEGER      NTOT(MAXROW)
      INTEGER      ROWSEP(MAXROW)
      CHARACTER*20 ITITL2(MAXLIN,NUMCLI)
      CHARACTER*20 IVALUE(MAXROW,NUMCLI)
      CHARACTER*4  ITYPCO(NUMCLI)
      INTEGER      NCTIT2(MAXLIN,NUMCLI)
      INTEGER      NCVALU(MAXROW,NUMCLI)
      INTEGER      NCOLSP(MAXLIN,NUMCLI)
      INTEGER      IWHTML(NUMCLI)
      INTEGER      IWRTF(NUMCLI)
      REAL         AMAT(MAXROW,NUMCLI)
      LOGICAL IFRST
      LOGICAL ILAST
      LOGICAL IFLAGS
      LOGICAL IFLAGE
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      ISUBRO='XXXX'
      IBUGA3='OFF'
      IERROR='OFF'
C
      P = 0
      U = P + M
      PMIX = U + N + 1
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
      IF(IFORSW.EQ.'E')NUMDIG=-7
      IF(IFORSW.EQ.'-2')NUMDIG=-2
      IF(IFORSW.EQ.'-3')NUMDIG=-3
      IF(IFORSW.EQ.'-4')NUMDIG=-4
      IF(IFORSW.EQ.'-5')NUMDIG=-5
      IF(IFORSW.EQ.'-6')NUMDIG=-6
      IF(IFORSW.EQ.'-7')NUMDIG=-7
      IF(IFORSW.EQ.'-8')NUMDIG=-8
      IF(IFORSW.EQ.'-9')NUMDIG=-9
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,1) TITLE,K
    1   FORMAT(' MIXTURE MODEL FOR ',A10,' WITH',I5,' CLUSTERS')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,2)(KK,KK=1,K)
    2   FORMAT(' CLUSTER',3X,9(6X,I4,3X))
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C     PRINT CLUSTER PROBABILITIES
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,3)
    3   FORMAT(' MIXTURE PROBABILITIES')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,333)(WORK1(PMIX,KK),KK=1,K)
  333   FORMAT((12X,10F12.6))
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
C     PRINT MEANS
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
C
CCCCC   WRITE(ICOUT,4)
CCCC4   FORMAT(' CLUSTER MEANS')
CCCCC   CALL DPWRST('XXX','WRIT')
C
CCCCC   DO 10 J=1,N
CCCCC      WRITE(ICOUT,5)CLAB1(J),CLAB2(J),(WORK1(U+J,KK),KK=1,K)
CCCC5      FORMAT(1X,2A4,5X,10F12.4)
CCCCC      CALL DPWRST('XXX','WRIT')
CCC10   CONTINUE
C
        ITITLE=' '
        NCTITL=0
        ITITL9='Cluster Means'
        NCTIT9=13
C
        IF(K.LE.6)THEN
          NLOOP=1
        ELSE
          NLOOP=K/6
          NTEMP=MOD(K,6)
          IF(NTEMP.GT.0)NLOOP=NLOOP+1
        ENDIF
C
        IWHTML(1)=100
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IWHTML(7)=150
        IWHTML(8)=150
        IINC2=1200
        IINC1=1500
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC1
        IWRTF(3)=IWRTF(2)+IINC1
        IWRTF(4)=IWRTF(3)+IINC1
        IWRTF(5)=IWRTF(4)+IINC1
        IWRTF(6)=IWRTF(5)+IINC1
        IWRTF(7)=IWRTF(6)+IINC1
        IWRTF(8)=IWRTF(7)+IINC1
C
        DO1010II=1,NLOOP
          IF(II.EQ.NLOOP)THEN
            NUMCOL=MOD(K,6)
            IF(NUMCOL.EQ.0)NUMCOL=6
          ELSE
            NUMCOL=6
          ENDIF
          NUMCOL=NUMCOL+1
          NUMLIN=1
C
          DO1020J=1,NUMCLI
            DO1030I=1,MAXLIN
              ITITL2(I,J)=' '
              NCTIT2(I,J)=0
              NCOLSP(I,J)=1
 1030       CONTINUE
 1020     CONTINUE
C
          ISTRT=(II-1)*7+1
          IEND=ISTRT+6
          IF(IEND.GT.N)IEND=N
C
          ITITL2(1,1)='Variable'
          NCTIT2(1,1)=8
C
          DO1040L=ISTRT,IEND
            ITITL2(1,L+1)='Cluster '
            WRITE(ITITL2(1,L+1)(9:11),'(I3)')L
            NCTIT2(1,L+1)=11
 1040     CONTINUE
C
          NMAX=0
          ICNT=0
          ICNT2=0
          DO1050I=1,NUMCOL
            VALIGN(I)='b'
            ALIGN(I)='r'
            NTOT(I)=15
            ITYPCO(I)='NUME'
            IF(I.EQ.1)THEN
              NTOT(I)=12
              ALIGN(I)='l'
              ITYPCO(I)='ALPH'
            ENDIF
            NMAX=NMAX+NTOT(I)
 1050     CONTINUE
C
          ICNT=0
          DO1060J=1,N
            ICNT=ICNT+1
            IDIGI2(ICNT,1)=0
            IVALUE(ICNT,1)(1:4)=CLAB1(J)(1:4)
            IVALUE(ICNT,1)(5:8)=CLAB2(J)(1:4)
            NCVALU(ICNT,1)=8
            AMAT(ICNT,1)=0.0
C
            ICNT2=1
            DO1065KK=ISTRT,IEND
C
              ICNT2=ICNT2+1
              IDIGI2(ICNT,ICNT2)=NUMDIG
              IVALUE(ICNT,ICNT2)=' '
              NCVALU(ICNT,ICNT2)=0
              AMAT(ICNT,ICNT2)=WORK1(U+J,KK)
              ROWSEP(ICNT)=0
C
 1065       CONTINUE
 1060    CONTINUE
C
          ROWSEP(ICNT)=1
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDT5B(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                NCOLSP,ROWSEP,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
 1010   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,6)
    6   FORMAT(' DETERMINANTS')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,666)(C(1,N+1,J),J=1,K)
  666   FORMAT((12X,10E12.4))
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
CCCCC   WRITE(ICOUT,7)
CCCC7   FORMAT(' WITHIN CLUSTER VARIANCES AND CORRELATIONS')
CCCCC   CALL DPWRST('XXX','WRIT')
C
CCCCC   DO 30 I=1,N
CCCCC      DO 30 J=I,N
CCCCC         DO 20 KK=1,K
CCCCC            Z=C(I,I,KK)*C(J,J,KK)
CCCCC            WORK(KK)=C(I,J,KK)
CCCCC            IF(I.EQ.J) Z=0.
CCC20            IF(Z.NE.0.) WORK(KK)=C(I,J,KK)*Z**(-0.5)
CCCCC            IF(I.EQ.J) THEN
CCCCC              WRITE(ICOUT,999)
CCCCC              CALL DPWRST('XXX','WRIT')
CCCCC            ENDIF
CCCCC            WRITE(ICOUT,9) CLAB1(I),CLAB2(I),CLAB1(J),CLAB2(J),
CCCCC1                          (WORK(KK),KK=1,K)
CCCC9            FORMAT(1X,2A4,2X,2A4,10F12.4)
CCCCC            CALL DPWRST('XXX','WRIT')
CCC30   CONTINUE
C
        ITITLE=' '
        NCTITL=0
        ITITL9='Within Cluster Variances and Correlations'
        NCTIT9=41
C
        IWHTML(1)=100
        IWHTML(2)=100
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IWHTML(7)=150
        IWHTML(8)=150
        IWHTML(9)=150
        IINC2=1200
        IINC1=1500
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC2
        IWRTF(3)=IWRTF(2)+IINC1
        IWRTF(4)=IWRTF(3)+IINC1
        IWRTF(5)=IWRTF(4)+IINC1
        IWRTF(6)=IWRTF(5)+IINC1
        IWRTF(7)=IWRTF(6)+IINC1
        IWRTF(8)=IWRTF(7)+IINC1
        IWRTF(9)=IWRTF(8)+IINC1
C
        DO1110II=1,NLOOP
          IF(II.EQ.NLOOP)THEN
            NUMCOL=MOD(K,6)
            IF(NUMCOL.EQ.0)NUMCOL=6
          ELSE
            NUMCOL=6
          ENDIF
          NUMCOL=NUMCOL+2
          NUMLIN=1
C
          DO1120J=1,NUMCLI
            DO1130I=1,MAXLIN
              ITITL2(I,J)=' '
              NCTIT2(I,J)=0
              NCOLSP(I,J)=1
 1130       CONTINUE
 1120     CONTINUE
C
          ISTRT=(II-1)*7+1
          IEND=ISTRT+6
          IF(IEND.GT.N)IEND=N
C
          ITITL2(1,1)='I'
          NCTIT2(1,1)=1
          ITITL2(1,2)='J'
          NCTIT2(1,2)=1
C
          DO1140L=ISTRT,IEND
            ITITL2(1,L+2)='Cluster '
            WRITE(ITITL2(1,L+2)(9:11),'(I3)')L
            NCTIT2(1,L+2)=11
 1140     CONTINUE
C
          NMAX=0
          ICNT=0
          ICNT2=0
          DO1150I=1,NUMCOL
            VALIGN(I)='b'
            ALIGN(I)='r'
            NTOT(I)=15
            ITYPCO(I)='NUME'
            IF(I.LE.2)THEN
              NTOT(I)=12
              ALIGN(I)='l'
              ITYPCO(I)='ALPH'
            ENDIF
            NMAX=NMAX+NTOT(I)
 1150     CONTINUE
C
          ICNT=0
          DO1160I=1,N
            DO1165J=I,N
              ICNT=ICNT+1
              IDIGI2(ICNT,1)=0
              IDIGI2(ICNT,2)=0
              IVALUE(ICNT,1)(1:4)=CLAB1(I)(1:4)
              IVALUE(ICNT,1)(5:8)=CLAB2(I)(1:4)
              NCVALU(ICNT,1)=8
              AMAT(ICNT,1)=0.0
              IVALUE(ICNT,2)(1:4)=CLAB1(J)(1:4)
              IVALUE(ICNT,2)(5:8)=CLAB2(J)(1:4)
              NCVALU(ICNT,2)=8
              AMAT(ICNT,2)=0.0
C
              ICNT2=2
              DO1168KK=ISTRT,IEND
C
                Z=C(I,I,KK)*C(J,J,KK)
                WORK(KK)=C(I,J,KK)
                IF(I.EQ.J)Z=0.
                IF(Z.NE.0.)WORK(KK)=C(I,J,KK)*Z**(-0.5)
                ICNT2=ICNT2+1
                IDIGI2(ICNT,ICNT2)=NUMDIG
                IVALUE(ICNT,ICNT2)=' '
                NCVALU(ICNT,ICNT2)=0
                AMAT(ICNT,ICNT2)=WORK(KK)
                ROWSEP(ICNT)=0
 1168         CONTINUE
C
 1165       CONTINUE
 1160     CONTINUE
C
          ROWSEP(ICNT)=1
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDT5B(ITITLE,NCTITL,
     1                ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                NCOLSP,ROWSEP,
     1                ICAPSW,ICAPTY,IFRST,ILAST,
     1                IFLAGS,IFLAGE,
     1                ISUBRO,IBUGA3,IERROR)
C
 1110   CONTINUE
      ENDIF
C
C     PRINT PROBABILITIES
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
CCCCC   WRITE(ICOUT,11)
CCC11   FORMAT(' BELONGING PROBABILITIES')
CCCCC   CALL DPWRST('XXX','WRIT')
CCCCC   DO 40 I=1,M
CCCCC      WRITE(ICOUT,12) RLAB(I),(WORK1(P+I,KK),KK=1,K)
CCC12      FORMAT(1X,A8,2X,10F12.6)
CCCCC      CALL DPWRST('XXX','WRIT')
CCC40   CONTINUE
C
        ITITLE=' '
        NCTITL=0
        ITITL9='Belonging Probabilities'
        NCTIT9=23
C
C       DO 2 LOOPS:
C
C          1) FIRST LOOP IS FOR THE NUMBER OF ROWS (OBSERVATIONS)
C          2) SECOND LOOP IS FOR THE NUMBER OF COLUMNS (CLUSTERS)
C
        IF(M.LE.MAXROW)THEN
          NLOOP2=1
        ELSE
          NLOOP2=M/MAXROW
          IF(MOD(M,MAXROW).GT.0)NLOOP2=NLOOP2+1
        ENDIF
C
        IF(K.LE.6)THEN
          NLOOP=1
        ELSE
          NLOOP=K/6
          IF(MOD(K,6).GT.0)NLOOP=NLOOP+1
        ENDIF
C
        IWHTML(1)=120
        IWHTML(2)=150
        IWHTML(3)=150
        IWHTML(4)=150
        IWHTML(5)=150
        IWHTML(6)=150
        IWHTML(7)=150
        IWHTML(8)=150
        IINC2=1200
        IINC1=1500
        IWRTF(1)=IINC2
        IWRTF(2)=IWRTF(1)+IINC1
        IWRTF(3)=IWRTF(2)+IINC1
        IWRTF(4)=IWRTF(3)+IINC1
        IWRTF(5)=IWRTF(4)+IINC1
        IWRTF(6)=IWRTF(5)+IINC1
        IWRTF(7)=IWRTF(6)+IINC1
        IWRTF(8)=IWRTF(7)+IINC1
C
        DO1201JJ=1,NLOOP2
          IROW1=(JJ-1)*MAXROW+1
          IROW2=JJ*MAXROW
          IF(IROW2.GT.M)IROW2=M
          DO1210II=1,NLOOP
            IF(II.EQ.NLOOP)THEN
              NUMCOL=MOD(K,6)
              IF(NUMCOL.EQ.0)NUMCOL=6
            ELSE
              NUMCOL=6
            ENDIF
            NUMCOL=NUMCOL+1
            NUMLIN=1
C
            DO1220J=1,NUMCLI
              DO1230I=1,MAXLIN
                ITITL2(I,J)=' '
                NCTIT2(I,J)=0
                NCOLSP(I,J)=1
 1230         CONTINUE
 1220       CONTINUE
C
            ISTRT=(II-1)*7+1
            IEND=ISTRT+6
            IF(IEND.GT.N)IEND=N
C
            ITITL2(1,1)='Observation'
            NCTIT2(1,1)=11
C
            DO1240L=ISTRT,IEND
              ITITL2(1,L+1)='Cluster '
              WRITE(ITITL2(1,L+1)(9:11),'(I3)')L
              NCTIT2(1,L+1)=11
 1240       CONTINUE
C
            NMAX=0
            ICNT=0
            ICNT2=0
            DO1250I=1,NUMCOL
              VALIGN(I)='b'
              ALIGN(I)='r'
              NTOT(I)=15
              ITYPCO(I)='NUME'
              IF(I.EQ.1)THEN
                NTOT(I)=12
                ALIGN(I)='l'
                ITYPCO(I)='ALPH'
              ENDIF
              NMAX=NMAX+NTOT(I)
 1250       CONTINUE
C
            ICNT=0
            DO1260J=IROW1,IROW2
              ICNT=ICNT+1
              IDIGI2(ICNT,1)=0
              IVALUE(ICNT,1)(1:8)=RLAB(J)(1:8)
              NCVALU(ICNT,1)=8
              AMAT(ICNT,1)=0.0
C
              ICNT2=1
              DO1265KK=ISTRT,IEND
C
                ICNT2=ICNT2+1
                IDIGI2(ICNT,ICNT2)=NUMDIG
                IVALUE(ICNT,ICNT2)=' '
                NCVALU(ICNT,ICNT2)=0
                AMAT(ICNT,ICNT2)=WORK1(P+J,KK)
                ROWSEP(ICNT)=0
C
 1265       CONTINUE
 1260    CONTINUE
C
            ROWSEP(ICNT)=1
            IFRST=.TRUE.
            ILAST=.TRUE.
            IFLAGS=.TRUE.
            IFLAGE=.TRUE.
            CALL DPDT5B(ITITLE,NCTITL,
     1                  ITITL9,NCTIT9,ITITL2,NCTIT2,
     1                  MAXLIN,NUMLIN,NUMCLI,NUMCOL,
     1                  IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,
     1                  IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,
     1                  NCOLSP,ROWSEP,
     1                  ICAPSW,ICAPTY,IFRST,ILAST,
     1                  IFLAGS,IFLAGE,
     1                  ISUBRO,IBUGA3,IERROR)
 1210     CONTINUE
 1201   CONTINUE
C
      ENDIF
      RETURN
      END
      SUBROUTINE CSPLIT(MM, M, N, A, CLAB, IR, KA, TH, IORD, DMIWRK,
     *                  IWORK, DMWORK, WORK)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      FINDS OPTIMAL SPLIT OF VARIABLES
C
C   DESCRIPTION
C   -----------
C
C   1.  INITIALLY, THE FIRST CLUSTER CONSISTS OF ALL VARIABLES WITHIN
C       THE BLOCK IR AND THE SECOND CLUSTER IS EMPTY.  THE REDUCTION IN
C       THE WITHIN-CLUSTER SUM OF SQUARES FOR MOVING EACH VARIABLE
C       FROM THE FIRST CLUSTER TO THE SECOND IS CALCULATED.  THE
C       VARIABLE THAT REDUCES THE SUM OF SQUARES THE MOST IS MOVED AND
C       THIS CONTINUES UNTIL ALL VARIABLES ARE MOVED WITH EACH
C       REDUCTION STORED.  THEN THE SPLIT THAT HAD THE SMALLEST
C       REDUCTION OF ALL IS RETURNED AS THE OPTIMUM SPLIT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM, M, N, A, CLAB, TH, IORD, DMIWRK, DMWORK -- SEE SUBROUTINE SPLIT2
C
C   IR    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         NUMBER OF BLOCK TO BE SPLIT.
C
C   KA    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         NUMBER OF BLOCKS.
C
C   IWORK INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND SECOND
C            DIMENSION MUST BE AT LEAST KA.
C         THE MATRIX DEFINING THE BOUNDARIES OF THE BLOCKS.
C
C         IWORK(1,I) IS 1 + THE FIRST ROW IN BLOCK I
C         IWORK(2,I) IS 1 + THE LAST ROW IN BLOCK I
C         IWORK(3,I) IS 1 + THE FIRST COLUMN IN BLOCK I
C         IWORK(4,I) IS 1 + THE LAST COLUMN IN BLOCK I
C
C   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
C            DIMENSION MUST BE AT LEAST MAX(M,N).
C
C         WORK(1,I) = FIRST CASE IN CASE CLUSTER I
C         WORK(2,I) = LAST CASE IN CASE CLUSTER I
C         WORK(3,I) = REDUCTION IN SSQ DUE TO SPLITTING
C         WORK(4,I) = LAST CASE IN FIRST CLUSTER OF SPLIT OF I
C         WORK(5,I) = 1 IF CASE IS INCLUDED IN PRESENT VARIABLE SPLIT
C         WORK(6,I) = NUMBER OF VARIABLES IN I-TH ROW OF PRESENT
C                     VARIABLE SPLIT
C         WORK(7,I) = MEAN OF I-TH CASE, FIRST VARIABLE CLUSTER
C         WORK(8,I) = NUMBER OF VARIABLES SECOND CLUSTER
C         WORK(9,I) = MEAN OF I-TH CASE, SECOND CLUSTER
C
C         WORK(10-18,I) ARE SIMILAR WITH VARIABLES AND CASES REVERSED.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 276.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMWORK, DMIWRK
      DIMENSION A(MM,*), IWORK(DMIWRK,*), WORK(DMWORK,*)
      CHARACTER*4 CLAB(*), C
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C
      XM=99999.
      DO 10 I=1,M
   10    WORK(5,I)=0.
C
C     LOOK FOR BLOCKS WITHIN THRESHOLD
C
      JL=INT(WORK(10,IR))
      JU=INT(WORK(11,IR))
      DO 40 K=1,KA
         IF(IWORK(3,K).EQ.JL+1.AND.IWORK(4,K).EQ.JU+1) THEN
            IL=IWORK(1,K)
            IF(IL.LT.0) IL=-IL
            IU=IWORK(2,K)
C
C     COMPUTE VARIANCES
C
            NC=0
            DO 30 I=IL-1,IU-1
               S1=0.
               S2=0.
               S3=0.
               DO 20 J=JL,JU
                  IF(A(I,J).NE.XM) THEN
                     S1=S1+1
                     S2=S2+A(I,J)
                     S3=S3+A(I,J)**2
                  ENDIF
   20          CONTINUE
               WORK(6,I)=S1
               IF(S1.NE.0.) THEN
                  WORK(7,I)=S2/S1
                  S3=S3/S1-(S2/S1)**2
               ENDIF
               IF(S3.GT.TH) THEN
                  WORK(5,I)=1.
                  NC=1
               ENDIF
   30       CONTINUE
            IF(NC.EQ.0) IWORK(3,K)=-IWORK(3,K)
         ENDIF
   40 CONTINUE
C
C     FIND BEST VARIABLE SPLIT
C
      DO 50 I=1,M
         WORK(8,I)=0.
   50    WORK(9,I)=0.
      DM=0.
      WORK(12,IR)=0.
      WORK(13,IR)=JL
      DO 100 J=JL,JU-1
         JJ=JU-J+JL
         JD=JJ
         DD=-R1MACH(2)
         DO 70 L=JL,JJ
            IF(IORD.LT.2.OR.L.EQ.JJ) THEN
               DL=0.
               DO 60 I=1,M
                  IF(WORK(5,I).NE.0.AND.A(I,L).NE.XM) THEN
                    DL=DL+(A(I,L)-WORK(7,I))**2*(WORK(6,I)+1.)/WORK(6,I)
                    DL=DL-(A(I,L)-WORK(9,I))**2*WORK(8,I)/(WORK(8,I)+1.)
                  ENDIF
   60          CONTINUE
               IF(DL.GT.DD) THEN
                  DD=DL
                  JD=L
               ENDIF
            ENDIF
   70    CONTINUE
C
C     INTERCHANGE JD AND JJ
C
         DO 80 I=1,M
            CC=A(I,JJ)
            A(I,JJ)=A(I,JD)
   80       A(I,JD)=CC
         C = CLAB(JJ)
         CLAB(JJ) = CLAB(JD)
         CLAB(JD) = C
C
C     UPDATE MEANS
C
         DO 90 I=1,M
            IF(WORK(5,I).NE.0..AND.A(I,JJ).NE.XM) THEN
               WORK(6,I)=WORK(6,I)-1.
               IF(WORK(6,I).NE.0.)WORK(7,I)=WORK(7,I)+(WORK(7,I)-
     *                           A(I,JJ))/WORK(6,I)
               WORK(8,I)=WORK(8,I)+1.
               WORK(9,I)=WORK(9,I)-(WORK(9,I)-A(I,JJ))/WORK(8,I)
            ENDIF
   90    CONTINUE
         DM=DM+DD
         IF(DM.GE.WORK(12,IR)) THEN
            WORK(12,IR)=DM
            WORK(13,IR)=JJ-1
         ENDIF
  100 CONTINUE
      RETURN
      END
      SUBROUTINE INVERT(MM, M, A, DET, WORK, IWORK, IERR, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      COMPUTES THE INVERSE AND DETERMINANT OF THE SYMMETRIC MATRIX
C      (E.G., A COVARIANCE MATRIX)
C
C   DESCRIPTION
C   -----------
C
C   1.  THE LINPACK SUBROUTINE SSIFA IS CALLED TO FACTOR THE MATRIX AND
C       THEN THE LINPACK SUBROUTINE SSIDI IS CALLED TO USE THE
C       FACTORIZATION TO FIND THE INVERSE AND DETERMINANT.  THE INPUT
C       MATRIX MUST BE SYMMETRIC AND IS OVERWRITTEN WITH ITS INVERSE ON
C       OUTPUT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF ROWS AND COLUMNS IN THE MATRIX A.
C
C   A     REAL SYMMETRIC MATRIX WHOSE FIRST DIMENSION MUST BE MM AND
C            WHOSE SECOND DIMENSION MUST BE AT LEAST M (CHANGED ON
C            OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   WORK  REAL VECTOR DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   IWORK INTEGER VECTOR DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR ERROR MESSAGES.
C
C   OUTPUT PARAMETERS
C   -----------------
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
C            DIMENSION MUST BE AT LEAST N.
C         THE INVERSE OF THE INPUT MATRIX.
C
C   DET   REAL VECTOR DIMENSIONED AT LEAST 2.
C         THE DETERMINANT OF THE MATRIX.
C
C         THE DETERMINANT IS  DET(1) ** DET(2).
C
C   IERR  INTEGER SCALAR.
C         ERROR FLAG.
C
C         IF IERR = 0, NO ERROR CONDITION WAS DETECTED.
C
C         IF IERR = K, THE K-TH PIVOT BLOCK IS SINGULAR.  THE INVERSE IS
C                      NOT COMPUTED.  ERROR CONDITION SET IN CMLIB
C                      ROUTINE SSIFA.
C
C   REFERENCES
C   ----------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 69.
C
C     NBS CORE MATH LIBRARY, VOLS. 1-4 (GAITHERSBURG: QA297.C69 IN NBS
C     LIBRARY, ADMIN E-120).
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER OUNIT
      DIMENSION A(MM,*), IWORK(*), WORK(*), DET(*), INERT(3)
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     NOTE: FOR DATAPLOT, REPLACE OLDER LINPAC ROUTINES WITH
C           VERSIONS THAT ARE USED IN DATAPLOT.
C
CCCCC CALL SSIFA(A,MM,M,IWORK,IERR)
CCCCC IF (IERR .NE. 0) THEN
CCCCC    IF (OUNIT .GT. 0) THEN
CCCCC       WRITE(ICOUT,1)
CCC1        FORMAT('MATRIX TO BE INVERTED MAY BE SINGULAR')
CCCCC       CALL DPWRST('XXX','WRIT')
CCCCC       GOTO9000
CCCCC ENDIF
CCCCC JOB = 111
CCCCC CALL SSIDI(A,MM,M,IWORK,DET,INERT,WORK,JOB)
      CALL SGECO(A,MM,M,IWORK,RCOND,WORK)
C
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2571)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,2572)
        CALL DPWRST('XXX','ERRO ')
        GOTO9000
      ELSE
        IJOB=1
        CALL SGEDI(A,MM,M,IWORK,DET,WORK,IJOB)
      END IF
  999 FORMAT(1X)
 2571 FORMAT('****** ERROR IN INVERT ********')
 2572 FORMAT('       THE INPUT MATRIX IS SINGULAR')
CCCCC END CHANGE
C
      DO 10 I = 1 , M
         DO 10 J = I , M
 10         A(J,I) = A(I,J)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE KMEANS(N, X, K, XMISS, DMSUM1, DMSUM2, SUM, JMIN, DMIN)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      ASSIGNS A VARIABLE TO ITS CLOSEST CLUSTER AND UPDATES THE SUMMARY
C      STATISTICS
C
C   DESCRIPTION
C   -----------
C
C   1.  THE DISTANCE BETWEEN THE CASE X AND THE CENTER OF EACH CLUSTER
C       IS COMPUTED AND X IS ASSIGNED TO THE CLUSTER WITH THE SMALLEST
C       DISTANCE.  THE SUMMARY STATISTICS FOR THE ASSIGNED CLUSTER ARE
C       THEN UPDATED.
C
C   INPUT PARAMETERS
C   ----------------
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   X     REAL VECTOR DIMENSIONED AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CLUSTERS.
C
C   XMISS REAL SCALAR (UNCHANGED ON OUTPUT).
C         VALUE THAT A DATA VALUE IS SET TO IF CONSIDERED MISSING.
C
C   DMSUM1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST 7.
C
C   DMSUM2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE SECOND DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST N.
C
C   OUTPUT PARAMETERS
C   ------------------
C
C   SUM   REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMSUM1, WHOSE SECOND
C            DIMENSION MUST BE DMSUM2, AND WHOSE THIRD DIMENSION MUST
C            BE AT LEAST K+1.
C         THE PARAMETERS FOR EACH CLUSTER.
C
C   JMIN  INTEGER SCALAR.
C         CLUSTER WHOSE CENTER X IS CLOSEST TO.
C
C   DMIN  REAL SCALAR.
C         DISTANCE BETWEEN X AND CENTER OF JMIN CLUSTER.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 84-105.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMSUM1, DMSUM2
      DIMENSION SUM(DMSUM1,DMSUM2,*), X(*)
C
      INCLUDE 'DPCOMC.INC'
C
      JMIN=1
      DMIN=R1MACH(2)
C
C     CALCULATE DISTANCE TO EACH CLUSTER CENTER
C
      DO 20 J=1,K
         XP=R1MACH(4)
         DD=0.
         DO 10 I=1,N
            IF (X(I).NE.XMISS) THEN
               DD=DD+(X(I)-SUM(1,I,J))**2
               XP=XP+1.
            ENDIF
   10    CONTINUE
         DD=(DD/XP)**0.5
         IF(DD.LE.DMIN) THEN
            DMIN=DD
            JMIN=J
         ENDIF
   20 CONTINUE
C
C     UPDATE SUMMARY STATISTICS FOR CHOSEN CLUSTER
C
      DO 30 I=1,N
         IF(X(I).NE.XMISS) CALL SINGLE(X(I),SUM(2,I,JMIN),SUM(3,I,JMIN),
     *      SUM(4,I,JMIN),SUM(5,I,JMIN),SUM(6,I,JMIN),SUM(7,I,JMIN))
   30 CONTINUE
      RETURN
      END
      SUBROUTINE KOUT(MM, M, N, A, CLAB, RLAB, TITLE, KK, DMSUM1,
     *                DMSUM2, SUM, NCLUS, DCLUS, DD, R, CWORK, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      PRINTS OUTPUT FOR K-MEANS ALGORITHM
C
C   DESCRIPTION
C   -----------
C
C   1.  THE OUTPUT CONSISTS OF THE OVERALL STATISTICS FOR THE CURRENT
C       PARTITION, FOLLOWED BY THE STATISTICS FOR EACH CLUSTER.  THE
C       ANALYSIS OF VARIANCE IS COMPUTED FOR EACH VARIABLE IN THE
C       PARTITION.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
C            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE VARIABLES.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE CASES.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   KK    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CLUSTERS.
C
C   DMSUM1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST 7.
C
C   DMSUM2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE SECOND DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST N.
C
C   SUM   REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMSUM1, WHOSE SECOND
C            DIMENSION MUST BE DMSUM2, AND WHOSE THIRD DIMENSION MUST
C            BE AT LEAST K+1 (UNCHANGED ON OUTPUT).
C         THE PARAMETERS FOR EACH CLUSTER.
C
C         SUM(1,J,I) IS THE VALUE OF THE J-TH VARIABLE AT THE CENTER OF
C                       CLUSTER I
C         SUM(2,J,I) IS THE NUMBER OF NON-MISSING OBSERVATIONS FOR THE
C                       J-TH VARIABLE IN CLUSTER I
C         SUM(3,J,I) IS THE MEAN OF THE J-TH VARIABLE IN CLUSTER I
C         SUM(4,J,I) IS THE STANDARD DEVIATION OF THE J-TH VARIABLE IN
C                       CLUSTER I
C         SUM(5,J,I) IS THE MINIMUM OF THE J-TH VARIABLE IN CLUSTER I
C         SUM(6,J,I) IS THE MAXIMUM OF THE J-TH VARIABLE IN CLUSTER I
C         SUM(7,J,I) IS THE SUM OF SQUARED DEVIATIONS FOR THE J-TH
C                       VARIABLE FROM THE MEAN OF CLUSTER I
C
C         THE K+1-ST ROW OF SUM STORES THE SAME CALCULATIONS AS ABOVE
C            EXCEPT FOR THE ENTIRE DATA SET RATHER THAN FOR AN
C            INDIVIDUAL CLUSTER
C
C   NCLUS INTEGER VECTOR DIMENSIONED AT LEAST M (UNCHANGED ON OUTPUT).
C         NCLUS(I) IS THE CLUSTER FOR CASE I.
C
C   DCLUS REAL VECTOR DIMENSIONED AT LEAST M (UNCHANGED ON OUTPUT).
C         DCLUS(I) IS THE DISTANCE OF EACH CASE TO THE CLOSEST CLUSTER.
C
C   DD    REAL VECTOR DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   R     REAL VECTOR DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   CWORK VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 110.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMSUM1, DMSUM2, OUNIT
      DIMENSION SUM(DMSUM1,DMSUM2,*), NCLUS(*), DCLUS(*), A(MM,*), R(*),
     *          DD(*)
      CHARACTER*4 CLAB(*), RLAB(*), CWORK(*)
      CHARACTER*10 TITLE
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     1/2008: MODIFIED FOR DATAPLOT TO USE DATAPLOT I/O
C
      DATA NPAGE,LC/0,0/
C
C     OUTPUT MEAN SQUARE CALCULATION OVER ALL CLUSTERS
C
  999 FORMAT(1X)
C
      NPAGE=NPAGE+1
C
      WRITE(ICOUT,1) NPAGE
    1 FORMAT('1',110X,I5)
      WRITE(ICOUT,2) KK
    2 FORMAT(' OVERALL MEAN SQUARE CALCULATIONS, FOR EACH VARIABLE, ',
     1       ' WITH',I5,'  CLUSTERS')
      CALL DPWRST('XXX','WRIT')
C
      ASSW=0.
      DO 20 J=1,N
         SD=0.
         SC=0.
         SSB=0.
         SSW=0.
         DO 10 K=1,KK
            SD=SD+SUM(3,J,K)*SUM(2,J,K)
            SSB=SSB+SUM(3,J,K)**2*SUM(2,J,K)
            SSW=SSW+SUM(7,J,K)
   10       SC=SC+SUM(2,J,K)
         DFB=KK-1.
         DFW=SC-DFB-1.
         ASSW=ASSW+SSW
         IF(SC.GT.0.) SSB=SSB-SD**2/SC
         IF(DFB.GT.0.) SSB=SSB/DFB
         IF(DFW.GT.0.) SSW=SSW/DFW
         RATIO=0.
         IF(LC.NE.0.AND.SSW.GT.0.) RATIO=(R(J)/SSW-1.)*(1.+DFW)+1.
         R(J)=SSW
C
         WRITE(ICOUT,3)CLAB(J),SSW,DFW,SSB,DFB,RATIO
    3    FORMAT(' VARIABLE',4X,A4,F20.6,
     1          '(WITHIN MEAN SQ.)',F4.0,'(WITHIN DF)',F20.6,
     1          '(BETWEEN MSQ)',F4.0,'(BETWEEN DF)',F6.1,'(FRATIO)')
         CALL DPWRST('XXX','WRIT')
C
   20 CONTINUE
C
      WRITE(ICOUT,4) ASSW
    4 FORMAT(' OVERALL WITHIN SUM OF SQUARES',F20.6)
      CALL DPWRST('XXX','WRIT')
C
      LC=LC+1
C
C     OUTPUT STATISTICS FOR EACH CLUSTER
C
      DO 50 K=1,KK
C
         WRITE(ICOUT,5)
    5    FORMAT(1X,131('-'))
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,6) K,KK
    6    FORMAT(I5,'   TH CLUSTER OF',I5)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,7)
    7    FORMAT('CLUSTER MEMBERS WITH THEIR DISTANCES TO THE ',
     1          'CLUSTER CENTER')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,17)(I,I=1,10)
   17    FORMAT(13X,10I11)
         CALL DPWRST('XXX','WRIT')
C
         L=0
         DO 30 I=1,M
            IF(NCLUS(I).EQ.K) THEN
               L=L+1
               CWORK(L)=RLAB(I)
               DD(L)=DCLUS(I)
            ENDIF
            IF ((L.GE.10.OR.I.GE.M).AND.L.NE.0) THEN
C
               WRITE(ICOUT,8)(CWORK(LL),LL=1,L)
    8          FORMAT(15X,10(7X,A4))
               CALL DPWRST('XXX','WRIT')
               WRITE(ICOUT,9)(DD(LL),LL=1,L)
    9          FORMAT(15X,10F11.4)
               CALL DPWRST('XXX','WRIT')
C
               L=0
            ENDIF
   30    CONTINUE
C
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,11)
   11    FORMAT('SUMMARY STATISTICS FOR THE CLUSTER')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,12)
   12    FORMAT(' LABEL',5X,'CENTRE',8X,'COUNT',12X,'AVE',
     1           13X,'SD',11X,'XMIN',11X,'XMAX',12X,'SSQ')
         CALL DPWRST('XXX','WRIT')
C
         DO 40 J=1,N
C
            WRITE(ICOUT,13)CLAB(J),(SUM(I,J,K),I=1,7)
   13       FORMAT(1X,A4,7F15.6)
            CALL DPWRST('XXX','WRIT')
C
   40    CONTINUE
   50 CONTINUE
      RETURN
      END
      SUBROUTINE MIX(MM, M, N, A, CLAB1, CLAB2, RLAB, TITLE, K, MXITER,
     *               NCOV, DMWORK, WORK1, DMWRK1, DMWRK2, WORK2, DMWRK3,
     *               WORK3, IWORK,
     *               ICAPTY, ICAPSW, IFORSW, IERR)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      FITS THE MIXTURE MODEL BY A MAXIMUM LOG-LIKEHOOD CRITERION
C
C   DESCRIPTION
C   -----------
C
C   1.  THE DATA ARE ASSUMED TO BE A RANDOM SAMPLE OF SIZE M FROM A
C       MIXTURE OF K MULTIVARIATE NORMAL DISTRIBUTIONS IN N DIMENSIONS.
C       THE PROBABILITY THAT THE J-TH OBSERVATION WAS DRAWN FROM THE
C       I-TH NORMAL FOR J=1,...,M I=1,...,K IS USED TO ESTIMATE WHICH
C       NORMAL EACH OBSERVATION WAS SAMPLED FROM, AND HENCE GROUP THE
C       OBSERVATIONS INTO K CLUSTERS.  THE CRITERION TO BE MAXIMIZED IS
C       THE LOG LIKELIHOOD
C
C             SUM LOG(G(I)) OVER I=1,...,M
C
C       WHERE G(I) IS THE PROBABILITY DENSITY OF THE I-TH OBSERVATION.
C
C       SEE PAGE 116 OF THE REFERENCE FOR A FURTHER DESCRIPTION OF G.
C
C   2.  THE MANY PARAMETERS PRESENT IN THE BETWEEN-NORMAL COVARIANCE
C       MATRICES REQUIRE MUCH DATA FOR THEIR ESTIMATION.  A RULE OF
C       THUMB IS THAT M SHOULD BE GREATER THAN (N+1)(N+2)K/2.  EVEN
C       WITH MANY OBSERVATIONS, THE PROCEDURE IS VULNERABLE TO
C       NONNORMALITY OR LINEAR DEPENDENCE AMONG THE VARIABLES.  TO
C       REDUCE THIS SENSITIVITY ONE CAN MAKE ASSUMPTIONS ON THESE
C       COVARIANCE MATRICES BY SETTING THE NCOV PARAMETER TO:
C
C       1  IF THE COVARIANCE MATRICES ARE ARBITRARY
C       2  IF THE COVARIANCE MATRICES IN DIFFERENT NORMALS ARE EQUAL
C       3  IF THE COVARIANCE MATRICES ARE EQUAL AND DIAGONAL
C       4  IF ALL VARIABLES HAVE THE SAME VARIANCE AND ARE PAIRWISE
C             INDEPENDENT
C
C   3.  AFTER EVERY 5 ITERATIONS, THE CLUSTER PROBABILITIES, MEANS, AND
C       DETERMINANTS OF COVARIANCE MATRICES ARE PRINTED OUT.  ALSO, THE
C       WITHIN-CLUSTER VARIANCES AND CORRELATIONS FOR EVERY PAIR OF
C       VARIABLES FOR EACH CLUSTER, AND FINALLY EVERY OBSERVATION AND
C       ITS BELONGING PROBABILILTY FOR EACH CLUSTER IS PRINTED.  THE
C       LOG LIKELIHOOD IS PRINTED AFTER EACH ITERATION.  THE ITERATIONS
C       STOP EITHER AFTER THE MAXIMUM NUMBER OF ITERATIONS HAVE BEEN
C       REACHED OR AFTER THE INCREASE IN THE LOG LIKELIHOOD FROM ONE
C       ITERATION TO ANOTHER IS LESS THAT .0001.  ALL OUTPUT IS SENT TO
C       FORTRAN UNIT OUNIT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
C            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE VARIABLES.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE CASES.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE DESIRED NUMBER OF CLUSTERS.
C
C   MXITER INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.
C
C   NCOV  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         DETERMINES STRUCTURE OF THE WITHIN-CLUSTER COVARIANCE MATRIX
C
C             NCOV = 1   GENERAL COVARIANCES
C             NCOV = 2   COVARIANCES EQUAL BETWEEN CLUSTERS
C             NCOV = 3   COVARIANCES EQUAL AND DIAGONAL
C             NCOV = 4   COVARIANCES SPHERICAL
C
C   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF THE MATRIX WORK1.  MUST BE AT LEAST
C            2*M+N+1.
C
C   WORK1 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND WHOSE
C            SECOND DIMENSION MUST BE AT LEAST K.
C         WORK MATRIX.
C
C   DMWRK1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX WORK2.  MUST BE AT LEAST N.
C
C   DMWRK2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE SECOND DIMENSION OF THE MATRIX WORK2.  MUST BE AT LEAST
C            N+1.
C
C   WORK2 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWRK1, WHOSE SECOND
C            DIMENSION MUST BE DMWRK2, AND WHOSE THIRD DIMENSION MUST BE
C            AT LEAST K+1.
C         WORK MATRIX.
C
C   DMWRK3 INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF THE MATRIX WORK3.  MUST BE AT LEAST
C             N.
C
C   WORK3 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWRK3 AND WHOSE
C            SECOND DIMENSION MUST BE AT LEAST N+1.
C         WORK MATRIX.
C
C   IWORK INTEGER VECTOR DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   OUTPUT PARAMETER
C   ----------------
C
C   IERR  INTEGER SCALAR.
C         ERROR FLAG.
C
C         IF IERR = 0, NO ERROR WAS DETECTED.
C
C         IF IERR = K, THE K-TH PIVOT BLOCK OF ONE OF THE COVARIANCE
C                      MATRICES WAS SINGULAR.  THEREFORE, AN INVERSE
C                      COULD NOT BE CALCULATED AND EXECUTION WAS
C                      TERMINATED.  THE ERROR FLAG WAS SET IN CMLIB
C                      SUBROUTINE SSIFA.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 113-129.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER OUNIT, P, U, PMIX, T, DMWORK, DMWRK1, DMWRK2, DMWRK3
      DIMENSION A(MM,*), WORK1(DMWORK,*), WORK2(DMWRK1,DMWRK2,*),
     *           DETER(2), IWORK(*), WORK3(DMWRK3,*)
      CHARACTER*4 CLAB1(*), CLAB2(*)
      CHARACTER*8 RLAB(*)
      CHARACTER*10 TITLE
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      LOGICAL DONE
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     INITIALIZE
C
      DONE = .FALSE.
      P = 0
      U = P + M
      PMIX = U + N + 1
      T = PMIX
      XLL1 = -R1MACH(2)
      DO 10 J=1,K
   10    WORK2(1,N+1,J)=0.
      DO 30 I=1,M
         DO 20 J=1,K
   20       WORK1(P+I,J)=0.
         J=(I*K)/(M+1)+1
   30    WORK1(P+I,J)=1.
      DO 200 IT=1,MXITER
C
C     UPDATE MEANS AND COVARIANCES
C
         DO 40 J=1,K
   40       CALL CLUMOM(MM,M,N,A,J,WORK1(P+1,J),WORK1(U+1,J),DMWRK1,
     *               DMWRK2,WORK2)
C
C     UPDATE WEIGHTS
C
         WW=0.
         DO 60 J=1,K
            WORK1(PMIX,J)=0.
            DO 50 I=1,M
   50          WORK1(PMIX,J)=WORK1(PMIX,J)+WORK1(P+I,J)
   60    WW=WW+WORK1(PMIX,J)
         DO 70 J=1,K
   70       IF(WW.NE.0.) WORK1(PMIX,J)=WORK1(PMIX,J)/WW
C
C     ADJUST FOR COVARIANCE STRUCTURE
C
         IF(NCOV.NE.1) THEN
            DO 100 I=1,N
               DO 100 II=1,N
                  WORK2(I,II,1)=WORK1(PMIX,1)*WORK2(I,II,1)
                  DO 80 J=2,K
   80                WORK2(I,II,1)=WORK2(I,II,1)+WORK2(I,II,J)*
     *                             WORK1(PMIX,J)
                  IF(NCOV.GE.3.AND.I.NE.II) WORK2(I,II,1)=0.
                  DO 90 J=2,K
   90                WORK2(I,II,J)=WORK2(I,II,1)
  100       CONTINUE
            IF (NCOV.EQ.4) THEN
               CC=0.
               DO 110 I=1,N
  110             CC=CC+WORK2(I,I,1)
               CC=CC/N
               DO 120 I=1,N
                  DO 120 J=1,K
  120                WORK2(I,I,J)=CC
            ENDIF
         ENDIF
         II=IT-1
         IF(((II/5)*5.EQ.II.OR.DONE))
     *       CALL COVOUT(MM,M,N,A,CLAB1,CLAB2,RLAB,TITLE,K,DMWORK,WORK1,
     *            DMWRK1,DMWRK2,WORK2,WORK1(T+1,1),
     *            ICAPTY,ICAPSW,IFORSW)
         IF (DONE) RETURN
C
C     UPDATE BELONGING PROBABILITIES
C
         DO 160 J=1,K
C
C     COMPUTE INVERSES AND DETERMINANTS OF COVARIANCE MATRICES
C
            DO 130 III = 1 , N
               DO 130 JJJ = 1 , N
 130              WORK3(III,JJJ) = WORK2(III,JJJ,J)
            CALL INVERT(DMWRK3,N,WORK3,DETER,WORK3(1,N+1),IWORK,IERR,
     *                  OUNIT)
            IF (IERR .NE. 0) RETURN
            DET = DETER(1) * (10. ** DETER(2))
            DO 140 III = 1 , N
               DO 140 JJJ = 1 , N
 140              WORK2(III,JJJ,J) = WORK3(III,JJJ)
            IF(DET.EQ.0.) RETURN
            DET=SQRT(ABS(DET))
            WORK2(1,N+1,J)=DET
C
C     COMPUTE PROBABILITY DENSITY FOR THE I-TH OBSERVATION FROM THE J-TH
C     NORMAL
C
            DO 160 I=1,M
               S=0.
               DO 150 L=1,N
                  DO 150 LL=1,N
  150                S=S+WORK2(L,LL,J)*(A(I,L)-WORK1(U+L,J))*(A(I,LL)-
     *                   WORK1(U+LL,J))
               IF(S.GT.100.) S=100.
  160          WORK1(T+I,J)=EXP(-S/2.)*WORK1(PMIX,J)/DET
C
C     COMPUTES LOG LIKELIHOOD
C
         XLL=0.
         DO 180 I=1,M
            S=0.
            DO 170 J=1,K
  170          S=S+WORK1(T+I,J)
            IF(S.EQ.0.) S=R1MACH(4)
            XLL=XLL+LOG(S)
            DO 180 J=1,K
  180          WORK1(T+I,J)=WORK1(T+I,J)/S
            IF (IPRINT.EQ.'ON') THEN
               WRITE(ICOUT,1) IT,XLL
    1          FORMAT(' ITERATION = ',I5,' LOG LIKELIHOOD = ',F12.6)
               CALL DPWRST('XXX','WRIT')
            ENDIF
C
C     UPDATE PROBABILITY THE I-TH OBSERVATION WAS DRAWN FROM THE J-TH
C     NORMAL
C
         DO 190 I=1,M
            DO 190 J=1,K
               XIT=MXITER
               ALPHA=1.+.7*IT/XIT
               WORK1(P+I,J)=ALPHA*WORK1(T+I,J)-(ALPHA-1.)*WORK1(P+I,J)
C
C     AT EVERY FIFTH ITERATION, SET PROBABILITIES TO EITHER ZERO OR ONE
C
               IF(IT.EQ.5.AND.WORK1(P+I,J).GT.0.5) WORK1(P+I,J)=1.
               IF(IT.EQ.5.AND.WORK1(P+I,J).LE.0.5) WORK1(P+I,J)=0.
               IF(WORK1(P+I,J).GT.1.) WORK1(P+I,J)=1.
               IF(WORK1(P+I,J).LT.0.) WORK1(P+I,J)=0.
  190    CONTINUE
C
C     RETURN IF NO CHANGE IN LOG LIKELIHOOD
C
         IF (XLL-XLL1 .LE. .00001) DONE = .TRUE.
         XLL1 = XLL
  200 CONTINUE
      RETURN
      END
      SUBROUTINE MIXIND(MM, M, N, A, CLAB, RLAB, TITLE, K, DMWORK,
     *                  WORK1, WORK2, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      FITS THE MIXTURE MODEL FROM K MULTIVARIATE NORMALS WHERE K IS
C      THE DESIRED NUMBER OF CLUSTERS.  THE VARIABLES ARE ASSUMED TO
C      HAVE VARIANCE CONSTANT OVER DIFFERENT CLUSTERS
C
C   DESCRIPTION
C   -----------
C
C   1.  THE DATA ARE ASSUMED TO BE A RANDOM SAMPLE OF SIZE M FROM A
C       MIXTURE OF K MULTIVARIATE NORMAL DISTRIBUTIONS IN N DIMENSIONS.
C       THE SUBROUTINE PREDICTS THE DISTRIBUTION THAT EACH OBSERVATION
C       WAS SAMPLED FROM AND HENCE GROUPS THE OBSERVATIONS INTO K
C       CLUSTERS.  SEE PAGE 113 OF THE REFERENCE FOR A FURTHER
C       DESCRIPTION OF THE MIXTURE ALGORITHM.
C
C   2.  THE ROUTINE BEGINS WITH THE CLUSTER OF ALL OBJECTS AND THEN
C       DIVIDES INTO TWO, THEN THREE, ..., THEN FINALLY K CLUSTERS.
C       THE RESULTS ARE PRINTED AFTER EACH DIVISION ON FORTRAN UNIT
C       OUNIT.  THE RESULTS CONSIST OF THE WITHIN-CLUSTER VARIANCES FOR
C       EACH VARIABLE, THEN SETS UP A COLUMN FOR EACH CLUSTER.  THE
C       MIXTURE PROBABILITY IS THE PROBABILITY THAT A NEW OBJECT WILL
C       BE GROUPED INTO THAT CLUSTER.  THEN THE MEANS OF THE VARIABLES
C       FOR THE CLUSTER ARE PRINTED, AS WELL AS THE PROBABILITIES THAT
C       EACH CASE BELONGS TO EACH CLUSTER.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
C            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE VARIABLES.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE CASES.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CLUSTERS.
C
C   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF THE MATRIX WORK1.  MUST BE AT LEAST
C            N+M+1.
C
C   WORK1 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND WHOSE
C            SECOND DIMENSION MUST BE AT LEAST K.
C         WORK MATRIX.
C
C   WORK2 REAL VECTOR DIMENSIONED AT LEAST N.
C         WORK VECTOR.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 113-129.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMWORK, U, P, PMIX, OUNIT
      DIMENSION A(MM,*), WORK1(DMWORK,*), WORK2(*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
C
      INCLUDE 'DPCOMC.INC'
C
      U = 0
      P = U + N
      PMIX = P + M + 1
      XM=99999.
      TH=.0001
      DO 160 KK=1,K
C
C     IF NOT FIRST PASS, FIND FURTHEST CASE FROM PRESENT MEANS
C
         DM=0.
         IM=1
         IF(KK.NE.1) THEN
            DO 30 I=1,M
               DI=R1MACH(2)/N
               DO 20 KL=1,KK-1
                  DD=0.
                  XC=0.
                  DO 10 J=1,N
                     IF(A(I,J).NE.XM) THEN
                        XC=XC+1.
                        DD=DD+(A(I,J)-WORK1(U+J,KL))**2 /WORK2(J)
                        IF(DD.GT.DI*N) GO TO 20
                     ENDIF
   10             CONTINUE
                  IF(XC.EQ.0.) GO TO 30
                  DD=DD/XC
   20             IF(DD.LT.DI) DI=DD
               IF(DI.GE.DM) THEN
                  DM=DI
                  IM=I
               ENDIF
   30       CONTINUE
         ENDIF
C
C     BEGIN A NEW CLUSTER LABELED KK
C
         DO 40 J=1,N
   40       WORK1(U+J,KK)=A(IM,J)
         WORK1(PMIX,KK)=EXP(0.5*N)
         ITER=25
         DO 150 IT=1,ITER
C
C     UPDATE PROBABILITIES OF BELONGING
C
            DO 90 I=1,M
               PP=0.
               DO 60 KL=1,KK
                  DD=0.
                  DO 50 J=1,N
                     IF(A(I,J).NE.XM.AND.KK.NE.1)
     *                  DD=DD+(A(I,J)-WORK1(U+J,KL))**2/(WORK2(J)*2.)
   50             CONTINUE
                  IF(DD.GT.100.) DD=100.
                  WORK1(P+I,KL)=WORK1(PMIX,KL)*EXP(-DD)
   60          PP=PP+WORK1(P+I,KL)
               IF(PP.NE.0.) THEN
                  PN=0.
                  DO 70 KL=1,KK
                     WORK1(P+I,KL)=WORK1(P+I,KL)/PP
                     IF(WORK1(P+I,KL).LT.TH) WORK1(P+I,KL)=0.
                     PN =PN+WORK1(P+I,KL)
   70             CONTINUE
                  DO 80 KL=1,KK
   80                WORK1(P+I,KL)=WORK1(P+I,KL)/PN
               ENDIF
   90       CONTINUE
C
C     UPDATE MIXTURE PROBABILITIES
C
            DO 100 KL=1,KK
               WORK1(PMIX,KL)=0.
               DO 100 I=1,M
  100             WORK1(PMIX,KL)=WORK1(PMIX,KL)+WORK1(P+I,KL)/M
C
C     UPDATE CLUSTER ESTIMATES, EACH ONE A WEIGHTED MEAN
C
            DO 120 KL=1,KK
               DO 120 J=1,N
                  WORK1(U+J,KL)=0.
                  DO 110 I=1,M
  110                WORK1(U+J,KL)=WORK1(U+J,KL)+A(I,J)*WORK1(P+I,KL)
  120       IF(WORK1(PMIX,KL).NE.0.) WORK1(U+J,KL)=WORK1(U+J,KL)/
     *                                            (WORK1(PMIX,KL)*M)
            DO 140 J=1,N
               WORK2(J)=0.
               DO 130 I=1,M
                  DO 130 KL=1,KK
  130                WORK2(J)=WORK2(J)+(A(I,J)-WORK1(U+J,KL))**2*
     *                                  WORK1(P+I,KL)
  140       WORK2(J)=WORK2(J)/M
  150    CONTINUE
C
C     PRINT RESULTS OF ITERATION
C
         IF (OUNIT .GT. 0) CALL MIXOUT(MM,M,N,A,CLAB,RLAB,TITLE,KK,
     *                                 DMWORK,WORK1,WORK2,OUNIT)
  160 CONTINUE
      RETURN
      END
      SUBROUTINE MIXOUT(MM, M, N, A, CLAB, RLAB, TITLE, K, DMWORK,
     *                  WORK1, WORK2, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      PRINTS THE RESULTS FOR EACH ITERATION OF MIXIND
C
C   DESCRIPTION
C   -----------
C
C   1.  SEE SUBROUTINE MIXIND FOR DESCRIPTION OF OUTPUT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE CURRENT NUMBER OF CLUSTERS.
C
C   FOR OTHER PARAMETERS -- SEE SUBROUTINE MIXIND
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 129.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMWORK, U, P, PMIX, OUNIT
      DIMENSION A(MM,*), WORK1(DMWORK,*), WORK2(*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      U = 0
      P = U + N
      PMIX = P + M + 1
C
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1) TITLE,K
    1 FORMAT(' MIXTURE MODEL FOR',2X,A10,'WITH',I5,' CLUSTERS')
      CALL DPWRST('XXX','WRIT')
C
C     PRINT VARIANCES
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2)
    2 FORMAT(' WITHIN CLUSTER VARIANCES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,222)(WORK2(J),CLAB(J),J=1,N)
  222 FORMAT(5(F15.6,'(',A4,')'))
      CALL DPWRST('XXX','WRIT')
C
C     PRINT CLUSTER PROBABILITIES
C
      WRITE(ICOUT,3)(KK,KK=1,K)
    3 FORMAT(9X,' CLUSTER', 9(I3,1X,' CLUSTER'))
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,4)(WORK1(PMIX,KK),KK=1,K)
    4 FORMAT(' MIXTURE PROBABILITIES',/(7X,10F12.6))
      CALL DPWRST('XXX','WRIT')
C
C     PRINT MEANS
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,5)
    5 FORMAT(' CLUSTER MEANS')
      CALL DPWRST('XXX','WRIT')
C
      DO 10 J=1,N
         WRITE(ICOUT,6) CLAB(J),(WORK1(U+J,KK),KK=1,K)
    6    FORMAT(1X,A4,2X,10F12.4)
         CALL DPWRST('XXX','WRIT')
   10 CONTINUE
C
C     PRINT PROBABILITIES
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,7)
    7 FORMAT(' BELONGING PROBABILITIES')
      CALL DPWRST('XXX','WRIT')
C
      DO 20 I=1,M
         WRITE(ICOUT,8) RLAB(I),(WORK1(P+I,KK),KK=1,K)
    8    FORMAT(1X,A4,2X,10F12.6)
         CALL DPWRST('XXX','WRIT')
   20 CONTINUE
      RETURN
      END
      SUBROUTINE QUICK(MM, M, N, A, CLAB, RLAB, TITLE, THRESH, XMISS,
     *                 NC, IWORK, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      FINDS A QUICK PARTITION OF THE CASES BY COMPARING, TO A USER-
C      DEFINED THRESHOLD, THE EUCLIDEAN DISTANCES TO THE EXISTING
C      CLUSTER LEADERS
C
C   DESCRIPTION
C   -----------
C
C   1.  INITIALLY, THE FIRST CASE WILL BE ASSIGNED TO THE FIRST CLUSTER
C       AND BECOMES THE LEADER OF THE FIRST CLUSTER.  THEN, GIVEN A NEW
C       CASE, CYCLE THROUGH THE EXISTING CLUSTERS IN ORDER.  PLACE THE
C       CASE IN THE FIRST CLUSTER WHERE THE DISTANCE BETWEEN THE CASE
C       AND THE CLUSTER LEADER IS LESS THAN THE THRESHOLD.  IF NO
C       CLUSTER EXISTS, PLACE THE CASE IN A NEW CLUSTER MAKING IT THE
C       CLUSTER LEADER.  ONCE THE MAXIMUM NUMBER OF DESIRED CLUSTERS
C       HAS BEEN REACHED, NO NEW CLUSTERS WILL BE FORMED AND CASES NOT
C       BELONGING TO AN EXISTING CLUSTER WILL BE IGNORED.
C
C   2.  THE DISTANCE FUNCTION USED IS THE EUCLIDEAN DISTANCE.  THE
C       VARIABLES SHOULD BE SCALED SIMILARLY (CLUSTER SUBROUTINE STAND
C       CAN BE USED TO STANDARDIZE THE VARIABLES).  ANY MISSING VALUES
C       WILL BE IGNORED IN THE DISTANCE CALCULATION.
C
C   3.  THE OUTPUT IS ON FORTRAN UNIT OUNIT, WHICH FOR EACH CLUSTER IS
C       THE CLUSTER LEADER AND ITS VALUES FOLLOWED BY THE OTHER
C       MEMBERS.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
C            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
C         THE MATRIX OF DATA VALUES.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE VARIABLES.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
C            (UNCHANGED ON OUTPUT).
C         THE LABELS OF THE CASES.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   THRESH REAL SCALAR (UNCHANGED ON OUTPUT).
C         THRESHOLD SUCH THAT ANY TWO CASES WHOSE DISTANCE IS LESS
C         THAN THRESH WILL BE ASSIGNED TO THE SAME CLUSTER.
C
C   XMISS REAL SCALAR (UNCHANGED ON OUTPUT).
C         MISSING VALUE CODE.  IF A(I,J) = XMISS, THEN THE VALUE FOR THE
C         J-TH VARIABLE FOR THE I-TH CASE IS ASSUMED TO BE MISSING.
C
C   NC    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         MAXIMUM NUMBER OF CLUSTERS DESIRED.
C
C   IWORK INTEGER VECTOR DIMENSIONED AT LEAST M+NC.
C         WORK VECTOR.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 74-83.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      DIMENSION A(MM,*), IWORK(*)
      INTEGER OUNIT
      CHARACTER*4 CLAB(*), RLAB(*), AA(20)
      CHARACTER*10 TITLE
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      LL = 0
      LC = LL + NC
C
C     ASSIGN THE FIRST CASE TO THE FIRST CLUSTER
C
      KC=1
      IWORK(LL+1)=1
      DMAX=N * THRESH**2
      DO 30 I=1,M
         IWORK(LC+I)=0
         DO 20 KK=1,KC
            K=KC-KK+1
            L=IWORK(LL+K)
C
C     COMPUTES DISTANCE BETWEEN CASE AND CLUSTER LEADER
C
            DD=0.
            DC=0.
            DO 10 J=1,N
               IF (A(L,J).NE.XMISS.AND.A(I,J).NE.XMISS) THEN
                  DC=DC+1.
                  DD=DD+(A(L,J)-A(I,J))**2
C
C     GET NEXT CLUSTER IF DISTANCE IS TOO LARGE
C
                  IF(DD.GT.DMAX) GO TO 20
               ENDIF
   10       CONTINUE
            IF(DC.NE.0.) DD=SQRT(DD/DC)
C
C     ASSIGN CASE I TO CLUSTER K IF DISTANCE BELOW THRESHOLD
C
            IF (DD.LE.THRESH) THEN
               IWORK(LC+I)=K
               GO TO 30
            ENDIF
   20    CONTINUE
C
C     CREATE NEW CLUSTER AND LEADER
C
         IF (KC.NE.NC) THEN
            KC=KC+1
            IWORK(LC+I)=KC
            IWORK(LL+KC)=I
         ENDIF
   30 CONTINUE
C
C     OUTPUT CLUSTER LEADERS
C
      IF (OUNIT .LE. 0) GOTO9000
C
      WRITE(ICOUT,1)
    1 FORMAT(' CLUSTER LEADERS')
      CALL DPWRST('XXX','WRIT')
C
      DO 40 K=1,KC
         I=IWORK(LL+K)
C
         WRITE(OUNIT,2) K, RLAB(I),(A(I,J),J=1,MAX(N,10))
    2    FORMAT(' CLUSTER',I4,2X,A4,10F11.4)
         CALL DPWRST('XXX','WRIT')
         IF (N.GT.10)THEN
            WRITE(OUNIT,12) (A(I,J),J=11,N)
   12       FORMAT(18X,10F11.4)
            CALL DPWRST('XXX','WRIT')
         ENDIF
   40 CONTINUE
C
      WRITE(ICOUT,3)
    3 FORMAT(1X)
      CALL DPWRST('XXX','WRIT')
C
C     OUTPUT CLUSTERS
C
      KC=KC+1
      DO 50 K=1,KC
         KK=K-1
         J=0
         DO 50 I=1,M
            IF (J.EQ.20) J=0
            IF (IWORK(LC+I).EQ.KK) THEN
               J=J+1
               AA(J)=RLAB(I)
            ENDIF
            IF (J.EQ.20.OR.(I.EQ.M.AND.J.NE.0)) THEN
               WRITE(OUNIT,4) KK,(AA(JJ),JJ=1,J)
    4          FORMAT(' CLUSTER',I5,20(1X,A4))
               CALL DPWRST('XXX','WRIT')
            ENDIF
   60    CONTINUE
   50 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE RSPLIT(MM, M, N, A, RLAB, IR, KA, TH, IORD, DMIWRK,
     *                  IWORK, DMWORK, WORK)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      FINDS OPTIMAL SPLIT OF THE CASES
C
C   DESCRIPTION
C   -----------
C
C   1.  INITIALLY, THE FIRST CLUSTER CONSISTS OF ALL CASES WITHIN THE
C       BLOCK IR AND THE SECOND CLUSTER IS EMPTY.  THE REDUCTION IN THE
C       WITHIN-CLUSTER SUM OF SQUARES FOR MOVING EACH CASE FROM THE
C       FIRST CLUSTER TO THE SECOND IS CALCULATED.  THE CASE THAT
C       REDUCES THE SUM OF SQUARES THE MOST IS MOVED AND THIS CONTINUES
C       UNTIL ALL CASES ARE MOVED WITH EACH REDUCTION STORED.  THEN THE
C       SPLIT THAT HAD THE SMALLEST REDUCTION OF ALL IS RETURNED AS THE
C       OPTIMUM SPLIT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM, M, N, A, RLAB, TH, IORD, DMIWRK, DMWORK -- SEE SUBROUTINE SPLIT2
C
C   IR    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         NUMBER OF BLOCK TO BE SPLIT.
C
C   KA    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         NUMBER OF BLOCKS.
C
C   IWORK INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND SECOND
C            DIMENSION MUST BE AT LEAST KA.
C         THE MATRIX DEFINING THE BOUNDARIES OF THE BLOCKS.
C
C         IWORK(1,I) IS 1 + THE FIRST ROW IN BLOCK I
C         IWORK(2,I) IS 1 + THE LAST ROW IN BLOCK I
C         IWORK(3,I) IS 1 + THE FIRST COLUMN IN BLOCK I
C         IWORK(4,I) IS 1 + THE LAST COLUMN IN BLOCK I
C
C   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
C            DIMENSION MUST BE AT LEAST MAX(M,N).
C
C         WORK(1,I) = FIRST CASE IN CASE CLUSTER I
C         WORK(2,I) = LAST CASE IN CASE CLUSTER I
C         WORK(3,I) = REDUCTION IN SSQ DUE TO SPLITTING
C         WORK(4,I) = LAST CASE IN FIRST CLUSTER OF SPLIT OF I
C         WORK(5,I) = 1 IF CASE IS INCLUDED IN PRESENT VARIABLE SPLIT
C         WORK(6,I) = NUMBER OF VARIABLES IN I-TH ROW OF PRESENT
C                        VARIABLE SPLIT
C         WORK(7,I) = MEAN OF I-TH CASE, FIRST VARIABLE CLUSTER
C         WORK(8,I) = NUMBER OF VARIABLES SECOND CLUSTER
C         WORK(9,I) = MEAN OF I-TH CASE, SECOND CLUSTER
C
C         WORK(10-18,I) ARE SIMILAR WITH VARIABLES AND CASES REVERSED.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 277.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMWORK, DMIWRK
      DIMENSION A(MM,*),IWORK(DMIWRK,*),WORK(DMWORK,*)
      CHARACTER*4 RLAB(*), C
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      XM=99999.
      DO 10 J=1,N
   10    WORK(14,J)=0.
C
C     LOOK FOR BLOCKS WITHIN THRESHOLD
C
      IL=INT(WORK(1,IR))
      IU=INT(WORK(2,IR))
      DO 40 K=1,KA
         IF(IWORK(1,K).EQ.IL+1.AND.IWORK(2,K).EQ.IU+1) THEN
            JL=IWORK(3,K)
            JU=IWORK(4,K)
            IF(JL.LT.0) JL=-JL
C
C     COMPUTE VARIANCES
C
            NC=0
            DO 30 J=JL-1,JU-1
               S1=0.
               S2=0.
               S3=0.
               DO 20 I=IL,IU
                  IF(A(I,J).NE.XM) THEN
                     S1=S1+1
                     S2=S2+A(I,J)
                     S3=S3+A(I,J)**2
                  ENDIF
   20          CONTINUE
               WORK(15,J)=S1
               IF(S1.NE.0) THEN
                  S3=S3/S1-(S2/S1)**2
                  WORK(16,J)=S2/S1
               ENDIF
               IF(S3.GT.TH) THEN
                  WORK(14,J)=1.
                  NC=1
               ENDIF
   30       CONTINUE
            IF(NC.EQ.0) IWORK(1,K)=-IWORK(1,K)
         ENDIF
   40 CONTINUE
C
C     FIND BEST CASE SPLIT
C
      DO 50 J=1,N
         WORK(17,J)=0.
   50    WORK(18,J)=0.
      DM=0.
      WORK(3,IR)=0.
      WORK(4,IR)=IL
      DO 100 I=IL,IU-1
         II=IU-I+IL
         ID=II
         DD=-R1MACH(2)
         DO 70 L=IL,II
            IF((IORD.NE.1.AND.IORD.NE.3).OR.L.EQ.II) THEN
               DL=0.
               DO 60 J=1,N
                  IF(WORK(14,J).NE.0.AND.A(L,J).NE.XM) THEN
                     DL=DL+(A(L,J)-WORK(16,J))**2*(WORK(15,J)+1)/
     *                     WORK(15,J)
                     DL=DL-(A(L,J)-WORK(18,J))**2*WORK(17,J)/
     *                     (WORK(17,J)+1)
                  ENDIF
   60          CONTINUE
               IF(DL.GT.DD) THEN
                  DD=DL
                  ID=L
               ENDIF
            ENDIF
   70    CONTINUE
C
C     INTERCHANGE ID AND II
C
         DO 80 J=1,N
            CC=A(II,J)
            A(II,J)=A(ID,J)
   80       A(ID,J)=CC
         C = RLAB(II)
         RLAB(II) = RLAB(ID)
         RLAB(ID) = C
C
C     UPDATE MEANS
C
         DO 90 J=1,N
            IF(WORK(14,J).NE.0.AND.A(II,J).NE.XM) THEN
               WORK(15,J)=WORK(15,J)-1.
               IF(WORK(15,J).NE.0.)WORK(16,J)=WORK(16,J)+
     *                            (WORK(16,J)-A(II,J))/WORK(15,J)
               WORK(17,J)=WORK(17,J)+1.
               WORK(18,J)=WORK(18,J)-(WORK(18,J)-A(II,J))/WORK(17,J)
            ENDIF
   90    CONTINUE
         DM=DM+DD
         IF(DM.GE.WORK(3,IR)) THEN
            WORK(3,IR)=DM
            WORK(4,IR)=II-1
         ENDIF
  100 CONTINUE
      RETURN
      END
      SUBROUTINE SINGLE(X, COUNT, AVE, SD, XMIN, XMAX, SSQ)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      INCORPORATES A NEW VALUE INTO THE SUMMARY STATISTICS
C
C   INPUT PARAMETERS
C   ----------------
C
C   SEE SUBROUTINE BUILD FOR PARAMETER DESCRIPTIONS.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 109.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INCLUDE 'DPCOMC.INC'
C
      IF(COUNT.EQ.0.) THEN
         AVE=0.
         SD=0.
         XMIN=R1MACH(2)
         XMAX=-R1MACH(2)
         SSQ=0.
      ENDIF
      COUNT=COUNT+1.
      AVE=AVE+(X-AVE)/COUNT
      IF(COUNT.NE.1.) SSQ=SSQ+COUNT*(X-AVE)**2/(COUNT-1.)
      SD=(SSQ/COUNT)**0.5
      IF(XMIN.GT.X) XMIN=X
      IF(XMAX.LT.X) XMAX=X
      RETURN
      END
      SUBROUTINE SPLIT(MM, M, N, A, CLAB, RLAB, DMW, W, IL, IU, DMU, U,
     *                 WCLAB, IM, DM)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      SPLITS A ROW CLUSTER ON SELECTED VARIABLES
C
C   DESCRIPTION
C   -----------
C
C   1.  INITIALLY, THE FIRST CLUSTER CONSISTS OF ALL CASES BETWEEN IL
C       AND IU AND THE SECOND CLUSTER IS EMPTY.  THE WEIGHTED MEANS ARE
C       DETERMINED AND USED TO FIND THE REDUCTION IN THE WITHIN-CLUSTER
C       SUM OF SQUARES FOR MOVING EACH CASE FROM THE FIRST CLUSTER TO
C       THE SECOND.  THE OBJECT THAT REDUCES THE SUM OF SQUARES THE
C       MOST IS MOVED AND THIS CONTINUES UNTIL ALL OBJECTS ARE MOVED
C       WITH EACH REDUCTION STORED.  THEN THE SPLIT THAT HAD THE
C       SMALLEST REDUCTION OF ALL IS RETURNED AS THE OPTIMUM SPLIT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM, M, N, A, CLAB, RLAB, DMW, W -- SEE SUBROUTINE SPLIT1
C
C   IL, IU INTEGER SCALARS (UNCHANGED ON OUTPUT).
C         THE FIRST AND LAST OBJECTS IN THE BLOCK TO BE SPLIT.
C
C   DMU   INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX U.  MUST BE AT LEAST 4.
C
C   U     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMU AND SECOND
C            DIMENSION MUST BE AT LEAST N (CHANGED ON OUTPUT).
C         MATRIX OF CLUSTER MEANS.
C
C   OUTPUT PARAMETERS
C   -----------------
C
C   WCLAB INTEGER VECTOR DIMENSIONED AT LEAST N.
C         WCLAB(I) WILL STORE THE CLUSTER (EITHER 1 OR 2) OBJECT I WAS
C            ASSIGNED TO.
C
C   IM    INTEGER SCALAR.
C         THE BORDER OF THE SPLIT.  OBJECTS IL,...,IM WERE ASSIGNED TO
C            CLUSTER 1 AND OBJECTS IM+1,...,IU WERE ASSIGNED TO CLUSTER
C            2.
C
C   DM    INTEGER SCALAR.
C         THE REDUCTION IN THE WITHIN-CLUSTER SUM OF SQUARES.
C
C   REFERENCE
C   ---------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGE 272.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMW, DMU, WCLAB(*)
      DIMENSION W(DMW,*), A(MM,*), U(DMU,*)
      CHARACTER*4 CLAB(*), RLAB(*), CTEMP
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     FIND WEIGHTED MEAN OF ALL CASES
C
      TH=R1MACH(4)
      DO 10 J=1,N
         U(1,J)=0.
         U(3,J)=0.
         U(2,J)=TH
         U(4,J)=TH
   10 CONTINUE
      DO 30 J=1,N
         IF(WCLAB(J).NE.0) THEN
            DO 20 I=IL,IU
               U(1,J)=U(1,J)+A(I,J)*W(I,J)
   20          U(2,J)=U(2,J)+WCLAB(J)
            U(1,J)=U(1,J)/U(2,J)
         ENDIF
   30 CONTINUE
      DM=0.
      DD=0.
      DO 80 IC=IL,IU
         II=IU-IC+IL
         DMAX=-R1MACH(2)
         IMAX=II
C
C     DETERMINE THE EFFECT OF MOVING ITH CASE
C
         DO 50 I=IL,II
            D=0.
            DO 40 J=1,N
               IF(WCLAB(J).NE.0) THEN
                 IF(U(2,J).EQ.W(I,J)) U(2,J)=W(I,J)+TH
                 D=D+W(I,J)*U(2,J)*(A(I,J)-U(1,J))**2/(U(2,J)-W(I,J))
                 D=D-W(I,J)*U(4,J)*(A(I,J)-U(3,J))**2/(U(4,J)+W(I,J))
               ENDIF
   40       CONTINUE
C
C     STORE THE LARGEST
C
            IF(D.GT.DMAX) THEN
               IMAX=I
               DMAX=D
            ENDIF
   50    CONTINUE
         DD=DD+DMAX
         IF(DD.GT.DM) IM=II-1
         IF(DD.GT.DM) DM=DD
C
C     UPDATE MEANS OF THE TWO CLUSTERS
C
         I=IMAX
         DO 60 J=1,N
            IF(WCLAB(J).NE.0) THEN
               U(2,J)=U(2,J)-W(I,J)
               IF(U(2,J).LT.TH) U(2,J)=TH
               U(1,J)=U(1,J)+(U(1,J)-A(I,J))*W(I,J)/U(2,J)
               U(4,J)=U(4,J)+W(I,J)
               U(3,J)=U(3,J)-(U(3,J)-A(I,J))*W(I,J)/U(4,J)
            ENDIF
   60    CONTINUE
C
C     INTERCHANGE SELECTED ROW WITH LAST FEASIBLE ROW
C
         DO 70 J=1,N
            C=A(I,J)
            A(I,J)=A(II,J)
            A(II,J)=C
            C=W(I,J)
            W(I,J)=W(II,J)
   70       W(II,J)=C
         CTEMP = RLAB(I)
         RLAB(I) = RLAB(II)
         RLAB(II) = CTEMP
   80 CONTINUE
      RETURN
      END
      SUBROUTINE SPLIT1(MM, M, N, A, CLAB, RLAB, TITLE, DMW, W, TH,
     *                  KD, IWORK, DMIWRK, IWORK1, DMWORK, WORK, IERR,
     *                  OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      SPLITS THE CASES IN EACH VARIABLE UNTIL ALL WITHIN-CLUSTER
C      VARIANCES ARE SMALLER THAN A USER-SPECIFIED THRESHOLD
C
C   DESCRIPTION
C   -----------
C
C   1.  THE THRESHOLD IS THE LARGEST WITHIN-CLUSTER VARIANCE FOR EACH
C       VARIABLE.  THE VARIABLES MUST BE SCALED SIMILARLY (CLUSTER
C       SUBROUTINE STAND CAN BE USED TO STANDARDIZE THE VARIABLES).
C       THE ROUTINE STARTS WITH ONE CLUSTER OF ALL CASES FOR EACH
C       VARIABLE.  FOR EACH CLUSTER WHOSE VARIANCE IS LARGER THAN THE
C       THRESHOLD, IT IS SPLIT INTO TWO CLUSTERS SUCH THAT THE SUM OF
C       THE TWO WITHIN-CLUSTER VARIANCES IS SMALLEST.  THIS REPEATS
C       UNTIL ALL CLUSTER VARIANCES ARE SMALLER THAN THE THRESHOLD.
C       THE THRESHOLD SHOULD BE CHOSEN WISELY AS A LARGE THRESHOLD WILL
C       PRODUCE A FEW LARGE CLUSTERS AND A SMALL THRESHOLD WILL PRODUCE
C       MANY SMALL CLUSTERS.
C
C   2.  A MATRIX CAN BE USED TO WEIGH THE DATA VALUES.  A WEIGHT OF 1.
C       WILL GIVE THE VALUE FULL WEIGHT, A WEIGHT OF 0.  WILL GIVE THE
C       VALUE NO WEIGHT (IE.  A MISSING VALUE).  ALL WEIGHTS MUST BE
C       BETWEEN 0.  AND 1., AND THE WEIGHT MATRIX WILL BE DESTROYED
C       DURING EXECUTION.
C
C   3.  THE OUTPUT DIAGRAM IS AN ARRAY WITH THE VARIABLES LABELING THE
C       COLUMNS AND THE CASES LABELING THE ROWS AND THE VARIABLE VALUES
C       MULTIPLIED BY 10 AS THE ELEMENTS OF THE ARRAY.  THE HORIZONTAL
C       LINES OUTLINE THE BLOCKS AS EACH BLOCK IS ASSUMED TO CONTAIN
C       ONLY ONE VARIABLE AND HENCE, ONLY ONE COLUMN.  THE OUTPUT
C       DIAGRAM IS WRITTEN ON FORTRAN UNIT OUNIT.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF CASES.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
C            DIMENSION MUST BE AT LEAST M (CHANGED ON OUTPUT).
C         THE DATA MATRIX.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N
C            (UNCHANGED ON OUTPUT).
C         LABELS OF THE VARIABLES.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
C            (CHANGED ON OUTPUT).
C         LABELS OF THE CASES.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   DMW   INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX W.  MUST BE AT LEAST M.
C
C   W     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMW AND SECOND
C            DIMENSION MUST BE AT LEAST N (CHANGED ON OUTPUT).
C         W(I,J) IS THE WEIGHT OF VARIABLE J FOR CASE I AND SHOULD BE
C            BETWEEN 0. AND 1.  MISSING VALUES SHOULD BE GIVEN A WEIGHT
C            OF 0.
C
C   TH    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THRESHOLD VARIANCE FOR VARIABLES WITHIN CLUSTERS.
C
C   KD    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE MAXIMUM NUMBER OF BLOCKS ALLOCATED (SECOND DIMENSION OF
C            IWORK1).  THE SMALLEST K SHOULD BE IS M AND THE LARGEST IS
C            N*M.
C
C   IWORK INTEGER VECTOR DIMENSIONED AT LEAST 2*M+N.
C         WORK VECTOR.
C
C   DMIWRK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX IWORK1.  MUST BE AT LEAST 4.
C
C   IWORK1 INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND
C            SECOND DIMENSION MUST BE AT LEAST KD.
C         WORK MATRIX.
C
C   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX WORK.  MUST BE AT LEAST 4.
C
C   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
C            SECOND MUST BE AT LEAST N (CHANGED ON OUTPUT).
C         WORK MATRIX.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   OUTPUT PARAMETER
C   ----------------
C
C   IERR  INTEGER SCALAR.
C         ERROR FLAG.
C
C         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
C
C         IERR = 1, THE NUMBER OF BLOCKS NEEDED WAS LARGER THAN THE
C                   NUMBER OF BLOCKS ALLOCATED.  EXECUTION IS
C                   TERMINATED.  INCREASE KD.
C
C         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
C                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
C                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
C                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
C                   RESULTS FOR THAT CLUSTER.
C
C   REFERENCES
C   ----------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 251-271.
C
C     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
C        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMW, DMIWRK, DMWORK, OUNIT
      DIMENSION A(MM,*), W(DMW,*), IWORK1(DMIWRK,*), IWORK(*),
     *           WORK(DMWORK,*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     INTEGER WORK VECTOR OFFSETS
C
      IERR = 0
      IWCLAB=0
      INC1=N
      INC2=N+M
C
C     INITIALIZE CLUSTER OF ALL ROWS
C
      IWORK(INC1+1)=1
      IWORK(INC2+1)=M
      KR=0
      KC=0
   10 KR=KR+1
      IF(KR.EQ.0) GOTO 50
      SP=0.
      IL=IWORK(INC1+KR)
      IU=IWORK(INC2+KR)
C
C     IDENTIFY VARIABLES WITHIN THRESHOLD FOR WITHIN-CLUSTER VARIANCES
C
      DO 40 J=1,N
         IWORK(IWCLAB+J)=1
         S1=0.
         S2=0.
         S3=0.
         DO 20 I=IL,IU
            IF(W(I,J).NE.0.) THEN
               S1=S1+W(I,J)
               S2=S2+W(I,J)*A(I,J)
               S3=S3+W(I,J)*A(I,J)**2
            ENDIF
   20    CONTINUE
         IF(S1.NE.0.) THEN
            S2=S2/S1
            S3=S3/S1-S2**2
            IF(S3.GT.TH) THEN
               SP=1.
               GOTO 40
            ENDIF
            KC=KC+1
            IF (KC .GT. KD) THEN
               IF (OUNIT .GT. 0) THEN
                  WRITE(OUNIT,*)
   22             FORMAT(' TOO MANY BLOCKS FOR SPACE ALLOCATED, ',
     1                   'INCREASE KD AND SECOND DIMENSION OF IWORK1')
                  CALL DPWRST('XXX','WRIT')
               ENDIF
               IERR = 1
               RETURN
            ENDIF
            IWORK1(1,KC)=IL+1
            IWORK1(2,KC)=IU+1
            IWORK1(3,KC)=J+1
            IWORK1(4,KC)=J+1
            DO 30 I=IL,IU
   30          W(I,J)=0.
         ENDIF
         IWORK(IWCLAB+J)=0
   40 CONTINUE
C
C     SPLIT CLUSTER KR IF NECESSARY
C
      IF(SP.EQ.0.) THEN
         KR=KR-2
         GO TO 10
      ENDIF
      CALL SPLIT(MM,M,N,A,CLAB,RLAB,DMW,W,IL,IU,DMWORK,WORK,
     *           IWORK(IWCLAB+1),IM,DM)
      IWORK(INC2+KR+1)=IWORK(INC2+KR)
      IWORK(INC2+KR)=IM
      IWORK(INC1+KR+1)=IM+1
      GO TO 10
  50  CALL BLOCK(MM, M+1, N+1, A, CLAB, RLAB, TITLE, KC, DMIWRK, IWORK1,
     *           IERR, OUNIT)
      RETURN
      END
      SUBROUTINE SPLIT2(MM, M, N, A, CLAB, RLAB, TITLE, KD, TH, IORD,
     *                  DMIWRK, IWORK, DMWORK, WORK, IERR, OUNIT)
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      SPLITS MATRIX OF CASE-BY-VARIABLE DATA VALUES INTO BLOCKS UNTIL
C      ALL WITHIN-BLOCK VARIANCES ARE LESS THAN A GIVEN THRESHOLD.
C      INCLUDES USER-CONTROLLED CONSTRAINTS
C
C   DESCRIPTION
C   -----------
C
C   1.  THE THRESHOLD IS THE LARGEST VARIANCE FOR THE DATA VALUES IN
C       THE BLOCKS.  THE VARIABLES SHOULD BE SCALED SIMILARLY (CLUSTER
C       SUBROUTINE CAN BE USED TO STANDARDIZE THE VARIABLES.  THE
C       ROUTINE STARTS WITH THE DATA MATRIX AS ONE BLOCK.  THEN THE
C       BLOCK WITH THE LARGEST VARIANCE IS CHOSEN AND IF THAT VARIANCE
C       IS LARGER THAN THE THRESHOLD, THE BLOCK IS OPTIMALLY SPLIT BY
C       BOTH CASES AND VARIABLES.  THE VARIANCES FOR THE NEW BLOCKS ARE
C       DETERMINED AND THE PROCESS REPEATS BY FINDING THE NEWEST
C       LARGEST VARIANCE.  ONCE THE LARGEST VARIANCE IS LESS THAN THE
C       THRESHOLD, THE RESULTS ARE PRINTED IN A BLOCK DIAGRAM ON
C       FORTRAN UNIT OUNIT.  THE THRESHOLD SHOULD BE CHOSEN WISELY AS A
C       LARGE THRESHOLD WILL PRODUCE A FEW LARGE BLOCKS AND A SMALL
C       THRESHOLD WILL PRODUCE MANY SMALL BLOCKS.
C
C   2.  MISSING VALUES SHOULD BE REPRESENTED BY 99999.
C
C   3.  THE CASES AND/OR VARIABLES CAN BE CONSTRAINED BY THE IORD
C       PARAMETER.  SETTING IORD = 0 HAS BOTH CASES AND VARIABLES
C       UNCONSTRAINED; SETTING IORD = 1 CONSTRAINS ONLY CASES; SETTING
C       IORD = 2 CONSTRAINS ONLY VARIABLES; AND SETTING IORD = 3
C       CONSTRAINS BOTH CASES AND VARIABLES.
C
C   3.  THE BLOCK DIAGRAM IS THE DATA MATRIX WITH THE DATA VALUES
C       MULTIPLIED BY 10.  THE BLOCKS ARE OUTLINED BY THE VERTICAL AND
C       HORIZONTAL LINES.
C
C   INPUT PARAMETERS
C   ----------------
C
C   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX A.  MUST BE AT LEAST M.
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF OBJECTS.
C
C   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF VARIABLES.
C
C   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
C            DIMENSION MUST BE AT LEAST M (CHANGED ON OUTPUT).
C         THE DATA MATRIX.
C
C         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
C
C   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N
C            (CHANGED ON OUTPUT).
C         ORDERED LABELS OF THE COLUMNS.
C
C   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
C            (CHANGED ON OUTPUT).
C         ORDERED LABELS OF THE ROWS.
C
C   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF DATA SET.
C
C   KD    INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         MAXIMUM NUMBER OF BLOCKS.  SHOULD BE BETWEEN M AND N*M.
C
C   TH    REAL SCALAR (UNCHANGED ON OUTPUT).
C         THRESHOLD VARIANCE FOR DATA VALUES WITHIN A BLOCK.
C
C   IORD  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         ORDERING PARAMETER.
C
C            IORD = 0 CASES AND VARIABLES ARE UNCONSTRAINED
C            IORD = 1 CONSTRAIN CASES
C            IORD = 2 CONSTRAIN VARIABLES
C            IORD = 3 CASES AND VARIABLES ARE CONSTRAINED
C
C   DMIWRK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX IWORK.  MUST BE AT LEAST 4.
C
C   IWORK INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND SECOND
C            DIMENSION MUST BE AT LEAST KC.
C         WORK MATRIX.
C
C   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX WORK.  MUST BE AT LEAST 18.
C
C   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
C            DIMENSION MUST BE AT LEAST MAX(M,N).
C         WORK MATRIX.
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   OUTPUT PARAMETER
C   ----------------
C
C   IERR  INTEGER SCALAR.
C         ERROR FLAG.
C
C         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
C
C         IERR = 1, THE NUMBER OF BLOCKS NEEDED WAS LARGER THAN THE
C                   NUMBER OF BLOCKS ALLOCATED.  EXECUTION IS
C                   TERMINATED.  INCREASE KD.
C
C         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
C                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
C                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
C                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
C                   RESULTS FOR THAT CLUSTER.
C
C   REFERENCES
C   ----------
C
C     HARTIGAN, J. A. (1972) "DIRECT CLUSTERING OF A DATA MATRIX."
C        JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION. VOL. 67,
C        PAGES 123-129.
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 251-277.
C
C     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
C        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMIWRK, DMWORK, OUNIT
      DIMENSION A(MM,*), IWORK(DMIWRK,*), WORK(DMWORK,*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
C
      INCLUDE 'DPCOMC.INC'
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C
C     INITIALIZE BLOCKS AND ROW AND COLUMN CLUSTERS
C
      IERR = 0
      WORK(1,1)=1.
      WORK(2,1)=M
      WORK(10,1)=1.
      WORK(11,1)=N
      KR=1
      KC=1
      KA=1
      IWORK(1,1)=2
      IWORK(2,1)=M+1
      IWORK(3,1)=2
      IWORK(4,1)=N+1
      IR=1
      IC=1
      K=KD
      CALL RSPLIT(MM,M,N,A,RLAB,IR,KA,TH,IORD,DMIWRK,IWORK,DMWORK,WORK)
      CALL CSPLIT(MM,M,N,A,CLAB,IC,KA,TH,IORD,DMIWRK,IWORK,DMWORK,WORK)
   10 IF (KA .GT. KD) THEN
         IF (OUNIT .GT. 0) THEN
            WRITE(ICOUT,1)
    1       FORMAT(' NUMBER OF BLOCKS ALLOCATED IS TOO SMALL. ',
     1             'INCREASE KD')
         ENDIF
         IERR = 1
         RETURN
      ENDIF
C
C     FIND BEST CASE OR VARIABLE SPLIT
C
      IB=1
      XB=0.
      DO 20 I=1,KR
         IF(WORK(3,I).GT.XB) THEN
            XB=WORK(3,I)
            IB=I
         ENDIF
   20 CONTINUE
      DO 30 J=1,KC
         IF(WORK(12,J).GT.XB) THEN
            XB=WORK(12,J)
            IB=J+M
         ENDIF
   30 CONTINUE
      IF(XB.EQ.0.) GOTO 60
C
C     SPLIT CASE CLUSTER
C
      KKC=KA
      IF(IB.LE.M) THEN
         IL=INT(WORK(1,IB))
         IU=INT(WORK(2,IB))
         IM=INT(WORK(4,IB))
         DO 40 K=1,KA
            IF(IWORK(1,K).EQ.IL+1.AND.IWORK(2,K).EQ.IU+1) THEN
               KKC=KKC+1
               IWORK(1,KKC)=IM+2
               IWORK(2,KKC)=IWORK(2,K)
               IWORK(2,K)=IM+1
               IWORK(3,KKC)=IWORK(3,K)
               IWORK(4,KKC)=IWORK(4,K)
            ENDIF
   40    CONTINUE
         KA=KKC
         WORK(2,IB)=IM
         KR=KR+1
         WORK(1,KR)=IM+1
         WORK(2,KR)=IU
         CALL RSPLIT(MM,M,N,A,RLAB,IB,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
     *               WORK)
         CALL RSPLIT(MM,M,N,A,RLAB,KR,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
     *               WORK)
         GO TO 10
      ELSE
C
C    SPLIT VARIABLE CLUSTER
C
         JB=IB-M
         JL=INT(WORK(10,JB))
         JU=INT(WORK(11,JB))
         JM=INT(WORK(13,JB))
         DO 50 K=1,KA
            IF(IWORK(3,K).EQ.JL+1.AND.IWORK(4,K).EQ.JU+1) THEN
               KKC=KKC+1
               IWORK(3,KKC)=JM+2
               IWORK(4,KKC)=IWORK(4,K)
               IWORK(4,K)=JM+1
               IWORK(1,KKC)=IWORK(1,K)
               IWORK(2,KKC)=IWORK(2,K)
            ENDIF
   50    CONTINUE
         KA=KKC
         WORK(11,JB)=JM
         KC=KC+1
         WORK(10,KC)=JM+1
         WORK(11,KC)=JU
         CALL CSPLIT(MM,M,N,A,CLAB,KC,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
     *               WORK)
         CALL CSPLIT(MM,M,N,A,CLAB,JB,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
     *               WORK)
         GO TO 10
      ENDIF
   60 DO 70 K=1,KA
        DO 70 J=1,4
   70      IF(IWORK(J,K).LT.0) IWORK(J,K)=-IWORK(J,K)
      CALL BLOCK(MM,M+1,N+1,A,CLAB,RLAB,TITLE,KA,DMIWRK,IWORK,IERR,
     *           OUNIT)
      RETURN
      END
      SUBROUTINE SLINK(M, DMD, D, DMIWRK, IWORK, WORK)
CCCCC SUBROUTINE SLINK(M, DMD, D, DRLAB, DTITLE, DMIWRK, IWORK, WORK,
CCCCC*                 TLAB, IOUT, IERR, OUNIT)
C
C     2017/04: MODIIFIED FOR DATAPLOT.  SUPPRESS TREE1 AND
C              BLOCK1 ROUTINES (DISPLAYING CLUSTERS WILL BE
C              DONE BY THE CALLING DATAPLOT ROUTINE).
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
C   PURPOSE
C   -------
C
C      UTILIZES THE SINGLE-LINKAGE CLUSTERING ALGORITHM TO CONSTRUCT
C      A TREE FROM A USER-SPECIFIED DISTANCE MATRIX
C
C   DESCRIPTION
C   -----------
C
C   1.  THE ALGORITHM TO COMPUTE SINGLE-LINKAGE TREES IS FOUND ON PAGES
C       191-195 OF THE REFERENCE.  THE DATA MATRIX ARE THE DISTANCES
C       BETWEEN THE CASES.  THE DISTANCES SHOULD BE CALCULATED ON
C       SCALED DATA (CLUSTER SUBROUTINE STAND CAN BE USED TO
C       STANDARDIZE THE VARIABLES).  THE OUTPUT CAN BE THE REGULAR
C       REGULAR TREE OUTPUT OR THE BLOCK REPRESENTATION OF THE TREE AND
C       IS WRITTEN ON FORTRAN UNIT OUNIT.
C
C   2.  THE REGULAR TREE LISTS THE CASES VERTICALLY AND HAS HORIZONTAL
C       LINES EMANATING FROM EACH CASE.  EACH CLUSTER WILL CORRESPOND
C       TO A VERTICAL LINE BETWEEN TWO HORIZONTAL LINES.  THE CASES
C       BETWEEN AND INCLUDED IN THE HORIZONTAL LINES ARE THE MEMBERS OF
C       THE CLUSTER.  THE DISTANCE FROM THE CASE NAMES TO THE VERTICAL
C       LINES CORRESPOND TO THE CLUSTER DIAMETER OR THE DISTANCE
C       BETWEEN THE FIRST AND LAST CASES.
C
C   3.  THE BLOCK DIAGRAM PRINTS THE DISTANCE MATRIX WITH THE CASES
C       LABELING BOTH HORIZONTAL AND VERTICAL AXES.  THE DISTANCES HAVE
C       BEEN MULTIPLIED BY 10.  THE HORIZONTAL BOUNDARIES OF THE BLOCKS
C       ARE REPRESENTED BY DASHES AND THE VERTICAL BOUNDARIES BY QUOTE
C       MARKS.  COMMAS REPRESENT THE CORNERS OF THE BLOCKS.
C
C   INPUT PARAMETERS
C   ----------------
C
C   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE NUMBER OF OBJECTS.
C
C   DMD   INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX D.  MUST BE AT LEAST M.
C
C   D     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMD AND SECOND
C            DIMENSION MUST BE AT LEAST M (CHANGED ON OUTPUT).
C         THE MATRIX OF DISTANCES.  ORDERED ON OUTPUT SUCH THAT ALL
C            CLUSTERS ARE CONTIGUOUS IN THE ORDER.
C
C         D(I,J) = DISTANCE FROM CASE I TO CASE J
C
C   DRLAB VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
C            (CHANGED ON OUTPUT).
C         LABELS OF THE CASES.  ORDERED ON OUTPUT.
C
C   DTITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
C         TITLE OF THE DATA SET.
C
C   DMIWRK INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         THE LEADING DIMENSION OF MATRIX IWORK.  MUST BE AT LEAST 4.
C
C   IWORK INTEGER VECTOR WHOSE FIRST DIMENSION MUST BE AT DMIWRK AND
C            WHOSE SECOND DIMENSION MUST BE AT LEAST M+1.
C         WORK VECTOR.
C
C   WORK  REAL VECTOR DIMENSIONED AT LEAST M+1.
C         WORK VECTOR.
C
C   TLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M+1
C         WORK VECTOR.
C
C         IF THE REGULAR TREE DIAGRAM IS NOT CHOSEN, TLAB CAN HAVE
C            LENGTH 1.
C
C   IOUT  INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         OPTION FOR CHOOSING FORM OF OUTPUT.  IOUT HAS THE DECIMAL
C           EXPANSION AB SUCH THAT IF
C
C              A .NE. 0  THE REGULAR TREE WILL BE PRINTED
C              B .NE. 0  THE BLOCKED TREE WILL BE PRINTED
C
C   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
C         UNIT NUMBER FOR OUTPUT.
C
C   OUTPUT PARAMETER
C   ----------------
C
C   IERR  INTEGER SCALAR.
C         ERROR FLAG.
C
C         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
C
C         IERR = 1, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
C                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
C                   CLUSTER AND ITS VALUES ARE PRINTED ON UNIT OUNIT.
C                   EXECUTION WILL CONTINUE WITH QUESTIONABLE RESULTS
C                   FOR THAT CLUSTER.  ERROR FLAG SET IN THE REGULAR
C                   TREE OUTPUT ROUTINE.
C
C         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
C                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
C                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
C                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
C                   RESULTS FOR THAT CLUSTER.  ERROR FLAG SET IN THE
C                   BLOCK TREE OUTPUT ROUTINE.
C
C   REFERENCES
C   ----------
C
C     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
C        SONS, INC., NEW YORK.  PAGES 191-215.
C
C     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
C        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
C
C<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
C
      INTEGER DMIWRK, DMD, OUNIT
      DIMENSION D(DMD,*), WORK(*), IWORK(DMIWRK,*)
CCCCC CHARACTER*4 DRLAB(*), TLAB(*), CTEMP
CCCCC CHARACTER*10 DTITLE
C
      INCLUDE 'DPCOMC.INC'
C
      IERR = 0
      DO 10 I = 1 , M
         IWORK(4,I) = I
   10 CONTINUE
      D(1,1)=R1MACH(2)
C
C     FIND THE OBJECT CLOSEST TO THE FIRST OBJECT
C
      DO 20 K=2 , M
         IF(D(1,K).LT.D(1,1)) THEN
            D(1,1)=D(1,K)
            IWORK(4,1)=K
         ENDIF
   20 CONTINUE
C
C     SET UP THE CLUSTERS
C
      DO 90 NEXT = 1,M-1
         J = NEXT + 1
         DMIN=R1MACH(2)
         IMIN=NEXT
C
C     FIND THE SMALLEST OF THE SMALLEST DISTANCES SO FAR COMPUTED
C
         DO 30 I=1,NEXT
            IF(D(I,I).LT.DMIN) THEN
               DMIN=D(I,I)
               IMIN=I
            ENDIF
   30    CONTINUE
         WORK(J+1)=100.*DMIN
         I=IWORK(4,IMIN)
C
C     PLACE THE OBJECT JUST DETERMINED IN THE NEXT POSITION BY
C     EXCHANGING IT WITH THE ONE CURRENTLY THERE
C
         DO 40 K=1,M
            A=D(I,K)
            D(I,K)=D(J,K)
            D(J,K)=A
   40    CONTINUE
CNIST    CTEMP = DRLAB(I)
CNIST    DRLAB(I)= DRLAB(J)
CNIST    DRLAB(J) = CTEMP
         DO 50 K=1,M
            A=D(K,I)
            D(K,I)=D(K,J)
            D(K,J)=A
   50    CONTINUE
         ITEMP = IWORK(4,I)
         IWORK(4,I) = IWORK(4,J)
         IWORK(4,J) = ITEMP
         DO 60 K=1,NEXT
            IF(IWORK(4,K).EQ.I) IWORK(4,K)=1
            IF(IWORK(4,K).EQ.J) IWORK(4,K)=I
   60    CONTINUE
C
C     UPDATE THE SMALLEST DISTANCES
C
         DO 80 I=1,J
            IWORK(4,J)=J
            IF(IWORK(4,I).LE.J) THEN
               IWORK(4,I)=I
               D(I,I)=R1MACH(2)
               DO 70 K=J,M
                  IF(K.NE.J.AND.D(I,K).LT.D(I,I)) THEN
                     D(I,I)=D(I,K)
                     IWORK(4,I)=K
                  ENDIF
   70          CONTINUE
            ENDIF
   80    CONTINUE
   90 CONTINUE
C
C     FIND BOUNDARIES OF CLUSTERS
C
      WORK(2)=R1MACH(2)
      M1 = M + 1
      DO 140 K=2,M1
         IWORK(1,K)=K
         IWORK(2,K)=K
         DO 100 L=K,M1
            IF(L.NE.K) THEN
               IF(WORK(L).GT.WORK(K)) GO TO 110
            ENDIF
            IWORK(2,K)=L
  100    CONTINUE
  110    CONTINUE
         DO 120 L=2,K
            LL=K-L+2
            IF(L.NE.2) THEN
               IF(WORK(LL).GT.WORK(K)) GO TO 130
            ENDIF
  120    CONTINUE
  130    IWORK(1,K)=LL
  140 CONTINUE
      MM2=M-1
      DO 160 K=1,MM2
         DO 150 L=1,2
            IWORK(L,K)=IWORK(L,K+2)
  150    CONTINUE
         WORK(K)=WORK(K+2)
  160 CONTINUE
C
C     SCALE CLUSTER DIAMETERS BETWEEN 1 AND 100
C
      XMAX = 0.
      DO 170 K=1,MM2
         IF(XMAX.LT.WORK(K)) XMAX=WORK(K)
  170 CONTINUE
      DO 180 K=1,MM2
         IWORK(3,K)=INT((WORK(K)*100)/XMAX)
  180 CONTINUE
C
C     REORDER DISTANCE MATRIX
C
      DO 190 I=1,M
         D(I,I)=0.
  190 CONTINUE
C
C     PRODUCE OUTPUT
C
CNIST IA = IOUT / 10
CNIST IB = MOD(IOUT,10)
CNIST IF (IA .NE. 0) THEN
CNIST    IF (OUNIT .GT. 0) WRITE(OUNIT,1)
CNIS1    FORMAT('1')
CNIST    TLAB(1) = DTITLE
CNIST    DO 200 I = 1 , M
CN200       TLAB(I+1) = DRLAB(I)
CNIST    CALL TREE1(M+1,M-1,DMIWRK,IWORK,TLAB,IERR,OUNIT)
CNIST ENDIF
CNIST IF (IB .NE. 0) THEN
CNIST    DO 210 K = 1, M-1
CNIST       IWORK(3,K) = IWORK(1,K)
CNIST       IWORK(4,K) = IWORK(2,K)
C210     CONTINUE
CNIST    CALL BLOCK(DMD,M+1,M+1,D,DRLAB,DRLAB,DTITLE,M-1,DMIWRK,IWORK,
CNIST*              IERR,OUNIT)
CNIST ENDIF
      RETURN
      END
      SUBROUTINE BSWAP(KK,NSAM,NREPR,DYSMA,DYSMB,BETER,DYS,SKY,S,IFLAG,
     1                 LARGE,ISUBRO,IBUGA3)
CNIST SUBROUTINE BSWAP(KK,NSAM,NREPR,DYS,SKY,S,LUB) 
C
C     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
C
C        1. USE DATAPLOT OUTPUT
C        2. RECODE A BIT FOR BETTER READABILITY
C        3. ADD TEMPORARY ARRAYS TO CALL LIST
C
C     NOTE THAT WE CAN USE THIS ROUTINE FOR BOTH "CLARA" AND
C     "PAM".  JUST NEED TO ADD TEMPORARY ARRAYS TO CALL LIST.
C     PAM USES "NN" RATHER THAN "NSAMP", BUT THIS CAN BE TAKEN
C     CARE OF IN THE CALLING ROUTINE.  ALSO, THE FEEDBACK
C     MESSAGE IS SLIGHTLY DIFFERENT FOR PAM, SO ADD A FLAG
C     TO SPECIFY WHETHER BEING CALLED FROM CLARA OR PAM.
C
C     KK        = NUMBER OF CLUSTERS
C     NSAM      = NUMBER OF SAMPLES
C     NREPR     = 
C     DYSMA     = 
C     DYSMB     = 
C     BETER     = 
C     DYS       = THE OUTPUT MATRIX CONTAINING THE DISTANCES
C     SKY       = SUM OF DISSIMILARITIES/DISTANCES
C     S         = 
C     LUB       = OUTPUT UNIT (DON'T USE)
C
      DIMENSION NREPR(*)
      DIMENSION DYS(*)
      DIMENSION DYSMA(*)
      DIMENSION DYSMB(*)
      DIMENSION BETER(*)
C
      INTEGER  MEET
      EXTERNAL MEET
C
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** AT THE BEGINNING OF BSWAP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)KK,NSAM,S,IFLAG
   15   FORMAT('KK,NSAM,S,IFLAG  = ',2I8,G15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CC
CC   FIRST ALGORITHM: BUILD.
CC
      NNY=0 
      DO 17 J=1,NSAM
        NREPR(J)=0
        DYSMA(J)=1.1*S+1.0
   17 CONTINUE
C
   20 CONTINUE
      DO 22 JA=1,NSAM 
        IF(NREPR(JA).NE.0)GO TO 22
        BETER(JA)=0.
        DO 21 J=1,NSAM
          NJAJ=MEET(JA,J) 
          CMD=DYSMA(J)-DYS(NJAJ)
          IF(CMD.GT.0.0)BETER(JA)=BETER(JA)+CMD 
   21   CONTINUE
   22 CONTINUE
      AMMAX=0.
      DO 31 JA=1,NSAM
        IF(NREPR(JA).NE.0)GO TO 31
        IF(BETER(JA).LT.AMMAX)GO TO 31
        AMMAX=BETER(JA) 
        NMAX=JA 
   31 CONTINUE
      NREPR(NMAX)=1 
      NNY=NNY+1 
      DO 41 J=1,NSAM
        NJN=MEET(NMAX,J)
        IF(DYS(NJN).LT.DYSMA(J))DYSMA(J)=DYS(NJN) 
   41 CONTINUE
C
      IF(NNY.NE.KK)GO TO 20 
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,9011)NNY
 9011   FORMAT('***** BEFORE 51 LOOP--, NNY = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO9020II=1,NSAM
          WRITE(ICOUT,9025)II,NREPR(II)
 9025     FORMAT('II,NREPR(II) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
C
      SKY=0.
      DO 51 J=1,NSAM
        SKY=SKY+DYSMA(J)
   51 CONTINUE
      IF(KK.EQ.1)GOTO9090
      RSAM=NSAM 
      ASKY=SKY/RSAM 
CNIST WRITE(LUB,9100)ASKY 
C9100 FORMAT(1X/33H  RESULT OF BUILD FOR THIS SAMPLE/2X,
CNISTF '  AVERAGE DISTANCE  =   ',F12.3)
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      IF(IFLAG.EQ.'CLAR')THEN
        IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
          WRITE(ICOUT,9100)
 9100     FORMAT('RESULT OF BUILD FOR THIS SAMPLE')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9101)ASKY
 9101     FORMAT('  AVERAGE DISTANCE =  ',F12.5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ELSE
        IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
          WRITE(ICOUT,9110)
 9110     FORMAT('RESULT OF BUILD')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9111)ASKY
 9111     FORMAT('  AVERAGE DISSIMILARITY =  ',F12.5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
CC
CC   SECOND ALGORITHM: SWAP.
CC
   60 CONTINUE
C
      DO 63 J=1,NSAM
        DYSMA(J)=1.1*S+1.0
        DYSMB(J)=1.1*S+1.0
        DO 62 JA=1,NSAM 
          IF(NREPR(JA).EQ.0)GO TO 62
          NJAJ=MEET(JA,J) 
          IF(DYS(NJAJ).GE.DYSMA(J))THEN
            IF(DYS(NJAJ).GE.DYSMB(J))GO TO 62 
            DYSMB(J)=DYS(NJAJ)
          ELSE
            DYSMB(J)=DYSMA(J) 
            DYSMA(J)=DYS(NJAJ)
          ENDIF
   62   CONTINUE
   63 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** AFTER 63 LOOP--')
        CALL DPWRST('XXX','BUG ')
        DO116I=1,NSAM
          WRITE(ICOUT,117)I,DYS(I),DYSMA(I),DYSMB(I)
  117     FORMAT('I,DYS(I),DYSMA(I),DYSMB(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
  116   CONTINUE
      ENDIF
C
      DZSKY=1.0 
      DO 73 K=1,NSAM
        IF(NREPR(K).EQ.1)GO TO 73 
C
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
          WRITE(ICOUT,171)K,NREPR(K),DZ,DZSKY
  171     FORMAT('K,NREPR(K),DZ,DZSKY = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO 72 JA=1,NSAM 
          IF(NREPR(JA).EQ.0)GO TO 72
          DZ=0. 
          DO 71 J=1,NSAM
            NJAJ=MEET(JA,J) 
            NKJ=MEET(K,J) 
            IF(DYS(NJAJ).NE.DYSMA(J))THEN
              IF(DYS(NKJ).LT.DYSMA(J))DZ=DZ-DYSMA(J)+DYS(NKJ)
            ELSE
              SMALL=DYSMB(J)
              IF(DYS(NJAJ).LT.SMALL)SMALL=DYS(NKJ)
              DZ=DZ-DYSMA(J)+SMALL
            ENDIF
   71     CONTINUE
          IF(DZ.GE.DZSKY)GO TO 72
          DZSKY=DZ
          KBEST=K
          NBEST=JA
   72   CONTINUE
   73 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,9017)DZSKY
 9017   FORMAT('***** AFTER 73 LOOP, DZSKY = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(DZSKY.GE.0.0)GOTO9090
      NREPR(KBEST)=1
      NREPR(NBEST)=0
      SKY=SKY+DZSKY 
      GO TO 60
 9090 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,9001)
 9001   FORMAT('***** AT THE END OF BSWAP--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DYSTA(NSAM,JPP,NSEL,X,DYS,NDYST,AMISS,JHALT,
     1                 ISUBRO,IBUGA3)
CNIST SUBROUTINE DYSTA(NSAM,JPP,NSEL,X,MAXXX,MAXTT,DYS,NDYST,JTMD,
CNIST1 VALMD,JHALT,LUB,FNAMEB)
C
C     KAUFFMAN AND ROUSSEEUW CODE FROM CLARA ALGORITHM.  THIS
C     ROUTINE COMPUTES EITHER EUCLIDEAN DISTANCE OR MANHATTAN
C     DISTANCE BETWEEN ALL OBJECTS OF A SAMPLE (CLARA VERSION)
C
C       NSAM    = NUMBER OF SAMPLES
C       JPP     = NUMBER OF VARIABLES
C       NSEL    = INTEGER ARRAY CONTAINING OBJECTS SELECTED
C       X       = THE DATA MATRIX
C       MAXXX   = THE MAXIMUM OF ROWS TIMES COLUMNS, WE DON'T USE
C       MAXTT   = THE MAXIMUM OF VARIABLES (COLUMNS), WE DON'T USE
C       DYS     = THE OUTPUT MATRIX CONTAINING THE DISTANCES
C       NDYST   = 1 => EUCLIDEAN DISTANCES
C                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
C       JHALT   = SET TO 1 FOR ERROR CONDITION
C       JTMD    = FOR MISSING VALUES, WE DON'T USE
C       VALMD   = FOR MISSING VALUES, WE DON'T USE
C       LUB     = OUTPUT UNIT
C                 (WE USE DATAPLOT OUTPUT STRUCTURE, SO
C                 REMOVE THIS)
C       FNAMEB  = OUTPUT FILE NAME
C                 (WE USE DATAPLOT OUTPUT STRUCTURE, SO
C                 REMOVE THIS)
C
C     CHANGES FOR INCORPORATING INTO DATAPLOT:
C
C        1. USE DATAPLOT I/O ROUTINES
C        2. FOR DATAPLOT, ONLY USE A SINGLE VALUE TO DENOTE
C           MISSING VALUES
C        3. RECODED SLIGHTLY TO REDUCE USE OF GO TO's (THIS
C           WAS JUST TO IMPROVE READABILITY OF THE CODE)
C
CNIST DIMENSION X(MAXXX),DYS(4951)
CNIST DIMENSION NSEL(100),JTMD(MAXTT),VALMD(MAXTT)
CNIST CHARACTER*30 FNAMEB
C
      DIMENSION X(*)
      DIMENSION DYS(*)
      DIMENSION NSEL(*)
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DYSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NSAM,JPP,AMISS
   55   FORMAT('NSAM,JPP,AMISS  = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO56I=1,NSAM
          WRITE(ICOUT,57)I,NSEL(I)
   57     FORMAT('I,NSEL(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
        DO58I=1,JPP
          WRITE(ICOUT,59)I,X(I)
   59     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   58   CONTINUE
      ENDIF
C
      JHALT=0
      PP=JPP
      NLK=1 
      DYS(1)=0.0
      DO 100 L=2,NSAM 
         LSUBT=L-1 
         LSEL=NSEL(L)
         DO 20 K=1,LSUBT 
            KSEL=NSEL(K)
            CLK=0.0 
            NLK=NLK+1 
            NPRES=0 
            DO 30 J=1,JPP 
               NUMLJ=(LSEL-1)*JPP+J
               NUMKJ=(KSEL-1)*JPP+J
CNIST          IF(JTMD(J).GE.0)GO TO 40
CNIST          IF(X(NUMLJ).EQ.VALMD(J))GO TO 30
CNIST          IF(X(NUMKJ).EQ.VALMD(J))GO TO 30
               IF(X(NUMLJ).EQ.AMISS)GO TO 30
               IF(X(NUMKJ).EQ.AMISS)GO TO 30
CNI40          CONTINUE
               NPRES=NPRES+1 
               IF(NDYST.NE.1)THEN
                 CLK=CLK+ABS(X(NUMLJ)-X(NUMKJ))
               ELSE
                 CLK=CLK+(X(NUMLJ)-X(NUMKJ))*(X(NUMLJ)-X(NUMKJ))
               ENDIF
   30       CONTINUE
            RPRES=NPRES 
            IF(NPRES.EQ.0)THEN
              JHALT=1 
CNIST         WRITE(LUB,9400)LSEL,KSEL
C9400 FORMAT(1X,8H OBJECTS,I8,4H AND,I8,23H HAVE NO COMMON MEASURE, 
CNISTF6HMENTS,/49H  SO THE DISTANCE BETWEEN THEM CANNOT BE COMPUTED)
CNIST         IF(FNAMEB.NE.'CON')WRITE(*,9400)LSEL,KSEL
              WRITE(ICOUT,999)
  999         FORMAT(1X)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9401)LSEL,KSEL
 9401         FORMAT('***** OBJECTS ',I8,' AND ',I8,' HAVE NO ',
     1               'COMMON MEASURE, SO')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9403)
 9403         FORMAT('      THE DISTANCE BETWEEN THEM CANNOT BE ',
     1               'COMPUTED.')
              CALL DPWRST('XXX','BUG ')
              DYS(NLK)=0.0
              GO TO 20
            ENDIF
            IF(NDYST.EQ.1)THEN
              DYS(NLK)=SQRT(CLK*(PP/RPRES)) 
            ELSE
              DYS(NLK)=CLK*(PP/RPRES) 
            ENDIF
   20    CONTINUE
  100 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DYSTA--')
        CALL DPWRST('XXX','BUG ')
        DO9056I=1,NLK
          WRITE(ICOUT,9057)I,DYS(I)
 9057     FORMAT('I,DYS(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9056   CONTINUE
      ENDIF
C
      RETURN
      END
      FUNCTION MEET(L,J)
      IF(L.GT.J)THEN
CC
CC      J LESS THAN L
CC
        MEET=(L-2)*(L-1)/2+J+1
      ELSEIF(L.EQ.J)THEN
CC
CC      J EQUALS L
CC
        MEET=1
      ELSE
CC
CC      L LESS THAN J
CC
        MEET=(J-2)*(J-1)/2+L+1
      ENDIF
C
      RETURN
      END 
      SUBROUTINE DYSTAP(NN,JPP,MAXNN,MAXPP,X,DYS,NDYST,AMISS,JHALT,
     1                 ISUBRO,IBUGA3)
C
CNIST SUBROUTINE DYSTA(NN,JPP,MAXNN,MAXPP,MAXHH,X,DYS,NDYST,JTMD,
CNIST1 VALMD,LAB,JHALT,LUB,FNAMEB)
C
C     KAUFFMAN AND ROUSSEEUW CODE FROM PAM ALGORITHM.  THIS
C     ROUTINE COMPUTES EITHER EUCLIDEAN DISTANCE OR MANHATTAN
C     DISTANCE BETWEEN ALL OBJECTS OF A SAMPLE (CLARA VERSION)
C
C       NN      = NUMBER OF SAMPLES
C       JPP     = NUMBER OF VARIABLES
C       MAXN    = THE ROW DIMENSION OF X
C       MAXPP   = THE COLUMN DIMENSION OF X
C       MAXHH   = THE MAXIMUM DIMENSION FOR THE DISTANCES
C                 (DATAPLOT DOES NOT USE)
C       X       = THE DATA MATRIX
C       DYS     = THE OUTPUT MATRIX CONTAINING THE DISTANCES
C       NDYST   = 1 => EUCLIDEAN DISTANCES
C                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
C       JTMD    = FOR MISSING VALUES, WE DON'T USE
C       VALMD   = FOR MISSING VALUES, WE DON'T USE
C       LAB     = ...
C       JHALT   = SET TO 1 FOR ERROR CONDITION
C       LUB     = OUTPUT UNIT
C                 (DATAPLOT DOES NOT USE)
C       FNAMEB  = OUTPUT FILE NAME
C                 (DATAPLOT DOES NOT USE)
C
C     CHANGES FOR INCORPORATING INTO DATAPLOT:
C
C        1. USE DATAPLOT I/O ROUTINES
C        2. FOR DATAPLOT, ONLY USE A SINGLE VALUE TO DENOTE
C           MISSING VALUES
C        3. RECODED SLIGHTLY TO REDUCE USE OF GO TO's (THIS
C           WAS JUST TO IMPROVE READABILITY OF THE CODE)
C
CNIST DIMENSION X(MAXNN,MAXPP),DYS(MAXHH),JTMD(MAXPP),VALMD(MAXPP)
CNIST CHARACTER LAB(3,MAXNN)
CNIST CHARACTER*30 FNAMEB
      DIMENSION X(MAXNN,MAXPP)
      DIMENSION DYS(*)
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DYSTAP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NN,JPP,AMISS
   55   FORMAT('NN,JPP,AMISS  = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO58I=1,NN
          WRITE(ICOUT,59)I,(X(I,J),J=1,JPP)
   59     FORMAT('I,X(I,J) = ',I8,30G15.7)
          CALL DPWRST('XXX','BUG ')
   58   CONTINUE
      ENDIF
C
      JHALT=0
      PP=JPP
      NLK=1 
      DYS(1)=0.0
      DO 100 L=2,NN 
        LSUBT=L-1 
        DO 20 K=1,LSUBT 
          CLK=0.0 
          NLK=NLK+1 
          NPRES=0 
          DO 30 J=1,JPP 
CNIST       IF(JTMD(J).GE.0)GOTO 40 
CNIST       IF(X(L,J).EQ.VALMD(J))GOTO 30 
CNIST       IF(X(K,J).EQ.VALMD(J))GOTO 30 
            IF(X(L,J).EQ.AMISS)GOTO30
            IF(X(K,J).EQ.AMISS)GOTO30
CNI40       CONTINUE
            NPRES=NPRES+1 
            IF(NDYST.NE.1)THEN
              CLK=CLK+ABS(X(L,J)-X(K,J))
            ELSE
              CLK=CLK+(X(L,J)-X(K,J))*(X(L,J)-X(K,J)) 
            ENDIF
   30     CONTINUE
          RPRES=NPRES 
          IF(NPRES.EQ.0)THEN
            JHALT=1
CNIST       WRITE(LUB,9400)LAB(1,L),LAB(2,L),LAB(3,L),LAB(1,K),LAB(2,K)
CNIST1                     ,LAB(3,K)
C9400       FORMAT('  OBJECTS ',3A1,' AND ',3A1,
CNIST1             ' HAVE NO COMMON MEASUREMENTS.')
CNIST       IF(FNAMEB.NE.'CON')WRITE(*,9400)LAB(1,L),LAB(2,L),LAB(3,L),
CNIST1                                      LAB(1,K),LAB(2,K),LAB(3,K)
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9401)L,K
 9401       FORMAT('***** OBJECTS ',I8,' AND ',I8,' HAVE NO ',
     1             'COMMON MEASURE, SO')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9403)
 9403       FORMAT('      THE DISTANCE BETWEEN THEM CANNOT BE ',
     1             'COMPUTED.')
            CALL DPWRST('XXX','BUG ')
            DYS(NLK)=0.0
            GOTO 20 
          ENDIF
          IF(NDYST.EQ.1)THEN
            DYS(NLK)=SQRT(CLK*(PP/RPRES)) 
          ELSE
            DYS(NLK)=CLK*(PP/RPRES) 
          ENDIF
   20   CONTINUE
  100 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'STAP')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DYSTA--')
        CALL DPWRST('XXX','BUG ')
        DO9056I=1,NLK
          WRITE(ICOUT,9057)I,DYS(I)
 9057     FORMAT('I,DYS(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9056   CONTINUE
      ENDIF
C
      RETURN
      END 
      SUBROUTINE SELEC(KK,NN,JPP,NSTAN,NDYST,ZB,NSAM,MDATA,
     1                 AMISS,NREPR,NSEL,DYS,X,NR,NAFS,
     1                 TTD,RADUS,RATT,
     1                 TTNEW,RDNEW,
     1                 NRNEW,NSNEW,NPNEW,NS,NP,NEW,
     1                 LARGE,ISUBRO,IBUGA3)
C
C     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
C
C        1. USE DATAPLOT OUTPUT
C        2. RECODE A BIT FOR BETTER READABILITY
C
C     KK        = NUMBER OF CLUSTERS
C     NN        = NUMBER OF ROWS (CASES)
C     JPP       = NUMBER OF COLUMNS (VARIABLES)
C     NSTAN     = 0 => NO STANDARDIZATION APPLIED
C                 1 =>    STANDARDIZATION APPLIED
C     NDYST     = 1 => EUCLIDEAN DISTANCES
C                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
C     ZB        = TOTAL DISTANCE
C     NSAM      = NUMBER OF SAMPLES
C     MDATA     = 0 => NO MISSING DATA
C                 1 => THERE IS MISSING DATA
C     AMISS     = NUMBER DENOTING A MISSING VALUE
C     NREPR     = INTEGER ARRAY
C                    0 => NOT A REPRESENTATIVE OBJECT
C                    1 => IS  A REPRESENTATIVE OBJECT
C     NSEL      = INTEGER ARRAY CONTAINING OBJECTS SELECTED
C     DYS       = THE OUTPUT MATRIX CONTAINING THE DISTANCES
C     X         = THE DATA MATRIX
C     MAXTT     = THE MAXIMUM OF ROWS TIMES COLUMNS
C     MAXXX     = THE MAXIMUM NUBER OF VARIABLES (COLUMNS)
C     NR        =
C     NAFS      =
C     TTD       = AVERAGE DISTANCE TO EACH MEDOID
C     RADUS     = MAXIMUM DISTANCE TO EACH MEDOID
C     RATT      = MAXIMUM DISTANCE OF MEDOID DIVIDED BY MINIMUM
C                 DISTANCE TO ANOTHER MEDOID
C
CNIST SUBROUTINE SELEC(KK,NN,JPP,NSTAN,NDYST,ZB,NSAM,LUB,MDATA,
CNISTF JTMD,VALMD,NREPR,NSEL,DYS,X,MAXXX,MAXTT,NR,NAFS,
CNISTF TTD,RADUS,RATT)
CNIST DIMENSION NREPR(100),NSEL(100),DYS(4951),X(MAXXX),NEW(30)
CNIST DIMENSION NRNEW(30),NSNEW(30),NPNEW(30),TTNEW(30),RDNEW(30)
CNIST DIMENSION NS(30),NR(30),NP(30),TTD(30),RADUS(30),RATT(30)
CNIST DIMENSION JTMD(MAXTT),VALMD(MAXTT)
      PARAMETER (MAXCLU=30)
C
      DIMENSION DYS(*)
      DIMENSION X(*)
      DIMENSION TTD(*)
      DIMENSION RADUS(*)
      DIMENSION RATT(*)
      DIMENSION NREPR(*)
      DIMENSION NSEL(*)
      DIMENSION NR(*)
C
      DIMENSION NRNEW(*)
      DIMENSION NSNEW(*)
      DIMENSION NPNEW(*)
      DIMENSION TTNEW(*)
      DIMENSION RDNEW(*)
      DIMENSION NS(*)
      DIMENSION NP(*)
      DIMENSION NEW(*)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ELEC')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF SELEC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)KK,NN,JPP,NSTAN,NDYST,NSAM
   55   FORMAT('KK,NN,JPP,NSTAN,NDYST,NSAM  = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
CC
CC   NAFS = 1 IF A DISTANCE CANNOT BE CALCULATED
CC
      NAFS=0
CC
CC    IDENTIFICATION OF REPRESENTATIVE OBJECTS, AND INITIALIZATIONS
CC
      JK=0
      DO 10 J=1,NSAM
        IF(NREPR(J).EQ.0)GO TO 10 
        JK=JK+1 
        NR(JK)=NSEL(J)
        NS(JK)=0
        TTD(JK)=0.
        RADUS(JK)=-1.
        NP(JK)=J
   10 CONTINUE
CC
CC   ASSIGNMENT OF THE OBJECTS OF THE ENTIRE DATA SET TO A CLUSTER, 
CC   COMPUTATION OF SOME STATISTICS, DETERMINATION OF THE 
CC   NEW ORDERING OF THE CLUSTERS 
CC
      ZB=0. 
      PP=JPP
      NEWF=0
      JN=0
   15 CONTINUE
      JN=JN+1 
      IF(MDATA.NE.0)THEN
        PRES=0. 
        DO 70 JK=1,KK 
          DSUM=0. 
          NRJK=NR(JK) 
          ABC=0.
          DO 50 JP=1,JPP
            NA=(NRJK-1)*JPP+JP
            NB=(JN-1)*JPP+JP
            IF(X(NA).NE.AMISS .AND. X(NB).NE.AMISS)THEN
              ABC=ABC+1.
              TRA=ABS(X(NA)-X(NB))
              IF(NDYST.EQ.1)TRA=TRA*TRA
              DSUM=DSUM+TRA
            ENDIF
   50     CONTINUE
          IF(ABC.LT.0.5)GO TO 70
          DSUM=DSUM*ABC/PP
          IF(PRES.GT.0.5)THEN
            IF(DSUM.GE.DNULL)GO TO 70
          ELSE
            PRES=1.
          ENDIF
          DNULL=DSUM
          JKABC=JK
   70   CONTINUE
        IF(PRES.GT.0.5)GO TO 80 
CNIST   WRITE(LUB,9000)JN 
C9000   FORMAT('  OBJECT',I5,37H DOESNT HAVE COMMON MEASUREMENTS WITH,
CNISTF         53H ANY OF THE MEDOIDS AND THEREFORE CANNOT BE ASSIGNED.) 
CNIST   WRITE(LUB,9002)
C9002   FORMAT(1X,' THIS SAMPLE IS NOT CONSIDERED ANY FURTHER')
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9000)JN
 9000   FORMAT('****** OBJECT ',I5,' DOES NOT HAVE COMMON MEASUREMENTS',
     1         'WITH ANY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)
 9001   FORMAT('       OF THE MEDOIDS AND THEREFORE CANNOT BE ',
     1         'ASSIGNED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002)
 9002   FORMAT('       THIS SAMPLE IS NOT CONSIDERED ANY FURTHER.')
        CALL DPWRST('XXX','BUG ')
        NAFS=1
        GOTO9090
      ENDIF
C
      DO 30 JK=1,KK 
        DSUM=0. 
        NRJK=NR(JK) 
        DO 20 JP=1,JPP
          NA=(NRJK-1)*JPP+JP
          NB=(JN-1)*JPP+JP
          TRA=ABS(X(NA)-X(NB))
          IF(NDYST.EQ.1)TRA=TRA*TRA
          DSUM=DSUM+TRA
   20   CONTINUE
        IF(JK.EQ.1 .OR. DSUM.LT.DNULL)THEN
          DNULL=DSUM
          JKABC=JK
        ENDIF
   30 CONTINUE
C
   80 CONTINUE
      IF(NDYST.EQ.1)DNULL=SQRT(DNULL)
      ZB=ZB+DNULL 
      TTD(JKABC)=TTD(JKABC)+DNULL 
      IF(DNULL.GT.RADUS(JKABC))RADUS(JKABC)=DNULL 
      NS(JKABC)=NS(JKABC)+1
      IF(NEWF.GE.KK)GO TO 90
      IF(NEWF.GE.1)THEN
        DO 82 JNEW=1,NEWF 
          IF(JKABC.EQ.NEW(JNEW))GO TO 90
   82   CONTINUE
      ENDIF
      NEWF=NEWF+1 
      NEW(NEWF)=JKABC 
   90 CONTINUE
      IF(JN.LT.NN)GO TO 15
CC
CC    A PERMUTATION IS CARRIED OUT ON VECTORS NR,NS,NP,TTD,RADUS 
CC    USING THE INFORMATION IN VECTOR NEW. 
CC
      DO 92 JK=1,KK 
        NJK=NEW(JK) 
        NRNEW(JK)=NR(NJK) 
        NSNEW(JK)=NS(NJK) 
        NPNEW(JK)=NP(NJK) 
        TTNEW(JK)=TTD(NJK)
        RDNEW(JK)=RADUS(NJK)
   92 CONTINUE
      DO 94 JK=1,KK 
        NR(JK)=NRNEW(JK)
        NS(JK)=NSNEW(JK)
        NP(JK)=NPNEW(JK)
        TTD(JK)=TTNEW(JK) 
        RADUS(JK)=RDNEW(JK) 
   94 CONTINUE
CC
CC   PRINTING OF RESULTS FOR ENTIRE DATA SET
CC
      RNN=NN
      ZM=ZB/RNN 
CNIST WRITE(LUB,9010)ZB,ZM
C9010 FORMAT(33H  RESULTS FOR THE ENTIRE DATA SET/3X, 
CNISTF       20H TOTAL DISTANCE    =,F15.3/3X,20H AVERAGE DISTANCE  =,F15.3) 
C
      IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
        WRITE(ICOUT,9010)
 9010   FORMAT('RESULTS FOR THE ENTIRE DATA SET')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)ZB
 9011   FORMAT('  TOTAL DISTANCE    = ',F15.3)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)ZM
 9012   FORMAT('  AVERAGE DISTANCE  = ',F15.3)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NSTAN.EQ.0)THEN
CNIST     WRITE(LUB,9020) 
C9020     FORMAT(/46H  CLUSTER SIZE MEDOID    COORDINATES OF MEDOID)
          WRITE(ICOUT,9020)
 9020     FORMAT('  CLUSTER SIZE MEDOID    COORDINATES OF MEDOID')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NSTAN.NE.0)THEN
CNIST     WRITE(LUB,9025) 
C9025     FORMAT(/46H  CLUSTER SIZE MEDOID    COORDINATES OF MEDOID,
CNISTF           28H (STANDARDIZED MEASUREMENTS))
          WRITE(ICOUT,9025)
 9025     FORMAT('  CLUSTER SIZE MEDOID   COORDINATES OF MEDOID ',
     1           ' (STANDARDIZED MEASUREMENTS)')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
      DO 100  JK=1,KK 
        JKA=(NR(JK)-1)*JPP+1
        JKB=JKA-1+JPP 
        IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
CNIST     WRITE(LUB,9030)JK,NS(JK),NR(JK),(X(J),J=JKA,JKB)
C9030     FORMAT(/1X,I8,I5,I7,2X,5F11.2,20(/23X,5F11.2))
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          JSTOP=JKA+4
          IF(JSTOP.GT.JKB)JSTOP=JKB
          WRITE(ICOUT,9030)JK,NS(JK),NR(JK),(X(J),J=JKA,JSTOP)
 9030     FORMAT(1X,I8,I5,I7,2X,5F11.2)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(JKB.GT.JKA+4)THEN
          JSTRT=JKA+5
          NTEMP=JKB - JSTRT + 1
          NLOOP=NTEMP/5
          IF(MOD(NTEMP,5).GT.0)NLOOP=NLOOP+1
          DO9031L=1,NLOOP
            JSTOP=JSTRT+4
            IF(JSTOP.GT.JKB)JSTOP=JKB
            IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
              WRITE(ICOUT,9032)(X(J),J=JSTRT,JSTOP)
 9032         FORMAT(23X,5F11.2)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            JSTRT=JSTRT+5
 9031     CONTINUE
        ENDIF
  100 CONTINUE
      DO 101 J=1,KK 
        RNS=NS(J) 
        TTD(J)=TTD(J)/RNS 
  101 CONTINUE
CNIST WRITE(LUB,9040)(TTD(J),J=1,KK)
C9040 FORMAT(/33H  AVERAGE DISTANCE TO EACH MEDOID,6(/2X,5F12.3))
CNIST WRITE(LUB,9050)(RADUS(J),J=1,KK)
C9050 FORMAT(/33H  MAXIMUM DISTANCE TO EACH MEDOID,6(/2X,5F12.3))
      IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9040)
 9040   FORMAT('  AVERAGE DISTANCE TO EACH MEDOID')
        CALL DPWRST('XXX','BUG ')
        NLOOP=KK/5
        IF(MOD(KK,5).GT.0)NLOOP=NLOOP+1
        JSTRT=1
        DO9041L=1,NLOOP
          JSTOP=JSTRT+1
          IF(JSTOP.GT.KK)JSTOP=KK
          WRITE(ICOUT,9042)(TTD(J),J=JSTRT,JSTOP)
 9042     FORMAT(2X,5F11.2)
          CALL DPWRST('XXX','BUG ')
          JSTRT=JSTRT+5
 9041   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9050)
 9050   FORMAT('  MAXIMUM DISTANCE TO EACH MEDOID')
        CALL DPWRST('XXX','BUG ')
        JSTRT=1
        DO9051L=1,NLOOP
          JSTOP=JSTRT+1
          IF(JSTOP.GT.KK)JSTOP=KK
          WRITE(ICOUT,9042)(RADUS(J),J=JSTRT,JSTOP)
          CALL DPWRST('XXX','BUG ')
          JSTRT=JSTRT+5
 9051   CONTINUE
      ENDIF
C
      IF(KK.GT.1)THEN
CC
CC       COMPUTATION OF MINIMAL DISTANCE OF MEDOID KA TO ANY
CC       OTHER MEDOID FOR COMPARISON WITH THE RADIUS OF CLUSTER KA.
CC
        DO 120 KA=1,KK
          NSTRT=0
          NPA=NP(KA)
          DO 110 KB=1,KK
            IF(KB.EQ.KA)GO TO 110
            NPB=NP(KB)
            NPAB=MEET(NPA,NPB)
            IF(NSTRT.EQ.0)THEN
               NSTRT=1
            ELSE
               IF(DYS(NPAB).GE.RATT(KA))GO TO 110
            ENDIF
            RATT(KA)=DYS(NPAB)
            IF(RATT(KA).NE.0.)GO TO 110 
            IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
CNIST         WRITE(LUB,9054)KA,KB
C9054         FORMAT(/51H  THE DISSIMILARITY BETWEEN THE MEDOIDS OF CLUSTERS, 
CNISTF               I3,5H AND ,I3,9H IS ZERO.)
CNIST         WRITE(LUB,9056) 
C9056         FORMAT('  IN THE FOLLOWING VECTOR A VALUE OF -1 IS GIVEN TO',
CNISTF               ' BOTH CLUSTERS.')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9054)
 9054         FORMAT('THE DISSIMILARITY BETWEEN THE MEDOIDS OF ',
     1               'CLUSTERS',I3,' AND ',I3,' IS ZERO.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9056)
 9056         FORMAT('IN THE FOLLOWING VECTOR A VALUE OF -1 IS GIVEN ',
     1               'TO BOTH CLUSTERS.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
            RATT(KA)=-1.
  110     CONTINUE
          IF(RATT(KA).GT.(-0.5))RATT(KA)=RADUS(KA)/RATT(KA) 
  120   CONTINUE
CNIST   WRITE(LUB,9060)(RATT(J),J=1,KK) 
C9060   FORMAT(/49H  MAXIMUM DISTANCE TO A MEDOID DIVIDED BY MINIMUM/ 
CNISTF        42H  DISTANCE OF THE MEDOID TO ANOTHER MEDOID,6(/2X,5F12.3))
        IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
          WRITE(ICOUT,9060)
 9060     FORMAT('  MAXIMUM DISTANCE TO A MEDOID DIVIDED BY MINIMUM')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9061)
 9061     FORMAT('  DISTANCE OF THE MEDOID TO ANOTHER MEDOID')
          CALL DPWRST('XXX','BUG ')
          NLOOP=KK/5
          IF(MOD(KK,5).GT.0)NLOOP=NLOOP+1
          JSTRT=1
          DO9063L=1,NLOOP
            JSTOP=JSTRT+1
            IF(JSTOP.GT.KK)JSTOP=KK
            WRITE(ICOUT,9064)(RATT(J),J=JSTRT,JSTOP)
 9064       FORMAT(2X,5F11.2)
            CALL DPWRST('XXX','BUG ')
            JSTRT=JSTRT+5
 9063     CONTINUE
        ENDIF
      ENDIF
C
 9090 CONTINUE
      RETURN
      END
      SUBROUTINE RESUL(KK,NN,JPP,LARGE,NDYST,X,NRX,AMISS,IC1,IOUNI1)
CNIST SUBROUTINE RESUL(KK,NN,JPP,LARGE,NDYST,LUB,MDATA,JTMD,
CNIST1                 VALMD,X,MAXXX,MAXTT,NRX)
C
C     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
C
C        1. USE DATAPLOT OUTPUT
C        2. RECODE A BIT FOR BETTER READABILITY
C
C     KK        = NUMBER OF CLUSTERS
C     NN        = NUMBER OF ROWS (CASES)
C     JPP       = NUMBER OF COLUMNS (VARIABLES)
C     LARGE     = SPECIFY WHAT WILL BE OUTPUT
C     NDYST     = 1 => EUCLIDEAN DISTANCES
C                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
C     LUB       = OUTPUT UNIT (WE DON'T USE)
C     MDATA     = MISSING VALUES PRESENT (WE DON'T USE)
C     JTMD      = FOR MISSING VALUES (WE DON'T USE)
C     VALMD     = FOR MISSING VALUES (WE DON'T USE)
C     X         = THE DATA MATRIX
C     MAXXX     = THE MAXIMUM OF ROWS TIMES COLUMNS (WE DON'T USE)
C     MAXTT     = THE MAXIMUM NUMBER OF VARIABLES (COLUMNS) (WE DON'T USE)
C
      DIMENSION X(*)
      DIMENSION NRX(*)
      DIMENSION IC1(*)
C
      PARAMETER (LYNF=25)
      DIMENSION LYNE(LYNF)
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      PP=JPP
CC
CC   CLUSTERING VECTOR IS INCORPORATED INTO X, AND PRINTED. 
CC
      JN=0
  100 CONTINUE
      JN=JN+1 
      NJNB=(JN-1)*JPP
      DO 145 JK=1,KK
        IF(NRX(JK).EQ.JN)GO TO 220
  145 CONTINUE
      JNA=(JN-1)*JPP+1
      DO 190 JK=1,KK
        DSUM=0. 
        NRJK=(NRX(JK)-1)*JPP
        ABC=0.
        DO 180 J=1,JPP
          NA=NRJK+J
          NB=NJNB+J
          IF(X(NA).EQ.AMISS .OR. X(NB).EQ.AMISS)GO TO 180
          ABC=ABC+1.
          TRA=ABS(X(NA)-X(NB))
          IF(NDYST.EQ.1)TRA=TRA*TRA
          DSUM=DSUM+TRA
  180   CONTINUE
        IF(NDYST.EQ.1)DSUM=SQRT(DSUM)
        DSUM=DSUM*ABC/PP
        IF(JK.EQ.1)DNULL=DSUM+0.1
        IF(DSUM.GE.DNULL)GO TO 190
        DNULL=DSUM
        JKSKY=JK
  190 CONTINUE
C
      X(JNA)=JKSKY
  220 CONTINUE
      IF(JN.LT.NN)GO TO 100 
C
      DO 230 JK=1,KK
        NRJK=NRX(JK)
        NRJKA=(NRJK-1)*JPP+1
        X(NRJKA)=JK 
  230 CONTINUE
CNIST WRITE(LUB,9110) 
C9110 FORMAT(//2X,18H CLUSTERING VECTOR/3X,17(1H*)/)
      IF(IPRINT.EQ.'ON' .AND. LARGE.GE.1)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9110)
 9110   FORMAT('   CLUSTERING VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9111)
 9111   FORMAT('   *****************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICNT=0
      MTEL=0
      MTELP=LYNF
  240 CONTINUE
      DO 250 J=1,MTELP
        MTEL=MTEL+1 
        MTELA=(MTEL-1)*JPP+1
        LYNE(J)=X(MTELA)
        ICNT=ICNT+1
        IC1(ICNT)=LYNE(J)
  250 CONTINUE
CNIST WRITE(LUB,9120)(LYNE(J),J=1,MTELP)
C
      IF(IPRINT.EQ.'ON' .AND. LARGE.GE.1)THEN
        WRITE(ICOUT,9120)(LYNE(J),J=1,MTELP)
 9120   FORMAT(4X,25I3)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      DO9125J=1,MTELP
        WRITE(IOUNI1,'(I7)')LYNE(J)
 9125 CONTINUE
C
      IF(MTEL.GE.NN)GO TO 300
      NNTEL=NN-MTEL 
      IF(NNTEL.GE.LYNF)GO TO 240
      MTELP=NN-MTEL
      GO TO 240
CC
CC   WHEN LARGE IS NOT ZERO, LIST OF ALL CLUSTER ELEMENTS IN ENTIRE 
CC   DATA SET IS GIVEN. 
CC
  300 CONTINUE
      IF(LARGE.LE.0)GO TO 330 
CNIST WRITE(LUB,9130) 
C9130 FORMAT(//4X,27HCLUSTER SIZE MEDOID OBJECTS) 
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9130)
 9130   FORMAT('    CLUSTER SIZE MEDOID OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO 320 KA=1,KK
        MTT=0 
        J=0 
  325   CONTINUE
        J=J+1 
        JA=(J-1)*JPP+1
        NXJA=INT(X(JA)+0.1) 
        IF(NXJA.EQ.KA)MTT=MTT+1 
        IF(J.LT.NN)GO TO 325
CNIST   WRITE(LUB,9140)KA,MTT,NRX(KA) 
C9140   FORMAT(/3X,I8,I5,I7)
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9140)KA,MTT,NRX(KA)
 9140     FORMAT(3X,I8,I5,I7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        MTT=0 
        J=0 
  315   CONTINUE
        J=J+1 
        JA=(J-1)*JPP+1
        NXJA=INT(X(JA)+0.1) 
        IF(NXJA.NE.KA)GO TO 310 
        MTT=MTT+1 
        LYNE(MTT)=J 
        IF(MTT.NE.10)GO TO 310
        MTT=0 
C
CNIST   WRITE(LUB,9150)(LYNE(JJ),JJ=1,10) 
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9150)(LYNE(JJ),JJ=1,10)
 9150     FORMAT(24X,10I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
  310   CONTINUE
        IF(J.LT.NN)GO TO 315
CNIST   IF(MTT.NE.0)WRITE(LUB,9150)(LYNE(JJ),JJ=1,MTT)
        IF(MTT.NE.0 .AND. IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9150)(LYNE(JJ),JJ=1,MTT)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  320 CONTINUE
  330 CONTINUE
      RETURN
      END
      SUBROUTINE CSTAT(KK,NN,NSEND,NREPR,RADUS,DAMER,TTD,SEPAR,Z,S, 
     1                 DYS,NCLUV,NELEM,JPP,MAXNN,MAXPP,X,JDYSS,NSTAN,
     1                 IOUNI2,ISUBRO,IBUGA3)
CNIST SUBROUTINE CSTAT(KK,NN,NSEND,NREPR,RADUS,DAMER,TTD,SEPAR,Z,S, 
CNIST1                 MAXHH,DYS,NCLUV,NELEM,JPP,MAXNN,MAXPP,X,LAB,
CNIST1                 LUB,JDYSS,NSTAN)
C
C     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
C
C        1. USE DATAPLOT OUTPUT
C        2. RECODE A BIT FOR BETTER READABILITY
C
C     KK        = NUMBER OF CLUSTERS
C     NN        = NUMBER OF ROWS (CASES)
C     NSEND     = 
C     NREPR     = 
C     RADUS     = MAXIMUM DISSIMILARITY TO EACH MEDOID
C     DAMER     = 
C     TTD       = AVERAGE DISSIMILARITY TO EACH MEDOID
C     SEPAR     = 
C     Z         = 
C     S         = 
C     MAXHH     = MAXIMUM DIMENSION FOR DISTANCES
C                 (MAXNN*(MAXNN-1)/2 + 1)
C                 EQUALS 4951 IN ROUSSEEUW
C     DYS       = VECTOR CONTAINING THE DISSIMILARITIES
C     NCLUV     = 
C     NELEM     = 
C     JPP       = NUMBER OF COLUMNS (VARIABLES)
C     MAXNN     = MAXIMUM NUMBER OF ROWS
C                 SET TO 100 BY ROUSSEEUW
C     MAXPP     = MAXIMUM NUMBER OF VARIABLES
C                 SET TO 20 BY ROUSSEEUW
C     X         = THE DATA MATRIX
C     LAB       = VARIABLE LABELS
C                 USE "001", "002", ETC.
C     JDYSS     = 1 => DISSIMILARITY MATRIX
C                 1 =  MEASUREMENT DATA
C     NSTAN     = 0 => NO STANDARDIZATION OF VARIABLES
C                 1 => VARIABLES ARE STANDARDIZED
C     LUB       = OUTPUT UNIT (WE DON'T USE)
C
      DIMENSION SEPAR(*)
      DIMENSION DAMER(*)
      DIMENSION TTD(*) 
      DIMENSION DYS(*)
      DIMENSION X(MAXNN,MAXPP)
C
      DIMENSION NCLUV(*)
      DIMENSION NSEND(*)
      DIMENSION NREPR(*)
      DIMENSION NELEM(*) 
      DIMENSION RADUS(*)
C
      CHARACTER*1 LAB(3,MAXNN)
      CHARACTER*1 JDRAW(30)
      CHARACTER*3 LAB1
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF CSTAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)KK,NN
   55   FORMAT('KK,NN = ',2I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO 130 J=1,NN 
        IF(NREPR(J).EQ.1)THEN
          NSEND(J)=J
        ELSE
          DSMAL=1.1*S+1.0 
          DO 110 K=1,NN 
            IF(NREPR(K).EQ.0)GO TO 110
            NJAJ=MEET(K,J)
            IF(DYS(NJAJ).GE.DSMAL)GO TO 110 
            DSMAL=DYS(NJAJ) 
            KSMAL=K 
  110     CONTINUE
          NSEND(J)=KSMAL
        ENDIF
  130 CONTINUE
C
      JK=1  
      NPLAC=NSEND(1)
      DO 135 J=1,NN 
        NCLUV(J)=0
        IF(NSEND(J).EQ.NPLAC)NCLUV(J)=1 
  135 CONTINUE
C
      DO 145 JA=2,NN
        NPLAC=NSEND(JA) 
        IF(NCLUV(NPLAC).NE.0)GO TO 145
        JK=JK+1 
        DO 140 J=2,NN 
          IF(NSEND(J).EQ.NPLAC)NCLUV(J)=JK
  140   CONTINUE
        IF(JK.EQ.KK)GO TO 148
  145 CONTINUE
C
C     ANALYSIS OF THE CLUSTERING.
C
  148 CONTINUE
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9200) 
 9200   FORMAT('CLUSTERS ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9201) 
 9201   FORMAT(2X,' NUMBER  MEDOID   SIZE      OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO 160 NUMCL=1,KK 
        NTT=0 
        RADUS(NUMCL)=-1.0 
        TTT=0.0
C
        DO 150 J=1,NN
          IF(NCLUV(J).NE.NUMCL)GO TO 150
          NTT=NTT+1 
          M=NSEND(J)
          NELEM(NTT)=J
          NJM=MEET(J,M) 
          TTT=TTT+DYS(NJM)
          IF(DYS(NJM).GT.RADUS(NUMCL))RADUS(NUMCL)=DYS(NJM)
  150   CONTINUE
C
        RTT=NTT 
        TTD(NUMCL)=TTT/RTT
        NSS=NTT 
        IF(NSS.GT.10)NSS=10 
        DO 152 L=1,NSS
          LEEN=3*(L-1)+1
          LTWE=3*(L-1)+2
          LDRE=3*L
          NCASE=NELEM(L)
          LAB1='000'
          WRITE(LAB1,'(I3)')NCASE
          JDRAW(LEEN)=LAB1(1:1)
          JDRAW(LTWE)=LAB1(2:2)
          JDRAW(LDRE)=LAB1(3:3)
  152   CONTINUE
C
        NSSDR=NSS*3 
CNIST   WRITE(LUB,9210)NUMCL,LAB(1,M),LAB(2,M),LAB(3,M),NTT,
CNIST1                 (JDRAW(K),K=1,NSSDR)
C9210   FORMAT(/1X,I5,6X,3A1,2X,I6,5X,10(3A1,1X)) 
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          LAB1='000'
          WRITE(LAB1,'(I3)')M
          WRITE(ICOUT,9210)NUMCL,LAB1,NTT,
     1                     (JDRAW(K),K=1,NSSDR)
 9210     FORMAT(I5,6X,A3,2X,I6,5X,10(3A1,1X)) 
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NTT.LE.10)GO TO 160
        KAUNT=0 
        DO 154 L=11,NTT 
          KAUNT=KAUNT+1 
          LEEN=3*(KAUNT-1)+1
          LTWE=3*(KAUNT-1)+2
          LDRE=3*KAUNT
          NCASE=NELEM(L)
          LAB1='000'
          WRITE(LAB1,'(I3)')NCASE
          JDRAW(LEEN)=LAB1(1:1)
          JDRAW(LTWE)=LAB1(2:2)
          JDRAW(LDRE)=LAB1(3:3)
          IF(KAUNT.EQ.10)THEN
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9215)(JDRAW(K),K=1,30)
 9215         FORMAT(28X,10(3A1,1X))
              CALL DPWRST('XXX','BUG ')
            ENDIF
            KAUNT=0 
          ENDIF
  154   CONTINUE
C
        IF(KAUNT.GE.1 .AND. IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9215)(JDRAW(K),K=1,LDRE)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  160 CONTINUE
C
      IF(JDYSS.NE.1)THEN
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          IF(NSTAN.EQ.0)THEN
            WRITE(ICOUT,9220) 
 9220       FORMAT('COORDINATES OF MEDOIDS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9221) 
 9221       FORMAT('**********************')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
          ELSEIF(NSTAN.EQ.1)THEN
            WRITE(ICOUT,9230) 
 9230       FORMAT('COORDINATES OF MEDOIDS (USING STANDARDIZED ',
     1             'MEASUREMENTS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9231) 
 9231       FORMAT(
     1      '*******************************************************')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        DO 210 NUMCL=1,KK 
          DO 220 L=1,NN 
            IF(NCLUV(L).EQ.NUMCL)GO TO 225
  220     CONTINUE
  225     CONTINUE
          M=NSEND(L)
          LAB1='000'
          WRITE(LAB1,'(I3)')M
          ILOOP=JPP/8
          IREM=MOD(JPP,8)
          IF(IREM.GT.0)ILOOP=ILOOP+1
          DO9243II=1,ILOOP
            ISTRT=(II-1)*8 + 1
            ISTOP=II*8
            IF(ISTOP.GT.JPP)ISTOP=JPP
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9240)LAB1,(X(M,J),J=ISTRT,ISTOP)
 9240         FORMAT(1X,A3,2X,8F9.2)
              CALL DPWRST('XXX','BUG ')
            ENDIF
            WRITE(IOUNI2,9240)LAB1,(X(M,J),J=ISTRT,ISTOP)
 9243     CONTINUE
  210   CONTINUE
      ENDIF
C
      RNN=NN
      IF(KK.EQ.1)THEN
        DAMER(1)=S
        GO TO 300 
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9270) 
 9270   FORMAT('CLUSTERING VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9271) 
 9271   FORMAT('*****************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,9280)(NCLUV(J),J=1,NN)
 9280   FORMAT(11X,50(20I3/11X))
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9290) 
 9290   FORMAT('CLUSTERING CHARACTERISTICS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9291) 
 9291   FORMAT('**************************')
        CALL DPWRST('XXX','BUG ')
      ENDIF
CC
CC    NUML = NUMBER OF L-CLUSTERS.
CC
      NUML=0
      DO 40 K=1,KK
CC
CC      IDENTIFICATION OF CLUSTER K:
CC         NEL=NUMBER OF OBJECTS
CC         NELEM=VECTOR OF OBJECTS
CC
        NEL=0 
C
        DO 23 J=1,NN
          IF(NCLUV(J).NE.K)GO TO 23 
          NEL=NEL+1 
          NELEM(NEL)=J
   23   CONTINUE
C
        IF(NEL.EQ.1)THEN
          NVN=NELEM(1)
          DAMER(K)=0. 
          SEPAR(K)=1.1*S+1.0
          DO 250 J=1,NN 
            IF(J.EQ.NVN)GO TO 250 
            MEVJ=MEET(NVN,J)
            IF(SEPAR(K).GT.DYS(MEVJ))SEPAR(K)=DYS(MEVJ)
  250     CONTINUE
CC
CC        IS CLUSTER K     1) AN L-CLUSTER ?
CC                         2) AN L*-CLUSTER ?
CC
          LAB1='000'
          WRITE(LAB1,'(I3)')NVN
          IF(SEPAR(K).NE.0.)THEN
            NUML=NUML+1 
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9310)K
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9320)LAB1
 9320         FORMAT(8X,' IT IS A SINGLETON CONSISTING OF OBJECT  ',A3)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9321)SEPAR(K)
 9321         FORMAT(8X,' ITS SEPARATION = ',F11.2) 
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSE
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9324)K,LAB1
 9324         FORMAT(' CLUSTER ',I4,' IS A SINGLETON CONSISTING OF',
     1               ' OBJECT ',A3,'. IT IS NOT ISOLATED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9326) 
 9326         FORMAT(' ** IT IS NOT ADVISABLE TO DIVIDE THE DATA INTO', 
     1               ' SO MANY CLUSTERS.') 
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
C
        ELSE
          DAM=-1. 
          SEP=1.1*S+1.0 
          KAND=1
C
          DO 26 JA=1,NEL
            NVNA=NELEM(JA)
            AJA=-1. 
            AJB=1.1*S+1.0 
            DO 25 JB=1,NN 
              JNDZ=MEET(NVNA,JB)
              IF(NCLUV(JB).EQ.K)GO TO 30
              IF(DYS(JNDZ).LT.AJB)AJB=DYS(JNDZ) 
              GO TO 25
   30         CONTINUE
              IF(DYS(JNDZ).GT.AJA)AJA=DYS(JNDZ) 
   25       CONTINUE
            IF(AJA.GE.AJB)KAND=0
            IF(DAM.LT.AJA)DAM=AJA 
            IF(SEP.GT.AJB)SEP=AJB 
   26     CONTINUE
C
          SEPAR(K)=SEP
          DAMER(K)=DAM
          IF(KAND.EQ.0)GO TO 40 
CC
CC        DIAMETER AND SEPARATION OF ISOLATED CLUSTERS
CC
          IF(IPRINT.EQ.'ON')THEN
            WRITE(ICOUT,9310)K
 9310       FORMAT('CLUSTER ',I4,' IS ISOLATED')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9330)DAM,SEP
 9330       FORMAT(8X,' WITH DIAMETER  =',F11.2,' AND SEPARATION =',
     1             F11.2)
            CALL DPWRST('XXX','BUG ')
          ENDIF
          NUML=NUML+1 
          IF(DAM.LT.SEP)THEN
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9350) 
 9350         FORMAT(8X,' THEREFORE IT IS AN L*-CLUSTER.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSE
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9340) 
 9340         FORMAT(8X,' IT IS AN L-CLUSTER.')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
C
        ENDIF
C
   40 CONTINUE
C
      IF(IPRINT.EQ.'ON')THEN
        IF(NUML.EQ.0)THEN
          WRITE(ICOUT,9360)
 9360     FORMAT(' THERE ARE NO ISOLATED CLUSTERS')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NUML.GE.1)THEN
          WRITE(ICOUT,9365)NUML
 9365     FORMAT(' THE NUMBER OF ISOLATED CLUSTERS = ',I4)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
  300 CONTINUE
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9370)
 9370   FORMAT('  DIAMETER OF EACH CLUSTER')
        CALL DPWRST('XXX','BUG ')
        ILOOP=KK/8
        IREM=MOD(KK,8)
        IF(IREM.GT.0)ILOOP=ILOOP+1
        DO9375II=1,ILOOP
          ISTRT=(II-1)*8 + 1
          ISTOP=II*8
          IF(ISTOP.GT.KK)ISTOP=KK
          WRITE(ICOUT,9371)(DAMER(J),J=ISTRT,ISTOP)
 9371     FORMAT(2X,8F9.2)
          CALL DPWRST('XXX','BUG ')
 9375   CONTINUE
      ENDIF
C
      IF(KK.NE.1)THEN
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9380)
 9380     FORMAT('  SEPARATION OF EACH CLUSTER')
          CALL DPWRST('XXX','BUG ')
          ILOOP=KK/8
          IREM=MOD(KK,8)
          IF(IREM.GT.0)ILOOP=ILOOP+1
          DO9385II=1,ILOOP
            ISTRT=(II-1)*8 + 1
            ISTOP=II*8
            IF(ISTOP.GT.KK)ISTOP=KK
            WRITE(ICOUT,9381)(SEPAR(J),J=ISTRT,ISTOP)
 9381       FORMAT(2X,8F9.2) 
            CALL DPWRST('XXX','BUG ')
 9385     CONTINUE
        ENDIF
      ENDIF
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9390)
 9390   FORMAT('  AVERAGE DISSIMILARITY TO EACH MEDOID')
        CALL DPWRST('XXX','BUG ')
        ILOOP=KK/8
        IREM=MOD(KK,8)
        IF(IREM.GT.0)ILOOP=ILOOP+1
        DO9395II=1,ILOOP
          ISTRT=(II-1)*8 + 1
          ISTOP=II*8
          IF(ISTOP.GT.KK)ISTOP=KK
          WRITE(ICOUT,9391)(TTD(J),J=ISTRT,ISTOP)
 9391     FORMAT(2X,8F9.2)
          CALL DPWRST('XXX','BUG ')
 9395   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9400)
 9400   FORMAT('  MAXIMUM DISSIMILARITY TO EACH MEDOID')
        CALL DPWRST('XXX','BUG ')
        ILOOP=KK/8
        IREM=MOD(KK,8)
        IF(IREM.GT.0)ILOOP=ILOOP+1
        DO9405II=1,ILOOP
          ISTRT=(II-1)*8 + 1
          ISTOP=II*8
          IF(ISTOP.GT.KK)ISTOP=KK
          WRITE(ICOUT,9401)(RADUS(J),J=1,KK)
 9401     FORMAT(2X,8F9.2) 
          CALL DPWRST('XXX','BUG ')
 9405   CONTINUE
      ENDIF
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF CSTAT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE SUPCL(DYS,KKA,KKB,AREST,NER)
CNIST SUBROUTINE SUPCL(MAXHH,DYS,KKA,KKB,AREST,MAXNN,NER) 
C
C     DYS    = VECTOR OF DISTANCES
C     KKA    = ...
C     KKB    = ...
C     AREST  = ...
C     NER    = ...
C
      DIMENSION DYS(*)
      DIMENSION NER(*)
C
      KKC=KKB-1 
      AREST=0.
      DO 20 L=KKA,KKC 
        LNER=NER(L) 
        KKD=L+1 
        DO 10 J=KKD,KKB 
          JNER=NER(J) 
          MLJ=MEET(LNER,JNER) 
          IF(DYS(MLJ).GT.AREST)AREST=DYS(MLJ) 
   10   CONTINUE
   20 CONTINUE
C
      RETURN
      END 
      SUBROUTINE AVERL(NN,KWAN,NER,BAN,DYS,
     1                 NCLUT,LAT,LBT,BANLAT,BANLBT,
     1                 IOUNI2,IOUNI3,IOUNI4,IAGNME,ISUBRO,IBUGA3)
CNIST SUBROUTINE AVERL(NN,MAXNN,KWAN,NER,BAN,MAXHH,DYS,LUB) 
C
C     NN    = NUMBER OF OBJECTS
C     MAXNN = MAXIMUM NUMBER OF OBJECTS (DATAPLOT DOES NOT USE)
C     KWAN  = NUMBER OF OBJECTS IN EACH CLUSTER
C     NER   = FINAL ORDERING OF OBJECTS
C     BAN   = DISSIMILARITIES BETWEEN CLUSTERS
C     MAXHH = MAXIMUM NUMBER OF DISSIMILARITIES (DATAPLOT DOES NOT USE)
C     DYS   = VECTOR OF DISSIMILARITIES
C     LUB   = OUTOPUT UNIT (DATAPLOT DOES NOT USE)
C
      DIMENSION DYS(*)
      DIMENSION BAN(*)
      DIMENSION BANLAT(*)
      DIMENSION BANLBT(*)
C
      DIMENSION NER(*)
      DIMENSION KWAN(*)
      DIMENSION NCLUT(*)
      DIMENSION LAT(*)
      DIMENSION LBT(*)
C
      CHARACTER*4 IAGNME
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C     INITIALIZATION
C
C     NCLU   = NUMBER OF CLUSTERS
C     KWAN   = NUMBER OF OBJECTS IN EACH CLUSTER
C     NER    = OBJECT ID'S FOR THE CLUSTER
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,5)NN
    5   FORMAT('BEGINING OF AVERL: NN = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICNT=0
      NCLU=NN-1 
      DO 10 L=1,NN
        KWAN(L)=1 
        NER(L)=L
   10 CONTINUE
CC
CC    FIND CLOSEST CLUSTERS
CC
  100 CONTINUE
      J=1
C
   80 CONTINUE
      J=J+1 
      IF(KWAN(J).EQ.0)GOTO 80
      NEJ=MEET(1,J) 
      SMALD=DYS(NEJ)*1.1+1.0
      NNS=NN-1
      DO 120 L=1,NNS  
        IF(KWAN(L).EQ.0)GO TO 120 
        LMUCH=L+1 
        DO 110 J=LMUCH,NN 
          IF(KWAN(J).EQ.0)GO TO 110 
          NLJ=MEET(L,J) 
          IF(DYS(NLJ).GT.SMALD)GO TO 110
          SMALD=DYS(NLJ)
          LA=L
          LB=J  
  110   CONTINUE
  120 CONTINUE
CC
CC    DETERMINE LFYRS AND LLAST
CC
      DO 200 L=1,NN 
        IF(NER(L).EQ.LA)LFYRS=L 
        IF(NER(L).EQ.LB)LLAST=L 
  200 CONTINUE
      BAN(LLAST)=SMALD
CC
CC    IF THE TWO CLUSTERS ARE NEXT TO EACH OTHER,
CC    NER MUST NOT BE CHANGED
CC
      LNEXT=LFYRS+KWAN(LA)
      IF(LNEXT.NE.LLAST)THEN
CC
CC      UPDATING NER AND BAN
CC
        LPUT=LFYRS+KWAN(LA) 
        LNUM=LLAST-LPUT 
        DO 220 L=1,LNUM 
          LKA=NER(LPUT) 
          AKB=BAN(LPUT) 
          LENDA=LLAST+KWAN(LB)-2
          LENDB=LENDA+1 
          DO 210 J=LPUT,LENDA 
            NER(J)=NER(J+1) 
            BAN(J)=BAN(J+1) 
  210     CONTINUE
          NER(LENDB)=LKA
          BAN(LENDB)=AKB
  220   CONTINUE
      ENDIF
CC
CC    CALCULATE NEW DISSIMILARITIES
CC
CC    SUPPORT FOR DIFFERENT METHODS
CC
      IF(IAGNME.EQ.'SING')THEN
        DO 241 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 241 
          IF(KWAN(LQ).EQ.0)GO TO 241
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          DNEW=DYS(NAQ)
          IF(DYS(NBQ).LT.DNEW)DNEW=DYS(NBQ)
          DYS(NAQ)=DNEW
  241   CONTINUE
      ELSEIF(IAGNME.EQ.'COMP')THEN
        DO 242 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 242 
          IF(KWAN(LQ).EQ.0)GO TO 242
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          DNEW=DYS(NAQ)
          IF(DNEW.LT.DYS(NBQ))DNEW=DYS(NBQ)
          DYS(NAQ)=DNEW
  242   CONTINUE
      ELSEIF(IAGNME.EQ.'CENT')THEN
        DO 243 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 243 
          IF(KWAN(LQ).EQ.0)GO TO 243
          TA=KWAN(LA) 
          TB=KWAN(LB) 
          FA=TA/(TA+TB) 
          FB=TB/(TA+TB) 
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          NAB=MEET(LA,LB)
          D=FA*DYS(NAQ)*DYS(NAQ) + FB*DYS(NBQ)*DYS(NBQ)
          D=D + FC*DYS(NAB)*DYS(NAB)
          DYS(NAQ)=SQRT(D)
  243   CONTINUE
      ELSEIF(IAGNME.EQ.'WARD')THEN
        DO 244 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 244 
          IF(KWAN(LQ).EQ.0)GO TO 244
          TA=KWAN(LA) 
          TB=KWAN(LB) 
          TQ=KWAN(LQ) 
          FA=(TA + TQ)/(TA + TB + TQ)
          FB=(TB + TQ)/(TA + TB + TQ)
          FC=-TQ/(TA + TB + TQ)
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          NAB=MEET(LA,LB)
          D=FA*DYS(NAQ)*DYS(NAQ) + FB*DYS(NBQ)*DYS(NBQ)
          D=D + FC*DYS(NAB)*DYS(NAB)
          DYS(NAQ)=SQRT(D)
  244   CONTINUE
      ELSEIF(IAGNME.EQ.'WAVL')THEN
        DO 245 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 245 
          IF(KWAN(LQ).EQ.0)GO TO 245
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          DYS(NAQ)=(DYS(NAQ) + DYS(NBQ))/2.0
  245   CONTINUE
      ELSEIF(IAGNME.EQ.'GOWE')THEN
        DO 246 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 246 
          IF(KWAN(LQ).EQ.0)GO TO 246
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          NAB=MEET(LA,LB)
          D=(DYS(NAQ)*DYS(NAQ) + DYS(NBQ)*DYS(NBQ))/2.0
          D=D - (DYS(NAB)*DYS(NAB))/4.0
          DYS(NAQ)=SQRT(D)
  246   CONTINUE
      ELSE
        DO 240 LQ=1,NN
          IF(LQ.EQ.LA.OR.LQ.EQ.LB)GO TO 240 
          IF(KWAN(LQ).EQ.0)GO TO 240
          TA=KWAN(LA) 
          TB=KWAN(LB) 
          FA=TA/(TA+TB) 
          FB=TB/(TA+TB) 
          NAQ=MEET(LA,LQ) 
          NBQ=MEET(LB,LQ) 
          DYS(NAQ)=FA*DYS(NAQ)+FB*DYS(NBQ)
  240   CONTINUE
      ENDIF
C
      IF(NCLU.EQ.1 .AND. IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9100) 
 9100   FORMAT('THE FINAL ORDERING OF THE OBJECTS IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        ILOOP=NN/5
        IF(MOD(NN,5).GT.0)ILOOP=ILOOP+1
        DO9111II=1,ILOOP
          ISTRT=(II-1)*5+1
          ISTOP=II*5
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9110)(NER(L),L=ISTRT,ISTOP)
 9110     FORMAT(5(I9,6X))
          CALL DPWRST('XXX','BUG ')
 9111   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9120) 
 9120   FORMAT('THE DISSIMILARITIES BETWEEN CLUSTERS ARE') 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        ILOOP=(NN-1)/5
        IF(MOD((NN-1),5).GT.0)ILOOP=ILOOP+1
        DO9131II=1,ILOOP
          ISTRT=(II-1)*5+2
          ISTOP=II*5 + 1
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9130)(BAN(L),L=ISTRT,ISTOP)
 9130     FORMAT(3X,5F15.3) 
          CALL DPWRST('XXX','BUG ')
 9131   CONTINUE
      ENDIF
C
      KWAN(LA)=KWAN(LA)+KWAN(LB)
      KWAN(LB)=0
C
C     PRINT RESULTS FROM CURRENT CLUSTER
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
        WRITE(ICOUT,9910)NCLU,LA,LB,LFYRS,LLAST
 9910   FORMAT('AVERL: NCLU,LA,LB,LFYRS,LLAST = ',5I5)
        CALL DPWRST('XXX','BUG ')
        DO9920L=1,NN
          WRITE(ICOUT,9921)L,NER(L),KWAN(L),BAN(L)
 9921     FORMAT('L,NER(L),KWAN(L),BAN(L) = ',3I5,F12.5)
          CALL DPWRST('XXX','BUG ')
 9920   CONTINUE
      ENDIF
C
      DISTMX=-1.0
      DO1020II=1,NN
        IF(BAN(II).GT.DISTMX)DISTMX=BAN(II)
 1020 CONTINUE
C
      DO1025II=1,NN
        IF(LA.EQ.NER(II))THEN
          AVAL1=BAN(II)
        ELSEIF(LB.EQ.NER(II))THEN
          AVAL2=BAN(II)
        ENDIF
 1025 CONTINUE
      ICNT=ICNT+1
      WRITE(IOUNI4,'(3I5,2E15.7)')NCLU,LA,LB,AVAL1,AVAL2
      NCLU=NCLU-1 
      IF(NCLU.GT.0)GOTO 100 
C
C     NOW CREATE DATA FOR:
C
C         1. DENDOGRAM (IOUNI3)
C         2. ICICLE PLOT (IOUNI2)
C
      REWIND(IOUNI4)
C
      DO2010KK=1,ICNT
        READ(IOUNI4,'(3I5,2E15.7)',END=2019,ERR=2019)
     1      NCLUT(KK),LAT(KK),LBT(KK),BANLAT(KK),BANLBT(KK)
 2010 CONTINUE
C
      ITAG=0
      DO2020KK=1,ICNT
C
C       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
C       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
C       THE APPROPRIATE X-COORDINATE.
C
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        AVAL1=BANLBT(KK)
C
        IF(KK.EQ.1)THEN
C
C         FIRST CLUSTER BEING FORMED
C
          XVAL1=1.0
          XVAL2=2.0
          DO2021JJ=1,NN
            IF(IFRST.EQ.NER(JJ))XVAL1=REAL(JJ)
            IF(ISEC.EQ.NER(JJ))XVAL2=REAL(JJ)
 2021     CONTINUE
          YVAL1=0.0
          YVAL2=AVAL1
          ITAG=ITAG+1
          WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
          WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
          WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
          WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL1,REAL(ITAG)
C
        ELSE
C
C         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
C         PREVIOUS LA.
C
          IFLAGL=0
          IFLAGR=0
          DO2030JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
C
C             MATCH WITH PREVIOUS CLUSTER FOUND
C
              ISEC2=LBT(JJ)
              DO2031LL=1,NN
                IF(ISEC2.EQ.NER(LL))THEN
                  XVAL1=REAL(LL) - 0.5
                  XVAL3=REAL(LL)
                ENDIF
                IF(ISEC.EQ.NER(LL))THEN
                  XVAL2=REAL(LL)
                  XVAL4=XVAL2
                ENDIF
 2031         CONTINUE
              YVAL1=BANLBT(JJ)
              YVAL2=AVAL1
              YVAL3=0.0
              IFLAGL=1
              GOTO2039
            ENDIF
 2030     CONTINUE
 2039     CONTINUE
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,2035)KK,IFLAGL
 2035       FORMAT('AFTER 2030 LOOP: KK,IFLAGL = ',2I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
 2038       FORMAT('XVAL1,XVAL2,YVAL1,YVAL2,YVAL3 = ',5G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
C         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
C
          IF(IFLAGL.EQ.0)THEN
            DO2040JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO2041LL=1,NN
                  IF(IFRST.EQ.NER(LL))THEN
                    XVAL1=REAL(LL)
                    XVAL3=REAL(LL)
                  ENDIF
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL) - 0.5
                    XVAL4=REAL(LL)
                  ENDIF
 2041           CONTINUE
                YVAL1=0.0
                YVAL2=AVAL1
                YVAL3=BANLBT(JJ)
                IFLAGR=1
                GOTO2049
              ENDIF
 2040       CONTINUE
 2049       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2045)KK,IFLAGR
 2045         FORMAT('AFTER 2040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ELSEIF(IFLAGL.EQ.1)THEN
            DO2050JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO2053LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL) - 0.5
                    XVAL4=REAL(LL)
                  ENDIF
 2053           CONTINUE
                YVAL2=AVAL1
                YVAL3=BANLBT(JJ)
                IFLAGR=1
                GOTO2059
              ENDIF
 2050       CONTINUE
 2059       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2055)KK,IFLAGR
 2055         FORMAT('AFTER 2050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ENDIF
C
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL3,REAL(ITAG)
C
          ELSE
C
C           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
C           NEW CLUSTER
C
            DO2061JJ=1,NN
              IF(IFRST.EQ.NER(JJ))THEN
                XVAL1=REAL(JJ)
                XVAL3=REAL(JJ)
              ENDIF
              IF(ISEC.EQ.NER(JJ))THEN
                XVAL2=REAL(JJ)
                XVAL4=REAL(JJ)
              ENDIF
 2061       CONTINUE
            YVAL1=0.0
            YVAL2=AVAL1
            ITAG=ITAG+1
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL1,REAL(ITAG)
C
          ENDIF
        ENDIF
C
 2020 CONTINUE
C
      ITAG=0
      DO3020KK=1,ICNT
C
C       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
C       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
C       THE APPROPRIATE X-COORDINATE.
C
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        NCLU=NCLUT(KK)
        YVAL=REAL(NCLU)
C
        IF(KK.EQ.1)THEN
C
C         FIRST CLUSTER BEING FORMED
C
          XVAL1=1.0
          XVAL2=2.0
          DO3021JJ=1,NN
            IF(IFRST.EQ.NER(JJ))XVAL1=REAL(JJ)
            IF(ISEC.EQ.NER(JJ))XVAL2=REAL(JJ)
 3021     CONTINUE
C
          XVAL1=(XVAL1-1.0)*2.0 + 1.0
          XVAL2=XVAL1 + 1.0
          XVAL3=(XVAL2-1.0)*2.0 + 1.0
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL3,YVAL,REAL(ITAG)
C
        ELSEIF(KK.EQ.ICNT)THEN
C
C         LAST CLUSTER BEING FORMED
C
          XVAL2=2.0
          DO307JJ=1,NN
            IF(ISEC.EQ.NER(JJ))THEN
              XVAL2=REAL(JJ)
              XVAL2=(XVAL2-1.0)*2.0 + 1.0
              XVAL1=XVAL2 - 1.0
            ENDIF
  307     CONTINUE
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
        ELSE
C
C         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
C         PREVIOUS LA.
C
          IFLAGL=0
          IFLAGR=0
          IFLAG3=0
          DO3030JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
C
C             MATCH WITH PREVIOUS CLUSTER FOUND
C
              ISEC2=LBT(JJ)
              DO3031LL=1,NN
                IF(ISEC2.EQ.NER(LL))THEN
                  XVAL1=REAL(LL)
                  XVAL1=(XVAL1-1.0)*2.0 + 2.0
                ENDIF
                IF(ISEC.EQ.NER(LL))THEN
                  XVAL2=REAL(LL)
                  XVAL2=(XVAL2-1.0)*2.0 + 1.0
                  IFLAG3=1
                ENDIF
 3031         CONTINUE
              IFLAGL=1
              GOTO3039
            ENDIF
 3030     CONTINUE
 3039     CONTINUE
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,3035)KK,IFLAGL,XVAL1,XVAL2,YVAL
 3035       FORMAT('AFTER 3030 LOOP: KK,IFLAGL,XVAL1,XVAL2,YVAL = ',
     1             2I6,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)IFRST,ISEC,ISEC2
 3036       FORMAT('                 IFRST,ISEC,ISEC2 = ',3I6)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
C         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
C
          IF(IFLAGL.EQ.0)THEN
            DO3040JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO3041LL=1,NN
                  IF(IFRST.EQ.NER(LL))THEN
                    XVAL1=REAL(LL)
                    XVAL1=(XVAL1-1.0)*2.0 + 1.0
                  ENDIF
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL1-2.0)*2.0 + 1.0
                  ENDIF
 3041           CONTINUE
                GOTO3049
              ENDIF
 3040       CONTINUE
 3049       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3045)KK,IFLAGR
 3045         FORMAT('AFTER 3040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
 3048         FORMAT('XVAL1,XVAL2,YVAL = ',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ELSEIF(IFLAGL.EQ.1 .AND. IFLAG3.EQ.0)THEN
            DO3050JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO3053LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL2-2.0)*2.0 + 1.0
                  ENDIF
 3053           CONTINUE
                IFLAGR=1
                GOTO3059
              ENDIF
 3050       CONTINUE
 3059       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3055)KK,IFLAGR
 3055         FORMAT('AFTER 3050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ENDIF
C
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ELSE
C
C           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
C           NEW CLUSTER
C
            DO3061JJ=1,NN
              IF(IFRST.EQ.NER(JJ))THEN
                XVAL1=REAL(JJ)
                XVAL1=2.0*XVAL1
              ENDIF
              IF(ISEC.EQ.NER(JJ))THEN
                XVAL2=REAL(JJ)
                XVAL2=2.0*(XVAL2-1.0) + 1.0
              ENDIF
 3061       CONTINUE
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ENDIF
        ENDIF
C
 3020 CONTINUE
C
 2019 CONTINUE
      RETURN
      END 
      SUBROUTINE SPLYT(NN,KWAN,NER,BAN,DYS, 
     1                 NCLUT,LAT,LBT,BANLAT,BANLBT,
     1                 IOUNI2,IOUNI3,IOUNI4,ISUBRO,IBUGA3)
C
CNIST SUBROUTINE SPLYT(NN,MAXNN,KWAN,NER,BAN,MAXHH,DYS,LUB) 
C
C     NN     = NUMBER OF OBJECTS
C     MAXNN  = MAXIMUM NUMBER OF OBJECTS
C     KWAN   = NUMBER OF OBJECTS IN EACH CLUSTER
C     NER    = ORDERING OF OBJECTS
C     BAN    = DISSIMILARITIES BETWEEN CLUSTERS
C     MAXHH  = MAXIMUM NUMBER FO DISSIMILARITIES (DATAPLOT DOES NOT USE)
C     DYS    = VECTOR OF DISSIMILARITIES
C     LUB    = OUTPUT UNIT (DATAPLOT DOES NOT USE)
C
      DIMENSION KWAN(*)
      DIMENSION DYS(*)
      DIMENSION NER(*)
      DIMENSION BAN(*)
      DIMENSION BANLAT(*)
      DIMENSION BANLBT(*)
C
      DIMENSION NCLUT(*)
      DIMENSION LAT(*)
      DIMENSION LBT(*)
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,5)NN
    5   FORMAT('BEGINING OF AVERL: NN = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ICNT=0
C
CC    INITIALIZATION
CC
      NCLU=1
      NHALF=NN*(NN-1)/2+1 
      DO 10 L=1,NN
        KWAN(L)=0 
        BAN(L)=0. 
        NER(L)=L
   10 CONTINUE
      KWAN(1)=NN
      JA=1
CC
CC    COMPUTATION OF DIAMETER OF DATA SET
CC
      CS=0.0
      K=0 
   20 CONTINUE
      K=K+1 
      IF(DYS(K).GT.CS)CS=DYS(K)
      IF(K.LT.NHALF)GO TO 20
CC
CC    PREPARE FOR SPLITTING
CC
   30 CONTINUE
      JB=JA+KWAN(JA)-1
      JMA=JB
CC
CC    SPECIAL CASE OF A PAIR OF OBJECTS
CC
      IF(KWAN(JA).EQ.2)THEN
        KWAN(JA)=1
        KWAN(JB)=1
        JAN=NER(JA) 
        JBN=NER(JB) 
        JAB=MEET(JAN,JBN) 
        BAN(JB)=DYS(JAB)
        GO TO 400 
      ENDIF
CC
CC    FINDING FIRST OBJECT TO BE SHIFTED
CC
      BYGSD=-1. 
      DO 110 L=JA,JB
        LNER=NER(L) 
        SD=0. 
        DO 100 J=JA,JB
          JNER=NER(J) 
          NLJ=MEET(LNER,JNER) 
          SD=SD+DYS(NLJ)
  100   CONTINUE
        IF(SD.LE.BYGSD)GO TO 110
        BYGSD=SD
        LNDSD=L 
  110 CONTINUE
CC
CC    SHIFTING THE FIRST OBJECT
CC
      KWAN(JA)=KWAN(JA)-1 
      KWAN(JB)=1
      IF(JB.NE.LNDSD)THEN
        LCHAN=NER(LNDSD)
        LMM=JB-1
        DO 112 LMMA=LNDSD,LMM 
          LMMB=LMMA+1 
          NER(LMMA)=NER(LMMB) 
  112   CONTINUE
        NER(JB)=LCHAN 
      ENDIF
      SPLYN=0.
      JMA=JB-1
CC
CC    FINDING THE NEXT OBJECT TO BE SHIFTED
CC
  120 CONTINUE
      SPLYN=SPLYN+1.
      REST=JMA-JA 
      BDYFF=-1. 
      DO 150 L=JA,JMA 
        LNER=NER(L) 
        DA=0. 
        DO 130 J=JA,JMA 
          JNER=NER(J) 
          NLJ=MEET(LNER,JNER) 
          DA=DA+DYS(NLJ)
  130   CONTINUE
        DA=DA/REST
        DB=0. 
        JMB=JMA+1 
        DO 140 J=JMB,JB 
          JNER=NER(J) 
          NLJ=MEET(LNER,JNER) 
          DB=DB+DYS(NLJ)
  140   CONTINUE
        DB=DB/SPLYN 
        DYFF=DA-DB
        IF(DYFF.LE.BDYFF)GO TO 150
        BDYFF=DYFF
        JAWAY=L 
  150 CONTINUE
      JMB=JMA+1
CC
CC    SHIFTING THE NEXT OBJECT WHEN NECESSARY
CC
      IF(BDYFF.LE.0.)GO TO 200
      IF(JMA.NE.JAWAY)THEN
        LCHAN=NER(JAWAY)
        LMZ=JMA-1 
        DO 160 LXX=JAWAY,LMZ
          LXXP=LXX+1
          NER(LXX)=NER(LXXP)
  160   CONTINUE
        NER(JMA)=LCHAN
      ENDIF
C
      DO 170 LXX=JMB,JB 
        LXY=LXX-1 
        IF(NER(LXY).LT.NER(LXX))GO TO 180 
        LCHAN=NER(LXY)
        NER(LXY)=NER(LXX) 
        NER(LXX)=LCHAN
  170 CONTINUE
  180 CONTINUE
      KWAN(JA)=KWAN(JA)-1 
      KWAN(JMA)=KWAN(JMB)+1 
      KWAN(JMB)=0 
      JMA=JMA-1 
      JMB=JMA+1 
      IF(JMA.NE.JA)GO TO 120
CC
CC    SWITCH THE TWO PARTS WHEN NECESSARY
CC
  200 CONTINUE
      IF(NER(JA).GE.NER(JMB))THEN
        LXXA=JA 
        DO 220 LGRB=JMB,JB
          LXXA=LXXA+1 
          LCHAN=NER(LGRB) 
          DO 210 LXY=LXXA,LGRB
            LXF=LGRB-LXY+LXXA 
            LXG=LXF-1 
            NER(LXF)=NER(LXG) 
  210     CONTINUE
          NER(LXG)=LCHAN
  220   CONTINUE
        LLQ=KWAN(JMB) 
        KWAN(JMB)=0 
        JMA=JA+JB-JMA-1 
        JMB=JMA+1 
        KWAN(JMB)=KWAN(JA)
        KWAN(JA)=LLQ
      ENDIF
CC
CC    COMPUTE LEVEL FOR BANNER
CC
      IF(NCLU.EQ.1)BAN(JMB)=CS
      IF(NCLU.EQ.1)GO TO 400
      CALL SUPCL(DYS,JA,JB,AREST,NER) 
      BAN(JMB)=AREST
C
  400 CONTINUE
C
      DO1025II=1,NN
        IF(JA.EQ.NER(II))THEN
          AVAL1=BAN(II)
        ELSEIF(JB.EQ.NER(II))THEN
          AVAL2=BAN(II)
        ENDIF
 1025 CONTINUE
      ICNT=ICNT+1
      WRITE(IOUNI4,'(3I5,2E15.7)')NCLU,JA,JB,AVAL1,AVAL2
C
      NCLU=NCLU+1 
      IF(NCLU.EQ.2 .AND. IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
CNIST   WRITE(ICOUT,9000)NN,JMA,KWAN(JMB)
C9000   FORMAT(//22H AT THE FIRST STEP THE,I4,20H OBJECTS ARE DIVIDED,
CNIST1         5H INTO/3X,I4,12H OBJECTS AND,I4,8H OBJECTS)
        WRITE(ICOUT,9000)NN
 9000   FORMAT('AT THE FIRST STEP THE',I4,' OBJECTS ARE DIVIDED INTO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)JMA,KWAN(JMB)
 9001   FORMAT(2X,I4,' OBJECTS AND',I4,' OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NCLU.EQ.NN)GOTO 500
CC
CC    CONTINUE SPLITTING UNTIL ALL OBJECTS ARE SEPARATED
CC
      IF(JB.EQ.NN)GO TO 430 
  420 CONTINUE
      JA=JA+KWAN(JA)
      IF(JA.GT.NN)GO TO 430 
      IF(KWAN(JA).LE.1)GO TO 420
      GO TO 30
  430 CONTINUE
      JA=1
      IF(KWAN(JA).EQ.1)GO TO 420
      GO TO 30
C
  500 CONTINUE
C
      ILOOP=NN/5
      IREM=MOD(NN,5)
      IF(IREM.GT.0)ILOOP=ILOOP+1
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9100) 
 9100   FORMAT('THE FINAL ORDERING OF THE OBJECTS IS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9111II=1,ILOOP
          ISTRT=(II-1)*5 + 1
          ISTOP=II*5
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9110)(NER(L),L=ISTRT,ISTOP)
 9110     FORMAT(5(I9,6X))
          CALL DPWRST('XXX','BUG ')
 9111   CONTINUE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9120) 
 9120   FORMAT('THE DIAMETERS OF THE CLUSTERS ARE')
        CALL DPWRST('XXX','BUG ')
        DO9131II=1,ILOOP
          ISTRT=(II-1)*5 + 2
          ISTOP=II*5
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9130)(BAN(L),L=ISTRT,ISTOP)
 9130     FORMAT(3X,5F15.3) 
          CALL DPWRST('XXX','BUG ')
 9131   CONTINUE
      ENDIF
C
C     NOW CREATE DATA FOR:
C
C         1. DENDOGRAM (IOUNI3)
C         2. ICICLE PLOT (IOUNI2)
C
      REWIND(IOUNI4)
C
      DO2010KK=1,ICNT
        READ(IOUNI4,'(3I5,2E15.7)',END=2019,ERR=2019)
     1      NCLUT(KK),LAT(KK),LBT(KK),BANLAT(KK),BANLBT(KK)
 2010 CONTINUE
C
      ITAG=0
      DO2020KK=1,ICNT
C
C       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
C       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
C       THE APPROPRIATE X-COORDINATE.
C
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        AVAL1=BANLBT(KK)
C
        IF(KK.EQ.1)THEN
C
C         FIRST CLUSTER BEING FORMED
C
          XVAL1=1.0
          XVAL2=2.0
          DO2021JJ=1,NN
            IF(IFRST.EQ.NER(JJ))XVAL1=REAL(JJ)
            IF(ISEC.EQ.NER(JJ))XVAL2=REAL(JJ)
 2021     CONTINUE
          YVAL1=0.0
          YVAL2=AVAL1
          ITAG=ITAG+1
          WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
          WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
          WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
          WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL1,REAL(ITAG)
C
        ELSE
C
C         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
C         PREVIOUS LA.
C
          IFLAGL=0
          IFLAGR=0
          DO2030JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
C
C             MATCH WITH PREVIOUS CLUSTER FOUND
C
              ISEC2=LBT(JJ)
              DO2031LL=1,NN
                IF(ISEC2.EQ.NER(LL))THEN
                  XVAL1=REAL(LL) - 0.5
                  XVAL3=REAL(LL)
                ENDIF
                IF(ISEC.EQ.NER(LL))THEN
                  XVAL2=REAL(LL)
                  XVAL4=XVAL2
                ENDIF
 2031         CONTINUE
              YVAL1=BANLBT(JJ)
              YVAL2=AVAL1
              YVAL3=0.0
              IFLAGL=1
              GOTO2039
            ENDIF
 2030     CONTINUE
 2039     CONTINUE
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,2035)KK,IFLAGL
 2035       FORMAT('AFTER 2030 LOOP: KK,IFLAGL = ',2I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
 2038       FORMAT('XVAL1,XVAL2,YVAL1,YVAL2,YVAL3 = ',5G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
C         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
C
          IF(IFLAGL.EQ.0)THEN
            DO2040JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO2041LL=1,NN
                  IF(IFRST.EQ.NER(LL))THEN
                    XVAL1=REAL(LL)
                    XVAL3=REAL(LL)
                  ENDIF
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL) - 0.5
                    XVAL4=REAL(LL)
                  ENDIF
 2041           CONTINUE
                YVAL1=0.0
                YVAL2=AVAL1
                YVAL3=BANLBT(JJ)
                IFLAGR=1
                GOTO2049
              ENDIF
 2040       CONTINUE
 2049       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2045)KK,IFLAGR
 2045         FORMAT('AFTER 2040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ELSEIF(IFLAGL.EQ.1)THEN
            DO2050JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO2053LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL) - 0.5
                    XVAL4=REAL(LL)
                  ENDIF
 2053           CONTINUE
                YVAL2=AVAL1
                YVAL3=BANLBT(JJ)
                IFLAGR=1
                GOTO2059
              ENDIF
 2050       CONTINUE
 2059       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2055)KK,IFLAGR
 2055         FORMAT('AFTER 2050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ENDIF
C
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL3,REAL(ITAG)
C
          ELSE
C
C           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
C           NEW CLUSTER
C
            DO2061JJ=1,NN
              IF(IFRST.EQ.NER(JJ))THEN
                XVAL1=REAL(JJ)
                XVAL3=REAL(JJ)
              ENDIF
              IF(ISEC.EQ.NER(JJ))THEN
                XVAL2=REAL(JJ)
                XVAL4=REAL(JJ)
              ENDIF
 2061       CONTINUE
            YVAL1=0.0
            YVAL2=AVAL1
            ITAG=ITAG+1
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL1,REAL(ITAG)
C
          ENDIF
        ENDIF
C
 2020 CONTINUE
C
      ITAG=0
      DO3020KK=1,ICNT
C
C       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
C       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
C       THE APPROPRIATE X-COORDINATE.
C
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        NCLU=NCLUT(KK)
        YVAL=REAL(NCLU)
C
        IF(KK.EQ.1)THEN
C
C         FIRST CLUSTER BEING FORMED
C
          XVAL1=1.0
          XVAL2=2.0
          DO3021JJ=1,NN
            IF(IFRST.EQ.NER(JJ))XVAL1=REAL(JJ)
            IF(ISEC.EQ.NER(JJ))XVAL2=REAL(JJ)
 3021     CONTINUE
C
          XVAL1=(XVAL1-1.0)*2.0 + 1.0
          XVAL2=XVAL1 + 1.0
          XVAL3=(XVAL2-1.0)*2.0 + 1.0
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL3,YVAL,REAL(ITAG)
C
        ELSEIF(KK.EQ.ICNT)THEN
C
C         LAST CLUSTER BEING FORMED
C
          XVAL2=2.0
          DO307JJ=1,NN
            IF(ISEC.EQ.NER(JJ))THEN
              XVAL2=REAL(JJ)
              XVAL2=(XVAL2-1.0)*2.0 + 1.0
              XVAL1=XVAL2 - 1.0
            ENDIF
  307     CONTINUE
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
        ELSE
C
C         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
C         PREVIOUS LA.
C
          IFLAGL=0
          IFLAGR=0
          IFLAG3=0
          DO3030JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
C
C             MATCH WITH PREVIOUS CLUSTER FOUND
C
              ISEC2=LBT(JJ)
              DO3031LL=1,NN
                IF(ISEC2.EQ.NER(LL))THEN
                  XVAL1=REAL(LL)
                  XVAL1=(XVAL1-1.0)*2.0 + 2.0
                ENDIF
                IF(ISEC.EQ.NER(LL))THEN
                  XVAL2=REAL(LL)
                  XVAL2=(XVAL2-1.0)*2.0 + 1.0
                  IFLAG3=1
                ENDIF
 3031         CONTINUE
              IFLAGL=1
              GOTO3039
            ENDIF
 3030     CONTINUE
 3039     CONTINUE
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,3035)KK,IFLAGL,XVAL1,XVAL2,YVAL
 3035       FORMAT('AFTER 3030 LOOP: KK,IFLAGL,XVAL1,XVAL2,YVAL = ',
     1             2I6,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)IFRST,ISEC,ISEC2
 3036       FORMAT('                 IFRST,ISEC,ISEC2 = ',3I6)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
C         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
C         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
C
          IF(IFLAGL.EQ.0)THEN
            DO3040JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO3041LL=1,NN
                  IF(IFRST.EQ.NER(LL))THEN
                    XVAL1=REAL(LL)
                    XVAL1=(XVAL1-1.0)*2.0 + 1.0
                  ENDIF
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL1-2.0)*2.0 + 1.0
                  ENDIF
 3041           CONTINUE
                GOTO3049
              ENDIF
 3040       CONTINUE
 3049       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3045)KK,IFLAGR
 3045         FORMAT('AFTER 3040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
 3048         FORMAT('XVAL1,XVAL2,YVAL = ',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ELSEIF(IFLAGL.EQ.1 .AND. IFLAG3.EQ.0)THEN
            DO3050JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
C
C               MATCH WITH PREVIOUS CLUSTER FOUND
C
                ISEC2=LBT(JJ)
                DO3053LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL2-2.0)*2.0 + 1.0
                  ENDIF
 3053           CONTINUE
                IFLAGR=1
                GOTO3059
              ENDIF
 3050       CONTINUE
 3059       CONTINUE
C
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3055)KK,IFLAGR
 3055         FORMAT('AFTER 3050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
              CALL DPWRST('XXX','BUG ')
            ENDIF
C
          ENDIF
C
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ELSE
C
C           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
C           NEW CLUSTER
C
            DO3061JJ=1,NN
              IF(IFRST.EQ.NER(JJ))THEN
                XVAL1=REAL(JJ)
                XVAL1=2.0*XVAL1
              ENDIF
              IF(ISEC.EQ.NER(JJ))THEN
                XVAL2=REAL(JJ)
                XVAL2=2.0*(XVAL2-1.0) + 1.0
              ENDIF
 3061       CONTINUE
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ENDIF
        ENDIF
C
 3020 CONTINUE
C
C
 2019 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9905)
 9905   FORMAT('END OF SPLYT')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE BANAG(NN,BAN,NER,IOUNI5,IAGNBA,ISUBRO,IERROR)
C
CNIST SUBROUTINE BANAG(NN,MAXNN,BAN,NER,LAB,NUM,LUB)
C
C     ORIGINAL ROUTINE USED TO DRAW "BANNER" LINE PRINTER GRAPH FOR
C     AGGLOMERATIVE CLUSTERING ALGORITHM (AGNES).
C
C     NN       = NUMBER OF ROWS IN THE DISSIMILARITY MATRIX
C     MAXNN    = MAXIMUM NUMBER OF ROWS ALLOWED (NOT USED)
C     BAN      = DISSIMILARITIES BETWEEN CLUSTERS
C     NER      = FINAL ORDERING OF THE OBJECTS
C     LAB      = OBJECT LABELS
C     NUM      = LABELING STRING FOR BANNER PLOT
C     LUB      = OUTPUT UNIT FOR PRINTING (NOT USED)
C
      DIMENSION BAN(*)
      DIMENSION NER(*)
C
      CHARACTER*4 IAGNBA
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
CNIST CHARACTER*1 LAB(3,MAXNN)
      CHARACTER*3 LAB
      CHARACTER*1 JDRAW(78)
      CHARACTER*1 NUM(13)
      CHARACTER*1 JBLAN,JSTAR,JSEPA
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      NUM(1)='0'
      NUM(2)='1'
      NUM(3)='2'
      NUM(4)='3'
      NUM(5)='4'
      NUM(6)='5'
      NUM(7)='6'
      NUM(8)='7'
      NUM(9)='8'
      NUM(10)='9'
      NUM(11)=' '
      NUM(12)='*'
      NUM(13)='+'
      JBLAN=NUM(11)
      JSTAR=NUM(12)
      JSEPA=NUM(13)
C
      IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9000) 
 9000   FORMAT(34X,'************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001) 
 9001   FORMAT(34X,'*',10X,'*')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002) 
 9002   FORMAT(34X,'*  BANNER  *')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9000) 
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9200) 
 9200   FORMAT(25('0  '),'1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9201) 
 9201   FORMAT(26('.  '))
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,9210) 
 9210   FORMAT('0  0  0  1  1  2  2  2  3  3  4  4  4  5  5  ', 
     1         '6  6  6  7  7  8  8  8  9  9  0')
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,9220) 
 9220   FORMAT(5('0  4  8  2  6  '),'0')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     SUP = MAXIMUM VALUE IN BAN
C
      SUP=0.0 
      DO 70 K=2,NN
        IF(BAN(K).GT.SUP)SUP=BAN(K)
   70 CONTINUE
C
      AC=0.0
      DO 80 K=1,NN
        KEARL=K 
        IF(K.EQ.1)KEARL=2 
        KAFTE=K+1 
        IF(K.EQ.NN)KAFTE=NN 
        SYZE=BAN(KEARL) 
        IF(BAN(KAFTE).LT.SYZE)SYZE=BAN(KAFTE) 
C
        IF(ISUBRO.EQ.'ANAG')THEN
          WRITE(ICOUT,8001)K,KEARL,KAFTE,SYZE
 8001     FORMAT('K,KEARL,KAFTE,SYZE = ',3I8,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        AC=AC+1.0-(SYZE/SUP)
        LEMPT=INT((SYZE/SUP)*75.0+0.01) 
        IF(LEMPT.NE.0)THEN
          DO 81 L=1,LEMPT 
            JDRAW(L)=JBLAN
   81     CONTINUE
        ENDIF
        LADD=LEMPT+1
        KAUNT=0 
        NCASE=NER(K)
C
        LAB='000'
        IF(NCASE.LE.9)THEN
          WRITE(LAB(3:3),'(I1)')NCASE
        ELSEIF(NCASE.LE.99)THEN
          WRITE(LAB(2:3),'(I2)')NCASE
        ELSE
          WRITE(LAB(1:3),'(I3)')NCASE
        ENDIF
C
        DO 83 L=LADD,78 
          KAUNT=KAUNT+1 
          IF(KAUNT.EQ.5)KAUNT=1 
          IF(KAUNT.EQ.1)JDRAW(L)=LAB(1:1)
          IF(KAUNT.EQ.2)JDRAW(L)=LAB(2:2)
          IF(KAUNT.EQ.3)JDRAW(L)=LAB(3:3)
          IF(KAUNT.EQ.4)JDRAW(L)=JSEPA
   83   CONTINUE
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,78)
 9100     FORMAT(1X,78A1) 
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(K.EQ.NN)GO TO 90 
        SYZE=BAN(KAFTE) 
        LEMPT=INT((SYZE/SUP)*75.0+0.01) 
        IF(LEMPT.EQ.0)GOTO 86 
        DO 85 L=1,LEMPT 
          JDRAW(L)=JBLAN
   85   CONTINUE
   86   CONTINUE
        LADD=LEMPT+1
C
        DO 87 L=LADD,78 
         JDRAW(L)=JSTAR
   87   CONTINUE
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,78)
          CALL DPWRST('XXX','BUG ')
        ENDIF
   80 CONTINUE
C
   90 CONTINUE
C
      IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
        WRITE(ICOUT,9200) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9201) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9220) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9300)SUP
 9300   FORMAT(' THE ACTUAL HIGHEST LEVEL IS   ',F25.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RNN=NN
      AC=AC/RNN 
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9310)AC 
 9310   FORMAT(' THE AGGLOMERATIVE COEFFICIENT OF THIS DATA SET IS  ',
     1         F5.2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END 
      SUBROUTINE BANDY(NN,BAN,NER,IOUNI5,IAGNBA,ISUBRO,IERROR)
C
CNIST SUBROUTINE BANDY(NN,MAXNN,BAN,NER,LAB,NUM,LUB)
C
C     ORIGINAL ROUTINE USED TO DRAW "BANNER" LINE PRINTER GRAPH FOR
C     DIVISIVE CLUSTERING ALGORITHM (DIANA).
C
C     NN       = NUMBER OF ROWS IN THE DISSIMILARITY MATRIX
C     MAXNN    = MAXIMUM NUMBER OF ROWS ALLOWED (NOT USED)
C     BAN      = DISSIMILARITIES BETWEEN CLUSTERS
C     NER      = FINAL ORDERING OF THE OBJECTS
C     LAB      = OBJECT LABELS
C     NUM      = LABELING STRING FOR BANNER PLOT
C     LUB      = OUTPUT UNIT FOR PRINTING (NOT USED)
C
      DIMENSION BAN(*)
      DIMENSION NER(*)
C
CNIST CHARACTER*1 LAB(3,MAXNN),JDRAW(78),NUM(13),JSTAR,JSEPA
      CHARACTER*4 IAGNBA
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*3 LAB
      CHARACTER*1 JDRAW(78)
      CHARACTER*1 NUM(13)
      CHARACTER*1 JBLAN,JSTAR,JSEPA
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      NUM(1)='0'
      NUM(2)='1'
      NUM(3)='2'
      NUM(4)='3'
      NUM(5)='4'
      NUM(6)='5'
      NUM(7)='6'
      NUM(8)='7'
      NUM(9)='8'
      NUM(10)='9'
      NUM(11)=' '
      NUM(12)='*'
      NUM(13)='+'
      JBLAN=NUM(11)
      JSTAR=NUM(12)
      JSEPA=NUM(13)
C
      IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9000) 
 9000   FORMAT(34X,'************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001) 
 9001   FORMAT(34X,'*',10X,'*')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002) 
 9002   FORMAT(34X,'*  BANNER  *')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9000) 
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9200) 
 9200   FORMAT(25('0  '),'1')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9201) 
 9201   FORMAT(26('.  '))
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,9210) 
 9210   FORMAT('0  0  0  1  1  2  2  2  3  3  4  4  4  5  5  ', 
     1         '6  6  6  7  7  8  8  8  9  9  0')
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,9220) 
 9220   FORMAT(5('0  4  8  2  6  '),'0')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     SUP = MAXIMUM VALUE IN BAN
C
      SUP=0.0 
      DO 70 K=2,NN
        IF(BAN(K).GT.SUP)SUP=BAN(K) 
   70 CONTINUE
      DO 71 K=2,NN
        BAN(K)=BAN(K)/SUP
   71 CONTINUE
C
      DC=0.0
      DO 80 K=1,NN
        NCASE=NER(K)
        DO 81 L=1,19
          LALFA=(L-1)*4+1 
          LBETA=(L-1)*4+2 
          LGAMA=(L-1)*4+3 
          LDELT=L*4 
          LAB='000'
          IF(NCASE.LE.9)THEN
            WRITE(LAB(3:3),'(I1)')NCASE
          ELSEIF(NCASE.LE.99)THEN
            WRITE(LAB(2:3),'(I2)')NCASE
          ELSE
            WRITE(LAB(1:3),'(I3)')NCASE
          ENDIF
          JDRAW(LALFA)=LAB(1:1)
          JDRAW(LBETA)=LAB(2:2)
          JDRAW(LGAMA)=LAB(3:3)
          JDRAW(LDELT)=JSEPA
   81   CONTINUE
C
        JDRAW(77)=LAB(1:1)
        JDRAW(78)=LAB(2:2)
        KEARL=K 
        IF(K.EQ.1)KEARL=2 
        KAFTE=K+1 
        IF(K.EQ.NN)KAFTE=NN 
        SYZE=BAN(KEARL) 
        IF(BAN(KAFTE).LT.SYZE)SYZE=BAN(KAFTE) 
        DC=DC+1.0-SYZE
        LENGT=INT((1.0-SYZE)*75.0+0.01)+3 
C
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,LENGT) 
 9100     FORMAT(1X,78A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(K.EQ.NN)GO TO 90 
        SYZE=BAN(KAFTE) 
        LENGT=INT((1.0-SYZE)*75.0+0.01)+3 
        DO 82 L=1,LENGT 
         JDRAW(L)=JSTAR
   82   CONTINUE
C
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,LENGT) 
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
   80 CONTINUE
   90 CONTINUE
C
      IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
        WRITE(ICOUT,9200) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9201) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9220) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9300)SUP
 9300   FORMAT(' THE ACTUAL DIAMETER OF THIS DATA SET IS   ',F25.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RNN=NN
      DC=DC/RNN 
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9310)DC 
 9310   FORMAT(' THE DIVISIVE COEFFICIENT OF THIS DATA SET IS  ',F5.2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DYSTAF(NN,JPP,MAXNN,MAXPP,X,DSS,NDYST,AMISS,JHALT,
     1                  ISUBRO,IBUGA3)
C
CNIST SUBROUTINE DYSTA(NN,JPP,MAXNN,MAXPP,MAXHH,X,DSS,NDYST,
CNISTF JTMD,VALMD,LAB,JHALT,LUB,FNAMEB)
C
C     KAUFFMAN AND ROUSSEEUW CODE FROM FANNY ALGORITHM.  THIS
C     ROUTINE COMPUTES EITHER EUCLIDEAN DISTANCE OR MANHATTAN
C     DISTANCE BETWEEN ALL OBJECTS (FANNY VERSION).
C
C       NN      = NUMBER OF SAMPLES
C       JPP     = NUMBER OF VARIABLES
C       MAXNNN  = THE ROW DIMENSION OF X
C       MAXPP   = THE COLUMN DIMENSION OF X
C       MAXHH   = THE MAXIMUM DIMENSION FOR THE DISTANCES
C                 (DATAPLOT DOES NOT USE)
C       X       = THE DATA MATRIX
C       DSS     = THE OUTPUT MATRIX CONTAINING THE DISTANCES
C       NDYST   = 1 => EUCLIDEAN DISTANCES
C                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
C       JTMD    = FOR MISSING VALUES, WE DON'T USE
C       VALMD   = FOR MISSING VALUES, WE DON'T USE
C       LAB     = OBJECT LABELS
C                 (DATAPLOT AUTOMATICALLY USES ROW-ID)
C       JHALT   = SET TO 1 FOR ERROR CONDITION
C       LUB     = OUTPUT UNIT
C                 (DATAPLOT DOES NOT USE)
C       FNAMEB  = OUTPUT FILE NAME
C                 (DATAPLOT DOES NOT USE)
C
C     CHANGES FOR INCORPORATING INTO DATAPLOT:
C
C        1. USE DATAPLOT I/O ROUTINES
C        2. FOR DATAPLOT, ONLY USE A SINGLE VALUE TO DENOTE
C           MISSING VALUES
C        3. RECODED SLIGHTLY TO REDUCE USE OF GO TO's (THIS
C           WAS JUST TO IMPROVE READABILITY OF THE CODE)
C
      DIMENSION X(MAXNN,MAXPP)
      DIMENSION DSS(*)
CNIST DIMENSION JTMD(MAXPP)
CNIST DIMENSION VALMD(MAXPP)
CNIST CHARACTER LAB(3,MAXNN)
CNIST CHARACTER*30 FNAMEB
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DYSTAF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NN,JPP,AMISS
   55   FORMAT('NN,JPP,AMISS  = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO58I=1,NN
          WRITE(ICOUT,59)I,(X(I,J),J=1,JPP)
   59     FORMAT('I,X(I,J) = ',I8,30G15.7)
          CALL DPWRST('XXX','BUG ')
   58   CONTINUE
      ENDIF
C
      JHALT=0
      PP=JPP
      NNSUB=NN-1
      NLK=0 
      DO 100 L=1,NNSUB
        LPLUS=L+1 
        DO 20 K=LPLUS,NN  
          CLK=0.0 
          NLK=NLK+1 
          NPRES=0 
          DO 30 J=1,JPP 
CNIST       IF(JTMD(J).GE.0)GOTO 40 
CNIST       IF(X(L,J).EQ.VALMD(J))GOTO 30 
CNIST       IF(X(K,J).EQ.VALMD(J))GOTO 30 
            IF(X(K,J).EQ.AMISS)GOTO30
CNI40       CONTINUE
            NPRES=NPRES+1 
            IF(NDYST.NE.1)THEN
              CLK=CLK+ABS(X(L,J)-X(K,J))
            ELSE
              CLK=CLK+(X(L,J)-X(K,J))*(X(L,J)-X(K,J)) 
            ENDIF
   30       CONTINUE
            RPRES=NPRES 
            IF(NPRES.EQ.0)THEN
              JHALT=1 
CNIST         WRITE(LUB,9400)LAB(1,L),LAB(2,L),LAB(3,L),
CNIST1                       LAB(1,K),LAB(2,K),LAB(3,K)
C9400         FORMAT('  OBJECTS ',3A1,' AND ',3A1)
CNIST1               ' HAVE NO COMMON MEASUREMENTS')
CNIST         IF(FNAMEB.NE.'CON')WRITE(*,9400)LAB(1,L),LAB(2,L),LAB(3,L),
CNIST1                                        LAB(1,K),LAB(2,K),LAB(3,K)
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9401)L,K
 9401       FORMAT('***** OBJECTS ',I8,' AND ',I8,' HAVE NO ',
     1             'COMMON MEASURE, SO')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9403)
 9403       FORMAT('      THE DISTANCE BETWEEN THEM CANNOT BE ',
     1             'COMPUTED.')
            CALL DPWRST('XXX','BUG ')
            DSS(NLK)=0.0
            GOTO 20 
          ENDIF
          IF(NDYST.NE.1)THEN
            DSS(NLK)=CLK*(PP/RPRES) 
          ELSE
            DSS(NLK)=SQRT(CLK*(PP/RPRES)) 
          ENDIF
   20   CONTINUE
  100 CONTINUE
C
      RETURN
      END 
CC  
CC  
      SUBROUTINE CADDY(NN,MAXNN,P,K,KTRUE,
     1                 NFUZZ,NCLUV,RDRAW,NELEM,EDA,EDB,
     1                 IOUNI1,IOUNI2,IBUGA3,ISUBRO)
C
CNIST SUBROUTINE CADDY(NN,MAXNN,MAXKK,P,LAB,K,KTRUE,LUB,
CNIST1                 NFUZZ,NCLUV,RDRAW,NELEM,EDA,EDB)
C
      DIMENSION P(MAXNN,*)
      DIMENSION RDRAW(*)
      DIMENSION NCLUV(*)
      DIMENSION NELEM(*)
      DIMENSION NFUZZ(*)
      CHARACTER JDRAW(30)
CNIST CHARACTER LAB(3,MAXNN)
      CHARACTER*3 LAB
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADDY')THEN
        WRITE(ICOUT,5)
    5   FORMAT('AT THE BEGINNING OF CADDY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)NN,MAXNN
    6   FORMAT('NN,MAXNN = ',2I6)
        CALL DPWRST('XXX','BUG ')
        DO7II=1,NN
          WRITE(ICOUT,8)II,(P(II,JJ),JJ=1,MIN(K,10))
    8     FORMAT('II,(P(II,JJ),JJ=1,K) = ',I6,10G15.7)
          CALL DPWRST('XXX','BUG ')
    7   CONTINUE
      ENDIF
C
      PBEST=P(1,1)
      NBEST=1
C
      DO 10 L=2,K 
        IF(P(1,L).LE.PBEST)GOTO10
          PBEST=P(1,L)
          NBEST=L 
   10 CONTINUE
C
      NFUZZ(1)=NBEST
      NCLUV(1)=1
      KTRUE=1 
C
      DO 20 M=2,NN
        PBEST=P(M,1)
        NBEST=1 
        DO 30 L=2,K 
         IF(P(M,L).LE.PBEST)GOTO30
           PBEST=P(M,L)
           NBEST=L 
   30   CONTINUE
C
        JSTAY=0 
        DO 40 KTRY=1,KTRUE
          IF(NFUZZ(KTRY).NE.NBEST)GO TO 40
            NCLUV(M)=KTRY 
            JSTAY=1 
   40   CONTINUE
C
        IF(JSTAY.EQ.1)GO TO 20
        KTRUE=KTRUE+1 
        NFUZZ(KTRUE)=NBEST
        NCLUV(M)=KTRUE
   20 CONTINUE
C
      IF(KTRUE.GE.K)GO TO 100 
C
      KNEXT=KTRUE+1 
      DO 60 KWALK=KNEXT,K 
        DO 70 KLEFT=1,K 
          JSTAY=0 
          KSUP=KWALK-1
          DO 80 KTRY=1,KSUP 
            IF(NFUZZ(KTRY).NE.KLEFT)GO TO 80
            JSTAY=1 
   80     CONTINUE
          IF(JSTAY.NE.1)THEN
            NFUZZ(KWALK)=KLEFT
            GO TO 60
          ENDIF
   70   CONTINUE
   60 CONTINUE
C
  100 CONTINUE
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9210) 
 9210   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9200)
 9200   FORMAT('FUZZY CLUSTERING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9201)
 9201   FORMAT('****************') 
        CALL DPWRST('XXX','BUG ')
        ILOOP=K/10
        IF(MOD(K,10).GT.0)ILOOP=ILOOP+1
        DO9205II=1,ILOOP
          ISTRT=(II-1)*10 + 1
          ISTOP=II*10
          IF(ISTOP.GT.K)ISTOP=K
          WRITE(ICOUT,9202)(L,L=ISTRT,ISTOP)
 9202     FORMAT(3X,10I7) 
          CALL DPWRST('XXX','BUG ')
 9205   CONTINUE
      ENDIF
C
      DO 110 M=1,NN 
        DO 120 L=1,K
          LFUZZ=NFUZZ(L)
          RDRAW(L)=P(M,LFUZZ) 
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADDY')THEN
            WRITE(ICOUT,121)M,L,LFUZZ,RDRAW(L)
  121       FORMAT('M,L,LFUZZ,RDRAW(L) = ',3I6,F7.4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
  120   CONTINUE
C
        IF(IPRINT.EQ.'ON')THEN
          LAB='000'
          IF(M.LE.9)THEN
            WRITE(LAB(3:3),'(I1)')M
          ELSEIF(M.LE.99)THEN
            WRITE(LAB(2:3),'(I2)')M
          ELSEIF(M.LE.999)THEN
            WRITE(LAB(1:3),'(I3)')M
          ENDIF
C
          ILOOP=K/10
          IF(MOD(K,10).GT.0)ILOOP=ILOOP+1
          DO9225II=1,ILOOP
            ISTRT=(II-1)*10 + 1
            ISTOP=II*10
            IF(ISTOP.GT.K)ISTOP=K
            IF(II.EQ.1)THEN
              WRITE(ICOUT,9220)LAB,(RDRAW(L),L=ISTRT,ISTOP)
 9220         FORMAT(A3,1X,10F7.4)
              CALL DPWRST('XXX','BUG ')
            ELSE
              WRITE(ICOUT,9221)(RDRAW(L),L=ISTRT,ISTOP)
 9221         FORMAT(4X,10F7.4)
              CALL DPWRST('XXX','BUG ')
            ENDIF
 9225     CONTINUE
        ENDIF
C
        WRITE(IOUNI1,9228)M,(RDRAW(L),L=1,K)
 9228   FORMAT(I5,30F10.4)
C
  110 CONTINUE
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9300)EDA
 9300   FORMAT('PARTITION COEFFICIENT OF DUNN = ',F5.2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9301)EDB
 9301   FORMAT('ITS NORMALIZED VERSION        = ',F5.2)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9230) 
 9230   FORMAT(' CLOSEST HARD CLUSTERING')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9231) 
 9231   FORMAT('************************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(KTRUE.LT.K)THEN
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9210) 
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9240)
 9240     FORMAT('FOR THIS HARD CLUSTERING, IT TURNS OUT THAT')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9241)KTRUE
 9241     FORMAT('ONLY THE FIRST ',I4,' CLUSTERS ARE NONEMPTY.') 
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9210) 
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9210) 
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9250) 
 9250   FORMAT('CLUSTER NUMBER    SIZE    OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      DO 160 NUMCL=1,KTRUE
        NTT=0 
        DO 150 J=1,NN 
          IF(NCLUV(J).NE.NUMCL)GO TO 150
          NTT=NTT+1 
          NELEM(NTT)=J
  150   CONTINUE
        NSS=NTT 
        IF(NSS.GT.10)NSS=10 
        DO 152 L=1,NSS
          LEEN=3*(L-1)+1
          LTWE=3*(L-1)+2
          LDRE=3*L
          NCASE=NELEM(L)
          LAB='000'
          IF(NCASE.LE.9)THEN
            WRITE(LAB(3:3),'(I1)')NCASE
          ELSEIF(NCASE.LE.99)THEN
            WRITE(LAB(2:3),'(I2)')NCASE
          ELSEIF(NCASE.LE.999)THEN
            WRITE(LAB(1:3),'(I3)')NCASE
          ENDIF
          JDRAW(LEEN)=LAB(1:1)
          JDRAW(LTWE)=LAB(2:2)
          JDRAW(LDRE)=LAB(3:3)
  152   CONTINUE
C
        NSSDR=NSS*3 
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9210) 
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9260)NUMCL,NTT,(JDRAW(LL),LL=1,NSSDR) 
 9260     FORMAT(5X,I5,5X,I6,5X,10(3A1,1X))
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(NTT.LE.10)GO TO 160
        KAUNT=0 
        DO 154 L=11,NTT 
          KAUNT=KAUNT+1 
          LEEN=3*(KAUNT-1)+1
          LTWE=3*(KAUNT-1)+2
          LDRE=3*KAUNT
          NCASE=NELEM(L)
          LAB='000'
          IF(NCASE.LE.9)THEN
            WRITE(LAB(3:3),'(I1)')NCASE
          ELSEIF(NCASE.LE.99)THEN
            WRITE(LAB(2:3),'(I2)')NCASE
          ELSEIF(NCASE.LE.999)THEN
            WRITE(LAB(1:3),'(I3)')NCASE
          ENDIF
          JDRAW(LEEN)=LAB(1:1)
          JDRAW(LTWE)=LAB(2:2)
          JDRAW(LDRE)=LAB(3:3)
          IF(KAUNT.EQ.10)THEN
            IF(IPRINT.EQ.'ON')THEN
              WRITE(ICOUT,9270)(JDRAW(LL),LL=1,30)
 9270         FORMAT(27X,10(3A1,1X))
              CALL DPWRST('XXX','BUG ')
              KAUNT=0
            ENDIF
          ENDIF
  154   CONTINUE
C
        IF(KAUNT.GE.1 .AND. IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9270)(JDRAW(LL),LL=1,LDRE)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  160 CONTINUE
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9280) 
 9280   FORMAT('CLUSTERING VECTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9281) 
 9281   FORMAT('*****************')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9210) 
        CALL DPWRST('XXX','BUG ')
C
        ILOOP=NN/20
        IF(MOD(NN,20).GT.0)ILOOP=ILOOP+1
        DO9295II=1,ILOOP
          ISTRT=(II-1)*20 + 1
          ISTOP=II*20
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9290)(NCLUV(J),J=ISTRT,ISTOP)
 9290     FORMAT(10X,20I3)
          CALL DPWRST('XXX','BUG ')
 9295  CONTINUE
C
      ENDIF
C
      DO9296II=1,NN
        WRITE(IOUNI2,'(I5)')NCLUV(II)
 9296 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADDY')THEN
        WRITE(ICOUT,9910)
 9910   FORMAT('AT THE END OF CADDY')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE FUZZY(NN,MAXNN,P,DP,PT,DSS,ESP,EF,EDA,EDB,K,
     1                 IBUGA3,ISUBRO)
C
CNIST SUBROUTINE FUZZY(NN,MAXNN,MAXKK,MAXHH,P,DP,PT,LAB,DSS,ESP,EF,
CNIST1                 EDA,EDB,K,LUB)
C
      DIMENSION P(MAXNN,*),DP(MAXNN,*)
      DIMENSION DSS(*),PT(*),ESP(*),EF(*)
CNIST CHARACTER LAB(3,MAXNN)
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
C
      CHARACTER*3 LAB
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
        WRITE(ICOUT,10)
   10   FORMAT('AT THE BEGINNING OF FUZZY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11)NN,MAXNN,K
   11   FORMAT('NN,MAXNN,K = ',3I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
CC
CC     R IS THE EXPONENT, STRICTLY LARGER THAN 1.0
CC     EPS IS THE PRECISION FOR THE ITERATIONS
CC     NYT IS THE MAXIMAL NUMBER OF ITERATIONS
CC
      R=2.0
      EPS=0.000001
      NYT=500
CC
CC   INITIAL FUZZY CLUSTERING 
CC
      NNSUB=NN-1
      RVERS=1./R
      RKME=REAL(K-1)
      DO 30 M=1,NN
        DO 20 L=1,K
          DP(M,L)=0.
          P(M,L)=0.1/RKME 
   20   CONTINUE
   30 CONTINUE
C
      NDK=NN/K
      ND=NDK
      L=1 
      DO 50 M=1,NN
        P(M,L)=0.9
        IF(M.GE.ND)THEN
          ND=ND+NDK 
          L=L+1 
          IF(L.EQ.K)ND=NN 
        ENDIF
        DO 40 LX=1,K
          P(M,LX)=P(M,LX)**R
   40   CONTINUE
   50 CONTINUE
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999) 
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999) 
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9110) 
 9110   FORMAT(' ITERATION     OBJECTIVE FUNCTION')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999) 
        CALL DPWRST('XXX','BUG ')
      ENDIF
CC
CC   INITIAL CRITERION VALUE
CC
      CRYT=0. 
      DO 100 L=1,K
        ESP(L)=0.
        EF(L)=0.
        DO 90 M=1,NN
          ESP(L)=ESP(L)+P(M,L)
C
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
            WRITE(ICOUT,91)L,M,P(M,L),ESP(L)
   91       FORMAT('AT DO 100: L,M,P(M,L),ESP(L) = ',2I5,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
C
          DO 80 J=1,NN
            IF(J.EQ.M)GO TO 80
              J2=MIN0(M,J)
              J1=(J2-1)*NN-(J2*(J2+1))/2+MAX0(M,J)
              DP(M,L)=DP(M,L)+P(J,L)*DSS(J1)
              EF(L)=EF(L)+P(J,L)*P(M,L)*DSS(J1)
C
              IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
                WRITE(ICOUT,82)J,J1,J2,DSS(J1),DP(M,L),EF(L)
   82           FORMAT('AT DO 80: J,J1,J2,DSS(J1),DP(M,L),EF(L)=',
     1                3I5,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
   80     CONTINUE
   90   CONTINUE
        CRYT=CRYT+EF(L)/(ESP(L)*2.)
  100 CONTINUE
      CRT=CRYT
      REEN=1./(R-1.)
CC
CC   START OF ITERATIONS
CC
      KAUNT=1 
      M=0 
CC
CC   THE NEW MEMBERSHIP COEFFICIENTS OF THE OBJECTS ARE CALCULATED, 
CC   AND THE RESULTING VALUE OF THE CRITERION IS COMPUTED.
CC
  200 CONTINUE
      M=M+1 
      DT=0. 
      DO 210 L=1,K
        PT(L)=((2.*ESP(L)*ESP(L))/(2.*ESP(L)*DP(M,L)-EF(L)))**REEN
        DT=DT+PT(L) 
  210 CONTINUE
C
      XX=0.
      DO 220 L=1,K
        PT(L)=PT(L)/DT
        IF(PT(L).LE.0.)XX=XX+PT(L)
  220 CONTINUE
C
      DO 240 L=1,K
        IF(PT(L).LE.0.)PT(L)=0.
        PT(L)=(PT(L)/(1.0-XX))**R
        ESP(L)=ESP(L)+PT(L)-P(M,L)
        DO 230 J=1,NN
          IF(J.EQ.M)GO TO 230
            J2=MIN0(M,J)
            J1=(J2-1)*NN-(J2*(J2+1))/2+MAX0(M,J)
            DDD=(PT(L)-P(M,L))*DSS(J1)
            DP(J,L)=DP(J,L)+DDD
            EF(L)=EF(L)+2.*P(J,L)*DDD
  230   CONTINUE
        P(M,L)=PT(L)
  240 CONTINUE
C
      IF(M.LT.NN)GO TO 200
      CRYT=0.
      EDA=0.
      DO 250 L=1,K
        ANN=NN
        EDA=EDA+ESP(L)/ANN
        CRYT=CRYT+EF(L)/(ESP(L)*2.)
  250 CONTINUE
CC
CC   CRITERION IS PRINTED AND TESTED FOR CONVERGENCE
CC
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9120)KAUNT,CRYT 
 9120   FORMAT(I5,11X,F11.4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF((CRT/CRYT-1.).LE.EPS)GO TO 500
      IF(KAUNT.LT.NYT)THEN
        M=0
        KAUNT=KAUNT+1
        CRT=CRYT
        GO TO 200
      ENDIF
C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9130)NYT
 9130   FORMAT('The maximum number of iterations (',I3,
     1         ') has been reached.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9131)
 9131   FORMAT('The iterative procedure is therefore interrupted.')
        CALL DPWRST('XXX','BUG ')
        GO TO 500
      ENDIF
CC
CC   NON-FUZZYNESS INDEX OF LIBERT IS COMPUTED
CC
  500 CONTINUE
      SMALL=1.
      FL=0. 
      DO 410 MM=1,NN
        BBB=P(MM,1)**RVERS
        DO 400 J=2,K
          AAA=P(MM,J)**RVERS
          IF(AAA.GT.BBB)BBB=AAA 
  400   CONTINUE
        IF(BBB.LT.SMALL)SMALL=BBB 
        FL=FL+BBB 
  410 CONTINUE
C
      RNN=NN
      FL=(FL/RNN+SMALL)/2.
      RK=K
      FL=(RK*FL-1.)/(RK-1.) 
C
CC    IF(IPRINT.EQ.'ON')THEN
CC      WRITE(ICOUT,999)
CC      CALL DPWRST('XXX','BUG ')
CC      WRITE(ICOUT,9135)FL
C9135   FORMAT('NON-FUZZYNESS INDEX OF LIBERT = ',F5.2)
CC      CALL DPWRST('XXX','BUG ')
CC      WRITE(ICOUT,999)
CC      CALL DPWRST('XXX','BUG ')
CC    ENDIF
C
      ZK=K
      EDB=(ZK*EDA-1.)/(ZK-1.)
      DO 520 M=1,NN
        DO 510 L=1,K
          P(M,L)=P(M,L)**RVERS
  510   CONTINUE
  520 CONTINUE
C
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
        WRITE(ICOUT,9910)
 9910   FORMAT('AT THE END OF FUZZY')
        CALL DPWRST('XXX','BUG ')
        DO9911II=1,NN
          WRITE(ICOUT,9912)II,(P(II,JJ),JJ=1,K)
 9912     FORMAT('II,(P(II,JJ),JJ=1,K) = ',I6,20F7.4)
          CALL DPWRST('XXX','BUG ')
 9911   CONTINUE
      ENDIF
C
      RETURN
      END

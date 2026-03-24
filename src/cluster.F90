!     NOTE THAT THERE ARE MANY APPROACHES AND ALGORITHMS FOR
!     PERFORMING CLUSTER ANALYSIS.  THIS FILE CONTAINS ROUTINES
!     FROM THE FOLLOWING SOURCES:
!
!     1) JOHN HARTIGAN (1979), "ALGORITHM AS 136", APPLIED
!        STATISTICS, VOL. 28, NO. 1.
!
!     2) JOHN HARTIGAN (1975), "CLUSTERING ALGORITHMS",
!        WILEY.
!
!        WE ACTUALLY USE THE ROUTINES AS EXTRACTED FROM THE
!        CMLIB LIBRARY.  THESE ARE SLIGHTLY DIFFERENT THAN THE
!        ROUTINES AS GIVEN IN THE BOOK.
!
!        WE DO INCLUDE ALL THE ROUTINES.  ALSO, WE USE THE
!        K-MEANS ALGORITHM FROM APPLIED STATISTICS RATHER THAN
!        THE VERSION FROM THE BOOK (THE APPLIED STATISTICS
!        VERSION IS SOMEWHAT SIMPLER THAN THE BOOK VERSION).
!
!     3) KAUFMAN AND ROUSSEEUW (1990), "FINDING GROUPS IN
!        DATA", WILEY.
!
!        THESE ROUTINES ARE DESIGNED TO BE MORE ROBUST THAN
!        SOME OF THE STANDARD CLUSTERING ALGORITHMS.
!
!        SPECIFICALLY, WE SUPPORT THE K-MEDOIDS METHODS FROM
!        THE "PAM" AND "CLARA" ALGORITHMS.
!
!        KAUFMAN AND ROUSSEEUW SUPPORT THE FOLLOWING PROGRAMS:
!
!        1. DAISY  - FOR CREATING DISSIMILARITY MATRICES
!                    (DATAPLOT HAS COMMANDS TO DO WHAT DAISY
!                    CAN DO, SO NOTHING EXPLICITLY IMPLEMENTED
!                    FROM DAISY)
!        2. PAM    - PARTITIONING AROUND MEDOIDS BASED ON EITHER
!                    MEASUREMENT DATA OR A DISSIMILARITY MATRIX.
!                    CURRENTLY LIMITED TO A MAXIMUM OF 100 OBJECTS.
!        3. CLARA  - PARTITIONING AROUND MEDOIDS FOR THE CASE OF MORE
!                    THAN 100 OBJECTS.  ONLY APPLIED TO MEASUREMENT
!                    DATA.
!        4. FANNY  - FUZZY CLUSTERING.  NOT CURRENTLY IMPLEMENTED.
!        5. AGNES  - HIERARCHIAL CLUSTERING (OR AGGLOMERATIVE NESTING).
!        6. MONA   - HIERARCHIAL CLUSTERING FOR BINARY DATA.  NOT
!                    CURRENTLY SUPPORTED.
!
!     ALSO INCLUDE ROUTINES FROM APPLIED STATISTICS 136
!     SPECIFICALLY FOR K-MEANS CLUSTERING.
!
!     LIST OF ROUTINES:
!
!     HARTIGAN'S K-MEANS FROM APPLIED STATISTICS 136:
!
!        1. KMNS   - THE ROUTINE THAT IS CALLED FROM DATAPLOT
!        2. OPTRA  - OPTIMAL TRANSFER STAGE
!        3. QTRAN  - QUICK TRANSFER STAGE
!
!     ADDITIONAL HARTIGAN CLUSTERING CODES.  CURRENTLY WE USE
!     "MIX" FOR NORMAL MIXTURE MODELS AND SLINK FOR SINGLE
!     LINKAGE (NEAREST NEIGHBOR) CLUSTERING.  NOT ALL OF THESE
!     ROUTINES ARE CURRENTLY ACTIVELY USED.
!
!         1. MIX    - PERFORMS NORMAL MIXTURE CLUSTERING
!         2. COVOUT - USED BY MIX TO PRINT OUTPUT
!         3. INVERT - USED BY MIX TO INVERT A MATRIX
!         4. CLUMOM - USED BY MIX TO COMPUTE WEIGHTED MEANS AND
!                     STANDARD DEVIATIONS
!
!         5. MIXIND - NORMAL MIXTURE WITH SPECIFIC COVARIANCE
!                     MODEL (NOT CURRENTLY USED)
!         6. MIXOUT - PRINT OUTPUT OF MIXIND
!
!        FOLLOWING ARE NOT ACTIVELY CALLED
!
!         7. SLINK  - PERFORMS SINGLE LINKAGE CLUSTERING
!         8. BUILD  - K-MEANS FROM CMLIB
!         9. KMEANS - (CALLED BY BUILD)
!        10. SINGLE - CALLED BY BUILD/KMEANS
!        11. KOUT   - CALLED BY BUILD/KMEANS
!
!        12. SPLIT1 - SPLITTING ALGORITHM FOR CLUSTERING
!                     (NOT CURRENTLY USED)
!        13. SPLIT2 - SPLITTING ALGORITHM FOR CLUSTERING
!                     (NOT CURRENTLY USED)
!        14. CSPLIT - USED BY SPLIT1/SPLIT2
!        15. RSPLIT - USED BY SPLIT1/SPLIT2
                                                                                                                                  
!        16. QUICK  - PERFORMS A "QUICK" CLUSTERING (NOT
!                    CURRENTLY USED)
!
!     ROUTINES FROM KAUFFMAN AND ROUSSEEUW
!
!        1. BSWAP  (FOR CLARA AND PAM)
!        2. DYSTA  (FOR CLARA)
!        3. DYSTAP (FOR PAM)
!        4. DYSTAF (FOR FANNY)
!        5. MEET   (FOR CLARA AND PAM)
!        6. RESUL  (FOR CLARA)
!        7. SELEC  (FOR CLARA)
!        8. CSTAT  (PAM)
!        9. SUPCL  (AGNES)
!       10. AVERL  (AGNES)
!       11. BANAG  (AGNES)
!       12. SPLYT  (DIANA)
!       13. BANDY  (DIANA)
!       14. CADDY  (FANNY)
!       15. FUZZY  (FANNY)
!
      SUBROUTINE KMNS(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,   &
          ITRAN, LIVE, ITER, WSS, IFAULT)
!
!     ALGORITHM AS 136  APPL. STATIST. (1979) VOL.28, NO.1
!
!     Divide M points in N-dimensional space into K clusters so that
!     the within cluster sum of squares is minimized.
!
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), WSS(K), DT(2)
      REAL    ZERO, ONE
!
!     Define BIG to be a very large positive number
!
      DATA BIG /1.E30/, ZERO /0.0/, ONE /1.0/
!
      IFAULT = 3
      IF (K .LE. 1 .OR. K .GE. M) RETURN
      IFAULT = 0
!
!     For each point I, find its two closest centres, IC1(I) and
!     IC2(I).     Assign it to IC1(I).
!
      DO 50 I = 1, M
        IC1(I) = 1
        IC2(I) = 2
        DO 10 IL = 1, 2
          DT(IL) = ZERO
          DO 11 J = 1, N
            DA = A(I,J) - C(IL,J)
            DT(IL) = DT(IL) + DA*DA
   11     CONTINUE
   10   CONTINUE
        IF (DT(1) .GT. DT(2)) THEN
          IC1(I) = 2
          IC2(I) = 1
          TEMP = DT(1)
          DT(1) = DT(2)
          DT(2) = TEMP
        END IF
        DO 55 L = 3, K
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
   55   CONTINUE
   50 CONTINUE
!
!     Update cluster centres to be the average of points contained
!     within them.
!
      DO 70 L = 1, K
         NC(L) = 0
         DO 60 J = 1, N
            C(L,J) = ZERO
   60    CONTINUE
   70 CONTINUE
      DO 90 I = 1, M
         L = IC1(I)
         NC(L) = NC(L) + 1
         DO 80 J = 1, N
           C(L,J) = C(L,J) + A(I,J)
   80    CONTINUE
   90 CONTINUE
!
!     Check to see if there is any empty cluster at this stage
!
      DO 120 L = 1, K
         IF (NC(L) .EQ. 0) THEN
            IFAULT = 1
            RETURN
         END IF
         IFAULT = 0
         AA = NC(L)
         DO 110 J = 1, N
            C(L,J) = C(L,J) / AA
  110    CONTINUE
!
!        Initialize AN1, AN2, ITRAN & NCP
!        AN1(L) = NC(L) / (NC(L) - 1)
!        AN2(L) = NC(L) / (NC(L) + 1)
!        ITRAN(L) = 1 if cluster L is updated in the quick-transfer
!                   stage,
!                 = 0 otherwise
!        In the optimal-transfer stage, NCP(L) stores the step at which
!        cluster L is last updated.
!        In the quick-transfer stage, NCP(L) stores the step at which
!        cluster L is last updated plus M.
!
         AN2(L) = AA / (AA + ONE)
         AN1(L) = BIG
         IF (AA .GT. ONE) AN1(L) = AA / (AA - ONE)
         ITRAN(L) = 1
         NCP(L) = -1
  120 CONTINUE
      INDX = 0
      DO 140 IJ = 1, ITER
!
!     In this stage, there is only one pass through the data.   Each
!     point is re-allocated, if necessary, to the cluster that will
!     induce the maximum reduction in within-cluster sum of squares.
!
      CALL OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,   &
              ITRAN, LIVE, INDX)
!
!     Stop if no transfer took place in the last M optimal transfer
!     steps.
!
      IF (INDX .EQ. M) GO TO 150
!
!     Each point is tested in turn to see if it should be re-allocated
!     to the cluster to which it is most likely to be transferred,
!     IC2(I), from its present cluster, IC1(I).   Loop through the
!     data until no further change is to take place.
!
      CALL QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,   &
             ITRAN, INDX)
!
!     If there are only two clusters, there is no need to re-enter the
!     optimal transfer stage.
!
      IF (K .EQ. 2) GO TO 150
!
!     NCP has to be set to 0 before entering OPTRA.
!
      DO 130 L = 1, K
         NCP(L) = 0
  130 CONTINUE
  140 CONTINUE
!
!     Since the specified number of iterations has been exceeded, set
!     IFAULT = 2.   This may indicate unforeseen looping.
!
      IFAULT = 2
!
!     Compute within-cluster sum of squares for each cluster.
!
  150 CONTINUE
      DO 160 L = 1, K
         WSS(L) = ZERO
         DO 165 J = 1, N
            C(L,J) = ZERO
  165    CONTINUE
  160 CONTINUE
      DO 170 I = 1, M
         II = IC1(I)
         DO 175 J = 1, N
            C(II,J) = C(II,J) + A(I,J)
  175    CONTINUE
  170 CONTINUE
      DO 190 J = 1, N
         DO 180 L = 1, K
           C(L,J) = C(L,J) / FLOAT(NC(L))
  180    CONTINUE
         DO 195 I = 1, M
            II = IC1(I)
            DA = A(I,J) - C(II,J)
           WSS(II) = WSS(II) + DA*DA
  195    CONTINUE
  190 CONTINUE
!
      RETURN
      END SUBROUTINE KMNS
!
!
      SUBROUTINE OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,   &
            ITRAN, LIVE, INDX)
!
!     ALGORITHM AS 136.1  APPL. STATIST. (1979) VOL.28, NO.1
!
!     This is the optimal transfer stage.
!
!     Each point is re-allocated, if necessary, to the cluster that
!     will induce a maximum reduction in the within-cluster sum of
!     squares.
!
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), ZERO, ONE
!
!     Define BIG to be a very large positive number.
!
      DATA BIG /1.0E30/, ZERO /0.0/, ONE/1.0/
!
!     If cluster L is updated in the last quick-transfer stage, it
!     belongs to the live set throughout this stage.   Otherwise, at
!     each step, it is not in the live set if it has not been updated
!     in the last M optimal transfer steps.
!
      DO 10 L = 1, K
      IF (ITRAN(L) .EQ. 1) LIVE(L) = M + 1
   10 CONTINUE
      DO 100 I = 1, M
      INDX = INDX + 1
      L1 = IC1(I)
      L2 = IC2(I)
      LL = L2
!
!     If point I is the only member of cluster L1, no transfer.
!
      IF (NC(L1) .EQ. 1) GO TO 90
!
!     If L1 has not yet been updated in this stage, no need to
!     re-compute D(I).
!
      IF (NCP(L1) .EQ. 0) GO TO 30
      DE = ZERO
      DO 20 J = 1, N
        DF = A(I,J) - C(L1,J)
        DE = DE + DF*DF
   20   CONTINUE
      D(I) = DE * AN1(L1)
!
!     Find the cluster with minimum R2.
!
   30   DA = ZERO
      DO 40 J = 1, N
        DB = A(I,J) - C(L2,J)
        DA = DA + DB*DB
   40   CONTINUE
      R2 = DA * AN2(L2)
      DO 60 L = 1, K
!
!     If I >= LIVE(L1), then L1 is not in the live set.   If this is
!     true, we only need to consider clusters that are in the live set
!     for possible transfer of point I.   Otherwise, we need to consider
!     all possible clusters.
!
        IF (I .GE. LIVE(L1) .AND. I .GE. LIVE(L) .OR. L .EQ. L1 .OR.   &
              L .EQ. LL) GO TO 60
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
!
!     If no transfer is necessary, L2 is the new IC2(I).
!
        IC2(I) = L2
        GO TO 90
!
!     Update cluster centres, LIVE, NCP, AN1 & AN2 for clusters L1 and
!     L2, and update IC1(I) & IC2(I).
!
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
!
!     ITRAN(L) = 0 before entering QTRAN.   Also, LIVE(L) has to be
!     decreased by M before re-entering OPTRA.
!
      ITRAN(L) = 0
      LIVE(L) = LIVE(L) - M
  110 CONTINUE
!
      RETURN
      END SUBROUTINE OPTRA
!
!
      SUBROUTINE QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,   &
          ITRAN, INDX)
!
!     ALGORITHM AS 136.2  APPL. STATIST. (1979) VOL.28, NO.1
!
!     This is the quick transfer stage.
!     IC1(I) is the cluster which point I belongs to.
!     IC2(I) is the cluster which point I is most likely to be
!         transferred to.
!     For each point I, IC1(I) & IC2(I) are switched, if necessary, to
!     reduce within-cluster sum of squares.  The cluster centres are
!     updated after each step.
!
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K)
      REAL    A(M,N), D(M), C(K,N), AN1(K), AN2(K), ZERO, ONE
!
!     Define BIG to be a very large positive number
!
      DATA BIG /1.0E30/, ZERO /0.0/, ONE /1.0/
!
!     In the optimal transfer stage, NCP(L) indicates the step at which
!     cluster L is last updated.   In the quick transfer stage, NCP(L)
!     is equal to the step at which cluster L is last updated plus M.
!
      ICOUN = 0
      ISTEP = 0
   10 DO 70 I = 1, M
      ICOUN = ICOUN + 1
      ISTEP = ISTEP + 1
      L1 = IC1(I)
      L2 = IC2(I)
!
!     If point I is the only member of cluster L1, no transfer.
!
      IF (NC(L1) .EQ. 1) GO TO 60
!
!     If ISTEP > NCP(L1), no need to re-compute distance from point I to
!     cluster L1.   Note that if cluster L1 is last updated exactly M
!     steps ago, we still need to compute the distance from point I to
!     cluster L1.
!
      IF (ISTEP .GT. NCP(L1)) GO TO 30
      DA = ZERO
      DO 20 J = 1, N
        DB = A(I,J) - C(L1,J)
        DA = DA + DB*DB
   20   CONTINUE
      D(I) = DA * AN1(L1)
!
!     If ISTEP >= both NCP(L1) & NCP(L2) there will be no transfer of
!     point I at this step.
!
   30   IF (ISTEP .GE. NCP(L1) .AND. ISTEP .GE. NCP(L2)) GO TO 60
      R2 = D(I) / AN2(L2)
      DD = ZERO
      DO 40 J = 1, N
        DE = A(I,J) - C(L2,J)
        DD = DD + DE*DE
        IF (DD .GE. R2) GO TO 60
   40   CONTINUE
!
!     Update cluster centres, NCP, NC, ITRAN, AN1 & AN2 for clusters
!     L1 & L2.   Also update IC1(I) & IC2(I).   Note that if any
!     updating occurs in this stage, INDX is set back to 0.
!
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
!
!     If no re-allocation took place in the last M steps, return.
!
   60   IF (ICOUN .EQ. M) RETURN
   70 CONTINUE
      GO TO 10
      END SUBROUTINE QTRAN
      SUBROUTINE BLOCK(MM, M, N, D, CLAB, RLAB, TITLE, KC, DMNB, NB,   &
                       IERR, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      PRINTS OUTLINES OF BLOCKS OVER A DISTANCE MATRIX
!
!   DESCRIPTION
!   -----------
!
!   1.  THERE EXISTS AN ORDERING OF THE ROWS OF THE BLOCK SUCH THAT
!       EVERY BLOCK CONSISTS OF A SET OF OBJECTS CONTIGUOUS IN THAT
!       ORDER.  THE ALGORITHM IS GIVEN ON PAGE 156 OF THE FIRST
!       REFERENCE.  THE ROW OBJECTS ARE STORED IN THE VECTOR RLAB IN
!       SUCH AN ORDER.  SIMILARLY, THE COLUMNS CAN BE ORDERED WHICH IS
!       STORED IN THE CLAB ARRAY.
!
!   2.  THIS ORDERING OF THE OBJECTS ALLOWS THE BLOCKS TO BE NAMED BY
!       GIVING THE LOCATION OF THE FIRST AND LAST ROW AND COLUMN IN THE
!       ARRAY FOR EACH BLOCK.  THE FIRST TWO COLUMNS OF THE NB ARRAY
!       STORE THE FIRST AND LAST ROWS IN EACH BLOCK AND THE THIRD AND
!       FOURTH COLUMNS STORE THE FIRST AND LAST COLUMNS IN EACH BLOCK
!
!   3.  THE FINAL BLOCK DIAGRAM PRINTS THE ROW LABELS AND THE COLUMN
!       LABELS AND THE DISTANCE MATRIX WHERE EACH VALUE IS MULTIPLIED
!       BY 10.  THE HORIZONTAL BOUNDARIES OF THE BLOCKS ARE REPRESENTED
!       BY DASHES AND THE VERTICAL BOUNDARIES BY QUOTE MARKS.  COMMAS
!       REPRESENT THE CORNERS OF THE BLOCKS.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX D.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF OBJECTS.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   D     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
!            DIMENSION MUST BE AT LEAST M (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DISTANCES.
!
!         D(I,J) = DISTANCE FROM CASE I TO CASE J
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N
!            (UNCHANGED ON OUTPUT).
!         ORDERED LABELS OF THE COLUMNS.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
!            (UNCHANGED ON OUTPUT).
!         ORDERED LABELS OF THE ROWS.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   KC    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF BLOCKS.
!
!   DMNB  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX NB.  MUST BE AT LEAST 4.
!
!   NB    REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMNB AND SECOND
!            DIMENSION MUST BE AT LEAST KC (UNCHANGED ON OUTPUT).
!         THE MATRIX DEFINING THE BOUNDARIES OF THE BLOCKS.
!
!         NB(1,I) IS 1 + THE FIRST ROW IN BLOCK I
!         NB(2,I) IS 1 + THE LAST ROW IN BLOCK I
!         NB(3,I) IS 1 + THE FIRST COLUMN IN BLOCK I
!         NB(4,I) IS 1 + THE LAST COLUMN IN BLOCK I
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   OUTPUT PARAMETER
!   ----------------
!
!   IERR  INTEGER SCALAR.
!         ERROR FLAG.
!
!         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
!
!         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
!                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
!                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
!                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
!                   RESULTS FOR THAT CLUSTER.
!
!   REFERENCES
!   ----------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 168.
!
!     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
!        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMNB, OUNIT
      DIMENSION D(MM,*), NB(DMNB,*), IA(26)
      CHARACTER*4 CLAB(*), RLAB(*), DD, AE(26)
      CHARACTER*10 TITLE
!CCCC CHARACTER*1 DASH,DITTO,COMMA,BLANK,STAR,DOT,AA(26)
      CHARACTER*1 DASH,DITTO,COMMA,BLANK,AA(26)
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA DD/'----'/
!CCCC DATA DASH,DITTO,COMMA,BLANK,STAR,DOT/'-','''',',',' ','*','.'/
      DATA DASH,DITTO,COMMA,BLANK/'-','''',',',' '/
!
!     CHECK BOUNDARY ARRAY NB
!
      IF (OUNIT .LE. 0) RETURN
      DO 10 K=1,KC
         IF(NB(1,K).LT.2.OR.NB(1,K).GT.NB(2,K).OR.NB(2,K).GT.M .OR.   &
            NB(3,K).LT.2.OR.NB(3,K).GT.NB(4,K).OR.NB(4,K).GT.N) THEN
            WRITE(ICOUT,1) K
            CALL DPWRST('XXX','WRIT')
            WRITE(ICOUT,6) (NB(I,K)-1,I=1,4)
            CALL DPWRST('XXX','WRIT')
            IERR = 2
         ENDIF
   10 CONTINUE
    1 FORMAT(' BAD BOUNDARY IN BLOCK ',I3)
    6 FORMAT(' BOUNDARIES ARE ', 4I5)
!
      JPP=(N-2)/25+1
      DO 80 JP=1,JPP
         JLP=25*(JP-1)+1
         JUP=25*JP+1
         IF(JUP.GT.N-1) JUP=N-1
         JR=JUP-JLP+1
!
!     WRITE TITLES
!
         WRITE(ICOUT,999)
  999    FORMAT(1X)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,2) TITLE
    2    FORMAT(' BLOCKED ARRAY ',A10)
         CALL DPWRST('XXX','WRIT')
!
!     WRITE OUT ARRAY ONE LINE AT A TIME
!
         WRITE(ICOUT,3)(CLAB(J),J=JLP,JUP)
    3    FORMAT(10X,25(1X,A4))
         CALL DPWRST('XXX','WRIT')
         DO 85 I=1,M
            I1=I-1
            DO 20 L=1,26
               AE(L)=BLANK
               AA(L)=BLANK
   20       CONTINUE
            IF (I .NE. 1) THEN
!
!     FILL IN DISTANCES
!
               DO 30 J=JLP,JUP
                  IA(J-JLP+1)=INT(D(I1,J)*10.)
   30          CONTINUE
!
!     FILL IN VERTICAL BOUNDARIES
!
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
!
!     FILL IN HORIZONTAL BOUNDARIES
!
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
                        IF(AA(J).EQ.BLANK) AA(J)=DASH
   50                CONTINUE
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
   85    CONTINUE
   80 CONTINUE
      RETURN
      END SUBROUTINE BLOCK
      SUBROUTINE BUILD(MM, M, N, A, CLAB, RLAB, K, ITER, XMISS,   &
                       DMSUM1, DMSUM2, SUM, IWORK, WORK, CWORK)
!CCCC SUBROUTINE BUILD(MM, M, N, A, CLAB, RLAB, TITLE, K, ITER, XMISS,
!CCCC1                 DMSUM1, DMSUM2, SUM, IWORK, WORK, CWORK, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      BUILDS CLUSTERS BY THE K-MEANS ALGORITHM, PRINTING THE RESULTS
!      FOR ALL INTERMEDIATE ITERATIONS
!
!   DESCRIPTION
!   -----------
!
!   1.  THE VARIABLES SHOULD BE SCALED SIMILARLY (CLUSTER SUBROUTINE
!       STAND CAN BE USED TO STANDARDIZE THE VARIABLES).
!
!   2.  THE ROUTINE ITERATES FROM 1 TO THE DESIRED NUMBER OF CLUSTERS.
!       THE FIRST ITERATION STARTS WITH THE CLUSTER OF ALL CASES AND
!       COMPUTES THE SUMMARY STATISTICS FOR EACH VARIABLE AND THE
!       DISTANCES FROM EACH CASE TO THE CLUSTER CENTER WITH ALL THE
!       CALCULATIONS BEING PRINTED.  THE SECOND ITERATION DIVIDES THE
!       CLUSTER INTO TWO CLUSTERS, MOVING CASES FROM ONE TO THE OTHER
!       UNTIL EITHER NO FURTHER MOVEMENTS DECREASE THE DISTANCES
!       BETWEEN EACH CASE AND THE CENTER OF ITS ASSIGNED CLUSTER OR THE
!       MAXIMUM NUMBER OF MOVEMENTS PER ITERATION HAS BEEN REACHED.
!       FOR THE THIRD AND SUBSEQUENT ITERATIONS, THE CLUSTER WITH THE
!       LARGEST VARIANCE IS SPLIT AND ITS CASES ARE ASSIGNED TO THE
!       CLUSTER WHOSE MEAN IS THE SMALLEST DISTANCE FROM THE CASE.  THE
!       MEANS ARE THEN UPDATED AND THE PROCESS OF REASSIGNING CASES TO
!       CLUSTERS CONTINUES UNTIL NO REASSIGNMENTS ARE MADE FOR AN
!       ITERATION.
!
!   3.  THE CLUSTERS AND THEIR STATISTICS WILL BE PRINTED OUT AFTER EACH
!       ITERATION ON FORTRAN UNIT OUNIT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
!            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE VARIABLES.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE CASES.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CLUSTERS DESIRED.
!
!   ITER  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         MAXIMUM NUMBER OF MOVEMENTS ALLOWED PER ITERATION.
!
!   XMISS REAL SCALAR (UNCHANGED ON OUTPUT).
!         MISSING VALUE CODE.  IF A(I,J) = XMISS, THEN THE VALUE FOR THE
!         J-TH VARIABLE FOR THE I-TH CASE IS ASSUMED TO BE MISSING.
!
!   DMSUM1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST 7.
!
!   DMSUM2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE SECOND DIMENSION OF THE MATRIX SUM. MUST BE AT LEAST N.
!
!   SUM   REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMSUM1, WHOSE SECOND
!            DIMENSION MUST BE DMSUM2 AND WHOSE THIRD DIMENSION MUST
!            BE AT LEAST K+1.
!         WORK MATRIX.
!
!   IWORK INTEGER VECTOR DIMENSIONED AT LEAST M.
!         WORK VECTOR.
!
!   WORK  REAL VECTOR DIMENSIONED AT LEAST 2*N+2*M.
!         WORK VECTOR.
!
!   CWORK VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 84-108.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER DMSUM1, DMSUM2, DCLUS, OUNIT
      INTEGER DMSUM1, DMSUM2, DCLUS
      DIMENSION SUM(DMSUM1,DMSUM2,*), A(MM,*), WORK(*), IWORK(*)
      CHARACTER*4 CLAB(*), RLAB(*), CWORK(*)
!CCCC CHARACTER*10 TITLE
!
!     SUM(1,J,I) IS THE VALUE OF THE J-TH VARIABLE AT THE CENTER OF
!                   CLUSTER I
!     SUM(2,J,I) IS THE NUMBER OF NON-MISSING OBSERVATIONS FOR THE J-TH
!                   VARIABLE IN CLUSTER I
!     SUM(3,J,I) IS THE MEAN OF THE J-TH VARIABLE IN CLUSTER I
!     SUM(4,J,I) IS THE STANDARD DEVIATION OF THE J-TH VARIABLE IN
!                   CLUSTER I
!     SUM(5,J,I) IS THE MINIMUM OF THE J-TH VARIABLE IN CLUSTER I
!     SUM(6,J,I) IS THE MAXIMUM OF THE J-TH VARIABLE IN CLUSTER I
!     SUM(7,J,I) IS THE SUM OF SQUARED DEVIATIONS FOR THE J-TH VARIABLE
!                   FROM THE MEAN OF CLUSTER I
!
!     THE K+1-ST ROW OF SUM STORES THE SAME CALCULATIONS AS ABOVE EXCEPT
!        FOR THE ENTIRE DATA SET RATHER THAN FOR AN INDIVIDUAL CLUSTER
!
      KKK=0
      KK=0
      KM=0
!
      DCLUS = 2*N + M
      DO 13 I=1,7
         DO 12 J=1,N
            DO 11 KK=1,K+1
               SUM(I,J,KK)=0.
   11       CONTINUE
   12    CONTINUE
   13 CONTINUE
!
!     LOOP ONCE FOR EACH DESIRED CLUSTER
!
      DO 130 KK=1,K
         DO 60 NC=1,ITER
            ERR=0.
            DO 20 KKK=1,KK
               DO 25 J=1,N
                  IF(NC.EQ.1.OR.SUM(1,J,KKK).NE.SUM(3,J,KKK)) ERR=1.
   25          CONTINUE
   20       CONTINUE
!
!     IF NO CHANGES HAVE BEEN MADE, OUTPUT THE CLUSTER
!
            IF(ERR.EQ.0.) GO TO 70
            DO 30 KKK=1,KK
               DO 35 J=1,N
                  SUM(2,J,KKK)=0.
                  SUM(1,J,KKK)=SUM(3,J,KKK)
   35          CONTINUE
   30       CONTINUE
            DO 50 I=1,M
               DO 40 J=1,N
                  WORK(J)=A(I,J)
   40          CONTINUE
               IWORK(I)=NC
!
!     FIND BEST CLUSTER FOR CASE I
!
               CALL KMEANS(N, WORK, KK, XMISS, DMSUM1, DMSUM2, SUM,   &
                           IWORK(I), WORK(DCLUS+I))
   50       CONTINUE
   60    CONTINUE
!CC70    IF (OUNIT .GT. 0) CALL KOUT(MM, M, N, A, CLAB, RLAB, TITLE, KK,
!CCCC*                     DMSUM1, DMSUM2, SUM, IWORK, WORK(DCLUS+1),
!CCCC*                     WORK(N+1), WORK(M+N+1), CWORK, OUNIT)
   70    CALL KOUT(M, N, CLAB, RLAB, KK,   &
                   DMSUM1, DMSUM2, SUM, IWORK, WORK(DCLUS+1),   &
                   WORK(N+1), WORK(M+N+1), CWORK)
                                                                                                                                  
!
!     CREATE A NEW CLUSTER BY SPLITTING VARIABLE WITH LARGEST WITHIN-
!     CLUSTER VARIANCE AT THAT VALUE OF THAT VARIABLE AT THE CENTER
!     OF THE CLUSTER
!
         SM=0.
         DO 80 J=1,N
            DO 85 KKK=1,KK
               IF(SUM(4,J,KKK).GE.SM) THEN
                  SM=SUM(4,J,KKK)
                  KM=KKK
               ENDIF
   85       CONTINUE
   80    CONTINUE
         KN=KK+1
         DO 90 JJ=1,N
            SUM(2,JJ,KM)=0.
            SUM(3,JJ,KM)=0.
            SUM(2,JJ,KN)=0.
            SUM(3,JJ,KN)=0.
   90    CONTINUE
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
      END SUBROUTINE BUILD
      SUBROUTINE CLUMOM(MM, M, N, A, ICLUS, W, U, DMC1, DMC2, C)
!
!  NOTE: RENAMED FOR DATAPLOT TO AVOID NAME CONFLICT.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      COMPUTES WEIGHTED MEANS AND COVARIANCES
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
!            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!   ICLUS INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE CLUSTER NUMBER.
!
!   W     REAL VECTOR DIMENSIONED AT LEAST M (UNCHANGED ON OUTPUT).
!         VECTOR OF WEIGHTS FOR THE OBJECTS.
!
!   DMC1  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX C.  MUST BE AT LEAST N.
!
!   DMC2  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE SECOND DIMENSION OF THE MATRIX C.  MUST BE AT LEAST N.
!
!   OUTPUT PARAMETERS
!   -----------------
!
!   U     REAL VECTOR DIMENSIONED AT LEAST N.
!         VECTOR OF WEIGHTED CLUSTER MEANS FOR EACH VARIABLE.
!
!   C     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMC1, WHOSE SECOND
!            DIMENSION MUST BE DMC2, AND WHOSE THIRD DIMENSION MUST BE
!            AT LEAST K.
!         C(I,J,K) IS THE IJ-TH ELEMENT OF THE COVARIANCE MATRIX FOR THE
!            K-TH CLUSTER.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 73.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMC1, DMC2
      DIMENSION C(DMC1,DMC2,*), W(*), U(*), A(MM,*)
!
      INCLUDE 'DPCOMC.INC'
!
      SP=0.
      DO 10 I=1,M
         SP=SP+W(I)
   10 CONTINUE
      IF(SP.EQ.0.) SP=R1MACH(4)
!
!     COMPUTED WEIGHTED MEANS
!
      DO 30 J=1,N
         SS=0.
         DO 20 I=1,M
            SS=SS+A(I,J)*W(I)
   20    CONTINUE
         U(J)=SS/SP
   30 CONTINUE
!
!     COMPUTED WEIGHTED COVARIANCES
!
      DO 50 J=1,N
         DO 55 K=1,J
            SS=0.
            DO 40 I=1,M
               SS=SS+(A(I,J)-U(J))*(A(I,K)-U(K))*W(I)
   40       CONTINUE
            C(J,K,ICLUS)=SS/SP
            C(K,J,ICLUS)=C(J,K,ICLUS)
   55    CONTINUE
   50 CONTINUE
      RETURN
      END SUBROUTINE CLUMOM
      SUBROUTINE COVOUT(M, N, CLAB1, CLAB2, RLAB, TITLE, K,   &
                        DMWORK, WORK1, DMC1, DMC2, C, WORK,   &
                        ICAPTY,ICAPSW,IFORSW)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      PRINTS RESULTS OF MIX
!
!   DESCRIPTION
!   -----------
!
!   1.  SEE DESCRIPTION OF MIX FOR DESCRIPTION OF OUTPUT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   SEE SUBROUTINE MIX FOR PARAMETERS
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 127.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER DMWORK, P, U, PMIX, DMC1, DMC2, OUNIT
!CCCC DIMENSION A(MM,*), WORK1(DMWORK,*), C(DMC1,DMC2,*), WORK(*)
      INTEGER DMWORK, P, U, PMIX, DMC1, DMC2
      DIMENSION WORK1(DMWORK,*), C(DMC1,DMC2,*), WORK(*)
      CHARACTER*4 CLAB1(*), CLAB2(*)
      CHARACTER*8 RLAB(*)
      CHARACTER*10 TITLE
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOST.INC'
!
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
!
      INCLUDE 'DPCOP2.INC'
!
      ISUBRO='XXXX'
      IBUGA3='OFF'
      IERROR='OFF'
!
      P = 0
      U = P + M
      PMIX = U + N + 1
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
      IF(IFORSW.EQ.'E')NUMDIG=-7
      IF(IFORSW.EQ.'-2')NUMDIG=-2
      IF(IFORSW.EQ.'-3')NUMDIG=-3
      IF(IFORSW.EQ.'-4')NUMDIG=-4
      IF(IFORSW.EQ.'-5')NUMDIG=-5
      IF(IFORSW.EQ.'-6')NUMDIG=-6
      IF(IFORSW.EQ.'-7')NUMDIG=-7
      IF(IFORSW.EQ.'-8')NUMDIG=-8
      IF(IFORSW.EQ.'-9')NUMDIG=-9
!
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
!
!     PRINT CLUSTER PROBABILITIES
!
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
!
!     PRINT MEANS
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
!
!CCCC   WRITE(ICOUT,4)
!CCC4   FORMAT(' CLUSTER MEANS')
!CCCC   CALL DPWRST('XXX','WRIT')
!
!CCCC   DO 10 J=1,N
!CCCC      WRITE(ICOUT,5)CLAB1(J),CLAB2(J),(WORK1(U+J,KK),KK=1,K)
!CCC5      FORMAT(1X,2A4,5X,10F12.4)
!CCCC      CALL DPWRST('XXX','WRIT')
!CC10   CONTINUE
!
        ITITLE=' '
        NCTITL=0
        ITITL9='Cluster Means'
        NCTIT9=13
!
        IF(K.LE.6)THEN
          NLOOP=1
        ELSE
          NLOOP=K/6
          NTEMP=MOD(K,6)
          IF(NTEMP.GT.0)NLOOP=NLOOP+1
        ENDIF
!
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
!
        DO 1010 II=1,NLOOP
          IF(II.EQ.NLOOP)THEN
            NUMCOL=MOD(K,6)
            IF(NUMCOL.EQ.0)NUMCOL=6
          ELSE
            NUMCOL=6
          ENDIF
          NUMCOL=NUMCOL+1
          NUMLIN=1
!
          DO 1020 J=1,NUMCLI
            DO 1030 I=1,MAXLIN
              ITITL2(I,J)=' '
              NCTIT2(I,J)=0
              NCOLSP(I,J)=1
 1030       CONTINUE
 1020     CONTINUE
!
          ISTRT=(II-1)*7+1
          IEND=ISTRT+6
          IF(IEND.GT.N)IEND=N
!
          ITITL2(1,1)='Variable'
          NCTIT2(1,1)=8
!
          DO 1040 L=ISTRT,IEND
            ITITL2(1,L+1)='Cluster '
            WRITE(ITITL2(1,L+1)(9:11),'(I3)')L
            NCTIT2(1,L+1)=11
 1040     CONTINUE
!
          NMAX=0
          ICNT=0
          ICNT2=0
          DO 1050 I=1,NUMCOL
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
!
          ICNT=0
          DO 1060 J=1,N
            ICNT=ICNT+1
            IDIGI2(ICNT,1)=0
            IVALUE(ICNT,1)(1:4)=CLAB1(J)(1:4)
            IVALUE(ICNT,1)(5:8)=CLAB2(J)(1:4)
            NCVALU(ICNT,1)=8
            AMAT(ICNT,1)=0.0
!
            ICNT2=1
            DO 1065 KK=ISTRT,IEND
!
              ICNT2=ICNT2+1
              IDIGI2(ICNT,ICNT2)=NUMDIG
              IVALUE(ICNT,ICNT2)=' '
              NCVALU(ICNT,ICNT2)=0
              AMAT(ICNT,ICNT2)=WORK1(U+J,KK)
              ROWSEP(ICNT)=0
!
 1065       CONTINUE
 1060    CONTINUE
!
          ROWSEP(ICNT)=1
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDT5B(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      NCOLSP,ROWSEP,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
 1010   CONTINUE
!
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
!CCCC   WRITE(ICOUT,7)
!CCC7   FORMAT(' WITHIN CLUSTER VARIANCES AND CORRELATIONS')
!CCCC   CALL DPWRST('XXX','WRIT')
!
!CCCC   DO 30 I=1,N
!CCCC      DO 30 J=I,N
!CCCC         DO 20 KK=1,K
!CCCC            Z=C(I,I,KK)*C(J,J,KK)
!CCCC            WORK(KK)=C(I,J,KK)
!CCCC            IF(I.EQ.J) Z=0.
!CC20            IF(Z.NE.0.) WORK(KK)=C(I,J,KK)*Z**(-0.5)
!CCCC            IF(I.EQ.J) THEN
!CCCC              WRITE(ICOUT,999)
!CCCC              CALL DPWRST('XXX','WRIT')
!CCCC            ENDIF
!CCCC            WRITE(ICOUT,9) CLAB1(I),CLAB2(I),CLAB1(J),CLAB2(J),
!CCCC1                          (WORK(KK),KK=1,K)
!CCC9            FORMAT(1X,2A4,2X,2A4,10F12.4)
!CCCC            CALL DPWRST('XXX','WRIT')
!CC30   CONTINUE
!
        ITITLE=' '
        NCTITL=0
        ITITL9='Within Cluster Variances and Correlations'
        NCTIT9=41
!
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
!
        DO 1110 II=1,NLOOP
          IF(II.EQ.NLOOP)THEN
            NUMCOL=MOD(K,6)
            IF(NUMCOL.EQ.0)NUMCOL=6
          ELSE
            NUMCOL=6
          ENDIF
          NUMCOL=NUMCOL+2
          NUMLIN=1
!
          DO 1120 J=1,NUMCLI
            DO 1130 I=1,MAXLIN
              ITITL2(I,J)=' '
              NCTIT2(I,J)=0
              NCOLSP(I,J)=1
 1130       CONTINUE
 1120     CONTINUE
!
          ISTRT=(II-1)*7+1
          IEND=ISTRT+6
          IF(IEND.GT.N)IEND=N
!
          ITITL2(1,1)='I'
          NCTIT2(1,1)=1
          ITITL2(1,2)='J'
          NCTIT2(1,2)=1
!
          DO 1140 L=ISTRT,IEND
            ITITL2(1,L+2)='Cluster '
            WRITE(ITITL2(1,L+2)(9:11),'(I3)')L
            NCTIT2(1,L+2)=11
 1140     CONTINUE
!
          NMAX=0
          ICNT=0
          ICNT2=0
          DO 1150 I=1,NUMCOL
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
!
          ICNT=0
          DO 1160 I=1,N
            DO 1165 J=I,N
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
!
              ICNT2=2
              DO 1168 KK=ISTRT,IEND
!
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
!
 1165       CONTINUE
 1160     CONTINUE
!
          ROWSEP(ICNT)=1
          IFRST=.TRUE.
          ILAST=.TRUE.
          IFLAGS=.TRUE.
          IFLAGE=.TRUE.
          CALL DPDT5B(ITITLE,NCTITL,   &
                      ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                      MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                      IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                      IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                      NCOLSP,ROWSEP,   &
                      ICAPSW,ICAPTY,IFRST,ILAST,   &
                      IFLAGS,IFLAGE,   &
                      ISUBRO,IBUGA3,IERROR)
!
 1110   CONTINUE
      ENDIF
!
!     PRINT PROBABILITIES
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
!CCCC   WRITE(ICOUT,11)
!CC11   FORMAT(' BELONGING PROBABILITIES')
!CCCC   CALL DPWRST('XXX','WRIT')
!CCCC   DO 40 I=1,M
!CCCC      WRITE(ICOUT,12) RLAB(I),(WORK1(P+I,KK),KK=1,K)
!CC12      FORMAT(1X,A8,2X,10F12.6)
!CCCC      CALL DPWRST('XXX','WRIT')
!CC40   CONTINUE
!
        ITITLE=' '
        NCTITL=0
        ITITL9='Belonging Probabilities'
        NCTIT9=23
!
!       DO 2 LOOPS:
!
!          1) FIRST LOOP IS FOR THE NUMBER OF ROWS (OBSERVATIONS)
!          2) SECOND LOOP IS FOR THE NUMBER OF COLUMNS (CLUSTERS)
!
        IF(M.LE.MAXROW)THEN
          NLOOP2=1
        ELSE
          NLOOP2=M/MAXROW
          IF(MOD(M,MAXROW).GT.0)NLOOP2=NLOOP2+1
        ENDIF
!
        IF(K.LE.6)THEN
          NLOOP=1
        ELSE
          NLOOP=K/6
          IF(MOD(K,6).GT.0)NLOOP=NLOOP+1
        ENDIF
!
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
!
        DO 1201 JJ=1,NLOOP2
          IROW1=(JJ-1)*MAXROW+1
          IROW2=JJ*MAXROW
          IF(IROW2.GT.M)IROW2=M
          DO 1210 II=1,NLOOP
            IF(II.EQ.NLOOP)THEN
              NUMCOL=MOD(K,6)
              IF(NUMCOL.EQ.0)NUMCOL=6
            ELSE
              NUMCOL=6
            ENDIF
            NUMCOL=NUMCOL+1
            NUMLIN=1
!
            DO 1220 J=1,NUMCLI
              DO 1230 I=1,MAXLIN
                ITITL2(I,J)=' '
                NCTIT2(I,J)=0
                NCOLSP(I,J)=1
 1230         CONTINUE
 1220       CONTINUE
!
            ISTRT=(II-1)*7+1
            IEND=ISTRT+6
            IF(IEND.GT.N)IEND=N
!
            ITITL2(1,1)='Observation'
            NCTIT2(1,1)=11
!
            DO 1240 L=ISTRT,IEND
              ITITL2(1,L+1)='Cluster '
              WRITE(ITITL2(1,L+1)(9:11),'(I3)')L
              NCTIT2(1,L+1)=11
 1240       CONTINUE
!
            NMAX=0
            ICNT=0
            ICNT2=0
            DO 1250 I=1,NUMCOL
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
!
            ICNT=0
            DO 1260 J=IROW1,IROW2
              ICNT=ICNT+1
              IDIGI2(ICNT,1)=0
              IVALUE(ICNT,1)(1:8)=RLAB(J)(1:8)
              NCVALU(ICNT,1)=8
              AMAT(ICNT,1)=0.0
!
              ICNT2=1
              DO 1265 KK=ISTRT,IEND
!
                ICNT2=ICNT2+1
                IDIGI2(ICNT,ICNT2)=NUMDIG
                IVALUE(ICNT,ICNT2)=' '
                NCVALU(ICNT,ICNT2)=0
                AMAT(ICNT,ICNT2)=WORK1(P+J,KK)
                ROWSEP(ICNT)=0
!
 1265       CONTINUE
 1260    CONTINUE
!
            ROWSEP(ICNT)=1
            IFRST=.TRUE.
            ILAST=.TRUE.
            IFLAGS=.TRUE.
            IFLAGE=.TRUE.
            CALL DPDT5B(ITITLE,NCTITL,   &
                        ITITL9,NCTIT9,ITITL2,NCTIT2,   &
                        MAXLIN,NUMLIN,NUMCLI,NUMCOL,   &
                        IVALUE,NCVALU,AMAT,ITYPCO,MAXROW,ICNT,   &
                        IDIGI2,NTOT,IWHTML,IWRTF,VALIGN,ALIGN,NMAX,   &
                        NCOLSP,ROWSEP,   &
                        ICAPSW,ICAPTY,IFRST,ILAST,   &
                        IFLAGS,IFLAGE,   &
                        ISUBRO,IBUGA3,IERROR)
 1210     CONTINUE
 1201   CONTINUE
!
      ENDIF
      RETURN
      END SUBROUTINE COVOUT
      SUBROUTINE CSPLIT(MM, M, A, CLAB, IR, KA, TH, IORD, DMIWRK,   &
                        IWORK, DMWORK, WORK)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      FINDS OPTIMAL SPLIT OF VARIABLES
!
!   DESCRIPTION
!   -----------
!
!   1.  INITIALLY, THE FIRST CLUSTER CONSISTS OF ALL VARIABLES WITHIN
!       THE BLOCK IR AND THE SECOND CLUSTER IS EMPTY.  THE REDUCTION IN
!       THE WITHIN-CLUSTER SUM OF SQUARES FOR MOVING EACH VARIABLE
!       FROM THE FIRST CLUSTER TO THE SECOND IS CALCULATED.  THE
!       VARIABLE THAT REDUCES THE SUM OF SQUARES THE MOST IS MOVED AND
!       THIS CONTINUES UNTIL ALL VARIABLES ARE MOVED WITH EACH
!       REDUCTION STORED.  THEN THE SPLIT THAT HAD THE SMALLEST
!       REDUCTION OF ALL IS RETURNED AS THE OPTIMUM SPLIT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM, M, N, A, CLAB, TH, IORD, DMIWRK, DMWORK -- SEE SUBROUTINE SPLIT2
!
!   IR    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         NUMBER OF BLOCK TO BE SPLIT.
!
!   KA    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         NUMBER OF BLOCKS.
!
!   IWORK INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND SECOND
!            DIMENSION MUST BE AT LEAST KA.
!         THE MATRIX DEFINING THE BOUNDARIES OF THE BLOCKS.
!
!         IWORK(1,I) IS 1 + THE FIRST ROW IN BLOCK I
!         IWORK(2,I) IS 1 + THE LAST ROW IN BLOCK I
!         IWORK(3,I) IS 1 + THE FIRST COLUMN IN BLOCK I
!         IWORK(4,I) IS 1 + THE LAST COLUMN IN BLOCK I
!
!   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
!            DIMENSION MUST BE AT LEAST MAX(M,N).
!
!         WORK(1,I) = FIRST CASE IN CASE CLUSTER I
!         WORK(2,I) = LAST CASE IN CASE CLUSTER I
!         WORK(3,I) = REDUCTION IN SSQ DUE TO SPLITTING
!         WORK(4,I) = LAST CASE IN FIRST CLUSTER OF SPLIT OF I
!         WORK(5,I) = 1 IF CASE IS INCLUDED IN PRESENT VARIABLE SPLIT
!         WORK(6,I) = NUMBER OF VARIABLES IN I-TH ROW OF PRESENT
!                     VARIABLE SPLIT
!         WORK(7,I) = MEAN OF I-TH CASE, FIRST VARIABLE CLUSTER
!         WORK(8,I) = NUMBER OF VARIABLES SECOND CLUSTER
!         WORK(9,I) = MEAN OF I-TH CASE, SECOND CLUSTER
!
!         WORK(10-18,I) ARE SIMILAR WITH VARIABLES AND CASES REVERSED.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 276.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMWORK, DMIWRK
      DIMENSION A(MM,*), IWORK(DMIWRK,*), WORK(DMWORK,*)
      CHARACTER*4 CLAB(*), C
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
      XM=99999.
      DO 10 I=1,M
         WORK(5,I)=0.
   10 CONTINUE
!
!     LOOK FOR BLOCKS WITHIN THRESHOLD
!
      JL=INT(WORK(10,IR))
      JU=INT(WORK(11,IR))
      DO 40 K=1,KA
         IF(IWORK(3,K).EQ.JL+1.AND.IWORK(4,K).EQ.JU+1) THEN
            IL=IWORK(1,K)
            IF(IL.LT.0) IL=-IL
            IU=IWORK(2,K)
!
!     COMPUTE VARIANCES
!
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
!
!     FIND BEST VARIABLE SPLIT
!
      DO 50 I=1,M
         WORK(8,I)=0.
         WORK(9,I)=0.
   50 CONTINUE
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
!
!     INTERCHANGE JD AND JJ
!
         DO 80 I=1,M
            CC=A(I,JJ)
            A(I,JJ)=A(I,JD)
            A(I,JD)=CC
   80    CONTINUE
         C = CLAB(JJ)
         CLAB(JJ) = CLAB(JD)
         CLAB(JD) = C
!
!     UPDATE MEANS
!
         DO 90 I=1,M
            IF(WORK(5,I).NE.0..AND.A(I,JJ).NE.XM) THEN
               WORK(6,I)=WORK(6,I)-1.
               IF(WORK(6,I).NE.0.)WORK(7,I)=WORK(7,I)+(WORK(7,I)-   &
                                 A(I,JJ))/WORK(6,I)
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
      END SUBROUTINE CSPLIT
      SUBROUTINE INVERT(MM, M, A, DET, WORK, IWORK, IERR)
!CCCC SUBROUTINE INVERT(MM, M, A, DET, WORK, IWORK, IERR, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      COMPUTES THE INVERSE AND DETERMINANT OF THE SYMMETRIC MATRIX
!      (E.G., A COVARIANCE MATRIX)
!
!   DESCRIPTION
!   -----------
!
!   1.  THE LINPACK SUBROUTINE SSIFA IS CALLED TO FACTOR THE MATRIX AND
!       THEN THE LINPACK SUBROUTINE SSIDI IS CALLED TO USE THE
!       FACTORIZATION TO FIND THE INVERSE AND DETERMINANT.  THE INPUT
!       MATRIX MUST BE SYMMETRIC AND IS OVERWRITTEN WITH ITS INVERSE ON
!       OUTPUT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF ROWS AND COLUMNS IN THE MATRIX A.
!
!   A     REAL SYMMETRIC MATRIX WHOSE FIRST DIMENSION MUST BE MM AND
!            WHOSE SECOND DIMENSION MUST BE AT LEAST M (CHANGED ON
!            OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   WORK  REAL VECTOR DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   IWORK INTEGER VECTOR DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR ERROR MESSAGES.
!
!   OUTPUT PARAMETERS
!   -----------------
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
!            DIMENSION MUST BE AT LEAST N.
!         THE INVERSE OF THE INPUT MATRIX.
!
!   DET   REAL VECTOR DIMENSIONED AT LEAST 2.
!         THE DETERMINANT OF THE MATRIX.
!
!         THE DETERMINANT IS  DET(1) ** DET(2).
!
!   IERR  INTEGER SCALAR.
!         ERROR FLAG.
!
!         IF IERR = 0, NO ERROR CONDITION WAS DETECTED.
!
!         IF IERR = K, THE K-TH PIVOT BLOCK IS SINGULAR.  THE INVERSE IS
!                      NOT COMPUTED.  ERROR CONDITION SET IN CMLIB
!                      ROUTINE SSIFA.
!
!   REFERENCES
!   ----------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 69.
!
!     NBS CORE MATH LIBRARY, VOLS. 1-4 (GAITHERSBURG: QA297.C69 IN NBS
!     LIBRARY, ADMIN E-120).
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER OUNIT
!CCCC DIMENSION A(MM,*), IWORK(*), WORK(*), DET(*), INERT(3)
      DIMENSION A(MM,*), IWORK(*), WORK(*), DET(*)
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!     NOTE: FOR DATAPLOT, REPLACE OLDER LINPAC ROUTINES WITH
!           VERSIONS THAT ARE USED IN DATAPLOT.
!
      IERR=0
!CCCC CALL SSIFA(A,MM,M,IWORK,IERR)
!CCCC IF (IERR .NE. 0) THEN
!CCCC    IF (OUNIT .GT. 0) THEN
!CCCC       WRITE(ICOUT,1)
!CC1        FORMAT('MATRIX TO BE INVERTED MAY BE SINGULAR')
!CCCC       CALL DPWRST('XXX','WRIT')
!CCCC       GO TO 9000
!CCCC ENDIF
!CCCC JOB = 111
!CCCC CALL SSIDI(A,MM,M,IWORK,DET,INERT,WORK,JOB)
      CALL SGECO(A,MM,M,IWORK,RCOND,WORK)
!
      EPS=1.0E-20
      IF(RCOND.LE.EPS)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2571)
        CALL DPWRST('XXX','ERRO ')
        WRITE(ICOUT,2572)
        CALL DPWRST('XXX','ERRO ')
        GO TO 9000
      ELSE
        IJOB=1
        CALL SGEDI(A,MM,M,IWORK,DET,WORK,IJOB)
      END IF
  999 FORMAT(1X)
 2571 FORMAT('****** ERROR IN INVERT ********')
 2572 FORMAT('       THE INPUT MATRIX IS SINGULAR')
!CCCC END CHANGE
!
      DO 10 I = 1 , M
         DO 20 J = I , M
            A(J,I) = A(I,J)
 20      CONTINUE
 10   CONTINUE
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE INVERT
      SUBROUTINE KMEANS(N, X, K, XMISS, DMSUM1, DMSUM2, SUM, JMIN, DMIN)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      ASSIGNS A VARIABLE TO ITS CLOSEST CLUSTER AND UPDATES THE SUMMARY
!      STATISTICS
!
!   DESCRIPTION
!   -----------
!
!   1.  THE DISTANCE BETWEEN THE CASE X AND THE CENTER OF EACH CLUSTER
!       IS COMPUTED AND X IS ASSIGNED TO THE CLUSTER WITH THE SMALLEST
!       DISTANCE.  THE SUMMARY STATISTICS FOR THE ASSIGNED CLUSTER ARE
!       THEN UPDATED.
!
!   INPUT PARAMETERS
!   ----------------
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   X     REAL VECTOR DIMENSIONED AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CLUSTERS.
!
!   XMISS REAL SCALAR (UNCHANGED ON OUTPUT).
!         VALUE THAT A DATA VALUE IS SET TO IF CONSIDERED MISSING.
!
!   DMSUM1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST 7.
!
!   DMSUM2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE SECOND DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST N.
!
!   OUTPUT PARAMETERS
!   ------------------
!
!   SUM   REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMSUM1, WHOSE SECOND
!            DIMENSION MUST BE DMSUM2, AND WHOSE THIRD DIMENSION MUST
!            BE AT LEAST K+1.
!         THE PARAMETERS FOR EACH CLUSTER.
!
!   JMIN  INTEGER SCALAR.
!         CLUSTER WHOSE CENTER X IS CLOSEST TO.
!
!   DMIN  REAL SCALAR.
!         DISTANCE BETWEEN X AND CENTER OF JMIN CLUSTER.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 84-105.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMSUM1, DMSUM2
      DIMENSION SUM(DMSUM1,DMSUM2,*), X(*)
!
      INCLUDE 'DPCOMC.INC'
!
      JMIN=1
      DMIN=R1MACH(2)
!
!     CALCULATE DISTANCE TO EACH CLUSTER CENTER
!
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
!
!     UPDATE SUMMARY STATISTICS FOR CHOSEN CLUSTER
!
      DO 30 I=1,N
         IF(X(I).NE.XMISS) CALL SINGLE(X(I),SUM(2,I,JMIN),SUM(3,I,JMIN),   &
            SUM(4,I,JMIN),SUM(5,I,JMIN),SUM(6,I,JMIN),SUM(7,I,JMIN))
   30 CONTINUE
      RETURN
      END SUBROUTINE KMEANS
      SUBROUTINE KOUT(M, N, CLAB, RLAB, KK, DMSUM1,   &
                      DMSUM2, SUM, NCLUS, DCLUS, DD, R, CWORK)
!CCCC SUBROUTINE KOUT(MM, M, N, A, CLAB, RLAB, TITLE, KK, DMSUM1,
!CCCC*                DMSUM2, SUM, NCLUS, DCLUS, DD, R, CWORK, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      PRINTS OUTPUT FOR K-MEANS ALGORITHM
!
!   DESCRIPTION
!   -----------
!
!   1.  THE OUTPUT CONSISTS OF THE OVERALL STATISTICS FOR THE CURRENT
!       PARTITION, FOLLOWED BY THE STATISTICS FOR EACH CLUSTER.  THE
!       ANALYSIS OF VARIANCE IS COMPUTED FOR EACH VARIABLE IN THE
!       PARTITION.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
!            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE VARIABLES.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE CASES.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   KK    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CLUSTERS.
!
!   DMSUM1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST 7.
!
!   DMSUM2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE SECOND DIMENSION OF THE MATRIX SUM.  MUST BE AT LEAST N.
!
!   SUM   REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMSUM1, WHOSE SECOND
!            DIMENSION MUST BE DMSUM2, AND WHOSE THIRD DIMENSION MUST
!            BE AT LEAST K+1 (UNCHANGED ON OUTPUT).
!         THE PARAMETERS FOR EACH CLUSTER.
!
!         SUM(1,J,I) IS THE VALUE OF THE J-TH VARIABLE AT THE CENTER OF
!                       CLUSTER I
!         SUM(2,J,I) IS THE NUMBER OF NON-MISSING OBSERVATIONS FOR THE
!                       J-TH VARIABLE IN CLUSTER I
!         SUM(3,J,I) IS THE MEAN OF THE J-TH VARIABLE IN CLUSTER I
!         SUM(4,J,I) IS THE STANDARD DEVIATION OF THE J-TH VARIABLE IN
!                       CLUSTER I
!         SUM(5,J,I) IS THE MINIMUM OF THE J-TH VARIABLE IN CLUSTER I
!         SUM(6,J,I) IS THE MAXIMUM OF THE J-TH VARIABLE IN CLUSTER I
!         SUM(7,J,I) IS THE SUM OF SQUARED DEVIATIONS FOR THE J-TH
!                       VARIABLE FROM THE MEAN OF CLUSTER I
!
!         THE K+1-ST ROW OF SUM STORES THE SAME CALCULATIONS AS ABOVE
!            EXCEPT FOR THE ENTIRE DATA SET RATHER THAN FOR AN
!            INDIVIDUAL CLUSTER
!
!   NCLUS INTEGER VECTOR DIMENSIONED AT LEAST M (UNCHANGED ON OUTPUT).
!         NCLUS(I) IS THE CLUSTER FOR CASE I.
!
!   DCLUS REAL VECTOR DIMENSIONED AT LEAST M (UNCHANGED ON OUTPUT).
!         DCLUS(I) IS THE DISTANCE OF EACH CASE TO THE CLOSEST CLUSTER.
!
!   DD    REAL VECTOR DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   R     REAL VECTOR DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   CWORK VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 110.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER DMSUM1, DMSUM2, OUNIT
      INTEGER DMSUM1, DMSUM2
!CCCC DIMENSION SUM(DMSUM1,DMSUM2,*), NCLUS(*), DCLUS(*), A(MM,*), R(*),
      DIMENSION SUM(DMSUM1,DMSUM2,*), NCLUS(*), DCLUS(*), R(*), DD(*)
      CHARACTER*4 CLAB(*), RLAB(*), CWORK(*)
!CCCC CHARACTER*10 TITLE
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!     1/2008: MODIFIED FOR DATAPLOT TO USE DATAPLOT I/O
!
      DATA NPAGE,LC/0,0/
!
!     OUTPUT MEAN SQUARE CALCULATION OVER ALL CLUSTERS
!
  999 FORMAT(1X)
!
      NPAGE=NPAGE+1
!
      WRITE(ICOUT,1) NPAGE
    1 FORMAT('1',110X,I5)
      WRITE(ICOUT,2) KK
    2 FORMAT(' OVERALL MEAN SQUARE CALCULATIONS, FOR EACH VARIABLE, ',   &
             ' WITH',I5,'  CLUSTERS')
      CALL DPWRST('XXX','WRIT')
!
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
            SC=SC+SUM(2,J,K)
   10    CONTINUE
         DFB=KK-1.
         DFW=SC-DFB-1.
         ASSW=ASSW+SSW
         IF(SC.GT.0.) SSB=SSB-SD**2/SC
         IF(DFB.GT.0.) SSB=SSB/DFB
         IF(DFW.GT.0.) SSW=SSW/DFW
         RATIO=0.
         IF(LC.NE.0.AND.SSW.GT.0.) RATIO=(R(J)/SSW-1.)*(1.+DFW)+1.
         R(J)=SSW
!
         WRITE(ICOUT,3)CLAB(J),SSW,DFW,SSB,DFB,RATIO
    3    FORMAT(' VARIABLE',4X,A4,F20.6,   &
                '(WITHIN MEAN SQ.)',F4.0,'(WITHIN DF)',F20.6,   &
                '(BETWEEN MSQ)',F4.0,'(BETWEEN DF)',F6.1,'(FRATIO)')
         CALL DPWRST('XXX','WRIT')
!
   20 CONTINUE
!
      WRITE(ICOUT,4) ASSW
    4 FORMAT(' OVERALL WITHIN SUM OF SQUARES',F20.6)
      CALL DPWRST('XXX','WRIT')
!
      LC=LC+1
!
!     OUTPUT STATISTICS FOR EACH CLUSTER
!
      DO 50 K=1,KK
!
         WRITE(ICOUT,5)
    5    FORMAT(1X,131('-'))
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,6) K,KK
    6    FORMAT(I5,'   TH CLUSTER OF',I5)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,7)
    7    FORMAT('CLUSTER MEMBERS WITH THEIR DISTANCES TO THE ',   &
                'CLUSTER CENTER')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,17)(I,I=1,10)
   17    FORMAT(13X,10I11)
         CALL DPWRST('XXX','WRIT')
!
         L=0
         DO 30 I=1,M
            IF(NCLUS(I).EQ.K) THEN
               L=L+1
               CWORK(L)=RLAB(I)
               DD(L)=DCLUS(I)
            ENDIF
            IF ((L.GE.10.OR.I.GE.M).AND.L.NE.0) THEN
!
               WRITE(ICOUT,8)(CWORK(LL),LL=1,L)
    8          FORMAT(15X,10(7X,A4))
               CALL DPWRST('XXX','WRIT')
               WRITE(ICOUT,9)(DD(LL),LL=1,L)
    9          FORMAT(15X,10F11.4)
               CALL DPWRST('XXX','WRIT')
!
               L=0
            ENDIF
   30    CONTINUE
!
         WRITE(ICOUT,999)
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,11)
   11    FORMAT('SUMMARY STATISTICS FOR THE CLUSTER')
         CALL DPWRST('XXX','WRIT')
         WRITE(ICOUT,12)
   12    FORMAT(' LABEL',5X,'CENTRE',8X,'COUNT',12X,'AVE',   &
                 13X,'SD',11X,'XMIN',11X,'XMAX',12X,'SSQ')
         CALL DPWRST('XXX','WRIT')
!
         DO 40 J=1,N
!
            WRITE(ICOUT,13)CLAB(J),(SUM(I,J,K),I=1,7)
   13       FORMAT(1X,A4,7F15.6)
            CALL DPWRST('XXX','WRIT')
!
   40    CONTINUE
   50 CONTINUE
      RETURN
      END SUBROUTINE KOUT
      SUBROUTINE MIX(MM, M, N, A, CLAB1, CLAB2, RLAB, TITLE, K, MXITER,   &
                     NCOV, DMWORK, WORK1, DMWRK1, DMWRK2, WORK2, DMWRK3,   &
                     WORK3, IWORK,   &
                     ICAPTY, ICAPSW, IFORSW, IERR)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      FITS THE MIXTURE MODEL BY A MAXIMUM LOG-LIKEHOOD CRITERION
!
!   DESCRIPTION
!   -----------
!
!   1.  THE DATA ARE ASSUMED TO BE A RANDOM SAMPLE OF SIZE M FROM A
!       MIXTURE OF K MULTIVARIATE NORMAL DISTRIBUTIONS IN N DIMENSIONS.
!       THE PROBABILITY THAT THE J-TH OBSERVATION WAS DRAWN FROM THE
!       I-TH NORMAL FOR J=1,...,M I=1,...,K IS USED TO ESTIMATE WHICH
!       NORMAL EACH OBSERVATION WAS SAMPLED FROM, AND HENCE GROUP THE
!       OBSERVATIONS INTO K CLUSTERS.  THE CRITERION TO BE MAXIMIZED IS
!       THE LOG LIKELIHOOD
!
!             SUM LOG(G(I)) OVER I=1,...,M
!
!       WHERE G(I) IS THE PROBABILITY DENSITY OF THE I-TH OBSERVATION.
!
!       SEE PAGE 116 OF THE REFERENCE FOR A FURTHER DESCRIPTION OF G.
!
!   2.  THE MANY PARAMETERS PRESENT IN THE BETWEEN-NORMAL COVARIANCE
!       MATRICES REQUIRE MUCH DATA FOR THEIR ESTIMATION.  A RULE OF
!       THUMB IS THAT M SHOULD BE GREATER THAN (N+1)(N+2)K/2.  EVEN
!       WITH MANY OBSERVATIONS, THE PROCEDURE IS VULNERABLE TO
!       NONNORMALITY OR LINEAR DEPENDENCE AMONG THE VARIABLES.  TO
!       REDUCE THIS SENSITIVITY ONE CAN MAKE ASSUMPTIONS ON THESE
!       COVARIANCE MATRICES BY SETTING THE NCOV PARAMETER TO:
!
!       1  IF THE COVARIANCE MATRICES ARE ARBITRARY
!       2  IF THE COVARIANCE MATRICES IN DIFFERENT NORMALS ARE EQUAL
!       3  IF THE COVARIANCE MATRICES ARE EQUAL AND DIAGONAL
!       4  IF ALL VARIABLES HAVE THE SAME VARIANCE AND ARE PAIRWISE
!             INDEPENDENT
!
!   3.  AFTER EVERY 5 ITERATIONS, THE CLUSTER PROBABILITIES, MEANS, AND
!       DETERMINANTS OF COVARIANCE MATRICES ARE PRINTED OUT.  ALSO, THE
!       WITHIN-CLUSTER VARIANCES AND CORRELATIONS FOR EVERY PAIR OF
!       VARIABLES FOR EACH CLUSTER, AND FINALLY EVERY OBSERVATION AND
!       ITS BELONGING PROBABILILTY FOR EACH CLUSTER IS PRINTED.  THE
!       LOG LIKELIHOOD IS PRINTED AFTER EACH ITERATION.  THE ITERATIONS
!       STOP EITHER AFTER THE MAXIMUM NUMBER OF ITERATIONS HAVE BEEN
!       REACHED OR AFTER THE INCREASE IN THE LOG LIKELIHOOD FROM ONE
!       ITERATION TO ANOTHER IS LESS THAT .0001.  ALL OUTPUT IS SENT TO
!       FORTRAN UNIT OUNIT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
!            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE VARIABLES.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE CASES.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE DESIRED NUMBER OF CLUSTERS.
!
!   MXITER INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE MAXIMUM NUMBER OF ITERATIONS ALLOWED.
!
!   NCOV  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         DETERMINES STRUCTURE OF THE WITHIN-CLUSTER COVARIANCE MATRIX
!
!             NCOV = 1   GENERAL COVARIANCES
!             NCOV = 2   COVARIANCES EQUAL BETWEEN CLUSTERS
!             NCOV = 3   COVARIANCES EQUAL AND DIAGONAL
!             NCOV = 4   COVARIANCES SPHERICAL
!
!   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF THE MATRIX WORK1.  MUST BE AT LEAST
!            2*M+N+1.
!
!   WORK1 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND WHOSE
!            SECOND DIMENSION MUST BE AT LEAST K.
!         WORK MATRIX.
!
!   DMWRK1 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX WORK2.  MUST BE AT LEAST N.
!
!   DMWRK2 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE SECOND DIMENSION OF THE MATRIX WORK2.  MUST BE AT LEAST
!            N+1.
!
!   WORK2 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWRK1, WHOSE SECOND
!            DIMENSION MUST BE DMWRK2, AND WHOSE THIRD DIMENSION MUST BE
!            AT LEAST K+1.
!         WORK MATRIX.
!
!   DMWRK3 INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF THE MATRIX WORK3.  MUST BE AT LEAST
!             N.
!
!   WORK3 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWRK3 AND WHOSE
!            SECOND DIMENSION MUST BE AT LEAST N+1.
!         WORK MATRIX.
!
!   IWORK INTEGER VECTOR DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   OUTPUT PARAMETER
!   ----------------
!
!   IERR  INTEGER SCALAR.
!         ERROR FLAG.
!
!         IF IERR = 0, NO ERROR WAS DETECTED.
!
!         IF IERR = K, THE K-TH PIVOT BLOCK OF ONE OF THE COVARIANCE
!                      MATRICES WAS SINGULAR.  THEREFORE, AN INVERSE
!                      COULD NOT BE CALCULATED AND EXECUTION WAS
!                      TERMINATED.  THE ERROR FLAG WAS SET IN CMLIB
!                      SUBROUTINE SSIFA.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 113-129.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER OUNIT, P, U, PMIX, T, DMWORK, DMWRK1, DMWRK2, DMWRK3
      INTEGER P, U, PMIX, T, DMWORK, DMWRK1, DMWRK2, DMWRK3
      DIMENSION A(MM,*), WORK1(DMWORK,*), WORK2(DMWRK1,DMWRK2,*),   &
                 DETER(2), IWORK(*), WORK3(DMWRK3,*)
      CHARACTER*4 CLAB1(*), CLAB2(*)
      CHARACTER*8 RLAB(*)
      CHARACTER*10 TITLE
      CHARACTER*4 ICAPTY
      CHARACTER*4 ICAPSW
      CHARACTER*4 IFORSW
      LOGICAL DONE
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!     INITIALIZE
!
      DONE = .FALSE.
      P = 0
      U = P + M
      PMIX = U + N + 1
      T = PMIX
      XLL1 = -R1MACH(2)
      DO 10 J=1,K
         WORK2(1,N+1,J)=0.
   10 CONTINUE
      DO 30 I=1,M
         DO 20 J=1,K
            WORK1(P+I,J)=0.
   20    CONTINUE
         J=(I*K)/(M+1)+1
         WORK1(P+I,J)=1.
   30 CONTINUE
      DO 200 IT=1,MXITER
!
!     UPDATE MEANS AND COVARIANCES
!
         DO 40 J=1,K
            CALL CLUMOM(MM,M,N,A,J,WORK1(P+1,J),WORK1(U+1,J),DMWRK1,   &
                     DMWRK2,WORK2)
   40    CONTINUE
!
!     UPDATE WEIGHTS
!
         WW=0.
         DO 60 J=1,K
            WORK1(PMIX,J)=0.
            DO 50 I=1,M
               WORK1(PMIX,J)=WORK1(PMIX,J)+WORK1(P+I,J)
   50       CONTINUE
            WW=WW+WORK1(PMIX,J)
   60    CONTINUE
         DO 70 J=1,K
            IF(WW.NE.0.) WORK1(PMIX,J)=WORK1(PMIX,J)/WW
   70    CONTINUE
!
!     ADJUST FOR COVARIANCE STRUCTURE
!
         IF(NCOV.NE.1) THEN
            DO 100 I=1,N
               DO 105 II=1,N
                  WORK2(I,II,1)=WORK1(PMIX,1)*WORK2(I,II,1)
                  DO 80 J=2,K
                     WORK2(I,II,1)=WORK2(I,II,1)+WORK2(I,II,J)*   &
                                   WORK1(PMIX,J)
   80             CONTINUE
                  IF(NCOV.GE.3.AND.I.NE.II) WORK2(I,II,1)=0.
                  DO 90 J=2,K
                     WORK2(I,II,J)=WORK2(I,II,1)
   90             CONTINUE
  105          CONTINUE
  100       CONTINUE
            IF (NCOV.EQ.4) THEN
               CC=0.
               DO 110 I=1,N
                  CC=CC+WORK2(I,I,1)
  110          CONTINUE
               CC=CC/N
               DO 120 I=1,N
                  DO 125 J=1,K
                     WORK2(I,I,J)=CC
  125             CONTINUE
  120             CONTINUE
            ENDIF
         ENDIF
         II=IT-1
         IF(((II/5)*5.EQ.II.OR.DONE))   &
             CALL COVOUT(M,N,CLAB1,CLAB2,RLAB,TITLE,K,DMWORK,WORK1,   &
                  DMWRK1,DMWRK2,WORK2,WORK1(T+1,1),   &
                  ICAPTY,ICAPSW,IFORSW)
         IF (DONE) RETURN
!
!     UPDATE BELONGING PROBABILITIES
!
         DO 160 J=1,K
!
!     COMPUTE INVERSES AND DETERMINANTS OF COVARIANCE MATRICES
!
            DO 130 III = 1 , N
               DO 135 JJJ = 1 , N
                  WORK3(III,JJJ) = WORK2(III,JJJ,J)
 135           CONTINUE
 130        CONTINUE
!CCCC       CALL INVERT(DMWRK3,N,WORK3,DETER,WORK3(1,N+1),IWORK,IERR,
!CCCC*                  OUNIT)
            CALL INVERT(DMWRK3,N,WORK3,DETER,WORK3(1,N+1),IWORK,IERR)
            IF (IERR .NE. 0) RETURN
            DET = DETER(1) * (10. ** DETER(2))
            DO 140 III = 1 , N
               DO 145 JJJ = 1 , N
                  WORK2(III,JJJ,J) = WORK3(III,JJJ)
 145           CONTINUE
 140        CONTINUE
            IF(DET.EQ.0.) RETURN
            DET=SQRT(ABS(DET))
            WORK2(1,N+1,J)=DET
!
!     COMPUTE PROBABILITY DENSITY FOR THE I-TH OBSERVATION FROM THE J-TH
!     NORMAL
!
            DO 165 I=1,M
               S=0.
               DO 150 L=1,N
                  DO 155 LL=1,N
                     S=S+WORK2(L,LL,J)*(A(I,L)-WORK1(U+L,J))*(A(I,LL)-   &
                         WORK1(U+LL,J))
  155             CONTINUE
  150          CONTINUE
               IF(S.GT.100.) S=100.
               WORK1(T+I,J)=EXP(-S/2.)*WORK1(PMIX,J)/DET
  165       CONTINUE
  160    CONTINUE
!
!     COMPUTES LOG LIKELIHOOD
!
         XLL=0.
         DO 180 I=1,M
            S=0.
            DO 170 J=1,K
               S=S+WORK1(T+I,J)
  170       CONTINUE
            IF(S.EQ.0.) S=R1MACH(4)
            XLL=XLL+LOG(S)
            DO 185 J=1,K
               WORK1(T+I,J)=WORK1(T+I,J)/S
  185       CONTINUE
  180     CONTINUE
          IF (IPRINT.EQ.'ON') THEN
             WRITE(ICOUT,1) IT,XLL
    1        FORMAT(' ITERATION = ',I5,' LOG LIKELIHOOD = ',F12.6)
             CALL DPWRST('XXX','WRIT')
          ENDIF
!
!     UPDATE PROBABILITY THE I-TH OBSERVATION WAS DRAWN FROM THE J-TH
!     NORMAL
!
         DO 190 I=1,M
            DO 195 J=1,K
               XIT=MXITER
               ALPHA=1.+.7*IT/XIT
               WORK1(P+I,J)=ALPHA*WORK1(T+I,J)-(ALPHA-1.)*WORK1(P+I,J)
!
!     AT EVERY FIFTH ITERATION, SET PROBABILITIES TO EITHER ZERO OR ONE
!
               IF(IT.EQ.5.AND.WORK1(P+I,J).GT.0.5) WORK1(P+I,J)=1.
               IF(IT.EQ.5.AND.WORK1(P+I,J).LE.0.5) WORK1(P+I,J)=0.
               IF(WORK1(P+I,J).GT.1.) WORK1(P+I,J)=1.
               IF(WORK1(P+I,J).LT.0.) WORK1(P+I,J)=0.
  195      CONTINUE
  190    CONTINUE
!
!     RETURN IF NO CHANGE IN LOG LIKELIHOOD
!
         IF (XLL-XLL1 .LE. .00001) DONE = .TRUE.
         XLL1 = XLL
  200 CONTINUE
      RETURN
      END SUBROUTINE MIX
      SUBROUTINE MIXIND(MM, M, N, A, CLAB, RLAB, TITLE, K, DMWORK,   &
                        WORK1, WORK2)
!CCCC*                  WORK1, WORK2, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      FITS THE MIXTURE MODEL FROM K MULTIVARIATE NORMALS WHERE K IS
!      THE DESIRED NUMBER OF CLUSTERS.  THE VARIABLES ARE ASSUMED TO
!      HAVE VARIANCE CONSTANT OVER DIFFERENT CLUSTERS
!
!   DESCRIPTION
!   -----------
!
!   1.  THE DATA ARE ASSUMED TO BE A RANDOM SAMPLE OF SIZE M FROM A
!       MIXTURE OF K MULTIVARIATE NORMAL DISTRIBUTIONS IN N DIMENSIONS.
!       THE SUBROUTINE PREDICTS THE DISTRIBUTION THAT EACH OBSERVATION
!       WAS SAMPLED FROM AND HENCE GROUPS THE OBSERVATIONS INTO K
!       CLUSTERS.  SEE PAGE 113 OF THE REFERENCE FOR A FURTHER
!       DESCRIPTION OF THE MIXTURE ALGORITHM.
!
!   2.  THE ROUTINE BEGINS WITH THE CLUSTER OF ALL OBJECTS AND THEN
!       DIVIDES INTO TWO, THEN THREE, ..., THEN FINALLY K CLUSTERS.
!       THE RESULTS ARE PRINTED AFTER EACH DIVISION ON FORTRAN UNIT
!       OUNIT.  THE RESULTS CONSIST OF THE WITHIN-CLUSTER VARIANCES FOR
!       EACH VARIABLE, THEN SETS UP A COLUMN FOR EACH CLUSTER.  THE
!       MIXTURE PROBABILITY IS THE PROBABILITY THAT A NEW OBJECT WILL
!       BE GROUPED INTO THAT CLUSTER.  THEN THE MEANS OF THE VARIABLES
!       FOR THE CLUSTER ARE PRINTED, AS WELL AS THE PROBABILITIES THAT
!       EACH CASE BELONGS TO EACH CLUSTER.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
!            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE VARIABLES.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE CASES.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CLUSTERS.
!
!   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF THE MATRIX WORK1.  MUST BE AT LEAST
!            N+M+1.
!
!   WORK1 REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND WHOSE
!            SECOND DIMENSION MUST BE AT LEAST K.
!         WORK MATRIX.
!
!   WORK2 REAL VECTOR DIMENSIONED AT LEAST N.
!         WORK VECTOR.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 113-129.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER DMWORK, U, P, PMIX, OUNIT
      INTEGER DMWORK, U, P, PMIX
      DIMENSION A(MM,*), WORK1(DMWORK,*), WORK2(*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
!
      INCLUDE 'DPCOMC.INC'
!
      U = 0
      P = U + N
      PMIX = P + M + 1
      XM=99999.
      TH=.0001
      DO 160 KK=1,K
!
!     IF NOT FIRST PASS, FIND FURTHEST CASE FROM PRESENT MEANS
!
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
                  IF(DD.LT.DI) DI=DD
   20          CONTINUE
               IF(DI.GE.DM) THEN
                  DM=DI
                  IM=I
               ENDIF
   30       CONTINUE
         ENDIF
!
!     BEGIN A NEW CLUSTER LABELED KK
!
         DO 40 J=1,N
            WORK1(U+J,KK)=A(IM,J)
   40    CONTINUE
         WORK1(PMIX,KK)=EXP(0.5*N)
         ITER=25
         DO 150 IT=1,ITER
!
!     UPDATE PROBABILITIES OF BELONGING
!
            DO 90 I=1,M
               PP=0.
               DO 60 KL=1,KK
                  DD=0.
                  DO 50 J=1,N
                     IF(A(I,J).NE.XM.AND.KK.NE.1)   &
                        DD=DD+(A(I,J)-WORK1(U+J,KL))**2/(WORK2(J)*2.)
   50             CONTINUE
                  IF(DD.GT.100.) DD=100.
                  WORK1(P+I,KL)=WORK1(PMIX,KL)*EXP(-DD)
                  PP=PP+WORK1(P+I,KL)
   60          CONTINUE
               IF(PP.NE.0.) THEN
                  PN=0.
                  DO 70 KL=1,KK
                     WORK1(P+I,KL)=WORK1(P+I,KL)/PP
                     IF(WORK1(P+I,KL).LT.TH) WORK1(P+I,KL)=0.
                     PN =PN+WORK1(P+I,KL)
   70             CONTINUE
                  DO 80 KL=1,KK
                     WORK1(P+I,KL)=WORK1(P+I,KL)/PN
   80             CONTINUE
               ENDIF
   90       CONTINUE
!
!     UPDATE MIXTURE PROBABILITIES
!
            DO 100 KL=1,KK
               WORK1(PMIX,KL)=0.
               DO 105 I=1,M
                  WORK1(PMIX,KL)=WORK1(PMIX,KL)+WORK1(P+I,KL)/M
  105          CONTINUE
  100       CONTINUE
!
!     UPDATE CLUSTER ESTIMATES, EACH ONE A WEIGHTED MEAN
!
            DO 120 KL=1,KK
               DO 125 J=1,N
                  WORK1(U+J,KL)=0.
                  DO 110 I=1,M
                     WORK1(U+J,KL)=WORK1(U+J,KL)+A(I,J)*WORK1(P+I,KL)
  110             CONTINUE
                  IF(WORK1(PMIX,KL).NE.0.)   &
                     WORK1(U+J,KL)=WORK1(U+J,KL)/(WORK1(PMIX,KL)*M)
  125          CONTINUE
  120       CONTINUE
            DO 140 J=1,N
               WORK2(J)=0.
               DO 130 I=1,M
                  DO 135 KL=1,KK
                     WORK2(J)=WORK2(J)+(A(I,J)-WORK1(U+J,KL))**2*   &
                                        WORK1(P+I,KL)
  135             CONTINUE
  130          CONTINUE
               WORK2(J)=WORK2(J)/M
  140       CONTINUE
  150    CONTINUE
!
!     PRINT RESULTS OF ITERATION
!
!CCCC    IF (OUNIT .GT. 0) CALL MIXOUT(MM,M,N,A,CLAB,RLAB,TITLE,KK,
!CCCC*                                 DMWORK,WORK1,WORK2,OUNIT)
         CALL MIXOUT(M,N,CLAB,RLAB,TITLE,KK,   &
                     DMWORK,WORK1,WORK2)
  160 CONTINUE
      RETURN
      END SUBROUTINE MIXIND
      SUBROUTINE MIXOUT(M, N, CLAB, RLAB, TITLE, K, DMWORK,   &
                        WORK1, WORK2)
!CCCC SUBROUTINE MIXOUT(MM, M, N, A, CLAB, RLAB, TITLE, K, DMWORK,
!CCCC*                  WORK1, WORK2, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      PRINTS THE RESULTS FOR EACH ITERATION OF MIXIND
!
!   DESCRIPTION
!   -----------
!
!   1.  SEE SUBROUTINE MIXIND FOR DESCRIPTION OF OUTPUT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   K     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE CURRENT NUMBER OF CLUSTERS.
!
!   FOR OTHER PARAMETERS -- SEE SUBROUTINE MIXIND
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 129.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER DMWORK, U, P, PMIX, OUNIT
!CCCC DIMENSION A(MM,*), WORK1(DMWORK,*), WORK2(*)
      INTEGER DMWORK, U, P, PMIX
      DIMENSION WORK1(DMWORK,*), WORK2(*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      U = 0
      P = U + N
      PMIX = P + M + 1
!
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,1) TITLE,K
    1 FORMAT(' MIXTURE MODEL FOR',2X,A10,'WITH',I5,' CLUSTERS')
      CALL DPWRST('XXX','WRIT')
!
!     PRINT VARIANCES
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,2)
    2 FORMAT(' WITHIN CLUSTER VARIANCES')
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,222)(WORK2(J),CLAB(J),J=1,N)
  222 FORMAT(5(F15.6,'(',A4,')'))
      CALL DPWRST('XXX','WRIT')
!
!     PRINT CLUSTER PROBABILITIES
!
      WRITE(ICOUT,3)(KK,KK=1,K)
    3 FORMAT(9X,' CLUSTER', 9(I3,1X,' CLUSTER'))
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,4)(WORK1(PMIX,KK),KK=1,K)
    4 FORMAT(' MIXTURE PROBABILITIES',/(7X,10F12.6))
      CALL DPWRST('XXX','WRIT')
!
!     PRINT MEANS
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,5)
    5 FORMAT(' CLUSTER MEANS')
      CALL DPWRST('XXX','WRIT')
!
      DO 10 J=1,N
         WRITE(ICOUT,6) CLAB(J),(WORK1(U+J,KK),KK=1,K)
    6    FORMAT(1X,A4,2X,10F12.4)
         CALL DPWRST('XXX','WRIT')
   10 CONTINUE
!
!     PRINT PROBABILITIES
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','WRIT')
      WRITE(ICOUT,7)
    7 FORMAT(' BELONGING PROBABILITIES')
      CALL DPWRST('XXX','WRIT')
!
      DO 20 I=1,M
         WRITE(ICOUT,8) RLAB(I),(WORK1(P+I,KK),KK=1,K)
    8    FORMAT(1X,A4,2X,10F12.6)
         CALL DPWRST('XXX','WRIT')
   20 CONTINUE
      RETURN
      END SUBROUTINE MIXOUT
      SUBROUTINE QUICK(MM, M, N, A, RLAB, THRESH, XMISS,   &
                       NC, IWORK, OUNIT)
!CCCC SUBROUTINE QUICK(MM, M, N, A, CLAB, RLAB, TITLE, THRESH, XMISS,
!CCCC*                 NC, IWORK, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      FINDS A QUICK PARTITION OF THE CASES BY COMPARING, TO A USER-
!      DEFINED THRESHOLD, THE EUCLIDEAN DISTANCES TO THE EXISTING
!      CLUSTER LEADERS
!
!   DESCRIPTION
!   -----------
!
!   1.  INITIALLY, THE FIRST CASE WILL BE ASSIGNED TO THE FIRST CLUSTER
!       AND BECOMES THE LEADER OF THE FIRST CLUSTER.  THEN, GIVEN A NEW
!       CASE, CYCLE THROUGH THE EXISTING CLUSTERS IN ORDER.  PLACE THE
!       CASE IN THE FIRST CLUSTER WHERE THE DISTANCE BETWEEN THE CASE
!       AND THE CLUSTER LEADER IS LESS THAN THE THRESHOLD.  IF NO
!       CLUSTER EXISTS, PLACE THE CASE IN A NEW CLUSTER MAKING IT THE
!       CLUSTER LEADER.  ONCE THE MAXIMUM NUMBER OF DESIRED CLUSTERS
!       HAS BEEN REACHED, NO NEW CLUSTERS WILL BE FORMED AND CASES NOT
!       BELONGING TO AN EXISTING CLUSTER WILL BE IGNORED.
!
!   2.  THE DISTANCE FUNCTION USED IS THE EUCLIDEAN DISTANCE.  THE
!       VARIABLES SHOULD BE SCALED SIMILARLY (CLUSTER SUBROUTINE STAND
!       CAN BE USED TO STANDARDIZE THE VARIABLES).  ANY MISSING VALUES
!       WILL BE IGNORED IN THE DISTANCE CALCULATION.
!
!   3.  THE OUTPUT IS ON FORTRAN UNIT OUNIT, WHICH FOR EACH CLUSTER IS
!       THE CLUSTER LEADER AND ITS VALUES FOLLOWED BY THE OTHER
!       MEMBERS.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE FIRST DIMENSION OF THE MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND WHOSE SECOND
!            DIMENSION MUST BE AT LEAST N (UNCHANGED ON OUTPUT).
!         THE MATRIX OF DATA VALUES.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE VARIABLES.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M.
!            (UNCHANGED ON OUTPUT).
!         THE LABELS OF THE CASES.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   THRESH REAL SCALAR (UNCHANGED ON OUTPUT).
!         THRESHOLD SUCH THAT ANY TWO CASES WHOSE DISTANCE IS LESS
!         THAN THRESH WILL BE ASSIGNED TO THE SAME CLUSTER.
!
!   XMISS REAL SCALAR (UNCHANGED ON OUTPUT).
!         MISSING VALUE CODE.  IF A(I,J) = XMISS, THEN THE VALUE FOR THE
!         J-TH VARIABLE FOR THE I-TH CASE IS ASSUMED TO BE MISSING.
!
!   NC    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         MAXIMUM NUMBER OF CLUSTERS DESIRED.
!
!   IWORK INTEGER VECTOR DIMENSIONED AT LEAST M+NC.
!         WORK VECTOR.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 74-83.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      DIMENSION A(MM,*), IWORK(*)
      INTEGER OUNIT
!CCCC CHARACTER*4 CLAB(*), RLAB(*), AA(20)
      CHARACTER*4 RLAB(*), AA(20)
!CCCC CHARACTER*10 TITLE
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      LL = 0
      LC = LL + NC
      IF(OUNIT.LE.0) OUNIT = IPR
!
!     ASSIGN THE FIRST CASE TO THE FIRST CLUSTER
!
      KC=1
      IWORK(LL+1)=1
      DMAX=N * THRESH**2
      DO 30 I=1,M
         IWORK(LC+I)=0
         DO 20 KK=1,KC
            K=KC-KK+1
            L=IWORK(LL+K)
!
!     COMPUTES DISTANCE BETWEEN CASE AND CLUSTER LEADER
!
            DD=0.
            DC=0.
            DO 10 J=1,N
               IF (A(L,J).NE.XMISS.AND.A(I,J).NE.XMISS) THEN
                  DC=DC+1.
                  DD=DD+(A(L,J)-A(I,J))**2
!
!     GET NEXT CLUSTER IF DISTANCE IS TOO LARGE
!
                  IF(DD.GT.DMAX) GO TO 20
               ENDIF
   10       CONTINUE
            IF(DC.NE.0.) DD=SQRT(DD/DC)
!
!     ASSIGN CASE I TO CLUSTER K IF DISTANCE BELOW THRESHOLD
!
            IF (DD.LE.THRESH) THEN
               IWORK(LC+I)=K
               GO TO 30
            ENDIF
   20    CONTINUE
!
!     CREATE NEW CLUSTER AND LEADER
!
         IF (KC.NE.NC) THEN
            KC=KC+1
            IWORK(LC+I)=KC
            IWORK(LL+KC)=I
         ENDIF
   30 CONTINUE
!
!     OUTPUT CLUSTER LEADERS
!
!CCCC IF (OUNIT .LE. 0) GO TO 9000
      IF (IPR .LE. 0) GO TO 9000
!
      WRITE(ICOUT,1)
    1 FORMAT(' CLUSTER LEADERS')
      CALL DPWRST('XXX','WRIT')
!
      DO 40 K=1,KC
         I=IWORK(LL+K)
!
         WRITE(OUNIT,2) K, RLAB(I),(A(I,J),J=1,MAX(N,10))
    2    FORMAT(' CLUSTER',I4,2X,A4,10F11.4)
         CALL DPWRST('XXX','WRIT')
         IF (N.GT.10)THEN
            WRITE(OUNIT,12) (A(I,J),J=11,N)
   12       FORMAT(18X,10F11.4)
            CALL DPWRST('XXX','WRIT')
         ENDIF
   40 CONTINUE
!
      WRITE(ICOUT,3)
    3 FORMAT(1X)
      CALL DPWRST('XXX','WRIT')
!
!     OUTPUT CLUSTERS
!
      KC=KC+1
      DO 50 K=1,KC
         KK=K-1
         J=0
         DO 60 I=1,M
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
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE QUICK
      SUBROUTINE RSPLIT(MM, N, A, RLAB, IR, KA, TH, IORD, DMIWRK,   &
                        IWORK, DMWORK, WORK)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      FINDS OPTIMAL SPLIT OF THE CASES
!
!   DESCRIPTION
!   -----------
!
!   1.  INITIALLY, THE FIRST CLUSTER CONSISTS OF ALL CASES WITHIN THE
!       BLOCK IR AND THE SECOND CLUSTER IS EMPTY.  THE REDUCTION IN THE
!       WITHIN-CLUSTER SUM OF SQUARES FOR MOVING EACH CASE FROM THE
!       FIRST CLUSTER TO THE SECOND IS CALCULATED.  THE CASE THAT
!       REDUCES THE SUM OF SQUARES THE MOST IS MOVED AND THIS CONTINUES
!       UNTIL ALL CASES ARE MOVED WITH EACH REDUCTION STORED.  THEN THE
!       SPLIT THAT HAD THE SMALLEST REDUCTION OF ALL IS RETURNED AS THE
!       OPTIMUM SPLIT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM, M, N, A, RLAB, TH, IORD, DMIWRK, DMWORK -- SEE SUBROUTINE SPLIT2
!
!   IR    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         NUMBER OF BLOCK TO BE SPLIT.
!
!   KA    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         NUMBER OF BLOCKS.
!
!   IWORK INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND SECOND
!            DIMENSION MUST BE AT LEAST KA.
!         THE MATRIX DEFINING THE BOUNDARIES OF THE BLOCKS.
!
!         IWORK(1,I) IS 1 + THE FIRST ROW IN BLOCK I
!         IWORK(2,I) IS 1 + THE LAST ROW IN BLOCK I
!         IWORK(3,I) IS 1 + THE FIRST COLUMN IN BLOCK I
!         IWORK(4,I) IS 1 + THE LAST COLUMN IN BLOCK I
!
!   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
!            DIMENSION MUST BE AT LEAST MAX(M,N).
!
!         WORK(1,I) = FIRST CASE IN CASE CLUSTER I
!         WORK(2,I) = LAST CASE IN CASE CLUSTER I
!         WORK(3,I) = REDUCTION IN SSQ DUE TO SPLITTING
!         WORK(4,I) = LAST CASE IN FIRST CLUSTER OF SPLIT OF I
!         WORK(5,I) = 1 IF CASE IS INCLUDED IN PRESENT VARIABLE SPLIT
!         WORK(6,I) = NUMBER OF VARIABLES IN I-TH ROW OF PRESENT
!                        VARIABLE SPLIT
!         WORK(7,I) = MEAN OF I-TH CASE, FIRST VARIABLE CLUSTER
!         WORK(8,I) = NUMBER OF VARIABLES SECOND CLUSTER
!         WORK(9,I) = MEAN OF I-TH CASE, SECOND CLUSTER
!
!         WORK(10-18,I) ARE SIMILAR WITH VARIABLES AND CASES REVERSED.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 277.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMWORK, DMIWRK
      DIMENSION A(MM,*),IWORK(DMIWRK,*),WORK(DMWORK,*)
      CHARACTER*4 RLAB(*), C
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
      XM=99999.
      DO 10 J=1,N
         WORK(14,J)=0.
   10 CONTINUE
!
!     LOOK FOR BLOCKS WITHIN THRESHOLD
!
      IL=INT(WORK(1,IR))
      IU=INT(WORK(2,IR))
      DO 40 K=1,KA
         IF(IWORK(1,K).EQ.IL+1.AND.IWORK(2,K).EQ.IU+1) THEN
            JL=IWORK(3,K)
            JU=IWORK(4,K)
            IF(JL.LT.0) JL=-JL
!
!     COMPUTE VARIANCES
!
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
!
!     FIND BEST CASE SPLIT
!
      DO 50 J=1,N
         WORK(17,J)=0.
         WORK(18,J)=0.
   50 CONTINUE
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
                     DL=DL+(A(L,J)-WORK(16,J))**2*(WORK(15,J)+1)/   &
                           WORK(15,J)
                     DL=DL-(A(L,J)-WORK(18,J))**2*WORK(17,J)/   &
                           (WORK(17,J)+1)
                  ENDIF
   60          CONTINUE
               IF(DL.GT.DD) THEN
                  DD=DL
                  ID=L
               ENDIF
            ENDIF
   70    CONTINUE
!
!     INTERCHANGE ID AND II
!
         DO 80 J=1,N
            CC=A(II,J)
            A(II,J)=A(ID,J)
            A(ID,J)=CC
   80    CONTINUE
         C = RLAB(II)
         RLAB(II) = RLAB(ID)
         RLAB(ID) = C
!
!     UPDATE MEANS
!
         DO 90 J=1,N
            IF(WORK(14,J).NE.0.AND.A(II,J).NE.XM) THEN
               WORK(15,J)=WORK(15,J)-1.
               IF(WORK(15,J).NE.0.)WORK(16,J)=WORK(16,J)+   &
                                  (WORK(16,J)-A(II,J))/WORK(15,J)
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
      END SUBROUTINE RSPLIT
      SUBROUTINE SINGLE(X, COUNT, AVE, SD, XMIN, XMAX, SSQ)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      INCORPORATES A NEW VALUE INTO THE SUMMARY STATISTICS
!
!   INPUT PARAMETERS
!   ----------------
!
!   SEE SUBROUTINE BUILD FOR PARAMETER DESCRIPTIONS.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 109.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INCLUDE 'DPCOMC.INC'
!
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
      END SUBROUTINE SINGLE
      SUBROUTINE SPLIT(MM, N, A, RLAB, DMW, W, IL, IU, DMU, U,   &
                       WCLAB, IM, DM)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      SPLITS A ROW CLUSTER ON SELECTED VARIABLES
!
!   DESCRIPTION
!   -----------
!
!   1.  INITIALLY, THE FIRST CLUSTER CONSISTS OF ALL CASES BETWEEN IL
!       AND IU AND THE SECOND CLUSTER IS EMPTY.  THE WEIGHTED MEANS ARE
!       DETERMINED AND USED TO FIND THE REDUCTION IN THE WITHIN-CLUSTER
!       SUM OF SQUARES FOR MOVING EACH CASE FROM THE FIRST CLUSTER TO
!       THE SECOND.  THE OBJECT THAT REDUCES THE SUM OF SQUARES THE
!       MOST IS MOVED AND THIS CONTINUES UNTIL ALL OBJECTS ARE MOVED
!       WITH EACH REDUCTION STORED.  THEN THE SPLIT THAT HAD THE
!       SMALLEST REDUCTION OF ALL IS RETURNED AS THE OPTIMUM SPLIT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM, M, N, A, CLAB, RLAB, DMW, W -- SEE SUBROUTINE SPLIT1
!
!   IL, IU INTEGER SCALARS (UNCHANGED ON OUTPUT).
!         THE FIRST AND LAST OBJECTS IN THE BLOCK TO BE SPLIT.
!
!   DMU   INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX U.  MUST BE AT LEAST 4.
!
!   U     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMU AND SECOND
!            DIMENSION MUST BE AT LEAST N (CHANGED ON OUTPUT).
!         MATRIX OF CLUSTER MEANS.
!
!   OUTPUT PARAMETERS
!   -----------------
!
!   WCLAB INTEGER VECTOR DIMENSIONED AT LEAST N.
!         WCLAB(I) WILL STORE THE CLUSTER (EITHER 1 OR 2) OBJECT I WAS
!            ASSIGNED TO.
!
!   IM    INTEGER SCALAR.
!         THE BORDER OF THE SPLIT.  OBJECTS IL,...,IM WERE ASSIGNED TO
!            CLUSTER 1 AND OBJECTS IM+1,...,IU WERE ASSIGNED TO CLUSTER
!            2.
!
!   DM    INTEGER SCALAR.
!         THE REDUCTION IN THE WITHIN-CLUSTER SUM OF SQUARES.
!
!   REFERENCE
!   ---------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGE 272.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMW, DMU, WCLAB(*)
      DIMENSION W(DMW,*), A(MM,*), U(DMU,*)
!CCCC CHARACTER*4 CLAB(*), RLAB(*), CTEMP
      CHARACTER*4 RLAB(*), CTEMP
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!     FIND WEIGHTED MEAN OF ALL CASES
!
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
               U(2,J)=U(2,J)+WCLAB(J)
   20       CONTINUE
            U(1,J)=U(1,J)/U(2,J)
         ENDIF
   30 CONTINUE
      DM=0.
      DD=0.
      DO 80 IC=IL,IU
         II=IU-IC+IL
         DMAX=-R1MACH(2)
         IMAX=II
!
!     DETERMINE THE EFFECT OF MOVING ITH CASE
!
         DO 50 I=IL,II
            D=0.
            DO 40 J=1,N
               IF(WCLAB(J).NE.0) THEN
                 IF(U(2,J).EQ.W(I,J)) U(2,J)=W(I,J)+TH
                 D=D+W(I,J)*U(2,J)*(A(I,J)-U(1,J))**2/(U(2,J)-W(I,J))
                 D=D-W(I,J)*U(4,J)*(A(I,J)-U(3,J))**2/(U(4,J)+W(I,J))
               ENDIF
   40       CONTINUE
!
!     STORE THE LARGEST
!
            IF(D.GT.DMAX) THEN
               IMAX=I
               DMAX=D
            ENDIF
   50    CONTINUE
         DD=DD+DMAX
         IF(DD.GT.DM) IM=II-1
         IF(DD.GT.DM) DM=DD
!
!     UPDATE MEANS OF THE TWO CLUSTERS
!
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
!
!     INTERCHANGE SELECTED ROW WITH LAST FEASIBLE ROW
!
         DO 70 J=1,N
            C=A(I,J)
            A(I,J)=A(II,J)
            A(II,J)=C
            C=W(I,J)
            W(I,J)=W(II,J)
            W(II,J)=C
   70    CONTINUE
         CTEMP = RLAB(I)
         RLAB(I) = RLAB(II)
         RLAB(II) = CTEMP
   80 CONTINUE
      RETURN
      END SUBROUTINE SPLIT
      SUBROUTINE SPLIT1(MM, M, N, A, CLAB, RLAB, TITLE, DMW, W, TH,   &
                        KD, IWORK, DMIWRK, IWORK1, DMWORK, WORK, IERR,   &
                        OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      SPLITS THE CASES IN EACH VARIABLE UNTIL ALL WITHIN-CLUSTER
!      VARIANCES ARE SMALLER THAN A USER-SPECIFIED THRESHOLD
!
!   DESCRIPTION
!   -----------
!
!   1.  THE THRESHOLD IS THE LARGEST WITHIN-CLUSTER VARIANCE FOR EACH
!       VARIABLE.  THE VARIABLES MUST BE SCALED SIMILARLY (CLUSTER
!       SUBROUTINE STAND CAN BE USED TO STANDARDIZE THE VARIABLES).
!       THE ROUTINE STARTS WITH ONE CLUSTER OF ALL CASES FOR EACH
!       VARIABLE.  FOR EACH CLUSTER WHOSE VARIANCE IS LARGER THAN THE
!       THRESHOLD, IT IS SPLIT INTO TWO CLUSTERS SUCH THAT THE SUM OF
!       THE TWO WITHIN-CLUSTER VARIANCES IS SMALLEST.  THIS REPEATS
!       UNTIL ALL CLUSTER VARIANCES ARE SMALLER THAN THE THRESHOLD.
!       THE THRESHOLD SHOULD BE CHOSEN WISELY AS A LARGE THRESHOLD WILL
!       PRODUCE A FEW LARGE CLUSTERS AND A SMALL THRESHOLD WILL PRODUCE
!       MANY SMALL CLUSTERS.
!
!   2.  A MATRIX CAN BE USED TO WEIGH THE DATA VALUES.  A WEIGHT OF 1.
!       WILL GIVE THE VALUE FULL WEIGHT, A WEIGHT OF 0.  WILL GIVE THE
!       VALUE NO WEIGHT (IE.  A MISSING VALUE).  ALL WEIGHTS MUST BE
!       BETWEEN 0.  AND 1., AND THE WEIGHT MATRIX WILL BE DESTROYED
!       DURING EXECUTION.
!
!   3.  THE OUTPUT DIAGRAM IS AN ARRAY WITH THE VARIABLES LABELING THE
!       COLUMNS AND THE CASES LABELING THE ROWS AND THE VARIABLE VALUES
!       MULTIPLIED BY 10 AS THE ELEMENTS OF THE ARRAY.  THE HORIZONTAL
!       LINES OUTLINE THE BLOCKS AS EACH BLOCK IS ASSUMED TO CONTAIN
!       ONLY ONE VARIABLE AND HENCE, ONLY ONE COLUMN.  THE OUTPUT
!       DIAGRAM IS WRITTEN ON FORTRAN UNIT OUNIT.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF CASES.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
!            DIMENSION MUST BE AT LEAST M (CHANGED ON OUTPUT).
!         THE DATA MATRIX.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N
!            (UNCHANGED ON OUTPUT).
!         LABELS OF THE VARIABLES.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
!            (CHANGED ON OUTPUT).
!         LABELS OF THE CASES.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   DMW   INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX W.  MUST BE AT LEAST M.
!
!   W     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMW AND SECOND
!            DIMENSION MUST BE AT LEAST N (CHANGED ON OUTPUT).
!         W(I,J) IS THE WEIGHT OF VARIABLE J FOR CASE I AND SHOULD BE
!            BETWEEN 0. AND 1.  MISSING VALUES SHOULD BE GIVEN A WEIGHT
!            OF 0.
!
!   TH    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THRESHOLD VARIANCE FOR VARIABLES WITHIN CLUSTERS.
!
!   KD    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE MAXIMUM NUMBER OF BLOCKS ALLOCATED (SECOND DIMENSION OF
!            IWORK1).  THE SMALLEST K SHOULD BE IS M AND THE LARGEST IS
!            N*M.
!
!   IWORK INTEGER VECTOR DIMENSIONED AT LEAST 2*M+N.
!         WORK VECTOR.
!
!   DMIWRK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX IWORK1.  MUST BE AT LEAST 4.
!
!   IWORK1 INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND
!            SECOND DIMENSION MUST BE AT LEAST KD.
!         WORK MATRIX.
!
!   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX WORK.  MUST BE AT LEAST 4.
!
!   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
!            SECOND MUST BE AT LEAST N (CHANGED ON OUTPUT).
!         WORK MATRIX.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   OUTPUT PARAMETER
!   ----------------
!
!   IERR  INTEGER SCALAR.
!         ERROR FLAG.
!
!         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
!
!         IERR = 1, THE NUMBER OF BLOCKS NEEDED WAS LARGER THAN THE
!                   NUMBER OF BLOCKS ALLOCATED.  EXECUTION IS
!                   TERMINATED.  INCREASE KD.
!
!         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
!                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
!                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
!                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
!                   RESULTS FOR THAT CLUSTER.
!
!   REFERENCES
!   ----------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 251-271.
!
!     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
!        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMW, DMIWRK, DMWORK, OUNIT
      DIMENSION A(MM,*), W(DMW,*), IWORK1(DMIWRK,*), IWORK(*),   &
                 WORK(DMWORK,*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
!
      EXTERNAL SPLIT
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!     INTEGER WORK VECTOR OFFSETS
!
      IERR = 0
      IWCLAB=0
      INC1=N
      INC2=N+M
!
!     INITIALIZE CLUSTER OF ALL ROWS
!
      IWORK(INC1+1)=1
      IWORK(INC2+1)=M
      KR=0
      KC=0
   10 KR=KR+1
      IF(KR.EQ.0) GO TO  50
      SP=0.
      IL=IWORK(INC1+KR)
      IU=IWORK(INC2+KR)
!
!     IDENTIFY VARIABLES WITHIN THRESHOLD FOR WITHIN-CLUSTER VARIANCES
!
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
               GO TO  40
            ENDIF
            KC=KC+1
            IF (KC .GT. KD) THEN
               IF (OUNIT .GT. 0) THEN
                  WRITE(OUNIT,*)
!CC22             FORMAT(' TOO MANY BLOCKS FOR SPACE ALLOCATED, ',
!CCCC1                   'INCREASE KD AND SECOND DIMENSION OF IWORK1')
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
               W(I,J)=0.
   30       CONTINUE
         ENDIF
         IWORK(IWCLAB+J)=0
   40 CONTINUE
!
!     SPLIT CLUSTER KR IF NECESSARY
!
      IF(SP.EQ.0.) THEN
         KR=KR-2
         GO TO 10
      ENDIF
!CCCC CALL SPLIT(MM,M,N,A,CLAB,RLAB,DMW,W,IL,IU,DMWORK,WORK,
      CALL SPLIT(MM,N,A,RLAB,DMW,W,IL,IU,DMWORK,WORK,   &
                 IWORK(IWCLAB+1),IM,DM)
      IWORK(INC2+KR+1)=IWORK(INC2+KR)
      IWORK(INC2+KR)=IM
      IWORK(INC1+KR+1)=IM+1
      GO TO 10
  50  CALL BLOCK(MM, M+1, N+1, A, CLAB, RLAB, TITLE, KC, DMIWRK, IWORK1,   &
                 IERR, OUNIT)
      RETURN
      END SUBROUTINE SPLIT1
      SUBROUTINE SPLIT2(MM, M, N, A, CLAB, RLAB, TITLE, KD, TH, IORD,   &
                        DMIWRK, IWORK, DMWORK, WORK, IERR, OUNIT)
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      SPLITS MATRIX OF CASE-BY-VARIABLE DATA VALUES INTO BLOCKS UNTIL
!      ALL WITHIN-BLOCK VARIANCES ARE LESS THAN A GIVEN THRESHOLD.
!      INCLUDES USER-CONTROLLED CONSTRAINTS
!
!   DESCRIPTION
!   -----------
!
!   1.  THE THRESHOLD IS THE LARGEST VARIANCE FOR THE DATA VALUES IN
!       THE BLOCKS.  THE VARIABLES SHOULD BE SCALED SIMILARLY (CLUSTER
!       SUBROUTINE CAN BE USED TO STANDARDIZE THE VARIABLES.  THE
!       ROUTINE STARTS WITH THE DATA MATRIX AS ONE BLOCK.  THEN THE
!       BLOCK WITH THE LARGEST VARIANCE IS CHOSEN AND IF THAT VARIANCE
!       IS LARGER THAN THE THRESHOLD, THE BLOCK IS OPTIMALLY SPLIT BY
!       BOTH CASES AND VARIABLES.  THE VARIANCES FOR THE NEW BLOCKS ARE
!       DETERMINED AND THE PROCESS REPEATS BY FINDING THE NEWEST
!       LARGEST VARIANCE.  ONCE THE LARGEST VARIANCE IS LESS THAN THE
!       THRESHOLD, THE RESULTS ARE PRINTED IN A BLOCK DIAGRAM ON
!       FORTRAN UNIT OUNIT.  THE THRESHOLD SHOULD BE CHOSEN WISELY AS A
!       LARGE THRESHOLD WILL PRODUCE A FEW LARGE BLOCKS AND A SMALL
!       THRESHOLD WILL PRODUCE MANY SMALL BLOCKS.
!
!   2.  MISSING VALUES SHOULD BE REPRESENTED BY 99999.
!
!   3.  THE CASES AND/OR VARIABLES CAN BE CONSTRAINED BY THE IORD
!       PARAMETER.  SETTING IORD = 0 HAS BOTH CASES AND VARIABLES
!       UNCONSTRAINED; SETTING IORD = 1 CONSTRAINS ONLY CASES; SETTING
!       IORD = 2 CONSTRAINS ONLY VARIABLES; AND SETTING IORD = 3
!       CONSTRAINS BOTH CASES AND VARIABLES.
!
!   3.  THE BLOCK DIAGRAM IS THE DATA MATRIX WITH THE DATA VALUES
!       MULTIPLIED BY 10.  THE BLOCKS ARE OUTLINED BY THE VERTICAL AND
!       HORIZONTAL LINES.
!
!   INPUT PARAMETERS
!   ----------------
!
!   MM    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX A.  MUST BE AT LEAST M.
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF OBJECTS.
!
!   N     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF VARIABLES.
!
!   A     REAL MATRIX WHOSE FIRST DIMENSION MUST BE MM AND SECOND
!            DIMENSION MUST BE AT LEAST M (CHANGED ON OUTPUT).
!         THE DATA MATRIX.
!
!         A(I,J) IS THE VALUE FOR THE J-TH VARIABLE FOR THE I-TH CASE.
!
!   CLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST N
!            (CHANGED ON OUTPUT).
!         ORDERED LABELS OF THE COLUMNS.
!
!   RLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
!            (CHANGED ON OUTPUT).
!         ORDERED LABELS OF THE ROWS.
!
!   TITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF DATA SET.
!
!   KD    INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         MAXIMUM NUMBER OF BLOCKS.  SHOULD BE BETWEEN M AND N*M.
!
!   TH    REAL SCALAR (UNCHANGED ON OUTPUT).
!         THRESHOLD VARIANCE FOR DATA VALUES WITHIN A BLOCK.
!
!   IORD  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         ORDERING PARAMETER.
!
!            IORD = 0 CASES AND VARIABLES ARE UNCONSTRAINED
!            IORD = 1 CONSTRAIN CASES
!            IORD = 2 CONSTRAIN VARIABLES
!            IORD = 3 CASES AND VARIABLES ARE CONSTRAINED
!
!   DMIWRK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX IWORK.  MUST BE AT LEAST 4.
!
!   IWORK INTEGER MATRIX WHOSE FIRST DIMENSION MUST BE DMIWRK AND SECOND
!            DIMENSION MUST BE AT LEAST KC.
!         WORK MATRIX.
!
!   DMWORK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX WORK.  MUST BE AT LEAST 18.
!
!   WORK  REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMWORK AND SECOND
!            DIMENSION MUST BE AT LEAST MAX(M,N).
!         WORK MATRIX.
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   OUTPUT PARAMETER
!   ----------------
!
!   IERR  INTEGER SCALAR.
!         ERROR FLAG.
!
!         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
!
!         IERR = 1, THE NUMBER OF BLOCKS NEEDED WAS LARGER THAN THE
!                   NUMBER OF BLOCKS ALLOCATED.  EXECUTION IS
!                   TERMINATED.  INCREASE KD.
!
!         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
!                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
!                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
!                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
!                   RESULTS FOR THAT CLUSTER.
!
!   REFERENCES
!   ----------
!
!     HARTIGAN, J. A. (1972) "DIRECT CLUSTERING OF A DATA MATRIX."
!        JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION. VOL. 67,
!        PAGES 123-129.
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 251-277.
!
!     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
!        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
      INTEGER DMIWRK, DMWORK, OUNIT
      DIMENSION A(MM,*), IWORK(DMIWRK,*), WORK(DMWORK,*)
      CHARACTER*4 CLAB(*), RLAB(*)
      CHARACTER*10 TITLE
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOP2.INC'
!
!
!     INITIALIZE BLOCKS AND ROW AND COLUMN CLUSTERS
!
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
!CCCC CALL RSPLIT(MM,M,N,A,RLAB,IR,KA,TH,IORD,DMIWRK,IWORK,DMWORK,WORK)
      CALL RSPLIT(MM,N,A,RLAB,IR,KA,TH,IORD,DMIWRK,IWORK,DMWORK,WORK)
!CCCC CALL CSPLIT(MM,M,N,A,CLAB,IC,KA,TH,IORD,DMIWRK,IWORK,DMWORK,WORK)
      CALL CSPLIT(MM,M,A,CLAB,IC,KA,TH,IORD,DMIWRK,IWORK,DMWORK,WORK)
   10 IF (KA .GT. KD) THEN
         IF (OUNIT .GT. 0) THEN
            WRITE(ICOUT,1)
    1       FORMAT(' NUMBER OF BLOCKS ALLOCATED IS TOO SMALL. ',   &
                   'INCREASE KD')
         ENDIF
         IERR = 1
         RETURN
      ENDIF
!
!     FIND BEST CASE OR VARIABLE SPLIT
!
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
      IF(XB.EQ.0.) GO TO  60
!
!     SPLIT CASE CLUSTER
!
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
!CCCC    CALL RSPLIT(MM,M,N,A,RLAB,IB,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
         CALL RSPLIT(MM,N,A,RLAB,IB,KA,TH,IORD,DMIWRK,IWORK,DMWORK,   &
                     WORK)
!CCCC    CALL RSPLIT(MM,M,N,A,RLAB,KR,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
         CALL RSPLIT(MM,N,A,RLAB,KR,KA,TH,IORD,DMIWRK,IWORK,DMWORK,   &
                     WORK)
         GO TO 10
      ELSE
!
!    SPLIT VARIABLE CLUSTER
!
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
!CCCC    CALL CSPLIT(MM,M,N,A,CLAB,KC,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
         CALL CSPLIT(MM,M,A,CLAB,KC,KA,TH,IORD,DMIWRK,IWORK,DMWORK,   &
                     WORK)
!CCCC    CALL CSPLIT(MM,M,N,A,CLAB,JB,KA,TH,IORD,DMIWRK,IWORK,DMWORK,
         CALL CSPLIT(MM,M,A,CLAB,JB,KA,TH,IORD,DMIWRK,IWORK,DMWORK,   &
                     WORK)
         GO TO 10
      ENDIF
   60 CONTINUE
      DO 70 K=1,KA
        DO 75 J=1,4
           IF(IWORK(J,K).LT.0) IWORK(J,K)=-IWORK(J,K)
   75   CONTINUE
   70 CONTINUE
      CALL BLOCK(MM,M+1,N+1,A,CLAB,RLAB,TITLE,KA,DMIWRK,IWORK,IERR,   &
                 OUNIT)
      RETURN
      END SUBROUTINE SPLIT2
      SUBROUTINE SLINK(M, DMD, D, DMIWRK, IWORK, WORK)
!CCCC SUBROUTINE SLINK(M, DMD, D, DRLAB, DTITLE, DMIWRK, IWORK, WORK,
!CCCC*                 TLAB, IOUT, IERR, OUNIT)
!
!     2017/04: MODIIFIED FOR DATAPLOT.  SUPPRESS TREE1 AND
!              BLOCK1 ROUTINES (DISPLAYING CLUSTERS WILL BE
!              DONE BY THE CALLING DATAPLOT ROUTINE).
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!   PURPOSE
!   -------
!
!      UTILIZES THE SINGLE-LINKAGE CLUSTERING ALGORITHM TO CONSTRUCT
!      A TREE FROM A USER-SPECIFIED DISTANCE MATRIX
!
!   DESCRIPTION
!   -----------
!
!   1.  THE ALGORITHM TO COMPUTE SINGLE-LINKAGE TREES IS FOUND ON PAGES
!       191-195 OF THE REFERENCE.  THE DATA MATRIX ARE THE DISTANCES
!       BETWEEN THE CASES.  THE DISTANCES SHOULD BE CALCULATED ON
!       SCALED DATA (CLUSTER SUBROUTINE STAND CAN BE USED TO
!       STANDARDIZE THE VARIABLES).  THE OUTPUT CAN BE THE REGULAR
!       REGULAR TREE OUTPUT OR THE BLOCK REPRESENTATION OF THE TREE AND
!       IS WRITTEN ON FORTRAN UNIT OUNIT.
!
!   2.  THE REGULAR TREE LISTS THE CASES VERTICALLY AND HAS HORIZONTAL
!       LINES EMANATING FROM EACH CASE.  EACH CLUSTER WILL CORRESPOND
!       TO A VERTICAL LINE BETWEEN TWO HORIZONTAL LINES.  THE CASES
!       BETWEEN AND INCLUDED IN THE HORIZONTAL LINES ARE THE MEMBERS OF
!       THE CLUSTER.  THE DISTANCE FROM THE CASE NAMES TO THE VERTICAL
!       LINES CORRESPOND TO THE CLUSTER DIAMETER OR THE DISTANCE
!       BETWEEN THE FIRST AND LAST CASES.
!
!   3.  THE BLOCK DIAGRAM PRINTS THE DISTANCE MATRIX WITH THE CASES
!       LABELING BOTH HORIZONTAL AND VERTICAL AXES.  THE DISTANCES HAVE
!       BEEN MULTIPLIED BY 10.  THE HORIZONTAL BOUNDARIES OF THE BLOCKS
!       ARE REPRESENTED BY DASHES AND THE VERTICAL BOUNDARIES BY QUOTE
!       MARKS.  COMMAS REPRESENT THE CORNERS OF THE BLOCKS.
!
!   INPUT PARAMETERS
!   ----------------
!
!   M     INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE NUMBER OF OBJECTS.
!
!   DMD   INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX D.  MUST BE AT LEAST M.
!
!   D     REAL MATRIX WHOSE FIRST DIMENSION MUST BE DMD AND SECOND
!            DIMENSION MUST BE AT LEAST M (CHANGED ON OUTPUT).
!         THE MATRIX OF DISTANCES.  ORDERED ON OUTPUT SUCH THAT ALL
!            CLUSTERS ARE CONTIGUOUS IN THE ORDER.
!
!         D(I,J) = DISTANCE FROM CASE I TO CASE J
!
!   DRLAB VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M
!            (CHANGED ON OUTPUT).
!         LABELS OF THE CASES.  ORDERED ON OUTPUT.
!
!   DTITLE 10-CHARACTER VARIABLE (UNCHANGED ON OUTPUT).
!         TITLE OF THE DATA SET.
!
!   DMIWRK INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         THE LEADING DIMENSION OF MATRIX IWORK.  MUST BE AT LEAST 4.
!
!   IWORK INTEGER VECTOR WHOSE FIRST DIMENSION MUST BE AT DMIWRK AND
!            WHOSE SECOND DIMENSION MUST BE AT LEAST M+1.
!         WORK VECTOR.
!
!   WORK  REAL VECTOR DIMENSIONED AT LEAST M+1.
!         WORK VECTOR.
!
!   TLAB  VECTOR OF 4-CHARACTER VARIABLES DIMENSIONED AT LEAST M+1
!         WORK VECTOR.
!
!         IF THE REGULAR TREE DIAGRAM IS NOT CHOSEN, TLAB CAN HAVE
!            LENGTH 1.
!
!   IOUT  INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         OPTION FOR CHOOSING FORM OF OUTPUT.  IOUT HAS THE DECIMAL
!           EXPANSION AB SUCH THAT IF
!
!              A .NE. 0  THE REGULAR TREE WILL BE PRINTED
!              B .NE. 0  THE BLOCKED TREE WILL BE PRINTED
!
!   OUNIT INTEGER SCALAR (UNCHANGED ON OUTPUT).
!         UNIT NUMBER FOR OUTPUT.
!
!   OUTPUT PARAMETER
!   ----------------
!
!   IERR  INTEGER SCALAR.
!         ERROR FLAG.
!
!         IERR = 0, NO ERRORS WERE DETECTED DURING EXECUTION
!
!         IERR = 1, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
!                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
!                   CLUSTER AND ITS VALUES ARE PRINTED ON UNIT OUNIT.
!                   EXECUTION WILL CONTINUE WITH QUESTIONABLE RESULTS
!                   FOR THAT CLUSTER.  ERROR FLAG SET IN THE REGULAR
!                   TREE OUTPUT ROUTINE.
!
!         IERR = 2, EITHER THE FIRST AND LAST CASES OR THE CLUSTER
!                   DIAMETER FOR A CLUSTER IS OUT OF BOUNDS.  THE
!                   CLUSTER AND ITS BOUNDARIES ARE PRINTED ON UNIT
!                   OUNIT.  EXECUTION WILL CONTINUE WITH QUESTIONABLE
!                   RESULTS FOR THAT CLUSTER.  ERROR FLAG SET IN THE
!                   BLOCK TREE OUTPUT ROUTINE.
!
!   REFERENCES
!   ----------
!
!     HARTIGAN, J. A. (1975).  CLUSTERING ALGORITHMS, JOHN WILEY &
!        SONS, INC., NEW YORK.  PAGES 191-215.
!
!     HARTIGAN, J. A. (1975) PRINTER GRAPHICS FOR CLUSTERING. JOURNAL OF
!        STATISTICAL COMPUTATION AND SIMULATION. VOLUME 4,PAGES 187-213.
!
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!
!CCCC INTEGER DMIWRK, DMD, OUNIT
      INTEGER DMIWRK, DMD
      DIMENSION D(DMD,*), WORK(*), IWORK(DMIWRK,*)
!CCCC CHARACTER*4 DRLAB(*), TLAB(*), CTEMP
!CCCC CHARACTER*10 DTITLE
!
      INCLUDE 'DPCOMC.INC'
!
      IERR = 0
      DO 10 I = 1 , M
         IWORK(4,I) = I
   10 CONTINUE
      D(1,1)=R1MACH(2)
!
!     FIND THE OBJECT CLOSEST TO THE FIRST OBJECT
!
      DO 20 K=2 , M
         IF(D(1,K).LT.D(1,1)) THEN
            D(1,1)=D(1,K)
            IWORK(4,1)=K
         ENDIF
   20 CONTINUE
!
!     SET UP THE CLUSTERS
!
      DO 90 NEXT = 1,M-1
         J = NEXT + 1
         DMIN=R1MACH(2)
         IMIN=NEXT
!
!     FIND THE SMALLEST OF THE SMALLEST DISTANCES SO FAR COMPUTED
!
         DO 30 I=1,NEXT
            IF(D(I,I).LT.DMIN) THEN
               DMIN=D(I,I)
               IMIN=I
            ENDIF
   30    CONTINUE
         WORK(J+1)=100.*DMIN
         I=IWORK(4,IMIN)
!
!     PLACE THE OBJECT JUST DETERMINED IN THE NEXT POSITION BY
!     EXCHANGING IT WITH THE ONE CURRENTLY THERE
!
         DO 40 K=1,M
            A=D(I,K)
            D(I,K)=D(J,K)
            D(J,K)=A
   40    CONTINUE
!NIST    CTEMP = DRLAB(I)
!NIST    DRLAB(I)= DRLAB(J)
!NIST    DRLAB(J) = CTEMP
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
!
!     UPDATE THE SMALLEST DISTANCES
!
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
!
!     FIND BOUNDARIES OF CLUSTERS
!
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
!
!     SCALE CLUSTER DIAMETERS BETWEEN 1 AND 100
!
      XMAX = 0.
      DO 170 K=1,MM2
         IF(XMAX.LT.WORK(K)) XMAX=WORK(K)
  170 CONTINUE
      DO 180 K=1,MM2
         IWORK(3,K)=INT((WORK(K)*100)/XMAX)
  180 CONTINUE
!
!     REORDER DISTANCE MATRIX
!
      DO 190 I=1,M
         D(I,I)=0.
  190 CONTINUE
!
!     PRODUCE OUTPUT
!
!NIST IA = IOUT / 10
!NIST IB = MOD(IOUT,10)
!NIST IF (IA .NE. 0) THEN
!NIST    IF (OUNIT .GT. 0) WRITE(OUNIT,1)
!NIS1    FORMAT('1')
!NIST    TLAB(1) = DTITLE
!NIST    DO 200 I = 1 , M
!N200       TLAB(I+1) = DRLAB(I)
!NIST    CALL TREE1(M+1,M-1,DMIWRK,IWORK,TLAB,IERR,OUNIT)
!NIST ENDIF
!NIST IF (IB .NE. 0) THEN
!NIST    DO 210 K = 1, M-1
!NIST       IWORK(3,K) = IWORK(1,K)
!NIST       IWORK(4,K) = IWORK(2,K)
!210     CONTINUE
!NIST    CALL BLOCK(DMD,M+1,M+1,D,DRLAB,DRLAB,DTITLE,M-1,DMIWRK,IWORK,
!NIST*              IERR,OUNIT)
!NIST ENDIF
      RETURN
      END SUBROUTINE SLINK
      SUBROUTINE BSWAP(KK,NSAM,NREPR,DYSMA,DYSMB,BETER,DYS,SKY,S,IFLAG,   &
                       LARGE,ISUBRO,IBUGA3)
!NIST SUBROUTINE BSWAP(KK,NSAM,NREPR,DYS,SKY,S,LUB)
!
!     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
!
!        1. USE DATAPLOT OUTPUT
!        2. RECODE A BIT FOR BETTER READABILITY
!        3. ADD TEMPORARY ARRAYS TO CALL LIST
!
!     NOTE THAT WE CAN USE THIS ROUTINE FOR BOTH "CLARA" AND
!     "PAM".  JUST NEED TO ADD TEMPORARY ARRAYS TO CALL LIST.
!     PAM USES "NN" RATHER THAN "NSAMP", BUT THIS CAN BE TAKEN
!     CARE OF IN THE CALLING ROUTINE.  ALSO, THE FEEDBACK
!     MESSAGE IS SLIGHTLY DIFFERENT FOR PAM, SO ADD A FLAG
!     TO SPECIFY WHETHER BEING CALLED FROM CLARA OR PAM.
!
!     KK        = NUMBER OF CLUSTERS
!     NSAM      = NUMBER OF SAMPLES
!     NREPR     =
!     DYSMA     =
!     DYSMB     =
!     BETER     =
!     DYS       = THE OUTPUT MATRIX CONTAINING THE DISTANCES
!     SKY       = SUM OF DISSIMILARITIES/DISTANCES
!     S         =
!     LUB       = OUTPUT UNIT (DON'T USE)
!
      DIMENSION NREPR(*)
      DIMENSION DYS(*)
      DIMENSION DYSMA(*)
      DIMENSION DYSMB(*)
      DIMENSION BETER(*)
!
      INTEGER  MEET
      EXTERNAL MEET
!
      CHARACTER*4 IFLAG
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,11)
   11   FORMAT('***** AT THE BEGINNING OF BSWAP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,15)KK,NSAM,S,IFLAG
   15   FORMAT('KK,NSAM,S,IFLAG  = ',2I8,G15.7,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!C
!C   FIRST ALGORITHM: BUILD.
!C
      NBEST=0
      KBEST=0
      NMAX=0
      NNY=0
      DO 17 J=1,NSAM
        NREPR(J)=0
        DYSMA(J)=1.1*S+1.0
   17 CONTINUE
!
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
!
      IF(NNY.NE.KK)GO TO 20
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,9011)NNY
 9011   FORMAT('***** BEFORE 51 LOOP--, NNY = ',I8)
        CALL DPWRST('XXX','BUG ')
        DO 9020 II=1,NSAM
          WRITE(ICOUT,9025)II,NREPR(II)
 9025     FORMAT('II,NREPR(II) = ',2I8)
          CALL DPWRST('XXX','BUG ')
 9020   CONTINUE
      ENDIF
!
      SKY=0.
      DO 51 J=1,NSAM
        SKY=SKY+DYSMA(J)
   51 CONTINUE
      IF(KK.EQ.1)GO TO 9090
      RSAM=NSAM
      ASKY=SKY/RSAM
!NIST WRITE(LUB,9100)ASKY
!9100 FORMAT(1X/33H  RESULT OF BUILD FOR THIS SAMPLE/2X,
!NISTF '  AVERAGE DISTANCE  =   ',F12.3)
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
!C
!C   SECOND ALGORITHM: SWAP.
!C
   60 CONTINUE
!
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
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,16)
   16   FORMAT('***** AFTER 63 LOOP--')
        CALL DPWRST('XXX','BUG ')
        DO 116 I=1,NSAM
          WRITE(ICOUT,117)I,DYS(I),DYSMA(I),DYSMB(I)
  117     FORMAT('I,DYS(I),DYSMA(I),DYSMB(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','BUG ')
  116   CONTINUE
      ENDIF
!
      DZSKY=1.0
      DO 73 K=1,NSAM
        IF(NREPR(K).EQ.1)GO TO 73
!
        IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
          WRITE(ICOUT,171)K,NREPR(K),DZ,DZSKY
  171     FORMAT('K,NREPR(K),DZ,DZSKY = ',2I8,2G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,9017)DZSKY
 9017   FORMAT('***** AFTER 73 LOOP, DZSKY = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(DZSKY.GE.0.0)GO TO 9090
      NREPR(KBEST)=1
      NREPR(NBEST)=0
      SKY=SKY+DZSKY
      GO TO 60
 9090 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'SWAP')THEN
        WRITE(ICOUT,9001)
 9001   FORMAT('***** AT THE END OF BSWAP--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE BSWAP
      SUBROUTINE DYSTA(NSAM,JPP,NSEL,X,DYS,NDYST,AMISS,JHALT,   &
                       ISUBRO,IBUGA3)
!NIST SUBROUTINE DYSTA(NSAM,JPP,NSEL,X,MAXXX,MAXTT,DYS,NDYST,JTMD,
!NIST1 VALMD,JHALT,LUB,FNAMEB)
!
!     KAUFFMAN AND ROUSSEEUW CODE FROM CLARA ALGORITHM.  THIS
!     ROUTINE COMPUTES EITHER EUCLIDEAN DISTANCE OR MANHATTAN
!     DISTANCE BETWEEN ALL OBJECTS OF A SAMPLE (CLARA VERSION)
!
!       NSAM    = NUMBER OF SAMPLES
!       JPP     = NUMBER OF VARIABLES
!       NSEL    = INTEGER ARRAY CONTAINING OBJECTS SELECTED
!       X       = THE DATA MATRIX
!       MAXXX   = THE MAXIMUM OF ROWS TIMES COLUMNS, WE DON'T USE
!       MAXTT   = THE MAXIMUM OF VARIABLES (COLUMNS), WE DON'T USE
!       DYS     = THE OUTPUT MATRIX CONTAINING THE DISTANCES
!       NDYST   = 1 => EUCLIDEAN DISTANCES
!                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
!       JHALT   = SET TO 1 FOR ERROR CONDITION
!       JTMD    = FOR MISSING VALUES, WE DON'T USE
!       VALMD   = FOR MISSING VALUES, WE DON'T USE
!       LUB     = OUTPUT UNIT
!                 (WE USE DATAPLOT OUTPUT STRUCTURE, SO
!                 REMOVE THIS)
!       FNAMEB  = OUTPUT FILE NAME
!                 (WE USE DATAPLOT OUTPUT STRUCTURE, SO
!                 REMOVE THIS)
!
!     CHANGES FOR INCORPORATING INTO DATAPLOT:
!
!        1. USE DATAPLOT I/O ROUTINES
!        2. FOR DATAPLOT, ONLY USE A SINGLE VALUE TO DENOTE
!           MISSING VALUES
!        3. RECODED SLIGHTLY TO REDUCE USE OF GO TO's (THIS
!           WAS JUST TO IMPROVE READABILITY OF THE CODE)
!
!NIST DIMENSION X(MAXXX),DYS(4951)
!NIST DIMENSION NSEL(100),JTMD(MAXTT),VALMD(MAXTT)
!NIST CHARACTER*30 FNAMEB
!
      DIMENSION X(*)
      DIMENSION DYS(*)
      DIMENSION NSEL(*)
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DYSTA--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NSAM,JPP,AMISS
   55   FORMAT('NSAM,JPP,AMISS  = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 56 I=1,NSAM
          WRITE(ICOUT,57)I,NSEL(I)
   57     FORMAT('I,NSEL(I) = ',2I8)
          CALL DPWRST('XXX','BUG ')
   56   CONTINUE
        DO 58 I=1,JPP
          WRITE(ICOUT,59)I,X(I)
   59     FORMAT('I,X(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   58   CONTINUE
      ENDIF
!
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
!NIST          IF(JTMD(J).GE.0)GO TO 40
!NIST          IF(X(NUMLJ).EQ.VALMD(J))GO TO 30
!NIST          IF(X(NUMKJ).EQ.VALMD(J))GO TO 30
               IF(X(NUMLJ).EQ.AMISS)GO TO 30
               IF(X(NUMKJ).EQ.AMISS)GO TO 30
!NI40          CONTINUE
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
!NIST         WRITE(LUB,9400)LSEL,KSEL
!9400 FORMAT(1X,8H OBJECTS,I8,4H AND,I8,23H HAVE NO COMMON MEASURE,
!NISTF6HMENTS,/49H  SO THE DISTANCE BETWEEN THEM CANNOT BE COMPUTED)
!NIST         IF(FNAMEB.NE.'CON')WRITE(*,9400)LSEL,KSEL
              WRITE(ICOUT,999)
  999         FORMAT(1X)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9401)LSEL,KSEL
 9401         FORMAT('***** OBJECTS ',I8,' AND ',I8,' HAVE NO ',   &
                     'COMMON MEASURE, SO')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9403)
 9403         FORMAT('      THE DISTANCE BETWEEN THEM CANNOT BE ',   &
                     'COMPUTED.')
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
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DYSTA--')
        CALL DPWRST('XXX','BUG ')
        DO 9056 I=1,NLK
          WRITE(ICOUT,9057)I,DYS(I)
 9057     FORMAT('I,DYS(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9056   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DYSTA
      FUNCTION MEET(L,J)
      IF(L.GT.J)THEN
!C
!C      J LESS THAN L
!C
        MEET=(L-2)*(L-1)/2+J+1
      ELSEIF(L.EQ.J)THEN
!C
!C      J EQUALS L
!C
        MEET=1
      ELSE
!C
!C      L LESS THAN J
!C
        MEET=(J-2)*(J-1)/2+L+1
      ENDIF
!
      RETURN
      END FUNCTION MEET
      SUBROUTINE DYSTAP(NN,JPP,MAXNN,MAXPP,X,DYS,NDYST,AMISS,JHALT,   &
                       ISUBRO,IBUGA3)
!
!NIST SUBROUTINE DYSTA(NN,JPP,MAXNN,MAXPP,MAXHH,X,DYS,NDYST,JTMD,
!NIST1 VALMD,LAB,JHALT,LUB,FNAMEB)
!
!     KAUFFMAN AND ROUSSEEUW CODE FROM PAM ALGORITHM.  THIS
!     ROUTINE COMPUTES EITHER EUCLIDEAN DISTANCE OR MANHATTAN
!     DISTANCE BETWEEN ALL OBJECTS OF A SAMPLE (CLARA VERSION)
!
!       NN      = NUMBER OF SAMPLES
!       JPP     = NUMBER OF VARIABLES
!       MAXN    = THE ROW DIMENSION OF X
!       MAXPP   = THE COLUMN DIMENSION OF X
!       MAXHH   = THE MAXIMUM DIMENSION FOR THE DISTANCES
!                 (DATAPLOT DOES NOT USE)
!       X       = THE DATA MATRIX
!       DYS     = THE OUTPUT MATRIX CONTAINING THE DISTANCES
!       NDYST   = 1 => EUCLIDEAN DISTANCES
!                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
!       JTMD    = FOR MISSING VALUES, WE DON'T USE
!       VALMD   = FOR MISSING VALUES, WE DON'T USE
!       LAB     = ...
!       JHALT   = SET TO 1 FOR ERROR CONDITION
!       LUB     = OUTPUT UNIT
!                 (DATAPLOT DOES NOT USE)
!       FNAMEB  = OUTPUT FILE NAME
!                 (DATAPLOT DOES NOT USE)
!
!     CHANGES FOR INCORPORATING INTO DATAPLOT:
!
!        1. USE DATAPLOT I/O ROUTINES
!        2. FOR DATAPLOT, ONLY USE A SINGLE VALUE TO DENOTE
!           MISSING VALUES
!        3. RECODED SLIGHTLY TO REDUCE USE OF GO TO's (THIS
!           WAS JUST TO IMPROVE READABILITY OF THE CODE)
!
!NIST DIMENSION X(MAXNN,MAXPP),DYS(MAXHH),JTMD(MAXPP),VALMD(MAXPP)
!NIST CHARACTER LAB(3,MAXNN)
!NIST CHARACTER*30 FNAMEB
      DIMENSION X(MAXNN,MAXPP)
      DIMENSION DYS(*)
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DYSTAP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NN,JPP,AMISS
   55   FORMAT('NN,JPP,AMISS  = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 58 I=1,NN
          WRITE(ICOUT,59)I,(X(I,J),J=1,JPP)
   59     FORMAT('I,X(I,J) = ',I8,30G15.7)
          CALL DPWRST('XXX','BUG ')
   58   CONTINUE
      ENDIF
!
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
!NIST       IF(JTMD(J).GE.0)GO TO  40
!NIST       IF(X(L,J).EQ.VALMD(J))GO TO  30
!NIST       IF(X(K,J).EQ.VALMD(J))GO TO  30
            IF(X(L,J).EQ.AMISS)GO TO 30
            IF(X(K,J).EQ.AMISS)GO TO 30
!NI40       CONTINUE
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
!NIST       WRITE(LUB,9400)LAB(1,L),LAB(2,L),LAB(3,L),LAB(1,K),LAB(2,K)
!NIST1                     ,LAB(3,K)
!9400       FORMAT('  OBJECTS ',3A1,' AND ',3A1,
!NIST1             ' HAVE NO COMMON MEASUREMENTS.')
!NIST       IF(FNAMEB.NE.'CON')WRITE(*,9400)LAB(1,L),LAB(2,L),LAB(3,L),
!NIST1                                      LAB(1,K),LAB(2,K),LAB(3,K)
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9401)L,K
 9401       FORMAT('***** OBJECTS ',I8,' AND ',I8,' HAVE NO ',   &
                   'COMMON MEASURE, SO')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9403)
 9403       FORMAT('      THE DISTANCE BETWEEN THEM CANNOT BE ',   &
                   'COMPUTED.')
            CALL DPWRST('XXX','BUG ')
            DYS(NLK)=0.0
            GO TO  20
          ENDIF
          IF(NDYST.EQ.1)THEN
            DYS(NLK)=SQRT(CLK*(PP/RPRES))
          ELSE
            DYS(NLK)=CLK*(PP/RPRES)
          ENDIF
   20   CONTINUE
  100 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'STAP')THEN
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF DYSTA--')
        CALL DPWRST('XXX','BUG ')
        DO 9056 I=1,NLK
          WRITE(ICOUT,9057)I,DYS(I)
 9057     FORMAT('I,DYS(I) = ',I8,G15.7)
          CALL DPWRST('XXX','BUG ')
 9056   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE DYSTAP
      SUBROUTINE SELEC(KK,NN,JPP,NSTAN,NDYST,ZB,NSAM,MDATA,   &
                       AMISS,NREPR,NSEL,DYS,X,NR,NAFS,   &
                       TTD,RADUS,RATT,   &
                       TTNEW,RDNEW,   &
                       NRNEW,NSNEW,NPNEW,NS,NP,NEW,   &
                       LARGE,ISUBRO,IBUGA3)
!
!     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
!
!        1. USE DATAPLOT OUTPUT
!        2. RECODE A BIT FOR BETTER READABILITY
!
!     KK        = NUMBER OF CLUSTERS
!     NN        = NUMBER OF ROWS (CASES)
!     JPP       = NUMBER OF COLUMNS (VARIABLES)
!     NSTAN     = 0 => NO STANDARDIZATION APPLIED
!                 1 =>    STANDARDIZATION APPLIED
!     NDYST     = 1 => EUCLIDEAN DISTANCES
!                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
!     ZB        = TOTAL DISTANCE
!     NSAM      = NUMBER OF SAMPLES
!     MDATA     = 0 => NO MISSING DATA
!                 1 => THERE IS MISSING DATA
!     AMISS     = NUMBER DENOTING A MISSING VALUE
!     NREPR     = INTEGER ARRAY
!                    0 => NOT A REPRESENTATIVE OBJECT
!                    1 => IS  A REPRESENTATIVE OBJECT
!     NSEL      = INTEGER ARRAY CONTAINING OBJECTS SELECTED
!     DYS       = THE OUTPUT MATRIX CONTAINING THE DISTANCES
!     X         = THE DATA MATRIX
!     MAXTT     = THE MAXIMUM OF ROWS TIMES COLUMNS
!     MAXXX     = THE MAXIMUM NUBER OF VARIABLES (COLUMNS)
!     NR        =
!     NAFS      =
!     TTD       = AVERAGE DISTANCE TO EACH MEDOID
!     RADUS     = MAXIMUM DISTANCE TO EACH MEDOID
!     RATT      = MAXIMUM DISTANCE OF MEDOID DIVIDED BY MINIMUM
!                 DISTANCE TO ANOTHER MEDOID
!
!NIST SUBROUTINE SELEC(KK,NN,JPP,NSTAN,NDYST,ZB,NSAM,LUB,MDATA,
!NISTF JTMD,VALMD,NREPR,NSEL,DYS,X,MAXXX,MAXTT,NR,NAFS,
!NISTF TTD,RADUS,RATT)
!NIST DIMENSION NREPR(100),NSEL(100),DYS(4951),X(MAXXX),NEW(30)
!NIST DIMENSION NRNEW(30),NSNEW(30),NPNEW(30),TTNEW(30),RDNEW(30)
!NIST DIMENSION NS(30),NR(30),NP(30),TTD(30),RADUS(30),RATT(30)
!NIST DIMENSION JTMD(MAXTT),VALMD(MAXTT)
      PARAMETER (MAXCLU=30)
!
      DIMENSION DYS(*)
      DIMENSION X(*)
      DIMENSION TTD(*)
      DIMENSION RADUS(*)
      DIMENSION RATT(*)
      DIMENSION NREPR(*)
      DIMENSION NSEL(*)
      DIMENSION NR(*)
!
      DIMENSION NRNEW(*)
      DIMENSION NSNEW(*)
      DIMENSION NPNEW(*)
      DIMENSION TTNEW(*)
      DIMENSION RDNEW(*)
      DIMENSION NS(*)
      DIMENSION NP(*)
      DIMENSION NEW(*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ELEC')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF SELEC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)KK,NN,JPP,NSTAN,NDYST,NSAM
   55   FORMAT('KK,NN,JPP,NSTAN,NDYST,NSAM  = ',6I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!C
!C   NAFS = 1 IF A DISTANCE CANNOT BE CALCULATED
!C
      NAFS=0
      JKABC=0
      DNULL=0.0
!C
!C    IDENTIFICATION OF REPRESENTATIVE OBJECTS, AND INITIALIZATIONS
!C
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
!C
!C   ASSIGNMENT OF THE OBJECTS OF THE ENTIRE DATA SET TO A CLUSTER,
!C   COMPUTATION OF SOME STATISTICS, DETERMINATION OF THE
!C   NEW ORDERING OF THE CLUSTERS
!C
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
!NIST   WRITE(LUB,9000)JN
!9000   FORMAT('  OBJECT',I5,37H DOESNT HAVE COMMON MEASUREMENTS WITH,
!NISTF         53H ANY OF THE MEDOIDS AND THEREFORE CANNOT BE ASSIGNED.)
!NIST   WRITE(LUB,9002)
!9002   FORMAT(1X,' THIS SAMPLE IS NOT CONSIDERED ANY FURTHER')
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9000)JN
 9000   FORMAT('****** OBJECT ',I5,' DOES NOT HAVE COMMON MEASUREMENTS',   &
               'WITH ANY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)
 9001   FORMAT('       OF THE MEDOIDS AND THEREFORE CANNOT BE ',   &
               'ASSIGNED.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9002)
 9002   FORMAT('       THIS SAMPLE IS NOT CONSIDERED ANY FURTHER.')
        CALL DPWRST('XXX','BUG ')
        NAFS=1
        GO TO 9090
      ENDIF
!
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
!
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
!C
!C    A PERMUTATION IS CARRIED OUT ON VECTORS NR,NS,NP,TTD,RADUS
!C    USING THE INFORMATION IN VECTOR NEW.
!C
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
!C
!C   PRINTING OF RESULTS FOR ENTIRE DATA SET
!C
      RNN=NN
      ZM=ZB/RNN
!NIST WRITE(LUB,9010)ZB,ZM
!9010 FORMAT(33H  RESULTS FOR THE ENTIRE DATA SET/3X,
!NISTF       20H TOTAL DISTANCE    =,F15.3/3X,20H AVERAGE DISTANCE  =,F15.3)
!
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
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        IF(NSTAN.EQ.0)THEN
!NIST     WRITE(LUB,9020)
!9020     FORMAT(/46H  CLUSTER SIZE MEDOID    COORDINATES OF MEDOID)
          WRITE(ICOUT,9020)
 9020     FORMAT('  CLUSTER SIZE MEDOID    COORDINATES OF MEDOID')
          CALL DPWRST('XXX','BUG ')
        ELSEIF(NSTAN.NE.0)THEN
!NIST     WRITE(LUB,9025)
!9025     FORMAT(/46H  CLUSTER SIZE MEDOID    COORDINATES OF MEDOID,
!NISTF           28H (STANDARDIZED MEASUREMENTS))
          WRITE(ICOUT,9025)
 9025     FORMAT('  CLUSTER SIZE MEDOID   COORDINATES OF MEDOID ',   &
                 ' (STANDARDIZED MEASUREMENTS)')
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
      DO 100  JK=1,KK
        JKA=(NR(JK)-1)*JPP+1
        JKB=JKA-1+JPP
        IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
!NIST     WRITE(LUB,9030)JK,NS(JK),NR(JK),(X(J),J=JKA,JKB)
!9030     FORMAT(/1X,I8,I5,I7,2X,5F11.2,20(/23X,5F11.2))
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
          DO 9031 L=1,NLOOP
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
!NIST WRITE(LUB,9040)(TTD(J),J=1,KK)
!9040 FORMAT(/33H  AVERAGE DISTANCE TO EACH MEDOID,6(/2X,5F12.3))
!NIST WRITE(LUB,9050)(RADUS(J),J=1,KK)
!9050 FORMAT(/33H  MAXIMUM DISTANCE TO EACH MEDOID,6(/2X,5F12.3))
      IF(IPRINT.EQ.'ON' .AND. LARGE.EQ.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9040)
 9040   FORMAT('  AVERAGE DISTANCE TO EACH MEDOID')
        CALL DPWRST('XXX','BUG ')
        NLOOP=KK/5
        IF(MOD(KK,5).GT.0)NLOOP=NLOOP+1
        JSTRT=1
        DO 9041 L=1,NLOOP
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
        DO 9051 L=1,NLOOP
          JSTOP=JSTRT+1
          IF(JSTOP.GT.KK)JSTOP=KK
          WRITE(ICOUT,9042)(RADUS(J),J=JSTRT,JSTOP)
          CALL DPWRST('XXX','BUG ')
          JSTRT=JSTRT+5
 9051   CONTINUE
      ENDIF
!
      IF(KK.GT.1)THEN
!C
!C       COMPUTATION OF MINIMAL DISTANCE OF MEDOID KA TO ANY
!C       OTHER MEDOID FOR COMPARISON WITH THE RADIUS OF CLUSTER KA.
!C
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
!NIST         WRITE(LUB,9054)KA,KB
!9054         FORMAT(/51H  THE DISSIMILARITY BETWEEN THE MEDOIDS OF CLUSTERS,
!NISTF               I3,5H AND ,I3,9H IS ZERO.)
!NIST         WRITE(LUB,9056)
!9056         FORMAT('  IN THE FOLLOWING VECTOR A VALUE OF -1 IS GIVEN TO',
!NISTF               ' BOTH CLUSTERS.')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9054)
 9054         FORMAT('THE DISSIMILARITY BETWEEN THE MEDOIDS OF ',   &
                     'CLUSTERS',I3,' AND ',I3,' IS ZERO.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9056)
 9056         FORMAT('IN THE FOLLOWING VECTOR A VALUE OF -1 IS GIVEN ',   &
                     'TO BOTH CLUSTERS.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
            RATT(KA)=-1.
  110     CONTINUE
          IF(RATT(KA).GT.(-0.5))RATT(KA)=RADUS(KA)/RATT(KA)
  120   CONTINUE
!NIST   WRITE(LUB,9060)(RATT(J),J=1,KK)
!9060   FORMAT(/49H  MAXIMUM DISTANCE TO A MEDOID DIVIDED BY MINIMUM/
!NISTF        42H  DISTANCE OF THE MEDOID TO ANOTHER MEDOID,6(/2X,5F12.3))
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
          DO 9063 L=1,NLOOP
            JSTOP=JSTRT+1
            IF(JSTOP.GT.KK)JSTOP=KK
            WRITE(ICOUT,9064)(RATT(J),J=JSTRT,JSTOP)
 9064       FORMAT(2X,5F11.2)
            CALL DPWRST('XXX','BUG ')
            JSTRT=JSTRT+5
 9063     CONTINUE
        ENDIF
      ENDIF
!
 9090 CONTINUE
      RETURN
      END SUBROUTINE SELEC
      SUBROUTINE RESUL(KK,NN,JPP,LARGE,NDYST,X,NRX,AMISS,IC1,IOUNI1)
!NIST SUBROUTINE RESUL(KK,NN,JPP,LARGE,NDYST,LUB,MDATA,JTMD,
!NIST1                 VALMD,X,MAXXX,MAXTT,NRX)
!
!     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
!
!        1. USE DATAPLOT OUTPUT
!        2. RECODE A BIT FOR BETTER READABILITY
!
!     KK        = NUMBER OF CLUSTERS
!     NN        = NUMBER OF ROWS (CASES)
!     JPP       = NUMBER OF COLUMNS (VARIABLES)
!     LARGE     = SPECIFY WHAT WILL BE OUTPUT
!     NDYST     = 1 => EUCLIDEAN DISTANCES
!                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
!     LUB       = OUTPUT UNIT (WE DON'T USE)
!     MDATA     = MISSING VALUES PRESENT (WE DON'T USE)
!     JTMD      = FOR MISSING VALUES (WE DON'T USE)
!     VALMD     = FOR MISSING VALUES (WE DON'T USE)
!     X         = THE DATA MATRIX
!     MAXXX     = THE MAXIMUM OF ROWS TIMES COLUMNS (WE DON'T USE)
!     MAXTT     = THE MAXIMUM NUMBER OF VARIABLES (COLUMNS) (WE DON'T USE)
!
      DIMENSION X(*)
      DIMENSION NRX(*)
      DIMENSION IC1(*)
!
      PARAMETER (LYNF=25)
      DIMENSION LYNE(LYNF)
!
      INCLUDE 'DPCOP2.INC'
!
      JKSKY=0
      PP=JPP
      DNULL=0.0
!C
!C   CLUSTERING VECTOR IS INCORPORATED INTO X, AND PRINTED.
!C
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
!
      X(JNA)=JKSKY
  220 CONTINUE
      IF(JN.LT.NN)GO TO 100
!
      DO 230 JK=1,KK
        NRJK=NRX(JK)
        NRJKA=(NRJK-1)*JPP+1
        X(NRJKA)=JK
  230 CONTINUE
!NIST WRITE(LUB,9110)
!9110 FORMAT(//2X,18H CLUSTERING VECTOR/3X,17(1H*)/)
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
!
      ICNT=0
      MTEL=0
      MTELP=LYNF
  240 CONTINUE
      DO 250 J=1,MTELP
        MTEL=MTEL+1
        MTELA=(MTEL-1)*JPP+1
        LYNE(J)=INT(X(MTELA))
        ICNT=ICNT+1
        IC1(ICNT)=LYNE(J)
  250 CONTINUE
!NIST WRITE(LUB,9120)(LYNE(J),J=1,MTELP)
!
      IF(IPRINT.EQ.'ON' .AND. LARGE.GE.1)THEN
        WRITE(ICOUT,9120)(LYNE(J),J=1,MTELP)
 9120   FORMAT(4X,25I3)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      DO 9125 J=1,MTELP
        WRITE(IOUNI1,'(I7)')LYNE(J)
 9125 CONTINUE
!
      IF(MTEL.GE.NN)GO TO 300
      NNTEL=NN-MTEL
      IF(NNTEL.GE.LYNF)GO TO 240
      MTELP=NN-MTEL
      GO TO 240
!C
!C   WHEN LARGE IS NOT ZERO, LIST OF ALL CLUSTER ELEMENTS IN ENTIRE
!C   DATA SET IS GIVEN.
!C
  300 CONTINUE
      IF(LARGE.LE.0)GO TO 330
!NIST WRITE(LUB,9130)
!9130 FORMAT(//4X,27HCLUSTER SIZE MEDOID OBJECTS)
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9130)
 9130   FORMAT('    CLUSTER SIZE MEDOID OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      DO 320 KA=1,KK
        MTT=0
        J=0
  325   CONTINUE
        J=J+1
        JA=(J-1)*JPP+1
        NXJA=INT(X(JA)+0.1)
        IF(NXJA.EQ.KA)MTT=MTT+1
        IF(J.LT.NN)GO TO 325
!NIST   WRITE(LUB,9140)KA,MTT,NRX(KA)
!9140   FORMAT(/3X,I8,I5,I7)
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9140)KA,MTT,NRX(KA)
 9140     FORMAT(3X,I8,I5,I7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
!
!NIST   WRITE(LUB,9150)(LYNE(JJ),JJ=1,10)
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9150)(LYNE(JJ),JJ=1,10)
 9150     FORMAT(24X,10I5)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
  310   CONTINUE
        IF(J.LT.NN)GO TO 315
!NIST   IF(MTT.NE.0)WRITE(LUB,9150)(LYNE(JJ),JJ=1,MTT)
        IF(MTT.NE.0 .AND. IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9150)(LYNE(JJ),JJ=1,MTT)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  320 CONTINUE
  330 CONTINUE
      RETURN
      END SUBROUTINE RESUL
      SUBROUTINE CSTAT(KK,NN,NSEND,NREPR,RADUS,DAMER,TTD,SEPAR,S,   &
                       DYS,NCLUV,NELEM,JPP,MAXNN,MAXPP,X,JDYSS,NSTAN,   &
                       IOUNI2,ISUBRO,IBUGA3)
!NIST SUBROUTINE CSTAT(KK,NN,NSEND,NREPR,RADUS,DAMER,TTD,SEPAR,Z,S,
!NIST1                 MAXHH,DYS,NCLUV,NELEM,JPP,MAXNN,MAXPP,X,LAB,
!NIST1                 LUB,JDYSS,NSTAN)
!
!     THE FOLLOWING CHANGES WERE MADE TO INCORPORATE INTO DATAPLOT
!
!        1. USE DATAPLOT OUTPUT
!        2. RECODE A BIT FOR BETTER READABILITY
!
!     KK        = NUMBER OF CLUSTERS
!     NN        = NUMBER OF ROWS (CASES)
!     NSEND     =
!     NREPR     =
!     RADUS     = MAXIMUM DISSIMILARITY TO EACH MEDOID
!     DAMER     =
!     TTD       = AVERAGE DISSIMILARITY TO EACH MEDOID
!     SEPAR     =
!     Z         =
!     S         =
!     MAXHH     = MAXIMUM DIMENSION FOR DISTANCES
!                 (MAXNN*(MAXNN-1)/2 + 1)
!                 EQUALS 4951 IN ROUSSEEUW
!     DYS       = VECTOR CONTAINING THE DISSIMILARITIES
!     NCLUV     =
!     NELEM     =
!     JPP       = NUMBER OF COLUMNS (VARIABLES)
!     MAXNN     = MAXIMUM NUMBER OF ROWS
!                 SET TO 100 BY ROUSSEEUW
!     MAXPP     = MAXIMUM NUMBER OF VARIABLES
!                 SET TO 20 BY ROUSSEEUW
!     X         = THE DATA MATRIX
!     LAB       = VARIABLE LABELS
!                 USE "001", "002", ETC.
!     JDYSS     = 1 => DISSIMILARITY MATRIX
!                 1 =  MEASUREMENT DATA
!     NSTAN     = 0 => NO STANDARDIZATION OF VARIABLES
!                 1 => VARIABLES ARE STANDARDIZED
!     LUB       = OUTPUT UNIT (WE DON'T USE)
!
      DIMENSION SEPAR(*)
      DIMENSION DAMER(*)
      DIMENSION TTD(*)
      DIMENSION DYS(*)
      DIMENSION X(MAXNN,MAXPP)
!
      DIMENSION NCLUV(*)
      DIMENSION NSEND(*)
      DIMENSION NREPR(*)
      DIMENSION NELEM(*)
      DIMENSION RADUS(*)
!
!CCCC CHARACTER*1 LAB(3,MAXNN)
      CHARACTER*1 JDRAW(30)
      CHARACTER*3 LAB1
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
!
      INCLUDE 'DPCOP2.INC'
!
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
!
      KSMAL=0
!
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
!
      JK=1
      NPLAC=NSEND(1)
      DO 135 J=1,NN
        NCLUV(J)=0
        IF(NSEND(J).EQ.NPLAC)NCLUV(J)=1
  135 CONTINUE
!
      DO 145 JA=2,NN
        NPLAC=NSEND(JA)
        IF(NCLUV(NPLAC).NE.0)GO TO 145
        JK=JK+1
        DO 140 J=2,NN
          IF(NSEND(J).EQ.NPLAC)NCLUV(J)=JK
  140   CONTINUE
        IF(JK.EQ.KK)GO TO 148
  145 CONTINUE
!
!     ANALYSIS OF THE CLUSTERING.
!
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
!
      DO 160 NUMCL=1,KK
        NTT=0
        RADUS(NUMCL)=-1.0
        TTT=0.0
!
        DO 150 J=1,NN
          IF(NCLUV(J).NE.NUMCL)GO TO 150
          NTT=NTT+1
          M=NSEND(J)
          NELEM(NTT)=J
          NJM=MEET(J,M)
          TTT=TTT+DYS(NJM)
          IF(DYS(NJM).GT.RADUS(NUMCL))RADUS(NUMCL)=DYS(NJM)
  150   CONTINUE
!
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
!
        NSSDR=NSS*3
!NIST   WRITE(LUB,9210)NUMCL,LAB(1,M),LAB(2,M),LAB(3,M),NTT,
!NIST1                 (JDRAW(K),K=1,NSSDR)
!9210   FORMAT(/1X,I5,6X,3A1,2X,I6,5X,10(3A1,1X))
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          LAB1='000'
          WRITE(LAB1,'(I3)')M
          WRITE(ICOUT,9210)NUMCL,LAB1,NTT,   &
                           (JDRAW(K),K=1,NSSDR)
 9210     FORMAT(I5,6X,A3,2X,I6,5X,10(3A1,1X))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
!
        IF(KAUNT.GE.1 .AND. IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9215)(JDRAW(K),K=1,LDRE)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  160 CONTINUE
!
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
 9230       FORMAT('COORDINATES OF MEDOIDS (USING STANDARDIZED ',   &
                   'MEASUREMENTS')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9231)
 9231       FORMAT(   &
            '*******************************************************')
            CALL DPWRST('XXX','BUG ')
          ENDIF
        ENDIF
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
          DO 9243 II=1,ILOOP
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
!
      RNN=NN
      IF(KK.EQ.1)THEN
        DAMER(1)=S
        GO TO 300
      ENDIF
!
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
!
        WRITE(ICOUT,9280)(NCLUV(J),J=1,NN)
 9280   FORMAT(11X,50(20I3/11X))
        CALL DPWRST('XXX','BUG ')
!
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
!C
!C    NUML = NUMBER OF L-CLUSTERS.
!C
      NUML=0
      DO 40 K=1,KK
!C
!C      IDENTIFICATION OF CLUSTER K:
!C         NEL=NUMBER OF OBJECTS
!C         NELEM=VECTOR OF OBJECTS
!C
        NEL=0
!
        DO 23 J=1,NN
          IF(NCLUV(J).NE.K)GO TO 23
          NEL=NEL+1
          NELEM(NEL)=J
   23   CONTINUE
!
        IF(NEL.EQ.1)THEN
          NVN=NELEM(1)
          DAMER(K)=0.
          SEPAR(K)=1.1*S+1.0
          DO 250 J=1,NN
            IF(J.EQ.NVN)GO TO 250
            MEVJ=MEET(NVN,J)
            IF(SEPAR(K).GT.DYS(MEVJ))SEPAR(K)=DYS(MEVJ)
  250     CONTINUE
!C
!C        IS CLUSTER K     1) AN L-CLUSTER ?
!C                         2) AN L*-CLUSTER ?
!C
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
 9324         FORMAT(' CLUSTER ',I4,' IS A SINGLETON CONSISTING OF',   &
                     ' OBJECT ',A3,'. IT IS NOT ISOLATED.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,9326)
 9326         FORMAT(' ** IT IS NOT ADVISABLE TO DIVIDE THE DATA INTO',   &
                     ' SO MANY CLUSTERS.')
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,999)
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ENDIF
!
        ELSE
          DAM=-1.
          SEP=1.1*S+1.0
          KAND=1
!
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
!
          SEPAR(K)=SEP
          DAMER(K)=DAM
          IF(KAND.EQ.0)GO TO 40
!C
!C        DIAMETER AND SEPARATION OF ISOLATED CLUSTERS
!C
          IF(IPRINT.EQ.'ON')THEN
            WRITE(ICOUT,9310)K
 9310       FORMAT('CLUSTER ',I4,' IS ISOLATED')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9330)DAM,SEP
 9330       FORMAT(8X,' WITH DIAMETER  =',F11.2,' AND SEPARATION =',   &
                   F11.2)
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
!
        ENDIF
!
   40 CONTINUE
!
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
!
  300 CONTINUE
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9370)
 9370   FORMAT('  DIAMETER OF EACH CLUSTER')
        CALL DPWRST('XXX','BUG ')
        ILOOP=KK/8
        IREM=MOD(KK,8)
        IF(IREM.GT.0)ILOOP=ILOOP+1
        DO 9375 II=1,ILOOP
          ISTRT=(II-1)*8 + 1
          ISTOP=II*8
          IF(ISTOP.GT.KK)ISTOP=KK
          WRITE(ICOUT,9371)(DAMER(J),J=ISTRT,ISTOP)
 9371     FORMAT(2X,8F9.2)
          CALL DPWRST('XXX','BUG ')
 9375   CONTINUE
      ENDIF
!
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
          DO 9385 II=1,ILOOP
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
        DO 9395 II=1,ILOOP
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
        DO 9405 II=1,ILOOP
          ISTRT=(II-1)*8 + 1
          ISTOP=II*8
          IF(ISTOP.GT.KK)ISTOP=KK
          WRITE(ICOUT,9401)(RADUS(J),J=1,KK)
 9401     FORMAT(2X,8F9.2)
          CALL DPWRST('XXX','BUG ')
 9405   CONTINUE
      ENDIF
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END OF CSTAT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CSTAT
      SUBROUTINE SUPCL(DYS,KKA,KKB,AREST,NER)
!NIST SUBROUTINE SUPCL(MAXHH,DYS,KKA,KKB,AREST,MAXNN,NER)
!
!     DYS    = VECTOR OF DISTANCES
!     KKA    = ...
!     KKB    = ...
!     AREST  = ...
!     NER    = ...
!
      DIMENSION DYS(*)
      DIMENSION NER(*)
!
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
!
      RETURN
      END SUBROUTINE SUPCL
      SUBROUTINE AVERL(NN,KWAN,NER,BAN,DYS,   &
                       NCLUT,LAT,LBT,BANLAT,BANLBT,   &
                       IOUNI2,IOUNI3,IOUNI4,IAGNME,ISUBRO,IBUGA3)
!NIST SUBROUTINE AVERL(NN,MAXNN,KWAN,NER,BAN,MAXHH,DYS,LUB)
!
!     NN    = NUMBER OF OBJECTS
!     MAXNN = MAXIMUM NUMBER OF OBJECTS (DATAPLOT DOES NOT USE)
!     KWAN  = NUMBER OF OBJECTS IN EACH CLUSTER
!     NER   = FINAL ORDERING OF OBJECTS
!     BAN   = DISSIMILARITIES BETWEEN CLUSTERS
!     MAXHH = MAXIMUM NUMBER OF DISSIMILARITIES (DATAPLOT DOES NOT USE)
!     DYS   = VECTOR OF DISSIMILARITIES
!     LUB   = OUTOPUT UNIT (DATAPLOT DOES NOT USE)
!
      DIMENSION DYS(*)
      DIMENSION BAN(*)
      DIMENSION BANLAT(*)
      DIMENSION BANLBT(*)
!
      DIMENSION NER(*)
      DIMENSION KWAN(*)
      DIMENSION NCLUT(*)
      DIMENSION LAT(*)
      DIMENSION LBT(*)
!
      CHARACTER*4 IAGNME
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
!
      INCLUDE 'DPCOP2.INC'
!
!     INITIALIZATION
!
!     NCLU   = NUMBER OF CLUSTERS
!     KWAN   = NUMBER OF OBJECTS IN EACH CLUSTER
!     NER    = OBJECT ID'S FOR THE CLUSTER
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,5)NN
    5   FORMAT('BEGINING OF AVERL: NN = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      FC=0.0
      ICNT=0
      NCLU=NN-1
      DO 10 L=1,NN
        KWAN(L)=1
        NER(L)=L
   10 CONTINUE
!C
!C    FIND CLOSEST CLUSTERS
!C
  100 CONTINUE
      J=1
!
   80 CONTINUE
      J=J+1
      IF(KWAN(J).EQ.0)GO TO  80
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
!C
!C    DETERMINE LFYRS AND LLAST
!C
      DO 200 L=1,NN
        IF(NER(L).EQ.LA)LFYRS=L
        IF(NER(L).EQ.LB)LLAST=L
  200 CONTINUE
      BAN(LLAST)=SMALD
!C
!C    IF THE TWO CLUSTERS ARE NEXT TO EACH OTHER,
!C    NER MUST NOT BE CHANGED
!C
      LNEXT=LFYRS+KWAN(LA)
      IF(LNEXT.NE.LLAST)THEN
!C
!C      UPDATING NER AND BAN
!C
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
!C
!C    CALCULATE NEW DISSIMILARITIES
!C
!C    SUPPORT FOR DIFFERENT METHODS
!C
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
!
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
        DO 9111 II=1,ILOOP
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
        DO 9131 II=1,ILOOP
          ISTRT=(II-1)*5+2
          ISTOP=II*5 + 1
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9130)(BAN(L),L=ISTRT,ISTOP)
 9130     FORMAT(3X,5F15.3)
          CALL DPWRST('XXX','BUG ')
 9131   CONTINUE
      ENDIF
!
      KWAN(LA)=KWAN(LA)+KWAN(LB)
      KWAN(LB)=0
!
!     PRINT RESULTS FROM CURRENT CLUSTER
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
        WRITE(ICOUT,9910)NCLU,LA,LB,LFYRS,LLAST
 9910   FORMAT('AVERL: NCLU,LA,LB,LFYRS,LLAST = ',5I5)
        CALL DPWRST('XXX','BUG ')
        DO 9920 L=1,NN
          WRITE(ICOUT,9921)L,NER(L),KWAN(L),BAN(L)
 9921     FORMAT('L,NER(L),KWAN(L),BAN(L) = ',3I5,F12.5)
          CALL DPWRST('XXX','BUG ')
 9920   CONTINUE
      ENDIF
!
      DISTMX=-1.0
      DO 1020 II=1,NN
        IF(BAN(II).GT.DISTMX)DISTMX=BAN(II)
 1020 CONTINUE
!
      DO 1025 II=1,NN
        IF(LA.EQ.NER(II))THEN
          AVAL1=BAN(II)
        ELSEIF(LB.EQ.NER(II))THEN
          AVAL2=BAN(II)
        ENDIF
 1025 CONTINUE
      ICNT=ICNT+1
      WRITE(IOUNI4,'(3I5,2E15.7)')NCLU,LA,LB,AVAL1,AVAL2
      NCLU=NCLU-1
      IF(NCLU.GT.0)GO TO  100
!
!     NOW CREATE DATA FOR:
!
!         1. DENDOGRAM (IOUNI3)
!         2. ICICLE PLOT (IOUNI2)
!
      REWIND(IOUNI4)
!
      DO 2010 KK=1,ICNT
        READ(IOUNI4,'(3I5,2E15.7)',END=2019,ERR=2019)   &
            NCLUT(KK),LAT(KK),LBT(KK),BANLAT(KK),BANLBT(KK)
 2010 CONTINUE
!
      ITAG=0
      DO 2020 KK=1,ICNT
!
!       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
!       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
!       THE APPROPRIATE X-COORDINATE.
!
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        AVAL1=BANLBT(KK)
!
        IF(KK.EQ.1)THEN
!
!         FIRST CLUSTER BEING FORMED
!
          XVAL1=1.0
          XVAL2=2.0
          DO 2021 JJ=1,NN
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
!
        ELSE
!
!         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
!         PREVIOUS LA.
!
          IFLAGL=0
          IFLAGR=0
          DO 2030 JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
!
!             MATCH WITH PREVIOUS CLUSTER FOUND
!
              ISEC2=LBT(JJ)
              DO 2031 LL=1,NN
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
              GO TO 2039
            ENDIF
 2030     CONTINUE
 2039     CONTINUE
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,2035)KK,IFLAGL
 2035       FORMAT('AFTER 2030 LOOP: KK,IFLAGL = ',2I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
 2038       FORMAT('XVAL1,XVAL2,YVAL1,YVAL2,YVAL3 = ',5G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
!         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
!         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
!
          IF(IFLAGL.EQ.0)THEN
            DO 2040 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 2041 LL=1,NN
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
                GO TO 2049
              ENDIF
 2040       CONTINUE
 2049       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2045)KK,IFLAGR
 2045         FORMAT('AFTER 2040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ELSEIF(IFLAGL.EQ.1)THEN
            DO 2050 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 2053 LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL) - 0.5
                    XVAL4=REAL(LL)
                  ENDIF
 2053           CONTINUE
                YVAL2=AVAL1
                YVAL3=BANLBT(JJ)
                IFLAGR=1
                GO TO 2059
              ENDIF
 2050       CONTINUE
 2059       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2055)KK,IFLAGR
 2055         FORMAT('AFTER 2050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ENDIF
!
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL3,REAL(ITAG)
!
          ELSE
!
!           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
!           NEW CLUSTER
!
            DO 2061 JJ=1,NN
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
!
          ENDIF
        ENDIF
!
 2020 CONTINUE
!
      ITAG=0
      DO 3020 KK=1,ICNT
!
!       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
!       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
!       THE APPROPRIATE X-COORDINATE.
!
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        NCLU=NCLUT(KK)
        YVAL=REAL(NCLU)
!
        IF(KK.EQ.1)THEN
!
!         FIRST CLUSTER BEING FORMED
!
          XVAL1=1.0
          XVAL2=2.0
          DO 3021 JJ=1,NN
            IF(IFRST.EQ.NER(JJ))XVAL1=REAL(JJ)
            IF(ISEC.EQ.NER(JJ))XVAL2=REAL(JJ)
 3021     CONTINUE
!
          XVAL1=(XVAL1-1.0)*2.0 + 1.0
          XVAL2=XVAL1 + 1.0
          XVAL3=(XVAL2-1.0)*2.0 + 1.0
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL3,YVAL,REAL(ITAG)
!
        ELSEIF(KK.EQ.ICNT)THEN
!
!         LAST CLUSTER BEING FORMED
!
          XVAL2=2.0
          DO 307 JJ=1,NN
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
!
!         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
!         PREVIOUS LA.
!
          IFLAGL=0
          IFLAGR=0
          IFLAG3=0
          DO 3030 JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
!
!             MATCH WITH PREVIOUS CLUSTER FOUND
!
              ISEC2=LBT(JJ)
              DO 3031 LL=1,NN
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
              GO TO 3039
            ENDIF
 3030     CONTINUE
 3039     CONTINUE
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,3035)KK,IFLAGL,XVAL1,XVAL2,YVAL
 3035       FORMAT('AFTER 3030 LOOP: KK,IFLAGL,XVAL1,XVAL2,YVAL = ',   &
                   2I6,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)IFRST,ISEC,ISEC2
 3036       FORMAT('                 IFRST,ISEC,ISEC2 = ',3I6)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
!         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
!         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
!
          IF(IFLAGL.EQ.0)THEN
            DO 3040 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 3041 LL=1,NN
                  IF(IFRST.EQ.NER(LL))THEN
                    XVAL1=REAL(LL)
                    XVAL1=(XVAL1-1.0)*2.0 + 1.0
                  ENDIF
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL1-2.0)*2.0 + 1.0
                  ENDIF
 3041           CONTINUE
                GO TO 3049
              ENDIF
 3040       CONTINUE
 3049       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3045)KK,IFLAGR
 3045         FORMAT('AFTER 3040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
 3048         FORMAT('XVAL1,XVAL2,YVAL = ',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ELSEIF(IFLAGL.EQ.1 .AND. IFLAG3.EQ.0)THEN
            DO 3050 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 3053 LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL2-2.0)*2.0 + 1.0
                  ENDIF
 3053           CONTINUE
                IFLAGR=1
                GO TO 3059
              ENDIF
 3050       CONTINUE
 3059       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3055)KK,IFLAGR
 3055         FORMAT('AFTER 3050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ENDIF
!
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ELSE
!
!           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
!           NEW CLUSTER
!
            DO 3061 JJ=1,NN
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
!
 3020 CONTINUE
!
 2019 CONTINUE
      RETURN
      END SUBROUTINE AVERL
      SUBROUTINE SPLYT(NN,KWAN,NER,BAN,DYS,   &
                       NCLUT,LAT,LBT,BANLAT,BANLBT,   &
                       IOUNI2,IOUNI3,IOUNI4,ISUBRO,IBUGA3)
!
!NIST SUBROUTINE SPLYT(NN,MAXNN,KWAN,NER,BAN,MAXHH,DYS,LUB)
!
!     NN     = NUMBER OF OBJECTS
!     MAXNN  = MAXIMUM NUMBER OF OBJECTS
!     KWAN   = NUMBER OF OBJECTS IN EACH CLUSTER
!     NER    = ORDERING OF OBJECTS
!     BAN    = DISSIMILARITIES BETWEEN CLUSTERS
!     MAXHH  = MAXIMUM NUMBER FO DISSIMILARITIES (DATAPLOT DOES NOT USE)
!     DYS    = VECTOR OF DISSIMILARITIES
!     LUB    = OUTPUT UNIT (DATAPLOT DOES NOT USE)
!
      DIMENSION KWAN(*)
      DIMENSION DYS(*)
      DIMENSION NER(*)
      DIMENSION BAN(*)
      DIMENSION BANLAT(*)
      DIMENSION BANLBT(*)
!
      DIMENSION NCLUT(*)
      DIMENSION LAT(*)
      DIMENSION LBT(*)
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,5)NN
    5   FORMAT('BEGINING OF AVERL: NN = ',I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      LXF=0
      LXG=0
      LNDSD=0
      JMB=0
      L=0
      JMA=0
      JAWAY=0
      ICNT=0
!
!C    INITIALIZATION
!C
      NCLU=1
      NHALF=NN*(NN-1)/2+1
      DO 10 L=1,NN
        KWAN(L)=0
        BAN(L)=0.
        NER(L)=L
   10 CONTINUE
      KWAN(1)=NN
      JA=1
!C
!C    COMPUTATION OF DIAMETER OF DATA SET
!C
      CS=0.0
      K=0
   20 CONTINUE
      K=K+1
      IF(DYS(K).GT.CS)CS=DYS(K)
      IF(K.LT.NHALF)GO TO 20
!C
!C    PREPARE FOR SPLITTING
!C
   30 CONTINUE
      JB=JA+KWAN(JA)-1
      JMA=JB
!C
!C    SPECIAL CASE OF A PAIR OF OBJECTS
!C
      IF(KWAN(JA).EQ.2)THEN
        KWAN(JA)=1
        KWAN(JB)=1
        JAN=NER(JA)
        JBN=NER(JB)
        JAB=MEET(JAN,JBN)
        BAN(JB)=DYS(JAB)
        GO TO 400
      ENDIF
!C
!C    FINDING FIRST OBJECT TO BE SHIFTED
!C
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
!C
!C    SHIFTING THE FIRST OBJECT
!C
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
!C
!C    FINDING THE NEXT OBJECT TO BE SHIFTED
!C
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
!C
!C    SHIFTING THE NEXT OBJECT WHEN NECESSARY
!C
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
!
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
!C
!C    SWITCH THE TWO PARTS WHEN NECESSARY
!C
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
!C
!C    COMPUTE LEVEL FOR BANNER
!C
      IF(NCLU.EQ.1)BAN(JMB)=CS
      IF(NCLU.EQ.1)GO TO 400
      CALL SUPCL(DYS,JA,JB,AREST,NER)
      BAN(JMB)=AREST
!
  400 CONTINUE
!
      DO 1025 II=1,NN
        IF(JA.EQ.NER(II))THEN
          AVAL1=BAN(II)
        ELSEIF(JB.EQ.NER(II))THEN
          AVAL2=BAN(II)
        ENDIF
 1025 CONTINUE
      ICNT=ICNT+1
      WRITE(IOUNI4,'(3I5,2E15.7)')NCLU,JA,JB,AVAL1,AVAL2
!
      NCLU=NCLU+1
      IF(NCLU.EQ.2 .AND. IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
!NIST   WRITE(ICOUT,9000)NN,JMA,KWAN(JMB)
!9000   FORMAT(//22H AT THE FIRST STEP THE,I4,20H OBJECTS ARE DIVIDED,
!NIST1         5H INTO/3X,I4,12H OBJECTS AND,I4,8H OBJECTS)
        WRITE(ICOUT,9000)NN
 9000   FORMAT('AT THE FIRST STEP THE',I4,' OBJECTS ARE DIVIDED INTO')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9001)JMA,KWAN(JMB)
 9001   FORMAT(2X,I4,' OBJECTS AND',I4,' OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IF(NCLU.EQ.NN)GO TO  500
!C
!C    CONTINUE SPLITTING UNTIL ALL OBJECTS ARE SEPARATED
!C
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
!
  500 CONTINUE
!
      ILOOP=NN/5
      IREM=MOD(NN,5)
      IF(IREM.GT.0)ILOOP=ILOOP+1
!
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
        DO 9111 II=1,ILOOP
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
        DO 9131 II=1,ILOOP
          ISTRT=(II-1)*5 + 2
          ISTOP=II*5
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9130)(BAN(L),L=ISTRT,ISTOP)
 9130     FORMAT(3X,5F15.3)
          CALL DPWRST('XXX','BUG ')
 9131   CONTINUE
      ENDIF
!
!     NOW CREATE DATA FOR:
!
!         1. DENDOGRAM (IOUNI3)
!         2. ICICLE PLOT (IOUNI2)
!
      REWIND(IOUNI4)
!
      DO 2010 KK=1,ICNT
        READ(IOUNI4,'(3I5,2E15.7)',END=2019,ERR=2019)   &
            NCLUT(KK),LAT(KK),LBT(KK),BANLAT(KK),BANLBT(KK)
 2010 CONTINUE
!
      ITAG=0
      DO 2020 KK=1,ICNT
!
!       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
!       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
!       THE APPROPRIATE X-COORDINATE.
!
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        AVAL1=BANLBT(KK)
!
        IF(KK.EQ.1)THEN
!
!         FIRST CLUSTER BEING FORMED
!
          XVAL1=1.0
          XVAL2=2.0
          DO 2021 JJ=1,NN
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
!
        ELSE
!
!         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
!         PREVIOUS LA.
!
          IFLAGL=0
          IFLAGR=0
          DO 2030 JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
!
!             MATCH WITH PREVIOUS CLUSTER FOUND
!
              ISEC2=LBT(JJ)
              DO 2031 LL=1,NN
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
              GO TO 2039
            ENDIF
 2030     CONTINUE
 2039     CONTINUE
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,2035)KK,IFLAGL
 2035       FORMAT('AFTER 2030 LOOP: KK,IFLAGL = ',2I6)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
 2038       FORMAT('XVAL1,XVAL2,YVAL1,YVAL2,YVAL3 = ',5G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
!         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
!         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
!
          IF(IFLAGL.EQ.0)THEN
            DO 2040 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 2041 LL=1,NN
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
                GO TO 2049
              ENDIF
 2040       CONTINUE
 2049       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2045)KK,IFLAGR
 2045         FORMAT('AFTER 2040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ELSEIF(IFLAGL.EQ.1)THEN
            DO 2050 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 2053 LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL) - 0.5
                    XVAL4=REAL(LL)
                  ENDIF
 2053           CONTINUE
                YVAL2=AVAL1
                YVAL3=BANLBT(JJ)
                IFLAGR=1
                GO TO 2059
              ENDIF
 2050       CONTINUE
 2059       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,2055)KK,IFLAGR
 2055         FORMAT('AFTER 2050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2038)XVAL1,XVAL2,YVAL1,YVAL2,YVAL3
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ENDIF
!
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL1,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL1,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL2,REAL(ITAG)
            WRITE(IOUNI3,'(3E15.7)')XVAL2,YVAL3,REAL(ITAG)
!
          ELSE
!
!           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
!           NEW CLUSTER
!
            DO 2061 JJ=1,NN
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
!
          ENDIF
        ENDIF
!
 2020 CONTINUE
!
      ITAG=0
      DO 3020 KK=1,ICNT
!
!       LB IDENTIFIES "RIGHT HAND SIDE" OF BRANCH.  LA IDENTIFIES
!       WHICH CLUSTER IT IS JOINING.  NER WILL BE USED TO IDENTIFY
!       THE APPROPRIATE X-COORDINATE.
!
        IFRST=LAT(KK)
        ISEC=LBT(KK)
        NCLU=NCLUT(KK)
        YVAL=REAL(NCLU)
!
        IF(KK.EQ.1)THEN
!
!         FIRST CLUSTER BEING FORMED
!
          XVAL1=1.0
          XVAL2=2.0
          DO 3021 JJ=1,NN
            IF(IFRST.EQ.NER(JJ))XVAL1=REAL(JJ)
            IF(ISEC.EQ.NER(JJ))XVAL2=REAL(JJ)
 3021     CONTINUE
!
          XVAL1=(XVAL1-1.0)*2.0 + 1.0
          XVAL2=XVAL1 + 1.0
          XVAL3=(XVAL2-1.0)*2.0 + 1.0
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ITAG=ITAG+1
          WRITE(IOUNI2,'(3E15.7)')XVAL3,YVAL,REAL(ITAG)
!
        ELSEIF(KK.EQ.ICNT)THEN
!
!         LAST CLUSTER BEING FORMED
!
          XVAL2=2.0
          DO 307 JJ=1,NN
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
!
!         IF NOT THE FIRST, THEN CHECK IF LA MATCHES ANY
!         PREVIOUS LA.
!
          IFLAGL=0
          IFLAGR=0
          IFLAG3=0
          DO 3030 JJ=KK-1,1,-1
            IF(IFRST.EQ.LAT(JJ))THEN
!
!             MATCH WITH PREVIOUS CLUSTER FOUND
!
              ISEC2=LBT(JJ)
              DO 3031 LL=1,NN
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
              GO TO 3039
            ENDIF
 3030     CONTINUE
 3039     CONTINUE
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
            WRITE(ICOUT,3035)KK,IFLAGL,XVAL1,XVAL2,YVAL
 3035       FORMAT('AFTER 3030 LOOP: KK,IFLAGL,XVAL1,XVAL2,YVAL = ',   &
                   2I6,3G15.7)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3036)IFRST,ISEC,ISEC2
 3036       FORMAT('                 IFRST,ISEC,ISEC2 = ',3I6)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
!         IF NOT THE FIRST AND IF LA DOES NOT MATCH ANY PREVIOUS LA,
!         THEN CHECK IF LB MATCHES ANY PREVIOUS LA.
!
          IF(IFLAGL.EQ.0)THEN
            DO 3040 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 3041 LL=1,NN
                  IF(IFRST.EQ.NER(LL))THEN
                    XVAL1=REAL(LL)
                    XVAL1=(XVAL1-1.0)*2.0 + 1.0
                  ENDIF
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL1-2.0)*2.0 + 1.0
                  ENDIF
 3041           CONTINUE
                GO TO 3049
              ENDIF
 3040       CONTINUE
 3049       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3045)KK,IFLAGR
 3045         FORMAT('AFTER 3040 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
 3048         FORMAT('XVAL1,XVAL2,YVAL = ',3G15.7)
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ELSEIF(IFLAGL.EQ.1 .AND. IFLAG3.EQ.0)THEN
            DO 3050 JJ=KK-1,1,-1
              IF(ISEC.EQ.LAT(JJ))THEN
!
!               MATCH WITH PREVIOUS CLUSTER FOUND
!
                ISEC2=LBT(JJ)
                DO 3053 LL=1,NN
                  IF(ISEC2.EQ.NER(LL))THEN
                    XVAL2=REAL(LL)
                    XVAL2=(XVAL2-2.0)*2.0 + 1.0
                  ENDIF
 3053           CONTINUE
                IFLAGR=1
                GO TO 3059
              ENDIF
 3050       CONTINUE
 3059       CONTINUE
!
            IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'VERL')THEN
              WRITE(ICOUT,3055)KK,IFLAGR
 3055         FORMAT('AFTER 3050 LOOP: KK,IFLAGR = ',2I6)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,3048)XVAL1,XVAL2,YVAL
              CALL DPWRST('XXX','BUG ')
            ENDIF
!
          ENDIF
!
          IF(IFLAGL.EQ.1 .OR. IFLAGR.EQ.1)THEN
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL1,YVAL,REAL(ITAG)
            ITAG=ITAG+1
            WRITE(IOUNI2,'(3E15.7)')XVAL2,YVAL,REAL(ITAG)
          ELSE
!
!           NO MATCH WITH PREVIOUS CLUSTER, SO CREATING A
!           NEW CLUSTER
!
            DO 3061 JJ=1,NN
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
!
 3020 CONTINUE
!
!
 2019 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,9905)
 9905   FORMAT('END OF SPLYT')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE SPLYT
      SUBROUTINE BANAG(NN,BAN,NER,IOUNI5,IAGNBA,ISUBRO,IERROR)
!
!NIST SUBROUTINE BANAG(NN,MAXNN,BAN,NER,LAB,NUM,LUB)
!
!     ORIGINAL ROUTINE USED TO DRAW "BANNER" LINE PRINTER GRAPH FOR
!     AGGLOMERATIVE CLUSTERING ALGORITHM (AGNES).
!
!     NN       = NUMBER OF ROWS IN THE DISSIMILARITY MATRIX
!     MAXNN    = MAXIMUM NUMBER OF ROWS ALLOWED (NOT USED)
!     BAN      = DISSIMILARITIES BETWEEN CLUSTERS
!     NER      = FINAL ORDERING OF THE OBJECTS
!     LAB      = OBJECT LABELS
!     NUM      = LABELING STRING FOR BANNER PLOT
!     LUB      = OUTPUT UNIT FOR PRINTING (NOT USED)
!
      DIMENSION BAN(*)
      DIMENSION NER(*)
!
      CHARACTER*4 IAGNBA
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!NIST CHARACTER*1 LAB(3,MAXNN)
      CHARACTER*3 LAB
      CHARACTER*1 JDRAW(78)
      CHARACTER*1 NUM(13)
      CHARACTER*1 JBLAN,JSTAR,JSEPA
!
      INCLUDE 'DPCOP2.INC'
!
      IERROR='NO'
!
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
!
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
!
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
!
        WRITE(ICOUT,9210)
 9210   FORMAT('0  0  0  1  1  2  2  2  3  3  4  4  4  5  5  ',   &
               '6  6  6  7  7  8  8  8  9  9  0')
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,9220)
 9220   FORMAT(5('0  4  8  2  6  '),'0')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     SUP = MAXIMUM VALUE IN BAN
!
      SUP=0.0
      DO 70 K=2,NN
        IF(BAN(K).GT.SUP)SUP=BAN(K)
   70 CONTINUE
!
      AC=0.0
      DO 80 K=1,NN
        KEARL=K
        IF(K.EQ.1)KEARL=2
        KAFTE=K+1
        IF(K.EQ.NN)KAFTE=NN
        SYZE=BAN(KEARL)
        IF(BAN(KAFTE).LT.SYZE)SYZE=BAN(KAFTE)
!
        IF(ISUBRO.EQ.'ANAG')THEN
          WRITE(ICOUT,8001)IOUNI5,K,KEARL,KAFTE,SYZE
 8001     FORMAT('IOUNI5,K,KEARL,KAFTE,SYZE = ',4I8,G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
!
        LAB='000'
        IF(NCASE.LE.9)THEN
          WRITE(LAB(3:3),'(I1)')NCASE
        ELSEIF(NCASE.LE.99)THEN
          WRITE(LAB(2:3),'(I2)')NCASE
        ELSE
          WRITE(LAB(1:3),'(I3)')NCASE
        ENDIF
!
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
!
        IF(K.EQ.NN)GO TO 90
        SYZE=BAN(KAFTE)
        LEMPT=INT((SYZE/SUP)*75.0+0.01)
        IF(LEMPT.EQ.0)GO TO  86
        DO 85 L=1,LEMPT
          JDRAW(L)=JBLAN
   85   CONTINUE
   86   CONTINUE
        LADD=LEMPT+1
!
        DO 87 L=LADD,78
         JDRAW(L)=JSTAR
   87   CONTINUE
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,78)
          CALL DPWRST('XXX','BUG ')
        ENDIF
   80 CONTINUE
!
   90 CONTINUE
!
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
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9300)SUP
 9300   FORMAT(' THE ACTUAL HIGHEST LEVEL IS   ',F25.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RNN=NN
      AC=AC/RNN
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9310)AC
 9310   FORMAT(' THE AGGLOMERATIVE COEFFICIENT OF THIS DATA SET IS  ',   &
               F5.2)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE BANAG
      SUBROUTINE BANDY(NN,BAN,NER,IOUNI5,IAGNBA,ISUBRO,IERROR)
!
!NIST SUBROUTINE BANDY(NN,MAXNN,BAN,NER,LAB,NUM,LUB)
!
!     ORIGINAL ROUTINE USED TO DRAW "BANNER" LINE PRINTER GRAPH FOR
!     DIVISIVE CLUSTERING ALGORITHM (DIANA).
!
!     NN       = NUMBER OF ROWS IN THE DISSIMILARITY MATRIX
!     MAXNN    = MAXIMUM NUMBER OF ROWS ALLOWED (NOT USED)
!     BAN      = DISSIMILARITIES BETWEEN CLUSTERS
!     NER      = FINAL ORDERING OF THE OBJECTS
!     LAB      = OBJECT LABELS
!     NUM      = LABELING STRING FOR BANNER PLOT
!     LUB      = OUTPUT UNIT FOR PRINTING (NOT USED)
!
      DIMENSION BAN(*)
      DIMENSION NER(*)
!
!NIST CHARACTER*1 LAB(3,MAXNN),JDRAW(78),NUM(13),JSTAR,JSEPA
      CHARACTER*4 IAGNBA
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*3 LAB
      CHARACTER*1 JDRAW(78)
      CHARACTER*1 NUM(13)
      CHARACTER*1 JBLAN,JSTAR,JSEPA
!
      INCLUDE 'DPCOP2.INC'
!
      IERROR='YES'
!
      IF(ISUBRO.EQ.'ANDY')THEN
        WRITE(ICOUT,52)IOUNI5
   52   FORMAT('IOUNI5 = ',I5)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
!
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
!
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
!
        WRITE(ICOUT,9210)
 9210   FORMAT('0  0  0  1  1  2  2  2  3  3  4  4  4  5  5  ',   &
               '6  6  6  7  7  8  8  8  9  9  0')
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,9220)
 9220   FORMAT(5('0  4  8  2  6  '),'0')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!     SUP = MAXIMUM VALUE IN BAN
!
      SUP=0.0
      DO 70 K=2,NN
        IF(BAN(K).GT.SUP)SUP=BAN(K)
   70 CONTINUE
      DO 71 K=2,NN
        BAN(K)=BAN(K)/SUP
   71 CONTINUE
!
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
!
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
!
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,LENGT)
 9100     FORMAT(1X,78A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(K.EQ.NN)GO TO 90
        SYZE=BAN(KAFTE)
        LENGT=INT((1.0-SYZE)*75.0+0.01)+3
        DO 82 L=1,LENGT
         JDRAW(L)=JSTAR
   82   CONTINUE
!
        IF(IPRINT.EQ.'ON' .AND. IAGNBA.EQ.'ON')THEN
          WRITE(ICOUT,9100)(JDRAW(J),J=1,LENGT)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
   80 CONTINUE
   90 CONTINUE
!
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
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9300)SUP
 9300   FORMAT(' THE ACTUAL DIAMETER OF THIS DATA SET IS   ',F25.10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE BANDY
      SUBROUTINE DYSTAF(NN,JPP,MAXNN,MAXPP,X,DSS,NDYST,AMISS,JHALT,   &
                        ISUBRO,IBUGA3)
!
!NIST SUBROUTINE DYSTA(NN,JPP,MAXNN,MAXPP,MAXHH,X,DSS,NDYST,
!NISTF JTMD,VALMD,LAB,JHALT,LUB,FNAMEB)
!
!     KAUFFMAN AND ROUSSEEUW CODE FROM FANNY ALGORITHM.  THIS
!     ROUTINE COMPUTES EITHER EUCLIDEAN DISTANCE OR MANHATTAN
!     DISTANCE BETWEEN ALL OBJECTS (FANNY VERSION).
!
!       NN      = NUMBER OF SAMPLES
!       JPP     = NUMBER OF VARIABLES
!       MAXNNN  = THE ROW DIMENSION OF X
!       MAXPP   = THE COLUMN DIMENSION OF X
!       MAXHH   = THE MAXIMUM DIMENSION FOR THE DISTANCES
!                 (DATAPLOT DOES NOT USE)
!       X       = THE DATA MATRIX
!       DSS     = THE OUTPUT MATRIX CONTAINING THE DISTANCES
!       NDYST   = 1 => EUCLIDEAN DISTANCES
!                 2 => MANHATTAN (= CITY BLOCK) DISTANCES
!       JTMD    = FOR MISSING VALUES, WE DON'T USE
!       VALMD   = FOR MISSING VALUES, WE DON'T USE
!       LAB     = OBJECT LABELS
!                 (DATAPLOT AUTOMATICALLY USES ROW-ID)
!       JHALT   = SET TO 1 FOR ERROR CONDITION
!       LUB     = OUTPUT UNIT
!                 (DATAPLOT DOES NOT USE)
!       FNAMEB  = OUTPUT FILE NAME
!                 (DATAPLOT DOES NOT USE)
!
!     CHANGES FOR INCORPORATING INTO DATAPLOT:
!
!        1. USE DATAPLOT I/O ROUTINES
!        2. FOR DATAPLOT, ONLY USE A SINGLE VALUE TO DENOTE
!           MISSING VALUES
!        3. RECODED SLIGHTLY TO REDUCE USE OF GO TO's (THIS
!           WAS JUST TO IMPROVE READABILITY OF THE CODE)
!
      DIMENSION X(MAXNN,MAXPP)
      DIMENSION DSS(*)
!NIST DIMENSION JTMD(MAXPP)
!NIST DIMENSION VALMD(MAXPP)
!NIST CHARACTER LAB(3,MAXNN)
!NIST CHARACTER*30 FNAMEB
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'YSTA')THEN
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DYSTAF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)NN,JPP,AMISS
   55   FORMAT('NN,JPP,AMISS  = ',2I8,G15.7)
        CALL DPWRST('XXX','BUG ')
        DO 58 I=1,NN
          WRITE(ICOUT,59)I,(X(I,J),J=1,JPP)
   59     FORMAT('I,X(I,J) = ',I8,30G15.7)
          CALL DPWRST('XXX','BUG ')
   58   CONTINUE
      ENDIF
!
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
!NIST       IF(JTMD(J).GE.0)GO TO  40
!NIST       IF(X(L,J).EQ.VALMD(J))GO TO  30
!NIST       IF(X(K,J).EQ.VALMD(J))GO TO  30
            IF(X(K,J).EQ.AMISS)GO TO 30
!NI40       CONTINUE
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
!NIST         WRITE(LUB,9400)LAB(1,L),LAB(2,L),LAB(3,L),
!NIST1                       LAB(1,K),LAB(2,K),LAB(3,K)
!9400         FORMAT('  OBJECTS ',3A1,' AND ',3A1)
!NIST1               ' HAVE NO COMMON MEASUREMENTS')
!NIST         IF(FNAMEB.NE.'CON')WRITE(*,9400)LAB(1,L),LAB(2,L),LAB(3,L),
!NIST1                                        LAB(1,K),LAB(2,K),LAB(3,K)
            WRITE(ICOUT,999)
  999       FORMAT(1X)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9401)L,K
 9401       FORMAT('***** OBJECTS ',I8,' AND ',I8,' HAVE NO ',   &
                   'COMMON MEASURE, SO')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,9403)
 9403       FORMAT('      THE DISTANCE BETWEEN THEM CANNOT BE ',   &
                   'COMPUTED.')
            CALL DPWRST('XXX','BUG ')
            DSS(NLK)=0.0
            GO TO  20
          ENDIF
          IF(NDYST.NE.1)THEN
            DSS(NLK)=CLK*(PP/RPRES)
          ELSE
            DSS(NLK)=SQRT(CLK*(PP/RPRES))
          ENDIF
   20   CONTINUE
  100 CONTINUE
!
      RETURN
      END SUBROUTINE DYSTAF
!C
!C
      SUBROUTINE CADDY(NN,MAXNN,P,K,KTRUE,   &
                       NFUZZ,NCLUV,RDRAW,NELEM,EDA,EDB,   &
                       IOUNI1,IOUNI2,IBUGA3,ISUBRO)
!
!NIST SUBROUTINE CADDY(NN,MAXNN,MAXKK,P,LAB,K,KTRUE,LUB,
!NIST1                 NFUZZ,NCLUV,RDRAW,NELEM,EDA,EDB)
!
      DIMENSION P(MAXNN,*)
      DIMENSION RDRAW(*)
      DIMENSION NCLUV(*)
      DIMENSION NELEM(*)
      DIMENSION NFUZZ(*)
      CHARACTER JDRAW(30)
!NIST CHARACTER LAB(3,MAXNN)
      CHARACTER*3 LAB
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IBUGA3
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADDY')THEN
        WRITE(ICOUT,5)
    5   FORMAT('AT THE BEGINNING OF CADDY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,6)NN,MAXNN
    6   FORMAT('NN,MAXNN = ',2I6)
        CALL DPWRST('XXX','BUG ')
        DO 7 II=1,NN
          WRITE(ICOUT,8)II,(P(II,JJ),JJ=1,MIN(K,10))
    8     FORMAT('II,(P(II,JJ),JJ=1,K) = ',I6,10G15.7)
          CALL DPWRST('XXX','BUG ')
    7   CONTINUE
      ENDIF
!
      PBEST=P(1,1)
      NBEST=1
      KKK=0
      KM=0
!
      DO 10 L=2,K
        IF(P(1,L).LE.PBEST)GO TO 10
          PBEST=P(1,L)
          NBEST=L
   10 CONTINUE
!
      NFUZZ(1)=NBEST
      NCLUV(1)=1
      KTRUE=1
!
      DO 20 M=2,NN
        PBEST=P(M,1)
        NBEST=1
        DO 30 L=2,K
         IF(P(M,L).LE.PBEST)GO TO 30
           PBEST=P(M,L)
           NBEST=L
   30   CONTINUE
!
        JSTAY=0
        DO 40 KTRY=1,KTRUE
          IF(NFUZZ(KTRY).NE.NBEST)GO TO 40
            NCLUV(M)=KTRY
            JSTAY=1
   40   CONTINUE
!
        IF(JSTAY.EQ.1)GO TO 20
        KTRUE=KTRUE+1
        NFUZZ(KTRUE)=NBEST
        NCLUV(M)=KTRUE
   20 CONTINUE
!
      IF(KTRUE.GE.K)GO TO 100
!
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
!
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
        DO 9205 II=1,ILOOP
          ISTRT=(II-1)*10 + 1
          ISTOP=II*10
          IF(ISTOP.GT.K)ISTOP=K
          WRITE(ICOUT,9202)(L,L=ISTRT,ISTOP)
 9202     FORMAT(3X,10I7)
          CALL DPWRST('XXX','BUG ')
 9205   CONTINUE
      ENDIF
!
      DO 110 M=1,NN
        DO 120 L=1,K
          LFUZZ=NFUZZ(L)
          RDRAW(L)=P(M,LFUZZ)
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADDY')THEN
            WRITE(ICOUT,121)M,L,LFUZZ,RDRAW(L)
  121       FORMAT('M,L,LFUZZ,RDRAW(L) = ',3I6,F7.4)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
  120   CONTINUE
!
        IF(IPRINT.EQ.'ON')THEN
          LAB='000'
          IF(M.LE.9)THEN
            WRITE(LAB(3:3),'(I1)')M
          ELSEIF(M.LE.99)THEN
            WRITE(LAB(2:3),'(I2)')M
          ELSEIF(M.LE.999)THEN
            WRITE(LAB(1:3),'(I3)')M
          ENDIF
!
          ILOOP=K/10
          IF(MOD(K,10).GT.0)ILOOP=ILOOP+1
          DO 9225 II=1,ILOOP
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
!
        WRITE(IOUNI1,9228)M,(RDRAW(L),L=1,K)
 9228   FORMAT(I5,30F10.4)
!
  110 CONTINUE
!
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
!
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
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9250)
 9250   FORMAT('CLUSTER NUMBER    SIZE    OBJECTS')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
!
        NSSDR=NSS*3
        IF(IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9210)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,9260)NUMCL,NTT,(JDRAW(LL),LL=1,NSSDR)
 9260     FORMAT(5X,I5,5X,I6,5X,10(3A1,1X))
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
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
!
        IF(KAUNT.GE.1 .AND. IPRINT.EQ.'ON')THEN
          WRITE(ICOUT,9270)(JDRAW(LL),LL=1,LDRE)
          CALL DPWRST('XXX','BUG ')
        ENDIF
  160 CONTINUE
!
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
!
        ILOOP=NN/20
        IF(MOD(NN,20).GT.0)ILOOP=ILOOP+1
        DO 9295 II=1,ILOOP
          ISTRT=(II-1)*20 + 1
          ISTOP=II*20
          IF(ISTOP.GT.NN)ISTOP=NN
          WRITE(ICOUT,9290)(NCLUV(J),J=ISTRT,ISTOP)
 9290     FORMAT(10X,20I3)
          CALL DPWRST('XXX','BUG ')
 9295  CONTINUE
!
      ENDIF
!
      DO 9296 II=1,NN
        WRITE(IOUNI2,'(I5)')NCLUV(II)
 9296 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'ADDY')THEN
        WRITE(ICOUT,9910)
 9910   FORMAT('AT THE END OF CADDY')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE CADDY
      SUBROUTINE FUZZY(NN,MAXNN,P,DP,PT,DSS,ESP,EF,EDA,EDB,K,   &
                       IBUGA3,ISUBRO)
!
!NIST SUBROUTINE FUZZY(NN,MAXNN,MAXKK,MAXHH,P,DP,PT,LAB,DSS,ESP,EF,
!NIST1                 EDA,EDB,K,LUB)
!
      DIMENSION P(MAXNN,*),DP(MAXNN,*)
      DIMENSION DSS(*),PT(*),ESP(*),EF(*)
!NIST CHARACTER LAB(3,MAXNN)
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 ISUBRO
!
!CCCC CHARACTER*3 LAB
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
        WRITE(ICOUT,10)
   10   FORMAT('AT THE BEGINNING OF FUZZY')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,11)NN,MAXNN,K
   11   FORMAT('NN,MAXNN,K = ',3I6)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!C
!C     R IS THE EXPONENT, STRICTLY LARGER THAN 1.0
!C     EPS IS THE PRECISION FOR THE ITERATIONS
!C     NYT IS THE MAXIMAL NUMBER OF ITERATIONS
!C
      R=2.0
      EPS=0.000001
      NYT=500
!C
!C   INITIAL FUZZY CLUSTERING
!C
      NNSUB=NN-1
      RVERS=1./R
      RKME=REAL(K-1)
      DO 30 M=1,NN
        DO 20 L=1,K
          DP(M,L)=0.
          P(M,L)=0.1/RKME
   20   CONTINUE
   30 CONTINUE
!
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
!
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
!C
!C   INITIAL CRITERION VALUE
!C
      CRYT=0.
      DO 100 L=1,K
        ESP(L)=0.
        EF(L)=0.
        DO 90 M=1,NN
          ESP(L)=ESP(L)+P(M,L)
!
          IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
            WRITE(ICOUT,91)L,M,P(M,L),ESP(L)
   91       FORMAT('AT DO 100: L,M,P(M,L),ESP(L) = ',2I5,2G15.7)
            CALL DPWRST('XXX','BUG ')
          ENDIF
!
          DO 80 J=1,NN
            IF(J.EQ.M)GO TO 80
              J2=MIN0(M,J)
              J1=(J2-1)*NN-(J2*(J2+1))/2+MAX0(M,J)
              DP(M,L)=DP(M,L)+P(J,L)*DSS(J1)
              EF(L)=EF(L)+P(J,L)*P(M,L)*DSS(J1)
!
              IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
                WRITE(ICOUT,82)J,J1,J2,DSS(J1),DP(M,L),EF(L)
   82           FORMAT('AT DO 80: J,J1,J2,DSS(J1),DP(M,L),EF(L)=',   &
                      3I5,3G15.7)
                CALL DPWRST('XXX','BUG ')
              ENDIF
!
   80     CONTINUE
   90   CONTINUE
        CRYT=CRYT+EF(L)/(ESP(L)*2.)
  100 CONTINUE
      CRT=CRYT
      REEN=1./(R-1.)
!C
!C   START OF ITERATIONS
!C
      KAUNT=1
      M=0
!C
!C   THE NEW MEMBERSHIP COEFFICIENTS OF THE OBJECTS ARE CALCULATED,
!C   AND THE RESULTING VALUE OF THE CRITERION IS COMPUTED.
!C
  200 CONTINUE
      M=M+1
      DT=0.
      DO 210 L=1,K
        PT(L)=((2.*ESP(L)*ESP(L))/(2.*ESP(L)*DP(M,L)-EF(L)))**REEN
        DT=DT+PT(L)
  210 CONTINUE
!
      XX=0.
      DO 220 L=1,K
        PT(L)=PT(L)/DT
        IF(PT(L).LE.0.)XX=XX+PT(L)
  220 CONTINUE
!
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
!
      IF(M.LT.NN)GO TO 200
      CRYT=0.
      EDA=0.
      DO 250 L=1,K
        ANN=NN
        EDA=EDA+ESP(L)/ANN
        CRYT=CRYT+EF(L)/(ESP(L)*2.)
  250 CONTINUE
!C
!C   CRITERION IS PRINTED AND TESTED FOR CONVERGENCE
!C
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,9120)KAUNT,CRYT
 9120   FORMAT(I5,11X,F11.4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF((CRT/CRYT-1.).LE.EPS)GO TO 500
      IF(KAUNT.LT.NYT)THEN
        M=0
        KAUNT=KAUNT+1
        CRT=CRYT
        GO TO 200
      ENDIF
!
      IF(IPRINT.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9130)NYT
 9130   FORMAT('The maximum number of iterations (',I3,   &
               ') has been reached.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9131)
 9131   FORMAT('The iterative procedure is therefore interrupted.')
        CALL DPWRST('XXX','BUG ')
        GO TO 500
      ENDIF
!C
!C   NON-FUZZYNESS INDEX OF LIBERT IS COMPUTED
!C
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
!
      RNN=NN
      FL=(FL/RNN+SMALL)/2.
      RK=K
      FL=(RK*FL-1.)/(RK-1.)
!
!C    IF(IPRINT.EQ.'ON')THEN
!C      WRITE(ICOUT,999)
!C      CALL DPWRST('XXX','BUG ')
!C      WRITE(ICOUT,9135)FL
!9135   FORMAT('NON-FUZZYNESS INDEX OF LIBERT = ',F5.2)
!C      CALL DPWRST('XXX','BUG ')
!C      WRITE(ICOUT,999)
!C      CALL DPWRST('XXX','BUG ')
!C    ENDIF
!
      ZK=K
      EDB=(ZK*EDA-1.)/(ZK-1.)
      DO 520 M=1,NN
        DO 510 L=1,K
          P(M,L)=P(M,L)**RVERS
  510   CONTINUE
  520 CONTINUE
!
      IF(IBUGA3.EQ.'ON' .OR. ISUBRO.EQ.'UZZY')THEN
        WRITE(ICOUT,9910)
 9910   FORMAT('AT THE END OF FUZZY')
        CALL DPWRST('XXX','BUG ')
        DO 9911 II=1,NN
          WRITE(ICOUT,9912)II,(P(II,JJ),JJ=1,K)
 9912     FORMAT('II,(P(II,JJ),JJ=1,K) = ',I6,20F7.4)
          CALL DPWRST('XXX','BUG ')
 9911   CONTINUE
      ENDIF
!
      RETURN
      END SUBROUTINE FUZZY

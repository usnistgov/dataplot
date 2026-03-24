!     THIS FILE CONTAINS ROUTINES FOR BASIC COMPUTATIONAL
!     GEOMETRY.  THIS COMBINES CODES FROM A NUMBER OF SEPARATE
!     SOURCES:
!
!        1. ACM 523  - FOR 2D CONVEX HULL
!        2. JAVIER BERNAL'S CODES FOR DELAUNAY TRIANGULARIZATION
!           AND VORONI DIAGRAMS.
!
!           THIS IS FOR POSSIBLE FUTURE DEVELOPMENT, NOTHING
!           CURRENTLY ACTIVE WITHIN DATAPLOT.
!
!        3. NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!           ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19
!
!           a. MATRIX PERMANENT
!           b. MINIMUM SPANNING TREE
!           c. NEXT PERMUTATION
!
      SUBROUTINE CONVEX(N,X,M,IN,IA,IB,IH,NH,IL)
!
!     DATAPLOT NOTE: FROM ACM 523 (FOR FINDING THE 2D
!                    CONVEX HULL).  RENAME THE "SPLIT"
!                    ROUTINE TO "SPLITC" TO AVOID NAME
!                    CONFLICT.
!
! THIS SUBROUTINE DETERMINES WHICH OF THE M POINTS OF ARRAY
! X WHOSE SUBSCRIPTS ARE IN ARRAY IN ARE VERTICES OF THE
! MINIMUM AREA CONVEX POLYGON CONTAINING THE M POINTS. THE
! SUBSCRIPTS OF THE VERTICES ARE PLACED IN ARRAY IH IN THE
! ORDER THEY ARE FOUND. NH IS THE NUMBER OF ELEMENTS IN
! ARRAY IH AND ARRAY IL. ARRAY IL IS A LINKED LIST GIVING
! THE ORDER OF THE ELEMENTS OF ARRAY IH IN A COUNTER
! CLOCKWISE DIRECTION. THIS ALGORITHM CORRESPONDS TO A
! PREORDER TRAVERSAL OF A CERTAIN BINARY TREE. EACH VERTEX
! OF THE BINARY TREE REPRESENTS A SUBSET OF THE M POINTS.
! AT EACH STEP THE SUBSET OF POINTS CORRESPONDING TO THE
! CURRENT VERTEX OF THE TREE IS PARTITIONED BY A LINE
! JOINING TWO VERTICES OF THE CONVEX POLYGON. THE LEFT SON
! VERTEX IN THE BINARY TREE REPRESENTS THE SUBSET OF POINTS
! ABOVE THE PARTITIONING LINE AND THE RIGHT SON VERTEX, THE
! SUBSET BELOW THE LINE. THE LEAVES OF THE TREE REPRESENT
! EITHER NULL SUBSETS OR SUBSETS INSIDE A TRIANGLE WHOSE
! VERTICES COINCIDE WITH VERTICES OF THE CONVEX POLYGON.
! FORMAL PARAMETERS
! INPUT
! N  INTEGER           TOTAL NUMBER OF DATA POINTS
! X  REAL ARRAY (2,N)  (X,Y) CO-ORDINATES OF THE DATA
! M  INTEGER           NUMBER OF POINTS IN THE INPUT SUBSET
! IN INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE POINTS
!                      IN THE INPUT SUBSET
! WORK AREA
! IA INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF LEFT SON
!                      SUBSETS. SEE COMMENTS AFTER DIMENSION
!                      STATEMENTS
! IB INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF RIGHT SON
!                      SUBSETS
! OUTPUT
! IH INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
!                      VERTICES OF THE CONVEX HULL
! NH INTEGER           NUMBER OF ELEMENTS IN ARRAY IH AND
!                      ARRAY IL. SAME AS NUMBER OF VERTICES
!                      OF THE CONVEX POLYGON
! IL INTEGER ARRAY (M) A LINKED LIST GIVING IN ORDER IN A
!                      COUNTER-CLOCKWISE DIRECTION THE
!                      ELEMENTS OF ARRAY IH
      DIMENSION X(2,N)
      DIMENSION IN(M),IA(M),IB(M),IH(M),IL(M)
! THE UPPER END OF ARRAY IA IS USED TO STORE TEMPORARILY
! THE SIZES OF THE SUBSETS WHICH CORRESPOND TO RIGHT SON
! VERTICES, WHILE TRAVERSING DOWN THE LEFT SONS WHEN ON THE
! LEFT HALF OF THE TREE, AND TO STORE THE SIZES OF THE LEFT
! SONS WHILE TRAVERSING THE RIGHT SONS(DOWN THE RIGHT HALF)
      LOGICAL MAXE,MINE
      IF(M.EQ.1)GO TO  22
      IL(1)=2
      IL(2)=1
      KN=IN(1)
      KX=IN(2)
      IF(M.EQ.2)GO TO  21
      MP1=M+1
      MIN=1
      MX=1
      KX=IN(1)
      MAXE=.FALSE.
      MINE=.FALSE.
! FIND TWO VERTICES OF THE CONVEX HULL FOR THE INITIAL
! PARTITION
      DO 6 I=2,M
        J=IN(I)
!CCCC   IF(X(1,J)-X(1,KX))3,1,2
        IF(X(1,J)-X(1,KX).EQ.0.)THEN
          MAXE=.TRUE.
        ELSEIF(X(1,J)-X(1,KX).GT.0.)THEN
          MAXE=.FALSE.
          MX=I
          KX=J
        ENDIF
!CCCC   IF(X(1,J)-X(1,KN))5,4,6
        IF(X(1,J)-X(1,KN).EQ.0.)THEN
          MINE=.TRUE.
        ELSEIF(X(1,J)-X(1,KN).LT.0.)THEN
          MINE=.FALSE.
          MIN=I
          KN=J
        ENDIF
6     CONTINUE
! IF THE MAX AND MIN ARE EQUAL, ALL M POINTS LIE ON A
! VERTICAL LINE
      IF(KX.EQ.KN)GO TO  18
! IF MAXE (OR MINE) HAS THE VALUE TRUE THERE ARE SEVERAL
! MAXIMA (OR MINIMA) WITH EQUAL FIRST COORDINATES
      IF(MAXE.OR.MINE)GO TO  23
7     IH(1)=KX
      IH(2)=KN
      NH=3
      INH=1
      NIB=1
      MA=M
      IN(MX)=IN(M)
      IN(M)=KX
      MM=M-2
      IF(MIN.EQ.M)MIN=MX
      IN(MIN)=IN(M-1)
      IN(M-1)=KN
! BEGIN BY PARTITIONING THE ROOT OF THE TREE
      CALL SPLITC(N,X,MM,IN,IH(1),IH(2),0,IA,MB,MXA,IB,IA(MA),   &
        MXBB)
! FIRST TRAVERSE THE LEFT HALF OF THE TREE
! START WITH THE LEFT SON
8     NIB=NIB+IA(MA)
      MA=MA-1
9     IF(MXA.EQ.0)GO TO  11
      IL(NH)=IL(INH)
      IL(INH)=NH
      IH(NH)=IA(MXA)
      IA(MXA)=IA(MB)
      MB=MB-1
      NH=NH+1
      IF(MB.EQ.0)GO TO  10
      ILINH=IL(INH)
      CALL SPLITC(N,X,MB,IA,IH(INH),IH(ILINH),1,IA,MBB,MXA,   &
        IB(NIB),IA(MA),MXB)
      MB=MBB
      GO TO  8
! THEN THE RIGHT SON
10    INH=IL(INH)
11    INH=IL(INH)
      MA=MA+1
      NIB=NIB-IA(MA)
      IF(MA.GE.M)GO TO  12
      IF(IA(MA).EQ.0)GO TO  11
      ILINH=IL(INH)
! ON THE LEFT SIDE OF THE TREE, THE RIGHT SON OF A RIGHT SON
! MUST REPRESENT A SUBSET OF POINTS WHICH IS INSIDE A
! TRIANGLE WITH VERTICES WHICH ARE ALSO VERTICES OF THE
! CONVEX POLYGON AND HENCE THE SUBSET MAY BE NEGLECTED.
      CALL SPLITC(N,X,IA(MA),IB(NIB),IH(INH),IH(ILINH),2,IA,   &
        MB,MXA,IB(NIB),MBB,MXB)
      IA(MA)=MBB
      GO TO  9
! NOW TRAVERSE THE RIGHT HALF OF THE TREE
12    MXB=MXBB
      MA=M
      MB=IA(MA)
      NIA=1
      IA(MA)=0
! START WITH THE RIGHT SON
13    NIA=NIA+IA(MA)
      MA=MA-1
14    IF(MXB.EQ.0)GO TO  16
      IL(NH)=IL(INH)
      IL(INH)=NH
      IH(NH)=IB(MXB)
      IB(MXB)=IB(MB)
      MB=MB-1
      NH=NH+1
      IF(MB.EQ.0)GO TO  15
      ILINH=IL(INH)
      CALL SPLITC(N,X,MB,IB(NIB),IH(INH),IH(ILINH),-1,IA(NIA),   &
        IA(MA),MXA,IB(NIB),MBB,MXB)
      MB=MBB
      GO TO  13
! THEN THE LEFT SON
15    INH=IL(INH)
16    INH=IL(INH)
      MA=MA+1
      NIA=NIA-IA(MA)
      IF(MA.EQ.MP1)GO TO  17
      IF(IA(MA).EQ.0)GO TO  16
      ILINH=IL(INH)
! ON THE RIGHT SIDE OF THE TREE, THE LEFT SON OF A LEFT SON
! MUST REPRESENT A SUBSET OF POINTS WHICH IS INSIDE A
! TRIANGLE WITH VERTICES WHICH ARE ALSO VERTICES OF THE
! CONVEX POLYGON AND HENCE THE SUBSET MAY BE NEGLECTED.
      CALL SPLITC(N,X,IA(MA),IA(NIA),IH(INH),IH(ILINH),-2,   &
        IA(NIA),MBB,MXA,IB(NIB),MB,MXB)
      GO TO  14
17    NH=NH-1
      RETURN
! ALL THE SPECIAL CASES ARE HANDLED DOWN HERE
! IF ALL THE POINTS LIE ON A VERTICAL LINE
18    KX=IN(1)
      KN=IN(1)
      DO 20 I=1,M
        J=IN(I)
        IF(X(2,J).LE.X(2,KX))GO TO  19
        MX=I
        KX=J
19      IF(X(2,J).GE.X(2,KN))GO TO  20
        MIN=I
        KN=J
20    CONTINUE
      IF(KX.EQ.KN)GO TO  22
! IF THERE ARE ONLY TWO POINTS
21    IH(1)=KX
      IH(2)=KN
      NH=3
      IF((X(1,KN).EQ.X(1,KX)).AND.(X(2,KN).EQ.X(2,KX)))NH=2
      GO TO  17
! IF THERE IS ONLY ONE POINT
22    NH=2
      IH(1)=IN(1)
      IL(1)=1
      GO TO  17
! MULTIPLE EXTREMES ARE HANDLED HERE
! IF THERE ARE SEVERAL POINTS WITH THE (SAME) LARGEST
! FIRST COORDINATE
23    IF(.NOT.MAXE)GO TO  25
      DO 24 I=1,M
        J=IN(I)
        IF(X(1,J).NE.X(1,KX))GO TO  24
        IF(X(2,J).LE.X(2,KX))GO TO  24
        MX=I
        KX=J
24    CONTINUE
! IF THERE ARE SEVERAL POINTS WITH THE (SAME) SMALLEST
! FIRST COORDINATE
25    IF(.NOT.MINE)GO TO  7
      DO 26 I=1,M
        J=IN(I)
        IF(X(1,J).NE.X(1,KN))GO TO  26
        IF(X(2,J).GE.X(2,KN))GO TO  26
        MIN=I
        KN=J
26    CONTINUE
      GO TO  7
      END SUBROUTINE CONVEX
      SUBROUTINE CYCLES (SIGMA, N, SIGN, NCYCL, OPTION)
!
!     COUNT CYCLES, FIND SIGNS OF PERMUTATIONS, TAG AND/OR
!     INVERT.
!
!     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
!                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
!                ACADEMIC PRESS, 1978, CHAPTER 16.
!
      INTEGER SIGMA(N), SIGN, OPTION
!
      IS=1
      NCYCL=N
      DO 5 I=1,N
         I1=SIGMA(I)
6        CONTINUE
            IF (I1 .LE. I) GO TO  7
            NCYCL=NCYCL-1
            I2=SIGMA(I1)
            SIGMA(I1)=-I2
            I1=I2
            GO TO 6
7        CONTINUE
            IF (OPTION .NE. 0) IS=-ISIGN(1, SIGMA(I))
            SIGMA(I)=ISIGN(SIGMA(I), IS)
5     CONTINUE
!
      SIGN=1-2*MOD(N-NCYCL, 2)
      IF (OPTION .GE. 0) RETURN
      DO 10 I=1, N
         I1=-SIGMA(I)
         IF (I1 .LT. 0) GO TO 10
         I0=I
15       CONTINUE
            I2=SIGMA(I1)
            SIGMA(I1)=I0
            IF (I2 .LT. 0) GO TO 10
            I0=I1
            I1=I2
            GO TO 15
10    CONTINUE
!
      RETURN
      END SUBROUTINE CYCLES 
      SUBROUTINE CONYTB (Y, VAL, ROWID, N,   &
                         TEMP1,GROUP,   &
                         IBUGA3,IERROR)
!
!     PURPOSE--CONVERT THE OUTPUT FROM THE NEXT YOUNG TABLEAUX
!              OR RANDOM YOUNG TABLEAUX COMMANDS TO A DIFFERENT
!              FORMAT.
!
!              THESE COMMANDS RETURN THE DATA IN THE FORM WHERE
!              THE I-TH ELEMENT OF Y IDENTIFIES THE ROW THAT
!              CONTAINS THE VALUE I.  THIS FORM IS USED BECAUSE
!              IT IS CONVENIENT FOR COMPUTER PROCESSING.
!              HOWEVER, IT MAY BE CONVENIENT TO VIEW THE
!              TABLE AS A "ROWID" AND "VALUE" SINCE THIS MAKES
!              IT EASIER TO VISUALIZE THE TABLEAUX.  THIS ROUTINES
!              CONVERTS Y TO "ROWID" AND "VALUE".
!     INPUT  ARGUMENTS--Y      THE YUOUNG TABLEAUX IN THE FORMAT
!                              GENERATED BY THE "NEXT YOUNG TABLEAUX"
!                              AND THE "RANDOM YOUNG TABLEAUX"
!                              COMMAND.  INTEGER ARRAY.
!                     --N      AN INTEGER SCALAR CONTAINING THE
!                              NUMBER OF ELEMENTS IN THE YOUNG
!                              TABLEAUX.
!     OUTPUT ARGUMENTS--VAL    INTEGER ARRAY CONTAINING THE VALUE
!                              OF THE I-TH ENTRY IN THE YOUNG
!                              TABLEAUX.
!                     --ROWID  INTEGER ARRAY CONTAINING THE ROW ID
!                              OF THE I-TH ENTRY IN THE YOUNG
!                              TABLEAUX.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/8
!     ORIGINAL VERSION--AUGUST   2008
!     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
!                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
!                ACADEMIC PRESS, 1978, CHAPTER 14.
!
      INTEGER Y(*)
      INTEGER VAL(*)
      INTEGER ROWID(*)
      INTEGER N
!
      REAL TEMP1(*)
      REAL GROUP(*)
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,11)N
   11   FORMAT('FROM CONYTB: N = ',I8)
        CALL DPWRST('XXX','WRIT')
        DO 20 I=1,N
          WRITE(ICOUT,21)I,Y(I)
   21     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   20   CONTINUE
      ENDIF
!
!     STEP 1--DETERMINE THE NUMBER OF DISTINCT ROWS IN Y
!
      IWRITE='OFF'
      DO 100 I=1,N
        TEMP1(I)=REAL(Y(I))
  100 CONTINUE
      CALL DISTIN(TEMP1,N,IWRITE,GROUP,NUMDIS,IBUGA3,IERROR)
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)NUMDIS
  101   FORMAT('AFTER CALL DISTIN: NUMDIS=',I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
!
      CALL SORT(GROUP,NUMDIS,GROUP)
      GRPMIN=GROUP(1)
      GRPMAX=GROUP(NUMDIS)
      IF(GRPMIN.LT.1 .OR. GRPMAX.GT.N)THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,111)
  111   FORMAT('***** ERROR IN CONVERT YOUNG TABLEAUX--')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,113)
  113   FORMAT('      A ROW ID IN INPUT YOUNG TABLEAUX IS OUT OF ',   &
               'RANGE.')
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,115)N
  115   FORMAT('      NUMBER OF ELEMENTS IN YOUNG TABLEAUX    = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,116)GRPMIN
  116   FORMAT('      MINIMUM ROW ID                          = ',I8)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,117)GRPMAX
  117   FORMAT('      MAXIMUM ROW ID                          = ',I8)
        CALL DPWRST('XXX','WRIT')
        IERROR='YES'
        GO TO 9999
      ENDIF
!
!     STEP 2--LOOP THROUGH DISTINCT ROW ID'S.
!
      ICNT=0
      DO 200 I=1,NUMDIS
        IVAL=INT(GROUP(I) + 0.01)
!
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,201)I,IVAL
  201     FORMAT('I,IVAL = ',2I8)
          CALL DPWRST('XXX','WRIT')
        ENDIF
!
        DO 300 J=1,N
          IF(Y(J).EQ.IVAL)THEN
            ICNT=ICNT+1
            ROWID(ICNT)=Y(J)
            VAL(ICNT)=J
          ENDIF
  300   CONTINUE
  200 CONTINUE
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE CONYTB 
      SUBROUTINE EXHEAP (N, INDEX, I, J, ISGN)
!
!   SORT A LIST OF ITEMS INTO INTEGER ORDER.
!   NOTE THAT THIS ROUTINE HAS THE CALLING ROUTINE PERFORM
!   THE SWAP.  IT IS BASICALLY AN INTERNAL ROUTINE USED BY
!   OTHER NIJENHUIS AND WILF ROUTINES (I.E., DATAPLOT DOES
!   NOT CALL THIS ROUTINE DIRECTLY).
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', SECOND EDITION, ACADEMIC PRESS, 1978,
!              CH. 18, P. 141-142.
!=====================================================================
!
      L1=0
      L=0
      N1=0
!
      IF (INDEX.LT.0)THEN
         IF (INDEX .EQ. -1) THEN
            IF (ISGN .LE. 0) GO TO 70
            INDEX=2
            GO TO 9000
         ENDIF
         IF (ISGN .LT. 0) I=I+1
         J=L1
         L1=I
         INDEX=-1
         GO TO 9000
!
      ELSEIF (INDEX.EQ.0) THEN
         N1=N
         L=1+N/2
         GO TO 20
      ELSE
         IF (INDEX-1.LE.0) GO TO 30
         GO TO 40
      ENDIF
!
20    CONTINUE
      L=L-1
!
30    CONTINUE
      L1=L
!
40    CONTINUE
      I=L1+L1
      IF (I-N1.LT.0) THEN
         J=I+1
         INDEX=-2
         GO TO 9000
      ELSEIF (I-N1.EQ.0) THEN
         J=L1
         L1=I
         INDEX=-1
         GO TO 9000
      ENDIF
!
70    CONTINUE
      IF (L .GT. 1) GO TO 20
      IF (N1 .EQ. 1) THEN
         INDEX=0
         GO TO 9000
      ENDIF
      I=N1
      N1=N1-1
      J=1
      INDEX=1
      GO TO 9000
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE EXHEAP 
      subroutine inver2 (n,a,ainv)
      integer a,ainv,sum
      dimension a(n,n),ainv(n,n)
!
!   RENAME TO AVOID NAME CONFLICT WITH A ROUTINE USED IN
!   "CLUSTER.FOR".
!
!   INVERT AN INTEGER UPPER TRIANGULAR MATRIX.
!   IT IS BASICALLY AN INTERNAL ROUTINE USED BY
!   OTHER NIJENHUIS AND WILF ROUTINES (I.E., DATAPLOT DOES
!   NOT CALL THIS ROUTINE DIRECTLY).
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', SECOND EDITION, ACADEMIC PRESS, 1978,
!              CH. 18, P. 141-142.
!=====================================================================
!
      j=n
!
10    continue
      i=n
20    continue
      sum=0
      if (i.eq.j) sum=1
      k=i+1
!
25    continue
      if (k.gt.j) then
         ainv (i,j)=sum
         i=i-1
         if (i.gt.0) go to 20
         j=j-1
         if (j.gt.0) go to 10
      else
         sum=sum-a(i,k)*ainv(k,j)
         k=k+1
         go to 25
      endif
!
      return
      end
      subroutine minspt(dist,maxrow,n,endpt1,endpt2,u,y)
!
!   PURPOSE: COMPUTE MININUM SPANNING TREE OF A DISTANCE MATRIX.
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, P. 285.
!
!   DIST      - DISTANCE MATRIX (INPUT)
!   N         - NUMBER OF POINTS IN THE DISTANCE MATRIX
!   ENDPT     - 2-D VECTOR DEFINING EDGES OF THE MINIMAL
!               SPANNING TREE (OUTPUT)
!               (FOR DATAPLOT, SPLIT INTO 2 SEPARATE VECTORS)
!   U         - WORKING STORAGE (INTEGER)
!   Y         - WORKING STORAGE (REAL)
!=======================================================================
!
      integer u(*),endpt1(*),endpt2(*)
      dimension y(*),dist(maxrow,*)
!
      imin=0
      l=0
      do 21  i=2,n
         u(i)=1
         y(i)=dist(1,i)
21    continue
!
30    continue
      dmin=1.e37
      do 41  i=2,n
         if(y(i).le.0.0) go to 41
         if(y(i).ge.dmin) go to 41
         dmin=y(i)
         imin=i
41    continue
!
      l=l+1
      endpt1(l)=imin
      endpt2(l)=u(imin)
!
      if(l.eq.n-1) go to 999
      y(imin)=0
!
      do 111  i=2,n
         if(y(i).eq.0.0) go to 111
         d1=dist(i,imin)
         if(y(i).le.d1) go to 111
         u(i)=imin
         y(i)=d1
111   continue
      go to 30
!
999   continue
      return
      end
      SUBROUTINE NEXCOM(N,K,R,MTC)
!
!   GENERATE NEXT COMPOSITION OF N INTO K PARTS.
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, CH. x, P. 49.
!=====================================================================
!
      INTEGER R(K),T,H
      LOGICAL MTC
!
      COMMON/NIJWIL/NLAST,KLAST
!
      SAVE H, T
!
      IF(MTC)THEN
         IF(T.GT.1) H=0
         H=H+1
         T=R(H)
         R(H)=0
         R(1)=T-1
         R(H+1)=R(H+1)+1
         MTC=R(K).NE.N
      ELSE
         R(1)=N
         T=N
         H=0
         IF(K.GT.1)THEN
           DO 11 I=2,K
              R(I)=0
   11      CONTINUE
         ENDIF
         MTC=R(K).NE.N
      ENDIF
!
      RETURN
      END SUBROUTINE NEXCOM
      SUBROUTINE NEXEQU(N,NC,P,Q,MTC)
!
!     NEXT PARTITION OF AN N-SET.
!   GENERATE NEXT PARTITION OF AN N-SET.
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 11, P. 91.
!=====================================================================
!
      LOGICAL MTC
      INTEGER P(N),Q(N)
!
      SAVE NCLAST
!
      COMMON/NIJWIL/NLAST,KLAST
!
      NC=NCLAST
!
      IF(.NOT.MTC)THEN
!
!       FIRST IN SEQUENCE
!
        NLAST=1
        NC=1
        DO 11 I=1,N
          Q(I)=1
   11   CONTINUE
        P(1)=N
!
      ELSE
!
!       CONTINUE PREVIOUS SEQUENCE
!
        M=N
   30   CONTINUE
        L=Q(M)
        IF(P(L).EQ.1)THEN
          Q(M)=1
          M=M-1
          GO TO  30
        ENDIF
        NC=NC+M-N
        P(1)=P(1)+N-M
        IF(L.EQ.NC)THEN
          NC=NC+1
          P(NC)=0
        ENDIF
        Q(M)=L+1
        P(L)=P(L)-1
        P(L+1)=P(L+1)+1
      ENDIF
!
      MTC=NC.NE.N
      NCLAST=NC
!
      RETURN
      END SUBROUTINE NEXEQU
      SUBROUTINE NEXKSB(N,K,A,MTC)
!
!   GENERATE NEXT "k-SET OF AN N-SET".
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 3, P. 28.
!=======================================================================
!
      LOGICAL MTC
      INTEGER A(*)
      INTEGER H
!
      COMMON/NIJWIL/NLAST,KLAST
!
      SAVE H
!
      M2=0
!
      IF(MTC .AND. K.EQ.KLAST .AND. N.EQ.NLAST) THEN
        DO 41  H=1,K
           I=K+1-H
           M2=A(I)
           IF(M2.NE.N+1-H) GO TO  50
41      CONTINUE
      ELSE
        M2=0
        H=K
        NLAST=N
        KLAST=K
        MTC=.TRUE.
      ENDIF
!
50    CONTINUE
      DO 51 J=1,H
         I=K+J-H
         A(I)=M2+J
   51 CONTINUE
      MTC=(A(1).NE.N-K+1)
!
      RETURN
      END SUBROUTINE NEXKSB
      SUBROUTINE NEXPAR(N,R,M,D,MTC)
!
!   GENERATE NEXT PARTITION OF N.
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 9, P. 69.
!=====================================================================
!
      IMPLICIT INTEGER(A-Z)
      LOGICAL MTC
      DIMENSION M(*),R(*)
!
      COMMON/NIJWIL/NLAST,KLAST
!
      IF(N.NE.NLAST .OR. (.NOT.MTC)) THEN
!
!       NEW SEQUENCE
!
        IF(N.NE.NLAST)NLAST=N
        S=N
        D=0
        D=D+1
        R(D)=S
        M(D)=1
        MTC=M(D).NE.N
      ELSE
!
!       OLD SEQUENCE
!
        SUM=1
        IF (R(D).LE.1) THEN
          SUM=M(D)+1
          D=D-1
        ENDIF
        F=R(D)-1
        IF (M(D).NE.1) THEN
          M(D)=M(D)-1
          D=D+1
        ENDIF
        R(D)=F
        M(D)=1+SUM/F
        S=MOD(SUM,F)
        IF (S.GT.0) THEN
          D=D+1
          R(D)=S
          M(D)=1
        ENDIF
        MTC=M(D).NE.N
      ENDIF
!
      RETURN
      END SUBROUTINE NEXPAR
      SUBROUTINE NEXPER (RED,PINK,BROWN)
!-----------------------------------------------------------------------
!   NEXPER   COPIED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
!            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
!            MARYLAND 20899 FROM THE REFERENCE BELOW (COLOR ADDED)
!
!   FOR: COMPUTING THE NEXT PERMUTATION OF THE INTEGERS 1, 2, ..., N.
!        THE CALLING SEQUENCE IS
!
!                        CALL NEXPER (IPERM,N,LL)
!
!        WHERE IPERM IS AN INTEGER VECTOR DIMENSIONED AT LEAST N AND
!        LL IS A LOGICAL VARIABLE.  NONE OF THESE PASSED PARAMETERS
!        NEEDS TO BE DEFINED ON INPUT.  ON OUTPUT IPERM CONTAINS THE
!        CURRENT PERMUTATION OF THE FIRST N INTEGERS AND LL IS .TRUE.
!        UNLESS THIS PERMUTATION IS THE LAST PERMUTATION OF THE CYCLE
!        (IN WHICH CASE LL IS .FALSE.).
!
!   SUBPROGRAMS CALLED: -NONE-
!
!   CURRENT VERSION COMPLETED JANUARY 20, 1987
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, PP. 49-59.
!-----------------------------------------------------------------------
      IMPLICIT INTEGER (A-Z)
      LOGICAL BROWN
      DIMENSION RED(*)
      DATA MAROON / 0 /
!
      SILVER=0
      PURPLE=0
      BLACK=0
      ORANGE=0
      BLUE=0
!
      IF (PINK.EQ.MAROON) GO TO 40
   10 MAROON = PINK
      ORANGE = 1
      SILVER = 1
      PURPLE = 1
      DO 20 BLUE = 1, PINK
         PURPLE = PURPLE*BLUE
         RED(BLUE) = BLUE
   20 CONTINUE
   30 BROWN = ORANGE.NE.PURPLE
      RETURN
   40 IF (.NOT.BROWN) GO TO 10
      GO TO (50,60), SILVER
   50 GOLD = RED(2)
      RED(2) = RED(1)
      RED(1) = GOLD
      SILVER = 2
      ORANGE = ORANGE+1
      GO TO 30
   60 YELLOW = 3
      BLACK = ORANGE/2
   70 VIOLET = MOD(BLACK,YELLOW)
      IF (VIOLET.NE.0) GO TO 80
      BLACK = BLACK/YELLOW
      YELLOW = YELLOW+1
      GO TO 70
   80 BLACK = PINK
      GREEN = YELLOW-1
      DO 90 BLUE = 1, GREEN
         WHITE = RED(BLUE)-RED(YELLOW)
         IF (WHITE.LT.0) WHITE = WHITE+PINK
         IF (WHITE.GE.BLACK) GO TO 90
         BLACK = WHITE
         INDIGO = BLUE
   90 CONTINUE
      GOLD = RED(YELLOW)
      RED(YELLOW) = RED(INDIGO)
      RED(INDIGO) = GOLD
      SILVER = 1
      ORANGE = ORANGE+1
      RETURN
      END SUBROUTINE NEXPER 
      SUBROUTINE NEXSUB (N,IWORK,MTC,NCARD,J)
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19.
!=======================================================================
!
      LOGICAL MTC
      DIMENSION IWORK(*)
      COMMON/NIJWIL/NLAST,KLAST
!
      SAVE M
!
      DATA NLAST / 0 /
!
      IF (N.EQ.NLAST) GO TO 30
   10 CONTINUE
      M = 0
      MTC = .TRUE.
      DO 20 I = 1, N
         IWORK(I) = 0
   20 CONTINUE
      NCARD = 0
      NLAST = N
      RETURN
   30 CONTINUE
      IF (.NOT.MTC) GO TO 10
      M = M+1
      M1 = M
      J = 0
   40 CONTINUE
      J = J+1
      IF (MOD(M1,2).EQ.1) GO TO 50
      M1 = M1/2
      GO TO 40
   50 CONTINUE
      L = IWORK(J)
      IWORK(J) = 1-L
      NCARD = NCARD+1-2*L
      MTC = NCARD.NE.1.OR.IWORK(N).EQ.0
      RETURN
      END SUBROUTINE NEXSUB 
      SUBROUTINE NEXYTB (N, LAMBDA, Y, MTC)
!
!     PURPOSE: SUPPLIES THE SEQUENCE OF YOUNG
!              TABLEAUX OF GIVEN SHAPE.
!
!     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
!                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
!                ACADEMIC PRESS, 1978, CHAPTER 14.
!
      LOGICAL MTC
      INTEGER LAMBDA(N), Y(N), R, T
!
      T=N
      IF (.NOT. MTC) GO TO 40
      LAMBDA(1)=1
!
      DO 21 I=2, N
         LAMBDA(I)=0
21    CONTINUE
!
      DO 22 J=2,N
         LAMBDA(Y(J))=LAMBDA(Y(J))+1
         IF (Y(J) .LT. Y(J-1)) GO TO 30
22    CONTINUE
!
      MTC=.FALSE.
      GO TO 9999
!
30    CONTINUE
      T=LAMBDA(1+Y(J))
      I=N
!
31    CONTINUE
      IF (LAMBDA(I) .EQ. T) GO TO 32
      I=I-1
      GO TO 31
!
32    CONTINUE
      Y(J)=I
      LAMBDA(I)=LAMBDA(I)-1
      T=J-1
!
40    CONTINUE
      L=1
!
43    CONTINUE
      R=1
!
42    CONTINUE
      IF (R .LE. N) THEN
         IF (LAMBDA(R) .EQ. 0) THEN
            IF (L .LE. T) THEN
               GO TO 43
            ELSE
               GO TO 45
            ENDIF
         ENDIF
         Y(L)=R
         LAMBDA(R)=LAMBDA(R)-1
         L=L+1
         R=R+1
         GO TO 42
      ENDIF
!
45    CONTINUE
      IF (N .EQ. 1) THEN
         MTC=.FALSE.
         GO TO 9999
      ELSE
         DO 46 J=2,N
            IF (Y(J) .LT. Y(J-1)) GO TO 50
46       CONTINUE
      ENDIF
!
50    CONTINUE
      MTC=.TRUE.
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE NEXYTB 
      SUBROUTINE PERMAN (A,LDA,N,IWORK,WORK,APERM)
!
!-----------------------------------------------------------------------
!   PERMAN   COPIED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
!            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
!            MARYLAND 20899 FROM THE REFERENCE BELOW
!
!   FOR: COMPUTING THE PERMANENT OF THE N BY N MATRIX A.  THE PERMANENT
!        OF A MATRIX IS SIMILAR TO THE DETERMINANT EXCEPT THAT THE
!        ALTERNATING SIGN CHANGES FOR THE TERMS ARE EXCLUDED.  FOR
!        EXAMPLE, GIVEN THE MATRIX
!
!                               [A B C]
!                               [D E F]
!                               [G H I]
!
!        THE DETERMINANT IS  AEI+BFG+CDH-CEG-BDI-AFH  WHEREAS THE
!        PERMANENT IS  AEI+BFG+CDH+CEG+BDI+AFH .
!
!   NOTE: THE COMPUTING TIME IS PROPORTIONAL TO 2**N, AND ON THE
!         CYBER 180/855 AT NBS A VALUE OF N=20 REQUIRES ABOUT 30
!         SECONDS OF CPU TIME.
!
!   SUBPROGRAMS CALLED: NEXSUB (ATTACHED)
!
!   CURRENT VERSION COMPLETED JANUARY 20, 1987
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19.
!-----------------------------------------------------------------------
!   DEFINITION OF PASSED PARAMETERS:
!
!     * A = MATRIX (SIZE NXN) WHOSE PERMANENT IS TO BE COMPUTED (REAL)
!
!   * LDA = THE LEADING DIMENSION OF MATRIX A [LDA>=N] (INTEGER)
!
!     * N = THE NUMBER OF ROWS AND COLUMNS IN MATRIX A (INTEGER)
!
!   IWORK = VECTOR (LENGTH N) USED AS SCRATCH AREA (INTEGER)
!
!    WORK = VECTOR (LENGTH N) USED AS SCRATCH AREA (REAL)
!
!   APERM = THE PERMANENT OF MATRIX A (REAL)
!
!   * INDICATES PARAMETERS REQUIRING INPUT VALUES
!-----------------------------------------------------------------------
      LOGICAL MTC
      DIMENSION A(LDA,*),IWORK(*),WORK(*)
!
      DOUBLE PRECISION P
      DOUBLE PRECISION Z
      DOUBLE PRECISION SUM
      DOUBLE PRECISION PROD
!
      COMMON/NIJWIL/NLAST,KLAST
!
      NLAST=0
!
      P = 0.0D0
      N1 = N-1
      DO 20 I = 1, N
         SUM = 0.0D0
         DO 10 J = 1, N
            SUM = SUM+DBLE(A(I,J))
   10    CONTINUE
         WORK(I) = REAL(DBLE(A(I,N))-SUM/2.0D0)
   20 CONTINUE
      SGN = -1.0
   30 SGN = -SGN
      PROD = SGN
      CALL NEXSUB (N1,IWORK,MTC,NCARD,J)
      IF (NCARD.EQ.0) GO TO 50
      Z = 2.0D0*DBLE(IWORK(J))-1.0D0
      DO 40 I = 1, N
         WORK(I) = REAL(DBLE(WORK(I))+Z*DBLE(A(I,J)))
   40 CONTINUE
   50 DO 60 I = 1, N
         PROD = PROD*DBLE(WORK(I))
   60 CONTINUE
      P = P+PROD
      IF (MTC) GO TO 30
      APERM = REAL(2.0D0*DBLE(2*MOD(N,2)-1)*P)
      RETURN
      END SUBROUTINE PERMAN 
      SUBROUTINE POLYNW(N,A,X0,OPTION,VAL,B)
!
!     PERFORM OPERATIONS ON POLYNOMIALS IN
!     POWER AND FACTORIAL FORM.
!
!     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!                ALGORITHMS', ACADEMIC PRESS, 1975, CH. 15, P. 175.
!
!     N    = N-1 IS DEGREE OF INPUT POLYNOMIAL
!     A    = COEFFICIENTS OF INPUT POLYNOMIAL
!     X0   = CONSTANT IN TAYLOR EXPANSION
!     VAL  = VALUE OF f(X0) ON OUTPUT
!     B    = COEFFICENTS OF OUTPUT POLYNOMIAL
!
!     OPTION    >  0 => VALUE OF OPTION DEFINES NUMBER OF TERMS
!                       IN TAYLOR EXPANSION, B WILL CONTAIN THE
!                       COEFFICIENTS OF THE TAYLOR EXPANSION
!     OPTION    =  0 => RETURNS THE VALUE OF THE USUAL POLYNOMIAL
!                       AT X) IN VAL
!     OPTION    = -1 => RETURN IN VAL THE VALUE OF f(X) WITH
!                       A(1) ... A(N) CONSIDERED AS THE
!                       COEFFICIENTS IN THE FACTORIAL FORM.
!     OPTION    = -2 => STIRLING ALGORITHM
!     OPTION    = -3 => REVERSE STIRLING ALGORITHM
!
!     NOTE: NAME CHANGED FROM "POLY" TO "POLYNW" TO AVOID NAME
!           CONFLICT WITH ROUTINE ALREADY IN DATAPLOT.
!
      INTEGER A,B,OPTION,V,VAL,X0,Z
      DIMENSION A(N),B(N)
!
      VAL=A(N)
      IF (N.EQ.1) GO TO 9000
!
      N1=N-1
      IF (OPTION.EQ.0) THEN
         DO 25 I=1,N1
            I1=N-I
            VAL=VAL*X0+A(I1)
25       CONTINUE
         GO TO 9000
      ELSEIF (OPTION.EQ.(-1)) THEN
         DO 27 I=1,N1
            I1=N-I
            VAL=VAL*(X0-N1+I)+A(I1)
27       CONTINUE
         GO TO 9000
      ENDIF
!
      DO 10 I=1,N
         B(I)=A(I)
10    CONTINUE
!
      IF (OPTION.LT.0) THEN
         IF (N.EQ.2) GO TO 9000
         N2=N-2
         IF (OPTION.EQ.(-3)) THEN
            DO 75 J=1,N2
               Z=N1-J
               M=Z+1
80             CONTINUE
               B(M)=B(M)-Z*B(M+1)
               M=M+1
               IF (M.LE.N1) GO TO  80
75          CONTINUE
            GO TO  9000
         ENDIF
!
         DO 55 J=1,N2
            V=VAL
            M=N1
60          CONTINUE
            V=B(M)+J*V
            B(M)=V
            M=M-1
            IF (M.GT.J) GO TO  60
55       CONTINUE
         GO TO 9000
      ELSE
!
         MAX=MIN0(N1,OPTION)
         DO 35 J=1,MAX
            M=N1
            V=VAL
37          CONTINUE
            V=B(M)+V*X0
            B(M)=V
            M=M-1
            IF (M.GE.J) GO TO  37
35       CONTINUE
         VAL=B(1)
         GO TO 9000
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE POLYNW
      subroutine ranequ(n,l,q,a,b,c,iseed,ytemp)
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', ACADEMIC PRESS, 1975, P. 97.
!
      integer q(n),a(n),c(n)
      dimension b(n), ytemp(n)
      dimension xjunk(1)
!
      EXTERNAL UNIRAN
!
      COMMON/NIJWIL/NLAST,KLAST
!
      b(1)=1
!
      if(n.gt.nlast) then
         m=nlast
         nlast=n
         nm1=n-1
         do 5  l=m,nm1
            sum=1./float(l)
            l1=l-1
            do 6  k=1,l1
               sum=(sum+b(k))/float(l-k)
    6       continue
            b(l+1)=(sum+b(l))/float(l+1)
    5    continue
      endif
!
      do 11  i=1,n
         q(i)=0
   11 continue
      l=0
      m=n
!
   20 continue
!cccc z1=uni(1)
      ntemp=1
      call uniran(ntemp,iseed,xjunk)
      z1=xjunk(1)
      k=m-1
      t=1.0/float(m)
      m1=m-1
!
   60 continue
      if (k.eq.0) then
         l=l+1
         do 71  i=1,n
            if(q(i).eq.0)  q(i)=l
   71    continue
         go to 9000
      endif
!
      z=t*b(k)/b(m)
      if (z1.ge.z) then
         k=k-1
         t=t/float(m-1-k)
         z1=z1-z
         go to 60
      endif
!
      l1=n
!
   81 continue
      if (q(l1).ne.0) then
         l1=l1-1
         go to 81
      endif
      l=l+1
      q(l1)=l
!cccc call ranksb(m1,k,a)
      call ranksb(k,m1,iseed,ytemp,a)
      m2=1
      i=1
!
   90 continue
      if (q(i).eq.0) then
         c(m2)=i
         m2=m2+1
         q(i)=l
      endif
!
      if (i.eq.m) then
         do 131  i=1,k
            j=a(i)
            j=c(j)
            q(j)=0
  131    continue
         m=k
         go to 20
      else
         i=i+1
         go to 90
      endif
!
 9000 continue
      return
      end
      SUBROUTINE RANYTB (N, LAM, Y, ISEED)
!
!     PURPOSE: SELECTS A YOUNG TABLEAUX OF GIVEN SHAPE.
!
!     INPUT ARGUMENTS:
!     N       = THE SCALAR INTEGER THAT IS PARTITIONED
!     LAM     = INTEGER ARRY THAT SPECIFES THE PARTITION
!     ISEED   = SEED FOR RANDOM NUMBER GENERATOR
!
!     OUTPUT ARGUMENTS:
!     Y       = INTEGER ARRAY CONTAINING THE OUTPUT TABLEAUX
!
!     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
!                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
!                ACADEMIC PRESS, 1978, CHAPTER 14.
!
      INTEGER LAM(N), Y(N), H
      REAL XJUNK(1)
!
      DO 5 I=1, N
         Y(I)=0
5     CONTINUE
!
      I=0
      L=0
10    CONTINUE
      I=I+1
      M=LAM(I)
      DO 20 J=1,M
         Y(J) = Y(J)+1
         L=L+1
20    CONTINUE
!
      IF (L .LT. N) GO TO 10
      NTEMP=1
      DO 85 M = 1,N
40       CONTINUE
         CALL UNIRAN(NTEMP,ISEED,XJUNK)
         I=1+INT(XJUNK(1)*Y(1))
         CALL UNIRAN(NTEMP,ISEED,XJUNK)
         J=1+INT(XJUNK(1)*LAM(1))
         IF (I .GT. Y(J) .OR. J .GT. LAM(I)) GO TO 40
70       CONTINUE
         H=Y(J)+LAM(I)-I-J
!
         IF (H .EQ. 0) THEN
            LAM(I)=LAM(I)-1
            Y(J)=Y(J)-1
            Y(N+1-M)=I
            GO TO 85
         ENDIF
!
         CALL UNIRAN(NTEMP,ISEED,XJUNK)
         L=INT(1.0+REAL(H)*XJUNK(1))
         IF (L .GT. LAM(I)-J) THEN
            I=L-LAM(I)+1+J
         ELSE
           J=J+L
         ENDIF
         GO TO 70
!
85    CONTINUE
!
      DO 90 I=1, N
         LAM(Y(I))=LAM(Y(I))+1
90    CONTINUE
!
      RETURN
      END SUBROUTINE RANYTB 
                                                                                                                                  
      subroutine renumb(m,n,sig,tau,a)
!
!     Following subroutine from:
!
!     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!     ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19.
!
!CCCC P.155
!
!     THIS IS A UTILITY FOR MINIMUM SPANNING TREES
!
      integer sig(m),tau(n),a(m,*),t1,t2
      do 5  i=1,m
        i1=sig(i)
6       continue
        if(i1.le.i) go to 3
        i2=sig(i1)
        sig(i1)=-i2
        i1=i2
        go to 6
3       continue
        sig(i)=-sig(i)
5     continue
!
      if(tau(1).lt.0) go to 9
      do 7  j=1,n
        j1=tau(j)
8       continue
        if(j1.le.j) go to 77
        j2=tau(j1)
        tau(j1)=-j2
        j1=j2
        go to 8
77      tau(j)=-tau(j)
7     continue
!
9     continue
      do 10  i=1,m
        i1=-sig(i)
        if(i1.lt.0) go to 10
        lc=0
20      continue
        i1=sig(i1)
        lc=lc+1
        if(i1.gt.0) go to 20
        i1=i
        do 30  j=1,n
          if(tau(j).gt.0) go to 30
          j2=j
          k=lc
40        continue
          j1=j2
          t1=a(i1,j1)
50        continue
          i1=iabs(sig(i1))
!cccc     t1=a(i1,j1)
!cccc     j1=iabs(tau(j1))
!cccc     t2=a(i1,j1)
!cccc     a(i1,j1)=t1
          j1=iabs(tau(j1))
          t2=a(i1,j1)
          a(i1,j1)=t1
          t1=t2
          if(j1.ne.j2) go to 50
          k=k-1
          if(i1.ne.i) go to 50
          j2=iabs(tau(j2))
!
          if(k.ne.0) go to 40
30      continue
10    continue
!
      do 60  i=1,m
        sig(i)=iabs(sig(i))
60    continue
!
      if(tau(1).gt.0) return
      do 70  j=1,n
        tau(j)=iabs(tau(j))
70    continue
!
      return
      end
      subroutine renum2(m,n,sig,tau,a)
!
!     Following subroutine from:
!
!     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!     ALGORITHMS', ACADEMIC PRESS, 1975, P. 155.
!
!
!     THIS IS A COPY OF "renumb", BUT WITH A REAL RATHER
!     THAN AN INTEGER MATRIX.
!
      integer sig(m),tau(n)
      real t1, t2, a(m,n)
!
      do 5  i=1,m
         i1=sig(i)
6        continue
         if(i1.le.i) go to 5
         i2=sig(i1)
         sig(i1)=-i2
         i1=i2
         go to 6
         sig(i)=-sig(i)
5     continue
      if(tau(1).lt.0) go to 9
      do 7  j=1,n
         j1=tau(j)
8        continue
         if(j1.le.j) go to 7
         j2=tau(j1)
         tau(j1)=-j2
         j1=j2
         go to 8
         tau(j)=-tau(j)
7     continue
9     continue
      do 10  i=1,m
         i1=-sig(i)
         if(i1.lt.0) go to 10
         lc=0
20       continue
         i1=sig(i1)
         lc=lc+1
         if(i1.gt.0) go to 20
         i1=i
         do 30  j=1,n
           if(tau(j).gt.0) go to 30
           j2=j
           k=lc
40         continue
           j1=j2
           t1=a(i1,j1)
50         continue
           i1=iabs(sig(i1))
           t1=a(i1,j1)
           j1=iabs(tau(j1))
           t2=a(i1,j1)
           a(i1,j1)=t1
           t1=t2
           if(j1.ne.j2) go to 50
           k=k-1
           if(i1.ne.i) go to 50
           j2=iabs(tau(j2))
!
           if(k.ne.0) go to 40
30       continue
10    continue
      do 60  i=1,m
         sig(i)=iabs(sig(i))
60    continue
      if(tau(1).gt.0) return
      do 70  j=1,n
         tau(j)=iabs(tau(j))
70    continue
      return
      end
      subroutine spanfo(n,e,endpt,k,x,nv,y)
!
!     Following subroutine from:
!
!     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!     ALGORITHMS', ACADEMIC PRESS, 1975.
!
!CCCC PP. 106-108.
!
!     THIS SUBROUTINE IS USED FOR:
!
!
!        1) DETERMINE THE CONNECTIVITY OF A GRAPH
!
!        2) FIND SPANNING FOREST
!
!        3) USED IN COMPUTING MINIMAL SPANNING TREES.
!
      integer e1,e,endpt,s,t1,t2,t,v1,v2,x,y,z
      dimension endpt(2,e),x(n),nv(n),y(n),s(2)
!
      m=2
      s(1)=1
      s(2)=2
!
      do 10  i=1,n
        x(i)=-i
        nv(i)=1
        y(i)=0
10    continue
      j=1
      e1=e
20    continue
      v1=endpt(1,j)
      v2=endpt(2,j)
!
      t1=x(v1)
      if(t1.lt.0)  t1=v1
      t2=x(v2)
      if(t2.lt.0)  t2=v2
      if(t1.ne.t2) go to 40
      if(j.lt.e1)  go to 30
      e1=e1-1
      go to 60
!
30    continue
      endpt(1,j)=endpt(1,e1)
      endpt(2,j)=endpt(2,e1)
      endpt(1,e1)=v1
      endpt(2,e1)=v2
      e1=e1-1
      go to 20
!
40    continue
      if(nv(t1).le.nv(t2))  go to 50
      t=t1
      t1=t2
      t2=t
!
50    continue
      i3=-x(t2)
      y(i3)=t1
      x(t2)=x(t1)
      i=t1
!
55    continue
      x(i)=t2
      i=y(i)
      if(i.ne.0) go to 55
      nv(t2)=nv(t2)+nv(t1)
      nv(t1)=0
      j=j+1
      if(j.le.e1.and.j.lt.n)  go to 20
!
60    continue
      k=0
      do 70  i=1,n
        if(nv(i).eq.0)  go to 70
        k=k+1
        nv(k)=nv(i)
        y(i)=k
70    continue
      do 80  i=1,n
        t=x(i)
        if(t.lt.0)  t=i
        x(i)=y(t)
80    continue
      if(k.eq.1)  return
!
      i2=nv(1)
      nv(1)=1
      do 100  l=2,k
        i1=nv(l)
        nv(l)=nv(l-1)+i2-1
        i2=i1
100   continue
      do 110  i=1,e1
        i3=endpt(1,i)
        z=x(i3)
        y(i)=nv(z)
        nv(z)=nv(z)+1
110   continue
      call renumb(m,e1,s,y,endpt)
      i1=1
      do 120  l=1,k
        i2=nv(l)
        nv(l)=i2-i1+1
        i1=i2
120   continue
!
      return
      end
      subroutine spntre(e,n,a,k,m,stack,nstk,endpt,end,x,nv,y)
!
!     Following subroutine from:
!
!     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!     ALGORITHMS', ACADEMIC PRESS, 1975.
!
!CCCC P.263
!
!     THIS SUBROUTINE IS USED IN COMPUTING FOR MINIMAL
!     SPANNING TREES.
!
      integer a,comp,e,end,endpt,stack,x,y
      dimension a(n),stack(nstk),endpt(2,e),end(2,n),nv(n),y(n),x(n)
!
      if(k.eq.1) then
!
        n2=e-n+1
        do 21  i=1,n2
          stack(i)=i
21      continue
        m=n2+1
        stack(m)=n2
        return
      endif
!
      k1=k-1
      do 31  i=1,k1
         i3=a(i)
         end(1,i)=endpt(1,i3)
         end(2,i)=endpt(2,i3)
31    continue
      n3=n+1
      call spanfo(n3,k1,end,comp,x,nv,y)
      i1=a(k1)+1
      i2=e-n+k
      m1=m
!
      do 35  i=i1,i2
         i3=endpt(1,i)
         i4=endpt(2,i)
         if(x(i3).eq.x(i4)) go to 35
         m1=m1+1
         stack(m1)=i
35    continue
      stack(m1+1)=m1-m
      m=m1+1
      return
      end
      SUBROUTINE SPLITC(N,X,M,IN,II,JJ,S,IABV,NA,MAXA,IBEL,   &
        NB,MAXB)
!
!  DATAPLOT NOTE: THIS SUBROUTINE FROM ACM 523 (FOR COMPUTING
!                 THE 2D CONVEX HULL).  RENAMED FROM
!                 "SPLIT" TO "SPLITC" TO AVOID NAME CONFLICT.
!
! THIS SUBROUTINE TAKES THE M POINTS OF ARRAY X WHOSE
! SUBSCRIPTS ARE IN ARRAY IN AND PARTITIONS THEM BY THE
! LINE JOINING THE TWO POINTS IN ARRAY X WHOSE SUBSCRIPTS
! ARE II AND JJ. THE SUBSCRIPTS OF THE POINTS ABOVE THE
! LINE ARE PUT INTO ARRAY IABV, AND THE SUBSCRIPTS OF THE
! POINTS BELOW ARE PUT INTO ARRAY IBEL. NA AND NB ARE,
! RESPECTIVELY, THE NUMBER OF POINTS ABOVE THE LINE AND THE
! NUMBER BELOW. MAXA AND MAXB ARE THE SUBSCRIPTS FOR ARRAY
! X OF THE POINT FURTHEST ABOVE THE LINE AND THE POINT
! FURTHEST BELOW, RESPECTIVELY. IF EITHER SUBSET IS NULL
! THE CORRESPONDING SUBSCRIPT (MAXA OR MAXB) IS SET TO ZERO
! FORMAL PARAMETERS
! INPUT
! N    INTEGER           TOTAL NUMBER OF DATA POINTS
! X    REAL ARRAY (2,N)  (X,Y) CO-ORDINATES OF THE DATA
! M    INTEGER           NUMBER OF POINTS IN INPUT SUBSET
! IN   INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
!                        POINTS IN THE INPUT SUBSET
! II   INTEGER           SUBSCRIPT FOR ARRAY X OF ONE POINT
!                        ON THE PARTITIONING LINE
! JJ   INTEGER           SUBSCRIPT FOR ARRAY X OF ANOTHER
!                        POINT ON THE PARTITIONING LINE
! S    INTEGER           SWITCH TO DETERMINE OUTPUT. REFER
!                        TO COMMENTS BELOW
! OUTPUT
! IABV INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
!                        POINTS ABOVE THE PARTITIONING LINE
! NA   INTEGER           NUMBER OF ELEMENTS IN IABV
! MAXA INTEGER           SUBSCRIPT FOR ARRAY X OF POINT
!                        FURTHEST ABOVE THE LINE. SET TO
!                        ZERO IF NA IS ZERO
! IBEL INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
!                        POINTS BELOW THE PARTITIONING LINE
! NB   INTEGER           NUMBER OF ELEMENTS IN IBEL
! MAXB INTEGER           SUBSCRIPT FOR ARRAY X OF POINT
!                        FURTHEST BELOW THE LINE. SET TO
!                        ZERO IF NB IS ZERO
      DIMENSION X(2,N)
      DIMENSION IN(M),IABV(M),IBEL(M)
      INTEGER S
! IF S = 2 DONT SAVE IBEL,NB,MAXB.
! IF S =-2 DONT SAVE IABV,NA,MAXA.
! OTHERWISE SAVE EVERYTHING
! IF S IS POSITIVE THE ARRAY BEING PARTITIONED IS ABOVE
! THE INITIAL PARTITIONING LINE. IF IT IS NEGATIVE, THEN
! THE SET OF POINTS IS BELOW.
      LOGICAL T
      T=.FALSE.
! CHECK TO SEE IF THE LINE IS VERTICAL
      IF(X(1,JJ).NE.X(1,II))GO TO  1
      XT=X(1,II)
      DIR=SIGN(1.,X(2,JJ)-X(2,II))*SIGN(1.,FLOAT(S))
      T=.TRUE.
      GO TO  2
1     A=(X(2,JJ)-X(2,II))/(X(1,JJ)-X(1,II))
      B=X(2,II)-A*X(1,II)
2     UP=0.
      NA=0
      MAXA=0
      DOWN=0.
      NB=0
      MAXB=0
      DO 6 I=1,M
        IS=IN(I)
        IF(T)GO TO  3
        Z=X(2,IS)-A*X(1,IS)-B
        GO TO  4
3       Z=DIR*(X(1,IS)-XT)
4       IF(Z.LE.0.)GO TO  5
! THE POINT IS ABOVE THE LINE
        IF(S.EQ.-2)GO TO  6
        NA=NA+1
        IABV(NA)=IS
        IF(Z.LT.UP)GO TO  6
        UP=Z
        MAXA=NA
        GO TO  6
5       IF(S.EQ.2)GO TO  6
        IF(Z.GE.0.)GO TO  6
! THE POINT IS BELOW THE LINE
        NB=NB+1
        IBEL(NB)=IS
        IF(Z.GT.DOWN)GO TO  6
        DOWN=Z
        MAXB=NB
6     CONTINUE
      RETURN
      END SUBROUTINE SPLITC
      subroutine triang(n,zeta,sig)
!
!   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL
!              ALGORITHMS', SECOND EDITION, ACADEMIC PRESS, 1978,
!              CH. 25, P. 230.
!=====================================================================
!
      integer q,r,sig,t,zeta
      dimension sig(n), zeta(n,n)
!
      m=0
      l=0
      do 11 i=1,n
         sig(i)=0
11    continue
!
20    continue
      m=m+1
!
      if (sig(m).eq.0) go to 40
!
130   continue
      if (m.eq.n) return
      go to 20
!
40    continue
      t=m+1
      r=t
60    continue
      if (r.gt.n) then
         l=l+1
         q=sig(m)
         sig(m)=l
         if (q.eq.0) go to 130
         r=m+1
         m=q
         go to 60
      endif
!
      if (sig(r).ne.0.or.zeta(r,m).eq.0) then
         r=r+1
      else
         sig(r)=m
         m=r
         r=t
      endif
      go to 60
!
      end
      SUBROUTINE YTBHOO (VAL,ROWID,Y,N,   &
                         HOOKLE,COLID,   &
                         IBUGA3,IERROR)
!
!     PURPOSE--COMPUTE THE HOOK LENGTH FOR THE VALUES IN A
!              YOUNG TABLEAUX.  FOR EACH ENTRY IN THE TABLEAUX,
!              THE HOOK LENGTH IS THE SUM OF THE NUMBER OF
!              ENTRIES TO THE RIGHT AND ON THE SAME ROW, THE
!              NUMBER OF ENTRIES BELOW AND IN THE SAME COLUMN,
!              AND 1 (FOR THE ENTRY ITSELF).  THIS COMMAND
!              ASSUMES THE YOUNG TABLEAUX IS IN THE FORM GIVEN
!              BY THE "CONVERT YOUNG TABLEAUX" COMMAND (I.E.,
!              COLUMN ONE IS THE VALUE AND COLUMN TWO IS THE
!              ROWID).
!     INPUT  ARGUMENTS--VAL    INTEGER ARRAY CONTAINING THE VALUE
!                              OF THE I-TH ENTRY IN THE YOUNG
!                              TABLEAUX.
!                     --ROWID  INTEGER ARRAY CONTAINING THE ROW ID
!                              OF THE I-TH ENTRY IN THE YOUNG
!                              TABLEAUX.
!                     --N      AN INTEGER SCALAR CONTAINING THE
!                              NUMBER OF ELEMENTS IN THE YOUNG
!                              TABLEAUX.
!     OUTPUT ARGUMENTS--Y      AN INTEGER ARRAY CONTAINING THE
!                              HOOK LENGTH.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--2008/8
!     ORIGINAL VERSION--AUGUST   2008
!     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
!                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
!                ACADEMIC PRESS, 1978, CHAPTER 14.
!
      INTEGER Y(*)
      INTEGER VAL(*)
      INTEGER ROWID(*)
      INTEGER HOOKLE(*)
      INTEGER COLID(*)
      INTEGER N
!
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,11)N
   11   FORMAT('FROM YTBHOO: N = ',I8)
        CALL DPWRST('XXX','WRIT')
        DO 20 I=1,N
          WRITE(ICOUT,21)I,VAL(I),ROWID(I),Y(I)
   21     FORMAT('I,VAL(I),ROWID(I),Y(I) = ',I8,3G15.7)
          CALL DPWRST('XXX','WRIT')
   20   CONTINUE
      ENDIF
!
      DO 100 I=1,N
        IF(ROWID(I).LT.1 .OR. ROWID(I).GT.N)THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,111)
  111     FORMAT('***** ERROR IN YOUNG TABLEAUX HOOK LENGTH--')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,113)
  113     FORMAT('      A ROW ID IN INPUT YOUNG TABLEAUX IS OUT OF ',   &
               'RANGE.')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,115)N
  115     FORMAT('      NUMBER OF ELEMENTS IN YOUNG TABLEAUX    = ',   &
                 I8)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,116)I,ROWID(I)
  116     FORMAT('      ROW ',I8,' HAS ROW ID = ',I10)
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GO TO 9999
        ENDIF
        IF(VAL(I).LT.1 .OR. VAL(I).GT.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,123)
  123     FORMAT('      A VALUE FOR THE INPUT YOUNG TABLEAUX IS ',   &
               'OUT OF RANGE.')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,125)N
  125     FORMAT('      NUMBER OF ELEMENTS IN YOUNG TABLEAUX    = ',   &
                 I8)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,126)I,VAL(I)
  126     FORMAT('      ROW ',I8,' HAS VALUE = ',I10)
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GO TO 9999
        ENDIF
  100 CONTINUE
!
      ICOL=0
      IROW=0
!
      DO 200 I=1,N
        IF(ROWID(I).NE.IROW)THEN
          ICOL=1
          IROW=ROWID(I)
        ELSE
          ICOL=ICOL+1
        ENDIF
        COLID(I)=ICOL
  200 CONTINUE
!
      HOOKLE(N)=1
!
      DO 300 I=1,N-1
        IROW=ROWID(I)
        ICOL=COLID(I)
        ISUM1=0
        ISUM2=0
!
        DO 400 J=I+1,N
          IF(ROWID(J).EQ.ROWID(I))THEN
            ISUM1=ISUM1+1
          ELSE
            GO TO 409
          ENDIF
  400   CONTINUE
  409   CONTINUE
!
        DO 410 J=I+1,N
          IF(COLID(J).EQ.COLID(I))THEN
            ISUM2=ISUM2+1
          ENDIF
  410   CONTINUE
        HOOKLE(I)=ISUM1+ISUM2+1
!
  300 CONTINUE
!
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,901)
  901   FORMAT('AT END OF YTBHLE')
        CALL DPWRST('XXX','WRIT')
        DO 910 I=1,MIN(100,N)
          WRITE(ICOUT,911)
  911     FORMAT('I,ROWID(I),COLID(I),HOOKLE(I)=',I5,3I8)
          CALL DPWRST('XXX','WRIT')
  910   CONTINUE
      ENDIF
!
 9999 CONTINUE
      RETURN
      END SUBROUTINE YTBHOO 

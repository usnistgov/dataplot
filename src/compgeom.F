C     THIS FILE CONTAINS ROUTINES FOR BASIC COMPUTATIONAL
C     GEOMETRY.  THIS COMBINES CODES FROM A NUMBER OF SEPARATE
C     SOURCES:
C
C        1. ACM 523  - FOR 2D CONVEX HULL
C        2. JAVIER BERNAL'S CODES FOR DELAUNAY TRIANGULARIZATION
C           AND VORONI DIAGRAMS.
C
C           THIS IS FOR POSSIBLE FUTURE DEVELOPMENT, NOTHING
C           CURRENTLY ACTIVE WITHIN DATAPLOT.
C
C        3. NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C           ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19 
C
C           a. MATRIX PERMANENT
C           b. MINIMUM SPANNING TREE
C           c. NEXT PERMUTATION
C
      SUBROUTINE CONVEX(N,X,M,IN,IA,IB,IH,NH,IL)
C
C     DATAPLOT NOTE: FROM ACM 523 (FOR FINDING THE 2D
C                    CONVEX HULL).  RENAME THE "SPLIT"
C                    ROUTINE TO "SPLITC" TO AVOID NAME
C                    CONFLICT.
C
C THIS SUBROUTINE DETERMINES WHICH OF THE M POINTS OF ARRAY
C X WHOSE SUBSCRIPTS ARE IN ARRAY IN ARE VERTICES OF THE
C MINIMUM AREA CONVEX POLYGON CONTAINING THE M POINTS. THE
C SUBSCRIPTS OF THE VERTICES ARE PLACED IN ARRAY IH IN THE
C ORDER THEY ARE FOUND. NH IS THE NUMBER OF ELEMENTS IN
C ARRAY IH AND ARRAY IL. ARRAY IL IS A LINKED LIST GIVING
C THE ORDER OF THE ELEMENTS OF ARRAY IH IN A COUNTER
C CLOCKWISE DIRECTION. THIS ALGORITHM CORRESPONDS TO A
C PREORDER TRAVERSAL OF A CERTAIN BINARY TREE. EACH VERTEX
C OF THE BINARY TREE REPRESENTS A SUBSET OF THE M POINTS.
C AT EACH STEP THE SUBSET OF POINTS CORRESPONDING TO THE
C CURRENT VERTEX OF THE TREE IS PARTITIONED BY A LINE
C JOINING TWO VERTICES OF THE CONVEX POLYGON. THE LEFT SON
C VERTEX IN THE BINARY TREE REPRESENTS THE SUBSET OF POINTS
C ABOVE THE PARTITIONING LINE AND THE RIGHT SON VERTEX, THE
C SUBSET BELOW THE LINE. THE LEAVES OF THE TREE REPRESENT
C EITHER NULL SUBSETS OR SUBSETS INSIDE A TRIANGLE WHOSE
C VERTICES COINCIDE WITH VERTICES OF THE CONVEX POLYGON.
C FORMAL PARAMETERS
C INPUT
C N  INTEGER           TOTAL NUMBER OF DATA POINTS
C X  REAL ARRAY (2,N)  (X,Y) CO-ORDINATES OF THE DATA
C M  INTEGER           NUMBER OF POINTS IN THE INPUT SUBSET
C IN INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE POINTS
C                      IN THE INPUT SUBSET
C WORK AREA
C IA INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF LEFT SON
C                      SUBSETS. SEE COMMENTS AFTER DIMENSION
C                      STATEMENTS
C IB INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF RIGHT SON
C                      SUBSETS
C OUTPUT
C IH INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
C                      VERTICES OF THE CONVEX HULL
C NH INTEGER           NUMBER OF ELEMENTS IN ARRAY IH AND
C                      ARRAY IL. SAME AS NUMBER OF VERTICES
C                      OF THE CONVEX POLYGON
C IL INTEGER ARRAY (M) A LINKED LIST GIVING IN ORDER IN A
C                      COUNTER-CLOCKWISE DIRECTION THE
C                      ELEMENTS OF ARRAY IH
      DIMENSION X(2,N)
      DIMENSION IN(M),IA(M),IB(M),IH(M),IL(M)
C THE UPPER END OF ARRAY IA IS USED TO STORE TEMPORARILY
C THE SIZES OF THE SUBSETS WHICH CORRESPOND TO RIGHT SON
C VERTICES, WHILE TRAVERSING DOWN THE LEFT SONS WHEN ON THE
C LEFT HALF OF THE TREE, AND TO STORE THE SIZES OF THE LEFT
C SONS WHILE TRAVERSING THE RIGHT SONS(DOWN THE RIGHT HALF)
      LOGICAL MAXE,MINE
      IF(M.EQ.1)GOTO 22
      IL(1)=2
      IL(2)=1
      KN=IN(1)
      KX=IN(2)
      IF(M.EQ.2)GOTO 21
      MP1=M+1
      MIN=1
      MX=1
      KX=IN(1)
      MAXE=.FALSE.
      MINE=.FALSE.
C FIND TWO VERTICES OF THE CONVEX HULL FOR THE INITIAL
C PARTITION
      DO 6 I=2,M
        J=IN(I)
CCCCC   IF(X(1,J)-X(1,KX))3,1,2
        IF(X(1,J)-X(1,KX).EQ.0.)THEN
          MAXE=.TRUE.
        ELSEIF(X(1,J)-X(1,KX).GT.0.)THEN
          MAXE=.FALSE.
          MX=I
          KX=J
        ENDIF
CCCCC   IF(X(1,J)-X(1,KN))5,4,6
        IF(X(1,J)-X(1,KN).EQ.0.)THEN
          MINE=.TRUE.
        ELSEIF(X(1,J)-X(1,KN).LT.0.)THEN
          MINE=.FALSE.
          MIN=I
          KN=J
        ENDIF
6     CONTINUE
C IF THE MAX AND MIN ARE EQUAL, ALL M POINTS LIE ON A
C VERTICAL LINE
      IF(KX.EQ.KN)GOTO 18
C IF MAXE (OR MINE) HAS THE VALUE TRUE THERE ARE SEVERAL
C MAXIMA (OR MINIMA) WITH EQUAL FIRST COORDINATES
      IF(MAXE.OR.MINE)GOTO 23
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
C BEGIN BY PARTITIONING THE ROOT OF THE TREE
      CALL SPLITC(N,X,MM,IN,IH(1),IH(2),0,IA,MB,MXA,IB,IA(MA),
     1  MXBB)
C FIRST TRAVERSE THE LEFT HALF OF THE TREE
C START WITH THE LEFT SON
8     NIB=NIB+IA(MA)
      MA=MA-1
9     IF(MXA.EQ.0)GOTO 11
      IL(NH)=IL(INH)
      IL(INH)=NH
      IH(NH)=IA(MXA)
      IA(MXA)=IA(MB)
      MB=MB-1
      NH=NH+1
      IF(MB.EQ.0)GOTO 10
      ILINH=IL(INH)
      CALL SPLITC(N,X,MB,IA,IH(INH),IH(ILINH),1,IA,MBB,MXA,
     1  IB(NIB),IA(MA),MXB)
      MB=MBB
      GOTO 8
C THEN THE RIGHT SON
10    INH=IL(INH)
11    INH=IL(INH)
      MA=MA+1
      NIB=NIB-IA(MA)
      IF(MA.GE.M)GOTO 12
      IF(IA(MA).EQ.0)GOTO 11
      ILINH=IL(INH)
C ON THE LEFT SIDE OF THE TREE, THE RIGHT SON OF A RIGHT SON
C MUST REPRESENT A SUBSET OF POINTS WHICH IS INSIDE A
C TRIANGLE WITH VERTICES WHICH ARE ALSO VERTICES OF THE
C CONVEX POLYGON AND HENCE THE SUBSET MAY BE NEGLECTED.
      CALL SPLITC(N,X,IA(MA),IB(NIB),IH(INH),IH(ILINH),2,IA,
     1  MB,MXA,IB(NIB),MBB,MXB)
      IA(MA)=MBB
      GOTO 9
C NOW TRAVERSE THE RIGHT HALF OF THE TREE
12    MXB=MXBB
      MA=M
      MB=IA(MA)
      NIA=1
      IA(MA)=0
C START WITH THE RIGHT SON
13    NIA=NIA+IA(MA)
      MA=MA-1
14    IF(MXB.EQ.0)GOTO 16
      IL(NH)=IL(INH)
      IL(INH)=NH
      IH(NH)=IB(MXB)
      IB(MXB)=IB(MB)
      MB=MB-1
      NH=NH+1
      IF(MB.EQ.0)GOTO 15
      ILINH=IL(INH)
      CALL SPLITC(N,X,MB,IB(NIB),IH(INH),IH(ILINH),-1,IA(NIA),
     1  IA(MA),MXA,IB(NIB),MBB,MXB)
      MB=MBB
      GOTO 13
C THEN THE LEFT SON
15    INH=IL(INH)
16    INH=IL(INH)
      MA=MA+1
      NIA=NIA-IA(MA)
      IF(MA.EQ.MP1)GOTO 17
      IF(IA(MA).EQ.0)GOTO 16
      ILINH=IL(INH)
C ON THE RIGHT SIDE OF THE TREE, THE LEFT SON OF A LEFT SON
C MUST REPRESENT A SUBSET OF POINTS WHICH IS INSIDE A
C TRIANGLE WITH VERTICES WHICH ARE ALSO VERTICES OF THE
C CONVEX POLYGON AND HENCE THE SUBSET MAY BE NEGLECTED.
      CALL SPLITC(N,X,IA(MA),IA(NIA),IH(INH),IH(ILINH),-2,
     1  IA(NIA),MBB,MXA,IB(NIB),MB,MXB)
      GOTO 14
17    NH=NH-1
      RETURN
C ALL THE SPECIAL CASES ARE HANDLED DOWN HERE
C IF ALL THE POINTS LIE ON A VERTICAL LINE
18    KX=IN(1)
      KN=IN(1)
      DO 20 I=1,M
        J=IN(I)
        IF(X(2,J).LE.X(2,KX))GOTO 19
        MX=I
        KX=J
19      IF(X(2,J).GE.X(2,KN))GOTO 20
        MIN=I
        KN=J
20    CONTINUE
      IF(KX.EQ.KN)GOTO 22
C IF THERE ARE ONLY TWO POINTS
21    IH(1)=KX
      IH(2)=KN
      NH=3
      IF((X(1,KN).EQ.X(1,KX)).AND.(X(2,KN).EQ.X(2,KX)))NH=2
      GOTO 17
C IF THERE IS ONLY ONE POINT
22    NH=2
      IH(1)=IN(1)
      IL(1)=1
      GOTO 17
C MULTIPLE EXTREMES ARE HANDLED HERE
C IF THERE ARE SEVERAL POINTS WITH THE (SAME) LARGEST
C FIRST COORDINATE
23    IF(.NOT.MAXE)GOTO 25
      DO 24 I=1,M
        J=IN(I)
        IF(X(1,J).NE.X(1,KX))GOTO 24
        IF(X(2,J).LE.X(2,KX))GOTO 24
        MX=I
        KX=J
24    CONTINUE
C IF THERE ARE SEVERAL POINTS WITH THE (SAME) SMALLEST
C FIRST COORDINATE
25    IF(.NOT.MINE)GOTO 7
      DO 26 I=1,M
        J=IN(I)
        IF(X(1,J).NE.X(1,KN))GOTO 26
        IF(X(2,J).GE.X(2,KN))GOTO 26
        MIN=I
        KN=J
26    CONTINUE
      GOTO 7
      END
      SUBROUTINE CYCLES (SIGMA, N, SIGN, NCYCL, OPTION)
C
C     COUNT CYCLES, FIND SIGNS OF PERMUTATIONS, TAG AND/OR
C     INVERT.
C
C     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
C                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
C                ACADEMIC PRESS, 1978, CHAPTER 16.
C
      INTEGER SIGMA(N), SIGN, OPTION
C
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
C
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
C
      RETURN
      END
      SUBROUTINE CONYTB (Y, VAL, ROWID, N,
     1                   TEMP1,GROUP,
     1                   IBUGA3,IERROR)
C
C     PURPOSE--CONVERT THE OUTPUT FROM THE NEXT YOUNG TABLEAUX
C              OR RANDOM YOUNG TABLEAUX COMMANDS TO A DIFFERENT
C              FORMAT.
C
C              THESE COMMANDS RETURN THE DATA IN THE FORM WHERE
C              THE I-TH ELEMENT OF Y IDENTIFIES THE ROW THAT
C              CONTAINS THE VALUE I.  THIS FORM IS USED BECAUSE
C              IT IS CONVENIENT FOR COMPUTER PROCESSING.
C              HOWEVER, IT MAY BE CONVENIENT TO VIEW THE
C              TABLE AS A "ROWID" AND "VALUE" SINCE THIS MAKES
C              IT EASIER TO VISUALIZE THE TABLEAUX.  THIS ROUTINES
C              CONVERTS Y TO "ROWID" AND "VALUE".
C     INPUT  ARGUMENTS--Y      THE YUOUNG TABLEAUX IN THE FORMAT
C                              GENERATED BY THE "NEXT YOUNG TABLEAUX"
C                              AND THE "RANDOM YOUNG TABLEAUX"
C                              COMMAND.  INTEGER ARRAY.
C                     --N      AN INTEGER SCALAR CONTAINING THE
C                              NUMBER OF ELEMENTS IN THE YOUNG
C                              TABLEAUX.
C     OUTPUT ARGUMENTS--VAL    INTEGER ARRAY CONTAINING THE VALUE
C                              OF THE I-TH ENTRY IN THE YOUNG
C                              TABLEAUX.
C                     --ROWID  INTEGER ARRAY CONTAINING THE ROW ID
C                              OF THE I-TH ENTRY IN THE YOUNG
C                              TABLEAUX.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/8
C     ORIGINAL VERSION--AUGUST   2008
C     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
C                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
C                ACADEMIC PRESS, 1978, CHAPTER 14.
C
      INTEGER Y(*)
      INTEGER VAL(*)
      INTEGER ROWID(*)
      INTEGER N
C
      REAL TEMP1(*)
      REAL GROUP(*)
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
C
      REAL CPUMIN
      REAL CPUMAX
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,11)N
   11   FORMAT('FROM CONYTB: N = ',I8)
        CALL DPWRST('XXX','WRIT')
        DO20I=1,N
          WRITE(ICOUT,21)I,Y(I)
   21     FORMAT('I,Y(I) = ',I8,G15.7)
          CALL DPWRST('XXX','WRIT')
   20   CONTINUE
      ENDIF
C
C     STEP 1--DETERMINE THE NUMBER OF DISTINCT ROWS IN Y
C
      IWRITE='OFF'
      DO100I=1,N
        TEMP1(I)=REAL(Y(I))
  100 CONTINUE
      CALL DISTIN(TEMP1,N,IWRITE,GROUP,NUMDIS,IBUGA3,IERROR)
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,101)NUMDIS
  101   FORMAT('AFTER CALL DISTIN: NUMDIS=',I8)
        CALL DPWRST('XXX','WRIT')
      ENDIF
C
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
  113   FORMAT('      A ROW ID IN INPUT YOUNG TABLEAUX IS OUT OF ',
     1         'RANGE.')
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
        GOTO9999
      ENDIF
C
C     STEP 2--LOOP THROUGH DISTINCT ROW ID'S.
C
      ICNT=0
      DO200I=1,NUMDIS
        IVAL=INT(GROUP(I) + 0.01)
C
        IF(IBUGA3.EQ.'ON')THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,201)I,IVAL
  201     FORMAT('I,IVAL = ',2I8)
          CALL DPWRST('XXX','WRIT')
        ENDIF
C
        DO300J=1,N
          IF(Y(J).EQ.IVAL)THEN
            ICNT=ICNT+1
            ROWID(ICNT)=Y(J)
            VAL(ICNT)=J
          ENDIF
  300   CONTINUE
  200 CONTINUE
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE EXHEAP (N, INDEX, I, J, ISGN)
C
C   SORT A LIST OF ITEMS INTO INTEGER ORDER.
C   NOTE THAT THIS ROUTINE HAS THE CALLING ROUTINE PERFORM
C   THE SWAP.  IT IS BASICALLY AN INTERNAL ROUTINE USED BY
C   OTHER NIJENHUIS AND WILF ROUTINES (I.E., DATAPLOT DOES
C   NOT CALL THIS ROUTINE DIRECTLY).
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', SECOND EDITION, ACADEMIC PRESS, 1978,
C              CH. 18, P. 141-142. 
C=====================================================================
C
      IF (INDEX.LT.0)THEN
         IF (INDEX .EQ. -1) THEN
            IF (ISGN .LE. 0) GO TO 70
            INDEX=2
            GOTO9000
         ENDIF
         IF (ISGN .LT. 0) I=I+1
         J=L1
         L1=I
         INDEX=-1
         GOTO9000
C
      ELSEIF (INDEX.EQ.0) THEN
         N1=N
         L=1+N/2
         GOTO20
      ELSE
         IF (INDEX-1.LE.0) GO TO 30
         GO TO 40
      ENDIF
C
20    CONTINUE
      L=L-1
C
30    CONTINUE
      L1=L
C
40    CONTINUE
      I=L1+L1
      IF (I-N1.LT.0) THEN
         J=I+1
         INDEX=-2
         GOTO9000
      ELSEIF (I-N1.EQ.0) THEN
         J=L1
         L1=I
         INDEX=-1
         GOTO9000
      ENDIF
C
70    CONTINUE
      IF (L .GT. 1) GO TO 20
      IF (N1 .EQ. 1) THEN
         INDEX=0
         GOTO9000
      ENDIF
      I=N1
      N1=N1-1
      J=1
      INDEX=1
      GOTO9000
C
 9000 CONTINUE
      RETURN
      END
      subroutine inver2 (n,a,ainv)
      integer a,ainv,sum
      dimension a(n,n),ainv(n,n)
C
C   RENAME TO AVOID NAME CONFLICT WITH A ROUTINE USED IN
C   "CLUSTER.FOR".
C
C   INVERT AN INTEGER UPPER TRIANGULAR MATRIX.
C   IT IS BASICALLY AN INTERNAL ROUTINE USED BY
C   OTHER NIJENHUIS AND WILF ROUTINES (I.E., DATAPLOT DOES
C   NOT CALL THIS ROUTINE DIRECTLY).
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', SECOND EDITION, ACADEMIC PRESS, 1978,
C              CH. 18, P. 141-142. 
C=====================================================================
C
      j=n
c
10    continue
      i=n
20    continue
      sum=0
      if (i.eq.j) sum=1
      k=i+1
c
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
c
      return
      end
      subroutine minspt(dist,maxrow,n,endpt1,endpt2,u,y)
C
C   PURPOSE: COMPUTE MININUM SPANNING TREE OF A DISTANCE MATRIX.
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, P. 285.
C
C   DIST      - DISTANCE MATRIX (INPUT)
C   N         - NUMBER OF POINTS IN THE DISTANCE MATRIX
C   ENDPT     - 2-D VECTOR DEFINING EDGES OF THE MINIMAL
C               SPANNING TREE (OUTPUT)
C               (FOR DATAPLOT, SPLIT INTO 2 SEPARATE VECTORS)
C   U         - WORKING STORAGE (INTEGER)
C   Y         - WORKING STORAGE (REAL)
C=======================================================================
C
      integer u(*),endpt1(*),endpt2(*)
      dimension y(*),dist(maxrow,*)
c
      l=0
      do 21  i=2,n
         u(i)=1
         y(i)=dist(1,i)
21    continue
c
30    continue
      dmin=1.e37
      do 41  i=2,n
         if(y(i).le.0.0) go to 41
         if(y(i).ge.dmin) go to 41
         dmin=y(i)
         imin=i
41    continue
c
      l=l+1
      endpt1(l)=imin
      endpt2(l)=u(imin)
c
      if(l.eq.n-1) goto999
      y(imin)=0
c
      do 111  i=2,n
         if(y(i).eq.0.0) go to 111
         d1=dist(i,imin)
         if(y(i).le.d1) go to 111
         u(i)=imin
         y(i)=d1
111   continue
      go to 30
c
999   continue
      return
      end
      SUBROUTINE NEXCOM(N,K,R,MTC)
C
C   GENERATE NEXT COMPOSITION OF N INTO K PARTS.
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, CH. x, P. 49. 
C=====================================================================
C
      INTEGER R(K),T,H
      LOGICAL MTC
C
      COMMON/NIJWIL/NLAST,KLAST
C
      SAVE H, T
C
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
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE NEXEQU(N,NC,P,Q,MTC)
C
C     NEXT PARTITION OF AN N-SET.
C   GENERATE NEXT PARTITION OF AN N-SET.
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 11, P. 91. 
C=====================================================================
C
      LOGICAL MTC
      INTEGER P(N),Q(N)
C
      SAVE NCLAST
C
      COMMON/NIJWIL/NLAST,KLAST
C
      NC=NCLAST
C
      IF(.NOT.MTC)THEN
C
C       FIRST IN SEQUENCE
C
        NLAST=1
        NC=1
        DO 11 I=1,N
          Q(I)=1
   11   CONTINUE
        P(1)=N
C
      ELSE
C
C       CONTINUE PREVIOUS SEQUENCE
C
        M=N
   30   CONTINUE
        L=Q(M)
        IF(P(L).EQ.1)THEN
          Q(M)=1
          M=M-1
          GOTO 30
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
C
      MTC=NC.NE.N
      NCLAST=NC
C
      RETURN
      END
      SUBROUTINE NEXKSB(N,K,A,MTC)
C
C   GENERATE NEXT "k-SET OF AN N-SET".
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 3, P. 28. 
C=======================================================================
C
      LOGICAL MTC
      INTEGER A(*)
      INTEGER H
C
      COMMON/NIJWIL/NLAST,KLAST
C
      SAVE H
C
      IF(MTC .AND. K.EQ.KLAST .AND. N.EQ.NLAST) THEN
        DO 41  H=1,K
           I=K+1-H
           M2=A(I)
           IF(M2.NE.N+1-H) GOTO 50
41      CONTINUE
      ELSE
        M2=0
        H=K
        NLAST=N
        KLAST=K
        MTC=.TRUE.
      ENDIF
C
50    CONTINUE
      DO 51 J=1,H
         I=K+J-H
         A(I)=M2+J
   51 CONTINUE
      MTC=(A(1).NE.N-K+1)
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE NEXPAR(N,R,M,D,MTC)
C
C   GENERATE NEXT PARTITION OF N.
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 9, P. 69. 
C=====================================================================
C
      IMPLICIT INTEGER(A-Z)
      LOGICAL MTC
      DIMENSION M(*),R(*)
C
      COMMON/NIJWIL/NLAST,KLAST
C
      IF(N.NE.NLAST .OR. (.NOT.MTC)) THEN
C
C       NEW SEQUENCE
C
        IF(N.NE.NLAST)NLAST=N
        S=N
        D=0
        D=D+1
        R(D)=S
        M(D)=1
        MTC=M(D).NE.N
      ELSE
C
C       OLD SEQUENCE
C
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
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE NEXPER (RED,PINK,BROWN)
C-----------------------------------------------------------------------
C   NEXPER   COPIED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
C            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
C            MARYLAND 20899 FROM THE REFERENCE BELOW (COLOR ADDED)
C
C   FOR: COMPUTING THE NEXT PERMUTATION OF THE INTEGERS 1, 2, ..., N. 
C        THE CALLING SEQUENCE IS
C
C                        CALL NEXPER (IPERM,N,LL) 
C
C        WHERE IPERM IS AN INTEGER VECTOR DIMENSIONED AT LEAST N AND
C        LL IS A LOGICAL VARIABLE.  NONE OF THESE PASSED PARAMETERS
C        NEEDS TO BE DEFINED ON INPUT.  ON OUTPUT IPERM CONTAINS THE
C        CURRENT PERMUTATION OF THE FIRST N INTEGERS AND LL IS .TRUE. 
C        UNLESS THIS PERMUTATION IS THE LAST PERMUTATION OF THE CYCLE 
C        (IN WHICH CASE LL IS .FALSE.). 
C
C   SUBPROGRAMS CALLED: -NONE-
C
C   CURRENT VERSION COMPLETED JANUARY 20, 1987
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, PP. 49-59.
C-----------------------------------------------------------------------
      IMPLICIT INTEGER (A-Z)
      LOGICAL BROWN 
      DIMENSION RED(*)
      DATA MAROON / 0 /
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
      END 
      SUBROUTINE NEXSUB (N,IWORK,MTC,NCARD,J)
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19. 
C=======================================================================
C
      LOGICAL MTC
      DIMENSION IWORK(*)
      COMMON/NIJWIL/NLAST,KLAST
C
      SAVE M
C
      DATA NLAST / 0 /
C
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
      END 
      SUBROUTINE NEXYTB (N, LAMBDA, Y, MTC)
C
C     PURPOSE: SUPPLIES THE SEQUENCE OF YOUNG
C              TABLEAUX OF GIVEN SHAPE.
C
C     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
C                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
C                ACADEMIC PRESS, 1978, CHAPTER 14.
C
      LOGICAL MTC
      INTEGER LAMBDA(N), Y(N), R, S, S1, T
C
      T=N
      IF (.NOT. MTC) GO TO 40
      LAMBDA(1)=1
C
      DO 21 I=2, N
         LAMBDA(I)=0
21    CONTINUE
C
      DO 22 J=2,N
         LAMBDA(Y(J))=LAMBDA(Y(J))+1
         IF (Y(J) .LT. Y(J-1)) GO TO 30
22    CONTINUE
C
      MTC=.FALSE.
      GOTO9999
C
30    CONTINUE
      T=LAMBDA(1+Y(J))
      I=N
C
31    CONTINUE
      IF (LAMBDA(I) .EQ. T) GO TO 32
      I=I-1
      GO TO 31
C
32    CONTINUE
      Y(J)=I
      LAMBDA(I)=LAMBDA(I)-1
      T=J-1
C
40    CONTINUE
      L=1
C
43    CONTINUE
      R=1
C
42    CONTINUE
      IF (R .LE. N) THEN
         IF (LAMBDA(R) .EQ. 0) THEN
            IF (L .LE. T) THEN
               GO TO 43
            ELSE
               GOTO45
            ENDIF
         ENDIF
         Y(L)=R
         LAMBDA(R)=LAMBDA(R)-1
         L=L+1
         R=R+1
         GO TO 42
      ENDIF
C
45    CONTINUE
      IF (N .EQ. 1) THEN
         MTC=.FALSE.
         GOTO9999
      ELSE
         DO 46 J=2,N
            IF (Y(J) .LT. Y(J-1)) GO TO 50
46       CONTINUE
      ENDIF
C
50    CONTINUE
      MTC=.TRUE.
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE PERMAN (A,LDA,N,IWORK,WORK,APERM)
C
C-----------------------------------------------------------------------
C   PERMAN   COPIED BY CHARLES P. REEVE, STATISTICAL ENGINEERING
C            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
C            MARYLAND 20899 FROM THE REFERENCE BELOW
C
C   FOR: COMPUTING THE PERMANENT OF THE N BY N MATRIX A.  THE PERMANENT
C        OF A MATRIX IS SIMILAR TO THE DETERMINANT EXCEPT THAT THE
C        ALTERNATING SIGN CHANGES FOR THE TERMS ARE EXCLUDED.  FOR
C        EXAMPLE, GIVEN THE MATRIX
C
C                               [A B C] 
C                               [D E F] 
C                               [G H I] 
C
C        THE DETERMINANT IS  AEI+BFG+CDH-CEG-BDI-AFH  WHEREAS THE
C        PERMANENT IS  AEI+BFG+CDH+CEG+BDI+AFH .
C
C   NOTE: THE COMPUTING TIME IS PROPORTIONAL TO 2**N, AND ON THE
C         CYBER 180/855 AT NBS A VALUE OF N=20 REQUIRES ABOUT 30
C         SECONDS OF CPU TIME.
C
C   SUBPROGRAMS CALLED: NEXSUB (ATTACHED)
C
C   CURRENT VERSION COMPLETED JANUARY 20, 1987
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19. 
C-----------------------------------------------------------------------
C   DEFINITION OF PASSED PARAMETERS: 
C
C     * A = MATRIX (SIZE NXN) WHOSE PERMANENT IS TO BE COMPUTED (REAL)
C
C   * LDA = THE LEADING DIMENSION OF MATRIX A [LDA>=N] (INTEGER)
C
C     * N = THE NUMBER OF ROWS AND COLUMNS IN MATRIX A (INTEGER)
C
C   IWORK = VECTOR (LENGTH N) USED AS SCRATCH AREA (INTEGER)
C
C    WORK = VECTOR (LENGTH N) USED AS SCRATCH AREA (REAL)
C
C   APERM = THE PERMANENT OF MATRIX A (REAL)
C
C   * INDICATES PARAMETERS REQUIRING INPUT VALUES 
C-----------------------------------------------------------------------
      LOGICAL MTC
      DIMENSION A(LDA,*),IWORK(*),WORK(*)
C
      DOUBLE PRECISION P
      DOUBLE PRECISION Z
      DOUBLE PRECISION SUM
      DOUBLE PRECISION PROD
C
      COMMON/NIJWIL/NLAST,KLAST
C
      NLAST=0
C
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
      END 
      SUBROUTINE POLYNW(N,A,X0,OPTION,VAL,B)
C
C     PERFORM OPERATIONS ON POLYNOMIALS IN
C     POWER AND FACTORIAL FORM.
C
C     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C                ALGORITHMS', ACADEMIC PRESS, 1975, CH. 15, P. 175. 
C
C     N    = N-1 IS DEGREE OF INPUT POLYNOMIAL
C     A    = COEFFICIENTS OF INPUT POLYNOMIAL
C     X0   = CONSTANT IN TAYLOR EXPANSION
C     VAL  = VALUE OF f(X0) ON OUTPUT
C     B    = COEFFICENTS OF OUTPUT POLYNOMIAL
C
C     OPTION    >  0 => VALUE OF OPTION DEFINES NUMBER OF TERMS
C                       IN TAYLOR EXPANSION, B WILL CONTAIN THE
C                       COEFFICIENTS OF THE TAYLOR EXPANSION
C     OPTION    =  0 => RETURNS THE VALUE OF THE USUAL POLYNOMIAL
C                       AT X) IN VAL
C     OPTION    = -1 => RETURN IN VAL THE VALUE OF f(X) WITH
C                       A(1) ... A(N) CONSIDERED AS THE
C                       COEFFICIENTS IN THE FACTORIAL FORM.
C     OPTION    = -2 => STIRLING ALGORITHM
C     OPTION    = -3 => REVERSE STIRLING ALGORITHM
C
C     NOTE: NAME CHANGED FROM "POLY" TO "POLYNW" TO AVOID NAME
C           CONFLICT WITH ROUTINE ALREADY IN DATAPLOT.
C
      INTEGER A,B,OPTION,V,VAL,X0,Z
      DIMENSION A(N),B(N)
C
      VAL=A(N)
      IF (N.EQ.1) GOTO9000
C
      N1=N-1
      IF (OPTION.EQ.0) THEN
         DO 25 I=1,N1
            I1=N-I
            VAL=VAL*X0+A(I1)
25       CONTINUE
         GOTO9000
      ELSEIF (OPTION.EQ.(-1)) THEN
         DO 27 I=1,N1
            I1=N-I
            VAL=VAL*(X0-N1+I)+A(I1)
27       CONTINUE
         GOTO9000
      ENDIF
C
      DO 10 I=1,N
         B(I)=A(I)
10    CONTINUE
C
      IF (OPTION.LT.0) THEN
         IF (N.EQ.2) GOTO9000
         N2=N-2
         IF (OPTION.EQ.(-3)) THEN
            DO 75 J=1,N2
               Z=N1-J
               M=Z+1
80             CONTINUE
               B(M)=B(M)-Z*B(M+1)
               M=M+1
               IF (M.LE.N1) GOTO 80
75          CONTINUE
            GOTO 9000
         ENDIF
C
         DO 55 J=1,N2
            V=VAL
            M=N1
60          CONTINUE
            V=B(M)+J*V
            B(M)=V
            M=M-1
            IF (M.GT.J) GOTO 60
55       CONTINUE
         GOTO9000
      ELSE
C
         MAX=MIN0(N1,OPTION)
         DO 35 J=1,MAX
            M=N1
            V=VAL
37          CONTINUE
            V=B(M)+V*X0
            B(M)=V
            M=M-1
            IF (M.GE.J) GOTO 37
35       CONTINUE
         VAL=B(1)
         GOTO9000
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      subroutine ranequ(n,l,q,a,b,c,iseed,ytemp)
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', ACADEMIC PRESS, 1975, P. 97.
C
      integer q(n),a(n),c(n)
      dimension b(n), ytemp(n)
      dimension xjunk(1)
c
      EXTERNAL UNIRAN
c
      COMMON/NIJWIL/NLAST,KLAST
c
      b(1)=1
c
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
c
      do 11  i=1,n
         q(i)=0
   11 continue
      l=0
      m=n
c
   20 continue
ccccc z1=uni(1)
      ntemp=1
      call uniran(ntemp,iseed,xjunk)
      z1=xjunk(1)
      k=m-1
      t=1.0/float(m)
      m1=m-1
c
   60 continue
      if (k.eq.0) then
         l=l+1
         do 71  i=1,n
            if(q(i).eq.0)  q(i)=l
   71    continue
         goto9000
      endif
c
      z=t*b(k)/b(m)
      if (z1.ge.z) then
         k=k-1
         t=t/float(m-1-k)
         z1=z1-z
         go to 60
      endif
c
      l1=n
c
   81 continue
      if (q(l1).ne.0) then
         l1=l1-1
         go to 81
      endif
      l=l+1
      q(l1)=l
ccccc call ranksb(m1,k,a)
      call ranksb(k,m1,iseed,ytemp,a)
      m2=1
      i=1
c
   90 continue
      if (q(i).eq.0) then
         c(m2)=i
         m2=m2+1
         q(i)=l
      endif
c
  100 continue
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
c
 9000 continue
      return
      end
      SUBROUTINE RANYTB (N, LAM, Y, ISEED)
C
C     PURPOSE: SELECTS A YOUNG TABLEAUX OF GIVEN SHAPE.
C
C     INPUT ARGUMENTS:
C     N       = THE SCALAR INTEGER THAT IS PARTITIONED
C     LAM     = INTEGER ARRY THAT SPECIFES THE PARTITION
C     ISEED   = SEED FOR RANDOM NUMBER GENERATOR
C
C     OUTPUT ARGUMENTS:
C     Y       = INTEGER ARRAY CONTAINING THE OUTPUT TABLEAUX
C
C     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
C                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
C                ACADEMIC PRESS, 1978, CHAPTER 14.
C
      INTEGER LAM(N), Y(N), H
      REAL XJUNK(1)
C
      DO 5 I=1, N
         Y(I)=0
5     CONTINUE
C
      I=0
      L=0
10    CONTINUE
      I=I+1
      M=LAM(I)
      DO 20 J=1,M
         Y(J) = Y(J)+1
         L=L+1
20    CONTINUE
C
      IF (L .LT. N) GO TO 10
      NTEMP=1
      DO 85 M = 1,N
40       CONTINUE
         CALL UNIRAN(NTEMP,ISEED,XJUNK)
         I=1+XJUNK(1)*Y(1)
         CALL UNIRAN(NTEMP,ISEED,XJUNK)
         J=1+XJUNK(1)*LAM(1)
         IF (I .GT. Y(J) .OR. J .GT. LAM(I)) GO TO 40
70       CONTINUE
         H=Y(J)+LAM(I)-I-J
C
         IF (H .EQ. 0) THEN
            LAM(I)=LAM(I)-1
            Y(J)=Y(J)-1
            Y(N+1-M)=I
            GOTO85
         ENDIF
C
         CALL UNIRAN(NTEMP,ISEED,XJUNK)
         L=1+H*XJUNK(1)
         IF (L .GT. LAM(I)-J) THEN
            I=L-LAM(I)+1+J
         ELSE
           J=J+L
         ENDIF
         GO TO 70
C
85    CONTINUE
C
      DO 90 I=1, N
         LAM(Y(I))=LAM(Y(I))+1
90    CONTINUE
C
      RETURN
      END

      subroutine renumb(m,n,sig,tau,a)
C
C     Following subroutine from:
C
C     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C     ALGORITHMS', ACADEMIC PRESS, 1975, CH. 1,19. 
C
CCCCC P.155
C
C     THIS IS A UTILITY FOR MINIMUM SPANNING TREES
C
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
c
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
c
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
ccccc     t1=a(i1,j1)
ccccc     j1=iabs(tau(j1))
ccccc     t2=a(i1,j1)
ccccc     a(i1,j1)=t1
          j1=iabs(tau(j1))
          t2=a(i1,j1)
          a(i1,j1)=t1
          t1=t2
          if(j1.ne.j2) go to 50
          k=k-1
          if(i1.ne.i) go to 50
          j2=iabs(tau(j2))
55        continue
          if(k.ne.0) go to 40
30      continue
10    continue
c
      do 60  i=1,m
        sig(i)=iabs(sig(i))
60    continue
c
      if(tau(1).gt.0) return
      do 70  j=1,n
        tau(j)=iabs(tau(j))
70    continue
c
      return
      end
      subroutine renum2(m,n,sig,tau,a)
C
C     Following subroutine from:
C
C     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C     ALGORITHMS', ACADEMIC PRESS, 1975, P. 155.
C
C
C     THIS IS A COPY OF "renumb", BUT WITH A REAL RATHER
C     THAN AN INTEGER MATRIX.
C
      integer sig(m),tau(n)
      real t1, t2, a(m,n)
c
      do 5  i=1,m
      i1=sig(i)
6     if(i1.le.i) go to 5
      i2=sig(i1)
      sig(i1)=-i2
      i1=i2
      go to 6
5     sig(i)=-sig(i)
      if(tau(1).lt.0) go to 9
      do 7  j=1,n
      j1=tau(j)
8     if(j1.le.j) go to 7
      j2=tau(j1)
      tau(j1)=-j2
      j1=j2
      go to 8
7     tau(j)=-tau(j)
9     do 10  i=1,m
      i1=-sig(i)
      if(i1.lt.0) go to 10
      lc=0
20    i1=sig(i1)
      lc=lc+1
      if(i1.gt.0) go to 20
      i1=i
      do 30  j=1,n
      if(tau(j).gt.0) go to 30
      j2=j
      k=lc
40    j1=j2
      t1=a(i1,j1)
50    i1=iabs(sig(i1))
      t1=a(i1,j1)
      j1=iabs(tau(j1))
      t2=a(i1,j1)
      a(i1,j1)=t1
      t1=t2
      if(j1.ne.j2) go to 50
      k=k-1
      if(i1.ne.i) go to 50
      j2=iabs(tau(j2))
55    if(k.ne.0) go to 40
30    continue
10    continue
      do 60  i=1,m
60    sig(i)=iabs(sig(i))
      if(tau(1).gt.0) return
      do 70  j=1,n
70    tau(j)=iabs(tau(j))
      return
      end
      subroutine spanfo(n,e,endpt,k,x,nv,y)
C
C     Following subroutine from:
C
C     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C     ALGORITHMS', ACADEMIC PRESS, 1975.
C
CCCCC PP. 106-108.
C
C     THIS SUBROUTINE IS USED FOR:
C
C
C        1) DETERMINE THE CONNECTIVITY OF A GRAPH
C
C        2) FIND SPANNING FOREST
C
C        3) USED IN COMPUTING MINIMAL SPANNING TREES.
C
      integer e1,e,endpt,s,t1,t2,t,v1,v2,x,y,z
      dimension endpt(2,e),x(n),nv(n),y(n),s(2)
c
      m=2
      s(1)=1
      s(2)=2
c
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
c
25    continue
      t1=x(v1)
      if(t1.lt.0)  t1=v1
      t2=x(v2)
      if(t2.lt.0)  t2=v2
      if(t1.ne.t2) go to 40
      if(j.lt.e1)  go to 30
      e1=e1-1
      go to 60
c
30    continue
      endpt(1,j)=endpt(1,e1)
      endpt(2,j)=endpt(2,e1)
      endpt(1,e1)=v1
      endpt(2,e1)=v2
      e1=e1-1
      go to 20
c
40    continue
      if(nv(t1).le.nv(t2))  go to 50
      t=t1
      t1=t2
      t2=t
c
50    continue
      i3=-x(t2)
      y(i3)=t1
      x(t2)=x(t1)
      i=t1
c
55    continue
      x(i)=t2
      i=y(i)
      if(i.ne.0) go to 55
      nv(t2)=nv(t2)+nv(t1)
      nv(t1)=0
      j=j+1
      if(j.le.e1.and.j.lt.n)  go to 20
c
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
90    continue
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
c
      return
      end
      subroutine spntre(e,n,a,k,m,stack,nstk,endpt,end,x,nv,y)
C
C     Following subroutine from:
C
C     NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C     ALGORITHMS', ACADEMIC PRESS, 1975.
C
CCCCC P.263
C
C     THIS SUBROUTINE IS USED IN COMPUTING FOR MINIMAL
C     SPANNING TREES.
C
      integer a,comp,e,end,endpt,stack,x,y
      dimension a(n),stack(nstk),endpt(2,e),end(2,n),nv(n),y(n),x(n)
10    if(k.ne.1) go to 30
20    n2=e-n+1
      do 21  i=1,n2
21    stack(i)=i
      m=n2+1
      stack(m)=n2
      return
30    k1=k-1
      do 31  i=1,k1
      i3=a(i)
      end(1,i)=endpt(1,i3)
31    end(2,i)=endpt(2,i3)
      n3=n+1
      call spanfo(n3,k1,end,comp,x,nv,y)
      i1=a(k1)+1
      i2=e-n+k
      m1=m
32    do 35  i=i1,i2
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
      SUBROUTINE SPLITC(N,X,M,IN,II,JJ,S,IABV,NA,MAXA,IBEL,
     1  NB,MAXB)
C
C  DATAPLOT NOTE: THIS SUBROUTINE FROM ACM 523 (FOR COMPUTING
C                 THE 2D CONVEX HULL).  RENAMED FROM
C                 "SPLIT" TO "SPLITC" TO AVOID NAME CONFLICT.
C
C THIS SUBROUTINE TAKES THE M POINTS OF ARRAY X WHOSE
C SUBSCRIPTS ARE IN ARRAY IN AND PARTITIONS THEM BY THE
C LINE JOINING THE TWO POINTS IN ARRAY X WHOSE SUBSCRIPTS
C ARE II AND JJ. THE SUBSCRIPTS OF THE POINTS ABOVE THE
C LINE ARE PUT INTO ARRAY IABV, AND THE SUBSCRIPTS OF THE
C POINTS BELOW ARE PUT INTO ARRAY IBEL. NA AND NB ARE,
C RESPECTIVELY, THE NUMBER OF POINTS ABOVE THE LINE AND THE
C NUMBER BELOW. MAXA AND MAXB ARE THE SUBSCRIPTS FOR ARRAY
C X OF THE POINT FURTHEST ABOVE THE LINE AND THE POINT
C FURTHEST BELOW, RESPECTIVELY. IF EITHER SUBSET IS NULL
C THE CORRESPONDING SUBSCRIPT (MAXA OR MAXB) IS SET TO ZERO
C FORMAL PARAMETERS
C INPUT
C N    INTEGER           TOTAL NUMBER OF DATA POINTS
C X    REAL ARRAY (2,N)  (X,Y) CO-ORDINATES OF THE DATA
C M    INTEGER           NUMBER OF POINTS IN INPUT SUBSET
C IN   INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
C                        POINTS IN THE INPUT SUBSET
C II   INTEGER           SUBSCRIPT FOR ARRAY X OF ONE POINT
C                        ON THE PARTITIONING LINE
C JJ   INTEGER           SUBSCRIPT FOR ARRAY X OF ANOTHER
C                        POINT ON THE PARTITIONING LINE
C S    INTEGER           SWITCH TO DETERMINE OUTPUT. REFER
C                        TO COMMENTS BELOW
C OUTPUT
C IABV INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
C                        POINTS ABOVE THE PARTITIONING LINE
C NA   INTEGER           NUMBER OF ELEMENTS IN IABV
C MAXA INTEGER           SUBSCRIPT FOR ARRAY X OF POINT
C                        FURTHEST ABOVE THE LINE. SET TO
C                        ZERO IF NA IS ZERO
C IBEL INTEGER ARRAY (M) SUBSCRIPTS FOR ARRAY X OF THE
C                        POINTS BELOW THE PARTITIONING LINE
C NB   INTEGER           NUMBER OF ELEMENTS IN IBEL
C MAXB INTEGER           SUBSCRIPT FOR ARRAY X OF POINT
C                        FURTHEST BELOW THE LINE. SET TO
C                        ZERO IF NB IS ZERO
      DIMENSION X(2,N)
      DIMENSION IN(M),IABV(M),IBEL(M)
      INTEGER S
C IF S = 2 DONT SAVE IBEL,NB,MAXB.
C IF S =-2 DONT SAVE IABV,NA,MAXA.
C OTHERWISE SAVE EVERYTHING
C IF S IS POSITIVE THE ARRAY BEING PARTITIONED IS ABOVE
C THE INITIAL PARTITIONING LINE. IF IT IS NEGATIVE, THEN
C THE SET OF POINTS IS BELOW.
      LOGICAL T
      T=.FALSE.
C CHECK TO SEE IF THE LINE IS VERTICAL
      IF(X(1,JJ).NE.X(1,II))GOTO 1
      XT=X(1,II)
      DIR=SIGN(1.,X(2,JJ)-X(2,II))*SIGN(1.,FLOAT(S))
      T=.TRUE.
      GOTO 2
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
        IF(T)GOTO 3
        Z=X(2,IS)-A*X(1,IS)-B
        GOTO 4
3       Z=DIR*(X(1,IS)-XT)
4       IF(Z.LE.0.)GOTO 5
C THE POINT IS ABOVE THE LINE
        IF(S.EQ.-2)GOTO 6
        NA=NA+1
        IABV(NA)=IS
        IF(Z.LT.UP)GOTO 6
        UP=Z
        MAXA=NA
        GOTO 6
5       IF(S.EQ.2)GOTO 6
        IF(Z.GE.0.)GOTO 6
C THE POINT IS BELOW THE LINE
        NB=NB+1
        IBEL(NB)=IS
        IF(Z.GT.DOWN)GOTO 6
        DOWN=Z
        MAXB=NB
6     CONTINUE
      RETURN
      END
      subroutine triang(n,zeta,sig)
C
C   REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S., 'COMBINATORIAL 
C              ALGORITHMS', SECOND EDITION, ACADEMIC PRESS, 1978,
C              CH. 25, P. 230. 
C=====================================================================
C
      integer q,r,sig,t,zeta
      dimension sig(n), zeta(n,n)
c
      m=0
      l=0
      do 11 i=1,n
         sig(i)=0
11    continue
c
20    continue
      m=m+1
30    continue
      if (sig(m).eq.0) go to 40
c
130   continue
      if (m.eq.n) return
      go to 20
c
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
c
      if (sig(r).ne.0.or.zeta(r,m).eq.0) then
         r=r+1
      else
         sig(r)=m
         m=r
         r=t
      endif
      go to 60
c
      end
      SUBROUTINE YTBHOO (VAL, ROWID, Y, N,
     1                   HOOKLE,COLID,
     1                   IBUGA3,IERROR)
C
C     PURPOSE--COMPUTE THE HOOK LENGTH FOR THE VALUES IN A
C              YOUNG TABLEAUX.  FOR EACH ENTRY IN THE TABLEAUX,
C              THE HOOK LENGTH IS THE SUM OF THE NUMBER OF
C              ENTRIES TO THE RIGHT AND ON THE SAME ROW, THE
C              NUMBER OF ENTRIES BELOW AND IN THE SAME COLUMN,
C              AND 1 (FOR THE ENTRY ITSELF).  THIS COMMAND
C              ASSUMES THE YOUNG TABLEAUX IS IN THE FORM GIVEN
C              BY THE "CONVERT YOUNG TABLEAUX" COMMAND (I.E.,
C              COLUMN ONE IS THE VALUE AND COLUMN TWO IS THE
C              ROWID).
C     INPUT  ARGUMENTS--VAL    INTEGER ARRAY CONTAINING THE VALUE
C                              OF THE I-TH ENTRY IN THE YOUNG
C                              TABLEAUX.
C                     --ROWID  INTEGER ARRAY CONTAINING THE ROW ID
C                              OF THE I-TH ENTRY IN THE YOUNG
C                              TABLEAUX.
C                     --N      AN INTEGER SCALAR CONTAINING THE
C                              NUMBER OF ELEMENTS IN THE YOUNG
C                              TABLEAUX.
C     OUTPUT ARGUMENTS--Y      AN INTEGER ARRAY CONTAINING THE
C                              HOOK LENGTH.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2008/8
C     ORIGINAL VERSION--AUGUST   2008
C     REFERENCE: NIJENHUIS, ALBERT AND WILF, HERBERT S.,
C                'COMBINATORIAL ALGORITHMS', SECOND EDITION,
C                ACADEMIC PRESS, 1978, CHAPTER 14.
C
      INTEGER Y(*)
      INTEGER VAL(*)
      INTEGER ROWID(*)
      INTEGER HOOKLE(*)
      INTEGER COLID(*)
      INTEGER N
C
      CHARACTER*4 IBUGA3
      CHARACTER*4 IERROR
      CHARACTER*4 IWRITE
C
      REAL CPUMIN
      REAL CPUMAX
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,11)N
   11   FORMAT('FROM YTBHOO: N = ',I8)
        CALL DPWRST('XXX','WRIT')
        DO20I=1,N
          WRITE(ICOUT,21)I,VAL(I),ROWID(I)
   21     FORMAT('I,VAL(I),ROWID(I) = ',I8,2G15.7)
          CALL DPWRST('XXX','WRIT')
   20   CONTINUE
      ENDIF
C
      DO100I=1,N
        IF(ROWID(I).LT.1 .OR. ROWID(I).GT.N)THEN
          WRITE(ICOUT,999)
  999     FORMAT(1X)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,111)
  111     FORMAT('***** ERROR IN YOUNG TABLEAUX HOOK LENGTH--')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,113)
  113     FORMAT('      A ROW ID IN INPUT YOUNG TABLEAUX IS OUT OF ',
     1         'RANGE.')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,115)N
  115     FORMAT('      NUMBER OF ELEMENTS IN YOUNG TABLEAUX    = ',
     1           I8)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,116)I,ROWID(I)
  116     FORMAT('      ROW ',I8,' HAS ROW ID = ',I10)
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GOTO9999
        ENDIF
        IF(VAL(I).LT.1 .OR. VAL(I).GT.N)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,111)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,123)
  123     FORMAT('      A VALUE FOR THE INPUT YOUNG TABLEAUX IS ',
     1         'OUT OF RANGE.')
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,125)N
  125     FORMAT('      NUMBER OF ELEMENTS IN YOUNG TABLEAUX    = ',
     1           I8)
          CALL DPWRST('XXX','WRIT')
          WRITE(ICOUT,126)I,VAL(I)
  126     FORMAT('      ROW ',I8,' HAS VALUE = ',I10)
          CALL DPWRST('XXX','WRIT')
          IERROR='YES'
          GOTO9999
        ENDIF
  100 CONTINUE
C
      ICOL=0
      IROW=0
C
      DO200I=1,N
        IF(ROWID(I).NE.IROW)THEN
          ICOL=1
          IROW=ROWID(I)
        ELSE
          ICOL=ICOL+1
        ENDIF
        COLID(I)=ICOL
  200 CONTINUE
C
      HOOKLE(N)=1
C
      DO300I=1,N-1
        IROW=ROWID(I)
        ICOL=COLID(I)
        ISUM1=0
        ISUM2=0
C
        DO400J=I+1,N
          IF(ROWID(J).EQ.ROWID(I))THEN
            ISUM1=ISUM1+1
          ELSE
            GOTO409
          ENDIF
  400   CONTINUE
  409   CONTINUE
C
        DO410J=I+1,N
          IF(COLID(J).EQ.COLID(I))THEN
            ISUM2=ISUM2+1
          ENDIF
  410   CONTINUE
        HOOKLE(I)=ISUM1+ISUM2+1
C
  300 CONTINUE
C
      IF(IBUGA3.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','WRIT')
        WRITE(ICOUT,901)
  901   FORMAT('AT END OF YTBHLE')
        CALL DPWRST('XXX','WRIT')
        DO910I=1,MIN(100,N)
          WRITE(ICOUT,911)
  911     FORMAT('I,ROWID(I),COLID(I),HOOKLE(I)=',I5,3I8)
          CALL DPWRST('XXX','WRIT')
  910   CONTINUE
      ENDIF
C
 9999 CONTINUE
      RETURN
      END

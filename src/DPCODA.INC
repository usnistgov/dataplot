!-----THIS IS COMMON/INCLUDE FILE DPCODA.INC-----
!     THIS IS COMMON FOR DATA
!
!     UPDATED--AUGUST     1992. ADD DSIZE, DSYMB, DCOLOR, DFILL (ALAN)
!     UPDATED--JANUARY    1998. ADD MAXROM, MAXCOM
!     UPDATED--JULY       2019. USE A SINGLE COMMON BLOCK FOR A NUMBER
!                               OF THESE ARRAYS.  THIS CAN ALLOW FOR
!                               USING THIS SPACE AS SCRATCH SPACE FOR
!                               A FEW COMMANDS WITH LARGE MEMORY
!                               REQUIREMENTS
!     UPDATED--JANUARY    2024. TWEAK COMMON BLOCKS FOR COMPILATION ON
!                               MACOS WITH M1 CHIPS
!
!     NOTE--THE PARAMETER SETTINGS FOR
!           MAXOBV, MAXOBW, AND MAXPOP
!           ARE DONE IN FILE DPCOPA.INC
!
!               **************************************
!               **************************************
!               **  DEFINE AND EQUIVALENCE INTEGER  **
!               **  DATA                            **
!               **  VARIABLES AND ARRAYS            **
!               **************************************
!               **************************************
!
      DIMENSION ISUB(MAXOBV)
!
      COMMON /IDATA/I1DATA(100)
!
      EQUIVALENCE (I1DATA(1) ,IDEMXN)
      EQUIVALENCE (I1DATA(2) ,IDEMXC)
!
      EQUIVALENCE (I1DATA(11),MAXNK)
      EQUIVALENCE (I1DATA(12),NK)
      EQUIVALENCE (I1DATA(13),MAXCOL)
      EQUIVALENCE (I1DATA(14),NUMCOL)
      EQUIVALENCE (I1DATA(15),MAXN)
      EQUIVALENCE (I1DATA(16),N)
      EQUIVALENCE (I1DATA(17),MAXCHF)
      EQUIVALENCE (I1DATA(18),NUMCHF)
      EQUIVALENCE (I1DATA(19),MAXFUN)
      EQUIVALENCE (I1DATA(20),NUMFUN)
      EQUIVALENCE (I1DATA(21),MAXCHM)
      EQUIVALENCE (I1DATA(22),NUMCHM)
      EQUIVALENCE (I1DATA(23),NUMCON)
      EQUIVALENCE (I1DATA(24),MAXCON)
      EQUIVALENCE (I1DATA(25),MAXROM)
      EQUIVALENCE (I1DATA(26),MAXCOM)
!
      COMMON /ISUCBL/ISUB
!
!               ****************************************
!               ****************************************
!               **  DEFINE AND EQUIVALENCE CHARACTER  **
!               **  DATA                              **
!               **  VARIABLES AND ARRAYS              **
!               ****************************************
!               ****************************************
!
      CHARACTER*4 IPARNC
      CHARACTER*4 IPANC2
      CHARACTER*4 IPAROP
      CHARACTER*4 MODEL
      CHARACTER*4 IFUNC
      CHARACTER*4 IFUNC2
      CHARACTER*4 IFUNC3
!
      CHARACTER*4 IH1DAT
!
      DIMENSION IPARNC(MAXFN2)
      DIMENSION IPANC2(MAXFN2)
      DIMENSION IPAROP(MAXFN2)
      DIMENSION MODEL(MAXF3)
      DIMENSION IFUNC(MAXF1)
      DIMENSION IFUNC2(MAXF1)
      DIMENSION IFUNC3(MAXF1)
!
      COMMON/IP1CBL/ IPARNC
      COMMON/IP2CBL/ IPANC2
      COMMON/IP3CBL/ IPAROP
      COMMON/MODCBL/ MODEL
      COMMON/IF1CBL/ IFUNC
      COMMON/IF2CBL/ IFUNC2
      COMMON/IF3CBL/ IFUNC3
      COMMON/IH1CBL/ IH1DAT
!
!               ********************************************************
!               ********************************************************
!               **  DEFINE AND EQUIVALENCE REAL (SINGLE PRECISION FLOATI
!               **  DATA
!               **  VARIABLES AND ARRAYS
!               ********************************************************
!               ********************************************************
!
      DIMENSION PARLIM(100)
      DIMENSION PARLLM(100)
      DIMENSION PARULM(100)
!
!     2019/07: USE A SINGLE COMMON BLOCK FOR FOLLOWING SET OF ARRAYS
!     2024/01: BREAK INTO SEVERAL ARRAYS FOR COMPILATION ON MACOS WITH
!              M1 CHIPS
!
      DIMENSION PRED(MAXOBV)
      DIMENSION RES(MAXOBV)
!
      DIMENSION Y(MAXPOP)
      DIMENSION X(MAXPOP)
      DIMENSION X3D(MAXPOP)
      DIMENSION D(MAXPOP)
      DIMENSION DSIZE(MAXPOP)
      DIMENSION DSYMB(MAXPOP)
      DIMENSION DCOLOR(MAXPOP)
      DIMENSION DFILL(MAXPOP)
!
      DIMENSION YPLOT(MAXPOP)
      DIMENSION XPLOT(MAXPOP)
      DIMENSION X2PLOT(MAXPOP)
      DIMENSION TAGPLO(MAXPOP)
!
      DIMENSION V(MAXOBW)
!
      DIMENSION AMATR1(100,100)
!
!     -----
!
      COMMON /PARCBL/PARLIM,PARLLM,PARULM
!
!CCCC COMMON /YCBL/PRED,RES,Y,X,X3D,D,DSIZE,DSYMB,DCOLOR,DFILL,
!CCCC1             YPLOT,XPLOT,X2PLOT,TAGPLO
      COMMON /PRECBL/PRED
      COMMON /RESCBL/RES
!
      COMMON /YCBL/Y
      COMMON /XCBL/X
      COMMON /X3DCBL/X3D
!
      COMMON /DCBL1/D
      COMMON /DCBL2/DSIZE
      COMMON /DCBL3/DSYMB
      COMMON /DCBL4/DCOLOR
      COMMON /DCBL5/DFILL
!
      COMMON /YPLCBL/YPLOT
      COMMON /XPLCBL/XPLOT
      COMMON /X2PCBL/X2PLOT
      COMMON /TAGCBL/TAGPLO
!
      COMMON /VCBL/V
!
      COMMON /AM1CBL/AMATR1
!

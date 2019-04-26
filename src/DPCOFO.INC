C-----THIS IS COMMON/INCLUDE FILE DPCOFO.INC-----
C     THIS IS COMMON FOR FILE OPERATIONS (PART 1)
C
CCCCC UPDATED--APRIL       2009.  INCREASE DIMENSION OF IA TO 255
C
C               **************************************
C               **************************************
C               **  DEFINE AND EQUIVALENCE INTEGER  **
C               **  FILE OPERATION                  **
C               **  VARIABLES AND ARRAYS            **
C               **************************************
C               **************************************
C
      COMMON /IFILOP/I1FILO(10)
C
      EQUIVALENCE (I1FILO(2),NUMCHA)
CCCCC EQUIVALENCE (I1FILO(4),IPRON9)
CCCCC EQUIVALENCE (I1FILO(5),ICONN9)
C
C               **************************************
C               **************************************
C               **  DEFINE AND EQUIVALENCE CHARACTER**
C               **  FILE OPERATION                  **
C               **  VARIABLES AND ARRAYS            **
C               **************************************
C               **************************************
C
      CHARACTER*4 IDEV
CCCCC CHARACTER*4 IPROGR
CCCCC CHARACTER*4 ICONCL
CCCCC CHARACTER*4 IEOF9
      CHARACTER*4 IA
C
CCCCC DIMENSION IA(132)
      DIMENSION IA(255)
C
      CHARACTER*4 IH1FIL
C
CCCCC COMMON /IHFILO/IH1FIL(200)
      COMMON /IHFILO/IH1FIL(300)
C
      EQUIVALENCE (IH1FIL(1) ,IDEV)
CCCCC EQUIVALENCE (IH1FIL(3) ,IPROGR)
CCCCC EQUIVALENCE (IH1FIL(4) ,ICONCL)
CCCCC EQUIVALENCE (IH1FIL(5) ,IEOF9)
      EQUIVALENCE (IH1FIL(11),IA)
C

!-----THIS IS COMMON/INCLUDE FILE DPCOFO.INC-----
!     THIS IS COMMON FOR FILE OPERATIONS (PART 1)
!
!CCCC UPDATED--APRIL       2009. INCREASE DIMENSION OF IA TO 255
!CCCC UPDATED--AUGUST      2020. INCREASE DIMENSION OF IA TO 10000
!
!               **************************************
!               **************************************
!               **  DEFINE AND EQUIVALENCE INTEGER  **
!               **  FILE OPERATION                  **
!               **  VARIABLES AND ARRAYS            **
!               **************************************
!               **************************************
!
      COMMON /IFILOP/I1FILO(10)
!
      EQUIVALENCE (I1FILO(2),NUMCHA)
!CCCC EQUIVALENCE (I1FILO(4),IPRON9)
!CCCC EQUIVALENCE (I1FILO(5),ICONN9)
!
!               **************************************
!               **************************************
!               **  DEFINE AND EQUIVALENCE CHARACTER**
!               **  FILE OPERATION                  **
!               **  VARIABLES AND ARRAYS            **
!               **************************************
!               **************************************
!
      CHARACTER*4 IDEV
!CCCC CHARACTER*4 IPROGR
!CCCC CHARACTER*4 ICONCL
!CCCC CHARACTER*4 IEOF9
      CHARACTER*4 IA
!
!CCCC DIMENSION IA(132)
!CCCC DIMENSION IA(255)
!CCCC DIMENSION IA(1024)
      DIMENSION IA(10000)
!
      CHARACTER*4 IH1FIL
!
!CCCC COMMON /IHFILO/IH1FIL(200)
!CCCC COMMON /IHFILO/IH1FIL(300)
!CCCC COMMON /IHFILO/IH1FIL(500)
      COMMON /IHFILO/IH1FIL(11000)
!
      EQUIVALENCE (IH1FIL(1) ,IDEV)
!CCCC EQUIVALENCE (IH1FIL(3) ,IPROGR)
!CCCC EQUIVALENCE (IH1FIL(4) ,ICONCL)
!CCCC EQUIVALENCE (IH1FIL(5) ,IEOF9)
      EQUIVALENCE (IH1FIL(11),IA)
!

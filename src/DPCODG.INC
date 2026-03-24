!-----THIS IS COMMON/INCLUDE FILE DPCODG.INC-----
!     THIS IS COMMON FOR DIAGRAMMATIC GRAPHICS
!
!               ****************************************
!               ****************************************
!               **  DEFINE AND EQUIVALENCE CHARACTER  **
!               **  DIAGRAMMATIC GRAPHICS             **
!               **  VARIABLES AND ARRAYS              **
!               ****************************************
!               ****************************************
!
      CHARACTER*4 IDEFON
      CHARACTER*4 IDECAS
      CHARACTER*4 IDEJUS
      CHARACTER*4 IDECRL
!
      CHARACTER*4 IFONT
      CHARACTER*4 ICASE
      CHARACTER*4 IJUST
      CHARACTER*4 ICRLF
!
      CHARACTER*4 IH1DIA
!
      COMMON /IHDIAG/IH1DIA(40)
!
      EQUIVALENCE (IH1DIA(1) ,IDEFON)
      EQUIVALENCE (IH1DIA(2) ,IDECAS)
      EQUIVALENCE (IH1DIA(3) ,IDEJUS)
      EQUIVALENCE (IH1DIA(4) ,IDECRL)
!
      EQUIVALENCE (IH1DIA(21),IFONT)
      EQUIVALENCE (IH1DIA(22),ICASE)
      EQUIVALENCE (IH1DIA(23),IJUST)
      EQUIVALENCE (IH1DIA(24),ICRLF)
!
!               ****************************************************************
!               ****************************************************************
!               **  DEFINE AND EQUIVALENCE REAL (SINGLE PRECISION FLOATING POINT
!               **  DIAGRAMMATIC GRAPHICS
!               **  VARIABLES AND ARRAYS
!               ****************************************************************
!               ****************************************************************
!
      DIMENSION AMARG(6)
!
      COMMON /RDIAG/R1DIAG(40)
!
      EQUIVALENCE (R1DIAG(1),DEFWID)
      EQUIVALENCE (R1DIAG(2),DEFHEI)
!
      EQUIVALENCE (R1DIAG(11),DEFLMA)
      EQUIVALENCE (R1DIAG(12),DEFCHM)
      EQUIVALENCE (R1DIAG(13),DEFRMA)
      EQUIVALENCE (R1DIAG(14),DEFTMA)
      EQUIVALENCE (R1DIAG(15),DEFCVM)
      EQUIVALENCE (R1DIAG(16),DEFBMA)
!
      EQUIVALENCE (R1DIAG(21),WIDTH)
      EQUIVALENCE (R1DIAG(22),HEIGHT)
      EQUIVALENCE (R1DIAG(23),XSTART)
      EQUIVALENCE (R1DIAG(24),YSTART)
      EQUIVALENCE (R1DIAG(25),XEND)
      EQUIVALENCE (R1DIAG(26),YEND)
!
      EQUIVALENCE (R1DIAG(31),AMARG(1))
!

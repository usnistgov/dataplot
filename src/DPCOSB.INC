C-----THIS IS COMMON/INCLUDE FILE DPCOSB.INC-----
C     THIS IS COMMON FOR STATISTIC BLOCKS
CCCCC UPDATED--AUGUST    2016. ORIGINAL VERSION
C
      PARAMETER (MAXSBL=30)
      PARAMETER (MAXSBP=20)
C
      CHARACTER*4 ISBLSW
C
      CHARACTER*8 ISBNA1
      CHARACTER*8 ISBAN1
      CHARACTER*8 ISBPL1(MAXSBP)
      CHARACTER (LEN=MAXSTR) :: ISBLI1(MAXSBL)
C
      CHARACTER*8 ISBNA2
      CHARACTER*8 ISBAN2
      CHARACTER*8 ISBPL2(MAXSBP)
      CHARACTER (LEN=MAXSTR) :: ISBLI2(MAXSBL)
C
      CHARACTER*8 ISBNA3
      CHARACTER*8 ISBAN3
      CHARACTER*8 ISBPL3(MAXSBP)
      CHARACTER (LEN=MAXSTR) :: ISBLI3(MAXSBL)
C
      COMMON/ISBL/ISBLSW,ISBNA1,ISBAN1,ISBPL1,ISBLI1,ISBCN1,ISBCP1,
     1                   ISBNA2,ISBAN2,ISBPL2,ISBLI2,ISBCN2,ISBCP2,
     1                   ISBNA3,ISBAN3,ISBPL3,ISBLI3,ISBCN3,ISBCP3
C

      SUBROUTINE DMATH1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR MATH SYMBOLS (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(150)
      DIMENSION IXMAXD(150)
      DIMENSION IXDELD(150)
      DIMENSION ISTARD(150)
      DIMENSION NUMCOO(150)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
 
C     DEFINE CHARACTER  2220--/ (SLASH)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   9,  16/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9, -16/
C
      DATA IXMIND(   1)/ -11/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  22/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/   2/
C
C     DEFINE CHARACTER  2221--( (LEFT  PARENTHESES)
C
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   4,  16/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   2,  14/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   0,  11/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -2,   7/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -3,   2/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -3,  -2/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -2,  -7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   0, -11/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   2, -14/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   4, -16/
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',   2,  14/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   0,  10/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -1,   7/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -2,   2/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -2,  -2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -1,  -7/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   0, -10/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   2, -14/
C
      DATA IXMIND(   2)/  -7/
      DATA IXMAXD(   2)/   7/
      DATA IXDELD(   2)/  14/
      DATA ISTARD(   2)/   3/
      DATA NUMCOO(   2)/  18/
C
C     DEFINE CHARACTER  2222--) (RIGHT PARENTHESES)
C
      DATA IOPERA(  21),IX(  21),IY(  21)/'MOVE',  -4,  16/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -2,  14/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   0,  11/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   2,   7/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   3,   2/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   3,  -2/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   2,  -7/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   0, -11/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -2, -14/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -4, -16/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -2,  14/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   0,  10/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   1,   7/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   2,   2/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   2,  -2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   1,  -7/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   0, -10/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -2, -14/
C
      DATA IXMIND(   3)/  -7/
      DATA IXMAXD(   3)/   7/
      DATA IXDELD(   3)/  14/
      DATA ISTARD(   3)/  21/
      DATA NUMCOO(   3)/  18/
C
C     DEFINE CHARACTER  2223--LBRACKET (LEFT  BRACKET)
C
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',  -3,  16/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3, -16/
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',  -2,  16/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -2, -16/
      DATA IOPERA(  43),IX(  43),IY(  43)/'MOVE',  -3,  16/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   4,  16/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',  -3, -16/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   4, -16/
C
      DATA IXMIND(   4)/  -7/
      DATA IXMAXD(   4)/   7/
      DATA IXDELD(   4)/  14/
      DATA ISTARD(   4)/  39/
      DATA NUMCOO(   4)/   8/
C
C     DEFINE CHARACTER  2224--RBRACKET (RIGHT BRACKET)
C
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',   2,  16/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   2, -16/
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE',   3,  16/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   3, -16/
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',  -4,  16/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   3,  16/
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',  -4, -16/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   3, -16/
C
      DATA IXMIND(   5)/  -7/
      DATA IXMAXD(   5)/   7/
      DATA IXDELD(   5)/  14/
      DATA ISTARD(   5)/  47/
      DATA NUMCOO(   5)/   8/
C
C     DEFINE CHARACTER  2225--LBRACE (LEFT  BRACE)
C
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',   2,  16/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   0,  15/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -1,  14/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -2,  12/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -2,  10/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -1,   8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   0,   7/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   1,   5/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   1,   3/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -1,   1/
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',   0,  15/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -1,  13/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -1,  11/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   0,   9/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   1,   8/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2,   6/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   2,   4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   1,   2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -3,   0/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   1,  -2/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   2,  -4/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   2,  -6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   1,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   0,  -9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -1, -11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -1, -13/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   0, -15/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -1,  -1/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   1,  -3/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   1,  -5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   0,  -7/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -1,  -8/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -2, -10/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -2, -12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -1, -14/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   0, -15/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   2, -16/
C
      DATA IXMIND(   6)/  -7/
      DATA IXMAXD(   6)/   7/
      DATA IXDELD(   6)/  14/
      DATA ISTARD(   6)/  55/
      DATA NUMCOO(   6)/  37/
C
C     DEFINE CHARACTER  2226--RBRACE (RIGHT BRACE)
C
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  -2,  16/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   0,  15/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   1,  14/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   2,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   2,  10/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   1,   8/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   0,   7/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -1,   5/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -1,   3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   1,   1/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',   0,  15/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,  13/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   1,  11/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   0,   9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -1,   8/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -2,   6/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -2,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -1,   2/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   3,   0/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -1,  -2/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -2,  -4/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -2,  -6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -1,  -8/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   0,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   1, -11/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   1, -13/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   0, -15/
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',   1,  -1/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -1,  -3/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -1,  -5/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   0,  -7/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   1,  -8/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   2, -10/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   2, -12/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   1, -14/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   0, -15/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -2, -16/
C
      DATA IXMIND(   7)/  -7/
      DATA IXMAXD(   7)/   7/
      DATA IXDELD(   7)/  14/
      DATA ISTARD(   7)/  92/
      DATA NUMCOO(   7)/  37/
C
C     DEFINE CHARACTER  2227--LELBOW (LEFT  ELBOW)
C
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',   3,  16/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -4,   0/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   3, -16/
C
      DATA IXMIND(   8)/  -7/
      DATA IXMAXD(   8)/   7/
      DATA IXDELD(   8)/  14/
      DATA ISTARD(   8)/ 129/
      DATA NUMCOO(   8)/   3/
C
C     DEFINE CHARACTER  2228--RELBOW (RIGHT ELBOW)
C
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',  -3,  16/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   4,   0/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -3, -16/
C
      DATA IXMIND(   9)/  -7/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  14/
      DATA ISTARD(   9)/ 132/
      DATA NUMCOO(   9)/   3/
C
C     DEFINE CHARACTER  2229--VBAR (VERTICAL BAR)
C
      DATA IOPERA( 135),IX( 135),IY( 135)/'MOVE',   0,  16/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   0, -16/
C
      DATA IXMIND(  10)/  -4/
      DATA IXMAXD(  10)/   4/
      DATA IXDELD(  10)/   8/
      DATA ISTARD(  10)/ 135/
      DATA NUMCOO(  10)/   2/
C
C     DEFINE CHARACTER  2230--DVBAR (DOUBLE VERTICAL BAR)
C
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',  -3,  16/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -3, -16/
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',   3,  16/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   3, -16/
C
      DATA IXMIND(  11)/  -7/
      DATA IXMAXD(  11)/   7/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/ 137/
      DATA NUMCOO(  11)/   4/
C
C     DEFINE CHARACTER  2231--- (MINUS SIGN)
C
      DATA IOPERA( 141),IX( 141),IY( 141)/'MOVE',  -9,   0/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   9,   0/
C
      DATA IXMIND(  12)/ -13/
      DATA IXMAXD(  12)/  13/
      DATA IXDELD(  12)/  26/
      DATA ISTARD(  12)/ 141/
      DATA NUMCOO(  12)/   2/
C
C     DEFINE CHARACTER  2232--+ (PLUS SIGN)
C
      DATA IOPERA( 143),IX( 143),IY( 143)/'MOVE',   0,   9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   0,  -9/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',  -9,   0/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   9,   0/
C
      DATA IXMIND(  13)/ -13/
      DATA IXMAXD(  13)/  13/
      DATA IXDELD(  13)/  26/
      DATA ISTARD(  13)/ 143/
      DATA NUMCOO(  13)/   4/
C
C     DEFINE CHARACTER  2233--+- (PLUS OR MINUS)
C
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',   0,   8/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   0,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -8,   0/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   8,   0/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',  -8,  -9/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   8,  -9/
C
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  12/
      DATA IXDELD(  14)/  24/
      DATA ISTARD(  14)/ 147/
      DATA NUMCOO(  14)/   6/
C
C     DEFINE CHARACTER  2234---+ (MINUS OR PLUS)
C
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   0,   8/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   0,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',  -8,   8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   8,   8/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',  -8,   0/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   8,   0/
C
      DATA IXMIND(  15)/ -12/
      DATA IXMAXD(  15)/  12/
      DATA IXDELD(  15)/  24/
      DATA ISTARD(  15)/ 153/
      DATA NUMCOO(  15)/   6/
C
C     DEFINE CHARACTER  2235--TIMES (TIMES SIGN)
C
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',  -7,   7/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   7,  -7/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',   7,   7/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -7,  -7/
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  22/
      DATA ISTARD(  16)/ 159/
      DATA NUMCOO(  16)/   4/
C
C     DEFINE CHARACTER  2236--DOTP (DOT PRODUCT SIGN)
C
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',   0,   1/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -1,   0/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   0,  -1/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   1,   0/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   0,   1/
C
      DATA IXMIND(  17)/  -5/
      DATA IXMAXD(  17)/   5/
      DATA IXDELD(  17)/  10/
      DATA ISTARD(  17)/ 163/
      DATA NUMCOO(  17)/   5/
C
C     DEFINE CHARACTER  2237--DIVISION (DIVISION SIGN)
C
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   0,   9/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -1,   8/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   0,   7/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   1,   8/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   0,   9/
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',  -9,   0/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   9,   0/
      DATA IOPERA( 175),IX( 175),IY( 175)/'MOVE',   0,  -7/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -1,  -8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   0,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   1,  -8/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   0,  -7/
C
      DATA IXMIND(  18)/ -13/
      DATA IXMAXD(  18)/  13/
      DATA IXDELD(  18)/  26/
      DATA ISTARD(  18)/ 168/
      DATA NUMCOO(  18)/  12/
C
C     DEFINE CHARACTER  2238--= (EQUAL SIGN)
C
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  -9,   3/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   9,   3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'MOVE',  -9,  -3/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   9,  -3/
C
      DATA IXMIND(  19)/ -13/
      DATA IXMAXD(  19)/  13/
      DATA IXDELD(  19)/  26/
      DATA ISTARD(  19)/ 180/
      DATA NUMCOO(  19)/   4/
C
C     DEFINE CHARACTER  2239--NOTEQ (NOT EQUAL SIGN)
C
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',   7,   9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -7,  -9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -9,   3/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   9,   3/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',  -9,  -3/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   9,  -3/
C
      DATA IXMIND(  20)/ -13/
      DATA IXMAXD(  20)/  13/
      DATA IXDELD(  20)/  26/
      DATA ISTARD(  20)/ 184/
      DATA NUMCOO(  20)/   6/
C
C     DEFINE CHARACTER  2240--EQUIVALE (EQUIVALENCE SIGN)
C
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -9,   5/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   9,   5/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',  -9,   0/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   9,   0/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',  -9,  -5/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   9,  -5/
C
      DATA IXMIND(  21)/ -13/
      DATA IXMAXD(  21)/  13/
      DATA IXDELD(  21)/  26/
      DATA ISTARD(  21)/ 190/
      DATA NUMCOO(  21)/   6/
C
C     DEFINE CHARACTER  2241--< (LESS THAN SIGN)
C
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',   8,   9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -8,   0/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   8,  -9/
C
      DATA IXMIND(  22)/ -12/
      DATA IXMAXD(  22)/  12/
      DATA IXDELD(  22)/  24/
      DATA ISTARD(  22)/ 196/
      DATA NUMCOO(  22)/   3/
C
C     DEFINE CHARACTER  2242--> (GREATER THAN SIGN)
C
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',  -8,   9/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   8,   0/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -8,  -9/
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  24/
      DATA ISTARD(  23)/ 199/
      DATA NUMCOO(  23)/   3/
C
C     DEFINE CHARACTER  2243--LTEQ (LESS THAN OR EQUAL TO SIGN)
C
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',   8,  12/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -8,   5/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   8,  -2/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',  -8,  -4/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   8,  -4/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -8,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   8,  -9/
C
      DATA IXMIND(  24)/ -12/
      DATA IXMAXD(  24)/  12/
      DATA IXDELD(  24)/  24/
      DATA ISTARD(  24)/ 202/
      DATA NUMCOO(  24)/   7/
C
C     DEFINE CHARACTER  2244--GTEQ (GREATER THAN OR EQUAL TO SIGN)
C
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',  -8,  12/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   8,   5/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -8,  -2/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',  -8,  -4/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   8,  -4/
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',  -8,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   8,  -9/
C
      DATA IXMIND(  25)/ -12/
      DATA IXMAXD(  25)/  12/
      DATA IXDELD(  25)/  24/
      DATA ISTARD(  25)/ 209/
      DATA NUMCOO(  25)/   7/
C
C     DEFINE CHARACTER  2245--VARIES (VARIES SIGN)
C
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   9,  -5/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   7,  -5/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   5,  -4/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   3,  -2/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   0,   2/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -1,   3/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -3,   4/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -5,   4/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -7,   3/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -8,   1/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -8,  -1/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -7,  -3/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -5,  -4/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -3,  -4/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -1,  -3/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   0,  -2/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   3,   2/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   5,   4/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   7,   5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   9,   5/
C
      DATA IXMIND(  26)/ -12/
      DATA IXMAXD(  26)/  13/
      DATA IXDELD(  26)/  25/
      DATA ISTARD(  26)/ 216/
      DATA NUMCOO(  26)/  20/
C
C     DEFINE CHARACTER  2246--APPROX (APPROXIMATION SIGN)
C
      DATA IOPERA( 236),IX( 236),IY( 236)/'MOVE',  -9,  -3/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -9,  -1/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -8,   2/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -6,   3/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -4,   3/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -2,   2/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   2,  -1/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   4,  -2/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   6,  -2/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   8,  -1/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   9,   1/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',  -9,  -1/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -8,   1/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  -6,   2/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -4,   2/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -2,   1/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   2,  -2/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   4,  -3/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   6,  -3/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   8,  -2/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   9,   1/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   9,   3/
C
      DATA IXMIND(  27)/ -12/
      DATA IXMAXD(  27)/  12/
      DATA IXDELD(  27)/  24/
      DATA ISTARD(  27)/ 236/
      DATA NUMCOO(  27)/  22/
C
C     DEFINE CHARACTER  2247--CARAT (CARAT)
C
      DATA IOPERA( 258),IX( 258),IY( 258)/'MOVE',  -8,  -2/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   0,   3/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   8,  -2/
      DATA IOPERA( 261),IX( 261),IY( 261)/'MOVE',  -8,  -2/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   0,   2/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   8,  -2/
C
      DATA IXMIND(  28)/ -11/
      DATA IXMAXD(  28)/  11/
      DATA IXDELD(  28)/  22/
      DATA ISTARD(  28)/ 258/
      DATA NUMCOO(  28)/   6/
C
C     DEFINE CHARACTER  2248--PRIME (PRIME = RIGHT ACCENT)
C
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',   2,  12/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -3,   6/
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',   2,  12/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   3,  11/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -3,   6/
C
      DATA IXMIND(  29)/  -6/
      DATA IXMAXD(  29)/   6/
      DATA IXDELD(  29)/  12/
      DATA ISTARD(  29)/ 264/
      DATA NUMCOO(  29)/   5/
C
C     DEFINE CHARACTER  2249--LACCENT (LEFT ACCENT)
C
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',  -2,  12/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   3,   6/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',  -2,  12/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -3,  11/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   3,   6/
C
      DATA IXMIND(  30)/  -6/
      DATA IXMAXD(  30)/   6/
      DATA IXDELD(  30)/  12/
      DATA ISTARD(  30)/ 269/
      DATA NUMCOO(  30)/   5/
C
C     DEFINE CHARACTER  2250--BREVE (BREVE)
C
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',  -7,  12/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -6,  10/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -4,   8/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -1,   7/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   1,   7/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   4,   8/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   6,  10/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   7,  12/
      DATA IOPERA( 282),IX( 282),IY( 282)/'MOVE',  -7,  12/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -6,   9/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  -4,   7/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',  -1,   6/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   1,   6/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',   4,   7/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   6,   9/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',   7,  12/
C
      DATA IXMIND(  31)/ -10/
      DATA IXMAXD(  31)/  10/
      DATA IXDELD(  31)/  20/
      DATA ISTARD(  31)/ 274/
      DATA NUMCOO(  31)/  16/
C
C     DEFINE CHARACTER  2251--RQUOTE (RIGHT QUOTE)
C
      DATA IOPERA( 290),IX( 290),IY( 290)/'MOVE',   0,  10/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -1,  11/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',   0,  12/
      DATA IOPERA( 293),IX( 293),IY( 293)/'DRAW',   1,  11/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   1,   9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'DRAW',   0,   7/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',  -1,   6/
C
      DATA IXMIND(  32)/  -5/
      DATA IXMAXD(  32)/   5/
      DATA IXDELD(  32)/  10/
      DATA ISTARD(  32)/ 290/
      DATA NUMCOO(  32)/   7/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DMATH1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IFOUND,IERROR
   59 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
C               *********************************************
C               **  STEP 3--                               **
C               **  ADJUST THE COORDINATES IF A CIRCLE.    **
C               **  IF THE CHARACTER IS A CIRCLE           **
C               **  THEN SCALE THE FIGURE DOWN FROM        **
C               **  -17 TO 17  TO THE MORE USUAL -7 TO 7.  **
C               **  THE ORIGINAL CIRCLE WAS FROM -17 TO 17 **
C               **  RATHER THAN -7 TO 7 IN                 **
C               **  ORDER TO INCREASE THE RESOLUTION       **
C               **  AND GIVE A 32 POINT CIRCLE RATHER      **
C               **  THAN A 16 POINT CIRCLE.                **
C               *********************************************
C
      IF(ICHARN.EQ.102)GOTO1210
      GOTO1290
C
 1210 CONTINUE
      AFACTO=7.0/17.0
      DO1220J=1,NUMCO
      X(J)=X(J)*AFACTO
      Y(J)=Y(J)*AFACTO
 1220 CONTINUE
      IXMINS=(-7)
      IXMAXS=7
      IXDELS=14
C
 1290 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DMATH1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG4,ISUBG4,IFOUND,IERROR
 9012 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DMATH2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR MATH SYMBOLS (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(150)
      DIMENSION IXMAXD(150)
      DIMENSION IXDELD(150)
      DIMENSION ISTARD(150)
      DIMENSION NUMCOO(150)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
 
C     DEFINE CHARACTER  2252--LQUOTE (LEFT QUOTE)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   1,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   0,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -1,   9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -1,   7/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   0,   6/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   1,   7/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   0,   8/
C
      DATA IXMIND(  33)/  -5/
      DATA IXMAXD(  33)/   5/
      DATA IXDELD(  33)/  10/
      DATA ISTARD(  33)/   1/
      DATA NUMCOO(  33)/   7/
C
C     DEFINE CHARACTER  2253--NASPIRAT (NORMAL ASPIRATE)
C
      DATA IOPERA(   8),IX(   8),IY(   8)/'MOVE',   0,  10/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   1,  11/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   0,  12/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,  11/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -1,   9/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   0,   7/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   1,   6/
C
      DATA IXMIND(  34)/  -5/
      DATA IXMAXD(  34)/   5/
      DATA IXDELD(  34)/  10/
      DATA ISTARD(  34)/   8/
      DATA NUMCOO(  34)/   7/
C
C     DEFINE CHARACTER  2254--IASPIRAT (INVERTED ASPIRATE)
C
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -1,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   0,  11/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   1,   9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   1,   7/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   0,   6/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -1,   7/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   0,   8/
C
      DATA IXMIND(  35)/  -5/
      DATA IXMAXD(  35)/   5/
      DATA IXDELD(  35)/  10/
      DATA ISTARD(  35)/  15/
      DATA NUMCOO(  35)/   7/
C
C     DEFINE CHARACTER  2255--RADICAL (RADICAL)
C
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE', -10,   5/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -6,   5/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   0,  -7/
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',  -7,   5/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   0,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'MOVE',   9,  16/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   0,  -9/
C
      DATA IXMIND(  36)/ -13/
      DATA IXMAXD(  36)/   9/
      DATA IXDELD(  36)/  22/
      DATA ISTARD(  36)/  22/
      DATA NUMCOO(  36)/   7/
C
C     DEFINE CHARACTER  2256--SUBSET (SUBSET SYMBOL)
C
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',   8,   8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   1,   8/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -3,   7/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -5,   6/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -7,   4/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -8,   1/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -8,  -1/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -7,  -4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -5,  -6/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -3,  -7/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   1,  -8/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   8,  -8/
C
      DATA IXMIND(  37)/ -12/
      DATA IXMAXD(  37)/  12/
      DATA IXDELD(  37)/  24/
      DATA ISTARD(  37)/  29/
      DATA NUMCOO(  37)/  12/
C
C     DEFINE CHARACTER  2257--UNION (UNION SYMBOL)
C
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',  -8,   8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -8,   1/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -7,  -3/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -6,  -5/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -4,  -7/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -1,  -8/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   1,  -8/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   4,  -7/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   6,  -5/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   7,  -3/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   8,   1/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   8,   8/
C
      DATA IXMIND(  38)/ -12/
      DATA IXMAXD(  38)/  12/
      DATA IXDELD(  38)/  24/
      DATA ISTARD(  38)/  41/
      DATA NUMCOO(  38)/  12/
C
C     DEFINE CHARACTER  2258--SUPERSET (SUPERSET SYMBOL)
C
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',  -8,   8/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -1,   8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   3,   7/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   5,   6/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   7,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   8,   1/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   8,  -1/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   7,  -4/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   5,  -6/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   3,  -7/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -1,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -8,  -8/
C
      DATA IXMIND(  39)/ -12/
      DATA IXMAXD(  39)/  12/
      DATA IXDELD(  39)/  24/
      DATA ISTARD(  39)/  53/
      DATA NUMCOO(  39)/  12/
C
C     DEFINE CHARACTER  2259--INTERSEC (INTERSECTION SYMBOL)
C
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -8,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -8,  -1/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -7,   3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -6,   5/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -4,   7/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -1,   8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   1,   8/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   4,   7/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   6,   5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   7,   3/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   8,  -1/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   8,  -8/
C
      DATA IXMIND(  40)/ -12/
      DATA IXMAXD(  40)/  12/
      DATA IXDELD(  40)/  24/
      DATA ISTARD(  40)/  65/
      DATA NUMCOO(  40)/  12/
C
C     DEFINE CHARACTER  2260--ELEMENT (ELEMENT SYMBOL)
C
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE',   8,   8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   1,   8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,   7/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -5,   6/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -7,   4/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -8,   1/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -8,  -1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -7,  -4/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -5,  -6/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -3,  -7/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   1,  -8/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   8,  -8/
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',  -8,   0/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   4,   0/
C
      DATA IXMIND(  41)/ -12/
      DATA IXMAXD(  41)/  12/
      DATA IXDELD(  41)/  24/
      DATA ISTARD(  41)/  77/
      DATA NUMCOO(  41)/  14/
C
C     DEFINE CHARACTER  2261--RARROW (RIGHT ARROW)
C
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',   6,   2/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   9,   0/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   6,  -2/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',   3,   5/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   8,   0/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   3,  -5/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',  -9,   0/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   8,   0/
C
      DATA IXMIND(  42)/ -13/
      DATA IXMAXD(  42)/  13/
      DATA IXDELD(  42)/  26/
      DATA ISTARD(  42)/  91/
      DATA NUMCOO(  42)/   8/
C
C     DEFINE CHARACTER  2262--UARROW (UP ARROW)
C
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -2,   6/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   0,   9/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   2,   6/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',  -5,   3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   0,   8/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   5,   3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',   0,   8/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   0,  -9/
C
      DATA IXMIND(  43)/  -8/
      DATA IXMAXD(  43)/   8/
      DATA IXDELD(  43)/  16/
      DATA ISTARD(  43)/  99/
      DATA NUMCOO(  43)/   8/
C
C     DEFINE CHARACTER  2263--LARROW (LEFT ARROW)
C
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -6,   2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -9,   0/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,  -2/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -3,   5/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -8,   0/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -3,  -5/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -8,   0/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   9,   0/
C
      DATA IXMIND(  44)/ -13/
      DATA IXMAXD(  44)/  13/
      DATA IXDELD(  44)/  26/
      DATA ISTARD(  44)/ 107/
      DATA NUMCOO(  44)/   8/
C
C     DEFINE CHARACTER  2264--DARROW (DOWN ARROW)
C
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',  -2,  -6/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   0,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   2,  -6/
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',  -5,  -3/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   0,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   5,  -3/
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',   0,   9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   0,  -8/
C
      DATA IXMIND(  45)/  -8/
      DATA IXMAXD(  45)/   8/
      DATA IXDELD(  45)/  16/
      DATA ISTARD(  45)/ 115/
      DATA NUMCOO(  45)/   8/
C
C     DEFINE CHARACTER  2265--PARTIAL (PARTIAL DERIVATIVE = NABLA)
C
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',   6,   0/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   5,   3/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   4,   4/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   2,   5/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   0,   5/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -3,   4/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -5,   1/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -6,  -2/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -6,  -5/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -5,  -7/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4,  -8/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -2,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   0,  -9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   3,  -8/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   5,  -6/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,  -3/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   7,   2/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   7,   7/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   6,  10/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   5,  11/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   3,  12/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   0,  12/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -2,  11/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -3,  10/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -3,   9/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -2,   9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -2,  10/
      DATA IOPERA( 150),IX( 150),IY( 150)/'MOVE',   0,   5/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -2,   4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -4,   1/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -5,  -2/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -5,  -6/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -4,  -8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'MOVE',   0,  -9/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   2,  -8/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   4,  -6/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   5,  -3/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   6,   2/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   6,   7/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   5,  10/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   3,  12/
C
      DATA IXMIND(  46)/  -9/
      DATA IXMAXD(  46)/  10/
      DATA IXDELD(  46)/  19/
      DATA ISTARD(  46)/ 123/
      DATA NUMCOO(  46)/  41/
C
C     DEFINE CHARACTER  2266--DEL (DELTA = VECTOR OPERATOR)
C
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',  -8,  12/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   0,  -9/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',  -7,  12/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   0,  -7/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   8,  12/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0,  -9/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',  -8,  12/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   8,  12/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',  -7,  11/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   7,  11/
C
      DATA IXMIND(  47)/ -10/
      DATA IXMAXD(  47)/  10/
      DATA IXDELD(  47)/  20/
      DATA ISTARD(  47)/ 164/
      DATA NUMCOO(  47)/  10/
C
C     DEFINE CHARACTER  2267--LRADICAL (LONGER RADICAL)
C
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE', -14,   5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -9,   5/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   0,  -7/
      DATA IOPERA( 177),IX( 177),IY( 177)/'MOVE', -10,   4/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   0,  -9/
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',  16,  24/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   0,  -9/
C
      DATA IXMIND(  48)/ -17/
      DATA IXMAXD(  48)/  16/
      DATA IXDELD(  48)/  33/
      DATA ISTARD(  48)/ 174/
      DATA NUMCOO(  48)/   7/
C
C     DEFINE CHARACTER  2268--INTEGRAL (INTEGRAL)
C
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   9,  15/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   8,  14/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   9,  13/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  10,  14/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  10,  15/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   9,  16/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   7,  16/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   5,  15/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   3,  13/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   2,  11/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   1,   8/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   0,   4/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -2,  -8/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -3, -12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -4, -14/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',   4,  14/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   3,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   2,   8/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   0,  -4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -1,  -8/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -2, -11/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -3, -13/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -5, -15/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -7, -16/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -9, -16/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW', -10, -15/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW', -10, -14/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -9, -13/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -8, -14/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -9, -15/
C
      DATA IXMIND(  49)/ -12/
      DATA IXMAXD(  49)/  12/
      DATA IXDELD(  49)/  24/
      DATA ISTARD(  49)/ 181/
      DATA NUMCOO(  49)/  30/
C
C     DEFINE CHARACTER  2269--CINTEGRA (CIRCULAR INTEGRAL)
C
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',   9,  15/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   8,  14/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   9,  13/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  10,  14/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  10,  15/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   9,  16/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   7,  16/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   5,  15/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   3,  13/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   2,  11/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   1,   8/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   0,   4/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -2,  -8/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -3, -12/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -4, -14/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   4,  14/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   3,  12/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   2,   8/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   0,  -4/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -1,  -8/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -2, -11/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -3, -13/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -5, -15/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -7, -16/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -9, -16/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW', -10, -15/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW', -10, -14/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -9, -13/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -8, -14/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -9, -15/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -1,   7/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -4,   6/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -6,   4/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -7,   1/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -7,  -1/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -6,  -4/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -4,  -6/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -1,  -7/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   1,  -7/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   4,  -6/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   6,  -4/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   7,  -1/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   7,   1/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   6,   4/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   4,   6/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   1,   7/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -1,   7/
C
      DATA IXMIND(  50)/ -12/
      DATA IXMAXD(  50)/  12/
      DATA IXDELD(  50)/  24/
      DATA ISTARD(  50)/ 211/
      DATA NUMCOO(  50)/  47/
C
C     DEFINE CHARACTER  2270--INFINITY (INFINITY SIGN)
C
      DATA IOPERA( 258),IX( 258),IY( 258)/'MOVE',  10,  -1/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   9,  -3/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   7,  -4/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   5,  -4/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   3,  -3/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   2,  -2/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -1,   2/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -2,   3/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -4,   4/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -6,   4/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -8,   3/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -9,   1/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -9,  -1/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -8,  -3/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -6,  -4/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',  -4,  -4/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -2,  -3/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -1,  -2/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   2,   2/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   3,   3/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   5,   4/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   7,   4/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   9,   3/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  10,   1/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  10,  -1/
C
      DATA IXMIND(  51)/ -12/
      DATA IXMAXD(  51)/  13/
      DATA IXDELD(  51)/  25/
      DATA ISTARD(  51)/ 258/
      DATA NUMCOO(  51)/  25/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DMATH2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IFOUND,IERROR
   59 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
C               *********************************************
C               **  STEP 3--                               **
C               **  ADJUST THE COORDINATES IF A CIRCLE.    **
C               **  IF THE CHARACTER IS A CIRCLE           **
C               **  THEN SCALE THE FIGURE DOWN FROM        **
C               **  -17 TO 17  TO THE MORE USUAL -7 TO 7.  **
C               **  THE ORIGINAL CIRCLE WAS FROM -17 TO 17 **
C               **  RATHER THAN -7 TO 7 IN                 **
C               **  ORDER TO INCREASE THE RESOLUTION       **
C               **  AND GIVE A 32 POINT CIRCLE RATHER      **
C               **  THAN A 16 POINT CIRCLE.                **
C               *********************************************
C
      IF(ICHARN.EQ.102)GOTO1210
      GOTO1290
C
 1210 CONTINUE
      AFACTO=7.0/17.0
      DO1220J=1,NUMCO
      X(J)=X(J)*AFACTO
      Y(J)=Y(J)*AFACTO
 1220 CONTINUE
      IXMINS=(-7)
      IXMAXS=7
      IXDELS=14
C
 1290 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DMATH2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG4,ISUBG4,IFOUND,IERROR
 9012 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DMATH3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR MATH SYMBOLS (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(150)
      DIMENSION IXMAXD(150)
      DIMENSION IXDELD(150)
      DIMENSION ISTARD(150)
      DIMENSION NUMCOO(150)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
 
C     DEFINE CHARACTER  2271--% (PERCENT SIGN)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   9,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',  -4,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -2,  10/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -2,   8/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -3,   6/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -5,   5/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -9,   7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -9,   9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -8,  11/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -6,  12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -4,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -2,  11/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   1,  10/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   4,  10/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   7,  11/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   9,  12/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   5,  -2/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   3,  -3/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,  -5/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   2,  -7/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   4,  -9/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   6,  -9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   8,  -8/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   9,  -6/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   9,  -4/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   7,  -2/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   5,  -2/
C
      DATA IXMIND(  52)/ -12/
      DATA IXMAXD(  52)/  12/
      DATA IXDELD(  52)/  24/
      DATA ISTARD(  52)/   1/
      DATA NUMCOO(  52)/  29/
C
C     DEFINE CHARACTER  2272--& (AMPERSAND)
C
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',   9,   4/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   8,   3/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   9,   2/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  10,   3/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  10,   4/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   9,   5/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   8,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   7,   4/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   6,   2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   4,  -3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   2,  -6/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   0,  -8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -2,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -5,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -8,  -8/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -9,  -6/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -9,  -3/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -8,  -1/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -2,   3/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   0,   5/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   1,   7/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   1,   9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   0,  11/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -2,  12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -4,  11/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -5,   9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -5,   7/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -4,   4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -2,   1/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   3,  -6/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   5,  -8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   8,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   9,  -9/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  10,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  10,  -7/
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -5,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -7,  -8/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -8,  -6/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -8,  -3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -7,  -1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -5,   1/
      DATA IOPERA(  71),IX(  71),IY(  71)/'MOVE',  -5,   7/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -4,   5/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   4,  -6/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   6,  -8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   8,  -9/
C
      DATA IXMIND(  53)/ -12/
      DATA IXMAXD(  53)/  13/
      DATA IXDELD(  53)/  25/
      DATA ISTARD(  53)/  30/
      DATA NUMCOO(  53)/  46/
C
C     DEFINE CHARACTER  2273--@ (AT SIGN)
C
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   5,   4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   4,   6/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   2,   7/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -1,   7/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -3,   6/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -4,   5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -5,   2/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -5,  -1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -4,  -3/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -2,  -4/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   1,  -4/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   3,  -3/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   4,  -1/
      DATA IOPERA(  89),IX(  89),IY(  89)/'MOVE',  -1,   7/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -3,   5/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -4,   2/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -4,  -1/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -3,  -3/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -2,  -4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',   5,   7/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   4,  -1/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   4,  -3/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   6,  -4/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   8,  -4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  10,  -2/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  11,   1/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  11,   3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  10,   6/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   9,   8/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   7,  10/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   5,  11/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   2,  12/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -1,  12/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -4,  11/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -6,  10/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -8,   8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -9,   6/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW', -10,   3/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW', -10,   0/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -9,  -3/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -8,  -5/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -6,  -7/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -4,  -8/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -1,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   2,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   5,  -8/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   7,  -7/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   8,  -6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   6,   7/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   5,  -1/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   5,  -3/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   6,  -4/
C
      DATA IXMIND(  54)/ -13/
      DATA IXMAXD(  54)/  14/
      DATA IXDELD(  54)/  27/
      DATA ISTARD(  54)/  76/
      DATA NUMCOO(  54)/  52/
C
C     DEFINE CHARACTER  2274--$ (DOLLAR SIGN)
C
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -2,  16/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -2, -13/
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',   2,  16/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   2, -13/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   6,   9/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   5,   8/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   6,   7/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   7,   8/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   7,   9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   5,  11/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   2,  12/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -2,  12/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -5,  11/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -7,   9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -7,   7/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -6,   5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -5,   4/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -3,   3/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   3,   1/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   5,   0/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   7,  -2/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -7,   7/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -5,   5/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -3,   4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   3,   2/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   5,   1/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   6,   0/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   7,  -2/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   7,  -6/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   5,  -8/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   2,  -9/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -2,  -9/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -5,  -8/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -7,  -6/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -7,  -5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6,  -4/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -5,  -5/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -6,  -6/
C
      DATA IXMIND(  55)/ -10/
      DATA IXMAXD(  55)/  10/
      DATA IXDELD(  55)/  20/
      DATA ISTARD(  55)/ 128/
      DATA NUMCOO(  55)/  38/
C
C     DEFINE CHARACTER  2275--# (NUMBER SIGN)
C
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   1,  12/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -6, -16/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   7,  12/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0, -16/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',  -6,   1/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   8,   1/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',  -7,  -5/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   7,  -5/
C
      DATA IXMIND(  56)/ -10/
      DATA IXMAXD(  56)/  11/
      DATA IXDELD(  56)/  21/
      DATA ISTARD(  56)/ 166/
      DATA NUMCOO(  56)/   8/
C
C     DEFINE CHARACTER  2276--PARAGRAP (PARAGRAPH SYMBOL)
C
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',   3,   9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   2,   8/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   3,   7/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   4,   8/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   4,   9/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   3,  11/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   1,  12/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -1,  12/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -3,  11/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -4,   9/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -4,   7/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -3,   5/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -1,   3/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   4,   0/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',  -3,   5/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   2,   2/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   4,   0/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   5,  -2/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   5,  -4/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   4,  -6/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   2,  -8/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',  -2,   4/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -4,   2/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -5,   0/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -5,  -2/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -4,  -4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -2,  -6/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   3,  -9/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',  -4,  -4/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   1,  -7/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   3,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   4, -11/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   4, -13/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   3, -15/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   1, -16/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -1, -16/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -3, -15/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -4, -13/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -4, -12/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -3, -11/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -2, -12/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -3, -13/
C
      DATA IXMIND(  57)/  -8/
      DATA IXMAXD(  57)/   8/
      DATA IXDELD(  57)/  16/
      DATA ISTARD(  57)/ 174/
      DATA NUMCOO(  57)/  42/
C
C     DEFINE CHARACTER  2277--DAGGER (DAGGER)
C
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   0,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -1,  10/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   0,   8/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   1,  10/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   0,  12/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',   0,  12/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   0, -16/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',   0,   1/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -1,  -2/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   0, -16/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   1,  -2/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   0,   1/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',  -6,   5/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -4,   4/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -2,   5/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -4,   6/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -6,   5/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',  -6,   5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   6,   5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',   2,   5/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   4,   4/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   6,   5/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   4,   6/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   2,   5/
C
      DATA IXMIND(  58)/  -8/
      DATA IXMAXD(  58)/   8/
      DATA IXDELD(  58)/  16/
      DATA ISTARD(  58)/ 216/
      DATA NUMCOO(  58)/  24/
C
C     DEFINE CHARACTER  2278--DDAGGER (DOUBLE DAGGER)
C
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',   0,  12/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -1,  10/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   0,   8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   1,  10/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   0,  12/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',   0,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   0,  -2/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',   0,   2/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -1,   0/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   1,  -4/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   0,  -6/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -1,  -4/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   1,   0/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   0,   2/
      DATA IOPERA( 254),IX( 254),IY( 254)/'MOVE',   0,  -2/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',   0, -16/
      DATA IOPERA( 256),IX( 256),IY( 256)/'MOVE',   0, -12/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -1, -14/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   0, -16/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   1, -14/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   0, -12/
      DATA IOPERA( 261),IX( 261),IY( 261)/'MOVE',  -6,   5/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -4,   4/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -2,   5/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',  -4,   6/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -6,   5/
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',  -6,   5/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   6,   5/
      DATA IOPERA( 268),IX( 268),IY( 268)/'MOVE',   2,   5/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   4,   4/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   6,   5/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   4,   6/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   2,   5/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',  -6,  -9/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -4, -10/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -2,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -4,  -8/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -6,  -9/
      DATA IOPERA( 278),IX( 278),IY( 278)/'MOVE',  -6,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   6,  -9/
      DATA IOPERA( 280),IX( 280),IY( 280)/'MOVE',   2,  -9/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   4, -10/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   6,  -9/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   4,  -8/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   2,  -9/
C
      DATA IXMIND(  59)/  -8/
      DATA IXMAXD(  59)/   8/
      DATA IXDELD(  59)/  16/
      DATA ISTARD(  59)/ 240/
      DATA NUMCOO(  59)/  45/
C
C     DEFINE CHARACTER  2279--THEREEXI (THERE EXISTS SIGN)
C
      DATA IOPERA( 285),IX( 285),IY( 285)/'MOVE',   6,  12/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   6,  -9/
      DATA IOPERA( 287),IX( 287),IY( 287)/'MOVE',  -7,  12/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   6,  12/
      DATA IOPERA( 289),IX( 289),IY( 289)/'MOVE',  -2,   2/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',   6,   2/
      DATA IOPERA( 291),IX( 291),IY( 291)/'MOVE',  -7,  -9/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',   6,  -9/
C
      DATA IXMIND(  60)/  -9/
      DATA IXMAXD(  60)/  10/
      DATA IXDELD(  60)/  19/
      DATA ISTARD(  60)/ 285/
      DATA NUMCOO(  60)/   8/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DMATH3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IFOUND,IERROR
   59 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
C               *********************************************
C               **  STEP 3--                               **
C               **  ADJUST THE COORDINATES IF A CIRCLE.    **
C               **  IF THE CHARACTER IS A CIRCLE           **
C               **  THEN SCALE THE FIGURE DOWN FROM        **
C               **  -17 TO 17  TO THE MORE USUAL -7 TO 7.  **
C               **  THE ORIGINAL CIRCLE WAS FROM -17 TO 17 **
C               **  RATHER THAN -7 TO 7 IN                 **
C               **  ORDER TO INCREASE THE RESOLUTION       **
C               **  AND GIVE A 32 POINT CIRCLE RATHER      **
C               **  THAN A 16 POINT CIRCLE.                **
C               *********************************************
C
      IF(ICHARN.EQ.102)GOTO1210
      GOTO1290
C
 1210 CONTINUE
      AFACTO=7.0/17.0
      DO1220J=1,NUMCO
      X(J)=X(J)*AFACTO
      Y(J)=Y(J)*AFACTO
 1220 CONTINUE
      IXMINS=(-7)
      IXMAXS=7
      IXDELS=14
C
 1290 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DMATH3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG4,ISUBG4,IFOUND,IERROR
 9012 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DMATH4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR MATH SYMBOLS (PART 4).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C     UPDATED         --APRIL     1987.
C     UPDATED         --AUGUST    1992.  ADD ARROW CHARACTER.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IOP
      CHARACTER*4 IBUGD2
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IOPERA
C
C---------------------------------------------------------------------
C
      DIMENSION IOP(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION IOPERA(300)
      DIMENSION IX(300)
      DIMENSION IY(300)
C
      DIMENSION IXMIND(150)
      DIMENSION IXMAXD(150)
      DIMENSION IXDELD(150)
      DIMENSION ISTARD(150)
      DIMENSION NUMCOO(150)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C     DEFINE CHARACTER  2401--PRODUCT (PRODUCT SIGN)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -10,  16/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -10, -16/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',  -9,  16/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -9, -16/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   9,  16/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   9, -16/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  10,  16/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  10, -16/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE', -14,  16/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  14,  16/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE', -14, -16/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -5, -16/
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',   5, -16/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  14, -16/
C
      DATA IXMIND(  61)/ -17/
      DATA IXMAXD(  61)/  17/
      DATA IXDELD(  61)/  34/
      DATA ISTARD(  61)/   1/
      DATA NUMCOO(  61)/  14/
C
C     DEFINE CHARACTER  2402--SUMMATION (SUMMATION SIGN)
C
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE', -11,  16/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -1,   2/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW', -12, -16/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE', -12,  16/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,   2/
      DATA IOPERA(  20),IX(  20),IY(  20)/'MOVE', -13,  16/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,   1/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE', -13,  16/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  10,  16/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  12,   9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   9,  16/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE', -11, -15/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  10, -15/
      DATA IOPERA(  28),IX(  28),IY(  28)/'MOVE', -12, -16/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  10, -16/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  12,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   9, -16/
C
      DATA IXMIND(  62)/ -16/
      DATA IXMAXD(  62)/  15/
      DATA IXDELD(  62)/  31/
      DATA ISTARD(  62)/  15/
      DATA NUMCOO(  62)/  17/
C
C     DEFINE CHARACTER  2740--THEREEXI (THERE EXISTS SIGN)
C
      DATA IOPERA(  32),IX(  32),IY(  32)/'MOVE',   0,   9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -1,   8/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   0,   7/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   1,   8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   0,   9/
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',  -9,  -7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW', -10,  -8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -9,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -8,  -8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -9,  -7/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   9,  -7/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   8,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   9,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  10,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   9,  -7/
C
      DATA IXMIND(  63)/ -13/
      DATA IXMAXD(  63)/  13/
      DATA IXDELD(  63)/  26/
      DATA ISTARD(  63)/  32/
      DATA NUMCOO(  63)/  15/
C
C     DEFINE CHARACTER  XX--LVBAR (LONGER VERTICAL BAR)
C
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',   0,  20/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   0, -20/
C
      DATA IXMIND(  64)/  -2/
      DATA IXMAXD(  64)/   2/
      DATA IXDELD(  64)/   4/
      DATA ISTARD(  64)/  47/
      DATA NUMCOO(  64)/   2/
C
C     DEFINE CHARACTER  2800--HBAR (HORIZONTAL BAR)
C
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE', -14,   0/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  14,   0/
C
      DATA IXMIND(  65)/ -14/
      DATA IXMAXD(  65)/  14/
      DATA IXDELD(  65)/  28/
      DATA ISTARD(  65)/  49/
      DATA NUMCOO(  65)/   2/
C
C     DEFINE CHARACTER  2796--LHBAR (LONGER HORIZONTAL BAR)
C
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE', -20,   0/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  20,   0/
C
      DATA IXMIND(  66)/ -20/
      DATA IXMAXD(  66)/  20/
      DATA IXDELD(  66)/  40/
      DATA ISTARD(  66)/  51/
      DATA NUMCOO(  66)/   2/
C
C     DEFINE CHARACTER  XXX--CENTERED POINT
C
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',0,0/
C
      DATA IXMIND(101)/-2/
      DATA IXMAXD(101)/2/
      DATA IXDELD(101)/4/
      DATA ISTARD(101)/  53/
      DATA NUMCOO(101)/1/
C
C     DEFINE CHARACTER  905--CIRCLE
C
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',-2,17/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',-6,16/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',-8,15/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',-11,13/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',-13,11/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',-15,8/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',-16,6/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',-17,2/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',-17,-2/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',-16,-6/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',-15,-8/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',-13,-11/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',-11,-13/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',-8,-15/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',-6,-16/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',-2,-17/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',2,-17/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',6,-16/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',8,-15/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',11,-13/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',13,-11/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',15,-8/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',16,-6/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',17,-2/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',17,2/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',16,6/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',15,8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',13,11/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',11,13/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',8,15/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',6,16/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',2,17/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',-2,17/
C
      DATA IXMIND(102)/-17/
      DATA IXMAXD(102)/17/
      DATA IXDELD(102)/34/
      DATA ISTARD(102)/  54/
      DATA NUMCOO(102)/33/
C
C     DEFINE CHARACTER  841--SQUARE
C
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE',-6,-6/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',6,-6/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',6,6/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',-6,6/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',-6,-6/
C
      DATA IXMIND(103)/-6/
      DATA IXMAXD(103)/6/
      DATA IXDELD(103)/12/
      DATA ISTARD(103)/  87/
      DATA NUMCOO(103)/5/
C
C     DEFINE CHARACTER  842--TRIANGLE
C
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',0,8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',-7,-4/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',7,-4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',0,8/
C
      DATA IXMIND(104)/-7/
      DATA IXMAXD(104)/7/
      DATA IXDELD(104)/14/
      DATA ISTARD(104)/  92/
      DATA NUMCOO(104)/4/
C
C     DEFINE CHARACTER  843--DIAMOND
C
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',0,10/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',-6,0/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',0,-10/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',6,0/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',0,10/
C
      DATA IXMIND(105)/-6/
      DATA IXMAXD(105)/6/
      DATA IXDELD(105)/12/
      DATA ISTARD(105)/  96/
      DATA NUMCOO(105)/5/
C
C     DEFINE CHARACTER  844--STAR
C
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',0,9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',-2,3/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',-8,3/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',-3,-1/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',-5,-7/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',0,-3/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',5,-7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',3,-1/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',8,3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',2,3/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',0,9/
C
      DATA IXMIND(106)/-8/
      DATA IXMAXD(106)/8/
      DATA IXDELD(106)/16/
      DATA ISTARD(106)/ 101/
      DATA NUMCOO(106)/11/
C
C     DEFINE CHARACTER  847--ASTERISK
C
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',0,6/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',0,-6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',-5,3/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',5,-3/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',5,3/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',-5,-3/
C
      DATA IXMIND(107)/-5/
      DATA IXMAXD(107)/5/
      DATA IXDELD(107)/10/
      DATA ISTARD(107)/ 112/
      DATA NUMCOO(107)/6/
C
C     DEFINE CHARACTER  XXX--REVERSE TRIANGLE
C
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',0,-8/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',-7,4/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',7,4/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',0,-8/
C
      DATA IXMIND(108)/-7/
      DATA IXMAXD(108)/7/
      DATA IXDELD(108)/14/
      DATA ISTARD(108)/ 118/
      DATA NUMCOO(108)/4/
C
C     DEFINE CHARACTER  XX--VERTICAL BAR
C
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',0,8/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',0,-8/
C
      DATA IXMIND(109)/-2/
      DATA IXMAXD(109)/2/
      DATA IXDELD(109)/4/
      DATA ISTARD(109)/ 122/
      DATA NUMCOO(109)/2/
C
C     DEFINE CHARACTER  XX--HORIZONTAL BAR
C
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',-8,0/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',8,0/
C
      DATA IXMIND(110)/-8/
      DATA IXMAXD(110)/8/
      DATA IXDELD(110)/16/
      DATA ISTARD(110)/ 124/
      DATA NUMCOO(110)/2/
C
C     DEFINE CHARACTER  2262--UP ARROW
C
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',-2,6/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',0,9/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',2,6/
      DATA IOPERA( 129),IX( 129),IY( 129)/'MOVE',-5,3/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',0,8/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',5,3/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',0,8/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',0,-9/
C
      DATA IXMIND(111)/-8/
      DATA IXMAXD(111)/8/
      DATA IXDELD(111)/16/
      DATA ISTARD(111)/ 126/
      DATA NUMCOO(111)/8/
C
C     DEFINE CHARACTER  2264--DOWN ARROW
C
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',-2,-6/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',0,-9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',2,-6/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',-5,-3/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',0,-8/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',5,-3/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',0,9/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',0,-8/
C
      DATA IXMIND(112)/-8/
      DATA IXMAXD(112)/8/
      DATA IXDELD(112)/16/
      DATA ISTARD(112)/ 134/
      DATA NUMCOO(112)/8/
C
C     DEFINE CHARACTER  2263--LEFT ARROW
C
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',-6,2/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',-9,0/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',-6,-2/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',-3,5/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',-8,0/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',-3,-5/
      DATA IOPERA( 148),IX( 148),IY( 148)/'MOVE',-8,0/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',9,0/
C
      DATA IXMIND(113)/-13/
      DATA IXMAXD(113)/13/
      DATA IXDELD(113)/26/
      DATA ISTARD(113)/ 142/
      DATA NUMCOO(113)/8/
C
C     DEFINE CHARACTER  2261--RIGHT ARROW
C
      DATA IOPERA( 150),IX( 150),IY( 150)/'MOVE',6,2/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',9,0/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',6,-2/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',3,5/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',8,0/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',3,-5/
      DATA IOPERA( 156),IX( 156),IY( 156)/'MOVE',-9,0/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',8,0/
C
      DATA IXMIND(114)/-13/
      DATA IXMAXD(114)/13/
      DATA IXDELD(114)/26/
      DATA ISTARD(114)/ 150/
      DATA NUMCOO(114)/8/
C
C     DEFINE CHARACTER  804--BACK SLASH
C
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',-7,12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',7,-12/
C
      DATA IXMIND(115)/-7/
      DATA IXMAXD(115)/7/
      DATA IXDELD(115)/14/
      DATA ISTARD(115)/ 158/
      DATA NUMCOO(115)/2/
C
C     DEFINE CHARACTER  XX--UNDERSCORE
C
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',-8,-10/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',8,-10/
C
      DATA IXMIND(116)/-8/
      DATA IXMAXD(116)/8/
      DATA IXDELD(116)/16/
      DATA ISTARD(116)/ 160/
      DATA NUMCOO(116)/2/
C
C     DEFINE CHARACTER  XXX--CUBE
C
      DATA IOPERA(  162),IX(  162),IY(  162)/'MOVE',-6,-6/
      DATA IOPERA(  163),IX(  163),IY(  163)/'DRAW',6,-6/
      DATA IOPERA(  164),IX(  164),IY(  164)/'DRAW',6,6/
      DATA IOPERA(  165),IX(  165),IY(  165)/'DRAW',-6,6/
      DATA IOPERA(  166),IX(  166),IY(  166)/'DRAW',-6,-6/
      DATA IOPERA(  167),IX(  167),IY(  167)/'MOVE',-6,6/
      DATA IOPERA(  168),IX(  168),IY(  168)/'DRAW',-4,8/
      DATA IOPERA(  169),IX(  169),IY(  169)/'DRAW',8,8/
      DATA IOPERA(  170),IX(  170),IY(  170)/'DRAW',6,6/
      DATA IOPERA(  171),IX(  171),IY(  171)/'MOVE',8,8/
      DATA IOPERA(  172),IX(  172),IY(  172)/'DRAW',8,-2/
      DATA IOPERA(  173),IX(  173),IY(  173)/'DRAW',6,-6/
C
      DATA IXMIND(117)/-6/
      DATA IXMAXD(117)/8/
      DATA IXDELD(117)/14/
      DATA ISTARD(117)/ 162/
      DATA NUMCOO(117)/12/
C
C     DEFINE CHARACTER  XXX--PYRAMID
C
      DATA IOPERA(  174),IX(  174),IY(  174)/'MOVE',0,8/
      DATA IOPERA(  175),IX(  175),IY(  175)/'DRAW',-7,-4/
      DATA IOPERA(  176),IX(  176),IY(  176)/'DRAW',7,-4/
      DATA IOPERA(  177),IX(  177),IY(  177)/'DRAW',0,8/
      DATA IOPERA(  178),IX(  178),IY(  178)/'DRAW',6,2/
      DATA IOPERA(  179),IX(  179),IY(  179)/'DRAW',7,-4/
C
      DATA IXMIND(118)/-7/
      DATA IXMAXD(118)/7/
      DATA IXDELD(118)/14/
      DATA ISTARD(118)/ 174/
      DATA NUMCOO(118)/6/
C
C     AUGUST 1992.  ADD ARROW.  USE TRIANGLE COORDINATES FOR
C     NOW, MAY MODIFY LATER AS NEEDED (MODIFY SO THAT ARROW
C     COMES TO A POINT AT 0,0 SO ARROW HEAD IS AT CENTER POINT)
C     DEFINE CHARACTER  XXX--ARROW
C
      DATA IOPERA(  180),IX(  180),IY(  180)/'MOVE',0,0/
      DATA IOPERA(  181),IX(  181),IY(  181)/'DRAW',-7,-12/
      DATA IOPERA(  182),IX(  182),IY(  182)/'DRAW',7,-12/
      DATA IOPERA(  183),IX(  183),IY(  183)/'DRAW',0,0/
C
      DATA IXMIND(119)/-7/
      DATA IXMAXD(119)/7/
      DATA IXDELD(119)/14/
      DATA ISTARD(119)/ 180/
      DATA NUMCOO(119)/4/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      NUMCO=1
      ISTART=1
      ISTOP=1
      NC=1
C
C               ******************************************
C               **  TREAT THE ROMAN SIMPLEX UPPER CASE  **
C               **  HERSHEY CHARACTER SET CASE          **
C               ******************************************
C
C
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DMATH4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGG4,ISUBG4,IFOUND,IERROR
   59 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **************************************
C               **  STEP 2--                        **
C               **  EXTRACT THE COORDINATES         **
C               **  FOR THIS PARTICULAR CHARACTER.  **
C               **************************************
C
 1000 CONTINUE
      ISTART=ISTARD(ICHARN)
      NC=NUMCOO(ICHARN)
      ISTOP=ISTART+NC-1
      J=0
      DO1100I=ISTART,ISTOP
      J=J+1
      IOP(J)=IOPERA(I)
      X(J)=IX(I)
      Y(J)=IY(I)
 1100 CONTINUE
      NUMCO=J
      IXMINS=IXMIND(ICHARN)
      IXMAXS=IXMAXD(ICHARN)
      IXDELS=IXDELD(ICHARN)
C
C               *********************************************
C               **  STEP 3--                               **
C               **  ADJUST THE COORDINATES IF A CIRCLE.    **
C               **  IF THE CHARACTER IS A CIRCLE           **
C               **  THEN SCALE THE FIGURE DOWN FROM        **
C               **  -17 TO 17  TO THE MORE USUAL -7 TO 7.  **
C               **  THE ORIGINAL CIRCLE WAS FROM -17 TO 17 **
C               **  RATHER THAN -7 TO 7 IN                 **
C               **  ORDER TO INCREASE THE RESOLUTION       **
C               **  AND GIVE A 32 POINT CIRCLE RATHER      **
C               **  THAN A 16 POINT CIRCLE.                **
C               *********************************************
C
      IF(ICHARN.EQ.102)GOTO1210
      GOTO1290
C
 1210 CONTINUE
      AFACTO=7.0/17.0
      DO1220J=1,NUMCO
      X(J)=X(J)*AFACTO
      Y(J)=Y(J)*AFACTO
 1220 CONTINUE
      IXMINS=(-7)
      IXMAXS=7
      IXDELS=14
C
 1290 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'OFF'.AND.ISUBG4.NE.'MATH')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DMATH4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG4,ISUBG4,IFOUND,IERROR
 9012 FORMAT('IBUGG4,ISUBG4,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
      DO9015I=1,NUMCO
      WRITE(ICOUT,9016)I,IOP(I),X(I),Y(I)
 9016 FORMAT('I,IOP(I),X(I),Y(I) = ',I8,2X,A4,2F10.2)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9019 CONTINUE
      WRITE(ICOUT,9021)IXMINS,IXMAXS,IXDELS
 9021 FORMAT('IXMINS,IXMAXS,IXDELS = ',3I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DNBRAN(N,ALPHA,BETA,ALAMB1,ALAMB2,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE DOUBLY NON-CENTRAL BETA DISTRIBUTION WITH SHAPE
C              PARAMETERS ALPHA AND BETA AND NON-CENTRALITY
C              PARAMETERS LAMBDA1 AND LAMBDA2.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --ALPHA  = THE SINGLE PRECISION VALUE OF THE
C                                FIRST  SHAPE PARAMETER.
C                     --BETA   = THE SINGLE PRECISION VALUE OF THE
C                                SECOND SHAPE PARAMETER.
C                     --ALAMB1 = THE SINGLE PRECISION VALUE OF THE
C                                FIRST NON-CENTRALITY PARAMETER.
C                     --ALAMB2 = THE SINGLE PRECISION VALUE OF THE
C                                SECOND NON-CENTRALITY PARAMETER.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE DOUBLY NON-CENTRAL BETA DISTRIBUTION
C             WITH SHAPE PARAMETER VALUES = ALPHA, BETA, ALAMB1, AND
C             ALAMB2.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --ALPHA AND BETA  SHOULD BE POSITIVE.
C                 --ALAMB1 AND ALAMB2 SHOULD BE NON-NEGATIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NCCRAN, CHSRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KOTZ, AND BALAKRISHNAN, "CONTINUOUS
C                 UNIVARIATE DISTRIBUTIONS VOLUME 2", SECOND EDITION,
C                 1994, PAGES 502-503.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.5
C     ORIGINAL VERSION--MAY       2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(1)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
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
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF DOUBLY NON-CENTRAL',
     1' BETA RANDOM NUMBERS IS NON-POSITIVE.')
      IF(ALPHA.LE.0.0)THEN
        WRITE(ICOUT,16)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALPHA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   16 FORMAT('***** ERROR--THE SHAPE PARAMETER ALPHA FOR THE ',
     1'DOUBLY NON-CENTRAL BETA RANDOM NUMBERS IS NON-POSITIVE.')
      IF(BETA.LE.0.0)THEN
        WRITE(ICOUT,26)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)BETA
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   26 FORMAT('***** ERROR--THE SHAPE PARAMETER BETA FOR THE ',
     1'DOUBLY NON-CENTRAL BETA RANDOM NUMBERS IS NON-POSITIVE.')
      IF(ALAMB1.LT.0.0)THEN
        WRITE(ICOUT,36)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,37)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB1
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   36 FORMAT('***** ERROR--THE DOUBLY NON-CENTRALITY PARAMETER ',
     1       'LAMBDA1')
   37 FORMAT('      FOR THE DOUBLY NON-CENTRAL BETA RANDOM NUMBERS ',
     1       'IS NEGATIVE.')
      IF(ALAMB2.LT.0.0)THEN
        WRITE(ICOUT,38)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,39)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB2
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   38 FORMAT('***** ERROR--THE DOUBLY NON-CENTRALITY PARAMETER ',
     1       'LAMBDA2')
   39 FORMAT('      FOR THE DOUBLY NON-CENTRAL BETA RANDOM NUMBERS ',
     1       'IS NEGATIVE.')
C
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C     USE THE CENTRAL AND NON-CENTRAL CHI-SQUARE RANDOM NUMBER
C     ROUTINE TO GENERATE NON-CENTRAL BETA RANDOM NUMBERS.
C
C     NCB = NCCHISQ(NU1,LAMBDA)/(NCCHISQ(NU1,LAMBDA)+NCCHISQUARE(NU2))
C
      ANU1=ALPHA
      ANU2=BETA
      NTEMP=1
      DO100I=1,N
        CALL NCCRAN(NTEMP,ANU1,ALAMB1,ISEED,XTEMP)
        TERM1=XTEMP(1)
        CALL NCCRAN(NTEMP,ANU2,ALAMB2,ISEED,XTEMP)
        TERM2=XTEMP(1)
        X(I)=TERM1/(TERM1+TERM2)
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DNFCDF(X,DF1,DF2,ALAMB1,ALAMB2,CDF)
C
C     PURPOSE--THIS SUBROUTINE COMPUTES THE CUMULATIVE DISTRIBUTION
C              FUNCTION VALUE FOR THE DOUBLY NON-CENTRAL F DISTRIBUTION
C              WITH REAL DEGREES OF FREEDOM
C              PARAMETERS = NU1 AND NU2.
C              THIS DISTRIBUTION IS DEFINED FOR ALL NON-NEGATIVE X.
C---------------------------------------------------------------------
C   CDFDNF   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
C            DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY, GAITHERSBURG,
C            MARYLAND  20899
C
C   FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE DOUBLY 
C        NONCENTRAL F DISTRIBUTION TO A SPECIFIED ACCURACY (TRUNCATION
C        ERROR IN THE INFINITE SERIES REPRESENTATION GIVEN BY EQUATION
C        2.2 IN REFERENCE 1 BELOW).  THE BETA C.D.F. ROUTINE IS CALLED
C        AT MOST TWO TIMES.  FURTHER VALUES OF THE BETA C.D.F. ARE
C        OBTAINED FROM RECURRENCE RELATIONS GIVEN IN REFERENCE 2.
C        REFERENCE 3 GIVES A DETAILED DESCRIPTION OF THE ALGORITHM
C        HEREIN.
C
C        THIS PROGRAM MAY ALSO BE EFFICIENTLY USED TO COMPUTE THE
C        CUMULATIVE DISTRIBUTION FUNCTIONS OF THE SINGLY NONCENTRAL
C        AND CENTRAL F DISTRIBUTIONS BY SETTING THE APPROPRIATE
C        NONCENTRALITY PARAMETERS EQUAL TO ZERO.
C
C        CHECKS ARE MADE TO ASSURE THAT ALL PASSED PARAMETERS ARE
C        WITHIN VALID RANGES AS GIVEN BELOW.  NO UPPER LIMIT IS SET
C        FOR THE NONCENTRALITY PARAMETERS, BUT VALUES UP TO ABOUT
C        10,000 CAN BE HANDLED WITH THE CURRENT DIMENSION LIMITS.  THE
C        COMPUTED VALUE CDFX IS VALID ONLY IF IFLAG=0 ON RETURN.
C
C   NOTE: IN EQUATION 2.2 OF REFERENCE 1 THE AUTHOR HAS MISTAKENLY
C         REVERSED THE ARGUMENTS OF THE INCOMPLETE BETA FUNCTION.
C         THEY SHOULD READ [(M/2)+R,(N/2+S)] WHERE M AND N ARE THE
C         DEGREES OF FREEDOM ASSOCIATED WITH THE NUMERATOR AND
C         DENOMINATOR RESPECTIVELY OF THE F STATISTIC.  TO FURTHER
C         CONFUSE THE ISSUE, THE AUTHOR HAS REVERSED THE USAGE OF
C         M AND N IN SECTION 1 OF THE PAPER.
C
C   NOTE: IN SUBROUTINE EDGEF THE DOUBLE PRECISION CONSTANT DEUFLO IS 
C         THE EXPONENTIAL UNDERFLOW LIMIT WHOSE CURRENT VALUE IS SET
C         AT -69D0.  ON A COMPUTER WHERE DEXP(-69D0) CAUSES UNDERFLOW 
C         THIS LIMIT SHOULD BE CHANGED. 
C
C   SUBPROGRAMS CALLED: CDFBET (BETA C.D.F.)
C                       DGAMLN (DOUBLE PRECISION LOG OF GAMMA FUNCTION)
C                       POISSF, EDGEF (ATTACHED)
C
C   CURRENT VERSION COMPLETED SEPTEMBER 29, 1988
C
C   REFERENCES: 
C
C   1. BULGREN, W.G., 'ON REPRESENTATIONS OF THE DOUBLY NONCENTRAL F
C      DISTRIBUTION', JOURNAL OF THE AMERICAN STATISTICAL ASSOCIATION,
C      MARCH 1971, VOLUME 66, NO. 333, PP. 184-186.
C
C   2. ABRAMOWITZ, MILTON, AND STEGUN, IRENE A., 'HANDBOOK OF
C      MATHEMATICAL FUNCTIONS', NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED
C      MATHEMATICS SERIES 55, NOVEMBER 1970, P. 944.
C
C   3. REEVE, CHARLES P., 'AN ALGORITHM FOR COMPUTING THE DOUBLY
C      NONCENTRAL F C.D.F. TO A SPECIFIED ACCURACY', STATISTICAL
C      ENGINEERING DIVISION NOTE 86-4, NOVEMBER 1986.
C---------------------------------------------------------------------
C   DEFINITION OF PASSED PARAMETERS: 
C
C        * X = VALUE (>=0) AT WHICH THE C.D.F. IS TO BE COMPUTED (REAL)
C
C      * DF1 = DEGREES OF FREEDOM (>0) IN THE NUMERATOR (REAL)
C
C      * DF2 = DEGREES OF FREEDOM (>0) IN THE DENOMINATOR (REAL)
C
C   * ALAMB1 = THE NONCENTRALITY PARAMETER (>=0) FOR THE NUMERATOR
C              (REAL) [EQUAL TO ZERO FOR THE CENTRAL F DISTRIBUTION]
C
C   * ALAMB2 = THE NONCENTRALITY PARAMETER (>=0) FOR THE DENOMINATOR
C              (REAL) [EQUAL TO ZERO FOR THE SINGLY NONCENTRAL F AND
C              CENTRAL F DISTRIBUTIONS] 
C
C      * EPS = THE DESIRED ABSOLUTE ACCURACY OF THE C.D.F. (REAL)
C              [1 >= EPS >= 10**(-10)]
C
C      IFLAG = ERROR INDICATOR ON OUTPUT (INTEGER)   INTERPRETATION:  
C                0 -> NO ERRORS DETECTED
C              1,2 -> ERROR FLAGS FROM SUBROUTINE CDFBET
C                3 -> EITHER ALAMB1 OR ALAMB2 IS < 0
C                4 -> EITHER DF1 OR DF2 IS <= 0
C                5 -> EPS IS OUTSIDE THE RANGE [10**(-10),1]
C                6 -> VECTOR DIMENSIONS ARE TOO SMALL - INCREASE NX
C
C       CDFX = THE DOUBLY NONCENTRAL F C.D.F. EVALUATED AT X (REAL)
C
C   * INDICATES PARAMETERS REQUIRING INPUT VALUES 
C---------------------------------------------------------------------
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--94/9
C     ORIGINAL VERSION--SEPTEMBER 1994.
C
      PARAMETER (NX=1000)
      DIMENSION BFI(NX),BFJ(NX),POI(NX),POJ(NX)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      CDF=0.0
      IF(DF1.LE.0.0)GOTO50
      IF(DF2.LE.0.0)GOTO55
      IF(X.LT.0.0)GOTO60
      IF(ALAMB1.LT.0.0)GOTO70
      IF(ALAMB2.LT.0.0)GOTO80
      IF(ALAMB1.GT.10000.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB1
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  301 FORMAT('**** FATAL DIAGNOSTIC--THE FIRST NON-CENTRALITY ',
     *       'PARAMETER HAS A VALUE GREATER THAN 10000.')
      IF(ALAMB2.GT.10000.0)THEN
        WRITE(ICOUT,303)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB2
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  303 FORMAT('**** FATAL DIAGNOSTIC--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER HAS A VALUE GREATER THAN 10000.')
      GOTO90
   50 WRITE(ICOUT,15)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)DF1
      CALL DPWRST('XXX','BUG ')
      CDF=0.0
      RETURN
   55 WRITE(ICOUT,23)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,47)DF2
      CALL DPWRST('XXX','BUG ')
      CDF=0.0
      RETURN
   60 WRITE(ICOUT,4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)X
      CALL DPWRST('XXX','BUG ')
      CDF=0.0
      RETURN
   70 WRITE(ICOUT,24)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)ALAMB1
      CALL DPWRST('XXX','BUG ')
      CDF=0.0
      RETURN
   80 WRITE(ICOUT,25)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)ALAMB2
      CALL DPWRST('XXX','BUG ')
      CDF=0.0
      RETURN
    4 FORMAT('***** NON-FATAL DIAGNOSTIC--THE 1ST INPUT ARGUMENT ',
     1'TO THE DNFCDF SUBROUTINE IS NEGATIVE *****')
   15 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1'DNFCDF SUBROUTINE IS NON-POSITIVE *****')
   23 FORMAT('***** FATAL ERROR--THE 3RD INPUT ARGUMENT TO THE ',
     1'DNFCDF SUBROUTINE IS NON-POSITIVE *****')
   24 FORMAT('***** FATAL ERROR--THE FOURTH INPUT ARGUMENT TO THE ',
     1'DNFCDF SUBROUTINE IS NEGATIVE *****')
   25 FORMAT('***** FATAL ERROR--THE FIFTH INPUT ARGUMENT TO THE ',
     1'DNFCDF SUBROUTINE IS NEGATIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8,' *****')
C
   90 CONTINUE
C
CCCCC MAY 2004.  INCREASE ERROR CRITIERION.
CCCCC EPS=1.0E-5
      EPS=1.0E-6
C
C--- SET ERROR CRITERION FOR THE BETA C.D.F. (PECULIAR TO CDFBET)
C
      EPS3 = 0.001*EPS
C
      FA = 0.5*ALAMB1
      GA = 0.5*ALAMB2
      FB = 0.5*DF1
      GB = 0.5*DF2
      YY = DF2/(DF2+DF1*X)
      IF (YY.GE.1.0) GOTO9999
      XX = 1.0-YY
      IF (XX.GE.1.0) THEN
         CDF = 1.0 
         GOTO9999
      ENDIF
C
C--- COMPUTE POISSON PROBABILITIES IN VECTORS POI AND POJ
C
      IFLAG=0
      CALL POISSF(FA,EPS,IMIN,NI,POI,NX,IFLAG)
      IF (IFLAG.NE.0) THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('*****ERROR FROM DNFCDF--ERROR CONDITION RETURNED FROM ',
     *       'THE POISSF ROUTINE. ****')
      FC = FB+REAL(IMIN)
      CALL POISSF (GA,EPS,JMIN,NJ,POJ,NX,IFLAG)
      IF (IFLAG.NE.0) THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
      GC = GB+REAL(JMIN)
C
C--- COMPUTE BETA C.D.F. BY RECURRENCE WHEN I=IMIN AND J=JMIN TO JMAX 
C
      CALL EDGEF(NJ,GC,FC,YY,XX,BFJ,CDF,POJ,POI,EPS3,IFLAG,1)
      IF (NI.LE.1.OR.IFLAG.NE.0)THEN
        GOTO9999
      ENDIF
C
C--- COMPUTE BETA C.D.F. BY RECURRENCE WHEN J=JMIN AND I=IMIN TO IMAX 
C
      BFI(1) = BFJ(1)
      CALL EDGEF (NI,FC,GC,XX,YY,BFI,CDF,POI,POJ,EPS3,IFLAG,2)
      IF (NJ.LE.1.OR.IFLAG.NE.0)THEN
        GOTO9999
      ENDIF
C
C--- COMPUTE BETA C.D.F. BY RECURRENCE WHEN I>IMIN AND J>JMIN
C
      DO120 I = 2, NI
         BFJ(1) = BFI(I)
         DO110 J = 2, NJ
            BFJ(J) = XX*BFJ(J)+YY*BFJ(J-1)
            CDF = CDF+POI(I)*POJ(J)*BFJ(J)
  110    CONTINUE
  120 CONTINUE
C
 9999 CONTINUE
      RETURN
C
      END
      REAL FUNCTION DNFFU3(X)
C
C     PURPOSE--DNTPDF CALLS DIFF TO FIND A NUMERICAL DERIVATIVE FOR
C              THE DOUBLY NON-CENTRAL CUMULATIVE DISTRIBUTION
C              FUNCTION.  DNFFU3 IS A FUNCTION THAT CALL DNFCDF.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE DERIVATIVE
C                                IS TO BE EVALUATED.
C     OUTPUT--THE SINGLE PRECISION FUNCTION VALUE DNFFU3.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DNFCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.5
C     ORIGINAL VERSION--MAY       2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL ANU1
      REAL ANU2
      REAL ALAMB1
      REAL ALAMB2
      COMMON/DNFCOM/ANU1,ANU2,ALAMB1,ALAMB2
C
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DNFCDF(X,ANU1,ANU2,ALAMB1,ALAMB2,CDF)
      DNFFU3=CDF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DNFPDF(X,NU1,NU2,LAMBD1,LAMBD2,PDF)
C
C     PURPOSE--PROBABILITY DENSITY FUNCTION FOR THE DOUBLY NON-CENTRAL
C              F DISTRIBUTION.  THE PROBABILITY DENSITY FUNCTION
C              IS COMPUTED BY COMPUTING THE NUMERICAL DERIVATIVE OF
C              THE CUMULATIVE DISTRIBUTION FUNCTION.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --NU1    = THE FIRST DEGREES OF FREEDOM PARAMETER
C                     --NU2    = THE SECOND DEGREES OF FREEDOM PARAMETER
C                     --LAMB1  = THE FIRST NON-CENTRALITY PARAMETER
C                     --LAMB2  = THE SECOND NON-CENTRALITY PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY FUNCTION VALUE PDF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DIFF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/5
C     ORIGINAL VERSION--MAY       2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL  X
      REAL  NU1
      REAL  NU2
      REAL  LAMBD1
      REAL  LAMBD2
      REAL  PDF
C
      REAL DNFFU3
      EXTERNAL DNFFU3
      REAL ANU1
      REAL ANU2
      REAL ALAMB1
      REAL ALAMB2
      COMMON/DNFCOM/ANU1,ANU2,ALAMB1,ALAMB2
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      PDF=0.0
C
      IF(NU1.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)NU1
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('**** ERROR--THE FIRST DEGREES OF FREEDOM PARAMETER')
  102 FORMAT('     FOR DNFPDF IS NON-POSITIVE.  IT HAS THE VALUE ',
     1       E15.7)
C
      IF(NU2.LE.0.0)THEN
        WRITE(ICOUT,103)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,104)NU2
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  103 FORMAT('**** ERROR--THE SECOND DEGREES OF FREEDOM PARAMETER')
  104 FORMAT('     FOR DNFPDF IS NON-POSITIVE.  IT HAS THE VALUE ',
     1       E15.7)
C
      IF(LAMBD1.LT.0.0)THEN
        WRITE(ICOUT,303)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)LAMBD1
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  303 FORMAT('**** ERROR--THE FIRST NON-CENTRALITY PARAMETER IS ',
     1       'NEGATIVE.')
  304 FORMAT('     IT HAS THE VALUE ',E15.7)
C
      IF(LAMBD2.LT.0.0)THEN
        WRITE(ICOUT,305)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,306)LAMBD2
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  305 FORMAT('**** ERROR--THE SECOND NON-CENTRALITY PARAMETER IS ',
     1       'NEGATIVE.')
  306 FORMAT('     IT HAS THE VALUE ',E15.7)
C
C  FIND NUMERIC DERIVATIVE OF CDF ROUTINE
C
      IORD=1
      EPS=0.001
      ACCUR=0.0
      IFAIL=0
      X0 = X
      XMIN=MAX(X0 - 5.0,0.0)
      XMAX=X0 + 5.0
      ANU1=NU1
      ANU2=NU2
      ALAMB1=LAMBD1
      ALAMB2=LAMBD2
C
      CALL DIFF(IORD,X0,XMIN,XMAX,DNFFU3,EPS,ACCUR,PDF,ERROR,IFAIL)
C
      IF(IFAIL.EQ.1)THEN
  999     FORMAT(1X)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,401)
  401   FORMAT('***** WARNING IN NUMERICAL DERIVATIVE FOR DNFPDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,403)
  403   FORMAT('      THE ESTIMATED ERROR IN THE RESULT EXCEEDS THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,405)
  405   FORMAT('      REQUESTED ERROR, BUT THE MOST ACCURATE RESULT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,407)
  407   FORMAT('      POSSIBLE HAS BEEN RETURNED.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFAIL.EQ.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,411)
  411   FORMAT('***** ERROR IN NUMERICAL DERIVATIVE FOR DNFPDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,413)
  413   FORMAT('      ERROR IN THE INPUT TO THE DIFF ROUTINE.')
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ELSEIF(IFAIL.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,421)
  421   FORMAT('***** ERROR IN NUMERICAL DERIVATIVE FOR DNFPDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,423)
  423   FORMAT('      THE INTERVAL FOR DIFFERENTIATION, (',G15.7,
     1         ',',G15.7,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,425)
  425   FORMAT('      IS TOO SMALL.')
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DNFPPF(P,DF1,DF2,ALAMB1,ALAMB2,PPF)
C
C     PURPOSE   --PERCENT POINT FUNCTION FOR THE DOUBLY NON-CENTRAL F
C                 DISTRIBUTION.  USES A BISECTION METHOD.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--94/9
C     ORIGINAL VERSION--SEPTEMBER 1994.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA EPS /0.000001/
      DATA SIG /1.0E-6/
      DATA ZERO /0./
      DATA MAXIT /1000/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(P.LE.0.0.OR.P.GE.1.0)GOTO50
      IF(DF1.LT.0.0)GOTO55
      IF(DF2.LT.0.0)GOTO65
      IF(ALAMB1.LT.0.0)GOTO70
      IF(ALAMB2.LT.0.0)GOTO80
      IF(ALAMB1.GT.10000.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB1
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  301 FORMAT('**** FATAL DIAGNOSTIC--THE FIRST NON-CENTRALITY ',
     *       'PARAMETER HAS A VALUE GREATER THAN 10000.')
      IF(ALAMB2.GT.10000.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB2
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  302 FORMAT('**** FATAL DIAGNOSTIC--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER HAS A VALUE GREATER THAN 10000.')
      GOTO90
   50 WRITE(ICOUT,1)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)P
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      RETURN
   55 WRITE(ICOUT,11)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)NU1
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      RETURN
   65 WRITE(ICOUT,12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)NU2
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      RETURN
   70 WRITE(ICOUT,35)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)ALAMB1
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      GOTO9999
   80 WRITE(ICOUT,45)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,46)ALAMB2
      CALL DPWRST('XXX','BUG ')
      PPF=0.0
      GOTO9999
C
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1' DNFPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL')
   11 FORMAT('***** FATAL ERROR--THE 2ND INPUT ARGUMENT TO THE ',
     1' DNFPPF SUBROUTINE IS NON-POSITIVE.')
   12 FORMAT('***** FATAL ERROR--THE THIRD INPUT ARGUMENT TO THE ',
     1' DNFPPF SUBROUTINE IS NON-POSITIVE.')
   35 FORMAT('***** FATAL ERROR--THE FOURTH INPUT ARGUMENT TO THE ',
     1' DNFPPF SUBROUTINE IS NEGATIVE *****')
   45 FORMAT('***** FATAL ERROR--THE FIFTH INPUT ARGUMENT TO THE ',
     1' DNFPPF SUBROUTINE IS NEGATIVE *****')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8,' *****')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I15,' *****')
C
   90 CONTINUE
C
C  FIND BRACKETING INTERVAL.  USE CORRESPONDING CENTRAL F
C  AS INITIAL GUESS, INCREMENTS OF 100 AROUND IT.
C  AFTER SUCCESSFULLY FIND BRACKETING INTERVAL, THEN SWITCH TO 
C  MORE EFFICIENT BISECTION METHOD.
C
      XINC=5.0
      NU1=DF1+0.5
      NU2=DF2+0.5
      CALL FPPF(P,NU1,NU2,XL)
      ICOUNT=0
      MAXCNT=10000
C
   91 CONTINUE
      XR=XL+XINC
      IF(XL.LE.0.0)XL=0.0
      IF(XR.LE.0.0)XR=XL+1.0
      CALL DNFCDF(XL,DF1,DF2,ALAMB1,ALAMB1,CDFL)
      CALL DNFCDF(XR,DF1,DF2,ALAMB1,ALAMB2,CDFR)
      IF(CDFL.LT.P .AND. CDFR.LT.P)THEN
        XL=XR
      ELSEIF(CDFL.GT.P .AND. CDFR.GT.P)THEN
        XR=XL
        XL=XL-XINC
      ELSE
        GOTO99
      ENDIF
      ICOUNT=ICOUNT+1
      IF(ICOUNT.GT.MAXCNT)THEN
        WRITE(ICOUT,96)
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9999
      ENDIF
   96 FORMAT('***** FATAL ERROR--DNFPPF UNABLE TO FIND BRACKETING ',
     *       'INTERVAL. *****')
      GOTO91
C
C  BISECTION METHOD
C
   99 CONTINUE
      IC = 0
      FXL = -P
      FXR = 1.0 - P
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL DNFCDF(X,DF1,DF2,ALAMB1,ALAMB2,CDF)
      P1=CDF
      PPF=X
      FCS = P1 - P
      IF(FCS*FXL.GT.ZERO)GOTO110
      XR = X
      FXR = FCS 
      GOTO115
  110 CONTINUE
      XL = X
      FXL = FCS
  115 CONTINUE
      XRML = XR - XL
      IF(XRML.LE.SIG .AND. ABS(FCS).LE.EPS)GOTO9999
      IC = IC + 1
      IF(IC.LE.MAXIT)GOTO105
      WRITE(ICOUT,130)
      CALL DPWRST('XXX','BUG ')
  130 FORMAT('***** FATAL ERROR--DNFPPF ROUTINE DID NOT CONVERGE. ***')
      GOTO9999
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DNFRAN(N,ANU1,ANU2,ALAMB1,ALAMB2,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE DOUBLY NON-CENTRAL F DISTRIBUTION WITH SHAPE
C              PARAMETERS ANU1 AND ANU2 AND NON-CENTRALITY
C              PARAMETERS LAMBDA1 AND LAMBDA2.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --ANU1   = THE SINGLE PRECISION VALUE OF THE
C                                FIRST  SHAPE PARAMETER.
C                     --ANU2   = THE SINGLE PRECISION VALUE OF THE
C                                SECOND SHAPE PARAMETER.
C                     --ALAMB1 = THE SINGLE PRECISION VALUE OF THE
C                                FIRST NON-CENTRALITY PARAMETER.
C                     --ALAMB2 = THE SINGLE PRECISION VALUE OF THE
C                                SECOND NON-CENTRALITY PARAMETER.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE DOUBLY NON-CENTRAL F DISTRIBUTION
C             WITH SHAPE PARAMETER VALUES = ANU1, ANU2, ALAMB1, AND
C             ALAMB2.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --ANU1 AND ANU2  SHOULD BE POSITIVE.
C                 --ALAMB1 AND ALAMB2 SHOULD BE NON-NEGATIVE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NORRAN, CHSRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--JOHNSON, KOTZ, AND BALAKRISHNAN, "CONTINUOUS
C                 UNIVARIATE DISTRIBUTIONS VOLUME 2", SECOND EDITION,
C                 1994, PAGES 502-503.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.5
C     ORIGINAL VERSION--MAY       2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XTEMP(1)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
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
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF DOUBLY ',
     1'NON-CENTRAL F RANDOM NUMBERS IS NON-POSITIVE.')
      IF(ANU1.LE.0.0)THEN
        WRITE(ICOUT,16)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ANU1
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   16 FORMAT('***** ERROR--THE SHAPE PARAMETER NU1 FOR THE ',
     1'DOUBLY NON-CENTRAL F RANDOM NUMBERS IS NON-POSITIVE.')
      IF(ANU2.LE.0.0)THEN
        WRITE(ICOUT,26)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ANU2
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   26 FORMAT('***** ERROR--THE SHAPE PARAMETER NU2 FOR THE ',
     1'DOUBLY NON-CENTRAL F RANDOM NUMBERS IS NON-POSITIVE.')
      IF(ALAMB1.LT.0.0)THEN
        WRITE(ICOUT,36)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB1
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   36 FORMAT('***** ERROR--THE NON-CENTRALITY PARAMETER LAMBDA1 FOR ',
     1'THE DOUBLY NON-CENTRAL F RANDOM NUMBERS IS NEGATIVE.')
C
      IF(ALAMB2.LT.0.0)THEN
        WRITE(ICOUT,38)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,46)ALAMB2
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
   38 FORMAT('***** ERROR--THE NON-CENTRALITY PARAMETER LAMBDA2 FOR ',
     1'THE DOUBLY NON-CENTRAL F RANDOM NUMBERS IS NEGATIVE.')
   46 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',E15.8)
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
C
C  IF DEGREES OF FREEDOM ARE LESS THAN 1, THEN USE PERCENT
C  POINT METHOD (PROBABLY NOT MOST EFFICIENT METHOD, BUT LEAVE
C  UNTIL FIND A BETTER ALGORITHM).
C 
      IF(ANU1.LE.1.0 .OR. ANU2.LE.1.0)THEN
        CALL UNIRAN(N,ISEED,X)
        DO1378II=1,N
          ATEMP=X(II)
          CALL DNFPPF(ATEMP,ANU1,ANU2,ALAMB1,ALAMB2,PPF)
          X(II)=PPF
 1378   CONTINUE
      ELSE
        NTEMP=1
        DO100II=1,N
          CALL NORRAN(NTEMP,ISEED,XTEMP)
          X1=(XTEMP(1) + SQRT(ALAMB1))**2
          IF(ANU1.GT.1.0)THEN
            CALL CHSRAN(NTEMP,ANU1-1.0,ISEED,XTEMP)
            X1=X1+XTEMP(1)
          ENDIF
          CALL NORRAN(NTEMP,ISEED,XTEMP)
          X2=(XTEMP(1) + SQRT(ALAMB2))**2
          IF(ANU2.GT.1.0)THEN
            CALL CHSRAN(NTEMP,ANU2-1.0,ISEED,XTEMP)
            X2=X2+XTEMP(1)
          ENDIF
          X(II)=ANU2*X1/(ANU1*X2)
  100   CONTINUE
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DNTCDF(X, DF, DELTA, ALAMB, CDF)
CCCCC CONVERT TO DOUBLE PRECISION.  SINGLE PRECISION GIVES INACCURATE
CCCCC RESULTS ON A 32-BIT COMPUTER.
C
C--------------------------------------------------------------------
C   CDFDNT   WRITTEN BY CHARLES P. REEVE, STATISTICAL ENGINEERING
C            DIVISION, NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY, GAITHERSBURG,
C            MARYLAND  20899
C
C   FOR: COMPUTING THE CUMULATIVE DISTRIBUTION FUNCTION OF THE DOUBLY 
C        NONCENTRAL T DISTRIBUTION TO A SPECIFIED ACCURACY (TRUNCATION
C        ERROR IN THE INFINITE SERIES REPRESENTATION GIVEN BY EQUATION
C        4 IN REFERENCE 1 BELOW).  WHEN X<0 THE C.D.F. IS COMPUTED
C        FROM CDF(X,DF,DELTA,ALAMB) = 1 - CDF(-X,DF,-DELTA,ALAMB).
C        THE BETA C.D.F. ROUTINE IS CALLED AT MOST FOUR TIMES.  FURTHER
C        VALUES OF THE BETA C.D.F. ARE OBTAINED FROM RECURRENCE
C        RELATIONS GIVEN IN REFERENCE 2.  REFERENCE 3 GIVES A DETAILED
C        DESCRIPTION OF THE ALGORITHM HEREIN.
C
C        THIS PROGRAM MAY ALSO BE EFFICIENTLY USED TO COMPUTE THE
C        CUMULATIVE DISTRIBUTION FUNCTIONS OF THE SINGLY NONCENTRAL
C        AND CENTRAL T DISTRIBUTIONS BY SETTING THE APPROPRIATE
C        NONCENTRALITY PARAMETERS EQUAL TO ZERO.
C
C        CHECKS ARE MADE TO ASSURE THAT ALL PASSED PARAMETERS ARE
C        WITHIN VALID RANGES AS GIVEN BELOW.  NO UPPER LIMIT IS SET
C        FOR THE NONCENTRALITY PARAMETERS, BUT VALUES UP TO ABOUT 100 
C        FOR DELTA AND 10,000 FOR LAMBDA CAN BE HANDLED WITH THE
C        CURRENT DIMENSION LIMITS.  THE COMPUTED VALUE CDF IS VALID
C        ONLY IF IFLAG=0 ON RETURN.
C
C   NOTE: IN SUBROUTINE EDGET THE DOUBLE PRECISION CONSTANT DEUFLO IS 
C         THE EXPONENTIAL UNDERFLOW LIMIT WHOSE CURRENT VALUE IS SET
C         AT -69D0.  ON A COMPUTER WHERE DEXP(-69D0) CAUSES UNDERFLOW 
C         THIS LIMIT SHOULD BE CHANGED. 
C
C   SUBPROGRAMS CALLED: BETCDF (BETA C.D.F.)
C                       DLNGAM (DOUBLE PRECISION LOG OF GAMMA FUNCTION)
C                       POISST, EDGET, GRID
C
C   CURRENT VERSION COMPLETED SEPTEMBER 29, 1988
C
C   REFERENCES: 
C
C   1. KRISHNAN, MARAKATHA, 'SERIES REPRESENTATIONS OF THE DOUBLY
C      NONCENTRAL T DISTRIBUTION', JOURNAL OF THE AMERICAN STATISTICAL
C      ASSOCIATION, SEPTEMBER 1968, VOLUME 63, NO. 323, PP. 1004-1012.
C
C   2. ABRAMOWITZ, MILTON, AND STEGUN, IRENE A., 'HANDBOOK OF
C      MATHEMATICAL FUNCTIONS', NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY APPLIED
C      MATHEMATICS SERIES 55, NOVEMBER 1970, P. 944.
C
C   3. REEVE, CHARLES P., 'AN ALGORITHM FOR COMPUTING THE DOUBLY
C      NONCENTRAL T C.D.F. TO A SPECIFIED ACCURACY', STATISTICAL
C      ENGINEERING DIVISION NOTE 86-5, DECEMBER 1986.
C--------------------------------------------------------------------
C   DEFINITION OF PASSED PARAMETERS: 
C
C       * X = VALUE AT WHICH THE C.D.F. IS TO BE COMPUTED (REAL)
C
C      * DF = DEGREES OF FREEDOM (>0) IN THE DENOMINATOR (REAL)
C
C   * DELTA = THE NONCENTRALITY PARAMETER FOR THE NUMERATOR (REAL)
C             [EQUAL TO ZERO FOR THE CENTRAL T DISTRIBUTION]
C
C   * ALAMB = THE NONCENTRALITY PARAMETER (>=0) FOR THE DENOMINATOR
C             (REAL) [EQUAL TO ZERO FOR THE SINGLY NONCENTRAL T AND
C             CENTRAL T DISTRIBUTIONS]
C
C     * EPS = THE DESIRED ABSOLUTE ACCURACY OF THE C.D.F. (REAL)
C             [1 >= EPS >= 10**(-10)]
C             (NOTE: WE WILL HARD CODE THIS TO 1.0E-6)
C
C      CDF = THE DOUBLY NONCENTRAL T C.D.F. EVALUATED AT X (REAL)
C
C   * INDICATES PARAMETERS REQUIRING INPUT VALUES 
C--------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL X, DF, DELTA, ALAMB, CDF
C
      PARAMETER (NX=1000)
      DIMENSION BFI(NX),BFJ(NX),POI(NX),POJ(NX)
CCCCC DOUBLE PRECISION DARG,DFA
      LOGICAL LL
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      REAL CPUMIN,CPUMAX
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
CCCCC MAY 2004.  INCREASE ACCURACY
CCCCC DATA EPS/1.D-06/
      DATA EPS/1.D-08/
C
      DCDF=0.D0
      IF(DF.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)DF
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('**** ERROR--THE DEGREES OF FREEDOM PARAMETER')
  102 FORMAT('     IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
      IF(ABS(DELTA).GT.100.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,202)DELTA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  201 FORMAT('**** ERROR--THE FIRST NON-CENTRALITY ',
     *       'PARAMETER HAS AN ABSOLUTE VALUE GREATER THAN 100.')
  202 FORMAT('     IT HAS THE VALUE ',E15.7)
      IF(ALAMB.GT.10000.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)ALAMB
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  301 FORMAT('**** ERROR--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER HAS A VALUE GREATER THAN 10000.')
  302 FORMAT('     IT HAS THE VALUE ',E15.7)
      IF(ALAMB.LT.0.0)THEN
        WRITE(ICOUT,303)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)ALAMB
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  303 FORMAT('**** ERROR--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER IS NEGATIVE.')
  304 FORMAT('     IT HAS THE VALUE ',E15.7)
C
      IFLAG=0
      CDF=0.0
      EPS3 = 0.001*EPS
C
      DX=DBLE(X)
      DDF=DBLE(DF)
      DLAMB=DBLE(ALAMB)
      DDELTA=DBLE(DELTA)
      DCDF=0.0D0
C
      DELSQ = DDELTA**2
      FA = 0.5D0*DELSQ
      GA = 0.5D0*DLAMB
      GB = 0.5D0*DDF
      YY = DDF/(DDF+DX*DX)
      XX = 1.0D0-YY
C
C--- IF X<0 SET LL=.TRUE., REVERSE SIGN OF DELTA, AND USE THE
C--- IDENTITY DESCRIBED UP FRONT FOR COMPUTING THE C.D.F.
C
      LL = X.LT.0.0D0
      IF (XX.GE.1.0D0) THEN
         DCDF = 1.0D0
         GO TO 50
      ENDIF
      SDELTA = DDELTA
      IF (LL) SDELTA = -DDELTA 
C
C--- COMPUTE POISSON PROBABILITIES IN VECTOR POI
C
      CALL POISST(FA,EPS,IMIN,NI,POI,NX,IFLAG)
      IF (IFLAG.NE.0) THEN
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
CCCCC IF (IFLAG.NE.0) RETURN
      IF (YY.GE.1.0D0) GO TO 10 
      FC = 0.5D0+DBLE(IMIN)
C
C--- COMPUTE POISSON PROBABILITIES IN VECTOR POJ
C
      CALL POISST (GA,EPS,JMIN,NJ,POJ,NX,IFLAG)
CCCCC IF (IFLAG.NE.0) RETURN
      IF (IFLAG.NE.0) THEN
        WRITE(ICOUT,501)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  501 FORMAT('**** ERROR--DNTCDF ROUTINE RETURNED AN ',
     1       'ERROR FROM THE POISST ROUTINE. ***')
      GC = GB+DBLE(JMIN)
C
C--- SUM THE TERMS CORRESPONDING TO 'EVEN' VALUES OF INDEX I
C
      CALL GRIDD(NI,NJ,FC,GC,BFI,BFJ,POI,POJ,XX,YY,EPS3,DCDF,IFLAG)
CCCCC IF (IFLAG.NE.0) RETURN
      IF (IFLAG.NE.0) THEN
        WRITE(ICOUT,401)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  401 FORMAT('**** ERROR--DNTCDF ROUTINE RETURNED AN ',
     1       'FROM THE GRID ROUTINE. ***')
   10 IF (DDELTA.EQ.0.0D0) THEN
         NI = 0
         SUM = 0.0D0
         IF (YY.GE.1.0D0) GO TO 40
      ELSE
C
C--- COMPUTE 'POISSON-LIKE' PROBABILITIES IN VECTOR POI
C
         K = INT(FA)
         IF (IMIN.GT.0) THEN
            IMIN = IMIN-1
            NI = NI+1
         ENDIF
         DFA = FA
         DARG = (DBLE(K)+0.5D0)*DLOG(DFA)-DFA-DLNGAM(DBLE(K)+1.5D0)
         L = K-IMIN+1
         POI(L) = DSIGN(DEXP(DARG),SDELTA)
         SUM = POI(L)
         DO 20 I = K-1, IMIN, -1
            L = L-1 
            POI(L) = POI(L+1)*(DBLE(I)+1.5D0)/FA
            SUM = SUM+POI(L)
   20    CONTINUE
         L = K-IMIN+1
         DO 30 I = K+1, IMIN+NI-1
            L = L+1 
            POI(L) = POI(L-1)*FA/(DBLE(I)+0.5D0)
            SUM = SUM+POI(L)
   30    CONTINUE
         IF (YY.GE.1.0D0) GO TO 40
         FC = 1.0D0+DBLE(IMIN)
C
C--- SUM THE TERMS CORRESPONDING TO 'ODD' VALUES OF INDEX I 
C
         CALL GRIDD(NI,NJ,FC,GC,BFI,BFJ,POI,POJ,XX,YY,EPS3,DCDF,IFLAG)
CCCCC    IF (IFLAG.NE.0) RETURN
         IF (IFLAG.NE.0) THEN
           WRITE(ICOUT,401)
           CALL DPWRST('XXX','BUG ')
           GOTO9999
         ENDIF
      ENDIF
C
C--- COMPUTE THE NORMAL C.D.F. AT -SDELTA
C
   40 PHI = 0.5D0*(1.0D0-SUM)
C
C--- COMPUTE THE DOUBLY NONCENTRAL T C.D.F. AT X, USING AN IDENTITY
C--- IF X<0
C
      DCDF = 0.5D0*DCDF+PHI
   50 IF (LL) DCDF = 1.0D0-DCDF 
C
 9999 CONTINUE
      CDF=SNGL(DCDF)
      RETURN
      END
      REAL FUNCTION DNTFU3(X)
C
C     PURPOSE--DNTPDF CALLS DIFF TO FIND A NUMERICAL DERIVATIVE
C              FOR THE NON-CENTRAL CUMULATIVE DISTRIBUTION FUNCTION.
C              DNTFU3 IS A FUNCTION THAT CALL DNTCDF.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE DERIVATIVE
C                                IS TO BE EVALUATED.
C     OUTPUT--THE SINGLE PRECISION FUNCTION VALUE DNTFU3.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--NCBCDF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATION INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATION INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004.3
C     ORIGINAL VERSION--APRIL     2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL ANU
      REAL DELTA
      REAL ALAMB
      COMMON/DNTCOM/ANU,DELTA,ALAMB
C
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      CALL DNTCDF(X,ANU,DELTA,ALAMB,CDF)
      DNTFU3=CDF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DNTPDF(X, ANU, DELTA, LAMBDA, PDF)
C
C     PURPOSE--PROBABILITY DENSITY FUNCTION FOR THE NON-CENTRAL
C              T DISTRIBUTION.  THE PROBABILITY DENSITY FUNCTION
C              IS COMPUTED BY COMPUTING THE NUMERICAL DERIVATIVE OF
C              THE CUMULATIVE DISTRIBUTION FUNCTION.
C     INPUT  ARGUMENTS--X      = THE SINGLE PRECISION VALUE AT
C                                WHICH THE PROBABILITY DENSITY
C                                FUNCTION IS TO BE EVALUATED.
C                     --ANU    = THE DEGREES OF FREEDOM SHAPE PARAMETER
C                     --DELTA  = THE FIRST NON-CENTRALITY SHAPE PARAMETER
C                     --LAMBDA = THE SECOND NON-CENTRALITY PARAMETER
C     OUTPUT ARGUMENTS--PDF    = THE SINGLE PRECISION PROBABILITY
C                                DENSITY FUNCTION VALUE.
C     OUTPUT--THE SINGLE PRECISION PROBABILITY DENSITY FUNCTION VALUE PDF.
C     PRINTING--NONE.
C     RESTRICTIONS--NONE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--DIFF.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--NONE.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--2004/5
C     ORIGINAL VERSION--MAY       2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL  X
      REAL  ANU
      REAL  DELTA
      REAL  LAMBDA
      REAL  PDF
C
      REAL DNTFU3
      EXTERNAL DNTFU3
      REAL ANU2
      REAL DELTA2
      REAL ALAMB
      COMMON/DNTCOM/ANU2,DELTA2,ALAMB
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      PDF=0.0
C
      IF(ANU.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)ANU
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('**** ERROR--THE DEGREES OF FREEDOM PARAMETER')
  102 FORMAT('     IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
      IF(ABS(DELTA).GT.100.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,202)DELTA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  201 FORMAT('**** ERROR--THE FIRST NON-CENTRALITY ',
     *       'PARAMETER HAS AN ABSOLUTE VALUE GREATER THAN 100.')
  202 FORMAT('     IT HAS THE VALUE ',E15.7)
      IF(ALAMB.GT.10000.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)ALAMB
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  301 FORMAT('**** ERROR--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER HAS A VALUE GREATER THAN 10000.')
  302 FORMAT('     IT HAS THE VALUE ',E15.7)
      IF(ALAMB.LT.0.0)THEN
        WRITE(ICOUT,303)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,304)ALAMB
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  303 FORMAT('**** ERROR--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER IS NEGATIVE.')
  304 FORMAT('     IT HAS THE VALUE ',E15.7)
C
C
C  FIND NUMERIC DERIVATIVE OF CDF ROUTINE
C
      IORD=1
      EPS=0.0001
      ACCUR=0.0
      IFAIL=0
      X0 = X
      XMIN=X0 - 50.0
      XMAX=X0 + 50.0
      ANU2=ANU
      DELTA2=DELTA
      ALAMB=LAMBDA
C
      CALL DIFF(IORD,X0,XMIN,XMAX,DNTFU3,EPS,ACCUR,PDF,ERROR,IFAIL)
C
      IF(IFAIL.EQ.1)THEN
  999     FORMAT(1X)
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,401)
  401   FORMAT('***** WARNING IN NUMERICAL DERIVATIVE FOR DNTPDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,403)
  403   FORMAT('      THE ESTIMATED ERROR IN THE RESULT EXCEEDS THE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,405)
  405   FORMAT('      REQUESTED ERROR, BUT THE MOST ACCURATE RESULT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,407)
  407   FORMAT('      POSSIBLE HAS BEEN RETURNED.')
        CALL DPWRST('XXX','BUG ')
      ELSEIF(IFAIL.EQ.2)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,411)
  411   FORMAT('***** ERROR IN NUMERICAL DERIVATIVE FOR DNTPDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,413)
  413   FORMAT('      ERROR IN THE INPUT TO THE DIFF ROUTINE.')
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ELSEIF(IFAIL.EQ.3)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,421)
  421   FORMAT('***** ERROR IN NUMERICAL DERIVATIVE FOR DNTPDF--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,423)
  423   FORMAT('      THE INTERVAL FOR DIFFERENTIATION, (',G15.7,
     1         ',',G15.7,')')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,425)
  425   FORMAT('      IS TOO SMALL.')
        CALL DPWRST('XXX','BUG ')
        PDF=0.0
        GOTO9999
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DNTPPF(P,NU,DELTA,ALAMB,PPF)
C
C     PURPOSE   --PERCENT POINT FUNCTION FOR THE DOUBLY NON-CENTRAL T
C                 DISTRIBUTION.  USES A BISECTION METHOD.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--94/9
C     ORIGINAL VERSION--SEPTEMBER 1994.
C     UPDATED         --OCTOBER   2006. CALL LIST TO TPPF
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      REAL DELTA, ALAMB
      REAL NU
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DATA EPS /0.0001/
      DATA SIG /1.0E-5/
      DATA ZERO /0./
      DATA MAXIT /500/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      PPF=0.0
      IF(P.LE.0.0.OR.P.GE.1.0)THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
    1 FORMAT('***** FATAL ERROR--THE 1ST INPUT ARGUMENT TO THE ',
     1' DNTPPF SUBROUTINE IS OUTSIDE THE ALLOWABLE (0,1) INTERVAL.')
      IF(NU.LE.0.0)THEN
        WRITE(ICOUT,101)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,102)NU
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  101 FORMAT('**** FATAL DIAGNOSTIC--THE DEGREES OF FREEDOM PARAMETER')
  102 FORMAT('     IS NON-POSITIVE.  IT HAS THE VALUE ',E15.7)
      IF(ABS(DELTA).GT.100.0)THEN
        WRITE(ICOUT,201)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,202)DELTA
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  201 FORMAT('**** FATAL DIAGNOSTIC--THE FIRST NON-CENTRALITY ',
     *       'PARAMETER HAS AN ABSOLUTE VALUE GREATER THAN 100.')
  202 FORMAT('     IT HAS THE VALUE ',E15.7)
      IF(ABS(ALAMB).GT.10000.0)THEN
        WRITE(ICOUT,301)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,302)ALAMB
        CALL DPWRST('XXX','BUG ')
        GOTO9999
      ENDIF
  301 FORMAT('**** FATAL DIAGNOSTIC--THE SECOND NON-CENTRALITY ',
     *       'PARAMETER HAS AN ABSOLUTE VALUE GREATER THAN 10000.')
  302 FORMAT('     IT HAS THE VALUE ',E15.7)
C
C  FIND BRACKETING INTERVAL.  USE CORRESPONDING CENTRAL CHI-SQUARE
C  AS INITIAL GUESS, INCREMENTS OF 100 AROUND IT.
C  AFTER SUCCESSFULLY FIND BRACKETING INTERVAL, THEN SWITCH TO 
C  MORE EFFICIENT BISECTION METHOD.
C
      NUINT=NU+0.5
      CALL TPPF(P,REAL(NUINT),XL)
      XINC=20.0
      ICOUNT=0
      MAXCNT=10000
C
   91 CONTINUE
      XR=XL+XINC
      CALL DNTCDF(XL,NU,DELTA,ALAMB,CDFL)
      CALL DNTCDF(XR,NU,DELTA,ALAMB,CDFR)
      IF(CDFL.LT.P .AND. CDFR.LT.P)THEN
        XL=XR
      ELSEIF(CDFL.GT.P .AND. CDFR.GT.P)THEN
        XL=XL-XINC
      ELSE
        GOTO99
      ENDIF
      ICOUNT=ICOUNT+1
      IF(ICOUNT.GT.MAXCNT)THEN
        WRITE(ICOUT,96)
        CALL DPWRST('XXX','BUG ')
        PPF=0.0
        GOTO9999
      ENDIF
   96 FORMAT('***** FATAL ERROR--DNTPPF UNABLE TO FIND BRACKETING ',
     *       'INTERVAL. *****')
      GOTO91
C
C  BISECTION METHOD
C
   99 CONTINUE
      IC = 0
      FXL = -P
      FXR = 1.0 - P
  105 CONTINUE
      X = (XL+XR)*0.5
      CALL DNTCDF(X,NU,DELTA,ALAMB,CDF)
      P1=CDF
      PPF=X
      FCS = P1 - P
      IF(FCS*FXL.GT.ZERO)GOTO110
      XR = X
      FXR = FCS 
      GOTO115
  110 CONTINUE
      XL = X
      FXL = FCS
  115 CONTINUE
      XRML = XR - XL
      IF(XRML.LE.SIG .AND. ABS(FCS).LE.EPS)GOTO9999
      IC = IC + 1
      IF(IC.LE.MAXIT)GOTO105
      WRITE(ICOUT,130)
      CALL DPWRST('XXX','BUG ')
  130 FORMAT('***** FATAL ERROR--DNTPPF ROUTINE DID NOT CONVERGE. ***')
      GOTO9999
C
 9999 CONTINUE
      RETURN
      END
      SUBROUTINE DNTRAN(N,ANU,DELTA,LAMBDA,ISEED,X)
C
C     PURPOSE--THIS SUBROUTINE GENERATES A RANDOM SAMPLE OF SIZE N
C              FROM THE STUDENT'S DOUBLY NON-CENTRAL T DISTRIBUTION
C              WITH INTEGER DEGREES OF FREEDOM PARAMETER NU AND
C              NON-CENTRALITY PARAMETERS DELTA AND LAMBDA.
C     INPUT  ARGUMENTS--N      = THE DESIRED INTEGER NUMBER
C                                OF RANDOM NUMBERS TO BE
C                                GENERATED.
C                     --NU     = THE INTEGER DEGREES OF FREEDOM
C                                PARAMETER.
C                     --DELTA  = THE REAL NON-CENTRALITY PARAMETER
C                                DELTA.
C                     --LAMBDA = THE REAL NON-CENTRALITY PARAMETER
C                                LAMBDA.
C     OUTPUT ARGUMENTS--X      = A SINGLE PRECISION VECTOR
C                                (OF DIMENSION AT LEAST N)
C                                INTO WHICH THE GENERATED
C                                RANDOM SAMPLE WILL BE PLACED.
C     OUTPUT--A RANDOM SAMPLE OF SIZE N
C             FROM THE STUDENT'S DOUBLY NON-CENTRAL T DISTRIBUTION
C             WITH DEGREES OF FREEDOM PARAMETER = NU AND
C             NON-CENTRALITY PARAMETERS DELTA AND LAMBDA.
C     PRINTING--NONE UNLESS AN INPUT ARGUMENT ERROR CONDITION EXISTS.
C     RESTRICTIONS--THERE IS NO RESTRICTION ON THE MAXIMUM VALUE
C                   OF N FOR THIS SUBROUTINE.
C                 --NU SHOULD BE A POSITIVE INTEGER VARIABLE.
C     OTHER DATAPAC   SUBROUTINES NEEDED--UNIRAN.
C     FORTRAN LIBRARY SUBROUTINES NEEDED--LOG, SQRT, SIN, COS.
C     MODE OF INTERNAL OPERATIONS--SINGLE PRECISION.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--MOOD AND GRABLE, INTRODUCTION TO THE
C                 THEORY OF STATISTICS, 1963, PAGE 233.
C               --JOHNSON, KOTZ, AND BALAKRISHNAN, CONTINUOUS
C                 UNIVARIATE DISTRIBUTIONS--VOLUME 2, SECOND EDITION,
C                 1994, CHAPTER 31.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C               EXCEPTION--HOLLERITH STRINGS IN FORMAT STATEMENTS
C                          DENOTED BY QUOTES RATHER THAN NH.
C     VERSION NUMBER--2004.3
C     ORIGINAL VERSION--MARCH     2004.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      REAL LAMBDA
      REAL DELTA
      DIMENSION X(*)
      DIMENSION Y(2),Z(2)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA PI/3.14159265359/
C
C-----START POINT-----------------------------------------------------
C
C     CHECK THE INPUT ARGUMENTS FOR ERRORS
C
      IF(N.LT.1)THEN
        WRITE(ICOUT,5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,47)N
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
      IF(ANU.LE.0.0)THEN
        WRITE(ICOUT,15)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,48)ANU
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
    5 FORMAT('***** ERROR--THE REQUESTED NUMBER OF DOUBLY NONCENTRAL',
     1' T RANDOM NUMBERS IS NON-POSITIVE')
   15 FORMAT('***** ERROR--THE DEGREES OF FREEDOM PARAMETER FOR THE ',
     1'DOUBLY NON-CENTRAL T RANDOM NUMBERS IS NON-POSITIVE')
   47 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',I8)
   48 FORMAT('***** THE VALUE OF THE ARGUMENT IS ',F12.5)
C
C     GENERATE N STUDENT'S DOUBLY NON-CENTRAL T RANDOM NUMBERS
C     USING THE DEFINITION THAT A STUDENT'S DOUBLY NON-CENTRAL T
C     VARIATE WITH NU DEGREES OF FREEDOM AND NON-CENTRALITY
C     PARAMETERS DELTA AND LAMBDA EQUALS A NORMAL VARIATE WITH
C     LOCATION PARAMETER DELTA DIVIDED BY  
C     SQRT(NON-CENTRAL-CHI-SQUARED(NU,LAMBDA)/NU).
C     FIRST GENERATE A NORMAL RANDOM NUMBER WITH LOCATION PARAMETER
C     DELTA, THEN GENERATE THE NON-CENTRAL CHI-SQUARE NUMBER.
C     THEN FORM THE RATIO OF THE FIRST DIVIDED BY THE SECOND.
C
      DO100I=1,N
C
C       NORMAL RANDOM NUMBER WITH LOCATION PARAMETER DELTA
C
        CALL UNIRAN(2,ISEED,Y)
        ARG1=-2.0*LOG(Y(1))
        ARG2=2.0*PI*Y(2)
        ZNORM=(SQRT(ARG1))*(COS(ARG2)) + DELTA
C
C       NON-CENTRAL CHI-SQUARE RANDOM NUMBER
C
        CALL NCCRAN(NTEMP,ANU,LAMBDA,ISEED,Y)
        X(I)=ZNORM/SQRT(Y(1)/ANU)
C
  100 CONTINUE
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DOGDRV(NR,N,X,F,G,A,P,XPLS,FPLS,SX,STEPMX,
CDPLT SUBROUTINE DOGDRV(NR,N,X,F,G,A,P,XPLS,FPLS,OPTFCN,SX,STEPMX,
     +     STEPTL,DLT,IRETCD,MXTAKE,SC,WRK1,WRK2,WRK3,IPR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND A NEXT NEWTON ITERATE (XPLS) BY THE DOUBLE DOGLEG METHOD
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C X(N)         --> OLD ITERATE X[K-1]
C F            --> FUNCTION VALUE AT OLD ITERATE, F(X)
C G(N)         --> GRADIENT  AT OLD ITERATE, G(X), OR APPROXIMATE
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN
C                  IN LOWER TRIANGULAR PART AND DIAGONAL
C P(N)         --> NEWTON STEP
C XPLS(N)     <--  NEW ITERATE X[K]
C FPLS        <--  FUNCTION VALUE AT NEW ITERATE, F(XPLS)
C FCN          --> NAME OF SUBROUTINE TO EVALUATE FUNCTION
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C STEPTL       --> RELATIVE STEP SIZE AT WHICH SUCCESSIVE ITERATES
C                  CONSIDERED CLOSE ENOUGH TO TERMINATE ALGORITHM
C DLT         <--> TRUST REGION RADIUS
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C IRETCD      <--  RETURN CODE
C                    =0 SATISFACTORY XPLS FOUND
C                    =1 FAILED TO FIND SATISFACTORY XPLS SUFFICIENTLY
C                       DISTINCT FROM X
C MXTAKE      <--  BOOLEAN FLAG INDICATING STEP OF MAXIMUM LENGTH USED
C SC(N)        --> WORKSPACE [CURRENT STEP]
C WRK1(N)      --> WORKSPACE (AND PLACE HOLDING ARGUMENT TO TREGUP)
C WRK2(N)      --> WORKSPACE
C WRK3(N)      --> WORKSPACE
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C
      DIMENSION X(N),XPLS(N),G(N),P(N)
      DIMENSION SX(N)
      DIMENSION SC(N),WRK1(N),WRK2(N),WRK3(N)
      DIMENSION A(NR,1)
      LOGICAL FSTDOG,NWTAKE,MXTAKE
CDPLT EXTERNAL OPTFCN
C
      IRETCD=4
      FSTDOG=.TRUE.
      TMP=0.
      DO 5 I=1,N
        TMP=TMP+SX(I)*SX(I)*P(I)*P(I)
    5 CONTINUE
      RNWTLN=SQRT(TMP)
C$    WRITE(IPR,954) RNWTLN
C
  100 CONTINUE
C
C FIND NEW STEP BY DOUBLE DOGLEG ALGORITHM
      CALL DOGSTP(NR,N,G,A,P,SX,RNWTLN,DLT,NWTAKE,FSTDOG,
     +     WRK1,WRK2,CLN,ETA,SC,IPR,STEPMX)
C
C CHECK NEW POINT AND UPDATE TRUST REGION
CDPLT CALL TREGUP(NR,N,X,F,G,A,OPTFCN,SC,SX,NWTAKE,STEPMX,STEPTL,DLT,
      CALL TREGUP(NR,N,X,F,G,A,SC,SX,NWTAKE,STEPMX,STEPTL,DLT,
     +     IRETCD,WRK3,FPLSP,XPLS,FPLS,MXTAKE,IPR,2,WRK1)
      IF(IRETCD.LE.1) RETURN
      GO TO 100
CC950 FORMAT(42H DOGDRV    INITIAL TRUST REGION NOT GIVEN.,
CC   +       22H  COMPUTE CAUCHY STEP.)
CC951 FORMAT(18H DOGDRV    ALPHA =,E20.13/
CC   +       18H DOGDRV    BETA  =,E20.13/
CC   +       18H DOGDRV    DLT   =,E20.13/
CC   +       18H DOGDRV    NWTAKE=,L1    )
CC952 FORMAT(28H DOGDRV    CURRENT STEP (SC))
CC954 FORMAT(18H0DOGDRV    RNWTLN=,E20.13)
CC955 FORMAT(14H DOGDRV       ,5(E20.13,3X))
      END
      SUBROUTINE DOGSTP(NR,N,G,A,P,SX,RNWTLN,DLT,NWTAKE,FSTDOG,
     +     SSD,V,CLN,ETA,SC,IPR,STEPMX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
C PURPOSE
C -------
C FIND NEW STEP BY DOUBLE DOGLEG ALGORITHM
C
C PARAMETERS
C ----------
C NR           --> ROW DIMENSION OF MATRIX
C N            --> DIMENSION OF PROBLEM
C G(N)         --> GRADIENT AT CURRENT ITERATE, G(X)
C A(N,N)       --> CHOLESKY DECOMPOSITION OF HESSIAN IN
C                  LOWER PART AND DIAGONAL
C P(N)         --> NEWTON STEP
C SX(N)        --> DIAGONAL SCALING MATRIX FOR X
C RNWTLN       --> NEWTON STEP LENGTH
C DLT         <--> TRUST REGION RADIUS
C NWTAKE      <--> BOOLEAN, =.TRUE. IF NEWTON STEP TAKEN
C FSTDOG      <--> BOOLEAN, =.TRUE. IF ON FIRST LEG OF DOGLEG
C SSD(N)      <--> WORKSPACE [CAUCHY STEP TO THE MINIMUM OF THE
C                  QUADRATIC MODEL IN THE SCALED STEEPEST DESCENT
C                  DIRECTION] [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C V(N)        <--> WORKSPACE  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C CLN         <--> CAUCHY LENGTH
C                  [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C ETA              [RETAIN VALUE BETWEEN SUCCESSIVE CALLS]
C SC(N)       <--  CURRENT STEP
C IPR          --> DEVICE TO WHICH TO SEND OUTPUT
C STEPMX       --> MAXIMUM ALLOWABLE STEP SIZE
C
C INTERNAL VARIABLES
C ------------------
C CLN              LENGTH OF CAUCHY STEP
C
      DIMENSION G(N),P(N)
      DIMENSION SX(N)
      DIMENSION SC(N),SSD(N),V(N)
      DIMENSION A(NR,1)
      LOGICAL NWTAKE,FSTDOG
      IPR=IPR
C
C CAN WE TAKE NEWTON STEP
C
      IF(RNWTLN.GT.DLT) GO TO 100
C     IF(RNWTLN.LE.DLT)
C     THEN
        NWTAKE=.TRUE.
        DO 10 I=1,N
          SC(I)=P(I)
   10   CONTINUE
        DLT=RNWTLN
C$      WRITE(IPR,951)
        GO TO 700
C     ELSE
C
C NEWTON STEP TOO LONG
C CAUCHY STEP IS ON DOUBLE DOGLEG CURVE
C
  100   NWTAKE=.FALSE.
        IF(.NOT.FSTDOG) GO TO 200
C       IF(FSTDOG)
C       THEN
C
C         CALCULATE DOUBLE DOGLEG CURVE (SSD)
          FSTDOG=.FALSE.
          ALPHA=0.
          DO 110 I=1,N
            ALPHA=ALPHA + (G(I)*G(I))/(SX(I)*SX(I))
  110     CONTINUE
          BETA=0.
          DO 130 I=1,N
            TMP=0.
            DO 120 J=I,N
              TMP=TMP + (A(J,I)*G(J))/(SX(J)*SX(J))
  120       CONTINUE
            BETA=BETA+TMP*TMP
  130     CONTINUE
          DO 140 I=1,N
            SSD(I)=-(ALPHA/BETA)*G(I)/SX(I)
  140     CONTINUE
          CLN=ALPHA*SQRT(ALPHA)/BETA
          ETA=.2 + (.8*ALPHA*ALPHA)/(-BETA*DDOT(N,G,1,P,1))
          DO 150 I=1,N
            V(I)=ETA*SX(I)*P(I) - SSD(I)
  150     CONTINUE
          IF (DLT .EQ. (-1.0)) DLT = MIN(CLN, STEPMX)
C$        WRITE(IPR,954) ALPHA,BETA,CLN,ETA
C$        WRITE(IPR,955)
C$        WRITE(IPR,960) (SSD(I),I=1,N)
C$        WRITE(IPR,956)
C$        WRITE(IPR,960) (V(I),I=1,N)
C       ENDIF
  200   IF(ETA*RNWTLN.GT.DLT) GO TO 220
C       IF(ETA*RNWTLN .LE. DLT)
C       THEN
C
C         TAKE PARTIAL STEP IN NEWTON DIRECTION
C
          DO 210 I=1,N
            SC(I)=(DLT/RNWTLN)*P(I)
  210     CONTINUE
C$        WRITE(IPR,957)
          GO TO 700
C       ELSE
  220     IF(CLN.LT.DLT) GO TO 240
C         IF(CLN.GE.DLT)
C         THEN
C           TAKE STEP IN STEEPEST DESCENT DIRECTION
C
            DO 230 I=1,N
              SC(I)=(DLT/CLN)*SSD(I)/SX(I)
  230       CONTINUE
C$          WRITE(IPR,958)
            GO TO 700
C         ELSE
C
C           CALCULATE CONVEX COMBINATION OF SSD AND ETA*P
C           WHICH HAS SCALED LENGTH DLT
C
  240       DOT1=DDOT(N,V,1,SSD,1)
            DOT2=DDOT(N,V,1,V,1)
            ALAM=(-DOT1+SQRT((DOT1*DOT1)-DOT2*(CLN*CLN-DLT*DLT)))/DOT2
            DO 250 I=1,N
              SC(I)=(SSD(I) + ALAM*V(I))/SX(I)
  250       CONTINUE
C$          WRITE(IPR,959)
C         ENDIF
C       ENDIF
C     ENDIF
  700 CONTINUE
C$    WRITE(IPR,952) FSTDOG,NWTAKE,RNWTLN,DLT
C$    WRITE(IPR,953)
C$    WRITE(IPR,960) (SC(I),I=1,N)
      RETURN
C
  951 FORMAT(27H0DOGSTP    TAKE NEWTON STEP)
  952 FORMAT(18H DOGSTP    FSTDOG=,L1/
     +       18H DOGSTP    NWTAKE=,L1/
     +       18H DOGSTP    RNWTLN=,E20.13/
     +       18H DOGSTP    DLT   =,E20.13)
  953 FORMAT(28H DOGSTP    CURRENT STEP (SC))
  954 FORMAT(18H DOGSTP    ALPHA =,E20.13/
     +       18H DOGSTP    BETA  =,E20.13/
     +       18H DOGSTP    CLN   =,E20.13/
     +       18H DOGSTP    ETA   =,E20.13)
  955 FORMAT(28H DOGSTP    CAUCHY STEP (SSD))
  956 FORMAT(12H DOGSTP    V)
  957 FORMAT(48H0DOGSTP    TAKE PARTIAL STEP IN NEWTON DIRECTION)
  958 FORMAT(50H0DOGSTP    TAKE STEP IN STEEPEST DESCENT DIRECTION)
  959 FORMAT(39H0DOGSTP    TAKE CONVEX COMBINATION STEP)
  960 FORMAT(14H DOGSTP       ,5(E20.13,3X))
      END
      SUBROUTINE DOTPRO(V1,V2,N,DP)
C
C     PURPOSE--TO COMPUTE THE DOT PRODUCT
C              BETWEEN 2 SINGLE-PRECISION VECTORS--
C              V1 AND V2.
C              THE OUTPUT WILL BWE THE SINGLE PRECISION VALUE DP.
C              ALL INTERNAL CALCULATIONS ARE CARRIED
C              OUT IN DOUBLE PRECISION.T
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1978.
C     UPDATED         --FEBRUARY  1982.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION DSUM,DV1,DV2,DPROD
      DIMENSION V1(*)
      DIMENSION V2(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      DSUM=0.0D0
      DO100I=1,N
      DV1=V1(I)
      DV2=V2(I)
      DPROD=DV1*DV2
      DSUM=DSUM+DPROD
  100 CONTINUE
      DP=DSUM
C
      RETURN
      END
      SUBROUTINE DP1H4H(ISTART,ISTOP,ISTRIN,
     1IWORD1,IWORD2,IWORD3,NUMWD,NUMCH,IBUG1H,IERROR)
C
C     PURPOSE--CONVERT THE STRING FOUND IN LOCATIONS ISTART
C              THROUGH ISTOP (INCLUSIVE) IN ISTRIN(.).
C              FROM 1 CHARACTER PER WORD REPRESENTATIONS
C              TO PACKED 4 CHARACTERS PER WORD REPRESENTATIONS
C              IN IWORD1, IWORD2, AND IWORD3.
C     NOTE--AT MOST 12 CHARACTERS WILL BE OPERATED ON.
C     NOTE--AT MOST 3 WORDS WILL BE FORMED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH    1979.
C     UPDATED         --JANUARY  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ISTRIN
      CHARACTER*4 IWORD1
      CHARACTER*4 IWORD2
      CHARACTER*4 IWORD3
      CHARACTER*4 IBUG1H
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION ISTRIN(*)
C
C     NUMBPC = NUMBER OF BITS PER CHARACTER.
C     NUMCPW = NUMBER OF CHARACTERS PER WORD.
C     THESE VALUES WILL CHANGE DEPENDING
C     ON THE COMPUTER AND ARE DEFINED IN THE SUBROUTINE INITMC.
C     HOWEVER, IN ANY EVENT, THE OUTPUT FROM THIS
C     SUBROUTINE WILL BE 4 CHARACTERS PER WORD
C     (FOR A MORE GENERAL SUBROUTINE, SEE DP1HXH).
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DP1H'
      ISUBN2='4H  '
C
      IERROR='NO'
C
      NUMASC=4
C
      IF(IBUG1H.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP1H4H--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ISTART,ISTOP
   52 FORMAT('ISTART,ISTOP = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)(ISTRIN(I),I=ISTART,ISTOP)
   53 FORMAT('ISTRIN(.) = ',115A1)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
C               **********************************
C               **  STEP 1--                    **
C               **  INITIALIZE SOME VARIABLES.  **
C               **********************************
C
      ISTEPN='1'
      IF(IBUG1H.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IEND=0
      IWORD1=' '
      IWORD2=' '
      IWORD3=' '
      NUMWD=0
      NUMCH=0
C
C               *************************************
C               **  STEP 2--                       **
C               **  PACK 4 CHARACTERS INTO A WORD  **
C               **  FOR AS MANY AS 3 WORDS.        **
C               *************************************
C
      ISTEPN='2'
      IF(IBUG1H.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      I1MIN=1
      I1MAX=NUMASC
      I2MIN=I1MAX+1
      I2MAX=2*I1MAX
      I3MIN=I2MAX+1
      I3MAX=3*I1MAX
C
      J=0
      IF(ISTART.GT.ISTOP)GOTO250
      ISTOP2=ISTART+3*NUMASC-1
      IMAX=ISTOP
      IF(ISTOP.GT.ISTOP2)IMAX=ISTOP2
      DO200I=ISTART,IMAX
      J=J+1
      JM1=J-1
      L=J-(NUMASC*(JM1/NUMASC))
      K=NUMBPC*(L-1)
      K=IABS(K)
      IF(I1MIN.LE.J.AND.J.LE.I1MAX)GOTO211
      IF(I2MIN.LE.J.AND.J.LE.I2MAX)GOTO212
      IF(I3MIN.LE.J.AND.J.LE.I3MAX)GOTO213
      GOTO250
  211 CONTINUE
      CALL DPCHEX(0,NUMBPC,ISTRIN(I),K,NUMBPC,IWORD1)
      NUMWD=1
      GOTO200
  212 CONTINUE
      CALL DPCHEX(0,NUMBPC,ISTRIN(I),K,NUMBPC,IWORD2)
      NUMWD=2
      GOTO200
  213 CONTINUE
      CALL DPCHEX(0,NUMBPC,ISTRIN(I),K,NUMBPC,IWORD3)
      NUMWD=3
  200 CONTINUE
  250 CONTINUE
      NUMCH=J
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
C
 9000 CONTINUE
C
      IF(IBUG1H.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END OF DP1H4H--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)NUMBPC,NUMCPW,NUMASC
 9012 FORMAT('NUMBPC,NUMCPW,NUMASC = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NUMWD,NUMCH
 9013 FORMAT('NUMWD, NUMCH = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IWORD1,IWORD2,IWORD3
 9014 FORMAT('IWORD1,IWORD2,IWORD3 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPOCH (A, X)
C***BEGIN PROLOGUE  DPOCH
C***PURPOSE  Evaluate a generalization of Pochhammer's symbol.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C1, C7A
C***TYPE      DOUBLE PRECISION (POCH-S, DPOCH-D)
C***KEYWORDS  FNLIB, POCHHAMMER, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate a double precision generalization of Pochhammer's symbol
C (A)-sub-X = GAMMA(A+X)/GAMMA(A) for double precision A and X.
C For X a non-negative integer, POCH(A,X) is just Pochhammer's symbol.
C This is a preliminary version that does not handle wrong arguments
C properly and may not properly handle the case when the result is
C computed to less than half of double precision.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D9LGMC, DFAC, DGAMMA, DGAMR, DLGAMS, DLNREL, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DPOCH
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A, X, ABSA, ABSAX, ALNGA, ALNGAX, AX, B, PI,
     1  SGNGA, SGNGAX, DFAC, DLNREL, D9LGMC, DGAMMA, DGAMR, DCOT
      EXTERNAL DGAMMA
      EXTERNAL DCOT
      SAVE PI
      DATA PI / 3.1415926535 8979323846 2643383279 503 D0 /
C***FIRST EXECUTABLE STATEMENT  DPOCH
      AX = A + X
      IF (AX.GT.0.0D0) GO TO 30
      IF (AINT(AX).NE.AX) GO TO 30
C
      IF (A .GT. 0.0D0 .OR. AINT(A) .NE. A) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
    1 FORMAT('***** ERROR FROM DPOCH, A+X IS A NON-POSITIVE INTERGER ',
     1       'BUT A IS NOT. *****')
C
C WE KNOW HERE THAT BOTH A+X AND A ARE NON-POSITIVE INTEGERS.
C
      DPOCH = 1.0D0
      IF (X.EQ.0.D0) RETURN
C
      N = X
      IF (MIN(A+X,A).LT.(-20.0D0)) GO TO 20
C
      IA = A
      DPOCH = (-1.0D0)**N * DFAC(-IA)/DFAC(-IA-N)
      RETURN
C
 20   DPOCH = (-1.0D0)**N * EXP ((A-0.5D0)*DLNREL(X/(A-1.0D0))
     1  + X*LOG(-A+1.0D0-X) - X + D9LGMC(-A+1.0D0) - D9LGMC(-A-X+1.D0))
      RETURN
C
C A+X IS NOT ZERO OR A NEGATIVE INTEGER.
C
 30   DPOCH = 0.0D0
      IF (A.LE.0.0D0 .AND. AINT(A).EQ.A) RETURN
C
      N = ABS(X)
      IF (DBLE(N).NE.X .OR. N.GT.20) GO TO 50
C
C X IS A SMALL NON-POSITIVE INTEGER, PRESUMMABLY A COMMON CASE.
C
      DPOCH = 1.0D0
      IF (N.EQ.0) RETURN
      DO 40 I=1,N
        DPOCH = DPOCH * (A+I-1)
 40   CONTINUE
      RETURN
C
 50   ABSAX = ABS(A+X)
      ABSA = ABS(A)
      IF (MAX(ABSAX,ABSA).GT.20.0D0) GO TO 60
      DPOCH = DGAMMA(A+X) * DGAMR(A)
      RETURN
C
 60   IF (ABS(X).GT.0.5D0*ABSA) GO TO 70
C
C ABS(X) IS SMALL AND BOTH ABS(A+X) AND ABS(A) ARE LARGE.  THUS,
C A+X AND A MUST HAVE THE SAME SIGN.  FOR NEGATIVE A, WE USE
C GAMMA(A+X)/GAMMA(A) = GAMMA(-A+1)/GAMMA(-A-X+1) *
C SIN(PI*A)/SIN(PI*(A+X))
C
      B = A
      IF (B.LT.0.0D0) B = -A - X + 1.0D0
      DPOCH = EXP ((B-0.5D0)*DLNREL(X/B) + X*LOG(B+X) - X
     1  + D9LGMC(B+X) - D9LGMC(B) )
      IF (A.LT.0.0D0 .AND. DPOCH.NE.0.0D0) DPOCH =
     1  DPOCH/(COS(PI*X) + DCOT(PI*A)*SIN(PI*X) )
      RETURN
C
 70   CALL DLGAMS (A+X, ALNGAX, SGNGAX)
      CALL DLGAMS (A, ALNGA, SGNGA)
      DPOCH = SGNGAX * SGNGA * EXP(ALNGAX-ALNGA)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DPOCH1(A, X)
C***BEGIN PROLOGUE  DPOCH1
C***PURPOSE  Calculate a generalization of Pochhammer's symbol starting
C            from first order.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C1, C7A
C***TYPE      DOUBLE PRECISION (POCH1-S, DPOCH1-D)
C***KEYWORDS  FIRST ORDER, FNLIB, POCHHAMMER, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate a double precision generalization of Pochhammer's symbol
C for double precision A and X for special situations that require
C especially accurate values when X is small in
C        POCH1(A,X) = (POCH(A,X)-1)/X
C                   = (GAMMA(A+X)/GAMMA(A) - 1.0)/X .
C This specification is particularly suited for stably computing
C expressions such as
C        (GAMMA(A+X)/GAMMA(A) - GAMMA(B+X)/GAMMA(B))/X
C             = POCH1(A,X) - POCH1(B,X)
C Note that POCH1(A,0.0) = PSI(A)
C
C When ABS(X) is so small that substantial cancellation will occur if
C the straightforward formula is used, we use an expansion due
C to Fields and discussed by Y. L. Luke, The Special Functions and Their
C Approximations, Vol. 1, Academic Press, 1969, page 34.
C
C The ratio POCH(A,X) = GAMMA(A+X)/GAMMA(A) is written by Luke as
C        (A+(X-1)/2)**X * polynomial in (A+(X-1)/2)**(-2) .
C In order to maintain significance in POCH1, we write for positive a
C        (A+(X-1)/2)**X = EXP(X*LOG(A+(X-1)/2)) = EXP(Q)
C                       = 1.0 + Q*EXPREL(Q) .
C Likewise the polynomial is written
C        POLY = 1.0 + X*POLY1(A,X) .
C Thus,
C        POCH1(A,X) = (POCH(A,X) - 1) / X
C                   = EXPREL(Q)*(Q/X + Q*POLY1(A,X)) + POLY1(A,X)
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCOT, DEXPRL, DPOCH, DPSI, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  DPOCH1
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION A, X, ABSA, ABSX, ALNEPS, ALNVAR, B, BERN(20),
     1  BINV, BP, GBERN(21), GBK, PI, POLY1, Q, RHO, SINPXX, SINPX2,
     2  SQTBIG, TERM, TRIG, VAR, VAR2, DPSI, DEXPRL, DCOT, DPOCH
      LOGICAL FIRST
      EXTERNAL DCOT
      SAVE BERN, PI, SQTBIG, ALNEPS, FIRST
      DATA BERN  (  1) / +.8333333333 3333333333 3333333333 333 D-1    /
      DATA BERN  (  2) / -.1388888888 8888888888 8888888888 888 D-2    /
      DATA BERN  (  3) / +.3306878306 8783068783 0687830687 830 D-4    /
      DATA BERN  (  4) / -.8267195767 1957671957 6719576719 576 D-6    /
      DATA BERN  (  5) / +.2087675698 7868098979 2100903212 014 D-7    /
      DATA BERN  (  6) / -.5284190138 6874931848 4768220217 955 D-9    /
      DATA BERN  (  7) / +.1338253653 0684678832 8269809751 291 D-10   /
      DATA BERN  (  8) / -.3389680296 3225828668 3019539124 944 D-12   /
      DATA BERN  (  9) / +.8586062056 2778445641 3590545042 562 D-14   /
      DATA BERN  ( 10) / -.2174868698 5580618730 4151642386 591 D-15   /
      DATA BERN  ( 11) / +.5509002828 3602295152 0265260890 225 D-17   /
      DATA BERN  ( 12) / -.1395446468 5812523340 7076862640 635 D-18   /
      DATA BERN  ( 13) / +.3534707039 6294674716 9322997780 379 D-20   /
      DATA BERN  ( 14) / -.8953517427 0375468504 0261131811 274 D-22   /
      DATA BERN  ( 15) / +.2267952452 3376830603 1095073886 816 D-23   /
      DATA BERN  ( 16) / -.5744724395 2026452383 4847971943 400 D-24   /
      DATA BERN  ( 17) / +.1455172475 6148649018 6626486727 132 D-26   /
      DATA BERN  ( 18) / -.3685994940 6653101781 8178247990 866 D-28   /
      DATA BERN  ( 19) / +.9336734257 0950446720 3255515278 562 D-30   /
      DATA BERN  ( 20) / -.2365022415 7006299345 5963519636 983 D-31   /
      DATA PI / 3.1415926535 8979323846 2643383279 503 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DPOCH1
      IF (FIRST) THEN
         SQTBIG = 1.0D0/SQRT(24.0D0*D1MACH(1))
         ALNEPS = LOG(D1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      IF (X.EQ.0.0D0) DPOCH1 = DPSI(A)
      IF (X.EQ.0.0D0) RETURN
C
      ABSX = ABS(X)
      ABSA = ABS(A)
      IF (ABSX.GT.0.1D0*ABSA) GO TO 70
      IF (ABSX*LOG(MAX(ABSA,2.0D0)).GT.0.1D0) GO TO 70
C
      BP = A
      IF (A.LT.(-0.5D0)) BP = 1.0D0 - A - X
      INCR = 0
      IF (BP.LT.10.0D0) INCR = 11.0D0 - BP
      B = BP + INCR
C
      VAR = B + 0.5D0*(X-1.0D0)
      ALNVAR = LOG(VAR)
      Q = X*ALNVAR
C
      POLY1 = 0.0D0
      IF (VAR.GE.SQTBIG) GO TO 40
      VAR2 = (1.0D0/VAR)**2
C
      RHO = 0.5D0*(X+1.0D0)
      GBERN(1) = 1.0D0
      GBERN(2) = -RHO/12.0D0
      TERM = VAR2
      POLY1 = GBERN(2)*TERM
C
      NTERMS = -0.5D0*ALNEPS/ALNVAR + 1.0D0
CCCCC+   'NTERMS IS TOO BIG, MAYBE D1MACH(3) IS BAD', 1, 2)
      IF (NTERMS .GT. 20) THEN
        WRITE(ICOUT,1)
    1   FORMAT('***** ERORR FROM DPOCH1, INTERNAL ERROR.  *******')
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
      IF (NTERMS.LT.2) GO TO 40
C
      DO 30 K=2,NTERMS
        GBK = 0.0D0
        DO 20 J=1,K
          NDX = K - J + 1
          GBK = GBK + BERN(NDX)*GBERN(J)
 20     CONTINUE
        GBERN(K+1) = -RHO*GBK/K
C
        TERM = TERM * (2*K-2-X)*(2*K-1-X)*VAR2
        POLY1 = POLY1 + GBERN(K+1)*TERM
 30   CONTINUE
C
 40   POLY1 = (X-1.0D0)*POLY1
      DPOCH1 = DEXPRL(Q)*(ALNVAR+Q*POLY1) + POLY1
C
      IF (INCR.EQ.0) GO TO 60
C
C WE HAVE DPOCH1(B,X), BUT BP IS SMALL, SO WE USE BACKWARDS RECURSION
C TO OBTAIN DPOCH1(BP,X).
C
      DO 50 II=1,INCR
        I = INCR - II
        BINV = 1.0D0/(BP+I)
        DPOCH1 = (DPOCH1 - BINV) / (1.0D0 + X*BINV)
 50   CONTINUE
C
 60   IF (BP.EQ.A) RETURN
C
C WE HAVE DPOCH1(BP,X), BUT A IS LT -0.5.  WE THEREFORE USE A REFLECTION
C FORMULA TO OBTAIN DPOCH1(A,X).
C
      SINPXX = SIN(PI*X)/X
      SINPX2 = SIN(0.5D0*PI*X)
      TRIG = SINPXX*DCOT(PI*B) - 2.0D0*SINPX2*(SINPX2/X)
C
      DPOCH1 = TRIG + (1.0D0 + X*TRIG)*DPOCH1
      RETURN
C
 70   DPOCH1 = (DPOCH(A,X) - 1.0D0) / X
      RETURN
C
      END
      DOUBLE PRECISION FUNCTION DPSI (X)
C***BEGIN PROLOGUE  DPSI
C***PURPOSE  Compute the Psi (or Digamma) function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7C
C***TYPE      DOUBLE PRECISION (PSI-S, DPSI-D, CPSI-C)
C***KEYWORDS  DIGAMMA FUNCTION, FNLIB, PSI FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C DPSI calculates the double precision Psi (or Digamma) function for
C double precision argument X.  PSI(X) is the logarithmic derivative
C of the Gamma function of X.
C
C Series for PSI        on the interval  0.          to  1.00000E+00
C                                        with weighted error   5.79E-32
C                                         log weighted error  31.24
C                               significant figures required  30.93
C                                    decimal places required  32.05
C
C
C Series for APSI       on the interval  0.          to  1.00000E-02
C                                        with weighted error   7.75E-33
C                                         log weighted error  32.11
C                               significant figures required  28.88
C                                    decimal places required  32.71
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  D1MACH, DCOT, DCSEVL, INITDS, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770601  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890911  Removed unnecessary intrinsics.  (WRB)
C   890911  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900727  Added EXTERNAL statement.  (WRB)
C   920618  Removed space from variable name.  (RWC, WRB)
C***END PROLOGUE  DPSI
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
      DOUBLE PRECISION X, PSICS(42), APSICS(16), AUX, DXREL, PI, XBIG,
     1  Y, DCOT, DCSEVL
      LOGICAL FIRST
      EXTERNAL DCOT
      SAVE PSICS, APSICS, PI, NTPSI, NTAPSI, XBIG, DXREL, FIRST
      DATA PSICS(  1) / -.3805708083 5217921520 4376776670 39 D-1     /
      DATA PSICS(  2) / +.4914153930 2938712748 2046996542 77 D+0     /
      DATA PSICS(  3) / -.5681574782 1244730242 8920647340 81 D-1     /
      DATA PSICS(  4) / +.8357821225 9143131362 7756507478 62 D-2     /
      DATA PSICS(  5) / -.1333232857 9943425998 0792741723 93 D-2     /
      DATA PSICS(  6) / +.2203132870 6930824892 8723979795 21 D-3     /
      DATA PSICS(  7) / -.3704023817 8456883592 8890869492 29 D-4     /
      DATA PSICS(  8) / +.6283793654 8549898933 6514187176 90 D-5     /
      DATA PSICS(  9) / -.1071263908 5061849855 2835417470 74 D-5     /
      DATA PSICS( 10) / +.1831283946 5484165805 7315898103 78 D-6     /
      DATA PSICS( 11) / -.3135350936 1808509869 0057797968 85 D-7     /
      DATA PSICS( 12) / +.5372808776 2007766260 4719191436 15 D-8     /
      DATA PSICS( 13) / -.9211681415 9784275717 8806326247 30 D-9     /
      DATA PSICS( 14) / +.1579812652 1481822782 2528840328 23 D-9     /
      DATA PSICS( 15) / -.2709864613 2380443065 4405894097 07 D-10    /
      DATA PSICS( 16) / +.4648722859 9096834872 9473195295 49 D-11    /
      DATA PSICS( 17) / -.7975272563 8303689726 5047977727 37 D-12    /
      DATA PSICS( 18) / +.1368272385 7476992249 2510538928 38 D-12    /
      DATA PSICS( 19) / -.2347515606 0658972717 3206779807 19 D-13    /
      DATA PSICS( 20) / +.4027630715 5603541107 9079250062 81 D-14    /
      DATA PSICS( 21) / -.6910251853 1179037846 5474229747 71 D-15    /
      DATA PSICS( 22) / +.1185604713 8863349552 9291395257 68 D-15    /
      DATA PSICS( 23) / -.2034168961 6261559308 1542104842 23 D-16    /
      DATA PSICS( 24) / +.3490074968 6463043850 3742329323 51 D-17    /
      DATA PSICS( 25) / -.5988014693 4976711003 0110813934 93 D-18    /
      DATA PSICS( 26) / +.1027380162 8080588258 3980057122 13 D-18    /
      DATA PSICS( 27) / -.1762704942 4561071368 3592601053 86 D-19    /
      DATA PSICS( 28) / +.3024322801 8156920457 4540354901 33 D-20    /
      DATA PSICS( 29) / -.5188916830 2092313774 2860888746 66 D-21    /
      DATA PSICS( 30) / +.8902773034 5845713905 0058874879 99 D-22    /
      DATA PSICS( 31) / -.1527474289 9426728392 8949719040 00 D-22    /
      DATA PSICS( 32) / +.2620731479 8962083136 3583180799 99 D-23    /
      DATA PSICS( 33) / -.4496464273 8220696772 5983880533 33 D-24    /
      DATA PSICS( 34) / +.7714712959 6345107028 9193642666 66 D-25    /
      DATA PSICS( 35) / -.1323635476 1887702968 1026389333 33 D-25    /
      DATA PSICS( 36) / +.2270999436 2408300091 2773119999 99 D-26    /
      DATA PSICS( 37) / -.3896419021 5374115954 4913919999 99 D-27    /
      DATA PSICS( 38) / +.6685198138 8855302310 6798933333 33 D-28    /
      DATA PSICS( 39) / -.1146998665 4920864872 5299199999 99 D-28    /
      DATA PSICS( 40) / +.1967938588 6541405920 5154133333 33 D-29    /
      DATA PSICS( 41) / -.3376448818 9750979801 9072000000 00 D-30    /
      DATA PSICS( 42) / +.5793070319 3214159246 6773333333 33 D-31    /
      DATA APSICS(  1) / -.8327107910 6929076017 4456932269 D-3        /
      DATA APSICS(  2) / -.4162518421 9273935282 1627121990 D-3        /
      DATA APSICS(  3) / +.1034315609 7874129117 4463193961 D-6        /
      DATA APSICS(  4) / -.1214681841 3590415298 7299556365 D-9        /
      DATA APSICS(  5) / +.3113694319 9835615552 1240278178 D-12       /
      DATA APSICS(  6) / -.1364613371 9317704177 6516100945 D-14       /
      DATA APSICS(  7) / +.9020517513 1541656513 0837974000 D-17       /
      DATA APSICS(  8) / -.8315429974 2159146482 9933635466 D-19       /
      DATA APSICS(  9) / +.1012242570 7390725418 8479482666 D-20       /
      DATA APSICS( 10) / -.1562702494 3562250762 0478933333 D-22       /
      DATA APSICS( 11) / +.2965427168 0890389613 3226666666 D-24       /
      DATA APSICS( 12) / -.6746868867 6570216374 1866666666 D-26       /
      DATA APSICS( 13) / +.1803453116 9718990421 3333333333 D-27       /
      DATA APSICS( 14) / -.5569016182 4598360746 6666666666 D-29       /
      DATA APSICS( 15) / +.1958679226 0773625173 3333333333 D-30       /
      DATA APSICS( 16) / -.7751958925 2333568000 0000000000 D-32       /
      DATA PI / 3.1415926535 8979323846 2643383279 50 D0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DPSI
      IF (FIRST) THEN
         NTPSI = INITDS (PSICS, 42, 0.1*REAL(D1MACH(3)) )
         NTAPSI = INITDS (APSICS, 16, 0.1*REAL(D1MACH(3)) )
C
         XBIG = 1.0D0/SQRT(D1MACH(3))
         DXREL = SQRT(D1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
C
      IF (Y.GT.10.0D0) GO TO 50
C
C DPSI(X) FOR ABS(X) .LE. 2
C
      N = X
      IF (X.LT.0.D0) N = N - 1
      Y = X - N
      N = N - 1
      DPSI = DCSEVL (2.D0*Y-1.D0, PSICS, NTPSI)
      IF (N.EQ.0) RETURN
C
      IF (N.GT.0) GO TO 30
C
      N = -N
      IF (X .EQ. 0.D0) THEN
        WRITE(ICOUT,1)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
    1 FORMAT('***** ERORR FROM DPSI, X IS ZERO..  *******')
      IF (X .LT. 0.D0 .AND. X+N-2 .EQ. 0.D0)THEN
        WRITE(ICOUT,2)
        CALL DPWRST('XXX','BUG ')
        RETURN
      ENDIF
    2 FORMAT('***** ERORR FROM DPSI, X IS A NEGATIVE INTEGER.  ******')
      IF (X.LT.(-0.5D0).AND.ABS((X-AINT(X-0.5D0))/X).LT.DXREL)THEN
        WRITE(ICOUT,3)
        CALL DPWRST('XXX','BUG ')
      ENDIF
    3 FORMAT('***** WARNING FROM DPSI, ANSWER IS LESS THAN HALF ',
     1 'PRECISION BECAUSE X IS TOO NEAR A NEGATIVE INTEGER.  ****')
C
      DO 20 I=1,N
        DPSI = DPSI - 1.D0/(X+I-1)
 20   CONTINUE
      RETURN
C
C DPSI(X) FOR X .GE. 2.0 AND X .LE. 10.0
C
 30   DO 40 I=1,N
        DPSI = DPSI + 1.0D0/(Y+I)
 40   CONTINUE
      RETURN
C
C DPSI(X) FOR ABS(X) .GT. 10.0
C
 50   AUX = 0.D0
      IF (Y.LT.XBIG) AUX = DCSEVL (2.D0*(10.D0/Y)**2-1.D0, APSICS,
     1  NTAPSI)
C
      IF (X.LT.0.D0) DPSI = LOG(ABS(X)) - 0.5D0/X + AUX
     1  - PI*DCOT(PI*X)
      IF (X.GT.0.D0) DPSI = LOG(X) - 0.5D0/X + AUX
      RETURN
C
      END
      SUBROUTINE DP3DP1(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1MAXNPP,
     1IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     DONE?
C     PURPOSE--FORM A PARTICULAR 3-DIMENSIONAL PLOT--
C              NAMELY, A Y VERSUS X1 AND X2 PLOT,
C              WHEN HAVE NO VERSUS AND NO EQUALS.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--FEBRUARY  1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHVERT
      CHARACTER*4 IHVER2
      CHARACTER*4 IHHOR
      CHARACTER*4 IHHOR2
      CHARACTER*4 IHHO2
      CHARACTER*4 IHHO22
      CHARACTER*4 IHSET
      CHARACTER*4 IHSET2
      CHARACTER*4 IERRO4
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
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DP3D'
      ISUBN2='P1  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      NUMAR1=0
C
      D2MIN=0.0
      DEL=0.0
C
C               ********************************
C               **  STEP 10--                 **
C               **  TREAT THE CASE WHEN HAVE  **
C               **  NO    VERSUS        AND   **
C               **  NO    FOR X =             **
C               ********************************
C
 1000 CONTINUE
C
      IF(IBUGG3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP3DP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NPLOTV,NPLOTP,NS
   52 FORMAT('NPLOTV,NPLOTP,NS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53 FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IBUGG3,IBUGQ
   54 FORMAT('IBUGG3,IBUGQ = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IFOUND,IERROR
   55 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)MAXNPP
   56 FORMAT('MAXNPP = ',I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      ISTEPN='10'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ********************************************
C               **  STEP 11--                             **
C               **  CHECK THE VALIDITY OF ARGUMENT 1      **
C               **  (THIS WILL BE THE RESPONSE VARIABLE)  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHVERT=IHARG(1)
      IHVER2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHVERT,IHVER2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IVAV=IVALUE(ILOCV)
      NLOCAL=IN(ILOCV)
C
      IF(IBUGG3.EQ.'ON')WRITE(ICOUT,1107)IHVERT,IHVER2,ILOCV,IERROR,
     1IVAV,NLOCAL
 1107 FORMAT('IHVERT,IHVER2,ILOCV,IERROR,IVAV,NLOCAL = ',
     1A4,A4,2X,I8,2X,A4,I8,I8)
      IF(IBUGG3.EQ.'ON')CALL DPWRST('XXX','BUG ')
C
C               *****************************************
C               **  STEP 12--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='12'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO1290
      DO1200J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO1210
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO1210
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO1220
 1200 CONTINUE
      GOTO1290
 1210 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO1290
 1220 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO1290
 1290 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO1295
      WRITE(ICOUT,1291)NUMARG,ILOCQ
 1291 FORMAT('NUMARG,ILOCQ = ',12I8)
      CALL DPWRST('XXX','BUG ')
 1295 CONTINUE
C
C               *********************************************
C               **  STEP 13--                              **
C               **  FORM THE VECTOR ISUB(.)                **
C               **  DEPENDING ON THE TYPE OF CASE          **
C               **  FOR THE QUALIFIER.                     **
C               *********************************************
C
      ISTEPN='13'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO1310
      IF(ICASEQ.EQ.'SUBS')GOTO1320
      IF(ICASEQ.EQ.'FOR')GOTO1330
C
 1310 CONTINUE
      DO1315I=1,NLOCAL
      ISUB(I)=1
 1315 CONTINUE
      NQ=NLOCAL
      GOTO1350
C
 1320 CONTINUE
      NIOLD=NLOCAL
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERRO4)
      NQ=NIOLD
      GOTO1350
C
 1330 CONTINUE
      NIOLD=NLOCAL
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERRO4)
      NQ=NFOR
      GOTO1350
C
 1350 CONTINUE
C
C               **********************************************************
C               **  STEP 14--                                           **
C               **  BRANCH ACCORDING TO THE NUMBER OF ARGUMENTS BEFORE  **
C               **  'SUBS', 'FOR', AND 'AND'.                          **
C               **********************************************************
C
      ISTEPN='14'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ILOCA=NUMARG+1
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMARG
      IF(ILOCA.LT.ILOCQ)NUMAR1=ILOCA-1
      IF(ILOCQ.LT.ILOCA)NUMAR1=ILOCQ-1
      IF(ILOCA.EQ.ILOCQ)NUMAR1=NUMARG
      IF(NUMAR1.EQ.3)GOTO1700
      IF(NUMAR1.EQ.4)GOTO1800
      WRITE(ICOUT,1401)
 1401 FORMAT('***** ERROR IN DP3DP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1402)
 1402 FORMAT('      NUMAR1 NOT = 3 OR 4. ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1403)NUMAR1
 1403 FORMAT('      NUMAR1 = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1404)NUMARG,IHARG(NUMARG),ILOCA,ILOCQ
 1404 FORMAT('      NUMARG,IHARG(NUMARG),ILOCA,ILOCQ = ',
     1I6,2X,A4,2I6)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1408)
 1408 FORMAT('      THE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)WRITE(ICOUT,1409)(IANS(I),I=1,IWIDTH)
 1409 FORMAT('      ',100A1)
      IF(IWIDTH.GE.1)CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
C
C               *************************************************************
C               **  STEP 17--                                              **
C               **  TREAT THE 3 VARIABLE CASE (WITH NO VS AND NO =) CASE.  **
C               *************************************************************
C
 1700 CONTINUE
      ISTEPN='17'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ****************************************
C               **  STEP 17.1--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 1  **
C               ****************************************
C
      ISTEPN='17.1'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHVERT=IHARG(1)
      IHVER2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHVERT,IHVER2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IVAV=IVALUE(ILOCV)
C
C               ****************************************
C               **  STEP 17.2--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 2  **
C               ****************************************
C
      ISTEPN='17.2'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHHOR=IHARG(2)
      IHHOR2=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCH,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IHAV=IVALUE(ILOCH)
C
C               ****************************************
C               **  STEP 17.3--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 3  **
C               ****************************************
C
      ISTEPN='17.3'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHHO2=IHARG(3)
      IHHO22=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHO2,IHHO22,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCH2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IHAV2=IVALUE(ILOCH2)
C
C               *************************************************************
C               **  STEP 17.4--                                            **
C               **  FORM THE VERTICAL AND 2 HORIZONTAL AXIS                **
C               **  VARIABLES (Y(.)AND X(.) AND X3D(.), RESPECTIVELY)      **
C               **  FOR THE PLOT.                                          **
C               **  RESET THE D(.) VECTOR TO ONES.                         **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).          **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).          **
C               *************************************************************
C
      ISTEPN='17.4'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IAND1.EQ.'NO')GOTO1719
      IF(NPLOTP.LE.0)GOTO1719
      D1MAX=D(1)
      DO1710I=1,NPLOTP
      IF(D(I).GT.D1MAX)D1MAX=D(I)
 1710 CONTINUE
      D2MIN=1.0
      IF(IAND1.EQ.'YES')DEL=D1MAX-D2MIN+1.0
 1719 CONTINUE
C
      L=NPLOTP
C
      NLOCAL=IN(ILOCV)
      DO1720I=1,NLOCAL
      IF(ISUB(I).EQ.0)GOTO1720
      L=L+1
C
      IF(L.LE.MAXNPP)GOTO1729
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1721)
 1721 FORMAT('***** ERROR IN DP3DP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1723)
 1723 FORMAT('      THE NUMBER OF PLOT POINTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1724)MAXNPP
 1724 FORMAT('      HAS JUST EXCEEDED ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1725)I,NLOCAL,L,MAXN,MAXNPP,NPLOTP
 1725 FORMAT('I,NLOCAL,L,MAXN,MAXNPP,NPLOTP = ',6I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1726)IAND1,IAND2,IFOUND,IERROR
 1726 FORMAT('IAND1,IAND2,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1729 CONTINUE
C
      IJ=MAXN*(IVAV-1)+I
      IF(IVAV.LE.MAXCOL)Y(L)=V(IJ)
      IF(IVAV.EQ.MAXCP1)Y(L)=PRED(I)
      IF(IVAV.EQ.MAXCP2)Y(L)=RES(I)
      IF(IVAV.EQ.MAXCP3)Y(L)=YPLOT(I)
      IF(IVAV.EQ.MAXCP4)Y(L)=XPLOT(I)
      IF(IVAV.EQ.MAXCP5)Y(L)=X2PLOT(I)
      IF(IVAV.EQ.MAXCP6)Y(L)=TAGPLO(I)
      IJ=MAXN*(IHAV-1)+I
      IF(IHAV.LE.MAXCOL)X(L)=V(IJ)
      IF(IHAV.EQ.MAXCP1)X(L)=PRED(I)
      IF(IHAV.EQ.MAXCP2)X(L)=RES(I)
      IF(IHAV.EQ.MAXCP3)X(L)=YPLOT(I)
      IF(IHAV.EQ.MAXCP4)X(L)=XPLOT(I)
      IF(IHAV.EQ.MAXCP5)X(L)=X2PLOT(I)
      IF(IHAV.EQ.MAXCP6)X(L)=TAGPLO(I)
      IJ=MAXN*(IHAV2-1)+I
      IF(IHAV2.LE.MAXCOL)X3D(L)=V(IJ)
      IF(IHAV2.EQ.MAXCP1)X3D(L)=PRED(I)
      IF(IHAV2.EQ.MAXCP2)X3D(L)=RES(I)
      IF(IHAV2.EQ.MAXCP3)X3D(L)=YPLOT(I)
      IF(IHAV2.EQ.MAXCP4)X3D(L)=XPLOT(I)
      IF(IHAV2.EQ.MAXCP5)X3D(L)=X2PLOT(I)
      IF(IHAV2.EQ.MAXCP6)X3D(L)=TAGPLO(I)
      IF(IAND1.EQ.'NO')D(L)=1.0
      IF(IAND1.EQ.'YES')D(L)=1.0+DEL
 1720 CONTINUE
      NPLOTP=L
      NPLOTV=2
      IF(IAND1.EQ.'YES'.AND.NPLOTV.GT.2)NPLOTV=NPLOTV
      GOTO9000
C
C
C               *************************************************************
C               **  STEP 18--                                              **
C               **  TREAT THE 4 VARIABLE CASE (WITH NO VS AND NO =) CASE.  **
C               *************************************************************
C
 1800 CONTINUE
      ISTEPN='18'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               ****************************************
C               **  STEP 18.1--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 1  **
C               ****************************************
C
      ISTEPN='18.1'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHVERT=IHARG(1)
      IHVER2=IHARG2(1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHVERT,IHVER2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IVAV=IVALUE(ILOCV)
C
C               ****************************************
C               **  STEP 18.2--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 2  **
C               ****************************************
C
      ISTEPN='18.2'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHHOR=IHARG(2)
      IHHOR2=IHARG2(2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCH,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IHAV=IVALUE(ILOCH)
C
C               ****************************************
C               **  STEP 18.3--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 3  **
C               ****************************************
C
      ISTEPN='18.3'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHHO2=IHARG(3)
      IHHO22=IHARG2(3)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHO2,IHHO22,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCH2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IHAV2=IVALUE(ILOCH2)
C
C               ****************************************
C               **  STEP 18.4--                       **
C               **  CHECK THE VALIDITY OF ARGUMENT 4  **
C               ****************************************
C
      ISTEPN='18.4'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IHSET=IHARG(4)
      IHSET2=IHARG2(4)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHSET,IHSET2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCD,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      ISETV=IVALUE(ILOCD)
C
C               *************************************************************
C               **  STEP 18.5--                                            **
C               **  FORM THE VERTICAL AND 2 HORIZONTAL AXIS                **
C               **  VARIABLES (Y(.)AND X(.) AND X3D(.), RESPECTIVELY)      **
C               **  FOR THE PLOT.                                          **
C               **  RESET THE D(.) VECTOR TO ONES.                         **
C               **  DEFINE THE NUMBER OF PLOT POINTS    (NPLOTP).          **
C               **  DEFINE THE NUMBER OF PLOT VARIABLES (NPLOTV).          **
C               *************************************************************
C
      ISTEPN='18.5'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IAND1.EQ.'NO')GOTO1819
      IF(NPLOTP.LE.0)GOTO1819
      D1MAX=D(1)
      DO1810I=1,NPLOTP
      IF(D(I).GT.D1MAX)D1MAX=D(I)
 1810 CONTINUE
      I=1
      IJ=MAXN*(ISETV-1)+I
      IF(ISETV.LE.MAXCOL)D2MIN=V(IJ)
      IF(ISETV.EQ.MAXCP1)D2MIN=PRED(I)
      IF(ISETV.EQ.MAXCP2)D2MIN=RES(I)
      IF(ISETV.EQ.MAXCP3)D2MIN=YPLOT(I)
      IF(ISETV.EQ.MAXCP4)D2MIN=XPLOT(I)
      IF(ISETV.EQ.MAXCP5)D2MIN=X2PLOT(I)
      IF(ISETV.EQ.MAXCP6)D2MIN=TAGPLO(I)
      NLOCAL=IN(ILOCV)
      DO1811I=1,NLOCAL
      IJ=MAXN*(ISETV-1)+I
      IF(ISETV.LE.MAXCOL)GOTO1812
      IF(ISETV.EQ.MAXCP1)GOTO1813
      IF(ISETV.EQ.MAXCP2)GOTO1814
      IF(ISETV.EQ.MAXCP3)GOTO1815
      IF(ISETV.EQ.MAXCP4)GOTO1816
      IF(ISETV.EQ.MAXCP5)GOTO1817
      IF(ISETV.EQ.MAXCP6)GOTO1818
 1812 CONTINUE
      IF(V(IJ).LT.D2MIN)D2MIN=V(IJ)
      GOTO1811
 1813 CONTINUE
      IF(PRED(I).LT.D2MIN)D2MIN=PRED(I)
      GOTO1811
 1814 CONTINUE
      IF(RES(I).LT.D2MIN)D2MIN=RES(I)
      GOTO1811
 1815 CONTINUE
      IF(YPLOT(I).LT.D2MIN)D2MIN=YPLOT(I)
      GOTO1811
 1816 CONTINUE
      IF(XPLOT(I).LT.D2MIN)D2MIN=XPLOT(I)
      GOTO1811
 1817 CONTINUE
      IF(X2PLOT(I).LT.D2MIN)D2MIN=X2PLOT(I)
      GOTO1811
 1818 CONTINUE
      IF(TAGPLO(I).LT.D2MIN)D2MIN=TAGPLO(I)
      GOTO1811
 1811 CONTINUE
      IF(IAND1.EQ.'YES')DEL=D1MAX-D2MIN+1.0
 1819 CONTINUE
C
      L=NPLOTP
C
      NLOCAL=IN(ILOCV)
      DO1820I=1,NLOCAL
      IF(ISUB(I).EQ.0)GOTO1820
      L=L+1
C
      IF(L.LE.MAXNPP)GOTO1829
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1821)
 1821 FORMAT('***** ERROR IN DP3DP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1823)
 1823 FORMAT('      THE NUMBER OF PLOT POINTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1824)MAXNPP
 1824 FORMAT('      HAS JUST EXCEEDED ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1825)I,NLOCAL,L,MAXN,MAXNPP,NPLOTP
 1825 FORMAT('I,NLOCAL,L,MAXN,MAXNPP,NPLOTP = ',6I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1826)IAND1,IAND2,IFOUND,IERROR
 1826 FORMAT('IAND1,IAND2,IFOUND,IERROR = ',A4,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
 1829 CONTINUE
C
      IJ=MAXN*(IVAV-1)+I
      IF(IVAV.LE.MAXCOL)Y(L)=V(IJ)
      IF(IVAV.EQ.MAXCP1)Y(L)=PRED(I)
      IF(IVAV.EQ.MAXCP2)Y(L)=RES(I)
      IF(IVAV.EQ.MAXCP3)Y(L)=YPLOT(I)
      IF(IVAV.EQ.MAXCP4)Y(L)=XPLOT(I)
      IF(IVAV.EQ.MAXCP5)Y(L)=X2PLOT(I)
      IF(IVAV.EQ.MAXCP6)Y(L)=TAGPLO(I)
      IJ=MAXN*(IHAV-1)+I
      IF(IHAV.LE.MAXCOL)X(L)=V(IJ)
      IF(IHAV.EQ.MAXCP1)X(L)=PRED(I)
      IF(IHAV.EQ.MAXCP2)X(L)=RES(I)
      IF(IHAV.EQ.MAXCP3)X(L)=YPLOT(I)
      IF(IHAV.EQ.MAXCP4)X(L)=XPLOT(I)
      IF(IHAV.EQ.MAXCP5)X(L)=X2PLOT(I)
      IF(IHAV.EQ.MAXCP6)X(L)=TAGPLO(I)
      IJ=MAXN*(IHAV2-1)+I
      IF(IHAV2.LE.MAXCOL)X3D(L)=V(IJ)
      IF(IHAV2.EQ.MAXCP1)X3D(L)=PRED(I)
      IF(IHAV2.EQ.MAXCP2)X3D(L)=RES(I)
      IF(IHAV2.EQ.MAXCP3)X3D(L)=YPLOT(I)
      IF(IHAV2.EQ.MAXCP4)X3D(L)=XPLOT(I)
      IF(IHAV2.EQ.MAXCP5)X3D(L)=X2PLOT(I)
      IF(IHAV2.EQ.MAXCP6)X3D(L)=TAGPLO(I)
      IJ=MAXN*(ISETV-1)+I
      IF(IAND1.EQ.'NO'.AND.ISETV.LE.MAXCOL)D(L)=V(IJ)
      IF(IAND1.EQ.'NO'.AND.ISETV.EQ.MAXCP1)D(L)=PRED(I)
      IF(IAND1.EQ.'NO'.AND.ISETV.EQ.MAXCP2)D(L)=RES(I)
      IF(IAND1.EQ.'NO'.AND.ISETV.EQ.MAXCP3)D(L)=YPLOT(I)
      IF(IAND1.EQ.'NO'.AND.ISETV.EQ.MAXCP4)D(L)=XPLOT(I)
      IF(IAND1.EQ.'NO'.AND.ISETV.EQ.MAXCP5)D(L)=X2PLOT(I)
      IF(IAND1.EQ.'NO'.AND.ISETV.EQ.MAXCP6)D(L)=TAGPLO(I)
      IF(IAND1.EQ.'YES'.AND.ISETV.LE.MAXCOL)D(L)=V(IJ)+DEL
      IF(IAND1.EQ.'YES'.AND.ISETV.EQ.MAXCP1)D(L)=PRED(I)+DEL
      IF(IAND1.EQ.'YES'.AND.ISETV.EQ.MAXCP2)D(L)=RES(I)+DEL
      IF(IAND1.EQ.'YES'.AND.ISETV.EQ.MAXCP3)D(L)=YPLOT(I)+DEL
      IF(IAND1.EQ.'YES'.AND.ISETV.EQ.MAXCP4)D(L)=XPLOT(I)+DEL
      IF(IAND1.EQ.'YES'.AND.ISETV.EQ.MAXCP5)D(L)=X2PLOT(I)+DEL
      IF(IAND1.EQ.'YES'.AND.ISETV.EQ.MAXCP6)D(L)=TAGPLO(I)+DEL
 1820 CONTINUE
      NPLOTP=L
      NPLOTV=3
      IF(IAND1.EQ.'YES'.AND.NPLOTV.GT.3)NPLOTV=NPLOTV
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DP3DP1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IBUGG3,IBUGQ
 9014 FORMAT('IBUGG3,IBUGQ = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IFOUND,IERROR
 9015 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)MAXNPP
 9016 FORMAT('MAXNPP = ',I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DP3DP2(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IVSLOC,NUMVS,
     1MAXNPP,
     1IBUGG3,IBUGQ,IFOUND,IERROR)
C
C     PURPOSE--FORM VARIOUS 3-DIMENSIONAL PLOTS
C              WHEN HAVE 1 OR MORE VERSUS ENTERED.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--DECEMBER  1977.
C     UPDATED         --JANUARY   1978.
C     UPDATED         --FEBRUARY  1978.
C     UPDATED         --MAY       1978.
C     UPDATED         --JUNE      1978.
C     UPDATED         --JULY      1978.
C     UPDATED         --NOVEMBER  1978.
C     UPDATED         --FEBRUARY  1979.
C     UPDATED         --MARCH     1979.
C     UPDATED         --JULY      1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGQ
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ICASEQ
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IHVERT
      CHARACTER*4 IHVER2
      CHARACTER*4 IHHOR
      CHARACTER*4 IHHOR2
      CHARACTER*4 IHHO2
      CHARACTER*4 IHHO22
      CHARACTER*4 IERRO4
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION IVSLOC(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DP3D'
      ISUBN2='P2  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
CCCCC MAXNPP=1000
C
      KSTART=0
C
      DEL=0.0
C
C               ***********************************************************
C               **  STEP 20--                                            **
C               **  TREAT THE CASE WHEN HAVE 1 OR MORE 'VERSUS' ENTERED  **
C               ***********************************************************
C
 2000 CONTINUE
C
      IF(IBUGG3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP3DP2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)NPLOTV,NPLOTP,NS
   52 FORMAT('NPLOTV,NPLOTP,NS = ',3I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53 FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)IBUGG3,IBUGQ
   54 FORMAT('IBUGG3,IBUGQ = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,55)IFOUND,IERROR
   55 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,56)MAXNPP
   56 FORMAT('MAXNPP = ',I8)
      CALL DPWRST('XXX','BUG ')
   90 CONTINUE
C
      ISTEPN='20'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C               *****************************************
C               **  STEP 21--                          **
C               **  CHECK TO SEE THE TYPE CASE--       **
C               **    1) UNQUALIFIED (THAT IS, FULL);  **
C               **    2) SUBSET/EXCEPT; OR             **
C               **    3) FOR.                          **
C               *****************************************
C
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASEQ='FULL'
      ILOCQ=NUMARG+1
      IF(NUMARG.LT.1)GOTO2190
      DO2100J=1,NUMARG
      J1=J
      IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO2110
      IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO2110
      IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO2120
 2100 CONTINUE
      GOTO2190
 2110 CONTINUE
      ICASEQ='SUBS'
      ILOCQ=J1
      GOTO2190
 2120 CONTINUE
      ICASEQ='FOR'
      ILOCQ=J1
      GOTO2190
 2190 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO2195
      WRITE(ICOUT,2191)NUMARG,ILOCQ
 2191 FORMAT('NUMARG,ILOCQ = ',2I8)
      CALL DPWRST('XXX','BUG ')
 2195 CONTINUE
C
C               **********************************
C               **  STEP 22--                   **
C               **  DETERMINE WHICH VARIABLES   **
C               **  ARE TO BE GROUPED TOGETHER  **
C               **********************************
C
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      L=NPLOTP
C
      NEWSET=0
      DO2200J=1,NUMVS
      JM1=J-1
      IF(J.EQ.1)KSTART=1
      IF(J.GE.2)KSTART=IVSLOC(JM1)+3
      KSTOP=IVSLOC(J)-1
      IVS=IVSLOC(J)
C
      IVSP1=IVS+1
      IVSP2=IVS+2
      DO2210K=KSTART,KSTOP
      NEWSET=NEWSET+1
C
      IHVERT=IHARG(K)
      IHVER2=IHARG2(K)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHVERT,IHVER2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCV,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IVAV=IVALUE(ILOCV)
      NLOCAL=IN(ILOCV)
C
      IHHOR=IHARG(IVSP1)
      IHHOR2=IHARG2(IVSP1)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHOR,IHHOR2,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCH,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IHAV=IVALUE(ILOCH)
C
      IHHO2=IHARG(IVSP2)
      IHHO22=IHARG2(IVSP2)
      IHWUSE='V'
      MESSAG='YES'
      CALL CHECKN(IHHO2,IHHO22,IHWUSE,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCH2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IHAV2=IVALUE(ILOCH2)
C
      ISETV=NEWSET
C
      IF(IAND1.EQ.'NO')GOTO2280
      IF(NPLOTP.LE.0)GOTO2280
      D1MAX=D(1)
      DO2220I=1,NPLOTP
      IF(D(I).GT.D1MAX)D1MAX=D(I)
 2220 CONTINUE
      D2MIN=1.0
      IF(IAND1.EQ.'YES')DEL=D1MAX-D2MIN+1.0
 2280 CONTINUE
C
      IF(IBUGG3.EQ.'OFF')GOTO2289
      WRITE(ICOUT,2282)IHVERT,ILOCV,IERROR,IVAV,NLOCAL
 2282 FORMAT('IHVERT,ILOCV,IERROR,IVAV,NLOCAL = ',
     1A4,2X,I8,2X,A4,2X,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2283)IHHOR,ILOCH,IERROR,IHAV,NLOCAL
 2283 FORMAT('IHHOR,ILOCH,IERROR,IHAV,NLOCAL = ',
     1A4,2X,I8,2X,A4,2X,2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2284)IHHOR2,ILOCH2,IERROR,IHAV2,NLOCAL
 2284 FORMAT('IHHOR2,ILOCH2,IERROR,IHAV2,NLOCAL = ',
     1A4,2X,I8,2X,A4,2X,2I8)
      CALL DPWRST('XXX','BUG ')
 2289 CONTINUE
C
C               *********************************************
C               **  STEP 23--                              **
C               **  FORM THE VECTOR ISUB(.)                **
C               **  DEPENDING ON THE TYPE OF CASE          **
C               **  FOR THE QUALIFIER.                     **
C               **  BRANCH TO THE PRPPER CASE.             **
C               *********************************************
C
      ISTEPN='23'
      IF(IBUGG3.EQ.'ON')CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICASEQ.EQ.'FULL')GOTO2310
      IF(ICASEQ.EQ.'SUBS')GOTO2320
      IF(ICASEQ.EQ.'FOR')GOTO2330
CCCCC IF(IHARG(J).EQ.'SUBS'.AND.IHARG2(J).EQ.'ET  ') GOTO2320
CCCCC IF(IHARG(J).EQ.'EXCE'.AND.IHARG2(J).EQ.'PT  ') GOTO2320
CCCCC IF(IHARG(J).EQ.'FOR '.AND.IHARG2(J).EQ.'    ') GOTO2330
C
 2310 CONTINUE
      DO2315I=1,NLOCAL
      ISUB(I)=1
 2315 CONTINUE
      NQ=NLOCAL
      GOTO2350
C
 2320 CONTINUE
      NIOLD=NLOCAL
      CALL DPSUBS(NIOLD,ILOCS,NS,IBUGQ,IERRO4)
      NQ=NIOLD
      GOTO2350
C
 2330 CONTINUE
      NIOLD=NLOCAL
      CALL DPFOR(NIOLD,NFOR,IROW1,IROWN,
     1NLOCAL,ILOCS,NS,IBUGQ,IERRO4)
      NQ=NFOR
      GOTO2350
C
 2350 CONTINUE
C
      DO2360I=1,NLOCAL
      IF(ISUB(I).EQ.0)GOTO2360
      L=L+1
C
      IF(L.LE.MAXNPP)GOTO2369
      WRITE(ICOUT,2362)
 2362 FORMAT('***** PLOT FORMATION ERROR IN DP3DP2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2363)
 2363 FORMAT('      THE NUMBER OF PLOT POINTS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2364)MAXNPP
 2364 FORMAT('      HAS JUST EXCEEDED ',I8,' *****')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 2369 CONTINUE
C
      IJ=MAXN*(IVAV-1)+I
      IF(IVAV.LE.MAXCOL)Y(L)=V(IJ)
      IF(IVAV.EQ.MAXCP1)Y(L)=PRED(I)
      IF(IVAV.EQ.MAXCP2)Y(L)=RES(I)
      IF(IVAV.EQ.MAXCP3)Y(L)=YPLOT(I)
      IF(IVAV.EQ.MAXCP4)Y(L)=XPLOT(I)
      IF(IVAV.EQ.MAXCP5)Y(L)=X2PLOT(I)
      IF(IVAV.EQ.MAXCP6)Y(L)=TAGPLO(I)
      IJ=MAXN*(IHAV-1)+I
      IF(IHAV.LE.MAXCOL)X(L)=V(IJ)
      IF(IHAV.EQ.MAXCP1)X(L)=PRED(I)
      IF(IHAV.EQ.MAXCP2)X(L)=RES(I)
      IF(IHAV.EQ.MAXCP3)X(L)=YPLOT(I)
      IF(IHAV.EQ.MAXCP4)X(L)=XPLOT(I)
      IF(IHAV.EQ.MAXCP5)X(L)=X2PLOT(I)
      IF(IHAV.EQ.MAXCP6)X(L)=TAGPLO(I)
      IJ=MAXN*(IHAV2-1)+I
      IF(IHAV2.LE.MAXCOL)X3D(L)=V(IJ)
      IF(IHAV2.EQ.MAXCP1)X3D(L)=PRED(I)
      IF(IHAV2.EQ.MAXCP2)X3D(L)=RES(I)
      IF(IHAV2.EQ.MAXCP3)X3D(L)=YPLOT(I)
      IF(IHAV2.EQ.MAXCP4)X3D(L)=XPLOT(I)
      IF(IHAV2.EQ.MAXCP5)X3D(L)=X2PLOT(I)
      IF(IHAV2.EQ.MAXCP6)X3D(L)=TAGPLO(I)
      IF(IAND1.EQ.'NO')D(L)=ISETV
      IF(IAND1.EQ.'YES')D(L)=ISETV+DEL
 2360 CONTINUE
 2210 CONTINUE
 2200 CONTINUE
      NPLOTP=L
C
      DHOLD=D(1)
      DO2370I=1,NPLOTP
      IF(D(I).NE.DHOLD)GOTO2375
 2370 CONTINUE
      NPLOTV=2
      GOTO2399
 2375 CONTINUE
      NPLOTV=3
      GOTO2399
C
 2399 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DPPLO1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)IBUGG3,IBUGQ
 9014 FORMAT('IBUGG3,IBUGQ = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IFOUND,IERROR
 9015 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9026)MAXNPP
 9026 FORMAT('MAXNPP = ',I8)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DP3DP3(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  PARAM,IPARN,IPARN2,NUMPAR,ITYPEH,IW2HOL,IW22HO,
     1                  W2HOLD,NWHOLD,
     1                  IFOLOC,MAXNPP,IANGLU,IFORSW,
     1                  IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--FORM THE 3-DIMENSIONAL PLOT OF A 2-VARIABLE FUNCTION
C              (THAT IS, FORM TRACES FROM A SURFACE)
C              WHEN HAVE 1 OR MORE    =    ENTERED,
C              THAT IS, WHEN HAVE   PLOT Y = ... FOR X = ...
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--FEBRUARY  1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --NOVEMBER  1981.
C     UPDATED         --DECEMBER  1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --APRIL     1992. FIX   PLOT CONSTANT
C     UPDATED         --SEPTEMBER 2015. SUPPORT FOR FUNCTION BLOCKS
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW2HOL
      CHARACTER*4 IW22HO
      CHARACTER*4 IANGLU
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IHWUSE
      CHARACTER*4 MESSAG
      CHARACTER*4 IWD1
      CHARACTER*4 IWD12
      CHARACTER*4 IWD2
      CHARACTER*4 IWD22
      CHARACTER*4 IVERTI
      CHARACTER*4 IVDU11
      CHARACTER*4 IVDU12
      CHARACTER*4 IVDU21
      CHARACTER*4 IVDU22
      CHARACTER*4 IH
      CHARACTER*4 IH2
      CHARACTER*4 IHPARN
      CHARACTER*4 IHPAR2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
      CHARACTER*4 IFTEXP
      CHARACTER*4 IFTORD
C
C---------------------------------------------------------------------
C
      DIMENSION PARAM(*)
      DIMENSION IPARN(*)
      DIMENSION IPARN2(*)
C
      DIMENSION ITYPEH(*)
      DIMENSION IW2HOL(*)
      DIMENSION IW22HO(*)
      DIMENSION W2HOLD(*)
C
      DIMENSION IFOLOC(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
      INCLUDE 'DPCOFB.INC'
C
      CHARACTER*8 IFBNAM
      CHARACTER*8 IFBANS
      CHARACTER*4 IFEESV
      COMMON/IFEED/IFEESV
C
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DP3D'
      ISUBN2='P3  '
      IFTORD='DATA'
      IFTEXP='+'
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
      NUMIT1=1
      NUMIT2=0
      LOCDU1=0
      LOCDU2=0
      I2=0
      I2M1=0
C
      DEL=0.0
C
C     CHECK IF FIRST ARGUMENT IS A FUNCTION BLOCK NAME
C
      IFLGFB=0
      IFBNAM=' '
      IFBANS=' '
      IF(IHARG(1).EQ.IFBNA1(1:4) .AND. IHARG2(1).EQ.IFBNA1(5:8))THEN
        IFLGFB=1
        IFBNAM=IFBNA1
        IFBANS=IFBAN1
      ELSEIF(IHARG(1).EQ.IFBNA2(1:4) .AND. IHARG2(1).EQ.IFBNA2(5:8))THEN
        IFLGFB=2
        IFBNAM=IFBNA2
        IFBANS=IFBAN2
      ELSEIF(IHARG(1).EQ.IFBNA3(1:4) .AND. IHARG2(1).EQ.IFBNA3(5:8))THEN
        IFLGFB=3
        IFBNAM=IFBNA3
        IFBANS=IFBAN3
      ENDIF
C
C               ************************************************************
C               **  TREAT THE CASE WHEN HAVE 1 OR MORE '=' ENTERED        **
C               **  THAT IS, TREAT THE 3D-PLOT Y = ... FOR X = ... CASE   **
C               ************************************************************
C
 3000 CONTINUE
C
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP3DP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPLOTV,NPLOTP,NS,MAXNPP
   52   FORMAT('NPLOTV,NPLOTP,NS,MAXNPP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2
   53   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IANGLU,IBUGG3,IBUGCO,IBUGEV,IBUGQ
   54   FORMAT('IANGLU,IBUGG3,IBUGCO,IBUGEV,IBUGQ = ',
     1         4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IFOUND,IERROR
   55   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************************************
C               **  STEP 2--                             **
C               **  DETERMINE THE MAX TRACE DESIGNATION  **
C               **  (A NUMBER) AS CONTAINED              **
C               **  IN THE VECTOR D(.).                  **
C               *******************************************
C
      ISTEPN='2'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IAND1.EQ.'NO')GOTO119
      IF(NPLOTP.LE.0)GOTO119
      D1MAX=D(1)
      DO110I=1,NPLOTP
        IF(D(I).GT.D1MAX)D1MAX=D(I)
  110 CONTINUE
      D2MIN=1.0
      IF(IAND1.EQ.'YES')DEL=D1MAX-D2MIN+1.0
  119 CONTINUE
C
C               *******************************************************
C               **  STEP 3--                                        **
C               **  DETERMINE THE NAME OF THE FIRST  DUMMY VARIABLE  **
C               **  (IT NEVER GETS STORED PERMANENTLY)               **
C               **  IMMEDIATELY FOLLOWING THE FIRST  'FOR' KEYWORD   **
C               *******************************************************
C
      ISTEPN='3'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO3100J=1,NUMARG
        J2=J
        IF(IHARG(J).EQ.'FOR')GOTO3119
 3100 CONTINUE
C
      IBRAN=3111
      WRITE(ICOUT,3111)
 3111 FORMAT('***** ERROR IN 3D PLOT (DP3DP3)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3112)
 3112 FORMAT('      THE FIRST    FOR    NOT FOUND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3113)
 3113 FORMAT('      EVEN THOUGH THE STRING    =    WAS FOUND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3114)
 3114 FORMAT('      THE ENTIRE COMMAND LINE WAS AS FOLLOWS--')
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
 3115   FORMAT('      ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 3119 CONTINUE
      IFOLP0=J2
C
      IF(IFOLP0.GE.NUMARG)THEN
        WRITE(ICOUT,3111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3122)
 3122   FORMAT('      THE FIRST    FOR    WAS THE FINAL WORD ON THE ',
     1         'COMMAND LINE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3124)
 3124   FORMAT('      THE WORD    FOR    SHOULD HAVE BEEN FOLLOWED ',
     1         'BY 11 WORDS --')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3126)
 3126   FORMAT('      1) A DUMMY VARIABLE NAME;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3127)
 3127   FORMAT('      2) AN EQUAL SIGN;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3128)
 3128   FORMAT('      3) ONE LIMIT (LOWER OR UPPER) ',
     1         'FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3129)
 3129   FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3130)
 3130   FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) ',
     1         'FOR THE DUMMY VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3131)
 3131   FORMAT('      6) THE SECOND    FOR    AND ITS 5 WORDS ')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3132)
 3132   FORMAT('      (DUMMY NAME, EQUAL SIGN, LOWER, INCREMENT,',
     1         ' UPPER)')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3114)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IFOLP1=IFOLP0+1
      IVDU11=IHARG(IFOLP1)
      IVDU12=IHARG2(IFOLP1)
C
C               *******************************************************
C               **  STEP 3.1--                                       **
C               **  DETERMINE THE NAME OF THE SECOND DUMMY VARIABLE  **
C               **  (IT NEVER GETS STORED PERMANENTLY)               **
C               **  IMMEDIATELY FOLLOWING THE SECOND 'FOR' KEYWORD   **
C               *******************************************************
C
      ISTEPN='3.1'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFOLP6=IFOLP0+6
      IF(IFOLP6.GT.NUMARG)GOTO3159
      DO3150J=IFOLP6,NUMARG
        J2=J
        IF(IHARG(J).EQ.'FOR')GOTO3169
 3150 CONTINUE
 3159 CONTINUE
C
      IBRAN=3161
      WRITE(ICOUT,3111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3162)
 3162 FORMAT('      THE SECOND    FOR    NOT FOUND,')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3163)
 3163 FORMAT('      EVEN THOUGH THE STRING    =    WAS FOUND.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3114)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
 3169 CONTINUE
C
      IF2LOC=J2
C
      IF(IFOLP0.GE.NUMARG)THEN
        WRITE(ICOUT,3111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3172)
 3172   FORMAT('      THE SECOND    FOR    WAS THE FINAL WORD ON THE ',
     1         'COMMAND LINE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3174)
 3174   FORMAT('      THE WORD    FOR    SHOULD HAVE BEEN FOLLOWED ',
     1         'BY 5 WORDS --')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3176)
 3176   FORMAT('      1) A DUMMY VARIABLE NAME;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3177)
 3177   FORMAT('      2) AN EQUAL SIGN;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3178)
 3178   FORMAT('      3) ONE LIMIT (LOWER OR UPPER) ',
     1         'FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3179)
 3179   FORMAT('      4) THE INCREMENT FOR THE DUMMY VARIABLE;')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3180)
 3180   FORMAT('      5) THE OTHER LIMIT (UPPER OR LOWER) ',
     1         'FOR THE DUMMY VARIABLE.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3114)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF2LP1=IF2LOC+1
      IVDU21=IHARG(IF2LP1)
      IVDU22=IHARG2(IF2LP1)
C
C               *******************************************
C               **  STEP 4--                             **
C               **  EVALUATE THE FUNCTION OVER           **
C               **  THE VARIOUS POINTS IN THE INTERVAL.  **
C               *******************************************
C
      ISTEPN='4'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      NUMAM1=NUMARG-1
      NUMAM2=NUMARG-2
      NUMAM3=NUMARG-3
      NUMAM6=NUMARG-6
      NUMAM7=NUMARG-7
      NUMAM8=NUMARG-8
      NUMAM9=NUMARG-9
C
      ILOCA=NUMAM8
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMAM9
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        START1=ARG(ILOCA)
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        START1=VALUE(ILOC)
      ELSE
        GOTO3270
      ENDIF
C
      ILOCA=NUMAM7
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMAM8
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        AINC1=ARG(ILOCA)
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        AINC1=VALUE(ILOC)
      ELSE
        GOTO3270
      ENDIF
C
      ILOCA=NUMAM6
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMAM7
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        STOP1=ARG(ILOCA)
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        STOP1=VALUE(ILOC)
      ELSE
        GOTO3270
      ENDIF
C
      ILOCA=NUMAM2
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMAM3
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        START2=ARG(ILOCA)
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        START2=VALUE(ILOC)
      ELSE
        GOTO3270
      ENDIF
C
      ILOCA=NUMAM1
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMAM2
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        AINC2=ARG(ILOCA)
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        AINC2=VALUE(ILOC)
      ELSE
        GOTO3270
      ENDIF
C
      ILOCA=NUMARG
      IF(IHARG(NUMARG).EQ.'AND')ILOCA=NUMAM1
      IF(IARGT(ILOCA).EQ.'NUMB')THEN
        STOP2=ARG(ILOCA)
      ELSEIF(IARGT(ILOCA).EQ.'WORD')THEN
        IH=IHARG(ILOCA)
        IH2=IHARG2(ILOCA)
        IHWUSE='P'
        MESSAG='YES'
        CALL CHECKN(IH,IH2,IHWUSE,
     1              IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,MAXNAM,
     1              ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOC,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
        STOP2=VALUE(ILOC)
      ELSE
        GOTO3270
      ENDIF
C
      GOTO3280
C
 3270 CONTINUE
      WRITE(ICOUT,3111)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3272)
 3272 FORMAT('      AN ARGUMENT TYPE WHICH SHOULD BE EITHER A NUMBER ',
     1       'OR A WORD IS NEITHER.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3274)IHARG(ILOCA)
 3274 FORMAT('      ARGUMENT                  = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3275)ILOCA
 3275 FORMAT('      LOCATION IN ARGUMENT LIST = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3276)IARGT(ILOCA)
 3276 FORMAT('      ARGUMENT TYPE             = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3114)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      IERROR='YES'
      GOTO9000
C
 3280 CONTINUE
      IF(START1.NE.STOP1.AND.AINC1.NE.0.0)GOTO3297
      IF(START2.NE.STOP2.AND.AINC2.NE.0.0)GOTO3297
C
      WRITE(ICOUT,3281)
 3281 FORMAT('***** NOTE FROM DP3DP3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3282)
 3282 FORMAT('      BOTH LOWER AND UPPER LIMITS OF THE FUNCTION ',
     1       'INTERVALS OF INTEREST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3284)
 3284 FORMAT('      ARE IDENTICAL OR THE INCREMENT IS IDENTICALLY ',
     1       'ZERO.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3286)START1
 3286 FORMAT('      FIRST  LOWER LIMIT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3287)AINC1
 3287 FORMAT('      FIRST  INCREMENT   = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3288)STOP1
 3288 FORMAT('      FIRST  UPPER LIMIT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3289)START2
 3289 FORMAT('      SECOND LOWER LIMIT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3290)AINC2
 3290 FORMAT('      SECOND INCREMENT   = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3291)STOP2
 3291 FORMAT('      SECOND UPPER LIMIT = ',G15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,3114)
      CALL DPWRST('XXX','BUG ')
      IF(IWIDTH.GE.1)THEN
        WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
        CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,3294)
 3294 FORMAT('      RESULTING ACTION--ONLY A SINGLE POINT ',
     1       'WAS OUTPUTTED FOR PLOTTING.')
      CALL DPWRST('XXX','BUG ')
      NUMIT1=1
      GOTO3299
 3297 CONTINUE
C
C     *****THE FOLLOWING CORRECTIVE LINE ADDED AUGUST 1983*****
      IF(START1.EQ.STOP1)AINC1=0.0
      IF(START1.LT.STOP1.AND.AINC1.LT.0.0)AINC1=-AINC1
      IF(START1.GT.STOP1.AND.AINC1.GT.0.0)AINC1=-AINC1
C     *****THE FOLLOWING 2 CORRECTIVE LINES ADDED AUGUST 1983*****
      IF(AINC1.EQ.0.0)NUMIT1=1
      IF(AINC1.NE.0.0)NUMIT1=(STOP1-START1)/AINC1
      IF(NUMIT1.LT.0)NUMIT1=-NUMIT1
      NUMIT1=NUMIT1+1
C
C     *****THE FOLLOWING CORRECTIVE LINE ADDED AUGUST 1983*****
      IF(START2.EQ.STOP2)AINC2=0.0
      IF(START2.LT.STOP2.AND.AINC2.LT.0.0)AINC2=-AINC2
      IF(START2.GT.STOP2.AND.AINC2.GT.0.0)AINC2=-AINC2
C     *****THE FOLLOWING 2 CORRECTIVE LINES ADDED AUGUST 1983*****
      IF(AINC2.EQ.0.0)NUMIT2=1
      IF(AINC2.NE.0.0)NUMIT2=(STOP2-START2)/AINC2
      IF(NUMIT2.LT.0)NUMIT2=-NUMIT2
      NUMIT2=NUMIT2+1
C
 3299 CONTINUE
C
C               ***********************************************************
C               **  STEP 5--                                            **
C               **  EXTRACT THE FUNCTIONAL                               **
C               **  EXPRESSION FROM THE INPUT COMMAND LINE.              **
C               ***********************************************************
C
      ISTEPN='5'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MAXN2=MAXCHF
      MAXN3=MAXCHF
      MAXN4=MAXCHF
C
      IF(IHARG(2).EQ.'=')IWD1='='
      IF(IHARG(2).EQ.'=')IWD12=' '
      IF(IHARG(2).NE.'=')IWD1='PLOT'
      IF(IHARG(2).NE.'=')IWD12=' '
      IWD2='FOR'
      IWD22=' '
      CALL DPEXST(IANS,IWIDTH,IWD1,IWD12,IWD2,IWD22,MAXN2,
     1IFUNC2,N2F,IBUGG3,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
      IF(IFOUND.EQ.'NO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3111)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3372)
 3372   FORMAT('      INVALID COMMAND FORM FOR FUNCTION PLOTTING.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3373)
 3373   FORMAT('      GENERAL FORM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3374)
 3374   FORMAT('      PLOT ... = ...  ',
     1         'FOR ... = ... ...')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,3114)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GE.1)THEN
          WRITE(ICOUT,3115)(IANS(I),I=1,MIN(100,IWIDTH))
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               ***********************************************************
C               **  STEP 5.1--                                           **
C               **  FIRST CHECK TO SEE IF HAVE THE VERTICAL LINES CASE;  **
C               **  THEN EXTRACT THE UNDERLYING FUNCTION FROM            **
C               **  FUNCTION DEFINITIONS.                                **
C               ***********************************************************
C
      ISTEPN='5.1'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IVERTI='NO'
      DO3380I=1,NUMARG
        IF(IHARG(I).EQ.'VERT'.AND.IHARG2(I).EQ.'ICAL')GOTO3385
 3380 CONTINUE
      GOTO3389
 3385 CONTINUE
C
      IMAX=N2F-12
      IF(IMAX.LE.0)GOTO3389
      DO3386I=1,IMAX
      IP1=I+1
      IP2=I+2
      IP3=I+3
      IP4=I+4
      IP5=I+5
      IP6=I+6
      IP7=I+7
      IF(IFUNC2(I).NE.'V')GOTO3386
      IF(IFUNC2(IP1).NE.'E')GOTO3386
      IF(IFUNC2(IP2).NE.'R')GOTO3386
      IF(IFUNC2(IP3).NE.'T')GOTO3386
      IF(IFUNC2(IP4).NE.'I')GOTO3386
      IF(IFUNC2(IP5).NE.'C')GOTO3386
      IF(IFUNC2(IP6).NE.'A')GOTO3386
      IF(IFUNC2(IP7).NE.'L')GOTO3386
      IVERTI='YES'
      N2F=I-1
      GOTO3389
 3386 CONTINUE
 3389 CONTINUE
C
      IF(IFLGFB.LE.0)THEN
        CALL DPEXFU(IFUNC2,N2F,IHNAME,IHNAM2,IUSE,IVSTAR,IVSTOP,
     1              NUMNAM,IANS,IWIDTH,IFUNC,NUMCHF,MAXCHF,IFUNC3,
     1              N3F,MAXN3,
     1              IBUGG3,IERROR)
        IF(IERROR.EQ.'YES')GOTO9000
C
C               **********************************************************
C               **  STEP 6--                                            **
C               **  MAKE A NON-CALCULATING PASS AT THE FUNCTION         **
C               **  SO AS TO EXTRACT ALL PARAMETER AND VARIABLE NAMES.  **
C               **********************************************************
C
        ISTEPN='6'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IPASS=1
        CALL COMPIM(IFUNC3,N3F,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1              IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,AJUNK,
     1              IBUGCO,IBUGEV,IERROR)
C
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')THEN
          WRITE(ICOUT,3411)NUMPV,IPARN(1),IPARN2(1),PARAM(1)
 3411     FORMAT('NUMPV,IPARN(1),IPARN2(1),PARAM(1) = ',
     1           I8,2(2X,A4),G15.7)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IERROR.EQ.'YES')GOTO9000
C
C               ***********************************************
C               **  STEP 7--                                 **
C               **  CHECK THAT ALL PARAMETERS                **
C               **  IN THE FUNCTION ARE ALREADY PRESENT      **
C               **  IN THE AVAILABLE NAME LIST IHNAME(.).    **
C               **  ALSO CHECK THAT THE VARIABLE NAME        **
C               **  THAT FOLLOWS FOR (THAT IS, THE DUMMY     **
C               **  VAIRABLE IS IN THE FUNCTION.             **
C               ***********************************************
C
        ISTEPN='7'
        IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1     CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
        IP=0
        IV=0
        IF(NUMPV.GE.1)THEN
          DO3600J=1,NUMPV
            IHPARN=IPARN(J)
            IHPAR2=IPARN2(J)
            IF(IHPARN.EQ.IVDU11.AND.IHPAR2.EQ.IVDU12)GOTO3620
            IF(IHPARN.EQ.IVDU21.AND.IHPAR2.EQ.IVDU22)GOTO3630
            IHWUSE='P'
            MESSAG='YES'
            CALL CHECKN(IHPARN,IHPAR2,IHWUSE,
     1                  IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,
     1                  NUMNAM,MAXNAM,
     1                  ISUBN1,ISUBN2,MESSAG,IANS,IWIDTH,ILOCP,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
C
 3610       CONTINUE
            IP=IP+1
            PARAM(J)=VALUE(ILOCP)
            GOTO3600
C
 3620       CONTINUE
            IV=IV+1
            LOCDU1=J
C
 3630       CONTINUE
            IV=IV+1
            LOCDU2=J
C
 3600     CONTINUE
        ENDIF
CCCCC   THE FOLLOWING 10 LINES WERE ADDED   APRIL 1992 (JJF)
        IF(LOCDU1.LE.0)THEN
           IV=IV+1
           NUMPV=NUMPV+1
           LOCDU1=NUMPV
        ENDIF
        IF(LOCDU2.LE.0)THEN
           IV=IV+1
           NUMPV=NUMPV+1
           LOCDU2=NUMPV
        ENDIF
        NUMPAR=IP
        NUMVAR=IV
      ENDIF
C
C               ******************************
C               **  STEP 8--                **
C               **  EVALUATE THE FUNCTION.  **
C               ******************************
C
      ISTEPN='8'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IPASS=2
      L=NPLOTP
      L2=L
      ISET=0
C
      DO3810I1=1,NUMIT1
        I1M1=I1-1
        AI1=I1
        RESUL1=START1+(AI1-1.0)*AINC1
C
        IF(I1.GT.1)THEN
          IF(START1.LT.STOP1.AND.RESUL1.GT.STOP1)GOTO3818
          IF(START1.GT.STOP1.AND.RESUL1.LT.STOP1)GOTO3818
        ENDIF
        ISET=ISET+1
C
        DO3820I2=1,NUMIT2
          I2M1=I2-1
          AI2=I2
          RESUL2=START2+(AI2-1.0)*AINC2
C
          IF(I2.EQ.1)GOTO3829
          IF(START2.LT.STOP2.AND.RESUL2.GT.STOP2)GOTO3828
          IF(START2.GT.STOP2.AND.RESUL2.LT.STOP2)GOTO3828
 3829     CONTINUE
          L2=L2+1
C
          IF(L2.GT.MAXNPP)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3832)MAXNPP
 3832       FORMAT('      THE NUMBER OF PLOT POINTS HAS JUST EXCEEDED ',
     1             I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          XTEMP=RESUL1
          YTEMP=RESUL2
C
          IF(IFLGFB.LE.0)THEN
            PARAM(LOCDU1)=XTEMP
            PARAM(LOCDU2)=YTEMP
            CALL COMPIM(IFUNC3,N3F,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1                  IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,ZTEMP,
     1                  IBUGCO,IBUGEV,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
          ELSE
C
C           FUNCTION BLOCK CASE:
C
C              STEP 1: COMPUTE FUNCTION BLOCK (BUT FIRST SET CURRENT
C                      VALUE OF DESIRED PARAMETER)
C
            IFEESV=IFEEDB
            IFEEDB='OFF'
C
            DO3901KK=1,2
              AVAL=XTEMP
              IF(KK.EQ.2)AVAL=YTEMP
              IF(IHARG(1).EQ.IFBNA1(1:4) .AND.
     1           IHARG2(1).EQ.IFBNA1(5:8))THEN
                IH=IFBPL1(KK)(1:4)
                IH2=IFBPL1(KK)(5:8)
              ELSEIF(IHARG(1).EQ.IFBNA2(1:4) .AND.
     1               IHARG2(1).EQ.IFBNA2(5:8))THEN
                IH=IFBPL2(KK)(1:4)
                IH2=IFBPL2(KK)(5:8)
              ELSEIF(IHARG(1).EQ.IFBNA3(1:4) .AND.
     1               IHARG2(1).EQ.IFBNA3(5:8))THEN
                IH=IFBPL3(KK)(1:4)
                IH2=IFBPL3(KK)(5:8)
              ENDIF
C
              DO3905II=1,NUMNAM
                IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.
     1             IUSE(II).EQ.'P')THEN
                  VALUE(II)=AVAL
                  IVALUE(II)=INT(AVAL+0.5)
                  GOTO3909
                ENDIF
 3905         CONTINUE
C
C             PARAMETER NAME NOT FOUND IN CURRENT LIST, SO NEED TO ADD
C             TO NAME LIST
C
              IF(NUMNAM.LT.MAXNAM)THEN
                NUMNAM=NUMNAM+1
                IHNAME(NUMNAM)=IH
                IHNAM2(NUMNAM)=IH2
                IUSE(NUMNAM)='P'
                VALUE(NUMNAM)=AVAL
                IVALUE(NUMNAM)=INT(AVAL+ 0.5)
              ELSE
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3111)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3907)
 3907           FORMAT('      MAXIMUM NUMBER OF NAMES EXCEEDED.')
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
 3909         CONTINUE
 3901       CONTINUE
C
            CALL DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                  IBUGG3,IBUGG3,IBUGCO,IBUGEV,IBUGEV,
     1                  ISUBRO,IFOUND,IERROR)
            IFEEDB=IFEESV
            IF(IERROR.EQ.'YES')GOTO9000
C
C           STEP 2: RETRIEVE RESPONSE
C
            DO3915II=1,NUMNAM
              IF(IFBANS(1:4).EQ.IHNAME(II) .AND.
     1           IFBANS(5:8).EQ.IHNAM2(II))THEN
                IF(IUSE(II).EQ.'P')THEN
                  ZTEMP=VALUE(II)
                  GOTO3929
                ELSEIF(IUSE(II).EQ.'V')THEN
                  ICOLR=IVALUE(II)
                  IJ=MAXN*(ICOLR-1)+1
                  IF(ICOLR.LE.MAXCOL)ZTEMP=V(IJ)
                  IF(ICOLR.EQ.MAXCP1)ZTEMP=PRED(1)
                  IF(ICOLR.EQ.MAXCP2)ZTEMP=RES(1)
                  IF(ICOLR.EQ.MAXCP3)ZTEMP=YPLOT(1)
                  IF(ICOLR.EQ.MAXCP4)ZTEMP=XPLOT(1)
                  IF(ICOLR.EQ.MAXCP5)ZTEMP=X2PLOT(1)
                  IF(ICOLR.EQ.MAXCP6)ZTEMP=TAGPLO(1)
                  GOTO3929
                ENDIF
              ENDIF
 3915       CONTINUE
C
C           PARAMETER/VARIABLE NAME NOT FOUND
C
            WRITE(ICOUT,3111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3921)
 3921       FORMAT('      EXPECTED PARAMETER/VARIABLE NOT FOUND IN ',
     1             'NAME TABLE.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3923)IFBANS
 3923       FORMAT('      EXPECTED NAME = ',A8)
            CALL DPWRST('XXX','BUG ')
C
 3929       CONTINUE
C
          ENDIF
C
          IF(IVERTI.EQ.'YES')THEN
            HOLD=ZTEMP
            ZTEMP=YTEMP
            YTEMP=HOLD
          ENDIF
C
          Y(L2)=ZTEMP
          X(L2)=XTEMP
          X3D(L2)=YTEMP
          IF(IAND1.EQ.'NO')D(L2)=ISET
          IF(IAND1.EQ.'YES')D(L2)=ISET+DEL
 3820   CONTINUE
 3828   CONTINUE
 3810 CONTINUE
C
      N2PT1=I2
      GOTO3889
C
 3818 CONTINUE
      N2PT1=I2M1
 3889 CONTINUE
C
      DO3910I2=1,NUMIT2
        I2M1=I2-1
        AI2=I2
        RESUL2=START2+(AI2-1.0)*AINC2
C
        IF(I2.EQ.1)GOTO3919
        IF(START2.LT.STOP2.AND.RESUL2.GT.STOP2)GOTO3918
        IF(START2.GT.STOP2.AND.RESUL2.LT.STOP2)GOTO3918
 3919   CONTINUE
        ISET=ISET+1
C
        DO3920I1=1,NUMIT1
          I1M1=I1-1
          AI1=I1
          RESUL1=START1+(AI1-1.0)*AINC1
C
          IF(I1.GT.1)THEN
            IF(START1.LT.STOP1.AND.RESUL1.GT.STOP1)GOTO3928
            IF(START1.GT.STOP1.AND.RESUL1.LT.STOP1)GOTO3928
          ENDIF
          L2=L2+1
C
          IF(L2.GT.MAXNPP)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3832)MAXNPP
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          XTEMP=RESUL1
          YTEMP=RESUL2
C
          IF(IFLGFB.LE.0)THEN
            PARAM(LOCDU1)=XTEMP
            PARAM(LOCDU2)=YTEMP
            CALL COMPIM(IFUNC3,N3F,IPASS,PARAM,IPARN,IPARN2,NUMPV,
     1                  IANGLU,ITYPEH,IW2HOL,IW22HO,W2HOLD,NWHOLD,ZTEMP,
     1                  IBUGCO,IBUGEV,IERROR)
            IF(IERROR.EQ.'YES')GOTO9000
          ELSE
C
C           FUNCTION BLOCK CASE:
C
C              STEP 1: COMPUTE FUNCTION BLOCK (BUT FIRST SET CURRENT
C                      VALUE OF DESIRED PARAMETER)
C
            IFEESV=IFEEDB
            IFEEDB='OFF'
C
            DO3951KK=1,2
              AVAL=XTEMP
              IF(KK.EQ.2)AVAL=YTEMP
              IF(IHARG(1).EQ.IFBNA1(1:4) .AND.
     1           IHARG2(1).EQ.IFBNA1(5:8))THEN
                IH=IFBPL1(KK)(1:4)
                IH2=IFBPL1(KK)(5:8)
              ELSEIF(IHARG(1).EQ.IFBNA2(1:4) .AND.
     1               IHARG2(1).EQ.IFBNA2(5:8))THEN
                IH=IFBPL2(KK)(1:4)
                IH2=IFBPL2(KK)(5:8)
              ELSEIF(IHARG(1).EQ.IFBNA3(1:4) .AND.
     1               IHARG2(1).EQ.IFBNA3(5:8))THEN
                IH=IFBPL3(KK)(1:4)
                IH2=IFBPL3(KK)(5:8)
              ENDIF
C
              DO3955II=1,NUMNAM
                IF(IH.EQ.IHNAME(II) .AND. IH2.EQ.IHNAM2(II) .AND.
     1             IUSE(II).EQ.'P')THEN
                  VALUE(II)=AVAL
                  IVALUE(II)=INT(AVAL+0.5)
                  GOTO3959
                ENDIF
 3955         CONTINUE
C
C             PARAMETER NAME NOT FOUND IN CURRENT LIST, SO NEED TO ADD
C             TO NAME LIST
C
              IF(NUMNAM.LT.MAXNAM)THEN
                NUMNAM=NUMNAM+1
                IHNAME(NUMNAM)=IH
                IHNAM2(NUMNAM)=IH2
                IUSE(NUMNAM)='P'
                VALUE(NUMNAM)=AVAL
                IVALUE(NUMNAM)=INT(AVAL+ 0.5)
              ELSE
                WRITE(ICOUT,999)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3111)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,3907)
                CALL DPWRST('XXX','BUG ')
              ENDIF
C
 3959         CONTINUE
 3951       CONTINUE
C
            CALL DPFBEX(IFBNAM,IANGLU,ISEED,IFTEXP,IFTORD,IFORSW,
     1                  IBUGG3,IBUGG3,IBUGCO,IBUGEV,IBUGEV,
     1                  ISUBRO,IFOUND,IERROR)
            IFEEDB=IFEESV
            IF(IERROR.EQ.'YES')GOTO9000
C
C           STEP 2: RETRIEVE RESPONSE
C
            DO3970II=1,NUMNAM
              IF(IFBANS(1:4).EQ.IHNAME(II) .AND.
     1           IFBANS(5:8).EQ.IHNAM2(II))THEN
                IF(IUSE(II).EQ.'P')THEN
                  ZTEMP=VALUE(II)
                  GOTO3979
                ELSEIF(IUSE(II).EQ.'V')THEN
                  ICOLR=IVALUE(II)
                  IJ=MAXN*(ICOLR-1)+1
                  IF(ICOLR.LE.MAXCOL)ZTEMP=V(IJ)
                  IF(ICOLR.EQ.MAXCP1)ZTEMP=PRED(1)
                  IF(ICOLR.EQ.MAXCP2)ZTEMP=RES(1)
                  IF(ICOLR.EQ.MAXCP3)ZTEMP=YPLOT(1)
                  IF(ICOLR.EQ.MAXCP4)ZTEMP=XPLOT(1)
                  IF(ICOLR.EQ.MAXCP5)ZTEMP=X2PLOT(1)
                  IF(ICOLR.EQ.MAXCP6)ZTEMP=TAGPLO(1)
                  GOTO3979
                ENDIF
              ENDIF
 3970       CONTINUE
C
C           PARAMETER/VARIABLE NAME NOT FOUND
C
            WRITE(ICOUT,3111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3921)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,3923)IFBANS
            CALL DPWRST('XXX','BUG ')
C
 3979       CONTINUE
C
          ENDIF
C
          IF(IVERTI.EQ.'YES')THEN
            HOLD=ZTEMP
            ZTEMP=YTEMP
            YTEMP=HOLD
          ENDIF
C
          Y(L2)=ZTEMP
          X(L2)=XTEMP
          X3D(L2)=YTEMP
          IF(IAND1.EQ.'NO')D(L2)=ISET
          IF(IAND1.EQ.'YES')D(L2)=ISET+DEL
 3920   CONTINUE
 3928   CONTINUE
 3910 CONTINUE
C
      N2PT2=I2
      GOTO3989
C
 3918 CONTINUE
      N2PT2=I2M1
C
 3989 CONTINUE
      L=L2
      NPLOTP=L
      N2=N2PT1+N2PT2
C
C               *****************************
C               **  STEP 9--               **
C               **  DETERMINE THE NUMBER   **
C               **  OF PLOT VARIABLES.     **
C               **  STORE THIS IN NPLOTV.  **
C               *****************************
C
      ISTEPN='9'
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DHOLD=D(1)
      DO4830I=1,NPLOTP
        IF(D(I).NE.DHOLD)GOTO4835
 4830 CONTINUE
      NPLOTV=2
      GOTO4890
 4835 CONTINUE
      NPLOTV=3
      GOTO4890
C
 4890 CONTINUE
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'ON' .OR. ISUBRO.EQ.'3DP3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP3DP3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,MAXNPP,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,MAXNPP,ICASPL,IAND1,IAND2 = ',
     1         4I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DP3DPL(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,IFORSW,MAXNPP,
     1                  IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,
     1                  IFOUND,IERROR)
C
C     PURPOSE--FORM A VARIETY OF 3-DIMENSIONAL PLOTS--
C                1) A Y VERSUS X1 AND X2 PLOT,
C                2) A MULTI-TRACE (OR MULLTI-SURFACE) PLOT
C                3) A 3-DIMENSIONAL FUNCTION PLOT.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20878
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--JANAURY   1981.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICASPL
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IANGLU
      CHARACTER*4 IFORSW
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ITYPEH
      CHARACTER*4 IW2HOL
      CHARACTER*4 IW22HO
      CHARACTER*4 IPARN
      CHARACTER*4 IPARN2
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
C     ***** THE FOLLOWING 4 DIMENSIONS RAISED FROM 150 TO 225 AUGUST 1983 *****
C     ***** THE FOLLOWING 4 DIMENSIONS RAISED FROM 225 TO 1000 AUGUST 1986 *****
CCCCC DIMENSION ITYPEH(225)
CCCCC DIMENSION IW2HOL(225)
CCCCC DIMENSION IW22HO(225)
CCCCC DIMENSION W2HOLD(225)
      DIMENSION ITYPEH(1000)
      DIMENSION IW2HOL(1000)
      DIMENSION IW22HO(1000)
      DIMENSION W2HOLD(1000)
C
      DIMENSION PARAM(100)
      DIMENSION IPARN(100)
      DIMENSION IPARN2(100)
C
      DIMENSION IVSLOC(100)
      DIMENSION IEQLOC(100)
      DIMENSION IFOLOC(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
C
      ISUBN1='DP3D'
      ISUBN2='PL  '
C
      MAXCP1=MAXCOL+1
      MAXCP2=MAXCOL+2
      MAXCP3=MAXCOL+3
      MAXCP4=MAXCOL+4
      MAXCP5=MAXCOL+5
      MAXCP6=MAXCOL+6
C
C     ***** THE FOLLOWING 6 LINES INSERTED AUGUST 1983 *****
      DO40I=1,1000
        ITYPEH(I)='    '
        IW2HOL(I)='    '
        IW22HO(I)='    '
        W2HOLD(I)=0.0
   40 CONTINUE
C
C               ***************************
C               **  TREAT THE PLOT CASE  **
C               ***************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP3DPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NPLOTV,NPLOTP,NS,MAXNPP
   52   FORMAT('NPLOTV,NPLOTP,NS,MAXNPP = ',4I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICASPL,IAND1,IAND2,IFOUND,IERROR
   53   FORMAT('ICASPL,IAND1,IAND2,IFOUND,IERROR = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IANGLU,IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ
   54   FORMAT('IANGLU,IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ***************************
C               **  STEP 1--             **
C               **  EXTRACT THE COMMAND  **
C               ***************************
C
      ISTEPN='1'
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(ICOM.EQ.'3DPL'.AND.ICOM2.EQ.'OT')THEN
        ILASTC=0
      ELSEIF(NUMARG.GE.1.AND.
     1       ICOM.EQ.'3D'.AND.IHARG(1).EQ.'PLOT')THEN
        ILASTC=1
      ELSEIF(NUMARG.GE.2.AND. ICOM.EQ.'3' .AND.
     1       IHARG(1).EQ.'D'.AND.IHARG(2).EQ.'PLOT')THEN
        ILASTC=2
      ELSE
        IFOUND='NO'
        GOTO9000
      ENDIF
C
      IF(ILASTC.GT.0)THEN
        CALL ADJUST(ILASTC,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG)
      ENDIF
      IFOUND='YES'
C
C               *******************************************************
C               **  STEP 2--                                         **
C               **  CHECK FOR THE PROPER NUMBER OF INPUT ARGUMENTS.  **
C               *******************************************************
C
      ISTEPN='2'
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      MINNA=2
      MAXNA=100
      CALL CHECKA(NUMARG,MINNA,MAXNA,IANS,IWIDTH,ISUBN1,ISUBN2,IERROR)
      IF(IERROR.EQ.'YES')GOTO9000
C
C               *******************************************
C               **  STEP 3--                             **
C               **  DETERMINE IF HAD OR HAVE THE 'AND'   **
C               **  CONTINUATION CASE.                   **
C               **  IF THE PREVIOUS PLOT COMMAND LINE    **
C               **  HAD AN 'AND' CONTINUATION,           **
C               **  OR IF THE PRESENT PLOT COMMAND LINE  **
C               **  HAS AN 'AND' CONTINUATION,           **
C               **  THEN SET SOME FLAG VARIABLES.        **
C               *******************************************
C
C
      ISTEPN='3'
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IAND1.EQ.'NO')NPLOTV=0
      IF(IAND1.EQ.'NO')NPLOTP=0
      IAND2='NO'
      IF(IHARG(NUMARG).EQ.'AND')IAND2='YES'
      L=NPLOTP
C
C               ***************************************
C               **  STEP 4--                         **
C               **  DETERMINE THE TYPE OF PLOT CASE  **
C               **  (FOR THIS COMMAND LINE ONLY)--   **
C               **       1)  PLOT ... VERSUS         **
C               **       2)  PLOT ... FOR X =        **
C               **       3)  NEITHER OF THE ABOVE.   **
C               ***************************************
C
      ISTEPN='4'
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      ICASPL='3DNO'
C
      NUMEQ=0
      NUMFO=0
      NUMVS=0
      NUMDV=0
      DO811J=1,NUMARG
        J2=J
        IF(IHARG(J).EQ.'=')THEN
          NUMEQ=NUMEQ+1
          IEQLOC(NUMEQ)=J2
        ELSEIF(IHARG(J).EQ.'VS' .OR. IHARG(J).EQ.'VS.' .OR.
     1     IHARG(J).EQ.'VERS' .AND. IHARG2(J).EQ.'US  ')THEN
          NUMVS=NUMVS+1
          IVSLOC(NUMVS)=J2
        ELSEIF(IHARG(J).EQ.'FOR')THEN
          JP1=J+1
          IF(JP1.GT.NUMARG)GOTO837
          IF(IHARG(JP1).EQ.'I   '.AND.IHARG2(JP1).EQ.'    ')GOTO837
          IF(IHARG(JP1).EQ.'ROW '.AND.IHARG2(JP1).EQ.'    ')GOTO837
          NUMDV=NUMDV+1
  837     CONTINUE
          NUMFO=NUMFO+1
          IFOLOC(NUMFO)=J2
        ENDIF
  811 CONTINUE
C
      IF(NUMEQ.EQ.0)ICASPL='3DNO'
      IF(NUMEQ.EQ.1.AND.NUMFO.EQ.1.AND.NUMDV.LE.0)ICASPL='3DNO'
      IF(NUMEQ.EQ.1.AND.NUMFO.EQ.1.AND.NUMDV.GE.1)ICASPL='3DEF'
      IF(NUMEQ.GE.2)ICASPL='3DEF'
      IF(NUMVS.GE.1)ICASPL='3DVS'
C
C               ******************************************
C               **  STEP 5--                            **
C               **  BRANCH ACCORDING TO THE PLOT CASE.  **
C               ******************************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')THEN
        ISTEPN='5'
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,911)
  911   FORMAT('FROM THE MIDDLE OF DP3DPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,912)ICASPL
  912   FORMAT('ICASPL = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ICASPL.EQ.'3DNO')THEN
        CALL DP3DP1(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              MAXNPP,
     1              IBUGG3,IBUGQ,IFOUND,IERROR)
      ELSEIF(ICASPL.EQ.'3DVS')THEN
        CALL DP3DP2(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IVSLOC,NUMVS,
     1              MAXNPP,
     1              IBUGG3,IBUGQ,IFOUND,IERROR)
      ELSEIF(ICASPL.EQ.'3DEF')THEN
        CALL DP3DP3(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              PARAM,IPARN,IPARN2,NUMPAR,ITYPEH,IW2HOL,IW22HO,
     1              W2HOLD,NWHOLD,
     1              IFOLOC,MAXNPP,IANGLU,IFORSW,
     1              IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
      ELSE
        WRITE(ICOUT,921)
  921   FORMAT('***** ERROR IN 3D PLOT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,922)
  922   FORMAT('      ICASPL NOT   3DNO, 3DVS, OR 3DEF')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,923)ICASPL,NUMEQ,NUMFO,NUMDV
  923   FORMAT('ICASPL,NUMEQ,NUMFO,NUMDV = ',A4,3I8)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'3DPL')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF DP3DPL--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013   FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1         3I8,3(2X,A4))
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9020)
 9020   FORMAT('I,Y(.),X(.),D(.),ISUB(.)--')
        CALL DPWRST('XXX','BUG ')
        DO9021I=1,NPLOTP
          WRITE(ICOUT,9022)I,Y(I),X(I),D(I),ISUB(I)
 9022     FORMAT(I8,E15.7,E15.7,E15.7,I8)
          CALL DPWRST('XXX','BUG ')
 9021   CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE DP3DTR(Z,X,Y,N,XEYE0,YEYE0,ZEYE0,XORIG,YORIG,ZORIG,
     1I3DTRA,
     1XEYE,YEYE,ZEYE,
     1ZT,XT,IBUGPL,ISUBRO,IERROR)
C
C     PURPOSE--CARRY OUT THE PERSPECTIVE TRANSFORMATION
C              WHICH CONVERTS POINTS IN 3-SPACE
C              TO POINTS ON THE PERSPECTIVE PLANE.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82/7
C     ORIGINAL VERSION--MARCH     1979.
C     UPDATED         --APRIL     1979.
C     UPDATED         --JANUARY   1981.
C     UPDATED         --FEBRUARY  1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --OCTOBER   1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --JULY      1986.
C     UPDATED         --SEPTEMBER 1988.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 I3DTRA
C
      CHARACTER*4 IBUGPL
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
C---------------------------------------------------------------------
C
      DIMENSION Z(*)
      DIMENSION X(*)
      DIMENSION Y(*)
C
      DIMENSION ZT(*)
      DIMENSION XT(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DP3D'
      ISUBN2='TR  '
C
      IERROR='NO'
C
      DNXX=0.0
      DNXY=0.0
      DNXZ=0.0
      DNYX=0.0
      DNYY=0.0
      DNYZ=0.0
      DNZX=0.0
      DNZY=0.0
      DNZZ=0.0
      DCXX=0.0
      DCXY=0.0
      DCXZ=0.0
      DCYX=0.0
      DCYY=0.0
      DCYZ=0.0
      DCZX=0.0
      DCZY=0.0
      DCZZ=0.0
      DENOMX=0.0
      DENOMY=0.0
      DENOMZ=0.0
C
      IF(IBUGPL.EQ.'OFF'.AND.ISUBRO.NE.'3DTR')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP3DTR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGPL,ISUBRO,IERROR
   52 FORMAT('IBUGPL,ISUBRO,IERROR = ',3A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)I3DTRA
   53 FORMAT('I3DTRA = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,61)N
   61 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,62)XEYE0,YEYE0,ZEYE0
   62 FORMAT('XEYE0, YEYE0, ZEYE0 = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,63)XORIG,YORIG,ZORIG
   63 FORMAT('XORIG, YORIG, ZORIG = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      DO65I=1,N
      WRITE(ICOUT,66)I,Z(I),X(I),Y(I)
   66 FORMAT('I,Z(I),X(I),Y(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
   65 CONTINUE
   90 CONTINUE
C
C               ********************************************
C               **  STEP 11--                             **
C               **  COMPUTE THE MIN AND MAX OF THE DATA.  **
C               ********************************************
C
      ISTEPN='11'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      XMIN=X(1)
      XMAX=X(1)
      YMIN=Y(1)
      YMAX=Y(1)
      ZMIN=Z(1)
      ZMAX=Z(1)
C
      DO1100I=1,N
      IF(X(I).LT.XMIN)XMIN=X(I)
      IF(X(I).GT.XMAX)XMAX=X(I)
      IF(Y(I).LT.YMIN)YMIN=Y(I)
      IF(Y(I).GT.YMAX)YMAX=Y(I)
      IF(Z(I).LT.ZMIN)ZMIN=Z(I)
      IF(Z(I).GT.ZMAX)ZMAX=Z(I)
 1100 CONTINUE
      XRANGE=XMAX-XMIN
      YRANGE=YMAX-YMIN
      ZRANGE=ZMAX-ZMIN
C
C               ****************************************
C               **  STEP 12--                         **
C               **  IF (XEYE0,YEYE0,ZEYE0) IS UNDEFINED  **
C               **  (THAT IS, = CPU MINIMUM),         **
C               **  THEN COMPUTE DEFAULT VALUES.      **
C               ****************************************
C
      ISTEPN='12'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
      XEYE=XEYE0
      YEYE=YEYE0
      ZEYE=ZEYE0
      IF(XEYE0.LE.CPUMIN)XEYE=XMAX+3.0*XRANGE
      IF(YEYE0.LE.CPUMIN)YEYE=YMAX+3.0*YRANGE
      IF(ZEYE0.LE.CPUMIN)ZEYE=ZMAX+3.0*ZRANGE
C
C               *******************************************
C               **  STEP 13--                            **
C               **  COMPUTE MIDRANGES FOR THE X, Y,      **
C               **  AND Z VECTORS.                       **
C               **  THIS WILL DEFINE THE 'MIDDLE POINT'  **
C               **  OF THE 3-D PLOT.                     **
C               *******************************************
C
      ISTEPN='13'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      XM=(XMIN+XMAX)/2.0
      YM=(YMIN+YMAX)/2.0
      ZM=(ZMIN+ZMAX)/2.0
C
C               ***************************************************
C               **  STEP 14--                                    **
C               **  COMPUTE THE ENDPONTS OF THE 3-PRONGED AXIS.  **
C               ***************************************************
C
      ISTEPN='14'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      FACTOR=1.25
C
      XORIG2=XORIG
      YORIG2=YORIG
      ZORIG2=ZORIG
      IF(XORIG.EQ.CPUMIN)XORIG2=XMIN
      IF(YORIG.EQ.CPUMIN)YORIG2=YMIN
      IF(ZORIG.EQ.CPUMIN)ZORIG2=ZMIN
C
      NP1=N+1
      X(NP1)=XORIG2
      Y(NP1)=YORIG2
      Z(NP1)=ZORIG2
C
      NP2=N+2
      X(NP2)=XORIG2+FACTOR*XRANGE
      Y(NP2)=YORIG2
      Z(NP2)=ZORIG2
C
      NP3=N+3
      X(NP3)=XORIG2
      Y(NP3)=YORIG2
      Z(NP3)=ZORIG2
C
      NP4=N+4
      X(NP4)=XORIG2
      Y(NP4)=YORIG2+FACTOR*YRANGE
      Z(NP4)=ZORIG2
C
      NP5=N+5
      X(NP5)=XORIG2
      Y(NP5)=YORIG2
      Z(NP5)=ZORIG2
C
      NP6=N+6
      X(NP6)=XORIG2
      Y(NP6)=YORIG2
      Z(NP6)=ZORIG2+FACTOR*ZRANGE
C
C               ***************************************************************
C               **  STEP 15--                                                **
C               **  DETERMINE 3 POINTS WHICH WILL DEFINE EXTREMAL POINTS     **
C               **  ON THE VISUAL PLANE.                                     **
C               **  THIS IS NEEDED SO THAT THE UNDERLYING GRAPHICS SOFTWARE  **
C               **  WILL SHOW A CLOSE POINT CLOAD/FIGURE                     **
C               **  AS BEING LARGE IN APPEARANCE,                            **
C               **  AND A DISTANT POINT CLOUD                                **
C               **  AS BEING SMALL IN APPEARANCE.                            **
C               **  SUCH A STEP IS NECESSARY BECAUSE THE                     **
C               **  UNDERLYING GRAPHICS SOFTWARE WILL BY DEFAULT             **
C               **  GIVE FULL RESOLUTION TO ALL DATA CLOUDS/FIGRUES          **
C               **  WHICH WILL HAVE THE NET EFFECT OF                        **
C               **  ALL DATA CLOUDS/FIGURES BEING LARGE.                     **
C               **  THE 3 CALCULATED EXTREMAL POINTS WILL NEVER              **
C               **  EXPLICITELY APPEAR ON THE PLOT (THEY WILL                **
C               **  HAVE A BLANK PLOT CHARAXCTER AUTOMATICALLY);             **
C               **  THERE EXISTENCE ONLY SERVES TO ASSURE THAT THE           **
C               **  PLOT WINDOW IS APPROPRIATELY STRETCHED.                  **
C               ***************************************************************
C
C               ************************************************************
C               **  STEP 15.1--                                           **
C               **  DEFINE THE PERIPHERAL VISION ANGLE.                   **
C               **  THIS HAS BEEN SET TO 60 DEGREES                       **
C               **  (30   DEGREES ABOVE THE NORMAL LINE                   **
C               **  TO THE VISUAL PLANE AND 30   DEGREES BELOW            **
C               **  THE NORMAL LINE TO THE PLANE).                        **
C               **  COMPUTE THE RADIUS OF THE CIRCLE ON THE VISUAL PLANE  **
C               **  WHICH IS JUST AT THE EDGE OF VISIBILITY--             **
C               **  THE LARGER THE PERIPHERAL ANGLE,                      **
C               **  THE LARGER THE RADIUS, AND VICE VERSA.                **
C               ************************************************************
C
      ISTEPN='15.1'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      THETA=3.1415926/12.0
      ARG=(XEYE-XM)**2+(YEYE-YM)**2+(ZEYE-ZM)**2
      DIST=0.0
      IF(ARG.GT.0.0)DIST=SQRT(ARG)
      RADIUS=DIST*TAN(THETA)
      IF(IBUGPL.EQ.'OFF'.AND.ISUBRO.NE.'3DTR')GOTO1519
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1511)
 1511 FORMAT('***** FROM THE MIDDLE OF DP3DTR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1512)THETA,ARG,DIST,RADIUS
 1512 FORMAT('THETA,ARG,DIST,RADIUS = ',4E15.7)
      CALL DPWRST('XXX','BUG ')
 1519 CONTINUE
C
C               ***********************************************************
C               **  STEP 15.2--                                          **
C               **  DETERMINE THE 2 POINTS ON THIS CIRCLE OF VISIBILITY  **
C               **  WHICH INTERSECT WITH THE X = XM PLANE.               **
C               ***********************************************************
C
      ISTEPN='15.2'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      XD=XM
      YD1=YM
      YD2=YM
      ZD1=ZM
      ZD2=ZM
C
C     ***** 7 LINES OF CODE TO ADJUST FOR DIVISION BY 0 ADDED AUGUST 1983 *****
      EPS=0.0000001
      XDEL=XEYE-XM
      IF(XDEL.EQ.0.0)XDEL=EPS
      YDEL=YEYE-YM
      IF(YDEL.EQ.0.0)YDEL=EPS
      ZDEL=ZEYE-ZM
      IF(ZDEL.EQ.0.0)ZDEL=EPS
C
      DISC=1.0+(ZDEL/YDEL)**2
      DENOM=0.0
      IF(DISC.GT.0.0)DENOM=SQRT(DISC)
      IF(DISC.LT.0.0)GOTO1520
      ZD1=ZM+RADIUS/DENOM
      ZD2=ZM+RADIUS/(-DENOM)
      YD1=CPUMIN
      IF(YDEL.NE.0.0)YD1=YM-ZDEL*(ZD1-ZM)/YDEL
      YD2=CPUMAX
      IF(YDEL.NE.0.0)YD2=YM-ZDEL*(ZD2-ZM)/YDEL
 1520 CONTINUE
C
      NP7=N+7
      X(NP7)=XM
      Y(NP7)=YD1
      Z(NP7)=ZD1
C
      NP8=N+8
      X(NP8)=XM
      Y(NP8)=YD2
      Z(NP8)=ZD2
C
      IF(IBUGPL.EQ.'OFF'.AND.ISUBRO.NE.'3DTR')GOTO1529
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1521)XM,RADIUS
 1521 FORMAT('XM,RADIUS = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1522)DISC,DENOM
 1522 FORMAT('DISC,DENOM = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1523)XD,YD1,YD2,ZD1,ZD2
 1523 FORMAT('XD,YD1,YD2,ZD1,ZD2 = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1524)N,NP7,X(NP7),Y(NP7),Z(NP7)
 1524 FORMAT('N,NP7,X(NP7),Y(NP7),Z(NP7) = ',2I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1525)N,NP8,X(NP8),Y(NP8),Z(NP8)
 1525 FORMAT('N,NP8,X(NP8),Y(NP8),Z(NP8) = ',2I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 1529 CONTINUE
C
C               ***********************************************************
C               **  STEP 15.3--                                          **
C               **  DETERMINE THE 2 POINTS ON THIS CIRCLE OF VISIBILITY  **
C               **  WHICH INTERSECT WITH THE Y = YM PLANE.               **
C               ***********************************************************
C
      ISTEPN='15.3'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      XD1=XM
      XD2=XM
      YD=YM
      ZD1=ZM
      ZD2=ZM
C
C     ***** 3 LINES OF CODE TO CHECK FOR DIVISION BY 0 ADDED AUGUST 1983 *****
      DISC=CPUMAX
      IF(XDEL.NE.0.0)DISC=1.0+(ZDEL/XDEL)**2
      DENOM=0.0
      IF(DISC.GT.0.0)DENOM=SQRT(DISC)
      IF(DISC.LT.0.0)GOTO1530
      ZD1=ZM+RADIUS/DENOM
      ZD2=ZM+RADIUS/(-DENOM)
      XD1=CPUMIN
      IF(XDEL.NE.0.0)XD1=XM-ZDEL*(ZD1-ZM)/XDEL
      XD2=CPUMAX
      IF(XDEL.NE.0.0)XD2=XM-ZDEL*(ZD2-ZM)/XDEL
C
 1530 CONTINUE
      NP9=N+9
      X(NP9)=XD1
      Y(NP9)=YM
      Z(NP9)=ZD1
C
      NP10=N+10
      X(NP10)=XD2
      Y(NP10)=YM
      Z(NP10)=ZD2
C
      IF(IBUGPL.EQ.'OFF'.AND.ISUBRO.NE.'3DTR')GOTO1539
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1531)YM,RADIUS
 1531 FORMAT('YM,RADIUS = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1532)DISC,DENOM
 1532 FORMAT('DISC,DENOM = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1533)XD1,XD2,YD,ZD1,ZD2
 1533 FORMAT('XD1,XD2,YD,ZD1,ZD2 = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1534)N,NP9,X(NP9),Y(NP9),Z(NP9)
 1534 FORMAT('N,NP9,X(NP9),Y(NP9),Z(NP9) = ',2I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1535)N,NP10,X(NP10),Y(NP10),Z(NP10)
 1535 FORMAT('N,NP10,X(NP10),Y(NP10),Z(NP10) = ',2I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 1539 CONTINUE
C
C               ***********************************************************
C               **  STEP 15.4--                                          **
C               **  DETERMINE THE 2 POINTS ON THIS CIRCLE OF VISIBILITY  **
C               **  WHICH INTERSECT WITH THE Z = ZM PLANE.               **
C               ***********************************************************
C
      ISTEPN='15.4'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      XD1=XM
      XD2=XM
      YD1=YM
      YD2=YM
      ZD=ZM
C
C     ***** 3 LINES OF CODE TO CHECK FOR DIVISION BY 0 ADDED AUGUST 1983 *****
      DISC=CPUMAX
      IF(YDEL.NE.0.0)DISC=1.0+(XDEL/YDEL)**2
      DENOM=0.0
      IF(DISC.GT.0.0)DENOM=SQRT(DISC)
      IF(DISC.LT.0.0)GOTO1540
      XD1=XM+RADIUS/DENOM
      XD2=XM+RADIUS/(-DENOM)
      YD1=CPUMIN
      IF(YDEL.NE.0.0)YD1=YM-XDEL*(XD1-XM)/YDEL
      YD2=CPUMAX
      IF(YDEL.NE.0.0)YD2=YM-XDEL*(XD2-XM)/YDEL
C
 1540 CONTINUE
      NP11=N+11
      X(NP11)=XD1
      Y(NP11)=YD1
      Z(NP11)=ZM
C
      NP12=N+12
      X(NP12)=XD2
      Y(NP12)=YD2
      Z(NP12)=ZM
C
      IF(IBUGPL.EQ.'OFF'.AND.ISUBRO.NE.'3DTR')GOTO1549
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1541)ZM,RADIUS
 1541 FORMAT('ZM,RADIUS = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1542)DISC,DENOM
 1542 FORMAT('DISC,DENOM = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1543)XD1,XD2,YD1,YD2,ZD
 1543 FORMAT('XD1,XD2,YD1,YD2,ZD = ',5E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1544)N,NP11,X(NP11),Y(NP11),Z(NP11)
 1544 FORMAT('N,NP11,X(NP11),Y(NP11),Z(NP11) = ',2I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1545)N,NP12,X(NP12),Y(NP12),Z(NP12)
 1545 FORMAT('N,NP12,X(NP12),Y(NP12),Z(NP12) = ',2I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 1549 CONTINUE
C
C               *********************************************************
C               **  STEP 20--
C               **  (GENERAL DISCUSSION)                               **
C               **  DETERMINE (IN ORIGINAL COORDINATE SYSTEM VALUES)          **
C               **  WHERE THE DATA POINTS FALL ON THE VISUAL PLANE.           **
C               **  FOR EACH (XD,YD,ZD) DATA POINT,                           **
C               **  DETERMINE WHERE THE VISUAL RAY FROM                       **
C               **  THE DATA POINT TO OUR EYE                                 **
C               **  STRIKES THE VISUAL (PERSPECTIVE) PLANE.                   **
C               **  THE VISUAL PLANE IS THAT PLANE                            **
C               **  WHICH IS NORMAL TO OUR EYE                                **
C               **  AND WHICH CONTAINS THE AVERAGE POINT (XM,YM,ZM).          **
C               **  THE EQUATION OF THE VISUAL PLANE IS                       **
C               **  (XEYE-XM)(X-XM) + (YEYE-YM)(Y-YM) + (ZEYE-ZM)(Z-ZM) = 0
C               **  WHERE X, Y, Z ARE THE DUMMY VARIABLES                     **
C               **  REPRESENTING ANY POINT (X,Y,Z) ON THAT PLANE.             **
C               **  THIS EQUATION MUST BE SOLVED FOR X, Y, AND Z.             **
C               **  THE EQUATIONS OF THE LINE FROM THE DATA POINT (XD,YD,ZD)  **
C               **  TO OUR EYE (XEYE,YEYE,ZEYE) ARE
C               **  (X-XD)/(XEYE-XD) = (Y-YD)/(YEYE-YD) = (Z-ZD)/(ZEYE-ZD)
C               **  WHERE (XD,YD,ZD) REPRESENTS A DATA POINT.                 **
C               **  THE VISUAL PLANE EQUATION AND THE LINE EQUATIONS          **
C               **  MUST BE COMBINED TO SOLVE FOR THE VALUES (X,Y,Z)          **
C               **  ON THE VISUAL PLANE AS OUR EYE SEES THEM.                 **
C               ****************************************************************
C
C               ****************************************************************
C               **  STEP 21--
C               **  THE FINAL PLOT STATEMENT WILL INVOLVE
C               **  ONLY 2 VECTORS.
C               **  AT THE MOMENT, THE POINTS (XP,YP,ZP)
C               **  ON THE VISUAL PLANE ARE DEFINED
C               **  BY 3 COORDINATE VALUES.
C               **  TO REDUCE THE 3 COORDINATE VALUES
C               **  TO 2 COORDINATE VALUES,
C               **  WE MUST ROTATE THE VISUAL PLANE
C               **  SO THAT IT IS PARALLEL TO THE ORIGINAL XZ PLANE.
C               **  TO CARRY OUT SUCH A ROTATION, WE MUST
C               **  DETERMINE THE DIRECTION NUMBERS AND DIRECTION COSINES
C               **  OF THE NEW AXES IN TERMS OF THE OLD COORDINATE SYSTEM.
C               **  THE NEW Y AXIS WILL (BY CONSTRUCTION) BE
C               **  ON THE NORMAL LINE TRAVELING FROM
C               **  THE AVERAGE POINT (XM,YM,ZM) TO OUR EYE POINT (XEYE,YEYE,ZEY
C               **  AND WILL THEREFORE HAVE DIRECTIONS NUMBERS XEYE, YEYE, AND Z
C               **  THE NEW Z AXIS WILL BE PERPENDICULAR TO THE NEW Y AXIS
C               **  AND WILL RESIDE IN THE PLANE CONTAINING THE
C               **  THE FOLLOWING 3 POINTS--
C               **      1) THE AVERAGE POINT (XM,YM,ZM)
C               **      2) THE EYE POINT (XEYE,YEYE,ZEYE)
C               **      3) SOME POINT (SAY (XM,YM,ZM+1)) OF THE OLD Z AXIS
C               **         DISPLACED OVER SO AS TO EMANATE FROM (XM,YM,ZM).
C               **  THE ABOVE 3 POINTS DEFINE A VERTICAL PLANE.
C               **  THE PURPOSE OF THE VERTICAL PLANE IS TO DEFINE
C               **  WHICH DIRECTION IS 'UP' IN THE FINAL PICTURE.
C               **  THE EQUATION OF THE VERTICAL PLANE IS
C               **  (A-XM)(X-XM) + (B-YM)(Y-YM) + (C-ZM)(Z-ZM) = 0 .
C               **  THIS EQUATION MUST BE SOLVED FOR A, B, AND C.
C               **  WITHOUT LOSS OF GENERALITY, A MAY BE INITIALLY SET TO 1.
C               **  THE SOLUTION TURNS OUT TO BE
C               **      A = 1
C               **      B = -XEYE/YEYE
C               **      C = 0
C               **  NOTE, HOWEVER, THAT THESE A, B, AND C VALUES
C               **  FOR THIS VERTICAL PLANE WILL BE IDENTICAL TO THE
C               **  DIRECTION NUMBERS FOR THE NORMAL TO THIS VERTICAL PLANE
C               **  WHICH IS IDENTICALLY THE NEW X AXIS
C               **  AND SO THE ABOVE A, B, AND C VALUES DEFINE THE DIRECTION
C               **  DIRECTION NUMBERS FOR THE NEW X AXIS.
C               **  TO SOLVE FOR THE DIRECTION NUMBERS FOR THE NEW Z AXIS,
C               **  WE SEEK 3 DIRECTION NUMBERS D, E, AND F
C               **  WHICH MUST BE PERPENDICULAR TO BOTH THE
C               **  NEW Y AXIS (WITH DIRECTION NUMBERS XEYE, YEYE, AND ZEYE)
C               **  AND THE NEW X AXIS (WITH DIRECTION NUMBERS A, B, AND C ABOVE
C               **  WITHOUT LOSS OF GENERALITY, D MAY BE INITIALLY SET TO 1.
C               **  NOTE THAT WHENEVER 2 LINES ARE PERPENDICULAR,
C               **  THE INNER PRODUCT OF THE DIRECTION NUMBERS MUST = 0.
C               **  WITHOUT LOSS OF GENERALITY, D MAY BE INITIALLY SET TO 1.
C               **  INCORPORATING THE 2 INNER PRODUCT EQUATIONS,
C               **  WE MAY SOLVE FOR E AND F.
C               **  THE SOLUTIONS TURN OUT TO BE
C               **      D = 1
C               **      E = YEYE/XEYE
C               **      F = (-XEYE*XEYE - YEYE*YEYE) / (XEYE*ZEYE)
C               **
C               **  IN SUMMARY, THE DIRECTION NUMBERS FOR THE 3 NEW AXES
C               **  MAY BE WRITTEN AS
C               **      NEW X AXIS:  YEYE       -XEYE     0
C               **      NEW Y AXIS:  XEYE       YEYE      ZEYE
C               **      NEW Z AXIS:  -XEYE*ZEYE   -YEYE*ZEYE  XEYE*XEYE+YEYE*YEY
C               **  NOTE THAT BY INSPECTION WE SEE RETROSPECTIVELY
C               **  THAT THE 3 INNER PRODUCTS ALL = 0
C               **  AND SO THE 3 DEFINED AXES ARE ALL PERPENDICULAR
C               **  (AS THEY SHOULD BE).
C               **
C               **  THE CORRESPONDING DIRECTION COSINES
C               **  ARE GOTTEN BY NORMALIZATION TO UNITY;
C               **  LET US SYMBOLICALLY REPRESENT THEM BY--
C               **      DCXX   DCXY   DCXZ
C               **      DCYX   DCYY   DCYZ
C               **      DCZX   DCZY   DCZZ
C               **  THE ABOVE RESULTS WERE ACTUALLY ARRIVED AT
C               **  (AND ARE VALID FOR) BY DISPLACING THE OLD ORIGIN
C               **  FROM (0,0,0) TO (XM,YM,ZM).
C               **  THIS SIMPLIFIES THE EQUATIONS CONSIDERABLY.
C               **
C               **  GIVEN THAT WE NOW HAVE THE DIRECTION COSINES
C               **  OF THE NEW AXES IN TERMS OF THE OLD COORDINATES,
C               **  WE MAKE USE OF
C               **  EISENHART (COORDINATE GEOMETRY, PAGE 160) WHICH STATES
C               **  THAT THE LINEAR TRANSFORMATION THAT IS NEEDED TO CARRY OUT
C               **  THE ROTATION FROM THE VISUAL PLANE TO THE XZ PLANE
C               **  IS GIVEN BY
C               **      XT = XM + DCXX(X-XM) + DCXY(Y-YM) + DCXZ(Z-ZM)
C               **      YT = YM + DCYX(X-XM) + DCYY(Y-YM) + DCYZ(Z-ZM)
C               **      ZT = ZM + DCZX(X-XM) + DCZY(Y-YM) + DCZZ(Z-ZM)
C               **
C               **  NOTE THAT BY INSPECTION OF THE ABOVE TRANSFORMATION
C               **  IT IS SEEN THAT (XM,YM,ZM) IS MAPPED INTO (XM,YM,ZM)
C               **  (AS IT SHOULD BE).
C               **  NOTE ALSO THAT THE EYE POINT AND ANY POINT ALONG THE LINE
C               **  OF SIGHT WOULD HAVE BEEN MAPPED INTO (XM,YM,ZM)
C               **  AS IT SHOULD BE.
C               **  NOTE ALSO THAT ALL POINTS ON THE VISUAL PLANE
C               **  SINCE THEY SATISFY
C               **     (XEYE-XM)(X-XM) + (YEYE-YM)(Y-YM) + (ZEYE-ZM)(Z-ZM) = 0
C               **  GETS MAPPED INTO THE CONSTANT YT VALUE OF YT = YM
C               **  AND SO THE TRANSFORMED PLOT SURFACE IS ONE WHICH
C               **  IS PARALLEL TO THE XZ PLANE BUT IS DISPLACED
C               **  YM UNITS OUT FROM THE XZ PLANE.
C               **  THIS PLOT PLANE WILL CONTAIN THE POINT (XM,YM,ZM).
C               ****************************************************************
C
      ISTEPN='21'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DNXX=YEYE
      DNXY=-XEYE
      DNXZ=0.0
      DNYX=XEYE
      DNYY=YEYE
      DNYZ=ZEYE
      DNZX=-XEYE*ZEYE
      DNZY=-YEYE*ZEYE
      DNZZ=XEYE*XEYE+YEYE*YEYE
C
      DENOMX=SQRT(DNXX**2+DNXY**2+DNXZ**2)
      DENOMY=SQRT(DNYX**2+DNYY**2+DNYZ**2)
      DENOMZ=SQRT(DNZX**2+DNZY**2+DNZZ**2)
C
C     ***** 15 LINES OF CODE TO CHECK FOR DIVISION BY 0 ADDED AUGUST 1983 *****
      DCXX=CPUMAX
      DCXY=CPUMAX
      DCXZ=CPUMAX
      IF(DENOMX.EQ.0.0)GOTO2121
      DCXX=DNXX/DENOMX
      DCXY=DNXY/DENOMX
      DCXZ=DNXZ/DENOMX
 2121 CONTINUE
C
      DCYX=CPUMAX
      DCYY=CPUMAX
      DCYZ=CPUMAX
      IF(DENOMY.EQ.0.0)GOTO2122
      DCYX=DNYX/DENOMY
      DCYY=DNYY/DENOMY
      DCYZ=DNYZ/DENOMY
 2122 CONTINUE
C
      DCZX=CPUMAX
      DCZY=CPUMAX
      DCZZ=CPUMAX
      IF(DENOMZ.EQ.0.0)GOTO2123
      DCZX=DNZX/DENOMZ
      DCZY=DNZY/DENOMZ
      DCZZ=DNZZ/DENOMZ
 2123 CONTINUE
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  BRANCH TO THE APPROPRIATE                   **
C               **  TRANSFORMATION                              **
C               **************************************************
C
      ISTEPN='22'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(I3DTRA.EQ.'ORTH')GOTO2300
      GOTO2400
C
C               **************************************************
C               **  STEP 23--                                   **
C               **  TREAT THE ORTHOGRAPHIC TRANSFORMATION CASE  **
C               **************************************************
C
 2300 CONTINUE
C
      ISTEPN='23'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     THE FOLLOWING IS INCORRECT (XM FOR X(I) ETC.)
      DO2310I=1,NP12
C
C     ***** 5 LINES OF CODE TO ADJUST FOR DIVISION BY 0 ADDED AUGUST 1983 *****
      EPS=0.0000001
      A11=XDEL
      A12=YDEL
      A13=ZDEL
      A23=YEYE-YM
      IF(A23.EQ.0.0)A21=EPS
      A23=-(XEYE-XM)
      IF(A23.EQ.0.0)A22=EPS
      A23=0.0
      A31=0.0
      A32=ZEYE-ZM
      IF(A32.EQ.0.0)A32=EPS
      A33=-(YEYE-YM)
      IF(A33.EQ.0.0)A33=EPS
C
      R1=XDEL*XM+YDEL*YM+ZDEL*ZM
      R2=(YEYE-YM)*XM-(XEYE-XM)*YM
      R3=(ZEYE-ZM)*YM-(YEYE-YM)*ZM
C
      P12=-A23/A11
      P13=-A32/(P12*A12+A23)
C
      ZPI=(P13*(P12*R1+R2)+R3)/
     1(P13*P12*A13+A33)
      YPI=(R3-A33*ZPI)/A32
      XPI=(R2-A23*YPI)/A21
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1WRITE(ICOUT,2311)I,XPI,YPI,ZPI
 2311 FORMAT('I,XPI,YPI,ZPI = ',I8,3E15.7)
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL DPWRST('XXX','BUG ')
C
      DELX=XPI-XM
      DELY=YPI-YM
      DELZ=ZPI-ZM
      XT(I)=XM+DCXX*DELX+DCXY*DELY+DCXZ*DELZ
CCCCC YT(I)=YM+DCYX*DELX+DCYY*DELY+DCYZ*DELZ
      ZT(I)=XM+DCZX*DELX+DCZY*DELY+DCZZ*DELZ
C
 2310 CONTINUE
C
C               **************************************************
C               **  STEP 22--                                   **
C               **  TREAT THE PERSPECTIVE TRANSFORMATION CASE   **
C               **************************************************
C
 2400 CONTINUE
C
      ISTEPN='24'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      DO2410I=1,NP12
C
      ISTEPN='1'
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
C     ***** 5 LINES OF CODE TO ADJUST FOR DIVISION BY 0 ADDED AUGUST 1983 *****
      EPS=0.0000001
      A11=XDEL
      A12=YDEL
      A13=ZDEL
      A21=YEYE-Y(I)
      IF(A21.EQ.0.0)A21=EPS
      A22=-(XEYE-X(I))
      IF(A22.EQ.0.0)A22=EPS
      A23=0.0
      A31=0.0
      A32=ZEYE-Z(I)
      IF(A32.EQ.0.0)A32=EPS
      A33=-(YEYE-Y(I))
      IF(A33.EQ.0.0)A33=EPS
C
      R1=XDEL*XM+YDEL*YM+ZDEL*ZM
      R2=(YEYE-Y(I))*X(I)-(XEYE-X(I))*Y(I)
      R3=(ZEYE-Z(I))*Y(I)-(YEYE-Y(I))*Z(I)
C
      P12=-A21/A11
      P13=-A32/(P12*A12+A22)
C
      ZPI=(P13*(P12*R1+R2)+R3)/
     1(P13*P12*A13+A33)
      YPI=(R3-A33*ZPI)/A32
      XPI=(R2-A22*YPI)/A21
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1WRITE(ICOUT,2411)I,XPI,YPI,ZPI
 2411 FORMAT('I,XPI,YPI,ZPI = ',I8,3E15.7)
      IF(IBUGPL.EQ.'ON'.OR.ISUBRO.EQ.'3DTR')
     1CALL DPWRST('XXX','BUG ')
C
C
      DELX=XPI-XM
      DELY=YPI-YM
      DELZ=ZPI-ZM
      XT(I)=XM+DCXX*DELX+DCXY*DELY+DCXZ*DELZ
CCCCC YT(I)=YM+DCYX*DELX+DCYY*DELY+DCYZ*DELZ
      ZT(I)=XM+DCZX*DELX+DCZY*DELY+DCZZ*DELZ
C
 2410 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGPL.EQ.'OFF'.AND.ISUBRO.NE.'3DTR')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END OF DP3DTR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGPL,ISUBRO,IERROR
 9012 FORMAT('IBUGPL.ISUBRO,IERROR = ',3A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9022)XEYE0,YEYE0,ZEYE0
 9022 FORMAT('XEYE0, YEYE0, ZEYE0 = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9023)XEYE,YEYE,ZEYE
 9023 FORMAT('XEYE, YEYE, ZEYE = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9024)XORIG,YORIG,ZORIG
 9024 FORMAT('XORIG, YORIG, ZORIG = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9025)XORIG2,YORIG2,ZORIG2
 9025 FORMAT('XORIG2, YORIG2, ZORIG2 = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9026)XRANGE,YRANGE,ZRANGE
 9026 FORMAT('XRANGE, YRANGE, ZRANGE = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9027)XM,YM,ZM
 9027 FORMAT('XM, YM, ZM = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9028)N,NP12
 9028 FORMAT('N,NP12 = ',2I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9029)
 9029 FORMAT('I,X(I),Y(I),Z(I),XT(I),ZT(I)')
      CALL DPWRST('XXX','BUG ')
      DO9031I=1,NP12
      WRITE(ICOUT,9032)I,X(I),Y(I),Z(I),
     1XT(I),ZT(I)
 9032 FORMAT(I4,7E11.4)
      CALL DPWRST('XXX','BUG ')
 9031 CONTINUE
      WRITE(ICOUT,9041)
 9041 FORMAT('DNXX,DNXY,DNXZ,DNYX,DNYY,DNYZ,DNZX,DNZY,DNZZ = ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9042)DNXX,DNXY,DNXZ,DNYX,DNYY,DNYZ,DNZX,DNZY,DNZZ
 9042 FORMAT(9E13.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9043)DENOMX,DENOMY,DENOMZ
 9043 FORMAT('DENOMX,DENOMY,DENOMZ = ',3E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9045)
 9045 FORMAT('DCXX,DCXY,DCXZ,DCYX,DCYY,DCYZ,DCZX,DCZY,DCZZ = ')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9046)DCXX,DCXY,DCXZ,DCYX,DCYY,DCYZ,DCZX,DCZY,DCZZ
 9046 FORMAT(9E13.5)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9051)XMIN,XMAX
 9051 FORMAT('XMIN,XMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9052)YMIN,YMAX
 9052 FORMAT('YMIN,YMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9053)ZMIN,ZMAX
 9053 FORMAT('ZMIN,ZMAX = ',2E15.7)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9054)I3DTRA
 9054 FORMAT('I3DTRA = ',A4)
      CALL DPWRST('XXX','BUG ')
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DP3MSM(X,N,XS,ICHANG,IBUGG3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE TAKES THE DATA IN THE VECTOR X,
C              APPLIES A 3-TERM MEDIAN SMOOTH, AND PUTS THE
C              RESULTS IN A VECTOR XS.
C     NOTE--THE VECTOR X REMAINS UNCHANGED.
C     OUTPUT ARGUMENTS--XS     = THE SINGLE PRECISION VECTOR
C                                CONTAINING SMOOTHED VALUES.
C                     --ICHANG = THE CHARACTER VARIABLE
C                                CONTAINING EITHER YES OR NO
C                                DEPENDING ON WHETHER OR NOT THE
C                                SMOOTHED DATA IS CHANGED OR NOT
C                                FROM THE ORIGINAL DATA.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR
C             OF SMOOTHED VALUES.
C     ASSUMPTION--THE VECTOR X HAS AT LEAST 3 VALUES.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--MCNEIL, INTERACTIVE DATA ANALYSIS
C                 1977, PAGE 144
C                 (= SOURCE OF ALGORITHM).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION--JULY      1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHANG
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XS(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGG3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP3MSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG3
   52 FORMAT('IBUGG3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               **************************
C               **  SMOOTH THE DATA     **
C               **  VIA 3-TERM MEDIANS  **
C               **************************
C
      ARG1=X(1)
      ARG2=X(2)
      ARG3=3*X(2)+2*X(3)
      CALL DPMED3(ARG1,ARG2,ARG3,XMED3,IBUGG3,IERROR)
      XS(1)=XMED3
C
      NM1=N-1
      DO1100I=2,NM1
      IM1=I-1
      IP1=I+1
      ARG1=X(IM1)
      ARG2=X(I)
      ARG3=X(IP1)
      CALL DPMED3(ARG1,ARG2,ARG3,XMED3,IBUGG3,IERROR)
      XS(I)=XMED3
 1100 CONTINUE
C
      ARG1=X(N)
      ARG2=X(NM1)
      NM2=N-2
      ARG3=3*X(NM1)+2*X(NM2)
      CALL DPMED3(ARG1,ARG2,ARG3,XMED3,IBUGG3,IERROR)
      XS(N)=XMED3
C
      ICHANG='NO'
      DO1200I=1,N
      IF(XS(I).NE.X(I))GOTO1210
 1200 CONTINUE
      GOTO1290
 1210 CONTINUE
      ICHANG='YES'
 1290 CONTINUE
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DP3MSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG3
 9012 FORMAT('IBUGG3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHANG
 9013 FORMAT('ICHANG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)N
 9014 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N
      WRITE(ICOUT,9016)I,X(I)
 9016 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DP3RSM(X,XJUNK,N,XS,ICHANG,IBUGG3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE TAKES THE DATA IN THE VECTOR X,
C              AND REPEATEDLY APPLIES A 3-TERM MEDIAN SMOOTH
C              UNTIL NO CHANGE OCCURS AFTER THE SMOOTHING OPERATION.
C     OUTPUT ARGUMENTS--XS     = THE SINGLE PRECISION VECTOR
C                                CONTAINING SMOOTHED VALUES.
C                     --ICHANG = THE CHARACTER VARIABLE
C                                CONTAINING EITHER YES OR NO
C                                DEPENDING ON WHETHER OR NOT THE
C                                SMOOTHED DATA IS CHANGED OR NOT
C                                FROM THE ORIGINAL DATA.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR
C             OF SMOOTHED VALUES.
C     NOTE--THE VECTOR X REMAINS UNCHANGED.
C     ASSUMPTION--THE VECTOR X HAS AT LEAST 3 VALUES.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--MCNEIL, INTERACTIVE DATA ANALYSIS
C                 1977, PAGE 145
C                 (= SOURCE OF ALGORITHM).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION--JULY      1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHANG
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XJUNK(*)
      DIMENSION XS(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGG3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP3RSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG3
   52 FORMAT('IBUGG3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               **********************************
C               **  SMOOTH THE DATA             **
C               **  VIA 3-TERM MEDIANS          **
C               **  REPEATED UNTIL CONVERGENCE  **
C               **********************************
C
      IF(N.GE.1)GOTO190
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN DP3RSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('     NUMBER OF OBSERVATIONS IS NON-POSITIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)N
  113 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  190 CONTINUE
C
      IF(N.GE.3)GOTO290
      DO210I=1,N
      XS(I)=X(I)
  210 CONTINUE
      ICHANG='NO'
      GOTO9000
  290 CONTINUE
C
C               ************************************************
C               **  STEP 1--                                  **
C               **  SMOOTH THE RAW DATA WITH 3-TERM MEDIANS.  **
C               **  IF NO CHANGE, THEN EXIT.                  **
C               ************************************************
C
      CALL DP3MSM(X,N,XS,ICHANG,IBUGG3,IERROR)
      IF(ICHANG.EQ.'NO')GOTO9000
C
C               *********************************************
C               **  STEP 2--                               **
C               **  SINCE THERE WAS A CHANGE, THEN         **
C               **  REPEATEDLY SMOOTH THE SMOOTHED VALUES  **
C               **  UNTIL NO CHANGE.                       **
C               *********************************************
C
      NUMIT=0
      MAXIT=100
 1100 CONTINUE
      NUMIT=NUMIT+1
      CALL DP3MSM(XS,N,XJUNK,ICHANG,IBUGG3,IERROR)
      DO1200I=1,N
      XS(I)=XJUNK(I)
 1200 CONTINUE
      IF(NUMIT.GT.MAXIT)GOTO1250
      IF(ICHANG.EQ.'YES')GOTO1100
      GOTO1290
 1250 CONTINUE
C     WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1251)
 1251 FORMAT('***** ERROR IN DP3RSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1252)
 1252 FORMAT('     NUMBER OF ITERATIONS HAS JUST')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1253)MAXIT
 1253 FORMAT('     EXCEEDED ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1290 CONTINUE
C
C               ********************************************************
C               **  STEP 3--                                          **
C               **  MAKE A FINAL CHECK TO SEE IF THE SMOOTHED VALUES  **
C               **  HAVE CHANGED ANY FROM THE RAW DATA.               **
C               ********************************************************
C
      ICHANG='NO'
      DO1300I=1,N
      IF(XS(I).NE.X(I))GOTO1310
 1300 CONTINUE
      GOTO1390
 1310 CONTINUE
      ICHANG='YES'
 1390 CONTINUE
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DP3RSM--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG3
 9012 FORMAT('IBUGG3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHANG
 9013 FORMAT('ICHANG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)N
 9014 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N
      WRITE(ICOUT,9016)I,X(I),XJUNK(I),XS(I)
 9016 FORMAT('I,X(I),XJUNK(I),XS(I) = ',I8,3E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DP3RSR(X,XJUNK,N,PRED2,RES2,IBUGG3,IERROR)
C
C     PURPOSE--THIS SUBROUTINE TAKES THE DATA IN THE VECTOR X,
C              AND APPLIES A 3RSR SMOOTHING, THAT IS,
C              MEDIANS OF 3 (REPEATED UNTIL NO CHANGE) FOLLOWED BY
C              SPLITTING (REPEATED UNTIL NO CHANGE).
C     OUTPUT ARGUMENTS--PRED2  = THE SINGLE PRECISION VECTOR
C                                CONTAINING SMOOTHED VALUES.
C                       RES2   = THE SINGLE PRECISION VECTOR
C                                CONTAINING RESIDUALS FROM THE SMOOTH.
C     OUTPUT--THE COMPUTED SINGLE PRECISION VECTOR
C             OF SMOOTHED VALUES.
C     NOTE--THE VECTOR X REMAINS UNCHANGED.
C     LANGUAGE--ANSI FORTRAN (1977)
C     REFERENCES--MCNEIL, INTERACTIVE DATA ANALYSIS
C                 1977, PAGE 146 AND 124
C                 (= SOURCE OF ALGORITHM).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION--JULY      1983.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICHANG
C
      CHARACTER*4 IBUGG3
      CHARACTER*4 IERROR
C
C---------------------------------------------------------------------
C
      DIMENSION X(*)
      DIMENSION XJUNK(*)
      DIMENSION PRED2(*)
      DIMENSION RES2(*)
C
C---------------------------------------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----START POINT-----------------------------------------------------
C
      IERROR='NO'
C
      IF(IBUGG3.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP3RSR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)IBUGG3
   52 FORMAT('IBUGG3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)N
   53 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO55I=1,N
      WRITE(ICOUT,56)I,X(I)
   56 FORMAT('I,X(I) = ',I8,E15.7)
      CALL DPWRST('XXX','BUG ')
   55 CONTINUE
   90 CONTINUE
C
C               **********************************
C               **  CARRY OUT A 3RSR SMOOTHING  **
C               **********************************
C
      IF(N.GE.1)GOTO190
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,111)
  111 FORMAT('***** ERROR IN DP3RSR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,112)
  112 FORMAT('     NUMBER OF OBSERVATIONS IS NON-POSITIVE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,113)N
  113 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
  190 CONTINUE
C
      IF(N.GE.3)GOTO290
      DO210I=1,N
      PRED2(I)=X(I)
      RES2(I)=0.0
  210 CONTINUE
      ICHANG='NO'
      GOTO9000
  290 CONTINUE
C
C               ********************************************
C               **  STEP 1--                              **
C               **  CARRY OUT A 3R SMOOTHING, THAT IS,    **
C               **  APPLY 3-TERM MEDIANS AND REPEAT THIS  **
C               **  UNTIL NO CHANGE                       **
C               ********************************************
C
      CALL DP3RSM(X,RES2,N,PRED2,ICHANG,IBUGG3,IERROR)
C
C               *************************************************************
C               **  STEP 2--                                               **
C               **  SET UP A LOOP.                                         **
C               **  INSIDE THE LOOP, CARRY OUT                             **
C               **     1) A SPLIT & SMOOTH                                 **
C               **     2) A 3-TERM MEDIAN SMOOTH REPEATED UNTIL NO CHANGE  **
C               **  REPEAT THE LOOP UNTIL NO CHANGE                        **
C               *************************************************************
C
 1000 CONTINUE
C
C               *******************************
C               **  STEP 2.1--               **
C               **  SPLIT & SMOOTH THE DATA  **
C               *******************************
C
      CALL DPSPSM(PRED2,N,RES2,ICHANG,IBUGG3,IERROR)
C
C               ****************************************************************
C               **  STEP 2.2--                                                 *
C               **  CARRY OUT A 3R SMOOTHING ON THE SMOOTHED VALUES, THAT IS,  *
C               **  APPLY 3-TERM MEDIANS AND REPEAT UNTIL NO CHANGE            *
C               ****************************************************************
C
      CALL DP3RSM(RES2,XJUNK,N,PRED2,ICHANG,IBUGG3,IERROR)
C
C               *******************************************************
C               **  STEP 2.3--                                       **
C               **  COPY RESIDUALS INTO RES2(.).                     **
C               **  IF CHANGES HAD OCCURRED IN LAST 3R SMOOTH,       **
C               **  THEN REPEAT ENTIRE SPLIT/SMOOTH AND 3R PROCESS.  **
C               *******************************************************
C
      DO1100I=1,N
      RES2(I)=X(I)-PRED2(I)
 1100 CONTINUE
C
      IF(ICHANG.EQ.'YES')GOTO1000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG3.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DP3RSR--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGG3
 9012 FORMAT('IBUGG3 = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHANG
 9013 FORMAT('ICHANG = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)N
 9014 FORMAT('N = ',I8)
      CALL DPWRST('XXX','BUG ')
      DO9015I=1,N
      WRITE(ICOUT,9016)I,X(I),XJUNK(I),PRED2(I),RES2(I)
 9016 FORMAT('I,X(I),XJUNK(I),PRED2(I),RES2(I) = ',I8,4E15.7)
      CALL DPWRST('XXX','BUG ')
 9015 CONTINUE
 9090 CONTINUE
C
      RETURN
      END
      SUBROUTINE DP4PLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1                  IANGLU,MAXNPP,
     1                  CLLIMI,CLWIDT,
     1                  ICONT,NUMHPP,IMANUF,
     1                  XMATN,YMATN,XMITN,YMITN,
     1                  ISQUAR,
     1                  IVGMSW,IHGMSW,
     1                  IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1                  PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1                  IX3AUT,ITIAUT,
CCCCC                   MARCH 1996.  ADD FOLLOWING LINE
     1                  IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,
     1                  IHSTMC,IHSTOP,
CCCCC                   MARCH 2002.  ADD FOLLOWING LINE
     1                  I4PLMC,I4PLDI,
     1                  ICAPSW,
     1                  IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,
     1                  IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1                  IFOUND,IERROR)
C
C     PURPOSE--GENERATE THE FOLLOWING 4 PLOTS
C              (ALL ON THE SAME PAGE)--
C                 1) A RUN SEQUENCE PLOT;
C                 2) A LAG PLOT;
C                 3) A HISTOGRAM;
C                 4) A NORMAL PROBABILITY PLOT;
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--89/2
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--JANUARY   1989.
C     UPDATED         --APRIL     1989. BY ALAN
C     UPDATED         --NOVEMBER  1989. CHAR*4 FOR IBUGU.. (NELSON)
C     UPDATED         --NOVEMBER  1989. DIM. FOR CLLIMI & CLWIDT
C     UPDATED         --NOVEMBER  1989. ALLOW 4PLOT SYNONYM
C     UPDATED         --AUGUST    1992. ARGUMENT LIST TO DPGRAP
C     UPDATED         --DECEMBER  1993. ARGUMENT LIST TO DPPP
C     UPDATED         --DECEMBER  1993. BUG WITH X3LAB
C     UPDATED         --MARCH     1996. IRHSTG
C     UPDATED         --AUGUST    1999. ARGUMENT LIST TO DPGRAP
C     UPDATED         --MARCH     2002. SIZE BASED ON MULTIPLOT
C                                       CORNER COORDINATES
C     UPDATED         --OCTOBER   2006. I4PLDI (GENERATE EXPO
C                                       PROB PLOT INSTEAD OF
C                                       NORMAL PROB PLOT)
C     UPDATED         --JANUARY   2010. CALL LIST TO DPHIST
C     UPDATED         --JUNE      2011. SUPPORT FOR "HIGHLIGHT" OPTION
C     UPDATED         --FEBRUARY  2012. FOR LAG PLOT, ADD CHECK FOR
C                                       EMPTY CHARACTER SETTING
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICASPL
      CHARACTER*4 I4PLDI
      CHARACTER*4 ICONT
      CHARACTER*4 IPOWE
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
C
CCCCC THE FOLLOWING 4 LINES WERE INSERTED NOVEMBER 1989
CCCCC (BUG UNCOVERED BY NELSON HSU)
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
C
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISQUAR
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IREPCH
      CHARACTER*4 IMPSW
C
      CHARACTER*4 I4PLOT
C
      CHARACTER*4 IERAS2
      CHARACTER*4 ICOPS2
      CHARACTER*24 ICHAP2
      CHARACTER*24 ICHAP3
      CHARACTER*4 ILINP2
C
      CHARACTER*4 IFEED9
C
      CHARACTER*4 IANSRS
      CHARACTER*4 IANSLP
      CHARACTER*4 IANSHI
      CHARACTER*4 IANSNP
      CHARACTER*4 IANSEP
C
      CHARACTER*4 IMANUF
C
      CHARACTER*4 IX3AUT
      CHARACTER*4 ITIAUT
C
      CHARACTER*4 IRHSTG
      CHARACTER*4 IHSTCW
      CHARACTER*4 IASHWT
      CHARACTER*4 IHSTEB
      CHARACTER*4 IHSTOU
      CHARACTER*4 IHSTOP
      CHARACTER*4 I4PLMC
      CHARACTER*4 IHIGH
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC THE FOLLOWING 2 LINES WERE INSERTED NOVEMBER 1989
CCCCC (BUG UNCOVERED BY NELSON HSU)
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
C
      DIMENSION IANSRS(20)
      DIMENSION IANSLP(10)
      DIMENSION IANSHI(10)
      DIMENSION IANSNP(24)
      DIMENSION IANSEP(29)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA (IANSRS(I),I=1,18)
     1/'R   ','U   ','N   ','    ',
     1 'S   ','E   ','Q   ','U   ','E   ','N   ','C   ','E   ',
     1 '    ',
     1 'P   ','L   ','O   ','T   ','    '/
      DATA (IANSLP(I),I=1,9)
     1/'L   ','A   ','G   ','    ',
     1 'P   ','L   ','O   ','T   ','    '/
      DATA (IANSHI(I),I=1,10)
     1/'H   ','I   ','S   ','T   ','O   ',
     1 'G   ','R   ','A   ','M   ','    '/
      DATA (IANSNP(I),I=1,24)
     1/'N   ','O   ','R   ','M   ','A   ','L   ','    ',
CCCCC THE FOLLOWING LINE WAS FIXED     DECEMBER 1993
CCCCC1 'P   ','R   ','O   ','B   ','A   ','B   ','A   ','B   ',
     1 'P   ','R   ','O   ','B   ','A   ','B   ',
     1 'I   ','L   ','I   ','T   ','Y   ','    ',
     1 'P   ','L   ','O   ','T   ','    '/
      DATA (IANSEP(I),I=1,29)
     1/'E   ','X   ','P   ','O   ','N   ','E   ','N   ',
     1 'T   ','I   ','A   ','L   ','    ',
     1 'P   ','R   ','O   ','B   ','A   ','B   ',
     1 'I   ','L   ','I   ','T   ','Y   ','    ',
     1 'P   ','L   ','O   ','T   ','    '/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      ISUBN1='DP4P'
      ISUBN2='LO  '
C
      I4PLOT='ON'
      IHIGH='OFF'
      NDONE=0
CCCCC IVLOC=7
CCCCC IF(IANS(7).EQ.'    ')IVLOC=8
      DO21I=1,50
        IF(IANS(I).EQ.'P' .AND. IANS(I+1).EQ.'L' .AND.
     1     IANS(I+2).EQ.'O' .AND. IANS(I+3).EQ.'T')THEN
          IVLOC=I+5
          GOTO29
        ENDIF
 21   CONTINUE
 29   CONTINUE
      NCRS=18
      NCLP=9
      NCHI=10
CCCCC THE FOLLOWING CORRECTION WAS MADE JANUARY 1989
      NCNP=24
      NCEP=29
CCCCC NCNP=25
C
C               ******************************************
C               **  TREAT THE 4-PLOT ... ANALYSIS CASE  **
C               ******************************************
C
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'4PLO')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DP4PLO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52   FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ,ISUBRO,NUMARG
   53   FORMAT('IBUGG2,IBUGG3,IBUGQ,ISUBRO,NUMARG = ',4(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)PMXMIN,PMXMAX,PMYMIN,PMYMAX
   54   FORMAT('PMXMIN,PMXMAX,PMYMIN,PMYMAX = ',4G15.7)
        CALL DPWRST('XXX','BUG ')
        IF(NUMARG.GE.1)THEN
          DO61I=1,NUMARG
            WRITE(ICOUT,62)I,IHARG(I),IARGT(I)
   62       FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
            CALL DPWRST('XXX','BUG ')
   61     CONTINUE
        ENDIF
      ENDIF
C
CCCCC THE FOLLOWING SECTION WAS INSERTED NOVEMBER 1989
C               **************************************************
C               **  STEP 10--                                   **
C               **  EXTRACT THE COMMAND                         **
C               **************************************************
C
      IF(ICOM.EQ.'HIGH' .OR. ICOM.EQ.'SUBS')THEN
        IHIGH='ON'
        ICOM=IHARG(1)
        ICOM2=IHARG2(1)
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
      ENDIF
C
      IF(ICOM.EQ.'4PLO')THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='4   '
        ICOM2='    '
        IHARG(1)='PLOT'
        IHARG2(1)='    '
        IARG(1)=(-1)
        ARG(1)=(-1.0)
        IARGT(1)='WORD'
      ENDIF
C
C               **************************************************
C               **   STEP 20--                                  **
C               **   SAVE INITIAL SETTINGS                      **
C               **************************************************
C
      ISTEPN='20'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      PWXMN2=PWXMIN
      PWXMX2=PWXMAX
      PWYMN2=PWYMIN
      PWYMX2=PWYMAX
      PMXMN2=PMXMIN
      PMXMX2=PMXMAX
      PMYMN2=PMYMIN
      PMYMX2=PMYMAX
      IF(I4PLMC.EQ.'OFF')THEN
        PMXMIN=15.0
        PMXMAX=85.0
        PMYMIN=20.0
        PMYMAX=90.0
      ENDIF
      IERAS2=IERASW
      ICOPS2=ICOPSW
      ICHAP2=ICHAPA(1)
      ICHAP3=ICHAPA(2)
      ILINP2=ILINPA(1)
      IFEED9=IFEEDB
C
C               **************************************************
C               **   STEP 21--                                  **
C               **   GENERATE THE RUN SEQUENCE PLOT             **
C               **************************************************
C
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC MARCH 2002.  USE MULTIPLOT CORNER COORDINATES
CCCCC PWXMIN=0.0
CCCCC PWXMAX=50.0
CCCCC PWYMIN=50.0
CCCCC PWYMAX=100.0
      PWXMIN=PMXMIN
      PWXMAX=PMXMIN + (PMXMAX-PMXMIN)/2.0
      PWYMIN=PMYMAX - (PMYMAX-PMYMIN)/2.0
      PWYMAX=PMYMAX
      ICOPSW='OFF'
      IF(IHIGH.EQ.'OFF')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='PLOT'
        CALL DPPLOT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IANGLU,MAXNPP,
     1              IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,
     1              IFOUND,IERROR)
CCCCC   THE FOLLOWING CORRECTION WAS MADE JANUARY 1989
CCCCC   IF(IERROR.EQ.'YES')GOTO9000       JANUARY 1989
        IF(IERROR.EQ.'YES')GOTO2800
      ELSE
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='HIGH'
        IHARG(1)='RUN '
        IHARG2(1)='    '
        IHARG(2)='SEQU'
        IHARG2(2)='ENCE'
        IHARG(3)='PLOT'
        IHARG2(3)='    '
        IARG(2)=(-1)
        ARG(2)=(-1.0)
        IARGT(2)='WORD'
        IARG(3)=(-1)
        ARG(3)=(-1.0)
        IARGT(3)='WORD'
        CALL DPRUNS(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO2800
      ENDIF
C
      J=0
      DO2111I=1,NCRS
        J=J+1
        IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSRS(I)
        IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSRS(I)
 2111 CONTINUE
      GOTO2500
C
C               **************************************************
C               **   STEP 22--                                  **
C               **   GENERATE THE LAG PLOT                      **
C               **************************************************
C
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC MARCH 2002.  USE MULTIPLOT CORNER COORDINATES
CCCCC PWXMIN=50.0
CCCCC PWXMAX=100.0
CCCCC PWYMIN=50.0
CCCCC PWYMAX=100.0
      PWXMIN=PMXMIN + (PMXMAX-PMXMIN)/2.0
      PWXMAX=PMXMAX
      PWYMIN=PMYMAX - (PMYMAX-PMYMIN)/2.0
      PWYMAX=PMYMAX
      IERASW='OFF'
      ICOPSW='OFF'
      IF(ICHAP2.EQ.'BL' .OR. ICHAP2.EQ.'BLANK' .OR.
     1   ICHAP2.EQ.' ')THEN
        ICHAPA(1)='X   '
      ELSE
        ICHAPA(1)=ICHAP2
      ENDIF
      ILINPA(1)='    '
      IFEEDB='OFF'
      IF(IHIGH.EQ.'OFF')THEN
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='LAG '
        IHARG(1)='PLOT'
        IHARG2(1)='    '
        CALL DPLAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC   THE FOLLOWING CORRECTION WAS MADE JANUARY 1989
CCCCC   IF(IERROR.EQ.'YES')GOTO9000       JANUARY 1989
        IF(IERROR.EQ.'YES')GOTO2800
      ELSE
        IF(ICHAP3.EQ.'BL' .OR. ICHAP3.EQ.'BLAN' .OR.
     1     ICHAP3.EQ.' ')THEN
          ICHAPA(2)='X   '
        ENDIF
        ILINPA(2)='    '
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='HIGH'
        IHARG(1)='LAG '
        IHARG2(1)='    '
        IHARG(2)='PLOT'
        IHARG2(2)='    '
        CALL DPLAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1             IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO2800
      ENDIF
C
      J=0
      DO2211I=1,NCLP
        J=J+1
        IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSLP(I)
        IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSLP(I)
 2211 CONTINUE
      GOTO2500
C
C               **************************************************
C               **   STEP 23--                                  **
C               **   GENERATE THE HISTOGRAM                     **
C               **************************************************
C
 2300 CONTINUE
      ISTEPN='23'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC MARCH 2002.  USE MULTIPLOT CORNER COORDINATES
CCCCC PWXMIN=0.0
CCCCC PWXMAX=50.0
CCCCC PWYMIN=0.0
CCCCC PWYMAX=50.0
      PWXMIN=PMXMIN
      PWXMAX=PMXMIN + (PMXMAX-PMXMIN)/2.0
      PWYMIN=PMYMIN
      PWYMAX=PMYMIN + (PMYMAX-PMYMIN)/2.0
      IERASW='OFF'
      ICOPSW='OFF'
      ICHAPA(1)='    '
      ILINPA(1)='SOLI'
      IFEEDB='OFF'
      IF(IHIGH.EQ.'OFF')THEN
        ICOM='HIST'
        CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              CLLIMI,CLWIDT,
CCCCC               MARCH 1996.  ADD FOLLOWING LINE.
CCCCC1              IRHSTG,IHSTCW,IHSTEB,IHSTOU,
CCCCC               JANUARY 2010.  ADD IHSTEB, IHSTOU TO FOLLOWING LINE
     1              IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,
     1              IHSTMC,IHSTOP,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC   THE FOLLOWING CORRECTION WAS MADE JANUARY 1989
CCCCC   IF(IERROR.EQ.'YES')GOTO9000       JANUARY 1989
        IF(IERROR.EQ.'YES')GOTO2800
      ELSE
        ICHAPA(2)='    '
        ILINPA(2)='SOLI'
        ISHIFT=1
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='HIGH'
        IHARG(1)='HIST'
        IHARG2(1)='    '
        CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1              CLLIMI,CLWIDT,
     1              IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,
     1              IHSTMC,IHSTOP,
     1              IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO2800
      ENDIF
C
      J=0
      DO2311I=1,NCHI
        J=J+1
        IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSHI(I)
        IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSHI(I)
 2311 CONTINUE
      GOTO2500
C
C               **************************************************
C               **   STEP 24--                                  **
C               **   GENERATE THE NORMAL PROBABILITY PLOT       **
C               **************************************************
C
 2400 CONTINUE
      ISTEPN='24'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
CCCCC MARCH 2002.  USE MULTIPLOT CORNER COORDINATES
CCCCC PWXMIN=50.0
CCCCC PWXMAX=100.0
CCCCC PWYMIN=0.0
CCCCC PWYMAX=50.0
C
CCCCC OCTOBER 2006.  OPTIONALLY GENERATE EXPONENTIAL
CCCCC                PROBABILITY PLOT (E.G., WHEN CHECKING
CCCCC                FOR HOMOGENEOUS POISSON PROCESS).
C
      PWXMIN=PMXMIN + (PMXMAX-PMXMIN)/2.0
      PWXMAX=PMXMAX
      PWYMIN=PMYMIN
      PWYMAX=PMYMIN + (PMYMAX-PMYMIN)/2.0
      IERASW='OFF'
      ICOPSW=ICOPS2
      ICHAPA(1)=ICHAP2
      ILINPA(1)=ILINP2
      IFEEDB='OFF'
      IF(IHIGH.EQ.'OFF')THEN
        ISHIFT=2
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='NORM'
        IF(I4PLDI.EQ.'EXPO')ICOM='EXPO'
        IHARG(1)='PROB'
        IHARG(2)='PLOT'
        CALL DPPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
CCCCC             THE FOLLOWING LINE WAS CHANGED     DECEMBER 1993
CCCCC1            IBUGG2,IBUGG3,IBUGQ,IFOUND,IERROR)
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
CCCCC   THE FOLLOWING CORRECTION WAS MADE JANUARY 1989
CCCCC   IF(IERROR.EQ.'YES')GOTO9000       JANUARY 1989
        IF(IERROR.EQ.'YES')GOTO2800
      ELSE
        ICHAPA(2)=ICHAP2
        ILINPA(2)=ILINP2
        ISHIFT=3
        CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='NORM'
        IF(I4PLDI.EQ.'EXPO')ICOM='EXPO'
        IHARG(1)='HIGH'
        IHARG2(1)='    '
        IHARG(2)='PROB'
        IHARG2(2)='    '
        IHARG(3)='PLOT'
        IHARG2(3)='    '
        CALL DPPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1            IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
        IF(IERROR.EQ.'YES')GOTO2800
      ENDIF
C
      J=0
      IF(I4PLDI.EQ.'EXPO')THEN
        DO2413I=1,NCEP
          J=J+1
          IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSEP(I)
          IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSEP(I)
 2413   CONTINUE
      ELSE
        DO2411I=1,NCNP
          J=J+1
          IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSNP(I)
          IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSNP(I)
 2411   CONTINUE
      ENDIF
      GOTO2500
C
C               **************************************************
C               **   STEP 25--                                  **
C               **   PLOT THE CURRENT PLOT (OUT OF THE 4)       **
C               **************************************************
 2500 CONTINUE
      IF(IVLOC.LE.IWIDTH)THEN
        DO2502I=IVLOC,IWIDTH
          J=J+1
          IF(IX3AUT.EQ.'ON')IX3LTE(J)=IANSLC(I)
          IF(ITIAUT.EQ.'ON')ITITTE(J)=IANSLC(I)
 2502   CONTINUE
      ENDIF
      IF(IX3AUT.EQ.'ON')NCX3LA=J
      IF(ITIAUT.EQ.'ON')NCTITL=J
C
      ICONT=IDCONT(1)
      IPOWE=IDPOWE(1)
      NUMHPP=IDNHPP(1)
C
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')THEN
        WRITE(ICOUT,2507)IMANUF,NUMDEV,IDMANU(1)
 2507   FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC ADD FOLLOWING TO DPGRAP ARGUMENT LIST
      IMPARG=2
      CALL DPGRAP(Y,X,X3D,D,N,NPLOTP,ICASPL,ICONT,IPOWE,NUMHPP,
     1XMATN,YMATN,XMITN,YMITN,
     1ISQUAR,
     1IVGMSW,IHGMSW,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1YPLOT,XPLOT,X2PLOT,TAGPLO,
     1IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1IMPARG,
     1PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1MAXCOL,
CCCCC AUGUST 1992.  ADD FOLLOWING LINE
     1DSIZE,DSYMB,DCOLOR,DFILL,
     1ICAPSW,
     1IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1IERROR)
CCCCC IF(IFOUND.EQ.'YES'.AND.IAND2.EQ.'NO')IAUTEX='OFF'
      IF(IERROR.EQ.'NO')IAND1=IAND2
      IF(IERROR.EQ.'YES')GOTO9000
      NDONE=NDONE+1
      IF(NDONE.LE.1)GOTO2200
      IF(NDONE.EQ.2)GOTO2300
      IF(NDONE.EQ.3)GOTO2400
      IF(NDONE.GE.4)GOTO2800
      GOTO9000
C
C               **************************************************
C               **   STEP 28--                                  **
C               **   REINSTATE INITIAL SETTINGS                 **
C               **************************************************
C
 2800 CONTINUE
C
      ISTEPN='28'
      IF(IBUGG3.EQ.'ON'.OR.ISUBRO.EQ.'4PLO')THEN
        CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
        WRITE(ICOUT,2807)IMANUF,NUMDEV,IDMANU(1)
 2807   FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      PWXMIN=PWXMN2
      PWXMAX=PWXMX2
      PWYMIN=PWYMN2
      PWYMAX=PWYMX2
      PMXMIN=PMXMN2
      PMXMAX=PMXMX2
      PMYMIN=PMYMN2
      PMYMAX=PMYMX2
      IERASW=IERAS2
      ICOPSW=ICOPS2
      ICHAPA(1)=ICHAP2
      ICHAPA(2)=ICHAP3
      ILINPA(1)=ILINP2
      IFEEDB=IFEED9
      IF(IHIGH.EQ.'OFF')THEN
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='4   '
        IHARG(1)='PLOT'
      ELSE
        ISHIFT=1
        CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1              IBUGG3,IERROR)
        ICOM='HIGH'
        IHARG(1)='4   '
        IHARG(2)='PLOT'
      ENDIF
CCCCC THE FOLLOWING 1-LINE INSERTION WAS MADE JANUARY 1989
      IF(IERROR.EQ.'YES')GOTO9000
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IFOUND='YES'
      IF(IBUGG2.EQ.'ON' .OR. ISUBRO.EQ.'4PLO')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DP4PLO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFOUND,IERROR
 9012   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DP6PLO(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IANGLU,MAXNPP,
     1CLLIMI,CLWIDT,
     1ICONT,NUMHPP,IMANUF,
     1XMATN,YMATN,XMITN,YMITN,
     1ISQUAR,
     1IVGMSW,IHGMSW,
     1IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1IX3AUT,ITIAUT,
CCCCC MARCH 1996.  ADD FOLLOWING LINE
     1IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,
     1IHSTMC,IHSTOP,
CCCCC MARCH 2002.  ADD FOLLOWING LINE
     1I6PLMC,
     1ICAPSW,
     1IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,
     1IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1IFOUND,IERROR)
C
C     PURPOSE--GENERATE THE FOLLOWING 6 (POST-FIT VALIDATION)
C              PLOTS (ALL ON THE SAME PAGE)--
C                 1) Y & PREDICTED VERSUS X
C                 2) RESIDUALS VERSUS X
C                 3) RESIDUALS VERSUS PREDICTED
C                 4) LAG PLOT OF RESIDUALS
C                 5) HISTOGRAM OF RESIDUALS
C                 6) NORMAL PROBABILITY PLOT OF RESIDUALS
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93/12
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--DECEMBER  1993.
C     UPDATED       --MARCH      1996. IRHSTG TO DPHIST
C     UPDATED       --AUGUST     1999. DPGRAP ARGUMENT LIST
C     UPDATED       --MARCH      2002. SUPPORT FOR MULTIPLOT CORNER
C                                      COORDINATES (I6PLMC SWITCH)
C     UPDATED       --JANUARY    2010. CALL LIST TO DPHIST
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 ICAPSW
      CHARACTER*4 ICASPL
      CHARACTER*4 ICONT
      CHARACTER*4 IPOWE
      CHARACTER*4 IAND1
      CHARACTER*4 IAND2
      CHARACTER*4 IANGLU
      CHARACTER*4 IBUGG2
      CHARACTER*4 IBUGG3
C
      CHARACTER*4 IBUGUG
      CHARACTER*4 IBUGU2
      CHARACTER*4 IBUGU3
      CHARACTER*4 IBUGU4
C
      CHARACTER*4 IBUGCO
      CHARACTER*4 IBUGEV
      CHARACTER*4 IBUGQ
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISQUAR
      CHARACTER*4 IVGMSW
      CHARACTER*4 IHGMSW
      CHARACTER*4 IREPCH
      CHARACTER*4 IMPSW
C
      CHARACTER*4 I6PLOT
C
      CHARACTER*4 IERAS2
      CHARACTER*4 ICOPS2
      CHARACTER*24 ICHAP1
      CHARACTER*24 ICHAP2
      CHARACTER*4 ILINP1
      CHARACTER*4 ILINP2
C
      CHARACTER*4 IFEED9
C
      CHARACTER*4 IANSYX
      CHARACTER*4 IANSRX
      CHARACTER*4 IANSRP
      CHARACTER*4 IANSLP
      CHARACTER*4 IANSHI
      CHARACTER*4 IANSNP
C
      CHARACTER*4 ICOMSV
      CHARACTER*4 ICO2SV
      CHARACTER*4 IHARSV
      CHARACTER*4 IHA2SV
      CHARACTER*4 IARTSV
C
      CHARACTER*4 IMANUF
C
      CHARACTER*4 IX3AUT
      CHARACTER*4 ITIAUT
C
      CHARACTER*4 IHVERT
      CHARACTER*4 IHVER2
      CHARACTER*4 IHHORI
      CHARACTER*4 IHHOR2
C
      CHARACTER*4 IH4
      CHARACTER*1 IH1
CCCCC MARCH 1996.  ADD FOLLOWING LINE
      CHARACTER*4 IRHSTG
      CHARACTER*4 IHSTCW
      CHARACTER*4 IASHWT
      CHARACTER*4 IHSTEB
      CHARACTER*4 IHSTOU
      CHARACTER*4 IHSTOP
CCCCC MARCH 2002.  ADD FOLLOWING LINE
      CHARACTER*4 I6PLMC
C
      CHARACTER*4 ISTEPN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
      DIMENSION CLLIMI(*)
      DIMENSION CLWIDT(*)
C
      DIMENSION IANSYX(30)
      DIMENSION IANSRX(30)
      DIMENSION IANSRP(30)
      DIMENSION IANSLP(30)
      DIMENSION IANSHI(30)
      DIMENSION IANSNP(30)
C
      DIMENSION IHARSV(100)
      DIMENSION IHA2SV(100)
      DIMENSION IARGSV(100)
      DIMENSION ARGSV(100)
      DIMENSION IARTSV(100)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPC.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCODA.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      CHARACTER*4 IFEEDB
      CHARACTER*4 IPRINT
      CHARACTER*240 ICOUT
C
      COMMON /MACH/IRD,IPR,CPUMIN,CPUMAX,NUMBPC,NUMCPW,NUMBPW
      COMMON /PRINT/IFEEDB,IPRINT
      COMMON /TEXTOU/ICOUT,NCOUT,ILOUT
C
C-----DATA STATEMENTS-------------------------------------------------
C
      DATA (IANSYX(I),I=1,5)
     1/'P   ','L   ','O   ','T   ','    '/
      DATA (IANSRX(I),I=1,9)
     1/'P   ','L   ','O   ','T   ','    ',
     1 'R   ','E   ','S   ','    '/
      DATA (IANSRP(I),I=1,14)
     1/'P   ','L   ','O   ','T   ','    ',
     1 'R   ','E   ','S   ','    ',
     1 'P   ','R   ','E   ','D   ','    '/
      DATA (IANSLP(I),I=1,13)
     1/'L   ','A   ','G   ','    ',
     1 'P   ','L   ','O   ','T   ','    ',
     1 'R   ','E   ','S   ','    '/
      DATA (IANSHI(I),I=1,14)
     1/'H   ','I   ','S   ','T   ','O   ',
     1 'G   ','R   ','A   ','M   ','    ',
     1 'R   ','E   ','S   ','    '/
      DATA (IANSNP(I),I=1,28)
     1/'N   ','O   ','R   ','M   ','A   ','L   ','    ',
     1 'P   ','R   ','O   ','B   ','A   ','B   ',
     1 'I   ','L   ','I   ','T   ','Y   ','    ',
     1 'P   ','L   ','O   ','T   ','    ',
     1 'R   ','E   ','S   ','    '/
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      ISUBN1='DP6P'
      ISUBN2='LO  '
C
      I6PLOT='ON'
      NDONE=0
      ILOCV=7
      IF(IANS(7).EQ.'    ')ILOCV=8
      NCYX=5
      NCRX=9
      NCRP=14
      NCLP=13
      NCHI=14
      NCNP=28
C
C               *************************************************
C               **  TREAT THE     6-PLOT Y X    ANALYSIS CASE  **
C               *************************************************
C
      IF(IBUGG2.EQ.'OFF'.AND.ISUBRO.NE.'6PLO')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DP6PLO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICASPL,IAND1,IAND2
   52 FORMAT('ICASPL,IAND1,IAND2 = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,53)IBUGG2,IBUGG3,IBUGQ
   53 FORMAT('IBUGG2,IBUGG3,IBUGQ = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,54)NUMARG
   54 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO69
      DO61I=1,NUMARG
         WRITE(ICOUT,62)I,IHARG(I),IARGT(I)
   62    FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
         CALL DPWRST('XXX','BUG ')
   61 CONTINUE
   69 CONTINUE
   90 CONTINUE
C
C               **************************************************
C               **  STEP 10--                                   **
C               **  EXTRACT THE COMMAND                         **
C               **************************************************
C
      IF(ICOM.EQ.'6PLO')THEN
         ISHIFT=1
         CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1   IBUGG3,IERROR)
         ICOM='6   '
         ICOM2='    '
         IHARG(1)='PLOT'
         IHARG2(1)='    '
         IARG(1)=(-1)
         ARG(1)=(-1.0)
         IARGT(1)='WORD'
      ENDIF
C
C               **************************************************
C               **   STEP 20--                                  **
C               **   SAVE INITIAL SETTINGS                      **
C               **************************************************
C
      ISTEPN='20'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      PWXMN2=PWXMIN
      PWXMX2=PWXMAX
      PWYMN2=PWYMIN
      PWYMX2=PWYMAX
      PMXMN2=PMXMIN
      PMXMX2=PMXMAX
      PMYMN2=PMYMIN
      PMYMX2=PMYMAX
      IF(I6PLMC.EQ.'OFF')THEN
        PMXMIN=10.0
        PMXMAX=90.0
        PMYMIN=20.0
        PMYMAX=90.0
      ENDIF
      PMXINC=(PMXMAX-PMXMIN)/3.0
      PMYINC=(PMYMAX-PMYMIN)/2.0
      IERAS2=IERASW
      ICOPS2=ICOPSW
      ICHAP1=ICHAPA(1)
      ICHAP2=ICHAPA(2)
      ILINP1=ILINPA(1)
      ILINP2=ILINPA(2)
      IFEED9=IFEEDB
C
      IHVERT=IHARG(2)
      IHVER2=IHARG2(2)
      IHHORI=IHARG(3)
      IHHOR2=IHARG2(3)
C
      ICOMSV=ICOM
      ICO2SV=ICOM2
      NUMASV=NUMARG
      DO1100I=1,NUMARG
         IHARSV(I)=IHARG(I)
         IHA2SV(I)=IHARG2(I)
         IARGSV(I)=IARG(I)
         ARGSV(I)=ARG(I)
         IARTSV(I)=IARGT(I)
 1100 CONTINUE
C
C               **************************************
C               **  STEP XX--                       **
C               **  LOCATE SUBSET/EXCEPT/FOR        **
C               **  (STORE IT IN ILOCSF)            **
C               **************************************
C
      ILOCSF=IWIDTH+1
      IMAX=IWIDTH-6
      IF(IMAX.GE.1)THEN
         DO1200I=1,IMAX
            IP1=I+1
            IP2=I+2
            IP3=I+3
            IP4=I+4
            IP5=I+5
            IP6=I+6
            ILOCSF=I
            IF(IANSLC(I).EQ.'S'.AND.IANSLC(IP1).EQ.'U'.AND.
     1         IANSLC(IP2).EQ.'B'.AND.IANSLC(IP3).EQ.'S'.AND.
     1         IANSLC(IP4).EQ.'E'.AND.IANSLC(IP5).EQ.'T'.AND.
     1         IANSLC(IP6).EQ.' ')GOTO1290
            IF(IANSLC(I).EQ.'E'.AND.IANSLC(IP1).EQ.'X'.AND.
     1         IANSLC(IP2).EQ.'C'.AND.IANSLC(IP3).EQ.'E'.AND.
     1         IANSLC(IP4).EQ.'P'.AND.IANSLC(IP5).EQ.'T'.AND.
     1         IANSLC(IP6).EQ.' ')GOTO1290
            IF(IANSLC(I).EQ.'F'.AND.IANSLC(IP1).EQ.'O'.AND.
     1          IANSLC(IP2).EQ.'R'.AND.IANSLC(IP3).EQ.' ')GOTO1290
            IF(IANSLC(I).EQ.'s'.AND.IANSLC(IP1).EQ.'u'.AND.
     1         IANSLC(IP2).EQ.'b'.AND.IANSLC(IP3).EQ.'s'.AND.
     1         IANSLC(IP4).EQ.'e'.AND.IANSLC(IP5).EQ.'t'.AND.
     1         IANSLC(IP6).EQ.' ')GOTO1290
            IF(IANSLC(I).EQ.'e'.AND.IANSLC(IP1).EQ.'x'.AND.
     1         IANSLC(IP2).EQ.'c'.AND.IANSLC(IP3).EQ.'e'.AND.
     1         IANSLC(IP4).EQ.'p'.AND.IANSLC(IP5).EQ.'t'.AND.
     1         IANSLC(IP6).EQ.' ')GOTO1290
            IF(IANSLC(I).EQ.'f'.AND.IANSLC(IP1).EQ.'o'.AND.
     1         IANSLC(IP2).EQ.'r'.AND.IANSLC(IP3).EQ.' ')GOTO1290
 1200    CONTINUE
         ILOCSF=IWIDTH+1
      ENDIF
 1290 CONTINUE
C
C               **************************************************
C               **   STEP 21--                                  **
C               **   GENERATE     PLOT Y PRED VS X              **
C               **************************************************
C
 2100 CONTINUE
      ISTEPN='21'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=1
C
CCCCC MARCH 2002.  SUPPORT FOR MULUTPLOT CORNER COORDINATES
CCCCC PWXMIN=10.0
CCCCC PWXMAX=36.666667
CCCCC PWYMIN=50.0
CCCCC PWYMAX=90.0
      PWXMIN=PMXMIN
      PWXMAX=PMXMIN + PMXINC
      PWYMIN=PMYMIN + PMYINC
      PWYMAX=PMYMAX
C
      ICOPSW='OFF'
      IFEEDB='OFF'
      ICHAPA(1)='X   '
      ICHAPA(2)='    '
      ILINPA(1)='    '
      ILINPA(2)='SOLI'
C
      ISHIFT=1
      CALL SHIFTR(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG3,IERROR)
      ICOM='PLOT'
      ICOM2='    '
      IHARG(1)=IHVERT
      IHARG2(1)=IHVER2
      IHARG(2)='PRED'
      IHARG2(2)='   '
      IHARG(3)='VS  '
      IHARG2(3)='    '
      IHARG(4)=IHHORI
      IHARG2(4)=IHHOR2
      CALL DPPLOT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IANGLU,MAXNPP,
     1IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO8000
C
      IF(IX3AUT.EQ.'ON'.OR.ITIAUT.EQ.'ON')THEN
         J=NCYX
         DO2111I=1,4
            IH4='    '
            IH1=IHVERT(I:I)
            IH4(1:1)=IH1
            IF(IH1.NE.' ')THEN
               J=J+1
               IANSYX(J)=IH4
            ENDIF
 2111    CONTINUE
         DO2112I=1,4
            IH4='    '
            IH1=IHVER2(I:I)
            IH4(1:1)=IH1
            IF(IH1.NE.' ')THEN
               J=J+1
               IANSYX(J)=IH4
            ENDIF
 2112    CONTINUE
         J=J+1
         IANSYX(J)=' '
         J=J+1
         IANSYX(J)='P'
         J=J+1
         IANSYX(J)='R'
         J=J+1
         IANSYX(J)='E'
         J=J+1
         IANSYX(J)='D'
         J=J+1
         IANSYX(J)=' '
         J=J+1
         IANSYX(J)='V'
         J=J+1
         IANSYX(J)='S'
         J=J+1
         IANSYX(J)=' '
         DO2113I=1,4
            IH4='    '
            IH1=IHHORI(I:I)
            IH4(1:1)=IH1
            IF(IH1.NE.' ')THEN
               J=J+1
               IANSYX(J)=IH4
            ENDIF
 2113    CONTINUE
         DO2114I=1,4
            IH4='    '
            IH1=IHHOR2(I:I)
            IH4(1:1)=IH1
            IF(IH1.NE.' ')THEN
               J=J+1
               IANSYX(J)=IH4
            ENDIF
 2114    CONTINUE
      ENDIF
      JHOLD=J
      GOTO6000
C
C               **************************************************
C               **   STEP 22--                                  **
C               **   GENERATE     PLOT RES X                    **
C               **************************************************
C
 2200 CONTINUE
      ISTEPN='22'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=2
C
CCCCC MARCH 2002.  SUPPORT FOR MULUTPLOT CORNER COORDINATES
CCCCC PWXMIN=36.666667
CCCCC PWXMAX=63.333333
CCCCC PWYMIN=50.0
CCCCC PWYMAX=90.0
      PWXMIN=PMXMIN + PMXINC
      PWXMAX=PMXMIN + 2.0*PMXINC
      PWYMIN=PMYMIN + PMYINC
      PWYMAX=PMYMAX
C
      IERASW='OFF'
      ICOPSW='OFF'
      IFEEDB='OFF'
      ICHAPA(1)=ICHAP1
      ILINPA(1)=ILINP1
C
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG3,IERROR)
      ICOM='PLOT'
      ICOM2='    '
      IHARG(1)='RES '
      IHARG2(1)='   '
      IHARG(2)=IHHORI
      IHARG2(2)=IHHOR2
      CALL DPPLOT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IANGLU,MAXNPP,
     1IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO8000
C
      IF(IX3AUT.EQ.'ON'.OR.ITIAUT.EQ.'ON')THEN
         J=NCRX
         DO2211I=1,4
            IH4='    '
            IH1=IHHORI(I:I)
            IH4(1:1)=IH1
            IH4(1:1)=IH1
            IF(IH1.NE.' ')THEN
               J=J+1
               IANSRX(J)=IH4
            ENDIF
 2211    CONTINUE
         DO2212I=1,4
            IH4='    '
            IH1=IHHOR2(I:I)
            IH4(1:1)=IH1
            IF(IH1.NE.' ')THEN
               J=J+1
               IANSRX(J)=IH4
            ENDIF
 2212    CONTINUE
         J=J+1
         IANSRX(J)=' '
      ENDIF
      JHOLD=J
      GOTO6000
C
C               **************************************************
C               **   STEP 23--                                  **
C               **   GENERATE     PLOT RES PRED                 **
C               **************************************************
C
 2300 CONTINUE
      ISTEPN='23'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=3
C
CCCCC MARCH 2002.  SUPPORT FOR MULUTPLOT CORNER COORDINATES
CCCCC PWXMIN=63.333333
CCCCC PWXMAX=90.0
CCCCC PWYMIN=50.0
CCCCC PWYMAX=90.0
      PWXMIN=PMXMIN + 2.0*PMXINC
      PWXMAX=PMXMAX
      PWYMIN=PMYMIN + PMYINC
      PWYMAX=PMYMAX
C
      IERASW='OFF'
      ICOPSW='OFF'
      IFEEDB='OFF'
      ICHAP1=ICHAPA(1)
      ILINP1=ILINPA(1)
C
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG3,IERROR)
      ICOM='PLOT'
      ICOM2='    '
      IHARG(1)='RES '
      IHARG2(1)='    '
      IHARG(2)='PRED'
      IHARG2(2)='   '
      CALL DPPLOT(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IANGLU,MAXNPP,
     1IBUGG2,IBUGG3,IBUGCO,IBUGEV,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO8000
C
      J=NCRP
      JHOLD=J
      GOTO6000
C
C               **************************************************
C               **   STEP 24--                                  **
C               **   GENERATE    LAG PLOT RES                   **
C               **************************************************
C
 2400 CONTINUE
      ISTEPN='24'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=4
C
CCCCC MARCH 2002.  SUPPORT FOR MULUTPLOT CORNER COORDINATES
CCCCC PWXMIN=10.0
CCCCC PWXMAX=36.666667
CCCCC PWYMIN=10.0
CCCCC PWYMAX=50.0
      PWXMIN=PMXMIN
      PWXMAX=PMXMIN + PMXINC
      PWYMIN=PMYMIN
      PWYMAX=PMYMIN + PMYINC
C
      IERASW='OFF'
      ICOPSW='OFF'
      IFEEDB='OFF'
      ICHAPA(1)='X   '
      ILINPA(1)='    '
C
      ISHIFT=1
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG3,IERROR)
      ICOM='LAG '
      ICOM2='    '
      IHARG(1)='PLOT'
      IHARG2(1)='    '
      IHARG(2)='RES '
      IHARG2(2)='    '
      CALL DPLAG(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO8000
C
      J=NCLP
      JHOLD=J
      GOTO6000
C
C               **************************************************
C               **   STEP 25--                                  **
C               **   GENERATE     HISTOGRAM RES                 **
C               **************************************************
C
 2500 CONTINUE
      ISTEPN='25'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=5
C
CCCCC MARCH 2002.  SUPPORT FOR MULUTPLOT CORNER COORDINATES
CCCCC PWXMIN=36.666667
CCCCC PWXMAX=63.333333
CCCCC PWYMIN=10.0
CCCCC PWYMAX=50.0
      PWXMIN=PMXMIN + PMXINC
      PWXMAX=PMXMIN + 2.0*PMXINC
      PWYMIN=PMYMIN
      PWYMAX=PMYMIN + PMYINC
C
      IERASW='OFF'
      ICOPSW='OFF'
      IFEEDB='OFF'
      ICHAPA(1)='    '
      ILINPA(1)='SOLI'
C
      ISHIFT=2
      CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
     1IBUGG3,IERROR)
      ICOM='HIST'
      ICOM2='    '
      IHARG(1)='RES '
      IHARG2(1)='    '
      CALL DPHIST(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1CLLIMI,CLWIDT,
CCCCC MARCH 1996.  ADD FOLLOWING LINE
     1IRHSTG,IHSTCW,IASHWT,IHSTEB,IHSTOU,
     1IHSTMC,IHSTOP,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO8000
C
      J=NCHI
      JHOLD=J
      GOTO6000
C
C               **************************************************
C               **   STEP 26--                                  **
C               **   GENERATE     NORMAL PROBABILITY PLOT RES   **
C               **************************************************
C
 2600 CONTINUE
      ISTEPN='26'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IFLAG=6
C
CCCCC MARCH 2002.  SUPPORT FOR MULTIPLOT CORNER COORDINATES
CCCCC PWXMIN=63.333333
CCCCC PWXMAX=90.0
CCCCC PWYMIN=10.0
CCCCC PWYMAX=50.0
      PWXMIN=PMXMIN + 2.0*PMXINC
      PWXMAX=PMXMAX
      PWYMIN=PMYMIN
      PWYMAX=PMYMIN + PMYINC
C
      IERASW='OFF'
      ICOPSW=ICOPS2
      IFEEDB='OFF'
      ICHAPA(1)=ICHAP1
      ILINPA(1)=ILINP1
C
CCCCC ISHIFT=0
CCCCC CALL SHIFTL(ISHIFT,IHARG,IHARG2,IARG,ARG,IARGT,NUMARG,
CCCCC1IBUGG3,IERROR)
      ICOM='NORM'
      ICOM2='    '
      IHARG(1)='PROB'
      IHARG2(1)='    '
      IHARG(2)='PLOT'
      IHARG2(2)='    '
      IHARG(3)='RES '
      IHARG2(3)='    '
      CALL DPPP(NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2,
     1IBUGG2,IBUGG3,IBUGQ,ISUBRO,IFOUND,IERROR)
      IF(IERROR.EQ.'YES')GOTO8000
C
      J=NCNP
      JHOLD=J
      GOTO6000
C
C               **************************************************
C               **   STEP 60--                                  **
C               **   PLOT THE CURRENT PLOT (OUT OF THE 6)       **
C               **************************************************
 6000 CONTINUE
      ISTEPN='60'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IX3AUT.EQ.'ON')THEN
         J=0
         DO6011I=1,JHOLD
            J=J+1
            IF(IFLAG.EQ.1)IX3LTE(J)=IANSYX(J)
            IF(IFLAG.EQ.2)IX3LTE(J)=IANSRX(J)
            IF(IFLAG.EQ.3)IX3LTE(J)=IANSRP(J)
            IF(IFLAG.EQ.4)IX3LTE(J)=IANSLP(J)
            IF(IFLAG.EQ.5)IX3LTE(J)=IANSHI(J)
            IF(IFLAG.EQ.6)IX3LTE(J)=IANSNP(J)
 6011    CONTINUE
         IF(ILOCSF.LE.IWIDTH)THEN
            J=JHOLD
            DO6012I=ILOCSF,IWIDTH
               J=J+1
               IX3LTE(J)=IANSLC(I)
 6012       CONTINUE
         ENDIF
         NCX3LA=J
      ENDIF
C
      IF(ITIAUT.EQ.'ON')THEN
         J=0
         DO6021I=1,JHOLD
            J=J+1
            IF(IFLAG.EQ.1)ITITTE(J)=IANSYX(J)
            IF(IFLAG.EQ.2)ITITTE(J)=IANSRX(J)
            IF(IFLAG.EQ.3)ITITTE(J)=IANSRP(J)
            IF(IFLAG.EQ.4)ITITTE(J)=IANSLP(J)
            IF(IFLAG.EQ.5)ITITTE(J)=IANSHI(J)
            IF(IFLAG.EQ.6)ITITTE(J)=IANSNP(J)
 6021    CONTINUE
         IF(ILOCSF.LE.IWIDTH)THEN
            J=JHOLD
            DO6022I=ILOCSF,IWIDTH
               J=J+1
               ITITTE(J)=IANSLC(I)
 6022       CONTINUE
         ENDIF
         NCTITL=J
      ENDIF
C
      ICONT=IDCONT(1)
      IPOWE=IDPOWE(1)
      NUMHPP=IDNHPP(1)
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')THEN
         WRITE(ICOUT,6031)IMANUF,NUMDEV,IDMANU(1)
 6031    FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
         CALL DPWRST('XXX','BUG ')
         WRITE(ICOUT,6032)NDONE
 6032    FORMAT('NDONE = ',I8)
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
CCCCC ADD FOLLOWING TO DPGRAP ARGUMENT LIST
      IMPARG=2
      CALL DPGRAP(Y,X,X3D,D,N,NPLOTP,ICASPL,ICONT,IPOWE,NUMHPP,
     1XMATN,YMATN,XMITN,YMITN,
     1ISQUAR,
     1IVGMSW,IHGMSW,
     1IHNAME,IHNAM2,IUSE,IN,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1YPLOT,XPLOT,X2PLOT,TAGPLO,
     1IMPSW,IMPNR,IMPNC,IMPCO,IMPCO9,
     1IMPARG,
     1PMXMIN,PMXMAX,PMYMIN,PMYMAX,
     1MAXCOL,
     1DSIZE,DSYMB,DCOLOR,DFILL,
     1ICAPSW,
     1IBUGUG,IBUGU2,IBUGU3,IBUGU4,ISUBRO,
     1IERROR)
      IF(IERROR.EQ.'NO')IAND1=IAND2
      IF(IERROR.EQ.'YES')GOTO9000
C
      NDONE=NDONE+1
C
      ICOM=ICOMSV
      ICOM2=ICO2SV
      NUMARG=NUMASV
      DO6050I=1,NUMARG
         IHARG(I)=IHARSV(I)
         IHARG2(I)=IHA2SV(I)
         IARG(I)=IARGSV(I)
         ARG(I)=ARGSV(I)
         IARGT(I)=IARTSV(I)
 6050 CONTINUE
C
      IF(NDONE.LE.1)GOTO2200
      IF(NDONE.EQ.2)GOTO2300
      IF(NDONE.EQ.3)GOTO2400
      IF(NDONE.EQ.4)GOTO2500
      IF(NDONE.EQ.5)GOTO2600
      IF(NDONE.GE.6)GOTO8000
C
C               **************************************************
C               **   STEP 80--                                  **
C               **   REINSTATE INITIAL SETTINGS                 **
C               **************************************************
C
 8000 CONTINUE
      ISTEPN='80'
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IBUGG2.EQ.'ON'.OR.ISUBRO.EQ.'6PLO')THEN
         WRITE(ICOUT,8007)IMANUF,NUMDEV,IDMANU(1)
 8007    FORMAT('IMANUF,NUMDEV,IDMANU(1) = ',A4,I8,2X,A4)
         CALL DPWRST('XXX','BUG ')
      ENDIF
C
      PWXMIN=PWXMN2
      PWXMAX=PWXMX2
      PWYMIN=PWYMN2
      PWYMAX=PWYMX2
C
      IERASW=IERAS2
      ICOPSW=ICOPS2
      IFEEDB=IFEED9
      ICHAPA(1)=ICHAP1
      ICHAPA(2)=ICHAP2
      ILINPA(1)=ILINP1
      ILINPA(2)=ILINP2
C
      ICOM='6   '
      ICOM2='    '
      IHARG(1)='PLOT'
      IHARG2(1)='    '
      IHARG(2)=IHVERT
      IHARG2(2)=IHVER2
      IHARG(3)=IHHORI
      IHARG2(3)=IHHOR2
      IF(IERROR.EQ.'YES')GOTO9000
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG2.EQ.'OFF'.AND.ISUBRO.NE.'6PLO')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DP6PLO--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IFOUND,IERROR
 9012 FORMAT('IFOUND,IERROR = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2
 9013 FORMAT('NPLOTV,NPLOTP,NS,ICASPL,IAND1,IAND2 = ',
     1I8,I8,I8,2X,A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)NDONE
 9014 FORMAT('NDONE = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9015)IHVERT,IHVER2
 9015 FORMAT('IHVERT,IHVER2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9016)IHHORI,IHHOR2
 9016 FORMAT('IHHORI,IHHOR2 = ',A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9017)NUMARG
 9017 FORMAT('NUMARG = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9018)IBUGG2,ISUBRO,IERROR
 9018 FORMAT('IBUGG2,ISUBRO,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      IF(NUMARG.LE.0)GOTO9029
      DO9021I=1,NUMARG
         WRITE(ICOUT,9022)I,IHARG(I),IARGT(I)
 9022    FORMAT('I,IHARG(I),IARGT(I) = ',I8,2X,A4,2X,A4)
         CALL DPWRST('XXX','BUG ')
 9021 CONTINUE
 9029 CONTINUE
 9090 CONTINUE
C
      RETURN
      END

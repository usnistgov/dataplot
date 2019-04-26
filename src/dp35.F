      SUBROUTINE DRTIL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3151--LOWER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   5,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   3,  -2/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   3,  -6/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   4,  -8/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   5,  -9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   7,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   9,  -7/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  10,  -5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',   6,   5/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   4,  -2/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   4,  -8/
      DATA IOPERA(  12),IX(  12),IY(  12)/'MOVE',   5,   5/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   7,   5/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   5,  -2/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   4,  -6/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE',   3,  -2/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   3,   1/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   2,   4/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   0,   5/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -2,   5/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -5,   4/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -7,   1/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -8,  -2/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -8,  -4/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -7,  -7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -6,  -8/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -4,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -2,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   0,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   1,  -7/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   2,  -5/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   3,  -2/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  -4,   4/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -6,   1/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -7,  -2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -7,  -5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -6,  -7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -2,   5/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -4,   3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -5,   1/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -6,  -2/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -6,  -5/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -5,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -4,  -9/
C
      DATA IXMIND(   1)/ -11/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  22/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  44/
C
C     DEFINE CHARACTER   3152--LOWER CASE B
C
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',  -2,  12/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -4,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -5,  -1/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -5,  -5/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -4,  -7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -3,  -8/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -1,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   1,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   4,  -8/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   6,  -5/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   7,  -2/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   7,   0/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   6,   3/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   5,   4/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   3,   5/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   1,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -1,   4/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -2,   3/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -3,   1/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -4,  -2/
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -1,  12/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -3,   5/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -4,   1/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -4,  -5/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -3,  -8/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   4,  -7/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,  -5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   6,  -2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   6,   1/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   5,   3/
      DATA IOPERA(  75),IX(  75),IY(  75)/'MOVE',  -5,  12/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   0,  12/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -2,   5/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -4,  -2/
      DATA IOPERA(  79),IX(  79),IY(  79)/'MOVE',   1,  -9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   3,  -7/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   4,  -5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   5,  -2/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   5,   1/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   4,   4/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   3,   5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',  -4,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -1,  11/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',  -3,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -2,  10/
C
      DATA IXMIND(   2)/  -9/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  19/
      DATA ISTARD(   2)/  45/
      DATA NUMCOO(   2)/  45/
C
C     DEFINE CHARACTER   3153--LOWER CASE C
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   5,   1/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   5,   2/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   4,   2/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   4,   0/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   6,   0/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   6,   2/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   5,   4/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   3,   5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   0,   5/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -3,   4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -5,   1/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -6,  -2/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -6,  -4/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -5,  -7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -4,  -8/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -2,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   0,  -9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   3,  -8/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   5,  -5/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',  -3,   3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -4,   1/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -5,  -2/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -5,  -5/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -4,  -7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',   0,   5/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -2,   3/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -3,   1/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -4,  -2/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -4,  -5/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -3,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   3)/  -9/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  18/
      DATA ISTARD(   3)/  90/
      DATA NUMCOO(   3)/  31/
C
C     DEFINE CHARACTER   3154--LOWER CASE D
C
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',   7,  12/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   4,   1/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   3,  -3/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   3,  -6/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   4,  -8/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   5,  -9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   7,  -9/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   9,  -7/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  10,  -5/
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',   8,  12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   5,   1/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   4,  -3/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   4,  -8/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',   4,  12/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   9,  12/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   5,  -2/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   4,  -6/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',   3,  -2/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   3,   1/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   2,   4/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   0,   5/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -2,   5/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -5,   4/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -7,   1/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -8,  -2/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -8,  -4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -7,  -7/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -6,  -8/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -4,  -9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -2,  -9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   0,  -8/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   1,  -7/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   2,  -5/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   3,  -2/
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',  -5,   3/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -6,   1/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -7,  -2/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -7,  -5/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -6,  -7/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',  -2,   5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -4,   3/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -5,   1/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6,  -2/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -6,  -5/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -5,  -8/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -4,  -9/
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',   5,  12/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   8,  11/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',   6,  12/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   7,  10/
C
      DATA IXMIND(   4)/ -11/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  22/
      DATA ISTARD(   4)/ 121/
      DATA NUMCOO(   4)/  50/
C
C     DEFINE CHARACTER   3155--LOWER CASE E
C
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',  -5,  -4/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -1,  -3/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   2,  -2/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   5,   0/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   6,   2/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   5,   4/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,   5/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   0,   5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -3,   4/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -5,   1/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -6,  -2/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -6,  -4/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -5,  -7/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -4,  -8/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -2,  -9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   0,  -9/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   3,  -8/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   5,  -6/
      DATA IOPERA( 189),IX( 189),IY( 189)/'MOVE',  -3,   3/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -4,   1/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -5,  -2/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -5,  -5/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -4,  -7/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   0,   5/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -2,   3/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -3,   1/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -4,  -2/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -4,  -5/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -3,  -8/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   5)/  -9/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  18/
      DATA ISTARD(   5)/ 171/
      DATA NUMCOO(   5)/  30/
C
C     DEFINE CHARACTER   3156--LOWER CASE F
C
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   8,  10/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   8,  11/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   7,  11/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   7,   9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   9,   9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   9,  11/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   8,  12/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   6,  12/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   4,  11/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   2,   9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   1,   7/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   0,   4/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -1,   0/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -3,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -4, -12/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -5, -14/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -7, -16/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',   2,   8/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   1,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   0,   0/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -2,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -3, -12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',   6,  12/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   4,  10/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   3,   8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   2,   5/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   1,   0/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -1,  -8/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -2, -11/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -3, -13/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -5, -15/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -7, -16/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -9, -16/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW', -10, -15/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW', -10, -13/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -8, -13/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -8, -15/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -9, -15/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -9, -14/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',  -4,   5/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   7,   5/
C
      DATA IXMIND(   6)/  -8/
      DATA IXMAXD(   6)/   8/
      DATA IXDELD(   6)/  16/
      DATA ISTARD(   6)/ 201/
      DATA NUMCOO(   6)/  41/
C
C     DEFINE CHARACTER   3157--LOWER CASE G
C
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',   6,   5/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   2,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   1, -12/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -1, -15/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -3, -16/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',   7,   5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   3,  -9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   1, -13/
      DATA IOPERA( 250),IX( 250),IY( 250)/'MOVE',   6,   5/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   8,   5/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   4,  -9/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   2, -13/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   0, -15/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -3, -16/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -6, -16/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -8, -15/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -9, -14/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -9, -12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -7, -12/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -7, -14/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -8, -14/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -8, -13/
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',   4,  -2/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   4,   1/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   3,   4/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   1,   5/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -1,   5/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -4,   4/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -6,   1/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -7,  -2/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -7,  -4/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',  -6,  -7/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -5,  -8/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -3,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -1,  -9/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   1,  -8/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   2,  -7/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   3,  -5/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   4,  -2/
      DATA IOPERA( 281),IX( 281),IY( 281)/'MOVE',  -4,   3/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -5,   1/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -6,  -2/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  -6,  -5/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',  -5,  -7/
      DATA IOPERA( 286),IX( 286),IY( 286)/'MOVE',  -1,   5/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',  -3,   3/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',  -4,   1/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -5,  -2/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -5,  -5/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -4,  -8/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -3,  -9/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/ 242/
      DATA NUMCOO(   7)/  51/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3158--LOWER CASE H
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -3,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -7,  -9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'MOVE',  -2,  12/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -8,  -9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'MOVE',  -6,  12/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -1,  12/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',  -5,  -2/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -3,   2/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,   4/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   1,   5/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   3,   5/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   5,   4/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   6,   2/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   6,  -1/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   4,  -6/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',   5,   4/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   5,   0/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,  -4/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   4,  -8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   5,   2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   3,  -3/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   3,  -6/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   4,  -8/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   5,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   7,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   9,  -7/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  10,  -5/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',  -5,  12/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -2,  11/
      DATA IOPERA(  32),IX(  32),IY(  32)/'MOVE',  -4,  12/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -3,  10/
C
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  22/
      DATA ISTARD(   8)/   1/
      DATA NUMCOO(   8)/  33/
C
C     DEFINE CHARACTER   3159--LOWER CASE I
C
      DATA IOPERA(  34),IX(  34),IY(  34)/'MOVE',   1,  12/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   1,  10/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   3,  10/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   3,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   1,  12/
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',   2,  12/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   2,  10/
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',   1,  11/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   3,  11/
      DATA IOPERA(  43),IX(  43),IY(  43)/'MOVE',  -6,   1/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -5,   3/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -3,   5/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -1,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   0,   4/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   1,   2/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   1,  -1/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -1,  -6/
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',   0,   4/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   0,   0/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -1,  -4/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -1,  -8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',   0,   2/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -2,  -3/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -2,  -6/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -1,  -8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   0,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   2,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   4,  -7/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   5,  -5/
C
      DATA IXMIND(   9)/  -7/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  13/
      DATA ISTARD(   9)/  34/
      DATA NUMCOO(   9)/  29/
C
C     DEFINE CHARACTER   3160--LOWER CASE J
C
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',   3,  12/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   3,  10/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   5,  10/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   5,  12/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   3,  12/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',   4,  12/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   4,  10/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   3,  11/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,  11/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -5,   1/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,   3/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -2,   5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   0,   5/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   1,   4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   2,   2/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   2,  -1/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   0,  -8/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -1, -11/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -2, -13/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -4, -15/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -6, -16/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -8, -16/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -9, -15/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -9, -13/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -7, -13/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -7, -15/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -8, -15/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -8, -14/
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',   1,   4/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   1,  -1/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -1,  -8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -2, -11/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -3, -13/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   1,   2/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   0,  -2/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -2,  -9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -3, -12/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -4, -14/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -6, -16/
C
      DATA IXMIND(  10)/  -7/
      DATA IXMAXD(  10)/   6/
      DATA IXDELD(  10)/  13/
      DATA ISTARD(  10)/  63/
      DATA NUMCOO(  10)/  39/
C
C     DEFINE CHARACTER   3161--LOWER CASE K
C
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',  -3,  12/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -9,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -7,  -9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -2,  12/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -8,  -9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -6,  12/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -1,  12/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -7,  -9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',   7,   3/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   7,   4/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   6,   4/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   6,   2/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   8,   2/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   8,   4/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   7,   5/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   5,   5/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   3,   4/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -1,   0/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -3,  -1/
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',  -5,  -1/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -3,  -1/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -1,  -2/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   0,  -3/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   2,  -7/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   3,  -8/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   5,  -8/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -1,  -3/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   1,  -7/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   2,  -8/
      DATA IOPERA( 131),IX( 131),IY( 131)/'MOVE',  -3,  -1/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -2,  -2/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   0,  -8/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   1,  -9/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   3,  -9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   5,  -8/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   7,  -5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',  -5,  12/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -2,  11/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',  -4,  12/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -3,  10/
C
      DATA IXMIND(  11)/ -11/
      DATA IXMAXD(  11)/  11/
      DATA IXDELD(  11)/  22/
      DATA ISTARD(  11)/ 102/
      DATA NUMCOO(  11)/  40/
C
C     DEFINE CHARACTER   3162--LOWER CASE L
C
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',   2,  12/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -1,   1/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -2,  -3/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -2,  -6/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -1,  -8/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   0,  -9/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   2,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   4,  -7/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   5,  -5/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',   3,  12/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   0,   1/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -1,  -3/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -1,  -8/
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',  -1,  12/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   4,  12/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   0,  -2/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -1,  -6/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',   0,  12/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   3,  11/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',   1,  12/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   2,  10/
C
      DATA IXMIND(  12)/  -6/
      DATA IXMAXD(  12)/   6/
      DATA IXDELD(  12)/  12/
      DATA ISTARD(  12)/ 142/
      DATA NUMCOO(  12)/  21/
C
C     DEFINE CHARACTER   3163--LOWER CASE M
C
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE', -17,   1/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW', -16,   3/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW', -14,   5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW', -12,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW', -11,   4/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW', -10,   2/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW', -10,  -1/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW', -12,  -9/
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE', -11,   4/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW', -11,  -1/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW', -13,  -9/
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE', -11,   2/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW', -12,  -2/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW', -14,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW', -12,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE', -10,  -1/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -8,   2/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -6,   4/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -4,   5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -2,   5/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   0,   4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   1,   2/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   1,  -1/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -1,  -9/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   0,   4/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   0,  -1/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -2,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',   0,   2/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -1,  -2/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -3,  -9/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -1,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   1,  -1/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   3,   2/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   5,   4/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   7,   5/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   9,   5/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  11,   4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  12,   2/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  12,  -1/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  10,  -6/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',  11,   4/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  11,   0/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  10,  -4/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  10,  -8/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  11,   2/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   9,  -3/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   9,  -6/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  10,  -8/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  11,  -9/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  13,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  15,  -7/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  16,  -5/
C
      DATA IXMIND(  13)/ -18/
      DATA IXMAXD(  13)/  17/
      DATA IXDELD(  13)/  35/
      DATA ISTARD(  13)/ 163/
      DATA NUMCOO(  13)/  52/
C
C     DEFINE CHARACTER   3164--LOWER CASE N
C
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE', -11,   1/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW', -10,   3/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -8,   5/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -6,   5/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -5,   4/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -4,   2/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -4,  -1/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -6,  -9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -5,   4/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -5,  -1/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -7,  -9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',  -5,   2/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -6,  -2/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -8,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -6,  -9/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',  -4,  -1/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -2,   2/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   0,   4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   2,   5/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   4,   5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   6,   4/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   7,   2/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   7,  -1/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   5,  -6/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   6,   4/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   6,   0/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   5,  -4/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   5,  -8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',   6,   2/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   4,  -3/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   4,  -6/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   5,  -8/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',   6,  -9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   8,  -9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',  10,  -7/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  11,  -5/
C
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  12/
      DATA IXDELD(  14)/  24/
      DATA ISTARD(  14)/ 215/
      DATA NUMCOO(  14)/  36/
C
C     DEFINE CHARACTER   3165--LOWER CASE O
C
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',  -1,   5/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -4,   4/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -6,   1/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -7,  -2/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -7,  -4/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -6,  -7/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -5,  -8/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -2,  -9/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   1,  -9/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   4,  -8/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   6,  -5/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   7,  -2/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   7,   0/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   6,   3/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   5,   4/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   2,   5/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -1,   5/
      DATA IOPERA( 268),IX( 268),IY( 268)/'MOVE',  -4,   3/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -5,   1/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -6,  -2/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -6,  -5/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -5,  -7/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',   4,  -7/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   5,  -5/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   6,  -2/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   6,   1/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   5,   3/
      DATA IOPERA( 278),IX( 278),IY( 278)/'MOVE',  -1,   5/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -3,   3/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -4,   1/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -5,  -2/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -5,  -5/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -4,  -8/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  -2,  -9/
      DATA IOPERA( 285),IX( 285),IY( 285)/'MOVE',   1,  -9/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   3,  -7/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',   4,  -5/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   5,  -2/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',   5,   1/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',   4,   4/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',   2,   5/
C
      DATA IXMIND(  15)/ -10/
      DATA IXMAXD(  15)/  10/
      DATA IXDELD(  15)/  20/
      DATA ISTARD(  15)/ 251/
      DATA NUMCOO(  15)/  41/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC LOWER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3166--LOWER CASE P
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -10,   1/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9,   3/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -7,   5/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,   5/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -4,   4/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -3,   2/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -3,  -1/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -4,  -5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -7, -16/
      DATA IOPERA(  10),IX(  10),IY(  10)/'MOVE',  -4,   4/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -4,  -1/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -5,  -5/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -8, -16/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',  -4,   2/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -5,  -2/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -9, -16/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -3,  -2/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -2,   1/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -1,   3/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   0,   4/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,   5/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   4,   5/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   6,   4/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   7,   3/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   8,   0/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   8,  -2/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   7,  -5/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   5,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   2,  -9/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   0,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -2,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -3,  -5/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -3,  -2/
      DATA IOPERA(  34),IX(  34),IY(  34)/'MOVE',   6,   3/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   7,   1/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   7,  -2/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   6,  -5/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   5,  -7/
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',   4,   5/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   5,   4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   6,   1/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   6,  -2/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   5,  -5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   4,  -7/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   2,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE', -12, -16/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -4, -16/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',  -8, -15/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW', -11, -16/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',  -8, -14/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW', -10, -16/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE',  -7, -14/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -6, -16/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',  -8, -15/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -5, -16/
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  22/
      DATA ISTARD(  16)/   1/
      DATA NUMCOO(  16)/  55/
C
C     DEFINE CHARACTER   3167--LOWER CASE Q
C
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',   5,   5/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -1, -16/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',   6,   5/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   0, -16/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',   5,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   7,   5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   1, -16/
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',   3,  -2/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   3,   1/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   2,   4/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   0,   5/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -2,   5/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -5,   4/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -7,   1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -8,  -2/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -8,  -4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -7,  -7/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -6,  -8/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -4,  -9/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -2,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   0,  -8/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   1,  -7/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   2,  -5/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   3,  -2/
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -5,   3/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -6,   1/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -7,  -2/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -7,  -5/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -6,  -7/
      DATA IOPERA(  85),IX(  85),IY(  85)/'MOVE',  -2,   5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -4,   3/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -5,   1/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -6,  -2/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -6,  -5/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -5,  -8/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -4,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  -4, -16/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   4, -16/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',   0, -15/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -3, -16/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   0, -14/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -2, -16/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',   1, -14/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   2, -16/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',   0, -15/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   3, -16/
C
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  21/
      DATA ISTARD(  17)/  56/
      DATA NUMCOO(  17)/  46/
C
C     DEFINE CHARACTER   3168--LOWER CASE R
C
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',  -8,   1/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -7,   3/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -5,   5/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -3,   5/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -2,   4/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -1,   2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -1,  -2/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -3,  -9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -2,   4/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -2,  -2/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -4,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -2,   2/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -3,  -2/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -5,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -3,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',   7,   3/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   7,   4/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   6,   4/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   6,   2/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   8,   2/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   8,   4/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   7,   5/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   5,   5/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   3,   4/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   1,   2/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -1,  -2/
C
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/   9/
      DATA IXDELD(  18)/  18/
      DATA ISTARD(  18)/ 102/
      DATA NUMCOO(  18)/  26/
C
C     DEFINE CHARACTER   3169--LOWER CASE S
C
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',   6,   2/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   6,   3/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   5,   3/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   5,   1/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   7,   1/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   7,   3/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   6,   4/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   3,   5/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   0,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -3,   4/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -4,   3/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -4,   1/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -3,  -1/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -1,  -2/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   2,  -3/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   4,  -4/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   5,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',  -3,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -4,   1/
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',  -3,   0/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -1,  -1/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   2,  -2/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   4,  -3/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',   5,  -4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   4,  -8/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',  -4,   3/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -3,   1/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -1,   0/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   2,  -1/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   4,  -2/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   5,  -4/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   5,  -6/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   4,  -8/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   1,  -9/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -2,  -9/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -5,  -8/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -6,  -7/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -6,  -5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -4,  -5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -4,  -7/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -5,  -7/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -5,  -6/
C
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/ 128/
      DATA NUMCOO(  19)/  42/
C
C     DEFINE CHARACTER   3170--LOWER CASE T
C
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   2,  12/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -1,   1/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -2,  -3/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -2,  -6/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -1,  -8/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   2,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   4,  -7/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   5,  -5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',   3,  12/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   0,   1/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -1,  -3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -1,  -8/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',   2,  12/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   4,  12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   0,  -2/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -1,  -6/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',  -4,   5/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   6,   5/
C
      DATA IXMIND(  20)/  -7/
      DATA IXMAXD(  20)/   7/
      DATA IXDELD(  20)/  14/
      DATA ISTARD(  20)/ 170/
      DATA NUMCOO(  20)/  19/
C
C     DEFINE CHARACTER   3171--LOWER CASE U
C
      DATA IOPERA( 189),IX( 189),IY( 189)/'MOVE', -11,   1/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW', -10,   3/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -8,   5/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -6,   5/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -5,   4/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -4,   2/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -4,  -1/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -6,  -6/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',  -5,   4/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -5,   0/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -6,  -4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6,  -8/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',  -5,   2/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -7,  -3/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -7,  -6/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -6,  -8/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -4,  -9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -2,  -9/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   0,  -8/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   2,  -6/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   4,  -3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',   6,   5/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   4,  -3/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   4,  -6/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   5,  -8/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   6,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   8,  -9/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  10,  -7/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  11,  -5/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',   7,   5/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   5,  -3/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   5,  -8/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',   6,   5/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   8,   5/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   6,  -2/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   5,  -6/
C
      DATA IXMIND(  21)/ -12/
      DATA IXMAXD(  21)/  12/
      DATA IXDELD(  21)/  24/
      DATA ISTARD(  21)/ 189/
      DATA NUMCOO(  21)/  36/
C
C     DEFINE CHARACTER   3172--LOWER CASE V
C
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',  -9,   1/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -8,   3/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -6,   5/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -4,   5/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -3,   4/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -2,   2/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -2,  -1/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -4,  -6/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',  -3,   4/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -3,   0/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -4,  -4/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -4,  -8/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',  -3,   2/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -5,  -3/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -5,  -6/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -4,  -8/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -2,  -9/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   0,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   2,  -8/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   4,  -6/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   6,  -3/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   7,   1/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',   7,   5/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   6,   5/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   6,   4/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   7,   2/
C
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/ 225/
      DATA NUMCOO(  22)/  26/
C
C     DEFINE CHARACTER   3173--LOWER CASE W
C
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE', -14,   1/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW', -13,   3/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW', -11,   5/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -9,   5/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -8,   4/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -7,   2/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -7,  -1/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -9,  -6/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',  -8,   4/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -8,   0/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -9,  -4/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -9,  -8/
      DATA IOPERA( 263),IX( 263),IY( 263)/'MOVE',  -8,   2/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW', -10,  -3/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW', -10,  -6/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -9,  -8/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -7,  -9/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -5,  -9/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -3,  -8/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -1,  -6/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   0,  -3/
      DATA IOPERA( 272),IX( 272),IY( 272)/'MOVE',   2,   5/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   0,  -3/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   0,  -6/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   1,  -8/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   3,  -9/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   5,  -9/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   7,  -8/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   9,  -6/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  11,  -3/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  12,   1/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  12,   5/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  11,   5/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  11,   4/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',  12,   2/
      DATA IOPERA( 286),IX( 286),IY( 286)/'MOVE',   3,   5/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',   1,  -3/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   1,  -8/
      DATA IOPERA( 289),IX( 289),IY( 289)/'MOVE',   2,   5/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',   4,   5/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',   2,  -2/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',   1,  -6/
C
      DATA IXMIND(  23)/ -15/
      DATA IXMAXD(  23)/  15/
      DATA IXDELD(  23)/  30/
      DATA ISTARD(  23)/ 251/
      DATA NUMCOO(  23)/  42/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIL4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC LOWER CASE (PART 4).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3174--LOWER CASE X
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -8,   1/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -6,   4/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -4,   5/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -2,   5/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   0,   4/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   1,   2/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   1,   0/
      DATA IOPERA(   8),IX(   8),IY(   8)/'MOVE',  -2,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -1,   4/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -1,   0/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -2,  -4/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -3,  -6/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -5,  -8/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -7,  -9/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -9,  -9/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW', -10,  -8/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW', -10,  -6/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -8,  -6/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -8,  -8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -9,  -8/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -9,  -7/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   0,   3/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   0,   0/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -1,  -4/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -1,  -7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE',   8,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   8,   4/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   7,   4/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   7,   2/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   9,   2/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   9,   4/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   8,   5/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   6,   5/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   4,   4/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   2,   2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   1,   0/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   0,  -4/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,  -8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   1,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE',  -2,  -4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -2,  -6/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -1,  -8/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   1,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   3,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   7,  -5/
C
      DATA IXMIND(  24)/ -11/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  22/
      DATA ISTARD(  24)/   1/
      DATA NUMCOO(  24)/  46/
C
C     DEFINE CHARACTER   3175--LOWER CASE Y
C
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE', -10,   1/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -9,   3/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -7,   5/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -5,   5/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -4,   4/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -3,   2/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -3,  -1/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -5,  -6/
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',  -4,   4/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -4,   0/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -5,  -4/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -5,  -8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE',  -4,   2/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -6,  -3/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -6,  -6/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',  -5,  -8/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -3,  -9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -1,  -9/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   1,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   3,  -6/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   5,  -2/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',   7,   5/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   3,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2, -12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   0, -15/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -2, -16/
      DATA IOPERA(  73),IX(  73),IY(  73)/'MOVE',   8,   5/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   4,  -9/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   2, -13/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   7,   5/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   9,   5/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   5,  -9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   3, -13/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   1, -15/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -2, -16/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -5, -16/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -7, -15/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -8, -14/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -8, -12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -6, -12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -6, -14/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -7, -14/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -7, -13/
C
      DATA IXMIND(  25)/ -11/
      DATA IXMAXD(  25)/  11/
      DATA IXDELD(  25)/  22/
      DATA ISTARD(  25)/  47/
      DATA NUMCOO(  25)/  43/
C
C     DEFINE CHARACTER   3176--LOWER CASE Z
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   7,   5/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   6,   3/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   4,   1/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -4,  -5/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -6,  -7/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -7,  -9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   6,   3/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -3,   3/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -5,   2/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -6,   0/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',   4,   3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   0,   4/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -3,   4/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -4,   3/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',   4,   3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   0,   5/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -3,   5/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -5,   3/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -6,   0/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',  -6,  -7/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   3,  -7/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   5,  -6/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   6,  -4/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -4,  -7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   0,  -8/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   3,  -8/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   4,  -7/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  -4,  -7/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   0,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   3,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   5,  -7/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   6,  -4/
C
      DATA IXMIND(  26)/ -10/
      DATA IXMAXD(  26)/  10/
      DATA IXDELD(  26)/  20/
      DATA ISTARD(  26)/  90/
      DATA NUMCOO(  26)/  32/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIL4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIL4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC NUMERIC (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3250--0
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   2,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -1,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -3,   9/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -5,   6/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -6,   3/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -7,  -1/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -7,  -4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -6,  -7/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -5,  -8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -3,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -1,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   2,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   4,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   6,  -3/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   7,   0/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   8,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   8,   7/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   7,  10/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   6,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   4,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   2,  12/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',  -1,  10/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -3,   8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -4,   6/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -5,   3/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -6,  -1/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -6,  -5/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -5,  -7/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',   2,  -7/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   4,  -5/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   5,  -3/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   6,   0/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   7,   4/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   7,   8/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   6,  10/
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',   2,  12/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   0,  11/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -2,   8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -3,   6/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -4,   3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -5,  -1/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -5,  -6/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -4,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -3,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',  -1,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   1,  -8/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   3,  -5/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   4,  -3/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   5,   0/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   6,   4/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   6,   9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   5,  11/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   4,  12/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  21/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  53/
C
C     DEFINE CHARACTER   3251--1
C
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',   2,   8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -3,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -1,  -9/
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE',   5,  12/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   3,   8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -2,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',   5,  12/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -1,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   5,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   2,   9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -1,   7/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -3,   6/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',   2,   8/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   0,   7/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -3,   6/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  11/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  54/
      DATA NUMCOO(   2)/  15/
C
C     DEFINE CHARACTER   3252--2
C
      DATA IOPERA(  69),IX(  69),IY(  69)/'MOVE',  -3,   7/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -3,   8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -2,   8/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -2,   6/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,   6/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -4,   8/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -3,  10/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -2,  11/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   1,  12/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   4,  12/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   7,  11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   8,   9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   8,   7/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   7,   5/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   5,   3/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -5,  -3/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -7,  -5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -9,  -9/
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE',   6,  11/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   7,   9/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   7,   7/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   6,   5/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   4,   3/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   1,   1/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',   4,  12/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   5,  11/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   6,   9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   6,   7/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   5,   5/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   3,   3/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,  -3/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',  -8,  -7/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  -6/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -5,  -6/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   0,  -7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   5,  -7/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   6,  -6/
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',  -5,  -6/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   0,  -8/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   5,  -8/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',  -5,  -6/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   0,  -9/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   3,  -9/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   5,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   6,  -6/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   6,  -5/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  69/
      DATA NUMCOO(   3)/  46/
C
C     DEFINE CHARACTER   3253--3
C
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',  -3,   7/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -3,   8/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -2,   8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -2,   6/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -4,   6/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -4,   8/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -3,  10/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -2,  11/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   1,  12/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   4,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   7,  11/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   8,   9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   8,   7/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   7,   5/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   6,   4/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   4,   3/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   1,   2/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   6,  11/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   7,   9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   7,   7/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   6,   5/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   5,   4/
      DATA IOPERA( 137),IX( 137),IY( 137)/'MOVE',   4,  12/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   5,  11/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   6,   9/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   6,   7/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   5,   5/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   3,   3/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   1,   2/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE',  -1,   2/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   1,   2/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   4,   1/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   5,   0/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   6,  -2/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   6,  -5/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   5,  -7/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   3,  -8/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   0,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -3,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -6,  -8/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -7,  -7/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -8,  -5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -8,  -3/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -6,  -3/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -6,  -5/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -7,  -5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -7,  -4/
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',   4,   0/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   5,  -2/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   5,  -5/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   4,  -7/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   1,   2/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   3,   1/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   4,  -1/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   4,  -5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   3,  -7/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   2,  -8/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   0,  -9/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/ 115/
      DATA NUMCOO(   4)/  58/
C
C     DEFINE CHARACTER   3254--4
C
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',   5,   8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   0,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   2,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',   8,  12/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,   8/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   1,  -9/
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',   8,  12/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   2,  -9/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   8,  12/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -8,  -3/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   8,  -3/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  21/
      DATA ISTARD(   5)/ 173/
      DATA NUMCOO(   5)/  11/
C
C     DEFINE CHARACTER   3255--5
C
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',  -1,  12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -6,   2/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -1,  12/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   9,  12/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',  -1,  11/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   7,  11/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -2,  10/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   3,  10/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   7,  11/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   9,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',  -6,   2/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -5,   3/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -2,   4/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   1,   4/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   4,   3/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   5,   2/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   6,   0/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,  -3/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   5,  -6/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   3,  -8/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -1,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -4,  -9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -6,  -8/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -7,  -7/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -8,  -5/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -8,  -3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -6,  -3/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -6,  -5/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -7,  -5/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -7,  -4/
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',   4,   2/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   5,   0/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   5,  -3/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   4,  -6/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   2,  -8/
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE',   1,   4/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   3,   3/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   4,   1/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   4,  -3/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   3,  -6/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   1,  -8/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  11/
      DATA IXDELD(   6)/  21/
      DATA ISTARD(   6)/ 184/
      DATA NUMCOO(   6)/  42/
C
C     DEFINE CHARACTER   3256--6
C
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   7,   8/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   7,   9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   6,   9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   6,   7/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   8,   7/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   8,   9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   7,  11/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   5,  12/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   2,  12/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -1,  11/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -3,   9/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -5,   6/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -6,   3/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -7,  -1/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -7,  -4/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -6,  -7/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -5,  -8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -3,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   0,  -9/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   3,  -8/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   5,  -6/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',   6,  -4/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   6,  -1/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   5,   1/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   4,   2/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   2,   3/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -1,   3/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -3,   2/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -4,   1/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -5,  -1/
      DATA IOPERA( 256),IX( 256),IY( 256)/'MOVE',  -2,   9/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',  -4,   6/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -5,   3/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -6,  -1/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',  -6,  -5/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -5,  -7/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',   4,  -6/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   5,  -4/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   5,  -1/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   4,   1/
      DATA IOPERA( 266),IX( 266),IY( 266)/'MOVE',   2,  12/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   0,  11/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -2,   8/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -3,   6/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -4,   3/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -5,  -1/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',  -5,  -6/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',  -4,  -8/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -3,  -9/
      DATA IOPERA( 275),IX( 275),IY( 275)/'MOVE',   0,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   2,  -8/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   3,  -7/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   4,  -4/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',   4,   0/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   3,   2/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',   2,   3/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  11/
      DATA IXDELD(   7)/  21/
      DATA ISTARD(   7)/ 226/
      DATA NUMCOO(   7)/  56/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC NUMERIC (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3257--7
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -4,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -6,   6/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   9,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   8,   9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   6,   6/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   2,   1/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   0,  -2/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -1,  -5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -2,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'MOVE',   0,  -1/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -2,  -5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -3,  -9/
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',   6,   6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   0,   0/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -2,  -3/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -3,  -5/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -4,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -2,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',  -5,   9/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -2,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   0,  12/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   5,   9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',  -3,  11/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   0,  11/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   5,   9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE',  -5,   9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -3,  10/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   0,  10/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   5,   9/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   7,   9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   8,  10/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   9,  12/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  11/
      DATA IXDELD(   8)/  21/
      DATA ISTARD(   8)/   1/
      DATA NUMCOO(   8)/  32/
C
C     DEFINE CHARACTER   3258--8
C
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',   1,  12/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -2,  11/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -3,  10/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -4,   8/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -4,   5/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -3,   3/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -1,   2/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   2,   2/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   5,   3/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   7,   4/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   8,   6/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   8,   9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   7,  11/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   5,  12/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   1,  12/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',   3,  12/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -2,  11/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',  -2,  10/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -3,   8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -3,   4/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -2,   3/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',  -3,   3/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   0,   2/
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',   1,   2/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   5,   3/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',   6,   4/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   7,   6/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   7,   9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   6,  11/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   7,  11/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   3,  12/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',   1,  12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -1,  10/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -2,   8/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -2,   4/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -1,   2/
      DATA IOPERA(  69),IX(  69),IY(  69)/'MOVE',   2,   2/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   4,   3/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   5,   4/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   6,   6/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   6,  10/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   5,  12/
      DATA IOPERA(  75),IX(  75),IY(  75)/'MOVE',  -1,   2/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -5,   1/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -7,  -1/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -8,  -3/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -8,  -6/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -7,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -4,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   0,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   4,  -8/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   5,  -7/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   6,  -5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   6,  -2/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   5,   0/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   4,   1/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   2,   2/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   0,   2/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -5,   1/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  -4,   1/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -6,  -1/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',  -7,  -3/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -7,  -6/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -6,  -8/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',  -7,  -8/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -2,  -9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   4,  -8/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',   4,  -7/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   5,  -5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   5,  -2/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   4,   0/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',   4,   1/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   1,   2/
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',  -1,   2/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -3,   1/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -5,  -1/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,  -3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -6,  -6/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -5,  -8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -4,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',   0,  -9/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   2,  -8/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   3,  -7/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   4,  -5/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   4,  -1/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   3,   1/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   2,   2/
C
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  11/
      DATA IXDELD(   9)/  21/
      DATA ISTARD(   9)/  33/
      DATA NUMCOO(   9)/  87/
C
C     DEFINE CHARACTER   3259--9
C
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   6,   4/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   5,   2/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   4,   1/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   2,   0/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -1,   0/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -3,   1/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -4,   2/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -5,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -5,   7/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -4,   9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -2,  11/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   1,  12/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   4,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   6,  11/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   7,  10/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   8,   7/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   8,   4/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   7,   0/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,  -3/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   4,  -6/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   2,  -8/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -1,  -9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -4,  -9/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -6,  -8/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -7,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -7,  -4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -5,  -4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -5,  -6/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -6,  -6/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -6,  -5/
      DATA IOPERA( 150),IX( 150),IY( 150)/'MOVE',  -3,   2/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -4,   4/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -4,   7/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -3,   9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'MOVE',   6,  10/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   7,   8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   7,   4/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   6,   0/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   5,  -3/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   3,  -6/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',  -1,   0/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -2,   1/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -3,   3/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -3,   7/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -2,  10/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -1,  11/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   1,  12/
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',   4,  12/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   5,  11/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   6,   9/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   6,   4/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   5,   0/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   4,  -3/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   3,  -5/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   1,  -8/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -1,  -9/
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  11/
      DATA IXDELD(  10)/  21/
      DATA ISTARD(  10)/ 120/
      DATA NUMCOO(  10)/  56/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIU1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC UPPER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER  3501--UPPER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   3,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9,  -8/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   1,   8/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   2,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   2,  10/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   3,  -8/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',   3,  12/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   3,  10/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   4,  -7/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   4,  -9/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE',  -6,  -3/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   2,  -3/
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE', -12,  -9/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -6,  -9/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -1,  -9/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   6,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -9,  -8/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW', -11,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',  -9,  -8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -7,  -9/
      DATA IOPERA(  21),IX(  21),IY(  21)/'MOVE',   2,  -8/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   0,  -9/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   2,  -7/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   1,  -9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',   4,  -7/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   5,  -9/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  26/
C
C     DEFINE CHARACTER  3502--UPPER CASE B
C
      DATA IOPERA(  27),IX(  27),IY(  27)/'MOVE',  -3,  12/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -9,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',  -2,  12/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -8,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -1,  12/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -7,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  -6,  12/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   5,  12/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   8,  11/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   9,   9/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   9,   7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   8,   4/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   7,   3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   4,   2/
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',   7,  11/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   8,   9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   8,   7/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   7,   4/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   6,   3/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',   5,  12/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   6,  11/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   7,   9/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   7,   7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   6,   4/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   4,   2/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE',  -4,   2/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   4,   2/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   6,   1/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   7,  -1/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   7,  -3/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   6,  -6/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   4,  -8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   0,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW', -12,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'MOVE',   5,   1/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   6,  -1/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   6,  -3/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   5,  -6/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   3,  -8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',   4,   2/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   5,   0/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   5,  -3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   4,  -6/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   2,  -8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   0,  -9/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -5,  12/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -2,  11/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',  -4,  12/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -3,  10/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   0,  12/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -2,  10/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',   1,  12/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -2,  11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -8,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW', -11,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -8,  -7/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW', -10,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -7,  -7/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -6,  -9/
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',  -8,  -8/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -5,  -9/
C
      DATA IXMIND(   2)/ -12/
      DATA IXMAXD(   2)/  12/
      DATA IXDELD(   2)/  24/
      DATA ISTARD(   2)/  27/
      DATA NUMCOO(   2)/  61/
C
C     DEFINE CHARACTER  3503--UPPER CASE C
C
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   8,  10/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   9,  10/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  10,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   9,   6/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   9,   8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   8,  10/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   7,  11/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   5,  12/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   2,  12/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -1,  11/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -3,   9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,   6/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -6,   3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  -1/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -7,  -4/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -6,  -7/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -5,  -8/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -2,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   1,  -9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   3,  -8/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   5,  -6/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   6,  -4/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -1,  10/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -3,   8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -4,   6/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -5,   3/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -6,  -1/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -6,  -5/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -5,  -7/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',   2,  12/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   0,  11/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -2,   8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',  -3,   6/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -4,   3/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -5,  -1/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -5,  -6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -4,  -8/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  11/
      DATA IXDELD(   3)/  21/
      DATA ISTARD(   3)/  88/
      DATA NUMCOO(   3)/  38/
C
C     DEFINE CHARACTER  3504--UPPER CASE D
C
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',  -3,  12/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -9,  -9/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -2,  12/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -8,  -9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',  -1,  12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -7,  -9/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',  -6,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   3,  12/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   6,  11/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   7,  10/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   8,   7/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   8,   3/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   7,  -1/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   5,  -5/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   3,  -7/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   1,  -8/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -3,  -9/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW', -12,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE',   5,  11/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   6,  10/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   7,   7/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   7,   3/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   6,  -1/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   4,  -5/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   2,  -7/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',   3,  12/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',   5,  10/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   6,   7/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   6,   3/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   5,  -1/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   3,  -5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   0,  -8/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -3,  -9/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',  -5,  12/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -2,  11/
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',  -4,  12/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -3,  10/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',   0,  12/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -2,  10/
      DATA IOPERA( 165),IX( 165),IY( 165)/'MOVE',   1,  12/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -2,  11/
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',  -8,  -8/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW', -11,  -9/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',  -8,  -7/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW', -10,  -9/
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',  -7,  -7/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -6,  -9/
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',  -8,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -5,  -9/
C
      DATA IXMIND(   4)/ -12/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  23/
      DATA ISTARD(   4)/ 126/
      DATA NUMCOO(   4)/  49/
C
C     DEFINE CHARACTER  3505--UPPER CASE E
C
      DATA IOPERA( 175),IX( 175),IY( 175)/'MOVE',  -3,  12/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -9,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'MOVE',  -2,  12/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -8,  -9/
      DATA IOPERA( 179),IX( 179),IY( 179)/'MOVE',  -1,  12/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -7,  -9/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',   3,   6/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   1,  -2/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',  -6,  12/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   9,  12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   8,   6/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -4,   2/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   2,   2/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE', -12,  -9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   3,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   5,  -4/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',  -5,  12/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -2,  11/
      DATA IOPERA( 193),IX( 193),IY( 193)/'MOVE',  -4,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -3,  10/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',   0,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -2,  10/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',   1,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -2,  11/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',   5,  12/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   8,  11/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   6,  12/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   8,  10/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',   7,  12/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   8,   9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',   8,  12/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   8,   6/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',   3,   6/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   1,   2/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   1,  -2/
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',   2,   4/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   0,   2/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   1,   0/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',   2,   3/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -1,   2/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   1,   1/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -8,  -8/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW', -11,  -9/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -8,  -7/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW', -10,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -7,  -7/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -6,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -8,  -8/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -5,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',  -2,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   3,  -8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   0,  -9/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   3,  -7/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',   3,  -7/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   5,  -4/
C
      DATA IXMIND(   5)/ -12/
      DATA IXMAXD(   5)/  11/
      DATA IXDELD(   5)/  23/
      DATA ISTARD(   5)/ 175/
      DATA NUMCOO(   5)/  55/
C
C     DEFINE CHARACTER  3506--UPPER CASE F
C
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',  -3,  12/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -9,  -9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'MOVE',  -2,  12/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -8,  -9/
      DATA IOPERA( 234),IX( 234),IY( 234)/'MOVE',  -1,  12/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',  -7,  -9/
      DATA IOPERA( 236),IX( 236),IY( 236)/'MOVE',   3,   6/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   1,  -2/
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE',  -6,  12/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   9,  12/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   8,   6/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -4,   2/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   2,   2/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE', -12,  -9/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -4,  -9/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',  -5,  12/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -2,  11/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',  -4,  12/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -3,  10/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',   0,  12/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -2,  10/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',   1,  12/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -2,  11/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',   5,  12/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   8,  11/
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',   6,  12/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   8,  10/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',   7,  12/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   8,   9/
      DATA IOPERA( 259),IX( 259),IY( 259)/'MOVE',   8,  12/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   8,   6/
      DATA IOPERA( 261),IX( 261),IY( 261)/'MOVE',   3,   6/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   1,   2/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   1,  -2/
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',   2,   4/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   0,   2/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   1,   0/
      DATA IOPERA( 267),IX( 267),IY( 267)/'MOVE',   2,   3/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -1,   2/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',   1,   1/
      DATA IOPERA( 270),IX( 270),IY( 270)/'MOVE',  -8,  -8/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW', -11,  -9/
      DATA IOPERA( 272),IX( 272),IY( 272)/'MOVE',  -8,  -7/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW', -10,  -9/
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',  -7,  -7/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -6,  -9/
      DATA IOPERA( 276),IX( 276),IY( 276)/'MOVE',  -8,  -8/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',  -5,  -9/
C
      DATA IXMIND(   6)/ -12/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  22/
      DATA ISTARD(   6)/ 230/
      DATA NUMCOO(   6)/  48/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIU1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIU2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC UPPER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER  3507--UPPER CASE G
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   8,  10/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   9,  10/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  10,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   9,   6/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   9,   8/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   8,  10/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   7,  11/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   5,  12/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   2,  12/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -1,  11/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -3,   9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -5,   6/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -6,   3/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -7,  -1/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -7,  -4/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -6,  -7/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -5,  -8/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -2,  -9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   0,  -9/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   3,  -8/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   5,  -6/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   7,  -2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',  -1,  10/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -3,   8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -4,   6/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -5,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -6,  -1/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -6,  -5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -5,  -7/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',   4,  -6/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   5,  -5/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   6,  -2/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',   2,  12/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   0,  11/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -2,   8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -3,   6/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -4,   3/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -5,  -1/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -5,  -6/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -4,  -8/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -2,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   0,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   2,  -8/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   4,  -5/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,  -2/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',   2,  -2/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  10,  -2/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',   3,  -2/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   5,  -3/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',   4,  -2/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   5,  -5/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE',   8,  -2/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   6,  -4/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE',   9,  -2/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   6,  -3/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  12/
      DATA IXDELD(   7)/  22/
      DATA ISTARD(   7)/   1/
      DATA NUMCOO(   7)/  55/
C
C     DEFINE CHARACTER  3508--UPPER CASE H
C
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',  -4,  12/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW', -10,  -9/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',  -3,  12/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -9,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',  -2,  12/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -8,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   8,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   2,  -9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',   9,  12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   3,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',  10,  12/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   4,  -9/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',  -7,  12/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   1,  12/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   5,  12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  13,  12/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -6,   2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   6,   2/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE', -13,  -9/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',  -1,  -9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   7,  -9/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',  -6,  12/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,  11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -5,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -4,  10/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -1,  12/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -3,  10/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',   0,  12/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -3,  11/
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',   6,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   9,  11/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   7,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   8,  10/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',  11,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   9,  10/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  12,  12/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   9,  11/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',  -9,  -8/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW', -12,  -9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',  -9,  -7/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW', -11,  -9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  -8,  -7/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -7,  -9/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',  -9,  -8/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -6,  -9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',   3,  -8/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   0,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',   3,  -7/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   1,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',   4,  -7/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   5,  -9/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE',   3,  -8/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   6,  -9/
C
      DATA IXMIND(   8)/ -13/
      DATA IXMAXD(   8)/  13/
      DATA IXDELD(   8)/  26/
      DATA ISTARD(   8)/  56/
      DATA NUMCOO(   8)/  54/
C
C     DEFINE CHARACTER  3509--UPPER CASE I
C
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',   2,  12/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -4,  -9/
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',   3,  12/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -3,  -9/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',   4,  12/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -2,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',  -1,  12/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   7,  12/
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',  -7,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   1,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   0,  12/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   3,  11/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',   1,  12/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   2,  10/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   5,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   3,  10/
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',   6,  12/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',   3,  11/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -3,  -8/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -6,  -9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',  -3,  -7/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -5,  -9/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',  -2,  -7/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -1,  -9/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',  -3,  -8/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   0,  -9/
C
      DATA IXMIND(   9)/  -7/
      DATA IXMAXD(   9)/   7/
      DATA IXDELD(   9)/  14/
      DATA ISTARD(   9)/ 110/
      DATA NUMCOO(   9)/  26/
C
C     DEFINE CHARACTER  3510--UPPER CASE J
C
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',   5,  12/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   0,  -5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -1,  -7/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -3,  -9/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',   6,  12/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   2,  -1/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   1,  -4/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   0,  -6/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE',   7,  12/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   3,  -1/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   1,  -6/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -1,  -8/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -3,  -9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -5,  -9/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -7,  -8/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -8,  -6/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -8,  -4/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -7,  -3/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -6,  -3/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -5,  -4/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',  -5,  -5/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -6,  -6/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -7,  -6/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',  -7,  -4/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -7,  -5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -6,  -5/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -6,  -4/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -7,  -4/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',   2,  12/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  10,  12/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   3,  12/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   6,  11/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   4,  12/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   5,  10/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   8,  12/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   6,  10/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   9,  12/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   6,  11/
C
      DATA IXMIND(  10)/  -9/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  19/
      DATA ISTARD(  10)/ 136/
      DATA NUMCOO(  10)/  38/
C
C     DEFINE CHARACTER  3511--UPPER CASE K
C
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',  -3,  12/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -9,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',  -2,  12/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',  -8,  -9/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',  -1,  12/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -7,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  10,  11/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -5,   0/
      DATA IOPERA( 182),IX( 182),IY( 182)/'MOVE',  -1,   3/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   3,  -9/
      DATA IOPERA( 184),IX( 184),IY( 184)/'MOVE',   0,   3/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   4,  -9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',   1,   4/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   5,  -8/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',  -6,  12/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   2,  12/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',   7,  12/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  13,  12/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE', -12,  -9/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -4,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   0,  -9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   7,  -9/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',  -5,  12/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -2,  11/
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',  -4,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -3,  10/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   0,  12/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -2,  10/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',   1,  12/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -2,  11/
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',   8,  12/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  10,  11/
      DATA IOPERA( 206),IX( 206),IY( 206)/'MOVE',  12,  12/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  10,  11/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',  -8,  -8/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW', -11,  -9/
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',  -8,  -7/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW', -10,  -9/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',  -7,  -7/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',  -6,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',  -8,  -8/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',  -5,  -9/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   3,  -8/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   1,  -9/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',   3,  -7/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   2,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',   4,  -7/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   6,  -9/
C
      DATA IXMIND(  11)/ -12/
      DATA IXMAXD(  11)/  11/
      DATA IXDELD(  11)/  23/
      DATA ISTARD(  11)/ 174/
      DATA NUMCOO(  11)/  48/
C
C     DEFINE CHARACTER  3512--UPPER CASE L
C
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -1,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -7,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   0,  12/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -6,  -9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   1,  12/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -5,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',  -4,  12/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   4,  12/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE', -10,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   5,  -9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   7,  -3/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',  -3,  12/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   0,  11/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',  -2,  12/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -1,  10/
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',   2,  12/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   0,  10/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   3,  12/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   0,  11/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',  -6,  -8/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -9,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',  -6,  -7/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -8,  -9/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',  -5,  -7/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -4,  -9/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',  -6,  -8/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',  -3,  -9/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',   0,  -9/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   5,  -8/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',   2,  -9/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   6,  -6/
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',   4,  -9/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   7,  -3/
C
      DATA IXMIND(  12)/ -10/
      DATA IXMAXD(  12)/  10/
      DATA IXDELD(  12)/  20/
      DATA ISTARD(  12)/ 222/
      DATA NUMCOO(  12)/  33/
C
C     DEFINE CHARACTER  3513--UPPER CASE M
C
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',  -5,  12/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW', -11,  -8/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -5,  11/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -4,  -7/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -4,  -9/
      DATA IOPERA( 260),IX( 260),IY( 260)/'MOVE',  -4,  12/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',  -3,  -7/
      DATA IOPERA( 262),IX( 262),IY( 262)/'MOVE',  -3,  12/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -2,  -6/
      DATA IOPERA( 264),IX( 264),IY( 264)/'MOVE',   9,  12/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -2,  -6/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -4,  -9/
      DATA IOPERA( 267),IX( 267),IY( 267)/'MOVE',   9,  12/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   3,  -9/
      DATA IOPERA( 269),IX( 269),IY( 269)/'MOVE',  10,  12/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   4,  -9/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',  11,  12/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   5,  -9/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',  -8,  12/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -3,  12/
      DATA IOPERA( 275),IX( 275),IY( 275)/'MOVE',   9,  12/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  14,  12/
      DATA IOPERA( 277),IX( 277),IY( 277)/'MOVE', -14,  -9/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -8,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'MOVE',   0,  -9/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',   8,  -9/
      DATA IOPERA( 281),IX( 281),IY( 281)/'MOVE',  -7,  12/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -5,  11/
      DATA IOPERA( 283),IX( 283),IY( 283)/'MOVE',  -6,  12/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  -5,  10/
      DATA IOPERA( 285),IX( 285),IY( 285)/'MOVE',  12,  12/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',  10,  10/
      DATA IOPERA( 287),IX( 287),IY( 287)/'MOVE',  13,  12/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',  10,  11/
      DATA IOPERA( 289),IX( 289),IY( 289)/'MOVE', -11,  -8/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW', -13,  -9/
      DATA IOPERA( 291),IX( 291),IY( 291)/'MOVE', -11,  -8/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -9,  -9/
      DATA IOPERA( 293),IX( 293),IY( 293)/'MOVE',   4,  -8/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',   1,  -9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'MOVE',   4,  -7/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',   2,  -9/
      DATA IOPERA( 297),IX( 297),IY( 297)/'MOVE',   5,  -7/
      DATA IOPERA( 298),IX( 298),IY( 298)/'DRAW',   6,  -9/
      DATA IOPERA( 299),IX( 299),IY( 299)/'MOVE',   4,  -8/
      DATA IOPERA( 300),IX( 300),IY( 300)/'DRAW',   7,  -9/
C
      DATA IXMIND(  13)/ -14/
      DATA IXMAXD(  13)/  14/
      DATA IXDELD(  13)/  28/
      DATA ISTARD(  13)/ 255/
      DATA NUMCOO(  13)/  46/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIU2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIU3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC UPPER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER  3514--UPPER CASE N
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -3,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9,  -8/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',  -3,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   4,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',  -2,  12/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   4,  -6/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  -1,  12/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   5,  -6/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',  10,  11/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   5,  -6/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   4,  -9/
      DATA IOPERA(  12),IX(  12),IY(  12)/'MOVE',  -6,  12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -1,  12/
      DATA IOPERA(  14),IX(  14),IY(  14)/'MOVE',   7,  12/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  13,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE', -12,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -6,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -5,  12/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,  11/
      DATA IOPERA(  20),IX(  20),IY(  20)/'MOVE',  -4,  12/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,  10/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   8,  12/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  10,  11/
      DATA IOPERA(  24),IX(  24),IY(  24)/'MOVE',  12,  12/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  10,  11/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE',  -9,  -8/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW', -11,  -9/
      DATA IOPERA(  28),IX(  28),IY(  28)/'MOVE',  -9,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -7,  -9/
C
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  25/
      DATA ISTARD(  14)/   1/
      DATA NUMCOO(  14)/  29/
C
C     DEFINE CHARACTER  3515--UPPER CASE O
C
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',   1,  12/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -2,  11/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -4,   9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -6,   6/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -7,   3/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -8,  -1/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -8,  -4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -7,  -7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -6,  -8/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -4,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -1,  -9/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   2,  -8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   4,  -6/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   6,  -3/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   7,   0/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   8,   4/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   8,   7/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   7,  10/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   6,  11/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   4,  12/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   1,  12/
      DATA IOPERA(  51),IX(  51),IY(  51)/'MOVE',  -3,   9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5,   6/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -6,   3/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -7,  -1/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -7,  -5/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -6,  -7/
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE',   3,  -6/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   5,  -3/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   6,   0/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   7,   4/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   7,   8/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   6,  10/
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',   1,  12/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -1,  11/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -3,   8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -4,   6/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -5,   3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -6,  -1/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -6,  -6/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -5,  -8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -4,  -9/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  -1,  -9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   1,  -8/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   3,  -5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   4,  -3/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   5,   0/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   6,   4/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   6,   9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   5,  11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   4,  12/
C
      DATA IXMIND(  15)/ -11/
      DATA IXMAXD(  15)/  11/
      DATA IXDELD(  15)/  22/
      DATA ISTARD(  15)/  30/
      DATA NUMCOO(  15)/  51/
C
C     DEFINE CHARACTER  3516--UPPER CASE P
C
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',  -3,  12/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -9,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'MOVE',  -2,  12/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -8,  -9/
      DATA IOPERA(  85),IX(  85),IY(  85)/'MOVE',  -1,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -7,  -9/
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE',  -6,  12/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   6,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   9,  11/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  10,   9/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  10,   7/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   9,   4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   7,   2/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   3,   1/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',  -5,   1/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   8,  11/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   9,   9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   9,   7/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   8,   4/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   6,   2/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   6,  12/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   7,  11/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   8,   9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   8,   7/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   7,   4/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   5,   2/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   3,   1/
      DATA IOPERA( 108),IX( 108),IY( 108)/'MOVE', -12,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -4,  -9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -5,  12/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',  -2,  11/
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',  -4,  12/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -3,  10/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',   0,  12/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -2,  10/
      DATA IOPERA( 116),IX( 116),IY( 116)/'MOVE',   1,  12/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -2,  11/
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',  -8,  -8/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW', -11,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',  -8,  -7/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW', -10,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',  -7,  -7/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -6,  -9/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',  -8,  -8/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -5,  -9/
C
      DATA IXMIND(  16)/ -12/
      DATA IXMAXD(  16)/  11/
      DATA IXDELD(  16)/  23/
      DATA ISTARD(  16)/  81/
      DATA NUMCOO(  16)/  45/
C
C     DEFINE CHARACTER  3517--UPPER CASE Q
C
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',   1,  12/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -2,  11/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -4,   9/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -6,   6/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -7,   3/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -8,  -1/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -8,  -4/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -7,  -7/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -6,  -8/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -4,  -9/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -1,  -9/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   2,  -8/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   4,  -6/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   6,  -3/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   7,   0/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   8,   4/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   8,   7/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   7,  10/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   6,  11/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   4,  12/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   1,  12/
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',  -3,   9/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -5,   6/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -6,   3/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -7,  -1/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -7,  -5/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -6,  -7/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   3,  -6/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   5,  -3/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   6,   0/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   7,   4/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   7,   8/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   6,  10/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',   1,  12/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -1,  11/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -3,   8/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -4,   6/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -5,   3/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -6,  -1/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -6,  -6/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -5,  -8/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -4,  -9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',  -1,  -9/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   1,  -8/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   3,  -5/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   4,  -3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   5,   0/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   6,   4/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   6,   9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   5,  11/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   4,  12/
      DATA IOPERA( 177),IX( 177),IY( 177)/'MOVE',  -6,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -5,  -4/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -3,  -3/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -2,  -3/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   0,  -4/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   1,  -6/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   2, -11/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   3, -12/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   4, -12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   5, -11/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   2, -12/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   3, -13/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   4, -13/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',   1,  -6/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   1, -13/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   2, -14/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   4, -14/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   5, -11/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   5, -10/
C
      DATA IXMIND(  17)/ -11/
      DATA IXMAXD(  17)/  11/
      DATA IXDELD(  17)/  22/
      DATA ISTARD(  17)/ 126/
      DATA NUMCOO(  17)/  70/
C
C     DEFINE CHARACTER  3518--UPPER CASE R
C
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',  -3,  12/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -9,  -9/
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',  -2,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -8,  -9/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',  -1,  12/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -7,  -9/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',  -6,  12/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   5,  12/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   8,  11/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   9,   9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   9,   7/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   8,   4/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   7,   3/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   4,   2/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -4,   2/
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',   7,  11/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   8,   9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   8,   7/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   7,   4/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   6,   3/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',   5,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   6,  11/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   7,   9/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   7,   7/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   6,   4/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   4,   2/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',   0,   2/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',   2,   1/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   3,   0/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   5,  -6/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   6,  -7/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   7,  -7/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   8,  -6/
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',   5,  -7/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   6,  -8/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   7,  -8/
      DATA IOPERA( 232),IX( 232),IY( 232)/'MOVE',   3,   0/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   4,  -8/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   5,  -9/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   7,  -9/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   8,  -6/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   8,  -5/
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE', -12,  -9/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -4,  -9/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',  -5,  12/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -2,  11/
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',  -4,  12/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -3,  10/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',   0,  12/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -2,  10/
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',   1,  12/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -2,  11/
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',  -8,  -8/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW', -11,  -9/
      DATA IOPERA( 250),IX( 250),IY( 250)/'MOVE',  -8,  -7/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW', -10,  -9/
      DATA IOPERA( 252),IX( 252),IY( 252)/'MOVE',  -7,  -7/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -6,  -9/
      DATA IOPERA( 254),IX( 254),IY( 254)/'MOVE',  -8,  -8/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -5,  -9/
C
      DATA IXMIND(  18)/ -12/
      DATA IXMAXD(  18)/  12/
      DATA IXDELD(  18)/  24/
      DATA ISTARD(  18)/ 196/
      DATA NUMCOO(  18)/  60/
C
C     DEFINE CHARACTER  3519--UPPER CASE S
C
      DATA IOPERA( 256),IX( 256),IY( 256)/'MOVE',   8,  10/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   9,  10/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  10,  12/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   9,   6/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   9,   8/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   8,  10/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   7,  11/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   4,  12/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   0,  12/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',  -3,  11/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',  -5,   9/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',  -5,   6/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -4,   4/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -2,   2/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',   4,  -1/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',   5,  -3/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   5,  -6/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   4,  -8/
      DATA IOPERA( 274),IX( 274),IY( 274)/'MOVE',  -4,   6/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',  -3,   4/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   4,   0/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   5,  -2/
      DATA IOPERA( 278),IX( 278),IY( 278)/'MOVE',  -3,  11/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -4,   9/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -4,   7/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -3,   5/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',   3,   2/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',   5,   0/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   6,  -2/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',   6,  -5/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',   5,  -7/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',   4,  -8/
      DATA IOPERA( 288),IX( 288),IY( 288)/'DRAW',   1,  -9/
      DATA IOPERA( 289),IX( 289),IY( 289)/'DRAW',  -3,  -9/
      DATA IOPERA( 290),IX( 290),IY( 290)/'DRAW',  -6,  -8/
      DATA IOPERA( 291),IX( 291),IY( 291)/'DRAW',  -7,  -7/
      DATA IOPERA( 292),IX( 292),IY( 292)/'DRAW',  -8,  -5/
      DATA IOPERA( 293),IX( 293),IY( 293)/'DRAW',  -8,  -3/
      DATA IOPERA( 294),IX( 294),IY( 294)/'DRAW',  -9,  -9/
      DATA IOPERA( 295),IX( 295),IY( 295)/'DRAW',  -8,  -7/
      DATA IOPERA( 296),IX( 296),IY( 296)/'DRAW',  -7,  -7/
C
      DATA IXMIND(  19)/ -11/
      DATA IXMAXD(  19)/  12/
      DATA IXDELD(  19)/  23/
      DATA ISTARD(  19)/ 256/
      DATA NUMCOO(  19)/  41/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIU3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIU3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTIU4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX ITALIC UPPER CASE (PART 4).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER  3520--UPPER CASE T
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   2,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE',   3,  12/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -3,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE',   4,  12/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -2,  -9/
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',  -5,  12/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -7,   6/
      DATA IOPERA(   9),IX(   9),IY(   9)/'MOVE',  11,  12/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  10,   6/
      DATA IOPERA(  11),IX(  11),IY(  11)/'MOVE',  -5,  12/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  11,  12/
      DATA IOPERA(  13),IX(  13),IY(  13)/'MOVE',  -7,  -9/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   1,  -9/
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',  -4,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',  -7,   6/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -2,  12/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -6,   9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'MOVE',   0,  12/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -5,  11/
      DATA IOPERA(  21),IX(  21),IY(  21)/'MOVE',   7,  12/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  10,  11/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   8,  12/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  10,  10/
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',   9,  12/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  10,   9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'MOVE',  10,  12/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  10,   6/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',  -3,  -8/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -6,  -9/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -3,  -7/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -5,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  -2,  -7/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -1,  -9/
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',  -3,  -8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   0,  -9/
C
      DATA IXMIND(  20)/ -11/
      DATA IXMAXD(  20)/  11/
      DATA IXDELD(  20)/  22/
      DATA ISTARD(  20)/   1/
      DATA NUMCOO(  20)/  36/
C
C     DEFINE CHARACTER  3521--UPPER CASE U
C
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',  -4,  12/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -7,   1/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -8,  -3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -8,  -6/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -7,  -8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -4,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   0,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   3,  -8/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   5,  -6/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   6,  -3/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  10,  11/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',  -3,  12/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -6,   1/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -7,  -3/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -7,  -7/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -6,  -8/
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',  -2,  12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -5,   1/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -6,  -3/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -6,  -7/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -4,  -9/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',  -7,  12/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   1,  12/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',   7,  12/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  13,  12/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',  -6,  12/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -3,  11/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',  -5,  12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -4,  10/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',  -1,  12/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -3,  10/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',   0,  12/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -3,  11/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',   8,  12/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  10,  11/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  12,  12/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  10,  11/
C
      DATA IXMIND(  21)/ -12/
      DATA IXMAXD(  21)/  13/
      DATA IXDELD(  21)/  25/
      DATA ISTARD(  21)/  37/
      DATA NUMCOO(  21)/  37/
C
C     DEFINE CHARACTER  3522--UPPER CASE V
C
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',  -4,  12/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -4,  10/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -3,  -7/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -3,  -9/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',  -3,  11/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -2,  -6/
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -2,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -1,  -5/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',   9,  11/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -3,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -6,  12/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   1,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',   6,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  12,  12/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',  -5,  12/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -4,  10/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',  -1,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -2,  10/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',   0,  12/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -3,  11/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',   7,  12/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   9,  11/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',  11,  12/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   9,  11/
C
      DATA IXMIND(  22)/ -10/
      DATA IXMAXD(  22)/  10/
      DATA IXDELD(  22)/  20/
      DATA ISTARD(  22)/  74/
      DATA NUMCOO(  22)/  24/
C
C     DEFINE CHARACTER  3523--UPPER CASE W
C
      DATA IOPERA(  98),IX(  98),IY(  98)/'MOVE',  -5,  12/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -5,  10/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -7,  -7/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -7,  -9/
      DATA IOPERA( 102),IX( 102),IY( 102)/'MOVE',  -4,  11/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -6,  -6/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',  -3,  12/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -5,  -5/
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',   3,  12/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -5,  -5/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -7,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',   3,  12/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   3,  10/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   1,  -7/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   1,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',   4,  11/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   2,  -6/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',   5,  12/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   3,  -5/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  11,  11/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   3,  -5/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   1,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',  -8,  12/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   0,  12/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',   3,  12/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   5,  12/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   8,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  14,  12/
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',  -7,  12/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -4,  11/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -6,  12/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -5,  10/
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE',  -2,  12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -4,   9/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',  -1,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -4,  11/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',   9,  12/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  11,  11/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',  13,  12/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  11,  11/
C
      DATA IXMIND(  23)/ -13/
      DATA IXMAXD(  23)/  13/
      DATA IXDELD(  23)/  26/
      DATA ISTARD(  23)/  98/
      DATA NUMCOO(  23)/  40/
C
C     DEFINE CHARACTER  3524--UPPER CASE X
C
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',  -4,  12/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   2,  -9/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',  -3,  12/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   3,  -9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',  -2,  12/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   4,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE',   9,  11/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',  -9,  -8/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',  -6,  12/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   1,  12/
      DATA IOPERA( 148),IX( 148),IY( 148)/'MOVE',   6,  12/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  12,  12/
      DATA IOPERA( 150),IX( 150),IY( 150)/'MOVE', -12,  -9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -6,  -9/
      DATA IOPERA( 152),IX( 152),IY( 152)/'MOVE',  -1,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   6,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'MOVE',  -5,  12/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -3,  10/
      DATA IOPERA( 156),IX( 156),IY( 156)/'MOVE',  -1,  12/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',  -2,  10/
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',   0,  12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -2,  11/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',   7,  12/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   9,  11/
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',  11,  12/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   9,  11/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',  -9,  -8/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW', -11,  -9/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',  -9,  -8/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -7,  -9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',   2,  -8/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   0,  -9/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   2,  -7/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   1,  -9/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   3,  -7/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   5,  -9/
C
      DATA IXMIND(  24)/ -11/
      DATA IXMAXD(  24)/  11/
      DATA IXDELD(  24)/  22/
      DATA ISTARD(  24)/ 138/
      DATA NUMCOO(  24)/  36/
C
C     DEFINE CHARACTER  3525--UPPER CASE Y
C
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',  -5,  12/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -1,   2/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  -4,  -9/
      DATA IOPERA( 177),IX( 177),IY( 177)/'MOVE',  -4,  12/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   0,   2/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -3,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',  -3,  12/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   1,   2/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -2,  -9/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',  10,  11/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   1,   2/
      DATA IOPERA( 185),IX( 185),IY( 185)/'MOVE',  -7,  12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   0,  12/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   7,  12/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  13,  12/
      DATA IOPERA( 189),IX( 189),IY( 189)/'MOVE',  -7,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   1,  -9/
      DATA IOPERA( 191),IX( 191),IY( 191)/'MOVE',  -6,  12/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -4,  11/
      DATA IOPERA( 193),IX( 193),IY( 193)/'MOVE',  -2,  12/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -3,  10/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',  -1,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -4,  11/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',   8,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  10,  11/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',  12,  12/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  10,  11/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',  -3,  -8/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -6,  -9/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',  -3,  -7/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -5,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',  -2,  -7/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -1,  -9/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -3,  -8/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',   0,  -9/
C
      DATA IXMIND(  25)/ -11/
      DATA IXMAXD(  25)/  11/
      DATA IXDELD(  25)/  22/
      DATA ISTARD(  25)/ 174/
      DATA NUMCOO(  25)/  35/
C
C     DEFINE CHARACTER  3526--UPPER CASE Z
C
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',   8,  12/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW', -10,  -9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',   9,  12/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -9,  -9/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',  10,  12/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -8,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',  10,  12/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -4,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -6,   6/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE', -10,  -9/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   4,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   6,  -3/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',  -3,  12/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -6,   6/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -2,  12/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -5,   9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',   0,  12/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -4,  11/
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',   0,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   4,  -8/
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',   2,  -9/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   5,  -6/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',   3,  -9/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   6,  -3/
C
      DATA IXMIND(  26)/ -11/
      DATA IXMAXD(  26)/  11/
      DATA IXDELD(  26)/  22/
      DATA ISTARD(  26)/ 209/
      DATA NUMCOO(  26)/  24/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTIU4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTIU4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTL1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX LOWER CASE (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3101--LOWER CASE A
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -4,   2/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,   3/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -3,   3/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -3,   1/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -5,   1/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -5,   3/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -4,   4/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -2,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   2,   5/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   4,   4/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   5,   3/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   6,   1/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   6,  -6/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   7,  -8/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   8,  -9/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE',   4,   3/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   5,   1/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   5,  -6/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   6,  -8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'MOVE',   2,   5/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   3,   4/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   4,   2/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   4,  -6/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   5,  -8/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   8,  -9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   9,  -9/
      DATA IOPERA(  27),IX(  27),IY(  27)/'MOVE',   4,   0/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   3,  -1/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  -2,  -2/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  -5,  -3/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -6,  -5/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -6,  -6/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -5,  -8/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -2,  -9/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   1,  -9/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   3,  -8/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   4,  -6/
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',  -4,  -3/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -5,  -5/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -5,  -6/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -4,  -8/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',   3,  -1/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',  -1,  -2/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',  -3,  -3/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  -4,  -5/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -4,  -6/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -3,  -8/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -2,  -9/
C
      DATA IXMIND(   1)/  -9/
      DATA IXMAXD(   1)/  11/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  48/
C
C     DEFINE CHARACTER   3102--LOWER CASE B
C
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE',  -6,  12/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -6,  -9/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -5,  -8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -3,  -8/
      DATA IOPERA(  53),IX(  53),IY(  53)/'MOVE',  -5,  11/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -5,  -7/
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',  -9,  12/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -4,  12/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -4,  -8/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',  -4,   2/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -3,   4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -1,   5/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   1,   5/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   4,   4/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   6,   2/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   7,  -1/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   7,  -3/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   6,  -6/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   4,  -8/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   1,  -9/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -1,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -3,  -8/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -4,  -6/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',   5,   2/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   6,   0/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   6,  -4/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   5,  -6/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',   1,   5/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',   3,   4/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   4,   3/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   5,   0/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   5,  -4/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   4,  -7/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   3,  -8/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   1,  -9/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -8,  12/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -6,  11/
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',  -7,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -6,  10/
C
      DATA IXMIND(   2)/ -11/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  21/
      DATA ISTARD(   2)/  49/
      DATA NUMCOO(   2)/  39/
C
C     DEFINE CHARACTER   3103--LOWER CASE C
C
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   5,   1/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   5,   2/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   4,   2/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   4,   0/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   6,   0/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   6,   2/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   4,   4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   2,   5/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -1,   5/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -4,   4/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -6,   2/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -7,  -1/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -7,  -3/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -6,  -6/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -4,  -8/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -1,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   1,  -9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   4,  -8/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   6,  -6/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -5,   2/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -6,   0/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',  -6,  -4/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -5,  -6/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',  -1,   5/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -3,   4/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -4,   3/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -5,   0/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -5,  -4/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -4,  -7/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',  -3,  -8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/   9/
      DATA IXDELD(   3)/  19/
      DATA ISTARD(   3)/  88/
      DATA NUMCOO(   3)/  31/
C
C     DEFINE CHARACTER   3104--LOWER CASE D
C
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',   4,  12/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   4,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   9,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',   5,  11/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   5,  -8/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   1,  12/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   6,  12/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   6,  -9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',   4,   2/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   3,   4/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   1,   5/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -1,   5/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -4,   4/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -6,   2/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -7,  -1/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -7,  -3/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -6,  -6/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -4,  -8/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -1,  -9/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   1,  -9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   3,  -8/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   4,  -6/
      DATA IOPERA( 141),IX( 141),IY( 141)/'MOVE',  -5,   2/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -6,   0/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -6,  -4/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -5,  -6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',  -1,   5/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -3,   4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -4,   3/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -5,   0/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -5,  -4/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -4,  -7/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -3,  -8/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -1,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   2,  12/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   4,  11/
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',   3,  12/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   4,  10/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',   6,  -7/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   7,  -9/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',   6,  -8/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   8,  -9/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  11/
      DATA IXDELD(   4)/  21/
      DATA ISTARD(   4)/ 119/
      DATA NUMCOO(   4)/  42/
C
C     DEFINE CHARACTER   3105--LOWER CASE E
C
      DATA IOPERA( 161),IX( 161),IY( 161)/'MOVE',  -5,  -1/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   6,  -1/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   6,   1/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   5,   3/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   4,   4/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   1,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -1,   5/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -4,   4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -6,   2/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -7,  -1/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -7,  -3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -6,  -6/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -4,  -8/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -1,  -9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   1,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   4,  -8/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',   5,   0/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   5,   1/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   4,   3/
      DATA IOPERA( 181),IX( 181),IY( 181)/'MOVE',  -5,   2/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -6,   0/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -6,  -4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -5,  -6/
      DATA IOPERA( 185),IX( 185),IY( 185)/'MOVE',   4,  -1/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   4,   2/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   3,   4/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   1,   5/
      DATA IOPERA( 189),IX( 189),IY( 189)/'MOVE',  -1,   5/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -3,   4/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -4,   3/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',  -5,   0/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -5,  -4/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',  -4,  -7/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -3,  -8/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -1,  -9/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/   9/
      DATA IXDELD(   5)/  19/
      DATA ISTARD(   5)/ 161/
      DATA NUMCOO(   5)/  36/
C
C     DEFINE CHARACTER   3106--LOWER CASE F
C
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',   5,  10/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   5,  11/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   4,  11/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   4,   9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,   9/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   6,  11/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   5,  12/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   2,  12/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   0,  11/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -1,  10/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -2,   7/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -2,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',   0,  10/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -1,   7/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -1,  -8/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',   2,  12/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   1,  11/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   0,   9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   0,  -9/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -5,   5/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   4,   5/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -5,  -9/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   3,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -2,  -8/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -4,  -9/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -2,  -7/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -3,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   0,  -7/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   1,  -9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   0,  -8/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   2,  -9/
C
      DATA IXMIND(   6)/  -7/
      DATA IXMAXD(   6)/   7/
      DATA IXDELD(   6)/  14/
      DATA ISTARD(   6)/ 197/
      DATA NUMCOO(   6)/  31/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTL1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTL2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX LOWER CASE (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3107--LOWER CASE G
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   6,   4/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   7,   3/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   8,   4/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   7,   5/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   6,   5/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   4,   4/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   3,   3/
      DATA IOPERA(   8),IX(   8),IY(   8)/'MOVE',  -1,   5/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -3,   4/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -4,   3/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -5,   1/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -5,  -1/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -4,  -3/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -3,  -4/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',  -1,  -5/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  -5/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   3,  -4/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   4,  -3/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   5,  -1/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   5,   1/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   4,   3/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   3,   4/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   1,   5/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -1,   5/
      DATA IOPERA(  25),IX(  25),IY(  25)/'MOVE',  -3,   3/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -4,   1/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -4,  -1/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -3,  -3/
      DATA IOPERA(  29),IX(  29),IY(  29)/'MOVE',   3,  -3/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   4,  -1/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   4,   1/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   3,   3/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  -1,   5/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -2,   4/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -3,   2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -3,  -2/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -2,  -4/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -1,  -5/
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',   1,  -5/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   2,  -4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   3,  -2/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   3,   2/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   2,   4/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   1,   5/
      DATA IOPERA(  45),IX(  45),IY(  45)/'MOVE',  -4,  -3/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',  -5,  -4/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -6,  -6/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -6,  -7/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -5,  -9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -4, -10/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -1, -11/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   3, -11/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   6, -12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   7, -13/
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',  -4,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -1, -10/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   3, -10/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   6, -11/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE',  -6,  -7/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -5,  -8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   3,  -9/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   6, -10/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   7, -12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   7, -13/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   6, -15/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   3, -16/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -3, -16/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -6, -15/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -7, -13/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -7, -12/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -6, -10/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -3,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',  -3, -16/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -5, -15/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -6, -13/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -6, -12/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5, -10/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -3,  -9/
C
      DATA IXMIND(   7)/  -9/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  19/
      DATA ISTARD(   7)/   1/
      DATA NUMCOO(   7)/  79/
C
C     DEFINE CHARACTER   3108--LOWER CASE H
C
      DATA IOPERA(  80),IX(  80),IY(  80)/'MOVE',  -6,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -6,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -5,  11/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -5,  -8/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -9,  12/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -4,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -4,  -9/
      DATA IOPERA(  87),IX(  87),IY(  87)/'MOVE',  -4,   1/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -3,   3/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -2,   4/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   0,   5/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   3,   5/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   5,   4/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   6,   3/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   7,   0/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   7,  -9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   5,   3/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   6,   0/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   6,  -8/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',   3,   5/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   4,   4/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   5,   1/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   5,  -9/
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',  -9,  -9/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -1,  -9/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',   2,  -9/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  10,  -9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -8,  12/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -6,  11/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',  -7,  12/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -6,  10/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',  -6,  -8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -8,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -6,  -7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -7,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',  -4,  -7/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -3,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',  -4,  -8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -2,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',   5,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   3,  -9/
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',   5,  -7/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   4,  -9/
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',   7,  -7/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   8,  -9/
      DATA IOPERA( 125),IX( 125),IY( 125)/'MOVE',   7,  -8/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   9,  -9/
C
      DATA IXMIND(   8)/ -11/
      DATA IXMAXD(   8)/  12/
      DATA IXDELD(   8)/  23/
      DATA ISTARD(   8)/  80/
      DATA NUMCOO(   8)/  47/
C
C     DEFINE CHARACTER   3109--LOWER CASE I
C
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',  -1,  12/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -1,  10/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   1,  10/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   1,  12/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -1,  12/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   0,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   0,  10/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',  -1,  11/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   1,  11/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',  -1,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -1,  -9/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',   0,   4/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   0,  -8/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',  -4,   5/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   1,   5/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   1,  -9/
      DATA IOPERA( 143),IX( 143),IY( 143)/'MOVE',  -4,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   4,  -9/
      DATA IOPERA( 145),IX( 145),IY( 145)/'MOVE',  -3,   5/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',  -1,   4/
      DATA IOPERA( 147),IX( 147),IY( 147)/'MOVE',  -2,   5/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -1,   3/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -1,  -8/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -3,  -9/
      DATA IOPERA( 151),IX( 151),IY( 151)/'MOVE',  -1,  -7/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -2,  -9/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   1,  -7/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   2,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'MOVE',   1,  -8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   3,  -9/
C
      DATA IXMIND(   9)/  -6/
      DATA IXMAXD(   9)/   6/
      DATA IXDELD(   9)/  12/
      DATA ISTARD(   9)/ 127/
      DATA NUMCOO(   9)/  30/
C
C     DEFINE CHARACTER   3110--LOWER CASE J
C
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',   0,  12/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   0,  10/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   2,  10/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   2,  12/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   0,  12/
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',   1,  12/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   1,  10/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',   0,  11/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   2,  11/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   0,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   0, -12/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -1, -15/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -2, -16/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   1,   4/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   1, -11/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   0, -14/
      DATA IOPERA( 173),IX( 173),IY( 173)/'MOVE',  -3,   5/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   2,   5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   2, -11/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   1, -14/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   0, -15/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -2, -16/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -5, -16/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -6, -15/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -6, -13/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -4, -13/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -4, -15/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -5, -15/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -5, -14/
      DATA IOPERA( 186),IX( 186),IY( 186)/'MOVE',  -2,   5/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   0,   4/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',  -1,   5/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   0,   3/
C
      DATA IXMIND(  10)/  -7/
      DATA IXMAXD(  10)/   6/
      DATA IXDELD(  10)/  13/
      DATA ISTARD(  10)/ 157/
      DATA NUMCOO(  10)/  33/
C
C     DEFINE CHARACTER   3111--LOWER CASE K
C
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -6,  12/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -6,  -9/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',  -5,  11/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -5,  -8/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',  -9,  12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -4,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -4,  -9/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',   5,   4/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -4,  -5/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',   0,  -1/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   7,  -9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   0,  -2/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   6,  -9/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',  -1,  -2/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   5,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',   2,   5/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   9,   5/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -9,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -1,  -9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',   2,  -9/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   9,  -9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',  -8,  12/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -6,  11/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',  -7,  12/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',  -6,  10/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',   3,   5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   5,   4/
      DATA IOPERA( 217),IX( 217),IY( 217)/'MOVE',   8,   5/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   5,   4/
      DATA IOPERA( 219),IX( 219),IY( 219)/'MOVE',  -6,  -8/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -8,  -9/
      DATA IOPERA( 221),IX( 221),IY( 221)/'MOVE',  -6,  -7/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -7,  -9/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -4,  -7/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -3,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'MOVE',  -4,  -8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -2,  -9/
      DATA IOPERA( 227),IX( 227),IY( 227)/'MOVE',   5,  -7/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   3,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',   4,  -7/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   8,  -9/
C
      DATA IXMIND(  11)/ -11/
      DATA IXMAXD(  11)/  11/
      DATA IXDELD(  11)/  22/
      DATA ISTARD(  11)/ 190/
      DATA NUMCOO(  11)/  41/
C
C     DEFINE CHARACTER   3112--LOWER CASE L
C
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -1,  12/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -1,  -9/
      DATA IOPERA( 233),IX( 233),IY( 233)/'MOVE',   0,  11/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   0,  -8/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',  -4,  12/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   1,  12/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   1,  -9/
      DATA IOPERA( 238),IX( 238),IY( 238)/'MOVE',  -4,  -9/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   4,  -9/
      DATA IOPERA( 240),IX( 240),IY( 240)/'MOVE',  -3,  12/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -1,  11/
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',  -2,  12/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -1,  10/
      DATA IOPERA( 244),IX( 244),IY( 244)/'MOVE',  -1,  -8/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -3,  -9/
      DATA IOPERA( 246),IX( 246),IY( 246)/'MOVE',  -1,  -7/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',  -2,  -9/
      DATA IOPERA( 248),IX( 248),IY( 248)/'MOVE',   1,  -7/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   2,  -9/
      DATA IOPERA( 250),IX( 250),IY( 250)/'MOVE',   1,  -8/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',   3,  -9/
C
      DATA IXMIND(  12)/  -6/
      DATA IXMAXD(  12)/   6/
      DATA IXDELD(  12)/  12/
      DATA ISTARD(  12)/ 231/
      DATA NUMCOO(  12)/  21/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTL2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTL3(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX LOWER CASE (PART 3).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3113--LOWER CASE M
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE', -12,   5/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW', -12,  -9/
      DATA IOPERA(   3),IX(   3),IY(   3)/'MOVE', -11,   4/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW', -11,  -8/
      DATA IOPERA(   5),IX(   5),IY(   5)/'MOVE', -15,   5/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW', -10,   5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW', -10,  -9/
      DATA IOPERA(   8),IX(   8),IY(   8)/'MOVE', -10,   1/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -9,   3/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -8,   4/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -6,   5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -3,   5/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -1,   4/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   0,   3/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   1,   0/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -1,   3/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',   0,   0/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',   0,  -8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'MOVE',  -3,   5/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,   4/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -1,   1/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -1,  -9/
      DATA IOPERA(  24),IX(  24),IY(  24)/'MOVE',   1,   1/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   2,   3/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   3,   4/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   5,   5/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   8,   5/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',  10,   4/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',  11,   3/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  12,   0/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  12,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',  10,   3/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  11,   0/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  11,  -8/
      DATA IOPERA(  36),IX(  36),IY(  36)/'MOVE',   8,   5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   9,   4/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  10,   1/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  10,  -9/
      DATA IOPERA(  40),IX(  40),IY(  40)/'MOVE', -15,  -9/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -7,  -9/
      DATA IOPERA(  42),IX(  42),IY(  42)/'MOVE',  -4,  -9/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   4,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',   7,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',  15,  -9/
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE', -14,   5/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW', -12,   4/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE', -13,   5/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW', -12,   3/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE', -12,  -8/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW', -14,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE', -12,  -7/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW', -13,  -9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'MOVE', -10,  -7/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -9,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE', -10,  -8/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',  -8,  -9/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',  -1,  -8/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -3,  -9/
      DATA IOPERA(  60),IX(  60),IY(  60)/'MOVE',  -1,  -7/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'MOVE',   1,  -7/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   2,  -9/
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',   1,  -8/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   3,  -9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',  10,  -8/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   8,  -9/
      DATA IOPERA(  68),IX(  68),IY(  68)/'MOVE',  10,  -7/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   9,  -9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',  12,  -7/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  13,  -9/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',  12,  -8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  14,  -9/
C
      DATA IXMIND(  13)/ -17/
      DATA IXMAXD(  13)/  17/
      DATA IXDELD(  13)/  34/
      DATA ISTARD(  13)/   1/
      DATA NUMCOO(  13)/  73/
C
C     DEFINE CHARACTER   3114--LOWER CASE N
C
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',  -6,   5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -6,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'MOVE',  -5,   4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -5,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'MOVE',  -9,   5/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -4,   5/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -4,  -9/
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',  -4,   1/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',  -3,   3/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -2,   4/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   0,   5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   3,   5/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   5,   4/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   6,   3/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   7,   0/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   7,  -9/
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',   5,   3/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   6,   0/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   6,  -8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',   3,   5/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   4,   4/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   5,   1/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   5,  -9/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',  -9,  -9/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -1,  -9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',   2,  -9/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  10,  -9/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',  -8,   5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -6,   4/
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',  -7,   5/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -6,   3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -6,  -8/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -8,  -9/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',  -6,  -7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',  -7,  -9/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',  -4,  -7/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',  -3,  -9/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',  -4,  -8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -2,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',   5,  -8/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   3,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',   5,  -7/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   4,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',   7,  -7/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   8,  -9/
      DATA IOPERA( 119),IX( 119),IY( 119)/'MOVE',   7,  -8/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   9,  -9/
C
      DATA IXMIND(  14)/ -11/
      DATA IXMAXD(  14)/  12/
      DATA IXDELD(  14)/  23/
      DATA ISTARD(  14)/  74/
      DATA NUMCOO(  14)/  47/
C
C     DEFINE CHARACTER   3115--LOWER CASE O
C
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',  -1,   5/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -4,   4/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -6,   2/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -7,  -1/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -7,  -3/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -6,  -6/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -4,  -8/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -1,  -9/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   1,  -9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   4,  -8/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   6,  -6/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   7,  -3/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   7,  -1/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   6,   2/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   4,   4/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   1,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -1,   5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',  -5,   2/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -6,   0/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -6,  -4/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -5,  -6/
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',   5,  -6/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   6,  -4/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   6,   0/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   5,   2/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',  -1,   5/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',  -3,   4/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',  -4,   3/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -5,   0/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -5,  -4/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -4,  -7/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -3,  -8/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -1,  -9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'MOVE',   1,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   3,  -8/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   4,  -7/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   5,  -4/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   5,   0/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   4,   3/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   3,   4/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   1,   5/
C
      DATA IXMIND(  15)/ -10/
      DATA IXMAXD(  15)/  10/
      DATA IXDELD(  15)/  20/
      DATA ISTARD(  15)/ 121/
      DATA NUMCOO(  15)/  41/
C
C     DEFINE CHARACTER   3116--LOWER CASE P
C
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',  -6,   5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6, -16/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',  -5,   4/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -5, -15/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',  -9,   5/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -4,   5/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -4, -16/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',  -4,   2/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -3,   4/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -1,   5/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   1,   5/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   4,   4/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   6,   2/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   7,  -1/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   7,  -3/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   6,  -6/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   4,  -8/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   1,  -9/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -1,  -9/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -3,  -8/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -4,  -6/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',   5,   2/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   6,   0/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   6,  -4/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   5,  -6/
      DATA IOPERA( 187),IX( 187),IY( 187)/'MOVE',   1,   5/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   3,   4/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   4,   3/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   5,   0/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   5,  -4/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   4,  -7/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   3,  -8/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   1,  -9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'MOVE',  -9, -16/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -1, -16/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',  -8,   5/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -6,   4/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',  -7,   5/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -6,   3/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',  -6, -15/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -8, -16/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',  -6, -14/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -7, -16/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',  -4, -14/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -3, -16/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -4, -15/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -2, -16/
C
      DATA IXMIND(  16)/ -11/
      DATA IXMAXD(  16)/  10/
      DATA IXDELD(  16)/  21/
      DATA ISTARD(  16)/ 162/
      DATA NUMCOO(  16)/  47/
C
C     DEFINE CHARACTER   3117--LOWER CASE Q
C
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',   4,   4/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   4, -16/
      DATA IOPERA( 211),IX( 211),IY( 211)/'MOVE',   5,   3/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   5, -15/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',   3,   4/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   5,   4/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   6,   5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   6, -16/
      DATA IOPERA( 217),IX( 217),IY( 217)/'MOVE',   4,   2/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   3,   4/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',   1,   5/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -1,   5/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -4,   4/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -6,   2/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -7,  -1/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -7,  -3/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -6,  -6/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -4,  -8/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -1,  -9/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   1,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   3,  -8/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   4,  -6/
      DATA IOPERA( 231),IX( 231),IY( 231)/'MOVE',  -5,   2/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',  -6,   0/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',  -6,  -4/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',  -5,  -6/
      DATA IOPERA( 235),IX( 235),IY( 235)/'MOVE',  -1,   5/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  -3,   4/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',  -4,   3/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -5,   0/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',  -5,  -4/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',  -4,  -7/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',  -3,  -8/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',  -1,  -9/
      DATA IOPERA( 243),IX( 243),IY( 243)/'MOVE',   1, -16/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   9, -16/
      DATA IOPERA( 245),IX( 245),IY( 245)/'MOVE',   4, -15/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   2, -16/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',   4, -14/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   3, -16/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',   6, -14/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   7, -16/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',   6, -15/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',   8, -16/
C
      DATA IXMIND(  17)/ -10/
      DATA IXMAXD(  17)/  10/
      DATA IXDELD(  17)/  20/
      DATA ISTARD(  17)/ 209/
      DATA NUMCOO(  17)/  44/
C
C     DEFINE CHARACTER   3118--LOWER CASE R
C
      DATA IOPERA( 253),IX( 253),IY( 253)/'MOVE',  -4,   5/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -4,  -9/
      DATA IOPERA( 255),IX( 255),IY( 255)/'MOVE',  -3,   4/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',  -3,  -8/
      DATA IOPERA( 257),IX( 257),IY( 257)/'MOVE',  -7,   5/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',  -2,   5/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',  -2,  -9/
      DATA IOPERA( 260),IX( 260),IY( 260)/'MOVE',   5,   3/
      DATA IOPERA( 261),IX( 261),IY( 261)/'DRAW',   5,   4/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',   4,   4/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',   4,   2/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   6,   2/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   6,   4/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   5,   5/
      DATA IOPERA( 267),IX( 267),IY( 267)/'DRAW',   3,   5/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',   1,   4/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -1,   2/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -2,  -1/
      DATA IOPERA( 271),IX( 271),IY( 271)/'MOVE',  -7,  -9/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   1,  -9/
      DATA IOPERA( 273),IX( 273),IY( 273)/'MOVE',  -6,   5/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',  -4,   4/
      DATA IOPERA( 275),IX( 275),IY( 275)/'MOVE',  -5,   5/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',  -4,   3/
      DATA IOPERA( 277),IX( 277),IY( 277)/'MOVE',  -4,  -8/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',  -6,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'MOVE',  -4,  -7/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -5,  -9/
      DATA IOPERA( 281),IX( 281),IY( 281)/'MOVE',  -2,  -7/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -1,  -9/
      DATA IOPERA( 283),IX( 283),IY( 283)/'MOVE',  -2,  -8/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',   0,  -9/
C
      DATA IXMIND(  18)/  -9/
      DATA IXMAXD(  18)/   8/
      DATA IXDELD(  18)/  17/
      DATA ISTARD(  18)/ 253/
      DATA NUMCOO(  18)/  32/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTL3--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTL4(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX LOWER CASE (PART 4).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3119--LOWER CASE S
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   5,   3/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   6,   5/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   6,   1/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   5,   3/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   4,   4/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   2,   5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -2,   5/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -4,   4/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -5,   3/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -5,   1/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -4,  -1/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -2,  -2/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   3,  -3/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   5,  -4/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   6,  -7/
      DATA IOPERA(  16),IX(  16),IY(  16)/'MOVE',  -4,   4/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -5,   1/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -4,   0/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -2,  -1/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   3,  -2/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   5,  -3/
      DATA IOPERA(  22),IX(  22),IY(  22)/'MOVE',   6,  -4/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   5,  -8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'MOVE',  -5,   3/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -4,   1/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -2,   0/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   3,  -1/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   5,  -2/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   6,  -4/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   6,  -7/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   5,  -8/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   3,  -9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -1,  -9/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -3,  -8/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -4,  -7/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -5,  -5/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -5,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -4,  -7/
C
      DATA IXMIND(  19)/  -8/
      DATA IXMAXD(  19)/   9/
      DATA IXDELD(  19)/  17/
      DATA ISTARD(  19)/   1/
      DATA NUMCOO(  19)/  38/
C
C     DEFINE CHARACTER   3120--LOWER CASE T
C
      DATA IOPERA(  39),IX(  39),IY(  39)/'MOVE',  -2,  10/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -2,  -4/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -1,  -7/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   0,  -8/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   2,  -9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   4,  -9/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   6,  -8/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   7,  -6/
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',  -1,  10/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -1,  -5/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   0,  -7/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',  -2,  10/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   0,  12/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   0,  -5/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   1,  -8/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   2,  -9/
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',  -5,   5/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   4,   5/
C
      DATA IXMIND(  20)/  -7/
      DATA IXMAXD(  20)/   8/
      DATA IXDELD(  20)/  15/
      DATA ISTARD(  20)/  39/
      DATA NUMCOO(  20)/  18/
C
C     DEFINE CHARACTER   3121--LOWER CASE U
C
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE',  -6,   5/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -6,  -4/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',  -5,  -7/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -4,  -8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',  -2,  -9/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   1,  -9/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   3,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   4,  -7/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',   5,  -5/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',  -5,   4/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -5,  -5/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -4,  -7/
      DATA IOPERA(  69),IX(  69),IY(  69)/'MOVE',  -9,   5/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',  -4,   5/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -4,  -5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -3,  -8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -2,  -9/
      DATA IOPERA(  74),IX(  74),IY(  74)/'MOVE',   5,   5/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   5,  -9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  10,  -9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE',   6,   4/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   6,  -8/
      DATA IOPERA(  79),IX(  79),IY(  79)/'MOVE',   2,   5/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',   7,   5/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   7,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'MOVE',  -8,   5/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -6,   4/
      DATA IOPERA(  84),IX(  84),IY(  84)/'MOVE',  -7,   5/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -6,   3/
      DATA IOPERA(  86),IX(  86),IY(  86)/'MOVE',   7,  -7/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   8,  -9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   7,  -8/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   9,  -9/
C
      DATA IXMIND(  21)/ -11/
      DATA IXMAXD(  21)/  12/
      DATA IXDELD(  21)/  23/
      DATA ISTARD(  21)/  57/
      DATA NUMCOO(  21)/  33/
C
C     DEFINE CHARACTER   3122--LOWER CASE V
C
      DATA IOPERA(  90),IX(  90),IY(  90)/'MOVE',  -6,   5/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   0,  -9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'MOVE',  -5,   5/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   0,  -7/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',  -4,   5/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   1,  -7/
      DATA IOPERA(  96),IX(  96),IY(  96)/'MOVE',   6,   4/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',   1,  -7/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   0,  -9/
      DATA IOPERA(  99),IX(  99),IY(  99)/'MOVE',  -8,   5/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',  -1,   5/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   2,   5/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   8,   5/
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',  -7,   5/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -4,   3/
      DATA IOPERA( 105),IX( 105),IY( 105)/'MOVE',  -2,   5/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -4,   4/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',   4,   5/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   6,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',   7,   5/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   6,   4/
C
      DATA IXMIND(  22)/  -9/
      DATA IXMAXD(  22)/   9/
      DATA IXDELD(  22)/  18/
      DATA ISTARD(  22)/  90/
      DATA NUMCOO(  22)/  21/
C
C     DEFINE CHARACTER   3123--LOWER CASE W
C
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',  -8,   5/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -4,  -9/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -7,   5/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -4,  -6/
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',  -6,   5/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -3,  -6/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',   0,   5/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',  -3,  -6/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',  -4,  -9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   0,   5/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   4,  -9/
      DATA IOPERA( 122),IX( 122),IY( 122)/'MOVE',   1,   5/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   4,  -6/
      DATA IOPERA( 124),IX( 124),IY( 124)/'MOVE',   0,   5/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',   2,   5/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',   5,  -6/
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',   8,   4/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   5,  -6/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   4,  -9/
      DATA IOPERA( 130),IX( 130),IY( 130)/'MOVE', -11,   5/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -3,   5/
      DATA IOPERA( 132),IX( 132),IY( 132)/'MOVE',   5,   5/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  11,   5/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE', -10,   5/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -7,   4/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',  -4,   5/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -6,   4/
      DATA IOPERA( 138),IX( 138),IY( 138)/'MOVE',   6,   5/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   8,   4/
      DATA IOPERA( 140),IX( 140),IY( 140)/'MOVE',  10,   5/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   8,   4/
C
      DATA IXMIND(  23)/ -12/
      DATA IXMAXD(  23)/  12/
      DATA IXDELD(  23)/  24/
      DATA ISTARD(  23)/ 111/
      DATA NUMCOO(  23)/  31/
C
C     DEFINE CHARACTER   3124--LOWER CASE X
C
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',  -6,   5/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   4,  -9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'MOVE',  -5,   5/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   5,  -9/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',  -4,   5/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   6,  -9/
      DATA IOPERA( 148),IX( 148),IY( 148)/'MOVE',   5,   4/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',  -5,  -8/
      DATA IOPERA( 150),IX( 150),IY( 150)/'MOVE',  -8,   5/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -1,   5/
      DATA IOPERA( 152),IX( 152),IY( 152)/'MOVE',   2,   5/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   8,   5/
      DATA IOPERA( 154),IX( 154),IY( 154)/'MOVE',  -8,  -9/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',  -2,  -9/
      DATA IOPERA( 156),IX( 156),IY( 156)/'MOVE',   1,  -9/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   8,  -9/
      DATA IOPERA( 158),IX( 158),IY( 158)/'MOVE',  -7,   5/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -5,   4/
      DATA IOPERA( 160),IX( 160),IY( 160)/'MOVE',  -2,   5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -4,   4/
      DATA IOPERA( 162),IX( 162),IY( 162)/'MOVE',   3,   5/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   5,   4/
      DATA IOPERA( 164),IX( 164),IY( 164)/'MOVE',   7,   5/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   5,   4/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',  -5,  -8/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -7,  -9/
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',  -5,  -8/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -3,  -9/
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',   4,  -8/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',   2,  -9/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   5,  -8/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   7,  -9/
C
      DATA IXMIND(  24)/ -10/
      DATA IXMAXD(  24)/  10/
      DATA IXDELD(  24)/  20/
      DATA ISTARD(  24)/ 142/
      DATA NUMCOO(  24)/  32/
C
C     DEFINE CHARACTER   3125--LOWER CASE Y
C
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',  -6,   5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   0,  -9/
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',  -5,   5/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   0,  -7/
      DATA IOPERA( 178),IX( 178),IY( 178)/'MOVE',  -4,   5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   1,  -7/
      DATA IOPERA( 180),IX( 180),IY( 180)/'MOVE',   6,   4/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   1,  -7/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -2, -13/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -4, -15/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -6, -16/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -8, -16/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -9, -15/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -9, -13/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -7, -13/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -7, -15/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -8, -15/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',  -8, -14/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',  -8,   5/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',  -1,   5/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   2,   5/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',   8,   5/
      DATA IOPERA( 196),IX( 196),IY( 196)/'MOVE',  -7,   5/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -4,   3/
      DATA IOPERA( 198),IX( 198),IY( 198)/'MOVE',  -2,   5/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -4,   4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'MOVE',   4,   5/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   6,   4/
      DATA IOPERA( 202),IX( 202),IY( 202)/'MOVE',   7,   5/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   6,   4/
C
      DATA IXMIND(  25)/ -10/
      DATA IXMAXD(  25)/   9/
      DATA IXDELD(  25)/  19/
      DATA ISTARD(  25)/ 174/
      DATA NUMCOO(  25)/  30/
C
C     DEFINE CHARACTER   3126--LOWER CASE Z
C
      DATA IOPERA( 204),IX( 204),IY( 204)/'MOVE',   4,   5/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  -6,  -9/
      DATA IOPERA( 206),IX( 206),IY( 206)/'MOVE',   5,   5/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -5,  -9/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',   6,   5/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -4,  -9/
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',   6,   5/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -6,   5/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',  -6,   1/
      DATA IOPERA( 213),IX( 213),IY( 213)/'MOVE',  -6,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   6,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   6,  -5/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -5,   5/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -6,   1/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -4,   5/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -6,   2/
      DATA IOPERA( 220),IX( 220),IY( 220)/'MOVE',  -3,   5/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -6,   3/
      DATA IOPERA( 222),IX( 222),IY( 222)/'MOVE',  -1,   5/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -6,   4/
      DATA IOPERA( 224),IX( 224),IY( 224)/'MOVE',   1,  -9/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   6,  -8/
      DATA IOPERA( 226),IX( 226),IY( 226)/'MOVE',   3,  -9/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   6,  -7/
      DATA IOPERA( 228),IX( 228),IY( 228)/'MOVE',   4,  -9/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   6,  -6/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',   5,  -9/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   6,  -5/
C
      DATA IXMIND(  26)/  -9/
      DATA IXMAXD(  26)/   9/
      DATA IXDELD(  26)/  18/
      DATA ISTARD(  26)/ 204/
      DATA NUMCOO(  26)/  28/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTL4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTL4--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTN1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX NUMERIC (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3200--0
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',  -1,  12/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -4,  11/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -6,   8/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',  -7,   3/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  -7,   0/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -6,  -5/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',  -4,  -8/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',  -1,  -9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   1,  -9/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   4,  -8/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   6,  -5/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   7,   0/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   7,   3/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   6,   8/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   4,  11/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  12/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -1,  12/
      DATA IOPERA(  18),IX(  18),IY(  18)/'MOVE',  -4,  10/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -5,   8/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -6,   4/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -6,  -1/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -5,  -5/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -4,  -7/
      DATA IOPERA(  24),IX(  24),IY(  24)/'MOVE',   4,  -7/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   5,  -5/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   6,  -1/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   6,   4/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   5,   8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   4,  10/
      DATA IOPERA(  30),IX(  30),IY(  30)/'MOVE',  -1,  12/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',  -3,  11/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -4,   9/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -5,   4/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -5,  -1/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -4,  -6/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -3,  -8/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -1,  -9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'MOVE',   1,  -9/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   3,  -8/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   4,  -6/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   5,  -1/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   5,   4/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   4,   9/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   3,  11/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   1,  12/
C
      DATA IXMIND(   1)/ -10/
      DATA IXMAXD(   1)/  10/
      DATA IXDELD(   1)/  20/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  45/
C
C     DEFINE CHARACTER   3201--1
C
      DATA IOPERA(  46),IX(  46),IY(  46)/'MOVE',  -1,  10/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',  -1,  -9/
      DATA IOPERA(  48),IX(  48),IY(  48)/'MOVE',   0,  10/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   0,  -8/
      DATA IOPERA(  50),IX(  50),IY(  50)/'MOVE',   1,  12/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   1,  -9/
      DATA IOPERA(  52),IX(  52),IY(  52)/'MOVE',   1,  12/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -2,   9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -4,   8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',  -5,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   5,  -9/
      DATA IOPERA(  57),IX(  57),IY(  57)/'MOVE',  -1,  -8/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',  -3,  -9/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE',  -1,  -7/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',  -2,  -9/
      DATA IOPERA(  61),IX(  61),IY(  61)/'MOVE',   1,  -7/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   2,  -9/
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',   1,  -8/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   3,  -9/
C
      DATA IXMIND(   2)/ -10/
      DATA IXMAXD(   2)/  10/
      DATA IXDELD(   2)/  20/
      DATA ISTARD(   2)/  46/
      DATA NUMCOO(   2)/  19/
C
C     DEFINE CHARACTER   3202--2
C
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -6,   8/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -6,   7/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',  -5,   7/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',  -5,   8/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',  -6,   8/
      DATA IOPERA(  70),IX(  70),IY(  70)/'MOVE',  -6,   9/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',  -5,   9/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -4,   8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',  -4,   7/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',  -5,   6/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -6,   6/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -7,   7/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -7,   8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -6,  10/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -5,  11/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -2,  12/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',   2,  12/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   5,  11/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   6,  10/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   7,   8/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   7,   6/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   6,   4/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   3,   2/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',  -2,   0/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -4,  -1/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',  -6,  -3/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',  -7,  -6/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -7,  -9/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',   5,  10/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   6,   8/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   6,   6/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   5,   4/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',   2,  12/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   4,  11/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   5,   8/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   5,   6/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',   4,   4/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   2,   2/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -2,   0/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',  -7,  -7/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -6,  -6/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',  -4,  -6/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   1,  -7/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   5,  -7/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   7,  -6/
      DATA IOPERA( 110),IX( 110),IY( 110)/'MOVE',  -4,  -6/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   1,  -8/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',   5,  -8/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',   6,  -7/
      DATA IOPERA( 114),IX( 114),IY( 114)/'MOVE',  -4,  -6/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   1,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   5,  -9/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   6,  -8/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   7,  -6/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   7,  -4/
C
      DATA IXMIND(   3)/ -10/
      DATA IXMAXD(   3)/  10/
      DATA IXDELD(   3)/  20/
      DATA ISTARD(   3)/  65/
      DATA NUMCOO(   3)/  55/
C
C     DEFINE CHARACTER   3203--3
C
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',  -6,   8/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',  -6,   7/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -5,   7/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -5,   8/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -6,   8/
      DATA IOPERA( 125),IX( 125),IY( 125)/'MOVE',  -6,   9/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -5,   9/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -4,   8/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -4,   7/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -5,   6/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -6,   6/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -7,   7/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -7,   8/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -6,  10/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -5,  11/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -2,  12/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   2,  12/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   5,  11/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   6,   9/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   6,   6/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   5,   4/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   2,   3/
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',   4,  11/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   5,   9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   5,   6/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   4,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',   1,  12/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   3,  11/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   4,   9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   4,   6/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   3,   4/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   1,   3/
      DATA IOPERA( 152),IX( 152),IY( 152)/'MOVE',  -1,   3/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   2,   3/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   4,   2/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   6,   0/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   7,  -2/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   7,  -5/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',   6,  -7/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',   5,  -8/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   2,  -9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -2,  -9/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -5,  -8/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',  -6,  -7/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',  -7,  -5/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',  -7,  -4/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',  -6,  -3/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',  -5,  -3/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',  -4,  -4/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',  -4,  -5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -5,  -6/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -6,  -6/
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   5,   0/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   6,  -2/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   6,  -5/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   5,  -7/
      DATA IOPERA( 176),IX( 176),IY( 176)/'MOVE',   1,   3/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   3,   2/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   4,   1/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   5,  -2/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   5,  -5/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,  -8/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   2,  -9/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',  -6,  -4/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -6,  -5/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -5,  -5/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -5,  -4/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -6,  -4/
C
      DATA IXMIND(   4)/ -10/
      DATA IXMAXD(   4)/  10/
      DATA IXDELD(   4)/  20/
      DATA ISTARD(   4)/ 120/
      DATA NUMCOO(   4)/  68/
C
C     DEFINE CHARACTER   3204--4
C
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',   1,   9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   1,  -9/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',   2,  10/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   2,  -8/
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',   3,  12/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   3,  -9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'MOVE',   3,  12/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -8,  -3/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',   8,  -3/
      DATA IOPERA( 197),IX( 197),IY( 197)/'MOVE',  -2,  -9/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   6,  -9/
      DATA IOPERA( 199),IX( 199),IY( 199)/'MOVE',   1,  -8/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -1,  -9/
      DATA IOPERA( 201),IX( 201),IY( 201)/'MOVE',   1,  -7/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   0,  -9/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',   3,  -7/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   4,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',   3,  -8/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   5,  -9/
C
      DATA IXMIND(   5)/ -10/
      DATA IXMAXD(   5)/  10/
      DATA IXDELD(   5)/  20/
      DATA ISTARD(   5)/ 188/
      DATA NUMCOO(   5)/  19/
C
C     DEFINE CHARACTER   3205--5
C
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -5,  12/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -7,   2/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -5,   4/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -2,   5/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   1,   5/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   4,   4/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   6,   2/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   7,  -1/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   7,  -3/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   6,  -6/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   4,  -8/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',   1,  -9/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -2,  -9/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -5,  -8/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -6,  -7/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -7,  -5/
      DATA IOPERA( 223),IX( 223),IY( 223)/'DRAW',  -7,  -4/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -6,  -3/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -5,  -3/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -4,  -4/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -4,  -5/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -5,  -6/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',  -6,  -6/
      DATA IOPERA( 230),IX( 230),IY( 230)/'MOVE',   5,   2/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   6,   0/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   6,  -4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   5,  -6/
      DATA IOPERA( 234),IX( 234),IY( 234)/'MOVE',   1,   5/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   3,   4/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',   4,   3/
      DATA IOPERA( 237),IX( 237),IY( 237)/'DRAW',   5,   0/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',   5,  -4/
      DATA IOPERA( 239),IX( 239),IY( 239)/'DRAW',   4,  -7/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   3,  -8/
      DATA IOPERA( 241),IX( 241),IY( 241)/'DRAW',   1,  -9/
      DATA IOPERA( 242),IX( 242),IY( 242)/'MOVE',  -6,  -4/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',  -6,  -5/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',  -5,  -5/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',  -5,  -4/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',  -6,  -4/
      DATA IOPERA( 247),IX( 247),IY( 247)/'MOVE',  -5,  12/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   5,  12/
      DATA IOPERA( 249),IX( 249),IY( 249)/'MOVE',  -5,  11/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',   3,  11/
      DATA IOPERA( 251),IX( 251),IY( 251)/'MOVE',  -5,  10/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -1,  10/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',   3,  11/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',   5,  12/
C
      DATA IXMIND(   6)/ -10/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  20/
      DATA ISTARD(   6)/ 207/
      DATA NUMCOO(   6)/  48/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTN1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTN2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX NUMERIC (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3206--6
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   4,   9/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',   4,   8/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',   5,   8/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   5,   9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   4,   9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'MOVE',   5,  10/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   4,  10/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   3,   9/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   3,   8/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',   4,   7/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   5,   7/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   6,   8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   6,   9/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   5,  11/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   3,  12/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   0,  12/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',  -3,  11/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -5,   9/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -6,   7/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -7,   3/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -7,  -3/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -6,  -6/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',  -4,  -8/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',  -1,  -9/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',   1,  -9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',   4,  -8/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   6,  -6/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   7,  -3/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   7,  -2/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   6,   1/
      DATA IOPERA(  31),IX(  31),IY(  31)/'DRAW',   4,   3/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   1,   4/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',  -1,   4/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',  -3,   3/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',  -4,   2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -5,   0/
      DATA IOPERA(  37),IX(  37),IY(  37)/'MOVE',  -4,   9/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',  -5,   7/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -6,   3/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -6,  -3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',  -5,  -6/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',  -4,  -7/
      DATA IOPERA(  43),IX(  43),IY(  43)/'MOVE',   5,  -6/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   6,  -4/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   6,  -1/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   5,   1/
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',   0,  12/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',  -2,  11/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',  -3,  10/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -4,   8/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -5,   4/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',  -5,  -3/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',  -4,  -6/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -3,  -8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',  -1,  -9/
      DATA IOPERA(  56),IX(  56),IY(  56)/'MOVE',   1,  -9/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   3,  -8/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   4,  -7/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   5,  -4/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   5,  -1/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   4,   2/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   3,   3/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',   1,   4/
C
      DATA IXMIND(   7)/ -10/
      DATA IXMAXD(   7)/  10/
      DATA IXDELD(   7)/  20/
      DATA ISTARD(   7)/   1/
      DATA NUMCOO(   7)/  63/
C
C     DEFINE CHARACTER   3207--7
C
      DATA IOPERA(  64),IX(  64),IY(  64)/'MOVE',  -7,  12/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -7,   6/
      DATA IOPERA(  66),IX(  66),IY(  66)/'MOVE',   7,  12/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   7,   9/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   6,   6/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   2,   1/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   1,  -1/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   0,  -5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',   0,  -9/
      DATA IOPERA(  73),IX(  73),IY(  73)/'MOVE',   1,   0/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   0,  -2/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',  -1,  -5/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',  -1,  -9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE',   6,   6/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   1,   1/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',  -1,  -2/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -2,  -5/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -2,  -9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   0,  -9/
      DATA IOPERA(  83),IX(  83),IY(  83)/'MOVE',  -7,   8/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -6,  10/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',  -4,  12/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',  -2,  12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   3,   9/
      DATA IOPERA(  88),IX(  88),IY(  88)/'DRAW',   5,   9/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   6,  10/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   7,  12/
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',  -5,  10/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',  -4,  11/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',  -2,  11/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   0,  10/
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',  -7,   8/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',  -6,   9/
      DATA IOPERA(  97),IX(  97),IY(  97)/'DRAW',  -4,  10/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -2,  10/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   3,   9/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/  64/
      DATA NUMCOO(   8)/  36/
C
C     DEFINE CHARACTER   3208--8
C
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',  -2,  12/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -5,  11/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',  -6,   9/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',  -6,   6/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',  -5,   4/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',  -2,   3/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   2,   3/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',   5,   4/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   6,   6/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   6,   9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   5,  11/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   2,  12/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -2,  12/
      DATA IOPERA( 113),IX( 113),IY( 113)/'MOVE',  -4,  11/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',  -5,   9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',  -5,   6/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',  -4,   4/
      DATA IOPERA( 117),IX( 117),IY( 117)/'MOVE',   4,   4/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   5,   6/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   5,   9/
      DATA IOPERA( 120),IX( 120),IY( 120)/'DRAW',   4,  11/
      DATA IOPERA( 121),IX( 121),IY( 121)/'MOVE',  -2,  12/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',  -3,  11/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',  -4,   9/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -4,   6/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -3,   4/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -2,   3/
      DATA IOPERA( 127),IX( 127),IY( 127)/'MOVE',   2,   3/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',   3,   4/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',   4,   6/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',   4,   9/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',   3,  11/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   2,  12/
      DATA IOPERA( 133),IX( 133),IY( 133)/'MOVE',  -2,   3/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',  -5,   2/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',  -6,   1/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',  -7,  -1/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',  -7,  -5/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',  -6,  -7/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',  -5,  -8/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -2,  -9/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   2,  -9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',   5,  -8/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   6,  -7/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   7,  -5/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   7,  -1/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   6,   1/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   5,   2/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   2,   3/
      DATA IOPERA( 149),IX( 149),IY( 149)/'MOVE',  -5,   1/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',  -6,  -1/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -6,  -5/
      DATA IOPERA( 152),IX( 152),IY( 152)/'DRAW',  -5,  -7/
      DATA IOPERA( 153),IX( 153),IY( 153)/'MOVE',   5,  -7/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   6,  -5/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   6,  -1/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   5,   1/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',  -2,   3/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -4,   2/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -5,  -1/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -5,  -5/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -4,  -8/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -2,  -9/
      DATA IOPERA( 163),IX( 163),IY( 163)/'MOVE',   2,  -9/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   4,  -8/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   5,  -5/
      DATA IOPERA( 166),IX( 166),IY( 166)/'DRAW',   5,  -1/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   4,   2/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   2,   3/
C
      DATA IXMIND(   9)/ -10/
      DATA IXMAXD(   9)/  10/
      DATA IXDELD(   9)/  20/
      DATA ISTARD(   9)/ 100/
      DATA NUMCOO(   9)/  69/
C
C     DEFINE CHARACTER   3209--9
C
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',  -5,  -5/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',  -5,  -6/
      DATA IOPERA( 171),IX( 171),IY( 171)/'DRAW',  -4,  -6/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',  -4,  -5/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',  -5,  -5/
      DATA IOPERA( 174),IX( 174),IY( 174)/'MOVE',   5,   3/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',   4,   1/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   3,   0/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   1,  -1/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',  -1,  -1/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',  -4,   0/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',  -6,   2/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',  -7,   5/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -7,   6/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',  -6,   9/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -4,  11/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -1,  12/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   1,  12/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   4,  11/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',   6,   9/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',   7,   6/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',   7,   0/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   6,  -4/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   5,  -6/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   3,  -8/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   0,  -9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -3,  -9/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -5,  -8/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -6,  -6/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -6,  -5/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -5,  -4/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -4,  -4/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',  -3,  -5/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -3,  -6/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',  -4,  -7/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',  -5,  -7/
      DATA IOPERA( 205),IX( 205),IY( 205)/'MOVE',  -5,   2/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  -6,   4/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',  -6,   7/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -5,   9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'MOVE',   4,  10/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',   5,   9/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   6,   6/
      DATA IOPERA( 212),IX( 212),IY( 212)/'DRAW',   6,   0/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   5,  -4/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   4,  -6/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',  -1,  -1/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',  -3,   0/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -4,   1/
      DATA IOPERA( 218),IX( 218),IY( 218)/'DRAW',  -5,   4/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -5,   7/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',  -4,  10/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',  -3,  11/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',  -1,  12/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',   1,  12/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',   3,  11/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',   4,   9/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',   5,   6/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',   5,  -1/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',   4,  -5/
      DATA IOPERA( 229),IX( 229),IY( 229)/'DRAW',   3,  -7/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',   2,  -8/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',   0,  -9/
C
      DATA IXMIND(  10)/ -10/
      DATA IXMAXD(  10)/  10/
      DATA IXDELD(  10)/  20/
      DATA ISTARD(  10)/ 169/
      DATA NUMCOO(  10)/  63/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTN2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      SUBROUTINE DRTS1(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX SYMBOLS (PART 1).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MARCH     1987.
C     UPDATED         --MAY       1982.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3210--. (PERIOD)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   0,  -6/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -1,  -7/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -1,  -8/
      DATA IOPERA(   4),IX(   4),IY(   4)/'DRAW',   0,  -9/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',   1,  -9/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',   2,  -8/
      DATA IOPERA(   7),IX(   7),IY(   7)/'DRAW',   2,  -7/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   1,  -6/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',   0,  -6/
      DATA IOPERA(  10),IX(  10),IY(  10)/'MOVE',   0,  -7/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',   0,  -8/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',   1,  -8/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',   1,  -7/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',   0,  -7/
C
      DATA IXMIND(   1)/  -5/
      DATA IXMAXD(   1)/   6/
      DATA IXDELD(   1)/  11/
      DATA ISTARD(   1)/   1/
      DATA NUMCOO(   1)/  14/
C
C     DEFINE CHARACTER   3211--, (COMMA)
C
      DATA IOPERA(  15),IX(  15),IY(  15)/'MOVE',   2,  -8/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   1,  -9/
      DATA IOPERA(  17),IX(  17),IY(  17)/'DRAW',   0,  -9/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -1,  -8/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -1,  -7/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',   0,  -6/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',   1,  -6/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',   2,  -7/
      DATA IOPERA(  23),IX(  23),IY(  23)/'DRAW',   2, -10/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   1, -12/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -1, -13/
      DATA IOPERA(  26),IX(  26),IY(  26)/'MOVE',   0,  -7/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',   0,  -8/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',   1,  -8/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   1,  -7/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   0,  -7/
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',   1,  -9/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',   2, -10/
      DATA IOPERA(  33),IX(  33),IY(  33)/'MOVE',   2,  -8/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   1, -12/
C
      DATA IXMIND(   2)/  -5/
      DATA IXMAXD(   2)/   6/
      DATA IXDELD(   2)/  11/
      DATA ISTARD(   2)/  15/
      DATA NUMCOO(   2)/  20/
C
C     DEFINE CHARACTER   3212--: (COLON)
C
      DATA IOPERA(  35),IX(  35),IY(  35)/'MOVE',   0,   5/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',  -1,   4/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',  -1,   3/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   0,   2/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',   1,   2/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',   2,   3/
      DATA IOPERA(  41),IX(  41),IY(  41)/'DRAW',   2,   4/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   1,   5/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   0,   5/
      DATA IOPERA(  44),IX(  44),IY(  44)/'MOVE',   0,   4/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   0,   3/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   1,   3/
      DATA IOPERA(  47),IX(  47),IY(  47)/'DRAW',   1,   4/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   0,   4/
      DATA IOPERA(  49),IX(  49),IY(  49)/'MOVE',   0,  -6/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',  -1,  -7/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',  -1,  -8/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   0,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   1,  -9/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',   2,  -8/
      DATA IOPERA(  55),IX(  55),IY(  55)/'DRAW',   2,  -7/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',   1,  -6/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   0,  -6/
      DATA IOPERA(  58),IX(  58),IY(  58)/'MOVE',   0,  -7/
      DATA IOPERA(  59),IX(  59),IY(  59)/'DRAW',   0,  -8/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   1,  -8/
      DATA IOPERA(  61),IX(  61),IY(  61)/'DRAW',   1,  -7/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   0,  -7/
C
      DATA IXMIND(   3)/  -5/
      DATA IXMAXD(   3)/   6/
      DATA IXDELD(   3)/  11/
      DATA ISTARD(   3)/  35/
      DATA NUMCOO(   3)/  28/
C
C     DEFINE CHARACTER   3213--; (SEMICOLON)
C
      DATA IOPERA(  63),IX(  63),IY(  63)/'MOVE',   0,   5/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',  -1,   4/
      DATA IOPERA(  65),IX(  65),IY(  65)/'DRAW',  -1,   3/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',   0,   2/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   1,   2/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   2,   3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'DRAW',   2,   4/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   1,   5/
      DATA IOPERA(  71),IX(  71),IY(  71)/'DRAW',   0,   5/
      DATA IOPERA(  72),IX(  72),IY(  72)/'MOVE',   0,   4/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   0,   3/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   1,   3/
      DATA IOPERA(  75),IX(  75),IY(  75)/'DRAW',   1,   4/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   0,   4/
      DATA IOPERA(  77),IX(  77),IY(  77)/'MOVE',   2,  -8/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',   1,  -9/
      DATA IOPERA(  79),IX(  79),IY(  79)/'DRAW',   0,  -9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -1,  -8/
      DATA IOPERA(  81),IX(  81),IY(  81)/'DRAW',  -1,  -7/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   0,  -6/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',   1,  -6/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',   2,  -7/
      DATA IOPERA(  85),IX(  85),IY(  85)/'DRAW',   2, -10/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   1, -12/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',  -1, -13/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',   0,  -7/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',   0,  -8/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   1,  -8/
      DATA IOPERA(  91),IX(  91),IY(  91)/'DRAW',   1,  -7/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   0,  -7/
      DATA IOPERA(  93),IX(  93),IY(  93)/'MOVE',   1,  -9/
      DATA IOPERA(  94),IX(  94),IY(  94)/'DRAW',   2, -10/
      DATA IOPERA(  95),IX(  95),IY(  95)/'MOVE',   2,  -8/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1, -12/
C
      DATA IXMIND(   4)/  -5/
      DATA IXMAXD(   4)/   6/
      DATA IXDELD(   4)/  11/
      DATA ISTARD(   4)/  63/
      DATA NUMCOO(   4)/  34/
C
C     DEFINE CHARACTER   3214--! (EXCLAMATION POINT)
C
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',   0,  12/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',  -1,  11/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',  -1,   9/
      DATA IOPERA( 100),IX( 100),IY( 100)/'DRAW',   0,   1/
      DATA IOPERA( 101),IX( 101),IY( 101)/'MOVE',   0,  12/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   0,  -2/
      DATA IOPERA( 103),IX( 103),IY( 103)/'DRAW',   1,  -2/
      DATA IOPERA( 104),IX( 104),IY( 104)/'MOVE',   0,  12/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   1,  12/
      DATA IOPERA( 106),IX( 106),IY( 106)/'DRAW',   1,  -2/
      DATA IOPERA( 107),IX( 107),IY( 107)/'MOVE',   1,  12/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   2,  11/
      DATA IOPERA( 109),IX( 109),IY( 109)/'DRAW',   2,   9/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   1,   1/
      DATA IOPERA( 111),IX( 111),IY( 111)/'MOVE',   0,  -6/
      DATA IOPERA( 112),IX( 112),IY( 112)/'DRAW',  -1,  -7/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -1,  -8/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   0,  -9/
      DATA IOPERA( 115),IX( 115),IY( 115)/'DRAW',   1,  -9/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   2,  -8/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   2,  -7/
      DATA IOPERA( 118),IX( 118),IY( 118)/'DRAW',   1,  -6/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   0,  -6/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   0,  -7/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   0,  -8/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   1,  -8/
      DATA IOPERA( 123),IX( 123),IY( 123)/'DRAW',   1,  -7/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',   0,  -7/
C
      DATA IXMIND(   5)/  -5/
      DATA IXMAXD(   5)/   6/
      DATA IXDELD(   5)/  11/
      DATA ISTARD(   5)/  97/
      DATA NUMCOO(   5)/  28/
C
C     DEFINE CHARACTER   3215--? (QUESTION MARK)
C
      DATA IOPERA( 125),IX( 125),IY( 125)/'MOVE',  -5,   7/
      DATA IOPERA( 126),IX( 126),IY( 126)/'DRAW',  -5,   8/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -4,   8/
      DATA IOPERA( 128),IX( 128),IY( 128)/'DRAW',  -4,   6/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -6,   6/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -6,   8/
      DATA IOPERA( 131),IX( 131),IY( 131)/'DRAW',  -5,  10/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',  -4,  11/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',  -2,  12/
      DATA IOPERA( 134),IX( 134),IY( 134)/'DRAW',   2,  12/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   5,  11/
      DATA IOPERA( 136),IX( 136),IY( 136)/'DRAW',   6,  10/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   7,   8/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   7,   6/
      DATA IOPERA( 139),IX( 139),IY( 139)/'DRAW',   6,   4/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',   5,   3/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',   1,   1/
      DATA IOPERA( 142),IX( 142),IY( 142)/'MOVE',   5,  10/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',   6,   9/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',   6,   5/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   5,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'MOVE',   2,  12/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   4,  11/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   5,   9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   5,   5/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   4,   3/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',   3,   2/
      DATA IOPERA( 152),IX( 152),IY( 152)/'MOVE',   0,   1/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',   0,  -2/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',   1,  -2/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   1,   1/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   0,   1/
      DATA IOPERA( 157),IX( 157),IY( 157)/'MOVE',   0,  -6/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -1,  -7/
      DATA IOPERA( 159),IX( 159),IY( 159)/'DRAW',  -1,  -8/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',   0,  -9/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',   1,  -9/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',   2,  -8/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   2,  -7/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   1,  -6/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   0,  -6/
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   0,  -7/
      DATA IOPERA( 167),IX( 167),IY( 167)/'DRAW',   0,  -8/
      DATA IOPERA( 168),IX( 168),IY( 168)/'DRAW',   1,  -8/
      DATA IOPERA( 169),IX( 169),IY( 169)/'DRAW',   1,  -7/
      DATA IOPERA( 170),IX( 170),IY( 170)/'DRAW',   0,  -7/
C
      DATA IXMIND(   6)/  -9/
      DATA IXMAXD(   6)/  10/
      DATA IXDELD(   6)/  19/
      DATA ISTARD(   6)/ 125/
      DATA NUMCOO(   6)/  46/
C
C     DEFINE CHARACTER   3218--& (AMPERSAND)
C
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',   9,   3/
      DATA IOPERA( 172),IX( 172),IY( 172)/'DRAW',   9,   4/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   8,   4/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',   8,   2/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  10,   2/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',  10,   4/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   9,   5/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   8,   5/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   7,   4/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   6,   2/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   4,  -3/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',   2,  -6/
      DATA IOPERA( 183),IX( 183),IY( 183)/'DRAW',   0,  -8/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',  -2,  -9/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',  -6,  -9/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',  -8,  -8/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',  -9,  -6/
      DATA IOPERA( 188),IX( 188),IY( 188)/'DRAW',  -9,  -3/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -8,  -1/
      DATA IOPERA( 190),IX( 190),IY( 190)/'DRAW',  -2,   3/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   0,   5/
      DATA IOPERA( 192),IX( 192),IY( 192)/'DRAW',   1,   7/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   1,   9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   0,  11/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -2,  12/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -4,  11/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',  -5,   9/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',  -5,   6/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',  -4,   3/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',  -2,   0/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   2,  -5/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',   5,  -8/
      DATA IOPERA( 203),IX( 203),IY( 203)/'DRAW',   7,  -9/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   9,  -9/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',  10,  -7/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',  10,  -6/
      DATA IOPERA( 207),IX( 207),IY( 207)/'MOVE',  -7,  -8/
      DATA IOPERA( 208),IX( 208),IY( 208)/'DRAW',  -8,  -6/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',  -8,  -3/
      DATA IOPERA( 210),IX( 210),IY( 210)/'DRAW',  -7,  -1/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',  -6,   0/
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',   0,   5/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   1,   9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'MOVE',   1,   7/
      DATA IOPERA( 215),IX( 215),IY( 215)/'DRAW',   0,  11/
      DATA IOPERA( 216),IX( 216),IY( 216)/'MOVE',  -4,  11/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',  -5,   7/
      DATA IOPERA( 218),IX( 218),IY( 218)/'MOVE',  -4,   4/
      DATA IOPERA( 219),IX( 219),IY( 219)/'DRAW',  -2,   1/
      DATA IOPERA( 220),IX( 220),IY( 220)/'DRAW',   2,  -4/
      DATA IOPERA( 221),IX( 221),IY( 221)/'DRAW',   5,  -7/
      DATA IOPERA( 222),IX( 222),IY( 222)/'DRAW',   7,  -8/
      DATA IOPERA( 223),IX( 223),IY( 223)/'MOVE',  -4,  -9/
      DATA IOPERA( 224),IX( 224),IY( 224)/'DRAW',  -6,  -8/
      DATA IOPERA( 225),IX( 225),IY( 225)/'DRAW',  -7,  -6/
      DATA IOPERA( 226),IX( 226),IY( 226)/'DRAW',  -7,  -3/
      DATA IOPERA( 227),IX( 227),IY( 227)/'DRAW',  -6,  -1/
      DATA IOPERA( 228),IX( 228),IY( 228)/'DRAW',  -2,   3/
      DATA IOPERA( 229),IX( 229),IY( 229)/'MOVE',  -5,   9/
      DATA IOPERA( 230),IX( 230),IY( 230)/'DRAW',  -4,   5/
      DATA IOPERA( 231),IX( 231),IY( 231)/'DRAW',  -1,   1/
      DATA IOPERA( 232),IX( 232),IY( 232)/'DRAW',   3,  -4/
      DATA IOPERA( 233),IX( 233),IY( 233)/'DRAW',   6,  -7/
      DATA IOPERA( 234),IX( 234),IY( 234)/'DRAW',   8,  -8/
      DATA IOPERA( 235),IX( 235),IY( 235)/'DRAW',   9,  -8/
      DATA IOPERA( 236),IX( 236),IY( 236)/'DRAW',  10,  -7/
C
      DATA IXMIND(   7)/ -13/
      DATA IXMAXD(   7)/  13/
      DATA IXDELD(   7)/  26/
      DATA ISTARD(   7)/ 171/
      DATA NUMCOO(   7)/  66/
C
C     DEFINE CHARACTER   3219--$ (DOLLAR SIGN)
C
      DATA IOPERA( 237),IX( 237),IY( 237)/'MOVE',  -2,  16/
      DATA IOPERA( 238),IX( 238),IY( 238)/'DRAW',  -2, -13/
      DATA IOPERA( 239),IX( 239),IY( 239)/'MOVE',   2,  16/
      DATA IOPERA( 240),IX( 240),IY( 240)/'DRAW',   2, -13/
      DATA IOPERA( 241),IX( 241),IY( 241)/'MOVE',   6,   7/
      DATA IOPERA( 242),IX( 242),IY( 242)/'DRAW',   6,   8/
      DATA IOPERA( 243),IX( 243),IY( 243)/'DRAW',   5,   8/
      DATA IOPERA( 244),IX( 244),IY( 244)/'DRAW',   5,   6/
      DATA IOPERA( 245),IX( 245),IY( 245)/'DRAW',   7,   6/
      DATA IOPERA( 246),IX( 246),IY( 246)/'DRAW',   7,   8/
      DATA IOPERA( 247),IX( 247),IY( 247)/'DRAW',   6,  10/
      DATA IOPERA( 248),IX( 248),IY( 248)/'DRAW',   5,  11/
      DATA IOPERA( 249),IX( 249),IY( 249)/'DRAW',   2,  12/
      DATA IOPERA( 250),IX( 250),IY( 250)/'DRAW',  -2,  12/
      DATA IOPERA( 251),IX( 251),IY( 251)/'DRAW',  -5,  11/
      DATA IOPERA( 252),IX( 252),IY( 252)/'DRAW',  -7,   9/
      DATA IOPERA( 253),IX( 253),IY( 253)/'DRAW',  -7,   6/
      DATA IOPERA( 254),IX( 254),IY( 254)/'DRAW',  -6,   4/
      DATA IOPERA( 255),IX( 255),IY( 255)/'DRAW',  -3,   2/
      DATA IOPERA( 256),IX( 256),IY( 256)/'DRAW',   3,   0/
      DATA IOPERA( 257),IX( 257),IY( 257)/'DRAW',   5,  -1/
      DATA IOPERA( 258),IX( 258),IY( 258)/'DRAW',   6,  -3/
      DATA IOPERA( 259),IX( 259),IY( 259)/'DRAW',   6,  -6/
      DATA IOPERA( 260),IX( 260),IY( 260)/'DRAW',   5,  -8/
      DATA IOPERA( 261),IX( 261),IY( 261)/'MOVE',  -6,   6/
      DATA IOPERA( 262),IX( 262),IY( 262)/'DRAW',  -5,   4/
      DATA IOPERA( 263),IX( 263),IY( 263)/'DRAW',  -3,   3/
      DATA IOPERA( 264),IX( 264),IY( 264)/'DRAW',   3,   1/
      DATA IOPERA( 265),IX( 265),IY( 265)/'DRAW',   5,   0/
      DATA IOPERA( 266),IX( 266),IY( 266)/'DRAW',   6,  -2/
      DATA IOPERA( 267),IX( 267),IY( 267)/'MOVE',  -5,  11/
      DATA IOPERA( 268),IX( 268),IY( 268)/'DRAW',  -6,   9/
      DATA IOPERA( 269),IX( 269),IY( 269)/'DRAW',  -6,   7/
      DATA IOPERA( 270),IX( 270),IY( 270)/'DRAW',  -5,   5/
      DATA IOPERA( 271),IX( 271),IY( 271)/'DRAW',  -3,   4/
      DATA IOPERA( 272),IX( 272),IY( 272)/'DRAW',   3,   2/
      DATA IOPERA( 273),IX( 273),IY( 273)/'DRAW',   6,   0/
      DATA IOPERA( 274),IX( 274),IY( 274)/'DRAW',   7,  -2/
      DATA IOPERA( 275),IX( 275),IY( 275)/'DRAW',   7,  -5/
      DATA IOPERA( 276),IX( 276),IY( 276)/'DRAW',   6,  -7/
      DATA IOPERA( 277),IX( 277),IY( 277)/'DRAW',   5,  -8/
      DATA IOPERA( 278),IX( 278),IY( 278)/'DRAW',   2,  -9/
      DATA IOPERA( 279),IX( 279),IY( 279)/'DRAW',  -2,  -9/
      DATA IOPERA( 280),IX( 280),IY( 280)/'DRAW',  -5,  -8/
      DATA IOPERA( 281),IX( 281),IY( 281)/'DRAW',  -6,  -7/
      DATA IOPERA( 282),IX( 282),IY( 282)/'DRAW',  -7,  -5/
      DATA IOPERA( 283),IX( 283),IY( 283)/'DRAW',  -7,  -3/
      DATA IOPERA( 284),IX( 284),IY( 284)/'DRAW',  -5,  -3/
      DATA IOPERA( 285),IX( 285),IY( 285)/'DRAW',  -5,  -5/
      DATA IOPERA( 286),IX( 286),IY( 286)/'DRAW',  -6,  -5/
      DATA IOPERA( 287),IX( 287),IY( 287)/'DRAW',  -6,  -4/
C
      DATA IXMIND(   8)/ -10/
      DATA IXMAXD(   8)/  10/
      DATA IXDELD(   8)/  20/
      DATA ISTARD(   8)/ 237/
      DATA NUMCOO(   8)/  51/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTS1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTS1--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
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
      SUBROUTINE DRTS2(ICHARN,IOP,X,Y,NUMCO,IXMINS,IXMAXS,IXDELS,
     1IBUGD2,IFOUND,IERROR)
C
C     PURPOSE--DEFINE AND SET THE HERSHEY CHARACTER SET COORDINATES
C              FOR ROMAN TRIPLEX SYMBOLS (PART 2).
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C           THIS SUBROUTINE MAY NOT BE COPIED, EXTRACTED,
C           MODIFIED, OR OTHERWISE USED IN A CONTEXT
C           OUTSIDE OF THE DATAPLOT LANGUAGE/SYSTEM.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--87/4
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MARCH     1981.
C     UPDATED         --MARCH     1982.
C     UPDATED         --MAY       1982.
C     UPDATED         --MARCH     1987.
C     UPDATED         --MAY       1987.
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
      DIMENSION IXMIND(30)
      DIMENSION IXMAXD(30)
      DIMENSION IXDELD(30)
      DIMENSION ISTARD(30)
      DIMENSION NUMCOO(30)
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
C     DEFINE CHARACTER   3220--/ (SLASH)
C
      DATA IOPERA(   1),IX(   1),IY(   1)/'MOVE',   9,  16/
      DATA IOPERA(   2),IX(   2),IY(   2)/'DRAW',  -9, -16/
      DATA IOPERA(   3),IX(   3),IY(   3)/'DRAW',  -8, -16/
      DATA IOPERA(   4),IX(   4),IY(   4)/'MOVE',   9,  16/
      DATA IOPERA(   5),IX(   5),IY(   5)/'DRAW',  10,  16/
      DATA IOPERA(   6),IX(   6),IY(   6)/'DRAW',  -8, -16/
C
      DATA IXMIND(   9)/ -11/
      DATA IXMAXD(   9)/  12/
      DATA IXDELD(   9)/  23/
      DATA ISTARD(   9)/   1/
      DATA NUMCOO(   9)/   6/
C
C     DEFINE CHARACTER   3221--( (LEFT PARENTHESES)
C
      DATA IOPERA(   7),IX(   7),IY(   7)/'MOVE',   3,  16/
      DATA IOPERA(   8),IX(   8),IY(   8)/'DRAW',   1,  14/
      DATA IOPERA(   9),IX(   9),IY(   9)/'DRAW',  -1,  11/
      DATA IOPERA(  10),IX(  10),IY(  10)/'DRAW',  -3,   7/
      DATA IOPERA(  11),IX(  11),IY(  11)/'DRAW',  -4,   2/
      DATA IOPERA(  12),IX(  12),IY(  12)/'DRAW',  -4,  -2/
      DATA IOPERA(  13),IX(  13),IY(  13)/'DRAW',  -3,  -7/
      DATA IOPERA(  14),IX(  14),IY(  14)/'DRAW',  -1, -11/
      DATA IOPERA(  15),IX(  15),IY(  15)/'DRAW',   1, -14/
      DATA IOPERA(  16),IX(  16),IY(  16)/'DRAW',   3, -16/
      DATA IOPERA(  17),IX(  17),IY(  17)/'MOVE',  -1,  10/
      DATA IOPERA(  18),IX(  18),IY(  18)/'DRAW',  -2,   7/
      DATA IOPERA(  19),IX(  19),IY(  19)/'DRAW',  -3,   3/
      DATA IOPERA(  20),IX(  20),IY(  20)/'DRAW',  -3,  -3/
      DATA IOPERA(  21),IX(  21),IY(  21)/'DRAW',  -2,  -7/
      DATA IOPERA(  22),IX(  22),IY(  22)/'DRAW',  -1, -10/
      DATA IOPERA(  23),IX(  23),IY(  23)/'MOVE',   1,  14/
      DATA IOPERA(  24),IX(  24),IY(  24)/'DRAW',   0,  12/
      DATA IOPERA(  25),IX(  25),IY(  25)/'DRAW',  -1,   9/
      DATA IOPERA(  26),IX(  26),IY(  26)/'DRAW',  -2,   3/
      DATA IOPERA(  27),IX(  27),IY(  27)/'DRAW',  -2,  -3/
      DATA IOPERA(  28),IX(  28),IY(  28)/'DRAW',  -1,  -9/
      DATA IOPERA(  29),IX(  29),IY(  29)/'DRAW',   0, -12/
      DATA IOPERA(  30),IX(  30),IY(  30)/'DRAW',   1, -14/
C
      DATA IXMIND(  10)/  -7/
      DATA IXMAXD(  10)/   7/
      DATA IXDELD(  10)/  14/
      DATA ISTARD(  10)/   7/
      DATA NUMCOO(  10)/  24/
C
C     DEFINE CHARACTER   3222--) (RIGHT PARENTHESES)
C
      DATA IOPERA(  31),IX(  31),IY(  31)/'MOVE',  -3,  16/
      DATA IOPERA(  32),IX(  32),IY(  32)/'DRAW',  -1,  14/
      DATA IOPERA(  33),IX(  33),IY(  33)/'DRAW',   1,  11/
      DATA IOPERA(  34),IX(  34),IY(  34)/'DRAW',   3,   7/
      DATA IOPERA(  35),IX(  35),IY(  35)/'DRAW',   4,   2/
      DATA IOPERA(  36),IX(  36),IY(  36)/'DRAW',   4,  -2/
      DATA IOPERA(  37),IX(  37),IY(  37)/'DRAW',   3,  -7/
      DATA IOPERA(  38),IX(  38),IY(  38)/'DRAW',   1, -11/
      DATA IOPERA(  39),IX(  39),IY(  39)/'DRAW',  -1, -14/
      DATA IOPERA(  40),IX(  40),IY(  40)/'DRAW',  -3, -16/
      DATA IOPERA(  41),IX(  41),IY(  41)/'MOVE',   1,  10/
      DATA IOPERA(  42),IX(  42),IY(  42)/'DRAW',   2,   7/
      DATA IOPERA(  43),IX(  43),IY(  43)/'DRAW',   3,   3/
      DATA IOPERA(  44),IX(  44),IY(  44)/'DRAW',   3,  -3/
      DATA IOPERA(  45),IX(  45),IY(  45)/'DRAW',   2,  -7/
      DATA IOPERA(  46),IX(  46),IY(  46)/'DRAW',   1, -10/
      DATA IOPERA(  47),IX(  47),IY(  47)/'MOVE',  -1,  14/
      DATA IOPERA(  48),IX(  48),IY(  48)/'DRAW',   0,  12/
      DATA IOPERA(  49),IX(  49),IY(  49)/'DRAW',   1,   9/
      DATA IOPERA(  50),IX(  50),IY(  50)/'DRAW',   2,   3/
      DATA IOPERA(  51),IX(  51),IY(  51)/'DRAW',   2,  -3/
      DATA IOPERA(  52),IX(  52),IY(  52)/'DRAW',   1,  -9/
      DATA IOPERA(  53),IX(  53),IY(  53)/'DRAW',   0, -12/
      DATA IOPERA(  54),IX(  54),IY(  54)/'DRAW',  -1, -14/
C
      DATA IXMIND(  11)/  -7/
      DATA IXMAXD(  11)/   7/
      DATA IXDELD(  11)/  14/
      DATA ISTARD(  11)/  31/
      DATA NUMCOO(  11)/  24/
C
C     DEFINE CHARACTER   3223--* (ASTERISK)
C
      DATA IOPERA(  55),IX(  55),IY(  55)/'MOVE',   0,  12/
      DATA IOPERA(  56),IX(  56),IY(  56)/'DRAW',  -1,  11/
      DATA IOPERA(  57),IX(  57),IY(  57)/'DRAW',   1,   1/
      DATA IOPERA(  58),IX(  58),IY(  58)/'DRAW',   0,   0/
      DATA IOPERA(  59),IX(  59),IY(  59)/'MOVE',   0,  12/
      DATA IOPERA(  60),IX(  60),IY(  60)/'DRAW',   0,   0/
      DATA IOPERA(  61),IX(  61),IY(  61)/'MOVE',   0,  12/
      DATA IOPERA(  62),IX(  62),IY(  62)/'DRAW',   1,  11/
      DATA IOPERA(  63),IX(  63),IY(  63)/'DRAW',  -1,   1/
      DATA IOPERA(  64),IX(  64),IY(  64)/'DRAW',   0,   0/
      DATA IOPERA(  65),IX(  65),IY(  65)/'MOVE',  -5,   9/
      DATA IOPERA(  66),IX(  66),IY(  66)/'DRAW',  -4,   9/
      DATA IOPERA(  67),IX(  67),IY(  67)/'DRAW',   4,   3/
      DATA IOPERA(  68),IX(  68),IY(  68)/'DRAW',   5,   3/
      DATA IOPERA(  69),IX(  69),IY(  69)/'MOVE',  -5,   9/
      DATA IOPERA(  70),IX(  70),IY(  70)/'DRAW',   5,   3/
      DATA IOPERA(  71),IX(  71),IY(  71)/'MOVE',  -5,   9/
      DATA IOPERA(  72),IX(  72),IY(  72)/'DRAW',  -5,   8/
      DATA IOPERA(  73),IX(  73),IY(  73)/'DRAW',   5,   4/
      DATA IOPERA(  74),IX(  74),IY(  74)/'DRAW',   5,   3/
      DATA IOPERA(  75),IX(  75),IY(  75)/'MOVE',   5,   9/
      DATA IOPERA(  76),IX(  76),IY(  76)/'DRAW',   4,   9/
      DATA IOPERA(  77),IX(  77),IY(  77)/'DRAW',  -4,   3/
      DATA IOPERA(  78),IX(  78),IY(  78)/'DRAW',  -5,   3/
      DATA IOPERA(  79),IX(  79),IY(  79)/'MOVE',   5,   9/
      DATA IOPERA(  80),IX(  80),IY(  80)/'DRAW',  -5,   3/
      DATA IOPERA(  81),IX(  81),IY(  81)/'MOVE',   5,   9/
      DATA IOPERA(  82),IX(  82),IY(  82)/'DRAW',   5,   8/
      DATA IOPERA(  83),IX(  83),IY(  83)/'DRAW',  -5,   4/
      DATA IOPERA(  84),IX(  84),IY(  84)/'DRAW',  -5,   3/
C
      DATA IXMIND(  12)/  -8/
      DATA IXMAXD(  12)/   8/
      DATA IXDELD(  12)/  16/
      DATA ISTARD(  12)/  55/
      DATA NUMCOO(  12)/  30/
C
C     DEFINE CHARACTER   3224--- (HYPHEN OR MINUS SIGN)
C
      DATA IOPERA(  85),IX(  85),IY(  85)/'MOVE',  -8,   1/
      DATA IOPERA(  86),IX(  86),IY(  86)/'DRAW',   9,   1/
      DATA IOPERA(  87),IX(  87),IY(  87)/'DRAW',   9,   0/
      DATA IOPERA(  88),IX(  88),IY(  88)/'MOVE',  -8,   1/
      DATA IOPERA(  89),IX(  89),IY(  89)/'DRAW',  -8,   0/
      DATA IOPERA(  90),IX(  90),IY(  90)/'DRAW',   9,   0/
C
      DATA IXMIND(  13)/ -12/
      DATA IXMAXD(  13)/  13/
      DATA IXDELD(  13)/  25/
      DATA ISTARD(  13)/  85/
      DATA NUMCOO(  13)/   6/
C
C     DEFINE CHARACTER   3225--+ (PLUS SIGN)
C
      DATA IOPERA(  91),IX(  91),IY(  91)/'MOVE',   0,   9/
      DATA IOPERA(  92),IX(  92),IY(  92)/'DRAW',   0,  -8/
      DATA IOPERA(  93),IX(  93),IY(  93)/'DRAW',   1,  -8/
      DATA IOPERA(  94),IX(  94),IY(  94)/'MOVE',   0,   9/
      DATA IOPERA(  95),IX(  95),IY(  95)/'DRAW',   1,   9/
      DATA IOPERA(  96),IX(  96),IY(  96)/'DRAW',   1,  -8/
      DATA IOPERA(  97),IX(  97),IY(  97)/'MOVE',  -8,   1/
      DATA IOPERA(  98),IX(  98),IY(  98)/'DRAW',   9,   1/
      DATA IOPERA(  99),IX(  99),IY(  99)/'DRAW',   9,   0/
      DATA IOPERA( 100),IX( 100),IY( 100)/'MOVE',  -8,   1/
      DATA IOPERA( 101),IX( 101),IY( 101)/'DRAW',  -8,   0/
      DATA IOPERA( 102),IX( 102),IY( 102)/'DRAW',   9,   0/
C
      DATA IXMIND(  14)/ -12/
      DATA IXMAXD(  14)/  13/
      DATA IXDELD(  14)/  25/
      DATA ISTARD(  14)/  91/
      DATA NUMCOO(  14)/  12/
C
C     DEFINE CHARACTER   3226--= (EQUAL SIGN)
C
      DATA IOPERA( 103),IX( 103),IY( 103)/'MOVE',  -8,   5/
      DATA IOPERA( 104),IX( 104),IY( 104)/'DRAW',   9,   5/
      DATA IOPERA( 105),IX( 105),IY( 105)/'DRAW',   9,   4/
      DATA IOPERA( 106),IX( 106),IY( 106)/'MOVE',  -8,   5/
      DATA IOPERA( 107),IX( 107),IY( 107)/'DRAW',  -8,   4/
      DATA IOPERA( 108),IX( 108),IY( 108)/'DRAW',   9,   4/
      DATA IOPERA( 109),IX( 109),IY( 109)/'MOVE',  -8,  -3/
      DATA IOPERA( 110),IX( 110),IY( 110)/'DRAW',   9,  -3/
      DATA IOPERA( 111),IX( 111),IY( 111)/'DRAW',   9,  -4/
      DATA IOPERA( 112),IX( 112),IY( 112)/'MOVE',  -8,  -3/
      DATA IOPERA( 113),IX( 113),IY( 113)/'DRAW',  -8,  -4/
      DATA IOPERA( 114),IX( 114),IY( 114)/'DRAW',   9,  -4/
C
      DATA IXMIND(  15)/ -12/
      DATA IXMAXD(  15)/  13/
      DATA IXDELD(  15)/  25/
      DATA ISTARD(  15)/ 103/
      DATA NUMCOO(  15)/  12/
C
C     DEFINE CHARACTER   3227--' (SINGLE QUOTE)
C
      DATA IOPERA( 115),IX( 115),IY( 115)/'MOVE',   1,  12/
      DATA IOPERA( 116),IX( 116),IY( 116)/'DRAW',   0,  11/
      DATA IOPERA( 117),IX( 117),IY( 117)/'DRAW',   0,   5/
      DATA IOPERA( 118),IX( 118),IY( 118)/'MOVE',   1,  11/
      DATA IOPERA( 119),IX( 119),IY( 119)/'DRAW',   0,   5/
      DATA IOPERA( 120),IX( 120),IY( 120)/'MOVE',   1,  12/
      DATA IOPERA( 121),IX( 121),IY( 121)/'DRAW',   2,  11/
      DATA IOPERA( 122),IX( 122),IY( 122)/'DRAW',   0,   5/
C
      DATA IXMIND(  16)/  -4/
      DATA IXMAXD(  16)/   5/
      DATA IXDELD(  16)/   9/
      DATA ISTARD(  16)/ 115/
      DATA NUMCOO(  16)/   8/
C
C     DEFINE CHARACTER   3228--  (DOUBLE QUOTE)
C
      DATA IOPERA( 123),IX( 123),IY( 123)/'MOVE',  -4,  12/
      DATA IOPERA( 124),IX( 124),IY( 124)/'DRAW',  -5,  11/
      DATA IOPERA( 125),IX( 125),IY( 125)/'DRAW',  -5,   5/
      DATA IOPERA( 126),IX( 126),IY( 126)/'MOVE',  -4,  11/
      DATA IOPERA( 127),IX( 127),IY( 127)/'DRAW',  -5,   5/
      DATA IOPERA( 128),IX( 128),IY( 128)/'MOVE',  -4,  12/
      DATA IOPERA( 129),IX( 129),IY( 129)/'DRAW',  -3,  11/
      DATA IOPERA( 130),IX( 130),IY( 130)/'DRAW',  -5,   5/
      DATA IOPERA( 131),IX( 131),IY( 131)/'MOVE',   5,  12/
      DATA IOPERA( 132),IX( 132),IY( 132)/'DRAW',   4,  11/
      DATA IOPERA( 133),IX( 133),IY( 133)/'DRAW',   4,   5/
      DATA IOPERA( 134),IX( 134),IY( 134)/'MOVE',   5,  11/
      DATA IOPERA( 135),IX( 135),IY( 135)/'DRAW',   4,   5/
      DATA IOPERA( 136),IX( 136),IY( 136)/'MOVE',   5,  12/
      DATA IOPERA( 137),IX( 137),IY( 137)/'DRAW',   6,  11/
      DATA IOPERA( 138),IX( 138),IY( 138)/'DRAW',   4,   5/
C
      DATA IXMIND(  17)/  -9/
      DATA IXMAXD(  17)/   9/
      DATA IXDELD(  17)/  18/
      DATA ISTARD(  17)/ 123/
      DATA NUMCOO(  17)/  16/
C
C     DEFINE CHARACTER   3229--  (DEGREES)
C
      DATA IOPERA( 139),IX( 139),IY( 139)/'MOVE',  -1,  12/
      DATA IOPERA( 140),IX( 140),IY( 140)/'DRAW',  -3,  11/
      DATA IOPERA( 141),IX( 141),IY( 141)/'DRAW',  -4,   9/
      DATA IOPERA( 142),IX( 142),IY( 142)/'DRAW',  -4,   7/
      DATA IOPERA( 143),IX( 143),IY( 143)/'DRAW',  -3,   5/
      DATA IOPERA( 144),IX( 144),IY( 144)/'DRAW',  -1,   4/
      DATA IOPERA( 145),IX( 145),IY( 145)/'DRAW',   1,   4/
      DATA IOPERA( 146),IX( 146),IY( 146)/'DRAW',   3,   5/
      DATA IOPERA( 147),IX( 147),IY( 147)/'DRAW',   4,   7/
      DATA IOPERA( 148),IX( 148),IY( 148)/'DRAW',   4,   9/
      DATA IOPERA( 149),IX( 149),IY( 149)/'DRAW',   3,  11/
      DATA IOPERA( 150),IX( 150),IY( 150)/'DRAW',   1,  12/
      DATA IOPERA( 151),IX( 151),IY( 151)/'DRAW',  -1,  12/
      DATA IOPERA( 152),IX( 152),IY( 152)/'MOVE',  -1,  12/
      DATA IOPERA( 153),IX( 153),IY( 153)/'DRAW',  -4,   9/
      DATA IOPERA( 154),IX( 154),IY( 154)/'DRAW',  -3,   5/
      DATA IOPERA( 155),IX( 155),IY( 155)/'DRAW',   1,   4/
      DATA IOPERA( 156),IX( 156),IY( 156)/'DRAW',   4,   7/
      DATA IOPERA( 157),IX( 157),IY( 157)/'DRAW',   3,  11/
      DATA IOPERA( 158),IX( 158),IY( 158)/'DRAW',  -1,  12/
      DATA IOPERA( 159),IX( 159),IY( 159)/'MOVE',   1,  12/
      DATA IOPERA( 160),IX( 160),IY( 160)/'DRAW',  -3,  11/
      DATA IOPERA( 161),IX( 161),IY( 161)/'DRAW',  -4,   7/
      DATA IOPERA( 162),IX( 162),IY( 162)/'DRAW',  -1,   4/
      DATA IOPERA( 163),IX( 163),IY( 163)/'DRAW',   3,   5/
      DATA IOPERA( 164),IX( 164),IY( 164)/'DRAW',   4,   9/
      DATA IOPERA( 165),IX( 165),IY( 165)/'DRAW',   1,  12/
C
      DATA IXMIND(  18)/  -7/
      DATA IXMAXD(  18)/   7/
      DATA IXDELD(  18)/  14/
      DATA ISTARD(  18)/ 139/
      DATA NUMCOO(  18)/  27/
C
C     DEFINE CHARACTER   3247--  (NO   SPACE BLANK)
C
      DATA IOPERA( 166),IX( 166),IY( 166)/'MOVE',   0, -32/
      DATA IOPERA( 167),IX( 167),IY( 167)/'MOVE',   0, -32/
C
      DATA IXMIND(  19)/   0/
      DATA IXMAXD(  19)/   0/
      DATA IXDELD(  19)/   0/
      DATA ISTARD(  19)/ 166/
      DATA NUMCOO(  19)/   2/
C
C     DEFINE CHARACTER   3248--  (HALF SPACE BLANK)
C
      DATA IOPERA( 168),IX( 168),IY( 168)/'MOVE',  -4, -32/
      DATA IOPERA( 169),IX( 169),IY( 169)/'MOVE',   4, -32/
C
      DATA IXMIND(  20)/  -5/
      DATA IXMAXD(  20)/   5/
      DATA IXDELD(  20)/  10/
      DATA ISTARD(  20)/ 168/
      DATA NUMCOO(  20)/   2/
C
C     DEFINE CHARACTER   3249--  (FULL SPACE BLANK)
C
      DATA IOPERA( 170),IX( 170),IY( 170)/'MOVE',  -8, -32/
      DATA IOPERA( 171),IX( 171),IY( 171)/'MOVE',   8, -32/
C
      DATA IXMIND(  21)/ -10/
      DATA IXMAXD(  21)/  10/
      DATA IXDELD(  21)/  20/
      DATA ISTARD(  21)/ 170/
      DATA NUMCOO(  21)/   2/
C
C     DEFINE CHARACTER   3216--  (LEFT  APOSTRAPHE)
C
      DATA IOPERA( 172),IX( 172),IY( 172)/'MOVE',   2,  12/
      DATA IOPERA( 173),IX( 173),IY( 173)/'DRAW',   0,  11/
      DATA IOPERA( 174),IX( 174),IY( 174)/'DRAW',  -1,   9/
      DATA IOPERA( 175),IX( 175),IY( 175)/'DRAW',  -1,   6/
      DATA IOPERA( 176),IX( 176),IY( 176)/'DRAW',   0,   5/
      DATA IOPERA( 177),IX( 177),IY( 177)/'DRAW',   1,   5/
      DATA IOPERA( 178),IX( 178),IY( 178)/'DRAW',   2,   6/
      DATA IOPERA( 179),IX( 179),IY( 179)/'DRAW',   2,   7/
      DATA IOPERA( 180),IX( 180),IY( 180)/'DRAW',   1,   8/
      DATA IOPERA( 181),IX( 181),IY( 181)/'DRAW',   0,   8/
      DATA IOPERA( 182),IX( 182),IY( 182)/'DRAW',  -1,   7/
      DATA IOPERA( 183),IX( 183),IY( 183)/'MOVE',   0,   7/
      DATA IOPERA( 184),IX( 184),IY( 184)/'DRAW',   0,   6/
      DATA IOPERA( 185),IX( 185),IY( 185)/'DRAW',   1,   6/
      DATA IOPERA( 186),IX( 186),IY( 186)/'DRAW',   1,   7/
      DATA IOPERA( 187),IX( 187),IY( 187)/'DRAW',   0,   7/
      DATA IOPERA( 188),IX( 188),IY( 188)/'MOVE',   0,  11/
      DATA IOPERA( 189),IX( 189),IY( 189)/'DRAW',  -1,   7/
      DATA IOPERA( 190),IX( 190),IY( 190)/'MOVE',  -1,   9/
      DATA IOPERA( 191),IX( 191),IY( 191)/'DRAW',   0,   8/
C
      DATA IXMIND(  22)/  -5/
      DATA IXMAXD(  22)/   6/
      DATA IXDELD(  22)/  11/
      DATA ISTARD(  22)/ 172/
      DATA NUMCOO(  22)/  20/
C
C     DEFINE CHARACTER   3217--  (RIGHT APOSTRAPHE)
C
      DATA IOPERA( 192),IX( 192),IY( 192)/'MOVE',   2,  10/
      DATA IOPERA( 193),IX( 193),IY( 193)/'DRAW',   1,   9/
      DATA IOPERA( 194),IX( 194),IY( 194)/'DRAW',   0,   9/
      DATA IOPERA( 195),IX( 195),IY( 195)/'DRAW',  -1,  10/
      DATA IOPERA( 196),IX( 196),IY( 196)/'DRAW',  -1,  11/
      DATA IOPERA( 197),IX( 197),IY( 197)/'DRAW',   0,  12/
      DATA IOPERA( 198),IX( 198),IY( 198)/'DRAW',   1,  12/
      DATA IOPERA( 199),IX( 199),IY( 199)/'DRAW',   2,  11/
      DATA IOPERA( 200),IX( 200),IY( 200)/'DRAW',   2,   8/
      DATA IOPERA( 201),IX( 201),IY( 201)/'DRAW',   1,   6/
      DATA IOPERA( 202),IX( 202),IY( 202)/'DRAW',  -1,   5/
      DATA IOPERA( 203),IX( 203),IY( 203)/'MOVE',   0,  11/
      DATA IOPERA( 204),IX( 204),IY( 204)/'DRAW',   0,  10/
      DATA IOPERA( 205),IX( 205),IY( 205)/'DRAW',   1,  10/
      DATA IOPERA( 206),IX( 206),IY( 206)/'DRAW',   1,  11/
      DATA IOPERA( 207),IX( 207),IY( 207)/'DRAW',   0,  11/
      DATA IOPERA( 208),IX( 208),IY( 208)/'MOVE',   1,   9/
      DATA IOPERA( 209),IX( 209),IY( 209)/'DRAW',   2,   8/
      DATA IOPERA( 210),IX( 210),IY( 210)/'MOVE',   2,  10/
      DATA IOPERA( 211),IX( 211),IY( 211)/'DRAW',   1,   6/
C
      DATA IXMIND(  23)/  -5/
      DATA IXMAXD(  23)/   6/
      DATA IXDELD(  23)/  11/
      DATA ISTARD(  23)/ 192/
      DATA NUMCOO(  23)/  20/
C
C     DEFINE CHARACTER    XXX--| (KEYBOARD VERTICAL BAR)
C
      DATA IOPERA( 212),IX( 212),IY( 212)/'MOVE',   0,  12/
      DATA IOPERA( 213),IX( 213),IY( 213)/'DRAW',   0,  -9/
      DATA IOPERA( 214),IX( 214),IY( 214)/'DRAW',   1,  -9/
      DATA IOPERA( 215),IX( 215),IY( 215)/'MOVE',   0,  12/
      DATA IOPERA( 216),IX( 216),IY( 216)/'DRAW',   1,  12/
      DATA IOPERA( 217),IX( 217),IY( 217)/'DRAW',   1,  -9/
C
      DATA IXMIND(  24)/  -4/
      DATA IXMAXD(  24)/   5/
      DATA IXDELD(  24)/   9/
      DATA ISTARD(  24)/ 212/
      DATA NUMCOO(  24)/   6/
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
      IF(IBUGD2.EQ.'OFF')GOTO90
      WRITE(ICOUT,999)
  999 FORMAT(1X)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,51)
   51 FORMAT('***** AT THE BEGINNING OF DRTS2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,52)ICHARN
   52 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,59)IBUGD2,IFOUND,IERROR
   59 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
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
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'OFF')GOTO9090
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9011)
 9011 FORMAT('***** AT THE END       OF DRTS2--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9012)IBUGD2,IFOUND,IERROR
 9012 FORMAT('IBUGD2,IFOUND,IERROR = ',A4,2X,A4,2X,A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9013)ICHARN
 9013 FORMAT('ICHARN = ',I8)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,9014)ISTART,ISTOP,NC,NUMCO
 9014 FORMAT('ISTART,ISTOP,NC,NUMCO = ',4I8)
      CALL DPWRST('XXX','BUG ')
      IF(NUMCO.GE.1.AND.NUMCO.LE.1000)GOTO9019
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

      LOGICAL(4) FUNCTION INITIALSETTINGS()
C
C
C  APRIL 2018: CURRENTLY SUPPORTED OPTIONS
C
C              -true            - set "true" mode, no longer needed
C              -echo            - echo mode for command line argument
C              -svga            - set frame to width 950 and height 700
C              -large           - set frame to width 1150 and height 1000
C              -filliben        - same as "-large"
C              -laptop          - same width as "-large", but height
C                                 same as "-svga"
C              -extrawide       - set frame to width 1600 and height 1000
C              -w <value>       - set number of horizontal pixels (width)
C                                 for frame
C              -h <value>       - set number of vertical pixels (height)
C                                 for frame
C              -tile            - tile text and graphics windows vertical
C              -notile          - do not tile text and graphics windows vertical
C              -tile_horizontal - tile text and graphics windows
C                                 horizontally
C
CQWIN USE DFWIN
CQWIN USE DFLIB
      USE IFPORT
      USE IFQWIN
      INTEGER I, ISTAT
CCCCC LOGICAL(4) RESULT
      TYPE (QWINFO) QWI
      CHARACTER*80 BUF
      TYPE (WINDOWCONFIG) DPSCREEN
      CHARACTER*4 QWSCRN
      COMMON/QUICKWN/DPSCREEN,QWSCRN,IQWNFT,IQWNFN
      CHARACTER*4 IECHO
      CHARACTER*80 FTEMP(50)
      COMMON/QUICKW2/IECHO,FTEMP
      INTEGER NCFTMP(50)
      COMMON/QUICKW3/NCFTMP,NPCARG
      CHARACTER*4 IQWNTL
      COMMON/QUICKW4/IQWNTL
C
      CHARACTER*4 IQWNFC
      CHARACTER*4 IQWNCL
      CHARACTER*80 IQWNFZ
      CHARACTER*4 IQWNPF
      CHARACTER*4 IQWNPM
      CHARACTER*4 IQWNTC
      COMMON/CQWNCP/IQWNFC,IQWNCL,IQWNFZ,IQWNPF,IQWNTC,IQWNPM(255)
C
C     MAY 2001.     EXTRACT FILE NAME AND ECHO ARGUMENTS OFF COMMAND
C                   LINE FOR PROCESSING IN CKCLAR ROUTINE
C
C     MARCH 2002:   ADD "-true" OPTION TO SPECIFY TRUE COLOR MODE
C     OCTOBER 2002: ADD "-tile" OPTION TO SPECIFY THAT TEXT AND GRAPHICS
C                   WINDOWS WILL BE TILED.
C
C     JUNE 2013.    ADD THE FOLLOWING OPTIONS:
C
C                   1.  -notile
C                   2.  -extrawide
C
C     APRIL 2018.   ADD THE FOLLOWING OPTIONS:
C
C                   1.  -tile_landscape
C
C  SET DEFAULT SIZE OF FRAME WINDOW
C
      QWI.X = 0
      QWI.Y = 0
      QWI.TYPE = QWIN$SET
C
      QWI.W = 950
      QWI.H = 700
      QWSCRN='SVGA'
C
      NUM=IARGC()
      IECHO='    '
      DO5I=1,50
        FTEMP(I)=' '
        NCFTMP(I)=0
    5 CONTINUE
      ICNT=0
CCCCC 2020/10: MAKE RGB THE DEFAULT
CCCCC IQWNCL='VGA '
      IQWNCL='RGB '
      IQWNTL='OFF'
      IF(NUM.GE.1)THEN
        DO10I=0,NUM
          CALL GETARG(I,BUF,ISTAT)
          IF(I.EQ.0)GOTO10
          IF(ISTAT.GE.2 .AND. BUF(1:2).EQ.'-W' .OR.
     &       BUF(1:2).EQ.'-w')THEN
            READ(BUF(3:ISTAT),'(I4.4)',ERR=19) QWI.W
          ELSEIF(ISTAT.GE.2 .AND. BUF(1:2).EQ.'-H' .OR.
     &       BUF(1:2).EQ.'-h')THEN
            READ(BUF(3:ISTAT),'(I4.4)',ERR=19) QWI.H
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-SVGA' .OR.
     &           BUF(1:5).EQ.'-svga')THEN
            QWI.W = 950
            QWI.H = 700
            QWSCRN='SVGA'
          ELSEIF(ISTAT.GE.6 .AND. BUF(1:6).EQ.'-LARGE' .OR.
     &           BUF(1:6).EQ.'-large')THEN
            QWI.W = 1150
            QWI.H = 1000
            QWSCRN='LARG'
          ELSEIF(ISTAT.GE.7 .AND. BUF(1:7).EQ.'-LAPTOP' .OR.
     &           BUF(1:7).EQ.'-laptop')THEN
            QWI.W = 1150
            QWI.H = 700
            QWSCRN='LAPTOP'
          ELSEIF(ISTAT.GE.9 .AND. BUF(1:9).EQ.'-FILLIBEN' .OR.
     &           BUF(1:9).EQ.'-filliben')THEN
            QWI.W = 1150
            QWI.H = 1000
            QWSCRN='LARG'
            IQWNTL='ON'
          ELSEIF(ISTAT.GE.10 .AND. BUF(1:10).EQ.'-EXTRAWIDE' .OR.
     &           BUF(1:10).EQ.'-extrawide')THEN
            QWI.W = 1600
            QWI.H = 1000
            QWSCRN='WIDE'
          ELSEIF(ISTAT.GE.16 .AND.
     &           BUF(1:16).EQ.'-TILE_HORIZONTAL' .OR.
     &           BUF(1:16).EQ.'-tile_horizontal')THEN
            IQWNTL='HORI'
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-TILE' .OR.
     &           BUF(1:5).EQ.'-tile')THEN
            IQWNTL='ON'
          ELSEIF(ISTAT.GE.7 .AND. BUF(1:7).EQ.'-NOTILE' .OR.
     &           BUF(1:7).EQ.'-notile')THEN
            IQWNTL='OFF'
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-TRUE' .OR.
     &           BUF(1:5).EQ.'-true')THEN
            IQWNCL='RGB'
          ELSEIF(ISTAT.GE.7 .AND. BUF(1:5).EQ.'-NOTRUE' .OR.
     &           BUF(1:7).EQ.'-notrue')THEN
            IQWNCL='VGA'
          ELSEIF(ISTAT.GE.4 .AND. BUF(1:4).EQ.'-RGB' .OR.
     &           BUF(1:4).EQ.'-rgb')THEN
            IQWNCL='RGB'
          ELSEIF(ISTAT.GE.6 .AND. BUF(1:6).EQ.'-NORGB' .OR.
     &           BUF(1:6).EQ.'-norgb')THEN
            IQWNCL='VGA'
          ELSEIF(ISTAT.GE.4 .AND. BUF(1:4).EQ.'ECHO' .OR.
     &           BUF(1:4).EQ.'echo')THEN
            IECHO(1:4)='ECHO'
          ELSE
            IF(ISTAT.GE.1 .AND. ISTAT.LE.80 .AND. ICNT.LE.49)THEN
              ICNT=ICNT+1
              NCFTMP(ICNT)=ISTAT
              FTEMP(ICNT)(1:ISTAT)=BUF(1:ISTAT)
            ENDIF
          ENDIF
   10   CONTINUE
   19   CONTINUE
        NPCARG=ICNT
      ENDIF
      I = SetWSizeQQ(QWIN$FRAMEWINDOW, QWI)
CCCCC I = DISPLAYCURSOR($GCURSORON)
      INITIALSETTINGS = .TRUE.
      END FUNCTION INITIALSETTINGS

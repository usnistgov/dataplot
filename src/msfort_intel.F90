      LOGICAL(4) FUNCTION INITIALSETTINGS()
!
!
!  APRIL 2018: CURRENTLY SUPPORTED OPTIONS
!
!              -true            - set "true" mode, no longer needed
!              -echo            - echo mode for command line argument
!              -svga            - set frame to width 950 and height 700
!              -large           - set frame to width 1150 and height 1000
!              -filliben        - same as "-large"
!              -laptop          - same width as "-large", but height
!                                 same as "-svga"
!              -extrawide       - set frame to width 1600 and height 1000
!              -w <value>       - set number of horizontal pixels (width)
!                                 for frame
!              -h <value>       - set number of vertical pixels (height)
!                                 for frame
!              -tile            - tile text and graphics windows vertical
!              -notile          - do not tile text and graphics windows vertical
!              -tile_horizontal - tile text and graphics windows
!                                 horizontally
!
!  JUNE 2021: IF THIS ROUTINE IS CALLED, THIS IMPLIES THAT THE
!             COMMAND LINE VERSION IS BEING EXECUTED.  SO SET
!             IGUIFL TO "OFF".  BUT CHECK FOR "-gui" AND "-nogui"
!             SWITCHES.  NOTE THAT QUICKWIN EXECUTABLE WILL ALWAYS
!             RUN AS IF "-nogui" SWITCH ENTERED.
!
!QWIN USE DFWIN
!QWIN USE DFLIB
      USE IFPORT
      USE IFQWIN
      INTEGER I, ISTAT
!CCCC LOGICAL(4) RESULT
      TYPE (QWINFO) QWI
      CHARACTER*80 BUF
      TYPE (windowconfig) DPSCREEN
      CHARACTER*4 QWSCRN
      COMMON/QUICKWN/DPSCREEN,QWSCRN,IQWNFT,IQWNFN
      CHARACTER*4 IECHO
      CHARACTER*80 FTEMP(50)
      COMMON/QUICKW2/IECHO,FTEMP
      INTEGER NCFTMP(50)
      COMMON/QUICKW3/NCFTMP,NPCARG
      CHARACTER*4 IQWNTL
      COMMON/QUICKW4/IQWNTL
      CHARACTER*4 IQWNFL
      COMMON/QUICKW5/IQWNFL
!
      CHARACTER*4 IQWNFC
      CHARACTER*4 IQWNCL
      CHARACTER*80 IQWNFZ
      CHARACTER*4 IQWNPF
      CHARACTER*4 IQWNPM
      CHARACTER*4 IQWNTC
      COMMON/CQWNCP/IQWNFC,IQWNCL,IQWNFZ,IQWNPF,IQWNTC,IQWNPM(255)
!
      INCLUDE 'DPCOST.INC'
!
!     MAY 2001.     EXTRACT FILE NAME AND ECHO ARGUMENTS OFF COMMAND
!                   LINE FOR PROCESSING IN CKCLAR ROUTINE
!
!     MARCH 2002:   ADD "-true" OPTION TO SPECIFY TRUE COLOR MODE
!     OCTOBER 2002: ADD "-tile" OPTION TO SPECIFY THAT TEXT AND GRAPHICS
!                   WINDOWS WILL BE TILED.
!
!     JUNE 2013.    ADD THE FOLLOWING OPTIONS:
!
!                   1.  -notile
!                   2.  -extrawide
!
!     APRIL 2018.   ADD THE FOLLOWING OPTIONS:
!
!                   1.  -tile_landscape
!
      IQWNFL='ON'
!
!  SET DEFAULT SIZE OF FRAME WINDOW
!
      QWI.X = 0
      QWI.Y = 0
      QWI.TYPE = QWIN$SET
!
      QWI.W = 950
      QWI.H = 700
      QWSCRN='SVGA'
!
      NUM=IARGC()
      IECHO='    '
      DO 5 I=1,50
        FTEMP(I)=' '
        NCFTMP(I)=0
    5 CONTINUE
      ICNT=0
!CCCC 2020/10: MAKE RGB THE DEFAULT
!CCCC IQWNCL='VGA '
      IQWNCL='RGB '
      IQWNTL='OFF'
      IF(NUM.GE.1)THEN
        DO 10 I=0,NUM
          CALL GETARG(I,BUF,ISTAT)
          IF(I.EQ.0)GO TO 10
          IF(ISTAT.GE.2 .AND. BUF(1:2).EQ.'-W' .OR.   &
             BUF(1:2).EQ.'-w')THEN
            READ(BUF(3:ISTAT),'(I4.4)',ERR=19) QWI.W
          ELSEIF(ISTAT.GE.2 .AND. BUF(1:2).EQ.'-H' .OR.   &
             BUF(1:2).EQ.'-h')THEN
            READ(BUF(3:ISTAT),'(I4.4)',ERR=19) QWI.H
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-SVGA' .OR.   &
                 BUF(1:5).EQ.'-svga')THEN
            QWI.W = 950
            QWI.H = 700
            QWSCRN='SVGA'
          ELSEIF(ISTAT.GE.6 .AND. BUF(1:6).EQ.'-LARGE' .OR.   &
                 BUF(1:6).EQ.'-large')THEN
            QWI.W = 1150
            QWI.H = 1000
            QWSCRN='LARG'
          ELSEIF(ISTAT.GE.7 .AND. BUF(1:7).EQ.'-LAPTOP' .OR.   &
                 BUF(1:7).EQ.'-laptop')THEN
            QWI.W = 1150
            QWI.H = 700
            QWSCRN='LAPTOP'
          ELSEIF(ISTAT.GE.9 .AND. BUF(1:9).EQ.'-FILLIBEN' .OR.   &
                 BUF(1:9).EQ.'-filliben')THEN
            QWI.W = 1150
            QWI.H = 1000
            QWSCRN='LARG'
            IQWNTL='ON'
          ELSEIF(ISTAT.GE.10 .AND. BUF(1:10).EQ.'-EXTRAWIDE' .OR.   &
                 BUF(1:10).EQ.'-extrawide')THEN
            QWI.W = 1600
            QWI.H = 1000
            QWSCRN='WIDE'
          ELSEIF(ISTAT.GE.16 .AND.   &
                 BUF(1:16).EQ.'-TILE_HORIZONTAL' .OR.   &
                 BUF(1:16).EQ.'-tile_horizontal')THEN
            IQWNTL='HORI'
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-TILE' .OR.   &
                 BUF(1:5).EQ.'-tile')THEN
            IQWNTL='ON'
          ELSEIF(ISTAT.GE.7 .AND. BUF(1:7).EQ.'-NOTILE' .OR.   &
                 BUF(1:7).EQ.'-notile')THEN
            IQWNTL='OFF'
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-TRUE' .OR.   &
                 BUF(1:5).EQ.'-true')THEN
            IQWNCL='RGB'
          ELSEIF(ISTAT.GE.7 .AND. BUF(1:5).EQ.'-NOTRUE' .OR.   &
                 BUF(1:7).EQ.'-notrue')THEN
            IQWNCL='VGA'
          ELSEIF(ISTAT.GE.4 .AND. BUF(1:4).EQ.'-RGB' .OR.   &
                 BUF(1:4).EQ.'-rgb')THEN
            IQWNCL='RGB'
          ELSEIF(ISTAT.GE.6 .AND. BUF(1:6).EQ.'-NORGB' .OR.   &
                 BUF(1:6).EQ.'-norgb')THEN
            IQWNCL='VGA'
          ELSEIF(ISTAT.GE.4 .AND. BUF(1:4).EQ.'ECHO' .OR.   &
                 BUF(1:4).EQ.'echo')THEN
            IECHO(1:4)='ECHO'
          ELSEIF(ISTAT.GE.3 .AND. BUF(1:3).EQ.'GUI' .OR.   &
                 BUF(1:3).EQ.'gui')THEN
            IGUIFL='OFF'
          ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'NOGUI' .OR.   &
                 BUF(1:5).EQ.'nogui')THEN
            IGUIFL='OFF'
          ELSE
            IF(ISTAT.GE.1 .AND. ISTAT.LE.80 .AND. ICNT.LE.49)THEN
              ICNT=ICNT+1
              ISTAT=MIN(80,ISTAT)
              NCFTMP(ICNT)=ISTAT
              FTEMP(ICNT)(1:ISTAT)=BUF(1:ISTAT)
            ENDIF
          ENDIF
   10   CONTINUE
   19   CONTINUE
        NPCARG=ICNT
      ENDIF
      I = SetWSizeQQ(QWIN$FRAMEWINDOW, QWI)
!CCCC I = DISPLAYCURSOR($GCURSORON)
      INITIALSETTINGS = .TRUE.
      END FUNCTION INITIALSETTINGS

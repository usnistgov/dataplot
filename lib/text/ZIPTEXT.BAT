REM THIS IS FILE   ZIPTEXT.BAT
REM PURPOSE--ZIP ALL    TEXT    FILES FOR DATAPLOT
REM DATE--SEPTEMBER 1993
REM DATE--JUNE      1995
REM DATE--JULY      1995
 
REM -----ZIP DATAPLOT'S TEXT FILES (==> TEXTFILE.EXE)-----
 
DEL      TEXTFILE.ZIP
DEL      TEXTFILE.EXE
 
PKZIP    TEXTFILE.ZIP *.*
ZIPTOEXE TEXTFILE.ZIP

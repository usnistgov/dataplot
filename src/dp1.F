C     dp1.f  - This file contains subroutines that may need to be
C              modified for a specific operating system.
C
C              In previous versions we have maintained separate files
C              for different operating systems/compilers.  Starting
C              with the 2018/04 version, we are replacing this with
C              pre-processor directives (#if/#endif constructs).
C
C              The following are the pre-processor directives for the
C              Windows platform.
C
C                 1. INTEL - code that is used by both the QuickWin and
C                    console versions of the Intel compiler.
C
C                 2. INTEL_QWIN - code that is used only by the Intel
C                    QuickWin version.
C
C                 3. INTEL_CONS - code that is used only by the Intel
C                    console version.
C
C                 4. INTEL_64 - use machine constants that assume 64-bits
C                    for both single precision and double precision (this
C                    is currently what we use for the production version
C                    of Dataplot).
C
C                 4. INTEL_32 - use machine constants that assume 32 bits
C                    for single precision and 64 bits for double precision.
C
C                 5. INTEL_128 - use machine constants that assume 64 bits
C                    for single precision and 128 bits for double
C                    precision.  Should only be used for 64-bit machines.
C                    Currently, a number of the special function routines
C                    have code that assume double precision is 64 bits, so
C                    this is not recommended.  When I get these routines
C                    straightend out, will make this the default for 64-bit
C                    machines.
C
C                 6. WIN32_LAHEY - code specific to the Lahey compiler.  I don't
C                    currently have a working version for Lahey, so this
C                    reserved for future use.
C
C                 7. WIN32_ABSOFT - code specific to the Absoft compiler.  I
C                    don't currently have a working version for Absoft, so
C                    this reserved for future use.
C
C              The following are the pre-processor directives for the
C              Linux platform.
C
C                 1. LINUX - specifies this is a Linux installation.
C
C                 2. I32 - use 32-bits for integers.
C
C                 3. I64 - use 64-bits for integers.
C
C                 4. SD  - use 32-bit for single precision real numbers
C
C                 5. DD  - use 64-bit for single precision real numbers
C
C                 6. DQ  - use 64-bit for single precision real numbers
C                          and 128-bit for double precision numbers.
C
C              The following are miscellaneous directives.
C
C                 1. HAVE_EXECUTE_COMMAND_LINE - the
C                    EXECUTE_COMMAND_LINE syntax was added in the
C                    Fortran 2008 standard to execute operating system
C                    commands. Support was added in the GNU gfortran
C                    compiler starting with version 4.6.  It is also
C                    supported in newer versions of the Intel Fortran
C                    compiler.  Since some sites may not have the
C                    newer compilers, this option was provided to
C                    allow this to be turned on or off as needed.
C
C                 2. HAVE_ISNAN - specify whether the ISNAN function is
C                    available.  This is primarily for Linux gfortran
C                    (versions 4.1.2 and older do not support it,
C                    versions newer than this do).
C
C                 3. Added some platform specific directives for older
C                    operating systems (e.g., VAX_VMS, NOS_VE).  These
C                    systems are generally obsolete, so they will
C                    typically not be defined.
C
C     The specific routines in this file are:
C
C      1. INITMC   - define machine constants for a specific platform
C      2. INITFO/  - specify certain file names for a specific platform
C         INITF2
C      3. DPOPFI   - open a file (typically does not need to be modified)
C      4. CKCLAR   - check for command line arguments
C      5. DPSYS2   - execute an operating system command
C      6. DPTIME   - time and date
C      7. DPEDIT   - main driver routine for the FED command (i.e., the
C                    built-in line editor)
C      8. DPWRST   - write to the terminal window
C      9. GRWRST   - write to the graphics device
C     10. DPSLEE   - implement the "sleep" command
C         DPSLE2   - similar to DPSLEE, but for internal use
C     11. DPFLSH   - flush an output file
C     12. RANLP/   - various random number generators
C         RANFT/
C         R250IN/
C         RND250
C     13. DPPID2   - return the process id
C     14. DPINF2   - check existence of a file
C     15. DPPRMP   - specify "advance" behavior for the command prompt
C     16. DPCPUT   - CPU time/usage
C     17. IQWNHE/  - implement some functionality for the
C         IQWNBR     Quick Win menu items
C     18. ISNANZ   - test for "not a number"
C     19. DPCLIP   - read from system clipboard
C     20. DPCLI2   - view/copy/run from system clipboard
C     21. DPCLI3   - clear clipboard
C     22. DPCLI4   - write strings/parameters to clipboard
C     23. DPCLI5   - write variables to clipboard
C     24. DPCLI6   - copy file to clipboard
C     25. DPCDIR   - change the current directory
C     26. DPPWD    - return current working directory
C     27. DPRM     - remove a file or a directory
C     28. DPMKDR   - make a new directory
C     29. DPCAT    - list contents of a file
C     30. DPDIR    - list files (Windows DIR or Linux ls command)
C
      SUBROUTINE INITMC(IBUGIN)
C
C     PURPOSE--DEFINE MACHINE CONSTANTS (INTEGER, REAL, AND
C              DOUBLE PRECISION) FOR A PARTICULAR COMPUTER,
C     NOTE--THIS SUBROUTINE DOES NOT ADHERE TO 1966 ANSI STANDARD
C           OR THE 1977 ANSI STANDARD
C           BECAUSE IT USES OCTAL CONSTANTS.
C     NOTE--TO ALTER THIS SUBROUTINE FOR A PARTICULAR ENVIRONMENT,
C     THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
C     REMOVING THE C FROM COLUMN 1.
C     NOTE--FOR IMPLEMENTATION CONVIENENCE, THE COMMENT LINES THAT
C     NEED TO BE DEACTIVATED ARE CODED AS "CXXXX" WHERE XXXX DEFINES
C     A PARTICULAR MACHINE.  THIS MEANS A SINGLE GLOBAL REPLACE CAN
C     BE USED TO UNCOMMENT THE APPROPRIATE LINES FOR A PARTICULAR
C     MACHINE (E.G., CHANGE '     ' TO '    ').
C     THE FOLLOWING CODES ARE USED:
C     APPO - APOLLO
C     BURR - BURROUGHS 1700
C     BUR2 - BURROUGHS 5700
C     BUR3 - BURROUGHS 6700
C     NVE  - CDC USING NOS/VE
C     205  - CDC 205 USING VSOS
C     CRAY - CRAY
C     DG   - DATA GENERAL ECLIPSE
C     HARR - HARRIS 220
C     HONE - HONEYWELL 600/6000
C     HP1  - HP 2100 FTN4
C     HP2  - HP 2100 FTN4
C     HP9  - HP 9000 (UNIX)
C     IBM  - IBM 370
C     PDP1 - PDP-10 (KA PROCESSOR)
C     PDP2 - PDP-10 (KI PROCESSOR)
C     PDP3 - PDP-11 (32 BIT)
C     PDP4 - PDP-11 (16 BIT)
C     PRIM - PRIME
C     UNIV - UNIVAC WITH FTN (I.E., 77 COMPILER)
C     UNI2 - UNIVAC WITH FOR (I.E., 66 COMPILER, NO LONGER SUPPORTED)
C     IBM- - IBM-PC USING 16 BIT DOS, 8087 CO-PROCESSOR
C     OS2  - IBM-PC USING OS/2 (32 BIT 386 USING OTG COMPILER)
C     MACI - MACINTOSH
C     SUN  - SUN (UNIX, CAN BE USED BY OTHER UNIX MACHINES, E.G. THE
C            SILICON GRAPHICS IRIS AND THE HP-9000).
C     CON1 - CONVEX (NATIVE MODE, WITHOUT -R8 OPTION)
C     CON2 - CONVEX (NATIVE MODE, WITH -R8 OPTION)
C     CON3 - CONVEX (IEEE MODE, WITHOUT -R8 OPTION)
C     CON4 - CONVEX (IEEE MODE, WITH -R8 OPTION)
C
C     NOTE--THIS SUBROUTINE IS IDENTICAL TO THE DPMACH SUBROUTINE.
C
C               **************************************************
C               **  DESCRIPTION OF INTEGER MACHINE CONSTANTS    **
C               **************************************************
C
C     TO DESCRIBE I/O UNIT NUMBERS--
C
C       I1MACH( 1) = THE STANDARD INPUT UNIT.
C       I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C       I1MACH( 3) = THE STANDARD PUNCH UNIT.
C       I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C
C     TO DESCRIBE WORDS--
C
C       I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C       I1MACH( 6) = THE NUMBER OF CHARACTERS PER INTEGER STORAGE UNIT.
C
C     TO DESCRIBE INTEGERS--
C
C       ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
C                  SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C                  WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
C
C       I1MACH( 7) = A, THE BASE.
C       I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C       I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C
C     TO DESCIBE FLOATING-POINT NUMBERS--
C
C       ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C       BASE-B FORM
C                  SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C                  WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
C                  0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
C
C       I1MACH(10) = B, THE BASE.
C
C     TO DESCIBE SINGLE-PRECISION--
C
C       I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C       I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C       I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C
C     TO DESCRIBE DOUBLE-PRECISION--
C
C       I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C       I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C       I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
C     THE VALUES OF
C     I1MACH(1) TO I1MACH(4) SHOULD BE CHECKED FOR CONSISTENCY
C     WITH THE LOCAL OPERATING SYSTEM.
C
C               *************************************************************
C               **  DESCRIPTION OF REAL (FLOATING POINT) MACHINE CONSTANTS  *
C               *************************************************************
C
C     R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C     R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C     R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C     R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C     R1MACH(5) = LOG10(B)
C
C     WHERE POSSIBLE, OCTAL OR HEXADECIMAL CONSTANTS HAVE BEEN USED
C     TO SPECIFY THE CONSTANTS EXACTLY WHICH HAS IN SOME CASES
C     REQUIRED THE USE OF EQUIVALENT INTEGER ARRAYS.
C
C               *********************************************************
C               **  DESCRIPTION OF DOUBLE PRECISION MACHINE CONSTANTS  **
C               *********************************************************
C
C     D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
C     D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
C     D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
C     D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
C     D1MACH( 5) = LOG10(B)
C
C     WHERE POSSIBLE, OCTAL OR HEXADECIMAL CONSTANTS HAVE BEEN USED
C     TO SPECIFY THE CONSTANTS EXACTLY WHICH HAS IN SOME CASES
C     REQUIRED THE USE OF EQUIVALENT INTEGER ARRAYS.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--THIS SUBROUTINE IS A MODIFICATION OF CODE
C           PROVIDED IN THE FOLLOWING ARTICLE--
C           CACM, 19XX.
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--82.6
C     ORIGINAL VERSION--SEPTEMBER 1980
C     UPDATED         --JULY      1981.
C     UPDATED         --AUGUST    1981.
C     UPDATED         --MAY       1982.
C     UPDATED         --JULY      1986 (IBM-PC AND MACINTOSH)
C     UPDATED         --OCTOBER   1986 (SUN)
C     UPDATED         --FEBRUARY  1988.  DIFFERENT GRAPHICS & ALPHA I/O (ALAN)
C     UPDATED         --FEBRUARY  1988.  UPDATED CYBER CONSTANTS (ALAN)
C     UPDATED         --JUNE      1989.  IBM-PC OS/2 & COMPAQ 386 CONSTANTS
C     UPDATED         --JUNE      1989.  INTEGER*2 (COMPAQ ERROR MESSAGE)
C     UPDATED         --JUNE      1990.  CODED COMMENTS FOR EASY "GLOBAL" EDIT
C                                        MOVE DATA AFTER EXECUTABLE
C     UPDATED         --AUGUST    1990.  (CONVEX, 4 DIFFERENT MODES, FROM CMLIB)
C     UPDATED         --APRIL     1992.  SAVE STATEMENTS
C     UPDATED         --APRIL     1992.  IHMOD1='386 '
C     UPDATED         --APRIL     1992.  ICOMPI='OTG '
C     UPDATED         --MAY       1992.  D.P. OVERFLOW PROBLEMS
C     UPDATED         --OCTOBER   1994.  FIX IBM-PC CONSTANTS
C     UPDATED         --OCTOBER   1996.  MS-FORTAN SUPPORT
C     UPDATED         --FEBRUARY  2006.  DOUBLE PRECISION VERSION
C                                        OF INTEL WINDOWS COMPILER
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
CCCCC FOLLOWING LINES FOR MICROSOFT FORTRAN VERSION
CCCCC USE DFLIB
CCCCC USE DFWIN
#ifdef INTEL_QWIN
      USE IFPORT
      USE IFQWIN
      TYPE (FONTINFO) MSFONT
CCCCC TYPE (QWINFO)  WINFO
      LOGICAL MODESTATUS
      TYPE (WINDOWCONFIG)   DPSCREEN
      CHARACTER*4 QWSCRN
      COMMON/QUICKWN/DPSCREEN,QWSCRN,IQWNFT,IQWNFN
CCCCC LOGICAL(4) RESZZ
      INTERFACE
        LOGICAL(4) FUNCTION INITIALSETTINGS
        END FUNCTION
      END INTERFACE
      EXTERNAL IQWNHE
      EXTERNAL IQWNBR
CCCCC EXTERNAL IQWNAB
#endif
#ifdef INTEL_CONS
      USE IFPORT
      INTERFACE
         FUNCTION DPABOR(SIGNUM)
         !DEC$ ATTRIBUTES C :: DPABOR
         INTEGER(4) DPABOR
         INTEGER(2) SIGNUM
      END FUNCTION
      END INTERFACE
#endif
C
      CHARACTER*4 IBUGIN
C
      INCLUDE 'DPCODV.INC'
C
      PARAMETER(MAXCLR=89)
      INTEGER IRED(MAXCLR), IBLUE(MAXCLR), IGREEN(MAXCLR)
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION D2MACH(5)
C
CCCCC THE FOLLOWING 12 LINES WERE ADDED  APRIL 1992
C
      SAVE R2MACH
      SAVE ISMALL
      SAVE ILARGE
      SAVE IRIGHT
      SAVE IDIVER
      SAVE ILOG10
C
      SAVE D2MACH
      SAVE JSMALL
      SAVE JLARGE
      SAVE JRIGHT
      SAVE JDIVER
      SAVE JLOG10
C
      DIMENSION ISMALL(2)
      DIMENSION ILARGE(2)
      DIMENSION IRIGHT(2)
      DIMENSION IDIVER(2)
      DIMENSION ILOG10(2)
C
      DIMENSION JSMALL(4)
      DIMENSION JLARGE(4)
      DIMENSION JRIGHT(4)
      DIMENSION JDIVER(4)
      DIMENSION JLOG10(4)
C
      DIMENSION I2MACH(16)
C
      DIMENSION R2MACH(5)
C
      EQUIVALENCE (R2MACH(1),ISMALL(1))
      EQUIVALENCE (R2MACH(2),ILARGE(1))
      EQUIVALENCE (R2MACH(3),IRIGHT(1))
      EQUIVALENCE (R2MACH(4),IDIVER(1))
      EQUIVALENCE (R2MACH(5),ILOG10(1))
C
      EQUIVALENCE (D2MACH(1),JSMALL(1))
      EQUIVALENCE (D2MACH(2),JLARGE(1))
      EQUIVALENCE (D2MACH(3),JRIGHT(1))
      EQUIVALENCE (D2MACH(4),JDIVER(1))
      EQUIVALENCE (D2MACH(5),JLOG10(1))
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOHO.INC'
CCCCC THE FOLLOWING LINE WAS INSERTED FOR GR & ALPHA UNITS  FEBRUARY 1989
      INCLUDE 'DPCOGR.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C  DEFINE RGB TABLES FOR QUICKWIN
C
      INCLUDE 'DPCOCT.INC'
C
C---------------------------------------------------------------------
C -----DATA STATEMENTS---------------------------------------------
CC
CC              *******************************************************
CC              **  MACHINE CONSTANTS FOR THE APOLLO                 **
C               **  MY THANKS TO NORM SHELLEY FOR THIS CONTRIBUTION  **
C               **  (JANUARY, 1985).  THESE VALUES ARE TENTATIVE     **
CC              **  AND HAVE NOT BEEN CHECKED.                       **
CC              *******************************************************
CC
CC
#ifdef APOLLO
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    7 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
CC    DOES APOLLO NORMALIZE THEIR FRACTION LIKE A    VAX?
CC    IF SO, CHANGE THE FOLLOWING 23 TO 24
CC    ASK APOLLO HOW THEY DO THEIR NUMBERS
      DATA I2MACH(11) /   23 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   52 /
      DATA I2MACH(15) / -1024/
      DATA I2MACH(16) /  1023/
CC
CC    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
CC    (THAT IS 2**-23 AND 2**22)
CC    FOR THE NUMBERS BELOW,
      DATA R2MACH(1) / 1.175495E-38 /
      DATA R2MACH(2) / 3.402823E38 /
      DATA R2MACH(3) / 1.1920928955078E-7 /
      DATA R2MACH(4) / 2.3841857910156E-7 /
      DATA R2MACH(5) / 0.3010300 /
CC
CC    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
CC    (THAT IS 2**-23 AND 2**22)
CC    FOR THE NUMBERS BELOW,
      DATA D2MACH(1) / 2.22507385850721D-308 /
      DATA D2MACH(2) / 1.79769313486231D308 /
      DATA D2MACH(3) / 1.1102230246252D-16 /
      DATA D2MACH(4) / 2.2204460492503D-16 /
      DATA D2MACH(5) / 0.3010299956639812 /
CC
      IHOST1='APOL'
      IHOST2='    '
      IHMOD1='DOMA'
      IHMOD2='    '
      IOPSY1='AEGI'
      IOPSY2='    '
      ICOMPI='FTN '
      ISITE='    '
#endif
CC
CC              ********************************************************
CC              **  MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.  **
CC              ********************************************************
CC
#ifdef BURROUGHS
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='1700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
CC
      DATA I2MACH( 1) /    7 /
      DATA I2MACH( 2) /    2 /
      DATA I2MACH( 3) /    2 /
      DATA I2MACH( 4) /    2 /
      DATA I2MACH( 5) /   36 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   33 /
      DATA I2MACH( 9) / Z1FFFFFFFF /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -256 /
      DATA I2MACH(13) /  255 /
      DATA I2MACH(14) /   60 /
      DATA I2MACH(15) / -256 /
      DATA I2MACH(16) /  255 /
CC
      DATA R2MACH(1) / Z400800000 /
      DATA R2MACH(2) / Z5FFFFFFFF /
      DATA R2MACH(3) / Z4E9800000 /
      DATA R2MACH(4) / Z4EA800000 /
      DATA R2MACH(5) / Z500E730E8 /
CC
      DATA JSMALL(1) / ZC00800000 /
      DATA JSMALL(2) / Z000000000 /
      DATA JLARGE(1) / ZDFFFFFFFF /
      DATA JLARGE(2) / ZFFFFFFFFF /
      DATA JRIGHT(1) / ZC 5800000 /
      DATA JRIGHT(2) / Z000000000 /
      DATA JDIVER(1) / ZC 6800000 /
      DATA JDIVER(2) / Z000000000 /
      DATA JLOG10(1) / ZD00E730E7 /
      DATA JLOG10(2) / ZC77800DC0 /
CC
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='1700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ********************************************************
CC              **  MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.  **
CC              ********************************************************
CC
#ifdef BURROUGHS_2
      DATA I2MACH( 1) /   5 /
      DATA I2MACH( 2) /   6 /
      DATA I2MACH( 3) /   7 /
      DATA I2MACH( 4) /   6 /
      DATA I2MACH( 5) /  48 /
      DATA I2MACH( 6) /   6 /
      DATA I2MACH( 7) /   2 /
      DATA I2MACH( 8) /  39 /
      DATA I2MACH( 9) / O0007777777777777 /
      DATA I2MACH(10) /   8 /
      DATA I2MACH(11) /  13 /
      DATA I2MACH(12) / -50 /
      DATA I2MACH(13) /  76 /
      DATA I2MACH(14) /  26 /
      DATA I2MACH(15) / -50 /
      DATA I2MACH(16) /  76 /
CC
      DATA R2MACH(1) / O1771000000000000 /
      DATA R2MACH(2) / O0777777777777777 /
      DATA R2MACH(3) / O1311000000000000 /
      DATA R2MACH(4) / O1301000000000000 /
      DATA R2MACH(5) / O1157163034761675 /
CC
      DATA JSMALL(1) / O1771000000000000 /
      DATA JSMALL(2) / O0000000000000000 /
      DATA JLARGE(1) / O0777777777777777 /
      DATA JLARGE(2) / O0007777777777777 /
      DATA JRIGHT(1) / O1461000000000000 /
      DATA JRIGHT(2) / O0000000000000000 /
      DATA JDIVER(1) / O1451000000000000 /
      DATA JDIVER(2) / O0000000000000000 /
      DATA JLOG10(1) / O1157163034761674 /
      DATA JLOG10(2) / O0006677466732724 /
CC
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='5700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              **************************************************************
CC              **  MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.  **
CC              **************************************************************
CC
#ifdef BURROUGHS_3
      DATA I2MACH( 1) /   5 /
      DATA I2MACH( 2) /   6 /
      DATA I2MACH( 3) /   7 /
      DATA I2MACH( 4) /   6 /
      DATA I2MACH( 5) /  48 /
      DATA I2MACH( 6) /   6 /
      DATA I2MACH( 7) /   2 /
      DATA I2MACH( 8) /  39 /
      DATA I2MACH( 9) / O0007777777777777 /
      DATA I2MACH(10) /   8 /
      DATA I2MACH(11) /  13 /
      DATA I2MACH(12) / -50 /
      DATA I2MACH(13) /  76 /
      DATA I2MACH(14) /  26 /
      DATA I2MACH(15) / -32754 /
      DATA I2MACH(16) /  32780 /
CC
      DATA R2MACH(1) / O1771000000000000 /
      DATA R2MACH(2) / O0777777777777777 /
      DATA R2MACH(3) / O1311000000000000 /
      DATA R2MACH(4) / O1301000000000000 /
      DATA R2MACH(5) / O1157163034761675 /
CC
      DATA JSMALL(1) / O1771000000000000 /
      DATA JSMALL(2) / O7770000000000000 /
      DATA JLARGE(1) / O0777777777777777 /
      DATA JLARGE(2) / O7777777777777777 /
      DATA JRIGHT(1) / O1461000000000000 /
      DATA JRIGHT(2) / O0000000000000000 /
      DATA JDIVER(1) / O1451000000000000 /
      DATA JDIVER(2) / O0000000000000000 /
      DATA JLOG10(1) / O1157163034761674 /
      DATA JLOG10(2) / O0006677466732724 /
CC
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='6700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              *******************************************************
CC              **  MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.  **
CC              *******************************************************
CC
C
C     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
C     FROM SANDIA LABS
C
C
#ifdef NOS_VE
       DATA I2MACH( 1) /     5 /
       DATA I2MACH( 2) /     6 /
       DATA I2MACH( 3) /     7 /
       DATA I2MACH( 4) /     6 /
       DATA I2MACH( 5) /    64 /
       DATA I2MACH( 6) /     8 /
       DATA I2MACH( 7) /     2 /
       DATA I2MACH( 8) /    63 /
       DATA I2MACH( 9) / 9223372036854775807 /
       DATA I2MACH(10) /     2 /
       DATA I2MACH(11) /    47 /
       DATA I2MACH(12) / -4095 /
       DATA I2MACH(13) /  4094 /
       DATA I2MACH(14) /    94 /
       DATA I2MACH(15) / -4095 /
       DATA I2MACH(16) /  4094 /
CC
       DATA R2MACH(1) / Z"3001800000000000" /
       DATA R2MACH(2) / Z"4FFEFFFFFFFFFFFE" /
       DATA R2MACH(3) / Z"3FD2800000000000" /
       DATA R2MACH(4) / Z"3FD3800000000000" /
       DATA R2MACH(5) / Z"3FFF9A209A84FBCF" /
CC
       DATA JSMALL(1) / Z"3001800000000000" /
       DATA JSMALL(2) / Z"3001000000000000" /
       DATA JLARGE(1) / Z"4FFEFFFFFFFFFFFE" /
       DATA JLARGE(2) / Z"4FFE000000000000" /
       DATA JRIGHT(1) / Z"3FD2800000000000" /
       DATA JRIGHT(2) / Z"3FD2000000000000" /
       DATA JDIVER(1) / Z"3FD3800000000000" /
       DATA JDIVER(2) / Z"3FD3000000000000" /
       DATA JLOG10(1) / Z"3FFF9A209A84FBCF" /
       DATA JLOG10(2) / Z"3FFFF7988F8959AC" /
CC
       IHOST1='NVE '
       IHOST2='    '
       IHMOD1='855 '
       IHMOD2='    '
       IOPSY1='NVE '
       IOPSY2='    '
       ICOMPI='FTN5'
       ISITE='NBS '
CC
CC  NOTE: 5/88.  FOR LEVEL 1.3.1, NEED TO SPECIFY "$LOCAL" AS THE CATALOG
CC        FOR THE INPUT AND OUTPUT FILES.  (UNITS 4, 5, 6, AND 7 ARE USED
CC        FOR TERMINAL I/O.  OTHERWISE, WILL USE THE DEFAULT CATALOG.
CC        4 - GRAPHICS INPUT
CC        5 - ALPHANUMERIC INPUT
CC        6 - GRAPHICS OUTPUT
CC        7 - ALPHANUMERIC OUTPUT.
CC        NOS/VE REQUIRES DIFFERENT UNITS FOR GRAPHICS AND ALPHANUMERIC
CC        I/O SINCE GRAPHICS I/O MUST BE IN "TRANSPARENT" MODE.
CC        NOTE THAT THE PROCEDURE ON NOS/VE THAT EXECUTES DATAPLOT WILL
CC        HANDLE CONNECTING THESE UNITS TO THE TERMINAL.
CC
      CALL SCLCMD('CREATE_VARIABLE N=STV_ZZZZZZ KIND=STATUS')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE4 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE5 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE6 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE7 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('REQUEST_TERMINAL $LOCAL.TAPE6 IEM=TRANSPARENT
     * STATUS=STV_ZZZZZZ ')
      CALL SCLCMD('REQUEST_TERMINAL $LOCAL.TAPE4 IEM=TRANSPARENT '//
     * 'TCM=F TTC=$CHAR(255) TFC=$CHAR(13) BKA=2 IOM=S TLM=N TTM=N '//
     * 'STATUS=STV_ZZZZZZ')
      OPEN(UNIT=4,FILE='$LOCAL.TAPE4')
      OPEN(UNIT=5,FILE='$INPUT')
      OPEN(UNIT=6,FILE='$LOCAL.TAPE6')
      OPEN(UNIT=7,FILE='$OUTPUT')
      CALL SCLCMD('DELETE_VARIABLE STV_ZZZZZZ')
#endif
CC
CC
CC              *********************************************************
CC              **  MACHINE CONSTANTS FOR THE CDC CYBER 200 SERIES.    **
CC              **  (WITH THANKS TO MARY BETH ALGEO, NBS  AUG., 1986   **
CC              *********************************************************
CC
CC
#ifdef CDC_205
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    7 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   64 /
      DATA I2MACH( 6) /    8 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   47 /
      DATA I2MACH( 9) / X'00007FFFFFFFFFFF' /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   47 /
      DATA I2MACH(12) / -28625 /
      DATA I2MACH(13) /  28718 /
      DATA I2MACH(14) /   94 /
      DATA I2MACH(15) / -28625 /
      DATA I2MACH(16) /  28718 /
CC
      DATA R2MACH(1) / X'9000400000000000' /
      DATA R2MACH(2) / X'6FFF7FFFFFFFFFFF' /
      DATA R2MACH(3) / X'FFA3400000000000' /
      DATA R2MACH(4) / X'FFA4400000000000' /
      DATA R2MACH(5) / X'FFD04D104D427DE8' /
CC
      DATA JSMALL(1) / X'9000400000000000' /
      DATA JSMALL(2) / X'8FD1000000000000' /
      DATA JLARGE(1) / X'6FFF7FFFFFFFFFFF' /
      DATA JLARGE(2) / X'6FD07FFFFFFFFFFF' /
      DATA JRIGHT(1) / X'FF74400000000000' /
      DATA JRIGHT(2) / X'FF45000000000000' /
      DATA JDIVER(1) / X'FF75400000000000' /
      DATA JDIVER(2) / X'FF46000000000000' /
      DATA JLOG10(1) / X'FFD04D104D427DE7' /
      DATA JLOG10(2) / X'FFA17DE623E2566A' /
CC
      IHOST1='205 '
      IHOST2='    '
      IHMOD1='205 '
      IHMOD2='    '
      IOPSY1='VSOS'
      IOPSY2='2.2 '
      ICOMPI='    '
      ISITE='    '
#endif
CC              ****************************************
CC              **  MACHINE CONSTANTS FOR THE CRAY 1  **
CC              ****************************************
CC
CC
#ifdef CRAY
      DATA I2MACH( 1) /   100 /
      DATA I2MACH( 2) /   101 /
      DATA I2MACH( 3) /   102 /
      DATA I2MACH( 4) /   101 /
      DATA I2MACH( 5) /    64 /
      DATA I2MACH( 6) /     8 /
      DATA I2MACH( 7) /     2 /
      DATA I2MACH( 8) /    63 /
      DATA I2MACH( 9) /  777777777777777777777B /
      DATA I2MACH(10) /     2 /
      DATA I2MACH(11) /    48 /
      DATA I2MACH(12) / -8192 /
      DATA I2MACH(13) /  8191 /
      DATA I2MACH(14) /    96 /
      DATA I2MACH(15) / -8192 /
      DATA I2MACH(16) /  8191 /
CC
      DATA R2MACH(1) / 200004000000000000000B /
      DATA R2MACH(2) / 577777777777777777777B /
      DATA R2MACH(3) / 377214000000000000000B /
      DATA R2MACH(4) / 377224000000000000000B /
      DATA R2MACH(5) / 377774642023241175720B /
CC
      DATA JSMALL(1) / 200004000000000000000B /
      DATA JSMALL(2) / 00000000000000000000B /
      DATA JLARGE(1) / 577777777777777777777B /
      DATA JLARGE(2) / 000007777777777777777B /
      DATA JRIGHT(1) / 377214000000000000000B /
      DATA JRIGHT(2) / 000000000000000000000B /
      DATA JDIVER(1) / 377224000000000000000B /
      DATA JDIVER(2) / 000000000000000000000B /
      DATA JLOG10(1) / 377774642023241175717B /
      DATA JLOG10(2) / 000007571421742254654B /
CC
      IHOST1='CRAY'
      IHOST2='    '
      IHMOD1='1'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC              ************************************************************
CC              **  MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200  **
CC              ************************************************************
CC
#ifdef DATA_GENERAL
      DATA I2MACH( 1) /   11 /
      DATA I2MACH( 2) /   12 /
      DATA I2MACH( 3) /    8 /
      DATA I2MACH( 4) /   10 /
      DATA I2MACH( 5) /   16 /
      DATA I2MACH( 6) /    2 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   15 /
      DATA I2MACH( 9) /32767 /
      DATA I2MACH(10) /   16 /
      DATA I2MACH(11) /    6 /
      DATA I2MACH(12) /  -64 /
      DATA I2MACH(13) /   63 /
      DATA I2MACH(14) /   14 /
      DATA I2MACH(15) /  -64 /
      DATA I2MACH(16) /   63 /
CC
      NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
      STATIC R2MACH(5)
CC
      DATA ISMALL/20K,0/,ILARGE/77777K,177777K/
      DATA IRIGHT/35420K,0/,IDIVER/36020K,0/
      DATA ILOG10/40423K,42023K/
CC
      NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
      STATIC D2MACH(5)
CC
      DATA JSMALL/20K,3*0/,JLARGE/77777K,3*177777K/
      DATA JRIGHT/31420K,3*0/,JDIVER/32020K,3*0/
      DATA JLOG10/40423K,42023K,50237K,74776K/
CC
      IHOST1='DG'
      IHOST2='    '
      IHMOD1='ECLI'
      IHMOD2='200'
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ********************************************
CC              **  MACHINE CONSTANTS FOR THE HARRIS 220  **
CC              ********************************************
CC
#ifdef HARRIS_220
      DATA I2MACH( 1) /       5 /
      DATA I2MACH( 2) /       6 /
      DATA I2MACH( 3) /       0 /
      DATA I2MACH( 4) /       6 /
      DATA I2MACH( 5) /      24 /
      DATA I2MACH( 6) /       3 /
      DATA I2MACH( 7) /       2 /
      DATA I2MACH( 8) /      23 /
      DATA I2MACH( 9) / 8388607 /
      DATA I2MACH(10) /       2 /
      DATA I2MACH(11) /      23 /
      DATA I2MACH(12) /    -127 /
      DATA I2MACH(13) /     127 /
      DATA I2MACH(14) /      38 /
      DATA I2MACH(15) /    -127 /
      DATA I2MACH(16) /     127 /
CC
      DATA ISMALL(1),ISMALL(2) / '20000000, '00000201 /
      DATA ILARGE(1),ILARGE(2) / '37777777, '00000177 /
      DATA IRIGHT(1),IRIGHT(2) / '20000000, '00000352 /
      DATA IDIVER(1),IDIVER(2) / '20000000, '00000353 /
      DATA ILOG10(1),ILOG10(2) / '23210115, '00000377 /
CC
      DATA JSMALL(1),JSMALL(2) / '20000000, '00000201 /
      DATA JLARGE(1),JLARGE(2) / '37777777, '37777577 /
      DATA JRIGHT(1),JRIGHT(2) / '20000000, '00000333 /
      DATA JDIVER(1),JDIVER(2) / '20000000, '00000334 /
      DATA JLOG10(1),JLOG10(2) / '23210115, '10237777 /
CC
      IHOST1='HARR'
      IHOST2='    '
      IHMOD1='220'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ************************************************************
CC              **  MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.  **
CC              ************************************************************
CC
#ifdef HONEYWELL
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /   43 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   36 /
      DATA I2MACH( 6) /    6 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   35 /
      DATA I2MACH( 9) / O377777777777 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   27 /
      DATA I2MACH(12) / -127 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   63 /
      DATA I2MACH(15) / -127 /
      DATA I2MACH(16) /  127 /
CC
      DATA R2MACH(1) / O402400000000 /
      DATA R2MACH(2) / O376777777777 /
      DATA R2MACH(3) / O714400000000 /
      DATA R2MACH(4) / O716400000000 /
      DATA R2MACH(5) / O776464202324 /
CC
      DATA JSMALL(1),JSMALL(2) / O402400000000, O000000000000 /
      DATA JLARGE(1),JLARGE(2) / O376777777777, O777777777777 /
      DATA JRIGHT(1),JRIGHT(2) / O604400000000, O000000000000 /
      DATA JDIVER(1),JDIVER(2) / O606400000000, O000000000000 /
      DATA JLOG10(1),JLOG10(2) / O776464202324, O117571775714 /
CC
      IHOST1='HONE'
      IHOST2='    '
      IHMOD1='6000'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ************************************************
CC              **  MACHINE CONSTANTS FOR THE HP 2100         **
CC              **  3 WORD DOUBLE PRECISION OPTION WITH FTN4  **
CC              ************************************************
CC
#ifdef HP_2100
      DATA I1MACH( 1) /    5 /
      DATA I1MACH( 2) /    6 /
      DATA I1MACH( 3) /    4 /
      DATA I1MACH( 4) /    1 /
      DATA I1MACH( 5) /   16 /
      DATA I1MACH( 6) /    2 /
      DATA I1MACH( 7) /    2 /
      DATA I1MACH( 8) /   15 /
      DATA I1MACH( 9) / 32767 /
      DATA I1MACH(10) /    2 /
      DATA I1MACH(11) /   23 /
      DATA I1MACH(12) / -128 /
      DATA I1MACH(13) /  127 /
      DATA I1MACH(14) /   39 /
      DATA I1MACH(15) / -128 /
      DATA I1MACH(16) /  127 /
CC
      DATA ISMALL(1), ISMALL(2) / 40000B,       1 /
      DATA ILARGE(1), ILARGE(2) / 77777B, 177776B /
      DATA IRIGHT(1), IRIGHT(2) / 40000B,    325B /
      DATA IDIVER(1), IDIVER(2) / 40000B,    327B /
      DATA ILOG10(1), ILOG10(2) / 46420B,  46777B /
CC
      DATA JSMALL(1), JSMALL(2), JSMALL(3) / 40000B,       0,       1 /
      DATA JLARGE(1), JLARGE(2), JLARGE(3) / 77777B, 177777B, 177776B /
      DATA JRIGHT(1), JRIGHT(2), JRIGHT(3) / 40000B,       0,    265B /
      DATA JDIVER(1), JDIVER(2), JDIVER(3) / 40000B,       0,    276B /
      DATA JLOG10(1), JLOG10(2), JLOG10(3) / 46420B,  46502B,  77777B /
CC
      IHOST1='HP'
      IHOST2='    '
      IHMOD1='2100'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='FTN4'
      ISITE='    '
#endif
CC
CC              ************************************************
CC              **  MACHINE CONSTANTS FOR THE HP 2100         **
CC              **  4 WORD DOUBLE PRECISION OPTION WITH FTN4  **
CC              ************************************************
CC
CC
#ifdef HP_2100_2
      DATA I1MACH( 1) /    5 /
      DATA I1MACH( 2) /    6 /
      DATA I1MACH( 3) /    4 /
      DATA I1MACH( 4) /    1 /
      DATA I1MACH( 5) /   16 /
      DATA I1MACH( 6) /    2 /
      DATA I1MACH( 7) /    2 /
      DATA I1MACH( 8) /   15 /
      DATA I1MACH( 9) / 32767 /
      DATA I1MACH(10) /    2 /
      DATA I1MACH(11) /   23 /
      DATA I1MACH(12) / -128 /
      DATA I1MACH(13) /  127 /
      DATA I1MACH(14) /   55 /
      DATA I1MACH(15) / -128 /
      DATA I1MACH(16) /  127 /
CC
      DATA ISMALL(1), ISMALL(2) / 40000B,       1 /
      DATA ILARGE(1), ILARGE(2) / 77777B, 177776B /
      DATA IRIGHT(1), IRIGHT(2) / 40000B,    325B /
      DATA IDIVER(1), IDIVER(2) / 40000B,    327B /
      DATA ILOG10(1), ILOG10(2) / 46420B,  46777B /
CC
      DATA JSMALL(1), JSMALL(2) /  40000B,       0 /
      DATA JSMALL(3), JSMALL(4) /       0,       1 /
      DATA JLARGE(1), JLARGE(2) /  77777B, 177777B /
      DATA JLARGE(3), JLARGE(4) / 177777B, 177776B /
      DATA JRIGHT(1), JRIGHT(2) /  40000B,       0 /
      DATA JRIGHT(3), JRIGHT(4) /       0,    225B /
      DATA JDIVER(1), JDIVER(2) /  40000B,       0 /
      DATA JDIVER(3), JDIVER(4) /       0,    227B /
      DATA JLOG10(1), JLOG10(2) /  46420B,  46502B /
      DATA JLOG10(3), JLOG10(4) /  76747B, 176377B /
CC
      IHOST1='HP'
      IHOST2='    '
      IHMOD1='2100'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='FTN4'
      ISITE='    '
#endif
CC              ********************************************************
CC              **  MACHINE CONSTANTS FOR THE HP 9000.  THESE VALUES  **
CC              **  ARE TENTATIVE AND HAVE NOT BEEN CHECKED           **
CC              ********************************************************
CC
#ifdef HP_9000
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    7 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   23 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   55 /
      DATA I2MACH(15) / -128 /
      DATA I2MACH(16) /  127 /
CC
      DATA R2MACH(1) / O00000000200 /
      DATA R2MACH(2) / O37777677777 /
      DATA R2MACH(3) / O00000032200 /
      DATA R2MACH(4) / O00000032400 /
      DATA R2MACH(5) / O04046637632 /
CC
      IHOST1='HP'
      IHOST2='    '
      IHMOD1='9000'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ********************************************************
CC              **  MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,     **
CC              **  XEROX SIGMA 5/7/9,                                **
CC              **  SEL SYSTEMS 85/86,                                **
CC              **  INTERDATA 30 AND 40,                              **
CC              **  PERKIN-ELMER 3230, 3240, 3242,                    **
CC              ********************************************************
CC
CC
#ifdef IBM
       DATA I2MACH( 1) /   5 /
       DATA I2MACH( 2) /   6 /
       DATA I2MACH( 3) /   7 /
       DATA I2MACH( 4) /   6 /
       DATA I2MACH( 5) /  32 /
       DATA I2MACH( 6) /   4 /
       DATA I2MACH( 7) /   2 /
       DATA I2MACH( 8) /  31 /
       DATA I2MACH( 9) / Z7FFFFFFF /
       DATA I2MACH(10) /  16 /
       DATA I2MACH(11) /   6 /
       DATA I2MACH(12) / -64 /
       DATA I2MACH(13) /  63 /
       DATA I2MACH(14) /  14 /
       DATA I2MACH(15) / -64 /
       DATA I2MACH(16) /  63 /
CC
       DATA R2MACH(1) / Z00100000 /
       DATA R2MACH(2) / Z7FFFFFFF /
       DATA R2MACH(3) / Z3B100000 /
       DATA R2MACH(4) / Z3C100000 /
       DATA R2MACH(5) / Z41134413 /
CC
       DATA JSMALL(1),JSMALL(2) / Z00100000, Z00000000 /
       DATA JLARGE(1),JLARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
       DATA JRIGHT(1),JRIGHT(2) / Z33100000, Z00000000 /
       DATA JDIVER(1),JDIVER(2) / Z34100000, Z00000000 /
       DATA JLOG10(1),JLOG10(2) / Z41134413, Z509F79FF /
CC
       IHOST1='IBM'
       IHOST2='    '
       IHMOD1='370'
       IHMOD2='    '
       IOPSY1='    '
       IOPSY2='    '
       ICOMPI='    '
       ISITE='    '
#endif
CC              ********************************************************
CC              **  MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).  **
CC              ********************************************************
C
#ifdef PDP_10_KA
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    5 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   36 /
      DATA I2MACH( 6) /    5 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   35 /
      DATA I2MACH( 9) / "377777777777 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   27 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   54 /
      DATA I2MACH(15) / -101 /
      DATA I2MACH(16) /  127 /
CC
      DATA R2MACH(1) / "000400000000 /
      DATA R2MACH(2) / "377777777777 /
      DATA R2MACH(3) / "146400000000 /
      DATA R2MACH(4) / "147400000000 /
      DATA R2MACH(5) / "177464202324 /
CC
      DATA JSMALL(1),JSMALL(2) / "033400000000, "000000000000 /
      DATA JLARGE(1),JLARGE(2) / "377777777777, "344777777777 /
      DATA JRIGHT(1),JRIGHT(2) / "113400000000, "000000000000 /
      DATA JDIVER(1),JDIVER(2) / "114400000000, "000000000000 /
      DATA JLOG10(1),JLOG10(2) / "177464202324, "144117571776 /
CC
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='10'
      IHMOD2='    '
      IOPSY1='KA'
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ********************************************************
CC              **  MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).  **
CC              ********************************************************
CC
#ifdef PDP_10_KI
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    5 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   36 /
      DATA I2MACH( 6) /    5 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   35 /
      DATA I2MACH( 9) / "377777777777 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   27 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   62 /
      DATA I2MACH(15) / -128 /
      DATA I2MACH(16) /  127 /
CC
      DATA R2MACH(1) / "000400000000 /
      DATA R2MACH(2) / "377777777777 /
      DATA R2MACH(3) / "146400000000 /
      DATA R2MACH(4) / "147400000000 /
      DATA R2MACH(5) / "177464202324 /
CC
      DATA JSMALL(1),JSMALL(2) / "000400000000, "000000000000 /
      DATA JLARGE(1),JLARGE(2) / "377777777777, "377777777777 /
      DATA JRIGHT(1),JRIGHT(2) / "103400000000, "000000000000 /
      DATA JDIVER(1),JDIVER(2) / "104400000000, "000000000000 /
      DATA JLOG10(1),JLOG10(2) / "177464202324, "476747767461 /
CC
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='10'
      IHMOD2='    '
      IOPSY1='KI'
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              *********************************************************
CC              **  MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING  **
CC              **  32-BIT INTEGER ARITHMETIC.                         **
CC              *********************************************************
CC
#ifdef PDP_11_32
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    5 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -127 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   56 /
      DATA I2MACH(15) / -127 /
      DATA I2MACH(16) /  127 /
CC
      DATA R2MACH(1) / O00040000000 /
      DATA R2MACH(2) / O17777777777 /
      DATA R2MACH(3) / O06440000000 /
      DATA R2MACH(4) / O06500000000 /
      DATA R2MACH(5) / O07746420233 /
CC
      DATA ISMALL(1) /    8388608 /
      DATA ILARGE(1) / 2147483647 /
      DATA IRIGHT(1) /  880803840 /
      DATA IDIVER(1) /  889192448 /
      DATA ILOG10(1) / 1067065499 /
CC
      DATA JSMALL(1),JSMALL(2) /    8388608,           0 /
      DATA JLARGE(1),JLARGE(2) / 2147483647,          -1 /
      DATA JRIGHT(1),JRIGHT(2) /  612368384,           0 /
      DATA JDIVER(1),JDIVER(2) /  620756992,           0 /
      DATA JLOG10(1),JLOG10(2) / 1067065498, -2063872008 /
      DATA JSMALL(1),JSMALL(2) / O00040000000, O00000000000 /
      DATA JLARGE(1),JLARGE(2) / O17777777777, O37777777777 /
      DATA JRIGHT(1),JRIGHT(2) / O04440000000, O00000000000 /
      DATA JDIVER(1),JDIVER(2) / O04500000000, O00000000000 /
      DATA JLOG10(1),JLOG10(2) / O07746420232, O20476747770 /
CC
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='10'
      IHMOD2='    '
      IOPSY1='KI'
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              *********************************************************
CC              **  MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING  **
CC              **  16-BIT INTEGER ARITHMETIC.                         **
CC              *********************************************************
CC
#ifdef PDP_11_16
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    5 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   16 /
      DATA I2MACH( 6) /    2 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   15 /
      DATA I2MACH( 9) / 32767 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -127 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   56 /
      DATA I2MACH(15) / -127 /
      DATA I2MACH(16) /  127 /
CC
      DATA ISMALL(1),ISMALL(2) /   128,     0 /
      DATA ILARGE(1),ILARGE(2) / 32767,    -1 /
      DATA IRIGHT(1),IRIGHT(2) / 13440,     0 /
      DATA IDIVER(1),IDIVER(2) / 13568,     0 /
      DATA ILOG10(1),ILOG10(2) / 16282,  8347 /
      DATA ISMALL(1),ISMALL(2) / O000200, O000000 /
      DATA ILARGE(1),ILARGE(2) / O077777, O177777 /
      DATA IRIGHT(1),IRIGHT(2) / O032200, O000000 /
      DATA IDIVER(1),IDIVER(2) / O032400, O000000 /
      DATA ILOG10(1),ILOG10(2) / O037632, O020233 /
CC
      DATA JSMALL(1),JSMALL(2) /    128,      0 /
      DATA JSMALL(3),JSMALL(4) /      0,      0 /
      DATA JLARGE(1),JLARGE(2) /  32767,     -1 /
      DATA JLARGE(3),JLARGE(4) /     -1,     -1 /
      DATA JRIGHT(1),JRIGHT(2) /   9344,      0 /
      DATA JRIGHT(3),JRIGHT(4) /      0,      0 /
      DATA JDIVER(1),JDIVER(2) /   9472,      0 /
      DATA JDIVER(3),JDIVER(4) /      0,      0 /
      DATA JLOG10(1),JLOG10(2) /  16282,   8346 /
      DATA JLOG10(3),JLOG10(4) / -31493, -12296 /
      DATA JSMALL(1),JSMALL(2) / O000200, O000000 /
      DATA JSMALL(3),JSMALL(4) / O000000, O000000 /
      DATA JLARGE(1),JLARGE(2) / O077777, O177777 /
      DATA JLARGE(3),JLARGE(4) / O177777, O177777 /
      DATA JRIGHT(1),JRIGHT(2) / O022200, O000000 /
      DATA JRIGHT(3),JRIGHT(4) / O000000, O000000 /
      DATA JDIVER(1),JDIVER(2) / O022400, O000000 /
      DATA JDIVER(3),JDIVER(4) / O000000, O000000 /
      DATA JLOG10(1),JLOG10(2) / O037632, O020232 /
      DATA JLOG10(3),JLOG10(4) / O102373, O147770 /
CC
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='11'
      IHMOD2='    '
      IOPSY1=' '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC     THE FOLLOWING IS FOR THE PRIME--
CC              **********************************************************
CC              **  MACHINE CONSTANTS FOR THE PRIME 50 SERIES.          **
CC              **  FOR F77 COMPILER WITH -INTL OPTION                  **
C               **  MY THANKS TO ING-YUNG LI TSE FOR THIS CONTRIBUTION  **
C               **  (NOVEMBER, 1986).                                   **
CC              **********************************************************
CC
#ifdef PRIME
      DATA I2MACH( 1) /    1 /
      DATA I2MACH( 2) /    1 /
      DATA I2MACH( 3) /    7 /
      DATA I2MACH( 4) /    1 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   23 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   47 /
      DATA I2MACH(15) / -32896 /
      DATA I2MACH(16) /  32639 /
CC
      R2MACH(1)=0.5*2.0**(-128)
      R2MACH(2)=(1.0-2.0**(-23))*2.0*(127)
      R2MACH(3)=2.0**(-22)
      R2MACH(4)=2.0**(-21)
      R2MACH(5)=ALOG10(2.0)
CC
      D2MACH(1)=0.5D0*2.0D0**(-32590)
      D2MACH(2)=(1.0D0-2.0D0**(-47))*2.0D0**(32638)
      D2MACH(3)=2.0D0**(-46)
      D2MACH(4)=2.0D0**(-45)
      D2MACH(5)=DLOG10(2.0D0)
CC
       IHOST1='PRIM'
       IHOST2='    '
       IHMOD1='X50 '
       IHMOD2='    '
       IOPSY1='PRIM'
       IOPSY2='OS  '
       ICOMPI='F77 '
       ISITE='    '
#endif
CC
CC     THE FOLLOWING IS FOR THE UNIVAC--
CC               ***************************************************************
CC               **  MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
CC               ***************************************************************
CC
#ifdef UNIVAC_1100_FTN
       DATA I2MACH( 1) /    5 /
       DATA I2MACH( 2) /    6 /
       DATA I2MACH( 3) /    1 /
       DATA I2MACH( 4) /    6 /
       DATA I2MACH( 5) /   36 /
       DATA I2MACH( 6) /    4 /
       DATA I2MACH( 7) /    2 /
       DATA I2MACH( 8) /   35 /
       DATA I2MACH( 9) / O377777777777 /
       DATA I2MACH(10) /    2 /
       DATA I2MACH(11) /   27 /
       DATA I2MACH(12) / -128 /
       DATA I2MACH(13) /  127 /
       DATA I2MACH(14) /   60 /
       DATA I2MACH(15) /-1024 /
       DATA I2MACH(16) / 1023 /
CC
       DATA R2MACH(1) / O000400000000 /
       DATA R2MACH(2) / O377777777777 /
       DATA R2MACH(3) / O146400000000 /
       DATA R2MACH(4) / O147400000000 /
       DATA R2MACH(5) / O177464202324 /
CC
       DATA JSMALL(1),JSMALL(2) /    128,      0 /
       DATA JSMALL(3),JSMALL(4) /      0,      0 /
       DATA JLARGE(1),JLARGE(2) /  32767,     -1 /
       DATA JLARGE(3),JLARGE(4) /     -1,     -1 /
       DATA JRIGHT(1),JRIGHT(2) /   9344,      0 /
       DATA JRIGHT(3),JRIGHT(4) /      0,      0 /
       DATA JDIVER(1),JDIVER(2) /   9472,      0 /
       DATA JDIVER(3),JDIVER(4) /      0,      0 /
       DATA JLOG10(1),JLOG10(2) /  16282,   8346 /
       DATA JLOG10(3),JLOG10(4) / -31493, -12296 /
       DATA JSMALL(1),JSMALL(2) / O000200, O000000 /
       DATA JSMALL(3),JSMALL(4) / O000000, O000000 /
       DATA JLARGE(1),JLARGE(2) / O077777, O177777 /
       DATA JLARGE(3),JLARGE(4) / O177777, O177777 /
       DATA JRIGHT(1),JRIGHT(2) / O022200, O000000 /
       DATA JRIGHT(3),JRIGHT(4) / O000000, O000000 /
       DATA JDIVER(1),JDIVER(2) / O022400, O000000 /
       DATA JDIVER(3),JDIVER(4) / O000000, O000000 /
       DATA JLOG10(1),JLOG10(2) / O037632, O020232 /
       DATA JLOG10(3),JLOG10(4) / O102373, O147770 /
CC
       IHOST1='UNIV'
       IHOST2='    '
       IHMOD1='1100'
       IHMOD2='    '
       IOPSY1='EXEC'
       IOPSY2='8'
       ICOMPI='FTN'
       ISITE='NBS'
#endif
CC
CC              ****************************************************************
CC              **  MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FOR COMPILER
CC              ****************************************************************
CC
#ifdef UNIVAC_1100_FOR
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    1 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   36 /
      DATA I2MACH( 6) /    6 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   35 /
      DATA I2MACH( 9) / O377777777777 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   27 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   60 /
      DATA I2MACH(15) /-1024 /
      DATA I2MACH(16) / 1023 /
CC
      DATA R2MACH(1) / O000400000000 /
      DATA R2MACH(2) / O377777777777 /
      DATA R2MACH(3) / O146400000000 /
      DATA R2MACH(4) / O147400000000 /
      DATA R2MACH(5) / O177464202324 /
CC
      DATA JSMALL(1),JSMALL(2) / O000040000000, O000000000000 /
      DATA JLARGE(1),JLARGE(2) / O377777777777, O777777777777 /
      DATA JRIGHT(1),JRIGHT(2) / O170540000000, O000000000000 /
      DATA JDIVER(1),JDIVER(2) / O170640000000, O000000000000 /
      DATA JLOG10(1),JLOG10(2) / O177746420232, O411757177572 /
CC
      IHOST1='UNIV'
      IHOST2='    '
      IHMOD1='1100'
      IHMOD2='    '
      IOPSY1='EXEC'
      IOPSY2='8'
      ICOMPI='FOR'
      ISITE='    '
#endif
CC
#ifdef WIN32_OTG
C     THE FOLLOWING IS FOR THE 16-BIT IBM-PC AND CLONES USING OTG COMPILER
C               ***********************************************************
C               **  MACHINE CONSTANTS FOR THE 16-BIT IBM-PC (NOT YET VERIFIED)
C               **  (WITH 8087 COPROCESSOR)                              **
C               **  (WITH APPRECIATION TO MARTIN KNAPP-CORDES,           **
C               **  JULY, 1986)                                          **
C               ***********************************************************
CC
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -125 /
      DATA I2MACH(13) /  128 /
      DATA I2MACH(14) /   53 /
      DATA I2MACH(15) / -1021 /
      DATA I2MACH(16) /  1024 /
CC
CCCCC DATA R2MACH(1) / Z'00800000' /
CCCCC DATA R2MACH(2) / Z'7F7FFFFF' /
CCCCC DATA R2MACH(3) / Z'33800000' /
CCCCC DATA R2MACH(4) / Z'34000000' /
CCCCC DATA R2MACH(5) / Z'3E9A209B' /
      DATA R2MACH(1) / 1.18E-38 /
      DATA R2MACH(2) / 3.340E+38 /
      DATA R2MACH(3) / 0.59E-07 /
      DATA R2MACH(4) / 1.19E-07 /
      DATA R2MACH(5) / 0.30102999566 /
CC
CCCCC DATA ISMALL(1) / Z'00800000' /
CCCCC DATA ILARGE(1) / Z'7F7FFFFF' /
CCCCC DATA IRIGHT(1) / Z'33800000' /
CCCCC DATA IDIVER(1) / Z'34000000' /
CCCCC DATA ILOG10(1) / Z'3E9A209B' /
      DATA D2MACH(1) / 2.23D-308 /
      DATA D2MACH(2) / 1.790D+308 /
      DATA D2MACH(3) / 1.11D-16 /
      DATA D2MACH(4) / 2.22D-16 /
      DATA D2MACH(5) / 0.30102999563981195D0 /
CC
CCCCC DATA JSMALL(1),JSMALL(2) / Z'00100000', Z'00000000' /
CCCCC DATA JLARGE(1),JLARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
CCCCC DATA JRIGHT(1),JRIGHT(2) / Z'3CA00000', Z'00000000' /
CCCCC DATA JDIVER(1),JDIVER(2) / Z'3CB00000', Z'00000000' /
CCCCC DATA JLOG10(1),JLOG10(2) / Z'3FD34413', Z'509F79FF' /
CC
      IHOST1='IBM-'
      IHOST2='PC  '
CCCCC THE FOLLOWING LINE WAS FIXED               APRIL 1992
CCCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
CCCCC IHMOD1='    '
      IHMOD1='386 '
      IHMOD2='    '
      IOPSY1='PC-D'
      IOPSY2='OS  '
CCCCC THE FOLLOWING LINE WAS FIXED   APRIL 1992
CCCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
CCCCC ICOMPI='    '
      ICOMPI='OTG '
      ISITE='    '
#endif
CC
CC    CONSTANTS FOR WINDOWS USING INTEL COMPILER
CC
#ifdef INTEL
CC
#ifdef INTEL_128
C
C     NEED TO GET INTEGER CONSTANTS THAT ASSUME 64 BITS
C     (FOLLOWING ARE THE 32-BIT CONSTANTS)
C
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -125 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   53 /
      DATA I2MACH(15) / -1021 /
      DATA I2MACH(16) /  1023 /
#else
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -125 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   53 /
      DATA I2MACH(15) / -1021 /
      DATA I2MACH(16) /  1023 /
#endif
CC
#ifdef INTEL_32
      DATA R2MACH(3) / 5.9604645E-08 /
      DATA R2MACH(5) / 3.0102999E-01 /
      DATA D2MACH(3) / 1.1102230246252E-16  /
      DATA D2MACH(5) / 0.30102999563981195D0 /
#endif
#ifdef INTEL_64
      DATA R2MACH(3) / 1.1102230246252E-16  /
      DATA R2MACH(5) / 0.30102999563981195D0 /
      DATA D2MACH(3) / 1.1102230246252E-16  /
      DATA D2MACH(5) / 0.30102999563981195D0 /
#endif
#ifdef INTEL_128
      DATA R2MACH(3) / 1.1102230246252E-16  /
      DATA R2MACH(5) / 0.30102999563981195D0 /
      DATA D2MACH(3) / 9.6296497219361792652798897129246366D-035  /
      DATA D2MACH(5) / 0.30102999563981195D0 /
#endif
CC
#ifdef INTEL_32
      R2MACH(1)=TINY(1.0)
      R2MACH(2)=HUGE(1.0)
      R2MACH(4)=EPSILON(1.0)
      D2MACH(1)=TINY(1.0D0)
      D2MACH(2)=HUGE(1.0D0)
      D2MACH(4)=EPSILON(1.0D0)
#endif
#ifdef INTEL_64
      R2MACH(1)=TINY(1.0)
      R2MACH(2)=HUGE(1.0)
      R2MACH(4)=EPSILON(1.0)
      D2MACH(1)=R2MACH(1)
      D2MACH(2)=R2MACH(2)
      D2MACH(4)=R2MACH(4)
#endif
#ifdef INTEL_128
      R2MACH(1)=TINY(1.0)
      R2MACH(2)=HUGE(1.0)
      R2MACH(4)=EPSILON(1.0)
      D2MACH(1)=TINY(1.0D0)
      D2MACH(2)=HUGE(1.0D0)
      D2MACH(4)=EPSILON(1.0D0)
#endif
CC
      IHOST1='IBM-'
      IHOST2='PC  '
CCCCC THE FOLLOWING LINE WAS FIXED               APRIL 1992
CCCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
CCCCC IHMOD1='    '
      IHMOD1='386 '
      IHMOD2='    '
      IOPSY1='PC-D'
      IOPSY2='OS  '
CCCCC THE FOLLOWING LINE WAS FIXED   APRIL 1992
CCCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
CCCCC ICOMPI='    '
CCCCC JUNE 1996.  FOR LAHEY COMPILER, SET ICOMPI SWITCH DIFFERENT.
CCCCC ICOMPI='OTG '
CLAHE ICOMPI='LAHE'
      ICOMPI='MS-F'
      ISITE='    '
C
#endif
CC
C     THE FOLLOWING IS FOR THE MACINTOCH (NOT YET VERIFIED)--
C               **************************************************************
C               **  MACHINE CONSTANTS FOR THE MACINTOCH (NOT YET VERIFIED)  **
C               **  (WITH APPRECIATION TO MARTIN KNAPP-CORDES,              **
C               **  JULY, 1986)                                             **
C               **************************************************************
CC
#ifdef MACINTOSH
       DATA I2MACH( 1) /    5 /
       DATA I2MACH( 2) /    6 /
       DATA I2MACH( 3) /    6 /
       DATA I2MACH( 4) /    0 /
       DATA I2MACH( 5) /   32 /
       DATA I2MACH( 6) /    4 /
       DATA I2MACH( 7) /    2 /
       DATA I2MACH( 8) /   31 /
       DATA I2MACH( 9) / 2147483647 /
       DATA I2MACH(10) /    2 /
       DATA I2MACH(11) /   24 /
       DATA I2MACH(12) / -125 /
       DATA I2MACH(13) /  128 /
       DATA I2MACH(14) /   53 /
       DATA I2MACH(15) / -1021 /
       DATA I2MACH(16) /  1024 /
CC
       DATA R2MACH(1) / Z'00800000' /
       DATA R2MACH(2) / Z'7F7FFFFF' /
       DATA R2MACH(3) / Z'33800000' /
       DATA R2MACH(4) / Z'34000000' /
       DATA R2MACH(5) / Z'3E9A209B' /
CC
       DATA ISMALL(1) / Z'00800000' /
       DATA ILARGE(1) / Z'7F7FFFFF' /
       DATA IRIGHT(1) / Z'33800000' /
       DATA IDIVER(1) / Z'34000000' /
       DATA ILOG10(1) / Z'3E9A209B' /
CC
       DATA JSMALL(1),JSMALL(2) / Z'00100000', Z'00000000' /
       DATA JLARGE(1),JLARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
       DATA JRIGHT(1),JRIGHT(2) / Z'3CA00000', Z'00000000' /
       DATA JDIVER(1),JDIVER(2) / Z'3CB00000', Z'00000000' /
       DATA JLOG10(1),JLOG10(2) / Z'3FD34413', Z'509F79FF' /
C
       IHOST1='MACI'
       IHOST2='NTOC'
       IHMOD1='    '
       IHMOD2='    '
       IOPSY1='MACI'
       IOPSY2='NTOC'
       ICOMPI='    '
       ISITE='    '
#endif
CC
CCCCC THE FOLLOWING WAS ADDED JUNE 1989--
C     THE FOLLOWING IS FOR THE 32-BIT IBM-PC/OS2 AND  COMPAQ 386/XX (NOT YET VER
C     (PROBABLY NOT FULLY CORRECT)
C               ***********************************************************
C               **  MACHINE CONSTANTS FOR THE 32-BIT IBM-PC (NOT YET VERIFIED)
C               **  (WITH 387 COPROCESSOR)                              **
C               **  (WITH APPRECIATION TO NELSON HSU                     **
C               **  JUNE, 1989)                                          **
C               ***********************************************************
CC
#ifdef OS2
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -125 /
      DATA I2MACH(13) /  128 /
      DATA I2MACH(14) /   53 /
      DATA I2MACH(15) / -1021 /
      DATA I2MACH(16) /  1024 /
CC
      DATA R2MACH(1) / Z'00800000' /
      DATA R2MACH(2) / Z'7F7FFFFF' /
      DATA R2MACH(3) / Z'33800000' /
      DATA R2MACH(4) / Z'34000000' /
      DATA R2MACH(5) / Z'3E9A209B' /
CC
      DATA ISMALL(1) / Z'00800000' /
      DATA ILARGE(1) / Z'7F7FFFFF' /
      DATA IRIGHT(1) / Z'33800000' /
      DATA IDIVER(1) / Z'34000000' /
      DATA ILOG10(1) / Z'3E9A209B' /
CC
      DATA JSMALL(1),JSMALL(2) / Z'00100000', Z'00000000' /
      DATA JLARGE(1),JLARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
      DATA JRIGHT(1),JRIGHT(2) / Z'3CA00000', Z'00000000' /
      DATA JDIVER(1),JDIVER(2) / Z'3CB00000', Z'00000000' /
      DATA JLOG10(1),JLOG10(2) / Z'3FD34413', Z'509F79FF' /
C
      IHOST1='IBM-'
      IHOST2='PC  '
      IHMOD1='    '
      IHMOD2='    '
      IOPSY1='OS38'
      IOPSY2='6   '
      ICOMPI='    '
      ISITE='    '
#endif
CC
CC              ****************************************************
CC              **  MACHINE CONSTANTS FOR THE SUN  (AND SUN 2)    **
CC              **  WITH APPRECIATION TO BILL ANDERSON, NBS       **
C               **  OCTOBER, 1987 (THESE VALUES ARE TENTATIVE     **
CC              **  AND HAVE NOT BEEN CHECKED                     **
CC              ****************************************************
CC
#ifdef SUN
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    5 /
      DATA I2MACH( 4) /    7 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
C
CC    DOES APOLLO NORMALIZE THEIR FRACTION LIKE A    VAX?
CC    IF SO, CHANGE THE FOLLOWING 23 TO 24
CC    ASK APOLLO HOW THEY DO THEIR NUMBERS
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -124 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   51 /
      DATA I2MACH(15) / -1013/
      DATA I2MACH(16) /  1013/
CC
CC    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
CC    (THAT IS 2**-23 AND 2**22)
CC    FOR THE NUMBERS BELOW,
      DATA R2MACH(1) / 1.175495E-38 /
      DATA R2MACH(2) / 3.402823E38 /
      DATA R2MACH(3) / 1.1920928955078E-7 /
      DATA R2MACH(4) / 2.3841857910156E-7 /
      DATA R2MACH(5) / 0.3010300 /
CC
CC    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
CC    (THAT IS 2**-23 AND 2**22)
CC    FOR THE NUMBERS BELOW,
      DATA D2MACH(1) / 2.22507385850721D-308 /
      DATA D2MACH(2) / 1.79769313486231D308 /
      DATA D2MACH(3) / 1.1102230246252D-16 /
      DATA D2MACH(4) / 2.2204460492503D-16 /
      DATA D2MACH(5) / 0.3010299956639812 /
CC
      IHOST1='SUN '
      IHOST2='    '
      IHMOD1='3   '
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
CC
#ifdef LINUX
CC  FOLLOWING SECTION FOR LINUX.  SUPPORT CONDITIONAL COMPILATION.
CC
CC  FOR INTEGERS - SPECIFY "-DI32"  OR  "-DI64"  BASED ON WHETHER
CC  YOU ARE RUNNING ON A 32-BIT OR A 64-BIT MACHINE.
CC
CC  2019/11: ALLOW SETTING IOPSY2 FOR MacOS AS THERE ARE A
CC           FEW OCCASIONS WHERE MacOS MAY NEED SLIGHTLY
CC           DIFFERENT OPTIONS THAN STANDARD LINUX.
#if defined(INTEGER32)
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   53 /
      DATA I2MACH(12) / -1021 /
      DATA I2MACH(13) /  1024 /
      DATA I2MACH(14) /   113 /
      DATA I2MACH(15) / -16382 /
      DATA I2MACH(16) /  16384 /
#elif defined(INTEGER64)
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   64 /
      DATA I2MACH( 6) /    8 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   63 /
      DATA I2MACH( 9) / 9223372036854775807 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   53 /
      DATA I2MACH(12) / -1021 /
      DATA I2MACH(13) /  1024 /
      DATA I2MACH(14) /   113 /
      DATA I2MACH(15) / -16382 /
      DATA I2MACH(16) /  16384 /
#else
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    6 /
      DATA I2MACH( 4) /    0 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   53 /
      DATA I2MACH(12) / -1021 /
      DATA I2MACH(13) /  1024 /
      DATA I2MACH(14) /   113 /
      DATA I2MACH(15) / -16382 /
      DATA I2MACH(16) /  16384 /
#endif
#if defined(SDOUBLE)
      DATA R2MACH(1) / 1.175495E-38 /
      DATA R2MACH(2) / 3.402823E38 /
      DATA R2MACH(3) / 1.1920928955078E-7 /
      DATA R2MACH(4) / 2.3841857910156E-7 /
      DATA R2MACH(5) / 0.3010300 /
      DATA D2MACH(1) / 2.2250738585072D-307 /
      DATA D2MACH(2) / 1.7976931348623D308  /
      DATA D2MACH(3) / 1.1102230246252D-16  /
      DATA D2MACH(4) / 2.2204460492503D-16  /
      DATA D2MACH(5) / 0.30102999566398D0   /
#elif defined(DDOUBLE)
      DATA R2MACH(1) / 2.2250738585072E-307 /
      DATA R2MACH(2) / 1.7976931348623E308  /
      DATA R2MACH(3) / 1.1102230246252E-16  /
      DATA R2MACH(4) / 2.2204460492503E-16  /
      DATA R2MACH(5) / 0.3010299956639E0    /
      DATA D2MACH(1) / 2.2250738585072D-307 /
      DATA D2MACH(2) / 1.7976931348623D308  /
      DATA D2MACH(3) / 1.1102230246252D-16  /
      DATA D2MACH(4) / 2.2204460492503D-16  /
      DATA D2MACH(5) / 0.30102999566398D0   /
#elif defined(DQUADRUPLE)
      DATA R2MACH(1) / 2.2250738585072D-307 /
      DATA R2MACH(2) / 1.7976931348623D308  /
      DATA R2MACH(3) / 1.1102230246252D-16  /
      DATA R2MACH(4) / 2.2204460492503D-16  /
      DATA R2MACH(5) / 0.30102999566398D0   /
      DATA D2MACH(1) / 3.3621031431120935062626778173217526D-4932 /
      DATA D2MACH(2) / 1.1897314953572317650857593266280070D+4932 /
      DATA D2MACH(3) / 9.6296497219361792652798897129246366D-035  /
      DATA D2MACH(4) / 1.9259299443872358530559779425849273D-034  /
      DATA D2MACH(5) / 0.30102999566398119521373889472449302      /
#else
      DATA R2MACH(1) / 1.175495E-38 /
      DATA R2MACH(2) / 3.402823E38 /
      DATA R2MACH(3) / 1.1920928955078E-7 /
      DATA R2MACH(4) / 2.3841857910156E-7 /
      DATA R2MACH(5) / 0.3010300 /
      DATA D2MACH(1) / 2.2250738585072D-307 /
      DATA D2MACH(2) / 1.7976931348623D308  /
      DATA D2MACH(3) / 1.1102230246252D-16  /
      DATA D2MACH(4) / 2.2204460492503D-16  /
      DATA D2MACH(5) / 0.30102999566398D0   /
#endif
C
      IHOST1='SUN '
      IHOST2='    '
      IHMOD1='3   '
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
#if defined(MACOS)
      IOPSY2='MAC '
#endif
      ICOMPI='gfor'
      ISITE='    '
#endif
CC
CC
CC              ************************************************
CC              **  MACHINE CONSTANTS FOR THE CONVEX          **
CC              **  CONVEX C-120, NATIVE MODE                 **
CC              **  EXTRACTED FROM CMLIB LIBRARY              **
CC              **  AUGUST, 1990                              **
CC              ************************************************
CC
#ifdef CONVEX_C120
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    0 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -127 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   53 /
      DATA I2MACH(15) / -1023 /
      DATA I2MACH(16) /  1023 /
C
      DATA D2MACH(1) / 5.562684646268007D-309 /
      DATA D2MACH(2) / 8.988465674311577D+307 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
C
      DATA R2MACH(1) / 2.9387360E-39 /
      DATA R2MACH(2) / 1.7014117E+38 /
      DATA R2MACH(3) / 5.9604645E-08 /
      DATA R2MACH(4) / 1.1920929E-07 /
      DATA R2MACH(5) / 3.0102999E-01 /
CC
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
CC
CC
CC              ************************************************
CC              **  MACHINE CONSTANTS FOR THE CONVEX          **
CC              **  EXTRACTED FROM CMLIB LIBRARY              **
CC              **  CONVEX C-120, NATIVE MODE WITH -R8 OPTION **
CC              **  AUGUST, 1990                              **
CC              ************************************************
CC
#ifdef CONVEX_C120_R8
      DATA I2MACH( 1) /     5 /
      DATA I2MACH( 2) /     6 /
      DATA I2MACH( 3) /     0 /
      DATA I2MACH( 4) /     6 /
      DATA I2MACH( 5) /    32 /
      DATA I2MACH( 6) /     4 /
      DATA I2MACH( 7) /     2 /
      DATA I2MACH( 8) /    31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /     2 /
      DATA I2MACH(11) /    53 /
      DATA I2MACH(12) / -1023 /
      DATA I2MACH(13) /  1023 /
      DATA I2MACH(14) /    53 /
      DATA I2MACH(15) / -1023 /
      DATA I2MACH(16) /  1023 /
C
      DATA R2MACH(1) / 5.562684646268007D-309 /
      DATA R2MACH(2) / 8.988465674311577D+307 /
      DATA R2MACH(3) / 1.110223024625157D-016 /
      DATA R2MACH(4) / 2.220446049250313D-016 /
      DATA R2MACH(5) / 3.010299956639812D-001 /
C
      DATA D2MACH(1) / 5.562684646268007D-309 /
      DATA D2MACH(2) / 8.988465674311577D+307 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
CC
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
CC
CC
CC              ************************************************
CC              **  MACHINE CONSTANTS FOR THE CONVEX          **
CC              **  EXTRACTED FROM CMLIB LIBRARY              **
CC              **  CONVEX C-120, IEEE MODE                   **
CC              **  AUGUST, 1990                              **
CC              ************************************************
CC
#ifdef CONVEX_C120_IEEE
      DATA I2MACH( 1) /    5 /
      DATA I2MACH( 2) /    6 /
      DATA I2MACH( 3) /    0 /
      DATA I2MACH( 4) /    6 /
      DATA I2MACH( 5) /   32 /
      DATA I2MACH( 6) /    4 /
      DATA I2MACH( 7) /    2 /
      DATA I2MACH( 8) /   31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /    2 /
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -125 /
      DATA I2MACH(13) /  128 /
      DATA I2MACH(14) /   53 /
      DATA I2MACH(15) / -1021 /
      DATA I2MACH(16) /  1024 /
C
      DATA R2MACH(1) / 1.1754945E-38 /
      DATA R2MACH(2) / 3.4028234E+38 /
      DATA R2MACH(3) / 5.9604645E-08 /
      DATA R2MACH(4) / 1.1920929E-07 /
      DATA R2MACH(5) / 3.0102999E-01 /
C
      DATA D2MACH(1) / 2.225073858507202D-308 /
      DATA D2MACH(2) / 1.797693134862315D+308 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
CC
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
CC
CC
CC              ************************************************
CC              **  MACHINE CONSTANTS FOR THE CONVEX          **
CC              **  EXTRACTED FROM CMLIB LIBRARY              **
CC              **  CONVEX C-120, IEEE MODE WITH -R8 OPTION   **
CC              **  AUGUST, 1990                              **
CC              ************************************************
CC
#ifdef CONVEX_C120_IEEE_R8
      DATA I2MACH( 1) /     5 /
      DATA I2MACH( 2) /     6 /
      DATA I2MACH( 3) /     0 /
      DATA I2MACH( 4) /     6 /
      DATA I2MACH( 5) /    32 /
      DATA I2MACH( 6) /     4 /
      DATA I2MACH( 7) /     2 /
      DATA I2MACH( 8) /    31 /
      DATA I2MACH( 9) / 2147483647 /
      DATA I2MACH(10) /     2 /
      DATA I2MACH(11) /    53 /
      DATA I2MACH(12) / -1021 /
      DATA I2MACH(13) /  1024 /
      DATA I2MACH(14) /    53 /
      DATA I2MACH(15) / -1021 /
      DATA I2MACH(16) /  1024 /
C
      DATA R2MACH(1) / 2.225073858507202D-308 /
      DATA R2MACH(2) / 1.797693134862315D+308 /
      DATA R2MACH(3) / 1.110223024625157D-016 /
      DATA R2MACH(4) / 2.220446049250313D-016 /
      DATA R2MACH(5) / 3.010299956639812D-001 /
C
      DATA D2MACH(1) / 2.225073858507202D-308 /
      DATA D2MACH(2) / 1.797693134862315D+308 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
CC
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
CC
CC
C    THE FOLLOWING IS FOR THE VAX--
C              ********************************************
C              **  MACHINE CONSTANTS FOR THE VAX-11/780  **
C              ********************************************
C
#ifdef VAX_VMS
       DATA I2MACH( 1) /    5 /
       DATA I2MACH( 2) /    6 /
       DATA I2MACH( 3) /    5 /
       DATA I2MACH( 4) /    6 /
       DATA I2MACH( 5) /   32 /
       DATA I2MACH( 6) /    4 /
       DATA I2MACH( 7) /    2 /
       DATA I2MACH( 8) /   31 /
       DATA I2MACH( 9) / 2147483647 /
       DATA I2MACH(10) /    2 /
       DATA I2MACH(11) /   24 /
       DATA I2MACH(12) / -127 /
       DATA I2MACH(13) /  127 /
       DATA I2MACH(14) /   56 /
       DATA I2MACH(15) / -127 /
       DATA I2MACH(16) /  127 /
C
       DATA R2MACH(1) / O00000000200 /
       DATA R2MACH(2) / O37777677777 /
       DATA R2MACH(3) / O00000032200 /
       DATA R2MACH(4) / O00000032400 /
       DATA R2MACH(5) / O04046637632 /
C
       DATA ISMALL(1) /        128 /
       DATA ILARGE(1) /     -32769 /
       DATA IRIGHT(1) /      13440 /
       DATA IDIVER(1) /      13568 /
       DATA ILOG10(1) /  547045274 /
C
       DATA JSMALL(1),JSMALL(2) /        128,           0 /
       DATA JLARGE(1),JLARGE(2) /     -32769,          -1 /
       DATA JRIGHT(1),JRIGHT(2) /       9344,           0 /
       DATA JDIVER(1),JDIVER(2) /       9472,           0 /
       DATA JLOG10(1),JLOG10(2) /  546979738,  -805665541 /
       DATA JSMALL(1),JSMALL(2) / O00000000200, O00000000000 /
       DATA JLARGE(1),JLARGE(2) / O37777677777, O37777777777 /
       DATA JRIGHT(1),JRIGHT(2) / O00000022200, O00000000000 /
       DATA JDIVER(1),JDIVER(2) / O00000022400, O00000000000 /
       DATA JLOG10(1),JLOG10(2) / O04046437632, O31776502373 /
CC
       IHOST1='VAX'
       IHOST2='    '
       IHMOD1='11'
       IHMOD2='780'
       IOPSY1='VMS'
       IOPSY2='    '
       ICOMPI='    '
       ISITE='    '
#endif
C
C
C-----START POINT-----------------------------------------------------
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,51)
   51   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)
   52   FORMAT('***** AT THE BEGINNING OF INITMC--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *************************
C               **  COPY OVER INTEGER  **
C               **  MACHINE CONSTANTS  **
C               *************************
C
      DO100I=1,16
        I1MACH(I)=I2MACH(I)
  100 CONTINUE
C
C               ********************************************************
C               **  COPY OVER REAL (SINGLE PRECISION FLOATING POINT)  **
C               **  MACHINE CONSTANTS                                 **
C               ********************************************************
C
      DO200I=1,5
        R1MACH(I)=R2MACH(I)
  200 CONTINUE
C
CC              **********************************
CC              **  COPY OVER DOUBLE PRECISION  **
CC              **  MACHINE CONSTANTS           **
CC              **********************************
C
CCCCC THE FOLLOWING 3 LINES WERE COMMENTED OUT MAY 1992 (JJF)
CCCCC TO AVOID UNEXPLAINABLE OVERFLOW PROBLEMS MAY 1992 (JJF)
      DO300I=1,5
        D1MACH(I)=D2MACH(I)
  300 CONTINUE
C
C               **************************************
C               **  COMPUTE SELECTED COMMONLY-USED  **
C               **  MACHINE CONSTANTS               **
C               **************************************
C
      IRD=I2MACH(1)
      IPR=I2MACH(2)
C
CCCCC THE FOLLOWING 5 LINES WERE ENTERED                         FEBRUARY 1989
CCCCC TO SET DIFFERENT UNITS FOR ALPHANUMERIC AND GRAPHICS I/O.  FEBRUARY 1989
CCCCC MOST HOSTS WILL SET THE SAME.  CDC NOS/VE REQUIRES GRAPHICS I/O
CCCCC TO BE IN "TRANSPARENT MODE", ALPHANUMERIC IN "NON-TRANSPARENT"
      IPRGR=IPR
      IRDGR=IRD
      IF(IHOST1.EQ.'NVE') IPRGR=6
      IF(IHOST1.EQ.'NVE') IPR=7
      IF(IHOST1.EQ.'NVE') IRDGR=4
C
      CPUMIN=-R2MACH(2)
      CPUMAX=R2MACH(2)
      NUMBPW=I2MACH(5)
      NUMCPW=I2MACH(6)
      NUMBPC=NUMBPW/NUMCPW
C
#ifdef INTEL_QWIN
      ISTATUS=SETEXITQQ(QWIN$EXITNOPERSIST)
      IQWNFT=INITIALIZEFONTS()
      IQWNFN=GETFONTINFO(MSFONT)
C
CCCCC NOVEMBER 2002.
CCCCC 1. RESET TITLE ON TEXT WINDOW.
CCCCC 2. MODIFY SOME OF THE MENU ITEMS
CCCCC 3. MODIFY THE HELP MENU TO GIVE INSTRUCTIONS FOR DATAPLOT
CCCCC    HELP
C
      IRESLT=SETACTIVEQQ(IPR)
      MODESTATUS=GETWINDOWCONFIG(DPSCREEN)
      DPSCREEN.TITLE="Dataplot Text Window"
      DPSCREEN%NUMTEXTROWS=500
      DPSCREEN%NUMTEXTCOLS=132
      DPSCREEN%mode = QWIN$SCROLLDOWN
      DPSCREEN.NUMXPIXELS=-1
      DPSCREEN.NUMYPIXELS=-1
      MODESTATUS=SETWINDOWCONFIG(DPSCREEN)
      IRESLT=ABOUTBOXQQ("Dataplot: Version 6/2013.  "//
     1"Dataplot is a language for graphics, fitting, "//
     1"general data analysis, and mathematics.  Dataplot "//
     1"commands are high-level, English-syntax, and "//
     1"self-descriptive.  The three most important commands are "//
     1"PLOT, FIT, and LET."C)
      IRESLT=MODIFYMENUROUTINEQQ(6,1,IQWNBR)
      IRESLT=MODIFYMENUROUTINEQQ(6,2,IQWNHE)
CCCCC IRESLT=MODIFYMENUROUTINEQQ(6,3,IQWNAB)
CCCCC IRESLT=DELETEMENUQQ(6,1)
C
CCCCC MARCH 2002: SET FOREGROUND/BACKGROUND COLOR FOR TEXT
CCCCC SCREEN BASED ON WHETHER OR NOT TRUE COLOR SPECIFIED.
C
      IF(IQWNCL.EQ.'VGA')THEN
         IQWNBC=0
         IQWNF2=15
         IRESLT=SETACTIVEQQ(IPR)
CCCCC    IRESLT=SETBKCOLOR(INT2(IQWNBC))
         IRESLT=SETBKCOLOR(IQWNBC)
         IRESLT=SETTEXTCOLOR(INT2(IQWNF2))
         CALL CLEARSCREEN($GCLEARSCREEN)
      ELSE
        IQWNBC=1
        IQWNF2=0
        IRESLT=SETACTIVEQQ(IPR)
        JTEMP=IQWNF2+1
        JTEMP2=RGBTOINTEGER(IRED(JTEMP),IGREEN(JTEMP),IBLUE(JTEMP))
        ISTATUS=SETTEXTCOLORRGB(JTEMP2)
        ISTATUS=SETCOLORRGB(JTEMP2)
        CALL CLEARSCREEN($GCLEARSCREEN)
        ISTATUS=DISPLAYCURSOR($GCURSORON)
      ENDIF
C
      DPSCREEN%mode = QWIN$SCROLLDOWN
      MODESTATUS=SETWINDOWCONFIG(DPSCREEN)
#endif
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF INITMC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IHOST1,IHOST2,IHMOD2,IHMOD2
 9012   FORMAT('IHOST1,IHOST2 (HOST),IHMOD2,IHMOD2 (MODEL)= ',
     1         2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IOPSY1,IOPSY2,ICOMPI
 9014   FORMAT('IOPSY1,IOPSY2 (OPERATING SYSTEM),ICOMPI (COMPILER) = ',
     1         A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISITE
 9016   FORMAT('ISITE         (SITE) = ',A4)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IPR,IRD,CPUMIN,CPUMAX
 9022   FORMAT('IPR,IRD,CPUMIN,CPUMAX = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)NUMBPC,NUMCPW,NUMBPW
 9024   FORMAT('NUMBPC,NUMCPW,NUMBPW = ',3I8)
        CALL DPWRST('XXX','BUG ')
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9030I=1,16
          IF(NUMBPW.EQ.32)THEN
            WRITE(ICOUT,9031)I,I1MACH(I)
 9031       FORMAT('I,I1MACH(I) = ',I8,2X,I11)
            CALL DPWRST('XXX','BUG ')
          ELSEIF(NUMBPW.EQ.36)THEN
            WRITE(ICOUT,9032)I,I1MACH(I)
 9032       FORMAT('I,I1MACH(I) = ',I8,2X,I12)
            CALL DPWRST('XXX','BUG ')
          ELSEIF(NUMBPW.EQ.48)THEN
            WRITE(ICOUT,9033)I,I1MACH(I)
 9033       FORMAT('I,I1MACH(I) = ',I8,2X,I16)
            CALL DPWRST('XXX','BUG ')
          ELSEIF(NUMBPW.EQ.60)THEN
            WRITE(ICOUT,9034)I,I1MACH(I)
 9034       FORMAT('I,I1MACH(I) = ',I8,2X,I20)
            CALL DPWRST('XXX','BUG ')
          ELSE
            WRITE(ICOUT,9035)I,I1MACH(I)
 9035       FORMAT('I,I1MACH(I) = ',I8,2X,I8)
            CALL DPWRST('XXX','BUG ')
          ENDIF
 9030   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9040I=1,5
          WRITE(ICOUT,9041)I,R1MACH(I)
 9041     FORMAT('I,R1MACH(I)  = ',I8,2X,E15.7)
          CALL DPWRST('XXX','BUG ')
 9040   CONTINUE
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO9050I=1,5
          WRITE(ICOUT,9051)I,D1MACH(I)
 9051     FORMAT('I,D1MACH(I)  = ',I8,2X,D15.7)
          CALL DPWRST('XXX','BUG ')
 9050   CONTINUE
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE INITFO(IBUGIN)
C
CCCCC THE FOLLOWING SECTION WAS ADDED JUNE 1990
C     IMPLEMENTATION NOTE--DATAPLOT CANNOT BE LINKED/LOADED
C           WITHOUT AN EDITOR PASS OF THIS FILE SO AS TO
C           SPECIFY THE FILE NAMES ON YOUR COMPUTER OF CHOICE.
C
C           DATAPLOT USES 2 TYPES OF FILES.  PERMANENT FILES (E.G., THE
C           ON-LINE HELP FILES) AND TEMPORARY FILES (E.G., THE PLOT FILE)
C           CREATED DURING A DATAPLOT SESSION.  THIS ROUTINE DEFINES WHERE
C           THE PERMANENT FILES WILL BE FOUND AND WHERE THE TEMPORARY FILES
C           WILL BE CREATED.  THIS WILL VARY FROM DEPENDING ON THE
C           HOST, OPERATING SYSTEM, AND THE SITE.  PRE-PROCESSOR
C           DIRECTIVES ARE USED TO ACTIVATE CODE FOR A SPECIFIC
C           OPERATING SYSTEM.
C
C           NO CHANGES REQUIRED AFTER THE LINE "END OF USER CHANGES".
C           HOWEVER, BE SURE TO CHECK THE ROUTINE DPOPFI WHICH MAY REQUIRE
C           A FEW LINES TO BE MODIFIED IN ORDER TO AUTOMATICALLY ACCESS
C           DATAPLOT REFERENCE FILES (I.E., SAMPLE DATA AND MACRO FILES).
C
C     PURPOSE--THIS IS SUBROUTING INITFO.
C              (THE   FO    AT THE END OF    INITFO   STANDS FOR
C              FILE OPERATIONS.
C              THIS SUBROUTINE DEFINES ALL OF THE FILE NAMES
C              THAT DATAPLOT USES, AND ALSO DEFINES
C              ATTRIBUTES OF SUCH FILES.
C
C     NOTE--TYPICAL FILE NAMES FOR THE MESSAGE FILE
C           FOR VARIOUS COMPUTERS--
C              VAX             --[DATAPLOT]DPMESF.TEX
C              CDC (NOS-2)     --DPMESF
C              CDC (NOS/VE)    --
C              HONEYWELL       --udd>dataplot>dpmesf.text
C              PERKIN-ELMER    --CALX:DPMESF.TEX/255       ACCOUNT:FILE.EXT/ACCO
C              IBM (EBCDIC)    --
C              PRIME           --DATAPLOT>DPMESF.TEX
C              UNIVAC          --
C              SUN             --/usr/local/lib/dataplot/dpmesf.tex
C              AT&T 3B20 (UNIX)--
C              APOLLO          --
C              DATA GENERAL    --
C              UNIX            --/usr/local/lib/dataplot/dpmesf.tex
C     NOTE--TYPICAL FORTRAN EXTENSIONS FOR DATAPLOT'S MAIN ROUTINE
C           FOR VARIOUS COMPUTERS--
C              VAX             --[DATAPLOT]MAIN.FOR
C              CDC (NOS-2)     --MAIN
C              CDC (NOS/VE)    --
C              HONEYWELL       --dataplot>main.fortran
C              PERKIN-ELMER    --
C              IBM (EBCDIC)    --
C              PRIME           --DATAPLOT>MAIN.F77
C              UNIVAC          --DATAPLOT.MAIN
C              SUN             --/usr/local/src/dataplot/main.f
C              AT&T 3B20 (UNIX)--
C              APOLLO          --
C              DATA GENERAL    --
C              CRAY            --
C     NOTE--TYPICAL INCLUDE STATEMENTS FOR VARIOUS COMPUTERS--
C              VAX             --INCLUDE 'DPCOMC.INC' (START IN COL. 7)
C              CDC (NOS-2)     --(NO INCLUDE CAPABILITY)
C              CDC (NOS/VE)    --
C              HONEYWELL       --%INCLUDE DPCOMC (START IN COL. 1)
C                                (AND SEARCHES FOR DPCOMC.INCL.FORTRAN)
C              PERKIN-ELMER    --
C              IBM (EBCDIC)    --
C              PRIME           --$INSERT DPCOMC.INC (START IN COL. 1)
C              UNIVAC          --
C                                (MUST PREPROCESS WITH PDP PROCESSOR)
C              SUN             --INCLUDE 'DPCOMC.INC'
C              AT&T 3B20 (UNIX)--
C              APOLLO          --
C              DATA GENERAL    --
C              CRAY            --
C     THE FILES THAT DATAPLOT USES ARE  --
C           1) A SIGN-ON MESSAGE FILE CONTAINING THE LATEST IN DATAPLOT
C              INFORMATION.  THIS FILE IS AUTOMATICALLY PRINTED OUT IN
C              THE FORM OF A MESSAGE WHICH THE ANALYST SEES WHEN SIGNING
C              ONTO DATAPLOT.  IT TYPICALLY CONSISTS OF ONLY A FEW LINES
C              OF INFORMATION.  THE VARIABLE NAMES ALL START WITH    IMES,
C              AS IN IMESNU, IMESNA, IMESST, ETC.
C           2) A NEWS FILE WHICH DATAPLOT MAKES USE OF WHENEVER THE
C              ANALYST ENTERS THE NEWS COMMAND.  THE VARIABLE NAMES ALL
C              START WITH    INEW,
C           3) A MAIL FILE DATAPLOT MAKES USE OF WHENEVER THE ANALYST
C              ENTERS THE MAIL COMMAND FOLLOWED BY HIS/HER LAST NAME.
C              THE VARIABLE NAMES ALL START WITH    IMAI.
C              NOTE: THE MAIL COMMAND IS OBSOLETE AND HAS BEEN REMOVED
C              FROM DATAPLOT.
C           4) A HELP (= DOCUMENTATION) FILE THAT DATAPLOT MAKES USE OF
C              WHENEVER THE ANALYST ENTERS THE HELP COMMAND.  THE VARIABLE
C              NAMES ALL START WITH    IHEL, AS IN IHELNU, IHELNA,
C              IHELST, ETC.
C           5) A BUGS FILE THAT DATAPLOT MAKES USE OF WHENEVER THE ANALYST
C              ENTERS THE BUGS COMMAND.  THE VARIABLE NAMES ALL START WITH
C              IBUG, AS IN IBUGNU, IBUGNA, IBUGST, ETC.
C           6) A QUERY FILE THAT DATAPLOT WRITES TO WHENEVER THE ANALYST
C              ENTERS THE QUERY COMMAND FOLLOWED BY A COMMENT OF INTEREST.
C              THE VARIABLE NAMES ALL START WITH    IQUE,
C              AS IN IQUENU, IQUENA, IQUEST, ETC.
C           7) A SIGN-ON SYSTEM LOGIN FILE THAT GETS CALLED EVERY TIME THAT
C              DATAPLOT GETS INVOKED.  THIS FILE IS A HANDY PLACE FOR THE
C              IMPLEMENTOR TO PLACE DATAPLOT COMMANDS SO AS TO TAILOR
C              DATAPLOT FOR AN ENTIRE SITE.  THE VARIABLE NAMES ALL START
C              WITH    ISYS, AS IN ISYSNU, ISYSNA, ISYSST, ETC.
C           8) A USER LOGIN FILE (IN THE USER'S DIRECTORY) THAT GETS
C              CALLED EVERY TIME THAT DATAPLOT GETS INVOKED BY THAT USER.
C              THIS FILE IS A HANDY PLACE FOR THE USER TO PLACE DATAPLOT
C              COMMANDS SO AS TO TAILOR DATAPLOT FOR THE INDIVIDUAL USER'S
C              PARTICULAR TERMINAL AND PLOTTER.  THE VARIABLE NAMES ALL
C              START WITH    ILOG, AS IN ILOGNU, ILOGNA, ILOGST, ETC.
C           9) A DIRECTORY FILE WHICH CONSISTS OF A LIST OF FILE NAMES
C              (AND 1-LINE DESCRIPTIONS) FOR INDIVIDUAL ON-LINE MASTER
C              REFERENCE FILES, INDIVIDUAL ON-LINE DATA FILES, AND
C              INDIVIDUAL ON-LINE PROGRAM FILES.  THIS FILE IS USUALLY
C              ACCESSED VIA THE LIST AND SEARCH COMMANDS.
C          10) A READ  FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
C              ARISES IN CONNECTION WITH THE READ AND SERIAL READ
C              COMMANDS IN READING VARIABLES/PARAMETERS/FUNCTIONS IN FROM
C              A MASS STORAGE FILE.  THE VARIABLE NAMES ALL START WITH
C              IREA, AS IN IREANU, IREANA, IREAST, ETC.
C          11) A WRITE FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
C              ARISES IN CONNECTION WITH THE WRITE COMMAND IN WRITING
C              VARIABLES/PARAMETERS/FUNCTIONS OUT TO A MASS STORAGE FILE.
C              THE VARIABLE NAMES ALL START WITH    IWRI, AS IN IWRINU,
C              IWRINA, IWRIST, ETC.
C          12) A SAVE FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
C              ARISES IN CONNECTION WITH THE SAVE AND RESTORE COMMANDS
C              IN EFFICIENTLY DUMPING OUT (OR ROLLING BACK IN) ALL OF
C              THE DATAPLOT INTERNAL SETTINGS FOR RESUMING A DATAPLOT RUN
C              AT A LATER TIME.  THIS FILE IS USED IN CONNECTION WITH
C              THE SAVE AND RESTORE COMMANDS.  THE VARIABLE NAMES ALL
C              START WITH    ISAV, AS IN ISAVNU, ISAVNA, ISAVST, ETC.
C          13) A LIST  FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
C              ARISES IN CONNECTION WITH THE LIST COMMAND IN PASSIVELY
C              LISTING THE CONTENTS OF A MASS STORAGE FILE.  THE VARIABLE
C              NAMES ALL START WITH    ILIS, AS IN ILISNU, ILISNA,
C              ILISST, ETC.
C          14) A MACRO FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
C              ARISES IN CONNECTION WITH THE    CREATE    AND    CALL
C              COMMANDS WHEN DYNAMICALLY FORMING OR EXECUTING A MACRO
C              WHILE RUNNING DATAPLOT.  THE VARIABLE NAMES ALL START
C              WITH    ICRE, AS IN ICRENU, ICRENA, ICREST, ETC.
C          15) A (TEXT) CAPTURE FILE WHOSE NAME IS SUPPLIED BY THE ANALYST
C              AND ARISES IN CONNECTION WITH THE CAPTURE/REDIRECT COMMANDS
C              WHEN DYNAMICALLY CAPTURING TEXT OUTPUT FROM ANY DATAPLOT
C              COMMANDS.  THE VARIABLE NAMES ALL START WITH    ICAP,
C              AS IN ICAPNU, ICAPNA, ICAPST, ETC.
C          16) A TEMPORARY SCRATCH FILE THAT DATAPLOT MAKES USE OF (TO
C              SAVE SPACE) DURING THE FIT COMMAND, THE PRE-FIT COMMAND,
C              AND THE SPLINE FIT COMMAND.  THE VARIABLE NAMES ALL START
C              WITH    ISCR, AS IN ISCRNU, ISCRNA, ISCRST, ETC.
C          17) FOR FUTURE DEVELOPMENT-- A DATA FILE THAT DATAPLOT COULD
C              COULD MAKE USE OF IN STORING THE MAIN INTERNAL DATA ARRAY
C              IF SUCH AN ARRAY IS LARGER THAN CAN BE HELD INTERNALLY IN
C              MAIN MEMORY.  SUCH A DATA FILE IS NOT CURRENTLY USED
C              BUT HAS BEEN ENTERED FOR FUTURE DEVELOPMENT.  THE VARIABLE
C              NAMES ALL START WITH    IDAT, AS IN IDATNU, IDATNA,
C              IDATST, ETC.
C          18) A PLOT FILE THAT DATAPLOT WRITES A PLOT OUT TO WHENEVER
C              SIMULTANEOUS SECONDARY PLOTS ARE CALLED FOR
C              (AS IN DEVICE 2 TEKTRONIX 4014
C                     DEVICE 2 HP-GL
C                     DEVICE 2 GENERAL
C                     DEVICE 2 etc.
C              THE VARIABLE NAMES ALL  TART WITH    IPL1,
C              AS IN IPL1NU, IPL1NA, IPL1ST, ETC.
C          19) ANOTHER PLOT FILE THAT DATAPLOT COULD WRITE A PLOT OUT TO
C              WHENEVER SIMULTANEOUS TERTIARY PLOTS ARE CALLED FOR
C              (AS IN DEVICE 3 TEKTRONIX 4014
C                     DEVICE 3 HP-GL
C                     DEVICE 3 GENERAL
C                     DEVICE 3 etc.
C              THE VARIABLE NAMES ALL START WITH    IPL2,
C              AS IN IPL2NU, IPL2NA, IPL2ST, ETC.
C          20) A PROGRAM FILE THAT DATAPLOT WRITES TO AND RUNS FROM IN
C              CONJUNCTION WITH CERTAIN "PRE-PACKAGED" COMMANDS SUCH AS
C              4-PLOT    AND    RUN RANDOMNESS.  THE VARIABLE NAMES ALL
C              START WITH    IPRO, AS IN IPRONU, IPRONA, IPROST, ETC.
C          21) A CONCLUSIONS FILE THAT DATAPLOT WRITES TO AND READS FROM
C              IN CONJUNCTION WITH FORMING CONCLUSIONS AS PART OF
C              DATAPLOT'S EXPERT SUB-SYSTEM THE VARIABLE NAMES ALL START
C              WITH    ICON, AS IN ICONNU, ICONNA, ICONST, ETC.
C          22) A COMMAND-SAVE FILE THAT DATAPLOT WRITES TO AND READS FROM
C              IN CONJUNCTION WITH SAVING COMMANDS (VIA THE    SAVE COMMAND
C              COMMAND), AND REEXECUTING COMMANDS (VIA THE CALL COMMAND).
C              THE VARIABLE NAMES ALL START WITH    ISAC,
C              AS IN ISACNU, ISACNA, ISACST, ETC.
C          23) A LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
C              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
C              EXPERT SUB-SYSTEM.  THE VARIABLE NAMES ALL START WITH
C              IEX1, AS IN IEX1NU, IEX1NA, IEX1ST, ETC.
C          24) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
C              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
C              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
C              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX2,
C              AS IN IEX2NU, IEX2NA, IEX2ST, ETC.
C          25) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
C              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
C              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
C              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX3,
C              AS IN IEX3NU, IEX3NA, IEX3ST, ETC.
C          26) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
C              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
C              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
C              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX4,
C              AS IN IEX4NU, IEX4NA, IEX4ST, ETC.
C          27) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
C              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
C              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
C              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX5,
C              AS IN IEX5NU, IEX5NA, IEX5ST, ETC.
CCCCC THE FOLLOWING SECTION IS A SHRINKAGE OF 9 SECTIONS JUNE 1990
C          28 TO 37) HELP (DOCUMENTATION) FILES WHICH DATAPLOT ACCESSES
C              IN CONJUNCTION WITH DISPLAYING INFORMATION AS PART OF
C              DATAPLOT'S HELP SUB-SYSTEM.  THE VARIABLE NAMES ALL START
C              WITH    IHE1 THROUGH IHE9, AS IN     IHE1NU, IHE1NA,
C              IHE1ST, ETC.  THROUGH   IHE9NU, IHE9NA, IHE9ST, ETC.
CCCCC THE FOLLOWING 9 SECTIONS WERE ADDED JUNE 1990
C          38 TO 47) MENU FILES WHICH DATAPLOT ACCESSES IN IN CONJUNCTION
C              WITH DISPLAYING INFORMATION AS PART OF DATAPLOT'S MENU
C              SUB-SYSTEM.  THE VARIABLE NAMES ALL START WITH    IME1
C              THROUGH IME9, AS IN     IME1NU, IME1NA, IME1ST, ETC.
C              THROUGH   IME9NU, IME9NA, IME9ST, ETC.
C              NOTE THAT THE MENU SUBSYSTEM IS OBSOLETE AND THESE FILES
C              ARE NO LONGER USED.
C     THE FILE ATTRIBUTES THAT DATAPLOT DEFINES ARE--
C           1) THE FORTRAN LOGICAL UNIT NUMBER
C              (AN INTEGER).
C              THE VARIABLE NAMES ALL END IN    NU,
C              AS IN IMESNU, IHELNU, IREANU, ETC.
C              RECOMMENDED SETTINGS (IF THESE POSE A CONFLICT
C              AT YOUR SITE, THEN CHANGE THEM ACCORDINGLY)--
C
C                 IMESNU=21
C                 INEWNU=22
C                 IMAINU=23
C                 IHELNU=24
C                 IBUGNU=25
C                 IQUENU=26
C                 ISYSNU=27
C                 ILOGNU=28
C                 IDIRNU=29
C                 IDICNU=30
C
C                 IREANU=31
C                 IWRINU=32
C                 ISAVNU=33
C                 ILISNU=34
C                 ICRENU=35
C
C                 ISCRNU=41
C                 IDATNU=42
C                 IPL1NU=43
C                 IPL2NU=44
C                 IPRONU=45
C                 ICONNU=46
C                 ISACNU=47
C
C                 IEX1NU=51
C                 IEX2NU=52
C                 IEX3NU=53
C                 IEX4NU=54
C                 IEX5NU=55
C
CCCCC THE FOLLOWING LINE WAS ADDED APRIL 1997
C                 IHRMNU=60
C                 IHE1NU=61
C                 IHE2NU=62
C                 IHE3NU=63
C                 IHE4NU=64
C                 IHE5NU=65
C                 IHE6NU=66
C                 IHE7NU=67
C                 IHE8NU=68
C                 IHE9NU=69
 
CCCCC THE FOLLOWING 9 LINES WERE ADDED JUNE 1990
C                 IME1NU=71
C                 IME2NU=72
C                 IME3NU=73
C                 IME4NU=74
C                 IME5NU=75
C                 IME6NU=76
C                 IME7NU=77
C                 IME8NU=78
C                 IME9NU=79
CCCCC THE FOLLOWING 11 LINES WERE ADDED AUGUST 1990
C                 IM10NU=80
C                 IM11NU=81
C                 IM12NU=82
C                 IM13NU=83
C                 IM14NU=84
C                 IM15NU=85
C                 IM16NU=86
C                 IM17NU=87
C                 IM18NU=88
C                 IM19NU=89
C                 IM20NU=90
CCCCC THE FOLLOWING 3 LINES WERE ADDED OCTOBER 1991
C                 IST1NU=91
C                 IST2NU=92
C                 IST3NU=93
CCCCC THE FOLLOWING LINE WAS ADDED FEBRUARY 1994
C                 IST4NU=94
C                 IST5NU=95
CCCCC THE FOLLOWING LINE WAS ADDED JANUARY  2004
C                 IZCHNU=97
CCCCC THE FOLLOWING LINE WAS ADDED DECEMBER 2018
C                 ILINNU=98
C
C           2) THE FILE NAME (A CHARACTER*80 VARIABLE).  THE VARIABLE
C              NAMES ALL END IN    NA, AS IN IMESNA, IHELNA, IREANA, ETC.
C              FILES 21 TO 27 AND 51 AND ABOVE ARE PERMANENT DATAPLOT
C              FILES, AND SO THE FULL FILE NAME DEFINING EXACTLY WHERE THE
C              FILE RESIDES SHOULD BE EXPLICIT (INCLUDING, IF NEED BE, THE
C              DEVICE, DIRECTORY, AND SUBDIRECTORIES SHOULD BE INCLUDED).
C              THESE FILES USUALLY RESIDE IN A MASTER DATAPLOT DIRECTORY
C              OR IN THE IMPLEMENTOR'S DIRECTORY; IF SUCH IS THE CASE,
C              THEN ALSO MAKE SURE THE SYSTEM-PROTECTION ON THESE FILES
C              IS SUCH THAT ANYBODY ELSE CAN ACCESS THEM--THAT IS, ALLOW
C              "WORLD" ACCESS.  FILES 31 TO 35 ARE USER-DEFINED FILES
C              AND SO CONTAIN A DUMMY NAME (-999).  FILES 41 TO 46 ARE
C              DATAPLOT-GENERATED FILES WHICH WILL END UP IN THE USER'S
C              CURRENT DIRECTORY.  THESE FILES MAY BE EITHER TEMPORARY OR
C              PERMANENT IN THE SENSE THAT THE USER MAY EITHER MANUALLY OR
C              AUTOMATICALLY DELETE THEM (IF HE/SHE SO CHOOSES) AFTER
C              EXITING OUT OF DATAPLOT.
C
C           3) THE FILE (EXISTENCE) STATUS (A CHARACTER*12 VARIABLE).
C              THE VARIABLE NAMES ALL END IN    ST, AS IN IMESST, IHELST,
C              IREAST, ETC.  THERE ARE 3 POSSIBLE SETTINGS--
C                 1) OLD     (THAT IS, THE FILE PRE-EXISTS)
C                 2) NEW     (THAT IS, THE FILE DOES NOT PRE-EXIST)
C                 3) UNKNOWN (THAT IS, EITHER CASE IS POSSIBLE)
C              FILES 21 TO 27 ARE OLD.
C              FILES 31 TO 35 ARE UNKNOWN.
C              FILES 41 TO 46 ARE UNKNOWN.
C              FILES 51 AND ABOVE ARE OLD.
C
C           4) THE FILE (FORTRAN I/O) FORMAT (A CHARACTER*12 VARIABLE).
C              THE VARIABLE NAMES ALL END IN    FO, AS IN IMESFO, IHELFO,
C              IREAFO, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
C                 1) FORMATTED   (THAT IS, THE CONTENTS OF THE FILE ARE
C                                READABLE VIA A FORMATTED FORTRAN READ).
C                                THE FILE IS THUS EDITABLE VIA MOST EDITORS,
C                                BUT ARE SLOWER TO CREATE AND READ.
C                 2) UNFORMATTED (THAT IS, THE CONTENTS OF THE FILE
C                                ARE READABLE ONLY VIA AN UNFORMATTED
C                                FORTRAN READ.  THE FILE IS THUS
C                                UNEDITABLE BY MOST EDITORS,
C                                BUT ARE FASTER TO CREATE AND READ.
C               ALL OF DATAPLOT'S FILES ARE FORMATTED
C               EXCEPT THE SCRATCH FILE (ISCRNA--FILE 41
C               AND    THE DATA    FILE (IDATNA--FILE 42).
C
C           5) THE FILE ACCESS ATTRIBUTE (A CHARACTER*12 VARIABLE).
C              THE VARIABLE NAMES ALL END IN    AC, AS IN IMESAC, IHELAC,
C              IREAAC, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
C                 1) SEQUENTIAL (THAT IS, THE CONTENTS OF THE FILE ARE
C                               ACCESSED IN A SEQUENTIAL FASHION.
C                               SEQUENTIAL ACCESS FILES ARE SIMPLER IN
C                               STRUCTURE BUT SLOWER TO ACCESS.
C     b           2) DIRECT     (THAT IS, THE CONTENTS OF THE FILE
C                               ARE ACCESSED DIRECTLY--
C                               A RECORD IN THE MIDDLE OF THE FILE
C                               MAY THUS BE ACCESSED DIRECTLY WITHOUT
C                               THE NEED TO READ THROUGH ALL PREVIOUS
C                               RECORDS.  DIRECT ACCESS FILES
C                               ARE USUALLY UNEDITABLE, ARE
C                               USUALLY MORE COMPLICATED IN STRUTURE,
C                               BUT ARE FASTER TO ACCESS.
C              DIRECT-ACCESS FILES ARE NOT SUPPORTED IN FORTRAN 77, THUS
C              DATAPLOT DOES NOT MAKE USE OF THEM (THAT IS, ALL OF
C              DATAPLOT'S FILE ARE SEQUENTIAL).  IF ONE WERE TO DEVIATE
C              FROM DATAPLOT'S DEFAULT SETTINGS IN REGARD TO SEQUENTIAL
C              VERSUS DIRECT-ACCESS FILES, THEN THE PRIMARY CANDIDATE
C              WOULD BE THE HELP FILE (IHELNA)--MAKING THIS DIRECT ACCESS
C              WOULD SPEED UP THE USE OF THE HELP COMMAND; THIS SHOULD BE
C              DONE ONLY, HOWEVER, AFTER THE DEFAULT DATAPLOT
C              IMPLEMENTATION HAS BEEN DONE AND IS RUNNING SATISFACTORILY.
C
C           6) THE FILE READ/WRITE PROTECTION ATTRIBUTE (A CHARACTER*12
C              VARIABLE).  THE VARIABLE NAMES ALL END IN  PR, AS IN IMESPR,
C              IHELPR, IREAPR, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
C                 1) READWRITE (THAT IS, THE CONTENTS OF THE FILE MAY
CC                              SEQUENTIAL ACCESS FILES ARE SIMPLER IN
C                               BE BOTH READ FROM AND WRITTEN TO DURING A
C                               DATAPLOT RUN.  THE FILE IS THUS FREELY
C                               ACCESSED FOR BOTH READING AND WRITING.
C                 2) READONLY   (THAT IS, THE FILE MAY
C                               BE READ FROM, BUT MAY NOT BE WRITTEN INTO.
C                               THE FILE THUS HAS ONLY LIMITED ACCESS.
C              FILES 21 TO 27 (EXCEPT FILE 23) ARE READONLY.
C              FILES 31 TO 35 ARE READWRITE.
C              FILES 41 TO 46 ARE READWRITE.
C              FILES 51 AND ABOVE ARE READONLY.
C
C           7) THE FILE OPEN/CLOSE STATUS (A CHARACTER*12 VARIABLE).
C              THE VARIABLE NAMES ALL END IN  CS, AS IN IMESCS, IHELCS,
C              IREACS, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
C                 1) OPEN   (THAT IS, THE FILE IS CURRENTLY OPEN).
C                 2) CLOSED (THAT IS, THE FILE IS CURRENTLY CLOSED).
C              UPON ACCESSING DATAPLOT, ALL FILES ARE CLOSED.  AT
C              VARIOUS TIMES WITHIN A DATAPLOT RUN, A GIVEN FILE MAY BE
C              OPEN OR CLOSED--DEPENDING ON WHAT THE ANALYST IS DOING.
C              UPON EXITING DATAPLOT, ANY FILES WHICH HAPPEN
C              TO BE OPEN WILL BE CLOSED.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--DECEMBER  1985.
C     UPDATED         --JULY      1986.
C     UPDATED         --SEPTEMBER 1987. (EXPANDED HELP)
C     UPDATED         --JANUARY   1988. (DIRECTORY FILE)
C     UPDATED         --AUGUST    1988. (DICTIONARY FILE)
C     UPDATED         --JUNE      1990. MENU 1 THRU 9
C     UPDATED         --JULY      1990. PL1/PL2/CON/SAC  NEW TO UNKNOWN
C     UPDATED         --AUGUST    1990. MENU 11 THRU 20
C     UPDATED         --SEPTEMBER 1990. USER-DEFINABLE DOS DIRECTORY
C     UPDATED         --APRIL     1991. MERGE ALAN/JJF VERSIONS
C     UPDATED         --OCTOBER   1991. STORAGE 1, 2, AND 3
C     UPDATED         --NOVEMBER  1991. HEAVILY MODIFIED FOR EASIER
C                                       USER IMPLEMENTATION (ALAN)
C     UPDATED         --MARCH     1992. GENERAL OUTPUT FILE
C                                       (INCLUDING LASER PRINTER)
C     UPDATED         --APRIL     1992. ADD SOME DECLARATIONS, MAKE
C                                       MODIFICATION INSTRUCTIONS CLEAR
C     UPDATED         --AUGUST    1992. FILE PERMISSION FOR DPST<1/2/3>F
C     UPDATED         --AUGUST    1992. FOR EDIT COMMAND
C     UPDATED         --JANUARY   1994. CHECK FOR SET DATAPLO$, FED$
C     UPDATED         --FEBRUARY  1994. DELETE SOME OBSOLETE COMMENTS
C                                       TO AVOID CONFUSION.
C     UPDATED         --APRIL     1996. FOR UNIX, ALLOW FILE AREA FOR
C                                       TO BE SET VIA:
C                                         setenv DATAPLOT_FILES
C     UPDATED         --APRIL     1996. SET PATH, NCPATH FOR PC
C     UPDATED         --JULY      1996. FOR UNIX, CHECK FOR PRESCENCE
C                                       OF "HOME" ENVIORNMENT VARIABLE
C                                       IF FOUND, READ DPLOGF FROM
C                                       HOME DIRECTORY RATHER THAN
C                                       CURRENT DIRECTORY
C     UPDATED         --JULY      1996. DATAPLOT_WEB VARIABLE
C     UPDATED         --AUGUST    1996. FIXES FOR SEARCHING SUB-DIRECTORIES
C     UPDATED         --APRIL     1997. BROWSER VARIABLE
C     UPDATED         --APRIL     1997. DATAPLOT_HOME_PAGE VARIABLE
C     UPDATED         --APRIL     1997. URL VARIABLE
C     UPDATED         --APRIL     1997. UNIT FOR WEB HELP COMMAND
C                                       (IHRMNU)
C     UPDATED         --APRIL     1997. COMBINE UNIX HOSTS
C     UPDATED         --APRIL     1997. DIFFERERT UNIT FOR CREATE AND
C                                       CALL
C     UPDATED         --MARCH     1999. UNIT FOR WEB HANDBOOK COMMAND
C     UPDATED         --JANUARY   2004. UNIT FOR CHARACTER DATA
C     UPDATED         --JUNE      2010. REMOVE MAIL AND QUERY FILES AS THESE
C                                       ARE NOW OBSOLETE.  ALSO REMOVE THE
C                                       "MENU" AND "EXPERT" FILES AS THESE ARE
C                                       ALSO NOW OBSOLETE.  RENUMBER SOME OF THE
C                                       FILES ACCORDINGLY.
C     UPDATED         --APRIL     2018. USE PRE-PROCESSSOR DIRECTIVES TO
C                                       ACTIVATE OS DEPENDENT CODE
C     UPDATED         --DECEMBER  2018. "ILINNU" FOR "READ LINE" COMMAND
C     UPDATED         --MARCH     2019. METHOD FOR EXTRACTING LOCATION
C                                       OF AUXILIARY FILES FOR LINUX
C     UPDATED         --MARCH     2019. FOR LINUX, USE "xdg-open" IF NO
C                                       BROWSER ENVIRONMENT VARIABLE SET
C     UPDATED         --DECEMBER  2019. RETRIEVE THE FOLLOWING
C                                       ENVIRONMENT VARIABLES FOR
C                                       WINDOWS:
C                                         1. %PROGRAMFILES%
C                                         2. %PROGRAMFILES(X86)%
C                                         3. %USERNAME%
C                                         4. %LOCALAPPDATA%
C                                         5. %COMPUTERNAME%
C                                         6. %USERPROFILE%
C                                         7. %PROCESSOR_ARCHITECTURE%
C                                       SAVE IN A COMMON BLOCK
C     UPDATED         --DECEMBER  2019. RETRIEVE THE FOLLOWING
C                                       ENVIRONMENT VARIABLES FOR
C                                       WINDOWS:
C                                         1. $HOME
C                                         2. $USER
C                                         3. $HOST
C                                         4. $PRINTER
C                                       SAVE IN A COMMON BLOCK
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
#ifdef INTEL_CONS
      USE MSFLIB
#endif
#ifdef INTEL_QWIN
      USE IFPORT
#endif
#ifdef INTEL
      CHARACTER*80 CTEMP
      CHARACTER*4 IBASLC
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
#endif
#ifdef WIN32_OTG
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
#endif
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 IBUGIN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C  NOVEMBER 1991.  FOLLOWING BLOCK ADDED
C
CCCCC CHARACTER*80 IPATH1
CCCCC CHARACTER*80 IPATH2
      CHARACTER (LEN=MAXFNC) :: IPATH1
      CHARACTER (LEN=MAXFNC) :: IPATH2
      CHARACTER*6  INAME
      CHARACTER*10 IEXT1
      CHARACTER*10 IEXT2
      CHARACTER*4  ICASFL
C
C  JUNE 1996.  FOLLOWING BLOCK ADDED
C
CCCCC CHARACTER*80 IPATH3
      CHARACTER (LEN=MAXFNC) :: IPATH3
      CHARACTER*4 IFHOME
C
C  JULY 1996.  FOLLOWING BLOCK ADDED
C
#ifdef LINUX
      CHARACTER*20 IGUII2
CCCCC CHARACTER*80 ITEMP
      CHARACTER (LEN=MAXFNC) :: ITEMP
#endif
C
C
CCCCC THE FOLLOWING LINE WAS ADDED SEPTEMBER 1990
CCCCC CHARACTER*80 ICDIR
      CHARACTER (LEN=MAXFNC) :: ICDIR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
CCCCC AUGUST 1992.  FOLLOWING COMMON BLOCK FOR EDIT COMMAND
CCCCC CHARACTER*80 IEDDIR
      CHARACTER (LEN=MAXFNC) :: IEDDIR
      CHARACTER*10 IEDEXT
      CHARACTER*4 IEDCAS
      COMMON /ICEDC4/
     1IEDDIR,IEDEXT,IEDCAS
      COMMON/ICEDI4/
     1NCEDT1,NCEDT2
C
      CHARACTER*80 PROFIL
      CHARACTER*80 P86FIL
      CHARACTER*80 APPDAT
      CHARACTER*80 COMNAM
      CHARACTER*80 UPROFI
      CHARACTER*80 DEFPRI
      CHARACTER*20 USRNAM
      CHARACTER*20 ISHELL
      CHARACTER*4  WINBIT
      COMMON/SYSVAR/PROFIL,P86FIL,APPDAT,COMNAM,UPROFI,USRNAM,DEFPRI,
     1              WINBIT,ISHELL
      COMMON/SYSVA2/NCPROF,NCP86F,NCAPPD,NCCOMP,NCUPRO,NCUSER,NCPRIN,
     1              NCSHEL
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='INIT'
      ISUBN2='FO  '
      IFHOME='NO'
      IGUIIO='PIPE'
C
      PROFIL='C:\Program Files'
      NCPROF=16
      P86FIL='C:\Program Files (x86)'
      NCP86F=22
      APPDAT=' '
      NCP86F=0
      USRNAM=' '
      NCUSER=0
      ISHELL=' '
      NCSHEL=0
      COMNAM=' '
      NCCOMP=0
      UPROFI=' '
      NCUPRO=0
      WINBIT='64'
      DEFPRI=' '
      NCPRIN=0
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF INITFO--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C  MAKE USER CHANGES HERE!!!!
C
C  NOVEMBER 1991.  FOLLOWING SECTION ADDED.  ONLY NEED TO DEFINE "PATH"
C  AND FILE EXTENSIONS ONCE HERE.  COMPLICATES LATER CODE SOMEWHAT,
C  BUT SIMPLIFIES MAKING HOST DEPENDENT CHANGES.
C
C  DATAPLOT USES 2 TYPES OF FILES.  ONE ARE PERMAMNENT FILES SUCH AS
C  THE ON-LINE HELP FILES AND THE NEWS FILES.  THE LOCAL INSTALLOR CAN
C  PUT THESE FILES WHEREVER DESIRED.  THESE FILES TYPICALLY HAVE A
C  ".TEX" EXTENSION, ALTHOUGH THIS CAN BE SET HOWEVER THE LOCAL
C  IMPLEMENTOR CHOOSES.
C
C  THE SECOND TYPE OF FILES ARE TEMPORARY FILES CREATED DURING A
C  DATAPLOT SESSION.  THIS WOULD INCLUDE THE PLOT FILE, SCRATCH FILES,
C  AND OTHER MISCELLANEOUS FILES.  THESE FILES TYPICALLY ARE CREATED IN
C  THE USER'S CURRENT DIRECTORY OR IN SOME TYPE OF TEMPORARY DIRECTORY.
C  AGAIN, THE LOCAL INSTALLOR CAN MAKE THAT CHOICE.  THE FILE EXTENSION
C  IS TYPICALLY ".DAT", BUT THIS CAN ALSO BE SET BY THE LOCAL
C  IMPLEMENTOR.
C
C  IPATH1   = DIRECTORY NAME WHERE DATAPLOT PERMANENT FILES ARE STORED
C  IEXT1    = EXTENSION FOR PERMANENT FILES
C  IPATH2   = DIRECTORY NAME FOR TEMPORARY FILES (E.G., SCRATCH FILES)
C  IEXT2    = EXTENSION FOR TEMPORARY FILES
C  ICASFL   = 'UPPE' MEANS FILE NAMES ARE UPPER CASE, 'LOWE' MEANS FILE
C             NAMES ARE LOWER CASE.  TYPICALLY SET TO 'LOWE' FOR UNIX
C             SYSTEMS, 'UPPE' FOR OTHERS.
C  IEDDIR   = DIRECTORY FOR THE EDIT COMMAND (WILL USUALLY BE SAME AS
C             IPATH1, BUT DIFFERS ON PC)
C
C  THERE IS A CORRESPONDING VARIABLE THAT DEFINES THE NUMBER OF
C  CHARACTERS, NOTE THAT SETTING THIS VARIABLE TO ZERO IMPLIES NO PATH
C  OR EXTENSION.
C
C --------------------
C
      IF(IHOST1.EQ.'VAX')THEN
        IPATH1='DATAPLO$:'
        NCP1=9
        IEDDIR=IPATH1
        NCEDT1=NCP1
        IPATH2=' '
        NCP2=0
        IEXT1='.TEX'
        NCEXT1=4
        IEXT2='.DAT'
        NCEXT2=4
        ICASFL='UPPE'
C
      ELSE IF(IHOST1.EQ.'NVE')THEN
        IPATH1='.CS2.APPLICATIONS.DATAPLOT.VER_2.'
        NCP1=33
        IEDDIR=IPATH1
        NCEDT1=NCP1
C
C       FOR NOS/VE, IMPLEMENTOR CAN DECIDE WHETHER TO PUT TEMPORARY FILES
C       IN THE CURRENT CATALOG OR USE $LOCAL
C
        IPATH2='$LOCAL.'
        NCP2=7
CCCCC   IPATH2=' '
CCCCC   NCP2=0
C       END FILES WITH A ".".  THIS TRAILING DOT IS JUST TO IDENTIFY THE
C       NAME AS A FILE TO DATAPLOT.  THE "DPOPFI" ROUTINE WILL STRIP IT OFF.
        IEXT1='.'
        NCEXT1=1
        IEXT2='.'
        NCEXT2=1
        ICASFL='UPPE'
C
CCCCC APRIL 1996.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF
CCCCC              "DATAPLOT_FILES" ENVIRONMENT VARIABLE
CCCCC APRIL 1997.  REDUCE TO 1 UNIX SECTION (A BUNCH OF CODE WAS
CCCCC              DELETED< ESSENTIALLY REDUNDANT)
CCCCC FEBRUARY 1998. FOLLOWING SECTION ADDED. WINDOWS VERSION OF
CCCCC                GUI NEEDS SPECIAL HANDLING OF TERMINAL I/O FOR
CCCCC                TCL/TK SCRIPTS TO WORK.  THE ENVIRONMENT VARIABLE
CCCCC                   DATAPLOT_GUI_IO <PIPE/FILE>
CCCCC                SPECIFIES WHETHER OR NOT TO DO THIS SPECIAL CODE.
C
      ELSE IF(
     1       (IHOST1.EQ.'SUN') .OR.
     1       (IHOST1.EQ.'CRAY' .AND. IOPSY1.EQ.'UNIX') .OR.
     1       (IHOST1.EQ.'CONV') .OR.
     1       (IHOST1.EQ.'SGI ') .OR.
     1       (IHOST1.EQ.'HP-9') .OR.
     1       (IHOST1.EQ.'AIX ') .OR.
     1       (IHOST1.EQ.'LINU') .OR.
     1       (IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')
     1    )THEN
C
#ifdef LINUX
        CALL getenv('DATAPLOT_GUI_IO',IGUII2)
        IF(IGUII2.EQ.'FILE'.OR.IGUII2.EQ.'file')IGUIIO='FILE'
C
C       CHECK FOR LOCATION OF DATAPLOT AUXILIARY FILES.
C
C         1. FIRST CHECK FOR THE ENVIRONMENT VARIABLE
C
C               DATAPLOT_FILES
C
C         2. IF DATAPLOT_FILES IS NOT DEFINED, THEN
C            CHECK IF THE FOLLOWING COMPILE OPTION
C            USED (THE SPECIFIED PATH CAN VARY).
C
C               '-DDPLIBDIR='"/usr/local/lib/dataplot"'
C
C         3. IF NEITHER OF THESE ARE USED, THEN DEFAULT
C            TO "/usr/local/lib/dataplot".
C
C       STEP 1: CHECK FOR "DATAPLOT_FILES" ENVIRONMENT VARIABLE.
C
        UNIXPN=' '
        UNIXPV='DATAPLOT_FILES'
        CALL getenv(UNIXPV,UNIXPN)
C
#define STRZZZ(x)    x
#ifdef DPLIBDIR
        IF(UNIXPN.EQ.' ')THEN
          UNIXPN=STRZZZ(DPLIBDIR)
          NLAST=0
          DO111II=80,1,-1
            IF(UNIXPN(II:II).NE.' ')THEN
              NLAST=II
              GOTO119
            ENDIF
  111     CONTINUE
  119     CONTINUE
        ENDIF
#endif
C
C       IF NEITHER DATAPLOT_FILES OR prefix COMPILE SWITCH
C       GIVEN, THEN DEFAULT TO "/usr/local/lib/dataplot".
C
C
        IF(UNIXPN.EQ.' ')THEN
          IPATH1='/usr/local/lib/dataplot/'
          NCP1=24
          UNIXPN=' '
          UNIXPN(1:NCP1)=IPATH1(1:NCP1)
          IUNXNC=NCP1
          PATH=' '
          PATH(1:NCP1)=UNIXPN(1:NCP1)
          NCPATH=NCP1
        ELSE
          DO1001I=80,1,-1
            NCP1=I
            IF(UNIXPN(I:I).NE.' ')GOTO1009
 1001     CONTINUE
 1009     CONTINUE
          IPATH1(1:NCP1)=UNIXPN(1:NCP1)
          IF(IPATH1(NCP1:NCP1).NE.'/')THEN
            NCP1=NCP1+1
            IPATH1(NCP1:NCP1)='/'
          ENDIF
          IUNXNC=NCP1
          UNIXPN=' '
          UNIXPN(1:IUNXNC)=IPATH1(1:IUNXNC)
          PATH=' '
          PATH(1:NCP1)=UNIXPN(1:NCP1)
          NCPATH=NCP1
        ENDIF
C
CCCCC   AUGUST 1996.  TO MAKE SEARCH OF SUB-DIRECTORIES WORK, SET PATH
CCCCC                 TO BE EMPTY.
C
        IPATH1=' '
        NCP1=0
C
CCCCC   JUNE 1996.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF
CCCCC               "HOME" ENVIRONMENT VARIABLE.  READ DPLOGF FROM
CCCCC               USER'S HOME DIRECTORY IF FOUND.  OTHERWISE,
CCCCC               CURRENT DIRECTORY.
C
        UNIXPV='HOME'
        CALL getenv(UNIXPV,IPATH3)
        IF(IPATH3.NE.' ')THEN
          IFHOME='YES'
          DO1002I=80,1,-1
            NCP3=I
            IF(IPATH3(I:I).NE.' ')GOTO1003
 1002     CONTINUE
 1003     CONTINUE
          NCP3=NCP3+1
          IPATH3(NCP3:NCP3)='/'
        ENDIF
        UPROFI(1:NCP3)=IPATH3(1:NCP3)
        NCUPRO=NCP3
C
        CALL getenv('USER',USRNAM)
        IF(USRNAM.NE.' ')THEN
          DO7002I=20,1,-1
            NCUSER=I
            IF(USRNAM(I:I).NE.' ')GOTO7003
 7002     CONTINUE
 7003     CONTINUE
        ENDIF
C
        CALL getenv('SHELL',ISHELL)
        IF(ISHELL.NE.' ')THEN
          DO7007I=20,1,-1
            NCSHEL=I
            IF(ISHELL(I:I).NE.' ')GOTO7008
 7007     CONTINUE
 7008     CONTINUE
        ENDIF
C
        IF(ISHELL.EQ.'/bin/bash')THEN
          CALL getenv('HOSTNAME',COMNAM)
        ELSE
          CALL getenv('HOST',COMNAM)
        ENDIF
        IF(COMNAM.NE.' ')THEN
          DO7012I=80,1,-1
            NCCOMP=I
            IF(COMNAM(I:I).NE.' ')GOTO7013
 7012     CONTINUE
 7013     CONTINUE
        ENDIF
C
        CALL getenv('PRINTER',DEFPRI)
        IF(DEFPRI.NE.' ')THEN
          DO7022I=80,1,-1
            NCPRIN=I
            IF(DEFPRI(I:I).NE.' ')GOTO7023
 7022     CONTINUE
 7023     CONTINUE
        ENDIF
C
        IEDDIR=' '
        NCEDT1=0
        IF(NCP1.GE.1)THEN
          IEDDIR=IPATH1
          NCEDT1=NCP1
        ENDIF
        IPATH2=' '
        NCP2=0
        IEXT1='.tex'
        NCEXT1=4
        IEXT2='.dat'
        NCEXT2=4
        ICASFL='LOWE'
C
CCCCC   JULY 1996.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF
CCCCC               "DATAPLOT_WEB" ENVIRONMENT VARIABLE.  IF ON,
CCCCC               TRUE, YES, ASSUME RUNNING DATAPLOT FROM A WEB PAGE.
CCCCC               IF SO, CREATE LOCAL FILES (E.G, DPPL1F.DAT) IN
CCCCC               /tmp DIRECTORY RATHER THAN CURRENT DIRECTORY.
C
        UNIXPV='DATAPLOT_WEB'
        CALL getenv(UNIXPV,ITEMP)
        IF(ITEMP.NE.' ')THEN
          IWBFLG='YES'
          IF(ITEMP.EQ.'NO')IWBFLG='NO'
          IF(ITEMP.EQ.'no')IWBFLG='NO'
          IF(ITEMP.EQ.'OFF')IWBFLG='NO'
          IF(ITEMP.EQ.'off')IWBFLG='NO'
          IF(ITEMP.EQ.'FALS')IWBFLG='NO'
          IF(ITEMP.EQ.'fals')IWBFLG='NO'
        ENDIF
C
CCCCC APRIL 1997.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF:
CCCCC
CCCCC              1)  "BROWSER" ENVIRONMENT VARIABLE.  THIS ENVIRONMENT
CCCCC                  VARIABLE IS USED BY THE "WEB HELP" COMMAND TO
CCCCC                  SPECIFY WHAT BROWSER WILL BE USED TO EXAMINE THE
CCCCC                  DATAPLOT REFERENCE MANUAL.
CCCCC
CCCCC                  2019/03: USE "xdg-open" IF NO BROWSER SPECIFIED,
CCCCC
CCCCC              2)  "DATAPLOT_URL" ENVIRONMENT VARIABLE.  THIS
CCCCC                  ENVIRONMENT VARIABLE SPECIFIES THE LOCATION OF
CCCCC                  THE DATAPLOT REFERENCE MANUAL.  DEFUALTS TO THE
CCCCC                  NIST SITE.  INCLUDED TO ALLOW SITES TO INSTALL THE
CCCCC                  REFERENCE MANUAL LOCALLY.
CCCCC
CCCCC              3)  "URL" ENVIRONMENT VARIABLE.  THIS ENVIRONMENT
CCCCC                  VARIABLE SPECIFIES THE DEFAULT URL TO USE FOR THE
CCCCC                  WEB COMMAND.
C
        IBROWS=' '
        IDPURL=' '
        IURL=' '
        UNIXPV='BROWSER'
        CALL getenv(UNIXPV,IBROWS)
CCCCC   IF(IBROWS.EQ.' ')IBROWS='firefox'
        IF(IBROWS.EQ.' ')IBROWS='xdg-open'
C
        UNIXPV='DATAPLOT_URL'
        CALL getenv(UNIXPV,IDPURL)
        IF(IDPURL.EQ.' ')THEN
          IDPURL(1:24)='http://www.itl.nist.gov/'
          IDPURL(25:49)='div898/software/dataplot/'
        ENDIF
C
        UNIXPV='URL'
        CALL getenv(UNIXPV,IURL)
        IF(IURL.EQ.' ')IURL(1:20)='http://www.nist.gov/'
C
        IPATH2=' '
        NCP2=0
        IF(IWBFLG.EQ.'YES')THEN
          IPATH2='/tmp/'
          NCP2=5
        ENDIF
#endif
C
CCCCC THE FOLLOWING SECTION WAS ADDED SEPTEMBER 1990
CCCCC IT ASSUMES THE IMPLEMENTER HAS 2 DOS LINES EXISTING--
CCCCC SET DATAPLO$=the full pathname for the directory
CCCCC              where the implementer put DATAPLOT.EXE
CCCCC              (e.g., SET DATAPLO$=C:\DATAPLOT\)
CCCCC              (this SET command goes anywhere in AUTOEXEC.BAT)
CCCCC SHELL=COMMAND.COM /E:288 /P
CCCCC              (this SHELL command goes as the last line
CCCCC              in CONFIG.SYS)
CCCCC JUNE 1996.  DEPENDING ON WHETHER OTG OR LAHEY COMPILER IS USED.
CCCCC OUR VERSION OF LAHEY (5.11) DOESN'T SEEM TO HAVE VARIABLE
CCCCC READING FUNCTION, SO HARD-CODE TO C:\DATAPLOT.
CCCCC
CCCCC OCTOBER 1996.  UPDATE FOR MICROSOFT COMPILER ON PC.  USE
CCCCC LIBRARY FUNCTION SETENVQQ (WORKS A LOT LIKE UNIX SETENV).
CCCCC APRIL 1997.  FOR IBM/PC SYSTEMS, CHECK FOR EXISTENCE OF:
CCCCC 1)  "BROWSER" SET VARIABLE.  THIS VARIABLE
CCCCC     IS USED BY THE "WEB HELP" COMMAND TO SPECIFY WHAT BROWSER
CCCCC     WILL BE USED TO EXAMINE THE DATAPLOT REFERENCE MANUAL.
CCCCC     DEFAULTS TO NETSCAPE.
CCCCC 2)  "DP_URL" SET VARIABLE.  THIS
CCCCC     VARIABLE SPECIFIES THE LOCATION OF THE DATAPLOT REFERENCE
CCCCC     MANUAL.  DEFUALTS TO THE NIST SITE.  INCLUDED TO ALLOW
CCCCC     SITES TO INSTALL THE REFERENCE MANUAL LOCALLY.
C
CCCCC 12/2002: CHANGE DEFAULT TO "C:\PROGRAM FILES\NIST\DATAPLOT\"
C
C     NEED TO UPDATE LAHEY AND ABSOFT TO RETRIEVE ENVIRONMENT VARIABLES
C
      ELSE IF(IHOST1.EQ.'IBM-')THEN
        IF(ICOMPI.EQ.'LAHE')THEN
#ifdef WIN32_LAHEY
CCCCC     IPATH1='C:\DATAPLOT\'
CCCCC     NCP1=12
          IPATH1='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCP1=31
CCCCC     ICDIR='C:\FED\'
CCCCC     NCEDT1=7
CCCCC     ICDIR='C:\DATAPLOT\'
CCCCC     NCEDT1=12
          ICDIR='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCEDT1=31
#endif
        ELSEIF(ICOMPI.EQ.'ABSO')THEN
#ifdef WIN32_ABSOFT
CCCCC     IPATH1='C:\DATAPLOT\'
CCCCC     NCP1=12
          IPATH1='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCP1=31
CCCCC     ICDIR='C:\FED\'
CCCCC     NCEDT1=7
CCCCC     ICDIR='C:\DATAPLOT\'
CCCCC     NCEDT1=12
          ICDIR='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCEDT1=31
#endif
        ELSE IF(ICOMPI.EQ.'MS-F')THEN
#ifdef INTEL
          IRESLT=GETENVQQ('USERNAME',USRNAM)
          IF(IRESLT.GT.0)THEN
            DO7111II=20,1,-1
              IF(USRNAM(II:II).NE.' ')THEN
                NCUSER=II
                GOTO7119
              ENDIF
 7111       CONTINUE
 7119       CONTINUE
          ENDIF
C
          IRESLT=GETENVQQ('COMPUTERNAME',COMNAM)
          IF(IRESLT.GT.0)THEN
            DO7121II=80,1,-1
              IF(COMNAM(II:II).NE.' ')THEN
                NCCOMP=II
                GOTO7129
              ENDIF
 7121       CONTINUE
 7129       CONTINUE
          ENDIF
C
          IRESLT=GETENVQQ('LOCALAPPDATA',APPDAT)
          IF(IRESLT.GT.0)THEN
            DO7131II=80,1,-1
              IF(APPDAT(II:II).NE.' ')THEN
                NCAPPD=II
                GOTO7139
              ENDIF
 7131       CONTINUE
 7139       CONTINUE
          ENDIF
C
          IRESLT=GETENVQQ('USERPROFILE',UPROFI)
          IF(IRESLT.GT.0)THEN
            DO7141II=80,1,-1
              IF(UPROFI(II:II).NE.' ')THEN
                NCUPRO=II
                GOTO7149
              ENDIF
 7141       CONTINUE
 7149       CONTINUE
          ENDIF
C
          WINBIT='64'
          CTEMP=' '
          IRESLT=GETENVQQ('PROCESSOR_ARCHITECTURE',CTEMP)
          IF(IRESLT.GT.0)THEN
            DO7101II=1,80
              IF(CTEMP(II:II+1).EQ.'32')THEN
                WINBIT='32'
              ENDIF
 7101       CONTINUE
 7109       CONTINUE
          ENDIF
C
          IRESLT=GETENVQQ('PROGRAMFILES',PROFIL)
          IF(IRESLT.GT.0)THEN
            DO7151II=80,1,-1
              IF(PROFIL(II:II).NE.' ')THEN
                NCPROF=II
                GOTO7159
              ENDIF
 7151       CONTINUE
 7159       CONTINUE
            IF(WINBIT.EQ.'64')THEN
              DO7153II=1,NCPROF-5
                IF(PROFIL(II:II+5).EQ.' (x86)')THEN
                  NCPROF=II-1
                  PROFIL(II:80)=' '
                  GOTO7155
                ELSEIF(PROFIL(II:II+5).EQ.' (X86)')THEN
                  NCPROF=II-1
                  PROFIL(II:80)=' '
                  GOTO7155
                ENDIF
 7153         CONTINUE
 7155         CONTINUE
            ENDIF
          ENDIF
C
          IF(WINBIT.EQ.'64')THEN
            IRESLT=GETENVQQ('PROGRAMFILES(X86)',P86FIL)
            IF(IRESLT.GT.0)THEN
              DO7161II=80,1,-1
                IF(P86FIL(II:II).NE.' ')THEN
                  NCP86F=II
                  GOTO7169
                ENDIF
 7161         CONTINUE
 7169         CONTINUE
            ENDIF
          ENDIF
C
          IRESLT=GETENVQQ('DATAPLO$',ICDIR)
          IF(IRESLT.LE.0)THEN
CCCCC       ICDIR='C:\DATAPLOT\'
            IF(WINBIT.EQ.'32')THEN
              ICDIR(1:NCPROF)=PROFIL(1:NCPROF)
              NCP1=NCPROF
              NCP1=NCP1+1
              ICDIR(NCP1:NCP1)=IBASLC
              ICDIR(NCP1+1:NCP1+4)='NIST'
              NCP1=NCP1+4
              NCP1=NCP1+1
              ICDIR(NCP1:NCP1)=IBASLC
              ICDIR(NCP1+1:NCP1+8)='DATAPLOT'
              NCP1=NCP1+8
              NCP1=NCP1+1
              ICDIR(NCP1:NCP1)=IBASLC
              NCDIR=NCP1
            ELSE
              ICDIR(1:NCP86F)=P86FIL(1:NCP86F)
              NCP1=NCP86F
              NCP1=NCP1+1
              ICDIR(NCP1:NCP1)=IBASLC
              ICDIR(NCP1+1:NCP1+4)='NIST'
              NCP1=NCP1+4
              NCP1=NCP1+1
              ICDIR(NCP1:NCP1)=IBASLC
              ICDIR(NCP1+1:NCP1+8)='DATAPLOT'
              NCP1=NCP1+8
              NCP1=NCP1+1
              ICDIR(NCP1:NCP1)=IBASLC
              NCDIR=NCP1
            ENDIF
          ELSE
            NCDIR=80
            NMAX=80
            CALL DPDB80(ICDIR,NCDIR,NMAX,IBUGIN,ISUBRO,IERROR)
            NCP1=NCDIR
          ENDIF
          IF(NCDIR.LE.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1111)
 1111       FORMAT('***** ERROR IN INITFO--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1112)
 1112       FORMAT('      ERROR IN DEFINING THE DATAPLOT DIRECTORY')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1121)ICDIR(1:80)
 1121       FORMAT('ICDIR(1:40) = ',A80)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1122)NCDIR
 1122       FORMAT('NCDIR = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          IPATH1(1:NCP1)=ICDIR(1:NCP1)
C
          IRESLT=GETENVQQ('FED$',IEDDIR)
          IF(IRESLT.EQ.0)THEN
CCCCC       ICDIR='C:\FED\'
CCCCC       NCEDT1=7
CCCCC       NCDIR=7
            IF(WINBIT.EQ.'32')THEN
              ICDIR(1:NCPROF)=PROFIL(1:NCPROF)
              NCEDT1=NCPROF
              NCEDT1=NCEDT1+1
              ICDIR(NCEDT1:NCEDT1)=IBASLC
              ICDIR(NCEDT1+1:NCEDT1+4)='NIST'
              NCEDT1=NCEDT1+4
              NCEDT1=NCEDT1+1
              ICDIR(NCEDT1:NCEDT1)=IBASLC
              ICDIR(NCEDT1+1:NCEDT1+8)='DATAPLOT'
              NCEDT1=NCEDT1+8
              NCEDT1=NCEDT1+1
              ICDIR(NCEDT1:NCEDT1)=IBASLC
              NCDIR=NCEDT1
            ELSE
              ICDIR(1:NCP86F)=P86FIL(1:NCP86F)
              NCEDT1=NCP86F
              NCEDT1=NCEDT1+1
              ICDIR(NCEDT1:NCEDT1)=IBASLC
              ICDIR(NCEDT1+1:NCEDT1+4)='NIST'
              NCEDT1=NCEDT1+4
              NCEDT1=NCEDT1+1
              ICDIR(NCEDT1:NCEDT1)=IBASLC
              ICDIR(NCEDT1+1:NCEDT1+8)='DATAPLOT'
              NCEDT1=NCEDT1+8
              NCEDT1=NCEDT1+1
              ICDIR(NCEDT1:NCEDT1)=IBASLC
              NCDIR=NCEDT1
            ENDIF
            NCDIR=NCEDT1
            IEDDIR(1:NCDIR)=ICDIR(1:NCDIR)
          ELSE
            NCDIR=80
            NMAX=80
            CALL DPDB80(IEDDIR,NCDIR,NMAX,IBUGIN,ISUBRO,IERROR)
            NCEDT1=NCDIR
          ENDIF
C
          IF(NCDIR.LE.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1212)
 1212       FORMAT('      ERROR IN DEFINING THE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1213)
 1213       FORMAT('      FED (= THE DATAPLOT EDITOR) DIRECTORY')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1221)ICDIR(1:80)
 1221       FORMAT('ICDIR(1:40) = ',A80)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1122)NCDIR
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
C         NEED TO CHANGE THE DEFAULT BROWSER
C
          IBROWS=' '
          IDPURL=' '
          IRESLT=GETENVQQ('BROWSER',IBROWS)
          IF(IRESLT.EQ.0)THEN
            IBROWS(1:42)='"C:\Program Files (x86)\Internet Explorer\'
            IBROWS(43:55)='iexplore.exe"'
          ENDIF
C
          IRESLT=GETENVQQ('DP_URL',IDPURL)
          IF(IRESLT.EQ.0)THEN
            IDPURL(1:24)='http://www.itl.nist.gov/'
            IDPURL(25:49)='div898/software/dataplot/'
          ENDIF
C
          IRESLT=GETENVQQ('URL',IDPURL)
          IF(IRESLT.EQ.0)THEN
            IURL(1:20)='http://www.nist.gov/'
          ENDIF
C
#endif
        ELSE IF(ICOMPI.EQ.'OTG ')THEN
#ifdef WIN32_OTG
          CALL DOSPARAM@('DATAPLO$',ICDIR)
          NCDIR=80
          NMAX=80
          CALL DPDB80(ICDIR,NCDIR,NMAX,IBUGIN,ISUBRO,IERROR)
          NCP1=NCDIR
C
CCCCC THE FOLLOWING ERROR CHECK & WRITE WAS ENTERED    JANUARY 1994
          IF(NCDIR.LE.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1111)
 1111       FORMAT('***** ERROR IN INITFO--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1112)
 1112       FORMAT('      ERROR IN DEFINING THE DATAPLOT DIRECTORY')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1114)
 1114       FORMAT('      PROBABLE CAUSE--BAD  AUTOEXEC.BAT  FILE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1115)
 1115       FORMAT('      MISSING OR INCORRECT    SET   STATEMENT:')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1116)
 1116       FORMAT('      SET DATAPLO$ = etc.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1121)ICDIR(1:40)
 1121       FORMAT('ICDIR(1:40) = ',A40)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1122)NCDIR
 1122       FORMAT('NCDIR = ',I8)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          IPATH1(1:NCP1)=ICDIR(1:NCP1)
C
          CALL DOSPARAM@('FED$',ICDIR)
          NCDIR=80
          NMAX=80
          CALL DPDB80(ICDIR,NCDIR,NMAX,IBUGIN,ISUBRO,IERROR)
          NCEDT1=NCDIR
C
CCCCC THE FOLLOWING ERROR CHECK & WRITE WAS ENTERED      JANUARY 1994
          IF(NCDIR.LE.0)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1111)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1212)
 1212       FORMAT('      ERROR IN DEFINING THE')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1213)
 1213       FORMAT('      FED (= THE DATAPLOT EDITOR) DIRECTORY')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1114)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1115)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1216)
 1216       FORMAT('      SET FED$ = etc.')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1121)ICDIR(1:40)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1122)NCDIR
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
#endif
        ELSE
CCCCC     ICDIR='C:\FED\'
CCCCC     NCEDT1=7
          ICDIR='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCEDT1=31
CCCCC     IPATH1='C:\DATAPLOT\'
CCCCC     NCP1=12
          IPATH1='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCP1=31
        ENDIF
C
        IEDDIR(1:NCEDT1)=ICDIR(1:NCEDT1)
CCCCC APRIL 1996.  SET PATH, NCPATH
        PATH(1:NCP1)=IPATH1(1:NCP1)
        NCPATH=NCP1
        IPATH1=' '
        NCP1=0
        IPATH2=' '
        NCP2=0
        IEXT1='.TEX'
        NCEXT1=4
        IEXT2='.DAT'
        NCEXT2=4
        ICASFL='UPPE'
C
CXXXX ELSE IF(IHOST1.EQ.'XXXX')THEN
CXXXX   INSERT CODE FOR UNSUPPORTED HOST HERE!!!
CXXXX   IPATH1=' '
CXXXX   NCP1=0
CXXXX   IPATH2=' '
CXXXX   NCP2=0
CXXXX   IEXT1='.TEX'
CXXXX   NCEXT1=4
CXXXX   IEXT2='.DAT'
CXXXX   NCEXT2=4
CXXXX   ICASFL='LOWE'
C
      ELSE
        IPATH1=' '
        NCP1=0
        IEDDIR=IPATH1
        NCEDT1=NCP1
        IPATH2=' '
        NCP2=0
        IEXT1=' '
        NCEXT1=0
        IEXT2=' '
        NCEXT2=0
        ICASFL='UPPE'
      END IF
C  END USER CHANGES!!!!
C
C --------------------
C
CCCCC AUGUST 1992.  DEFINE DIRECTORY AND EXTENSION FOR EDIT COMMAND
      IEDEXT=IEXT1
      IEDCAS=ICASFL
      NCEDT2=NCEXT1
C
      IMESNU=21
C  NOVEMBER 1991.
      INAME='DPMESF'
      IF(ICASFL.EQ.'LOWE')INAME='dpmesf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IMESNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IMESST='OLD'
      IMESFO='FORMATTED'
      IMESAC='SEQUENTIAL'
      IMESPR='READONLY'
      IMESCS='CLOSED'
C
      INEWNU=22
C  NOVEMBER 1991.
      INAME='DPNEWF'
      IF(ICASFL.EQ.'LOWE')INAME='dpnewf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,INEWNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      INEWST='OLD'
      INEWFO='FORMATTED'
      INEWAC='SEQUENTIAL'
      INEWPR='READONLY'
      INEWCS='CLOSED'
C
CCCCC IMAINU=23
C  NOVEMBER 1991.
CCCCC INAME='DPMAIF'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpmaif'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IMAINA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IMAIST='OLD'
CCCCC IMAIFO='FORMATTED'
CCCCC IMAIAC='SEQUENTIAL'
CCCCC IMAIPR='READONLY'
CCCCC IMAICS='CLOSED'
C
CCCCC IHELNU=24
      IHELNU=23
C  NOVEMBER 1991.
      INAME='DPHELF'
      IF(ICASFL.EQ.'LOWE')INAME='dphelf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHELNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHELST='OLD'
      IHELFO='FORMATTED'
      IHELAC='SEQUENTIAL'
      IHELPR='READONLY'
      IHELCS='CLOSED'
C
CCCCC IBUGNU=25
      IBUGNU=24
C  NOVEMBER 1991.
      INAME='DPBUGF'
      IF(ICASFL.EQ.'LOWE')INAME='dpbugf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IBUGNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IBUGST='OLD'
      IBUGFO='FORMATTED'
      IBUGAC='SEQUENTIAL'
      IBUGPR='READONLY'
      IBUGCS='CLOSED'
C
CCCCC IQUENU=26
C  NOVEMBER 1991.
CCCCC INAME='DPQUEF'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpquef'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IQUENA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IQUEST='OLD'
CCCCC IQUEFO='FORMATTED'
CCCCC IQUEAC='SEQUENTIAL'
CCCCC IQUEPR='READWRITE'
CCCCC IQUECS='CLOSED'
C
CCCCC ISYSNU=27
      ISYSNU=25
C  NOVEMBER 1991.
      INAME='DPSYSF'
      IF(ICASFL.EQ.'LOWE')INAME='dpsysf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,ISYSNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC ISYSST='NONE'
      ISYSST='OLD'
      ISYSFO='FORMATTED'
      ISYSAC='SEQUENTIAL'
CCCCC ISYSPR='READWRITE'
      ISYSPR='READONLY'
      ISYSCS='CLOSED'
C
CCCCC ILOGNU=28
      ILOGNU=26
C  NOVEMBER 1991.
      INAME='DPLOGF'
      IF(ICASFL.EQ.'LOWE')INAME='dplogf'
      NC=6
CCCCC JUNE 1996.  FOR UNIX, PATH DEPENDS ON "HOME" ENVIRONMENT VARIABLE
      IF(IFHOME.EQ.'YES')THEN
        CALL INITF2(INAME,NC,IPATH3,NCP3,IEXT1,NCEXT1,ILOGNA,IBUGIN)
      ELSE
        CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ILOGNA,IBUGIN)
      ENDIF
      IF(IHOST1.EQ.'NVE')ILOGNA='DPLOGF'
C  END OF NOVEMBER 1991 CHANGE
CCCCC ILOGST='NONE'
      ILOGST='OLD'
      ILOGFO='FORMATTED'
      ILOGAC='SEQUENTIAL'
CCCCC ILOGPR='READWRITE'
      ILOGPR='READONLY'
      ILOGCS='CLOSED'
C
CCCCC IDIRNU=29
      IDIRNU=27
C  NOVEMBER 1991.
      INAME='DPDIRF'
      IF(ICASFL.EQ.'LOWE')INAME='dpdirf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IDIRNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IDIRST='OLD'
      IDIRFO='FORMATTED'
      IDIRAC='SEQUENTIAL'
      IDIRPR='READONLY'
      IDIRCS='CLOSED'
C
CCCCC IDICNU=30
      IDICNU=28
C  NOVEMBER 1991.
      INAME='DPDICF'
      IF(ICASFL.EQ.'LOWE')INAME='dpdicf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IDICNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IDICST='OLD'
      IDICFO='FORMATTED'
      IDICAC='SEQUENTIAL'
      IDICPR='READONLY'
      IDICCS='CLOSED'
C
C --------------------
C
CCCCC IREANU=31
      IREANU=29
      IREANA='-999'
CCCCC IREAST='UNKNOWN'
      IREAST='OLD'
      IREAFO='FORMATTED'
      IREAAC='SEQUENTIAL'
CCCCC IREAPR='READWRITE'
      IREAPR='READONLY'
      IREACS='CLOSED'
C
      IREAN1=11
      IRENA1='-999'
      IREAS1='OLD'
      IREAF1='FORMATTED'
      IREAA1='SEQUENTIAL'
      IREAP1='READONLY'
      IREAC1='CLOSED'
C
      IREAN2=12
      IRENA2='-999'
      IREAS2='OLD'
      IREAF2='FORMATTED'
      IREAA2='SEQUENTIAL'
      IREAP2='READONLY'
      IREAC2='CLOSED'
C
      IREAN3=13
      IRENA3='-999'
      IREAS3='OLD'
      IREAF3='FORMATTED'
      IREAA3='SEQUENTIAL'
      IREAP3='READONLY'
      IREAC3='CLOSED'
C
CCCCC IWRINU=32
      IWRINU=30
      IWRINA='-999'
      IWRIST='UNKNOWN'
      IWRIFO='FORMATTED'
      IWRIAC='SEQUENTIAL'
      IWRIPR='READWRITE'
      IWRICS='CLOSED'
C
      IWRIN1=16
      IWRNA1='-999'
      IWRIS1='UNKNOWN'
      IWRIF1='FORMATTED'
      IWRIA1='SEQUENTIAL'
      IWRIP1='READWRITE'
      IWRIC1='CLOSED'
C
      IWRIN2=17
      IWRNA2='-999'
      IWRIS2='UNKNOWN'
      IWRIF2='FORMATTED'
      IWRIA2='SEQUENTIAL'
      IWRIP2='READWRITE'
      IWRIC2='CLOSED'
C
      IWRIN3=18
      IWRNA3='-999'
      IWRIS3='UNKNOWN'
      IWRIF3='FORMATTED'
      IWRIA3='SEQUENTIAL'
      IWRIP3='READWRITE'
      IWRIC3='CLOSED'
C
CCCCC ISAVNU=33
      ISAVNU=31
C  NOVEMBER 1991.
      INAME='DPSAVF'
      IF(ICASFL.EQ.'LOWE')INAME='dpsavf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ISAVNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      ISAVST='UNKNOWN'
      ISAVFO='UNFORMATTED'
      ISAVAC='SEQUENTIAL'
      ISAVPR='READWRITE'
      ISAVCS='CLOSED'
C
CCCCC ILISNU=34
      ILISNU=32
      ILISNA='-999'
CCCCC ILISST='UNKNOWN'
      ILISST='OLD'
      ILISFO='FORMATTED'
      ILISAC='SEQUENTIAL'
CCCCC ILISPR='READWRITE'
      ILISPR='READONLY'
      ILISCS='CLOSED'
C
CCCCC FIX BUG, HAVE CREATE COMMAND USE DIFFERNT UNIT NUMBER THAN
CCCCC MACRO.  THIS AVOIDS HANG WHEN "CREATE FILE." ENCOUNTERS A
CCCCC A CALL COMMAND.
CCCCC ICRENU=35
      ICRENU=50
      ICREN2=98
      ICRENA='-999'
      ICREST='UNKNOWN'
      ICREFO='FORMATTED'
      ICREAC='SEQUENTIAL'
      ICREPR='READWRITE'
      ICRECS='CLOSED'
C
C     ICAPNU=36
C  DECEMBER, 1989.  UNIT CONFLICT IF HAVE NESTED CALLS.
C  THIS IS AN UNRESOLVED BUG.
      ICAPNU=40
      ICAPNA='-999'
      ICAPST='UNKNOWN'
      ICAPFO='FORMATTED'
      ICAPAC='SEQUENTIAL'
      ICAPPR='READWRITE'
      ICAPCS='CLOSED'
C
      ICPNU2=49
      ICAPN2='-999'
      ICAPS2='UNKNOWN'
      ICAPF2='FORMATTED'
      ICAPA2='SEQUENTIAL'
      ICAPP2='READWRITE'
      ICAPC2='CLOSED'
C
C --------------------
C
      ISCRNU=41
C  NOVEMBER 1991.
      INAME='DPSCRF'
      IF(ICASFL.EQ.'LOWE')INAME='dpscrf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ISCRNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      ISCRST='UNKNOWN'
      ISCRFO='UNFORMATTED'
      ISCRAC='SEQUENTIAL'
      ISCRPR='READWRITE'
      ISCRCS='CLOSED'
C
      IDATNU=42
C  NOVEMBER 1991.
      INAME='DPDATF'
      IF(ICASFL.EQ.'LOWE')INAME='dpdatf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,IDATNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IDATST='NONE'
      IDATST='UNKNOWM'
      IDATFO='UNFORMATTED'
      IDATAC='SEQUENTIAL'
      IDATPR='READWRITE'
      IDATCS='CLOSED'
C
      IPL1NU=43
CCCCC IPL1ST='UNKNOWN'
C  NOVEMBER 1991.
      INAME='DPPL1F'
      IF(ICASFL.EQ.'LOWE')INAME='dppl1f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IPL1NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IPL1ST='NEW'
      IF(IHOST1.EQ.'HONE')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'CONV')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'CRAY')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IPL1ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')IPL1ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1991
      IF(IOPSY1.EQ.'UNIX')IPL1ST='UNKNOWN'
      IPL1FO='FORMATTED'
      IPL1AC='SEQUENTIAL'
      IPL1PR='READWRITE'
      IPL1CS='CLOSED'
C
      IPL2NU=44
C  NOVEMBER 1991.
      INAME='DPPL2F'
      IF(ICASFL.EQ.'LOWE')INAME='dppl2f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IPL2NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IPL2ST='UNKNOWN'
      IPL2ST='NEW'
      IF(IHOST1.EQ.'HONE')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'CONV')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'CRAY')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IPL2ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')IPL2ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1991
      IF(IOPSY1.EQ.'UNIX')IPL2ST='UNKNOWN'
      IPL2FO='FORMATTED'
      IPL2AC='SEQUENTIAL'
      IPL2PR='READWRITE'
      IPL2CS='CLOSED'
C
      IPRONU=45
C  NOVEMBER 1991.
      INAME='DPPROF'
      IF(ICASFL.EQ.'LOWE')INAME='dpprof'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,IPRONA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IPROST='UNKNOWN'
      IPROFO='FORMATTED'
      IPROAC='SEQUENTIAL'
      IPROPR='READWRITE'
      IPROCS='CLOSED'
C
      ICONNU=46
CCCCC ICONST='UNKNOWN'
C  NOVEMBER 1991.
      INAME='DPCONF'
      IF(ICASFL.EQ.'LOWE')INAME='dpconf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ICONNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      ICONST='NEW'
      IF(IHOST1.EQ.'HONE')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'205')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')ICONST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')ICONST='UNKNOWN'
CCCCC THE FOLLOWING LINE W AS ADDED NOVEMBER 1991
      IF(IOPSY1.EQ.'UNIX')ICONST='UNKNOWN'
      ICONFO='FORMATTED'
      ICONAC='SEQUENTIAL'
      ICONPR='READWRITE'
      ICONCS='CLOSED'
C
      ISACNU=47
C  NOVEMBER 1991.
      INAME='DPSACF'
      IF(ICASFL.EQ.'LOWE')INAME='dpsacf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ISACNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC ISACST='UNKNOWN'
      ISACST='NEW'
      IF(IHOST1.EQ.'HONE')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'205')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')ISACST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')ISACST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1990.
      IF(IOPSY1.EQ.'UNIX')ISACST='UNKNOWN'
      ISACFO='FORMATTED'
      ISACAC='SEQUENTIAL'
      ISACPR='READWRITE'
      ISACCS='CLOSED'
C
CCCCC THE FOLLOWING SECTION WAS ADDED      MARCH 1992
CCCCC TO DEFINE THE GENERAL OUTPUT FILE     MARCH 1992
      IOUTNU=49
C  NOVEMBER 1991.
      INAME='DPOUTF'
      IF(ICASFL.EQ.'LOWE')INAME='dpoutf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,IOUTNA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IOUTST='UNKNOWN'
      IOUTST='NEW'
      IF(IHOST1.EQ.'HONE')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'205')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IOUTST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')IOUTST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1990.
      IF(IOPSY1.EQ.'UNIX')IOUTST='UNKNOWN'
      IOUTFO='FORMATTED'
      IOUTAC='SEQUENTIAL'
      IOUTPR='READWRITE'
      IOUTCS='CLOSED'
C
C --------------------
C
CCCCC IEX1NU=51
C  NOVEMBER 1991.
CCCCC INAME='DPEX1F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpex1f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX1NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IEX1ST='OLD'
CCCCC IEX1FO='FORMATTED'
CCCCC IEX1AC='SEQUENTIAL'
CCCCC IEX1PR='READONLY'
CCCCC IEX1CS='CLOSED'
C
CCCCC IEX2NU=52
C  NOVEMBER 1991.
CCCCC INAME='DPEX2F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpex2f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX2NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IEX2ST='OLD'
CCCCC IEX2FO='FORMATTED'
CCCCC IEX2AC='SEQUENTIAL'
CCCCC IEX2PR='READONLY'
CCCCC IEX2CS='CLOSED'
C
CCCCC IEX3NU=53
CCCCC IEX3ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPEX3F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpex3f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX3NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IEX3FO='FORMATTED'
CCCCC IEX3AC='SEQUENTIAL'
CCCCC IEX3PR='READONLY'
CCCCC IEX3CS='CLOSED'
C
CCCCC IEX4NU=54
CCCCC IEX4ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPEX4F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpex4f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX4NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IEX4FO='FORMATTED'
CCCCC IEX4AC='SEQUENTIAL'
CCCCC IEX4PR='READONLY'
CCCCC IEX4CS='CLOSED'
C
CCCCC IEX5NU=55
CCCCC IEX5ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPEX5F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpex5f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX5NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IEX5FO='FORMATTED'
CCCCC IEX5AC='SEQUENTIAL'
CCCCC IEX5PR='READONLY'
CCCCC IEX5CS='CLOSED'
C
      IHHBNU=33
      IHHBST='OLD'
      INAME='HANDBK'
      IF(ICASFL.EQ.'LOWE')INAME='handbk'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHHBNA,IBUGIN)
      IHHBFO='FORMATTED'
      IHHBAC='SEQUENTIAL'
      IHHBPR='READONLY'
      IHHBCS='CLOSED'
C
      IHRMNU=34
      IHRMST='OLD'
      INAME='REFMAN'
      IF(ICASFL.EQ.'LOWE')INAME='refman'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHRMNA,IBUGIN)
      IHRMFO='FORMATTED'
      IHRMAC='SEQUENTIAL'
      IHRMPR='READONLY'
      IHRMCS='CLOSED'
C
      IHE1NU=61
      IHE1ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE1F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe1f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE1NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE1FO='FORMATTED'
      IHE1AC='SEQUENTIAL'
      IHE1PR='READONLY'
      IHE1CS='CLOSED'
C
      IHE2NU=62
      IHE2ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE2F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe2f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE2NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE2FO='FORMATTED'
      IHE2AC='SEQUENTIAL'
      IHE2PR='READONLY'
      IHE2CS='CLOSED'
C
      IHE3NU=63
      IHE3ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE3F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe3f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE3NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE3FO='FORMATTED'
      IHE3AC='SEQUENTIAL'
      IHE3PR='READONLY'
      IHE3CS='CLOSED'
C
      IHE4NU=64
      IHE4ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE4F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe4f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE4NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE4FO='FORMATTED'
      IHE4AC='SEQUENTIAL'
      IHE4PR='READONLY'
      IHE4CS='CLOSED'
C
      IHE5NU=65
      IHE5ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE5F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe5f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE5NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE5FO='FORMATTED'
      IHE5AC='SEQUENTIAL'
      IHE5PR='READONLY'
      IHE5CS='CLOSED'
C
      IHE6NU=66
      IHE6ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE6F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe6f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE6NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE6FO='FORMATTED'
      IHE6AC='SEQUENTIAL'
      IHE6PR='READONLY'
      IHE6CS='CLOSED'
C
      IHE7NU=67
      IHE7ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE7F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe7f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE7NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE7FO='FORMATTED'
      IHE7AC='SEQUENTIAL'
      IHE7PR='READONLY'
      IHE7CS='CLOSED'
C
      IHE8NU=68
      IHE8ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE8F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe8f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE8NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE8FO='FORMATTED'
      IHE8AC='SEQUENTIAL'
      IHE8PR='READONLY'
      IHE8CS='CLOSED'
C
      IHE9NU=69
      IHE9ST='OLD'
C  NOVEMBER 1991.
      INAME='DPHE9F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe9f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE9NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
      IHE9FO='FORMATTED'
      IHE9AC='SEQUENTIAL'
      IHE9PR='READONLY'
      IHE9CS='CLOSED'
C
CCCCC THE FOLLOWING 9 MENU SECTIONS WERE ADDED JUNE 1990
C
CCCCC IME1NU=71
CCCCC IME1ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME1F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme1f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME1NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME1FO='FORMATTED'
CCCCC IME1AC='SEQUENTIAL'
CCCCC IME1PR='READONLY'
CCCCC IME1CS='CLOSED'
C
CCCCC IME2NU=72
CCCCC IME2ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME2F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme2f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME2NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME2FO='FORMATTED'
CCCCC IME2AC='SEQUENTIAL'
CCCCC IME2PR='READONLY'
CCCCC IME2CS='CLOSED'
C
CCCCC IME3NU=73
CCCCC IME3ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME3F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme3f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME3NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME3FO='FORMATTED'
CCCCC IME3AC='SEQUENTIAL'
CCCCC IME3PR='READONLY'
CCCCC IME3CS='CLOSED'
C
CCCCC IME4NU=74
CCCCC IME4ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME4F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme4f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME4NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME4FO='FORMATTED'
CCCCC IME4AC='SEQUENTIAL'
CCCCC IME4PR='READONLY'
CCCCC IME4CS='CLOSED'
C
CCCCC IME5NU=75
CCCCC IME5ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME5F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme5f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME5NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME5FO='FORMATTED'
CCCCC IME5AC='SEQUENTIAL'
CCCCC IME5PR='READONLY'
CCCCC IME5CS='CLOSED'
C
CCCCC IME6NU=76
CCCCC IME6ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME6F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme6f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME6NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME6FO='FORMATTED'
CCCCC IME6AC='SEQUENTIAL'
CCCCC IME6PR='READONLY'
CCCCC IME6CS='CLOSED'
C
CCCCC IME7NU=77
CCCCC IME7ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME7F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme7f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME7NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME7FO='FORMATTED'
CCCCC IME7AC='SEQUENTIAL'
CCCCC IME7PR='READONLY'
CCCCC IME7CS='CLOSED'
C
CCCCC IME8NU=78
CCCCC IME8ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME8F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme8f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME8NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME8FO='FORMATTED'
CCCCC IME8AC='SEQUENTIAL'
CCCCC IME8PR='READONLY'
CCCCC IME8CS='CLOSED'
C
CCCCC IME9NU=79
CCCCC IME9ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPME9F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpme9f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME9NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IME9FO='FORMATTED'
CCCCC IME9AC='SEQUENTIAL'
CCCCC IME9PR='READONLY'
CCCCC IME9CS='CLOSED'
C
CCCCC THE FOLLOWING 11 SECTIONS (10 TO 20) WERE ADDED AUGUST 1990
CCCCC IM10NU=80
CCCCC IM10ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM10F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm10f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM10NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM10FO='FORMATTED'
CCCCC IM10AC='SEQUENTIAL'
CCCCC IM10PR='READONLY'
CCCCC IM10CS='CLOSED'
C
CCCCC IM11NU=81
CCCCC IM11ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM11F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm11f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM11NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM11FO='FORMATTED'
CCCCC IM11AC='SEQUENTIAL'
CCCCC IM11PR='READONLY'
CCCCC IM11CS='CLOSED'
C
CCCCC IM12NU=82
CCCCC IM12ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM12F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm12f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM12NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM12FO='FORMATTED'
CCCCC IM12AC='SEQUENTIAL'
CCCCC IM12PR='READONLY'
CCCCC IM12CS='CLOSED'
C
CCCCC IM13NU=83
CCCCC IM13ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM13F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm13f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM13NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM13FO='FORMATTED'
CCCCC IM13AC='SEQUENTIAL'
CCCCC IM13PR='READONLY'
CCCCC IM13CS='CLOSED'
C
CCCCC IM14NU=84
CCCCC IM14ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM14F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm14f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM14NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM14FO='FORMATTED'
CCCCC IM14AC='SEQUENTIAL'
CCCCC IM14PR='READONLY'
CCCCC IM14CS='CLOSED'
C
CCCCC IM15NU=85
CCCCC IM15ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM15F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm15f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM15NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM15FO='FORMATTED'
CCCCC IM15AC='SEQUENTIAL'
CCCCC IM15PR='READONLY'
CCCCC IM15CS='CLOSED'
C
CCCCC IM16NU=86
CCCCC IM16ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM16F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm16f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM16NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM16FO='FORMATTED'
CCCCC IM16AC='SEQUENTIAL'
CCCCC IM16PR='READONLY'
CCCCC IM16CS='CLOSED'
C
CCCCC IM17NU=87
CCCCC IM17ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM17F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm17f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM17NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM17FO='FORMATTED'
CCCCC IM17AC='SEQUENTIAL'
CCCCC IM17PR='READONLY'
CCCCC IM17CS='CLOSED'
C
CCCCC IM18NU=88
CCCCC IM18ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM18F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm18f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM18NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM18FO='FORMATTED'
CCCCC IM18AC='SEQUENTIAL'
CCCCC IM18PR='READONLY'
CCCCC IM18CS='CLOSED'
C
CCCCC IM19NU=89
CCCCC IM19ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM19F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm19f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM19NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM19FO='FORMATTED'
CCCCC IM19AC='SEQUENTIAL'
CCCCC IM19PR='READONLY'
CCCCC IM19CS='CLOSED'
C
CCCCC IM20NU=90
CCCCC IM20ST='OLD'
C  NOVEMBER 1991.
CCCCC INAME='DPM20F'
CCCCC IF(ICASFL.EQ.'LOWE')INAME='dpm20f'
CCCCC NC=6
CCCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM20NA,IBUGIN)
C  END OF NOVEMBER 1991 CHANGE
CCCCC IM20FO='FORMATTED'
CCCCC IM20AC='SEQUENTIAL'
CCCCC IM20PR='READONLY'
CCCCC IM20CS='CLOSED'
C
C --------------------
CCCCC THE FOLLOWING 3 SECTIONS WERE ADDED OCTOBER 1991
CCCCC MODIFIED MARCH 1992
CCCCC FOR STORAGE OF SELECTED OUTPUT FROM FIT, ANOVA, YATES, ETC.
C
      IST1NU=91
CCCCC IST1ST='UNKNOWN'
      IST1ST='NEW'
      IF(IHOST1.EQ.'HONE')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST1ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      IF(IOPSY1.EQ.'UNIX')IST1ST='UNKNOWN'
C  MARCH 1992.
      INAME='DPST1F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst1f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST1NA,IBUGIN)
C  END OF MARCH 1992 CHANGE
      IST1FO='FORMATTED'
      IST1AC='SEQUENTIAL'
CCCCC AUGUST 1992.  FILE PERMISSION SHOULD BE READ/WRITE
CCCCC IST1PR='READONLY'
      IST1PR='READWRITE'
      IST1CS='CLOSED'
C
      IST2NU=92
CCCCC IST2ST='UNKNOWN'
      IST2ST='NEW'
      IF(IHOST1.EQ.'HONE')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST2ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IST2ST='UNKNOWN'
C  MARCH 1992.
      INAME='DPST2F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst2f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST2NA,IBUGIN)
C  END OF MARCH 1992 CHANGE
      IST2FO='FORMATTED'
      IST2AC='SEQUENTIAL'
CCCCC AUGUST 1992.  FILE PERMISSION SHOULD BE READ/WRITE
CCCCC IST2PR='READONLY'
      IST2PR='READWRITE'
      IST2CS='CLOSED'
C
      IST3NU=93
CCCCC IST3ST='UNKNOWN'
      IST3ST='NEW'
      IF(IHOST1.EQ.'HONE')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST3ST='UNKNOWN'
CCCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IST3ST='UNKNOWN'
C  MARCH 1992.
      INAME='DPST3F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst3f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST3NA,IBUGIN)
C  END OF MARCH 1992 CHANGE
      IST3FO='FORMATTED'
      IST3AC='SEQUENTIAL'
CCCCC AUGUST 1992.  FILE PERMISSION SHOULD BE READ/WRITE
CCCCC IST3PR='READONLY'
      IST3PR='READWRITE'
      IST3CS='CLOSED'
C
C
      IST4NU=94
CCCCC IST4ST='UNKNOWN'
      IST4ST='NEW'
      IF(IHOST1.EQ.'HONE')IST4ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST4ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST4ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST4ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST4ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST4ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST4ST='UNKNOWN'
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IST4ST='UNKNOWN'
      INAME='DPST4F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst4f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST4NA,IBUGIN)
      IST4FO='FORMATTED'
      IST4AC='SEQUENTIAL'
      IST4PR='READWRITE'
      IST4CS='CLOSED'
C
      IST5NU=95
CCCCC IST5ST='UNKNOWN'
      IST5ST='NEW'
      IF(IHOST1.EQ.'HONE')IST5ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST5ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST5ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST5ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST5ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST5ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST5ST='UNKNOWN'
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IST5ST='UNKNOWN'
      INAME='DPST5F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst5f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST5NA,IBUGIN)
      IST5FO='FORMATTED'
      IST5AC='SEQUENTIAL'
      IST5PR='READWRITE'
      IST5CS='CLOSED'
C
      IZCHNU=97
CCCCC IZCHST='UNKNOWN'
      IZCHST='NEW'
      IF(IHOST1.EQ.'HONE')IZCHST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IZCHST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IZCHST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IZCHST='UNKNOWN'
      IF(IHOST1.EQ.'205')IZCHST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IZCHST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IZCHST='UNKNOWN'
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IZCHST='UNKNOWN'
      INAME='DPZCHF'
      IF(ICASFL.EQ.'LOWE')INAME='dpzchf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IZCHNA,IBUGIN)
      IZCHFO='FORMATTED'
      IZCHAC='SEQUENTIAL'
      IZCHPR='READWRITE'
      IZCHCS='CLOSED'
C
C
C --------------------
C
C     DEFINE THE CHARACTER WHICH
C     (IF FOUND IN A WORD)
C     SPECIFIES THAT WORD TO BE A FILE NAME
C     (AS OPPOSED TO A DATAPLOT
C     VARIABLE, PARAMETER, COMMAND, ETC.).
C     THE DEFAULT CHARACTER IS . (= PERIOD)
C
      IFCHAR='.'
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
#ifdef INTEL
 9000 CONTINUE
#endif
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF INITFO--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IMESNU,IMESST
 9021   FORMAT('IMESNU,IMESST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)INEWNU,INEWST
 9022   FORMAT('INEWNU,INEWST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)IMAINU,IMAIST
 9023   FORMAT('IMAINU,IMAIST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)IHELNU,IHELST
 9024   FORMAT('IHELNU,IHELST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9025)IBUGNU,IBUGST
 9025   FORMAT('IBUGNU,IBUGST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9026)IQUENU,IQUEST
 9026   FORMAT('IQUENU,IQUEST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9027)ISYSNU,ISYSST
 9027   FORMAT('ISYSNU,ISYSST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9028)ILOGNU,ILOGST
 9028   FORMAT('ILOGNU,ILOGST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9029)IDIRNU,IDIRST
 9029   FORMAT('IDIRNU,IDIRST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9030)IDICNU,IDICST
 9030   FORMAT('IDICNU,IDICST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9031)IREANU,IREAST
 9031   FORMAT('IREANU,IREAST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IWRINU,IWRIST
 9032   FORMAT('IWRINU,IWRIST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9033)ISAVNU,ISAVST
 9033   FORMAT('ISAVNU,ISAVST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9034)ICRENU,ICREST
 9034   FORMAT('ICRENU,ICREST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9035)ISCRNU,ISCRST
 9035   FORMAT('ISCRNU,ISCRST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9036)IDATNU,IDATST
 9036   FORMAT('IDATNU,IDATST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9037)IPL1NU,IPL1ST
 9037   FORMAT('IPL1NU,IPL1ST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9038)IPL2NU,IPL2ST
 9038   FORMAT('IPL2NU,IPL2ST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9039)IPRONU,IPROST
 9039   FORMAT('IPRONU,IPROST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9040)ICONNU,ICONST
 9040   FORMAT('ICONNU,ICONST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)IOUTNU,IOUTST
 9042   FORMAT('IOUTNU,IOUTST = ',I8,2X,A12)
        CALL DPWRST('XXX','BUG ')
CCCCC   AUGUST 1992.  ADD FOLLOWING LINES
        WRITE(ICOUT,9043)IEDDIR
 9043   FORMAT('IEDDIR=',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9044)NCEDT1,NCEDT2
 9044   FORMAT('NCEDT1,NCEDT2=',I4,1X,I4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9045)IEDEXT,IEDCAS
 9045   FORMAT('IEDEXT,IEDCAS = ',A4,1X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE INITF2(INAME,NC1,IPATH,NC2,IEXT,NC3,INAME2,IBUGIN)
C
C     PURPOSE--THIS IS SUBROUTING INITF2.  IT IS A UTILITY ROUTINE
C              FOR INITFO.  IT ADDS A FILE PATH AND EXTENSION TO
C              A FILE NAME.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERBURG, MD 20899
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/7
C     ORIGINAL VERSION--NOVEMBER  1991.
C     UPDATED         --APRIL     1992.   INPUT DEBUG STATMENTS (JJF)
C     UPDATED         --MAY       1992.   INITIALIZE INAME2
C     UPDATED         --JULY      1995.   IF PC--DO NOT ADD PATH
C     UPDATED         --APRIL     1996.   UNDO JULY 1995 CHANGE (FIX IN
C                                         DPOPFI FOR NON-C: DRIVE)
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGIN
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
C
C  NOVEMBER 1991.  FOLLOWING BLOCK ADDED
C
      CHARACTER (LEN=*) :: IPATH
      CHARACTER (LEN=*) :: INAME
      CHARACTER (LEN=*) :: INAME2
      CHARACTER (LEN=*) :: IEXT
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='INIT'
      ISUBN2='F2  '
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF INITF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)NC1,INAME(1:NC1)
   52   FORMAT('NC1,INAME = ',I3,1X,A)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)NC2,IPATH(1:MAX(1,NC2))
   53   FORMAT('NC2,IPATH = ',I3,1X,A)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)NC3,IEXT(1:NC3)
   54   FORMAT('NC3,IEXT = ',I3,1X,A)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 1---  **
C               **  ADD THE FILE*
C               **  PATH        *
C               *****************
C
CCCCC THE FOLLOWING LINE WAS ADDED   MAY 1992  (JJF)
      INAME2=' '
      NCSTR=0
      IF(NC2.GT.0)THEN
        INAME2(1:NC2)=IPATH(1:NC2)
        NCSTR=NC2
      END IF
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,61)NCSTR,INAME2(1:NC2),'$'
   61   FORMAT('AFTER PATH: NCSTR,INAME2(1:NC2) = ',I5,2X,A,A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************
C               **  STEP 2---  **
C               **  ADD THE FILE*
C               **  NAME        *
C               *****************
C
      IF(NC1.GT.0)THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NC1-1
        INAME2(NCSTR:NCSTR2)=INAME(1:NC1)
        NCSTR=NCSTR2
      END IF
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,71)NCSTR,INAME2(1:NCSTR),'$'
   71   FORMAT('AFTER FILE NAME: NCSTR,INAME2(1:NCSTR) = ',I5,2X,A,A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C               *****************
C               **  STEP 3---  **
C               **  ADD THE FILE*
C               **  EXTENSION   *
C               *****************
C
      IF(NC3.GT.0)THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NC3-1
C
        IF(IBUGIN.EQ.'ON')THEN
          WRITE(ICOUT,81)NCSTR,NCSTR2,NC3
   81     FORMAT('NCSTR,NCSTR2,NC3 = ',3I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,83)'$',IEXT(1:NC3),'$'
   83     FORMAT('IEXT(1:NC3) = ',A1,A,A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        INAME2(NCSTR:NCSTR2)=IEXT(1:NC3)
        NCSTR=NCSTR2
      END IF
 
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT.      **
C               *****************
C
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF INITF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)INAME2,NCSTR
 9024   FORMAT('INAME2,NCSTR = ',A,1X,I3)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,
     1                  IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--OPEN A FILE (BUT THERE MAY BE SOME SMALL DIFFERENCES
C              IN HOW THAT IS DONE FOR DIFFERENT COMPUTERS).
C     NOTE--A REMARK TO THE CDC, PERKIN-ELMER, HONEYWELL, ETC. IMPLEMENTORS--
C           YOUR SECTIONS BELOW MUST HAVE A FEW MORE LINES MANUALLY
C           INSERTED IF YOU WISH YOUR USERS TO HAVE THE ABILITY
C           OF ACCESSING DATAPLOT'S REFERENCE/DATA/MAP/FRACTAL/MACRO
C           FILES AUTOMATICALLY WITHOUT EXPLICITLY PREFIXING
C           THE FILE NAME WITH THE HOME DIRECTORY WHERE DATAPLOT RESIDES.
C           SEE FOR EXAMPLE THE GENERAL SECTION BELOW AND THE VAX SECTION
C           BELOW WHERE SUCH LOGIC HAS BEEN BUILT IN.
C           IF YOU OMIT THIS ADDITION, THEN NOTHING IS LOST PER SE
C           BUT THE USERS WILL HAVE TO SPELL OUT FULLY DATAPLOT'S
C           HOME DIRECTORY WHEN REFERENCING THESE ACCESSORY
C           REFERENCE/DATA/MAP/FRACTAL/MACRO/ETC. FILES.
C           E.G., LIST TEXAS.DAT              VERSUS
C                 LIST DATAPLO$:TEXAS.DAT
C
C     DANGER--THE INPUT ARGUMENT IFILE MAY UNDER CERTAIN
C             CIRCUMSTANCES BE CHANGED WITHIN THIS SUBROUTINE.
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--NOVEMBER  1985.
C     UPDATED         --SEPTEMBER 1986.
C     UPDATED         --OCTOBER   1987.  (FORM LOWER AND UPPER CASE NAMES)
C     UPDATED         --NOVEMBER  1987.  (CLOSE BEFORE OPEN FOR HONEYWELL)
C     UPDATED         --DECEMBER  1988.  (AUTO PREFIX OF DP'S HOME DIREC.)
C     UPDATED         --FEBRUARY  1989.  CYBER/CDC CASE (ALAN)
C     UPDATED         --FEBRUARY  1988.  CYBER/CDC DATAPLOT REF. FILES (ALAN)
C     UPDATED         --JULY      1989.  FIXED POSITION VALIUES FOR IFILE2(.:.)
C     UPDATED         --MAY       1990.  FOR UNIX (I.E., GENERAL CASE), TRY TO
C                                        OPEN FILES WITH TRAILING PERIOD
C                                        STRIPPED OFF.
C     UPDATED         --NOVEMBER  1991.  CHANGES MADE FOR EASIER IMPLEMENTING
C     UPDATED         --DECEMBER  1993.  ACTIVATE 3 CUNIX LINES
C     UPDATED         --AUGUST    1994.  COMMENT OUT WRITE STATEMENTS
C     UPDATED         --APRIL     1996.  EXTEND 6/95 CHANGE TO UNIX,
C     UPDATED         --APRIL     1996.  ALLOW PATH NAME FOR UNIX TO
C                                        BE SET FROM ENVIRONMENT VARIABLE,
C                                        SOFT-CODE BACKSLASH FOR PC
C                                        TO AVOID UNIX COMPILATION 
C                                        ERRORS
C     UPDATED         --JUNE      1995.  AUTO-READ FROM DP SUB-DIRECTORIES
C     UPDATED         --AUGUST    1996.  FIX TO SUB-DIRECTORIES
C     UPDATED         --OCTOBER   2014.  ADD A USER SPECIFIED "SEARCH
C                                        DIRECTORY" THAT WILL BE SEARCHED
C                                        AFTER THE CURRENT DIRECTORY BUT
C                                        BEFORE THE DATAPLOT AUXILLARY
C                                        DIRECTORIES.
C     UPDATED         --OCTOBER   2014.  SIMPLIFY CODE
C     UPDATED         --OCTOBER   2018.  ADD A "10STEP" DIRECTORY
C     UPDATED         --MARCH     2019.  REMOVE "TEK" DIRECTORY FROM
C                                        SEARCH PATH
C     UPDATED         --MARCH     2020.  ADD TWO ADDITIONAL USER SEARCH
C                                        DIRECTORIES
C     UPDATED         --APRIL     2020.  UP MAXIMUM NUMBER OF CHARACTERS
C                                        FROM 80 TO 256
C     UPDATED         --APRIL     2020.  ADD USER SEARCH DIRECTORIES
C                                        4, 5 AND 6
C
C---------------------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
C
CCCCC CHARACTER*80 IFILEL
CCCCC CHARACTER*80 IFILEU
CCCCC CHARACTER*80 FTEMP
CCCCC CHARACTER*255 IFILE2
CCCCC CHARACTER*255 IFILE3
CCCCC CHARACTER*255 IFILE4
      PARAMETER (MAXNCH=2*MAXFNC)
      CHARACTER (LEN=MAXNCH) :: IFILEL
      CHARACTER (LEN=MAXNCH) :: IFILEU
      CHARACTER (LEN=MAXNCH) :: FTEMP
      CHARACTER (LEN=MAXNCH) :: IFILE2
      CHARACTER (LEN=MAXNCH) :: IFILE3
      CHARACTER (LEN=MAXNCH) :: IFILE4
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
CCCCC APRIL 1996.  SOFT-CODE BACKSLASH CHARACTER
      CHARACTER*1 IBSLC
      CHARACTER*1 IPATSE
C
C-----COMMON------------------------------------------------
C
      CHARACTER (LEN=MAXFNC) :: IFILSV
      COMMON/FILSAV/IFILSV
C
      CHARACTER*80 PROFIL
      CHARACTER*80 P86FIL
      CHARACTER*80 APPDAT
      CHARACTER*80 COMNAM
      CHARACTER*80 UPROFI
      CHARACTER*80 DEFPRI
      CHARACTER*20 USRNAM
      CHARACTER*20 ISHELL
      CHARACTER*4  WINBIT
      COMMON/SYSVAR/PROFIL,P86FIL,APPDAT,COMNAM,UPROFI,USRNAM,DEFPRI,
     1              WINBIT,ISHELL
      COMMON/SYSVA2/NCPROF,NCP86F,NCAPPD,NCCOMP,NCUPRO,NCUSER,NCPRIN,
     1              NCSHEL
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPOP'
      ISUBN2='FI  '
      FTEMP=' '
      IFILE2=' '
      IFILEL=' '
      IFILEU=' '
      IFILSV=' '
C
CCCCC APRIL 1996.  SOFT-CODE BACKSLASH CHARACTER
      IF(IOPSY1.EQ.'UNIX')THEN
        IPATSE='/'
      ELSE
        CALL DPCONA(92,IBSLC)
        IPATSE=IBSLC
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPOPFI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IBUGS2,ISUBRO,IERROR,IPATSE,IOUNIT
   52   FORMAT('IBUGS2,ISUBRO,IERROR,IPATSE,IOUNIT = ',4(A4,2X),I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,62)IFILE(1:80)
   62   FORMAT('IFILE  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)ISTAT,IFORM,IACCES,IPROT,ICURST
   63   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,68)IREWIN,ISUBN0,IERRFI,IHOST1
   68   FORMAT('IREWIN,ISUBN0,IERRFI,IHOST1 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *******************
C               **  STEP 1--     **
C               **  OPEN A FILE  **
C               *******************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHOST1.EQ.'VAX')THEN
#ifdef VAX_VMS
C
C-------TREAT THE VAX 11/7XX VMS CASE-----------------------------------
C       (NOTE--IF HAVE    READONLY   ARGUMENT,
C       THEN THE VAX WILL ONLY ALLOW    STATUS='OLD'   ;
C       STATUS = ANYTHING ELSE ('UNKNOWN' OR 'NEW')
C       WILL RESULT IN THE FILE NOT BEING OPENED
C       AND AN ERROR CONDITION RESULTING.)
C
        IF(IPROT.EQ.'READONLY')THEN
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
     1         IOSTAT=IOSTA2,ACCESS=IACCES,
     1         CARRIAGE CONTROL='LIST',READONLY)
          IF(IOSTA2.EQ.0)GOTO2190
          IOSTA2=0
          IFILE2(10:MAXFNC)=IFILE(1:MAXFNC-9)
          IFILE2(1:9)='DATAPLO$:'
          OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS='OLD',FORM=IFORM,
          1IOSTAT=IOSTA2,ACCESS=IACCES,CARRIAGE CONTROL='LIST',READONLY)
          IF(IOSTA2.EQ.0)THEN
            IFILE=IFILE2
            GOTO2190
          ENDIF
          GOTO8000
        ELSE
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
     1         ERR=8000,ACCESS=IACCES,CARRIAGE CONTROL='LIST')
          GOTO2190
        ENDIF
C
 2190   CONTINUE
        ICURST='OPEN'
        IERRFI='NO'
        IERROR='NO'
        IF(IREWIN.EQ.'ON')REWIND IOUNIT
        GOTO9000
#endif
C
      ELSEIF(IHOST1.EQ.'CDC' .OR. IHOST1.EQ.'CYBE' .OR.
     1       IHOST1.EQ.'205' .OR. IHOST1.EQ.'NVE' )THEN
C
C-------TREAT THE CDC CASE------------------------------------------
C       REFERENCE--ALAN HECKERT, 2899
C
        IOSTA2=0
C
C       FOR CDC, NOS AND NOS/VE, STRIP OFF THE TRAILING '.'
C       SINCE THIS CAUSES THE OPEN TO FAIL
C
        FTEMP=IFILE
        DO 2250 I=MAXFNC,1,-1
          IF(FTEMP(I:I).EQ.' ')GOTO 2250
          IF(FTEMP(I:I).EQ.IFCHAR) FTEMP(I:I)=' '
          GOTO 2260
 2250   CONTINUE
 2260   CONTINUE
        IF(IHOST1.EQ.'NVE')THEN
          IF(IOUNIT.EQ.IRD)OPEN(UNIT=IOUNIT,FILE='$INPUT',STATUS='OLD')
          IF(IOUNIT.EQ.IPR)OPEN(UNIT=IOUNIT,FILE='$OUTPUT',STATUS='OLD')
        ELSE
          IF(IOUNIT.EQ.IRD)OPEN(UNIT=IOUNIT,FILE='INPUT',STATUS='OLD')
          IF(IOUNIT.EQ.IPR)OPEN(UNIT=IOUNIT,FILE='OUTPUT',STATUS='OLD')
        ENDIF
        IF(IOUNIT.NE.IRD.AND.IOUNIT.NE.IPR)
     1    OPEN(UNIT=IOUNIT,FILE=FTEMP,STATUS=ISTAT,FORM=IFORM,
     1         IOSTAT=IOSTA2)
C
C       JANUARY,1989: CHECK FOR REFERENCE FILES.  HANDLE FOR NBS NOS/VE
C       CASE.  NOTE THAT DATAPLOT IS INSTALLED AS A "SYSTEM APPLICATION"
C       AT NBS.  OTHER NOS/VE SITES MAY OR MAY NOT HAVE IT INSTALLED
C       THIS WAY.
C
        IF(IOSTA2.EQ.0)GOTO2290
        IOSTA2=0
        IFILE2(42:80)=FTEMP(1:39)
        IFILE2(1:41)='.CS2.APPLICATIONS.DATAPLOT.VER_2.SAMPLES.'
        OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS=ISTAT,FORM=IFORM,
     1       IOSTAT=IOSTA2)
        IF(IOSTA2.NE.0)GOTO8000
C
 2290   CONTINUE
        ICURST='OPEN'
        IERRFI='NO'
        IERROR='NO'
        IF(IREWIN.EQ.'ON')REWIND IOUNIT
        GOTO9000
C
CCCCC ELSEIF(IHOST1.EQ.'PERK')THEN
C
C-------TREAT THE PERKIN-ELMER CASE-----------------------------------
C       REFERENCE--LARRY KAETZEL, 2650
C
CCCCC   IOSTA2=0
CCCCC   OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
CCCCC1       IOSTAT=IOSTA2,RECL=132,SIZE=8)
CCCCC   IF(IOSTA2.NE.0)GOTO8000
CCCCC   ICURST='OPEN'
CCCCC   IERRFI='NO'
CCCCC   IERROR='NO'
CCCCC   IF(IREWIN.EQ.'ON')REWIND IOUNIT
CCCCC   GOTO9000
C
CCCCC ELSEIF(IHOST1.EQ.'HONE')THEN
C
C-------TREAT THE HONEYWELL-MULTICS CASE-----------------------------------
CCCCC   IOSTA2=0
CCCCC   IF(IPROT.EQ.'READONLY')THEN
CCCCC1    OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
CCCCC1         IOSTAT=IOSTA2,ACCESS=IACCES,MODE='INPUT')
CCCCC     IF(IOSTA2.NE.0)GOTO8000
CCCCC   ELSE
CCCCC     OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
CCCCC1         ERR=8000,ACCESS=IACCES)
CCCCC   ENDIF
CCCCC   ICURST='OPEN'
CCCCC   IERRFI='NO'
CCCCC   IERROR='NO'
CCCCC   IF(IREWIN.EQ.'ON')REWIND IOUNIT
CCCCC   GOTO9000
C
      ENDIF
C
C-----TREAT THE GENERAL CASE (E.G., UNIX, MICROSOFT WINDOWS)-----------
C
C     1990/05: IF CAN NOT OPEN FILE, STRIP OFF TRAILING PERIOD (IF FILE
C              NAME ENDS WITH PERIOD) AND TRY TO OPEN.  THIS FIXES UNIX
C              BUG (DATAPLOT COULD NOT OPEN A FILE THAT DID NOT CONTAIN
C              A SUFFIX, I.E. READ TEST.  FAILED FOR A FILE NAMED "TEST".
C              NOTE THAT ON UNIX, THE FILE "TEST" AND "TEST." ARE NOT THE
C              SAME SO DATAPLOT WILL NOW TRY TO OPEN IT BOTH WAYS).
C
      FTEMP(1:MAXFNC)=IFILE(1:MAXFNC)
C
CCCCC JULY 2002: STRIP OFF LEADING/TRAILING QUOTES
C
      IF(IFILE(1:1).EQ.'"')THEN
        DO1102I=2,MAXFNC
          IF(IFILE(I:I).EQ.'"')THEN
            FTEMP=' '
            FTEMP(1:I-2)=IFILE(2:I-1) 
            GOTO1103
          ENDIF
 1102   CONTINUE
 1103   CONTINUE
      ENDIF
C
C     DETERMINE LAST NON-BLANK CHARACTER IN FILE NAME
C
      DO1107I=MAXFNC,1,-1
        IF(FTEMP(I:I).NE.' ')THEN
          NCFILE=I
          GOTO1109
        ENDIF
 1107 CONTINUE
      IERROR='YES'
      GOTO9000
 1109 CONTINUE
C
      IFLAG=0
C
 1110 CONTINUE
C
C     CONVERT FILE NAME TO LOWER CASE
C
      NMAX=MAXFNC
      CALL DPLO80(FTEMP,IFILEL,NMAX,IBUGS2,IERROR)
      CALL DPUP80(FTEMP,IFILEU,NMAX,IBUGS2,IERROR)
C
C     NOTE: K =  1 IS THE THE CURRENT DIRECTORY
C           K =  2 IS THE THE HOME DIRECTORY
C                (IT IS USER SETTABLE WHETHER THIS DIRECTORY
C                WILL BE SEARCHED)
C           K =  3, 4 5, 6, 7, AND 8 ARE THE USER SPECIFIED SEARCH
C                DIRECTORIES
C           K >= 9 ARE THE DATAPLOT AUXILIARY DIRECTORIES
C
      DO1141K=1,19
C
        NC1=0
        IFILE2=' '
        IFILE3=' '
        IFILE4=' '
C
        IF(K.GE.9)THEN
C
C         ADD DATAPLOT AUXILLARY PATH
C
          IF (IOPSY1.EQ.'UNIX') THEN
             IFILE2(1:IUNXNC)=UNIXPN(1:IUNXNC)
             IFILE3(1:IUNXNC)=UNIXPN(1:IUNXNC)
             IFILE4(1:IUNXNC)=UNIXPN(1:IUNXNC)
             NC1=IUNXNC
          ELSE
             IFILE2(1:NCPATH)=PATH(1:NCPATH)
             IFILE3(1:NCPATH)=PATH(1:NCPATH)
             IFILE4(1:NCPATH)=PATH(1:NCPATH)
             NC1=NCPATH
          ENDIF
        ENDIF
C
        IF(K.EQ.2)THEN
          IF(IHOMPA.EQ.'OFF' .OR. NCUPRO.LT.1)GOTO1141
          IFILE2(1:NCUPRO)=UPROFI(1:NCUPRO)
          IFILE3(1:NCUPRO)=UPROFI(1:NCUPRO)
          IFILE4(1:NCUPRO)=UPROFI(1:NCUPRO)
          NC1=NCUPRO
          IF(UPROFI(NCUPRO:NCUPRO).NE.IPATSE)THEN
            NC1=NC1+1
            IFILE2(NC1:NC1)=IPATSE
            IFILE3(NC1:NC1)=IPATSE
            IFILE4(NC1:NC1)=IPATSE
          ENDIF
        ELSEIF(K.EQ.3)THEN
          IF(NCSEDI.GE.1)THEN
            IFILE2(1:NCSEDI)=ISEADI(1:NCSEDI)
            IFILE3(1:NCSEDI)=ISEADI(1:NCSEDI)
            IFILE4(1:NCSEDI)=ISEADI(1:NCSEDI)
            NC1=NCSEDI
            IF(ISEADI(NCSEDI:NCSEDI).NE.IPATSE)THEN
              NC1=NC1+1
              IFILE2(NC1:NC1)=IPATSE
              IFILE3(NC1:NC1)=IPATSE
              IFILE4(NC1:NC1)=IPATSE
            ENDIF
          ELSE
            GOTO1141
          ENDIF
        ELSEIF(K.EQ.4)THEN
          IF(NCSED2.GE.1)THEN
            IFILE2(1:NCSED2)=ISEAD2(1:NCSED2)
            IFILE3(1:NCSED2)=ISEAD2(1:NCSED2)
            IFILE4(1:NCSED2)=ISEAD2(1:NCSED2)
            NC1=NCSED2
            IF(ISEAD2(NCSED2:NCSED2).NE.IPATSE)THEN
              NC1=NC1+1
              IFILE2(NC1:NC1)=IPATSE
              IFILE3(NC1:NC1)=IPATSE
              IFILE4(NC1:NC1)=IPATSE
            ENDIF
          ELSE
            GOTO1141
          ENDIF
        ELSEIF(K.EQ.5)THEN
          IF(NCSED3.GE.1)THEN
            IFILE2(1:NCSED3)=ISEAD3(1:NCSED3)
            IFILE3(1:NCSED3)=ISEAD3(1:NCSED3)
            IFILE4(1:NCSED3)=ISEAD3(1:NCSED3)
            NC1=NCSED3
            IF(ISEAD3(NCSED3:NCSED3).NE.IPATSE)THEN
              NC1=NC1+1
              IFILE2(NC1:NC1)=IPATSE
              IFILE3(NC1:NC1)=IPATSE
              IFILE4(NC1:NC1)=IPATSE
            ENDIF
          ELSE
            GOTO1141
          ENDIF
        ELSEIF(K.EQ.6)THEN
          IF(NCSED4.GE.1)THEN
            IFILE2(1:NCSED4)=ISEAD4(1:NCSED4)
            IFILE3(1:NCSED4)=ISEAD4(1:NCSED4)
            IFILE4(1:NCSED4)=ISEAD4(1:NCSED4)
            NC1=NCSED4
            IF(ISEAD4(NCSED4:NCSED4).NE.IPATSE)THEN
              NC1=NC1+1
              IFILE2(NC1:NC1)=IPATSE
              IFILE3(NC1:NC1)=IPATSE
              IFILE4(NC1:NC1)=IPATSE
            ENDIF
          ELSE
            GOTO1141
          ENDIF
        ELSEIF(K.EQ.7)THEN
          IF(NCSED5.GE.1)THEN
            IFILE2(1:NCSED5)=ISEAD5(1:NCSED5)
            IFILE3(1:NCSED5)=ISEAD5(1:NCSED5)
            IFILE4(1:NCSED5)=ISEAD5(1:NCSED5)
            NC1=NCSED5
            IF(ISEAD5(NCSED5:NCSED5).NE.IPATSE)THEN
              NC1=NC1+1
              IFILE2(NC1:NC1)=IPATSE
              IFILE3(NC1:NC1)=IPATSE
              IFILE4(NC1:NC1)=IPATSE
            ENDIF
          ELSE
            GOTO1141
          ENDIF
        ELSEIF(K.EQ.8)THEN
          IF(NCSED6.GE.1)THEN
            IFILE2(1:NCSED6)=ISEAD6(1:NCSED6)
            IFILE3(1:NCSED6)=ISEAD6(1:NCSED6)
            IFILE4(1:NCSED6)=ISEAD6(1:NCSED6)
            NC1=NCSED6
            IF(ISEAD6(NCSED6:NCSED6).NE.IPATSE)THEN
              NC1=NC1+1
              IFILE2(NC1:NC1)=IPATSE
              IFILE3(NC1:NC1)=IPATSE
              IFILE4(NC1:NC1)=IPATSE
            ENDIF
          ELSE
            GOTO1141
          ENDIF
C
C       DATATPLOT HELP DIRECTORY
C
        ELSEIF(K.EQ.10)THEN
          IFILE2(NC1+1:NC1+5)='help/'
          IFILE3(NC1+1:NC1+5)='help/'
          IFILE4(NC1+1:NC1+5)='help/'
          NC1=NC1+5
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.11)THEN
          IFILE2(NC1+1:NC1+5)='data/'
          IFILE3(NC1+1:NC1+5)='data/'
          IFILE4(NC1+1:NC1+5)='data/'
          NC1=NC1+5
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.12)THEN
          IFILE2(NC1+1:NC1+4)='dex/'
          IFILE3(NC1+1:NC1+4)='dex/'
          IFILE4(NC1+1:NC1+4)='dex/'
          NC1=NC1+4
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.13)THEN
          IFILE2(NC1+1:NC1+7)='10step/'
          IFILE3(NC1+1:NC1+7)='10step/'
          IFILE4(NC1+1:NC1+7)='10step/'
          NC1=NC1+7
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.14)THEN
          IFILE2(NC1+1:NC1+7)='macros/'
          IFILE3(NC1+1:NC1+7)='macros/'
          IFILE4(NC1+1:NC1+7)='macros/'
          NC1=NC1+7
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.15)THEN
          IFILE2(NC1+1:NC1+9)='programs/'
          IFILE3(NC1+1:NC1+9)='programs/'
          IFILE4(NC1+1:NC1+9)='programs/'
          NC1=NC1+9
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.16)THEN
          IFILE2(NC1+1:NC1+5)='text/'
          IFILE3(NC1+1:NC1+5)='text/'
          IFILE4(NC1+1:NC1+5)='text/'
          NC1=NC1+5
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.17)THEN
          IFILE2(NC1+1:NC1+5)='menu/'
          IFILE3(NC1+1:NC1+5)='menu/'
          IFILE4(NC1+1:NC1+5)='menu/'
          NC1=NC1+5
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.18)THEN
          IFILE2(NC1+1:NC1+3)='ps/'
          IFILE3(NC1+1:NC1+3)='ps/'
          IFILE4(NC1+1:NC1+3)='ps/'
          NC1=NC1+3
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        ELSEIF(K.EQ.19)THEN
          IFILE2(NC1+1:NC1+8)='scripts/'
          IFILE3(NC1+1:NC1+8)='scripts/'
          IFILE4(NC1+1:NC1+8)='scripts/'
          NC1=NC1+8
          IFILE2(NC1:NC1)=IPATSE
          IFILE3(NC1:NC1)=IPATSE
          IFILE4(NC1:NC1)=IPATSE
        END IF
C
        IFILE2(NC1+1:NC1+NCFILE)=FTEMP(1:NCFILE)
        IFILE3(NC1+1:NC1+NCFILE)=IFILEL(1:NCFILE)
        IFILE4(NC1+1:NC1+NCFILE)=IFILEU(1:NCFILE)
        NC1=NC1+NCFILE
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')THEN
          WRITE(ICOUT,1201)K,NC1,NCFILE,FTEMP(1:80)
 1201     FORMAT('DPOPFI: K,NC1,NCFILE,FTEMP = ',3I5,A80)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1203)IFILE2(1:NC1)
 1203     FORMAT('IFILE2 = ',A)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1205)IFILE3(1:NC1)
 1205     FORMAT('IFILE3 = ',A)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1207)IFILE4(1:NC1)
 1207     FORMAT('IFILE4 = ',A)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1209)ISTAT,IFORM
 1209     FORMAT('ISTAT,IFORM = ',A12,2X,A12)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
C       OPEN FILE NAME AS IS
C
        IOSTA2=0
        IF(IFORM.EQ.'NULL')THEN
          OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS=ISTAT,IOSTAT=IOSTA2)
        ELSE
          OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS=ISTAT,FORM=IFORM,
     1         IOSTAT=IOSTA2)
        ENDIF
C
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')THEN
          WRITE(ICOUT,1211)IOSTA2
 1211     FORMAT('AFTER ASIS FILE OPEN: IOSTA2 = ',I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        IF(IOSTA2.NE.0)THEN
          CLOSE(UNIT=IOUNIT,ERR=1141)
        ELSE
          IFILSV=IFILE2(1:MAXFNC)
          GOTO1190
        ENDIF
C
C       NOW OPEN LOWER CASE FILE NAME - CURRENTLY ONLY FOR UNIX
C
        IF (IOPSY1.EQ.'UNIX') THEN
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE3,STATUS=ISTAT,FORM=IFORM,
     1         IOSTAT=IOSTA2)
          IF(IOSTA2.NE.0)THEN
            CLOSE(UNIT=IOUNIT,ERR=1141)
          ELSE
            IFILSV=IFILE3(1:MAXFNC)
            GOTO1190
          ENDIF
C
C       NOW OPEN UPPER CASE FILE NAME - CURRENTLY ONLY FOR UNIX
C
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE4,STATUS=ISTAT,FORM=IFORM,
     1         IOSTAT=IOSTA2)
          IF(IOSTA2.NE.0)THEN
            CLOSE(UNIT=IOUNIT,ERR=1141)
          ELSE
            IFILSV=IFILE4(1:MAXFNC)
            GOTO1190
          ENDIF
        ENDIF
C
 1141 CONTINUE
C
C  MAY, 1990.  CHECK IF FILE ENDS WITH PERIOD (OR THE FILE CHARACTER).
C              IF SO, STRIP IT OFF AND REPEAT THE ABOVE SEQUENCE OF
C              OPEN COMMANDS.
C
      IFLAG=IFLAG+1
      IF(IFLAG.GT.1)GOTO8000
      FTEMP=IFILE
      DO1175I=MAXFNC,1,-1
        IF(FTEMP(I:I).EQ.' ')GOTO1175
        IF(FTEMP(I:I).EQ.IFCHAR)THEN
          FTEMP(I:I)=' '
          GOTO1110
        ELSE
          GOTO8000
        ENDIF
 1175 CONTINUE
      GOTO8000
C
 1190 CONTINUE
      ICURST='OPEN'
      IERRFI='NO'
      IERROR='NO'
      IF(IREWIN.EQ.'ON')REWIND IOUNIT
      GOTO9000
C
C               ************************************
C               **  STEP 80--                     **
C               **  GENERATE AN ERROR MESSAGE     **
C               **  IF THE FILE CANNOT BE OPENED  **
C               ************************************
C
 8000 CONTINUE
      IERRFI='YES'
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8011)
 8011 FORMAT('***** ERROR IN ATTEMPTING TO OPEN A FILE.--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8021)IOUNIT
 8021 FORMAT('I/O UNIT    = ',I8)
      CALL DPWRST('XXX','BUG ')
CCCCC WRITE(ICOUT,8022)IFILE(1:256)
C8022 FORMAT('FILE NAME   = ',A256)
      print *,'ifile: ',ifile
      WRITE(ICOUT,8022)IFILE
 8022 FORMAT('FILE NAME   = ',A)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8023)ISTAT
 8023 FORMAT('FILE STATUS = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8024)IFORM
 8024 FORMAT('FILE FORMAT = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8025)IACCES
 8025 FORMAT('FILE ACCESS = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8026)IPROT
 8026 FORMAT('FILE PROTECTION         = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8027)ICURST
 8027 FORMAT('FILE CURRENT STATUS     = ',A12)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8028)ISUBN0
 8028 FORMAT('PREVIOUS (= CALLING) SUBROUTINE = ',A4)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8029)IERRFI
 8029 FORMAT('FILE-FINDING ERROR FLAG = ',A4)
      CALL DPWRST('XXX','BUG ')
      IF(IHOST1.EQ.'VAX')THEN
        WRITE(ICOUT,8030)IOSTA2
 8030   FORMAT('IOSTAT FLAG             = ',I8)
        CALL DPWRST('XXX','BUG ')
      ELSE
        WRITE(ICOUT,8031)IOST1,IOST2,IOST3
 8031   FORMAT('IOSTAT FLAGS            = ',3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
      WRITE(ICOUT,8032)IHOST1
 8032 FORMAT('HOST COMPUTER           = ',A4)
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPOPFI--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IERROR,IREWIN,IERRFI,IOUNIT,IOSTA2
 9012   FORMAT('IERROR,IREWIN,IERRFI,IOUNIT,IOSTA2 = ',3(A4,2X),2I5)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9023)ISTAT,IFORM,IACCES,IPROT,ICURST
 9023   FORMAT('ISTAT,IFORM,IACCES,IPROT,ICURST  = ',4(A12,2X),A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IFILE(1:80)
 9022   FORMAT('IFILE(1:80)  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9041)IFILEL(1:80)
 9041   FORMAT('IFILEL(1:80)  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9042)IFILEU(1:80)
 9042   FORMAT('IFILEU((1:80)  = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9043)IFILE2(1:80)
 9043   FORMAT('IFILE2(1:80)  = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE CKCLAR(ITEMNU,ITEMNA,ITEMST,ITEMFO,
     1                  ITEMAC,ITEMPR,ITEMCS,ITEMEF,ITEMRW,
     1                  NUMCLA,CLARG1,CLARG2,IECHFL,
     1                  ISUBN0,IBUGS2,ISUBRO,IERRFI)
C
C     PURPOSE--CHECK THE COMMAND LINE (IN DOS) TO INVOKE DATAPLOT,
C              DETERMINE IF THE COMMAND LINE HAS ATTACHED ARGUMENTS,
C              AND RECORD SUCH ARGUMENTS.
C     ORIGINAL VERSION--FEBRUARY 1992
C     UPDATED         --APRIL    1992  ADD OPERATING SYSTEM SPECIFIC
C                                      SUPPORT (ALAN)
C     UPDATED         --MAY      2001  COMMAND LINE ARGUMENT FOR
C                                      MICROSOFT FORTRAN.  NOTE
C                                      THIS ACTUALLY DONE IN 
C                                      "MSFORT.F", THIS ROUTINE JUST
C                                      EXTRACTS FROM A COMMON BLOCK.
C     UPDATED         --AUGUST   2016  CALL LIST TO DPINFI
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      INCLUDE 'DPCOPA.INC'
C
CCCCC CHARACTER*80 ITEMNA
      CHARACTER (LEN=MAXFNC) :: ITEMNA
      CHARACTER*12 ITEMST
      CHARACTER*12 ITEMFO
      CHARACTER*12 ITEMAC
      CHARACTER*12 ITEMPR
      CHARACTER*12 ITEMCS
      CHARACTER*4 ITEMEF
      CHARACTER*4 ITEMRW
C
CCCCC CHARACTER*80 CLARG1
      CHARACTER (LEN=*) :: CLARG1
      CHARACTER*4 CLARG2
C  ADD FOLLOWING LINE APRIL 1992.  (FOR NOS/VE CASE)
CCCCC CHARACTER*80 ITEMP
      CHARACTER (LEN=MAXFNC) :: ITEMP
C
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERRFI
C
      CHARACTER*4 IERROR
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*8 IACC
C
C-----COMMON VARIABLES (GENERAL)---------------------------------------
C  APRIL 1992.  ADD FOLLOWING INCLUDE FILE
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
C  APRIL 1992.  ADD FOLLOWING FOR UNIX
#ifdef LINUX
      INTEGER iargc
#endif
C  APRIL 1992.  ADD FOLLOWING FOR CRAY UNICOS
#ifdef LINUX_CRAY
      INTEGER GETOARG
#endif
C  APRIL 1992.  ADD FOLLOWING FOR VAX
#ifdef VAX_VMS
      INTEGER*2 NCTEMP
#endif
C
      CHARACTER*4 IECHO2
CCCCC CHARACTER*80 FTEMP(50)
      CHARACTER (LEN=MAXFNC) :: FTEMP(50)
      COMMON/QUICKW2/IECHO2,FTEMP
      INTEGER NCFTMP(50)
      COMMON/QUICKW3/NCFTMP,NPCARG
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IECHFL=0
      ICNT2=0
      CLARG1=' '
      CLARG2=' '
C
      NMACLA=0
      DO110II=1,50
        IMACAR(II)=' '
        IMACLA(II)=' '
        IMACLL(II)=0
        IMACNC(II)=0
  110 CONTINUE
C
C               ********************************
C               **  STEP 1--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
C  INSTALLERS NOTE:  FOR THOSE OPERATING SYSTEMS THAT SUPPORT
C  A LIBRARY ROUTINE FOR EXTRACTING ARGUMENTS FROM THE COMMAND
C  LINE, PUT IN A BRANCH AND USE THAT MECHANISM.  FOR THOSE THAT
C  DON'T, READ THE ARGUMENTS FROM THE FILE "DPARGS.DAT".
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C               ********************************
C               **  STEP 2A1-                 **
C               **  IBM/PC USING MICROSOFT    **
C               **  COMPILER.  THE MSFORT.F   **
C               **  ROUTINE ACTUALLY DOES THE **
C               **  EXTRACTION, SIMPLY GET OUT**
C               **  OF COMMON HERE.           **
C               ********************************
C
        NUMCLA=0
        NUMARG=NPCARG
        NMACLA=0
        IECHFL=0
C
CCCCC   DATAPLOT SPECIFIC SWITCHES (E.G., -tile) ARE PROCESSED IN
CCCCC   MSFORT_INTEL.F.
C
        IF(NUMARG.GE.1)THEN
          ICNT=0
          ICNT2=0
          DO1010I=1,NUMARG
            IJUNK=I
            ITEMP=' '
            ICNT=ICNT+1
            NC1=NCFTMP(ICNT)
            ITEMP(1:NC1)=FTEMP(ICNT)(1:NC1)
            IF(ICNT.EQ.1)THEN
C
C             CHECK TO SEE IF FIRST UNLABELLED ARGUMENT IS
C             A FILE NAME.
C
              CALL STRLEZ(ITEMP,MAXFNC,NC1)
              IFLAG=0
              DO1020KK=1,NC1
                IF(ITEMP(KK:KK).EQ.'.')THEN
                  IFLAG=1
                  GOTO1029
                ENDIF
 1020         CONTINUE
 1029         CONTINUE
C
              IF(IFLAG.EQ.1)THEN
                CLARG1=' '
                CLARG1(1:NC1)=ITEMP(1:NC1)
                NUMCLA=1
              ELSE
                ICNT2=ICNT2+1
                IF(ICNT2.LE.50)THEN
                  IMACAR(ICNT2)=' '
                  IF(NPOSL1.GT.0 .AND. NPOSL2.GT.0)THEN
                    NLEN=NC1-NPOSR1+1
                    IF(NLEN.GT.MAXFNC)NLEN=MAXFNC
                    IMACAR(ICNT2)(1:NLEN)=
     1                     ITEMP(NPOSR1:NC1)
                    NMACLA=NMACLA+1
                    IMACLL(NMACLA)=ICNT2
                    NLEN2=NPOSL2-NPOSL1+1
                    IF(NLEN2.GT.8)NLEN2=8
                    IMACLA(NMACLA)(1:NLEN2)=
     1                     ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                    IMACNC(NMACLA)=NLEN2
                  ELSE
                    IMACAR(ICNT2)(1:MIN(MAXFNC,NC1))=
     1                     ITEMP(1:MIN(MAXFNC,NC1))
                  ENDIF
                ENDIF
                NUMCLA=0
              ENDIF
            ELSEIF(ICNT.EQ.2)THEN
              IF(ITEMP(1:2).EQ.'0 ')THEN
                CLARG2='0'
                NC2=1
                NUMCLA=2
              ELSEIF(ITEMP(1:2).EQ.'1 ')THEN
                CLARG2='1'
                NC2=1
                NUMCLA=2
              ELSEIF(ITEMP(1:2).EQ.'2 ')THEN
                CLARG2='2'
                NC2=1
                NUMCLA=2
              ELSE
                CALL STRLEZ(ITEMP,MAXFNC,NCT)
                CALL STREQU(ITEMP,NCT,NPOSL1,NPOSL2,NPOSR1)
                ICNT2=ICNT2+1
                IF(ICNT2.LE.50)THEN
                  IMACAR(ICNT2)=' '
                  IF(NPOSL1.GT.0 .AND. NPOSL2.GT.0)THEN
                    NLEN=NCT-NPOSR1+1
                    IMACAR(ICNT2)(1:NLEN)=
     1                     ITEMP(NPOSR1:NCT)
                    NMACLA=NMACLA+1
                    IMACLL(NMACLA)=ICNT2
                    NLEN2=NPOSL2-NPOSL1+1
                    IF(NLEN2.GT.8)NLEN2=8
                    IMACLA(NMACLA)(1:NLEN2)=
     1                     ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                    IMACNC(NMACLA)=NLEN2
                  ELSE
                    IMACAR(ICNT2)(1:MIN(MAXFNC,NCT))=
     1                     ITEMP(1:MIN(MAXFNC,NCT))
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              CALL STRLEZ(ITEMP,MAXFNC,NCT)
              CALL STREQU(ITEMP,NCT,NPOSL1,NPOSL2,NPOSR1)
              ICNT2=ICNT2+1
              IF(ICNT2.LE.50)THEN
                IMACAR(ICNT2)=' '
                IF(NPOSL1.GT.0 .AND. NPOSL2.GT.0)THEN
                  NLEN=NCT-NPOSR1+1
                  IMACAR(ICNT2)(1:NLEN)=
     1                   ITEMP(NPOSR1:NCT)
                  NMACLA=NMACLA+1
                  IMACLL(NMACLA)=ICNT2
                  NLEN2=NPOSL2-NPOSL1+1
                  IF(NLEN2.GT.8)NLEN2=8
                  IMACLA(NMACLA)(1:NLEN2)=
     1                   ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                  IMACNC(NMACLA)=NLEN2
                ELSE
                  IMACAR(ICNT2)(1:MIN(80,NCT))=
     1                   ITEMP(1:MIN(80,NCT))
                ENDIF
              ENDIF
            ENDIF
 1010     CONTINUE
        ENDIF
        NMACAG=ICNT2
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG')THEN
C
C               ********************************
C               **  STEP 2A--                 **
C               **  IBM/PC 386 WITH OTG COMPILER
C               **  ALSO FOR HOSTS WITH NO    **
C               **  SPECIFIC MECHANISM FOR    **
C               **  CAPTURING COMMAND LINE    **
C               **  ARGUMENTS                 **
C               ********************************
C
        NUMCLA=0
        NC1=0
        CLARG2(1:1)=' '
C
C       APRIL 1992.  CHECK FOR OPERATING HOST.  READ FROM FILE
C       DPARGS.DAT.  IF NO OPERATING SPECIFIC MECHANISM SUPPORTED.
C
C       STEP 1--
C       INQUIRE TO SEE IF THE FILE EXISTS
C
        CALL DPINFI(ITEMNA,IEXIST,IOPEN,IACC,ISUBN0,IBUGS2,
     1              ISUBRO,IERRFI)
        IF(IEXIST.EQ.'YES')GOTO1100
        GOTO9000
C
C       STEP 2--
C       IF EXISTS, THEN OPEN THE FILE
C       AND READ THE FIRST (ONLY) LINE FROM THE FILE INTO CLARG1
C
 1100   CONTINUE
        CALL DPOPFI(ITEMNU,ITEMNA,ITEMST,ITEMFO,ITEMAC,ITEMPR,ITEMCS,
     1              ITEMRW,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
        READ(ITEMNU,1120)CLARG1
 1120   FORMAT(A80)
C
C       STEP 3--
C       CLOSE THE FILE
C
        CALL DPCLFI(ITEMNU,ITEMNA,ITEMST,ITEMFO,ITEMAC,ITEMPR,ITEMCS,
     1              ITEMEF,ITEMRW,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
C
C       STEP 4--
C       EXTRACT THE (1 OR 2) ARGUMENTS FROM THE FILE
C
        IF(CLARG1(1:4).EQ.'    ')GOTO9000
        IF(CLARG1(1:4).EQ.'ECHO')GOTO9000
C
        NCALL=80
        DO1130I=1,NCALL
          I2=I
          IF(CLARG1(I:I).EQ.' ')THEN
            NC1=I2-1
            GOTO1139
          ENDIF
 1130   CONTINUE
        NC1=NCALL
 1139   CONTINUE
C
        IF(NC1.GE.NCALL-1)THEN
          NUMCLA=1
        ELSEIF(NC1.LE.0)THEN
          NUMCLA=0
        ELSEIF(CLARG1(NC1+2:NC1+2).EQ.' ')THEN
          NUMCLA=1
        ELSE
          CLARG2(1:1)=CLARG1(NC1+2:NC1+2)
          NUMCLA=2
          DO1145I=NC1+1,NCALL
            CLARG1(I:I)=' '
 1145     CONTINUE
        ENDIF
        GOTO9000
C
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C               ********************************
C               **  STEP 2B--                 **
C               **  CYBER WITH NOS/VE         **
C               **  USE PARAM FUNCTION.  NOTE **
C               **  THAT PARAMETER MUST BE    **
C               **  NAMED (USE F FOR FILE AND **
C               **  B FOR BANNER OPTION.      **
C               **  NOTE THAT A C$   PARAM    **
C               **  STATEMENT APPEARS IN THE  **
C               **  MAIN PROGRAM (THIS IS     **
C               **  REQUIRED).                **
C               ********************************
C
#ifdef NOS_VE
        NUMCLA=0
        IF(TSTPARM('F')) THEN
          CALL GETCVAL('F',1,1,,'LOW',NC1,CLARG1)
          IF(CLARG1(1:4).EQ.'    ')GOTO9000
          IF(CLARG1(1:4).EQ.'ECHO')GOTO9000
          NUMCLA=1
        ENDIF
        IF(TSTPARM('B')) THEN
          CALL GETCVAL('B',1,1,'LOW',NC2,ITEMP)
          CLARG2(1:1)=ITEMP(1:1)
          NUMCLA=2
        ENDIF
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'CRAY')THEN
C
C               ********************************
C               **  STEP 2C.1--               **
C               **  CRAY UNIX USES A DIFFERENT**
C               **  USE GETOARG CALL.         **
C               ********************************
C
#ifdef LINUX_CRAY
        NUMCLA=0
        IRET=GETOARG(CLARG1)
        IF(IRET.EQ.0)THEN
          CLARG1=' '
          GOTO9000
        ELSE
          IF(CLARG1(1:4).EQ.'    ')GOTO9000
          IF(CLARG1(1:4).EQ.'ECHO')GOTO9000
          CALL STRLEN(CLARG1,MAXFNC,NC1)
          NUMCLA=1
        ENDIF
        IRET=GETOARG(ITEMP)
        IF(IRET.EQ.0)THEN
          CLARG2(1:1)=' '
        ELSE
          CLARG2(1:1)=ITEMP(1:1)
          NUMCLA=2
        ENDIF
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C               ********************************
C               **  STEP 2C--                 **
C               **  UNIX OPERATING SYSTEM     **
C               **  USE argv AND iargc CALLS. **
C               **  MAY NEED TO CHECK THAT THESE
C               **  ARE VALID ON YOUR UNIX    **
C               **  SYSTEM.                   **
C               ********************************
C
#ifdef LINUX
        NUMCLA=0
        NUMARG=iargc()
        NMACLA=0
        IECHFL=0
C
CCCCC   CHECK IF FIRST ARGUMENT IS EITHER "-GUI" OR "-NOGUI".
CCCCC   IF SO, SET "IPROAD" SWITCH.
CCCCC
CCCCC   2019/02: CHECK FOR FOLLOWING SWITCHES
CCCCC
CCCCC             -gui
CCCCC             -nogui
CCCCC             -echo
C
        IF(NUMARG.GE.1)THEN
          ICNT=0
          ICNT2=0
          DO4010I=1,NUMARG
            IJUNK=I
            ITEMP=' '
            CALL getarg(IJUNK,ITEMP)
            IF(ITEMP(1:4).EQ.'-gui' .OR. ITEMP(1:4).EQ.'-GUI')THEN
              IPROAD='OFF'
            ELSEIF(ITEMP(1:6).EQ.'-nogui' .OR.
     1             ITEMP(1:6).EQ.'-NOGUI')THEN
              IPROAD='ON'
            ELSEIF(ITEMP(1:5).EQ.'-echo' .OR.
     1             ITEMP(1:5).EQ.'-ECHO')THEN
              IECHFL=1
            ELSE
              ICNT=ICNT+1
              IF(ICNT.EQ.1)THEN
C
C               CHECK TO SEE IF FIRST UNLABELLED ARGUMENT IS
C               A FILE NAME.
C
                CALL STRLEZ(ITEMP,MAXFNC,NC1)
                CALL STREQU(ITEMP,NC1,NPOSL1,NPOSL2,NPOSR1)
                IFLAG=0
                DO4020KK=1,NC1
                  IF(ITEMP(KK:KK).EQ.'.')THEN
                    IFLAG=1
                    GOTO4029
                  ENDIF
 4020           CONTINUE
 4029           CONTINUE
C
                IF(IFLAG.EQ.1)THEN
                  CLARG1=' '
                  CLARG1(1:NC1)=ITEMP(1:NC1)
                  NUMCLA=1
                ELSE
                  ICNT2=ICNT2+1
                  IF(ICNT2.LE.50)THEN
                    IMACAR(ICNT2)=' '
                    IF(NPOSL1.GT.0 .AND. NPOSL2.GT.0)THEN
                      NLEN=NC1-NPOSR1+1
                      IMACAR(ICNT2)(1:NLEN)=
     1                       ITEMP(NPOSR1:NC1)
                      NMACLA=NMACLA+1
                      IMACLL(NMACLA)=ICNT2
                      NLEN2=NPOSL2-NPOSL1+1
                      IF(NLEN2.GT.8)NLEN2=8
                      IMACLA(NMACLA)(1:NLEN2)=
     1                       ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                      IMACNC(NMACLA)=NLEN2
                    ELSE
                      IMACAR(ICNT2)(1:MIN(80,NC1))=
     1                       ITEMP(1:MIN(80,NC1))
                    ENDIF
                  ENDIF
                  NUMCLA=0
                ENDIF
              ELSEIF(ICNT.EQ.2)THEN
                IF(ITEMP(1:2).EQ.'0 ')THEN
                  CLARG2='0'
                  NC2=1
                  NUMCLA=2
                ELSEIF(ITEMP(1:2).EQ.'1 ')THEN
                  CLARG2='1'
                  NC2=1
                  NUMCLA=2
                ELSEIF(ITEMP(1:2).EQ.'2 ')THEN
                  CLARG2='2'
                  NC2=1
                  NUMCLA=2
                ELSE
                  CALL STRLEZ(ITEMP,MAXFNC,NCT)
                  CALL STREQU(ITEMP,NCT,NPOSL1,NPOSL2,NPOSR1)
                  ICNT2=ICNT2+1
                  IF(ICNT2.LE.50)THEN
                    IMACAR(ICNT2)=' '
                    IF(NPOSL1.GT.0 .AND. NPOSL2.GT.0)THEN
                      NLEN=NCT-NPOSR1+1
                      IMACAR(ICNT2)(1:NLEN)=
     1                       ITEMP(NPOSR1:NCT)
                      NMACLA=NMACLA+1
                      IMACLL(NMACLA)=ICNT2
                      NLEN2=NPOSL2-NPOSL1+1
                      IF(NLEN2.GT.8)NLEN2=8
                      IMACLA(NMACLA)(1:NLEN2)=
     1                       ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                      IMACNC(NMACLA)=NLEN2
                    ELSE
                      IMACAR(ICNT2)(1:MIN(80,NCT))=
     1                       ITEMP(1:MIN(80,NCT))
                    ENDIF
                  ENDIF
                ENDIF
              ELSE
                CALL STRLEZ(ITEMP,MAXFNC,NCT)
                CALL STREQU(ITEMP,NCT,NPOSL1,NPOSL2,NPOSR1)
                ICNT2=ICNT2+1
                IF(ICNT2.LE.50)THEN
                  IMACAR(ICNT2)=' '
                  IF(NPOSL1.GT.0 .AND. NPOSL2.GT.0)THEN
                    NLEN=NCT-NPOSR1+1
                    IMACAR(ICNT2)(1:NLEN)=
     1                     ITEMP(NPOSR1:NCT)
                    NMACLA=NMACLA+1
                    IMACLL(NMACLA)=ICNT2
                    NLEN2=NPOSL2-NPOSL1+1
                    IF(NLEN2.GT.8)NLEN2=8
                    IMACLA(NMACLA)(1:NLEN2)=
     1                     ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                    IMACNC(NMACLA)=NLEN2
                  ELSE
                    IMACAR(ICNT2)(1:MIN(MAXFNC,NCT))=
     1                     ITEMP(1:MIN(MAXFNC,NCT))
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 4010     CONTINUE
          NMACAG=ICNT2
        ENDIF
C
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C               ********************************
C               **  STEP 2D--                 **
C               **  VAX/VMS                   **
C               **  USE CLI$PRESENT AND       **
C               **  CLI$GET_VALUE LIBRARY CALLS*
C               **  DEFAULT PARAMETER NAMES ARE*
C               **  P1 AND P2.                **
C               ********************************
C       IMPLEMENTORS NOTE.  THIS CODE HAS NOT BEEN TESTED!!!
C       IT LOOKS RIGHT ACCORDING TO VAX FORTRAN MANUAL, BUT IT
C       MAY NEED TO BE DEBUGGED.  ALSO, UNCOMMENT "INTEGER*2 NCTEMP"
C       LINE IN DECLARATION.
C
#ifdef VAX_VMS
        NUMCLA=0
        IF(CLI$PRESENT('P1'))THEN
          ISTATUS=CLI$GET_VALUE('P1',CLARG1,NCTEMP)
          IF(CLARG1(1:4).EQ.'    ')GOTO9000
          IF(CLARG1(1:4).EQ.'ECHO')GOTO9000
          NC1=NCTEMP
          NUMCLA=1
        ENDIF
        IF(CLI$PRESENT('P2'))THEN
          ISTATUS=CLI$GET_VALUE('P1',ITEMP,NCTEMP)
          CLARG2(1:1)=ITEMP(1:1)
          NUMCLA=2
        ENDIF
#endif
        GOTO9000
      ELSE
        GOTO9000
      ENDIF
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLAR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF SUBROUTINE CKCLAR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)NUMCLA,NC1,IEXIST
 9013   FORMAT('NUMCLA,NC1,IEXIST = ',2I8,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)CLARG1(1:80)
 9014   FORMAT('CLARG1 = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)CLARG2(1:1)
 9015   FORMAT('CLARG2 = ',A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSYS2(ITEXT,IWIDTH,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE IS USED BY DPSYST AND A FEW OTHER ROUTINES
C              TO ENTER AN OPERATING SYSTEM COMMAND. IT WAS ISOLATED
C              FROM DPSYST SO THAT THERE IS ONLY ONE ROUTINE THAT
C              ACTUALLY ISSUES AN OPERATING DEPENDENDENT CALL.
C     TO THE IMPLEMENTER--
C              SOME (SIMPLE) EDITING MUST BE DONE ONE THIS ROUTINE BEFORE
C              IT WILL RUN ON ANY COMPUTER.  IN GENERAL, ACTIVATE ALL
C              LINES RELATING TO YOUR COMPUTER BY REMOVING ALL PREFIXES
C              DESIGNATING YOUR COMPUTER.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTUTUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--89.3
C     ORIGINAL VERSION--APRIL      1992.
C     UPDATED         --APRIL      1992. ISUBRO & DEBUG STATEMENTS
C     UPDATED         --APRIL      1992. COMPILER=OTG
C     UPDATED         --JANUARY    1994. COMMENTS ABOVE
C     UPDATED         --MAY        1994. IMPLEMENT FOR CRAY
C     UPDATED         --JUNE       1996. LAHEY COMPILER FOR PC.
C     UPDATED         --OCTOBER    1996. MICROSOFT COMPILER FOR PC.
C     UPDATED         --NOVEMBER   2002. FOR MICROSOFT COMPILER,
C                                        OPTION TO USE EITHER
C     UPDATED         --NOVEMBER   2015. FOR WINDOWS, ISSUES IF STRING
C                                        CONTAINS QUOTES
C     UPDATED         --APRIL      2018. SUPPORT FOR
C                                        "COMMAND_LINE_EXECUTE"
C     UPDATED         --APRIL      2018. USE PRE_PROCESSOR DIRECTIVES
C                                        TO ALLOW SAME CODE TO BE USED
C                                        ON LINUX AND WINDOWS
C     UPDATED         --MARCH      2019. SUPPORT FOR "PERSISTANCE" AND
C                                        "HIDDEN" FOR WINDOWS SYSTEMS
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
#ifdef INTEL
      USE DFLIB
      USE DFWIN
      USE IFPORT
#endif
#ifdef INTEL_QWIN
      USE IFLPORT
#endif
#ifdef INTEL
#ifdef HAVE_EXECUTE_COMMAND_LINE
      CHARACTER*512 ISTRIN
#endif
#endif
C
      CHARACTER*(*) ITEXT
C
      CHARACTER*100 ICMSG
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*1 IQUOT1
      CHARACTER*1 IQUOT2
      CHARACTER*4 IBUGS2
C
      INTEGER IESTAT
      INTEGER ICSTAT
C
C  UNCOMMENT FOLLOWING TWO LINES FOR VAX/VMS
#ifdef VAX_VMS
      INTEGER LIB$SPAWN
      INTEGER ISTAT2
#endif
#ifdef LINUX
      LOGICAL system
#endif
      LOGICAL ISTATUS
      LOGICAL IWAIT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
      IBUGS2='OFF'
C
      IF(ISUBRO.EQ.'SYS2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSYS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IWIDTH
   53   FORMAT('IWIDTH= ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(ITEXT(I:I),I=1,MIN(IWIDTH,80))
   54   FORMAT('(ITEXT(I:I),I=1,IWIDTH) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GT.80)THEN
          WRITE(ICOUT,55)(ITEXT(I:I),I=81,MIN(IWIDTH,160))
   55     FORMAT('(ITEXT(I:I),I=1,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IWIDTH.GT.160)THEN
          WRITE(ICOUT,56)(ITEXT(I:I),I=161,MIN(IWIDTH,95))
   56     FORMAT('(ITEXT(I:I),I=161,IWIDTH) = ',95A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,61)IHOST1,IHOST2,IHMOD1,IHMOD2,ISUBRO
   61   FORMAT('IHOST1,IHOST2,IHMOD1,IHMOD2,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IOPSY1,IOPSY2,ICOMPI,ISITE,ISYSPE,ISYSHI
   63   FORMAT('IOPSY1,IOPSY2,ICOMPI,ISITE,ISYSPE,ISYSHI = ',
     1         5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 1--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
C     ALL LINUX/UNIX HOSTS TESTED BY OPERATING SYSTEM RATHER THAN HOST.
C     THE CRAY DOES NOT SUPPORT THE STANDARD UNIX CALL "SYSTEM", SO
C     EXPLICITLY TEST FOR IT.
C
C     TWO METHODS SUPPORTED:
C
C        1. THE FORTRAN 2008 STANDARD INTRODUCED THE
C           "COMMAND_LINE_EXECUTE" COMMAND FOR THIS PURPOSE.
C           THE gfortran COMPILER ADDED SUPPORT FOR THIS AT
C           VERSION 4.6.  USE A PRE-PROCESSOR FLAG SINCE OLDER
C           VERSIONS OF THE COMPILER ARE STILL AROUND.
C
C        2. USE THE "SYSTEM" CALL.  THIS WORKS FOR gfortran, BUT IT IS
C           NOT PART OF ANY FORTRAN STANDARD.  SO IT MAY OR MAY NOT BE
C           AVAILABLE ON COMPILERS OTHER THAN gfortran.
C
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        IF(IHOST1.EQ.'CRAY')GOTO8000
#ifdef LINUX
        IF(ILINSY.EQ.'COMM')THEN
C
C         USE FORTRAN 2008 "EXECUTE_COMMAND_LINE" SYNTAX
C
          IQUOT1='"'
          IQUOT2="'"
C
C         FOLLOWING BLOCK TO ENCLOSE STRING IN SINGLE QUOTES
C         DOES NOT SEEM NECCESSARY, SO COMMENT OUT FOR NOW.
C
CCCCC     IF(ITEXT(1:1).NE.IQUOT2 .AND.
CCCCC1       ITEXT(IWIDTH:IWIDTH).NE.IQUOT2 .AND.
CCCCC1       IWIDTH.LE.253)THEN
CCCCC       DO2325J=IWIDTH,1,-1
CCCCC         ITEXT(J+1:J+1)=ITEXT(J:J)
C2325       CONTINUE
CCCCC       ITEXT(IWIDTH+2:IWIDTH+2)=IQUOT1
CCCCC       ITEXT(1:1)=IQUOT1
CCCCC       IWIDTH=IWIDTH+2
CCCCC     ENDIF
C
          IWAIT=.FALSE.
          IF(ICLEWT.EQ.'ON')IWAIT=.TRUE.
C
C         2018/04: FOLLOWING LINE IS SUPPORTED FOR gcc VERSION 5.x.
C                  HOWEVER, IT DOES NOT APPEAR TO BE SUPPORTED ON
C                  EARLIER VERSIONS.
C
C                  IN THE BUILD FILE, ADD A PRE-PROCESSOR OPTION FOR
C                  THIS SO WE CAN CONDITIONALLY COMPILE THIS.
C
#ifdef HAVE_EXECUTE_COMMAND_LINE
          CALL EXECUTE_COMMAND_LINE(ITEXT,IWAIT,IESTAT,ICSTAT,ICMSG)
          IF(ICSTAT.GT.0)THEN
            WRITE(ICOUT,2311)
 2311       FORMAT('***** ERROR IN SYSTEM COMMAND--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2342)ICMSG
 2342       FORMAT('      EXECUTE_COMMAND_LINE FAILED WITH ERROR ',A100)
            CALL DPWRST('XXX','BUG ')
          ELSEIF(ICSTAT.LT.0)THEN
            WRITE(ICOUT,2311)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2344)
 2344       FORMAT('      COMMAND EXECUTION NOT SUPPORTED.')
            CALL DPWRST('XXX','BUG ')
          ELSE
CCCCC       IF(IFEEDB.EQ.'ON')THEN
CCCCC         WRITE(ICOUT,999)
CCCCC         CALL DPWRST('XXX','BUG ')
CCCCC         WRITE(ICOUT,2346)IESTAT
C2346         FORMAT('      COMMAND COMPLETED WITH STATUS ',I8)
CCCCC         CALL DPWRST('XXX','BUG ')
CCCCC       ENDIF
          ENDIF
#else
          WRITE(ICOUT,2311)
 2311     FORMAT('***** ERROR IN SYSTEM COMMAND--')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2342)ICMSG
 2342     FORMAT('      EXECUTE_COMMAND_LINE FAILED WITH ERROR',A100)
          CALL DPWRST('XXX','BUG ')
#endif
        ELSE
          ISTAT=system(ITEXT(1:IWIDTH))
        ENDIF
CCCCC   IF(ISTAT)IERROR='YES'
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-')THEN
        IF(ICOMPI.EQ.'MS-F')THEN
#ifdef INTEL
C
C         *****************************************************
C         *  PC USING INTEL (FORMERLY MICROSOFT, COMPAQ       *
C         *  DIGITAL).  SEVERAL METHODS SUPPORTED (SEE NOTES) *
C         *********************************************************
C
C         THE FOLLOWING METHODS ARE SUPPORTED:
C
C            1. SYSTEMQQ - THIS IS THE DEFAULT METHOD.  NOTE THE
C               FOLLOWING:
C
C               a. THIS IS PART OF THE INTEL LIBRARY ROUTINES AND
C                  IS NOT STANDARD FORTRAN.
C
C               b. SYSTEMQQ POPS UP A CONSOLE WINDOW, WAITS FOR
C                  THE COMMAND TO EXECUTE, AND THEN CLOSES THE
C                  WINDOW.
C
C               c. SYSTEMQQ DOES NOT PROVIDE CONTROL OVER OPENING/
C                  CLOSING THE COMMAND WINDOW AND DOES NOT HAVE AN
C                  AN OPTION FOR RETURNING CONTROL BACK TO DATAPLOT
C                  BEFORE THE OS COMMAND IS FINISHED EXECUTING.
C
C               d. PREVIOUS VERSIONS ALSO SUPPORTED RUNQQ.  HOWEVER,
C                  THERE ISN'T REALLY ANY REASON TO PREFER RUNQQ
C                  OVER SYSTEMQQ, SO WE HAVE DROPPED THIS AS A SEPARATE
C                  OPTION.
C
C            2. USE  CALL EXECUTE_COMMAND_LINE
C
C               THIS IS SUPPORTED IN THE FORTRAN 2008 STANDARD.
C               HOWEVER, IT MAY NOT BE SUPPORTED IN ALL WINDOWS
C               FORTRAN COMPILERS (ADDED TO INTEL COMPILER WITH
C               VERSION 16).
C
C            3. THE FOLLOWING ARE STILL EXPERIMENTAL:
C
C                  WINEXEC
C                  CreateProcess
C                  ShellScript
C
C               THESE ARE MORE COMPLEX THAN THE ABOVE OPTIONS, BUT
C               PROVIDE MUCH GREATER FLEXIBILITY IN HOW AN APPLICATION
C               IS STARTED AND RUN.  FOR NOW, THESE ARE NOT ACTUALLY
C               WORKING, BUT CODE IS LEFT IN FOR POSSIBLE FUTURE
C               DEVELOPMENT.
C
C         2019/03: SUPPORT FOR FOLLOWING OPTIONS
C
C                     SET SYSTEM PERSIST <ON/OFF>
C                     SET SYSTEM HIDDEN <ON/OFF>
C
C                  IF PERSIST IS SET TO "ON", LEAVE THE DOS COMMAND
C                  WINDOW OPEN AFTER THE COMMAND EXECUTES.  DO THIS
C                  BY ADDING "cmd /K" TO BEGINNING OF LINE.  THIS IS
C                  ONLY USED FOR THE SYSTEMQQ OPTION.
C
C                  IF HIDDEN IS SET TO "ON", DO NOT SHOW THE DOS
C                  COMMAND WINDOW.  HIDDEN WILL USE THE
C                  "EXECUTE_COMMAND_LINE" OPTION (IF AVAILABLE).  IF
C                  THIS OPTION IS NOT AVAILABLE, SYSTEMQQ WILL USED AND
C                  THE DOS COMMAND WINDOW WILL APPEAR.  NOTE THAT SOME
C                  DATAPLOT COMMANDS THAT CALL DPSYS2 WILL SET "HIDDEN"
C                  AUTOMATICALLY.
C
C         STEP 1: COUNT NUMBER OF DOUBLE QUOTES (") IN THE STRING
C
          IQUOT1='"'
          IQUOT2="'"
          NQUOTE=0
          DO2610II=1,IWIDTH
            IF(ITEXT(II:II).EQ.IQUOT1)NQUOTE=NQUOTE+1
 2610     CONTINUE
          NTEMP=MOD(NQUOTE,2)
          IF(NTEMP.EQ.1)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2611)
 2611       FORMAT('***** ERROR IN SYSTEM COMMAND--')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2613)NQUOTE
 2613       FORMAT('      UNBALANCED QUOTES DETECTED (',I3,' QUOTES ',
     1             'DETECTED).')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
CCCCC     IF(NQUOTE.GE.1)THEN
CCCCC       IWIDSV=IWIDTH
CCCCC       CALL DPCONA(92,IBASLC)
CCCCC       print *,'nquote,ibaslc = ',nquote,ibaslc
CCCCC       IFLAG=0
CCCCC       DO2621II=IWIDTH,1,-1
CCCCC         IF(IFLAG.EQ.1)THEN
CCCCC           IFLAG=0
CCCCC           GOTO2621
CCCCC         ENDIF
CCCCC         CALL DPCOAN(ITEXT(II:II),IVAL)
CCCCC         print *,'ii,itext(ii:ii),ival = ',ii,itext(ii:ii),ival
CCCCC         IF(ITEXT(II:II).EQ.IBASLC)THEN
CCCCC         IF(IVAL.EQ.92)THEN
CCCCC           print *,'itext(ii:ii) = \ case'
CCCCC           IF(II.GT.1 .AND.ITEXT(II-1:II-1).EQ.IBASLC)THEN
CCCCC             print *,'previous character also \'
CCCCC             IFLAG=1
CCCCC           ELSE
CCCCC             print *,'add \ character case'
CCCCC             ITEXT(II+1:IWIDSV+1)=ITEXT(II:IWIDSV)
CCCCC             ITEXT(II:II)=IBASLC
CCCCC             IWIDSV=IWIDSV+1
CCCCC           ENDIF
CCCCC         ELSE
CCCCC           print *,'no match \ case'
CCCCC         ENDIF
C2621       CONTINUE
CCCCC       IWIDTH=IWIDSV
CCCCC     ENDIF
C
C         STEP 2: 2019/03 - IF "HIDDEN" IS SPECIFIED OR A
C                 "SET QWIN SYSTEM EXECUTE COMMAND LINE" COMMAND GIVEN,
C                 THEN USE "CALL EXECUTE COMMAND LINE".
C
C                 NOTE THAT ALTHOUGH THE FOLLOWING WORKS WHEN RUNNING
C                 FROM A COMMAND WINDOW (I.E., DOES NOT OPEN NEW
C                 COMMAND WINDOW), IT DOES NOT WORK FROM WITHIN A
C                 "SYSTEMQQ" CALL.
C
C                     start /d <current path> /min cmd /c
C                           <string>
C
#ifdef HAVE_EXECUTE_COMMAND_LINE
          IF(ISYSHI.EQ.'ON' .OR. IQWNSY.EQ.'COMM')THEN
            NC=0
            ISTRIN=' '
            DO2410II=1,IWIDTH
              NC=NC+1
              ISTRIN(NC:NC)=ITEXT(II:II)
 2410       CONTINUE
CCCCC       NC=NC+1
CCCCC       ISTRIN(NC:NC)="'"
            IWAIT=.FALSE.
            IF(ICLEWT.EQ.'ON')IWAIT=.TRUE.
            CALL EXECUTE_COMMAND_LINE(ISTRIN,IWAIT,IESTAT,ICSTAT,ICMSG)
            IF(ICSTAT.GT.0)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2642)ICMSG
 2642         FORMAT('      EXECUTE_COMMAND_LINE FAILED WITH ERROR ',
     1               A100)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(ICSTAT.LT.0)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2644)
 2644         FORMAT('      COMMAND EXECUTION NOT SUPPORTED.')
              CALL DPWRST('XXX','BUG ')
            ELSE
CCCCC         IF(IFEEDB.EQ.'ON')THEN
CCCCC           WRITE(ICOUT,999)
CCCCC           CALL DPWRST('XXX','BUG ')
CCCCC           WRITE(ICOUT,2646)IESTAT
C2646           FORMAT('      COMMAND COMPLETED WITH STATUS ',I8)
CCCCC           CALL DPWRST('XXX','BUG ')
CCCCC         ENDIF
            ENDIF
C
            IF(ISUBRO.EQ.'SYS2')THEN
                WRITE(ICOUT,2411)NC
 2411           FORMAT('WINDOWS SYSTEM HIDDEN CASE: NC = ',I5)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2413)ISTRIN(1:MIN(NC,220))
 2413           FORMAT('ISTRIN: ',A)
                CALL DPWRST('XXX','BUG ')
            ENDIF
C
            GOTO9000
          ENDIF
#endif
C
C         STEP 2: FOR SYSTEMQQ, THE FIRST AND LAST OCCURENCES OF THE
C                 DOUBLE QUOTE NEED TO BE REPEATED.
C
C                 2018/04: CHANGE THIS SO WE SIMPLY ENCLOSE
C                          THE STRING IN SINGLE QUOTES REGARDLESS OF
C                          POSITION OR OCCURENCE OF DOUBLE QUOTES.
C
          IF(IQWNSY.EQ.'SYST' .OR. IQWNSY.EQ.'RUNQ')THEN
            IF(NQUOTE.GE.1)THEN
              NFIRST=1
              NLAST=NQUOTE
            ELSE
              NFIRST=0
              NLAST=0
            ENDIF
            NCNT=0
            IWIDT2=IWIDTH
            DO2620I=IWIDTH,1,-1
              IF(ITEXT(I:I).EQ.IQUOT1)THEN
                NCNT=NCNT+1
                IF(NCNT.EQ.NFIRST .OR. NCNT.EQ.NLAST)THEN
                  IF(I.LT.IWIDT2)THEN
                    DO2630J=IWIDT2,I+1,-1
                      ITEXT(J+1:J+1)=ITEXT(J:J)
 2630               CONTINUE
                  ENDIF
                  ITEXT(I+1:I+1)=IQUOT1
                  IWIDT2=IWIDT2+1
                ENDIF
              ENDIF
 2620       CONTINUE
            IWIDTH=IWIDT2
          ENDIF
C
          IF(ISYSPE.EQ.'ON' .AND. IWIDTH.LE.246)THEN
            IF(ITEXT(1:4).NE.'cmd ' .AND. ITEXT(1:4).NE.'CMD ')THEN
              DO22635I=IWIDTH,1,-1
                ITEXT(I+7:I+7)=ITEXT(I:I)
22635         CONTINUE
              ITEXT(1:7)='cmd /K '
              IWIDTH=IWIDTH+7
            ENDIF
          ENDIF
C
          ISTATUS=SYSTEMQQ(ITEXT(1:IWIDTH))
          IF(.NOT.ISTATUS)THEN
            IERRID=GETLASTERRORQQ()
            IF(IERRID.EQ.ERR$2BIG)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2631)
 2631         FORMAT('      THE ARGUMENT LIST EXCEEDS 128 BYTES OR ',
     1               'REQUIRED ENVIRONMENT SPACE TOO BIG.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IERRID.EQ.ERR$NOINT)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2632)
 2632         FORMAT('      THE COMMAND INTERPERTER NOT FOUND.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IERRID.EQ.ERR$NOEXEC)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2633)
 2633         FORMAT('      THE COMMAND INTERPERTER FILE HAS AN ',
     1               'INVALID FORMAT AND IS NOT EXECUTABLE.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IERRID.EQ.ERR$NOEMEM)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2634)
 2634         FORMAT('      INSUFFICIENT MEMORY TO EXECUTE THE ',
     1               'COMMAND.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSE
CCCCC.       IF(IFEEDB.EQ.'ON')THEN
CCCCC.         WRITE(ICOUT,999)
CCCCC.         CALL DPWRST('XXX','BUG ')
CCCCC.         WRITE(ICOUT,2641)ISTATUS
C2641         FORMAT('STATUS FROM SYSTEM COMMAND: ',L4)
CCCCC.         CALL DPWRST('XXX','BUG ')
CCCCC.       ENDIF
          ENDIF
C
CCCCC     IF(IQWNSY.EQ.'WINE')THEN
CCCCC       IRET=WinExec(ITEXT(1:IWIDTH)//' 'C,SW_SHOW)
CCCCC       GOTO9000
CCCCC     ELSEIF(IQWNSY.EQ.'SHEL')THEN
CCCCC       IHWND=GETHWNDQQ(QWIN$FRAMEWINDOW)
CCCCC       print *,'ihwnd: ',ihwnd
C
CCCCC       DO2628J=IWIDTH,1,-1
CCCCC         ITEXT(J+1:J+1)=ITEXT(J:J)
C2628       CONTINUE
CCCCC       ITEXT(IWIDTH+2:IWIDTH+2)=IQUOT1
CCCCC       ITEXT(1:1)=IQUOT1
CCCCC       IWIDTH=IWIDTH+2
C
CCCCC       IRET=ShellExecute(HWND=IHWND,
CCCCC1                        lpOperation="open"C,
CCCCC1                        lpFile=trim(ITEXT)//char(0),
CCCCC1                        lpParameters=NULL,
CCCCC1                        lpDirectory=NULL,
CCCCC1                        nShowCmd=SW_HIDE)
CCCCC1                        nShowCmd=SW_SHOWNORMAL)
CCCCC       print *,'iret = ',iret
CCCCC       GOTO9000
CCCCC     ELSEIF(IQWNSY.EQ.'CREA')THEN
CCCCC       GOTO9000
CCCCC     ENDIF
C
#endif
C
        ELSEIF(IOPSY1.EQ.'OS38' .OR. ICOMPI.EQ.'OTG ')THEN
C
C         *********************************************************
C         *  IBM PC 386 - OTG COMPILER USES THE OTG "CISSUE" CALL *
C         *********************************************************
C
          IFAIL=0
#ifdef WIN32_OTG
         CALL CISSUE(ITEXT(1:IWIDTH),IFAIL)
#endif
          IERROR='NO'
          IF(IFAIL.EQ.1)IERROR='YES'
        ELSEIF(ICOMPI.EQ.'LAHE')THEN
C
C         *****************************************************
C         *  WINDOWS USING LAHEY ROUTINE SYSTEM               *
C         *****************************************************
C
#ifdef WIN32_LAHEY
          CALL SYSTEM(ITEXT(1:IWIDTH))
#endif
        ENDIF
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C       *********************************************************
C       *  CDC - NOS/VE OPERATING SYSTEM.  USE "SCLCMD" TO PASS *
C       *  COMMANDS TO THE OPERATING SYSTEM.                    *
C       *  DATAPLOT WILL DO NO ERROR CHECKING ON THE COMMAND    *
C       *********************************************************
C
#ifdef NOS_VE
        CALL SCLCMD(ITEXT(1:IWIDTH))
#endif
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C       *********************************************************
C       *  VAX/VMS - LEFT TO IMPLEMENTOR                        *
C       *********************************************************
C
C       NOTE TO IMPLEMENTOR.  USE OF LIB$SPAWN HAS NOT BEEN TESTED,
C       BUT SHOULD WORK ACCORDING TO VAX FORTRAN GUIDE.
C
#ifdef VAX_VMS
        ISTATUS=LIB$SPAWN(ITEXT(1:IWIDTH))
#endif
      ELSE
        GOTO8000
      ENDIF
C
      GOTO9000
C
C     *********************************************************
C     *  OTHER   - LEFT TO IMPLEMENTOR                        *
C     *********************************************************
C
 8000 CONTINUE
      WRITE(ICOUT,8010)
 8010 FORMAT(1X,'THE SYSTEM COMMAND HAS NOT BEEN IMPLEMENTED AT THIS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8020)IHOST1
 8020 FORMAT(1X,'SITE FOR A ',A4,' HOST.')
      CALL DPWRST('XXX','BUG ')
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(ISUBRO.EQ.'SYS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSYS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ISTATUS,IERROR,IWIDTH
 9013   FORMAT('ISTATUS,IERROR = ',L4,2X,A4,2X,I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ITEXT(I:I),I=1,MIN(IWIDTH,80))
 9014   FORMAT('(ITEXT(I:I),I=1,IWIDTH) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GT.80)THEN
          WRITE(ICOUT,9015)(ITEXT(I:I),I=81,MIN(IWIDTH,160))
 9015     FORMAT('(ITEXT(I:I),I=1,IWIDTH) = ',120A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IWIDTH.GT.160)THEN
          WRITE(ICOUT,9016)(ITEXT(I:I),I=161,MIN(IWIDTH,95))
 9016     FORMAT('(ITEXT(I:I),I=161,IWIDTH) = ',95A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPTIME(CURRTIME,NCURRTIM,CURRDATE,NCURRDAT,
     1                  IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--PRINT OUT TIME AND DATE INFORMATION.
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--86/1
C     ORIGINAL VERSION--NOVEMBER  1980.
C     UPDATED         --MAY       1982.
C     UPDATED         --JANUARY   1986.
C     UPDATED         --SEPTEMBER 1990. TIME AND DATE FOR IBM-PC(JJF)
C     UPDATED         --APRIL     1992. FOR UNIX,VAX,CRAY,ETC. (ALAN)
C     UPDATED         --FEBRUARY  1993. ALSO BRANCH IF IMB- OTG
C     UPDATED         --FEBRUARY  1993. TIME & DATE AS OUTPUT ARGUMENT
C     UPDATED         --FEBRUARY  1993. CONDITIONAL WRITE OF TIME/DATE
C     UPDATED         --AUGUST    1993. FOR UNIX, VAX, NOS/VE (ALAN)
C     UPDATED         --AUGUST    1994. FOR UNIX, CURRDATE AND CURRTIME
C     UPDATED         --JUNE      1996. SUPPORT LAHEY COMPILER ON PC
C     UPDATED         --OCTOBER   1996. SUPPORT MS-FORTRAN COMPILER ON PC
C     UPDATED         --JULY      2018. USE FORTRAN 90 STANDARD CALL FOR
C                                       UNIX/LINUX
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*24 CURRTIME
      CHARACTER*24 CURRDATE
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
CCCCC THE FOLLOWING SECTIONS WERE ADDED          APRIL 1992 (ALAN)
CCCCC FOR HOST-DEPENDENT DECLARATIONS.           APRIL 1992 (ALAN)
CCCCC THE INSTALLER MUST COMMENT/UNCOMMENT OUT   APRIL 1992 (ALAN)
CCCCC APPROPRIATELY.                             APRIL 1992 (ALAN)
C
      CHARACTER*24 ADATE
      CHARACTER*24 ATIME
C
CCCCC FOR THE IBM/PC USING OTG COMPILER    SEPTEMBER 1990 (JJF)
#ifdef WIN32_OTG
      CHARACTER*8 TIME
      CHARACTER*8 DATE
#endif
C
CCCCC FOR THE VAX--
#ifdef VAX_VMS
      CHARACTER*23 DATETIME
      INTEGER LIB$DATE_TIME
      EXTERNAL LIB$DATE_TIME
#endif
C
CCCCC FOR NOS/VE--
#ifdef NOS_VE
      CHARACTER*10 DATE
      CHARACTER*8 TIME
#endif
C
CCCCC FOR UNIX--
#ifdef LINUX
CCCCC CHARACTER*24 fdate
#endif
C
CCCCC FOR CRAY UNICOS--
#ifdef LINUX_CRAY
      REAL DATE
      INTEGER TIME
#endif
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
CCCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1990 (JJF)
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPTI'
      ISUBN2='ME  '
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'TIME')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPTIME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,IERROR
   53   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               **********************************************
C               **  STEP 14--                               **
C               **  IF THE NEEDED SYSTEM CALL               **
C               **  EXISTS AT THIS COMPUTER INSTALLATION,   **
C               **  THEN HAVE THE DATAPLOT IMPLEMENTOR      **
C               **  ENTER THE CODE FOR SUCH A CALL.         **
C               **  IF THE NEEDED SYSTEM CALL               **
C               **  DOES NOT EXIST (THE DEFAULT) AT THIS    **
C               **  COMPUTER INSTALLATION,                  **
C               **  THEN WRITE OUT AN ERROR MESSAGE.        **
C               **********************************************
C
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'TIME')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C               ******************************************
C               **  STEP 2F--                           **
C               **  IBM/PC 386 WITH MICROSOFT COMPILER  **
C               ******************************************
C
#ifdef INTEL
        ADATE='NULL'
        ATIME='NULL'
        CALL DATE_AND_TIME(DATE=ADATE, TIME=ATIME)
CCCCC   CALL TIME(ATIME(1:11))
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,7011)
 7011     FORMAT('THE CURRENT DATE AND TIME ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7012)ADATE(1:8)
 7012     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,7013)ATIME(1:8)
 7013     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        CURRTIME(1:10)=ATIME(1:10)
        NCURRTIM=10
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C               ********************************
C               **  STEP 2C--                 **
C               **  UNIX OPERATING SYSTEM     **
C               **  USE fdate CALL.           **
C               **  CHECK THE DECLARATIONS    **
C               **  FOR SOME LINES THAT NEED  **
C               **  TO BE UNCOMMENTED.        **
C               **  MAY NEED TO CHECK THAT fdate
C               **  IS VALID ON YOUR UNIX     **
C               **  SYSTEM.                   **
C               ********************************
C
C      2018/07: USE FORTRAN 90 "DATE_AND_TIME" ROUTINE AS THIS IS
C               STANDARD FORTRAN
C
#ifdef LINUX
CCCCC   ADATE='NULL'
CCCCC   ATIME='NULL'
CCCCC   ADATE(1:24)=fdate()
C
CCCCC   IF(IFEEDB.EQ.'ON')THEN
CCCCC     WRITE(ICOUT,3011)
C3011     FORMAT('THE CURRENT DATE AND TIME ARE:')
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,3012)ADATE(1:11),ADATE(21:24)
C3012     FORMAT(A11,1X,A4)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC     WRITE(ICOUT,3013)ADATE(12:19)
C3013     FORMAT(A8)
CCCCC     CALL DPWRST('XXX','BUG ')
CCCCC   ENDIF
C
CCCCC   CURRDATE(1:11)=ADATE(1:11)
CCCCC   CURRDATE(12:15)=ADATE(21:24)
CCCCC   NCURRDAT=15
CCCCC   CURRTIME(1:8)=ADATE(12:19)
CCCCC   NCURRTIM=8
        ADATE='NULL'
        ATIME='NULL'
        CALL DATE_AND_TIME(DATE=ADATE, TIME=ATIME)
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,3011)
 3011     FORMAT('THE CURRENT DATE AND TIME ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3012)ADATE(1:8)
 3012     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,3013)ATIME(1:8)
 3013     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        CURRTIME(1:10)=ATIME(1:10)
        NCURRTIM=10
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.
     1      (ICOMPI.EQ.'OTG ' .OR. IOPSY1.EQ.'OS38'))THEN
C
CCCCC   THE FOLLOWING SECTION WAS ADDED FOR IBM-PC SEPTEMBER 1990 (JJF)
CCCCC   NOTE--TIME@() AND DATE@() ARE OTG RUN TIME LIBRARY ROUT. (JJF)
C
C               ********************************
C               **  STEP 2A--                 **
C               **  IBM/PC 386 WITH OTG COMPILER
C               ********************************
C
#ifdef WIN32_OTG
        ADATE='NULL'
        ATIME='NULL'
        ADATE(1:8)=DATE@()
        ATIME(1:8)=TIME@()
C
        IF(IFEEDB.EQ.'ON')THEN
           WRITE(ICOUT,1011)
 1011      FORMAT('THE CURRENT DATE AND TIME ARE:')
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1012)ADATE(1:8)
 1012      FORMAT(A8)
           CALL DPWRST('XXX','BUG ')
           WRITE(ICOUT,1013)ATIME(1:8)
 1013      FORMAT(A8)
           CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CURRTIME(1:8)=ATIME(1:8)
        NCURRTIM=8
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'LAHE')THEN
C
C               **************************************
C               **  STEP 2E--                       **
C               **  IBM/PC 386 WITH LAHEY COMPILER  **
C               **************************************
C
#ifdef WIN32_LAHEY
        ADATE='NULL'
        ATIME='NULL'
        CALL DATE(ADATE(1:8))
        CALL TIME(ATIME(1:11))
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,6011)
 6011     FORMAT('THE CURRENT DATE AND TIME ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6012)ADATE(1:8)
 6012     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,6013)ATIME(1:8)
 6013     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CURRTIME(1:11)=ATIME(1:11)
        NCURRTIM=11
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C
C               ********************************
C               **  STEP 2B--                 **
C               **  CYBER WITH NOS/VE         **
C               **  USE TEH DATE AND TIME     **
C               **  CALLS.  CHECK THE DECLARATIONS
C               **  FOR SOME LINES THAT NEED  **
C               **  TO BE UNCOMMENTED.        **
C               ********************************
C
#ifdef NOS_VE
        ADATE='NULL'
        ATIME='NULL'
        ADATE(1:10)=DATE()
        ATIME(1:10)=TIME()
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,2011)
 2011     FORMAT('THE CURRENT DATE AND TIME ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2012)ADATE(1:10)
 2012     FORMAT(A10)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2013)ATIME(1:8)
 2013     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CURRTIME(1:10)=ATIME(1:10)
        NCURRTIM=10
        CURRDATE(1:10)=ADATE(1:10)
        NCURRDAT=10
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'CRAY')THEN
C
C               ********************************
C               **  STEP 2C.1--               **
C               **  CRAY UNIX USES DATE AND   **
C               **  TIME CALLS.               **
C               **  CHECK THE DECLARATIONS    **
C               **  FOR SOME LINES THAT NEED  **
C               **  TO BE UNCOMMENTED.        **
C               ********************************
C
#ifdef LINUX_CRAY
        ADATE='NULL'
        ATIME='NULL'
        ATEMP=DATE()
        WRITE(ADATE(1:8),'(A8)')ATEMP
        ITEMP=TIME()
        WRITE(ATIME(1:8),'(A8)')ITEMP
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,4011)
 4011     FORMAT('THE CURRENT DATE AND TIME ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4012)ADATE(1:8)
 4012     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,4013)ATIME(1:8)
 4013     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CURRTIME(1:8)=ATIME(1:8)
        NCURRTIM=8
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C               ********************************
C               **  STEP 2D--                 **
C               **  VAX/VMS                   **
C               **  USE LIB$DATE_TIME         **
C               ********************************
C       IMPLEMENTORS NOTE.  THIS CODE HAS NOT BEEN TESTED!!!
C       IT LOOKS RIGHT ACCORDING TO VAX FORTRAN MANUAL, BUT IT
C       MAY NEED TO BE DEBUGGED.
C
#ifdef VAX_VMS
        ADATE='NULL'
        ATIME='NULL'
        ISTATUS=LIB$DATE_TIME(DATETIME)
        ISTATUS PROBABLY NEEDS TO BE DECLARED ABOVE
        ADATE(1:11)=DATETIME(1:11)
        ATIME(1:8)=DATETIME(13:20)
C
        IF(IFEEDB.EQ.'ON')THEN
          WRITE(ICOUT,5011)
 5011     FORMAT('THE CURRENT DATE AND TIME ARE:')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5012)ADATE(1:8)
 5012     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,5013)ATIME(1:8)
 5013     FORMAT(A8)
          CALL DPWRST('XXX','BUG ')
        ENDIF
C
        CURRTIME(1:8)=ATIME(1:8)
        NCURRTIM=8
        CURRDATE(1:11)=ADATE(1:11)
        NCURRDAT=11
#endif
        GOTO9000
      ELSE
C
C               ********************************
C               **  STEP 2E--                 **
C               **  UNSUPPORTED SYSTEMS.      **
C               ********************************
C
        IERROR='YES'
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPTIME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8012)
 8012   FORMAT('      THE DESIRED TIME CANNOT BE SHOWN BECAUSE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      THE REQUIRED CALL TO A SYSTEM-DEPENDENT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8015)
 8015   FORMAT('      ROUTINE TO SHOW SUCH TIME')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8016)
 8016   FORMAT('      HAS NOT BEEN IMPLEMENTED AT THIS INSTALLATION.')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)
 8021   FORMAT('      PLEASE REQUEST THE IMPLEMENTOR')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8022)
 8022   FORMAT('      TO ENTER THE CODE INTO THIS SUBROUTINE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8023)
 8023   FORMAT('      (DPTIME) TO CALL SUCH A SYSTEM-DEPENDENT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8024)
 8024   FORMAT('      ROUTINE.')
        CALL DPWRST('XXX','BUG ')
C
CCCCC   CALL XXX(ISTRIN,NCSTRI)
C
        GOTO9000
      ENDIF
C
C               ****************
C               **  STEP 90-- **
C               **  EXIT.     **
C               ****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'TIME')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPTIME--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IBUGS2,ISUBRO,IERROR
 9012   FORMAT('IBUGS2,ISUBRO,IERROR = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPEDIT(ICOM,IANSLC,IWIDTH,ISUBRO,IERROR)
C
C     PURPOSE--EDIT A FILE
C     ORIGINAL VERSION--JULY       1992
C
      INCLUDE 'DPCOPA.INC'
C
      CHARACTER*4 ICOM
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*4 ISOURC
CCCCC CHARACTER*80 IEDINA
      CHARACTER (LEN=MAXFNC) :: IEDINA
CCCCC THE FOLLOWING LINE WAS ADDED    JULY 1993
      CHARACTER*4 IDATAP
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IF(ISUBRO.EQ.'EDIT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPEDIT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)ICOM,ISUBRO,IWIDTH
   53   FORMAT('ICOM,ISUBRO,IWIDTH = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        DO55I=1,IWIDTH
          WRITE(ICOUT,57)I,IANSLC(I)
   57     FORMAT('I,IANSLC(I) = ',I6,2X,A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
C
      IERROR='NO'
      IF(ICOM.EQ.'FED')THEN
        ISTART=5
      ELSEIF(ICOM.EQ.'EDIT')THEN
        ISTART=6
      ELSE
        ISTART=6
      ENDIF
C
      IEDINA=' '
      J=0
      IF(ISTART.LE.IWIDTH)THEN
         DO1000I=ISTART,IWIDTH
            J=J+1
            IEDINA(J:J)=IANSLC(I)(1:1)
 1000    CONTINUE
      ENDIF
      NCSTR=J
C
      IF(NCSTR.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
 1001   FORMAT('NO FILE NAME GIVEN ON EDIT COMMAND')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(ISUBRO.EQ.'EDIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1010)ISTART,NCSTR
 1010   FORMAT('FROM DPEDIT: ISTART,NCSTR = ',2I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1012)IEDINA(1:MIN(80,NCSTR))
 1012   FORMAT('IEDINA(1:80) = ',A80)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C     ISOURC IS THE SOURCE OF THE NAME OF THE FILE TO BE EDITED.
C     IF THE FILE NAME IS IN C:\FED\FEDARG.TEX,
C     THEN ISOURC = 'FILE'
C     IF THE FILE NAME IS PASSED ON VIA A SUBROUTINE ARGUMENT,
C     THEN ISOURC = 'SUBR'
C
      ISOURC='SUBR'
C
C     IDATAP IS THE DATAPLOT-CONNECTION SWITCH.
C     IF FED IS CONNECTED TO DATAPLOT,
C     THEN IDATAP = 'ON'
C     IF FED IS STAND-ALONE,
C     THEN IDATAP = 'OFF'
C
CCCCC THE FOLLOWING LINE WAS ADDED    JULY 1993
      IDATAP='ON'
C
CCCCC THE FOLLOWING LINE WAS CHANGED    JULY 1993
CCCCC CALL EDMAI2(ISOURC,IEDINA)
      CALL EDMAI2(ISOURC,IEDINA,IDATAP)
C
      IF(ISUBRO.EQ.'EDIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END       OF DPEDIT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
 9000 CONTINUE
      RETURN
      END
      SUBROUTINE DPWRST(ISUBN0,TYPE)
C
C     PURPOSE--WRITE OUT THE NCOUT ELEMENTS OF THE
C              CHARACTER*240 STRING ICOUT(.:.)
C              TO A GENERAL GRAPHICS DEVICE.
C              THE VALUE OF THE VARIABLE    NCOUT
C     ICOUT AND NCOUT RESIDE IN COMMON   /TEXTOU/
C     INPUT ARGUMENTS--ICOUT (IN COMMON)
C     ISUBN0 = 6-CHARACTER NAME OF SUBROUTINE WHICH CALLED DPWRST.
C              (AND THEREBY HAVE WALKBACK INFORMATION).
C     TYPE--4 CHARACTER DEFINITION OF TYPE OF INPUT
C              1) TEXT
C              2) BUG
C              3) ERRO
C              4) LIST
C              5) HELP
C              6) WRIT (= ALWAYS WRITE EVEN IF FEEDBACK OFF)
C              7) ...
C     OUTPUT ARGUMENTS--NCOUT (DETERMINED HEREIN)
C     NOTE--ALL DATAPLOT TEXT OUTPUT IS FUNNELED THROUGH
C           THIS ONE SUBROUTINE.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 2089
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--93.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1993.
C     UPDATED            --SEPTEMBER 1993. ALWAYS WRITE IF TYPE = WRIT
C     UPDATED            --SEPTEMBER 1993. OMIT IBUGG4 AS BUG SWITCH
C     UPDATED            --FEBRUARY  2005. FOR RTF OUTPUT, SUPPRESS
C                                          LEADING SPACE.  NEED TO
C                                          ADD DPCOSU.INC.
C     UPDATED            --JANUARY   2006. ALLOW CAPTURE OUTPUT TO
C                                          BE OPTIONALLY WRITTEN TO
C                                          BOTH SCREEN AND CAPTURE
C                                          FILE
C     UPDATED            --JUNE      2006. MAKE LEADING SPACE
C                                          USER SETTABLE (SET
C                                          FORTRAN FORMAT CONTROL)
C     UPDATED            --APRIL     2009. SET MAXIMUM LINE WIDTH IN
C                                          MAXCLN (IF SET ABOVE 240,
C                                          THEN NEED TO CHANGE
C                                          "CHARACTER*240 ICOUT" IN
C                                          ALL ROUTINES
C     UPDATED            --SEPTEMBER 2010. ALLOW PROMPT AFTER USER
C                                          SPECIFIED NUMBER OF LINES
C     UPDATED            --DECEMBER  2015. SUPPORT CAPTURE SPLIT OPTION
C                                          (SECOND CAPTURE FILE)
C     UPDATED            --AUGUST    2016. TRAP ERROR ON WRITE
C     UPDATED            --SEPTEMBER 2018. SET WRITE FEEDBACK
C     UPDATED            --APRIL     2020. INCREASE MAXIMUM LINE
C                                          LENGTH (MAKE SETTABLE IN
C                                          DPCOP2.INC)
C
C-----NON-COMMON VARIABLES (GRAPHICS)----------------------------------
C
CCCCC MUST EVENTUALLY CHANGE THE FOLLOWING LINE FORM *3 TO *?
      CHARACTER (LEN=*) :: ISUBN0
      CHARACTER (LEN=4) :: TYPE
C
      CHARACTER*4 IBRANC
      CHARACTER*1 IBASLC
      CHARACTER*1 IJUNK
      CHARACTER*2 ISTR
      CHARACTER*20 IFORMT
C
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
C
      INTEGER ILNCNT
      COMMON/LINNUM/ILNCNT
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOTR.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
      MAXCLN=MAXOUT
C     MAY,1988.
CCCCC NCOUT=ABS(NCOUT)  JJF
C
CCCCC THE FOLLOWING LINE WAS CHANGED    SEPTEMBER 1993
CCCCC IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
      IF(ISUBG4.EQ.'WRST')THEN
         WRITE(IPR,999)
  999    FORMAT(1X)
         WRITE(IPR,51)
   51    FORMAT('***** AT THE BEGINNING OF DPWRST--')
         WRITE(IPR,52)ISUBN0
   52    FORMAT('THE CALLING ROUTINE (ISUBN0) WAS ',A3)
         WRITE(IPR,53)TYPE,IFORFM,IFEEDB,IHOST1
   53    FORMAT('TYPE,IFORFM,IFEEDB,IHOST1 = ',3(A4,1X),A4)
         WRITE(IPR,56)NCOUT,ILOUT
   56    FORMAT('NCOUT,ILOUT = ',2I8)
         WRITE(IPR,61)
   61    FORMAT('          123456789.123456789.123456789.123456')
         WRITE(IPR,63)ICOUT(1:240)
   63    FORMAT('ICOUT = ',A240)
      ENDIF
C
C               *********************************************
C               **  STEP 11--                              **
C               **  IF CALLED FOR,                         **
C               **  CARRY OUT ANY SUB-STRING TRANSLATIONS  **
C               *********************************************
C
CCCCC IF(NUMTRA.GE.1)THEN
CCCCC   CALL GRTRST(ICOUT,NCOUT,ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
CCCCC1              IBUGG4,ISUBG4,IERRG4)
C
C               **************************************************
C               **  STEP 12--                                   **
C               **  DETERMINE THE LENGTH OF THE STRING          **
C               **  (BY IGNORING BLANK CHARACTERS AT THE END)   **
C               **************************************************
C
      NCOUT=1
      DO1200I=MAXCLN,1,-1
        IF(ICOUT(I:I).NE.' ')THEN
          NCOUT=I
          GOTO1290
        ENDIF
 1200 CONTINUE
 1290 CONTINUE
C
C               ****************************
C               **  STEP 13--             **
C               **  WRITE OUT THE STRING  **
C               ****************************
C
CCCCC IOUNIT=6
CCCCC NOTE--IPR BELOW IS USUALLY 6
CCCCC       BUT COULD BE SET TO 7 IN TCSHME.FOR WITHIN TCDRIV.FOR
      IOUNIT=IPR
      IFORMT=' '
C
      IBRANC='NOWR'
CCCCC THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
CCCCC TO ALLOW ALWAYS-WRITING FOR L, WRITE, HELP, STAT SEPTEMBER 1993
CCCCC APRIL 2009: MODIFY ORDER (I.E., DO SPECIAL CASES FIRST)
CCCCC IF(IFEEDB.EQ.'ON')THEN
      IF(IFEEDB.EQ.'ON'.OR.TYPE.EQ.'WRIT'.OR.IFEEWR.EQ.'ON')THEN
         IF(1.LE.NCOUT.AND.NCOUT.LE.MAXCLN)THEN
            IBRANC='WRIT'
            IF(ICAPTY.EQ.'RTF ')THEN
               IFORMT='(    A1)'
               WRITE(IFORMT(2:5),'(I4)')NCOUT
               CALL DPCONA(92,IBASLC)
               WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)
     1               (ICOUT(I:I),I=1,NCOUT)
               IF(IRTFMD.EQ.'VERB')THEN
                 WRITE(IOUNIT,1319,IOSTAT=IOS,ERR=8800)IBASLC
 1319            FORMAT(A1,'line')
               ENDIF
            ELSEIF(ICAPSC.EQ.'ON  ' .AND. IOUNIT.NE.6 .AND.
     1             ICAPTY.NE.'HTML' .AND. ICAPTY.NE.'LATE')THEN
               IFORMT='(1X,    A1)'
               WRITE(IFORMT(5:8),'(I4)')NCOUT
               WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)
     1               (ICOUT(I:I),I=1,NCOUT)
               WRITE(6,IFORMT)(ICOUT(I:I),I=1,NCOUT)
               IF(ICAPSP.EQ.'ON')THEN
                 WRITE(ICPNU2,IFORMT)(ICOUT(I:I),I=1,NCOUT)
               ENDIF
#ifdef VAX_VMS
            ELSEIF((IHOST1.EQ.'VAX'.AND.ICOUT(1:1).EQ.'$') .OR.
     1        (IFORFM.EQ.'OFF'))THEN
               IF(IOUTLN.EQ.'ON')THEN
                 ILNCNT=ILNCNT+1
                 IFORMT='(I ,A2,240A1)'
                 IF(ILNCNT.GE.100000000)THEN
                   IDIGIT=9
                 ELSEIF(ILNCNT.GE.10000000)THEN
                   IDIGIT=8
                 ELSEIF(ILNCNT.GE.1000000)THEN
                   IDIGIT=7
                 ELSEIF(ILNCNT.GE.100000)THEN
                   IDIGIT=6
                 ELSEIF(ILNCNT.GE.10000)THEN
                   IDIGIT=5
                 ELSEIF(ILNCNT.GE.1000)THEN
                   IDIGIT=4
                 ELSEIF(ILNCNT.GE.100)THEN
                   IDIGIT=3
                 ELSEIF(ILNCNT.GE.10)THEN
                   IDIGIT=2
                 ELSE
                   IDIGIT=1
                 ENDIF
                 WRITE(IFORMT(3:3),'(I1)')IDIGIT
                 ISTR=': '
                 WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)
     1                 ILNCNT,ISTR,(ICOUT(I:I),I=1,NCOUT)
               ELSE
                 WRITE(IOUNIT,1311,IOSTAT=IOS,ERR=8800)
     1                 (ICOUT(I:I),I=1,NCOUT)
 1311            FORMAT(240A1)
               ENDIF
#endif
            ELSE
               IF(IOUTLN.EQ.'ON')THEN
                 ILNCNT=ILNCNT+1
                 IFORMT='(1X,I ,A2,240A1)'
                 IF(ILNCNT.GE.100000000)THEN
                   IDIGIT=9
                 ELSEIF(ILNCNT.GE.10000000)THEN
                   IDIGIT=8
                 ELSEIF(ILNCNT.GE.1000000)THEN
                   IDIGIT=7
                 ELSEIF(ILNCNT.GE.100000)THEN
                   IDIGIT=6
                 ELSEIF(ILNCNT.GE.10000)THEN
                   IDIGIT=5
                 ELSEIF(ILNCNT.GE.1000)THEN
                   IDIGIT=4
                 ELSEIF(ILNCNT.GE.100)THEN
                   IDIGIT=3
                 ELSEIF(ILNCNT.GE.10)THEN
                   IDIGIT=2
                 ELSE
                   IDIGIT=1
                 ENDIF
                 WRITE(IFORMT(6:6),'(I1)')IDIGIT
                 ISTR=': '
                 WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)
     1                 ILNCNT,ISTR,(ICOUT(I:I),I=1,NCOUT)
               ELSE
                 IFORMT='(1X,    A1)'
                 WRITE(IFORMT(5:8),'(I4)')NCOUT
                 WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)
     1                 (ICOUT(I:I),I=1,NCOUT)
C1312            FORMAT(1X,240A1)
              ENDIF
            ENDIF
         ENDIF
      ENDIF
C
C               ******************************
C               **  STEP 14--               **
C               **  RESET STRING VARIABLES  **
C               ******************************
C
      ICOUT=' '
      NCOUT=(-999)
      ILOUT=(-999)
      GOTO9000
C
 8800 CONTINUE
      WRITE(6,8801)IOUNIT
 8801 FORMAT('****** ERROR TRYING TO WRITE TO UNIT ',I8)
      WRITE(6,8802)IOS
 8802 FORMAT('       STATUS NUMBER = ',I8)
      WRITE(6,8803)
 8803 FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',
     1       'THAT DOES NOT HAVE WRITE PERMISSION.')
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
C
C     CHECK IF PROMPT REQUIRED
C
      IF(IGUIFL.EQ.'OFF' .AND. IPAULI.GT.0 .AND. IPR.EQ.6)THEN
        IPAUCN=IPAUCN+1
        IF(IPAUCN.GE.IPAULI)THEN
          WRITE(IPR,1101)
 1101     FORMAT('?:')
          READ(IRD,1105)IJUNK
 1105     FORMAT(A1)
          IPAUCN=0
          IF(IJUNK.EQ.'0')IPAULI=0
        ENDIF
      ENDIF
C
CCCCC THE FOLLOWING LINE WAS CHANGED    SEPTEMBER 1993
CCCCC IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
      IF(ISUBG4.EQ.'WRST')THEN
         WRITE(IPR,999)
         WRITE(IPR,9011)
 9011    FORMAT('***** AT THE END       OF DPWRST--')
         IF(NCOUT.GT.0)THEN
           DO9035I=1,NCOUT
             CALL DPCOAN(ICOUT(I:I),IASCNE)
             WRITE(IPR,9036)I,ICOUT(I:I),IASCNE
 9036        FORMAT(1H ,'I,ICOUT(I:I),IASCNE = ',I8,2X,A1,I8)
 9035      CONTINUE
         ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE GRWRST(ICSTR,NCSTR2,ISUBN0)
CCCCC SUBROUTINE GRWRST(ICSTR,NCSTR,ISUBN0)
C
C     PURPOSE--WRITE OUT THE NCSTR ELEMENTS OF THE
C              CHARACTER*130 STRING ICSTR(.:.)
C              OUT TO A GENERAL GRAPHICS DEVICE.
C              THE VALUE OF THE VARIABLE    NCSTR
C              IS THE NUMBER OF ELEMENTS IN ICSTR(.:.)
C              TO BE WRITTEN OUT.
C     NOTE--ISUBN0 = NAME OF SUBROUTINE WHICH CALLED GRWRST.
C                    (AND THEREBY HAVE WALKBACK INFORMATION).
C
C     WRITTEN BY--JAMES J. FILLIBEN
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-921-3651
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C     VERSION NUMBER--83.6
C     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
C     UPDATED         --MAY 1988.
C                       THE POSTSCRIPT DEVICE REQUIRES A "%!" TO BE FIRST
C                       16 BYTES, QUIC REQUIRES "^PY.." COMMAND TO START
C                       IN COLUMN 1.  ALSO, SOME DEVICES SUCH AS DICOMED,
C                       SHOUULD NOT CONTAIN LEADING SPACES IN THE FILE.
C                       FOR THESE CASE, SEND "NCSTR" AS NEGATIVE.  IF NCSTR
C                       IS NEGATIVE, THE LEADING SPACE FOR PRINT CONTROL
C                       WILL NOT BE ADDED.
C     UPDATED         --JANUARY  1989. SUN (BY BILL ANDERSON)
C     UPDATED         --JANUARY  1989. POSTSCRIPT (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. CGM (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. QMS QUIC (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. CALCOMP (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1989. ZETA (BY ALAN HECKERT)
C     UPDATED         --JANUARY  1994. ALPHA: 1X IN FORMAT (JJF)
C     UPDATED         --AUGUST   2016. TRAP WRITE ERRORS
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
      CHARACTER*130 ICSTR
C
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 IBRANC
C
      SAVE NUMERR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOTR.INC'
C
      COMMON/GRAERR/IGFLAG
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
      DATA NUMERR /0/
C
C-----START POINT-----------------------------------------------------
C
      IERRG4='NO'
C  MAY,1988.
      NCSTR=ABS(NCSTR2)
      IGFLAG=0
C
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRWRST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISUBN0,IBUGG3,ISUBG4,IERRG4
   52   FORMAT('ISUBN0 (CALLING SUBROUTINE),IBUGG3,ISUBG4,IERRG4 = ',
     1         3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IGUNIT,IMANUF,NCSTR,NUMTRA
   53   FORMAT('IGUNIT,IMANUF,NCSTR,NUMTRA = ',I8,2X,A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO55I=1,NCSTR
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,56)I,ICSTR(I:I),IASCNE
   56       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
        IF(NUMTRA.GT.0)THEN
          DO62I=1,NUMTRA
            WRITE(ICOUT,63)I,NCTRA1(I),ICTRA1(I),NCTRA2(I),ICTRA2(I)
   63       FORMAT('I,NCTRA1(I),ICTRA1(I),NCTRA2(I),ICTRA2(I) = ',
     1             I8,I8,2X,A30,I8,2X,A30)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
      ENDIF
C
C               *********************************************
C               **  STEP 11--                              **
C               **  IF CALLED FOR,                         **
C               **  CARRY OUT ANY SUB-STRING TRANSLATIONS  **
C               *********************************************
C
      IF(NUMTRA.GE.1)
     1CALL GRTRST(ICSTR,NCSTR,
     1ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
     1IBUGG4,ISUBG4,IERRG4)
C
C               ****************************
C               **  STEP 21--             **
C               **  WRITE OUT THE STRING  **
C               ****************************
C
      IBRANC='NOWR'
      IF(1.LE.NCSTR.AND.NCSTR.LE.130)THEN
        IBRANC='WRIT'
        IF((IHOST1.EQ.'VAX'.AND.ICSTR(1:1).EQ.'$') .OR.
     1     NCSTR2.LT.0)THEN
          WRITE(IGUNIT,2111,IOSTAT=IOS,ERR=8800)(ICSTR(I:I),I=1,NCSTR)
 2111     FORMAT(240A1)
CCCCC     NOTE--THE FOLLOWING FORMAT SHOULD BE USED  JANUARY 1994
CCCCC           INSTEAD ON SOME COMPUTERS            JANUARY 1994
CCCCC           (E.G., DEC ALPHA COMPUTERS)          JANUARY 1994
CCCCC           WHICH NEED A LEADING SPACE BEFORE    JANUARY 1994
CCCCC           ALL FORTRAN WRITE STATEMENTS--EVEN   JANUARY 1994
CCCCC           WRITE STATEMENTS WITH GRAPHICS       JANUARY 1994
CCCCC           DIRECTIVES.    JJF                   JANUARY 1994
C2111           FORMAT(1X,240A1)
        ELSE
          WRITE(IGUNIT,2121,IOSTAT=IOS,ERR=8800)(ICSTR(I:I),I=1,NCSTR)
 2121     FORMAT(240A1)
CCCCC     NOTE--THE FOLLOWING FORMAT SHOULD BE USED  JANUARY 1994
CCCCC           INSTEAD ON SOME COMPUTERS            JANUARY 1994
CCCCC           (E.G., DEC ALPHA COMPUTERS)          JANUARY 1994
CCCCC           WHICH NEED A LEADING SPACE BEFORE    JANUARY 1994
CCCCC           ALL FORTRAN WRITE STATEMENTS--EVEN   JANUARY 1994
CCCCC           WRITE STATEMENTS WITH GRAPHICS       JANUARY 1994
CCCCC           DIRECTIVES.    JJF                   JANUARY 1994
C2121           FORMAT(1X,240A1)
        ENDIF
      ENDIF
C
C               *******************************************
C               **  STEP 31--                            **
C               **  IF CALLED FOR,                       **
C               **  CALL THE LINE TRANSLATOR SUBROUTINE  **
C               **  WHICH CONVERTS A TEKTRONIX LINE      **
C               **  INTO A SET OF CALLS FOR              **
C               **  ANOTHER GRAPHICS DEVICE              **
C               **  (SEE SUBROUTINE GRTRTK).             **
C               *******************************************
C
CCCCC IF(ITRANS.EQ.'ON')CALL GRTRTK(ICSTR,NCSTR)
C
      GOTO9000
 8800 CONTINUE
      IF(NUMERR.LE.1000)NUMERR=NUMERR+1
      IF(NUMERR.LE.10)THEN
        WRITE(ICOUT,8801)IGUNIT
 8801   FORMAT('****** ERROR TRYING TO WRITE TO UNIT ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8802)IOS
 8802   FORMAT('       STATUS NUMBER = ',I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8803)
 8803   FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',
     1         'THAT DOES NOT HAVE WRITE PERMISSION.')
        CALL DPWRST('XXX','BUG ')
        IGFLAG=1
      ENDIF
      GOTO9000
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF GRWRST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IGUNIT,NCSTR,NUMTRA,IMANUF,IBRANC
 9013   FORMAT('IGUNIT,NCSTR,NUMTRA,IMANUF,IBRANC = ',3I8,2X,A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO9015I=1,NCSTR
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,9016)I,ICSTR(I:I),IASCNE
 9016       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
        IF(NUMTRA.GT.0)THEN
          DO9022I=1,NUMTRA
            WRITE(ICOUT,9023)I,ICTRA1(I),NCTRA1(I),NCTRA2(I),ICTRA2(I)
 9023       FORMAT('I,ICTRA1(I),NCTRA1(I),NCTRA2(I),ICTRA2(I) = ',
     1             I8,2X,A30,I8,2X,A30,I8)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSLEE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,
     1                  IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CAUSE DATAPLOT TO PAUSE FOR <X> SECONDS.  THIS COMMAND
C              IS SITE AND HOST DEPENDENT.  THE MAIN USAGE IS TO ALLOW
C              DELAY TO BE INSERTED IN MACROS AFTER A PLOT TO AVOID
C              THE HASSLE OF ENTERING A CARRIAGE RETURN AS NEEDED BY
C              THE PAUSE COMMAND.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 CENTER FOR APPLIED MATHEMATICS
C                 NATIONAL BUREAU OF STANDARDS
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--97.8
C     ORIGINAL VERSION--AUGUST     1997.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
#ifdef INTEL
      USE MSFLIB
#endif
C
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
C
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IARGT(*)
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IFOUND='YES'
      IERROR='NO'
C
      J2=0
C
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'SLEE')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IFOUND,IERROR
   53   FORMAT('IBUGD2,ISUBRO,IFOUND,IERROR = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)IHARG(1),IHARG2(1),IARGT(1),IARG(1),ARG(1)
   55   FORMAT('IHARG(1),IHARG2(1),IARGT(1),IARG(1),ARG(1) = ',
     1         2A4,2X,A4,2X,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  DETERMINE THE TIME VARIABLE                    **
C               *****************************************************
C
      IF(NUMARG.LE.0)THEN
        ASLEEP=5.0
      ELSE
        IF(IARGT(1).EQ.'NUMB')THEN
          ASLEEP=ARG(1)
        ELSE
          ASLEEP=5.0
        ENDIF
      ENDIF
C
C               ********************************
C               **  STEP 2--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       ****************************************************
C       *  IBM/PC 386 - MICROSOFT WINDOWS 95/NT COMPILER   *
C       ****************************************************
C
#ifdef INTEL
        CALL SLEEPQQ(INT(ASLEEP*1000.))
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C       *********************************************************
C       *  UNIX    - LEFT TO IMPLEMENTOR                        *
C       *  CODE ADDED MARCH, 1990 BY ALAN HECKERT.  USE THE     *
C       *  LIBRARY ROUTINE "SLEEem".  NOTE THAT UNIX CALLS ARE  *
C       *  CASE SENSITIVE, SO LEAVE CODE IN LOWER CASE.         *
C       *********************************************************
C
#ifdef LINUX
        CALL SLEEP(INT(ASLEEP+0.5))
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C       *********************************************************
C       *  CDC - NOS/VE LEFT TO IMPLEMENTOR                     *
C       *********************************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2111)
 2111   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2112)
 2112   FORMAT('      COMMAND NOT IMPLEMENTED FOR NOS/VE')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C       **************************************
C       *  VAX/VMS - LEFT TO IMPLEMENTOR     *
C       **************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2211)
 2211   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)
 2212   FORMAT('      COMMAND NOT IMPLEMENTED FOR VAX/VMS')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG ')THEN
C
C       **********************************
C       *  IBM/PC 386 - OTG COMPILER     *
C       **********************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
 2411   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      COMMAND NOT IMPLEMENTED FOR IBM OTG VERSION')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSE
C
C     *********************************************************
C     *  OTHER   - LEFT TO IMPLEMENTOR                        *
C     *********************************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      THE SLEEP COMMAND HAS NOT YET BEEN IMPLEMENTED ',
     1         ' FOR THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      COMPUTER/MODEL/OP-SYS/COMPILER/SITE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)IHOST1,IHMOD1,IOPSY1,ICOMPI,ISITE
 8021   FORMAT('IHOST1,IHMOD1,IOPSY1,ICOMPI,ISITE  = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'SLEE')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IFOUND,IERROR
 9032   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPSLE2(ASLEEP,IBUGD2,ISUBRO,IERROR)
C
C     PURPOSE--CAUSE DATAPLOT TO PAUSE FOR <X> SECONDS.  THIS COMMAND
C              IS SITE AND HOST DEPENDENT.  THE MAIN USAGE IS TO ALLOW
C              DELAY TO BE INSERTED IN MACROS AFTER A PLOT TO AVOID
C              THE HASSLE OF ENTERING A CARRIAGE RETURN AS NEEDED BY
C              THE PAUSE COMMAND.
C
C              THIS IS A MODIFIED VERSION OF THE DPSLEE ROUTINE.  THE
C              DPSLEE ROUTINE IMPLEMENTS THE "SLEEP" COMMAND.  THIS
C              ROUTINE IS USED TO IMPLEMENT A SLEEP FUNCTION INDEPENDENT
C              OF THE SLEEP COMMAND (I.E., USED INTERNALLY BY DATAPLOT).
C              FOR THIS ROUTINE, THE SLEEP PARAMETER HAS ALREADY BEEN
C              DETERMINED.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 WASHINGTON, D. C. 20234
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
#ifdef INTEL
      USE MSFLIB
#endif
C
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'SLE2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPSLE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGD2,ISUBRO,ASLEEP
   81   FORMAT('IBUGD2,ISUBRO,ASLEEP = ',2(A4,2X),G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 2--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       ****************************************************
C       *  IBM/PC 386 - MICROSOFT WINDOWS 95/NT COMPILER   *
C       ****************************************************
C
#ifdef INTEL
        CALL SLEEPQQ(INT(ASLEEP*1000.))
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C       *********************************************************
C       *  UNIX    - LEFT TO IMPLEMENTOR                        *
C       *  CODE ADDED MARCH, 1990 BY ALAN HECKERT.  USE THE     *
C       *  LIBRARY ROUTINE "SLEEem".  NOTE THAT UNIX CALLS ARE  *
C       *  CASE SENSITIVE, SO LEAVE CODE IN LOWER CASE.         *
C       *********************************************************
C
#ifdef LINUX
        CALL SLEEP(INT(ASLEEP+0.5))
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C       *********************************************************
C       *  CDC - NOS/VE LEFT TO IMPLEMENTOR                     *
C       *********************************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2111)
 2111   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2112)
 2112   FORMAT('      COMMAND NOT IMPLEMENTED FOR NOS/VE')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C       **************************************
C       *  VAX/VMS - LEFT TO IMPLEMENTOR     *
C       **************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2211)
 2211   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)
 2212   FORMAT('      COMMAND NOT IMPLEMENTED FOR VAX/VMS')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG ')THEN
C
C       **********************************
C       *  IBM/PC 386 - OTG COMPILER     *
C       **********************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
 2411   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      COMMAND NOT IMPLEMENTED FOR IBM OTG VERSION')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSE
C
C     *********************************************************
C     *  OTHER   - LEFT TO IMPLEMENTOR                        *
C     *********************************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      THE SLEEP COMMAND HAS NOT YET BEEN IMPLEMENTED ',
     1         ' FOR THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      COMPUTER/MODEL/OP-SYS/COMPILER/SITE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)IHOST1
 8021   FORMAT(' HOST     = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8022)IHMOD1
 8022   FORMAT(' MODEL    = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8023)IOPSY1
 8023   FORMAT(' OP-SYS   = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8024)ICOMPI
 8024   FORMAT(' COMPILER = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8025)ISITE
 8025   FORMAT(' SITE     = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'SLE2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSLE2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9032)IERROR
 9032   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPFLSH(IUNIT,IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--ENTER A "FLUSH" COMMAND TO CLEAR STANDARD OUTPUT.
C              NEEDED BY FRONT-END TO GET RID OF EXPECT CODE.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION AND TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--98.1
C     ORIGINAL VERSION--JANUARY    1998.
C
C-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
C
#ifdef INTEL
      USE PORTLIB
#endif
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IFOUND='NO'
      IERROR='NO'
C
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'FLSH')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPFLSH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGD2,ISUBRO
   81   FORMAT('IBUGD2,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 2--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       *********************************************
C       *  IBM/PC 386 - MS-FORTRAN COMPILER         *
C       *********************************************
C
#ifdef INTEL
        CALL FLUSH(IUNIT)
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C       ************************************
C       *  UNIX/LINUX - gfortran compiler  *
C       ************************************
C
#ifdef LINUX
        CALL FLUSH(IUNIT)
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C       *************************************
C       *  CDC - NOS/VE OPERATING SYSTEM.  **
C       *************************************
C
        GOTO9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C       **************************************
C       *  VAX/VMS - LEFT TO IMPLEMENTOR     *
C       **************************************
C
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.
     1      (IOPSY1.EQ.'OS38' .OR. ICOMPI.EQ.'OTG'))THEN
C
C       **********************************
C       *  IBM/PC 386 - OTG COMPILER     *
C       **********************************
C
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'LAHE')THEN
C
C       *********************************************
C       *  IBM/PC 386 - LAHEY      COMPILER         *
C       *********************************************
C
#ifdef WIN32_LAHEY
        CALL FLUSH(IUNIT)
#endif
        GOTO9000
      ELSE
C
C
C       **************************************
C       *  OTHER   - LEFT TO IMPLEMENTOR     *
C       **************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPFLSH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8012)
 8012   FORMAT('      THE INTERFACE TO FLUSH OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      HAS NOT YET BEEN IMPLEMENTED FOR THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      COMPUTER/MODEL/OP-SYS/COMPILER/SITE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)IHOST1
 8021   FORMAT(' HOST     = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8022)IHMOD1
 8022   FORMAT(' MODEL    = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8023)IOPSY1
 8023   FORMAT(' OP-SYS   = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8024)ICOMPI
 8024   FORMAT(' COMPILER = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8025)ISITE
 8025   FORMAT(' SITE     = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'FLSH')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPFLSH--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)IFOUND,IERROR
 9013   FORMAT('IFOUND,IERROR = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      REAL FUNCTION RANLP(IRESET)
CCCCC REAL FUNCTION RANLP(IDUM,IRESET)
C
      INTEGER TABPTJ
      INTEGER ITABLE(98)
      INTEGER ITABSV(98)
C   LEWIS-PAYNE GFSR UNIFORM RANDOM NUMBER GENERATOR
C
C  T. G. LEWIS & W. H. PAYNE (1973) GENERALIZED FEEDBACK SHIFT REGISTER
C   PSEUDORANDOM NUMBERS, JOURNAL OF THE ACM, VOLUME 20, PP. 456-468
C
C  USES PRIMITIVE TRINOMIAL WITH P=98 AND Q=27
C
C  ARGUMENT IS A DUMMY AND NEVER USED
C
C  THE USE OF THE "IEOR" FUNCTION IS NOT STANDARD IN THE
C  FORTRAN 77 (IS STANDARD FOR FORTRAN 90), SO PLACE IN DP1.FOR
C  FILE IN CASE IT NEEDS MODIFICATION.
C
C  THIS IS FORTRAN 77 IMPLEMENTATION OF A FORTRAN 95 CODE
C  FOUND IN MONAHAN (2001), "NUMERICAL METHODS OF STATISTICS",
C  CAMBRIDGE UNIVERSITY PRESS.
C
CCCCC INTEGER IDUM
C  PARAMETERS OF TAUSWORTHE SEQUENCE
      INTEGER P
      INTEGER Q 
      INTEGER K
      REAL    FN
C
      SAVE TABPTJ
CCCCC SAVE TABPSV
      SAVE ITABLE
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
      DATA P /98/
      DATA Q /27/
      DATA TABPTJ /0/
C  FN = 2**31
      DATA FN / 2147483648. /
C
      DATA ITABLE /346256726,591599773,1943131421,1173234223,
     & 1776849374,1119416586,172236044,985756773,1554281477,
     & 1503137291,650397619,1618395655,639939067,1448259547,
     & 1046853128,659170036,1034934222,279813371,326930100,
     & 367002640,648480182,1909733845,618563844,845531267,
     & 292262469,299413367,2139821356,1005803337,390139420,
     & 1161028423,2034360736,334070487,565633315,124796253,
     & 2104169336,2009751844,1999687407,83223028,1591328966,
     & 646701838,1935362333,795013136,680356918,1771711842,
     & 1324935502,1869840308,356745634,1061920662,614951490,
     & 261876461,703987800,797463948,178239686,1641708282,
     & 1539695556,1334926802,940547749,1957646566,1878491364,
     & 2033904942,1711106005,2138438575,647734238,1555990485,
     & 1210108489,1793192836,1819829578,751843064,345621400,
     & 575445974,1640918761,1379191461,1617832156,542966103,
     & 1305854952,1476721677,1466811698,1842260101,1666639833,
     & 217007402,685228354,902087789,32432242,789712994,702791444,
     & 1081111755,1572116899,321512624,644413114,863989644,
     & 1348681739,84379947,1955819746,941474606,984690559,
     & 1794209263,1704575856,1253913135 /
C
      DATA ITABSV /346256726,591599773,1943131421,1173234223,
     & 1776849374,1119416586,172236044,985756773,1554281477,
     & 1503137291,650397619,1618395655,639939067,1448259547,
     & 1046853128,659170036,1034934222,279813371,326930100,
     & 367002640,648480182,1909733845,618563844,845531267,
     & 292262469,299413367,2139821356,1005803337,390139420,
     & 1161028423,2034360736,334070487,565633315,124796253,
     & 2104169336,2009751844,1999687407,83223028,1591328966,
     & 646701838,1935362333,795013136,680356918,1771711842,
     & 1324935502,1869840308,356745634,1061920662,614951490,
     & 261876461,703987800,797463948,178239686,1641708282,
     & 1539695556,1334926802,940547749,1957646566,1878491364,
     & 2033904942,1711106005,2138438575,647734238,1555990485,
     & 1210108489,1793192836,1819829578,751843064,345621400,
     & 575445974,1640918761,1379191461,1617832156,542966103,
     & 1305854952,1476721677,1466811698,1842260101,1666639833,
     & 217007402,685228354,902087789,32432242,789712994,702791444,
     & 1081111755,1572116899,321512624,644413114,863989644,
     & 1348681739,84379947,1955819746,941474606,984690559,
     & 1794209263,1704575856,1253913135 /
C
C     START EXECUTABLE CODE
C
C     RESET INITIAL TABLE IF REQUESTED
C
      IF(IRESET.EQ.1)THEN
        DO100LL=1,P
          ITABLE(LL)=ITABSV(LL)
  100   CONTINUE
        TABPTJ = 0
      ENDIF
C
C     UPDATE POINTER
C
      TABPTJ = TABPTJ + 1
      IF(TABPTJ.GT.P) TABPTJ = 1
C
C     UPDATE DELAY POINTER
C
      K = TABPTJ + Q
      IF(K.GT.P) K = K - P
C
C     EXCLUSIVE OR OF TWO TABLE ENTRIES AND REPLACE WITH NEW ONE
C
      ITABLE(TABPTJ) = IEOR( ITABLE(K), ITABLE(TABPTJ) )
C
C     CONVERT BIG INTEGER TO FLOATING POINT NUMBER
C
      RANLP = REAL( ITABLE(TABPTJ) ) / FN
C
      RETURN
      END
      REAL FUNCTION RANFT(IRESET)
CCCCC REAL FUNCTION RANFT(IDUM,IRESET)
C
      INTEGER TABPTJ
      INTEGER ITABLE(521)
      INTEGER ITABSV(521)
C
C     FUSHIMI-TEZUKA GFSR UNIFORM RANDOM NUMBER GENERATOR
C
C     USES PRIMITIVE TRINOMIAL WITH P=521 AND Q=32 AS USED BY BRIGHT &
C     ENISON AND ARVILLIAS & MARITSAS BUT WITH RANDOM SEED MATRIX
C     FUSHIMI & TEZUKA GIVE RULES FOR TESTING K-DISTRIBUTION OF
C     SEQUENCE -- THE ORIGINAL SEED TABLE HAS BEEN CHECKED AND
C     31 BIT NUMBERS ARE 16-DISTRIBUTED (BEST POSSIBLE)
C
C     M. FUSHIMI & S. TEZUKA (1983) THE K-DISTRIBUTION OF GENERALIZED
C     FEEDBACK SHIFT REGISTER PSEUDORANDOM NUMBERS, COMMUNICATIONS OF
C     THE ACM, VOLUME 26, NUMBER 7, PP. 516-523
C
C     ARGUMENT IS A DUMMY AND NEVER USED
C
C     THE USE OF THE "IEOR" FUNCTION IS NOT STANDARD IN THE
C     FORTRAN 77 (IS STANDARD FOR FORTRAN 90), SO PLACE IN DP1.FOR
C     FILE IN CASE IT NEEDS MODIFICATION.
C
C     THIS IS FORTRAN 77 IMPLEMENTATION OF A FORTRAN 95 CODE
C     FOUND IN MONAHAN (2001), "NUMERICAL METHODS OF STATISTICS",
C     CAMBRIDGE UNIVERSITY PRESS.
C
CCCCC INTEGER IDUM
C  PARAMETERS OF TAUSWORTHE SEQUENCE
      INTEGER P
      INTEGER Q 
      INTEGER K
      REAL    FN
C
      SAVE TABPTJ
      SAVE ITABLE
C
      DATA P /521/
      DATA Q /32/
      DATA TABPTJ /0/
C  FN = 2**31
      DATA FN / 2147483648. /
C
      DATA (ITABLE(I),I=1,18)/
     &  1464221660, 1158328647, 1090310074,
     &  363453867, 1125650601, 1626204584, 
     &  596067919,  102301378, 1392342446,
     & 2117672210, 1470351739, 1107351344,
     & 1160753706, 1046087394,  142212969,
     &   24070872,  832220068, 561689965/
      DATA (ITABLE(I),I=19,36)/
     & 2132613190, 1327815900, 2099255323,
     & 1175377098, 2008300980, 1514090961,
     & 1793048224,  123482417,  899779517,
     & 14500045, 1036604204, 1819512164,
     & 373807068, 1185724401, 1969247094,
     & 117941294,  111922077, 2026157014/
      DATA (ITABLE(I),I=37,54)/
     &  972743819,  112361322,  818613141,
     & 1650818105, 1958655142, 340146731,
     &  244639603, 1374107263,  581629403,
     &   99815077,  407270832, 970490435,
     &  894442080,  502509560, 1772474916,
     &   92762028, 2125760521, 2119124955/
      DATA (ITABLE(I),I=55,72)/
     &  116833190,  815370972,  846774897,
     &  371565210,   14038994, 1877654635,
     &  469257780, 1255556676,  966738110,
     &  106141568, 1509906366, 182036763,
     & 1475162413,  355970676, 2057194637, 
     & 783547359,  710739309, 1091521749/
      DATA (ITABLE(I),I=73,90)/
     & 1400722769, 1231840169, 1795363303,
     &  378309524, 1696574748, 43924770,
     & 1656718469,  194341481, 2122127727,
     & 1192298313,  787836434, 1930262483,
     & 2033580199, 1180162588,  833652824,
     & 1019699940, 1177388520, 1454532182/
      DATA (ITABLE(I),I=91,108)/
     & 1516029073,    7158256, 49724360, 
     & 346179837,  711320736, 126147103,
     &  588000532, 1952681477,  872490485,
     &  929239679, 1230203969, 65553667,
     &  101370358,  777074835, 1448694438,
     & 37829780,  149952948, 1260879105/
      DATA (ITABLE(I),I=109,126)/
     &  226489139, 1261936689,  821434251,
     & 1820573641, 1034181831, 1908878446,
     & 1261839389, 1333596798,  474560247,
     &  179806371,  496186068, 720243575,
     & 1915930533, 1674665013, 1174195909,
     & 1483410280, 1538917937, 300722691/
      DATA (ITABLE(I),I=127,144)/
     & 1217246246, 1328435200, 1770412188,
     & 1931714531,  668347171, 1571429187, 
     & 1256455103, 1034215170,  321723372,
     & 1988373705, 1603828968, 338728032,
     &   22885627,  239160176, 1623174495,
     & 1208969624, 1809686301, 586768446/
      DATA (ITABLE(I),I=145,162)/
     &  572364898, 1157585773, 1489728638,
     &  357378493, 2096054839, 1071933685,
     &  749129112, 2063846670,  915116346,
     &   82547408, 99850294, 999162951,
     & 1757081564, 1222216251, 1107447002,
     &  620994065,  276726035, 1632374490/
      DATA (ITABLE(I),I=163,180)/
     & 1214463005, 1795143947,  986560526,
     &  401521995,  986551091, 242947950,
     &  859782703, 2097912305,   78110042,
     &  682967577,  335973424, 970829205,
     &  145698529,  623819323,  516197007,
     & 2036646416, 1174464179, 1697256876/
      DATA (ITABLE(I),I=181,198)/
     &  771031831,  815657619, 1369483732,
     &  183355178,   11443201, 1199834624,
     &  749080238, 1242421352, 1392163283,
     & 1253963316, 2104424001, 2146002364,
     &  873880383,  666935248, 1463559443,
     &  765865763, 2036382270, 1029929651/
      DATA (ITABLE(I),I=199,216)/
     & 1309449537,  505953903, 1679489248,
     &  650734968, 1915876652, 769087046,
     &  341910829, 1976547278,  405565903,
     &  233036143, 1775766920, 1734382081,
     & 1964094636, 1567409215,  264778756,
     &  550435508, 1957515327, 510628849/
      DATA (ITABLE(I),I=217,234)/
     &  794411731,  772129518, 2084613852,
     &  2056793406,  482508883, 628545509,
     &  492310170,    2535299, 1808581000,
     & 1337327362,  897123632, 474197437,
     &  524509642,   13182159,  361730672,
     & 81199647, 1070351284, 2071002916/
      DATA (ITABLE(I),I=235,252)/
     &  931058636, 1736643210, 1312184093,
     & 1368480008,  493635086, 795562041,
     &  778036865,  437663472,  678482929,
     &  124422133, 1661200800, 366950953,
     & 1919116534, 1534692645,  153200398,
     &    6196433, 1064234375, 195844762/
      DATA (ITABLE(I),I=253,270)/
     & 1617967730, 1745699796, 1054886058,
     & 1992470821, 1744580876, 1576550441,
     & 1430025201, 1944059630, 1993995952,
     & 1607653829,  198657449, 1646157905,
     &  944085034, 1627982402,  411083987,
     &  633677110,  839782297, 958537595/
      DATA (ITABLE(I),I=271,288)/
     & 1866523018,  211248150,  657188559,
     &  859714592,  953170728, 1859902523,
     &  609738329,   80132019,  306596664,
     & 1156862695, 74374927, 183915535,
     &  839428712, 1458285441,  172543676,
     &  838639082, 1071875913, 1925638755/
      DATA (ITABLE(I),I=289,306)/
     & 1631994995, 1278741481, 1939215638,
     &   37917347, 1621691517, 2054362142,
     &  422444128,  437522314,  453524070,
     &  959581287,   80501639, 76349063,
     & 1150964582, 1876521145,  746044173,
     & 1754884425,  802123077, 1527702920/
      DATA (ITABLE(I),I=307,324)/
     &  788492908,   78719119,  182306481,
     & 1713345545,  590352192, 674841804,
     & 1205060021,  525498090, 1593642166,
     &  927838578, 1304219579, 652879324,
     & 1448845945,  436724282, 2073385775,
     &  177580556, 1741619009, 1188575653/
      DATA (ITABLE(I),I=325,342)/
     &  498115577,  937246633,  511610086,
     &  106192814,  223714241, 1868866237,
     &  939024237,  323029456,  317407376, 
     & 316389284,  385186216, 1309020254,
     & 1880929110, 1816267930, 1682541052,
     &  402797268,  945227932, 1509316265/
      DATA (ITABLE(I),I=343,360)/
     & 1001627491,  228932404, 1523702251,
     &  121242082, 1901174818, 635982413,
     &  930304172, 1941268644,  183050837,
     & 1338834955,  465435419, 1437644759,
     & 1156952116, 1577273674,  700500350,
     &  804029596, 1358313048, 1416230126/
      DATA (ITABLE(I),I=361,378)/
     & 2018467981,  592185008, 1414209258,
     &  265994210, 1651218063, 90814660,
     & 1608601250, 1089576667,  921984300,
     & 1695616995, 1126839275, 129412032,
     & 1774571060,  962915884,  290498596,
     & 1179573341, 1667596730, 489164113/
      DATA (ITABLE(I),I=379,396)/
     &  813846475,  994357582,  450139720,
     & 2060869306,  266683479, 350860264,
     & 2065846033,  158671935, 1772005618,
     &  795205130, 1221884629, 1976326989,
     &  998135974, 1676548301,  614362620,
     &  491179564,  327793080, 922741005/
      DATA (ITABLE(I),I=397,414)/
     & 1528656048, 1775329675,  828056307,
     & 1448319189,  173470778, 1388056867,
     &  956906308,  219286173,  460771359,
     &  358199631,  864535676, 376750930,
     & 1271089154,   28090922, 1825207361,
     & 1603702579,  361991756, 174271141/
      DATA (ITABLE(I),I=415,432)/
     & 1954855926,  911232829, 1384270246,
     & 1739676571,  754274892, 502141603,
     & 2030672558, 1703564182, 1551225070,
     &  988276910, 1331500472, 1748831164,
     & 2144180506,  318684035,  298360627,
     &  172742244, 2028487811, 1491743352/
      DATA (ITABLE(I),I=433,450)/
     & 2006421986, 2146093508,  258253944,
     &  409586221, 1230527712, 1211734974,
     & 1042283517,  634961640,  954041537,
     & 1463203857, 1231982802, 2045112487,
     & 1729798774,   94381532, 1427476838,
     & 2063395629, 1924404847, 221056062/
      DATA (ITABLE(I),I=451,468)/
     &  142524724,  968769863, 2041559534,
     & 2144859819,  998479391, 1005906879,
     & 1285646169, 2022189916,  869720790,
     & 1623616048,   40216307, 1605606591,
     &  150466735, 1306162626, 1097415548,
     & 1673554800, 1842198841, 1564181888/
      DATA (ITABLE(I),I=469,486)/
     & 1857668689, 1720395937,  974689951,
     &  608747141,  601104479, 999903065,
     & 1311275680, 1133168246, 1273728926,
     & 1445065986, 1331462779, 1115324913,
     & 2028541775,  251232653,  514348969,
     & 1041442808, 1537551006, 949033491/
      DATA (ITABLE(I),I=487,504)/
     & 1044836968,  601139657, 1591139711,
     & 1818750333,  454615333, 2120569352,
     &  770493452,  357056354,  976831960,
     &  102270405,  871779235, 1860162811,
     &  689431451, 1600121392,  302523963,
     & 1426453692, 2047249983, 1147472047/
      DATA (ITABLE(I),I=505,521)/
     & 1159543869,   39709758, 1681972136,
     & 1578444291, 1047707446, 1600623169,
     &  145955414,  646318224,  698104242,
     & 1334831733, 1902759969, 1507811506,
     & 1480946742,  936424064, 1719078432,
     &  306219886, 1266805790/
C
      DATA (ITABSV(I),I=1,18)/
     &  1464221660, 1158328647, 1090310074,
     &  363453867, 1125650601, 1626204584, 
     &  596067919,  102301378, 1392342446,
     & 2117672210, 1470351739, 1107351344,
     & 1160753706, 1046087394,  142212969,
     &   24070872,  832220068, 561689965/
      DATA (ITABSV(I),I=19,36)/
     & 2132613190, 1327815900, 2099255323,
     & 1175377098, 2008300980, 1514090961,
     & 1793048224,  123482417,  899779517,
     & 14500045, 1036604204, 1819512164,
     & 373807068, 1185724401, 1969247094,
     & 117941294,  111922077, 2026157014/
      DATA (ITABSV(I),I=37,54)/
     &  972743819,  112361322,  818613141,
     & 1650818105, 1958655142, 340146731,
     &  244639603, 1374107263,  581629403,
     &   99815077,  407270832, 970490435,
     &  894442080,  502509560, 1772474916,
     &   92762028, 2125760521, 2119124955/
      DATA (ITABSV(I),I=55,72)/
     &  116833190,  815370972,  846774897,
     &  371565210,   14038994, 1877654635,
     &  469257780, 1255556676,  966738110,
     &  106141568, 1509906366, 182036763,
     & 1475162413,  355970676, 2057194637, 
     & 783547359,  710739309, 1091521749/
      DATA (ITABSV(I),I=73,90)/
     & 1400722769, 1231840169, 1795363303,
     &  378309524, 1696574748, 43924770,
     & 1656718469,  194341481, 2122127727,
     & 1192298313,  787836434, 1930262483,
     & 2033580199, 1180162588,  833652824,
     & 1019699940, 1177388520, 1454532182/
      DATA (ITABSV(I),I=91,108)/
     & 1516029073,    7158256, 49724360, 
     & 346179837,  711320736, 126147103,
     &  588000532, 1952681477,  872490485,
     &  929239679, 1230203969, 65553667,
     &  101370358,  777074835, 1448694438,
     & 37829780,  149952948, 1260879105/
      DATA (ITABSV(I),I=109,126)/
     &  226489139, 1261936689,  821434251,
     & 1820573641, 1034181831, 1908878446,
     & 1261839389, 1333596798,  474560247,
     &  179806371,  496186068, 720243575,
     & 1915930533, 1674665013, 1174195909,
     & 1483410280, 1538917937, 300722691/
      DATA (ITABSV(I),I=127,144)/
     & 1217246246, 1328435200, 1770412188,
     & 1931714531,  668347171, 1571429187, 
     & 1256455103, 1034215170,  321723372,
     & 1988373705, 1603828968, 338728032,
     &   22885627,  239160176, 1623174495,
     & 1208969624, 1809686301, 586768446/
      DATA (ITABSV(I),I=145,162)/
     &  572364898, 1157585773, 1489728638,
     &  357378493, 2096054839, 1071933685,
     &  749129112, 2063846670,  915116346,
     &   82547408, 99850294, 999162951,
     & 1757081564, 1222216251, 1107447002,
     &  620994065,  276726035, 1632374490/
      DATA (ITABSV(I),I=163,180)/
     & 1214463005, 1795143947,  986560526,
     &  401521995,  986551091, 242947950,
     &  859782703, 2097912305,   78110042,
     &  682967577,  335973424, 970829205,
     &  145698529,  623819323,  516197007,
     & 2036646416, 1174464179, 1697256876/
      DATA (ITABSV(I),I=181,198)/
     &  771031831,  815657619, 1369483732,
     &  183355178,   11443201, 1199834624,
     &  749080238, 1242421352, 1392163283,
     & 1253963316, 2104424001, 2146002364,
     &  873880383,  666935248, 1463559443,
     &  765865763, 2036382270, 1029929651/
      DATA (ITABSV(I),I=199,216)/
     & 1309449537,  505953903, 1679489248,
     &  650734968, 1915876652, 769087046,
     &  341910829, 1976547278,  405565903,
     &  233036143, 1775766920, 1734382081,
     & 1964094636, 1567409215,  264778756,
     &  550435508, 1957515327, 510628849/
      DATA (ITABSV(I),I=217,234)/
     &  794411731,  772129518, 2084613852,
     &  2056793406,  482508883, 628545509,
     &  492310170,    2535299, 1808581000,
     & 1337327362,  897123632, 474197437,
     &  524509642,   13182159,  361730672,
     & 81199647, 1070351284, 2071002916/
      DATA (ITABSV(I),I=235,252)/
     &  931058636, 1736643210, 1312184093,
     & 1368480008,  493635086, 795562041,
     &  778036865,  437663472,  678482929,
     &  124422133, 1661200800, 366950953,
     & 1919116534, 1534692645,  153200398,
     &    6196433, 1064234375, 195844762/
      DATA (ITABSV(I),I=253,270)/
     & 1617967730, 1745699796, 1054886058,
     & 1992470821, 1744580876, 1576550441,
     & 1430025201, 1944059630, 1993995952,
     & 1607653829,  198657449, 1646157905,
     &  944085034, 1627982402,  411083987,
     &  633677110,  839782297, 958537595/
      DATA (ITABSV(I),I=271,288)/
     & 1866523018,  211248150,  657188559,
     &  859714592,  953170728, 1859902523,
     &  609738329,   80132019,  306596664,
     & 1156862695, 74374927, 183915535,
     &  839428712, 1458285441,  172543676,
     &  838639082, 1071875913, 1925638755/
      DATA (ITABSV(I),I=289,306)/
     & 1631994995, 1278741481, 1939215638,
     &   37917347, 1621691517, 2054362142,
     &  422444128,  437522314,  453524070,
     &  959581287,   80501639, 76349063,
     & 1150964582, 1876521145,  746044173,
     & 1754884425,  802123077, 1527702920/
      DATA (ITABSV(I),I=307,324)/
     &  788492908,   78719119,  182306481,
     & 1713345545,  590352192, 674841804,
     & 1205060021,  525498090, 1593642166,
     &  927838578, 1304219579, 652879324,
     & 1448845945,  436724282, 2073385775,
     &  177580556, 1741619009, 1188575653/
      DATA (ITABSV(I),I=325,342)/
     &  498115577,  937246633,  511610086,
     &  106192814,  223714241, 1868866237,
     &  939024237,  323029456,  317407376, 
     & 316389284,  385186216, 1309020254,
     & 1880929110, 1816267930, 1682541052,
     &  402797268,  945227932, 1509316265/
      DATA (ITABSV(I),I=343,360)/
     & 1001627491,  228932404, 1523702251,
     &  121242082, 1901174818, 635982413,
     &  930304172, 1941268644,  183050837,
     & 1338834955,  465435419, 1437644759,
     & 1156952116, 1577273674,  700500350,
     &  804029596, 1358313048, 1416230126/
      DATA (ITABSV(I),I=361,378)/
     & 2018467981,  592185008, 1414209258,
     &  265994210, 1651218063, 90814660,
     & 1608601250, 1089576667,  921984300,
     & 1695616995, 1126839275, 129412032,
     & 1774571060,  962915884,  290498596,
     & 1179573341, 1667596730, 489164113/
      DATA (ITABSV(I),I=379,396)/
     &  813846475,  994357582,  450139720,
     & 2060869306,  266683479, 350860264,
     & 2065846033,  158671935, 1772005618,
     &  795205130, 1221884629, 1976326989,
     &  998135974, 1676548301,  614362620,
     &  491179564,  327793080, 922741005/
      DATA (ITABSV(I),I=397,414)/
     & 1528656048, 1775329675,  828056307,
     & 1448319189,  173470778, 1388056867,
     &  956906308,  219286173,  460771359,
     &  358199631,  864535676, 376750930,
     & 1271089154,   28090922, 1825207361,
     & 1603702579,  361991756, 174271141/
      DATA (ITABSV(I),I=415,432)/
     & 1954855926,  911232829, 1384270246,
     & 1739676571,  754274892, 502141603,
     & 2030672558, 1703564182, 1551225070,
     &  988276910, 1331500472, 1748831164,
     & 2144180506,  318684035,  298360627,
     &  172742244, 2028487811, 1491743352/
      DATA (ITABSV(I),I=433,450)/
     & 2006421986, 2146093508,  258253944,
     &  409586221, 1230527712, 1211734974,
     & 1042283517,  634961640,  954041537,
     & 1463203857, 1231982802, 2045112487,
     & 1729798774,   94381532, 1427476838,
     & 2063395629, 1924404847, 221056062/
      DATA (ITABSV(I),I=451,468)/
     &  142524724,  968769863, 2041559534,
     & 2144859819,  998479391, 1005906879,
     & 1285646169, 2022189916,  869720790,
     & 1623616048,   40216307, 1605606591,
     &  150466735, 1306162626, 1097415548,
     & 1673554800, 1842198841, 1564181888/
      DATA (ITABSV(I),I=469,486)/
     & 1857668689, 1720395937,  974689951,
     &  608747141,  601104479, 999903065,
     & 1311275680, 1133168246, 1273728926,
     & 1445065986, 1331462779, 1115324913,
     & 2028541775,  251232653,  514348969,
     & 1041442808, 1537551006, 949033491/
      DATA (ITABSV(I),I=487,504)/
     & 1044836968,  601139657, 1591139711,
     & 1818750333,  454615333, 2120569352,
     &  770493452,  357056354,  976831960,
     &  102270405,  871779235, 1860162811,
     &  689431451, 1600121392,  302523963,
     & 1426453692, 2047249983, 1147472047/
      DATA (ITABSV(I),I=505,521)/
     & 1159543869,   39709758, 1681972136,
     & 1578444291, 1047707446, 1600623169,
     &  145955414,  646318224,  698104242,
     & 1334831733, 1902759969, 1507811506,
     & 1480946742,  936424064, 1719078432,
     &  306219886, 1266805790/
C
C     START EXECUTABLE CODE
C
      IF(IRESET.EQ.1)THEN
        DO100LL=1,521
          ITABLE(LL)=ITABSV(LL)
  100   CONTINUE
        TABPTJ = 0
      ENDIF
C
C     UPDATE POINTER
C
      TABPTJ = TABPTJ + 1
      IF(TABPTJ.GT.P) TABPTJ = 1
C
C     UPDATE DELAY POINTER
C
      K = TABPTJ + Q
      IF(K.GT.P) K = K - P
C
C     COMPUTE EXCLUSIVE OR OF TWO TABLE ENTRIES AND REPLACE WITH NEW ONE
C
      ITABLE(TABPTJ) = IEOR( ITABLE(K), ITABLE(TABPTJ) )
C
C     CONVERT BIG INTEGER TO FLOATING POINT NUMBER
C
      RANFT = REAL( ITABLE(TABPTJ) ) / FN
C
      RETURN
      END
      SUBROUTINE DPPID2(IPID,ISUBRO,IERROR)
C
C     PURPOSE--THIS ROUTINE IS USED BY DPPID (AND POSSIBLY BY A
C              FEW OTHER ROUTINES) TO EXTRACT THE PROCESS ID.
C              THE PRIMARY USE OF THIS IS BUILDING UNIQUE FILE
C              NAMES.  HOWEVER, DATAPLOT USERS CAN USE IT FOR
C              WHATEVER PURPOSE THEY NEED.
C     TO THE IMPLEMENTER--
C              THIS IS A PLATFORM/COMPILER DEPENDENT ROUTINE,
C              SO YOU MAY NEED TO MODIFY IT FOR YOUR LOCAL
C              INSTALLATION.  IF IS CURRENTLY IMPLEMENTED FOR
C              INTEL COMPILER UNDER WINDOWS AND FOR THE g77
C              COMPILER UNDER UNIX.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2006.3
C     ORIGINAL VERSION--MARCH      2006.
C
C-----NON-COMMON VARIABLES (GRAPHICS)--------------------------------
C
#ifdef INTEL
      USE IFPORT
#endif
C
C
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
      IPID=0
C
      IF(ISUBRO.EQ.'PID2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPPID2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,59)ISUBRO,IERROR
   59   FORMAT('ISUBRO,IERROR= ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,61)IHOST1,IHOST2,IHMOD1,IHMOD2
   61   FORMAT('IHOST1,IHOST2,IHMOD1,IHMOD2 = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IOPSY1,IOPSY2,ICOMPI,ISITE
   63   FORMAT('IOPSY1,IOPSY2,ICOMPI,ISITE = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ********************************
C               **  STEP 1--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       *********************************************************
C       *  PC USING INTEL     COMPILER                          *
C       *********************************************************
C
#ifdef INTEL
        IPID=getpid()
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C       *********************************************************
C       *  UNIX.  USE THE LIBRARY ROUTINE "GETPID".             *
C       *********************************************************
C
#ifdef LINUX
        IPID=getpid()
#endif
        GOTO9000
      ELSE
C
C       *********************************************************
C       *  OTHER   - LEFT TO IMPLEMENTOR                        *
C       *********************************************************
C
        WRITE(ICOUT,8010)
 8010   FORMAT(1X,'THE PROCESS ID COMMAND HAS NOT BEEN IMPLEMENTED ',
     1         'AT THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8020)IHOST1
 8020   FORMAT(1X,'SITE FOR A ',A4,' HOST.')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(ISUBRO.EQ.'PID2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPPID2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IPID
 9015   FORMAT('PROCESS ID  = ',I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPINF2(IFILE,IEXIST,IFWRIT,ISUBN0,IBUGS2,
     1                  ISUBRO,IERRFI)
C
C     PURPOSE--THE DPINFI ROUTINE CHECKS FOR THE EXISTENCE OF A
C              FILE.  THIS ROUTINE IS A SLIGHT VARIATION THAT
C              CHECKS IF THE FILE CAN BE OPENED IN WRITE MODE.
C
C              THE PURPOSE OF THIS IS TO CHECK IF THE PLOT FILES
C              ARE IN USE BY ANOTHER PROCESS.  CURRENTLY UNDER LINUX,
C              IF TWO DATAPLOT SESSIONS ARE RUNNING IN THE SAME
C              DIRECTORY AT THE SAME TIME, BOTH ARE ALLOWED TO WRITE
C              TO THE FILE (I.E., THERE IS NO LOCK ON THE FILE).
C              HOWEVER, UNDER WINDOWS, IF A DATAPLOT SESSION IS
C              ALREADY RUNNING, THEN A SECOND DATAPLOT PROCESS
C              WILL NOT BE ABLE TO OPEN THE FILE IN WRITE MODE
C              (IT WILL IN FACT HANG DATAPLOT).  THIS IS IN
C              PARTICULAR AN ISSUE BECAUSE THE VERSION BUILT WITH
C              THE INTEL COMPILER DOES NOT AUTOMATICALLY CLOSE
C              IF THE GUI IS NOT SHUT DOWN CLEANLY.  IF THIS HAPPENS,
C              WHEN YOU RESTART THE DATAPLOT GUI, THE "DEAD" PROCESS
C              STILL HAS THE PLOT FILE LOCKED AND THE NEW SESSION
C              HANGS.
C
C              NOTE THAT INQUIRING ABOUT THE "WRITE" MODE IS A
C              FORTRAN 90 FEATURE NOT AVAILABLE IN FORTRAN 77.
C              SO FOR NOW, THIS COMMAND IS ONLY ACTIVE UNDER
C              WINDOWS.
C
C     WRITTEN BY--JAMES J. FILLIBEN
C     LANGUAGE--ANSI FORTRAN (1977)
C     ORIGINAL VERSION--MARCH     2006.
C
C---------------------------------------------------------------------
C
      CHARACTER*(*) IFILE
      CHARACTER*4 IEXIST
      CHARACTER*12 IFWRIT
      CHARACTER*4 ISUBN0
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERRFI
C
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
C
      LOGICAL LEXIST
C
C-----COMMON------------------------------------------------
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      ISUBN1='DPIN'
      ISUBN2='F2  '
      IERRFI='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'INF2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('*****AT THE BEGINNING OF DPINF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)IFILE
   52   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)IWRITE,ISUBN0,IHOST1
   54   FORMAT('IEXIST,ISUBN0,IHOST1 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               ******************************************************
C               **  STEP 1--                                        **
C               **  INQUIRE ABOUT THE EXISTENCE OF A FILE.          **
C               **  IF FILE DOES NOT EXIST, THEN ASSUME THAT IT     **
C               **  IS WRITTABLE.  IF FILE EXISTS, CHECK IF IT      **
C               **  IS A WRITABLE FILE.                             **
C               ******************************************************
C
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'INF2')
     1CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
C
      IEXIST='NO'
      INQUIRE(FILE=IFILE,EXIST=LEXIST)
      IF(LEXIST)IEXIST='YES'
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        IF(IEXIST.EQ.'NO')THEN
          IFWRIT='YES'
        ELSE
          IFWRIT='YES'
          INQUIRE(FILE=IFILE,WRITE=IFWRIT)
          IF(IFWRIT.EQ.'UNKOWN')IFWRIT='YES'
        ENDIF
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        IF(IEXIST.EQ.'NO')THEN
          IFWRIT='YES'
        ELSE
          IFWRIT='YES'
          INQUIRE(FILE=IFILE,WRITE=IFWRIT)
          IF(IFWRIT.EQ.'UNKOWN')IFWRIT='YES'
        ENDIF
      ELSE
         IFWRIT='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'INF2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('*****AT THE END       OF DPINF2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IFILE
 9012   FORMAT('IFILE = ',A80)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IEXIST,IFWRIT
 9014   FORMAT('IEXIST,IFWRIT = ',A4,2X,A12)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)ISUBN0,IERRFI,IHOST1
 9015   FORMAT('ISUBN0,IERRFI,IHOST1 = ',2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPPRMP(IPRDEF,IFLAG)
C
C     PURPOSE--THIS ROUTINE IS USED TO GENERATE EITHER AN
C              ADVANCING PROMPT (FORTRAN 77) OR A NON-ADVANCING
C              PROMPT (FORTRAN 90).
C     TO THE IMPLEMENTER--
C              THIS IS A PLATFORM/COMPILER DEPENDENT ROUTINE,
C              SO YOU MAY NEED TO MODIFY IT FOR YOUR LOCAL
C              INSTALLATION.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2007.8
C     ORIGINAL VERSION--AUGUST     2007.
C
C-----NON-COMMON VARIABLES (GRAPHICS)--------------------------------
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
C               ********************************
C               **  STEP 1--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
C     NOTE: VALUE OF IFLAG DETERMINES WHETHER A
C           "1X" IS REQUIRED.
C
C     NOTE: FOR THE INTEL FORTRAN COMPILER VERSION 13,
C           WHEN "ADVANCE='NO'" IS USED, THE PROMPT DOES
C           NOT PRINT.  FOR NOW, JUST SUPPRESS THIS OPTION.
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
CCCCC   WRITE(IPRDEF,1611,ADVANCE='NO')
        WRITE(IPRDEF,1611)
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        IF(ICOMPI.EQ.'gfor' .AND. IFLAG.EQ.2)THEN
          WRITE(IPRDEF,1611,ADVANCE='NO')
        ELSE
          WRITE(IPRDEF,1611)
        ENDIF
      ELSE
        IF(IFLAG.EQ.0)THEN
          WRITE(IPRDEF,1611)
        ELSE
          WRITE(IPRDEF,1613)
        ENDIF
      ENDIF
 1611 FORMAT('>')
 1613 FORMAT(1X,'>')
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      RETURN
      END
      SUBROUTINE DPCPUT(ATIME,IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--RETURN CURRENT CPU TIME USAGE.  CALL WITH INIT
C              SET TO 0 TO INITIALIZE THE TIMER.  OTHERWISE SET
C              INIT TO 1.  THIS ROUTINE UTILIZES FORTRAN 90
C              STANDARD CALLS, SO DO NOT MAKE HOST DEPENDENT.
C              IF COMPILING WITH A FORTRAN 77 COMPILER, YOU
C              MAY NEED TO COMMENT OUT THESE FORTRAN 90 CALLS.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1990)
C     VERSION NUMBER--2009/05
C     ORIGINAL VERSION--MAY       2009.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CPUT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPCPUT')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CALL CPU_TIME(ATIME)
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CPUT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('AT THE END OF DPCPUT')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9053)ATIME
 9053   FORMAT('ATIME = ',G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
#ifdef INTEL_QWIN
      SUBROUTINE IQWNHE(CHECKED)
#else
      SUBROUTINE IQWNHE()
#endif
C
C  CALL BACK ROUTINE FOR WHEN THE "USING HELP" MENU ITEM IS
C  SELECTED.
C
#ifdef INTEL_QWIN
      USE DFLIB
      USE IFQWIN
      LOGICAL CHECKED
      INTEGER IRET
#endif
C
#ifdef INTEL_QWIN
      CHECKED=.TRUE.
      IRET=MESSAGEBOXQQ(
     1'To access Dataplot help, enter the command HELP'C,
     1'Dataplot Help'C,
     1MB$OK)
#endif
C
      RETURN
      END
#ifdef INTEL_QWIN
      SUBROUTINE IQWNBR(CHECKED)
#else
      SUBROUTINE IQWNBR()
#endif
C
C  CALL BACK ROUTINE FOR WHEN THE "HELP CONTENTS" MENU ITEM
C  SELECTED.
C
#ifdef INTEL_QWIN
      USE DFLIB
      USE DFWIN
      USE IFQWIN
      LOGICAL CHECKED
      INTEGER IRET
#endif
C
#ifdef INTEL_QWIN
      CHECKED=.TRUE.
      IRET=MESSAGEBOXQQ(
     1'If you click OK, Dataplot will try to connect to the '//
     1'Dataplot Reference Manual at the NIST web site (using '//
     1'Internet Explorer).  Alternatively, you can use the,'//
     1'Dataplot WEB HELP command (WEB HELP allows you to '//
     1'specify a local site (if you have the installed a copy '//
     1'the Dataplot web pages locally) or a different browser '//
     1'(e.g., Netscape).  Enter HELP WEB HELP for details.'C,
     1'Accesing the On-line Dataplot Reference Manual'C,
     1MB$OKCANCEL)
C
      IF(IRET.EQ.MB$IDOK)THEN
        IRET=WinExec('Explorer.exe '//
     1       'http://www.itl.nist.gov/div898/'//
     1       'software/dataplot/document.htm'C,
     1       SW_SHOW)
      ENDIF
#endif
C
      RETURN
      END
      LOGICAL FUNCTION ISNANZ(VALUE)
C
C     PURPOSE--SOME FORTRAN COMPILERS SUPPORT AN "ISNAM" FUNCTION
C              THAT RETURNS TRUE IF THE TESTED VALUE IS
C              "NOT A NUMBER".  NOTE THAT THIS IS AN EXTENSION
C              TO THE FORTRAN STANDARD AND IS NOT CURRENTLY PART
C              OF THE FORTRAN STANDARD.  IT IS SUPPORTED IN
C              VERSION 4.4 AND ABOVE IN GFORTRAN AND ALSO FOR THE
C              INTEL COMPILER.  HOWEVER, IT IS NOT SUPPORTED IN
C              VERSION 4.1.2 OF GFORTRAN.  USE A "HAVE_ISNAM"
C              COMPILER SWITCH FOR LINUX.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1990)
C     VERSION NUMBER--2014/01
C     ORIGINAL VERSION--JANUARY   2014.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
#ifdef INTEL
      ISNANZ=ISNAN(VALUE)
#else
#ifdef HAVE_ISNAN
      ISNANZ=ISNAN(VALUE)
#else
      ISNANZ=.FALSE.
      IF(VALUE.NE.VALUE)ISNANZ=.TRUE.
#endif
#endif
C
      RETURN
      END
#ifdef INTEL
      SUBROUTINE DPCLIP(XSCRT,MAXVAL,NVAL,NUME,NUMVLN,PREAMV,
     1                  ISKIP,IGRPAU,
     1                  IVLIST,IVLIS2,IAVANM,MAXRDV,
     1                  ITYPE,ISTR,NCSTR,IEOF,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
C              SYSTEM AND COMPILER DEPENDENT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.10
C     ORIGINAL VERSION--OCTOBER    2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
C
      USE IFWIN
C
      REAL XSCRT(*)
C
      CHARACTER*4 IVLIST(MAXRDV)
      CHARACTER*4 IVLIS2(MAXRDV)
      CHARACTER*4 IAVANM
      CHARACTER*4 ITYPE
      CHARACTER*255 ISTR
      CHARACTER*4 IGRPAU
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*80 IATEMP
      CHARACTER*1  ICHR
      CHARACTER*1  ICHR2
      CHARACTER*1  ICHRN
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
      INTEGER IST
      INTEGER I
      INTEGER HDATA
      CHARACTER*1 STEXT(*)
      POINTER (PTEXT, STEXT)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLIP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLIP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,ITYPE,MAXVAL,NUME
   81   FORMAT('IBUGS2,ISUBRO,ITYPE,MAXVAL,NUME = ',3(A4,2X),2I10)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ITYPE.EQ.'VARI')THEN
        DO110I=1,MAXVAL
          XSCRT(I)=CPUMIN
  110   CONTINUE
      ELSE
        ISTR=' '
      ENDIF
      NVAL=0
      NCSTR=0
      IEOF=0
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
           iSt = OpenClipboard(NULL)
           hData = GetClipboardData(CF_TEXT)
           pText = LocalLock(hData)
C
           NLNRD=0
           NVAL=0
           ICNTR=0
C
C          IF HEADER LINES ARE SPECIFIED, CHECK FOR CR/LF UNTIL
C          ALL HEADER LINES READ.  CHECK FOR VARIABLE NAMES ON
C          LAST LINE, OR NEXT TO LAST LINE IF LAST LINE IS "----",
C          IF REQUESTED.
C
           IF(ISKIP.GT.0)THEN
C
             NVAR=0
             NLCURR=1
             IPTR1=1
             IPTR2=0
             IPTRP1=0
             IPTRP2=0
C
  200        CONTINUE
             ICNTR=ICNTR+1
             ICHR=STEXT(ICNTR)(1:1)
             IVAL=ICHAR(ICHR)
             IF(IVAL.EQ.0)THEN
               IEOF=1
               GOTO1099
             ENDIF
             IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
               IPTR2=ICNTR-1
               ICHRN=STEXT(ICNTR+1)(1:1)
               IVALN=ICHAR(ICHRN)
               IF((IVAL.EQ.10 .AND. IVALN.EQ.13) .OR.
     1            (IVAL.EQ.13 .AND. IVALN.EQ.10))THEN
                 ICNTR=ICNTR+1
               ENDIF
               IF(NLCURR.EQ.ISKIP-1)THEN
                 IPTRP1=IPTR1
                 IPTRP2=IPTR2
               ELSEIF(NLCURR.EQ.ISKIP)THEN
                 IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.
     1              STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
                   IPTR1=IPTRP1
                   IPTR2=IPTRP2
                 ENDIF
C
C                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
C                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
C                TREATED AS DELIMITERS.
C
                 IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')GOTO219
                 NVAR=0
                 ICHCN2=0
                 DO210II=IPTR1,IPTR2
                   ICHRN=STEXT(II)(1:1)
                   IVAL2=ICHAR(ICHRN)
                   IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.
     1               ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
                     IF(ICHCN2.EQ.0)GOTO210
                     ISTOP=II
                     NVAR=NVAR+1
                     IF(NVAR.LE.MAXRDV)THEN
                       NCHAR=ISTOP-ISTRT+1
                       DO220JJ=1,MIN(NCHAR,4)
                         IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  220                  CONTINUE
                       IF(NCHAR.GE.5)THEN
                         DO230JJ=5,MIN(NCHAR,8)
                           IVLIS2(NVAR)(JJ-4:JJ-4)=
     1                     STEXT(ISTRT+JJ-1)(1:1)
  230                    CONTINUE
                       ENDIF
                     ELSE
                       GOTO219
                     ENDIF
                     ICHCN2=0
                   ELSE
                     ICHCN2=ICHCN2+1
                     IF(ICHCN2.EQ.1)ISTRT=II
                     ICURR=II
                   ENDIF
  210            CONTINUE
C
                 IF(ICHCN2.GT.0)THEN
                   NVAR=NVAR+1
                   IVLIST(NVAR)='    '
                   IVLIS2(NVAR)='    '
                   ISTOP=ICURR
                   IF(NVAR.LE.MAXRDV)THEN
                     NCHAR=ISTOP-ISTRT+1
                     DO270JJ=1,MIN(NCHAR,4)
                       IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  270                CONTINUE
                     IF(NCHAR.GE.5)THEN
                       DO280JJ=5,MIN(NCHAR,8)
                         IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
  280                  CONTINUE
                     ENDIF
                   ENDIF
                 ENDIF
C
  219            CONTINUE
C
                 GOTO1000
               ENDIF
               NLCURR=NLCURR+1
               IPTR1=ICNTR+1
             ENDIF
             GOTO200
C
           ELSEIF(ISKIP.LT.0)THEN
C
C            FOR SKIP AUTOMATIC, READ UNTIL A LINE STARTING WITH
C            "----" IS ENCOUNTERED.
C
             NVAR=0
             NLCURR=1
             IPTR1=1
             IPTR2=0
             IPTR3=1
             IPTRP1=0
             IPTRP2=0
C
  300        CONTINUE
             ICNTR=ICNTR+1
             ICHR=STEXT(ICNTR)(1:1)
             IVAL=ICHAR(ICHR)
             IF(IVAL.EQ.0)THEN
               IEOF=1
               GOTO1099
             ENDIF
             IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
               IPTRP1=IPTR1
               IPTRP2=IPTR2
               IPTR1=IPTR3
               IPTR2=ICNTR-1
               ICHRN=STEXT(ICNTR+1)(1:1)
               IVALN=ICHAR(ICHRN)
               IF((IVAL.EQ.10 .AND. IVALN.EQ.13) .OR.
     1            (IVAL.EQ.13 .AND. IVALN.EQ.10))THEN
                 ICNTR=ICNTR+1
               ENDIF
               IPTR3=ICNTR+1
C
C              CHECK FOR "----"
C
               IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.
     1            STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
                 IPTR1=IPTRP1
                 IPTR2=IPTRP2
C
C                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
C                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
C                TREATED AS DELIMITERS.
C
                 IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')GOTO319
                 NVAR=0
                 ICHCN2=0
                 DO310II=IPTR1,IPTR2
                   ICHRN=STEXT(II)(1:1)
                   IVAL2=ICHAR(ICHRN)
                   IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.
     1               ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
                     IF(ICHCN2.EQ.0)GOTO310
                     ISTOP=II
                     NVAR=NVAR+1
                     IVLIST(NVAR)='   '
                     IVLIS2(NVAR)='   '
                     IF(NVAR.LE.MAXRDV)THEN
                       NCHAR=ISTOP-ISTRT+1
                       DO330JJ=1,MIN(NCHAR,4)
                         IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  330                  CONTINUE
                       IF(NCHAR.GE.5)THEN
                         DO340JJ=5,MIN(NCHAR,8)
                           IVLIS2(NVAR)(JJ-4:JJ-4)=
     1                     STEXT(ISTRT+JJ-1)(1:1)
  340                    CONTINUE
                       ENDIF
                     ELSE
                       GOTO319
                     ENDIF
                     ICHCN2=0
                   ELSE
                     ICHCN2=ICHCN2+1
                     IF(ICHCN2.EQ.1)ISTRT=II
                     ICURR=II
                   ENDIF
  310            CONTINUE
C
                 IF(ICHCN2.GT.0)THEN
                   NVAR=NVAR+1
                   IVLIST(NVAR)='    '
                   IVLIS2(NVAR)='    '
                   ISTOP=ICURR
                   IF(NVAR.LE.MAXRDV)THEN
                     NCHAR=ISTOP-ISTRT+1
                     DO370JJ=1,MIN(NCHAR,4)
                       IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  370                CONTINUE
                     IF(NCHAR.GE.5)THEN
                       DO380JJ=5,MIN(NCHAR,8)
                         IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
  380                  CONTINUE
                     ENDIF
                   ENDIF
                 ENDIF
C
  319            CONTINUE
C
                 GOTO1000
               ENDIF
               NLCURR=NLCURR+1
             ENDIF
             GOTO300
C
           ENDIF
C
 1000      CONTINUE
C
C            STRING CASE.  FOR NOW, READ FIRST RECORD AS A SINGLE
C            STRING.  DO NOT INCLUDE LEADING/TRAILING SPACES.
C
             IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')THEN
               DO2090II=1,10000
                 ICNTR=ICNTR+1
                 ICHR=STEXT(ICNTR)(1:1)
                 IVAL=ICHAR(ICHR)
                 IF(IVAL.EQ.0)THEN
                   GOTO1099
                 ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
                   GOTO1099
                 ELSEIF(IVAL.EQ.32)THEN
                   IF(NCSTR.GT.0 .AND. NCSTR.LT.255)THEN
                     NCSTR=NCSTR+1
                     ISTR(NCSTR:NCSTR)=ICHR
                   ENDIF
                 ELSE
                   IF(NCSTR.LT.255)THEN
                     NCSTR=NCSTR+1
                     ISTR(NCSTR:NCSTR)=ICHR
                   ENDIF
                 ENDIF
 2090          CONTINUE
               GOTO1099
             ENDIF
C
             ICHCNT=0
             DO1090II=1,80
C
               ICNTR=ICNTR+1
               ICHR=STEXT(ICNTR)(1:1)
               IVAL=ICHAR(ICHR)
C
C              NULL CHARACTER IMPLIES END OF CLIPBOARD DATA
C
               IF(IVAL.EQ.0)THEN
                 IEOF=1
                 IF(ICHCNT.EQ.0)GOTO1099
                 GOTO1080
C
C              SPACE INTERPRETED AS A DELIMITER THAT DOES NOT ALLOW
C              FOR "MISSING" COLUMNS
C
               ELSEIF(IVAL.EQ.32)THEN
                 IF(ICHCNT.GT.0)GOTO1080
C
C              COMMA AND TAB ARE ALL INTERPRETED AS FIELD DELIMITERS
C              THAT ALLOW FOR "MISSING" COLUMNS.  THAT IS, IF SUCCESSIVE
C              COMMAS OR TABS ARE ENCOUNTERED, INSERT A "MISSING VALUE".
C
               ELSEIF(IVAL.EQ.9 .OR. ICHR.EQ.',')THEN
                 IF(ICHCNT.GT.0)THEN
                   GOTO1080
                 ELSE
                   NVAL=NVAL+1
                   IF(NVAL.GT.MAXVAL)THEN
                     WRITE(ICOUT,1003)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,1005)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,1007)MAXVAL
                     CALL DPWRST('XXX','BUG ')
                     IERROR='YES'
                     GOTO1099
                   ENDIF
                   XSCRT(NVAL)=PREAMV
                   GOTO1000
                 ENDIF
C
C              LINE FEED/CARRIAGE RETURN SIGNIFY END OF RECORD
C              DELIMITERS.  IF NUMBER OF VARIABLES NOT SPECIFIED
C              ON READ, CHECK NUMBER OF VALUES READ ON FIRST LINE.
C
               ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
C
C                2020/02: IF LAST CHARACTER IS COMMA OR TAB, THEN
C                         ADD A MISSING VALUE.
C
                 ICHR2=STEXT(ICNTR-1)(1:1)
                 IVAL2=ICHAR(ICHR2)
                 IF(IVAL2.EQ.9 .OR. ICHR2.EQ.',')THEN
                   NVAL=NVAL+1
                   IF(NVAL.GT.MAXVAL)THEN
                     WRITE(ICOUT,1003)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,1005)
                     CALL DPWRST('XXX','BUG ')
                     WRITE(ICOUT,1007)MAXVAL
                     CALL DPWRST('XXX','BUG ')
                     IERROR='YES'
                     GOTO1099
                   ENDIF
                   XSCRT(NVAL)=PREAMV
                 ENDIF
                 ICHRN=STEXT(ICNTR+1)(1:1)
                 IVALN=ICHAR(ICHRN)
                 IF((IVAL.EQ.10 .AND. IVALN.EQ.13) .OR.
     1              (IVAL.EQ.13 .AND. IVALN.EQ.10))THEN
                   ICNTR=ICNTR+1
                 ENDIF
C
                 IF(NLNRD.EQ.0)THEN
                   IF(NUME.EQ.0)THEN
                     NUME=NVAL
                     IF(ICHCNT.GT.0)NUME=NVAL+1
                   ENDIF
                   NUMVLN=NVAL
                   IF(ICHCNT.GT.0)NUMVLN=NUMVLN+1
                 ENDIF
                 NLNRD=NLNRD+1
                 IF(ICHCNT.GT.0)GOTO1080
C
C              ADD CURRENT CHARACTER TO TEMPORARY STRING
C
               ELSE
                 ICHCNT=ICHCNT+1
                 IATEMP(ICHCNT:ICHCNT)=ICHR
               ENDIF
C
 1090        CONTINUE
             IERROR='YES'
             GOTO1099
C
C            PROCESS CURRENT FIELD
C
 1080        CONTINUE
             CALL EXTREA(IATEMP,ICHCNT,AVALUE,IBUGS2,ISUBRO,IERROR)
             IF(AVALUE.EQ.CPUMIN)GOTO1000
C
C            IF ERROR DETECTED, GIVE ROW AND COLUMN
C
             IF(IERROR.EQ.'YES')THEN
               IF(IGRPAU.EQ.'IGNO')THEN
                 AVALUE=PREAMV
               ELSE
                 WRITE(ICOUT,999)
                 CALL DPWRST('XXX','BUG ')
                 NJUNK=MOD(NVAL,NLNRD) + 1
                 WRITE(ICOUT,2005)NLNRD+1,NJUNK
 2005            FORMAT('       UNABLE TO READ NUMERIC FIELD ON ROW ',
     1                  I8,' AND COLUMN ',I8)
                 CALL DPWRST('XXX','BUG ')
                 GOTO1099
               ENDIF
             ENDIF
C
             NVAL=NVAL+1
             IF(NVAL.GT.MAXVAL)THEN
               WRITE(ICOUT,1003)
 1003          FORMAT('***** ERROR READING FROM CLIPBOARD')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,1005)
 1005          FORMAT('      MAXIMUM NUMBER OF VALUES EXCEEDED.')
               CALL DPWRST('XXX','BUG ')
               WRITE(ICOUT,1007)MAXVAL
 1007          FORMAT('      MAXIMUM NUMBER OF VALUES  = ',I10)
               CALL DPWRST('XXX','BUG ')
               IERROR='YES'
               GOTO1099
             ENDIF
             XSCRT(NVAL)=AVALUE
             IF(IVAL.EQ.0)THEN
               IEOF=1
               GOTO1099
             ENDIF
             GOTO1000
C
C            PROCESSING OF CLIPBOARD COMPLETED
C
 1099      CONTINUE
           iSt = LocalUnlock(hData)
           iSt = CloseClipboard()
           IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')THEN
             IF(NCSTR.GT.0)THEN
               DO1101II=NCSTR,1,-1
                 IF(ISTR(II:II).NE.' ')THEN
                   NCSTR=II
                   GOTO1103
                 ENDIF
 1101           CONTINUE
                NCSTR=0
 1103           CONTINUE
             ENDIF
             IF(NCSTR.LE.0)THEN
               NCSTR=4
               ISTR(1:4)='NULL'
             ENDIF
           ENDIF
           IF(IERROR.EQ.'YES')GOTO9000
        ELSE
C
          IF(IFEEDB.EQ.'ON' .AND. ITYPE.NE.'COMM')THEN
            WRITE(ICOUT,1011)
 1011       FORMAT('***** NOTHING AVAILABLE FROM CLIPBOARD')
            CALL DPWRST('XXX','BUG ')
          ENDIF
          IEOF=1
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2011)
 2011   FORMAT('***** READING FROM SYSTEM CLIPBOARD NOT SUPPORTED ',
     1         'ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLIP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLIP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR
 9021   FORMAT('IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR = ',
     1         2(A4,2X),2I10,2I5)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO9025I=1,NCSTR
            WRITE(ICOUT,9027)I,ISTR(I:I)
 9027       FORMAT('I,ISTR(I:I) = ',I5,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI2(ITYPE,IOUNIT,IHELMX,ILINRD,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
C              SYSTEM AND COMPILER DEPENDENT.
C
C              THE DPCLIP COMMAND IS USED TO EITHER READ A LIST
C              VARIABLES OR A SINGLE STRING.
C
C              DPCLI2 IS USED TO:
C
C                1. VIEW THE CONTENTS OF THE CLIPBOARD
C                2. TO COPY THE CONTENTS OF THE CLIPBOARD
C                   TO A USER SPECIFIED FILE.
C                3. TO EXTRACT THE NUMBER OF LINES IN THE CLIPBOARD
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" MUST COME BEFORE ANY OTHER DECLARATIONS
C
      USE IFWIN
C
      CHARACTER*4 ITYPE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*255 ISTR
      CHARACTER*1  ICHR
      CHARACTER*1  ICHRN
      CHARACTER*4  IRESP
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
      INTEGER IST
      INTEGER HDATA
      CHARACTER*1 STEXT(*)
      POINTER (PTEXT, STEXT)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,ITYPE,IHELMX
   81   FORMAT('IBUGS2,ISUBRO,ITYPE= ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ISTR=' '
      NCSTR=0
      ILINRD=0
      ICNTR=0
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C         VIEW THE CONTENTS OF THE CLIPBOARD OR COPY TO A
C         USER-SPECIFIED FILE
C
        IF(ITYPE.EQ.'VIEW' .OR. ITYPE.EQ.'COPY' .OR.
     1     ITYPE.EQ.'NLIN')THEN
C
C         VIEW THE CONTENTS OF THE CLIPBOARD
C
          IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
             iSt = OpenClipboard(NULL)
             hData = GetClipboardData(CF_TEXT)
             pText = LocalLock(hData)
C
 1000        CONTINUE
C
C            READ UNTIL NULL STRING OR CR/LF ENCOUNTERED.
C
             DO1090II=1,10000
               ICNTR=ICNTR+1
               ICHR=STEXT(ICNTR)(1:1)
               IVAL=ICHAR(ICHR)
               IF(IVAL.EQ.0)THEN
C
C                END OF STRING ENCOUNTERED
C
                 IF(NCSTR.GE.1)THEN
                   IF(ITYPE.EQ.'VIEW')THEN
                     NCSTR=MIN(240,NCSTR)
                     WRITE(ICOUT,1010)ISTR(1:NCSTR)
 1010                FORMAT(A)
                     CALL DPWRST('XXX','BUG ')
                   ELSEIF(ITYPE.EQ.'COPY')THEN
                     WRITE(IOUNIT,1020)ISTR(1:NCSTR)
 1020                FORMAT(A)
                   ELSE
                     IF(NCSTR.GE.1)ILINRD=ILINRD+1
                   ENDIF
                 ENDIF
                 NCSTR=0
                 ISTR=' '
                 GOTO1099
               ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
                 ICHRN=STEXT(ICNTR+1)(1:1)
                 IVALN=ICHAR(ICHRN)
                 IF(IVAL.EQ.10 .AND. IVALN.EQ.13)THEN
                   ICNTR=ICNTR+1
                 ELSEIF(IVAL.EQ.13 .AND. IVALN.EQ.10)THEN
                   ICNTR=ICNTR+1
                 ENDIF
C
C                END OF CURRENT RECORD
C
                 ILINRD=ILINRD+1
                 IF(ILINRD.GT.IHELMX .AND. ITYPE.EQ.'VIEW')THEN
                   WRITE(ICOUT,1051)
 1051              FORMAT('                            MORE...?')
                   CALL DPWRST('XXX','WRIT')
                   READ(IRD,1052)IRESP
 1052              FORMAT(A4)
                   IF(IRESP.EQ.'N')IRESP='NO'
                   IF(IRESP.EQ.'n')IRESP='NO'
                   IF(IRESP.EQ.'no')IRESP='NO'
                   IF(IRESP.EQ.'NO')GOTO1099
                   ILINRD=0
                 ENDIF
                 IF(NCSTR.GE.1)THEN
                   IF(ITYPE.EQ.'VIEW')THEN
                     NCSTR=MIN(240,NCSTR)
                     WRITE(ICOUT,1010)ISTR(1:NCSTR)
                     CALL DPWRST('XXX','BUG ')
                   ELSEIF(ITYPE.EQ.'COPY')THEN
                     WRITE(IOUNIT,1020)ISTR(1:NCSTR)
                   ENDIF
                 ENDIF
                 NCSTR=0
                 ISTR=' '
                 GOTO1000
               ELSE
C
C                ADD CURRENT CHARACTER TO BUFFER.  FOR VIEWING
C                ON THE SCREEN, LIMIT BUFFER TO 240 CHARACTERS.
C                FOR WRITING TO EXTERNAL FILE, LIMIT BUFFER TO
C                255 CHARACTERS.
C
                 IF(NCSTR.LT.255)THEN
                   NCSTR=NCSTR+1
                   ISTR(NCSTR:NCSTR)=ICHR
                 ENDIF
               ENDIF
 1090        CONTINUE
             GOTO1000
 1099        CONTINUE
C
C            PROCESSING OF CLIPBOARD COMPLETED
C
             iSt = LocalUnlock(hData)
             iSt = CloseClipboard()
             IF(IERROR.EQ.'YES')GOTO9000
          ELSE
            IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1110)
 1110         FORMAT('***** NOTHING AVAILABLE FROM CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
            ENDIF
            IERROR='YES'
          ENDIF
        ELSEIF(ITYPE.EQ.'RUN')THEN
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** VIEW/COPY FROM SYSTEM CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR
 9021   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI3(IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--CLEAR THE SYSTEM CLIPBOARD.  THIS IS OPERATING
C              SYSTEM AND COMPILER DEPENDENT.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
C
      USE IFWIN
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
      INTEGER IST
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO
   81   FORMAT('IBUGS2,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       CLEAR THE CLIPBOARD
C
        IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
           iSt = OpenClipboard(NULL)
           iSt = EmptyClipboard()
           iSt = CloseClipboard()
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('***** CLIPBOARD CLEARED')
              CALL DPWRST('XXX','BUG ')
           ENDIF
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** CLEAR SYSTEM CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI4(ISTR,NCSTR,IOP,ICLOSE,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--WRITE A STRING TO THE SYSTEM CLIPBOARD.
C              SYSTEM AND COMPILER DEPENDENT.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
C
      USE IFWIN
C
      INTEGER NCSTR
C
      CHARACTER*(*) ISTR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
      INTEGER IST
      INTEGER HDATA
      INTEGER ILEN
      INTEGER PSTRING
      INTEGER PDUMMY
C
      SAVE HDATA
      SAVE PSTRING
      SAVE PDUMMY
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,NCSTR,IOP,ICLOSE
   81   FORMAT('IBUGS2,ISUBRO,NCSTR,IOP,ICLOSE = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       CLEAR THE CLIPBOARD
C
CCCCC   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
           IVAL=ICHAR(ISTR(NCSTR:NCSTR))
           IF(IVAL.EQ.0)THEN
             ILEN=NCSTR
           ELSE
             ILEN=NCSTR+1
             IF(ILEN.GT.255)ILEN=255
             ISTR(ILEN:ILEN)=CHAR(0)
           ENDIF
           IF(IOP.EQ.1)THEN
             iSt = OpenClipboard(NULL)
             iSt = EmptyClipboard()
           ENDIF
C
           PSTRING=LOC(ISTR)
           hData=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE,ilen)
           pdummy=GlobalLock(hData)
           CALL CopyMemory(pdummy,pstring,ilen)
           iSt = SetClipboardData(CF_TEXT,hData)
           iSt = GlobalUnlock(hData)
           IF(ICLOSE.EQ.1)THEN
             iSt = CloseClipboard()
           ENDIF
C
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('***** STRING WRITTEN TO CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
           ENDIF
CCCCC   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** WRITE STRING TO CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI5(XSCRT,NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--WRITE VARIABLES TO THE CLIPBOARD.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.12
C     ORIGINAL VERSION--DECEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
C
      USE IFWIN
C
      REAL XSCRT(*)
C
      INTEGER NVAR
      INTEGER NROW
      INTEGER MAXVAL
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZC.INC'
      CHARACTER*5000000 ISTR
      EQUIVALENCE (CGARBG(1),ISTR)
      INTEGER MAXCHR
      PARAMETER(MAXCHR=5000000)
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
      INTEGER IST
      INTEGER I
      INTEGER HDATA
      INTEGER ILEN
      INTEGER PSTRING
      INTEGER PDUMMY
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI5')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI5--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,NVAR,NROW,MAXVAL,MAXCHR
   81   FORMAT('IBUGS2,ISUBRO,NVAR,NROW,MAXVAL,MAXCHR = ',2(A4,2X),4I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       CLEAR THE CLIPBOARD
C
CCCCC   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
CCCCC      hClipboard = OpenClipboard(GETHWNDQQ(QWIN$FRAMEWINDOW))
           iSt = OpenClipboard(NULL)
           iSt = EmptyClipboard()
C
           ILEN=0
           ICNT=0
           ISTR=' '
C
           DO1000I=1,NROW
             DO1100J=1,NVAR
               ICNT=ICNT+1
               AVAL=XSCRT(ICNT)
               IF(ILEN.LT.MAXCHR-17)THEN
                 ISTRT=ILEN+1
                 ISTOP=ILEN+15
                 WRITE(ISTR(ISTRT:ISTOP),'(E15.7)')AVAL
                 ILEN=ILEN+15
                 ILEN=ILEN+1
                 ISTR(ILEN:ILEN)=' '
               ELSE
                 GOTO1009
               ENDIF
 1100        CONTINUE
             ILEN=ILEN+1
             ISTR(ILEN:ILEN)=CHAR(13)
             ILEN=ILEN+1
             ISTR(ILEN:ILEN)=CHAR(10)
C
 1000      CONTINUE
 1009      CONTINUE
           ILEN=ILEN+1
           IF(ILEN.GT.MAXCHR)ILEN=MAXCHR
           ISTR(ILEN:ILEN)=CHAR(0)
C
           PSTRING=LOC(ISTR)
           hData=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE,ilen)
           pdummy=GlobalLock(hData)
           CALL CopyMemory(pdummy,pstring,ilen)
           iSt = SetClipboardData(CF_TEXT,hData)
           iSt = GlobalUnlock(hData)
           iSt = CloseClipboard()
C
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1311)
 1311         FORMAT('***** VARIABLES WRITTEN TO CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
           ENDIF
CCCCC   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** WRITE VARIABLES TO CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI6(IOUNIT,ISTR,MAXCHR,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--WRITE THE CONTENTS OF A FILE TO THE CLIPBOARD.
C              SYSTEM AND COMPILER DEPENDENT.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.12
C     ORIGINAL VERSION--DECEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
C
      USE IFWIN
C
      CHARACTER*(*) ISTR
C
      INTEGER IOUNIT
      INTEGER MAXCHR
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      CHARACTER*255 ISTRIN
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
      INTEGER IST
      INTEGER I
      INTEGER HDATA
      INTEGER ILEN
      INTEGER PSTRING
      INTEGER PDUMMY
C
      SAVE HDATA
      SAVE PSTRING
      SAVE PDUMMY
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI6--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,IOUNIT
   81   FORMAT('IBUGS2,ISUBRO,IOUNIT = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       CLEAR THE CLIPBOARD
C
CCCCC   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
           iSt = OpenClipboard(NULL)
           iSt = EmptyClipboard()
C
           ILEN=0
           DO1000I=1,100000
             READ(IOUNIT,'(A255)',END=1099,ERR=1099)ISTRIN
C
             IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI6')THEN
               WRITE(ICOUT,1001)I,ISTRIN(1:255)
 1001          FORMAT('I,ISTRIN: ',I8,2X,A255)
               CALL DPWRST('XXX','BUG ')
             ENDIF
C
             DO1010J=255,1,-1
               IF(ISTRIN(J:J).NE.' ')THEN
                 DO1020K=1,J
                   IF(ILEN.LT.MAXCHR-3)THEN
                     ILEN=ILEN+1
                     ISTR(ILEN:ILEN)=ISTRIN(K:K)
                   ENDIF
 1020            CONTINUE
                 IF(ILEN.LT.MAXCHR-2)THEN
                   ILEN=ILEN+1
                   ISTR(ILEN:ILEN)=CHAR(13)
                   ILEN=ILEN+1
                   ISTR(ILEN:ILEN)=CHAR(10)
                 ENDIF
                 GOTO1000
               ENDIF
 1010        CONTINUE
C
             IF(ILEN.LT.MAXCHR-2)THEN
               ILEN=ILEN+1
               ISTR(ILEN:ILEN)=CHAR(13)
               ILEN=ILEN+1
               ISTR(ILEN:ILEN)=CHAR(10)
             ENDIF
C
 1000      CONTINUE
 1099      CONTINUE
           ILEN=ILEN+1
           IF(ILEN.GT.MAXCHR)ILEN=MAXCHR
           ISTR(ILEN:ILEN)=CHAR(0)
           PSTRING=LOC(ISTR)
           hData=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE,ilen)
           pdummy=GlobalLock(hData)
           CALL CopyMemory(pdummy,pstring,ilen)
           iSt = SetClipboardData(CF_TEXT,hData)
           iSt = GlobalUnlock(hData)
           iSt = CloseClipboard()
C
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('***** FILE WRITTEN TO CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
           ENDIF
CCCCC   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** WRITE FILE TO CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI6--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
#endif
#ifdef LINUX
      SUBROUTINE DPCLIP(XSCRT,MAXVAL,NVAL,NUME,PREAMV,ISKIP,IGRPAU,
     1                  IVLIST,IVLIS2,IAVANM,MAXRDV,
     1                  ITYPE,ISTR,NCSTR,
     1                  IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
C              SYSTEM AND COMPILER DEPENDENT.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.10
C     ORIGINAL VERSION--OCTOBER    2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
      REAL XSCRT(*)
C
      CHARACTER*4 IVLIST(MAXRDV)
      CHARACTER*4 IVLIS2(MAXRDV)
      CHARACTER*4 IAVANM
      CHARACTER*4 ITYPE
      CHARACTER*255 ISTR
      CHARACTER*4 IGRPAU
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
CCCCC CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
CCCCC CHARACTER*80 IATEMP
CCCCC CHARACTER*1  ICHR
CCCCC CHARACTER*1  ICHRN
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
CCCCC INTEGER IST
      INTEGER I
CCCCC INTEGER HDATA
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLIP')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLIP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGS2,ISUBRO,ITYPE,IGRPAU
   53   FORMAT('IBUGS2,ISUBRO,ITYPE,IGRPAU = ',3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,55)MAXVAL,NUME,ISKIP,PREAMV
   55   FORMAT('MAXVAL,NUME,ISKIP,PREAMV = ',3I10,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(ITYPE.EQ.'VARI')THEN
        DO110I=1,MAXVAL
          XSCRT(I)=CPUMIN
  110   CONTINUE
      ELSE
        ISTR=' '
      ENDIF
      NVAL=0
      NCSTR=0
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
CINTE   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
CINTE      iSt = OpenClipboard(NULL)
CINTE      hData = GetClipboardData(CF_TEXT)
CINTE      pText = LocalLock(hData)
C
CINTE      NLNRD=0
CINTE      NVAL=0
CINTE      ICNTR=0
C
C          IF HEADER LINES ARE SPECIFIED, CHECK FOR CR/LF UNTIL
C          ALL HEADER LINES READ.  CHECK FOR VARIABLE NAMES ON
C          LAST LINE, OR NEXT TO LAST LINE IF LAST LINE IS "----",
C          IF REQUESTED.
C
CINTE      IF(ISKIP.GT.0)THEN
C
CINTE        NVAR=0
CINTE        NLCURR=1
CINTE        IPTR1=1
CINTE        IPTR2=0
CINTE        IPTRP1=0
CINTE        IPTRP2=0
C
CI200        CONTINUE
CINTE        ICNTR=ICNTR+1
CINTE        ICHR=STEXT(ICNTR)(1:1)
CINTE        IVAL=ICHAR(ICHR)
CINTE        IF(IVAL.EQ.0)GOTO1099
CINTE        IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
CINTE          IPTR2=ICNTR-1
CINTE          ICHRN=STEXT(ICNTR+1)(1:1)
CINTE          IVALN=ICHAR(ICHRN)
CINTE          IF(IVALN.EQ.10 .OR. IVALN.EQ.13)THEN
CINTE            ICNTR=ICNTR+1
CINTE          ENDIF
CINTE          IF(NLCURR.EQ.ISKIP-1)THEN
CINTE            IPTRP1=IPTR1
CINTE            IPTRP2=IPTR2
CINTE          ELSEIF(NLCURR.EQ.ISKIP)THEN
CINTE            IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.
CINTE1              STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
CINTE              IPTR1=IPTRP1
CINTE              IPTR2=IPTRP2
CINTE            ENDIF
C
C                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
C                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
C                TREATED AS DELIMITERS.
C
CINTE            IF(ITYPE.EQ.'STRI')GOTO219
CINTE            NVAR=0
CINTE            ICHCN2=0
CINTE            DO210II=IPTR1,IPTR2
CINTE              ICHRN=STEXT(II)(1:1)
CINTE              IVAL2=ICHAR(ICHRN)
CINTE              IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.
CINTE1               ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
CINTE                IF(ICHCN2.EQ.0)GOTO210
CINTE                ISTOP=II
CINTE                NVAR=NVAR+1
CINTE                IF(NVAR.LE.MAXRDV)THEN
CINTE                  NCHAR=ISTOP-ISTRT+1
CINTE                  DO220JJ=1,MIN(NCHAR,4)
CINTE                    IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
CI220                  CONTINUE
CINTE                  IF(NCHAR.GE.5)THEN
CINTE                    DO230JJ=5,MIN(NCHAR,8)
CINTE                      IVLIS2(NVAR)(JJ-4:JJ-4)=
CINTE1                     STEXT(ISTRT+JJ-1)(1:1)
CI230                    CONTINUE
CINTE                  ENDIF
CINTE                ELSE
CINTE                  GOTO219
CINTE                ENDIF
CINTE                ICHCN2=0
CINTE              ELSE
CINTE                ICHCN2=ICHCN2+1
CINTE                IF(ICHCN2.EQ.1)ISTRT=II
CINTE                ICURR=II
CINTE              ENDIF
CI210            CONTINUE
C
CINTE            IF(ICHCN2.GT.0)THEN
CINTE              NVAR=NVAR+1
CINTE              IVLIST(NVAR)='    '
CINTE              IVLIS2(NVAR)='    '
CINTE              ISTOP=ICURR
CINTE              IF(NVAR.LE.MAXRDV)THEN
CINTE                NCHAR=ISTOP-ISTRT+1
CINTE                DO270JJ=1,MIN(NCHAR,4)
CINTE                  IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
CI270                CONTINUE
CINTE                IF(NCHAR.GE.5)THEN
CINTE                  DO280JJ=5,MIN(NCHAR,8)
CINTE                    IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
CI280                  CONTINUE
CINTE                ENDIF
CINTE              ENDIF
CINTE            ENDIF
C
CI219            CONTINUE
C
CINTE            GOTO1000
CINTE          ENDIF
CINTE          NLCURR=NLCURR+1
CINTE          IPTR1=ICNTR+1
CINTE        ENDIF
CINTE        GOTO200
C
CINTE      ELSEIF(ISKIP.LT.0)THEN
C
C            FOR SKIP AUTOMATIC, READ UNTIL A LINE STARTING WITH
C            "----" IS ENCOUNTERED.
C
CINTE        NVAR=0
CINTE        NLCURR=1
CINTE        IPTR1=1
CINTE        IPTR2=0
CINTE        IPTR3=1
CINTE        IPTRP1=0
CINTE        IPTRP2=0
C
CI300        CONTINUE
CINTE        ICNTR=ICNTR+1
CINTE        ICHR=STEXT(ICNTR)(1:1)
CINTE        IVAL=ICHAR(ICHR)
CINTE        IF(IVAL.EQ.0)GOTO1099
CINTE        IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
CINTE          IPTRP1=IPTR1
CINTE          IPTRP2=IPTR2
CINTE          IPTR1=IPTR3
CINTE          IPTR2=ICNTR-1
CINTE          ICHRN=STEXT(ICNTR+1)(1:1)
CINTE          IVALN=ICHAR(ICHRN)
CINTE          IF(IVALN.EQ.10 .OR. IVALN.EQ.13)THEN
CINTE            ICNTR=ICNTR+1
CINTE          ENDIF
CINTE          IPTR3=ICNTR+1
C
C              CHECK FOR "----"
C
CINTE          IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.
CINTE1            STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
CINTE            IPTR1=IPTRP1
CINTE            IPTR2=IPTRP2
C
C                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
C                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
C                TREATED AS DELIMITERS.
C
CINTE            IF(ITYPE.EQ.'STRI')GOTO319
CINTE            NVAR=0
CINTE            ICHCN2=0
CINTE            DO310II=IPTR1,IPTR2
CINTE              ICHRN=STEXT(II)(1:1)
CINTE              IVAL2=ICHAR(ICHRN)
CINTE              IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.
CINTE1               ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
CINTE                IF(ICHCN2.EQ.0)GOTO310
CINTE                ISTOP=II
CINTE                NVAR=NVAR+1
CINTE                IVLIST(NVAR)='   '
CINTE                IVLIS2(NVAR)='   '
CINTE                IF(NVAR.LE.MAXRDV)THEN
CINTE                  NCHAR=ISTOP-ISTRT+1
CINTE                  DO330JJ=1,MIN(NCHAR,4)
CINTE                    IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
CI330                  CONTINUE
CINTE                  IF(NCHAR.GE.5)THEN
CINTE                    DO340JJ=5,MIN(NCHAR,8)
CINTE                      IVLIS2(NVAR)(JJ-4:JJ-4)=
CINTE1                     STEXT(ISTRT+JJ-1)(1:1)
CI340                    CONTINUE
CINTE                  ENDIF
CINTE                ELSE
CINTE                  GOTO319
CINTE                ENDIF
CINTE                ICHCN2=0
CINTE              ELSE
CINTE                ICHCN2=ICHCN2+1
CINTE                IF(ICHCN2.EQ.1)ISTRT=II
CINTE                ICURR=II
CINTE              ENDIF
CI310            CONTINUE
C
CINTE            IF(ICHCN2.GT.0)THEN
CINTE              NVAR=NVAR+1
CINTE              IVLIST(NVAR)='    '
CINTE              IVLIS2(NVAR)='    '
CINTE              ISTOP=ICURR
CINTE              IF(NVAR.LE.MAXRDV)THEN
CINTE                NCHAR=ISTOP-ISTRT+1
CINTE                DO370JJ=1,MIN(NCHAR,4)
CINTE                  IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
CI370                CONTINUE
CINTE                IF(NCHAR.GE.5)THEN
CINTE                  DO380JJ=5,MIN(NCHAR,8)
CINTE                    IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
CI380                  CONTINUE
CINTE                ENDIF
CINTE              ENDIF
CINTE            ENDIF
C
CI319            CONTINUE
C
CINTE            GOTO1000
CINTE          ENDIF
CINTE          NLCURR=NLCURR+1
CINTE        ENDIF
CINTE        GOTO300
C
CINTE      ENDIF
C
C1000      CONTINUE
C
C            STRING CASE.  FOR NOW, READ FIRST RECORD AS A SINGLE
C            STRING.
C
CINTE        IF(ITYPE.EQ.'STRI')THEN
CINTE          DO2090II=1,10000
CINTE            ICNTR=ICNTR+1
CINTE            ICHR=STEXT(ICNTR)(1:1)
CINTE            IVAL=ICHAR(ICHR)
CINTE            IF(IVAL.EQ.0)THEN
CINTE              GOTO1099
CINTE            ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
CINTE              GOTO1099
CINTE            ELSE
CINTE              IF(NCSTR.LT.255)THEN
CINTE                NCSTR=NCSTR+1
CINTE                ISTR(NCSTR:NCSTR)=ICHR
CINTE              ENDIF
CINTE            ENDIF
C2090          CONTINUE
CINTE          GOTO1099
CINTE        ENDIF
C
CINTE        ICHCNT=0
CINTE        DO1090II=1,80
C
CINTE          ICNTR=ICNTR+1
CINTE          ICHR=STEXT(ICNTR)(1:1)
CINTE          IVAL=ICHAR(ICHR)
C
C              NULL CHARACTER IMPLIES END OF CLIPBOARD DATA
C
CINTE          IF(IVAL.EQ.0)THEN
CINTE            IF(ICHCNT.EQ.0)GOTO1099
CINTE            GOTO1080
C
C              SPACE INTERPRETED AS A DELIMITER THAT DOES NOT ALLOW
C              FOR "MISSING" COLUMNS
C
CINTE          ELSEIF(IVAL.EQ.32)THEN
CINTE            IF(ICHCNT.GT.0)GOTO1080
C
C              COMMA AND TAB ARE ALL INTERPRETED AS FIELD DELIMITERS
C              THAT ALLOW FOR "MISSING" COLUMNS.  THAT IS, IF SUCCESSIVE
C              COMMAS OR TABS ARE ENCOUNTERED, INSERT A "MISSING VALUE".
C
CINTE          ELSEIF(IVAL.EQ.9 .OR. ICHR.EQ.',')THEN
CINTE            IF(ICHCNT.GT.0)THEN
CINTE              GOTO1080
CINTE            ELSE
CINTE              NVAL=NVAL+1
CINTE              IF(NVAL.GT.MAXVAL)THEN
CINTE                WRITE(ICOUT,1003)
CINTE                CALL DPWRST('XXX','BUG ')
CINTE                WRITE(ICOUT,1005)
CINTE                CALL DPWRST('XXX','BUG ')
CINTE                WRITE(ICOUT,1007)MAXVAL
CINTE                CALL DPWRST('XXX','BUG ')
CINTE                IERROR='YES'
CINTE                GOTO1099
CINTE              ENDIF
CINTE              XSCRT(NVAL)=PREAMV
CINTE              GOTO1000
CINTE            ENDIF
C
C              LINE FEED/CARRIAGE RETURN SIGNIFY END OF RECORD
C              DELIMITERS.  IF NUMBER OF VARIABLES NOT SPECIFIED
C              ON READ, CHECK NUMBER OF VALUES READ ON FIRST LINE.
C
CINTE          ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
CINTE            ICHRN=STEXT(ICNTR+1)(1:1)
CINTE            IVALN=ICHAR(ICHRN)
CINTE            IF(IVALN.EQ.10 .OR. IVALN.EQ.13)THEN
CINTE              ICNTR=ICNTR+1
CINTE            ENDIF
C
CINTE            IF(NLNRD.EQ.0)THEN
CINTE              IF(NUME.EQ.0)THEN
CINTE                NUME=NVAL
CINTE                IF(ICHCNT.GT.0)NUME=NVAL+1
CINTE              ENDIF
CINTE            ENDIF
CINTE            NLNRD=NLNRD+1
CINTE            IF(ICHCNT.GT.0)GOTO1080
C
C              ADD CURRENT CHARACTER TO TEMPORARY STRING
C
CINTE          ELSE
CINTE            ICHCNT=ICHCNT+1
CINTE            IATEMP(ICHCNT:ICHCNT)=ICHR
CINTE          ENDIF
C
C1090        CONTINUE
CINTE        IERROR='YES'
CINTE        GOTO1099
C
C            PROCESS CURRENT FIELD
C
C1080        CONTINUE
CINTE        CALL EXTREA(IATEMP,ICHCNT,AVALUE,IBUGS2,ISUBRO,IERROR)
CINTE        IF(IERROR.EQ.'YES')GOTO1099
CINTE        NVAL=NVAL+1
CINTE        IF(NVAL.GT.MAXVAL)THEN
CINTE          WRITE(ICOUT,1003)
C1003          FORMAT('***** ERROR READING FROM CLIPBOARD')
CINTE          CALL DPWRST('XXX','BUG ')
CINTE          WRITE(ICOUT,1005)
C1005          FORMAT('      MAXIMUM NUMBER OF VALUES EXCEEDED.')
CINTE          CALL DPWRST('XXX','BUG ')
CINTE          WRITE(ICOUT,1007)MAXVAL
C1007          FORMAT('      MAXIMUM NUMBER OF VALUES  = ',I10)
CINTE          CALL DPWRST('XXX','BUG ')
CINTE          IERROR='YES'
CINTE          GOTO1099
CINTE        ENDIF
CINTE        XSCRT(NVAL)=AVALUE
CINTE        IF(IVAL.EQ.0)GOTO1099
CINTE        GOTO1000
C
C            PROCESSING OF CLIPBOARD COMPLETED
C
C1099      CONTINUE
CINTE      iSt = LocalUnlock(hData)
CINTE      iSt = CloseClipboard()
CINTE      IF(IERROR.EQ.'YES')GOTO9000
CINTE   ELSE
CINTE     WRITE(ICOUT,1011)
C1011     FORMAT('***** NOTHING AVAILABLE FROM CLIPBOARD')
CINTE     CALL DPWRST('XXX','BUG ')
CINTE     IERROR='YES'
CINTE   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2011)
 2011   FORMAT('***** READING FROM SYSTEM CLIPBOARD NOT SUPPORTED ',
     1         'ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLIP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLIP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR
 9021   FORMAT('IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR = ',
     1         2(A4,2X),2I10,2I5)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO9025I=1,NCSTR
            WRITE(ICOUT,9027)I,ISTR(I:I)
 9027       FORMAT('I,ISTR(I:I) = ',I5,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
        IF(NVAL.GT.0)THEN
          DO9035I=1,NVAL
            WRITE(ICOUT,9037)I,IVLIST(I),IVLIS2(I)
 9037       FORMAT('I,IVLIST(I),IVLIS2(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI2(ITYPE,IOUNIT,IHELMX,ILINRD,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
C              SYSTEM AND COMPILER DEPENDENT.
C
C              THE DPCLIP COMMAND IS USED TO EITHER READ A LIST
C              VARIABLES OR A SINGLE STRING.
C
C              DPCLI2 IS USED TO:
C
C                1. VIEW THE CONTENTS OF THE CLIPBOARD
C                2. TO COPY THE CONTENTS OF THE CLIPBOARD
C                   TO A USER SPECIFIED FILE.
C                3. TO EXTRACT THE NUMBER OF LINES IN THE CLIPBOARD.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
CINTE USE IFWIN
C
      CHARACTER*4 ITYPE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*255 ISTR
CCCCC CHARACTER*1  ICHR
      CHARACTER*4  IRESP
      CHARACTER*6  IFORMT
C
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZI.INC'
C
      INTEGER ISTR2(MAXOBV)
      EQUIVALENCE (IGARBG(IIGAR1),ISTR2(1))
C
C     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
C
CINTE INTEGER IST
CINTE INTEGER I
CINTE INTEGER HDATA
CINTE CHARACTER*1 STEXT(*)
CINTE POINTER (PTEXT, STEXT)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI2')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,ITYPE,IHELMX
   81   FORMAT('IBUGS2,ISUBRO,ITYPE= ',3(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      ISTR=' '
      NCSTR=0
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C         VIEW THE CONTENTS OF THE CLIPBOARD OR COPY TO A
C         USER-SPECIFIED FILE
C
CINTE   IF(ITYPE.EQ.'VIEW' .OR. ITYPE.EQ.'COPY')THEN
C
C         VIEW THE CONTENTS OF THE CLIPBOARD
C
CINTE     IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
CINTE        iSt = OpenClipboard(NULL)
CINTE        hData = GetClipboardData(CF_TEXT)
CINTE        pText = LocalLock(hData)
C
CINTE        ILINRD=0
CINTE        ICNTR=0
C
C1000        CONTINUE
C
C             READ UNTIL NULL STRING OR CR/LF ENCOUNTERED.
C
CINTE        DO1090II=1,10000
CINTE          ICNTR=ICNTR+1
CINTE          ICHR=STEXT(ICNTR)(1:1)
CINTE          IVAL=ICHAR(ICHR)
CINTE          IF(IVAL.EQ.0)THEN
C
C                END OF STRING ENCOUNTERED
C
CINTE            IF(NCSTR.GE.1)THEN
CINTE              IF(ITYPE.EQ.'VIEW')THEN
CINTE                NCSTR=MIN(240,NCSTR)
CINTE                WRITE(ICOUT,1010)ISTR(1:NCSTR)
C1010                FORMAT(A)
CINTE                CALL DPWRST('XXX','BUG ')
CINTE              ELSEIF(ITYPE.EQ.'COPY')THEN
CINTE                WRITE(IOUNIT,1020)ISTR(1:NCSTR)
C1020                FORMAT(A)
CINTE              ENDIF
CINTE            ENDIF
CINTE            NCSTR=0
CINTE            ISTR=' '
CINTE            GOTO1099
CINTE          ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
C
C                END OF CURRENT RECORD
C
CINTE            ILINRD=ILINRD+1
CINTE            IF(ILINRD.GT.IHELMX .AND. ITYPE.EQ.'VIEW')THEN
CINTE              WRITE(ICOUT,1051)
C1051              FORMAT('                            MORE...?')
CINTE              CALL DPWRST('XXX','WRIT')
CINTE              READ(IRD,1052)IRESP
C1052              FORMAT(A4)
CINTE              IF(IRESP.EQ.'N')IRESP='NO'
CINTE              IF(IRESP.EQ.'n')IRESP='NO'
CINTE              IF(IRESP.EQ.'no')IRESP='NO'
CINTE              IF(IRESP.EQ.'NO')GOTO1099
CINTE              ILINRD=0
CINTE            ENDIF
CINTE            IF(NCSTR.GE.1)THEN
CINTE              IF(ITYPE.EQ.'VIEW')THEN
CINTE                NCSTR=MIN(240,NCSTR)
CINTE                WRITE(ICOUT,1010)ISTR(1:NCSTR)
CINTE                CALL DPWRST('XXX','BUG ')
CINTE              ELSEIF(ITYPE.EQ.'COPY')THEN
CINTE                WRITE(IOUNIT,1020)ISTR(1:NCSTR)
CINTE              ENDIF
CINTE            ENDIF
CINTE            NCSTR=0
CINTE            ISTR=' '
CINTE            GOTO1000
CINTE          ELSE
C
C                ADD CURRENT CHARACTER TO BUFFER.  FOR VIEWING
C                ON THE SCREEN, LIMIT BUFFER TO 240 CHARACTERS.
C                FOR WRITING TO EXTERNAL FILE, LIMIT BUFFER TO
C                255 CHARACTERS.
C
CINTE            IF(NCSTR.LT.255)THEN
CINTE              NCSTR=NCSTR+1
CINTE              ISTR(NCSTR:NCSTR)=ICHR
CINTE            ENDIF
CINTE          ENDIF
C1090        CONTINUE
CINTE        GOTO1000
C1099        CONTINUE
C
C            PROCESSING OF CLIPBOARD COMPLETED
C
CINTE        iSt = LocalUnlock(hData)
CINTE        iSt = CloseClipboard()
CINTE        IF(IERROR.EQ.'YES')GOTO9000
CINTE     ELSE
CINTE       WRITE(ICOUT,1110)
C1110       FORMAT('***** NOTHING AVAILABLE FROM CLIPBOARD')
CINTE       CALL DPWRST('XXX','BUG ')
CINTE       IERROR='YES'
CINTE     ENDIF
CINTE   ELSEIF(ITYPE.EQ.'RUN')THEN
CINTE   ENDIF
      ELSEIF(IOPSY1.EQ.'UNIX')THEN
        MAXCHR=MAXOBV
        NVAL=0
        ILINRD=0
        IERR=0
C
CCCCC   CALL XFETCH(ISTR2,NVAL,MAXCHR,IERR)
C
        IF(ITYPE.EQ.'VIEW' .OR. ITYPE.EQ.'COPY')THEN
C
          IF(IERR.EQ.1)THEN
            WRITE(ICOUT,1001)
 1001       FORMAT('***** ERROR IN VIEW CLIPBOARD')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1003)
 1003       FORMAT('      THE X11 DEVICE IS NOT CURRENTLY OPEN SO ',
     1             'THE X11 CLIPBOARD IS NOT AVAILABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(NVAL.GT.0)THEN
            IPTR=0
            ISTRT=1
            ISTOP=0
 1010       CONTINUE
              IPTR=IPTR+1
              ISTOP=0
              IVAL=ISTR2(IPTR)
C
              IF(IVAL.EQ.0)THEN
C
C               END OF STRING ENCOUNTERED
C
                ISTOP=IPTR-1
                IF(ISTOP.GE.ISTRT)THEN
                  NCHAR=ISTOP-ISTRT+1
                  IF(ITYPE.EQ.'VIEW')THEN
                    IF(NCHAR.GT.240)NCHAR=240
                  ELSE
                    IF(NCHAR.GT.255)NCHAR=255
                  ENDIF
                  IF(NCHAR.GE.1)THEN
                    ICNT=0
                    DO1015J=ISTRT,ISTRT+NCHAR-1
                      ICNT=ICNT+1
                      ISTR(ICNT:ICNT)=CHAR(ISTR2(J))
 1015               CONTINUE
                    IFORMT='(A   )'
                    WRITE(IFORMT(3:5),'(I3)')NCHAR
                    IF(ITYPE.EQ.'VIEW')THEN
                      WRITE(ICOUT,IFORMT)ISTR(1:NCHAR)
                      CALL DPWRST('XXX','BUG ')
                    ELSE
                      WRITE(IOUNIT,IFORMT)ISTR(1:NCHAR)
                    ENDIF
                  ENDIF
                ENDIF
C
                GOTO9000
C
              ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
C
C               END OF RECORD ENCOUNTERED
C
                ISTOP=IPTR-1
                ILINRD=ILINRD+1
C
                IF(ILINRD.GT.IHELMX .AND. ITYPE.EQ.'VIEW')THEN
                  WRITE(ICOUT,1051)
 1051             FORMAT('                            MORE...?')
                  CALL DPWRST('XXX','WRIT')
                  READ(IRD,1052)IRESP
 1052             FORMAT(A4)
                  IF(IRESP.EQ.'N')IRESP='NO'
                  IF(IRESP.EQ.'n')IRESP='NO'
                  IF(IRESP.EQ.'no')IRESP='NO'
                  IF(IRESP.EQ.'NO')GOTO9000
                  ILINRD=0
                ENDIF
C
                IF(IVAL.EQ.10 .AND. ISTR2(IPTR+1).EQ.13)IPTR=IPTR+1
                IF(IVAL.EQ.13 .AND. ISTR2(IPTR+1).EQ.10)IPTR=IPTR+1
C
                IF(ISTOP.GE.ISTRT)THEN
                  NCHAR=ISTOP-ISTRT+1
                  IF(ITYPE.EQ.'VIEW')THEN
                    IF(NCHAR.GT.240)NCHAR=240
                  ELSE
                    IF(NCHAR.GT.255)NCHAR=255
                  ENDIF
                  IF(NCHAR.GE.1)THEN
                    ICNT=0
                    DO1020J=ISTRT,ISTRT+NCHAR-1
                      ICNT=ICNT+1
                      ISTR(ICNT:ICNT)=CHAR(ISTR2(J))
 1020               CONTINUE
                    IFORMT='(A   )'
                    WRITE(IFORMT(3:5),'(I3)')NCHAR
                    IF(ITYPE.EQ.'VIEW')THEN
                      WRITE(ICOUT,IFORMT)ISTR(1:NCHAR)
                      CALL DPWRST('XXX','BUG ')
                    ELSE
                      WRITE(IOUNIT,IFORMT)ISTR(1:NCHAR)
                    ENDIF
                  ENDIF
                ENDIF
                ISTRT=IPTR+1
              ENDIF
              IF(IPTR.GE.NVAL)GOTO9000
              GOTO1010
          ELSE
            GOTO9000
          ENDIF
        ELSEIF(ITYPE.EQ.'NLIN')THEN
C
          IF(IERR.EQ.1)THEN
            WRITE(ICOUT,3001)
 3001       FORMAT('***** ERROR IN PROBE CLIPBOARD LINES')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1003)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
C
          IF(NVAL.GT.0)THEN
            IPTR=0
 3010       CONTINUE
              IPTR=IPTR+1
              IF(ISTR2(IPTR).EQ.10)THEN
                IF(ISTR2(IPTR+1).EQ.13)IPTR=IPTR+1
                ILINRD=ILINRD+1
              ELSEIF(ISTR2(IPTR).EQ.13)THEN
                IF(ISTR2(IPTR+1).EQ.10)IPTR=IPTR+1
                ILINRD=ILINRD+1
              ENDIF
              IF(IPTR.GE.NVAL)GOTO9000
              GOTO3010
          ELSE
            GOTO9000
          ENDIF
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** VIEW/COPY FROM SYSTEM CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR
 9021   FORMAT('IERROR = ',A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI3(IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--CLEAR THE SYSTEM CLIPBOARD.  THIS IS OPERATING
C              SYSTEM AND COMPILER DEPENDENT.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
C     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
C     BEFORE ANY OTHER DECLARATIONS
CINTE USE IFWIN
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI3')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI3--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO
   81   FORMAT('IBUGS2,ISUBRO = ',A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       CLEAR THE CLIPBOARD
C
CINTE   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
CINTE      iSt = OpenClipboard(NULL)
CINTE      iSt = EmptyClipboard()
CINTE      iSt = CloseClipboard()
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('***** CLIPBOARD CLEARED')
              CALL DPWRST('XXX','BUG ')
           ENDIF
CINTE   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** CLEAR SYSTEM CLIPBOARD NOT ',
     1         'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI4(ISTR,NCSTR,IOP,ICLOSE,IBUGS2,ISUBRO,IERROR)
C
C     PURPOSE--WRITE A STRING TO THE SYSTEM CLIPBOARD.
C              SYSTEM AND COMPILER DEPENDENT.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2014.11
C     ORIGINAL VERSION--NOVEMBER   2014.
C
C-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
C
      INTEGER NCSTR
C
      CHARACTER*(*) ISTR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI4')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCLI4--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,81)IBUGS2,ISUBRO,NCSTR,IOP,ICLOSE
   81   FORMAT('IBUGS2,ISUBRO,NCSTR,IOP,ICLOSE = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI4--')
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GE.1)THEN
          WRITE(ICOUT,9013)ISTR(1:MIN(80,NCSTR))
 9013     FORMAT('ISTR(1) = 'A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI5(XSCRT,NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR)
C
      REAL XSCRT(*)
C
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,52)NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR
   52   FORMAT('NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR = ',
     1         I8,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)XSCRT(1),XSCRT(2),XSCRT(3)
   54   FORMAT('XSCRT(1),XSCRT(2),XSCRT(3) = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCLI6(IOUNIT,IBUGS2,ISUBRO,IERROR)
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
C
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
C
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,52)IOUNIT,IBUGS2,ISUBRO,IERROR
   52   FORMAT('IOUNIT,IBUGS2,ISUBRO,IERROR = ',I8,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
#endif
      SUBROUTINE DPCDIR(IANS,IANSLC,IWIDTH,
     1                  IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1                  IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1                  IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--CHANGE THE CURRENT DIRECTORY.  NOTE THAT THIS COMMAND
C              IS SITE AND HOST DEPENDENT.  IT IS PROVIODED AS A
C              CONVENIENCE FUNCTION.  FOR EXAMPLE, THE WINDOWS NT
C              VERSION SETS THE CURRENT DIRECTORY TO THE DIRECTORY
C              WHERE THE DATAPLOT EXECUTABLE RESIDES.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--97.8
C     ORIGINAL VERSION--AUGUST     1997.
C
C-----NON-COMMON VARIABLES -------------------------------------------
C
#ifdef INTEL
      USE MSFLIB
      LOGICAL ISTATUS
#endif
CCCCC LOGICAL IRESLT
      CHARACTER*4 IANS
      CHARACTER*4 IANSLC
C
      CHARACTER*4 ITEXTE
      CHARACTER*4 ITEXTF
C
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
C
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      CHARACTER*4 IFUNC
      CHARACTER*1 IREPCH
C
      DIMENSION IANS(*)
      DIMENSION IANSLC(*)
C
      PARAMETER(MAXCH=256)
      DIMENSION ITEXTE(MAXCH)
      DIMENSION ITEXTF(MAXCH)
      CHARACTER*256 ITEXT2
      CHARACTER*256 ITEXT3
C
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
C
      IFOUND='NO'
      IERROR='NO'
C
      ITEXT2=' '
      ITEXT3=' '
      J2=0
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'CD  ')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IWIDTH,NUMNAM
   53   FORMAT('IBUGD2,ISUBRO,IWIDTH,NUMNAM = ',2(A4,2X),2I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANS(I),I=1,IWIDTH)
   54   FORMAT('(IANS(I),I=1,IWIDTH) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        DO76I=1,NUMNAM
          WRITE(ICOUT,77)I,IHNAME(I),IHNAM2(I),IUSE(I),
     1                   IVALUE(I),VALUE(I)
   77     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)= ',
     1           I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   76   CONTINUE
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (CD)  AND ITS LOCATION             **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
C  CHECK FOR "CD" FIRST
C
      DO1115I=1,IWIDTH-1
        ISTART=I+2
        IF(IANS(I).EQ.'C'.AND.IANS(I+1).EQ.'D'.AND.
     1     IANS(I+2).EQ.' ')GOTO1190
 1115 CONTINUE
C
C     NO MATCH
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCD--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL CD')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1190 CONTINUE
C
C               *******************************************************
C               **  STEP 1.2--                                       **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
C               *******************************************************
C
      IFOUND='YES'
C
      ISTOP=0
      IF(ISTART.LE.IWIDTH)THEN
        DO1220I=ISTART,IWIDTH
          IREV=IWIDTH-I+ISTART
          IF(IANS(IREV).NE.' ')THEN
            ISTOP=IREV
            GOTO1229
          ENDIF
 1220   CONTINUE
 1229   CONTINUE
      ENDIF
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      NCTEX=0
      IF(ISTART.LE.ISTOP .AND. ISTOP.GT.0)THEN
        ITEMP=ISTOP-ISTART+1
        IF(ITEMP.GT.MAXCH)ITEMP=MAXCH
        ISTOP=ISTART+ITEMP-1
C
        J=0
        DO1310I=ISTART,ISTOP
          J=J+1
          J2=J
          ITEXTE(J)=IANS(I)
          ITEXTF(J)=IANSLC(I)
 1310   CONTINUE
        NCTEX=J2
      ENDIF
C
C               ******************************************************
C               **  STEP 1.4--                                      **
C               **  CALL THE SUBROUTINE DPREPL                      **
C               **  WHICH WILL SCAN THE STRING FOR ALL OCCURRANCES  **
C               **  OF THE SUBSTRING VALU()                         **
C               **  AND REPLACE THEM BY THEIR LITERAL VALUES.       **
C               ******************************************************
C
      NCTEXT=NCTEX
      IF(NCTEXT.GE.1)CALL DPREPL(ITEXTE,NCTEXT,
     1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
     1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
     1IBUGD2,IERROR)
      IF(NCTEXT.GE.1)THEN
        DO1510I=1,NCTEXT
          ITEXT2(I:I)=ITEXTE(I)(1:1)
 1510   CONTINUE
      ENDIF
C
      NCTEXT=NCTEX
CCCCC IF(NCTEXT.GE.1)CALL DPREPL(ITEXTF,NCTEXT,
CCCCC1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
CCCCC1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
CCCCC1IBUGD2,IERROR)
      IF(NCTEXT.GE.1)THEN
        DO1610I=1,NCTEXT
          ITEXT3(I:I)=ITEXTF(I)(1:1)
 1610   CONTINUE
      ENDIF
C
C               ********************************
C               **  STEP 2--                  **
C               **  STEP THROUGH EACH HOST    **
C               ********************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
C
C       ****************************************************
C       *  IBM/PC 386 - MICROSOFT WINDOWS 95/NT COMPILER   *
C       ****************************************************
C
#ifdef INTEL
        ISTATUS=.TRUE.
        DO2510I=1,NCTEXT
          ISTART=I
          IF(ITEXT3(I:I).NE.' ')GOTO2519
 2510   CONTINUE
 2519   CONTINUE
        DO2520I=NCTEXT,1,-1
          ISTOP=I
          IF(ITEXT3(I:I).NE.' ')GOTO2529
 2520   CONTINUE
 2529   CONTINUE
        ISTATUS=.FALSE.
        ISTATUS=CHANGEDIRQQ(ITEXT3(ISTART:ISTOP))
        IF(ISTATUS)THEN
          WRITE(ICOUT,2501)
 2501     FORMAT('THE CURRENT DIRECTORY HAS BEEN CHANGED TO ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2502)ITEXT3(1:80)
 2502     FORMAT(5X,A80)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,2503)
 2503     FORMAT('*****WARNING: DATAPLOT WAS UNSUCCESSFUL IN ',
     1           'CHANGING THE CURRENT DIRECTORY')
          CALL DPWRST('XXX','BUG ')
        ENDIF
#endif
        GOTO9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
C
C       *********************************************
C       *  UNIX/LINUX - USE CHDIR CALL              *
C       *********************************************
C
#ifdef LINUX
        DO2310I=1,NCTEXT
          ISTART=I
          IF(ITEXT3(I:I).NE.' ')GOTO2319
 2310   CONTINUE
 2319   CONTINUE
        DO2320I=NCTEXT,1,-1
          ISTOP=I
          IF(ITEXT3(I:I).NE.' ')GOTO2329
 2320   CONTINUE
 2329   CONTINUE
C
        IRESLT=0
        IRESLT=CHDIR(ITEXT3(ISTART:ISTOP))
        IF(IRESLT.EQ.0)THEN
          WRITE(ICOUT,2301)
 2301     FORMAT('THE CURRENT DIRECTORY HAS BEEN CHANGED TO ')
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2302)ITEXT3(1:80)
 2302     FORMAT(A80)
          CALL DPWRST('XXX','BUG ')
        ELSE
          WRITE(ICOUT,2303)
 2303     FORMAT('*****WARNING: DATAPLOT WAS UNSUCCESSFUL IN ',
     1           'CHANGING THE CURRENT DIRECTORY')
          CALL DPWRST('XXX','BUG ')
        ENDIF
#endif
        GOTO9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
C
C       *********************************************************
C       *  CDC - NOS/VE OPERATING CD  EM.  USE "SCLCMD" TO PASS *
C       *  COMMANDS TO THE OPERATING CD  EM.                    *
C       *  DATAPLOT WILL DO NO ERROR CHECKING ON THE COMMAND    *
C       *********************************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2111)
 2111   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2112)
 2112   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2113)
 2113   FORMAT('      HAS NOT YET BEEN IMPLEMEMNTED FOR THE ',
     1         'NOS/VE VERSION')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
C
C       **********************************************
C       *  VAX/VMS - LEFT TO IMPLEMENTOR             *
C       **********************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2211)
 2211   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)
 2212   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2213)
 2213   FORMAT('      HAS NOT YET BEEN IMPLEMEMNTED FOR THE ',
     1         'VAX/VMS VERSION')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG ')THEN
C
C       *****************************************
C       *  IBM/PC 386 - OTG COMPILER            *
C       *****************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
 2411   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2413)
 2413   FORMAT('      HAS NOT YET BEEN IMPLEMEMNTED FOR THE ',
     1         'OTG VERSION')
        CALL DPWRST('XXX','BUG ')
        GOTO9000
      ELSE
C
C       *********************************************************
C       *  OTHER   - LEFT TO IMPLEMENTOR                        *
C       *********************************************************
C
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8012)
 8012   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      HAS NOT YET BEEN DONE FOR THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      COMPUTER/MODEL/OP-SYS/COMPILER/SITE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)IHOST1
 8021   FORMAT(' HOST     = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8022)IHMOD1
 8022   FORMAT(' MODEL    = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8023)IOPSY1
 8023   FORMAT(' OP-SYS   = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8024)ICOMPI
 8024   FORMAT(' COMPILER = ',A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8025)ISITE
 8025   FORMAT(' SITE     = ',A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'CD  ')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR,NCTEX,NCTEXT
 9015   FORMAT('IFOUND,IERROR,NCTEX,NCTEXT  = ',2(A4,2X),2I8,2X,A1)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)(ITEXTE(I),I=1,NCTEX)
 9016   FORMAT('(ITEXTE(I),I =1,NCTEX) = ',25A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,NCTEXT)
 9018   FORMAT('(ITEXT2(I),I=1,NCTEXT) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPPWD(CURDIR,MAXNCH,NLAST,IBUGS2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--RETURN CURRENT WORKING DIRECTORY.  THIS ROUTINE
C              UTILIZES THE "GETCWD" LIBRARY CALL.  THIS IS NOT
C              PART OF THE FORTRAN 90 STANDARD, BUT IT SHOULD BE
C              AVAILABLE ON MOST CURRENTLY SUPPORTED PLATFORMS.
C
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899-8980
C                 PHONE--301-975-2855
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1990)
C     VERSION NUMBER--2011/1
C     ORIGINAL VERSION--JANUARY   2011.
C
C-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
C
      CHARACTER (LEN=*) :: CURDIR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
C-----COMMON----------------------------------------------------------
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='YES'
      IERROR='NO'
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'PPWD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPPWD')
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      CURDIR=' '
      CALL GETCWD(CURDIR)
      NLAST=MAXNCH
C
      DO100I=MAXNCH,1,-1
        IF(CURDIR(I:I).NE.' ')THEN
          NLAST=I
          GOTO109
        ENDIF
  100 CONTINUE
  109 CONTINUE
C
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,111)CURDIR(1:NLAST)
  111   FORMAT('THE CURRENT WORKING DIRECTORY IS: ',A)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'PPWD')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('AT THE END OF DPPWD')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9053)CURDIR
 9053   FORMAT('CURDIR = ',A)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPRM(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--DELETE FILES OR DIRECTORIES USING OPERATING SYSTEM
C              COMMAND.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2019.09
C     ORIGINAL VERSION--SEPTEMBER  2019.
C
C-----NON-COMMON VARIABLES -------------------------------------------
C
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
      CHARACTER*4 ICASE
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'DPRM')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IWIDTH
   53   FORMAT('IBUGD2,ISUBRO,IWIDTH = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANSLC(I),I=1,IWIDTH)
   54   FORMAT('(IANSLC(I),I=1,IWIDTH) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (CD)  AND ITS LOCATION             **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
C  CHECK FOR "RM" FIRST
C
      ISTRT=1
      DO1115I=1,IWIDTH-1
        IF(IANS(I).EQ.'R'.AND.IANS(I+1).EQ.'M'.AND.
     1     IANS(I+2).EQ.' ')THEN
          ICASE='FILE'
          ISTRT=I+3
          GOTO1190
        ELSEIF(IANS(I).EQ.'R'   .AND. IANS(I+1).EQ.'M' .AND.
     1         IANS(I+2).EQ.'D' .AND. IANS(I+3).EQ.'I' .AND.
     1         IANS(I+4).EQ.'R' .AND. IANS(I+5).EQ.' ')THEN
          ICASE='DIRE'
          ISTRT=I+6
          GOTO1190
        ENDIF
 1115 CONTINUE
C
C     NO MATCH
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN RM (DPRM)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO RM')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1190 CONTINUE
C
C               *******************************************************
C               **  STEP 1.2--                                       **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
C               *******************************************************
C
      IFOUND='YES'
C
      ISTOP=0
      DO1220I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GOTO1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
C
      IF(ISTOP.LT.ISTRT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)
 1223   FORMAT('      NO ARGUMENT FOR RM/RMDIR COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        IF(ICASE.EQ.'FILE')THEN
          NCSTR=6
          ITEXT2(1:NCSTR)='ERASE '
        ELSE
          NCSTR=7
          ITEXT2(1:NCSTR)='DELTRE '
        ENDIF
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        IF(ICASE.EQ.'FILE')THEN
          NCSTR=3
          ITEXT2(1:NCSTR)='rm '
        ELSE
          NCSTR=6
          ITEXT2(1:NCSTR)='rm -r '
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1291)
 1291   FORMAT('      RM/RMDIR COMMAND NOT SUPPORTED ON THIS ',
     1         'PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO1310I=ISTRT,ISTOP
        NCSTR=NCSTR+1
        IF(NCSTR.GT.MAXCH)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)MAXCH
 1293     FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,
     1           ') EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310 CONTINUE
C
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISYSPE='OFF'
      ISYSHI='ON'
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'DPRM')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPRM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR,NCSTR,ISTRT,ISTOP
 9015   FORMAT('IFOUND,IERROR,NCSTR,ISTRT,ISTOP  = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,MIN(100,NCSTR))
 9018   FORMAT('(ITEXT2(1:NCSTR) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPMKDR(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--MAKE A NEW DIRECTORY.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2019.09
C     ORIGINAL VERSION--SEPTEMBER  2019.
C
C-----NON-COMMON VARIABLES -------------------------------------------
C
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'MKDR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPMKDR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IWIDTH
   53   FORMAT('IBUGD2,ISUBRO,IWIDTH = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANSLC(I),I=1,IWIDTH)
   54   FORMAT('(IANSLC(I),I=1,IWIDTH) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (CD)  AND ITS LOCATION             **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
C  CHECK FOR "MKDIR" FIRST
C
      ISTRT=1
      DO1115I=1,IWIDTH-1
        IF(IANS(I).EQ.'M'   .AND. IANS(I+1).EQ.'K' .AND.
     1         IANS(I+2).EQ.'D' .AND. IANS(I+3).EQ.'I' .AND.
     1         IANS(I+4).EQ.'R' .AND. IANS(I+5).EQ.' ')THEN
          ISTRT=I+6
          GOTO1190
        ENDIF
 1115 CONTINUE
C
C     NO MATCH
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN MKDIR (DPMKDR)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO MKDIR')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1190 CONTINUE
C
C               *******************************************************
C               **  STEP 1.2--                                       **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
C               *******************************************************
C
      IFOUND='YES'
C
      ISTOP=0
      DO1220I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GOTO1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
C
      IF(ISTOP.LT.ISTRT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)
 1223   FORMAT('      NO ARGUMENT FOR MKDIR COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        NCSTR=6
        ITEXT2(1:NCSTR)='MKDIR '
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        NCSTR=6
        ITEXT2(1:NCSTR)='mkdir '
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1291)
 1291   FORMAT('      MKDIR COMMAND NOT SUPPORTED ON THIS ',
     1         'PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO1310I=ISTRT,ISTOP
        NCSTR=NCSTR+1
        IF(NCSTR.GT.MAXCH)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)MAXCH
 1293     FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,
     1           ') EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310 CONTINUE
C
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'MKDR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPMKDR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR,NCSTR,ISTRT,ISTOP
 9015   FORMAT('IFOUND,IERROR,NCSTR,ISTRT,ISTOP  = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,MIN(100,NCSTR))
 9018   FORMAT('(ITEXT2(1:NCSTR) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPCAT(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--USE OPERATING SYSTEM COMMAND TO DISPLAY CONTENTS OF A
C              FILE.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2019.09
C     ORIGINAL VERSION--SEPTEMBER  2019.
C
C-----NON-COMMON VARIABLES -------------------------------------------
C
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'PCAT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IWIDTH
   53   FORMAT('IBUGD2,ISUBRO,IWIDTH = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANSLC(I),I=1,IWIDTH)
   54   FORMAT('(IANSLC(I),I=1,IWIDTH) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (CD)  AND ITS LOCATION             **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
C  CHECK FOR "CAT" FIRST
C
      ISTRT=1
      DO1115I=1,IWIDTH-1
        IF(IANS(I).EQ.'C'   .AND. IANS(I+1).EQ.'A'.AND.
     1     IANS(I+2).EQ.'T' .AND. IANS(I+3).EQ.' ')THEN
          ISTRT=I+4
          GOTO1190
        ELSEIF(IANS(I).EQ.'T'   .AND. IANS(I+1).EQ.'Y' .AND.
     1         IANS(I+2).EQ.'P' .AND. IANS(I+3).EQ.'E' .AND.
     1         IANS(I+4).EQ.' ')THEN
          ISTRT=I+5
          GOTO1190
        ENDIF
 1115 CONTINUE
C
C     NO MATCH
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN CAT (DPCAT)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO CAT (OR TYPE)')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1190 CONTINUE
C
C               *******************************************************
C               **  STEP 1.2--                                       **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
C               *******************************************************
C
      IFOUND='YES'
C
      ISTOP=0
      DO1220I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GOTO1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
C
      IF(ISTOP.LT.ISTRT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)
 1223   FORMAT('      NO ARGUMENT FOR CAT COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        NCSTR=5
        ITEXT2(1:NCSTR)='TYPE '
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        NCSTR=4
        ITEXT2(1:NCSTR)='cat '
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1291)
 1291   FORMAT('      CAT COMMAND NOT SUPPORTED ON THIS PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      DO1310I=ISTRT,ISTOP
        NCSTR=NCSTR+1
        IF(NCSTR.GT.MAXCH)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)MAXCH
 1293     FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,
     1           ') EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GOTO9000
        ENDIF
        ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310 CONTINUE
C
      IF(ICATMO.EQ.'ON' .AND. NCSTR.LE.248)THEN
        IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
          ITEXT2(NCSTR+1:NCSTR+7)=' | more'
          NCSTR=NCSTR+7
        ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
          ITEXT2(NCSTR+1:NCSTR+7)=' | more'
          NCSTR=NCSTR+7
        ENDIF
      ENDIF
C
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISYSPE='ON'
      ISYSHI='OFF'
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'PCAT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCAT--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR,NCSTR,ISTRT,ISTOP
 9015   FORMAT('IFOUND,IERROR,NCSTR,ISTRT,ISTOP  = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,MIN(100,NCSTR))
 9018   FORMAT('(ITEXT2(1:NCSTR) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END
      SUBROUTINE DPDIR(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
C
C     PURPOSE--USE OPERATING SYSTEM COMMAND TO PERFORM A "DIR" COMMAND
C              UNDER WINDOWS AND A "ls" COMMAND UNDER LINUX.
C     WRITTEN BY--ALAN HECKERT
C                 STATISTICAL ENGINEERING DIVISION
C                 INFORMATION TECHNOLOGY LABORATORY
C                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
C                 GAITHERSBURG, MD 20899
C                 PHONE--301-975-2899
C     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
C           OF THE NATIONAL BUREAU OF STANDARDS.
C     LANGUAGE--ANSI FORTRAN (1977)
C               HOST DEPENDENT
C     VERSION NUMBER--2019.09
C     ORIGINAL VERSION--SEPTEMBER  2019.
C
C-----NON-COMMON VARIABLES -------------------------------------------
C
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
C
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 ICLESV
C
C-----COMMON----------------------------------------------------------
C
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
C
C-----COMMON VARIABLES (GENERAL)--------------------------------------
C
      INCLUDE 'DPCOP2.INC'
C
C-----START POINT-----------------------------------------------------
C
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
C
      IF(IBUGD2.EQ.'ON' .OR. ISUBRO.EQ.'PDIR')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF DPRM--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IBUGD2,ISUBRO,IWIDTH
   53   FORMAT('IBUGD2,ISUBRO,IWIDTH = ',2(A4,2X),I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)(IANSLC(I),I=1,IWIDTH)
   54   FORMAT('(IANSLC(I),I=1,IWIDTH) = ',25A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
C               *****************************************************
C               **  STEP 1--                                       **
C               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
C               *****************************************************
C
C               *****************************************
C               **  STEP 1.1--                         **
C               **  DETERMINE THE COMMAND              **
C               **  (CD)  AND ITS LOCATION             **
C               **  ON THE LINE.                       **
C               **  DETERMINE THE START POSITION       **
C               **  (XSTART) OF THE FIRST CHARACTER    **
C               **  FOR THE STRING TO BE PRINTED.      **
C               *****************************************
C
C  CHECK FOR "CAT" FIRST
C
      ISTRT=1
      DO1115I=1,IWIDTH-1
        IF(IANS(I).EQ.'D'   .AND. IANS(I+1).EQ.'I'.AND.
     1     IANS(I+2).EQ.'R' .AND. IANS(I+3).EQ.' ')THEN
          ISTRT=I+4
          GOTO1190
        ELSEIF(IANS(I).EQ.'L'   .AND. IANS(I+1).EQ.'S' .AND.
     1         IANS(I+2).EQ.' ')THEN
          ISTRT=I+3
          GOTO1190
        ENDIF
 1115 CONTINUE
C
C     NO MATCH
C
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DIR (DPDIR)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO DIR (OR LS)')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GOTO9000
 1190 CONTINUE
C
C               *******************************************************
C               **  STEP 1.2--                                       **
C               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
C               *******************************************************
C
      IFOUND='YES'
C
      ISTOP=0
      DO1220I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GOTO1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
C
C               *****************************************
C               **  STEP 1.3--                         **
C               **  COPY OVER THE STRING OF INTEREST.  **
C               *****************************************
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        NCSTR=4
        ITEXT2(1:NCSTR)='DIR '
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        NCSTR=3
        ITEXT2(1:NCSTR)='ls '
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1291)
 1291   FORMAT('      DIR COMMAND NOT SUPPORTED ON THIS PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GOTO9000
      ENDIF
C
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        NCSTR=4
        ITEXT2(1:NCSTR)='DIR '
        IF(IDIRPA.EQ.'ON')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='/P '
          NCSTR=NCSTR+3
        ENDIF
        IF(IDIRLL.EQ.'ON')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='/N '
          NCSTR=NCSTR+3
        ELSE
          ITEXT2(NCSTR+1:NCSTR+3)='/W '
          NCSTR=NCSTR+3
        ENDIF
        IF(IDIRRE.EQ.'ON')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='/S '
          NCSTR=NCSTR+3
        ENDIF
        IF(IDIRSO.EQ.'ALPH')THEN
          ITEXT2(NCSTR+1:NCSTR+5)='/O:N '
          NCSTR=NCSTR+5
        ELSEIF(IDIRSO.EQ.'SIZE')THEN
          ITEXT2(NCSTR+1:NCSTR+5)='/O:S '
          NCSTR=NCSTR+5
        ELSEIF(IDIRSO.EQ.'DATE')THEN
          ITEXT2(NCSTR+1:NCSTR+5)='/O:D '
          NCSTR=NCSTR+5
        ENDIF
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        NCSTR=3
        ITEXT2(1:NCSTR)='ls '
        IF(IDIRLL.EQ.'ON')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='-l '
          NCSTR=NCSTR+3
        ENDIF
        IF(IDIRRE.EQ.'ON')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='-R '
          NCSTR=NCSTR+3
        ENDIF
        IF(IDIRSO.EQ.'SIZE')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='-s '
          NCSTR=NCSTR+3
        ELSEIF(IDIRSO.EQ.'DATE')THEN
          ITEXT2(NCSTR+1:NCSTR+3)='-t '
          NCSTR=NCSTR+3
        ENDIF
      ENDIF
C
      IF(ISTRT.LE.ISTOP)THEN
        DO1310I=ISTRT,ISTOP
          NCSTR=NCSTR+1
          IF(NCSTR.GT.MAXCH)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1181)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1293)MAXCH
 1293       FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,
     1             ') EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GOTO9000
          ENDIF
          ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310   CONTINUE
      ENDIF
C
      IF((IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU') .AND.
     1  IDIRPA.EQ.'ON' .AND. NCSTR.LE.248)THEN
        ITEXT2(NCSTR+1:NCSTR+7)=' | more'
        NCSTR=NCSTR+7
      ENDIF
C
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ICLESV=ICLEWT
      ISYSPE='ON'
      ISYSHI='OFF'
      ICLEWT='OFF'
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      ICLEWT=ICLESV
C
C               *****************
C               **  STEP 90--  **
C               **  EXIT       **
C               *****************
C
 9000 CONTINUE
      IF(IBUGD2.EQ.'ON'.OR.ISUBRO.EQ.'PDIR')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPDIR--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9015)IFOUND,IERROR,NCSTR,ISTRT,ISTOP
 9015   FORMAT('IFOUND,IERROR,NCSTR,ISTRT,ISTOP  = ',2(A4,2X),3I8)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9018)(ITEXT2(J:J),J=1,MIN(100,NCSTR))
 9018   FORMAT('(ITEXT2(1:NCSTR) = ',100A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
C
      RETURN
      END

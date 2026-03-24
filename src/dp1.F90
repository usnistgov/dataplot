!     dp1.f  - This file contains subroutines that may need to be
!              modified for a specific operating system.
!
!              In previous versions we have maintained separate files
!              for different operating systems/compilers.  Starting
!              with the 2018/04 version, we are replacing this with
!              pre-processor directives (#if/#endif constructs).
!
!              The following are the pre-processor directives for the
!              Windows platform.
!
!                 1. INTEL - code that is used by both the QuickWin and
!                    console versions of the Intel compiler.
!
!                 2. INTEL_QWIN - code that is used only by the Intel
!                    QuickWin version.
!
!                 3. INTEL_CONS - code that is used only by the Intel
!                    console version.
!
!                 4. INTEL_64 - use machine constants that assume 64-bits
!                    for both single precision and double precision (this
!                    is currently what we use for the production version
!                    of Dataplot).
!
!                 4. INTEL_32 - use machine constants that assume 32 bits
!                    for single precision and 64 bits for double precision.
!
!                 5. INTEL_128 - use machine constants that assume 64 bits
!                    for single precision and 128 bits for double
!                    precision.  Should only be used for 64-bit machines.
!                    Currently, a number of the special function routines
!                    have code that assume double precision is 64 bits, so
!                    this is not recommended.  When I get these routines
!                    straightend out, will make this the default for 64-bit
!                    machines.
!
!                 6. WIN32_LAHEY - code specific to the Lahey compiler.  I don't
!                    currently have a working version for Lahey, so this
!                    reserved for future use.
!
!                 7. WIN32_ABSOFT - code specific to the Absoft compiler.  I
!                    don't currently have a working version for Absoft, so
!                    this reserved for future use.
!
!              The following are the pre-processor directives for the
!              Linux platform.
!
!                 1. LINUX - specifies this is a Linux installation.
!
!                 2. I32 - use 32-bits for integers.
!
!                 3. I64 - use 64-bits for integers.
!
!                 4. SD  - use 32-bit for single precision real numbers
!
!                 5. DD  - use 64-bit for single precision real numbers
!
!                 6. DQ  - use 64-bit for single precision real numbers
!                          and 128-bit for double precision numbers.
!
!              The following are miscellaneous directives.
!
!                 1. HAVE_EXECUTE_COMMAND_LINE - the
!                    EXECUTE_COMMAND_LINE syntax was added in the
!                    Fortran 2008 standard to execute operating system
!                    commands. Support was added in the GNU gfortran
!                    compiler starting with version 4.6.  It is also
!                    supported in newer versions of the Intel Fortran
!                    compiler.  Since some sites may not have the
!                    newer compilers, this option was provided to
!                    allow this to be turned on or off as needed.
!
!                 2. HAVE_ISNAN - specify whether the ISNAN function is
!                    available.  This is primarily for Linux gfortran
!                    (versions 4.1.2 and older do not support it,
!                    versions newer than this do).
!
!                 3. CYGWIN - defines cut/paste operations for the
!                    Cygwin platform
                                                                                                                                  
!                 4. MACOSX - defines cut/paste operations for the
!                    MacOSX platform
                                                                                                                                  
!                 5. Added some platform specific directives for older
!                    operating systems (e.g., VAX_VMS, NOS_VE).  These
!                    systems are generally obsolete, so they will
!                    typically not be defined.
!
!                 6. DISABLE_SYSTEM_COMMAND - specifies that the SYSTEM
!                    command should be disabled.  This can be useful if
!                    Dataplot is being used in a CGI script web
!                    application.
!
!     The specific routines in this file are:
!
!      1. INITMC   - define machine constants for a specific platform
!      2. INITFO/  - specify certain file names for a specific platform
!         INITF2
!      3. DPOPFI   - open a file (typically does not need to be modified)
!      4. CKCLAR   - check for command line arguments
!      5. DPSYS2   - execute an operating system command
!      6. DPTIME   - time and date
!      7. DPEDIT   - main driver routine for the FED command (i.e., the
!                    built-in line editor)
!      8. DPWRST   - write to the terminal window
!      9. GRWRST   - write to the graphics device
!     10. DPSLEE   - implement the "sleep" command
!         DPSLE2   - similar to DPSLEE, but for internal use
!     11. DPFLSH   - flush an output file
!     12. RANLP/   - various random number generators
!         RANFT/
!         R250IN/
!         RND250
!     13. DPPID2   - return the process id
!     14. DPINF2   - check existence of a file
!     15. DPPRMP   - specify "advance" behavior for the command prompt
!     16. DPCPUT   - CPU time/usage
!     17. IQWNHE/  - implement some functionality for the
!         IQWNBR     Quick Win menu items
!     18. ISNANZ   - test for "not a number"
!     19. DPCLIP   - read from system clipboard
!     20. DPCLI2   - view/copy/run from system clipboard
!     21. DPCLI3   - clear clipboard
!     22. DPCLI4   - write strings/parameters to clipboard
!     23. DPCLI5   - write variables to clipboard
!     24. DPCLI6   - copy file to clipboard
!     25. DPCDIR   - change the current directory
!     26. DPPWD    - return current working directory
!     27. DPRM     - remove a file or a directory
!     28. DPMKDR   - make a new directory
!     29. DPCAT    - list contents of a file
!     30. DPDIR    - list files (Windows DIR or Linux ls command)
!
      SUBROUTINE INITMC(IBUGIN)
!
!     PURPOSE--DEFINE MACHINE CONSTANTS (INTEGER, REAL, AND
!              DOUBLE PRECISION) FOR A PARTICULAR COMPUTER,
!     NOTE--THIS SUBROUTINE DOES NOT ADHERE TO 1966 ANSI STANDARD
!           OR THE 1977 ANSI STANDARD
!           BECAUSE IT USES OCTAL CONSTANTS.
!     NOTE--TO ALTER THIS SUBROUTINE FOR A PARTICULAR ENVIRONMENT,
!     THE DESIRED SET OF DATA STATEMENTS SHOULD BE ACTIVATED BY
!     REMOVING THE C FROM COLUMN 1.
!     NOTE--FOR IMPLEMENTATION CONVIENENCE, THE COMMENT LINES THAT
!     NEED TO BE DEACTIVATED ARE CODED AS "CXXXX" WHERE XXXX DEFINES
!     A PARTICULAR MACHINE.  THIS MEANS A SINGLE GLOBAL REPLACE CAN
!     BE USED TO UNCOMMENT THE APPROPRIATE LINES FOR A PARTICULAR
!     MACHINE (E.G., CHANGE '     ' TO '    ').
!     THE FOLLOWING CODES ARE USED:
!     APPO - APOLLO
!     BURR - BURROUGHS 1700
!     BUR2 - BURROUGHS 5700
!     BUR3 - BURROUGHS 6700
!     NVE  - CDC USING NOS/VE
!     205  - CDC 205 USING VSOS
!     CRAY - CRAY
!     DG   - DATA GENERAL ECLIPSE
!     HARR - HARRIS 220
!     HONE - HONEYWELL 600/6000
!     HP1  - HP 2100 FTN4
!     HP2  - HP 2100 FTN4
!     HP9  - HP 9000 (UNIX)
!     IBM  - IBM 370
!     PDP1 - PDP-10 (KA PROCESSOR)
!     PDP2 - PDP-10 (KI PROCESSOR)
!     PDP3 - PDP-11 (32 BIT)
!     PDP4 - PDP-11 (16 BIT)
!     PRIM - PRIME
!     UNIV - UNIVAC WITH FTN (I.E., 77 COMPILER)
!     UNI2 - UNIVAC WITH FOR (I.E., 66 COMPILER, NO LONGER SUPPORTED)
!     IBM- - IBM-PC USING 16 BIT DOS, 8087 CO-PROCESSOR
!     OS2  - IBM-PC USING OS/2 (32 BIT 386 USING OTG COMPILER)
!     MACI - MACINTOSH
!     SUN  - SUN (UNIX, CAN BE USED BY OTHER UNIX MACHINES, E.G. THE
!            SILICON GRAPHICS IRIS AND THE HP-9000).
!     CON1 - CONVEX (NATIVE MODE, WITHOUT -R8 OPTION)
!     CON2 - CONVEX (NATIVE MODE, WITH -R8 OPTION)
!     CON3 - CONVEX (IEEE MODE, WITHOUT -R8 OPTION)
!     CON4 - CONVEX (IEEE MODE, WITH -R8 OPTION)
!
!     NOTE--THIS SUBROUTINE IS IDENTICAL TO THE DPMACH SUBROUTINE.
!
!               **************************************************
!               **  DESCRIPTION OF INTEGER MACHINE CONSTANTS    **
!               **************************************************
!
!     TO DESCRIBE I/O UNIT NUMBERS--
!
!       I1MACH( 1) = THE STANDARD INPUT UNIT.
!       I1MACH( 2) = THE STANDARD OUTPUT UNIT.
!       I1MACH( 3) = THE STANDARD PUNCH UNIT.
!       I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
!
!     TO DESCRIBE WORDS--
!
!       I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
!       I1MACH( 6) = THE NUMBER OF CHARACTERS PER INTEGER STORAGE UNIT.
!
!     TO DESCRIBE INTEGERS--
!
!       ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
!                  SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
!                  WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
!
!       I1MACH( 7) = A, THE BASE.
!       I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
!       I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
!
!     TO DESCIBE FLOATING-POINT NUMBERS--
!
!       ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
!       BASE-B FORM
!                  SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
!                  WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
!                  0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
!
!       I1MACH(10) = B, THE BASE.
!
!     TO DESCIBE SINGLE-PRECISION--
!
!       I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
!       I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
!       I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
!
!     TO DESCRIBE DOUBLE-PRECISION--
!
!       I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
!       I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
!       I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
!
!     THE VALUES OF
!     I1MACH(1) TO I1MACH(4) SHOULD BE CHECKED FOR CONSISTENCY
!     WITH THE LOCAL OPERATING SYSTEM.
!
!               *************************************************************
!               **  DESCRIPTION OF REAL (FLOATING POINT) MACHINE CONSTANTS  *
!               *************************************************************
!
!     R1MACH(1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
!     R1MACH(2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
!     R1MACH(3) = B**(-T), THE SMALLEST RELATIVE SPACING.
!     R1MACH(4) = B**(1-T), THE LARGEST RELATIVE SPACING.
!     R1MACH(5) = LOG10(B)
!
!     WHERE POSSIBLE, OCTAL OR HEXADECIMAL CONSTANTS HAVE BEEN USED
!     TO SPECIFY THE CONSTANTS EXACTLY WHICH HAS IN SOME CASES
!     REQUIRED THE USE OF EQUIVALENT INTEGER ARRAYS.
!
!               *********************************************************
!               **  DESCRIPTION OF DOUBLE PRECISION MACHINE CONSTANTS  **
!               *********************************************************
!
!     D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
!     D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
!     D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
!     D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
!     D1MACH( 5) = LOG10(B)
!
!     WHERE POSSIBLE, OCTAL OR HEXADECIMAL CONSTANTS HAVE BEEN USED
!     TO SPECIFY THE CONSTANTS EXACTLY WHICH HAS IN SOME CASES
!     REQUIRED THE USE OF EQUIVALENT INTEGER ARRAYS.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--THIS SUBROUTINE IS A MODIFICATION OF CODE
!           PROVIDED IN THE FOLLOWING ARTICLE--
!           CACM, 19XX.
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--82.6
!     ORIGINAL VERSION--SEPTEMBER 1980
!     UPDATED         --JULY      1981.
!     UPDATED         --AUGUST    1981.
!     UPDATED         --MAY       1982.
!     UPDATED         --JULY      1986 (IBM-PC AND MACINTOSH)
!     UPDATED         --OCTOBER   1986 (SUN)
!     UPDATED         --FEBRUARY  1988.  DIFFERENT GRAPHICS & ALPHA I/O (ALAN)
!     UPDATED         --FEBRUARY  1988.  UPDATED CYBER CONSTANTS (ALAN)
!     UPDATED         --JUNE      1989.  IBM-PC OS/2 & COMPAQ 386 CONSTANTS
!     UPDATED         --JUNE      1989.  INTEGER*2 (COMPAQ ERROR MESSAGE)
!     UPDATED         --JUNE      1990.  CODED COMMENTS FOR EASY "GLOBAL" EDIT
!                                        MOVE DATA AFTER EXECUTABLE
!     UPDATED         --AUGUST    1990.  (CONVEX, 4 DIFFERENT MODES, FROM CMLIB)
!     UPDATED         --APRIL     1992.  SAVE STATEMENTS
!     UPDATED         --APRIL     1992.  IHMOD1='386 '
!     UPDATED         --APRIL     1992.  ICOMPI='OTG '
!     UPDATED         --MAY       1992.  D.P. OVERFLOW PROBLEMS
!     UPDATED         --OCTOBER   1994.  FIX IBM-PC CONSTANTS
!     UPDATED         --OCTOBER   1996.  MS-FORTAN SUPPORT
!     UPDATED         --FEBRUARY  2006.  DOUBLE PRECISION VERSION
!                                        OF INTEL WINDOWS COMPILER
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!CCCC FOLLOWING LINES FOR MICROSOFT FORTRAN VERSION
!CCCC USE DFLIB
!CCCC USE DFWIN
#ifdef INTEL_QWIN
      USE IFPORT
      USE IFQWIN
      TYPE (FONTINFO) MSFONT
!CCCC TYPE (QWINFO)  WINFO
      LOGICAL MODESTATUS
      TYPE (WINDOWCONFIG)   DPSCREEN
      CHARACTER*4 QWSCRN
      COMMON/QUICKWN/DPSCREEN,QWSCRN,IQWNFT,IQWNFN
!CCCC LOGICAL(4) RESZZ
      INTERFACE
        LOGICAL(4) FUNCTION INITIALSETTINGS
        END FUNCTION
      END INTERFACE
      EXTERNAL IQWNHE
      EXTERNAL IQWNBR
!CCCC EXTERNAL IQWNAB
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
!
      CHARACTER*4 IBUGIN
!
      INCLUDE 'DPCODV.INC'
!
      PARAMETER(MAXCLR=163)
      INTEGER IRED(MAXCLR), IBLUE(MAXCLR), IGREEN(MAXCLR)
!
!---------------------------------------------------------------------
!
      DOUBLE PRECISION D2MACH(5)
!
!CCCC THE FOLLOWING 12 LINES WERE ADDED  APRIL 1992
!
      SAVE R2MACH
      SAVE ISMALL
      SAVE ILARGE
      SAVE IRIGHT
      SAVE IDIVER
      SAVE ILOG10
!
      SAVE D2MACH
      SAVE JSMALL
      SAVE JLARGE
      SAVE JRIGHT
      SAVE JDIVER
      SAVE JLOG10
!
      DIMENSION ISMALL(2)
      DIMENSION ILARGE(2)
      DIMENSION IRIGHT(2)
      DIMENSION IDIVER(2)
      DIMENSION ILOG10(2)
!
      DIMENSION JSMALL(4)
      DIMENSION JLARGE(4)
      DIMENSION JRIGHT(4)
      DIMENSION JDIVER(4)
      DIMENSION JLOG10(4)
!
      DIMENSION I2MACH(16)
!
      DIMENSION R2MACH(5)
!
      EQUIVALENCE (R2MACH(1),ISMALL(1))
      EQUIVALENCE (R2MACH(2),ILARGE(1))
      EQUIVALENCE (R2MACH(3),IRIGHT(1))
      EQUIVALENCE (R2MACH(4),IDIVER(1))
      EQUIVALENCE (R2MACH(5),ILOG10(1))
!
      EQUIVALENCE (D2MACH(1),JSMALL(1))
      EQUIVALENCE (D2MACH(2),JLARGE(1))
      EQUIVALENCE (D2MACH(3),JRIGHT(1))
      EQUIVALENCE (D2MACH(4),JDIVER(1))
      EQUIVALENCE (D2MACH(5),JLOG10(1))
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOMC.INC'
      INCLUDE 'DPCOHO.INC'
!CCCC THE FOLLOWING LINE WAS INSERTED FOR GR & ALPHA UNITS  FEBRUARY 1989
      INCLUDE 'DPCOGR.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!  DEFINE RGB TABLES FOR QUICKWIN
!
      INCLUDE 'DPCOCT.INC'
!
!---------------------------------------------------------------------
! -----DATA STATEMENTS---------------------------------------------
!C
!C              *******************************************************
!C              **  MACHINE CONSTANTS FOR THE APOLLO                 **
!               **  MY THANKS TO NORM SHELLEY FOR THIS CONTRIBUTION  **
!               **  (JANUARY, 1985).  THESE VALUES ARE TENTATIVE     **
!C              **  AND HAVE NOT BEEN CHECKED.                       **
!C              *******************************************************
!C
!C
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
!C    DOES APOLLO NORMALIZE THEIR FRACTION LIKE A    VAX?
!C    IF SO, CHANGE THE FOLLOWING 23 TO 24
!C    ASK APOLLO HOW THEY DO THEIR NUMBERS
      DATA I2MACH(11) /   23 /
      DATA I2MACH(12) / -128 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   52 /
      DATA I2MACH(15) / -1024/
      DATA I2MACH(16) /  1023/
!C
!C    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
!C    (THAT IS 2**-23 AND 2**22)
!C    FOR THE NUMBERS BELOW,
      DATA R2MACH(1) / 1.175495E-38 /
      DATA R2MACH(2) / 3.402823E38 /
      DATA R2MACH(3) / 1.1920928955078E-7 /
      DATA R2MACH(4) / 2.3841857910156E-7 /
      DATA R2MACH(5) / 0.3010300 /
!C
!C    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
!C    (THAT IS 2**-23 AND 2**22)
!C    FOR THE NUMBERS BELOW,
      DATA D2MACH(1) / 2.22507385850721D-308 /
      DATA D2MACH(2) / 1.79769313486231D308 /
      DATA D2MACH(3) / 1.1102230246252D-16 /
      DATA D2MACH(4) / 2.2204460492503D-16 /
      DATA D2MACH(5) / 0.3010299956639812 /
!C
      IHOST1='APOL'
      IHOST2='    '
      IHMOD1='DOMA'
      IHMOD2='    '
      IOPSY1='AEGI'
      IOPSY2='    '
      ICOMPI='FTN '
      ISITE='    '
#endif
!C
!C              ********************************************************
!C              **  MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.  **
!C              ********************************************************
!C
#ifdef BURROUGHS
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='1700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
!C
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
!C
      DATA R2MACH(1) / Z400800000 /
      DATA R2MACH(2) / Z5FFFFFFFF /
      DATA R2MACH(3) / Z4E9800000 /
      DATA R2MACH(4) / Z4EA800000 /
      DATA R2MACH(5) / Z500E730E8 /
!C
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
!C
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='1700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ********************************************************
!C              **  MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.  **
!C              ********************************************************
!C
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
!C
      DATA R2MACH(1) / O1771000000000000 /
      DATA R2MACH(2) / O0777777777777777 /
      DATA R2MACH(3) / O1311000000000000 /
      DATA R2MACH(4) / O1301000000000000 /
      DATA R2MACH(5) / O1157163034761675 /
!C
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
!C
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='5700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              **************************************************************
!C              **  MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.  **
!C              **************************************************************
!C
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
!C
      DATA R2MACH(1) / O1771000000000000 /
      DATA R2MACH(2) / O0777777777777777 /
      DATA R2MACH(3) / O1311000000000000 /
      DATA R2MACH(4) / O1301000000000000 /
      DATA R2MACH(5) / O1157163034761675 /
!C
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
!C
      IHOST1='BURR'
      IHOST2='    '
      IHMOD1='6700'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              *******************************************************
!C              **  MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.  **
!C              *******************************************************
!C
!
!     MACHINE CONSTANTS FOR THE CDC 170/180 SERIES USING NOS/VE
!     FROM SANDIA LABS
!
!
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
!C
       DATA R2MACH(1) / Z"3001800000000000" /
       DATA R2MACH(2) / Z"4FFEFFFFFFFFFFFE" /
       DATA R2MACH(3) / Z"3FD2800000000000" /
       DATA R2MACH(4) / Z"3FD3800000000000" /
       DATA R2MACH(5) / Z"3FFF9A209A84FBCF" /
!C
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
!C
       IHOST1='NVE '
       IHOST2='    '
       IHMOD1='855 '
       IHMOD2='    '
       IOPSY1='NVE '
       IOPSY2='    '
       ICOMPI='FTN5'
       ISITE='NBS '
!C
!C  NOTE: 5/88.  FOR LEVEL 1.3.1, NEED TO SPECIFY "$LOCAL" AS THE CATALOG
!C        FOR THE INPUT AND OUTPUT FILES.  (UNITS 4, 5, 6, AND 7 ARE USED
!C        FOR TERMINAL I/O.  OTHERWISE, WILL USE THE DEFAULT CATALOG.
!C        4 - GRAPHICS INPUT
!C        5 - ALPHANUMERIC INPUT
!C        6 - GRAPHICS OUTPUT
!C        7 - ALPHANUMERIC OUTPUT.
!C        NOS/VE REQUIRES DIFFERENT UNITS FOR GRAPHICS AND ALPHANUMERIC
!C        I/O SINCE GRAPHICS I/O MUST BE IN "TRANSPARENT" MODE.
!C        NOTE THAT THE PROCEDURE ON NOS/VE THAT EXECUTES DATAPLOT WILL
!C        HANDLE CONNECTING THESE UNITS TO THE TERMINAL.
!C
      CALL SCLCMD('CREATE_VARIABLE N=STV_ZZZZZZ KIND=STATUS')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE4 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE5 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE6 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('DETACH_FILE $LOCAL.TAPE7 STATUS=STV_ZZZZZZ')
      CALL SCLCMD('REQUEST_TERMINAL $LOCAL.TAPE6 IEM=TRANSPARENT   &
       STATUS=STV_ZZZZZZ ')
      CALL SCLCMD('REQUEST_TERMINAL $LOCAL.TAPE4 IEM=TRANSPARENT '//   &
       'TCM=F TTC=$CHAR(255) TFC=$CHAR(13) BKA=2 IOM=S TLM=N TTM=N '//   &
       'STATUS=STV_ZZZZZZ')
      OPEN(UNIT=4,FILE='$LOCAL.TAPE4')
      OPEN(UNIT=5,FILE='$INPUT')
      OPEN(UNIT=6,FILE='$LOCAL.TAPE6')
      OPEN(UNIT=7,FILE='$OUTPUT')
      CALL SCLCMD('DELETE_VARIABLE STV_ZZZZZZ')
#endif
!C
!C
!C              *********************************************************
!C              **  MACHINE CONSTANTS FOR THE CDC CYBER 200 SERIES.    **
!C              **  (WITH THANKS TO MARY BETH ALGEO, NBS  AUG., 1986   **
!C              *********************************************************
!C
!C
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
!C
      DATA R2MACH(1) / X'9000400000000000' /
      DATA R2MACH(2) / X'6FFF7FFFFFFFFFFF' /
      DATA R2MACH(3) / X'FFA3400000000000' /
      DATA R2MACH(4) / X'FFA4400000000000' /
      DATA R2MACH(5) / X'FFD04D104D427DE8' /
!C
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
!C
      IHOST1='205 '
      IHOST2='    '
      IHMOD1='205 '
      IHMOD2='    '
      IOPSY1='VSOS'
      IOPSY2='2.2 '
      ICOMPI='    '
      ISITE='    '
#endif
!C              ****************************************
!C              **  MACHINE CONSTANTS FOR THE CRAY 1  **
!C              ****************************************
!C
!C
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
!C
      DATA R2MACH(1) / 200004000000000000000B /
      DATA R2MACH(2) / 577777777777777777777B /
      DATA R2MACH(3) / 377214000000000000000B /
      DATA R2MACH(4) / 377224000000000000000B /
      DATA R2MACH(5) / 377774642023241175720B /
!C
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
!C
      IHOST1='CRAY'
      IHOST2='    '
      IHMOD1='1'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C              ************************************************************
!C              **  MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200  **
!C              ************************************************************
!C
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
!C
      NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
      STATIC R2MACH(5)
!C
      DATA ISMALL/20K,0/,ILARGE/77777K,177777K/
      DATA IRIGHT/35420K,0/,IDIVER/36020K,0/
      DATA ILOG10/40423K,42023K/
!C
      NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -
      STATIC D2MACH(5)
!C
      DATA JSMALL/20K,3*0/,JLARGE/77777K,3*177777K/
      DATA JRIGHT/31420K,3*0/,JDIVER/32020K,3*0/
      DATA JLOG10/40423K,42023K,50237K,74776K/
!C
      IHOST1='DG'
      IHOST2='    '
      IHMOD1='ECLI'
      IHMOD2='200'
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ********************************************
!C              **  MACHINE CONSTANTS FOR THE HARRIS 220  **
!C              ********************************************
!C
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
!C
      DATA ISMALL(1),ISMALL(2) / '20000000, '00000201 /
      DATA ILARGE(1),ILARGE(2) / '37777777, '00000177 /
      DATA IRIGHT(1),IRIGHT(2) / '20000000, '00000352 /
      DATA IDIVER(1),IDIVER(2) / '20000000, '00000353 /
      DATA ILOG10(1),ILOG10(2) / '23210115, '00000377 /
!C
      DATA JSMALL(1),JSMALL(2) / '20000000, '00000201 /
      DATA JLARGE(1),JLARGE(2) / '37777777, '37777577 /
      DATA JRIGHT(1),JRIGHT(2) / '20000000, '00000333 /
      DATA JDIVER(1),JDIVER(2) / '20000000, '00000334 /
      DATA JLOG10(1),JLOG10(2) / '23210115, '10237777 /
!C
      IHOST1='HARR'
      IHOST2='    '
      IHMOD1='220'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ************************************************************
!C              **  MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.  **
!C              ************************************************************
!C
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
!C
      DATA R2MACH(1) / O402400000000 /
      DATA R2MACH(2) / O376777777777 /
      DATA R2MACH(3) / O714400000000 /
      DATA R2MACH(4) / O716400000000 /
      DATA R2MACH(5) / O776464202324 /
!C
      DATA JSMALL(1),JSMALL(2) / O402400000000, O000000000000 /
      DATA JLARGE(1),JLARGE(2) / O376777777777, O777777777777 /
      DATA JRIGHT(1),JRIGHT(2) / O604400000000, O000000000000 /
      DATA JDIVER(1),JDIVER(2) / O606400000000, O000000000000 /
      DATA JLOG10(1),JLOG10(2) / O776464202324, O117571775714 /
!C
      IHOST1='HONE'
      IHOST2='    '
      IHMOD1='6000'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ************************************************
!C              **  MACHINE CONSTANTS FOR THE HP 2100         **
!C              **  3 WORD DOUBLE PRECISION OPTION WITH FTN4  **
!C              ************************************************
!C
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
!C
      DATA ISMALL(1), ISMALL(2) / 40000B,       1 /
      DATA ILARGE(1), ILARGE(2) / 77777B, 177776B /
      DATA IRIGHT(1), IRIGHT(2) / 40000B,    325B /
      DATA IDIVER(1), IDIVER(2) / 40000B,    327B /
      DATA ILOG10(1), ILOG10(2) / 46420B,  46777B /
!C
      DATA JSMALL(1), JSMALL(2), JSMALL(3) / 40000B,       0,       1 /
      DATA JLARGE(1), JLARGE(2), JLARGE(3) / 77777B, 177777B, 177776B /
      DATA JRIGHT(1), JRIGHT(2), JRIGHT(3) / 40000B,       0,    265B /
      DATA JDIVER(1), JDIVER(2), JDIVER(3) / 40000B,       0,    276B /
      DATA JLOG10(1), JLOG10(2), JLOG10(3) / 46420B,  46502B,  77777B /
!C
      IHOST1='HP'
      IHOST2='    '
      IHMOD1='2100'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='FTN4'
      ISITE='    '
#endif
!C
!C              ************************************************
!C              **  MACHINE CONSTANTS FOR THE HP 2100         **
!C              **  4 WORD DOUBLE PRECISION OPTION WITH FTN4  **
!C              ************************************************
!C
!C
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
!C
      DATA ISMALL(1), ISMALL(2) / 40000B,       1 /
      DATA ILARGE(1), ILARGE(2) / 77777B, 177776B /
      DATA IRIGHT(1), IRIGHT(2) / 40000B,    325B /
      DATA IDIVER(1), IDIVER(2) / 40000B,    327B /
      DATA ILOG10(1), ILOG10(2) / 46420B,  46777B /
!C
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
!C
      IHOST1='HP'
      IHOST2='    '
      IHMOD1='2100'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='FTN4'
      ISITE='    '
#endif
!C              ********************************************************
!C              **  MACHINE CONSTANTS FOR THE HP 9000.  THESE VALUES  **
!C              **  ARE TENTATIVE AND HAVE NOT BEEN CHECKED           **
!C              ********************************************************
!C
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
!C
      DATA R2MACH(1) / O00000000200 /
      DATA R2MACH(2) / O37777677777 /
      DATA R2MACH(3) / O00000032200 /
      DATA R2MACH(4) / O00000032400 /
      DATA R2MACH(5) / O04046637632 /
!C
      IHOST1='HP'
      IHOST2='    '
      IHMOD1='9000'
      IHMOD2='    '
      IOPSY1='    '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ********************************************************
!C              **  MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,     **
!C              **  XEROX SIGMA 5/7/9,                                **
!C              **  SEL SYSTEMS 85/86,                                **
!C              **  INTERDATA 30 AND 40,                              **
!C              **  PERKIN-ELMER 3230, 3240, 3242,                    **
!C              ********************************************************
!C
!C
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
!C
       DATA R2MACH(1) / Z00100000 /
       DATA R2MACH(2) / Z7FFFFFFF /
       DATA R2MACH(3) / Z3B100000 /
       DATA R2MACH(4) / Z3C100000 /
       DATA R2MACH(5) / Z41134413 /
!C
       DATA JSMALL(1),JSMALL(2) / Z00100000, Z00000000 /
       DATA JLARGE(1),JLARGE(2) / Z7FFFFFFF, ZFFFFFFFF /
       DATA JRIGHT(1),JRIGHT(2) / Z33100000, Z00000000 /
       DATA JDIVER(1),JDIVER(2) / Z34100000, Z00000000 /
       DATA JLOG10(1),JLOG10(2) / Z41134413, Z509F79FF /
!C
       IHOST1='IBM'
       IHOST2='    '
       IHMOD1='370'
       IHMOD2='    '
       IOPSY1='    '
       IOPSY2='    '
       ICOMPI='    '
       ISITE='    '
#endif
!C              ********************************************************
!C              **  MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).  **
!C              ********************************************************
!
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
!C
      DATA R2MACH(1) / "000400000000 /
      DATA R2MACH(2) / "377777777777 /
      DATA R2MACH(3) / "146400000000 /
      DATA R2MACH(4) / "147400000000 /
      DATA R2MACH(5) / "177464202324 /
!C
      DATA JSMALL(1),JSMALL(2) / "033400000000, "000000000000 /
      DATA JLARGE(1),JLARGE(2) / "377777777777, "344777777777 /
      DATA JRIGHT(1),JRIGHT(2) / "113400000000, "000000000000 /
      DATA JDIVER(1),JDIVER(2) / "114400000000, "000000000000 /
      DATA JLOG10(1),JLOG10(2) / "177464202324, "144117571776 /
!C
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='10'
      IHMOD2='    '
      IOPSY1='KA'
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ********************************************************
!C              **  MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).  **
!C              ********************************************************
!C
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
!C
      DATA R2MACH(1) / "000400000000 /
      DATA R2MACH(2) / "377777777777 /
      DATA R2MACH(3) / "146400000000 /
      DATA R2MACH(4) / "147400000000 /
      DATA R2MACH(5) / "177464202324 /
!C
      DATA JSMALL(1),JSMALL(2) / "000400000000, "000000000000 /
      DATA JLARGE(1),JLARGE(2) / "377777777777, "377777777777 /
      DATA JRIGHT(1),JRIGHT(2) / "103400000000, "000000000000 /
      DATA JDIVER(1),JDIVER(2) / "104400000000, "000000000000 /
      DATA JLOG10(1),JLOG10(2) / "177464202324, "476747767461 /
!C
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='10'
      IHMOD2='    '
      IOPSY1='KI'
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              *********************************************************
!C              **  MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING  **
!C              **  32-BIT INTEGER ARITHMETIC.                         **
!C              *********************************************************
!C
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
!C
      DATA R2MACH(1) / O00040000000 /
      DATA R2MACH(2) / O17777777777 /
      DATA R2MACH(3) / O06440000000 /
      DATA R2MACH(4) / O06500000000 /
      DATA R2MACH(5) / O07746420233 /
!C
      DATA ISMALL(1) /    8388608 /
      DATA ILARGE(1) / 2147483647 /
      DATA IRIGHT(1) /  880803840 /
      DATA IDIVER(1) /  889192448 /
      DATA ILOG10(1) / 1067065499 /
!C
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
!C
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='10'
      IHMOD2='    '
      IOPSY1='KI'
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              *********************************************************
!C              **  MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING  **
!C              **  16-BIT INTEGER ARITHMETIC.                         **
!C              *********************************************************
!C
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
!C
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
!C
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
!C
      IHOST1='PDP'
      IHOST2='    '
      IHMOD1='11'
      IHMOD2='    '
      IOPSY1=' '
      IOPSY2='    '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C     THE FOLLOWING IS FOR THE PRIME--
!C              **********************************************************
!C              **  MACHINE CONSTANTS FOR THE PRIME 50 SERIES.          **
!C              **  FOR F77 COMPILER WITH -INTL OPTION                  **
!               **  MY THANKS TO ING-YUNG LI TSE FOR THIS CONTRIBUTION  **
!               **  (NOVEMBER, 1986).                                   **
!C              **********************************************************
!C
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
!C
      R2MACH(1)=0.5*2.0**(-128)
      R2MACH(2)=(1.0-2.0**(-23))*2.0*(127)
      R2MACH(3)=2.0**(-22)
      R2MACH(4)=2.0**(-21)
      R2MACH(5)=ALOG10(2.0)
!C
      D2MACH(1)=0.5D0*2.0D0**(-32590)
      D2MACH(2)=(1.0D0-2.0D0**(-47))*2.0D0**(32638)
      D2MACH(3)=2.0D0**(-46)
      D2MACH(4)=2.0D0**(-45)
      D2MACH(5)=DLOG10(2.0D0)
!C
       IHOST1='PRIM'
       IHOST2='    '
       IHMOD1='X50 '
       IHMOD2='    '
       IOPSY1='PRIM'
       IOPSY2='OS  '
       ICOMPI='F77 '
       ISITE='    '
#endif
!C
!C     THE FOLLOWING IS FOR THE UNIVAC--
!C               ***************************************************************
!C               **  MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FTN COMPILER
!C               ***************************************************************
!C
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
!C
       DATA R2MACH(1) / O000400000000 /
       DATA R2MACH(2) / O377777777777 /
       DATA R2MACH(3) / O146400000000 /
       DATA R2MACH(4) / O147400000000 /
       DATA R2MACH(5) / O177464202324 /
!C
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
!C
       IHOST1='UNIV'
       IHOST2='    '
       IHMOD1='1100'
       IHMOD2='    '
       IOPSY1='EXEC'
       IOPSY2='8'
       ICOMPI='FTN'
       ISITE='NBS'
#endif
!C
!C              ****************************************************************
!C              **  MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES. FOR COMPILER
!C              ****************************************************************
!C
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
!C
      DATA R2MACH(1) / O000400000000 /
      DATA R2MACH(2) / O377777777777 /
      DATA R2MACH(3) / O146400000000 /
      DATA R2MACH(4) / O147400000000 /
      DATA R2MACH(5) / O177464202324 /
!C
      DATA JSMALL(1),JSMALL(2) / O000040000000, O000000000000 /
      DATA JLARGE(1),JLARGE(2) / O377777777777, O777777777777 /
      DATA JRIGHT(1),JRIGHT(2) / O170540000000, O000000000000 /
      DATA JDIVER(1),JDIVER(2) / O170640000000, O000000000000 /
      DATA JLOG10(1),JLOG10(2) / O177746420232, O411757177572 /
!C
      IHOST1='UNIV'
      IHOST2='    '
      IHMOD1='1100'
      IHMOD2='    '
      IOPSY1='EXEC'
      IOPSY2='8'
      ICOMPI='FOR'
      ISITE='    '
#endif
!C
#ifdef WIN32_OTG
!     THE FOLLOWING IS FOR THE 16-BIT IBM-PC AND CLONES USING OTG COMPILER
!               ***********************************************************
!               **  MACHINE CONSTANTS FOR THE 16-BIT IBM-PC (NOT YET VERIFIED)
!               **  (WITH 8087 COPROCESSOR)                              **
!               **  (WITH APPRECIATION TO MARTIN KNAPP-CORDES,           **
!               **  JULY, 1986)                                          **
!               ***********************************************************
!C
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
!C
!CCCC DATA R2MACH(1) / Z'00800000' /
!CCCC DATA R2MACH(2) / Z'7F7FFFFF' /
!CCCC DATA R2MACH(3) / Z'33800000' /
!CCCC DATA R2MACH(4) / Z'34000000' /
!CCCC DATA R2MACH(5) / Z'3E9A209B' /
      DATA R2MACH(1) / 1.18E-38 /
      DATA R2MACH(2) / 3.340E+38 /
      DATA R2MACH(3) / 0.59E-07 /
      DATA R2MACH(4) / 1.19E-07 /
      DATA R2MACH(5) / 0.30102999566 /
!C
!CCCC DATA ISMALL(1) / Z'00800000' /
!CCCC DATA ILARGE(1) / Z'7F7FFFFF' /
!CCCC DATA IRIGHT(1) / Z'33800000' /
!CCCC DATA IDIVER(1) / Z'34000000' /
!CCCC DATA ILOG10(1) / Z'3E9A209B' /
      DATA D2MACH(1) / 2.23D-308 /
      DATA D2MACH(2) / 1.790D+308 /
      DATA D2MACH(3) / 1.11D-16 /
      DATA D2MACH(4) / 2.22D-16 /
      DATA D2MACH(5) / 0.30102999563981195D0 /
!C
!CCCC DATA JSMALL(1),JSMALL(2) / Z'00100000', Z'00000000' /
!CCCC DATA JLARGE(1),JLARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
!CCCC DATA JRIGHT(1),JRIGHT(2) / Z'3CA00000', Z'00000000' /
!CCCC DATA JDIVER(1),JDIVER(2) / Z'3CB00000', Z'00000000' /
!CCCC DATA JLOG10(1),JLOG10(2) / Z'3FD34413', Z'509F79FF' /
!C
      IHOST1='IBM-'
      IHOST2='PC  '
!CCCC THE FOLLOWING LINE WAS FIXED               APRIL 1992
!CCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
!CCCC IHMOD1='    '
      IHMOD1='386 '
      IHMOD2='    '
      IOPSY1='PC-D'
      IOPSY2='OS  '
!CCCC THE FOLLOWING LINE WAS FIXED   APRIL 1992
!CCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
!CCCC ICOMPI='    '
      ICOMPI='OTG '
      ISITE='    '
#endif
!C
!C    CONSTANTS FOR WINDOWS USING INTEL COMPILER
!C
#ifdef INTEL
!C
#ifdef INTEL_128
!
!     NEED TO GET INTEGER CONSTANTS THAT ASSUME 64 BITS
!     (FOLLOWING ARE THE 32-BIT CONSTANTS)
!
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
!C
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
!C
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
!C
      IHOST1='IBM-'
      IHOST2='PC  '
!CCCC THE FOLLOWING LINE WAS FIXED               APRIL 1992
!CCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
!CCCC IHMOD1='    '
      IHMOD1='386 '
      IHMOD2='    '
      IOPSY1='PC-D'
      IOPSY2='OS  '
!CCCC THE FOLLOWING LINE WAS FIXED   APRIL 1992
!CCCC IN CONNECTION WITH CODE IN   DPSYS2.FOR    APRIL 1992
!CCCC ICOMPI='    '
!CCCC JUNE 1996.  FOR LAHEY COMPILER, SET ICOMPI SWITCH DIFFERENT.
!CCCC ICOMPI='OTG '
!LAHE ICOMPI='LAHE'
      ICOMPI='MS-F'
      ISITE='    '
!
#endif
!C
!     THE FOLLOWING IS FOR THE MACINTOCH (NOT YET VERIFIED)--
!               **************************************************************
!               **  MACHINE CONSTANTS FOR THE MACINTOCH (NOT YET VERIFIED)  **
!               **  (WITH APPRECIATION TO MARTIN KNAPP-CORDES,              **
!               **  JULY, 1986)                                             **
!               **************************************************************
!C
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
!C
       DATA R2MACH(1) / Z'00800000' /
       DATA R2MACH(2) / Z'7F7FFFFF' /
       DATA R2MACH(3) / Z'33800000' /
       DATA R2MACH(4) / Z'34000000' /
       DATA R2MACH(5) / Z'3E9A209B' /
!C
       DATA ISMALL(1) / Z'00800000' /
       DATA ILARGE(1) / Z'7F7FFFFF' /
       DATA IRIGHT(1) / Z'33800000' /
       DATA IDIVER(1) / Z'34000000' /
       DATA ILOG10(1) / Z'3E9A209B' /
!C
       DATA JSMALL(1),JSMALL(2) / Z'00100000', Z'00000000' /
       DATA JLARGE(1),JLARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
       DATA JRIGHT(1),JRIGHT(2) / Z'3CA00000', Z'00000000' /
       DATA JDIVER(1),JDIVER(2) / Z'3CB00000', Z'00000000' /
       DATA JLOG10(1),JLOG10(2) / Z'3FD34413', Z'509F79FF' /
!
       IHOST1='MACI'
       IHOST2='NTOC'
       IHMOD1='    '
       IHMOD2='    '
       IOPSY1='MACI'
       IOPSY2='NTOC'
       ICOMPI='    '
       ISITE='    '
#endif
!C
!CCCC THE FOLLOWING WAS ADDED JUNE 1989--
!     THE FOLLOWING IS FOR THE 32-BIT IBM-PC/OS2 AND  COMPAQ 386/XX (NOT YET VER
!     (PROBABLY NOT FULLY CORRECT)
!               ***********************************************************
!               **  MACHINE CONSTANTS FOR THE 32-BIT IBM-PC (NOT YET VERIFIED)
!               **  (WITH 387 COPROCESSOR)                              **
!               **  (WITH APPRECIATION TO NELSON HSU                     **
!               **  JUNE, 1989)                                          **
!               ***********************************************************
!C
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
!C
      DATA R2MACH(1) / Z'00800000' /
      DATA R2MACH(2) / Z'7F7FFFFF' /
      DATA R2MACH(3) / Z'33800000' /
      DATA R2MACH(4) / Z'34000000' /
      DATA R2MACH(5) / Z'3E9A209B' /
!C
      DATA ISMALL(1) / Z'00800000' /
      DATA ILARGE(1) / Z'7F7FFFFF' /
      DATA IRIGHT(1) / Z'33800000' /
      DATA IDIVER(1) / Z'34000000' /
      DATA ILOG10(1) / Z'3E9A209B' /
!C
      DATA JSMALL(1),JSMALL(2) / Z'00100000', Z'00000000' /
      DATA JLARGE(1),JLARGE(2) / Z'7FEFFFFF', Z'FFFFFFFF' /
      DATA JRIGHT(1),JRIGHT(2) / Z'3CA00000', Z'00000000' /
      DATA JDIVER(1),JDIVER(2) / Z'3CB00000', Z'00000000' /
      DATA JLOG10(1),JLOG10(2) / Z'3FD34413', Z'509F79FF' /
!
      IHOST1='IBM-'
      IHOST2='PC  '
      IHMOD1='    '
      IHMOD2='    '
      IOPSY1='OS38'
      IOPSY2='6   '
      ICOMPI='    '
      ISITE='    '
#endif
!C
!C              ****************************************************
!C              **  MACHINE CONSTANTS FOR THE SUN  (AND SUN 2)    **
!C              **  WITH APPRECIATION TO BILL ANDERSON, NBS       **
!               **  OCTOBER, 1987 (THESE VALUES ARE TENTATIVE     **
!C              **  AND HAVE NOT BEEN CHECKED                     **
!C              ****************************************************
!C
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
!
!C    DOES APOLLO NORMALIZE THEIR FRACTION LIKE A    VAX?
!C    IF SO, CHANGE THE FOLLOWING 23 TO 24
!C    ASK APOLLO HOW THEY DO THEIR NUMBERS
      DATA I2MACH(11) /   24 /
      DATA I2MACH(12) / -124 /
      DATA I2MACH(13) /  127 /
      DATA I2MACH(14) /   51 /
      DATA I2MACH(15) / -1013/
      DATA I2MACH(16) /  1013/
!C
!C    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
!C    (THAT IS 2**-23 AND 2**22)
!C    FOR THE NUMBERS BELOW,
      DATA R2MACH(1) / 1.175495E-38 /
      DATA R2MACH(2) / 3.402823E38 /
      DATA R2MACH(3) / 1.1920928955078E-7 /
      DATA R2MACH(4) / 2.3841857910156E-7 /
      DATA R2MACH(5) / 0.3010300 /
!C
!C    AM GOING TO USE HP-9000 NUMBERS FOR NOW AND ON MY OWN
!C    (THAT IS 2**-23 AND 2**22)
!C    FOR THE NUMBERS BELOW,
      DATA D2MACH(1) / 2.22507385850721D-308 /
      DATA D2MACH(2) / 1.79769313486231D308 /
      DATA D2MACH(3) / 1.1102230246252D-16 /
      DATA D2MACH(4) / 2.2204460492503D-16 /
      DATA D2MACH(5) / 0.3010299956639812 /
!C
      IHOST1='SUN '
      IHOST2='    '
      IHMOD1='3   '
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
!C
#ifdef LINUX
!C  FOLLOWING SECTION FOR LINUX.  SUPPORT CONDITIONAL COMPILATION.
!C
!C  FOR INTEGERS - SPECIFY "-DI32"  OR  "-DI64"  BASED ON WHETHER
!C  YOU ARE RUNNING ON A 32-BIT OR A 64-BIT MACHINE.
!C
!C  2019/11: ALLOW SETTING IOPSY2 FOR MacOS AS THERE ARE A
!C           FEW OCCASIONS WHERE MacOS MAY NEED SLIGHTLY
!C           DIFFERENT OPTIONS THAN STANDARD LINUX.
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
!
      IHOST1='SUN '
      IHOST2='    '
      IHMOD1='3   '
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
#if defined(MACOSX)
      IOPSY2='MAC '
#endif
      ICOMPI='gfor'
      ISITE='    '
#endif
!C
!C
!C              ************************************************
!C              **  MACHINE CONSTANTS FOR THE CONVEX          **
!C              **  CONVEX C-120, NATIVE MODE                 **
!C              **  EXTRACTED FROM CMLIB LIBRARY              **
!C              **  AUGUST, 1990                              **
!C              ************************************************
!C
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
!
      DATA D2MACH(1) / 5.562684646268007D-309 /
      DATA D2MACH(2) / 8.988465674311577D+307 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
!
      DATA R2MACH(1) / 2.9387360E-39 /
      DATA R2MACH(2) / 1.7014117E+38 /
      DATA R2MACH(3) / 5.9604645E-08 /
      DATA R2MACH(4) / 1.1920929E-07 /
      DATA R2MACH(5) / 3.0102999E-01 /
!C
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
!C
!C
!C              ************************************************
!C              **  MACHINE CONSTANTS FOR THE CONVEX          **
!C              **  EXTRACTED FROM CMLIB LIBRARY              **
!C              **  CONVEX C-120, NATIVE MODE WITH -R8 OPTION **
!C              **  AUGUST, 1990                              **
!C              ************************************************
!C
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
!
      DATA R2MACH(1) / 5.562684646268007D-309 /
      DATA R2MACH(2) / 8.988465674311577D+307 /
      DATA R2MACH(3) / 1.110223024625157D-016 /
      DATA R2MACH(4) / 2.220446049250313D-016 /
      DATA R2MACH(5) / 3.010299956639812D-001 /
!
      DATA D2MACH(1) / 5.562684646268007D-309 /
      DATA D2MACH(2) / 8.988465674311577D+307 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
!C
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
!C
!C
!C              ************************************************
!C              **  MACHINE CONSTANTS FOR THE CONVEX          **
!C              **  EXTRACTED FROM CMLIB LIBRARY              **
!C              **  CONVEX C-120, IEEE MODE                   **
!C              **  AUGUST, 1990                              **
!C              ************************************************
!C
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
!
      DATA R2MACH(1) / 1.1754945E-38 /
      DATA R2MACH(2) / 3.4028234E+38 /
      DATA R2MACH(3) / 5.9604645E-08 /
      DATA R2MACH(4) / 1.1920929E-07 /
      DATA R2MACH(5) / 3.0102999E-01 /
!
      DATA D2MACH(1) / 2.225073858507202D-308 /
      DATA D2MACH(2) / 1.797693134862315D+308 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
!C
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
!C
!C
!C              ************************************************
!C              **  MACHINE CONSTANTS FOR THE CONVEX          **
!C              **  EXTRACTED FROM CMLIB LIBRARY              **
!C              **  CONVEX C-120, IEEE MODE WITH -R8 OPTION   **
!C              **  AUGUST, 1990                              **
!C              ************************************************
!C
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
!
      DATA R2MACH(1) / 2.225073858507202D-308 /
      DATA R2MACH(2) / 1.797693134862315D+308 /
      DATA R2MACH(3) / 1.110223024625157D-016 /
      DATA R2MACH(4) / 2.220446049250313D-016 /
      DATA R2MACH(5) / 3.010299956639812D-001 /
!
      DATA D2MACH(1) / 2.225073858507202D-308 /
      DATA D2MACH(2) / 1.797693134862315D+308 /
      DATA D2MACH(3) / 1.110223024625157D-016 /
      DATA D2MACH(4) / 2.220446049250313D-016 /
      DATA D2MACH(5) / 3.010299956639812D-001 /
!C
      IHOST1='CONV'
      IHOST2='EX  '
      IHMOD1='C120'
      IHMOD2='    '
      IOPSY1='UNIX'
      IOPSY2='    '
      ICOMPI='f77 '
      ISITE='    '
#endif
!C
!C
!    THE FOLLOWING IS FOR THE VAX--
!              ********************************************
!              **  MACHINE CONSTANTS FOR THE VAX-11/780  **
!              ********************************************
!
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
!
       DATA R2MACH(1) / O00000000200 /
       DATA R2MACH(2) / O37777677777 /
       DATA R2MACH(3) / O00000032200 /
       DATA R2MACH(4) / O00000032400 /
       DATA R2MACH(5) / O04046637632 /
!
       DATA ISMALL(1) /        128 /
       DATA ILARGE(1) /     -32769 /
       DATA IRIGHT(1) /      13440 /
       DATA IDIVER(1) /      13568 /
       DATA ILOG10(1) /  547045274 /
!
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
!C
       IHOST1='VAX'
       IHOST2='    '
       IHMOD1='11'
       IHMOD2='780'
       IOPSY1='VMS'
       IOPSY2='    '
       ICOMPI='    '
       ISITE='    '
#endif
!
!
!-----START POINT-----------------------------------------------------
!
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,51)
   51   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)
   52   FORMAT('***** AT THE BEGINNING OF INITMC--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *************************
!               **  COPY OVER INTEGER  **
!               **  MACHINE CONSTANTS  **
!               *************************
!
      DO 100 I=1,16
        I1MACH(I)=I2MACH(I)
  100 CONTINUE
!
!               ********************************************************
!               **  COPY OVER REAL (SINGLE PRECISION FLOATING POINT)  **
!               **  MACHINE CONSTANTS                                 **
!               ********************************************************
!
      DO 200 I=1,5
        R1MACH(I)=R2MACH(I)
  200 CONTINUE
!
!C              **********************************
!C              **  COPY OVER DOUBLE PRECISION  **
!C              **  MACHINE CONSTANTS           **
!C              **********************************
!
!CCCC THE FOLLOWING 3 LINES WERE COMMENTED OUT MAY 1992 (JJF)
!CCCC TO AVOID UNEXPLAINABLE OVERFLOW PROBLEMS MAY 1992 (JJF)
      DO 300 I=1,5
        D1MACH(I)=D2MACH(I)
  300 CONTINUE
!
!               **************************************
!               **  COMPUTE SELECTED COMMONLY-USED  **
!               **  MACHINE CONSTANTS               **
!               **************************************
!
      IRD=I2MACH(1)
      IPR=I2MACH(2)
!
!CCCC THE FOLLOWING 5 LINES WERE ENTERED                         FEBRUARY 1989
!CCCC TO SET DIFFERENT UNITS FOR ALPHANUMERIC AND GRAPHICS I/O.  FEBRUARY 1989
!CCCC MOST HOSTS WILL SET THE SAME.  CDC NOS/VE REQUIRES GRAPHICS I/O
!CCCC TO BE IN "TRANSPARENT MODE", ALPHANUMERIC IN "NON-TRANSPARENT"
      IPRGR=IPR
      IRDGR=IRD
      IF(IHOST1.EQ.'NVE') IPRGR=6
      IF(IHOST1.EQ.'NVE') IPR=7
      IF(IHOST1.EQ.'NVE') IRDGR=4
!
      CPUMIN=-R2MACH(2)
      CPUMAX=R2MACH(2)
      NUMBPW=I2MACH(5)
      NUMCPW=I2MACH(6)
      NUMBPC=NUMBPW/NUMCPW
!
#ifdef INTEL_QWIN
      ISTATUS=SETEXITQQ(QWIN$EXITNOPERSIST)
      IQWNFT=INITIALIZEFONTS()
      IQWNFN=GETFONTINFO(MSFONT)
!
!CCCC NOVEMBER 2002.
!CCCC 1. RESET TITLE ON TEXT WINDOW.
!CCCC 2. MODIFY SOME OF THE MENU ITEMS
!CCCC 3. MODIFY THE HELP MENU TO GIVE INSTRUCTIONS FOR DATAPLOT
!CCCC    HELP
!
      IRESLT=SETACTIVEQQ(IPR)
      MODESTATUS=GETWINDOWCONFIG(DPSCREEN)
      DPSCREEN.TITLE="Dataplot Text Window"
      DPSCREEN%NUMTEXTROWS=500
      DPSCREEN%NUMTEXTCOLS=132
      DPSCREEN%mode = QWIN$SCROLLDOWN
      DPSCREEN.NUMXPIXELS=-1
      DPSCREEN.NUMYPIXELS=-1
      MODESTATUS=SETWINDOWCONFIG(DPSCREEN)
      IRESLT=ABOUTBOXQQ("Dataplot: Version 6/2013.  "//   &
      "Dataplot is a language for graphics, fitting, "//   &
      "general data analysis, and mathematics.  Dataplot "//   &
      "commands are high-level, English-syntax, and "//   &
      "self-descriptive.  The three most important commands are "//   &
      "PLOT, FIT, and LET."C)
      IRESLT=MODIFYMENUROUTINEQQ(6,1,IQWNBR)
      IRESLT=MODIFYMENUROUTINEQQ(6,2,IQWNHE)
!CCCC IRESLT=MODIFYMENUROUTINEQQ(6,3,IQWNAB)
!CCCC IRESLT=DELETEMENUQQ(6,1)
!
!CCCC MARCH 2002: SET FOREGROUND/BACKGROUND COLOR FOR TEXT
!CCCC SCREEN BASED ON WHETHER OR NOT TRUE COLOR SPECIFIED.
!
      IF(IQWNCL.EQ.'VGA')THEN
         IQWNBC=0
         IQWNF2=15
         IRESLT=SETACTIVEQQ(IPR)
!CCCC    IRESLT=SETBKCOLOR(INT2(IQWNBC))
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
!
      DPSCREEN%mode = QWIN$SCROLLDOWN
      MODESTATUS=SETWINDOWCONFIG(DPSCREEN)
#endif
!
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END OF INITMC--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9012)IHOST1,IHOST2,IHMOD2,IHMOD2
 9012   FORMAT('IHOST1,IHOST2 (HOST),IHMOD2,IHMOD2 (MODEL)= ',   &
               2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)IOPSY1,IOPSY2,ICOMPI
 9014   FORMAT('IOPSY1,IOPSY2 (OPERATING SYSTEM),ICOMPI (COMPILER) = ',   &
               A4,2X,A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9016)ISITE
 9016   FORMAT('ISITE         (SITE) = ',A4)
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9022)IPR,IRD,CPUMIN,CPUMAX
 9022   FORMAT('IPR,IRD,CPUMIN,CPUMAX = ',2I8,2G15.7)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9024)NUMBPC,NUMCPW,NUMBPW
 9024   FORMAT('NUMBPC,NUMCPW,NUMBPW = ',3I8)
        CALL DPWRST('XXX','BUG ')
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 9030 I=1,16
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
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 9040 I=1,5
          WRITE(ICOUT,9041)I,R1MACH(I)
 9041     FORMAT('I,R1MACH(I)  = ',I8,2X,E15.7)
          CALL DPWRST('XXX','BUG ')
 9040   CONTINUE
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        DO 9050 I=1,5
          WRITE(ICOUT,9051)I,D1MACH(I)
 9051     FORMAT('I,D1MACH(I)  = ',I8,2X,D15.7)
          CALL DPWRST('XXX','BUG ')
 9050   CONTINUE
!
      ENDIF
!
      RETURN
      END SUBROUTINE INITMC
      SUBROUTINE INITFO(IBUGIN)
!
!CCCC THE FOLLOWING SECTION WAS ADDED JUNE 1990
!     IMPLEMENTATION NOTE--DATAPLOT CANNOT BE LINKED/LOADED
!           WITHOUT AN EDITOR PASS OF THIS FILE SO AS TO
!           SPECIFY THE FILE NAMES ON YOUR COMPUTER OF CHOICE.
!
!           DATAPLOT USES 2 TYPES OF FILES.  PERMANENT FILES (E.G., THE
!           ON-LINE HELP FILES) AND TEMPORARY FILES (E.G., THE PLOT FILE)
!           CREATED DURING A DATAPLOT SESSION.  THIS ROUTINE DEFINES WHERE
!           THE PERMANENT FILES WILL BE FOUND AND WHERE THE TEMPORARY FILES
!           WILL BE CREATED.  THIS WILL VARY FROM DEPENDING ON THE
!           HOST, OPERATING SYSTEM, AND THE SITE.  PRE-PROCESSOR
!           DIRECTIVES ARE USED TO ACTIVATE CODE FOR A SPECIFIC
!           OPERATING SYSTEM.
!
!           NO CHANGES REQUIRED AFTER THE LINE "END OF USER CHANGES".
!           HOWEVER, BE SURE TO CHECK THE ROUTINE DPOPFI WHICH MAY REQUIRE
!           A FEW LINES TO BE MODIFIED IN ORDER TO AUTOMATICALLY ACCESS
!           DATAPLOT REFERENCE FILES (I.E., SAMPLE DATA AND MACRO FILES).
!
!     PURPOSE--THIS IS SUBROUTING INITFO.
!              (THE   FO    AT THE END OF    INITFO   STANDS FOR
!              FILE OPERATIONS.
!              THIS SUBROUTINE DEFINES ALL OF THE FILE NAMES
!              THAT DATAPLOT USES, AND ALSO DEFINES
!              ATTRIBUTES OF SUCH FILES.
!
!     NOTE--TYPICAL FILE NAMES FOR THE MESSAGE FILE
!           FOR VARIOUS COMPUTERS--
!              VAX             --[DATAPLOT]DPMESF.TEX
!              CDC (NOS-2)     --DPMESF
!              CDC (NOS/VE)    --
!              HONEYWELL       --udd>dataplot>dpmesf.text
!              PERKIN-ELMER    --CALX:DPMESF.TEX/255       ACCOUNT:FILE.EXT/ACCO
!              IBM (EBCDIC)    --
!              PRIME           --DATAPLOT>DPMESF.TEX
!              UNIVAC          --
!              SUN             --/usr/local/lib/dataplot/dpmesf.tex
!              AT&T 3B20 (UNIX)--
!              APOLLO          --
!              DATA GENERAL    --
!              UNIX            --/usr/local/lib/dataplot/dpmesf.tex
!     NOTE--TYPICAL FORTRAN EXTENSIONS FOR DATAPLOT'S MAIN ROUTINE
!           FOR VARIOUS COMPUTERS--
!              VAX             --[DATAPLOT]MAIN.FOR
!              CDC (NOS-2)     --MAIN
!              CDC (NOS/VE)    --
!              HONEYWELL       --dataplot>main.fortran
!              PERKIN-ELMER    --
!              IBM (EBCDIC)    --
!              PRIME           --DATAPLOT>MAIN.F77
!              UNIVAC          --DATAPLOT.MAIN
!              SUN             --/usr/local/src/dataplot/main.f
!              AT&T 3B20 (UNIX)--
!              APOLLO          --
!              DATA GENERAL    --
!              CRAY            --
!     NOTE--TYPICAL INCLUDE STATEMENTS FOR VARIOUS COMPUTERS--
!              VAX             --INCLUDE 'DPCOMC.INC' (START IN COL. 7)
!              CDC (NOS-2)     --(NO INCLUDE CAPABILITY)
!              CDC (NOS/VE)    --
!              HONEYWELL       --%INCLUDE DPCOMC (START IN COL. 1)
!                                (AND SEARCHES FOR DPCOMC.INCL.FORTRAN)
!              PERKIN-ELMER    --
!              IBM (EBCDIC)    --
!              PRIME           --$INSERT DPCOMC.INC (START IN COL. 1)
!              UNIVAC          --
!                                (MUST PREPROCESS WITH PDP PROCESSOR)
!              SUN             --INCLUDE 'DPCOMC.INC'
!              AT&T 3B20 (UNIX)--
!              APOLLO          --
!              DATA GENERAL    --
!              CRAY            --
!     THE FILES THAT DATAPLOT USES ARE  --
!           1) A SIGN-ON MESSAGE FILE CONTAINING THE LATEST IN DATAPLOT
!              INFORMATION.  THIS FILE IS AUTOMATICALLY PRINTED OUT IN
!              THE FORM OF A MESSAGE WHICH THE ANALYST SEES WHEN SIGNING
!              ONTO DATAPLOT.  IT TYPICALLY CONSISTS OF ONLY A FEW LINES
!              OF INFORMATION.  THE VARIABLE NAMES ALL START WITH    IMES,
!              AS IN IMESNU, IMESNA, IMESST, ETC.
!           2) A NEWS FILE WHICH DATAPLOT MAKES USE OF WHENEVER THE
!              ANALYST ENTERS THE NEWS COMMAND.  THE VARIABLE NAMES ALL
!              START WITH    INEW,
!           3) A MAIL FILE DATAPLOT MAKES USE OF WHENEVER THE ANALYST
!              ENTERS THE MAIL COMMAND FOLLOWED BY HIS/HER LAST NAME.
!              THE VARIABLE NAMES ALL START WITH    IMAI.
!              NOTE: THE MAIL COMMAND IS OBSOLETE AND HAS BEEN REMOVED
!              FROM DATAPLOT.
!           4) A HELP (= DOCUMENTATION) FILE THAT DATAPLOT MAKES USE OF
!              WHENEVER THE ANALYST ENTERS THE HELP COMMAND.  THE VARIABLE
!              NAMES ALL START WITH    IHEL, AS IN IHELNU, IHELNA,
!              IHELST, ETC.
!           5) A BUGS FILE THAT DATAPLOT MAKES USE OF WHENEVER THE ANALYST
!              ENTERS THE BUGS COMMAND.  THE VARIABLE NAMES ALL START WITH
!              IBUG, AS IN IBUGNU, IBUGNA, IBUGST, ETC.
!           6) A QUERY FILE THAT DATAPLOT WRITES TO WHENEVER THE ANALYST
!              ENTERS THE QUERY COMMAND FOLLOWED BY A COMMENT OF INTEREST.
!              THE VARIABLE NAMES ALL START WITH    IQUE,
!              AS IN IQUENU, IQUENA, IQUEST, ETC.
!           7) A SIGN-ON SYSTEM LOGIN FILE THAT GETS CALLED EVERY TIME THAT
!              DATAPLOT GETS INVOKED.  THIS FILE IS A HANDY PLACE FOR THE
!              IMPLEMENTOR TO PLACE DATAPLOT COMMANDS SO AS TO TAILOR
!              DATAPLOT FOR AN ENTIRE SITE.  THE VARIABLE NAMES ALL START
!              WITH    ISYS, AS IN ISYSNU, ISYSNA, ISYSST, ETC.
!           8) A USER LOGIN FILE (IN THE USER'S DIRECTORY) THAT GETS
!              CALLED EVERY TIME THAT DATAPLOT GETS INVOKED BY THAT USER.
!              THIS FILE IS A HANDY PLACE FOR THE USER TO PLACE DATAPLOT
!              COMMANDS SO AS TO TAILOR DATAPLOT FOR THE INDIVIDUAL USER'S
!              PARTICULAR TERMINAL AND PLOTTER.  THE VARIABLE NAMES ALL
!              START WITH    ILOG, AS IN ILOGNU, ILOGNA, ILOGST, ETC.
!           9) A DIRECTORY FILE WHICH CONSISTS OF A LIST OF FILE NAMES
!              (AND 1-LINE DESCRIPTIONS) FOR INDIVIDUAL ON-LINE MASTER
!              REFERENCE FILES, INDIVIDUAL ON-LINE DATA FILES, AND
!              INDIVIDUAL ON-LINE PROGRAM FILES.  THIS FILE IS USUALLY
!              ACCESSED VIA THE LIST AND SEARCH COMMANDS.
!          10) A READ  FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
!              ARISES IN CONNECTION WITH THE READ AND SERIAL READ
!              COMMANDS IN READING VARIABLES/PARAMETERS/FUNCTIONS IN FROM
!              A MASS STORAGE FILE.  THE VARIABLE NAMES ALL START WITH
!              IREA, AS IN IREANU, IREANA, IREAST, ETC.
!          11) A WRITE FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
!              ARISES IN CONNECTION WITH THE WRITE COMMAND IN WRITING
!              VARIABLES/PARAMETERS/FUNCTIONS OUT TO A MASS STORAGE FILE.
!              THE VARIABLE NAMES ALL START WITH    IWRI, AS IN IWRINU,
!              IWRINA, IWRIST, ETC.
!          12) A SAVE FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
!              ARISES IN CONNECTION WITH THE SAVE AND RESTORE COMMANDS
!              IN EFFICIENTLY DUMPING OUT (OR ROLLING BACK IN) ALL OF
!              THE DATAPLOT INTERNAL SETTINGS FOR RESUMING A DATAPLOT RUN
!              AT A LATER TIME.  THIS FILE IS USED IN CONNECTION WITH
!              THE SAVE AND RESTORE COMMANDS.  THE VARIABLE NAMES ALL
!              START WITH    ISAV, AS IN ISAVNU, ISAVNA, ISAVST, ETC.
!          13) A LIST  FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
!              ARISES IN CONNECTION WITH THE LIST COMMAND IN PASSIVELY
!              LISTING THE CONTENTS OF A MASS STORAGE FILE.  THE VARIABLE
!              NAMES ALL START WITH    ILIS, AS IN ILISNU, ILISNA,
!              ILISST, ETC.
!          14) A MACRO FILE WHOSE NAME IS SUPPLIED BY THE ANALYST AND
!              ARISES IN CONNECTION WITH THE    CREATE    AND    CALL
!              COMMANDS WHEN DYNAMICALLY FORMING OR EXECUTING A MACRO
!              WHILE RUNNING DATAPLOT.  THE VARIABLE NAMES ALL START
!              WITH    ICRE, AS IN ICRENU, ICRENA, ICREST, ETC.
!          15) A (TEXT) CAPTURE FILE WHOSE NAME IS SUPPLIED BY THE ANALYST
!              AND ARISES IN CONNECTION WITH THE CAPTURE/REDIRECT COMMANDS
!              WHEN DYNAMICALLY CAPTURING TEXT OUTPUT FROM ANY DATAPLOT
!              COMMANDS.  THE VARIABLE NAMES ALL START WITH    ICAP,
!              AS IN ICAPNU, ICAPNA, ICAPST, ETC.
!          16) A TEMPORARY SCRATCH FILE THAT DATAPLOT MAKES USE OF (TO
!              SAVE SPACE) DURING THE FIT COMMAND, THE PRE-FIT COMMAND,
!              AND THE SPLINE FIT COMMAND.  THE VARIABLE NAMES ALL START
!              WITH    ISCR, AS IN ISCRNU, ISCRNA, ISCRST, ETC.
!          17) FOR FUTURE DEVELOPMENT-- A DATA FILE THAT DATAPLOT COULD
!              COULD MAKE USE OF IN STORING THE MAIN INTERNAL DATA ARRAY
!              IF SUCH AN ARRAY IS LARGER THAN CAN BE HELD INTERNALLY IN
!              MAIN MEMORY.  SUCH A DATA FILE IS NOT CURRENTLY USED
!              BUT HAS BEEN ENTERED FOR FUTURE DEVELOPMENT.  THE VARIABLE
!              NAMES ALL START WITH    IDAT, AS IN IDATNU, IDATNA,
!              IDATST, ETC.
!          18) A PLOT FILE THAT DATAPLOT WRITES A PLOT OUT TO WHENEVER
!              SIMULTANEOUS SECONDARY PLOTS ARE CALLED FOR
!              (AS IN DEVICE 2 TEKTRONIX 4014
!                     DEVICE 2 HP-GL
!                     DEVICE 2 GENERAL
!                     DEVICE 2 etc.
!              THE VARIABLE NAMES ALL  TART WITH    IPL1,
!              AS IN IPL1NU, IPL1NA, IPL1ST, ETC.
!          19) ANOTHER PLOT FILE THAT DATAPLOT COULD WRITE A PLOT OUT TO
!              WHENEVER SIMULTANEOUS TERTIARY PLOTS ARE CALLED FOR
!              (AS IN DEVICE 3 TEKTRONIX 4014
!                     DEVICE 3 HP-GL
!                     DEVICE 3 GENERAL
!                     DEVICE 3 etc.
!              THE VARIABLE NAMES ALL START WITH    IPL2,
!              AS IN IPL2NU, IPL2NA, IPL2ST, ETC.
!          20) A PROGRAM FILE THAT DATAPLOT WRITES TO AND RUNS FROM IN
!              CONJUNCTION WITH CERTAIN "PRE-PACKAGED" COMMANDS SUCH AS
!              4-PLOT    AND    RUN RANDOMNESS.  THE VARIABLE NAMES ALL
!              START WITH    IPRO, AS IN IPRONU, IPRONA, IPROST, ETC.
!          21) A CONCLUSIONS FILE THAT DATAPLOT WRITES TO AND READS FROM
!              IN CONJUNCTION WITH FORMING CONCLUSIONS AS PART OF
!              DATAPLOT'S EXPERT SUB-SYSTEM THE VARIABLE NAMES ALL START
!              WITH    ICON, AS IN ICONNU, ICONNA, ICONST, ETC.
!          22) A COMMAND-SAVE FILE THAT DATAPLOT WRITES TO AND READS FROM
!              IN CONJUNCTION WITH SAVING COMMANDS (VIA THE    SAVE COMMAND
!              COMMAND), AND REEXECUTING COMMANDS (VIA THE CALL COMMAND).
!              THE VARIABLE NAMES ALL START WITH    ISAC,
!              AS IN ISACNU, ISACNA, ISACST, ETC.
!          23) A LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
!              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
!              EXPERT SUB-SYSTEM.  THE VARIABLE NAMES ALL START WITH
!              IEX1, AS IN IEX1NU, IEX1NA, IEX1ST, ETC.
!          24) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
!              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
!              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
!              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX2,
!              AS IN IEX2NU, IEX2NA, IEX2ST, ETC.
!          25) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
!              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
!              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
!              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX3,
!              AS IN IEX3NU, IEX3NA, IEX3ST, ETC.
!          26) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
!              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
!              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
!              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX4,
!              AS IN IEX4NU, IEX4NA, IEX4ST, ETC.
!          27) ANOTHER LOGIC-TREE MENU FILE THAT DATAPLOT ACCESSES IN
!              CONJUNCTION WITH DISPLAYING MENUS AS PART OF DATAPLOT'S
!              EXPERT SUB-SYSTEM (THIS FILE WILL BE USED IN FUTURE
!              VERSIONS).  THE VARIABLE NAMES ALL START WITH    IEX5,
!              AS IN IEX5NU, IEX5NA, IEX5ST, ETC.
!CCCC THE FOLLOWING SECTION IS A SHRINKAGE OF 9 SECTIONS JUNE 1990
!          28 TO 37) HELP (DOCUMENTATION) FILES WHICH DATAPLOT ACCESSES
!              IN CONJUNCTION WITH DISPLAYING INFORMATION AS PART OF
!              DATAPLOT'S HELP SUB-SYSTEM.  THE VARIABLE NAMES ALL START
!              WITH    IHE1 THROUGH IHE9, AS IN     IHE1NU, IHE1NA,
!              IHE1ST, ETC.  THROUGH   IHE9NU, IHE9NA, IHE9ST, ETC.
!CCCC THE FOLLOWING 9 SECTIONS WERE ADDED JUNE 1990
!          38 TO 47) MENU FILES WHICH DATAPLOT ACCESSES IN IN CONJUNCTION
!              WITH DISPLAYING INFORMATION AS PART OF DATAPLOT'S MENU
!              SUB-SYSTEM.  THE VARIABLE NAMES ALL START WITH    IME1
!              THROUGH IME9, AS IN     IME1NU, IME1NA, IME1ST, ETC.
!              THROUGH   IME9NU, IME9NA, IME9ST, ETC.
!              NOTE THAT THE MENU SUBSYSTEM IS OBSOLETE AND THESE FILES
!              ARE NO LONGER USED.
!     THE FILE ATTRIBUTES THAT DATAPLOT DEFINES ARE--
!           1) THE FORTRAN LOGICAL UNIT NUMBER
!              (AN INTEGER).
!              THE VARIABLE NAMES ALL END IN    NU,
!              AS IN IMESNU, IHELNU, IREANU, ETC.
!              RECOMMENDED SETTINGS (IF THESE POSE A CONFLICT
!              AT YOUR SITE, THEN CHANGE THEM ACCORDINGLY)--
!
!                 IMESNU=21
!                 INEWNU=22
!                 IMAINU=23
!                 IHELNU=24
!                 IBUGNU=25
!                 IQUENU=26
!                 ISYSNU=27
!                 ILOGNU=28
!                 IDIRNU=29
!                 IDICNU=30
!
!                 IREANU=31
!                 IWRINU=32
!                 ISAVNU=33
!                 ILISNU=34
!                 ICRENU=35
!
!                 ISCRNU=41
!                 IDATNU=42
!                 IPL1NU=43
!                 IPL2NU=44
!                 IPRONU=45
!                 ICONNU=46
!                 ISACNU=47
!
!                 IEX1NU=51
!                 IEX2NU=52
!                 IEX3NU=53
!                 IEX4NU=54
!                 IEX5NU=55
!
!CCCC THE FOLLOWING LINE WAS ADDED APRIL 1997
!                 IHRMNU=60
!                 IHE1NU=61
!                 IHE2NU=62
!                 IHE3NU=63
!                 IHE4NU=64
!                 IHE5NU=65
!                 IHE6NU=66
!                 IHE7NU=67
!                 IHE8NU=68
!                 IHE9NU=69
                                                                                                                                  
!CCCC THE FOLLOWING 9 LINES WERE ADDED JUNE 1990
!                 IME1NU=71
!                 IME2NU=72
!                 IME3NU=73
!                 IME4NU=74
!                 IME5NU=75
!                 IME6NU=76
!                 IME7NU=77
!                 IME8NU=78
!                 IME9NU=79
!CCCC THE FOLLOWING 11 LINES WERE ADDED AUGUST 1990
!                 IM10NU=80
!                 IM11NU=81
!                 IM12NU=82
!                 IM13NU=83
!                 IM14NU=84
!                 IM15NU=85
!                 IM16NU=86
!                 IM17NU=87
!                 IM18NU=88
!                 IM19NU=89
!                 IM20NU=90
!CCCC THE FOLLOWING 3 LINES WERE ADDED OCTOBER 1991
!                 IST1NU=91
!                 IST2NU=92
!                 IST3NU=93
!CCCC THE FOLLOWING LINE WAS ADDED FEBRUARY 1994
!                 IST4NU=94
!                 IST5NU=95
!CCCC THE FOLLOWING LINE WAS ADDED JANUARY  2004
!                 IZCHNU=97
!CCCC THE FOLLOWING LINE WAS ADDED DECEMBER 2018
!                 ILINNU=98
!
!           2) THE FILE NAME (A CHARACTER*80 VARIABLE).  THE VARIABLE
!              NAMES ALL END IN    NA, AS IN IMESNA, IHELNA, IREANA, ETC.
!              FILES 21 TO 27 AND 51 AND ABOVE ARE PERMANENT DATAPLOT
!              FILES, AND SO THE FULL FILE NAME DEFINING EXACTLY WHERE THE
!              FILE RESIDES SHOULD BE EXPLICIT (INCLUDING, IF NEED BE, THE
!              DEVICE, DIRECTORY, AND SUBDIRECTORIES SHOULD BE INCLUDED).
!              THESE FILES USUALLY RESIDE IN A MASTER DATAPLOT DIRECTORY
!              OR IN THE IMPLEMENTOR'S DIRECTORY; IF SUCH IS THE CASE,
!              THEN ALSO MAKE SURE THE SYSTEM-PROTECTION ON THESE FILES
!              IS SUCH THAT ANYBODY ELSE CAN ACCESS THEM--THAT IS, ALLOW
!              "WORLD" ACCESS.  FILES 31 TO 35 ARE USER-DEFINED FILES
!              AND SO CONTAIN A DUMMY NAME (-999).  FILES 41 TO 46 ARE
!              DATAPLOT-GENERATED FILES WHICH WILL END UP IN THE USER'S
!              CURRENT DIRECTORY.  THESE FILES MAY BE EITHER TEMPORARY OR
!              PERMANENT IN THE SENSE THAT THE USER MAY EITHER MANUALLY OR
!              AUTOMATICALLY DELETE THEM (IF HE/SHE SO CHOOSES) AFTER
!              EXITING OUT OF DATAPLOT.
!
!           3) THE FILE (EXISTENCE) STATUS (A CHARACTER*12 VARIABLE).
!              THE VARIABLE NAMES ALL END IN    ST, AS IN IMESST, IHELST,
!              IREAST, ETC.  THERE ARE 3 POSSIBLE SETTINGS--
!                 1) OLD     (THAT IS, THE FILE PRE-EXISTS)
!                 2) NEW     (THAT IS, THE FILE DOES NOT PRE-EXIST)
!                 3) UNKNOWN (THAT IS, EITHER CASE IS POSSIBLE)
!              FILES 21 TO 27 ARE OLD.
!              FILES 31 TO 35 ARE UNKNOWN.
!              FILES 41 TO 46 ARE UNKNOWN.
!              FILES 51 AND ABOVE ARE OLD.
!
!           4) THE FILE (FORTRAN I/O) FORMAT (A CHARACTER*12 VARIABLE).
!              THE VARIABLE NAMES ALL END IN    FO, AS IN IMESFO, IHELFO,
!              IREAFO, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
!                 1) FORMATTED   (THAT IS, THE CONTENTS OF THE FILE ARE
!                                READABLE VIA A FORMATTED FORTRAN READ).
!                                THE FILE IS THUS EDITABLE VIA MOST EDITORS,
!                                BUT ARE SLOWER TO CREATE AND READ.
!                 2) UNFORMATTED (THAT IS, THE CONTENTS OF THE FILE
!                                ARE READABLE ONLY VIA AN UNFORMATTED
!                                FORTRAN READ.  THE FILE IS THUS
!                                UNEDITABLE BY MOST EDITORS,
!                                BUT ARE FASTER TO CREATE AND READ.
!               ALL OF DATAPLOT'S FILES ARE FORMATTED
!               EXCEPT THE SCRATCH FILE (ISCRNA--FILE 41
!               AND    THE DATA    FILE (IDATNA--FILE 42).
!
!           5) THE FILE ACCESS ATTRIBUTE (A CHARACTER*12 VARIABLE).
!              THE VARIABLE NAMES ALL END IN    AC, AS IN IMESAC, IHELAC,
!              IREAAC, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
!                 1) SEQUENTIAL (THAT IS, THE CONTENTS OF THE FILE ARE
!                               ACCESSED IN A SEQUENTIAL FASHION.
!                               SEQUENTIAL ACCESS FILES ARE SIMPLER IN
!                               STRUCTURE BUT SLOWER TO ACCESS.
!     b           2) DIRECT     (THAT IS, THE CONTENTS OF THE FILE
!                               ARE ACCESSED DIRECTLY--
!                               A RECORD IN THE MIDDLE OF THE FILE
!                               MAY THUS BE ACCESSED DIRECTLY WITHOUT
!                               THE NEED TO READ THROUGH ALL PREVIOUS
!                               RECORDS.  DIRECT ACCESS FILES
!                               ARE USUALLY UNEDITABLE, ARE
!                               USUALLY MORE COMPLICATED IN STRUTURE,
!                               BUT ARE FASTER TO ACCESS.
!              DIRECT-ACCESS FILES ARE NOT SUPPORTED IN FORTRAN 77, THUS
!              DATAPLOT DOES NOT MAKE USE OF THEM (THAT IS, ALL OF
!              DATAPLOT'S FILE ARE SEQUENTIAL).  IF ONE WERE TO DEVIATE
!              FROM DATAPLOT'S DEFAULT SETTINGS IN REGARD TO SEQUENTIAL
!              VERSUS DIRECT-ACCESS FILES, THEN THE PRIMARY CANDIDATE
!              WOULD BE THE HELP FILE (IHELNA)--MAKING THIS DIRECT ACCESS
!              WOULD SPEED UP THE USE OF THE HELP COMMAND; THIS SHOULD BE
!              DONE ONLY, HOWEVER, AFTER THE DEFAULT DATAPLOT
!              IMPLEMENTATION HAS BEEN DONE AND IS RUNNING SATISFACTORILY.
!
!           6) THE FILE READ/WRITE PROTECTION ATTRIBUTE (A CHARACTER*12
!              VARIABLE).  THE VARIABLE NAMES ALL END IN  PR, AS IN IMESPR,
!              IHELPR, IREAPR, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
!                 1) READWRITE (THAT IS, THE CONTENTS OF THE FILE MAY
!C                              SEQUENTIAL ACCESS FILES ARE SIMPLER IN
!                               BE BOTH READ FROM AND WRITTEN TO DURING A
!                               DATAPLOT RUN.  THE FILE IS THUS FREELY
!                               ACCESSED FOR BOTH READING AND WRITING.
!                 2) READONLY   (THAT IS, THE FILE MAY
!                               BE READ FROM, BUT MAY NOT BE WRITTEN INTO.
!                               THE FILE THUS HAS ONLY LIMITED ACCESS.
!              FILES 21 TO 27 (EXCEPT FILE 23) ARE READONLY.
!              FILES 31 TO 35 ARE READWRITE.
!              FILES 41 TO 46 ARE READWRITE.
!              FILES 51 AND ABOVE ARE READONLY.
!
!           7) THE FILE OPEN/CLOSE STATUS (A CHARACTER*12 VARIABLE).
!              THE VARIABLE NAMES ALL END IN  CS, AS IN IMESCS, IHELCS,
!              IREACS, ETC.  THERE ARE 2 POSSIBLE SETTINGS--
!                 1) OPEN   (THAT IS, THE FILE IS CURRENTLY OPEN).
!                 2) CLOSED (THAT IS, THE FILE IS CURRENTLY CLOSED).
!              UPON ACCESSING DATAPLOT, ALL FILES ARE CLOSED.  AT
!              VARIOUS TIMES WITHIN A DATAPLOT RUN, A GIVEN FILE MAY BE
!              OPEN OR CLOSED--DEPENDING ON WHAT THE ANALYST IS DOING.
!              UPON EXITING DATAPLOT, ANY FILES WHICH HAPPEN
!              TO BE OPEN WILL BE CLOSED.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/7
!     ORIGINAL VERSION--DECEMBER  1985.
!     UPDATED         --JULY      1986.
!     UPDATED         --SEPTEMBER 1987. (EXPANDED HELP)
!     UPDATED         --JANUARY   1988. (DIRECTORY FILE)
!     UPDATED         --AUGUST    1988. (DICTIONARY FILE)
!     UPDATED         --JUNE      1990. MENU 1 THRU 9
!     UPDATED         --JULY      1990. PL1/PL2/CON/SAC  NEW TO UNKNOWN
!     UPDATED         --AUGUST    1990. MENU 11 THRU 20
!     UPDATED         --SEPTEMBER 1990. USER-DEFINABLE DOS DIRECTORY
!     UPDATED         --APRIL     1991. MERGE ALAN/JJF VERSIONS
!     UPDATED         --OCTOBER   1991. STORAGE 1, 2, AND 3
!     UPDATED         --NOVEMBER  1991. HEAVILY MODIFIED FOR EASIER
!                                       USER IMPLEMENTATION (ALAN)
!     UPDATED         --MARCH     1992. GENERAL OUTPUT FILE
!                                       (INCLUDING LASER PRINTER)
!     UPDATED         --APRIL     1992. ADD SOME DECLARATIONS, MAKE
!                                       MODIFICATION INSTRUCTIONS CLEAR
!     UPDATED         --AUGUST    1992. FILE PERMISSION FOR DPST<1/2/3>F
!     UPDATED         --AUGUST    1992. FOR EDIT COMMAND
!     UPDATED         --JANUARY   1994. CHECK FOR SET DATAPLO$, FED$
!     UPDATED         --FEBRUARY  1994. DELETE SOME OBSOLETE COMMENTS
!                                       TO AVOID CONFUSION.
!     UPDATED         --APRIL     1996. FOR UNIX, ALLOW FILE AREA FOR
!                                       TO BE SET VIA:
!                                         setenv DATAPLOT_FILES
!     UPDATED         --APRIL     1996. SET PATH, NCPATH FOR PC
!     UPDATED         --JULY      1996. FOR UNIX, CHECK FOR PRESCENCE
!                                       OF "HOME" ENVIORNMENT VARIABLE
!                                       IF FOUND, READ DPLOGF FROM
!                                       HOME DIRECTORY RATHER THAN
!                                       CURRENT DIRECTORY
!     UPDATED         --JULY      1996. DATAPLOT_WEB VARIABLE
!     UPDATED         --AUGUST    1996. FIXES FOR SEARCHING SUB-DIRECTORIES
!     UPDATED         --APRIL     1997. BROWSER VARIABLE
!     UPDATED         --APRIL     1997. DATAPLOT_HOME_PAGE VARIABLE
!     UPDATED         --APRIL     1997. URL VARIABLE
!     UPDATED         --APRIL     1997. UNIT FOR WEB HELP COMMAND
!                                       (IHRMNU)
!     UPDATED         --APRIL     1997. COMBINE UNIX HOSTS
!     UPDATED         --APRIL     1997. DIFFERERT UNIT FOR CREATE AND
!                                       CALL
!     UPDATED         --MARCH     1999. UNIT FOR WEB HANDBOOK COMMAND
!     UPDATED         --JANUARY   2004. UNIT FOR CHARACTER DATA
!     UPDATED         --JUNE      2010. REMOVE MAIL AND QUERY FILES AS THESE
!                                       ARE NOW OBSOLETE.  ALSO REMOVE THE
!                                       "MENU" AND "EXPERT" FILES AS THESE ARE
!                                       ALSO NOW OBSOLETE.  RENUMBER SOME OF THE
!                                       FILES ACCORDINGLY.
!     UPDATED         --APRIL     2018. USE PRE-PROCESSSOR DIRECTIVES TO
!                                       ACTIVATE OS DEPENDENT CODE
!     UPDATED         --DECEMBER  2018. "ILINNU" FOR "READ LINE" COMMAND
!     UPDATED         --MARCH     2019. METHOD FOR EXTRACTING LOCATION
!                                       OF AUXILIARY FILES FOR LINUX
!     UPDATED         --MARCH     2019. FOR LINUX, USE "xdg-open" IF NO
!                                       BROWSER ENVIRONMENT VARIABLE SET
!     UPDATED         --DECEMBER  2019. RETRIEVE THE FOLLOWING
!                                       ENVIRONMENT VARIABLES FOR
!                                       WINDOWS:
!                                         1. %PROGRAMFILES%
!                                         2. %PROGRAMFILES(X86)%
!                                         3. %USERNAME%
!                                         4. %LOCALAPPDATA%
!                                         5. %COMPUTERNAME%
!                                         6. %USERPROFILE%
!                                         7. %PROCESSOR_ARCHITECTURE%
!                                       SAVE IN A COMMON BLOCK
!     UPDATED         --DECEMBER  2019. RETRIEVE THE FOLLOWING
!                                       ENVIRONMENT VARIABLES FOR
!                                       WINDOWS:
!                                         1. $HOME
!                                         2. $USER
!                                         3. $HOST
!                                         4. $PRINTER
!                                       SAVE IN A COMMON BLOCK
!     UPDATED         --JUNE      2024. CHANGE THE DEFAULT EXTENSION
!                                       FOR DPPL1F AND DPPL2F TO ".PS"
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
#ifdef INTEL_CONS
      USE MSFLIB
#endif
#ifdef INTEL_QWIN
      USE IFPORT
#endif
#ifdef INTEL_CONSOLE
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
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 IBUGIN
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!  NOVEMBER 1991.  FOLLOWING BLOCK ADDED
!
!CCCC CHARACTER*80 IPATH1
!CCCC CHARACTER*80 IPATH2
      CHARACTER (LEN=MAXFNC) :: IPATH1
      CHARACTER (LEN=MAXFNC) :: IPATH2
      CHARACTER*6  INAME
      CHARACTER*10 IEXT1
      CHARACTER*10 IEXT2
      CHARACTER*10 IEXT3
      CHARACTER*4  ICASFL
!
!  JUNE 1996.  FOLLOWING BLOCK ADDED
!
!CCCC CHARACTER*80 IPATH3
      CHARACTER (LEN=MAXFNC) :: IPATH3
      CHARACTER*4 IFHOME
!
!  JULY 1996.  FOLLOWING BLOCK ADDED
!
#ifdef LINUX
      CHARACTER*20 IGUII2
!CCCC CHARACTER*80 ITEMP
      CHARACTER (LEN=MAXFNC) :: ITEMP
#endif
!
!
!CCCC THE FOLLOWING LINE WAS ADDED SEPTEMBER 1990
!CCCC CHARACTER*80 ICDIR
      CHARACTER (LEN=MAXFNC) :: ICDIR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHO.INC'
!CCCC AUGUST 1992.  FOLLOWING COMMON BLOCK FOR EDIT COMMAND
!CCCC CHARACTER*80 IEDDIR
      CHARACTER (LEN=MAXFNC) :: IEDDIR
      CHARACTER*10 IEDEXT
      CHARACTER*4 IEDCAS
      COMMON /ICEDC4/   &
      IEDDIR,IEDEXT,IEDCAS
      COMMON/ICEDI4/   &
      NCEDT1,NCEDT2
!
      CHARACTER*80 PROFIL
      CHARACTER*80 P86FIL
      CHARACTER*80 APPDAT
      CHARACTER*80 COMNAM
      CHARACTER*80 UPROFI
      CHARACTER*80 DEFPRI
      CHARACTER*20 USRNAM
      CHARACTER*20 ISHELL
      CHARACTER*4  WINBIT
      COMMON/SYSVAR/PROFIL,P86FIL,APPDAT,COMNAM,UPROFI,USRNAM,DEFPRI,   &
                    WINBIT,ISHELL
      COMMON/SYSVA2/NCPROF,NCP86F,NCAPPD,NCCOMP,NCUPRO,NCUSER,NCPRIN,   &
                    NCSHEL
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='INIT'
      ISUBN2='FO  '
      IFHOME='NO'
      IGUIIO='PIPE'
!
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
!
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF INITFO--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!  MAKE USER CHANGES HERE!!!!
!
!  NOVEMBER 1991.  FOLLOWING SECTION ADDED.  ONLY NEED TO DEFINE "PATH"
!  AND FILE EXTENSIONS ONCE HERE.  COMPLICATES LATER CODE SOMEWHAT,
!  BUT SIMPLIFIES MAKING HOST DEPENDENT CHANGES.
!
!  DATAPLOT USES 2 TYPES OF FILES.  ONE ARE PERMAMNENT FILES SUCH AS
!  THE ON-LINE HELP FILES AND THE NEWS FILES.  THE LOCAL INSTALLOR CAN
!  PUT THESE FILES WHEREVER DESIRED.  THESE FILES TYPICALLY HAVE A
!  ".TEX" EXTENSION, ALTHOUGH THIS CAN BE SET HOWEVER THE LOCAL
!  IMPLEMENTOR CHOOSES.
!
!  THE SECOND TYPE OF FILES ARE TEMPORARY FILES CREATED DURING A
!  DATAPLOT SESSION.  THIS WOULD INCLUDE THE PLOT FILE, SCRATCH FILES,
!  AND OTHER MISCELLANEOUS FILES.  THESE FILES TYPICALLY ARE CREATED IN
!  THE USER'S CURRENT DIRECTORY OR IN SOME TYPE OF TEMPORARY DIRECTORY.
!  AGAIN, THE LOCAL INSTALLOR CAN MAKE THAT CHOICE.  THE FILE EXTENSION
!  IS TYPICALLY ".DAT", BUT THIS CAN ALSO BE SET BY THE LOCAL
!  IMPLEMENTOR.
!
!  IPATH1   = DIRECTORY NAME WHERE DATAPLOT PERMANENT FILES ARE STORED
!  IEXT1    = EXTENSION FOR PERMANENT FILES
!  IPATH2   = DIRECTORY NAME FOR TEMPORARY FILES (E.G., SCRATCH FILES)
!  IEXT2    = EXTENSION FOR TEMPORARY FILES
!  IEXT3    = EXTENSION FOR POSTSCRIPT FILES
!  ICASFL   = 'UPPE' MEANS FILE NAMES ARE UPPER CASE, 'LOWE' MEANS FILE
!             NAMES ARE LOWER CASE.  TYPICALLY SET TO 'LOWE' FOR UNIX
!             SYSTEMS, 'UPPE' FOR OTHERS.
!  IEDDIR   = DIRECTORY FOR THE EDIT COMMAND (WILL USUALLY BE SAME AS
!             IPATH1, BUT DIFFERS ON PC)
!
!  THERE IS A CORRESPONDING VARIABLE THAT DEFINES THE NUMBER OF
!  CHARACTERS, NOTE THAT SETTING THIS VARIABLE TO ZERO IMPLIES NO PATH
!  OR EXTENSION.
!
! --------------------
!
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
        IEXT3='.PS'
        NCEXT3=3
        ICASFL='UPPE'
!
      ELSE IF(IHOST1.EQ.'NVE')THEN
        IPATH1='.CS2.APPLICATIONS.DATAPLOT.VER_2.'
        NCP1=33
        IEDDIR=IPATH1
        NCEDT1=NCP1
!
!       FOR NOS/VE, IMPLEMENTOR CAN DECIDE WHETHER TO PUT TEMPORARY FILES
!       IN THE CURRENT CATALOG OR USE $LOCAL
!
        IPATH2='$LOCAL.'
        NCP2=7
!CCCC   IPATH2=' '
!CCCC   NCP2=0
!       END FILES WITH A ".".  THIS TRAILING DOT IS JUST TO IDENTIFY THE
!       NAME AS A FILE TO DATAPLOT.  THE "DPOPFI" ROUTINE WILL STRIP IT OFF.
        IEXT1='.'
        NCEXT1=1
        IEXT2='.'
        NCEXT2=1
        IEXT3='.PS'
        NCEXT3=3
        ICASFL='UPPE'
!
!CCCC APRIL 1996.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF
!CCCC              "DATAPLOT_FILES" ENVIRONMENT VARIABLE
!CCCC APRIL 1997.  REDUCE TO 1 UNIX SECTION (A BUNCH OF CODE WAS
!CCCC              DELETED< ESSENTIALLY REDUNDANT)
!CCCC FEBRUARY 1998. FOLLOWING SECTION ADDED. WINDOWS VERSION OF
!CCCC                GUI NEEDS SPECIAL HANDLING OF TERMINAL I/O FOR
!CCCC                TCL/TK SCRIPTS TO WORK.  THE ENVIRONMENT VARIABLE
!CCCC                   DATAPLOT_GUI_IO <PIPE/FILE>
!CCCC                SPECIFIES WHETHER OR NOT TO DO THIS SPECIAL CODE.
!
      ELSE IF(   &
             (IHOST1.EQ.'SUN') .OR.   &
             (IHOST1.EQ.'CRAY' .AND. IOPSY1.EQ.'UNIX') .OR.   &
             (IHOST1.EQ.'CONV') .OR.   &
             (IHOST1.EQ.'SGI ') .OR.   &
             (IHOST1.EQ.'HP-9') .OR.   &
             (IHOST1.EQ.'AIX ') .OR.   &
             (IHOST1.EQ.'LINU') .OR.   &
             (IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')   &
          )THEN
!
#ifdef LINUX
        CALL getenv('DATAPLOT_GUI_IO',IGUII2)
        IF(IGUII2.EQ.'FILE'.OR.IGUII2.EQ.'file')IGUIIO='FILE'
!
!       CHECK FOR LOCATION OF DATAPLOT AUXILIARY FILES.
!
!         1. FIRST CHECK FOR THE ENVIRONMENT VARIABLE
!
!               DATAPLOT_FILES
!
!         2. IF DATAPLOT_FILES IS NOT DEFINED, THEN
!            CHECK IF THE FOLLOWING COMPILE OPTION
!            USED (THE SPECIFIED PATH CAN VARY).
!
!               '-DDPLIBDIR='"/usr/local/lib/dataplot"'
!
!         3. IF NEITHER OF THESE ARE USED, THEN DEFAULT
!            TO "/usr/local/lib/dataplot".
!
!       STEP 1: CHECK FOR "DATAPLOT_FILES" ENVIRONMENT VARIABLE.
!
        UNIXPN=' '
        UNIXPV='DATAPLOT_FILES'
        CALL getenv(UNIXPV,UNIXPN)
!
#define STRZZZ(x)    x
#ifdef DPLIBDIR
        IF(UNIXPN.EQ.' ')THEN
          UNIXPN=STRZZZ(DPLIBDIR)
          NLAST=0
!CCCC     DO111II=80,1,-1
          DO 111 II=255,1,-1
            IF(UNIXPN(II:II).NE.' ')THEN
              NLAST=II
              GO TO 119
            ENDIF
  111     CONTINUE
  119     CONTINUE
        ENDIF
#endif
!
!       IF NEITHER DATAPLOT_FILES OR prefix COMPILE SWITCH
!       GIVEN, THEN DEFAULT TO "/usr/local/lib/dataplot".
!
!
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
!CCCC     DO1001I=80,1,-1
          DO 1001 I=255,1,-1
            NCP1=I
            IF(UNIXPN(I:I).NE.' ')GO TO 1009
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
!
!CCCC   AUGUST 1996.  TO MAKE SEARCH OF SUB-DIRECTORIES WORK, SET PATH
!CCCC                 TO BE EMPTY.
!
        IPATH1=' '
        NCP1=0
!
!CCCC   JUNE 1996.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF
!CCCC               "HOME" ENVIRONMENT VARIABLE.  READ DPLOGF FROM
!CCCC               USER'S HOME DIRECTORY IF FOUND.  OTHERWISE,
!CCCC               CURRENT DIRECTORY.
!
        UNIXPV='HOME'
        CALL getenv(UNIXPV,IPATH3)
        IF(IPATH3.NE.' ')THEN
          IFHOME='YES'
          DO 1002 I=80,1,-1
            NCP3=I
            IF(IPATH3(I:I).NE.' ')GO TO 1003
 1002     CONTINUE
 1003     CONTINUE
          NCP3=NCP3+1
          IPATH3(NCP3:NCP3)='/'
        ENDIF
        UPROFI(1:NCP3)=IPATH3(1:NCP3)
        NCUPRO=NCP3
!
        CALL getenv('USER',USRNAM)
        IF(USRNAM.NE.' ')THEN
          DO 7002 I=20,1,-1
            NCUSER=I
            IF(USRNAM(I:I).NE.' ')GO TO 7003
 7002     CONTINUE
 7003     CONTINUE
        ENDIF
!
        CALL getenv('SHELL',ISHELL)
        IF(ISHELL.NE.' ')THEN
          DO 7007 I=20,1,-1
            NCSHEL=I
            IF(ISHELL(I:I).NE.' ')GO TO 7008
 7007     CONTINUE
 7008     CONTINUE
        ENDIF
!
        IF(ISHELL.EQ.'/bin/bash')THEN
          CALL getenv('HOSTNAME',COMNAM)
        ELSE
          CALL getenv('HOST',COMNAM)
        ENDIF
        IF(COMNAM.NE.' ')THEN
          DO 7012 I=80,1,-1
            NCCOMP=I
            IF(COMNAM(I:I).NE.' ')GO TO 7013
 7012     CONTINUE
 7013     CONTINUE
        ENDIF
!
        CALL getenv('PRINTER',DEFPRI)
        IF(DEFPRI.NE.' ')THEN
          DO 7022 I=80,1,-1
            NCPRIN=I
            IF(DEFPRI(I:I).NE.' ')GO TO 7023
 7022     CONTINUE
 7023     CONTINUE
        ENDIF
!
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
        IEXT3='.ps'
        NCEXT3=3
        ICASFL='LOWE'
!
!CCCC   JULY 1996.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF
!CCCC               "DATAPLOT_WEB" ENVIRONMENT VARIABLE.  IF ON,
!CCCC               TRUE, YES, ASSUME RUNNING DATAPLOT FROM A WEB PAGE.
!CCCC               IF SO, CREATE LOCAL FILES (E.G, DPPL1F.DAT) IN
!CCCC               /tmp DIRECTORY RATHER THAN CURRENT DIRECTORY.
!
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
!
!CCCC APRIL 1997.  FOR UNIX SYSTEMS, CHECK FOR EXISTENCE OF:
!CCCC
!CCCC              1)  "BROWSER" ENVIRONMENT VARIABLE.  THIS ENVIRONMENT
!CCCC                  VARIABLE IS USED BY THE "WEB HELP" COMMAND TO
!CCCC                  SPECIFY WHAT BROWSER WILL BE USED TO EXAMINE THE
!CCCC                  DATAPLOT REFERENCE MANUAL.
!CCCC
!CCCC                  2019/03: USE "xdg-open" IF NO BROWSER SPECIFIED,
!CCCC
!CCCC              2)  "DATAPLOT_URL" ENVIRONMENT VARIABLE.  THIS
!CCCC                  ENVIRONMENT VARIABLE SPECIFIES THE LOCATION OF
!CCCC                  THE DATAPLOT REFERENCE MANUAL.  DEFUALTS TO THE
!CCCC                  NIST SITE.  INCLUDED TO ALLOW SITES TO INSTALL THE
!CCCC                  REFERENCE MANUAL LOCALLY.
!CCCC
!CCCC              3)  "URL" ENVIRONMENT VARIABLE.  THIS ENVIRONMENT
!CCCC                  VARIABLE SPECIFIES THE DEFAULT URL TO USE FOR THE
!CCCC                  WEB COMMAND.
!
        IBROWS=' '
        IDPURL=' '
        IURL=' '
        UNIXPV='BROWSER'
        CALL getenv(UNIXPV,IBROWS)
!CCCC   IF(IBROWS.EQ.' ')IBROWS='firefox'
        IF(IOPSY2.EQ.'MAC ')THEN
          IF(IBROWS.EQ.' ')IBROWS='open'
        ELSE
          IF(IBROWS.EQ.' ')IBROWS='xdg-open'
        ENDIF
!
        UNIXPV='DATAPLOT_URL'
        CALL getenv(UNIXPV,IDPURL)
        IF(IDPURL.EQ.' ')THEN
          IDPURL(1:24)='http://www.itl.nist.gov/'
          IDPURL(25:49)='div898/software/dataplot/'
        ENDIF
!
        UNIXPV='URL'
        CALL getenv(UNIXPV,IURL)
        IF(IURL.EQ.' ')IURL(1:20)='http://www.nist.gov/'
!
        IPATH2=' '
        NCP2=0
        IF(IWBFLG.EQ.'YES')THEN
          IPATH2='/tmp/'
          NCP2=5
        ENDIF
#endif
!
!CCCC THE FOLLOWING SECTION WAS ADDED SEPTEMBER 1990
!CCCC IT ASSUMES THE IMPLEMENTER HAS 2 DOS LINES EXISTING--
!CCCC SET DATAPLO$=the full pathname for the directory
!CCCC              where the implementer put DATAPLOT.EXE
!CCCC              (e.g., SET DATAPLO$=C:\DATAPLOT\)
!CCCC              (this SET command goes anywhere in AUTOEXEC.BAT)
!CCCC SHELL=COMMAND.COM /E:288 /P
!CCCC              (this SHELL command goes as the last line
!CCCC              in CONFIG.SYS)
!CCCC JUNE 1996.  DEPENDING ON WHETHER OTG OR LAHEY COMPILER IS USED.
!CCCC OUR VERSION OF LAHEY (5.11) DOESN'T SEEM TO HAVE VARIABLE
!CCCC READING FUNCTION, SO HARD-CODE TO C:\DATAPLOT.
!CCCC
!CCCC OCTOBER 1996.  UPDATE FOR MICROSOFT COMPILER ON PC.  USE
!CCCC LIBRARY FUNCTION SETENVQQ (WORKS A LOT LIKE UNIX SETENV).
!CCCC APRIL 1997.  FOR IBM/PC SYSTEMS, CHECK FOR EXISTENCE OF:
!CCCC 1)  "BROWSER" SET VARIABLE.  THIS VARIABLE
!CCCC     IS USED BY THE "WEB HELP" COMMAND TO SPECIFY WHAT BROWSER
!CCCC     WILL BE USED TO EXAMINE THE DATAPLOT REFERENCE MANUAL.
!CCCC     DEFAULTS TO NETSCAPE.
!CCCC 2)  "DP_URL" SET VARIABLE.  THIS
!CCCC     VARIABLE SPECIFIES THE LOCATION OF THE DATAPLOT REFERENCE
!CCCC     MANUAL.  DEFUALTS TO THE NIST SITE.  INCLUDED TO ALLOW
!CCCC     SITES TO INSTALL THE REFERENCE MANUAL LOCALLY.
!
!CCCC 12/2002: CHANGE DEFAULT TO "C:\PROGRAM FILES\NIST\DATAPLOT\"
!
!     NEED TO UPDATE LAHEY AND ABSOFT TO RETRIEVE ENVIRONMENT VARIABLES
!
      ELSE IF(IHOST1.EQ.'IBM-')THEN
        IF(ICOMPI.EQ.'LAHE')THEN
#ifdef WIN32_LAHEY
!CCCC     IPATH1='C:\DATAPLOT\'
!CCCC     NCP1=12
          IPATH1='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCP1=31
!CCCC     ICDIR='C:\FED\'
!CCCC     NCEDT1=7
!CCCC     ICDIR='C:\DATAPLOT\'
!CCCC     NCEDT1=12
          ICDIR='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCEDT1=31
#endif
        ELSEIF(ICOMPI.EQ.'ABSO')THEN
#ifdef WIN32_ABSOFT
!CCCC     IPATH1='C:\DATAPLOT\'
!CCCC     NCP1=12
          IPATH1='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCP1=31
!CCCC     ICDIR='C:\FED\'
!CCCC     NCEDT1=7
!CCCC     ICDIR='C:\DATAPLOT\'
!CCCC     NCEDT1=12
          ICDIR='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCEDT1=31
#endif
        ELSE IF(ICOMPI.EQ.'MS-F')THEN
#ifdef INTEL
          IRESLT=GETENVQQ('USERNAME',USRNAM)
          IF(IRESLT.GT.0)THEN
            DO 7111 II=20,1,-1
              IF(USRNAM(II:II).NE.' ')THEN
                NCUSER=II
                GO TO 7119
              ENDIF
 7111       CONTINUE
 7119       CONTINUE
          ENDIF
!
          IRESLT=GETENVQQ('COMPUTERNAME',COMNAM)
          IF(IRESLT.GT.0)THEN
            DO 7121 II=80,1,-1
              IF(COMNAM(II:II).NE.' ')THEN
                NCCOMP=II
                GO TO 7129
              ENDIF
 7121       CONTINUE
 7129       CONTINUE
          ENDIF
!
          IRESLT=GETENVQQ('LOCALAPPDATA',APPDAT)
          IF(IRESLT.GT.0)THEN
            DO 7131 II=80,1,-1
              IF(APPDAT(II:II).NE.' ')THEN
                NCAPPD=II
                GO TO 7139
              ENDIF
 7131       CONTINUE
 7139       CONTINUE
          ENDIF
!
          IRESLT=GETENVQQ('USERPROFILE',UPROFI)
          IF(IRESLT.GT.0)THEN
            DO 7141 II=80,1,-1
              IF(UPROFI(II:II).NE.' ')THEN
                NCUPRO=II
                GO TO 7149
              ENDIF
 7141       CONTINUE
 7149       CONTINUE
          ENDIF
!
          WINBIT='64'
          CTEMP=' '
          IRESLT=GETENVQQ('PROCESSOR_ARCHITECTURE',CTEMP)
          IF(IRESLT.GT.0)THEN
            DO 7101 II=1,80
              IF(CTEMP(II:II+1).EQ.'32')THEN
                WINBIT='32'
              ENDIF
 7101       CONTINUE
 7109       CONTINUE
          ENDIF
!
          IRESLT=GETENVQQ('PROGRAMFILES',PROFIL)
          IF(IRESLT.GT.0)THEN
            DO 7151 II=80,1,-1
              IF(PROFIL(II:II).NE.' ')THEN
                NCPROF=II
                GO TO 7159
              ENDIF
 7151       CONTINUE
 7159       CONTINUE
            IF(WINBIT.EQ.'64')THEN
              DO 7153 II=1,NCPROF-5
                IF(PROFIL(II:II+5).EQ.' (x86)')THEN
                  NCPROF=II-1
                  PROFIL(II:80)=' '
                  GO TO 7155
                ELSEIF(PROFIL(II:II+5).EQ.' (X86)')THEN
                  NCPROF=II-1
                  PROFIL(II:80)=' '
                  GO TO 7155
                ENDIF
 7153         CONTINUE
 7155         CONTINUE
            ENDIF
          ENDIF
!
          IF(WINBIT.EQ.'64')THEN
            IRESLT=GETENVQQ('PROGRAMFILES(X86)',P86FIL)
            IF(IRESLT.GT.0)THEN
              DO 7161 II=80,1,-1
                IF(P86FIL(II:II).NE.' ')THEN
                  NCP86F=II
                  GO TO 7169
                ENDIF
 7161         CONTINUE
 7169         CONTINUE
            ENDIF
          ENDIF
!
          IRESLT=GETENVQQ('DATAPLO$',ICDIR)
          IF(IRESLT.LE.0)THEN
!CCCC       ICDIR='C:\DATAPLOT\'
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
            GO TO 9000
          ENDIF
          IPATH1(1:NCP1)=ICDIR(1:NCP1)
          IF(IPATH1(NCP1:NCP1).NE.'\')THEN
            NCP1=NCP1+1
            IPATH1(NCP1:NCP1)='\'
          ENDIF
!
          IRESLT=GETENVQQ('FED$',IEDDIR)
          IF(IRESLT.EQ.0)THEN
!CCCC       ICDIR='C:\FED\'
!CCCC       NCEDT1=7
!CCCC       NCDIR=7
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
!
          IF(IEDDIR(NCDIR:NCDIR).NE.'\')THEN
            NCDIR=NCDIR+1
            IEDDIR(NCDIR:NCDIR)='\'
          ENDIF
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
            GO TO 9000
          ENDIF
!
!         NEED TO CHANGE THE DEFAULT BROWSER
!
          IBROWS=' '
          IDPURL=' '
          IRESLT=GETENVQQ('BROWSER',IBROWS)
          IF(IRESLT.EQ.0)THEN
            IBROWS(1:42)='"C:\Program Files (x86)\Internet Explorer\'
            IBROWS(43:55)='iexplore.exe"'
          ENDIF
!
          IRESLT=GETENVQQ('DP_URL',IDPURL)
          IF(IRESLT.EQ.0)THEN
            IDPURL(1:24)='http://www.itl.nist.gov/'
            IDPURL(25:49)='div898/software/dataplot/'
          ENDIF
!
          IRESLT=GETENVQQ('URL',IDPURL)
          IF(IRESLT.EQ.0)THEN
            IURL(1:20)='http://www.nist.gov/'
          ENDIF
!
#endif
        ELSE IF(ICOMPI.EQ.'OTG ')THEN
#ifdef WIN32_OTG
          CALL DOSPARAM@('DATAPLO$',ICDIR)
          NCDIR=80
          NMAX=80
          CALL DPDB80(ICDIR,NCDIR,NMAX,IBUGIN,ISUBRO,IERROR)
          NCP1=NCDIR
!
!CCCC THE FOLLOWING ERROR CHECK & WRITE WAS ENTERED    JANUARY 1994
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
            GO TO 9000
          ENDIF
          IPATH1(1:NCP1)=ICDIR(1:NCP1)
!
          CALL DOSPARAM@('FED$',ICDIR)
          NCDIR=80
          NMAX=80
          CALL DPDB80(ICDIR,NCDIR,NMAX,IBUGIN,ISUBRO,IERROR)
          NCEDT1=NCDIR
!
!CCCC THE FOLLOWING ERROR CHECK & WRITE WAS ENTERED      JANUARY 1994
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
            GO TO 9000
          ENDIF
#endif
        ELSE
!CCCC     ICDIR='C:\FED\'
!CCCC     NCEDT1=7
          ICDIR='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCEDT1=31
!CCCC     IPATH1='C:\DATAPLOT\'
!CCCC     NCP1=12
          IPATH1='C:\PROGRAM FILES\NIST\DATAPLOT\'
          NCP1=31
        ENDIF
!
        IEDDIR(1:NCEDT1)=ICDIR(1:NCEDT1)
!CCCC APRIL 1996.  SET PATH, NCPATH
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
        IEXT3='.PS'
        NCEXT3=3
        ICASFL='UPPE'
!
!XXXX ELSE IF(IHOST1.EQ.'XXXX')THEN
!XXXX   INSERT CODE FOR UNSUPPORTED HOST HERE!!!
!XXXX   IPATH1=' '
!XXXX   NCP1=0
!XXXX   IPATH2=' '
!XXXX   NCP2=0
!XXXX   IEXT1='.TEX'
!XXXX   NCEXT1=4
!XXXX   IEXT2='.DAT'
!XXXX   NCEXT2=4
!XXXX   IEXT3='.PS'
!XXXX   NCEXT2=3
!XXXX   ICASFL='LOWE'
!
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
        IEXT3=' '
        NCEXT3=0
        ICASFL='UPPE'
      END IF
!  END USER CHANGES!!!!
!
! --------------------
!
!CCCC AUGUST 1992.  DEFINE DIRECTORY AND EXTENSION FOR EDIT COMMAND
      IEDEXT=IEXT1
      IEDCAS=ICASFL
      NCEDT2=NCEXT1
!
      IMESNU=21
!  NOVEMBER 1991.
      INAME='DPMESF'
      IF(ICASFL.EQ.'LOWE')INAME='dpmesf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IMESNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IMESST='OLD'
      IMESFO='FORMATTED'
      IMESAC='SEQUENTIAL'
      IMESPR='READONLY'
      IMESCS='CLOSED'
!
      INEWNU=22
!  NOVEMBER 1991.
      INAME='DPNEWF'
      IF(ICASFL.EQ.'LOWE')INAME='dpnewf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,INEWNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      INEWST='OLD'
      INEWFO='FORMATTED'
      INEWAC='SEQUENTIAL'
      INEWPR='READONLY'
      INEWCS='CLOSED'
!
!CCCC IMAINU=23
!  NOVEMBER 1991.
!CCCC INAME='DPMAIF'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpmaif'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IMAINA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IMAIST='OLD'
!CCCC IMAIFO='FORMATTED'
!CCCC IMAIAC='SEQUENTIAL'
!CCCC IMAIPR='READONLY'
!CCCC IMAICS='CLOSED'
!
!CCCC IHELNU=24
      IHELNU=23
!  NOVEMBER 1991.
      INAME='DPHELF'
      IF(ICASFL.EQ.'LOWE')INAME='dphelf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHELNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHELST='OLD'
      IHELFO='FORMATTED'
      IHELAC='SEQUENTIAL'
      IHELPR='READONLY'
      IHELCS='CLOSED'
!
!CCCC IBUGNU=25
      IBUGNU=24
!  NOVEMBER 1991.
      INAME='DPBUGF'
      IF(ICASFL.EQ.'LOWE')INAME='dpbugf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IBUGNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IBUGST='OLD'
      IBUGFO='FORMATTED'
      IBUGAC='SEQUENTIAL'
      IBUGPR='READONLY'
      IBUGCS='CLOSED'
!
!CCCC IQUENU=26
!  NOVEMBER 1991.
!CCCC INAME='DPQUEF'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpquef'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IQUENA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IQUEST='OLD'
!CCCC IQUEFO='FORMATTED'
!CCCC IQUEAC='SEQUENTIAL'
!CCCC IQUEPR='READWRITE'
!CCCC IQUECS='CLOSED'
!
!CCCC ISYSNU=27
      ISYSNU=25
!  NOVEMBER 1991.
      INAME='DPSYSF'
      IF(ICASFL.EQ.'LOWE')INAME='dpsysf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,ISYSNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC ISYSST='NONE'
      ISYSST='OLD'
      ISYSFO='FORMATTED'
      ISYSAC='SEQUENTIAL'
!CCCC ISYSPR='READWRITE'
      ISYSPR='READONLY'
      ISYSCS='CLOSED'
!
!CCCC ILOGNU=28
      ILOGNU=26
!  NOVEMBER 1991.
      INAME='DPLOGF'
      IF(ICASFL.EQ.'LOWE')INAME='dplogf'
      NC=6
!CCCC JUNE 1996.  FOR UNIX, PATH DEPENDS ON "HOME" ENVIRONMENT VARIABLE
      IF(IFHOME.EQ.'YES')THEN
        CALL INITF2(INAME,NC,IPATH3,NCP3,IEXT1,NCEXT1,ILOGNA,IBUGIN)
      ELSE
        CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ILOGNA,IBUGIN)
      ENDIF
      IF(IHOST1.EQ.'NVE')ILOGNA='DPLOGF'
!  END OF NOVEMBER 1991 CHANGE
!CCCC ILOGST='NONE'
      ILOGST='OLD'
      ILOGFO='FORMATTED'
      ILOGAC='SEQUENTIAL'
!CCCC ILOGPR='READWRITE'
      ILOGPR='READONLY'
      ILOGCS='CLOSED'
!
!CCCC IDIRNU=29
      IDIRNU=27
!  NOVEMBER 1991.
      INAME='DPDIRF'
      IF(ICASFL.EQ.'LOWE')INAME='dpdirf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IDIRNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IDIRST='OLD'
      IDIRFO='FORMATTED'
      IDIRAC='SEQUENTIAL'
      IDIRPR='READONLY'
      IDIRCS='CLOSED'
!
!CCCC IDICNU=30
      IDICNU=28
!  NOVEMBER 1991.
      INAME='DPDICF'
      IF(ICASFL.EQ.'LOWE')INAME='dpdicf'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IDICNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IDICST='OLD'
      IDICFO='FORMATTED'
      IDICAC='SEQUENTIAL'
      IDICPR='READONLY'
      IDICCS='CLOSED'
!
! --------------------
!
!CCCC IREANU=31
      IREANU=29
      IREANA='-999'
!CCCC IREAST='UNKNOWN'
      IREAST='OLD'
      IREAFO='FORMATTED'
      IREAAC='SEQUENTIAL'
!CCCC IREAPR='READWRITE'
      IREAPR='READONLY'
      IREACS='CLOSED'
!
      IREAN1=11
      IRENA1='-999'
      IREAS1='OLD'
      IREAF1='FORMATTED'
      IREAA1='SEQUENTIAL'
      IREAP1='READONLY'
      IREAC1='CLOSED'
!
      IREAN2=12
      IRENA2='-999'
      IREAS2='OLD'
      IREAF2='FORMATTED'
      IREAA2='SEQUENTIAL'
      IREAP2='READONLY'
      IREAC2='CLOSED'
!
      IREAN3=13
      IRENA3='-999'
      IREAS3='OLD'
      IREAF3='FORMATTED'
      IREAA3='SEQUENTIAL'
      IREAP3='READONLY'
      IREAC3='CLOSED'
!
!CCCC IWRINU=32
      IWRINU=30
      IWRINA='-999'
      IWRIST='UNKNOWN'
      IWRIFO='FORMATTED'
      IWRIAC='SEQUENTIAL'
      IWRIPR='READWRITE'
      IWRICS='CLOSED'
!
      IWRIN1=16
      IWRNA1='-999'
      IWRIS1='UNKNOWN'
      IWRIF1='FORMATTED'
      IWRIA1='SEQUENTIAL'
      IWRIP1='READWRITE'
      IWRIC1='CLOSED'
!
      IWRIN2=17
      IWRNA2='-999'
      IWRIS2='UNKNOWN'
      IWRIF2='FORMATTED'
      IWRIA2='SEQUENTIAL'
      IWRIP2='READWRITE'
      IWRIC2='CLOSED'
!
      IWRIN3=18
      IWRNA3='-999'
      IWRIS3='UNKNOWN'
      IWRIF3='FORMATTED'
      IWRIA3='SEQUENTIAL'
      IWRIP3='READWRITE'
      IWRIC3='CLOSED'
!
!CCCC ISAVNU=33
      ISAVNU=31
!  NOVEMBER 1991.
      INAME='DPSAVF'
      IF(ICASFL.EQ.'LOWE')INAME='dpsavf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ISAVNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      ISAVST='UNKNOWN'
      ISAVFO='UNFORMATTED'
      ISAVAC='SEQUENTIAL'
      ISAVPR='READWRITE'
      ISAVCS='CLOSED'
!
!CCCC ILISNU=34
      ILISNU=32
      ILISNA='-999'
!CCCC ILISST='UNKNOWN'
      ILISST='OLD'
      ILISFO='FORMATTED'
      ILISAC='SEQUENTIAL'
!CCCC ILISPR='READWRITE'
      ILISPR='READONLY'
      ILISCS='CLOSED'
!
!CCCC FIX BUG, HAVE CREATE COMMAND USE DIFFERNT UNIT NUMBER THAN
!CCCC MACRO.  THIS AVOIDS HANG WHEN "CREATE FILE." ENCOUNTERS A
!CCCC A CALL COMMAND.
!CCCC ICRENU=35
      ICRENU=50
      ICREN2=98
      ICRENA='-999'
      ICREST='UNKNOWN'
      ICREFO='FORMATTED'
      ICREAC='SEQUENTIAL'
      ICREPR='READWRITE'
      ICRECS='CLOSED'
!
!     ICAPNU=36
!  DECEMBER, 1989.  UNIT CONFLICT IF HAVE NESTED CALLS.
!  THIS IS AN UNRESOLVED BUG.
      ICAPNU=40
      ICAPNA='-999'
      ICAPST='UNKNOWN'
      ICAPFO='FORMATTED'
      ICAPAC='SEQUENTIAL'
      ICAPPR='READWRITE'
      ICAPCS='CLOSED'
!
      ICPNU2=49
      ICAPN2='-999'
      ICAPS2='UNKNOWN'
      ICAPF2='FORMATTED'
      ICAPA2='SEQUENTIAL'
      ICAPP2='READWRITE'
      ICAPC2='CLOSED'
!
! --------------------
!
      ISCRNU=41
!  NOVEMBER 1991.
      INAME='DPSCRF'
      IF(ICASFL.EQ.'LOWE')INAME='dpscrf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ISCRNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      ISCRST='UNKNOWN'
      ISCRFO='UNFORMATTED'
      ISCRAC='SEQUENTIAL'
      ISCRPR='READWRITE'
      ISCRCS='CLOSED'
!
      IDATNU=42
!  NOVEMBER 1991.
      INAME='DPDATF'
      IF(ICASFL.EQ.'LOWE')INAME='dpdatf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,IDATNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IDATST='NONE'
      IDATST='UNKNOWM'
      IDATFO='UNFORMATTED'
      IDATAC='SEQUENTIAL'
      IDATPR='READWRITE'
      IDATCS='CLOSED'
!
      IPL1NU=43
!CCCC IPL1ST='UNKNOWN'
!  NOVEMBER 1991.
      INAME='DPPL1F'
      IF(ICASFL.EQ.'LOWE')INAME='dppl1f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT3,NCEXT3,IPL1NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IPL1ST='NEW'
      IF(IHOST1.EQ.'HONE')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'CONV')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'CRAY')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IPL1ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IPL1ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')IPL1ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1991
      IF(IOPSY1.EQ.'UNIX')IPL1ST='UNKNOWN'
      IPL1FO='FORMATTED'
      IPL1AC='SEQUENTIAL'
      IPL1PR='READWRITE'
      IPL1CS='CLOSED'
!
      IPL2NU=44
!  NOVEMBER 1991.
      INAME='DPPL2F'
      IF(ICASFL.EQ.'LOWE')INAME='dppl2f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT3,NCEXT3,IPL2NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IPL2ST='UNKNOWN'
      IPL2ST='NEW'
      IF(IHOST1.EQ.'HONE')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'CONV')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'CRAY')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IPL2ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IPL2ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')IPL2ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1991
      IF(IOPSY1.EQ.'UNIX')IPL2ST='UNKNOWN'
      IPL2FO='FORMATTED'
      IPL2AC='SEQUENTIAL'
      IPL2PR='READWRITE'
      IPL2CS='CLOSED'
!
      IPRONU=45
!  NOVEMBER 1991.
      INAME='DPPROF'
      IF(ICASFL.EQ.'LOWE')INAME='dpprof'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,IPRONA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IPROST='UNKNOWN'
      IPROFO='FORMATTED'
      IPROAC='SEQUENTIAL'
      IPROPR='READWRITE'
      IPROCS='CLOSED'
!
      ICONNU=46
!CCCC ICONST='UNKNOWN'
!  NOVEMBER 1991.
      INAME='DPCONF'
      IF(ICASFL.EQ.'LOWE')INAME='dpconf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ICONNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      ICONST='NEW'
      IF(IHOST1.EQ.'HONE')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'205')ICONST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')ICONST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')ICONST='UNKNOWN'
!CCCC THE FOLLOWING LINE W AS ADDED NOVEMBER 1991
      IF(IOPSY1.EQ.'UNIX')ICONST='UNKNOWN'
      ICONFO='FORMATTED'
      ICONAC='SEQUENTIAL'
      ICONPR='READWRITE'
      ICONCS='CLOSED'
!
      ISACNU=47
!  NOVEMBER 1991.
      INAME='DPSACF'
      IF(ICASFL.EQ.'LOWE')INAME='dpsacf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,ISACNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC ISACST='UNKNOWN'
      ISACST='NEW'
      IF(IHOST1.EQ.'HONE')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'205')ISACST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')ISACST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')ISACST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1990.
      IF(IOPSY1.EQ.'UNIX')ISACST='UNKNOWN'
      ISACFO='FORMATTED'
      ISACAC='SEQUENTIAL'
      ISACPR='READWRITE'
      ISACCS='CLOSED'
!
!CCCC THE FOLLOWING SECTION WAS ADDED      MARCH 1992
!CCCC TO DEFINE THE GENERAL OUTPUT FILE     MARCH 1992
      IOUTNU=49
!  NOVEMBER 1991.
      INAME='DPOUTF'
      IF(ICASFL.EQ.'LOWE')INAME='dpoutf'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT1,NCEXT1,IOUTNA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IOUTST='UNKNOWN'
      IOUTST='NEW'
      IF(IHOST1.EQ.'HONE')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'205')IOUTST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IOUTST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED JULY 1990
      IF(IHOST1.EQ.'IBM-')IOUTST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED NOVEMBER 1990.
      IF(IOPSY1.EQ.'UNIX')IOUTST='UNKNOWN'
      IOUTFO='FORMATTED'
      IOUTAC='SEQUENTIAL'
      IOUTPR='READWRITE'
      IOUTCS='CLOSED'
!
! --------------------
!
!CCCC IEX1NU=51
!  NOVEMBER 1991.
!CCCC INAME='DPEX1F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpex1f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX1NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IEX1ST='OLD'
!CCCC IEX1FO='FORMATTED'
!CCCC IEX1AC='SEQUENTIAL'
!CCCC IEX1PR='READONLY'
!CCCC IEX1CS='CLOSED'
!
!CCCC IEX2NU=52
!  NOVEMBER 1991.
!CCCC INAME='DPEX2F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpex2f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX2NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IEX2ST='OLD'
!CCCC IEX2FO='FORMATTED'
!CCCC IEX2AC='SEQUENTIAL'
!CCCC IEX2PR='READONLY'
!CCCC IEX2CS='CLOSED'
!
!CCCC IEX3NU=53
!CCCC IEX3ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPEX3F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpex3f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX3NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IEX3FO='FORMATTED'
!CCCC IEX3AC='SEQUENTIAL'
!CCCC IEX3PR='READONLY'
!CCCC IEX3CS='CLOSED'
!
!CCCC IEX4NU=54
!CCCC IEX4ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPEX4F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpex4f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX4NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IEX4FO='FORMATTED'
!CCCC IEX4AC='SEQUENTIAL'
!CCCC IEX4PR='READONLY'
!CCCC IEX4CS='CLOSED'
!
!CCCC IEX5NU=55
!CCCC IEX5ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPEX5F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpex5f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IEX5NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IEX5FO='FORMATTED'
!CCCC IEX5AC='SEQUENTIAL'
!CCCC IEX5PR='READONLY'
!CCCC IEX5CS='CLOSED'
!
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
!
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
!
      IHE1NU=61
      IHE1ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE1F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe1f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE1NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE1FO='FORMATTED'
      IHE1AC='SEQUENTIAL'
      IHE1PR='READONLY'
      IHE1CS='CLOSED'
!
      IHE2NU=62
      IHE2ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE2F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe2f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE2NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE2FO='FORMATTED'
      IHE2AC='SEQUENTIAL'
      IHE2PR='READONLY'
      IHE2CS='CLOSED'
!
      IHE3NU=63
      IHE3ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE3F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe3f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE3NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE3FO='FORMATTED'
      IHE3AC='SEQUENTIAL'
      IHE3PR='READONLY'
      IHE3CS='CLOSED'
!
      IHE4NU=64
      IHE4ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE4F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe4f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE4NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE4FO='FORMATTED'
      IHE4AC='SEQUENTIAL'
      IHE4PR='READONLY'
      IHE4CS='CLOSED'
!
      IHE5NU=65
      IHE5ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE5F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe5f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE5NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE5FO='FORMATTED'
      IHE5AC='SEQUENTIAL'
      IHE5PR='READONLY'
      IHE5CS='CLOSED'
!
      IHE6NU=66
      IHE6ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE6F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe6f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE6NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE6FO='FORMATTED'
      IHE6AC='SEQUENTIAL'
      IHE6PR='READONLY'
      IHE6CS='CLOSED'
!
      IHE7NU=67
      IHE7ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE7F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe7f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE7NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE7FO='FORMATTED'
      IHE7AC='SEQUENTIAL'
      IHE7PR='READONLY'
      IHE7CS='CLOSED'
!
      IHE8NU=68
      IHE8ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE8F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe8f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE8NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE8FO='FORMATTED'
      IHE8AC='SEQUENTIAL'
      IHE8PR='READONLY'
      IHE8CS='CLOSED'
!
      IHE9NU=69
      IHE9ST='OLD'
!  NOVEMBER 1991.
      INAME='DPHE9F'
      IF(ICASFL.EQ.'LOWE')INAME='dphe9f'
      NC=6
      CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IHE9NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
      IHE9FO='FORMATTED'
      IHE9AC='SEQUENTIAL'
      IHE9PR='READONLY'
      IHE9CS='CLOSED'
!
!CCCC THE FOLLOWING 9 MENU SECTIONS WERE ADDED JUNE 1990
!
!CCCC IME1NU=71
!CCCC IME1ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME1F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme1f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME1NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME1FO='FORMATTED'
!CCCC IME1AC='SEQUENTIAL'
!CCCC IME1PR='READONLY'
!CCCC IME1CS='CLOSED'
!
!CCCC IME2NU=72
!CCCC IME2ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME2F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme2f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME2NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME2FO='FORMATTED'
!CCCC IME2AC='SEQUENTIAL'
!CCCC IME2PR='READONLY'
!CCCC IME2CS='CLOSED'
!
!CCCC IME3NU=73
!CCCC IME3ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME3F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme3f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME3NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME3FO='FORMATTED'
!CCCC IME3AC='SEQUENTIAL'
!CCCC IME3PR='READONLY'
!CCCC IME3CS='CLOSED'
!
!CCCC IME4NU=74
!CCCC IME4ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME4F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme4f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME4NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME4FO='FORMATTED'
!CCCC IME4AC='SEQUENTIAL'
!CCCC IME4PR='READONLY'
!CCCC IME4CS='CLOSED'
!
!CCCC IME5NU=75
!CCCC IME5ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME5F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme5f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME5NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME5FO='FORMATTED'
!CCCC IME5AC='SEQUENTIAL'
!CCCC IME5PR='READONLY'
!CCCC IME5CS='CLOSED'
!
!CCCC IME6NU=76
!CCCC IME6ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME6F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme6f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME6NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME6FO='FORMATTED'
!CCCC IME6AC='SEQUENTIAL'
!CCCC IME6PR='READONLY'
!CCCC IME6CS='CLOSED'
!
!CCCC IME7NU=77
!CCCC IME7ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME7F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme7f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME7NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME7FO='FORMATTED'
!CCCC IME7AC='SEQUENTIAL'
!CCCC IME7PR='READONLY'
!CCCC IME7CS='CLOSED'
!
!CCCC IME8NU=78
!CCCC IME8ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME8F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme8f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME8NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME8FO='FORMATTED'
!CCCC IME8AC='SEQUENTIAL'
!CCCC IME8PR='READONLY'
!CCCC IME8CS='CLOSED'
!
!CCCC IME9NU=79
!CCCC IME9ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPME9F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpme9f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IME9NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IME9FO='FORMATTED'
!CCCC IME9AC='SEQUENTIAL'
!CCCC IME9PR='READONLY'
!CCCC IME9CS='CLOSED'
!
!CCCC THE FOLLOWING 11 SECTIONS (10 TO 20) WERE ADDED AUGUST 1990
!CCCC IM10NU=80
!CCCC IM10ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM10F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm10f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM10NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM10FO='FORMATTED'
!CCCC IM10AC='SEQUENTIAL'
!CCCC IM10PR='READONLY'
!CCCC IM10CS='CLOSED'
!
!CCCC IM11NU=81
!CCCC IM11ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM11F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm11f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM11NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM11FO='FORMATTED'
!CCCC IM11AC='SEQUENTIAL'
!CCCC IM11PR='READONLY'
!CCCC IM11CS='CLOSED'
!
!CCCC IM12NU=82
!CCCC IM12ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM12F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm12f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM12NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM12FO='FORMATTED'
!CCCC IM12AC='SEQUENTIAL'
!CCCC IM12PR='READONLY'
!CCCC IM12CS='CLOSED'
!
!CCCC IM13NU=83
!CCCC IM13ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM13F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm13f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM13NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM13FO='FORMATTED'
!CCCC IM13AC='SEQUENTIAL'
!CCCC IM13PR='READONLY'
!CCCC IM13CS='CLOSED'
!
!CCCC IM14NU=84
!CCCC IM14ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM14F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm14f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM14NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM14FO='FORMATTED'
!CCCC IM14AC='SEQUENTIAL'
!CCCC IM14PR='READONLY'
!CCCC IM14CS='CLOSED'
!
!CCCC IM15NU=85
!CCCC IM15ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM15F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm15f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM15NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM15FO='FORMATTED'
!CCCC IM15AC='SEQUENTIAL'
!CCCC IM15PR='READONLY'
!CCCC IM15CS='CLOSED'
!
!CCCC IM16NU=86
!CCCC IM16ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM16F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm16f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM16NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM16FO='FORMATTED'
!CCCC IM16AC='SEQUENTIAL'
!CCCC IM16PR='READONLY'
!CCCC IM16CS='CLOSED'
!
!CCCC IM17NU=87
!CCCC IM17ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM17F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm17f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM17NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM17FO='FORMATTED'
!CCCC IM17AC='SEQUENTIAL'
!CCCC IM17PR='READONLY'
!CCCC IM17CS='CLOSED'
!
!CCCC IM18NU=88
!CCCC IM18ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM18F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm18f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM18NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM18FO='FORMATTED'
!CCCC IM18AC='SEQUENTIAL'
!CCCC IM18PR='READONLY'
!CCCC IM18CS='CLOSED'
!
!CCCC IM19NU=89
!CCCC IM19ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM19F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm19f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM19NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM19FO='FORMATTED'
!CCCC IM19AC='SEQUENTIAL'
!CCCC IM19PR='READONLY'
!CCCC IM19CS='CLOSED'
!
!CCCC IM20NU=90
!CCCC IM20ST='OLD'
!  NOVEMBER 1991.
!CCCC INAME='DPM20F'
!CCCC IF(ICASFL.EQ.'LOWE')INAME='dpm20f'
!CCCC NC=6
!CCCC CALL INITF2(INAME,NC,IPATH1,NCP1,IEXT1,NCEXT1,IM20NA,IBUGIN)
!  END OF NOVEMBER 1991 CHANGE
!CCCC IM20FO='FORMATTED'
!CCCC IM20AC='SEQUENTIAL'
!CCCC IM20PR='READONLY'
!CCCC IM20CS='CLOSED'
!
! --------------------
!CCCC THE FOLLOWING 3 SECTIONS WERE ADDED OCTOBER 1991
!CCCC MODIFIED MARCH 1992
!CCCC FOR STORAGE OF SELECTED OUTPUT FROM FIT, ANOVA, YATES, ETC.
!
      IST1NU=91
!CCCC IST1ST='UNKNOWN'
      IST1ST='NEW'
      IF(IHOST1.EQ.'HONE')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST1ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST1ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      IF(IOPSY1.EQ.'UNIX')IST1ST='UNKNOWN'
!  MARCH 1992.
      INAME='DPST1F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst1f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST1NA,IBUGIN)
!  END OF MARCH 1992 CHANGE
      IST1FO='FORMATTED'
      IST1AC='SEQUENTIAL'
!CCCC AUGUST 1992.  FILE PERMISSION SHOULD BE READ/WRITE
!CCCC IST1PR='READONLY'
      IST1PR='READWRITE'
      IST1CS='CLOSED'
!
      IST2NU=92
!CCCC IST2ST='UNKNOWN'
      IST2ST='NEW'
      IF(IHOST1.EQ.'HONE')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST2ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST2ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IST2ST='UNKNOWN'
!  MARCH 1992.
      INAME='DPST2F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst2f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST2NA,IBUGIN)
!  END OF MARCH 1992 CHANGE
      IST2FO='FORMATTED'
      IST2AC='SEQUENTIAL'
!CCCC AUGUST 1992.  FILE PERMISSION SHOULD BE READ/WRITE
!CCCC IST2PR='READONLY'
      IST2PR='READWRITE'
      IST2CS='CLOSED'
!
      IST3NU=93
!CCCC IST3ST='UNKNOWN'
      IST3ST='NEW'
      IF(IHOST1.EQ.'HONE')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'PERK')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'SUN')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'NVE')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'205')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'CDC')IST3ST='UNKNOWN'
      IF(IHOST1.EQ.'IBM-')IST3ST='UNKNOWN'
!CCCC THE FOLLOWING LINE WAS ADDED MARCH 1992
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')IST3ST='UNKNOWN'
!  MARCH 1992.
      INAME='DPST3F'
      IF(ICASFL.EQ.'LOWE')INAME='dpst3f'
      NC=6
      CALL INITF2(INAME,NC,IPATH2,NCP2,IEXT2,NCEXT2,IST3NA,IBUGIN)
!  END OF MARCH 1992 CHANGE
      IST3FO='FORMATTED'
      IST3AC='SEQUENTIAL'
!CCCC AUGUST 1992.  FILE PERMISSION SHOULD BE READ/WRITE
!CCCC IST3PR='READONLY'
      IST3PR='READWRITE'
      IST3CS='CLOSED'
!
!
      IST4NU=94
!CCCC IST4ST='UNKNOWN'
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
!
      IST5NU=95
!CCCC IST5ST='UNKNOWN'
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
!
      IZCHNU=97
!CCCC IZCHST='UNKNOWN'
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
!
!
! --------------------
!
!     DEFINE THE CHARACTER WHICH
!     (IF FOUND IN A WORD)
!     SPECIFIES THAT WORD TO BE A FILE NAME
!     (AS OPPOSED TO A DATAPLOT
!     VARIABLE, PARAMETER, COMMAND, ETC.).
!     THE DEFAULT CHARACTER IS . (= PERIOD)
!
      IFCHAR='.'
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
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
!CCCC   AUGUST 1992.  ADD FOLLOWING LINES
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
!
      RETURN
      END SUBROUTINE INITFO
      SUBROUTINE INITF2(INAME,NC1,IPATH,NC2,IEXT,NC3,INAME2,IBUGIN)
!
!     PURPOSE--THIS IS SUBROUTING INITF2.  IT IS A UTILITY ROUTINE
!              FOR INITFO.  IT ADDS A FILE PATH AND EXTENSION TO
!              A FILE NAME.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERBURG, MD 20899
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/7
!     ORIGINAL VERSION--NOVEMBER  1991.
!     UPDATED         --APRIL     1992.   INPUT DEBUG STATMENTS (JJF)
!     UPDATED         --MAY       1992.   INITIALIZE INAME2
!     UPDATED         --JULY      1995.   IF PC--DO NOT ADD PATH
!     UPDATED         --APRIL     1996.   UNDO JULY 1995 CHANGE (FIX IN
!                                         DPOPFI FOR NON-C: DRIVE)
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGIN
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
!
!  NOVEMBER 1991.  FOLLOWING BLOCK ADDED
!
      CHARACTER (LEN=*) :: IPATH
      CHARACTER (LEN=*) :: INAME
      CHARACTER (LEN=*) :: INAME2
      CHARACTER (LEN=*) :: IEXT
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='INIT'
      ISUBN2='F2  '
!
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
!
!               *****************
!               **  STEP 1---  **
!               **  ADD THE FILE*
!               **  PATH        *
!               *****************
!
!CCCC THE FOLLOWING LINE WAS ADDED   MAY 1992  (JJF)
      INAME2=' '
      NCSTR=0
      IF(NC2.GT.0)THEN
        INAME2(1:NC2)=IPATH(1:NC2)
        NCSTR=NC2
      END IF
!
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,61)NCSTR,INAME2(1:NC2),'$'
   61   FORMAT('AFTER PATH: NCSTR,INAME2(1:NC2) = ',I5,2X,A,A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************
!               **  STEP 2---  **
!               **  ADD THE FILE*
!               **  NAME        *
!               *****************
!
      IF(NC1.GT.0)THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NC1-1
        INAME2(NCSTR:NCSTR2)=INAME(1:NC1)
        NCSTR=NCSTR2
      END IF
!
      IF(IBUGIN.EQ.'ON')THEN
        WRITE(ICOUT,71)NCSTR,INAME2(1:NCSTR),'$'
   71   FORMAT('AFTER FILE NAME: NCSTR,INAME2(1:NCSTR) = ',I5,2X,A,A1)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!
!               *****************
!               **  STEP 3---  **
!               **  ADD THE FILE*
!               **  EXTENSION   *
!               *****************
!
      IF(NC3.GT.0)THEN
        NCSTR=NCSTR+1
        NCSTR2=NCSTR+NC3-1
!
        IF(IBUGIN.EQ.'ON')THEN
          WRITE(ICOUT,81)NCSTR,NCSTR2,NC3
   81     FORMAT('NCSTR,NCSTR2,NC3 = ',3I6)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,83)'$',IEXT(1:NC3),'$'
   83     FORMAT('IEXT(1:NC3) = ',A1,A,A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        INAME2(NCSTR:NCSTR2)=IEXT(1:NC3)
        NCSTR=NCSTR2
      END IF
                                                                                                                                  
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT.      **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE INITF2
      SUBROUTINE DPOPFI(IOUNIT,IFILE,ISTAT,IFORM,IACCES,IPROT,ICURST,   &
                        IREWIN,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--OPEN A FILE (BUT THERE MAY BE SOME SMALL DIFFERENCES
!              IN HOW THAT IS DONE FOR DIFFERENT COMPUTERS).
!     NOTE--A REMARK TO THE CDC, PERKIN-ELMER, HONEYWELL, ETC. IMPLEMENTORS--
!           YOUR SECTIONS BELOW MUST HAVE A FEW MORE LINES MANUALLY
!           INSERTED IF YOU WISH YOUR USERS TO HAVE THE ABILITY
!           OF ACCESSING DATAPLOT'S REFERENCE/DATA/MAP/FRACTAL/MACRO
!           FILES AUTOMATICALLY WITHOUT EXPLICITLY PREFIXING
!           THE FILE NAME WITH THE HOME DIRECTORY WHERE DATAPLOT RESIDES.
!           SEE FOR EXAMPLE THE GENERAL SECTION BELOW AND THE VAX SECTION
!           BELOW WHERE SUCH LOGIC HAS BEEN BUILT IN.
!           IF YOU OMIT THIS ADDITION, THEN NOTHING IS LOST PER SE
!           BUT THE USERS WILL HAVE TO SPELL OUT FULLY DATAPLOT'S
!           HOME DIRECTORY WHEN REFERENCING THESE ACCESSORY
!           REFERENCE/DATA/MAP/FRACTAL/MACRO/ETC. FILES.
!           E.G., LIST TEXAS.DAT              VERSUS
!                 LIST DATAPLO$:TEXAS.DAT
!
!     DANGER--THE INPUT ARGUMENT IFILE MAY UNDER CERTAIN
!             CIRCUMSTANCES BE CHANGED WITHIN THIS SUBROUTINE.
!     WRITTEN BY--JAMES J. FILLIBEN
!     LANGUAGE--ANSI FORTRAN (1977)
!     ORIGINAL VERSION--NOVEMBER  1985.
!     UPDATED         --SEPTEMBER 1986.
!     UPDATED         --OCTOBER   1987.  (FORM LOWER AND UPPER CASE NAMES)
!     UPDATED         --NOVEMBER  1987.  (CLOSE BEFORE OPEN FOR HONEYWELL)
!     UPDATED         --DECEMBER  1988.  (AUTO PREFIX OF DP'S HOME DIREC.)
!     UPDATED         --FEBRUARY  1989.  CYBER/CDC CASE (ALAN)
!     UPDATED         --FEBRUARY  1988.  CYBER/CDC DATAPLOT REF. FILES (ALAN)
!     UPDATED         --JULY      1989.  FIXED POSITION VALIUES FOR IFILE2(.:.)
!     UPDATED         --MAY       1990.  FOR UNIX (I.E., GENERAL CASE), TRY TO
!                                        OPEN FILES WITH TRAILING PERIOD
!                                        STRIPPED OFF.
!     UPDATED         --NOVEMBER  1991.  CHANGES MADE FOR EASIER IMPLEMENTING
!     UPDATED         --DECEMBER  1993.  ACTIVATE 3 CUNIX LINES
!     UPDATED         --AUGUST    1994.  COMMENT OUT WRITE STATEMENTS
!     UPDATED         --APRIL     1996.  EXTEND 6/95 CHANGE TO UNIX,
!     UPDATED         --APRIL     1996.  ALLOW PATH NAME FOR UNIX TO
!                                        BE SET FROM ENVIRONMENT VARIABLE,
!                                        SOFT-CODE BACKSLASH FOR PC
!                                        TO AVOID UNIX COMPILATION
!                                        ERRORS
!     UPDATED         --JUNE      1995.  AUTO-READ FROM DP SUB-DIRECTORIES
!     UPDATED         --AUGUST    1996.  FIX TO SUB-DIRECTORIES
!     UPDATED         --OCTOBER   2014.  ADD A USER SPECIFIED "SEARCH
!                                        DIRECTORY" THAT WILL BE SEARCHED
!                                        AFTER THE CURRENT DIRECTORY BUT
!                                        BEFORE THE DATAPLOT AUXILLARY
!                                        DIRECTORIES.
!     UPDATED         --OCTOBER   2014.  SIMPLIFY CODE
!     UPDATED         --OCTOBER   2018.  ADD A "10STEP" DIRECTORY
!     UPDATED         --MARCH     2019.  REMOVE "TEK" DIRECTORY FROM
!                                        SEARCH PATH
!     UPDATED         --MARCH     2020.  ADD TWO ADDITIONAL USER SEARCH
!                                        DIRECTORIES
!     UPDATED         --APRIL     2020.  UP MAXIMUM NUMBER OF CHARACTERS
!                                        FROM 80 TO 256
!     UPDATED         --APRIL     2020.  ADD USER SEARCH DIRECTORIES
!                                        4, 5 AND 6
!
!---------------------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
!
!CCCC CHARACTER*80 IFILE
      CHARACTER (LEN=MAXFNC) :: IFILE
      CHARACTER*12 ISTAT
      CHARACTER*12 IFORM
      CHARACTER*12 IACCES
      CHARACTER*12 IPROT
      CHARACTER*12 ICURST
      CHARACTER*4 IREWIN
      CHARACTER*4 ISUBN0
      CHARACTER*4 IERRFI
!
!CCCC CHARACTER*80 IFILEL
!CCCC CHARACTER*80 IFILEU
!CCCC CHARACTER*80 FTEMP
!CCCC CHARACTER*255 IFILE2
!CCCC CHARACTER*255 IFILE3
!CCCC CHARACTER*255 IFILE4
      PARAMETER (MAXNCH=2*MAXFNC)
      CHARACTER (LEN=MAXNCH) :: IFILEL
      CHARACTER (LEN=MAXNCH) :: IFILEU
      CHARACTER (LEN=MAXNCH) :: FTEMP
      CHARACTER (LEN=MAXNCH) :: IFILE2
      CHARACTER (LEN=MAXNCH) :: IFILE3
      CHARACTER (LEN=MAXNCH) :: IFILE4
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!CCCC APRIL 1996.  SOFT-CODE BACKSLASH CHARACTER
      CHARACTER*1 IBSLC
      CHARACTER*1 IPATSE
!
!-----COMMON------------------------------------------------
!
      CHARACTER (LEN=MAXFNC) :: IFILSV
      COMMON/FILSAV/IFILSV
!
      CHARACTER*80 PROFIL
      CHARACTER*80 P86FIL
      CHARACTER*80 APPDAT
      CHARACTER*80 COMNAM
      CHARACTER*80 UPROFI
      CHARACTER*80 DEFPRI
      CHARACTER*20 USRNAM
      CHARACTER*20 ISHELL
      CHARACTER*4  WINBIT
      COMMON/SYSVAR/PROFIL,P86FIL,APPDAT,COMNAM,UPROFI,USRNAM,DEFPRI,   &
                    WINBIT,ISHELL
      COMMON/SYSVA2/NCPROF,NCP86F,NCAPPD,NCCOMP,NCUPRO,NCUSER,NCPRIN,   &
                    NCSHEL
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPOP'
      ISUBN2='FI  '
      FTEMP=' '
      IFILE2=' '
      IFILEL=' '
      IFILEU=' '
      IFILSV=' '
!
!CCCC APRIL 1996.  SOFT-CODE BACKSLASH CHARACTER
      IF(IOPSY1.EQ.'UNIX')THEN
        IPATSE='/'
      ELSE
        CALL DPCONA(92,IBSLC)
        IPATSE=IBSLC
      ENDIF
!
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
!
!               *******************
!               **  STEP 1--     **
!               **  OPEN A FILE  **
!               *******************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IHOST1.EQ.'VAX')THEN
#ifdef VAX_VMS
!
!-------TREAT THE VAX 11/7XX VMS CASE-----------------------------------
!       (NOTE--IF HAVE    READONLY   ARGUMENT,
!       THEN THE VAX WILL ONLY ALLOW    STATUS='OLD'   ;
!       STATUS = ANYTHING ELSE ('UNKNOWN' OR 'NEW')
!       WILL RESULT IN THE FILE NOT BEING OPENED
!       AND AN ERROR CONDITION RESULTING.)
!
        IF(IPROT.EQ.'READONLY')THEN
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,   &
               IOSTAT=IOSTA2,ACCESS=IACCES,   &
               CARRIAGE CONTROL='LIST',READONLY)
          IF(IOSTA2.EQ.0)GO TO 2190
          IOSTA2=0
          IFILE2(10:MAXFNC)=IFILE(1:MAXFNC-9)
          IFILE2(1:9)='DATAPLO$:'
          OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS='OLD',FORM=IFORM,
          1IOSTAT=IOSTA2,ACCESS=IACCES,CARRIAGE CONTROL='LIST',READONLY)
          IF(IOSTA2.EQ.0)THEN
            IFILE=IFILE2
            GO TO 2190
          ENDIF
          GO TO 8000
        ELSE
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,   &
               ERR=8000,ACCESS=IACCES,CARRIAGE CONTROL='LIST')
          GO TO 2190
        ENDIF
!
 2190   CONTINUE
        ICURST='OPEN'
        IERRFI='NO'
        IERROR='NO'
        IF(IREWIN.EQ.'ON')REWIND IOUNIT
        GO TO 9000
#endif
!
      ELSEIF(IHOST1.EQ.'CDC' .OR. IHOST1.EQ.'CYBE' .OR.   &
             IHOST1.EQ.'205' .OR. IHOST1.EQ.'NVE' )THEN
!
!-------TREAT THE CDC CASE------------------------------------------
!       REFERENCE--ALAN HECKERT, 2899
!
        IOSTA2=0
!
!       FOR CDC, NOS AND NOS/VE, STRIP OFF THE TRAILING '.'
!       SINCE THIS CAUSES THE OPEN TO FAIL
!
        FTEMP=IFILE
        DO 2250 I=MAXFNC,1,-1
          IF(FTEMP(I:I).EQ.' ')GO TO  2250
          IF(FTEMP(I:I).EQ.IFCHAR) FTEMP(I:I)=' '
          GO TO  2260
 2250   CONTINUE
 2260   CONTINUE
        IF(IHOST1.EQ.'NVE')THEN
          IF(IOUNIT.EQ.IRD)OPEN(UNIT=IOUNIT,FILE='$INPUT',STATUS='OLD')
          IF(IOUNIT.EQ.IPR)OPEN(UNIT=IOUNIT,FILE='$OUTPUT',STATUS='OLD')
        ELSE
          IF(IOUNIT.EQ.IRD)OPEN(UNIT=IOUNIT,FILE='INPUT',STATUS='OLD')
          IF(IOUNIT.EQ.IPR)OPEN(UNIT=IOUNIT,FILE='OUTPUT',STATUS='OLD')
        ENDIF
        IF(IOUNIT.NE.IRD.AND.IOUNIT.NE.IPR)   &
          OPEN(UNIT=IOUNIT,FILE=FTEMP,STATUS=ISTAT,FORM=IFORM,   &
               IOSTAT=IOSTA2)
!
!       JANUARY,1989: CHECK FOR REFERENCE FILES.  HANDLE FOR NBS NOS/VE
!       CASE.  NOTE THAT DATAPLOT IS INSTALLED AS A "SYSTEM APPLICATION"
!       AT NBS.  OTHER NOS/VE SITES MAY OR MAY NOT HAVE IT INSTALLED
!       THIS WAY.
!
        IF(IOSTA2.EQ.0)GO TO 2290
        IOSTA2=0
        IFILE2(42:80)=FTEMP(1:39)
        IFILE2(1:41)='.CS2.APPLICATIONS.DATAPLOT.VER_2.SAMPLES.'
        OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS=ISTAT,FORM=IFORM,   &
             IOSTAT=IOSTA2)
        IF(IOSTA2.NE.0)GO TO 8000
!
 2290   CONTINUE
        ICURST='OPEN'
        IERRFI='NO'
        IERROR='NO'
        IF(IREWIN.EQ.'ON')REWIND IOUNIT
        GO TO 9000
!
!CCCC ELSEIF(IHOST1.EQ.'PERK')THEN
!
!-------TREAT THE PERKIN-ELMER CASE-----------------------------------
!       REFERENCE--LARRY KAETZEL, 2650
!
!CCCC   IOSTA2=0
!CCCC   OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
!CCCC1       IOSTAT=IOSTA2,RECL=132,SIZE=8)
!CCCC   IF(IOSTA2.NE.0)GO TO 8000
!CCCC   ICURST='OPEN'
!CCCC   IERRFI='NO'
!CCCC   IERROR='NO'
!CCCC   IF(IREWIN.EQ.'ON')REWIND IOUNIT
!CCCC   GO TO 9000
!
!CCCC ELSEIF(IHOST1.EQ.'HONE')THEN
!
!-------TREAT THE HONEYWELL-MULTICS CASE-----------------------------------
!CCCC   IOSTA2=0
!CCCC   IF(IPROT.EQ.'READONLY')THEN
!CCCC1    OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS='OLD',FORM=IFORM,
!CCCC1         IOSTAT=IOSTA2,ACCESS=IACCES,MODE='INPUT')
!CCCC     IF(IOSTA2.NE.0)GO TO 8000
!CCCC   ELSE
!CCCC     OPEN(UNIT=IOUNIT,FILE=IFILE,STATUS=ISTAT,FORM=IFORM,
!CCCC1         ERR=8000,ACCESS=IACCES)
!CCCC   ENDIF
!CCCC   ICURST='OPEN'
!CCCC   IERRFI='NO'
!CCCC   IERROR='NO'
!CCCC   IF(IREWIN.EQ.'ON')REWIND IOUNIT
!CCCC   GO TO 9000
!
      ENDIF
!
!-----TREAT THE GENERAL CASE (E.G., UNIX, MICROSOFT WINDOWS)-----------
!
!     1990/05: IF CAN NOT OPEN FILE, STRIP OFF TRAILING PERIOD (IF FILE
!              NAME ENDS WITH PERIOD) AND TRY TO OPEN.  THIS FIXES UNIX
!              BUG (DATAPLOT COULD NOT OPEN A FILE THAT DID NOT CONTAIN
!              A SUFFIX, I.E. READ TEST.  FAILED FOR A FILE NAMED "TEST".
!              NOTE THAT ON UNIX, THE FILE "TEST" AND "TEST." ARE NOT THE
!              SAME SO DATAPLOT WILL NOW TRY TO OPEN IT BOTH WAYS).
!
      FTEMP(1:MAXFNC)=IFILE(1:MAXFNC)
!
!CCCC JULY 2002: STRIP OFF LEADING/TRAILING QUOTES
!
      IF(IFILE(1:1).EQ.'"')THEN
        DO 1102 I=2,MAXFNC
          IF(IFILE(I:I).EQ.'"')THEN
            FTEMP=' '
            FTEMP(1:I-2)=IFILE(2:I-1)
            GO TO 1103
          ENDIF
 1102   CONTINUE
 1103   CONTINUE
      ENDIF
!
!     DETERMINE LAST NON-BLANK CHARACTER IN FILE NAME
!
      DO 1107 I=MAXFNC,1,-1
        IF(FTEMP(I:I).NE.' ')THEN
          NCFILE=I
          GO TO 1109
        ENDIF
 1107 CONTINUE
      IERROR='YES'
      GO TO 9000
 1109 CONTINUE
!
      IFLAG=0
!
 1110 CONTINUE
!
!     CONVERT FILE NAME TO LOWER CASE
!
      NMAX=MAXFNC
      CALL DPLO80(FTEMP,IFILEL,NMAX,IBUGS2,IERROR)
      CALL DPUP80(FTEMP,IFILEU,NMAX,IBUGS2,IERROR)
!
!     NOTE: K =  1 IS THE THE CURRENT DIRECTORY
!           K =  2 IS THE THE HOME DIRECTORY
!                (IT IS USER SETTABLE WHETHER THIS DIRECTORY
!                WILL BE SEARCHED)
!           K =  3, 4 5, 6, 7, AND 8 ARE THE USER SPECIFIED SEARCH
!                DIRECTORIES
!           K >= 9 ARE THE DATAPLOT AUXILIARY DIRECTORIES
!
      DO 1141 K=1,19
!
        NC1=0
        IFILE2=' '
        IFILE3=' '
        IFILE4=' '
!
        IF(K.GE.9)THEN
!
!         ADD DATAPLOT AUXILLARY PATH
!
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
!
        IF(K.EQ.2)THEN
          IF(IHOMPA.EQ.'OFF' .OR. NCUPRO.LT.1)GO TO 1141
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
            GO TO 1141
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
            GO TO 1141
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
            GO TO 1141
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
            GO TO 1141
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
            GO TO 1141
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
            GO TO 1141
          ENDIF
!
!       DATATPLOT HELP DIRECTORY
!
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
!
        IFILE2(NC1+1:NC1+NCFILE)=FTEMP(1:NCFILE)
        IFILE3(NC1+1:NC1+NCFILE)=IFILEL(1:NCFILE)
        IFILE4(NC1+1:NC1+NCFILE)=IFILEU(1:NCFILE)
        NC1=NC1+NCFILE
!
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
!
!       OPEN FILE NAME AS IS
!
        IOSTA2=0
        IF(IFORM.EQ.'NULL')THEN
          OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS=ISTAT,IOSTAT=IOSTA2)
        ELSE
          OPEN(UNIT=IOUNIT,FILE=IFILE2,STATUS=ISTAT,FORM=IFORM,   &
               IOSTAT=IOSTA2)
        ENDIF
!
        IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'OPFI')THEN
          WRITE(ICOUT,1211)IOSTA2
 1211     FORMAT('AFTER ASIS FILE OPEN: IOSTA2 = ',I6)
          CALL DPWRST('XXX','BUG ')
        ENDIF
!
        IF(IOSTA2.NE.0)THEN
          CLOSE(UNIT=IOUNIT,ERR=1141)
        ELSE
          IFILSV=IFILE2(1:MAXFNC)
          GO TO 1190
        ENDIF
!
!       NOW OPEN LOWER CASE FILE NAME - CURRENTLY ONLY FOR UNIX
!
        IF (IOPSY1.EQ.'UNIX') THEN
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE3,STATUS=ISTAT,FORM=IFORM,   &
               IOSTAT=IOSTA2)
          IF(IOSTA2.NE.0)THEN
            CLOSE(UNIT=IOUNIT,ERR=1141)
          ELSE
            IFILSV=IFILE3(1:MAXFNC)
            GO TO 1190
          ENDIF
!
!       NOW OPEN UPPER CASE FILE NAME - CURRENTLY ONLY FOR UNIX
!
          IOSTA2=0
          OPEN(UNIT=IOUNIT,FILE=IFILE4,STATUS=ISTAT,FORM=IFORM,   &
               IOSTAT=IOSTA2)
          IF(IOSTA2.NE.0)THEN
            CLOSE(UNIT=IOUNIT,ERR=1141)
          ELSE
            IFILSV=IFILE4(1:MAXFNC)
            GO TO 1190
          ENDIF
        ENDIF
!
 1141 CONTINUE
!
!  MAY, 1990.  CHECK IF FILE ENDS WITH PERIOD (OR THE FILE CHARACTER).
!              IF SO, STRIP IT OFF AND REPEAT THE ABOVE SEQUENCE OF
!              OPEN COMMANDS.
!
      IFLAG=IFLAG+1
      IF(IFLAG.GT.1)GO TO 8000
      FTEMP=IFILE
      DO 1175 I=MAXFNC,1,-1
        IF(FTEMP(I:I).EQ.' ')GO TO 1175
        IF(FTEMP(I:I).EQ.IFCHAR)THEN
          FTEMP(I:I)=' '
          GO TO 1110
        ELSE
          GO TO 8000
        ENDIF
 1175 CONTINUE
      GO TO 8000
!
 1190 CONTINUE
      ICURST='OPEN'
      IERRFI='NO'
      IERROR='NO'
      IF(IREWIN.EQ.'ON')REWIND IOUNIT
      GO TO 9000
!
!               ************************************
!               **  STEP 80--                     **
!               **  GENERATE AN ERROR MESSAGE     **
!               **  IF THE FILE CANNOT BE OPENED  **
!               ************************************
!
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
!CCCC WRITE(ICOUT,8022)IFILE(1:256)
!8022 FORMAT('FILE NAME   = ',A256)
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
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPOPFI
      SUBROUTINE CKCLAR(ITEMNU,ITEMNA,ITEMST,ITEMFO,   &
                        ITEMAC,ITEMPR,ITEMCS,ITEMEF,ITEMRW,   &
                        NUMCLA,CLARG1,CLARG2,IECHFL,   &
                        ISUBN0,IBUGS2,ISUBRO,IERRFI)
!
!     PURPOSE--CHECK THE COMMAND LINE (IN DOS) TO INVOKE DATAPLOT,
!              DETERMINE IF THE COMMAND LINE HAS ATTACHED ARGUMENTS,
!              AND RECORD SUCH ARGUMENTS.
!     ORIGINAL VERSION--FEBRUARY 1992
!     UPDATED         --APRIL    1992 ADD OPERATING SYSTEM SPECIFIC
!                                     SUPPORT (ALAN)
!     UPDATED         --MAY      2001 COMMAND LINE ARGUMENT FOR
!                                     MICROSOFT FORTRAN.  NOTE
!                                     THIS ACTUALLY DONE IN
!                                     "MSFORT.F", THIS ROUTINE JUST
!                                     EXTRACTS FROM A COMMON BLOCK.
!     UPDATED         --AUGUST   2016 CALL LIST TO DPINFI
!     UPDATED         --JUNE     2021 CHECK FOR "-GUI" AND "-NOGUI"
!                                     OPTIONS
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      INCLUDE 'DPCOPA.INC'
!
!CCCC CHARACTER*80 ITEMNA
      CHARACTER (LEN=MAXFNC) :: ITEMNA
      CHARACTER*12 ITEMST
      CHARACTER*12 ITEMFO
      CHARACTER*12 ITEMAC
      CHARACTER*12 ITEMPR
      CHARACTER*12 ITEMCS
      CHARACTER*4 ITEMEF
      CHARACTER*4 ITEMRW
!
!CCCC CHARACTER*80 CLARG1
      CHARACTER (LEN=*) :: CLARG1
      CHARACTER*4 CLARG2
!  ADD FOLLOWING LINE APRIL 1992.  (FOR NOS/VE CASE)
!CCCC CHARACTER*80 ITEMP
      CHARACTER (LEN=MAXFNC) :: ITEMP
!
      CHARACTER*4 ISUBN0
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERRFI
!
      CHARACTER*4 IERROR
      CHARACTER*4 IEXIST
      CHARACTER*4 IOPEN
      CHARACTER*8 IACC
!
      CHARACTER*4 IQWNFL
      COMMON/QUICKW5/IQWNFL
      CHARACTER*80 BUF
!
!-----COMMON VARIABLES (GENERAL)---------------------------------------
!  APRIL 1992.  ADD FOLLOWING INCLUDE FILE
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
!  APRIL 1992.  ADD FOLLOWING FOR UNIX
#ifdef LINUX
      INTEGER iargc
#endif
!  APRIL 1992.  ADD FOLLOWING FOR CRAY UNICOS
#ifdef LINUX_CRAY
      INTEGER GETOARG
#endif
!  APRIL 1992.  ADD FOLLOWING FOR VAX
#ifdef VAX_VMS
      INTEGER*2 NCTEMP
#endif
!
      CHARACTER*4 IECHO2
!CCCC CHARACTER*80 FTEMP(50)
      CHARACTER (LEN=MAXFNC) :: FTEMP(50)
      COMMON/QUICKW2/IECHO2,FTEMP
      INTEGER NCFTMP(50)
      COMMON/QUICKW3/NCFTMP,NPCARG
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IECHFL=0
      ICNT2=0
      CLARG1=' '
      CLARG2=' '
!
      NMACLA=0
      DO 110 II=1,50
        IMACAR(II)=' '
        IMACLA(II)=' '
        IMACLL(II)=0
        IMACNC(II)=0
  110 CONTINUE
!
!               ********************************
!               **  STEP 1--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
!  INSTALLERS NOTE:  FOR THOSE OPERATING SYSTEMS THAT SUPPORT
!  A LIBRARY ROUTINE FOR EXTRACTING ARGUMENTS FROM THE COMMAND
!  LINE, PUT IN A BRANCH AND USE THAT MECHANISM.  FOR THOSE THAT
!  DON'T, READ THE ARGUMENTS FROM THE FILE "DPARGS.DAT".
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!               ********************************
!               **  STEP 2A1-                 **
!               **  IBM/PC USING MICROSOFT    **
!               **  COMPILER.  THE MSFORT.F   **
!               **  ROUTINE ACTUALLY DOES THE **
!               **  EXTRACTION, SIMPLY GET OUT**
!               **  OF COMMON HERE.           **
!               ********************************
!
!       2021/06: FOR WINDOWS, COMMAND LINE ARGUMENTS ARE PROCESSED
!                IN "MSFORT_INTEL.F".  HOWEVER, THIS IS NOT EXECUTED
!                BY THE INTEL CONSOLE EXECUTABLE.  SO ADD SECTION
!                FOR PROCESSING COMMAND LINE ARGUMENTS WHEN RUNNING
!                THE CONSOLE VERSION RATHER THAN THE QUICKWIN VERSION.
!
!
        NUMARG=0
        NUMCLA=0
        IECHFL=0
!
        IF(IQWNFL.EQ.'OFF' .OR. IQWNFL.EQ.'CLOS')THEN
          NPCARG=0
          NUM=0
#ifdef INTEL_CONS
          NUM=IARGC()
#endif
          IF(NUM.GE.1)THEN
            DO 210 I=0,NUM
#ifdef INTEL_CONS
              CALL GETARG(I,BUF,ISTAT)
#else
              BUF=' '
              ISTAT=0
#endif
              IF(I.EQ.0)GO TO 210
              IF(ISTAT.GE.2 .AND. BUF(1:2).EQ.'-W' .OR.   &
                 BUF(1:2).EQ.'-w')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.2 .AND. BUF(1:2).EQ.'-H' .OR.   &
                     BUF(1:2).EQ.'-h')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-SVGA' .OR.   &
                     BUF(1:5).EQ.'-svga')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.6 .AND. BUF(1:6).EQ.'-LARGE' .OR.   &
                     BUF(1:6).EQ.'-large')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.7 .AND. BUF(1:7).EQ.'-LAPTOP' .OR.   &
                     BUF(1:7).EQ.'-laptop')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.9 .AND. BUF(1:9).EQ.'-FILLIBEN' .OR.   &
                     BUF(1:9).EQ.'-filliben')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.10 .AND. BUF(1:10).EQ.'-EXTRAWIDE' .OR.   &
                     BUF(1:10).EQ.'-extrawide')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.16 .AND.   &
                     BUF(1:16).EQ.'-TILE_HORIZONTAL' .OR.   &
                     BUF(1:16).EQ.'-tile_horizontal')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-TILE' .OR.   &
                     BUF(1:5).EQ.'-tile')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.7 .AND. BUF(1:7).EQ.'-NOTILE' .OR.   &
                     BUF(1:7).EQ.'-notile')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'-TRUE' .OR.   &
                     BUF(1:5).EQ.'-true')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.7 .AND. BUF(1:5).EQ.'-NOTRUE' .OR.   &
                     BUF(1:7).EQ.'-notrue')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.4 .AND. BUF(1:4).EQ.'-RGB' .OR.   &
                     BUF(1:4).EQ.'-rgb')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.6 .AND. BUF(1:6).EQ.'-NORGB' .OR.   &
                     BUF(1:6).EQ.'-norgb')THEN
                CONTINUE
              ELSEIF(ISTAT.GE.4 .AND. BUF(1:4).EQ.'ECHO' .OR.   &
                     BUF(1:4).EQ.'echo')THEN
                IECHFL=1
              ELSEIF(ISTAT.GE.3 .AND. BUF(1:3).EQ.'GUI' .OR.   &
                     BUF(1:3).EQ.'gui')THEN
                IGUIFL='ON'
              ELSEIF(ISTAT.GE.5 .AND. BUF(1:5).EQ.'NOGUI' .OR.   &
                     BUF(1:5).EQ.'nogui')THEN
                IGUIFL='OFF'
              ELSE
                IF(ISTAT.GE.1 .AND. ISTAT.LE.80 .AND. ICNT.LE.49)THEN
                  ICNT=ICNT+1
                  NCFTMP(ICNT)=ISTAT
                  FTEMP(ICNT)(1:ISTAT)=BUF(1:ISTAT)
                ENDIF
              ENDIF
  210       CONTINUE
            NPCARG=ICNT
          ENDIF
          GO TO 9000
        ELSEIF(IQWNFL.EQ.'ON' .OR. IQWNFL.EQ.'OPEN')THEN
          NUMARG=NPCARG
          IF(NUMARG.GE.1)THEN
            ICNT=0
            ICNT2=0
            DO 1010 I=1,NUMARG
              IJUNK=I
              ITEMP=' '
              ICNT=ICNT+1
              NC1=NCFTMP(ICNT)
              ITEMP(1:NC1)=FTEMP(ICNT)(1:NC1)
              IF(ICNT.EQ.1)THEN
!
!             CHECK TO SEE IF FIRST UNLABELLED ARGUMENT IS
!             A FILE NAME.
!
                CALL STRLEZ(ITEMP,MAXFNC,NC1)
                IFLAG=0
                DO 1020 KK=1,NC1
                  IF(ITEMP(KK:KK).EQ.'.')THEN
                    IFLAG=1
                    GO TO 1029
                  ENDIF
 1020           CONTINUE
 1029           CONTINUE
!
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
                      IMACAR(ICNT2)(1:NLEN)=   &
                             ITEMP(NPOSR1:NC1)
                      NMACLA=NMACLA+1
                      IMACLL(NMACLA)=ICNT2
                      NLEN2=NPOSL2-NPOSL1+1
                      IF(NLEN2.GT.8)NLEN2=8
                      IMACLA(NMACLA)(1:NLEN2)=   &
                             ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                      IMACNC(NMACLA)=NLEN2
                    ELSE
                      IMACAR(ICNT2)(1:MIN(MAXFNC,NC1))=   &
                           ITEMP(1:MIN(MAXFNC,NC1))
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
                      IMACAR(ICNT2)(1:NLEN)=   &
                             ITEMP(NPOSR1:NCT)
                      NMACLA=NMACLA+1
                      IMACLL(NMACLA)=ICNT2
                      NLEN2=NPOSL2-NPOSL1+1
                      IF(NLEN2.GT.8)NLEN2=8
                      IMACLA(NMACLA)(1:NLEN2)=   &
                             ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                      IMACNC(NMACLA)=NLEN2
                    ELSE
                      IMACAR(ICNT2)(1:MIN(MAXFNC,NCT))=   &
                             ITEMP(1:MIN(MAXFNC,NCT))
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
                    IMACAR(ICNT2)(1:NLEN)=   &
                           ITEMP(NPOSR1:NCT)
                    NMACLA=NMACLA+1
                    IMACLL(NMACLA)=ICNT2
                    NLEN2=NPOSL2-NPOSL1+1
                    IF(NLEN2.GT.8)NLEN2=8
                    IMACLA(NMACLA)(1:NLEN2)=   &
                           ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                    IMACNC(NMACLA)=NLEN2
                  ELSE
                    IMACAR(ICNT2)(1:MIN(80,NCT))=   &
                           ITEMP(1:MIN(80,NCT))
                  ENDIF
                ENDIF
              ENDIF
 1010     CONTINUE
          ENDIF
          NMACAG=ICNT2
          GO TO 9000
        ENDIF
!
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG')THEN
!
!               ********************************
!               **  STEP 2A--                 **
!               **  IBM/PC 386 WITH OTG COMPILER
!               **  ALSO FOR HOSTS WITH NO    **
!               **  SPECIFIC MECHANISM FOR    **
!               **  CAPTURING COMMAND LINE    **
!               **  ARGUMENTS                 **
!               ********************************
!
        NUMCLA=0
        NC1=0
        CLARG2(1:1)=' '
!
!       APRIL 1992.  CHECK FOR OPERATING HOST.  READ FROM FILE
!       DPARGS.DAT.  IF NO OPERATING SPECIFIC MECHANISM SUPPORTED.
!
!       STEP 1--
!       INQUIRE TO SEE IF THE FILE EXISTS
!
        CALL DPINFI(ITEMNA,IEXIST,IOPEN,IACC,ISUBN0,IBUGS2,   &
                    ISUBRO,IERRFI)
        IF(IEXIST.EQ.'YES')GO TO 1100
        GO TO 9000
!
!       STEP 2--
!       IF EXISTS, THEN OPEN THE FILE
!       AND READ THE FIRST (ONLY) LINE FROM THE FILE INTO CLARG1
!
 1100   CONTINUE
        CALL DPOPFI(ITEMNU,ITEMNA,ITEMST,ITEMFO,ITEMAC,ITEMPR,ITEMCS,   &
                    ITEMRW,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
        READ(ITEMNU,1120)CLARG1
 1120   FORMAT(A80)
!
!       STEP 3--
!       CLOSE THE FILE
!
        CALL DPCLFI(ITEMNU,ITEMNA,ITEMST,ITEMFO,ITEMAC,ITEMPR,ITEMCS,   &
                    ITEMEF,ITEMRW,ISUBN0,IERRFI,IBUGS2,ISUBRO,IERROR)
!
!       STEP 4--
!       EXTRACT THE (1 OR 2) ARGUMENTS FROM THE FILE
!
        IF(CLARG1(1:4).EQ.'    ')GO TO 9000
        IF(CLARG1(1:4).EQ.'ECHO')GO TO 9000
!
        NCALL=80
        DO 1130 I=1,NCALL
          I2=I
          IF(CLARG1(I:I).EQ.' ')THEN
            NC1=I2-1
            GO TO 1139
          ENDIF
 1130   CONTINUE
        NC1=NCALL
 1139   CONTINUE
!
        IF(NC1.GE.NCALL-1)THEN
          NUMCLA=1
        ELSEIF(NC1.LE.0)THEN
          NUMCLA=0
        ELSEIF(CLARG1(NC1+2:NC1+2).EQ.' ')THEN
          NUMCLA=1
        ELSE
          CLARG2(1:1)=CLARG1(NC1+2:NC1+2)
          NUMCLA=2
          DO 1145 I=NC1+1,NCALL
            CLARG1(I:I)=' '
 1145     CONTINUE
        ENDIF
        GO TO 9000
!
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!               ********************************
!               **  STEP 2B--                 **
!               **  CYBER WITH NOS/VE         **
!               **  USE PARAM FUNCTION.  NOTE **
!               **  THAT PARAMETER MUST BE    **
!               **  NAMED (USE F FOR FILE AND **
!               **  B FOR BANNER OPTION.      **
!               **  NOTE THAT A C$   PARAM    **
!               **  STATEMENT APPEARS IN THE  **
!               **  MAIN PROGRAM (THIS IS     **
!               **  REQUIRED).                **
!               ********************************
!
#ifdef NOS_VE
        NUMCLA=0
        IF(TSTPARM('F')) THEN
          CALL GETCVAL('F',1,1,,'LOW',NC1,CLARG1)
          IF(CLARG1(1:4).EQ.'    ')GO TO 9000
          IF(CLARG1(1:4).EQ.'ECHO')GO TO 9000
          NUMCLA=1
        ENDIF
        IF(TSTPARM('B')) THEN
          CALL GETCVAL('B',1,1,'LOW',NC2,ITEMP)
          CLARG2(1:1)=ITEMP(1:1)
          NUMCLA=2
        ENDIF
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'CRAY')THEN
!
!               ********************************
!               **  STEP 2C.1--               **
!               **  CRAY UNIX USES A DIFFERENT**
!               **  USE GETOARG CALL.         **
!               ********************************
!
#ifdef LINUX_CRAY
        NUMCLA=0
        IRET=GETOARG(CLARG1)
        IF(IRET.EQ.0)THEN
          CLARG1=' '
          GO TO 9000
        ELSE
          IF(CLARG1(1:4).EQ.'    ')GO TO 9000
          IF(CLARG1(1:4).EQ.'ECHO')GO TO 9000
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
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!               ********************************
!               **  STEP 2C--                 **
!               **  UNIX OPERATING SYSTEM     **
!               **  USE argv AND iargc CALLS. **
!               **  MAY NEED TO CHECK THAT THESE
!               **  ARE VALID ON YOUR UNIX    **
!               **  SYSTEM.                   **
!               ********************************
!
#ifdef LINUX
        NUMCLA=0
        NUMARG=iargc()
        NMACLA=0
        IECHFL=0
!
!CCCC   CHECK IF FIRST ARGUMENT IS EITHER "-GUI" OR "-NOGUI".
!CCCC   IF SO, SET "IPROAD" SWITCH.
!CCCC
!CCCC   2019/02: CHECK FOR FOLLOWING SWITCHES
!CCCC
!CCCC             -gui
!CCCC             -nogui
!CCCC             -echo
!CCCC
!CCCC   2021/06: SET IGUIFL FOR "-gui" AND "-nogui" SWITCHES
!
        IF(NUMARG.GE.1)THEN
          ICNT=0
          ICNT2=0
          DO 4010 I=1,NUMARG
            IJUNK=I
            ITEMP=' '
            CALL getarg(IJUNK,ITEMP)
            IF(ITEMP(1:4).EQ.'-gui' .OR. ITEMP(1:4).EQ.'-GUI')THEN
              IPROAD='OFF'
              IGUIFL='ON'
            ELSEIF(ITEMP(1:6).EQ.'-nogui' .OR.   &
                   ITEMP(1:6).EQ.'-NOGUI')THEN
              IGUIFL='OFF'
            ELSEIF(ITEMP(1:5).EQ.'-echo' .OR.   &
                   ITEMP(1:5).EQ.'-ECHO')THEN
              IECHFL=1
            ELSE
              ICNT=ICNT+1
              IF(ICNT.EQ.1)THEN
!
!               CHECK TO SEE IF FIRST UNLABELLED ARGUMENT IS
!               A FILE NAME.
!
                CALL STRLEZ(ITEMP,MAXFNC,NC1)
                CALL STREQU(ITEMP,NC1,NPOSL1,NPOSL2,NPOSR1)
                IFLAG=0
                DO 4020 KK=1,NC1
                  IF(ITEMP(KK:KK).EQ.'.')THEN
                    IFLAG=1
                    GO TO 4029
                  ENDIF
 4020           CONTINUE
 4029           CONTINUE
!
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
                      IMACAR(ICNT2)(1:NLEN)=   &
                             ITEMP(NPOSR1:NC1)
                      NMACLA=NMACLA+1
                      IMACLL(NMACLA)=ICNT2
                      NLEN2=NPOSL2-NPOSL1+1
                      IF(NLEN2.GT.8)NLEN2=8
                      IMACLA(NMACLA)(1:NLEN2)=   &
                             ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                      IMACNC(NMACLA)=NLEN2
                    ELSE
                      IMACAR(ICNT2)(1:MIN(80,NC1))=   &
                             ITEMP(1:MIN(80,NC1))
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
                      IMACAR(ICNT2)(1:NLEN)=   &
                             ITEMP(NPOSR1:NCT)
                      NMACLA=NMACLA+1
                      IMACLL(NMACLA)=ICNT2
                      NLEN2=NPOSL2-NPOSL1+1
                      IF(NLEN2.GT.8)NLEN2=8
                      IMACLA(NMACLA)(1:NLEN2)=   &
                             ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                      IMACNC(NMACLA)=NLEN2
                    ELSE
                      IMACAR(ICNT2)(1:MIN(80,NCT))=   &
                             ITEMP(1:MIN(80,NCT))
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
                    IMACAR(ICNT2)(1:NLEN)=   &
                           ITEMP(NPOSR1:NCT)
                    NMACLA=NMACLA+1
                    IMACLL(NMACLA)=ICNT2
                    NLEN2=NPOSL2-NPOSL1+1
                    IF(NLEN2.GT.8)NLEN2=8
                    IMACLA(NMACLA)(1:NLEN2)=   &
                           ITEMP(NPOSL1:NPOSL1+NLEN2-1)
                    IMACNC(NMACLA)=NLEN2
                  ELSE
                    IMACAR(ICNT2)(1:MIN(MAXFNC,NCT))=   &
                           ITEMP(1:MIN(MAXFNC,NCT))
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 4010     CONTINUE
          NMACAG=ICNT2
        ENDIF
!
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!               ********************************
!               **  STEP 2D--                 **
!               **  VAX/VMS                   **
!               **  USE CLI$PRESENT AND       **
!               **  CLI$GET_VALUE LIBRARY CALLS*
!               **  DEFAULT PARAMETER NAMES ARE*
!               **  P1 AND P2.                **
!               ********************************
!       IMPLEMENTORS NOTE.  THIS CODE HAS NOT BEEN TESTED!!!
!       IT LOOKS RIGHT ACCORDING TO VAX FORTRAN MANUAL, BUT IT
!       MAY NEED TO BE DEBUGGED.  ALSO, UNCOMMENT "INTEGER*2 NCTEMP"
!       LINE IN DECLARATION.
!
#ifdef VAX_VMS
        NUMCLA=0
        IF(CLI$PRESENT('P1'))THEN
          ISTATUS=CLI$GET_VALUE('P1',CLARG1,NCTEMP)
          IF(CLARG1(1:4).EQ.'    ')GO TO 9000
          IF(CLARG1(1:4).EQ.'ECHO')GO TO 9000
          NC1=NCTEMP
          NUMCLA=1
        ENDIF
        IF(CLI$PRESENT('P2'))THEN
          ISTATUS=CLI$GET_VALUE('P1',ITEMP,NCTEMP)
          CLARG2(1:1)=ITEMP(1:1)
          NUMCLA=2
        ENDIF
#endif
        GO TO 9000
      ELSE
        GO TO 9000
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE CKCLAR
      SUBROUTINE DPSYS2(ITEXT,IWIDTH,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE IS USED BY DPSYST AND A FEW OTHER ROUTINES
!              TO ENTER AN OPERATING SYSTEM COMMAND. IT WAS ISOLATED
!              FROM DPSYST SO THAT THERE IS ONLY ONE ROUTINE THAT
!              ACTUALLY ISSUES AN OPERATING DEPENDENDENT CALL.
!     TO THE IMPLEMENTER--
!              SOME (SIMPLE) EDITING MUST BE DONE ONE THIS ROUTINE BEFORE
!              IT WILL RUN ON ANY COMPUTER.  IN GENERAL, ACTIVATE ALL
!              LINES RELATING TO YOUR COMPUTER BY REMOVING ALL PREFIXES
!              DESIGNATING YOUR COMPUTER.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTUTUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--89.3
!     ORIGINAL VERSION--APRIL      1992.
!     UPDATED         --APRIL      1992. ISUBRO & DEBUG STATEMENTS
!     UPDATED         --APRIL      1992. COMPILER=OTG
!     UPDATED         --JANUARY    1994. COMMENTS ABOVE
!     UPDATED         --MAY        1994. IMPLEMENT FOR CRAY
!     UPDATED         --JUNE       1996. LAHEY COMPILER FOR PC.
!     UPDATED         --OCTOBER    1996. MICROSOFT COMPILER FOR PC.
!     UPDATED         --NOVEMBER   2002. FOR MICROSOFT COMPILER,
!                                        OPTION TO USE EITHER
!     UPDATED         --NOVEMBER   2015. FOR WINDOWS, ISSUES IF STRING
!                                        CONTAINS QUOTES
!     UPDATED         --APRIL      2018. SUPPORT FOR
!                                        "COMMAND_LINE_EXECUTE"
!     UPDATED         --APRIL      2018. USE PRE_PROCESSOR DIRECTIVES
!                                        TO ALLOW SAME CODE TO BE USED
!                                        ON LINUX AND WINDOWS
!     UPDATED         --MARCH      2019. SUPPORT FOR "PERSISTANCE" AND
!                                        "HIDDEN" FOR WINDOWS SYSTEMS
!     UPDATED         --DECEMBER   2022. PRE-PROCESSOR OPTION TO ALLOW
!                                        DISABLING SYSTEM COMMAND (E.G.,
!                                        FOR USE IN WEB APPLICATIONS)
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
#ifdef INTEL_QWIN
      USE IFLPORT
#endif
#ifdef INTEL
      USE DFLIB
      USE DFWIN
      USE IFPORT
#ifdef HAVE_EXECUTE_COMMAND_LINE
      CHARACTER*512 ISTRIN
#endif
#ifdef DISABLE_SYSTEM_COMMAND
#else
      CHARACTER*100 ICMSG
      INTEGER IESTAT
      INTEGER ICSTAT
      LOGICAL IWAIT
#endif
#endif
!
      CHARACTER*(*) ITEXT
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*1 IQUOT1
      CHARACTER*1 IQUOT2
      CHARACTER*4 IBUGS2
!
!  UNCOMMENT FOLLOWING TWO LINES FOR VAX/VMS
#ifdef VAX_VMS
      INTEGER LIB$SPAWN
      INTEGER ISTAT2
#endif
#ifdef LINUX
#ifdef DISABLE_SYSTEM_COMMAND
#else
      CHARACTER*100 ICMSG
      INTEGER IESTAT
      INTEGER ICSTAT
      LOGICAL system
      LOGICAL IWAIT
#endif
#endif
      LOGICAL ISTATUS
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
      IBUGS2='OFF'
!
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
   55     FORMAT('(ITEXT(I:I),I=81,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IWIDTH.GT.160)THEN
          WRITE(ICOUT,56)(ITEXT(I:I),I=161,MIN(IWIDTH,240))
   56     FORMAT('(ITEXT(I:I),I=161,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        WRITE(ICOUT,61)IHOST1,IHOST2,IHMOD1,IHMOD2,ISUBRO
   61   FORMAT('IHOST1,IHOST2,IHMOD1,IHMOD2,ISUBRO = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,63)IOPSY1,IOPSY2,ICOMPI,ISITE,ISYSPE,ISYSHI
   63   FORMAT('IOPSY1,IOPSY2,ICOMPI,ISITE,ISYSPE,ISYSHI = ',   &
               5(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               ********************************
!               **  STEP 1--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
!     ALL LINUX/UNIX HOSTS TESTED BY OPERATING SYSTEM RATHER THAN HOST.
!     THE CRAY DOES NOT SUPPORT THE STANDARD UNIX CALL "SYSTEM", SO
!     EXPLICITLY TEST FOR IT.
!
!     TWO METHODS SUPPORTED:
!
!        1. THE FORTRAN 2008 STANDARD INTRODUCED THE
!           "COMMAND_LINE_EXECUTE" COMMAND FOR THIS PURPOSE.
!           THE gfortran COMPILER ADDED SUPPORT FOR THIS AT
!           VERSION 4.6.  USE A PRE-PROCESSOR FLAG SINCE OLDER
!           VERSIONS OF THE COMPILER ARE STILL AROUND.
!
!        2. USE THE "SYSTEM" CALL.  THIS WORKS FOR gfortran, BUT IT IS
!           NOT PART OF ANY FORTRAN STANDARD.  SO IT MAY OR MAY NOT BE
!           AVAILABLE ON COMPILERS OTHER THAN gfortran.
!
 2311 FORMAT('***** ERROR IN SYSTEM COMMAND--')
!
      IF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
        IF(IHOST1.EQ.'CRAY')GO TO 8000
#ifdef LINUX
        IF(ILINSY.EQ.'COMM')THEN
!
!         USE FORTRAN 2008 "EXECUTE_COMMAND_LINE" SYNTAX
!
          IQUOT1='"'
          IQUOT2="'"
!
!         FOLLOWING BLOCK TO ENCLOSE STRING IN SINGLE QUOTES
!         DOES NOT SEEM NECCESSARY, SO COMMENT OUT FOR NOW.
!
!CCCC     IF(ITEXT(1:1).NE.IQUOT2 .AND.
!CCCC1       ITEXT(IWIDTH:IWIDTH).NE.IQUOT2 .AND.
!CCCC1       IWIDTH.LE.253)THEN
!CCCC       DO2325J=IWIDTH,1,-1
!CCCC         ITEXT(J+1:J+1)=ITEXT(J:J)
!2325       CONTINUE
!CCCC       ITEXT(IWIDTH+2:IWIDTH+2)=IQUOT1
!CCCC       ITEXT(1:1)=IQUOT1
!CCCC       IWIDTH=IWIDTH+2
!CCCC     ENDIF
!
          IWAIT=.FALSE.
          IF(ICLEWT.EQ.'ON')IWAIT=.TRUE.
!
!         2018/04: FOLLOWING LINE IS SUPPORTED FOR gcc VERSION 5.x.
!                  HOWEVER, IT DOES NOT APPEAR TO BE SUPPORTED ON
!                  EARLIER VERSIONS.
!
!                  IN THE BUILD FILE, ADD A PRE-PROCESSOR OPTION FOR
!                  THIS SO WE CAN CONDITIONALLY COMPILE THIS.
!
#ifdef HAVE_EXECUTE_COMMAND_LINE
#ifdef DISABLE_SYSTEM_COMMAND
          WRITE(ICOUT,2311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2321)
 2321     FORMAT('      THE SYSTEM COMMAND HAS BEEN DISABLED FOR ',   &
                 'THIS IMPLEMENTATION.')
          CALL DPWRST('XXX','BUG ')
#else
          CALL EXECUTE_COMMAND_LINE(ITEXT,IWAIT,IESTAT,ICSTAT,ICMSG)
          IF(ICSTAT.GT.0)THEN
            WRITE(ICOUT,2311)
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
!CCCC       IF(IFEEDB.EQ.'ON')THEN
!CCCC         WRITE(ICOUT,999)
!CCCC         CALL DPWRST('XXX','BUG ')
!CCCC         WRITE(ICOUT,2346)IESTAT
!2346         FORMAT('      COMMAND COMPLETED WITH STATUS ',I8)
!CCCC         CALL DPWRST('XXX','BUG ')
!CCCC       ENDIF
          ENDIF
#endif
#else
          WRITE(ICOUT,2311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2342)ICMSG
 2342     FORMAT('      EXECUTE_COMMAND_LINE FAILED WITH ERROR',A100)
          CALL DPWRST('XXX','BUG ')
#endif
        ELSE
#ifdef DISABLE_SYSTEM_COMMAND
          WRITE(ICOUT,2311)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,2323)
 2323     FORMAT('      THE SYSTEM COMMAND HAS BEEN DISABLED FOR ',   &
                 'THIS IMPLEMENTATION.')
          CALL DPWRST('XXX','BUG ')
#else
          ISTAT=system(ITEXT(1:IWIDTH))
#endif
        ENDIF
!CCCC   IF(ISTAT)IERROR='YES'
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-')THEN
        IF(ICOMPI.EQ.'MS-F')THEN
#ifdef INTEL
!
!         *****************************************************
!         *  PC USING INTEL (FORMERLY MICROSOFT, COMPAQ       *
!         *  DIGITAL).  SEVERAL METHODS SUPPORTED (SEE NOTES) *
!         *********************************************************
!
!         THE FOLLOWING METHODS ARE SUPPORTED:
!
!            1. SYSTEMQQ - THIS IS THE DEFAULT METHOD.  NOTE THE
!               FOLLOWING:
!
!               a. THIS IS PART OF THE INTEL LIBRARY ROUTINES AND
!                  IS NOT STANDARD FORTRAN.
!
!               b. SYSTEMQQ POPS UP A CONSOLE WINDOW, WAITS FOR
!                  THE COMMAND TO EXECUTE, AND THEN CLOSES THE
!                  WINDOW.
!
!               c. SYSTEMQQ DOES NOT PROVIDE CONTROL OVER OPENING/
!                  CLOSING THE COMMAND WINDOW AND DOES NOT HAVE AN
!                  AN OPTION FOR RETURNING CONTROL BACK TO DATAPLOT
!                  BEFORE THE OS COMMAND IS FINISHED EXECUTING.
!
!               d. PREVIOUS VERSIONS ALSO SUPPORTED RUNQQ.  HOWEVER,
!                  THERE ISN'T REALLY ANY REASON TO PREFER RUNQQ
!                  OVER SYSTEMQQ, SO WE HAVE DROPPED THIS AS A SEPARATE
!                  OPTION.
!
!            2. USE  CALL EXECUTE_COMMAND_LINE
!
!               THIS IS SUPPORTED IN THE FORTRAN 2008 STANDARD.
!               HOWEVER, IT MAY NOT BE SUPPORTED IN ALL WINDOWS
!               FORTRAN COMPILERS (ADDED TO INTEL COMPILER WITH
!               VERSION 16).
!
!            3. THE FOLLOWING ARE STILL EXPERIMENTAL:
!
!                  WINEXEC
!                  CreateProcess
!                  ShellScript
!
!               THESE ARE MORE COMPLEX THAN THE ABOVE OPTIONS, BUT
!               PROVIDE MUCH GREATER FLEXIBILITY IN HOW AN APPLICATION
!               IS STARTED AND RUN.  FOR NOW, THESE ARE NOT ACTUALLY
!               WORKING, BUT CODE IS LEFT IN FOR POSSIBLE FUTURE
!               DEVELOPMENT.
!
!         2019/03: SUPPORT FOR FOLLOWING OPTIONS
!
!                     SET SYSTEM PERSIST <ON/OFF>
!                     SET SYSTEM HIDDEN <ON/OFF>
!
!                  IF PERSIST IS SET TO "ON", LEAVE THE DOS COMMAND
!                  WINDOW OPEN AFTER THE COMMAND EXECUTES.  DO THIS
!                  BY ADDING "cmd /K" TO BEGINNING OF LINE.  THIS IS
!                  ONLY USED FOR THE SYSTEMQQ OPTION.
!
!                  IF HIDDEN IS SET TO "ON", DO NOT SHOW THE DOS
!                  COMMAND WINDOW.  HIDDEN WILL USE THE
!                  "EXECUTE_COMMAND_LINE" OPTION (IF AVAILABLE).  IF
!                  THIS OPTION IS NOT AVAILABLE, SYSTEMQQ WILL USED AND
!                  THE DOS COMMAND WINDOW WILL APPEAR.  NOTE THAT SOME
!                  DATAPLOT COMMANDS THAT CALL DPSYS2 WILL SET "HIDDEN"
!                  AUTOMATICALLY.
!
!         STEP 1: COUNT NUMBER OF DOUBLE QUOTES (") IN THE STRING
!
          IQUOT1='"'
          IQUOT2="'"
          NQUOTE=0
          DO 2610 II=1,IWIDTH
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
 2613       FORMAT('      UNBALANCED QUOTES DETECTED (',I3,' QUOTES ',   &
                   'DETECTED).')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
!CCCC     IF(NQUOTE.GE.1)THEN
!CCCC       IWIDSV=IWIDTH
!CCCC       CALL DPCONA(92,IBASLC)
!CCCC       print *,'nquote,ibaslc = ',nquote,ibaslc
!CCCC       IFLAG=0
!CCCC       DO2621II=IWIDTH,1,-1
!CCCC         IF(IFLAG.EQ.1)THEN
!CCCC           IFLAG=0
!CCCC           GO TO 2621
!CCCC         ENDIF
!CCCC         CALL DPCOAN(ITEXT(II:II),IVAL)
!CCCC         print *,'ii,itext(ii:ii),ival = ',ii,itext(ii:ii),ival
!CCCC         IF(ITEXT(II:II).EQ.IBASLC)THEN
!CCCC         IF(IVAL.EQ.92)THEN
!CCCC           print *,'itext(ii:ii) = \ case'
!CCCC           IF(II.GT.1 .AND.ITEXT(II-1:II-1).EQ.IBASLC)THEN
!CCCC             print *,'previous character also \'
!CCCC             IFLAG=1
!CCCC           ELSE
!CCCC             print *,'add \ character case'
!CCCC             ITEXT(II+1:IWIDSV+1)=ITEXT(II:IWIDSV)
!CCCC             ITEXT(II:II)=IBASLC
!CCCC             IWIDSV=IWIDSV+1
!CCCC           ENDIF
!CCCC         ELSE
!CCCC           print *,'no match \ case'
!CCCC         ENDIF
!2621       CONTINUE
!CCCC       IWIDTH=IWIDSV
!CCCC     ENDIF
!
!         STEP 2: 2019/03 - IF "HIDDEN" IS SPECIFIED OR A
!                 "SET QWIN SYSTEM EXECUTE COMMAND LINE" COMMAND GIVEN,
!                 THEN USE "CALL EXECUTE COMMAND LINE".
!
!                 NOTE THAT ALTHOUGH THE FOLLOWING WORKS WHEN RUNNING
!                 FROM A COMMAND WINDOW (I.E., DOES NOT OPEN NEW
!                 COMMAND WINDOW), IT DOES NOT WORK FROM WITHIN A
!                 "SYSTEMQQ" CALL.
!
!                     start /d <current path> /min cmd /c
!                           <string>
!
#ifdef HAVE_EXECUTE_COMMAND_LINE
          IF(ISYSHI.EQ.'ON' .OR. IQWNSY.EQ.'COMM')THEN
            NC=0
            ISTRIN=' '
            DO 2410 II=1,IWIDTH
              NC=NC+1
              ISTRIN(NC:NC)=ITEXT(II:II)
 2410       CONTINUE
!CCCC       NC=NC+1
!CCCC       ISTRIN(NC:NC)="'"
            IWAIT=.FALSE.
            IF(ICLEWT.EQ.'ON')IWAIT=.TRUE.
#ifdef DISABLE_SYSTEM_COMMAND
            WRITE(ICOUT,2311)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2325)
 2325       FORMAT('      THE SYSTEM COMMAND HAS BEEN DISABLED FOR ',   &
                   'THIS IMPLEMENTATION.')
            CALL DPWRST('XXX','BUG ')
#else
            CALL EXECUTE_COMMAND_LINE(ISTRIN,WAIT=IWAIT,EXITSTAT=IESTAT,CMDSTAT=ICSTAT,CMDMSG=ICMSG)
#endif
            IF(ICSTAT.GT.0)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2642)ICMSG
 2642         FORMAT('      EXECUTE_COMMAND_LINE FAILED WITH ERROR ',   &
                     A100)
              CALL DPWRST('XXX','BUG ')
            ELSEIF(ICSTAT.LT.0)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2644)
 2644         FORMAT('      COMMAND EXECUTION NOT SUPPORTED.')
              CALL DPWRST('XXX','BUG ')
            ELSE
!CCCC         IF(IFEEDB.EQ.'ON')THEN
!CCCC           WRITE(ICOUT,999)
!CCCC           CALL DPWRST('XXX','BUG ')
!CCCC           WRITE(ICOUT,2646)IESTAT
!2646           FORMAT('      COMMAND COMPLETED WITH STATUS ',I8)
!CCCC           CALL DPWRST('XXX','BUG ')
!CCCC         ENDIF
            ENDIF
!
            IF(ISUBRO.EQ.'SYS2')THEN
                WRITE(ICOUT,2411)NC
 2411           FORMAT('WINDOWS SYSTEM HIDDEN CASE: NC = ',I5)
                CALL DPWRST('XXX','BUG ')
                WRITE(ICOUT,2413)ISTRIN(1:MIN(NC,220))
 2413           FORMAT('ISTRIN: ',A)
                CALL DPWRST('XXX','BUG ')
            ENDIF
!
            GO TO 9000
          ENDIF
#endif
!
!         STEP 2: FOR SYSTEMQQ, THE FIRST AND LAST OCCURENCES OF THE
!                 DOUBLE QUOTE NEED TO BE REPEATED.
!
!                 2018/04: CHANGE THIS SO WE SIMPLY ENCLOSE
!                          THE STRING IN SINGLE QUOTES REGARDLESS OF
!                          POSITION OR OCCURENCE OF DOUBLE QUOTES.
!
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
            DO 2620 I=IWIDTH,1,-1
              IF(ITEXT(I:I).EQ.IQUOT1)THEN
                NCNT=NCNT+1
                IF(NCNT.EQ.NFIRST .OR. NCNT.EQ.NLAST)THEN
                  IF(I.LT.IWIDT2)THEN
                    DO 2630 J=IWIDT2,I+1,-1
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
!
          IF(ISYSPE.EQ.'ON' .AND. IWIDTH.LE.246)THEN
            IF(ITEXT(1:4).NE.'cmd ' .AND. ITEXT(1:4).NE.'CMD ')THEN
              DO 22635 I=IWIDTH,1,-1
                ITEXT(I+7:I+7)=ITEXT(I:I)
22635         CONTINUE
              ITEXT(1:7)='cmd /K '
              IWIDTH=IWIDTH+7
            ENDIF
          ENDIF
!
#ifdef DISABLE_SYSTEM_COMMAND
            WRITE(ICOUT,2311)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,2327)
 2327       FORMAT('      THE SYSTEM COMMAND HAS BEEN DISABLED FOR ',   &
                   'THIS IMPLEMENTATION.')
            CALL DPWRST('XXX','BUG ')
#else
          ISTATUS=SYSTEMQQ(ITEXT(1:IWIDTH))
          IF(.NOT.ISTATUS)THEN
            IERRID=GETLASTERRORQQ()
            IF(IERRID.EQ.ERR$2BIG)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2631)
 2631         FORMAT('      THE ARGUMENT LIST EXCEEDS 128 BYTES OR ',   &
                     'REQUIRED ENVIRONMENT SPACE TOO BIG.')
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
 2633         FORMAT('      THE COMMAND INTERPERTER FILE HAS AN ',   &
                     'INVALID FORMAT AND IS NOT EXECUTABLE.')
              CALL DPWRST('XXX','BUG ')
            ELSEIF(IERRID.EQ.ERR$NOEMEM)THEN
              WRITE(ICOUT,2611)
              CALL DPWRST('XXX','BUG ')
              WRITE(ICOUT,2634)
 2634         FORMAT('      INSUFFICIENT MEMORY TO EXECUTE THE ',   &
                     'COMMAND.')
              CALL DPWRST('XXX','BUG ')
            ENDIF
          ELSE
!CCCC.       IF(IFEEDB.EQ.'ON')THEN
!CCCC.         WRITE(ICOUT,999)
!CCCC.         CALL DPWRST('XXX','BUG ')
!CCCC.         WRITE(ICOUT,2641)ISTATUS
!2641         FORMAT('STATUS FROM SYSTEM COMMAND: ',L4)
!CCCC.         CALL DPWRST('XXX','BUG ')
!CCCC.       ENDIF
          ENDIF
#endif
!
!CCCC     IF(IQWNSY.EQ.'WINE')THEN
!CCCC       IRET=WinExec(ITEXT(1:IWIDTH)//' 'C,SW_SHOW)
!CCCC       GO TO 9000
!CCCC     ELSEIF(IQWNSY.EQ.'SHEL')THEN
!CCCC       IHWND=GETHWNDQQ(QWIN$FRAMEWINDOW)
!CCCC       print *,'ihwnd: ',ihwnd
!
!CCCC       DO2628J=IWIDTH,1,-1
!CCCC         ITEXT(J+1:J+1)=ITEXT(J:J)
!2628       CONTINUE
!CCCC       ITEXT(IWIDTH+2:IWIDTH+2)=IQUOT1
!CCCC       ITEXT(1:1)=IQUOT1
!CCCC       IWIDTH=IWIDTH+2
!
!CCCC       IRET=ShellExecute(HWND=IHWND,
!CCCC1                        lpOperation="open"C,
!CCCC1                        lpFile=trim(ITEXT)//char(0),
!CCCC1                        lpParameters=NULL,
!CCCC1                        lpDirectory=NULL,
!CCCC1                        nShowCmd=SW_HIDE)
!CCCC1                        nShowCmd=SW_SHOWNORMAL)
!CCCC       print *,'iret = ',iret
!CCCC       GO TO 9000
!CCCC     ELSEIF(IQWNSY.EQ.'CREA')THEN
!CCCC       GO TO 9000
!CCCC     ENDIF
!
#endif
!
        ELSEIF(IOPSY1.EQ.'OS38' .OR. ICOMPI.EQ.'OTG ')THEN
!
!         *********************************************************
!         *  IBM PC 386 - OTG COMPILER USES THE OTG "CISSUE" CALL *
!         *********************************************************
!
          IFAIL=0
#ifdef WIN32_OTG
         CALL CISSUE(ITEXT(1:IWIDTH),IFAIL)
#endif
          IERROR='NO'
          IF(IFAIL.EQ.1)IERROR='YES'
        ELSEIF(ICOMPI.EQ.'LAHE')THEN
!
!         *****************************************************
!         *  WINDOWS USING LAHEY ROUTINE SYSTEM               *
!         *****************************************************
!
#ifdef WIN32_LAHEY
          CALL SYSTEM(ITEXT(1:IWIDTH))
#endif
        ENDIF
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!       *********************************************************
!       *  CDC - NOS/VE OPERATING SYSTEM.  USE "SCLCMD" TO PASS *
!       *  COMMANDS TO THE OPERATING SYSTEM.                    *
!       *  DATAPLOT WILL DO NO ERROR CHECKING ON THE COMMAND    *
!       *********************************************************
!
#ifdef NOS_VE
        CALL SCLCMD(ITEXT(1:IWIDTH))
#endif
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!       *********************************************************
!       *  VAX/VMS - LEFT TO IMPLEMENTOR                        *
!       *********************************************************
!
!       NOTE TO IMPLEMENTOR.  USE OF LIB$SPAWN HAS NOT BEEN TESTED,
!       BUT SHOULD WORK ACCORDING TO VAX FORTRAN GUIDE.
!
#ifdef VAX_VMS
        ISTATUS=LIB$SPAWN(ITEXT(1:IWIDTH))
#endif
      ELSE
        GO TO 8000
      ENDIF
!
      GO TO 9000
!
!     *********************************************************
!     *  OTHER   - LEFT TO IMPLEMENTOR                        *
!     *********************************************************
!
 8000 CONTINUE
      WRITE(ICOUT,8010)
 8010 FORMAT(1X,'THE SYSTEM COMMAND HAS NOT BEEN IMPLEMENTED AT THIS')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,8020)IHOST1
 8020 FORMAT(1X,'SITE FOR A ',A4,' HOST.')
      CALL DPWRST('XXX','BUG ')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(ISUBRO.EQ.'SYS2')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPSYS2--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9013)ISTATUS,IERROR,IWIDTH
 9013   FORMAT('ISTATUS,IERROR,IWIDTH = ',L4,2X,A4,2X,I6)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9014)(ITEXT(I:I),I=1,MIN(IWIDTH,80))
 9014   FORMAT('(ITEXT(I:I),I=1,IWIDTH) = ',80A1)
        CALL DPWRST('XXX','BUG ')
        IF(IWIDTH.GT.80)THEN
          WRITE(ICOUT,9015)(ITEXT(I:I),I=81,MIN(IWIDTH,160))
 9015     FORMAT('(ITEXT(I:I),I=81,IWIDTH) = ',120A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
        IF(IWIDTH.GT.160)THEN
          WRITE(ICOUT,9017)(ITEXT(I:I),I=161,MIN(IWIDTH,240))
 9017     FORMAT('(ITEXT(I:I),I=161,IWIDTH) = ',80A1)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPSYS2
      SUBROUTINE DPTIME(CURRTIME,NCURRTIM,CURRDATE,NCURRDAT,   &
                        IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--PRINT OUT TIME AND DATE INFORMATION.
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--86/1
!     ORIGINAL VERSION--NOVEMBER  1980.
!     UPDATED         --MAY       1982.
!     UPDATED         --JANUARY   1986.
!     UPDATED         --SEPTEMBER 1990. TIME AND DATE FOR IBM-PC(JJF)
!     UPDATED         --APRIL     1992. FOR UNIX,VAX,CRAY,ETC. (ALAN)
!     UPDATED         --FEBRUARY  1993. ALSO BRANCH IF IMB- OTG
!     UPDATED         --FEBRUARY  1993. TIME & DATE AS OUTPUT ARGUMENT
!     UPDATED         --FEBRUARY  1993. CONDITIONAL WRITE OF TIME/DATE
!     UPDATED         --AUGUST    1993. FOR UNIX, VAX, NOS/VE (ALAN)
!     UPDATED         --AUGUST    1994. FOR UNIX, CURRDATE AND CURRTIME
!     UPDATED         --JUNE      1996. SUPPORT LAHEY COMPILER ON PC
!     UPDATED         --OCTOBER   1996. SUPPORT MS-FORTRAN COMPILER ON PC
!     UPDATED         --JULY      2018. USE FORTRAN 90 STANDARD CALL FOR
!                                       UNIX/LINUX
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*24 CURRTIME
      CHARACTER*24 CURRDATE
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
!CCCC THE FOLLOWING SECTIONS WERE ADDED          APRIL 1992 (ALAN)
!CCCC FOR HOST-DEPENDENT DECLARATIONS.           APRIL 1992 (ALAN)
!CCCC THE INSTALLER MUST COMMENT/UNCOMMENT OUT   APRIL 1992 (ALAN)
!CCCC APPROPRIATELY.                             APRIL 1992 (ALAN)
!
      CHARACTER*24 ADATE
      CHARACTER*24 ATIME
!
!CCCC FOR THE IBM/PC USING OTG COMPILER    SEPTEMBER 1990 (JJF)
#ifdef WIN32_OTG
      CHARACTER*8 TIME
      CHARACTER*8 DATE
#endif
!
!CCCC FOR THE VAX--
#ifdef VAX_VMS
      CHARACTER*23 DATETIME
      INTEGER LIB$DATE_TIME
      EXTERNAL LIB$DATE_TIME
#endif
!
!CCCC FOR NOS/VE--
#ifdef NOS_VE
      CHARACTER*10 DATE
      CHARACTER*8 TIME
#endif
!
!CCCC FOR UNIX--
#ifdef LINUX
!CCCC CHARACTER*24 fdate
#endif
!
!CCCC FOR CRAY UNICOS--
#ifdef LINUX_CRAY
      REAL DATE
      INTEGER TIME
#endif
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOF2.INC'
!CCCC THE FOLLOWING LINE WAS ADDED    SEPTEMBER 1990 (JJF)
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPTI'
      ISUBN2='ME  '
      IFOUND='YES'
      IERROR='NO'
!
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
!
!               **********************************************
!               **  STEP 14--                               **
!               **  IF THE NEEDED SYSTEM CALL               **
!               **  EXISTS AT THIS COMPUTER INSTALLATION,   **
!               **  THEN HAVE THE DATAPLOT IMPLEMENTOR      **
!               **  ENTER THE CODE FOR SUCH A CALL.         **
!               **  IF THE NEEDED SYSTEM CALL               **
!               **  DOES NOT EXIST (THE DEFAULT) AT THIS    **
!               **  COMPUTER INSTALLATION,                  **
!               **  THEN WRITE OUT AN ERROR MESSAGE.        **
!               **********************************************
!
      ISTEPN='12'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'TIME')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!               ******************************************
!               **  STEP 2F--                           **
!               **  IBM/PC 386 WITH MICROSOFT COMPILER  **
!               ******************************************
!
#ifdef INTEL
        ADATE='NULL'
        ATIME='NULL'
        CALL DATE_AND_TIME(DATE=ADATE, TIME=ATIME)
!CCCC   CALL TIME(ATIME(1:11))
!
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
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!               ********************************
!               **  STEP 2C--                 **
!               **  UNIX OPERATING SYSTEM     **
!               **  USE fdate CALL.           **
!               **  CHECK THE DECLARATIONS    **
!               **  FOR SOME LINES THAT NEED  **
!               **  TO BE UNCOMMENTED.        **
!               **  MAY NEED TO CHECK THAT fdate
!               **  IS VALID ON YOUR UNIX     **
!               **  SYSTEM.                   **
!               ********************************
!
!      2018/07: USE FORTRAN 90 "DATE_AND_TIME" ROUTINE AS THIS IS
!               STANDARD FORTRAN
!
#ifdef LINUX
!CCCC   ADATE='NULL'
!CCCC   ATIME='NULL'
!CCCC   ADATE(1:24)=fdate()
!
!CCCC   IF(IFEEDB.EQ.'ON')THEN
!CCCC     WRITE(ICOUT,3011)
!3011     FORMAT('THE CURRENT DATE AND TIME ARE:')
!CCCC     CALL DPWRST('XXX','BUG ')
!CCCC     WRITE(ICOUT,3012)ADATE(1:11),ADATE(21:24)
!3012     FORMAT(A11,1X,A4)
!CCCC     CALL DPWRST('XXX','BUG ')
!CCCC     WRITE(ICOUT,3013)ADATE(12:19)
!3013     FORMAT(A8)
!CCCC     CALL DPWRST('XXX','BUG ')
!CCCC   ENDIF
!
!CCCC   CURRDATE(1:11)=ADATE(1:11)
!CCCC   CURRDATE(12:15)=ADATE(21:24)
!CCCC   NCURRDAT=15
!CCCC   CURRTIME(1:8)=ADATE(12:19)
!CCCC   NCURRTIM=8
        ADATE='NULL'
        ATIME='NULL'
        CALL DATE_AND_TIME(DATE=ADATE, TIME=ATIME)
!
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
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.   &
            (ICOMPI.EQ.'OTG ' .OR. IOPSY1.EQ.'OS38'))THEN
!
!CCCC   THE FOLLOWING SECTION WAS ADDED FOR IBM-PC SEPTEMBER 1990 (JJF)
!CCCC   NOTE--TIME@() AND DATE@() ARE OTG RUN TIME LIBRARY ROUT. (JJF)
!
!               ********************************
!               **  STEP 2A--                 **
!               **  IBM/PC 386 WITH OTG COMPILER
!               ********************************
!
#ifdef WIN32_OTG
        ADATE='NULL'
        ATIME='NULL'
        ADATE(1:8)=DATE@()
        ATIME(1:8)=TIME@()
!
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
!
        CURRTIME(1:8)=ATIME(1:8)
        NCURRTIM=8
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'LAHE')THEN
!
!               **************************************
!               **  STEP 2E--                       **
!               **  IBM/PC 386 WITH LAHEY COMPILER  **
!               **************************************
!
#ifdef WIN32_LAHEY
        ADATE='NULL'
        ATIME='NULL'
        CALL DATE(ADATE(1:8))
        CALL TIME(ATIME(1:11))
!
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
!
        CURRTIME(1:11)=ATIME(1:11)
        NCURRTIM=11
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!
!               ********************************
!               **  STEP 2B--                 **
!               **  CYBER WITH NOS/VE         **
!               **  USE TEH DATE AND TIME     **
!               **  CALLS.  CHECK THE DECLARATIONS
!               **  FOR SOME LINES THAT NEED  **
!               **  TO BE UNCOMMENTED.        **
!               ********************************
!
#ifdef NOS_VE
        ADATE='NULL'
        ATIME='NULL'
        ADATE(1:10)=DATE()
        ATIME(1:10)=TIME()
!
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
!
        CURRTIME(1:10)=ATIME(1:10)
        NCURRTIM=10
        CURRDATE(1:10)=ADATE(1:10)
        NCURRDAT=10
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'CRAY')THEN
!
!               ********************************
!               **  STEP 2C.1--               **
!               **  CRAY UNIX USES DATE AND   **
!               **  TIME CALLS.               **
!               **  CHECK THE DECLARATIONS    **
!               **  FOR SOME LINES THAT NEED  **
!               **  TO BE UNCOMMENTED.        **
!               ********************************
!
#ifdef LINUX_CRAY
        ADATE='NULL'
        ATIME='NULL'
        ATEMP=DATE()
        WRITE(ADATE(1:8),'(A8)')ATEMP
        ITEMP=TIME()
        WRITE(ATIME(1:8),'(A8)')ITEMP
!
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
!
        CURRTIME(1:8)=ATIME(1:8)
        NCURRTIM=8
        CURRDATE(1:8)=ADATE(1:8)
        NCURRDAT=8
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!               ********************************
!               **  STEP 2D--                 **
!               **  VAX/VMS                   **
!               **  USE LIB$DATE_TIME         **
!               ********************************
!       IMPLEMENTORS NOTE.  THIS CODE HAS NOT BEEN TESTED!!!
!       IT LOOKS RIGHT ACCORDING TO VAX FORTRAN MANUAL, BUT IT
!       MAY NEED TO BE DEBUGGED.
!
#ifdef VAX_VMS
        ADATE='NULL'
        ATIME='NULL'
        ISTATUS=LIB$DATE_TIME(DATETIME)
        ISTATUS PROBABLY NEEDS TO BE DECLARED ABOVE
        ADATE(1:11)=DATETIME(1:11)
        ATIME(1:8)=DATETIME(13:20)
!
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
!
        CURRTIME(1:8)=ATIME(1:8)
        NCURRTIM=8
        CURRDATE(1:11)=ADATE(1:11)
        NCURRDAT=11
#endif
        GO TO 9000
      ELSE
!
!               ********************************
!               **  STEP 2E--                 **
!               **  UNSUPPORTED SYSTEMS.      **
!               ********************************
!
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
!
!CCCC   CALL XXX(ISTRIN,NCSTRI)
!
        GO TO 9000
      ENDIF
!
!               ****************
!               **  STEP 90-- **
!               **  EXIT.     **
!               ****************
!
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
!
      RETURN
      END SUBROUTINE DPTIME
      SUBROUTINE DPEDIT(ICOM,IANSLC,IWIDTH,ISUBRO,IERROR)
!
!     PURPOSE--EDIT A FILE
!     ORIGINAL VERSION--JULY       1992
!
      INCLUDE 'DPCOPA.INC'
!
      CHARACTER*4 ICOM
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*4 ISOURC
!CCCC CHARACTER*80 IEDINA
      CHARACTER (LEN=MAXFNC) :: IEDINA
!CCCC THE FOLLOWING LINE WAS ADDED    JULY 1993
      CHARACTER*4 IDATAP
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
        DO 55 I=1,IWIDTH
          WRITE(ICOUT,57)I,IANSLC(I)
   57     FORMAT('I,IANSLC(I) = ',I6,2X,A4)
          CALL DPWRST('XXX','BUG ')
   55   CONTINUE
      ENDIF
!
      IERROR='NO'
      IF(ICOM.EQ.'FED')THEN
        ISTART=5
      ELSEIF(ICOM.EQ.'EDIT')THEN
        ISTART=6
      ELSE
        ISTART=6
      ENDIF
!
      IEDINA=' '
      J=0
      IF(ISTART.LE.IWIDTH)THEN
         DO 1000 I=ISTART,IWIDTH
            J=J+1
            IEDINA(J:J)=IANSLC(I)(1:1)
 1000    CONTINUE
      ENDIF
      NCSTR=J
!
      IF(NCSTR.LT.1)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1001)
 1001   FORMAT('NO FILE NAME GIVEN ON EDIT COMMAND')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
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
!
!     ISOURC IS THE SOURCE OF THE NAME OF THE FILE TO BE EDITED.
!     IF THE FILE NAME IS IN C:\FED\FEDARG.TEX,
!     THEN ISOURC = 'FILE'
!     IF THE FILE NAME IS PASSED ON VIA A SUBROUTINE ARGUMENT,
!     THEN ISOURC = 'SUBR'
!
      ISOURC='SUBR'
!
!     IDATAP IS THE DATAPLOT-CONNECTION SWITCH.
!     IF FED IS CONNECTED TO DATAPLOT,
!     THEN IDATAP = 'ON'
!     IF FED IS STAND-ALONE,
!     THEN IDATAP = 'OFF'
!
!CCCC THE FOLLOWING LINE WAS ADDED    JULY 1993
      IDATAP='ON'
!
!CCCC THE FOLLOWING LINE WAS CHANGED    JULY 1993
!CCCC CALL EDMAI2(ISOURC,IEDINA)
      CALL EDMAI2(ISOURC,IEDINA,IDATAP)
!
      IF(ISUBRO.EQ.'EDIT')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9051)
 9051   FORMAT('***** AT THE END       OF DPEDIT--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
 9000 CONTINUE
      RETURN
      END SUBROUTINE DPEDIT
      SUBROUTINE DPWRST(ISUBN0,TYPE)
!
!     PURPOSE--WRITE OUT THE NCOUT ELEMENTS OF THE
!              CHARACTER*240 STRING ICOUT(.:.)
!              TO A GENERAL GRAPHICS DEVICE.
!              THE VALUE OF THE VARIABLE    NCOUT
!     ICOUT AND NCOUT RESIDE IN COMMON   /TEXTOU/
!     INPUT ARGUMENTS--ICOUT (IN COMMON)
!     ISUBN0 = 6-CHARACTER NAME OF SUBROUTINE WHICH CALLED DPWRST.
!              (AND THEREBY HAVE WALKBACK INFORMATION).
!     TYPE--4 CHARACTER DEFINITION OF TYPE OF INPUT
!              1) TEXT
!              2) BUG
!              3) ERRO
!              4) LIST
!              5) HELP
!              6) WRIT (= ALWAYS WRITE EVEN IF FEEDBACK OFF)
!              7) ...
!     OUTPUT ARGUMENTS--NCOUT (DETERMINED HEREIN)
!     NOTE--ALL DATAPLOT TEXT OUTPUT IS FUNNELED THROUGH
!           THIS ONE SUBROUTINE.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 2089
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--93.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1993.
!     UPDATED            --SEPTEMBER 1993. ALWAYS WRITE IF TYPE = WRIT
!     UPDATED            --SEPTEMBER 1993. OMIT IBUGG4 AS BUG SWITCH
!     UPDATED            --FEBRUARY  2005. FOR RTF OUTPUT, SUPPRESS
!                                          LEADING SPACE.  NEED TO
!                                          ADD DPCOSU.INC.
!     UPDATED            --JANUARY   2006. ALLOW CAPTURE OUTPUT TO
!                                          BE OPTIONALLY WRITTEN TO
!                                          BOTH SCREEN AND CAPTURE
!                                          FILE
!     UPDATED            --JUNE      2006. MAKE LEADING SPACE
!                                          USER SETTABLE (SET
!                                          FORTRAN FORMAT CONTROL)
!     UPDATED            --APRIL     2009. SET MAXIMUM LINE WIDTH IN
!                                          MAXCLN (IF SET ABOVE 240,
!                                          THEN NEED TO CHANGE
!                                          "CHARACTER*240 ICOUT" IN
!                                          ALL ROUTINES
!     UPDATED            --SEPTEMBER 2010. ALLOW PROMPT AFTER USER
!                                          SPECIFIED NUMBER OF LINES
!     UPDATED            --DECEMBER  2015. SUPPORT CAPTURE SPLIT OPTION
!                                          (SECOND CAPTURE FILE)
!     UPDATED            --AUGUST    2016. TRAP ERROR ON WRITE
!     UPDATED            --SEPTEMBER 2018. SET WRITE FEEDBACK
!     UPDATED            --APRIL     2020. INCREASE MAXIMUM LINE
!                                          LENGTH (MAKE SETTABLE IN
!                                          DPCOP2.INC)
!     UPDATED            --JANUARY   2021. FOR TYPE "ERRO", DON'T PRINT
!                                          IF "SET ERROR MESSAGE OFF"
!                                          COMMAND GIVEN
!     UPDATED            --APRIL     2021. ISSUE WITH
!
!                                             CAPTURE SCREEN ON
!                                             WRITE <file> ...
!
!                                          CHECK IF "WRITE" COMMAND IS
!                                          THE ISSUING COMMAND
!
!-----NON-COMMON VARIABLES (GRAPHICS)----------------------------------
!
!CCCC MUST EVENTUALLY CHANGE THE FOLLOWING LINE FORM *3 TO *?
      CHARACTER (LEN=*) :: ISUBN0
      CHARACTER (LEN=4) :: TYPE
!
      CHARACTER*4 IBRANC
      CHARACTER*1 IBASLC
      CHARACTER*1 IJUNK
      CHARACTER*2 ISTR
      CHARACTER*20 IFORMT
!
      CHARACTER*4 IRTFMD
      COMMON/COMRTF/IRTFMD
!
      INTEGER ILNCNT
      COMMON/LINNUM/ILNCNT
!
      CHARACTER*4 IWRIFC
      COMMON/WRIFIL/IWRIFC
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOTR.INC'
      INCLUDE 'DPCOSU.INC'
      INCLUDE 'DPCOST.INC'
      INCLUDE 'DPCOF2.INC'
      INCLUDE 'DPCOHK.INC'
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
      MAXCLN=MAXOUT
!     MAY,1988.
!CCCC NCOUT=ABS(NCOUT)  JJF
!
!CCCC THE FOLLOWING LINE WAS CHANGED    SEPTEMBER 1993
!CCCC IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
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
!
      IF(TYPE.EQ.'ERRO' .AND. IERMSG.EQ.'OFF')GO TO 9000
!
!               *********************************************
!               **  STEP 11--                              **
!               **  IF CALLED FOR,                         **
!               **  CARRY OUT ANY SUB-STRING TRANSLATIONS  **
!               *********************************************
!
!CCCC IF(NUMTRA.GE.1)THEN
!CCCC   CALL GRTRST(ICOUT,NCOUT,ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,
!CCCC1              IBUGG4,ISUBG4,IERRG4)
!
!               **************************************************
!               **  STEP 12--                                   **
!               **  DETERMINE THE LENGTH OF THE STRING          **
!               **  (BY IGNORING BLANK CHARACTERS AT THE END)   **
!               **************************************************
!
      NCOUT=1
      DO 1200 I=MAXCLN,1,-1
        IF(ICOUT(I:I).NE.' ')THEN
          NCOUT=I
          GO TO 1290
        ENDIF
 1200 CONTINUE
 1290 CONTINUE
!
!               ****************************
!               **  STEP 13--             **
!               **  WRITE OUT THE STRING  **
!               ****************************
!
!CCCC IOUNIT=6
!CCCC NOTE--IPR BELOW IS USUALLY 6
!CCCC       BUT COULD BE SET TO 7 IN TCSHME.FOR WITHIN TCDRIV.FOR
      IOUNIT=IPR
      IFORMT=' '
!
      IBRANC='NOWR'
!CCCC THE FOLLOWING LINE WAS CHANGED     SEPTEMBER 1993
!CCCC TO ALLOW ALWAYS-WRITING FOR L, WRITE, HELP, STAT SEPTEMBER 1993
!CCCC APRIL 2009: MODIFY ORDER (I.E., DO SPECIAL CASES FIRST)
!CCCC IF(IFEEDB.EQ.'ON')THEN
      IF(IFEEDB.EQ.'ON'.OR.TYPE.EQ.'WRIT'.OR.IFEEWR.EQ.'ON')THEN
         IF(1.LE.NCOUT.AND.NCOUT.LE.MAXCLN)THEN
            IBRANC='WRIT'
            IF(ICAPTY.EQ.'RTF ')THEN
               IFORMT='(    A1)'
               WRITE(IFORMT(2:5),'(I4)')NCOUT
               CALL DPCONA(92,IBASLC)
               WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)   &
                     (ICOUT(I:I),I=1,NCOUT)
               IF(IRTFMD.EQ.'VERB')THEN
                 WRITE(IOUNIT,1319,IOSTAT=IOS,ERR=8800)IBASLC
 1319            FORMAT(A1,'line')
               ENDIF
            ELSEIF(ICAPSC.EQ.'ON  ' .AND. IOUNIT.NE.6 .AND.   &
                   ICAPTY.NE.'HTML' .AND. ICAPTY.NE.'LATE')THEN
               IFORMT='(1X,    A1)'
               WRITE(IFORMT(5:8),'(I4)')NCOUT
               WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)   &
                     (ICOUT(I:I),I=1,NCOUT)
!
!              2021/04: CHECK FOR WRITE (OR PRINT) TO A FILE
!                       CASE.
!
               IFLAGT=1
               IF(ICOM.EQ.'WRIT' .OR.   &
                 (ICOM.EQ.'PRIN' .AND. ICOM2.EQ.'T   '))THEN
                 IF(IWRIFC.EQ.'FILE')IFLAGT=0
               ENDIF
!
               IF(IFLAGT.EQ.1)THEN
                 WRITE(6,IFORMT)(ICOUT(I:I),I=1,NCOUT)
               ENDIF
               IF(ICAPSP.EQ.'ON')THEN
                 WRITE(ICPNU2,IFORMT)(ICOUT(I:I),I=1,NCOUT)
               ENDIF
#ifdef VAX_VMS
            ELSEIF((IHOST1.EQ.'VAX'.AND.ICOUT(1:1).EQ.'$') .OR.   &
              (IFORFM.EQ.'OFF'))THEN
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
                 WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)   &
                       ILNCNT,ISTR,(ICOUT(I:I),I=1,NCOUT)
               ELSE
                 WRITE(IOUNIT,1311,IOSTAT=IOS,ERR=8800)   &
                       (ICOUT(I:I),I=1,NCOUT)
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
                 WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)   &
                       ILNCNT,ISTR,(ICOUT(I:I),I=1,NCOUT)
               ELSE
                 IFORMT='(1X,    A1)'
                 WRITE(IFORMT(5:8),'(I4)')NCOUT
                 WRITE(IOUNIT,IFORMT,IOSTAT=IOS,ERR=8800)   &
                       (ICOUT(I:I),I=1,NCOUT)
!1312            FORMAT(1X,240A1)
              ENDIF
            ENDIF
         ENDIF
      ENDIF
!
!               ******************************
!               **  STEP 14--               **
!               **  RESET STRING VARIABLES  **
!               ******************************
!
      ICOUT=' '
      NCOUT=(-999)
      ILOUT=(-999)
      GO TO 9000
!
 8800 CONTINUE
      WRITE(6,8801)IOUNIT
 8801 FORMAT('****** ERROR TRYING TO WRITE TO UNIT ',I8)
      WRITE(6,8802)IOS
 8802 FORMAT('       STATUS NUMBER = ',I8)
      WRITE(6,8803)
 8803 FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',   &
             'THAT DOES NOT HAVE WRITE PERMISSION.')
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
!
!     CHECK IF PROMPT REQUIRED
!
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
!
!CCCC THE FOLLOWING LINE WAS CHANGED    SEPTEMBER 1993
!CCCC IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
      IF(ISUBG4.EQ.'WRST')THEN
         WRITE(IPR,999)
         WRITE(IPR,9011)
 9011    FORMAT('***** AT THE END       OF DPWRST--')
         IF(NCOUT.GT.0)THEN
           DO 9035 I=1,NCOUT
             CALL DPCOAN(ICOUT(I:I),IASCNE)
             WRITE(IPR,9036)I,ICOUT(I:I),IASCNE
 9036        FORMAT(1H ,'I,ICOUT(I:I),IASCNE = ',I8,2X,A1,I8)
 9035      CONTINUE
         ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPWRST
      SUBROUTINE GRWRST(ICSTR,NCSTR2,ISUBN0)
!CCCC SUBROUTINE GRWRST(ICSTR,NCSTR,ISUBN0)
!
!     PURPOSE--WRITE OUT THE NCSTR ELEMENTS OF THE
!              CHARACTER*130 STRING ICSTR(.:.)
!              OUT TO A GENERAL GRAPHICS DEVICE.
!              THE VALUE OF THE VARIABLE    NCSTR
!              IS THE NUMBER OF ELEMENTS IN ICSTR(.:.)
!              TO BE WRITTEN OUT.
!     NOTE--ISUBN0 = NAME OF SUBROUTINE WHICH CALLED GRWRST.
!                    (AND THEREBY HAVE WALKBACK INFORMATION).
!
!     WRITTEN BY--JAMES J. FILLIBEN
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-921-3651
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!     VERSION NUMBER--83.6
!     ORIGINAL VERSION (AS A SEPARATE SUBROUTINE)--MAY       1983.
!     UPDATED         --MAY 1988.
!                       THE POSTSCRIPT DEVICE REQUIRES A "%!" TO BE FIRST
!                       16 BYTES, QUIC REQUIRES "^PY.." COMMAND TO START
!                       IN COLUMN 1.  ALSO, SOME DEVICES SUCH AS DICOMED,
!                       SHOUULD NOT CONTAIN LEADING SPACES IN THE FILE.
!                       FOR THESE CASE, SEND "NCSTR" AS NEGATIVE.  IF NCSTR
!                       IS NEGATIVE, THE LEADING SPACE FOR PRINT CONTROL
!                       WILL NOT BE ADDED.
!     UPDATED         --JANUARY  1989. SUN (BY BILL ANDERSON)
!     UPDATED         --JANUARY  1989. POSTSCRIPT (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. CGM (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. QMS QUIC (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. CALCOMP (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1989. ZETA (BY ALAN HECKERT)
!     UPDATED         --JANUARY  1994. ALPHA: 1X IN FORMAT (JJF)
!     UPDATED         --AUGUST   2016. TRAP WRITE ERRORS
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
      CHARACTER*130 ICSTR
!
      CHARACTER*4 ISUBN0
!
      CHARACTER*4 IBRANC
!
      SAVE NUMERR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOGR.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOTR.INC'
!
      COMMON/GRAERR/IGFLAG
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
      DATA NUMERR /0/
!
!-----START POINT-----------------------------------------------------
!
      IERRG4='NO'
!  MAY,1988.
      NCSTR=ABS(NCSTR2)
      IGFLAG=0
!
      IF(IBUGG4.EQ.'ON'.OR.ISUBG4.EQ.'WRST')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('***** AT THE BEGINNING OF GRWRST--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,52)ISUBN0,IBUGG3,ISUBG4,IERRG4
   52   FORMAT('ISUBN0 (CALLING SUBROUTINE),IBUGG3,ISUBG4,IERRG4 = ',   &
               3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,53)IGUNIT,IMANUF,NCSTR,NUMTRA
   53   FORMAT('IGUNIT,IMANUF,NCSTR,NUMTRA = ',I8,2X,A4,2X,2I8)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO 55 I=1,NCSTR
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,56)I,ICSTR(I:I),IASCNE
   56       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
   55     CONTINUE
        ENDIF
        IF(NUMTRA.GT.0)THEN
          DO 62 I=1,NUMTRA
            WRITE(ICOUT,63)I,NCTRA1(I),ICTRA1(I),NCTRA2(I),ICTRA2(I)
   63       FORMAT('I,NCTRA1(I),ICTRA1(I),NCTRA2(I),ICTRA2(I) = ',   &
                   I8,I8,2X,A30,I8,2X,A30)
            CALL DPWRST('XXX','BUG ')
   62     CONTINUE
        ENDIF
      ENDIF
!
!               *********************************************
!               **  STEP 11--                              **
!               **  IF CALLED FOR,                         **
!               **  CARRY OUT ANY SUB-STRING TRANSLATIONS  **
!               *********************************************
!
      IF(NUMTRA.GE.1)   &
      CALL GRTRST(ICSTR,NCSTR,   &
      ICTRA1,NCTRA1,ICTRA2,NCTRA2,NUMTRA,   &
      IBUGG4,ISUBG4,IERRG4)
!
!               ****************************
!               **  STEP 21--             **
!               **  WRITE OUT THE STRING  **
!               ****************************
!
      IBRANC='NOWR'
      IF(1.LE.NCSTR.AND.NCSTR.LE.130)THEN
        IBRANC='WRIT'
        IF((IHOST1.EQ.'VAX'.AND.ICSTR(1:1).EQ.'$') .OR.   &
           NCSTR2.LT.0)THEN
          WRITE(IGUNIT,2111,IOSTAT=IOS,ERR=8800)(ICSTR(I:I),I=1,NCSTR)
 2111     FORMAT(240A1)
!CCCC     NOTE--THE FOLLOWING FORMAT SHOULD BE USED  JANUARY 1994
!CCCC           INSTEAD ON SOME COMPUTERS            JANUARY 1994
!CCCC           (E.G., DEC ALPHA COMPUTERS)          JANUARY 1994
!CCCC           WHICH NEED A LEADING SPACE BEFORE    JANUARY 1994
!CCCC           ALL FORTRAN WRITE STATEMENTS--EVEN   JANUARY 1994
!CCCC           WRITE STATEMENTS WITH GRAPHICS       JANUARY 1994
!CCCC           DIRECTIVES.    JJF                   JANUARY 1994
!2111           FORMAT(1X,240A1)
        ELSE
          WRITE(IGUNIT,2121,IOSTAT=IOS,ERR=8800)(ICSTR(I:I),I=1,NCSTR)
 2121     FORMAT(240A1)
!CCCC     NOTE--THE FOLLOWING FORMAT SHOULD BE USED  JANUARY 1994
!CCCC           INSTEAD ON SOME COMPUTERS            JANUARY 1994
!CCCC           (E.G., DEC ALPHA COMPUTERS)          JANUARY 1994
!CCCC           WHICH NEED A LEADING SPACE BEFORE    JANUARY 1994
!CCCC           ALL FORTRAN WRITE STATEMENTS--EVEN   JANUARY 1994
!CCCC           WRITE STATEMENTS WITH GRAPHICS       JANUARY 1994
!CCCC           DIRECTIVES.    JJF                   JANUARY 1994
!2121           FORMAT(1X,240A1)
        ENDIF
      ENDIF
!
!               *******************************************
!               **  STEP 31--                            **
!               **  IF CALLED FOR,                       **
!               **  CALL THE LINE TRANSLATOR SUBROUTINE  **
!               **  WHICH CONVERTS A TEKTRONIX LINE      **
!               **  INTO A SET OF CALLS FOR              **
!               **  ANOTHER GRAPHICS DEVICE              **
!               **  (SEE SUBROUTINE GRTRTK).             **
!               *******************************************
!
!CCCC IF(ITRANS.EQ.'ON')CALL GRTRTK(ICSTR,NCSTR)
!
      GO TO 9000
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
 8803   FORMAT('       LIKELY CAUSE: TRYING TO WRITE TO A FILE ',   &
               'THAT DOES NOT HAVE WRITE PERMISSION.')
        CALL DPWRST('XXX','BUG ')
        IGFLAG=1
      ENDIF
      GO TO 9000
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
          DO 9015 I=1,NCSTR
            CALL DPCOAN(ICSTR(I:I),IASCNE)
            WRITE(ICOUT,9016)I,ICSTR(I:I),IASCNE
 9016       FORMAT('I,ICSTR(I:I),IASCNE = ',I8,2X,A1,I8)
            CALL DPWRST('XXX','BUG ')
 9015     CONTINUE
        ENDIF
        IF(NUMTRA.GT.0)THEN
          DO 9022 I=1,NUMTRA
            WRITE(ICOUT,9023)I,ICTRA1(I),NCTRA1(I),NCTRA2(I),ICTRA2(I)
 9023       FORMAT('I,ICTRA1(I),NCTRA1(I),NCTRA2(I),ICTRA2(I) = ',   &
                   I8,2X,A30,I8,2X,A30,I8)
            CALL DPWRST('XXX','BUG ')
 9022     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE GRWRST
      SUBROUTINE DPSLEE(IHARG,IHARG2,IARGT,IARG,ARG,NUMARG,   &
                        IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CAUSE DATAPLOT TO PAUSE FOR <X> SECONDS.  THIS COMMAND
!              IS SITE AND HOST DEPENDENT.  THE MAIN USAGE IS TO ALLOW
!              DELAY TO BE INSERTED IN MACROS AFTER A PLOT TO AVOID
!              THE HASSLE OF ENTERING A CARRIAGE RETURN AS NEEDED BY
!              THE PAUSE COMMAND.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 CENTER FOR APPLIED MATHEMATICS
!                 NATIONAL BUREAU OF STANDARDS
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--97.8
!     ORIGINAL VERSION--AUGUST     1997.
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
#ifdef INTEL
      USE MSFLIB
#endif
!
      CHARACTER*4 IHARG
      CHARACTER*4 IHARG2
      CHARACTER*4 IARGT
!
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      DIMENSION IHARG(*)
      DIMENSION IHARG2(*)
      DIMENSION IARG(*)
      DIMENSION ARG(*)
      DIMENSION IARGT(*)
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IFOUND='YES'
      IERROR='NO'
!
      J2=0
!
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
   55   FORMAT('IHARG(1),IHARG2(1),IARGT(1),IARG(1),ARG(1) = ',   &
               2A4,2X,A4,2X,I8,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  DETERMINE THE TIME VARIABLE                    **
!               *****************************************************
!
      IF(NUMARG.LE.0)THEN
        ASLEEP=5.0
      ELSE
        IF(IARGT(1).EQ.'NUMB')THEN
          ASLEEP=ARG(1)
        ELSE
          ASLEEP=5.0
        ENDIF
      ENDIF
!
!               ********************************
!               **  STEP 2--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       ****************************************************
!       *  IBM/PC 386 - MICROSOFT WINDOWS 95/NT COMPILER   *
!       ****************************************************
!
#ifdef INTEL
        CALL SLEEPQQ(INT(ASLEEP*1000.))
#endif
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!       *********************************************************
!       *  UNIX    - LEFT TO IMPLEMENTOR                        *
!       *  CODE ADDED MARCH, 1990 BY ALAN HECKERT.  USE THE     *
!       *  LIBRARY ROUTINE "SLEEem".  NOTE THAT UNIX CALLS ARE  *
!       *  CASE SENSITIVE, SO LEAVE CODE IN LOWER CASE.         *
!       *********************************************************
!
#ifdef LINUX
        CALL SLEEP(INT(ASLEEP+0.5))
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!       *********************************************************
!       *  CDC - NOS/VE LEFT TO IMPLEMENTOR                     *
!       *********************************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2111)
 2111   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2112)
 2112   FORMAT('      COMMAND NOT IMPLEMENTED FOR NOS/VE')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!       **************************************
!       *  VAX/VMS - LEFT TO IMPLEMENTOR     *
!       **************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2211)
 2211   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)
 2212   FORMAT('      COMMAND NOT IMPLEMENTED FOR VAX/VMS')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG ')THEN
!
!       **********************************
!       *  IBM/PC 386 - OTG COMPILER     *
!       **********************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
 2411   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      COMMAND NOT IMPLEMENTED FOR IBM OTG VERSION')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSE
!
!     *********************************************************
!     *  OTHER   - LEFT TO IMPLEMENTOR                        *
!     *********************************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      THE SLEEP COMMAND HAS NOT YET BEEN IMPLEMENTED ',   &
               ' FOR THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8014)
 8014   FORMAT('      COMPUTER/MODEL/OP-SYS/COMPILER/SITE')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8021)IHOST1,IHMOD1,IOPSY1,ICOMPI,ISITE
 8021   FORMAT('IHOST1,IHMOD1,IOPSY1,ICOMPI,ISITE  = ',4(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPSLEE
      SUBROUTINE DPSLE2(ASLEEP,IBUGD2,ISUBRO,IERROR)
!
!     PURPOSE--CAUSE DATAPLOT TO PAUSE FOR <X> SECONDS.  THIS COMMAND
!              IS SITE AND HOST DEPENDENT.  THE MAIN USAGE IS TO ALLOW
!              DELAY TO BE INSERTED IN MACROS AFTER A PLOT TO AVOID
!              THE HASSLE OF ENTERING A CARRIAGE RETURN AS NEEDED BY
!              THE PAUSE COMMAND.
!
!              THIS IS A MODIFIED VERSION OF THE DPSLEE ROUTINE.  THE
!              DPSLEE ROUTINE IMPLEMENTS THE "SLEEP" COMMAND.  THIS
!              ROUTINE IS USED TO IMPLEMENT A SLEEP FUNCTION INDEPENDENT
!              OF THE SLEEP COMMAND (I.E., USED INTERNALLY BY DATAPLOT).
!              FOR THIS ROUTINE, THE SLEEP PARAMETER HAS ALREADY BEEN
!              DETERMINED.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 WASHINGTON, D. C. 20234
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.11
!     ORIGINAL VERSION--NOVEMBER   2014.
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
#ifdef INTEL
      USE MSFLIB
#endif
!
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
!               ********************************
!               **  STEP 2--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       ****************************************************
!       *  IBM/PC 386 - MICROSOFT WINDOWS 95/NT COMPILER   *
!       ****************************************************
!
#ifdef INTEL
        CALL SLEEPQQ(INT(ASLEEP*1000.))
#endif
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!       *********************************************************
!       *  UNIX    - LEFT TO IMPLEMENTOR                        *
!       *  CODE ADDED MARCH, 1990 BY ALAN HECKERT.  USE THE     *
!       *  LIBRARY ROUTINE "SLEEem".  NOTE THAT UNIX CALLS ARE  *
!       *  CASE SENSITIVE, SO LEAVE CODE IN LOWER CASE.         *
!       *********************************************************
!
#ifdef LINUX
        CALL SLEEP(INT(ASLEEP+0.5))
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!       *********************************************************
!       *  CDC - NOS/VE LEFT TO IMPLEMENTOR                     *
!       *********************************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2111)
 2111   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2112)
 2112   FORMAT('      COMMAND NOT IMPLEMENTED FOR NOS/VE')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!       **************************************
!       *  VAX/VMS - LEFT TO IMPLEMENTOR     *
!       **************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2211)
 2211   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)
 2212   FORMAT('      COMMAND NOT IMPLEMENTED FOR VAX/VMS')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG ')THEN
!
!       **********************************
!       *  IBM/PC 386 - OTG COMPILER     *
!       **********************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
 2411   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      COMMAND NOT IMPLEMENTED FOR IBM OTG VERSION')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSE
!
!     *********************************************************
!     *  OTHER   - LEFT TO IMPLEMENTOR                        *
!     *********************************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8011)
 8011   FORMAT('***** ERROR IN DPSLEE--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8013)
 8013   FORMAT('      THE SLEEP COMMAND HAS NOT YET BEEN IMPLEMENTED ',   &
               ' FOR THIS')
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
        GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPSLE2
      SUBROUTINE DPFLSH(IUNIT,IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--ENTER A "FLUSH" COMMAND TO CLEAR STANDARD OUTPUT.
!              NEEDED BY FRONT-END TO GET RID OF EXPECT CODE.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION AND TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--98.1
!     ORIGINAL VERSION--JANUARY    1998.
!
!-----NON-COMMON VARIABLES (GRAPHICS)-------------------------------------------
!
#ifdef INTEL
      USE PORTLIB
#endif
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IFOUND='NO'
      IERROR='NO'
!
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
!
!               ********************************
!               **  STEP 2--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       *********************************************
!       *  IBM/PC 386 - MS-FORTRAN COMPILER         *
!       *********************************************
!
#ifdef INTEL
        CALL FLUSH(IUNIT)
#endif
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!       ************************************
!       *  UNIX/LINUX - gfortran compiler  *
!       ************************************
!
#ifdef LINUX
        CALL FLUSH(IUNIT)
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!       *************************************
!       *  CDC - NOS/VE OPERATING SYSTEM.  **
!       *************************************
!
        GO TO 9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!       **************************************
!       *  VAX/VMS - LEFT TO IMPLEMENTOR     *
!       **************************************
!
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.   &
            (IOPSY1.EQ.'OS38' .OR. ICOMPI.EQ.'OTG'))THEN
!
!       **********************************
!       *  IBM/PC 386 - OTG COMPILER     *
!       **********************************
!
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'LAHE')THEN
!
!       *********************************************
!       *  IBM/PC 386 - LAHEY      COMPILER         *
!       *********************************************
!
#ifdef WIN32_LAHEY
        CALL FLUSH(IUNIT)
#endif
        GO TO 9000
      ELSE
!
!
!       **************************************
!       *  OTHER   - LEFT TO IMPLEMENTOR     *
!       **************************************
!
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
        GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPFLSH
      REAL FUNCTION RANLP(IRESET)
!CCCC REAL FUNCTION RANLP(IDUM,IRESET)
!
      INTEGER TABPTJ
      INTEGER ITABLE(98)
      INTEGER ITABSV(98)
!   LEWIS-PAYNE GFSR UNIFORM RANDOM NUMBER GENERATOR
!
!  T. G. LEWIS & W. H. PAYNE (1973) GENERALIZED FEEDBACK SHIFT REGISTER
!   PSEUDORANDOM NUMBERS, JOURNAL OF THE ACM, VOLUME 20, PP. 456-468
!
!  USES PRIMITIVE TRINOMIAL WITH P=98 AND Q=27
!
!  ARGUMENT IS A DUMMY AND NEVER USED
!
!  THE USE OF THE "IEOR" FUNCTION IS NOT STANDARD IN THE
!  FORTRAN 77 (IS STANDARD FOR FORTRAN 90), SO PLACE IN DP1.FOR
!  FILE IN CASE IT NEEDS MODIFICATION.
!
!  THIS IS FORTRAN 77 IMPLEMENTATION OF A FORTRAN 95 CODE
!  FOUND IN MONAHAN (2001), "NUMERICAL METHODS OF STATISTICS",
!  CAMBRIDGE UNIVERSITY PRESS.
!
!CCCC INTEGER IDUM
!  PARAMETERS OF TAUSWORTHE SEQUENCE
      INTEGER P
      INTEGER Q
      INTEGER K
      REAL    FN
!
      SAVE TABPTJ
!CCCC SAVE TABPSV
      SAVE ITABLE
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
      DATA P /98/
      DATA Q /27/
      DATA TABPTJ /0/
!  FN = 2**31
      DATA FN / 2147483648. /
!
      DATA ITABLE /346256726,591599773,1943131421,1173234223,   &
       1776849374,1119416586,172236044,985756773,1554281477,   &
       1503137291,650397619,1618395655,639939067,1448259547,   &
       1046853128,659170036,1034934222,279813371,326930100,   &
       367002640,648480182,1909733845,618563844,845531267,   &
       292262469,299413367,2139821356,1005803337,390139420,   &
       1161028423,2034360736,334070487,565633315,124796253,   &
       2104169336,2009751844,1999687407,83223028,1591328966,   &
       646701838,1935362333,795013136,680356918,1771711842,   &
       1324935502,1869840308,356745634,1061920662,614951490,   &
       261876461,703987800,797463948,178239686,1641708282,   &
       1539695556,1334926802,940547749,1957646566,1878491364,   &
       2033904942,1711106005,2138438575,647734238,1555990485,   &
       1210108489,1793192836,1819829578,751843064,345621400,   &
       575445974,1640918761,1379191461,1617832156,542966103,   &
       1305854952,1476721677,1466811698,1842260101,1666639833,   &
       217007402,685228354,902087789,32432242,789712994,702791444,   &
       1081111755,1572116899,321512624,644413114,863989644,   &
       1348681739,84379947,1955819746,941474606,984690559,   &
       1794209263,1704575856,1253913135 /
!
      DATA ITABSV /346256726,591599773,1943131421,1173234223,   &
       1776849374,1119416586,172236044,985756773,1554281477,   &
       1503137291,650397619,1618395655,639939067,1448259547,   &
       1046853128,659170036,1034934222,279813371,326930100,   &
       367002640,648480182,1909733845,618563844,845531267,   &
       292262469,299413367,2139821356,1005803337,390139420,   &
       1161028423,2034360736,334070487,565633315,124796253,   &
       2104169336,2009751844,1999687407,83223028,1591328966,   &
       646701838,1935362333,795013136,680356918,1771711842,   &
       1324935502,1869840308,356745634,1061920662,614951490,   &
       261876461,703987800,797463948,178239686,1641708282,   &
       1539695556,1334926802,940547749,1957646566,1878491364,   &
       2033904942,1711106005,2138438575,647734238,1555990485,   &
       1210108489,1793192836,1819829578,751843064,345621400,   &
       575445974,1640918761,1379191461,1617832156,542966103,   &
       1305854952,1476721677,1466811698,1842260101,1666639833,   &
       217007402,685228354,902087789,32432242,789712994,702791444,   &
       1081111755,1572116899,321512624,644413114,863989644,   &
       1348681739,84379947,1955819746,941474606,984690559,   &
       1794209263,1704575856,1253913135 /
!
!     START EXECUTABLE CODE
!
!     RESET INITIAL TABLE IF REQUESTED
!
      IF(IRESET.EQ.1)THEN
        DO 100 LL=1,P
          ITABLE(LL)=ITABSV(LL)
  100   CONTINUE
        TABPTJ = 0
      ENDIF
!
!     UPDATE POINTER
!
      TABPTJ = TABPTJ + 1
      IF(TABPTJ.GT.P) TABPTJ = 1
!
!     UPDATE DELAY POINTER
!
      K = TABPTJ + Q
      IF(K.GT.P) K = K - P
!
!     EXCLUSIVE OR OF TWO TABLE ENTRIES AND REPLACE WITH NEW ONE
!
      ITABLE(TABPTJ) = IEOR( ITABLE(K), ITABLE(TABPTJ) )
!
!     CONVERT BIG INTEGER TO FLOATING POINT NUMBER
!
      RANLP = REAL( ITABLE(TABPTJ) ) / FN
!
      RETURN
      END FUNCTION RANLP
      REAL FUNCTION RANFT(IRESET)
!CCCC REAL FUNCTION RANFT(IDUM,IRESET)
!
      INTEGER TABPTJ
      INTEGER ITABLE(521)
      INTEGER ITABSV(521)
!
!     FUSHIMI-TEZUKA GFSR UNIFORM RANDOM NUMBER GENERATOR
!
!     USES PRIMITIVE TRINOMIAL WITH P=521 AND Q=32 AS USED BY BRIGHT &
!     ENISON AND ARVILLIAS & MARITSAS BUT WITH RANDOM SEED MATRIX
!     FUSHIMI & TEZUKA GIVE RULES FOR TESTING K-DISTRIBUTION OF
!     SEQUENCE -- THE ORIGINAL SEED TABLE HAS BEEN CHECKED AND
!     31 BIT NUMBERS ARE 16-DISTRIBUTED (BEST POSSIBLE)
!
!     M. FUSHIMI & S. TEZUKA (1983) THE K-DISTRIBUTION OF GENERALIZED
!     FEEDBACK SHIFT REGISTER PSEUDORANDOM NUMBERS, COMMUNICATIONS OF
!     THE ACM, VOLUME 26, NUMBER 7, PP. 516-523
!
!     ARGUMENT IS A DUMMY AND NEVER USED
!
!     THE USE OF THE "IEOR" FUNCTION IS NOT STANDARD IN THE
!     FORTRAN 77 (IS STANDARD FOR FORTRAN 90), SO PLACE IN DP1.FOR
!     FILE IN CASE IT NEEDS MODIFICATION.
!
!     THIS IS FORTRAN 77 IMPLEMENTATION OF A FORTRAN 95 CODE
!     FOUND IN MONAHAN (2001), "NUMERICAL METHODS OF STATISTICS",
!     CAMBRIDGE UNIVERSITY PRESS.
!
!CCCC INTEGER IDUM
!  PARAMETERS OF TAUSWORTHE SEQUENCE
      INTEGER P
      INTEGER Q
      INTEGER K
      REAL    FN
!
      SAVE TABPTJ
      SAVE ITABLE
!
      DATA P /521/
      DATA Q /32/
      DATA TABPTJ /0/
!  FN = 2**31
      DATA FN / 2147483648. /
!
      DATA (ITABLE(I),I=1,18)/   &
        1464221660, 1158328647, 1090310074,   &
        363453867, 1125650601, 1626204584,   &
        596067919,  102301378, 1392342446,   &
       2117672210, 1470351739, 1107351344,   &
       1160753706, 1046087394,  142212969,   &
         24070872,  832220068, 561689965/
      DATA (ITABLE(I),I=19,36)/   &
       2132613190, 1327815900, 2099255323,   &
       1175377098, 2008300980, 1514090961,   &
       1793048224,  123482417,  899779517,   &
       14500045, 1036604204, 1819512164,   &
       373807068, 1185724401, 1969247094,   &
       117941294,  111922077, 2026157014/
      DATA (ITABLE(I),I=37,54)/   &
        972743819,  112361322,  818613141,   &
       1650818105, 1958655142, 340146731,   &
        244639603, 1374107263,  581629403,   &
         99815077,  407270832, 970490435,   &
        894442080,  502509560, 1772474916,   &
         92762028, 2125760521, 2119124955/
      DATA (ITABLE(I),I=55,72)/   &
        116833190,  815370972,  846774897,   &
        371565210,   14038994, 1877654635,   &
        469257780, 1255556676,  966738110,   &
        106141568, 1509906366, 182036763,   &
       1475162413,  355970676, 2057194637,   &
       783547359,  710739309, 1091521749/
      DATA (ITABLE(I),I=73,90)/   &
       1400722769, 1231840169, 1795363303,   &
        378309524, 1696574748, 43924770,   &
       1656718469,  194341481, 2122127727,   &
       1192298313,  787836434, 1930262483,   &
       2033580199, 1180162588,  833652824,   &
       1019699940, 1177388520, 1454532182/
      DATA (ITABLE(I),I=91,108)/   &
       1516029073,    7158256, 49724360,   &
       346179837,  711320736, 126147103,   &
        588000532, 1952681477,  872490485,   &
        929239679, 1230203969, 65553667,   &
        101370358,  777074835, 1448694438,   &
       37829780,  149952948, 1260879105/
      DATA (ITABLE(I),I=109,126)/   &
        226489139, 1261936689,  821434251,   &
       1820573641, 1034181831, 1908878446,   &
       1261839389, 1333596798,  474560247,   &
        179806371,  496186068, 720243575,   &
       1915930533, 1674665013, 1174195909,   &
       1483410280, 1538917937, 300722691/
      DATA (ITABLE(I),I=127,144)/   &
       1217246246, 1328435200, 1770412188,   &
       1931714531,  668347171, 1571429187,   &
       1256455103, 1034215170,  321723372,   &
       1988373705, 1603828968, 338728032,   &
         22885627,  239160176, 1623174495,   &
       1208969624, 1809686301, 586768446/
      DATA (ITABLE(I),I=145,162)/   &
        572364898, 1157585773, 1489728638,   &
        357378493, 2096054839, 1071933685,   &
        749129112, 2063846670,  915116346,   &
         82547408, 99850294, 999162951,   &
       1757081564, 1222216251, 1107447002,   &
        620994065,  276726035, 1632374490/
      DATA (ITABLE(I),I=163,180)/   &
       1214463005, 1795143947,  986560526,   &
        401521995,  986551091, 242947950,   &
        859782703, 2097912305,   78110042,   &
        682967577,  335973424, 970829205,   &
        145698529,  623819323,  516197007,   &
       2036646416, 1174464179, 1697256876/
      DATA (ITABLE(I),I=181,198)/   &
        771031831,  815657619, 1369483732,   &
        183355178,   11443201, 1199834624,   &
        749080238, 1242421352, 1392163283,   &
       1253963316, 2104424001, 2146002364,   &
        873880383,  666935248, 1463559443,   &
        765865763, 2036382270, 1029929651/
      DATA (ITABLE(I),I=199,216)/   &
       1309449537,  505953903, 1679489248,   &
        650734968, 1915876652, 769087046,   &
        341910829, 1976547278,  405565903,   &
        233036143, 1775766920, 1734382081,   &
       1964094636, 1567409215,  264778756,   &
        550435508, 1957515327, 510628849/
      DATA (ITABLE(I),I=217,234)/   &
        794411731,  772129518, 2084613852,   &
        2056793406,  482508883, 628545509,   &
        492310170,    2535299, 1808581000,   &
       1337327362,  897123632, 474197437,   &
        524509642,   13182159,  361730672,   &
       81199647, 1070351284, 2071002916/
      DATA (ITABLE(I),I=235,252)/   &
        931058636, 1736643210, 1312184093,   &
       1368480008,  493635086, 795562041,   &
        778036865,  437663472,  678482929,   &
        124422133, 1661200800, 366950953,   &
       1919116534, 1534692645,  153200398,   &
          6196433, 1064234375, 195844762/
      DATA (ITABLE(I),I=253,270)/   &
       1617967730, 1745699796, 1054886058,   &
       1992470821, 1744580876, 1576550441,   &
       1430025201, 1944059630, 1993995952,   &
       1607653829,  198657449, 1646157905,   &
        944085034, 1627982402,  411083987,   &
        633677110,  839782297, 958537595/
      DATA (ITABLE(I),I=271,288)/   &
       1866523018,  211248150,  657188559,   &
        859714592,  953170728, 1859902523,   &
        609738329,   80132019,  306596664,   &
       1156862695, 74374927, 183915535,   &
        839428712, 1458285441,  172543676,   &
        838639082, 1071875913, 1925638755/
      DATA (ITABLE(I),I=289,306)/   &
       1631994995, 1278741481, 1939215638,   &
         37917347, 1621691517, 2054362142,   &
        422444128,  437522314,  453524070,   &
        959581287,   80501639, 76349063,   &
       1150964582, 1876521145,  746044173,   &
       1754884425,  802123077, 1527702920/
      DATA (ITABLE(I),I=307,324)/   &
        788492908,   78719119,  182306481,   &
       1713345545,  590352192, 674841804,   &
       1205060021,  525498090, 1593642166,   &
        927838578, 1304219579, 652879324,   &
       1448845945,  436724282, 2073385775,   &
        177580556, 1741619009, 1188575653/
      DATA (ITABLE(I),I=325,342)/   &
        498115577,  937246633,  511610086,   &
        106192814,  223714241, 1868866237,   &
        939024237,  323029456,  317407376,   &
       316389284,  385186216, 1309020254,   &
       1880929110, 1816267930, 1682541052,   &
        402797268,  945227932, 1509316265/
      DATA (ITABLE(I),I=343,360)/   &
       1001627491,  228932404, 1523702251,   &
        121242082, 1901174818, 635982413,   &
        930304172, 1941268644,  183050837,   &
       1338834955,  465435419, 1437644759,   &
       1156952116, 1577273674,  700500350,   &
        804029596, 1358313048, 1416230126/
      DATA (ITABLE(I),I=361,378)/   &
       2018467981,  592185008, 1414209258,   &
        265994210, 1651218063, 90814660,   &
       1608601250, 1089576667,  921984300,   &
       1695616995, 1126839275, 129412032,   &
       1774571060,  962915884,  290498596,   &
       1179573341, 1667596730, 489164113/
      DATA (ITABLE(I),I=379,396)/   &
        813846475,  994357582,  450139720,   &
       2060869306,  266683479, 350860264,   &
       2065846033,  158671935, 1772005618,   &
        795205130, 1221884629, 1976326989,   &
        998135974, 1676548301,  614362620,   &
        491179564,  327793080, 922741005/
      DATA (ITABLE(I),I=397,414)/   &
       1528656048, 1775329675,  828056307,   &
       1448319189,  173470778, 1388056867,   &
        956906308,  219286173,  460771359,   &
        358199631,  864535676, 376750930,   &
       1271089154,   28090922, 1825207361,   &
       1603702579,  361991756, 174271141/
      DATA (ITABLE(I),I=415,432)/   &
       1954855926,  911232829, 1384270246,   &
       1739676571,  754274892, 502141603,   &
       2030672558, 1703564182, 1551225070,   &
        988276910, 1331500472, 1748831164,   &
       2144180506,  318684035,  298360627,   &
        172742244, 2028487811, 1491743352/
      DATA (ITABLE(I),I=433,450)/   &
       2006421986, 2146093508,  258253944,   &
        409586221, 1230527712, 1211734974,   &
       1042283517,  634961640,  954041537,   &
       1463203857, 1231982802, 2045112487,   &
       1729798774,   94381532, 1427476838,   &
       2063395629, 1924404847, 221056062/
      DATA (ITABLE(I),I=451,468)/   &
        142524724,  968769863, 2041559534,   &
       2144859819,  998479391, 1005906879,   &
       1285646169, 2022189916,  869720790,   &
       1623616048,   40216307, 1605606591,   &
        150466735, 1306162626, 1097415548,   &
       1673554800, 1842198841, 1564181888/
      DATA (ITABLE(I),I=469,486)/   &
       1857668689, 1720395937,  974689951,   &
        608747141,  601104479, 999903065,   &
       1311275680, 1133168246, 1273728926,   &
       1445065986, 1331462779, 1115324913,   &
       2028541775,  251232653,  514348969,   &
       1041442808, 1537551006, 949033491/
      DATA (ITABLE(I),I=487,504)/   &
       1044836968,  601139657, 1591139711,   &
       1818750333,  454615333, 2120569352,   &
        770493452,  357056354,  976831960,   &
        102270405,  871779235, 1860162811,   &
        689431451, 1600121392,  302523963,   &
       1426453692, 2047249983, 1147472047/
      DATA (ITABLE(I),I=505,521)/   &
       1159543869,   39709758, 1681972136,   &
       1578444291, 1047707446, 1600623169,   &
        145955414,  646318224,  698104242,   &
       1334831733, 1902759969, 1507811506,   &
       1480946742,  936424064, 1719078432,   &
        306219886, 1266805790/
!
      DATA (ITABSV(I),I=1,18)/   &
        1464221660, 1158328647, 1090310074,   &
        363453867, 1125650601, 1626204584,   &
        596067919,  102301378, 1392342446,   &
       2117672210, 1470351739, 1107351344,   &
       1160753706, 1046087394,  142212969,   &
         24070872,  832220068, 561689965/
      DATA (ITABSV(I),I=19,36)/   &
       2132613190, 1327815900, 2099255323,   &
       1175377098, 2008300980, 1514090961,   &
       1793048224,  123482417,  899779517,   &
       14500045, 1036604204, 1819512164,   &
       373807068, 1185724401, 1969247094,   &
       117941294,  111922077, 2026157014/
      DATA (ITABSV(I),I=37,54)/   &
        972743819,  112361322,  818613141,   &
       1650818105, 1958655142, 340146731,   &
        244639603, 1374107263,  581629403,   &
         99815077,  407270832, 970490435,   &
        894442080,  502509560, 1772474916,   &
         92762028, 2125760521, 2119124955/
      DATA (ITABSV(I),I=55,72)/   &
        116833190,  815370972,  846774897,   &
        371565210,   14038994, 1877654635,   &
        469257780, 1255556676,  966738110,   &
        106141568, 1509906366, 182036763,   &
       1475162413,  355970676, 2057194637,   &
       783547359,  710739309, 1091521749/
      DATA (ITABSV(I),I=73,90)/   &
       1400722769, 1231840169, 1795363303,   &
        378309524, 1696574748, 43924770,   &
       1656718469,  194341481, 2122127727,   &
       1192298313,  787836434, 1930262483,   &
       2033580199, 1180162588,  833652824,   &
       1019699940, 1177388520, 1454532182/
      DATA (ITABSV(I),I=91,108)/   &
       1516029073,    7158256, 49724360,   &
       346179837,  711320736, 126147103,   &
        588000532, 1952681477,  872490485,   &
        929239679, 1230203969, 65553667,   &
        101370358,  777074835, 1448694438,   &
       37829780,  149952948, 1260879105/
      DATA (ITABSV(I),I=109,126)/   &
        226489139, 1261936689,  821434251,   &
       1820573641, 1034181831, 1908878446,   &
       1261839389, 1333596798,  474560247,   &
        179806371,  496186068, 720243575,   &
       1915930533, 1674665013, 1174195909,   &
       1483410280, 1538917937, 300722691/
      DATA (ITABSV(I),I=127,144)/   &
       1217246246, 1328435200, 1770412188,   &
       1931714531,  668347171, 1571429187,   &
       1256455103, 1034215170,  321723372,   &
       1988373705, 1603828968, 338728032,   &
         22885627,  239160176, 1623174495,   &
       1208969624, 1809686301, 586768446/
      DATA (ITABSV(I),I=145,162)/   &
        572364898, 1157585773, 1489728638,   &
        357378493, 2096054839, 1071933685,   &
        749129112, 2063846670,  915116346,   &
         82547408, 99850294, 999162951,   &
       1757081564, 1222216251, 1107447002,   &
        620994065,  276726035, 1632374490/
      DATA (ITABSV(I),I=163,180)/   &
       1214463005, 1795143947,  986560526,   &
        401521995,  986551091, 242947950,   &
        859782703, 2097912305,   78110042,   &
        682967577,  335973424, 970829205,   &
        145698529,  623819323,  516197007,   &
       2036646416, 1174464179, 1697256876/
      DATA (ITABSV(I),I=181,198)/   &
        771031831,  815657619, 1369483732,   &
        183355178,   11443201, 1199834624,   &
        749080238, 1242421352, 1392163283,   &
       1253963316, 2104424001, 2146002364,   &
        873880383,  666935248, 1463559443,   &
        765865763, 2036382270, 1029929651/
      DATA (ITABSV(I),I=199,216)/   &
       1309449537,  505953903, 1679489248,   &
        650734968, 1915876652, 769087046,   &
        341910829, 1976547278,  405565903,   &
        233036143, 1775766920, 1734382081,   &
       1964094636, 1567409215,  264778756,   &
        550435508, 1957515327, 510628849/
      DATA (ITABSV(I),I=217,234)/   &
        794411731,  772129518, 2084613852,   &
        2056793406,  482508883, 628545509,   &
        492310170,    2535299, 1808581000,   &
       1337327362,  897123632, 474197437,   &
        524509642,   13182159,  361730672,   &
       81199647, 1070351284, 2071002916/
      DATA (ITABSV(I),I=235,252)/   &
        931058636, 1736643210, 1312184093,   &
       1368480008,  493635086, 795562041,   &
        778036865,  437663472,  678482929,   &
        124422133, 1661200800, 366950953,   &
       1919116534, 1534692645,  153200398,   &
          6196433, 1064234375, 195844762/
      DATA (ITABSV(I),I=253,270)/   &
       1617967730, 1745699796, 1054886058,   &
       1992470821, 1744580876, 1576550441,   &
       1430025201, 1944059630, 1993995952,   &
       1607653829,  198657449, 1646157905,   &
        944085034, 1627982402,  411083987,   &
        633677110,  839782297, 958537595/
      DATA (ITABSV(I),I=271,288)/   &
       1866523018,  211248150,  657188559,   &
        859714592,  953170728, 1859902523,   &
        609738329,   80132019,  306596664,   &
       1156862695, 74374927, 183915535,   &
        839428712, 1458285441,  172543676,   &
        838639082, 1071875913, 1925638755/
      DATA (ITABSV(I),I=289,306)/   &
       1631994995, 1278741481, 1939215638,   &
         37917347, 1621691517, 2054362142,   &
        422444128,  437522314,  453524070,   &
        959581287,   80501639, 76349063,   &
       1150964582, 1876521145,  746044173,   &
       1754884425,  802123077, 1527702920/
      DATA (ITABSV(I),I=307,324)/   &
        788492908,   78719119,  182306481,   &
       1713345545,  590352192, 674841804,   &
       1205060021,  525498090, 1593642166,   &
        927838578, 1304219579, 652879324,   &
       1448845945,  436724282, 2073385775,   &
        177580556, 1741619009, 1188575653/
      DATA (ITABSV(I),I=325,342)/   &
        498115577,  937246633,  511610086,   &
        106192814,  223714241, 1868866237,   &
        939024237,  323029456,  317407376,   &
       316389284,  385186216, 1309020254,   &
       1880929110, 1816267930, 1682541052,   &
        402797268,  945227932, 1509316265/
      DATA (ITABSV(I),I=343,360)/   &
       1001627491,  228932404, 1523702251,   &
        121242082, 1901174818, 635982413,   &
        930304172, 1941268644,  183050837,   &
       1338834955,  465435419, 1437644759,   &
       1156952116, 1577273674,  700500350,   &
        804029596, 1358313048, 1416230126/
      DATA (ITABSV(I),I=361,378)/   &
       2018467981,  592185008, 1414209258,   &
        265994210, 1651218063, 90814660,   &
       1608601250, 1089576667,  921984300,   &
       1695616995, 1126839275, 129412032,   &
       1774571060,  962915884,  290498596,   &
       1179573341, 1667596730, 489164113/
      DATA (ITABSV(I),I=379,396)/   &
        813846475,  994357582,  450139720,   &
       2060869306,  266683479, 350860264,   &
       2065846033,  158671935, 1772005618,   &
        795205130, 1221884629, 1976326989,   &
        998135974, 1676548301,  614362620,   &
        491179564,  327793080, 922741005/
      DATA (ITABSV(I),I=397,414)/   &
       1528656048, 1775329675,  828056307,   &
       1448319189,  173470778, 1388056867,   &
        956906308,  219286173,  460771359,   &
        358199631,  864535676, 376750930,   &
       1271089154,   28090922, 1825207361,   &
       1603702579,  361991756, 174271141/
      DATA (ITABSV(I),I=415,432)/   &
       1954855926,  911232829, 1384270246,   &
       1739676571,  754274892, 502141603,   &
       2030672558, 1703564182, 1551225070,   &
        988276910, 1331500472, 1748831164,   &
       2144180506,  318684035,  298360627,   &
        172742244, 2028487811, 1491743352/
      DATA (ITABSV(I),I=433,450)/   &
       2006421986, 2146093508,  258253944,   &
        409586221, 1230527712, 1211734974,   &
       1042283517,  634961640,  954041537,   &
       1463203857, 1231982802, 2045112487,   &
       1729798774,   94381532, 1427476838,   &
       2063395629, 1924404847, 221056062/
      DATA (ITABSV(I),I=451,468)/   &
        142524724,  968769863, 2041559534,   &
       2144859819,  998479391, 1005906879,   &
       1285646169, 2022189916,  869720790,   &
       1623616048,   40216307, 1605606591,   &
        150466735, 1306162626, 1097415548,   &
       1673554800, 1842198841, 1564181888/
      DATA (ITABSV(I),I=469,486)/   &
       1857668689, 1720395937,  974689951,   &
        608747141,  601104479, 999903065,   &
       1311275680, 1133168246, 1273728926,   &
       1445065986, 1331462779, 1115324913,   &
       2028541775,  251232653,  514348969,   &
       1041442808, 1537551006, 949033491/
      DATA (ITABSV(I),I=487,504)/   &
       1044836968,  601139657, 1591139711,   &
       1818750333,  454615333, 2120569352,   &
        770493452,  357056354,  976831960,   &
        102270405,  871779235, 1860162811,   &
        689431451, 1600121392,  302523963,   &
       1426453692, 2047249983, 1147472047/
      DATA (ITABSV(I),I=505,521)/   &
       1159543869,   39709758, 1681972136,   &
       1578444291, 1047707446, 1600623169,   &
        145955414,  646318224,  698104242,   &
       1334831733, 1902759969, 1507811506,   &
       1480946742,  936424064, 1719078432,   &
        306219886, 1266805790/
!
!     START EXECUTABLE CODE
!
      IF(IRESET.EQ.1)THEN
        DO 100 LL=1,521
          ITABLE(LL)=ITABSV(LL)
  100   CONTINUE
        TABPTJ = 0
      ENDIF
!
!     UPDATE POINTER
!
      TABPTJ = TABPTJ + 1
      IF(TABPTJ.GT.P) TABPTJ = 1
!
!     UPDATE DELAY POINTER
!
      K = TABPTJ + Q
      IF(K.GT.P) K = K - P
!
!     COMPUTE EXCLUSIVE OR OF TWO TABLE ENTRIES AND REPLACE WITH NEW ONE
!
      ITABLE(TABPTJ) = IEOR( ITABLE(K), ITABLE(TABPTJ) )
!
!     CONVERT BIG INTEGER TO FLOATING POINT NUMBER
!
      RANFT = REAL( ITABLE(TABPTJ) ) / FN
!
      RETURN
      END FUNCTION RANFT
      SUBROUTINE DPPID2(IPID,ISUBRO,IERROR)
!
!     PURPOSE--THIS ROUTINE IS USED BY DPPID (AND POSSIBLY BY A
!              FEW OTHER ROUTINES) TO EXTRACT THE PROCESS ID.
!              THE PRIMARY USE OF THIS IS BUILDING UNIQUE FILE
!              NAMES.  HOWEVER, DATAPLOT USERS CAN USE IT FOR
!              WHATEVER PURPOSE THEY NEED.
!     TO THE IMPLEMENTER--
!              THIS IS A PLATFORM/COMPILER DEPENDENT ROUTINE,
!              SO YOU MAY NEED TO MODIFY IT FOR YOUR LOCAL
!              INSTALLATION.  IF IS CURRENTLY IMPLEMENTED FOR
!              INTEL COMPILER UNDER WINDOWS AND FOR THE g77
!              COMPILER UNDER UNIX.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2006.3
!     ORIGINAL VERSION--MARCH      2006.
!
!-----NON-COMMON VARIABLES (GRAPHICS)--------------------------------
!
#ifdef INTEL
      USE IFPORT
#endif
!
!
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
      IPID=0
!
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
!
!               ********************************
!               **  STEP 1--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       *********************************************************
!       *  PC USING INTEL     COMPILER                          *
!       *********************************************************
!
#ifdef INTEL
        IPID=getpid()
#endif
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!       *********************************************************
!       *  UNIX.  USE THE LIBRARY ROUTINE "GETPID".             *
!       *********************************************************
!
#ifdef LINUX
        IPID=getpid()
#endif
        GO TO 9000
      ELSE
!
!       *********************************************************
!       *  OTHER   - LEFT TO IMPLEMENTOR                        *
!       *********************************************************
!
        WRITE(ICOUT,8010)
 8010   FORMAT(1X,'THE PROCESS ID COMMAND HAS NOT BEEN IMPLEMENTED ',   &
               'AT THIS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,8020)IHOST1
 8020   FORMAT(1X,'SITE FOR A ',A4,' HOST.')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPPID2
      SUBROUTINE DPINF2(IFILE,IEXIST,IFWRIT,ISUBN0,IBUGS2,   &
                        ISUBRO,IERRFI)
!
!     PURPOSE--THE DPINFI ROUTINE CHECKS FOR THE EXISTENCE OF A
!              FILE.  THIS ROUTINE IS A SLIGHT VARIATION THAT
!              CHECKS IF THE FILE CAN BE OPENED IN WRITE MODE.
!
!              THE PURPOSE OF THIS IS TO CHECK IF THE PLOT FILES
!              ARE IN USE BY ANOTHER PROCESS.  CURRENTLY UNDER LINUX,
!              IF TWO DATAPLOT SESSIONS ARE RUNNING IN THE SAME
!              DIRECTORY AT THE SAME TIME, BOTH ARE ALLOWED TO WRITE
!              TO THE FILE (I.E., THERE IS NO LOCK ON THE FILE).
!              HOWEVER, UNDER WINDOWS, IF A DATAPLOT SESSION IS
!              ALREADY RUNNING, THEN A SECOND DATAPLOT PROCESS
!              WILL NOT BE ABLE TO OPEN THE FILE IN WRITE MODE
!              (IT WILL IN FACT HANG DATAPLOT).  THIS IS IN
!              PARTICULAR AN ISSUE BECAUSE THE VERSION BUILT WITH
!              THE INTEL COMPILER DOES NOT AUTOMATICALLY CLOSE
!              IF THE GUI IS NOT SHUT DOWN CLEANLY.  IF THIS HAPPENS,
!              WHEN YOU RESTART THE DATAPLOT GUI, THE "DEAD" PROCESS
!              STILL HAS THE PLOT FILE LOCKED AND THE NEW SESSION
!              HANGS.
!
!              NOTE THAT INQUIRING ABOUT THE "WRITE" MODE IS A
!              FORTRAN 90 FEATURE NOT AVAILABLE IN FORTRAN 77.
!              SO FOR NOW, THIS COMMAND IS ONLY ACTIVE UNDER
!              WINDOWS.
!
!     WRITTEN BY--JAMES J. FILLIBEN
!     LANGUAGE--ANSI FORTRAN (1977)
!     ORIGINAL VERSION--MARCH     2006.
!
!---------------------------------------------------------------------
!
      CHARACTER*(*) IFILE
      CHARACTER*4 IEXIST
      CHARACTER*12 IFWRIT
      CHARACTER*4 ISUBN0
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERRFI
!
      CHARACTER*4 ISUBN1
      CHARACTER*4 ISUBN2
      CHARACTER*4 ISTEPN
!
      LOGICAL LEXIST
!
!-----COMMON------------------------------------------------
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOF2.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      ISUBN1='DPIN'
      ISUBN2='F2  '
      IERRFI='NO'
!
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
!
!               ******************************************************
!               **  STEP 1--                                        **
!               **  INQUIRE ABOUT THE EXISTENCE OF A FILE.          **
!               **  IF FILE DOES NOT EXIST, THEN ASSUME THAT IT     **
!               **  IS WRITTABLE.  IF FILE EXISTS, CHECK IF IT      **
!               **  IS A WRITABLE FILE.                             **
!               ******************************************************
!
      ISTEPN='1'
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'INF2')   &
      CALL TRACE2(ISTEPN,ISUBN1,ISUBN2)
!
      IEXIST='NO'
      INQUIRE(FILE=IFILE,EXIST=LEXIST)
      IF(LEXIST)IEXIST='YES'
!
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPINF2
      SUBROUTINE DPPRMP(IPRDEF,IFLAG)
!
!     PURPOSE--THIS ROUTINE IS USED TO GENERATE EITHER AN
!              ADVANCING PROMPT (FORTRAN 77) OR A NON-ADVANCING
!              PROMPT (FORTRAN 90).
!     TO THE IMPLEMENTER--
!              THIS IS A PLATFORM/COMPILER DEPENDENT ROUTINE,
!              SO YOU MAY NEED TO MODIFY IT FOR YOUR LOCAL
!              INSTALLATION.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2007.8
!     ORIGINAL VERSION--AUGUST     2007.
!
!-----NON-COMMON VARIABLES (GRAPHICS)--------------------------------
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
!               ********************************
!               **  STEP 1--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
!     NOTE: VALUE OF IFLAG DETERMINES WHETHER A
!           "1X" IS REQUIRED.
!
!     NOTE: FOR THE INTEL FORTRAN COMPILER VERSION 13,
!           WHEN "ADVANCE='NO'" IS USED, THE PROMPT DOES
!           NOT PRINT.  FOR NOW, JUST SUPPRESS THIS OPTION.
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!CCCC   WRITE(IPRDEF,1611,ADVANCE='NO')
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      RETURN
      END SUBROUTINE DPPRMP
      SUBROUTINE DPCPUT(ATIME,IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--RETURN CURRENT CPU TIME USAGE.  CALL WITH INIT
!              SET TO 0 TO INITIALIZE THE TIMER.  OTHERWISE SET
!              INIT TO 1.  THIS ROUTINE UTILIZES FORTRAN 90
!              STANDARD CALLS, SO DO NOT MAKE HOST DEPENDENT.
!              IF COMPILING WITH A FORTRAN 77 COMPILER, YOU
!              MAY NEED TO COMMENT OUT THESE FORTRAN 90 CALLS.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1990)
!     VERSION NUMBER--2009/05
!     ORIGINAL VERSION--MAY       2009.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
      IERROR='NO'
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CPUT')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPCPUT')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CALL CPU_TIME(ATIME)
!
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
!
      RETURN
      END SUBROUTINE DPCPUT
#ifdef INTEL_QWIN
      SUBROUTINE IQWNHE(CHECKED)
#else
      SUBROUTINE IQWNHE()
#endif
!
!  CALL BACK ROUTINE FOR WHEN THE "USING HELP" MENU ITEM IS
!  SELECTED.
!
#ifdef INTEL_QWIN
      USE DFLIB
      USE IFQWIN
      LOGICAL CHECKED
      INTEGER IRET
#endif
!
#ifdef INTEL_QWIN
      CHECKED=.TRUE.
      IRET=MESSAGEBOXQQ(   &
      'To access Dataplot help, enter the command HELP'C,   &
      'Dataplot Help'C,   &
      MB$OK)
#endif
!
      RETURN
      END SUBROUTINE IQWNHE
#ifdef INTEL_QWIN
      SUBROUTINE IQWNBR(CHECKED)
#else
      SUBROUTINE IQWNBR()
#endif
!
!  CALL BACK ROUTINE FOR WHEN THE "HELP CONTENTS" MENU ITEM
!  SELECTED.
!
#ifdef INTEL_QWIN
      USE DFLIB
      USE DFWIN
      USE IFQWIN
      LOGICAL CHECKED
      INTEGER IRET
#endif
!
#ifdef INTEL_QWIN
      CHECKED=.TRUE.
      IRET=MESSAGEBOXQQ(   &
      'If you click OK, Dataplot will try to connect to the '//   &
      'Dataplot Reference Manual at the NIST web site (using '//   &
      'Internet Explorer).  Alternatively, you can use the,'//   &
      'Dataplot WEB HELP command (WEB HELP allows you to '//   &
      'specify a local site (if you have the installed a copy '//   &
      'the Dataplot web pages locally) or a different browser '//   &
      '(e.g., Netscape).  Enter HELP WEB HELP for details.'C,   &
      'Accesing the On-line Dataplot Reference Manual'C,   &
      MB$OKCANCEL)
!
      IF(IRET.EQ.MB$IDOK)THEN
        IRET=WinExec('Explorer.exe '//   &
             'http://www.itl.nist.gov/div898/'//   &
             'software/dataplot/document.htm'C,   &
             SW_SHOW)
      ENDIF
#endif
!
      RETURN
      END SUBROUTINE IQWNBR
      LOGICAL FUNCTION ISNANZ(VALUE)
!
!     PURPOSE--SOME FORTRAN COMPILERS SUPPORT AN "ISNAM" FUNCTION
!              THAT RETURNS TRUE IF THE TESTED VALUE IS
!              "NOT A NUMBER".  NOTE THAT THIS IS AN EXTENSION
!              TO THE FORTRAN STANDARD AND IS NOT CURRENTLY PART
!              OF THE FORTRAN STANDARD.  IT IS SUPPORTED IN
!              VERSION 4.4 AND ABOVE IN GFORTRAN AND ALSO FOR THE
!              INTEL COMPILER.  HOWEVER, IT IS NOT SUPPORTED IN
!              VERSION 4.1.2 OF GFORTRAN.  USE A "HAVE_ISNAM"
!              COMPILER SWITCH FOR LINUX.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1990)
!     VERSION NUMBER--2014/01
!     ORIGINAL VERSION--JANUARY   2014.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
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
!
      RETURN
      END FUNCTION ISNANZ
#ifdef INTEL
      SUBROUTINE DPCLIP(XSCRT,MAXVAL,NVAL,NUME,NUMVLN,PREAMV,   &
                        ISKIP,IGRPAU,   &
                        IVLIST,IVLIS2,IAVANM,MAXRDV,   &
                        ITYPE,ISTR,NCSTR,IEOF,   &
                        IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
!              SYSTEM AND COMPILER DEPENDENT.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.10
!     ORIGINAL VERSION--OCTOBER    2014.
!     UPDATED         --APRIL      2023. TWEAK DECLARATIONS FOR NEW
!                                        ONEAPI VERSION OF COMPILER
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
!     BEFORE ANY OTHER DECLARATIONS
!
      USE IFWIN
      USE IFWINTY
!
      REAL XSCRT(*)
!
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
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
      INTEGER IST
      INTEGER I
!CCCC INTEGER HDATA
      INTEGER(HANDLE)  ::  hData
      CHARACTER*1 STEXT(*)
      POINTER (PTEXT, STEXT)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      IF(ITYPE.EQ.'VARI')THEN
        DO 110 I=1,MAXVAL
          XSCRT(I)=CPUMIN
  110   CONTINUE
      ELSE
        ISTR=' '
      ENDIF
      NVAL=0
      NCSTR=0
      IEOF=0
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
        IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
           iSt = OpenClipboard(NULL)
           hData = GetClipboardData(CF_TEXT)
           pText = LocalLock(hData)
!
           NLNRD=0
           NVAL=0
           ICNTR=0
!
!          IF HEADER LINES ARE SPECIFIED, CHECK FOR CR/LF UNTIL
!          ALL HEADER LINES READ.  CHECK FOR VARIABLE NAMES ON
!          LAST LINE, OR NEXT TO LAST LINE IF LAST LINE IS "----",
!          IF REQUESTED.
!
           IF(ISKIP.GT.0)THEN
!
             NVAR=0
             NLCURR=1
             IPTR1=1
             IPTR2=0
             IPTRP1=0
             IPTRP2=0
!
  200        CONTINUE
             ICNTR=ICNTR+1
             ICHR=STEXT(ICNTR)(1:1)
             IVAL=ICHAR(ICHR)
             IF(IVAL.EQ.0)THEN
               IEOF=1
               GO TO 1099
             ENDIF
             IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
               IPTR2=ICNTR-1
               ICHRN=STEXT(ICNTR+1)(1:1)
               IVALN=ICHAR(ICHRN)
               IF((IVAL.EQ.10 .AND. IVALN.EQ.13) .OR.   &
                  (IVAL.EQ.13 .AND. IVALN.EQ.10))THEN
                 ICNTR=ICNTR+1
               ENDIF
               IF(NLCURR.EQ.ISKIP-1)THEN
                 IPTRP1=IPTR1
                 IPTRP2=IPTR2
               ELSEIF(NLCURR.EQ.ISKIP)THEN
                 IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.   &
                    STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
                   IPTR1=IPTRP1
                   IPTR2=IPTRP2
                 ENDIF
!
!                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
!                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
!                TREATED AS DELIMITERS.
!
                 IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')GO TO 219
                 NVAR=0
                 ICHCN2=0
                 DO 210 II=IPTR1,IPTR2
                   ICHRN=STEXT(II)(1:1)
                   IVAL2=ICHAR(ICHRN)
                   IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.   &
                     ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
                     IF(ICHCN2.EQ.0)GO TO 210
                     ISTOP=II
                     NVAR=NVAR+1
                     IF(NVAR.LE.MAXRDV)THEN
                       NCHAR=ISTOP-ISTRT+1
                       DO 220 JJ=1,MIN(NCHAR,4)
                         IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  220                  CONTINUE
                       IF(NCHAR.GE.5)THEN
                         DO 230 JJ=5,MIN(NCHAR,8)
                           IVLIS2(NVAR)(JJ-4:JJ-4)=   &
                           STEXT(ISTRT+JJ-1)(1:1)
  230                    CONTINUE
                       ENDIF
                     ELSE
                       GO TO 219
                     ENDIF
                     ICHCN2=0
                   ELSE
                     ICHCN2=ICHCN2+1
                     IF(ICHCN2.EQ.1)ISTRT=II
                     ICURR=II
                   ENDIF
  210            CONTINUE
!
                 IF(ICHCN2.GT.0)THEN
                   NVAR=NVAR+1
                   IVLIST(NVAR)='    '
                   IVLIS2(NVAR)='    '
                   ISTOP=ICURR
                   IF(NVAR.LE.MAXRDV)THEN
                     NCHAR=ISTOP-ISTRT+1
                     DO 270 JJ=1,MIN(NCHAR,4)
                       IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  270                CONTINUE
                     IF(NCHAR.GE.5)THEN
                       DO 280 JJ=5,MIN(NCHAR,8)
                         IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
  280                  CONTINUE
                     ENDIF
                   ENDIF
                 ENDIF
!
  219            CONTINUE
!
                 GO TO 1000
               ENDIF
               NLCURR=NLCURR+1
               IPTR1=ICNTR+1
             ENDIF
             GO TO 200
!
           ELSEIF(ISKIP.LT.0)THEN
!
!            FOR SKIP AUTOMATIC, READ UNTIL A LINE STARTING WITH
!            "----" IS ENCOUNTERED.
!
             NVAR=0
             NLCURR=1
             IPTR1=1
             IPTR2=0
             IPTR3=1
             IPTRP1=0
             IPTRP2=0
!
  300        CONTINUE
             ICNTR=ICNTR+1
             ICHR=STEXT(ICNTR)(1:1)
             IVAL=ICHAR(ICHR)
             IF(IVAL.EQ.0)THEN
               IEOF=1
               GO TO 1099
             ENDIF
             IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
               IPTRP1=IPTR1
               IPTRP2=IPTR2
               IPTR1=IPTR3
               IPTR2=ICNTR-1
               ICHRN=STEXT(ICNTR+1)(1:1)
               IVALN=ICHAR(ICHRN)
               IF((IVAL.EQ.10 .AND. IVALN.EQ.13) .OR.   &
                  (IVAL.EQ.13 .AND. IVALN.EQ.10))THEN
                 ICNTR=ICNTR+1
               ENDIF
               IPTR3=ICNTR+1
!
!              CHECK FOR "----"
!
               IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.   &
                  STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
                 IPTR1=IPTRP1
                 IPTR2=IPTRP2
!
!                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
!                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
!                TREATED AS DELIMITERS.
!
                 IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')GO TO 319
                 NVAR=0
                 ICHCN2=0
                 DO 310 II=IPTR1,IPTR2
                   ICHRN=STEXT(II)(1:1)
                   IVAL2=ICHAR(ICHRN)
                   IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.   &
                     ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
                     IF(ICHCN2.EQ.0)GO TO 310
                     ISTOP=II
                     NVAR=NVAR+1
                     IVLIST(NVAR)='   '
                     IVLIS2(NVAR)='   '
                     IF(NVAR.LE.MAXRDV)THEN
                       NCHAR=ISTOP-ISTRT+1
                       DO 330 JJ=1,MIN(NCHAR,4)
                         IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  330                  CONTINUE
                       IF(NCHAR.GE.5)THEN
                         DO 340 JJ=5,MIN(NCHAR,8)
                           IVLIS2(NVAR)(JJ-4:JJ-4)=   &
                           STEXT(ISTRT+JJ-1)(1:1)
  340                    CONTINUE
                       ENDIF
                     ELSE
                       GO TO 319
                     ENDIF
                     ICHCN2=0
                   ELSE
                     ICHCN2=ICHCN2+1
                     IF(ICHCN2.EQ.1)ISTRT=II
                     ICURR=II
                   ENDIF
  310            CONTINUE
!
                 IF(ICHCN2.GT.0)THEN
                   NVAR=NVAR+1
                   IVLIST(NVAR)='    '
                   IVLIS2(NVAR)='    '
                   ISTOP=ICURR
                   IF(NVAR.LE.MAXRDV)THEN
                     NCHAR=ISTOP-ISTRT+1
                     DO 370 JJ=1,MIN(NCHAR,4)
                       IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
  370                CONTINUE
                     IF(NCHAR.GE.5)THEN
                       DO 380 JJ=5,MIN(NCHAR,8)
                         IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
  380                  CONTINUE
                     ENDIF
                   ENDIF
                 ENDIF
!
  319            CONTINUE
!
                 GO TO 1000
               ENDIF
               NLCURR=NLCURR+1
             ENDIF
             GO TO 300
!
           ENDIF
!
 1000      CONTINUE
!
!            STRING CASE.  FOR NOW, READ FIRST RECORD AS A SINGLE
!            STRING.  DO NOT INCLUDE LEADING/TRAILING SPACES.
!
             IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')THEN
               DO 2090 II=1,10000
                 ICNTR=ICNTR+1
                 ICHR=STEXT(ICNTR)(1:1)
                 IVAL=ICHAR(ICHR)
                 IF(IVAL.EQ.0)THEN
                   GO TO 1099
                 ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
                   GO TO 1099
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
               GO TO 1099
             ENDIF
!
             ICHCNT=0
             DO 1090 II=1,80
!
               ICNTR=ICNTR+1
               ICHR=STEXT(ICNTR)(1:1)
               IVAL=ICHAR(ICHR)
!
!              NULL CHARACTER IMPLIES END OF CLIPBOARD DATA
!
               IF(IVAL.EQ.0)THEN
                 IEOF=1
                 IF(ICHCNT.EQ.0)GO TO 1099
                 GO TO 1080
!
!              SPACE INTERPRETED AS A DELIMITER THAT DOES NOT ALLOW
!              FOR "MISSING" COLUMNS
!
               ELSEIF(IVAL.EQ.32)THEN
                 IF(ICHCNT.GT.0)GO TO 1080
!
!              COMMA AND TAB ARE ALL INTERPRETED AS FIELD DELIMITERS
!              THAT ALLOW FOR "MISSING" COLUMNS.  THAT IS, IF SUCCESSIVE
!              COMMAS OR TABS ARE ENCOUNTERED, INSERT A "MISSING VALUE".
!
               ELSEIF(IVAL.EQ.9 .OR. ICHR.EQ.',')THEN
                 IF(ICHCNT.GT.0)THEN
                   GO TO 1080
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
                     GO TO 1099
                   ENDIF
                   XSCRT(NVAL)=PREAMV
                   GO TO 1000
                 ENDIF
!
!              LINE FEED/CARRIAGE RETURN SIGNIFY END OF RECORD
!              DELIMITERS.  IF NUMBER OF VARIABLES NOT SPECIFIED
!              ON READ, CHECK NUMBER OF VALUES READ ON FIRST LINE.
!
               ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!
!                2020/02: IF LAST CHARACTER IS COMMA OR TAB, THEN
!                         ADD A MISSING VALUE.
!
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
                     GO TO 1099
                   ENDIF
                   XSCRT(NVAL)=PREAMV
                 ENDIF
                 ICHRN=STEXT(ICNTR+1)(1:1)
                 IVALN=ICHAR(ICHRN)
                 IF((IVAL.EQ.10 .AND. IVALN.EQ.13) .OR.   &
                    (IVAL.EQ.13 .AND. IVALN.EQ.10))THEN
                   ICNTR=ICNTR+1
                 ENDIF
!
                 IF(NLNRD.EQ.0)THEN
                   IF(NUME.EQ.0)THEN
                     NUME=NVAL
                     IF(ICHCNT.GT.0)NUME=NVAL+1
                   ENDIF
                   NUMVLN=NVAL
                   IF(ICHCNT.GT.0)NUMVLN=NUMVLN+1
                 ENDIF
                 NLNRD=NLNRD+1
                 IF(ICHCNT.GT.0)GO TO 1080
!
!              ADD CURRENT CHARACTER TO TEMPORARY STRING
!
               ELSE
                 ICHCNT=ICHCNT+1
                 IATEMP(ICHCNT:ICHCNT)=ICHR
               ENDIF
!
 1090        CONTINUE
             IERROR='YES'
             GO TO 1099
!
!            PROCESS CURRENT FIELD
!
 1080        CONTINUE
             CALL EXTREA(IATEMP,ICHCNT,AVALUE,IBUGS2,ISUBRO,IERROR)
             IF(AVALUE.EQ.CPUMIN)GO TO 1000
!
!            IF ERROR DETECTED, GIVE ROW AND COLUMN
!
             IF(IERROR.EQ.'YES')THEN
               IF(IGRPAU.EQ.'IGNO')THEN
                 AVALUE=PREAMV
               ELSE
                 WRITE(ICOUT,999)
                 CALL DPWRST('XXX','BUG ')
                 NJUNK=MOD(NVAL,NLNRD) + 1
                 WRITE(ICOUT,2005)NLNRD+1,NJUNK
 2005            FORMAT('       UNABLE TO READ NUMERIC FIELD ON ROW ',   &
                        I8,' AND COLUMN ',I8)
                 CALL DPWRST('XXX','BUG ')
                 GO TO 1099
               ENDIF
             ENDIF
!
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
               GO TO 1099
             ENDIF
             XSCRT(NVAL)=AVALUE
             IF(IVAL.EQ.0)THEN
               IEOF=1
               GO TO 1099
             ENDIF
             GO TO 1000
!
!            PROCESSING OF CLIPBOARD COMPLETED
!
 1099      CONTINUE
           iSt = LocalUnlock(hData)
           iSt = CloseClipboard()
           IF(ITYPE.EQ.'STRI' .OR. ITYPE.EQ.'COMM')THEN
             IF(NCSTR.GT.0)THEN
               DO 1101 II=NCSTR,1,-1
                 IF(ISTR(II:II).NE.' ')THEN
                   NCSTR=II
                   GO TO 1103
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
           IF(IERROR.EQ.'YES')GO TO 9000
        ELSE
!
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
 2011   FORMAT('***** READING FROM SYSTEM CLIPBOARD NOT SUPPORTED ',   &
               'ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLIP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLIP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR
 9021   FORMAT('IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR = ',   &
               2(A4,2X),2I10,2I5)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO 9025 I=1,NCSTR
            WRITE(ICOUT,9027)I,ISTR(I:I)
 9027       FORMAT('I,ISTR(I:I) = ',I5,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLIP
      SUBROUTINE DPCLI2(ITYPE,IOUNIT,IHELMX,ILINRD,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
!              SYSTEM AND COMPILER DEPENDENT.
!
!              THE DPCLIP COMMAND IS USED TO EITHER READ A LIST
!              VARIABLES OR A SINGLE STRING.
!
!              DPCLI2 IS USED TO:
!
!                1. VIEW THE CONTENTS OF THE CLIPBOARD
!                2. TO COPY THE CONTENTS OF THE CLIPBOARD
!                   TO A USER SPECIFIED FILE.
!                3. TO EXTRACT THE NUMBER OF LINES IN THE CLIPBOARD
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.11
!     ORIGINAL VERSION--NOVEMBER   2014.
!     UPDATED         --APRIL      2023. TWEAK DECLARATIONS FOR NEW
!                                        ONEAPI VERSION OF COMPILER
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" MUST COME BEFORE ANY OTHER DECLARATIONS
!
      USE IFWIN
      USE IFWINTY
!
      CHARACTER*4 ITYPE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*255 ISTR
      CHARACTER*1  ICHR
      CHARACTER*1  ICHRN
      CHARACTER*4  IRESP
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
      INTEGER IST
!CCCC INTEGER HDATA
      INTEGER(HANDLE)  ::  hData
      CHARACTER*1 STEXT(*)
      POINTER (PTEXT, STEXT)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      ISTR=' '
      NCSTR=0
      ILINRD=0
      ICNTR=0
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!         VIEW THE CONTENTS OF THE CLIPBOARD OR COPY TO A
!         USER-SPECIFIED FILE
!
        IF(ITYPE.EQ.'VIEW' .OR. ITYPE.EQ.'COPY' .OR.   &
           ITYPE.EQ.'NLIN')THEN
!
!         VIEW THE CONTENTS OF THE CLIPBOARD
!
          IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
             iSt = OpenClipboard(NULL)
             hData = GetClipboardData(CF_TEXT)
             pText = LocalLock(hData)
!
 1000        CONTINUE
!
!            READ UNTIL NULL STRING OR CR/LF ENCOUNTERED.
!
             DO 1090 II=1,10000
               ICNTR=ICNTR+1
               ICHR=STEXT(ICNTR)(1:1)
               IVAL=ICHAR(ICHR)
               IF(IVAL.EQ.0)THEN
!
!                END OF STRING ENCOUNTERED
!
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
                 GO TO 1099
               ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
                 ICHRN=STEXT(ICNTR+1)(1:1)
                 IVALN=ICHAR(ICHRN)
                 IF(IVAL.EQ.10 .AND. IVALN.EQ.13)THEN
                   ICNTR=ICNTR+1
                 ELSEIF(IVAL.EQ.13 .AND. IVALN.EQ.10)THEN
                   ICNTR=ICNTR+1
                 ENDIF
!
!                END OF CURRENT RECORD
!
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
                   IF(IRESP.EQ.'NO')GO TO 1099
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
                 GO TO 1000
               ELSE
!
!                ADD CURRENT CHARACTER TO BUFFER.  FOR VIEWING
!                ON THE SCREEN, LIMIT BUFFER TO 240 CHARACTERS.
!                FOR WRITING TO EXTERNAL FILE, LIMIT BUFFER TO
!                255 CHARACTERS.
!
                 IF(NCSTR.LT.255)THEN
                   NCSTR=NCSTR+1
                   ISTR(NCSTR:NCSTR)=ICHR
                 ENDIF
               ENDIF
 1090        CONTINUE
             GO TO 1000
 1099        CONTINUE
!
!            PROCESSING OF CLIPBOARD COMPLETED
!
             iSt = LocalUnlock(hData)
             iSt = CloseClipboard()
             IF(IERROR.EQ.'YES')GO TO 9000
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
 1900   FORMAT('***** VIEW/COPY FROM SYSTEM CLIPBOARD NOT ',   &
               'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPCLI2
      SUBROUTINE DPCLI3(IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--CLEAR THE SYSTEM CLIPBOARD.  THIS IS OPERATING
!              SYSTEM AND COMPILER DEPENDENT.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.11
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
!     BEFORE ANY OTHER DECLARATIONS
!
      USE IFWIN
      USE IFWINTY
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
      INTEGER IST
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       CLEAR THE CLIPBOARD
!
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
 1900   FORMAT('***** CLEAR SYSTEM CLIPBOARD NOT ',   &
               'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI3
      SUBROUTINE DPCLI4(ISTR,NCSTR,IOP,ICLOSE,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--WRITE A STRING TO THE SYSTEM CLIPBOARD.
!              SYSTEM AND COMPILER DEPENDENT.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.11
!     ORIGINAL VERSION--NOVEMBER   2014.
!     UPDATED         --APRIL      2023. TWEAK DECLARATIONS FOR NEW
!                                        ONEAPI VERSION OF COMPILER
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
!     BEFORE ANY OTHER DECLARATIONS
!
      USE IFWIN
      USE IFWINTY
!
      INTEGER NCSTR
!
      CHARACTER*(*) ISTR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
      INTEGER IST
!CCCC INTEGER HDATA
!CCCC INTEGER ILEN
!CCCC INTEGER PSTRING
!CCCC INTEGER PDUMMY
      INTEGER(HANDLE)  ::  hData
      INTEGER(SIZE_T)  ::  ILEN
      INTEGER(SIZE_T)   ::  PSTRING
      INTEGER(SIZE_T)   ::  PDUMMY
!
      SAVE hData
      SAVE PSTRING
      SAVE PDUMMY
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       CLEAR THE CLIPBOARD
!
!CCCC   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
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
!
           PSTRING=LOC(ISTR)
           hData=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE,ILEN)
           PDUMMY=GlobalLock(hData)
           CALL CopyMemory(PDUMMY,PSTRING,ILEN)
           iSt = SetClipboardData(CF_TEXT,hData)
           iSt = GlobalUnlock(hData)
           IF(ICLOSE.EQ.1)THEN
             iSt = CloseClipboard()
           ENDIF
!
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('***** STRING WRITTEN TO CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
           ENDIF
!CCCC   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** WRITE STRING TO CLIPBOARD NOT ',   &
               'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI4--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI4
      SUBROUTINE DPCLI5(XSCRT,NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--WRITE VARIABLES TO THE CLIPBOARD.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.12
!     ORIGINAL VERSION--DECEMBER   2014.
!     UPDATED         --APRIL      2023. TWEAK DECLARATIONS FOR NEW
!                                        ONEAPI VERSION OF COMPILER
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
!     BEFORE ANY OTHER DECLARATIONS
!
      USE IFWIN
      USE IFWINTY
!
      REAL XSCRT(*)
!
      INTEGER NVAR
      INTEGER NROW
      INTEGER MAXVAL
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZC.INC'
      CHARACTER*5000000 ISTR
      EQUIVALENCE (CGARBG(1),ISTR)
      INTEGER MAXCHR
      PARAMETER(MAXCHR=5000000)
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
      INTEGER IST
      INTEGER I
!CCCC INTEGER HDATA
!CCCC INTEGER ILEN
!CCCC INTEGER PSTRING
!CCCC INTEGER PDUMMY
      INTEGER(HANDLE)  ::  hData
      INTEGER(SIZE_T)  ::  ILEN
      INTEGER(SIZE_T)   ::  PSTRING
      INTEGER(SIZE_T)   ::  PDUMMY
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       CLEAR THE CLIPBOARD
!
!CCCC   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
!CCCC      hClipboard = OpenClipboard(GETHWNDQQ(QWIN$FRAMEWINDOW))
           iSt = OpenClipboard(NULL)
           iSt = EmptyClipboard()
!
           ILEN=0
           ICNT=0
           ISTR=' '
!
           DO 1000 I=1,NROW
             DO 1100 J=1,NVAR
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
                 GO TO 1009
               ENDIF
 1100        CONTINUE
             ILEN=ILEN+1
             ISTR(ILEN:ILEN)=CHAR(13)
             ILEN=ILEN+1
             ISTR(ILEN:ILEN)=CHAR(10)
!
 1000      CONTINUE
 1009      CONTINUE
           ILEN=ILEN+1
           IF(ILEN.GT.MAXCHR)ILEN=MAXCHR
           ISTR(ILEN:ILEN)=CHAR(0)
!
           PSTRING=LOC(ISTR)
           hData=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE,ILEN)
           PDUMMY=GlobalLock(hData)
           CALL CopyMemory(PDUMMY,PSTRING,ILEN)
           iSt = SetClipboardData(CF_TEXT,hData)
           iSt = GlobalUnlock(hData)
           iSt = CloseClipboard()
!
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1311)
 1311         FORMAT('***** VARIABLES WRITTEN TO CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
           ENDIF
!CCCC   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** WRITE VARIABLES TO CLIPBOARD NOT ',   &
               'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI5')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI5--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI5
      SUBROUTINE DPCLI6(IOUNIT,ISTR,MAXCHR,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--WRITE THE CONTENTS OF A FILE TO THE CLIPBOARD.
!              SYSTEM AND COMPILER DEPENDENT.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.12
!     ORIGINAL VERSION--DECEMBER   2014.
!     UPDATED         --APRIL      2023. TWEAK DECLARATIONS FOR NEW
!                                        ONEAPI VERSION OF COMPILER
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
!     BEFORE ANY OTHER DECLARATIONS
!
      USE IFWIN
      USE IFWINTY
!
      CHARACTER*(*) ISTR
!
      INTEGER IOUNIT
      INTEGER MAXCHR
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      CHARACTER*255 ISTRIN
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
      INTEGER IST
      INTEGER I
!CCCC INTEGER HDATA
!CCCC INTEGER ILEN
!CCCC INTEGER PSTRING
!CCCC INTEGER PDUMMY
      INTEGER(HANDLE)  ::  hData
      INTEGER(SIZE_T)  ::  ILEN
      INTEGER(SIZE_T)   ::  PSTRING
      INTEGER(SIZE_T)   ::  PDUMMY
!
      SAVE HDATA
      SAVE PSTRING
      SAVE PDUMMY
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       CLEAR THE CLIPBOARD
!
!CCCC   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
           iSt = OpenClipboard(NULL)
           iSt = EmptyClipboard()
!
           ILEN=0
           DO 1000 I=1,100000
             READ(IOUNIT,'(A255)',END=1099,ERR=1099)ISTRIN
!
             IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI6')THEN
               WRITE(ICOUT,1001)I,ISTRIN(1:255)
 1001          FORMAT('I,ISTRIN: ',I8,2X,A255)
               CALL DPWRST('XXX','BUG ')
             ENDIF
!
             DO 1010 J=255,1,-1
               IF(ISTRIN(J:J).NE.' ')THEN
                 DO 1020 K=1,J
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
                 GO TO 1000
               ENDIF
 1010        CONTINUE
!
             IF(ILEN.LT.MAXCHR-2)THEN
               ILEN=ILEN+1
               ISTR(ILEN:ILEN)=CHAR(13)
               ILEN=ILEN+1
               ISTR(ILEN:ILEN)=CHAR(10)
             ENDIF
!
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
!
           IF(IFEEDB.EQ.'ON')THEN
              WRITE(ICOUT,1011)
 1011         FORMAT('***** FILE WRITTEN TO CLIPBOARD')
              CALL DPWRST('XXX','BUG ')
           ENDIF
!CCCC   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** WRITE FILE TO CLIPBOARD NOT ',   &
               'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
 9000 CONTINUE
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI6--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI6
#endif
#ifdef LINUX
      SUBROUTINE DPCLIP(XSCRT,MAXVAL,NVAL,NUME,NUMVLN,PREAMV,   &
                        ISKIP,IGRPAU,   &
                        IVLIST,IVLIS2,IAVANM,MAXRDV,   &
                        ITYPE,ISTR,NCSTR,   &
                        IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
!              SYSTEM AND COMPILER DEPENDENT.
!
!              FOR LINUX/CYGWIN/MACOS, READING FROM THE CLIPBOARD IS
!              PERFORMED BY COPYING THE CLIPBOARD CONTENTS TO
!              "dpst5f.dat" AND THEN PERFORMING A STANDARD READ, SO
!              FOR THESE SYSTEMS THIS IS A DUMMY ROUTINE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.10
!     ORIGINAL VERSION--OCTOBER    2014.
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
      REAL XSCRT(*)
!
      CHARACTER*4 IVLIST(MAXRDV)
      CHARACTER*4 IVLIS2(MAXRDV)
      CHARACTER*4 IAVANM
      CHARACTER*4 ITYPE
      CHARACTER*255 ISTR
      CHARACTER*4 IGRPAU
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
!CCCC CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!CCCC CHARACTER*80 IATEMP
!CCCC CHARACTER*1  ICHR
!CCCC CHARACTER*1  ICHRN
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
!CCCC INTEGER IST
      INTEGER I
!CCCC INTEGER HDATA
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
        WRITE(ICOUT,55)MAXVAL,NUME,NUMVLN,ISKIP,PREAMV
   55   FORMAT('MAXVAL,NUME,NUMVLN,ISKIP,PREAMV = ',4I10,G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      IF(ITYPE.EQ.'VARI')THEN
        DO 110 I=1,MAXVAL
          XSCRT(I)=CPUMIN
  110   CONTINUE
      ELSE
        ISTR=' '
      ENDIF
      NVAL=0
      NCSTR=0
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!INTE   IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
!INTE      iSt = OpenClipboard(NULL)
!INTE      hData = GetClipboardData(CF_TEXT)
!INTE      pText = LocalLock(hData)
!
!INTE      NLNRD=0
!INTE      NVAL=0
!INTE      ICNTR=0
!
!          IF HEADER LINES ARE SPECIFIED, CHECK FOR CR/LF UNTIL
!          ALL HEADER LINES READ.  CHECK FOR VARIABLE NAMES ON
!          LAST LINE, OR NEXT TO LAST LINE IF LAST LINE IS "----",
!          IF REQUESTED.
!
!INTE      IF(ISKIP.GT.0)THEN
!
!INTE        NVAR=0
!INTE        NLCURR=1
!INTE        IPTR1=1
!INTE        IPTR2=0
!INTE        IPTRP1=0
!INTE        IPTRP2=0
!
!I200        CONTINUE
!INTE        ICNTR=ICNTR+1
!INTE        ICHR=STEXT(ICNTR)(1:1)
!INTE        IVAL=ICHAR(ICHR)
!INTE        IF(IVAL.EQ.0)GO TO 1099
!INTE        IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!INTE          IPTR2=ICNTR-1
!INTE          ICHRN=STEXT(ICNTR+1)(1:1)
!INTE          IVALN=ICHAR(ICHRN)
!INTE          IF(IVALN.EQ.10 .OR. IVALN.EQ.13)THEN
!INTE            ICNTR=ICNTR+1
!INTE          ENDIF
!INTE          IF(NLCURR.EQ.ISKIP-1)THEN
!INTE            IPTRP1=IPTR1
!INTE            IPTRP2=IPTR2
!INTE          ELSEIF(NLCURR.EQ.ISKIP)THEN
!INTE            IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.
!INTE1              STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
!INTE              IPTR1=IPTRP1
!INTE              IPTR2=IPTRP2
!INTE            ENDIF
!
!                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
!                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
!                TREATED AS DELIMITERS.
!
!INTE            IF(ITYPE.EQ.'STRI')GO TO 219
!INTE            NVAR=0
!INTE            ICHCN2=0
!INTE            DO210II=IPTR1,IPTR2
!INTE              ICHRN=STEXT(II)(1:1)
!INTE              IVAL2=ICHAR(ICHRN)
!INTE              IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.
!INTE1               ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
!INTE                IF(ICHCN2.EQ.0)GO TO 210
!INTE                ISTOP=II
!INTE                NVAR=NVAR+1
!INTE                IF(NVAR.LE.MAXRDV)THEN
!INTE                  NCHAR=ISTOP-ISTRT+1
!INTE                  DO220JJ=1,MIN(NCHAR,4)
!INTE                    IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
!I220                  CONTINUE
!INTE                  IF(NCHAR.GE.5)THEN
!INTE                    DO230JJ=5,MIN(NCHAR,8)
!INTE                      IVLIS2(NVAR)(JJ-4:JJ-4)=
!INTE1                     STEXT(ISTRT+JJ-1)(1:1)
!I230                    CONTINUE
!INTE                  ENDIF
!INTE                ELSE
!INTE                  GO TO 219
!INTE                ENDIF
!INTE                ICHCN2=0
!INTE              ELSE
!INTE                ICHCN2=ICHCN2+1
!INTE                IF(ICHCN2.EQ.1)ISTRT=II
!INTE                ICURR=II
!INTE              ENDIF
!I210            CONTINUE
!
!INTE            IF(ICHCN2.GT.0)THEN
!INTE              NVAR=NVAR+1
!INTE              IVLIST(NVAR)='    '
!INTE              IVLIS2(NVAR)='    '
!INTE              ISTOP=ICURR
!INTE              IF(NVAR.LE.MAXRDV)THEN
!INTE                NCHAR=ISTOP-ISTRT+1
!INTE                DO270JJ=1,MIN(NCHAR,4)
!INTE                  IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
!I270                CONTINUE
!INTE                IF(NCHAR.GE.5)THEN
!INTE                  DO280JJ=5,MIN(NCHAR,8)
!INTE                    IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
!I280                  CONTINUE
!INTE                ENDIF
!INTE              ENDIF
!INTE            ENDIF
!
!I219            CONTINUE
!
!INTE            GO TO 1000
!INTE          ENDIF
!INTE          NLCURR=NLCURR+1
!INTE          IPTR1=ICNTR+1
!INTE        ENDIF
!INTE        GO TO 200
!
!INTE      ELSEIF(ISKIP.LT.0)THEN
!
!            FOR SKIP AUTOMATIC, READ UNTIL A LINE STARTING WITH
!            "----" IS ENCOUNTERED.
!
!INTE        NVAR=0
!INTE        NLCURR=1
!INTE        IPTR1=1
!INTE        IPTR2=0
!INTE        IPTR3=1
!INTE        IPTRP1=0
!INTE        IPTRP2=0
!
!I300        CONTINUE
!INTE        ICNTR=ICNTR+1
!INTE        ICHR=STEXT(ICNTR)(1:1)
!INTE        IVAL=ICHAR(ICHR)
!INTE        IF(IVAL.EQ.0)GO TO 1099
!INTE        IF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!INTE          IPTRP1=IPTR1
!INTE          IPTRP2=IPTR2
!INTE          IPTR1=IPTR3
!INTE          IPTR2=ICNTR-1
!INTE          ICHRN=STEXT(ICNTR+1)(1:1)
!INTE          IVALN=ICHAR(ICHRN)
!INTE          IF(IVALN.EQ.10 .OR. IVALN.EQ.13)THEN
!INTE            ICNTR=ICNTR+1
!INTE          ENDIF
!INTE          IPTR3=ICNTR+1
!
!              CHECK FOR "----"
!
!INTE          IF(STEXT(IPTR1).EQ.'-'.AND.STEXT(IPTR1+1).EQ.'-'.AND.
!INTE1            STEXT(IPTR1+2).EQ.'-'.AND.STEXT(IPTR1+3).EQ.'-')THEN
!INTE            IPTR1=IPTRP1
!INTE            IPTR2=IPTRP2
!
!                NOW LOOP THROUGH AND EXTRACT VARIABLE NAMES.
!                SPACES, COMMAS, TABS, AND DOUBLE QUOTES  WILL BE
!                TREATED AS DELIMITERS.
!
!INTE            IF(ITYPE.EQ.'STRI')GO TO 319
!INTE            NVAR=0
!INTE            ICHCN2=0
!INTE            DO310II=IPTR1,IPTR2
!INTE              ICHRN=STEXT(II)(1:1)
!INTE              IVAL2=ICHAR(ICHRN)
!INTE              IF(IVAL2.EQ.32 .OR. IVAL2.EQ.9 .OR.
!INTE1               ICHRN.EQ.',' .OR. ICHRN.EQ.'"')THEN
!INTE                IF(ICHCN2.EQ.0)GO TO 310
!INTE                ISTOP=II
!INTE                NVAR=NVAR+1
!INTE                IVLIST(NVAR)='   '
!INTE                IVLIS2(NVAR)='   '
!INTE                IF(NVAR.LE.MAXRDV)THEN
!INTE                  NCHAR=ISTOP-ISTRT+1
!INTE                  DO330JJ=1,MIN(NCHAR,4)
!INTE                    IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
!I330                  CONTINUE
!INTE                  IF(NCHAR.GE.5)THEN
!INTE                    DO340JJ=5,MIN(NCHAR,8)
!INTE                      IVLIS2(NVAR)(JJ-4:JJ-4)=
!INTE1                     STEXT(ISTRT+JJ-1)(1:1)
!I340                    CONTINUE
!INTE                  ENDIF
!INTE                ELSE
!INTE                  GO TO 319
!INTE                ENDIF
!INTE                ICHCN2=0
!INTE              ELSE
!INTE                ICHCN2=ICHCN2+1
!INTE                IF(ICHCN2.EQ.1)ISTRT=II
!INTE                ICURR=II
!INTE              ENDIF
!I310            CONTINUE
!
!INTE            IF(ICHCN2.GT.0)THEN
!INTE              NVAR=NVAR+1
!INTE              IVLIST(NVAR)='    '
!INTE              IVLIS2(NVAR)='    '
!INTE              ISTOP=ICURR
!INTE              IF(NVAR.LE.MAXRDV)THEN
!INTE                NCHAR=ISTOP-ISTRT+1
!INTE                DO370JJ=1,MIN(NCHAR,4)
!INTE                  IVLIST(NVAR)(JJ:JJ)=STEXT(ISTRT+JJ-1)(1:1)
!I370                CONTINUE
!INTE                IF(NCHAR.GE.5)THEN
!INTE                  DO380JJ=5,MIN(NCHAR,8)
!INTE                    IVLIS2(NVAR)(JJ-4:JJ-4)=STEXT(ISTRT+JJ-1)(1:1)
!I380                  CONTINUE
!INTE                ENDIF
!INTE              ENDIF
!INTE            ENDIF
!
!I319            CONTINUE
!
!INTE            GO TO 1000
!INTE          ENDIF
!INTE          NLCURR=NLCURR+1
!INTE        ENDIF
!INTE        GO TO 300
!
!INTE      ENDIF
!
!1000      CONTINUE
!
!            STRING CASE.  FOR NOW, READ FIRST RECORD AS A SINGLE
!            STRING.
!
!INTE        IF(ITYPE.EQ.'STRI')THEN
!INTE          DO2090II=1,10000
!INTE            ICNTR=ICNTR+1
!INTE            ICHR=STEXT(ICNTR)(1:1)
!INTE            IVAL=ICHAR(ICHR)
!INTE            IF(IVAL.EQ.0)THEN
!INTE              GO TO 1099
!INTE            ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!INTE              GO TO 1099
!INTE            ELSE
!INTE              IF(NCSTR.LT.255)THEN
!INTE                NCSTR=NCSTR+1
!INTE                ISTR(NCSTR:NCSTR)=ICHR
!INTE              ENDIF
!INTE            ENDIF
!2090          CONTINUE
!INTE          GO TO 1099
!INTE        ENDIF
!
!INTE        ICHCNT=0
!INTE        DO1090II=1,80
!
!INTE          ICNTR=ICNTR+1
!INTE          ICHR=STEXT(ICNTR)(1:1)
!INTE          IVAL=ICHAR(ICHR)
!
!              NULL CHARACTER IMPLIES END OF CLIPBOARD DATA
!
!INTE          IF(IVAL.EQ.0)THEN
!INTE            IF(ICHCNT.EQ.0)GO TO 1099
!INTE            GO TO 1080
!
!              SPACE INTERPRETED AS A DELIMITER THAT DOES NOT ALLOW
!              FOR "MISSING" COLUMNS
!
!INTE          ELSEIF(IVAL.EQ.32)THEN
!INTE            IF(ICHCNT.GT.0)GO TO 1080
!
!              COMMA AND TAB ARE ALL INTERPRETED AS FIELD DELIMITERS
!              THAT ALLOW FOR "MISSING" COLUMNS.  THAT IS, IF SUCCESSIVE
!              COMMAS OR TABS ARE ENCOUNTERED, INSERT A "MISSING VALUE".
!
!INTE          ELSEIF(IVAL.EQ.9 .OR. ICHR.EQ.',')THEN
!INTE            IF(ICHCNT.GT.0)THEN
!INTE              GO TO 1080
!INTE            ELSE
!INTE              NVAL=NVAL+1
!INTE              IF(NVAL.GT.MAXVAL)THEN
!INTE                WRITE(ICOUT,1003)
!INTE                CALL DPWRST('XXX','BUG ')
!INTE                WRITE(ICOUT,1005)
!INTE                CALL DPWRST('XXX','BUG ')
!INTE                WRITE(ICOUT,1007)MAXVAL
!INTE                CALL DPWRST('XXX','BUG ')
!INTE                IERROR='YES'
!INTE                GO TO 1099
!INTE              ENDIF
!INTE              XSCRT(NVAL)=PREAMV
!INTE              GO TO 1000
!INTE            ENDIF
!
!              LINE FEED/CARRIAGE RETURN SIGNIFY END OF RECORD
!              DELIMITERS.  IF NUMBER OF VARIABLES NOT SPECIFIED
!              ON READ, CHECK NUMBER OF VALUES READ ON FIRST LINE.
!
!INTE          ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!INTE            ICHRN=STEXT(ICNTR+1)(1:1)
!INTE            IVALN=ICHAR(ICHRN)
!INTE            IF(IVALN.EQ.10 .OR. IVALN.EQ.13)THEN
!INTE              ICNTR=ICNTR+1
!INTE            ENDIF
!
!INTE            IF(NLNRD.EQ.0)THEN
!INTE              IF(NUME.EQ.0)THEN
!INTE                NUME=NVAL
!INTE                IF(ICHCNT.GT.0)NUME=NVAL+1
!INTE              ENDIF
!INTE            ENDIF
!INTE            NLNRD=NLNRD+1
!INTE            IF(ICHCNT.GT.0)GO TO 1080
!
!              ADD CURRENT CHARACTER TO TEMPORARY STRING
!
!INTE          ELSE
!INTE            ICHCNT=ICHCNT+1
!INTE            IATEMP(ICHCNT:ICHCNT)=ICHR
!INTE          ENDIF
!
!1090        CONTINUE
!INTE        IERROR='YES'
!INTE        GO TO 1099
!
!            PROCESS CURRENT FIELD
!
!1080        CONTINUE
!INTE        CALL EXTREA(IATEMP,ICHCNT,AVALUE,IBUGS2,ISUBRO,IERROR)
!INTE        IF(IERROR.EQ.'YES')GO TO 1099
!INTE        NVAL=NVAL+1
!INTE        IF(NVAL.GT.MAXVAL)THEN
!INTE          WRITE(ICOUT,1003)
!1003          FORMAT('***** ERROR READING FROM CLIPBOARD')
!INTE          CALL DPWRST('XXX','BUG ')
!INTE          WRITE(ICOUT,1005)
!1005          FORMAT('      MAXIMUM NUMBER OF VALUES EXCEEDED.')
!INTE          CALL DPWRST('XXX','BUG ')
!INTE          WRITE(ICOUT,1007)MAXVAL
!1007          FORMAT('      MAXIMUM NUMBER OF VALUES  = ',I10)
!INTE          CALL DPWRST('XXX','BUG ')
!INTE          IERROR='YES'
!INTE          GO TO 1099
!INTE        ENDIF
!INTE        XSCRT(NVAL)=AVALUE
!INTE        IF(IVAL.EQ.0)GO TO 1099
!INTE        GO TO 1000
!
!            PROCESSING OF CLIPBOARD COMPLETED
!
!1099      CONTINUE
!INTE      iSt = LocalUnlock(hData)
!INTE      iSt = CloseClipboard()
!INTE      IF(IERROR.EQ.'YES')GO TO 9000
!INTE   ELSE
!INTE     WRITE(ICOUT,1011)
!1011     FORMAT('***** NOTHING AVAILABLE FROM CLIPBOARD')
!INTE     CALL DPWRST('XXX','BUG ')
!INTE     IERROR='YES'
!INTE   ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2011)
 2011   FORMAT('***** READING FROM SYSTEM CLIPBOARD NOT SUPPORTED ',   &
               'ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLIP')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLIP--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9021)IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR
 9021   FORMAT('IERROR,IAVANM,NVAL,NLNRD,NUME,NCSTR = ',   &
               2(A4,2X),2I10,2I5)
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GT.0)THEN
          DO 9025 I=1,NCSTR
            WRITE(ICOUT,9027)I,ISTR(I:I)
 9027       FORMAT('I,ISTR(I:I) = ',I5,2X,A1)
            CALL DPWRST('XXX','BUG ')
 9025     CONTINUE
        ENDIF
        IF(NVAL.GT.0)THEN
          DO 9035 I=1,NVAL
            WRITE(ICOUT,9037)I,IVLIST(I),IVLIS2(I)
 9037       FORMAT('I,IVLIST(I),IVLIS2(I) = ',I8,2X,2A4)
            CALL DPWRST('XXX','BUG ')
 9035     CONTINUE
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLIP
      SUBROUTINE DPCLI2(ITYPE,IOUNIT,IHELMX,ILINRD,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--READ FROM THE SYSTEM CLIPBOARD.  THIS IS OPERATING
!              SYSTEM AND COMPILER DEPENDENT.
!
!              THE DPCLIP COMMAND IS USED TO EITHER READ A LIST
!              VARIABLES OR A SINGLE STRING.
!
!              DPCLI2 IS USED TO:
!
!                1. VIEW THE CONTENTS OF THE CLIPBOARD
!                2. TO COPY THE CONTENTS OF THE CLIPBOARD
!                   TO A USER SPECIFIED FILE.
!                3. TO EXTRACT THE NUMBER OF LINES IN THE CLIPBOARD.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.11
!     ORIGINAL VERSION--NOVEMBER   2014.
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
!     NOTE THAT "USE IFWIN" (FOR INTEL COMPILER UNDER WINDOWS) MUST COME
!     BEFORE ANY OTHER DECLARATIONS
!INTE USE IFWIN
!
      CHARACTER*4 ITYPE
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
      CHARACTER*255 ISTR
!CCCC CHARACTER*1  ICHR
      CHARACTER*4  IRESP
      CHARACTER*6  IFORMT
!
      INCLUDE 'DPCOPA.INC'
      INCLUDE 'DPCOZI.INC'
!
      INTEGER ISTR2(MAXOBV)
      EQUIVALENCE (IGARBG(IIGAR1),ISTR2(1))
!
!     FOLLOWING DECLARATIONS FOR INTEL FORTRAN COMPILER UNDER WINDOWS
!
!INTE INTEGER IST
!INTE INTEGER I
!INTE INTEGER HDATA
!INTE CHARACTER*1 STEXT(*)
!INTE POINTER (PTEXT, STEXT)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
      ISTR=' '
      NCSTR=0
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!         VIEW THE CONTENTS OF THE CLIPBOARD OR COPY TO A
!         USER-SPECIFIED FILE
!
!INTE   IF(ITYPE.EQ.'VIEW' .OR. ITYPE.EQ.'COPY')THEN
!
!         VIEW THE CONTENTS OF THE CLIPBOARD
!
!INTE     IF (IsClipboardFormatAvailable(CF_TEXT)) THEN
!INTE        iSt = OpenClipboard(NULL)
!INTE        hData = GetClipboardData(CF_TEXT)
!INTE        pText = LocalLock(hData)
!
!INTE        ILINRD=0
!INTE        ICNTR=0
!
!1000        CONTINUE
!
!             READ UNTIL NULL STRING OR CR/LF ENCOUNTERED.
!
!INTE        DO1090II=1,10000
!INTE          ICNTR=ICNTR+1
!INTE          ICHR=STEXT(ICNTR)(1:1)
!INTE          IVAL=ICHAR(ICHR)
!INTE          IF(IVAL.EQ.0)THEN
!
!                END OF STRING ENCOUNTERED
!
!INTE            IF(NCSTR.GE.1)THEN
!INTE              IF(ITYPE.EQ.'VIEW')THEN
!INTE                NCSTR=MIN(240,NCSTR)
!INTE                WRITE(ICOUT,1010)ISTR(1:NCSTR)
!1010                FORMAT(A)
!INTE                CALL DPWRST('XXX','BUG ')
!INTE              ELSEIF(ITYPE.EQ.'COPY')THEN
!INTE                WRITE(IOUNIT,1020)ISTR(1:NCSTR)
!1020                FORMAT(A)
!INTE              ENDIF
!INTE            ENDIF
!INTE            NCSTR=0
!INTE            ISTR=' '
!INTE            GO TO 1099
!INTE          ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!
!                END OF CURRENT RECORD
!
!INTE            ILINRD=ILINRD+1
!INTE            IF(ILINRD.GT.IHELMX .AND. ITYPE.EQ.'VIEW')THEN
!INTE              WRITE(ICOUT,1051)
!1051              FORMAT('                            MORE...?')
!INTE              CALL DPWRST('XXX','WRIT')
!INTE              READ(IRD,1052)IRESP
!1052              FORMAT(A4)
!INTE              IF(IRESP.EQ.'N')IRESP='NO'
!INTE              IF(IRESP.EQ.'n')IRESP='NO'
!INTE              IF(IRESP.EQ.'no')IRESP='NO'
!INTE              IF(IRESP.EQ.'NO')GO TO 1099
!INTE              ILINRD=0
!INTE            ENDIF
!INTE            IF(NCSTR.GE.1)THEN
!INTE              IF(ITYPE.EQ.'VIEW')THEN
!INTE                NCSTR=MIN(240,NCSTR)
!INTE                WRITE(ICOUT,1010)ISTR(1:NCSTR)
!INTE                CALL DPWRST('XXX','BUG ')
!INTE              ELSEIF(ITYPE.EQ.'COPY')THEN
!INTE                WRITE(IOUNIT,1020)ISTR(1:NCSTR)
!INTE              ENDIF
!INTE            ENDIF
!INTE            NCSTR=0
!INTE            ISTR=' '
!INTE            GO TO 1000
!INTE          ELSE
!
!                ADD CURRENT CHARACTER TO BUFFER.  FOR VIEWING
!                ON THE SCREEN, LIMIT BUFFER TO 240 CHARACTERS.
!                FOR WRITING TO EXTERNAL FILE, LIMIT BUFFER TO
!                255 CHARACTERS.
!
!INTE            IF(NCSTR.LT.255)THEN
!INTE              NCSTR=NCSTR+1
!INTE              ISTR(NCSTR:NCSTR)=ICHR
!INTE            ENDIF
!INTE          ENDIF
!1090        CONTINUE
!INTE        GO TO 1000
!1099        CONTINUE
!
!            PROCESSING OF CLIPBOARD COMPLETED
!
!INTE        iSt = LocalUnlock(hData)
!INTE        iSt = CloseClipboard()
!INTE        IF(IERROR.EQ.'YES')GO TO 9000
!INTE     ELSE
!INTE       WRITE(ICOUT,1110)
!1110       FORMAT('***** NOTHING AVAILABLE FROM CLIPBOARD')
!INTE       CALL DPWRST('XXX','BUG ')
!INTE       IERROR='YES'
!INTE     ENDIF
!INTE   ELSEIF(ITYPE.EQ.'RUN')THEN
!INTE   ENDIF
      ELSEIF(IOPSY1.EQ.'UNIX')THEN
        MAXCHR=MAXOBV
        NVAL=0
        ILINRD=0
        IERR=0
!
!CCCC   CALL XFETCH(ISTR2,NVAL,MAXCHR,IERR)
!
        IF(ITYPE.EQ.'VIEW' .OR. ITYPE.EQ.'COPY')THEN
!
          IF(IERR.EQ.1)THEN
            WRITE(ICOUT,1001)
 1001       FORMAT('***** ERROR IN VIEW CLIPBOARD')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1003)
 1003       FORMAT('      THE X11 DEVICE IS NOT CURRENTLY OPEN SO ',   &
                   'THE X11 CLIPBOARD IS NOT AVAILABLE.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
          IF(NVAL.GT.0)THEN
            IPTR=0
            ISTRT=1
            ISTOP=0
 1010       CONTINUE
              IPTR=IPTR+1
              ISTOP=0
              IVAL=ISTR2(IPTR)
!
              IF(IVAL.EQ.0)THEN
!
!               END OF STRING ENCOUNTERED
!
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
                    DO 1015 J=ISTRT,ISTRT+NCHAR-1
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
!
                GO TO 9000
!
              ELSEIF(IVAL.EQ.10 .OR. IVAL.EQ.13)THEN
!
!               END OF RECORD ENCOUNTERED
!
                ISTOP=IPTR-1
                ILINRD=ILINRD+1
!
                IF(ILINRD.GT.IHELMX .AND. ITYPE.EQ.'VIEW')THEN
                  WRITE(ICOUT,1051)
 1051             FORMAT('                            MORE...?')
                  CALL DPWRST('XXX','WRIT')
                  READ(IRD,1052)IRESP
 1052             FORMAT(A4)
                  IF(IRESP.EQ.'N')IRESP='NO'
                  IF(IRESP.EQ.'n')IRESP='NO'
                  IF(IRESP.EQ.'no')IRESP='NO'
                  IF(IRESP.EQ.'NO')GO TO 9000
                  ILINRD=0
                ENDIF
!
                IF(IVAL.EQ.10 .AND. ISTR2(IPTR+1).EQ.13)IPTR=IPTR+1
                IF(IVAL.EQ.13 .AND. ISTR2(IPTR+1).EQ.10)IPTR=IPTR+1
!
                IF(ISTOP.GE.ISTRT)THEN
                  NCHAR=ISTOP-ISTRT+1
                  IF(ITYPE.EQ.'VIEW')THEN
                    IF(NCHAR.GT.240)NCHAR=240
                  ELSE
                    IF(NCHAR.GT.255)NCHAR=255
                  ENDIF
                  IF(NCHAR.GE.1)THEN
                    ICNT=0
                    DO 1020 J=ISTRT,ISTRT+NCHAR-1
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
              IF(IPTR.GE.NVAL)GO TO 9000
              GO TO 1010
          ELSE
            GO TO 9000
          ENDIF
        ELSEIF(ITYPE.EQ.'NLIN')THEN
!
          IF(IERR.EQ.1)THEN
            WRITE(ICOUT,3001)
 3001       FORMAT('***** ERROR IN PROBE CLIPBOARD LINES')
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1003)
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
!
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
              IF(IPTR.GE.NVAL)GO TO 9000
              GO TO 3010
          ELSE
            GO TO 9000
          ENDIF
        ENDIF
      ELSE
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1900)
 1900   FORMAT('***** VIEW/COPY FROM SYSTEM CLIPBOARD NOT ',   &
               'SUPPORTED ON THIS OS/COMPILER PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
      ENDIF
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPCLI2
      SUBROUTINE DPCLI3(IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--CLEAR THE SYSTEM CLIPBOARD.  THIS IS OPERATING
!              SYSTEM AND COMPILER DEPENDENT.
!
!              THIS VERSION IS FOR LINUX.  IT ASSUMES "xclip" IS
!              INSTALLED.  IT ISSUES THE COMMAND
!
!                 echo -n | xclip -selection clipboard
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2020.06
!     ORIGINAL VERSION--JUNE       2020.
!     UPDATED         --AUGUST     2023. SUPPORT FOR CYGWIN
!     UPDATED         --AUGUST     2023. SUPPORT FOR MACOS
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
#ifdef HAVE_XCLIP
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 ISSAV3
      CHARACTER*4 ISSAV4
      CHARACTER*80 ISTRIN
#elif defined(CYGWIN)
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 ISSAV3
      CHARACTER*4 ISSAV4
      CHARACTER*80 ISTRIN
#elif defined(MACOSX)
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 ISSAV3
      CHARACTER*4 ISSAV4
      CHARACTER*80 ISTRIN
#endif
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
#ifdef HAVE_XCLIP
!
!     FOR XCLIP, ISSUE THE COMMAND:
!
!         echo -n | xclip -selection clipboard
!
      ISTRIN=' '
      IF(IX11SE.EQ.'CLIP')THEN
        ISTRIN='echo -n | xclip -selection clipboard'
        NCSTR=36
      ELSEIF(IX11SE.EQ.'PRIM')THEN
        ISTRIN='echo -n | xclip -selection primary'
        NCSTR=34
      ELSE
        ISTRIN='echo -n | xclip -selection secondary'
        NCSTR=36
      ENDIF
!
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISSAV3=ICLEWT
      ISSAV4=ILINSY
      ISYSPE='ON'
      ICLEWT='OFF'
      ILINSY='COMM'
      CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      ICLEWT=ISSAV3
      ILINSY=ISSAV4
#elif defined(CYGWIN)
!
!     FOR CYGWIN, ISSUE THE COMMAND:
!
!         echo -n | /dev/clipboard
!
      ISTRIN=' '
      ISTRIN='echo -n | /dev/clipboard'
      NCSTR=24
!
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISSAV3=ICLEWT
      ISSAV4=ILINSY
      ISYSPE='ON'
      ICLEWT='OFF'
      ILINSY='COMM'
      CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      ICLEWT=ISSAV3
      ILINSY=ISSAV4
#elif defined(MACOSX)
!
!     FOR MACOS, ISSUE THE COMMAND:
!
!         echo -n | pbcopy
!
      ISTRIN=' '
      ISTRIN='echo -n | pbcopy'
      NCSTR=16
!
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISSAV3=ICLEWT
      ISSAV4=ILINSY
      ISYSPE='ON'
      ICLEWT='OFF'
      ILINSY='COMM'
      CALL DPSYS2(ISTRIN,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
      ICLEWT=ISSAV3
      ILINSY=ISSAV4
#else
      IERROR='YES'
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1211)
 1211 FORMAT('***** ERROR IN CLIPBOARD CLEAR COMMAND')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2201)
 2201 FORMAT('      THE xclip COMMAND IS NOT AVAILABLE.')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,2203)
 2203 FORMAT('      THE CONTENTS OF THE CLIPBOARD WILL NOT BE ',   &
             'CLEARED.')
      CALL DPWRST('XXX','BUG ')
#endif
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI3')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI3--')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI3
      SUBROUTINE DPCLI4(ISTR,NCSTR,IOP,ICLOSE,IBUGS2,ISUBRO,IERROR)
!
!     PURPOSE--WRITE A STRING TO THE SYSTEM CLIPBOARD.
!              SYSTEM AND COMPILER DEPENDENT.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2014.11
!     ORIGINAL VERSION--NOVEMBER   2014.
!
!-----NON-COMMON VARIABLES (GRAPHICS)---------------------------------
!
      INTEGER NCSTR
!
      CHARACTER*(*) ISTR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IERROR='NO'
!
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
!
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'CLI4')THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,9011)
 9011   FORMAT('***** AT THE END       OF DPCLI4--')
        CALL DPWRST('XXX','BUG ')
        IF(NCSTR.GE.1)THEN
          WRITE(ICOUT,9013)ISTR(1:MIN(80,NCSTR))
 9013     FORMAT('ISTR(1) = ',A80)
          CALL DPWRST('XXX','BUG ')
        ENDIF
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI4
      SUBROUTINE DPCLI5(XSCRT,NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR)
!
      REAL XSCRT(*)
!
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,52)NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR
   52   FORMAT('NVAR,NROW,MAXVAL,IBUGS2,ISUBRO,IERROR = ',   &
               I8,2(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,54)XSCRT(1),XSCRT(2),XSCRT(3)
   54   FORMAT('XSCRT(1),XSCRT(2),XSCRT(3) = ',3G15.7)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI5
      SUBROUTINE DPCLI6(IOUNIT,ISTR,MAXCHR,IBUGS2,ISUBRO,IERROR)
!
      CHARACTER*(*) ISTR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IERROR
!
      INCLUDE 'DPCOBE.INC'
      INCLUDE 'DPCOP2.INC'
!
      IF(IBUGS2.EQ.'ON' .OR. ISUBRO.EQ.'CLI6')THEN
        WRITE(ICOUT,52)IOUNIT,MAXCHR,ISTR(1:4),IBUGS2,ISUBRO,IERROR
   52   FORMAT('IOUNIT,MAXCHR,ISTR(1:4),IBUGS2,ISUBRO,IERROR = ',   &
               2I8,3(A4,2X),A4)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      RETURN
      END SUBROUTINE DPCLI6
#endif
      SUBROUTINE DPCDIR(IANS,IANSLC,IWIDTH,   &
                        IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
                        IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
                        IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--CHANGE THE CURRENT DIRECTORY.  NOTE THAT THIS COMMAND
!              IS SITE AND HOST DEPENDENT.  IT IS PROVIODED AS A
!              CONVENIENCE FUNCTION.  FOR EXAMPLE, THE WINDOWS NT
!              VERSION SETS THE CURRENT DIRECTORY TO THE DIRECTORY
!              WHERE THE DATAPLOT EXECUTABLE RESIDES.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--97.8
!     ORIGINAL VERSION--AUGUST     1997.
!
!-----NON-COMMON VARIABLES -------------------------------------------
!
#ifdef INTEL
      USE MSFLIB
      LOGICAL ISTATUS
#endif
!CCCC LOGICAL IRESLT
      CHARACTER*4 IANS
      CHARACTER*4 IANSLC
!
      CHARACTER*4 ITEXTE
      CHARACTER*4 ITEXTF
!
      CHARACTER*4 IHNAME
      CHARACTER*4 IHNAM2
      CHARACTER*4 IUSE
!
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      CHARACTER*4 IFUNC
      CHARACTER*1 IREPCH
!
      DIMENSION IANS(*)
      DIMENSION IANSLC(*)
!
      PARAMETER(MAXCH=256)
      DIMENSION ITEXTE(MAXCH)
      DIMENSION ITEXTF(MAXCH)
      CHARACTER*256 ITEXT2
      CHARACTER*256 ITEXT3
!
      DIMENSION IHNAME(*)
      DIMENSION IHNAM2(*)
      DIMENSION IUSE(*)
      DIMENSION IVALUE(*)
      DIMENSION VALUE(*)
      DIMENSION IVSTAR(*)
      DIMENSION IVSTOP(*)
      DIMENSION IFUNC(*)
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOBE.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
!
      IFOUND='NO'
      IERROR='NO'
!
      ITEXT2=' '
      ITEXT3=' '
      J2=0
!
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
        DO 76 I=1,NUMNAM
          WRITE(ICOUT,77)I,IHNAME(I),IHNAM2(I),IUSE(I),   &
                         IVALUE(I),VALUE(I)
   77     FORMAT('I,IHNAME(I),IHNAM2(I),IUSE(I),IVALUE(I),VALUE(I)= ',   &
                 I8,3(2X,A4),I8,G15.7)
          CALL DPWRST('XXX','BUG ')
   76   CONTINUE
      ENDIF
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (CD)  AND ITS LOCATION             **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
!  CHECK FOR "CD" FIRST
!
      DO 1115 I=1,IWIDTH-1
        ISTART=I+2
        IF(IANS(I).EQ.'C'.AND.IANS(I+1).EQ.'D'.AND.   &
           IANS(I+2).EQ.' ')GO TO 1190
 1115 CONTINUE
!
!     NO MATCH
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DPCD--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL CD')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1190 CONTINUE
!
!               *******************************************************
!               **  STEP 1.2--                                       **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
!               *******************************************************
!
      IFOUND='YES'
!
      ISTOP=0
      IF(ISTART.LE.IWIDTH)THEN
        DO 1220 I=ISTART,IWIDTH
          IREV=IWIDTH-I+ISTART
          IF(IANS(IREV).NE.' ')THEN
            ISTOP=IREV
            GO TO 1229
          ENDIF
 1220   CONTINUE
 1229   CONTINUE
      ENDIF
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
      NCTEX=0
      IF(ISTART.LE.ISTOP .AND. ISTOP.GT.0)THEN
        ITEMP=ISTOP-ISTART+1
        IF(ITEMP.GT.MAXCH)ITEMP=MAXCH
        ISTOP=ISTART+ITEMP-1
!
        J=0
        DO 1310 I=ISTART,ISTOP
          J=J+1
          J2=J
          ITEXTE(J)=IANS(I)
          ITEXTF(J)=IANSLC(I)
 1310   CONTINUE
        NCTEX=J2
      ENDIF
!
!               ******************************************************
!               **  STEP 1.4--                                      **
!               **  CALL THE SUBROUTINE DPREPL                      **
!               **  WHICH WILL SCAN THE STRING FOR ALL OCCURRANCES  **
!               **  OF THE SUBSTRING VALU()                         **
!               **  AND REPLACE THEM BY THEIR LITERAL VALUES.       **
!               ******************************************************
!
      NCTEXT=NCTEX
      IF(NCTEXT.GE.1)CALL DPREPL(ITEXTE,NCTEXT,   &
      IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,   &
      IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,   &
      IBUGD2,IERROR)
      IF(NCTEXT.GE.1)THEN
        DO 1510 I=1,NCTEXT
          ITEXT2(I:I)=ITEXTE(I)(1:1)
 1510   CONTINUE
      ENDIF
!
      NCTEXT=NCTEX
!CCCC IF(NCTEXT.GE.1)CALL DPREPL(ITEXTF,NCTEXT,
!CCCC1IHNAME,IHNAM2,IUSE,IVALUE,VALUE,NUMNAM,
!CCCC1IVSTAR,IVSTOP,IFUNC,NUMCHF,IREPCH,
!CCCC1IBUGD2,IERROR)
      IF(NCTEXT.GE.1)THEN
        DO 1610 I=1,NCTEXT
          ITEXT3(I:I)=ITEXTF(I)(1:1)
 1610   CONTINUE
      ENDIF
!
!               ********************************
!               **  STEP 2--                  **
!               **  STEP THROUGH EACH HOST    **
!               ********************************
!
      IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
!
!       ****************************************************
!       *  IBM/PC 386 - MICROSOFT WINDOWS 95/NT COMPILER   *
!       ****************************************************
!
#ifdef INTEL
        ISTATUS=.TRUE.
        DO 2510 I=1,NCTEXT
          ISTART=I
          IF(ITEXT3(I:I).NE.' ')GO TO 2519
 2510   CONTINUE
 2519   CONTINUE
        DO 2520 I=NCTEXT,1,-1
          ISTOP=I
          IF(ITEXT3(I:I).NE.' ')GO TO 2529
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
 2503     FORMAT('*****WARNING: DATAPLOT WAS UNSUCCESSFUL IN ',   &
                 'CHANGING THE CURRENT DIRECTORY')
          CALL DPWRST('XXX','BUG ')
        ENDIF
#endif
        GO TO 9000
      ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
!
!       *********************************************
!       *  UNIX/LINUX - USE CHDIR CALL              *
!       *********************************************
!
#ifdef LINUX
        DO 2310 I=1,NCTEXT
          ISTART=I
          IF(ITEXT3(I:I).NE.' ')GO TO 2319
 2310   CONTINUE
 2319   CONTINUE
        DO 2320 I=NCTEXT,1,-1
          ISTOP=I
          IF(ITEXT3(I:I).NE.' ')GO TO 2329
 2320   CONTINUE
 2329   CONTINUE
!
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
 2303     FORMAT('*****WARNING: DATAPLOT WAS UNSUCCESSFUL IN ',   &
                 'CHANGING THE CURRENT DIRECTORY')
          CALL DPWRST('XXX','BUG ')
        ENDIF
#endif
        GO TO 9000
      ELSEIF(IHOST1.EQ.'NVE')THEN
!
!       *********************************************************
!       *  CDC - NOS/VE OPERATING CD  EM.  USE "SCLCMD" TO PASS *
!       *  COMMANDS TO THE OPERATING CD  EM.                    *
!       *  DATAPLOT WILL DO NO ERROR CHECKING ON THE COMMAND    *
!       *********************************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2111)
 2111   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2112)
 2112   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2113)
 2113   FORMAT('      HAS NOT YET BEEN IMPLEMEMNTED FOR THE ',   &
               'NOS/VE VERSION')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IHOST1.EQ.'VAX')THEN
!
!       **********************************************
!       *  VAX/VMS - LEFT TO IMPLEMENTOR             *
!       **********************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2211)
 2211   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2212)
 2212   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2213)
 2213   FORMAT('      HAS NOT YET BEEN IMPLEMEMNTED FOR THE ',   &
               'VAX/VMS VERSION')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSEIF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'OTG ')THEN
!
!       *****************************************
!       *  IBM/PC 386 - OTG COMPILER            *
!       *****************************************
!
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2411)
 2411   FORMAT('***** ERROR IN DPCD--')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2412)
 2412   FORMAT('      THE INTERFACE TO CD OPERATIONS')
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,2413)
 2413   FORMAT('      HAS NOT YET BEEN IMPLEMEMNTED FOR THE ',   &
               'OTG VERSION')
        CALL DPWRST('XXX','BUG ')
        GO TO 9000
      ELSE
!
!       *********************************************************
!       *  OTHER   - LEFT TO IMPLEMENTOR                        *
!       *********************************************************
!
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
        GO TO 9000
      ENDIF
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPCDIR
      SUBROUTINE DPPWD(CURDIR,MAXNCH,NLAST,IBUGS2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--RETURN CURRENT WORKING DIRECTORY.  THIS ROUTINE
!              UTILIZES THE "GETCWD" LIBRARY CALL.  THIS IS NOT
!              PART OF THE FORTRAN 90 STANDARD, BUT IT SHOULD BE
!              AVAILABLE ON MOST CURRENTLY SUPPORTED PLATFORMS.
!
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899-8980
!                 PHONE--301-975-2855
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1990)
!     VERSION NUMBER--2011/1
!     ORIGINAL VERSION--JANUARY   2011.
!
!-----CHARACTER STATEMENTS FOR NON-COMMON VARIABLES-------------------
!
      CHARACTER (LEN=*) :: CURDIR
      CHARACTER*4 IBUGS2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
!-----COMMON----------------------------------------------------------
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='YES'
      IERROR='NO'
!
      IF(IBUGS2.EQ.'ON'.OR.ISUBRO.EQ.'PPWD')THEN
        WRITE(ICOUT,999)
  999   FORMAT(1X)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,51)
   51   FORMAT('AT THE BEGINNING OF DPPWD')
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
      CURDIR=' '
      CALL GETCWD(CURDIR)
      NLAST=MAXNCH
!
      DO 100 I=MAXNCH,1,-1
        IF(CURDIR(I:I).NE.' ')THEN
          NLAST=I
          GO TO 109
        ENDIF
  100 CONTINUE
  109 CONTINUE
!
      IF(IFEEDB.EQ.'ON')THEN
        WRITE(ICOUT,111)CURDIR(1:NLAST)
  111   FORMAT('THE CURRENT WORKING DIRECTORY IS: ',A)
        CALL DPWRST('XXX','BUG ')
      ENDIF
!
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
!
      RETURN
      END SUBROUTINE DPPWD
      SUBROUTINE DPRM(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--DELETE FILES OR DIRECTORIES USING OPERATING SYSTEM
!              COMMAND.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2019.09
!     ORIGINAL VERSION--SEPTEMBER  2019.
!
!-----NON-COMMON VARIABLES -------------------------------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
      CHARACTER*4 ICASE
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
!
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
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (CD)  AND ITS LOCATION             **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
!  CHECK FOR "RM" FIRST
!
      ISTRT=1
      DO 1115 I=1,IWIDTH-1
        IF(IANS(I).EQ.'R'.AND.IANS(I+1).EQ.'M'.AND.   &
           IANS(I+2).EQ.' ')THEN
          ICASE='FILE'
          ISTRT=I+3
          GO TO 1190
        ELSEIF(IANS(I).EQ.'R'   .AND. IANS(I+1).EQ.'M' .AND.   &
               IANS(I+2).EQ.'D' .AND. IANS(I+3).EQ.'I' .AND.   &
               IANS(I+4).EQ.'R' .AND. IANS(I+5).EQ.' ')THEN
          ICASE='DIRE'
          ISTRT=I+6
          GO TO 1190
        ENDIF
 1115 CONTINUE
!
!     NO MATCH
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN RM (DPRM)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO RM')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1190 CONTINUE
!
!               *******************************************************
!               **  STEP 1.2--                                       **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
!               *******************************************************
!
      IFOUND='YES'
!
      ISTOP=0
      DO 1220 I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GO TO 1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
!
      IF(ISTOP.LT.ISTRT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)
 1223   FORMAT('      NO ARGUMENT FOR RM/RMDIR COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
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
 1291   FORMAT('      RM/RMDIR COMMAND NOT SUPPORTED ON THIS ',   &
               'PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 1310 I=ISTRT,ISTOP
        NCSTR=NCSTR+1
        IF(NCSTR.GT.MAXCH)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)MAXCH
 1293     FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,   &
                 ') EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310 CONTINUE
!
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISYSPE='OFF'
      ISYSHI='ON'
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPRM
      SUBROUTINE DPMKDR(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--MAKE A NEW DIRECTORY.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2019.09
!     ORIGINAL VERSION--SEPTEMBER  2019.
!
!-----NON-COMMON VARIABLES -------------------------------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
!
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
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (CD)  AND ITS LOCATION             **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
!  CHECK FOR "MKDIR" FIRST
!
      ISTRT=1
      DO 1115 I=1,IWIDTH-1
        IF(IANS(I).EQ.'M'   .AND. IANS(I+1).EQ.'K' .AND.   &
               IANS(I+2).EQ.'D' .AND. IANS(I+3).EQ.'I' .AND.   &
               IANS(I+4).EQ.'R' .AND. IANS(I+5).EQ.' ')THEN
          ISTRT=I+6
          GO TO 1190
        ENDIF
 1115 CONTINUE
!
!     NO MATCH
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN MKDIR (DPMKDR)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO MKDIR')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1190 CONTINUE
!
!               *******************************************************
!               **  STEP 1.2--                                       **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
!               *******************************************************
!
      IFOUND='YES'
!
      ISTOP=0
      DO 1220 I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GO TO 1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
!
      IF(ISTOP.LT.ISTRT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)
 1223   FORMAT('      NO ARGUMENT FOR MKDIR COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
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
 1291   FORMAT('      MKDIR COMMAND NOT SUPPORTED ON THIS ',   &
               'PLATFORM.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
      DO 1310 I=ISTRT,ISTOP
        NCSTR=NCSTR+1
        IF(NCSTR.GT.MAXCH)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)MAXCH
 1293     FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,   &
                 ') EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310 CONTINUE
!
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPMKDR
      SUBROUTINE DPCAT(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--USE OPERATING SYSTEM COMMAND TO DISPLAY CONTENTS OF A
!              FILE.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2019.09
!     ORIGINAL VERSION--SEPTEMBER  2019.
!
!-----NON-COMMON VARIABLES -------------------------------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
!
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
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (CD)  AND ITS LOCATION             **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
!  CHECK FOR "CAT" FIRST
!
      ISTRT=1
      DO 1115 I=1,IWIDTH-1
        IF(IANS(I).EQ.'C'   .AND. IANS(I+1).EQ.'A'.AND.   &
           IANS(I+2).EQ.'T' .AND. IANS(I+3).EQ.' ')THEN
          ISTRT=I+4
          GO TO 1190
        ELSEIF(IANS(I).EQ.'T'   .AND. IANS(I+1).EQ.'Y' .AND.   &
               IANS(I+2).EQ.'P' .AND. IANS(I+3).EQ.'E' .AND.   &
               IANS(I+4).EQ.' ')THEN
          ISTRT=I+5
          GO TO 1190
        ENDIF
 1115 CONTINUE
!
!     NO MATCH
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN CAT (DPCAT)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO CAT (OR TYPE)')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1190 CONTINUE
!
!               *******************************************************
!               **  STEP 1.2--                                       **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
!               *******************************************************
!
      IFOUND='YES'
!
      ISTOP=0
      DO 1220 I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GO TO 1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
!
      IF(ISTOP.LT.ISTRT)THEN
        WRITE(ICOUT,999)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1181)
        CALL DPWRST('XXX','BUG ')
        WRITE(ICOUT,1223)
 1223   FORMAT('      NO ARGUMENT FOR CAT COMMAND.')
        CALL DPWRST('XXX','BUG ')
        IERROR='YES'
        GO TO 9000
      ENDIF
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
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
        GO TO 9000
      ENDIF
!
      DO 1310 I=ISTRT,ISTOP
        NCSTR=NCSTR+1
        IF(NCSTR.GT.MAXCH)THEN
          WRITE(ICOUT,999)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1181)
          CALL DPWRST('XXX','BUG ')
          WRITE(ICOUT,1293)MAXCH
 1293     FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,   &
                 ') EXCEEDED.')
          CALL DPWRST('XXX','BUG ')
          IERROR='YES'
          GO TO 9000
        ENDIF
        ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310 CONTINUE
!
      IF(ICATMO.EQ.'ON' .AND. NCSTR.LE.248)THEN
        IF(IHOST1.EQ.'IBM-'.AND.ICOMPI.EQ.'MS-F')THEN
          ITEXT2(NCSTR+1:NCSTR+7)=' | more'
          NCSTR=NCSTR+7
        ELSEIF(IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU')THEN
          ITEXT2(NCSTR+1:NCSTR+7)=' | more'
          NCSTR=NCSTR+7
        ENDIF
      ENDIF
!
      ISSAV1=ISYSPE
      ISSAV2=ISYSHI
      ISYSPE='ON'
      ISYSHI='OFF'
      CALL DPSYS2(ITEXT2,NCSTR,ISUBRO,IERROR)
      ISYSPE=ISSAV1
      ISYSHI=ISSAV2
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPCAT
      SUBROUTINE DPDIR(IANS,IANSLC,IWIDTH,IBUGD2,ISUBRO,IFOUND,IERROR)
!
!     PURPOSE--USE OPERATING SYSTEM COMMAND TO PERFORM A "DIR" COMMAND
!              UNDER WINDOWS AND A "ls" COMMAND UNDER LINUX.
!     WRITTEN BY--ALAN HECKERT
!                 STATISTICAL ENGINEERING DIVISION
!                 INFORMATION TECHNOLOGY LABORATORY
!                 NATIONAL INSTIUTE OF STANDARDS AND TECHNOLOGY
!                 GAITHERSBURG, MD 20899
!                 PHONE--301-975-2899
!     NOTE--DATAPLOT IS A REGISTERED TRADEMARK
!           OF THE NATIONAL BUREAU OF STANDARDS.
!     LANGUAGE--ANSI FORTRAN (1977)
!               HOST DEPENDENT
!     VERSION NUMBER--2019.09
!     ORIGINAL VERSION--SEPTEMBER  2019.
!
!-----NON-COMMON VARIABLES -------------------------------------------
!
      CHARACTER*4 IANS(*)
      CHARACTER*4 IANSLC(*)
      CHARACTER*4 IBUGD2
      CHARACTER*4 ISUBRO
      CHARACTER*4 IFOUND
      CHARACTER*4 IERROR
!
      PARAMETER(MAXCH=256)
      CHARACTER*256 ITEXT2
      CHARACTER*4 ISSAV1
      CHARACTER*4 ISSAV2
      CHARACTER*4 ICLESV
!
!-----COMMON----------------------------------------------------------
!
      INCLUDE 'DPCOHO.INC'
      INCLUDE 'DPCOST.INC'
!
!-----COMMON VARIABLES (GENERAL)--------------------------------------
!
      INCLUDE 'DPCOP2.INC'
!
!-----START POINT-----------------------------------------------------
!
      IFOUND='NO'
      IERROR='NO'
      ITEXT2=' '
!
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
!
!               *****************************************************
!               **  STEP 1--                                       **
!               **  EXTRACT THE TEXT STRING FROM THE COMMAND LINE  **
!               *****************************************************
!
!               *****************************************
!               **  STEP 1.1--                         **
!               **  DETERMINE THE COMMAND              **
!               **  (CD)  AND ITS LOCATION             **
!               **  ON THE LINE.                       **
!               **  DETERMINE THE START POSITION       **
!               **  (XSTART) OF THE FIRST CHARACTER    **
!               **  FOR THE STRING TO BE PRINTED.      **
!               *****************************************
!
!  CHECK FOR "CAT" FIRST
!
      ISTRT=1
      DO 1115 I=1,IWIDTH-1
        IF(IANS(I).EQ.'D'   .AND. IANS(I+1).EQ.'I'.AND.   &
           IANS(I+2).EQ.'R' .AND. IANS(I+3).EQ.' ')THEN
          ISTRT=I+4
          GO TO 1190
        ELSEIF(IANS(I).EQ.'L'   .AND. IANS(I+1).EQ.'S' .AND.   &
               IANS(I+2).EQ.' ')THEN
          ISTRT=I+3
          GO TO 1190
        ENDIF
 1115 CONTINUE
!
!     NO MATCH
!
      WRITE(ICOUT,999)
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1181)
 1181 FORMAT('***** ERROR IN DIR (DPDIR)--')
      CALL DPWRST('XXX','BUG ')
      WRITE(ICOUT,1182)
 1182 FORMAT('      COMMAND NOT EQUAL TO DIR (OR LS)')
      CALL DPWRST('XXX','BUG ')
      IERROR='YES'
      GO TO 9000
 1190 CONTINUE
!
!               *******************************************************
!               **  STEP 1.2--                                       **
!               **  DEFINE THE STOP  POSITION (ISTOP) FOR THE STRING.**
!               *******************************************************
!
      IFOUND='YES'
!
      ISTOP=0
      DO 1220 I=IWIDTH,ISTRT,-1
        IF(IANS(I)(1:1).NE.' ')THEN
          ISTOP=I
          GO TO 1229
        ENDIF
 1220 CONTINUE
 1229 CONTINUE
!
!               *****************************************
!               **  STEP 1.3--                         **
!               **  COPY OVER THE STRING OF INTEREST.  **
!               *****************************************
!
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
        GO TO 9000
      ENDIF
!
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
!
      IF(ISTRT.LE.ISTOP)THEN
        DO 1310 I=ISTRT,ISTOP
          NCSTR=NCSTR+1
          IF(NCSTR.GT.MAXCH)THEN
            WRITE(ICOUT,999)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1181)
            CALL DPWRST('XXX','BUG ')
            WRITE(ICOUT,1293)MAXCH
 1293       FORMAT('      MAXIMUM NUMBER OF CHARACTERS (',I3,   &
                   ') EXCEEDED.')
            CALL DPWRST('XXX','BUG ')
            IERROR='YES'
            GO TO 9000
          ENDIF
          ITEXT2(NCSTR:NCSTR)=IANSLC(I)(1:1)
 1310   CONTINUE
      ENDIF
!
      IF((IOPSY1.EQ.'UNIX' .OR. IOPSY1.EQ.'LINU') .AND.   &
        IDIRPA.EQ.'ON' .AND. NCSTR.LE.248)THEN
        ITEXT2(NCSTR+1:NCSTR+7)=' | more'
        NCSTR=NCSTR+7
      ENDIF
!
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
!
!               *****************
!               **  STEP 90--  **
!               **  EXIT       **
!               *****************
!
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
!
      RETURN
      END SUBROUTINE DPDIR

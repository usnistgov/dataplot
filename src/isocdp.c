/*
 * isocdp  - This subroutine provides an interface to the
 *           isocline subroutine.  This is an alternative to
 *           readline.  Intended primarily for the Windows
 *           platform (the Intel Fortran compiler is incompatible
 *           with libraries built using gcc under mingw/mysys).
 *
 *           We use readline in a basic way (just to get command line
 *           editing and history recall).  We do not add any custom
 *           features.  Also, we are only interested in supporting
 *           standard input.  Command line arguments are not currently
 *           supported.  This routine is intended to provide
 *           comparable capability using the isocline software.
 *
 *        For Dataplot return the character string as an integer
 *        array of ASCII decimal equivalents.  This is done for
 *        portability reasons (with some loss in efficiency).  Compilers
 *        are not consistent in how strings are passed when calling C
 *        from Fortran.
 *
 */

/*
 *  Note that calling C from Fortran is not standard.  I have
 *  provided the following compiler defintions to enhance portability.
 *
 *  1) The default is to assume that the Fortran compiler appends an
 *     underscore to the routine name.  Use -DNOUNDERSCORE if your
 *     compiler does not append the underscore.
 *  2) The default is to assume that the Fortran compiler converts
 *     routine names to lower case.  Use -DUPPERCASE if your
 *     Fortran compiler does not do this (e.g., the Cray).
 *  3) Character strings are the most troublesome issue for
 *     portability.  Passing character strings from Fortran to C
 *     is very compiler dependent.  I have addressed this issue
 *     by passing character strings as arrays of ASCII Decimal
 *     Equivalents (ADE's).  That is, the Fortran converts a
 *     character string to an array of the integer values where
 *     the integer is the ASCII collating sequence (i.e., A = 65,
 *     B = 66, etc.).  The end of the string is denoted by setting
 *     the value to 0.  This is easily accomplished on the Fortran
 *     side by using the ICHAR function.  The C code here then
 *     calls an internal routine to covnert the integer array to
 *     a C string.   Although a bit convoluted, this avoids a lot
 *     of messy portability issues.
 *
 *  Default is an underscore and lower case.  The compiler specified
 *  definitions -DNOUNDERSCORE and -DUPPERCASE can be specified to
 *  override these defaults.
 *
 */

#ifdef INTEGER8
#define INTEGER_PRECISION 1
#else
#define INTEGER_PRECISION 0
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include "isocline.h"

void isocdp(int *nchar,int *ierror,int istring[],int *icall);

/*
 *  isocdp is the routine that Datatplot calls.
 *
 *  A few comments:
 *
 *  1) Dataplot only uses isocline for reading terminal input.
 *  2) Dataplot will use ">" for the prompt.  Currently, this is
 *     not settable by the user.
 *  3) The maximum line length that Dataplot can currently handle
 *     is 255 characters.
 *
 *  The arguments are:
 *
 *  istring    - an integer array that contains the ASCII Decimal
 *               Equivalents to the character string returned from
 *               readline.
 *  nchar      - an integer that specifies the number of characters
 *               in the returned string.
 *  ierror     - an integer with the status (0 = normal status,
 *               1 = error)
 */
void isocdp(int *nchar,int *ierror,int istring[],int *icall)

{
     *ierror = 0;
     *nchar = 0;

  char *input, *prompt;
  static char *progname;
  int opt, fd, nch, nchmax, icall_temp;

  icall_temp = *icall;

  setlocale(LC_ALL,"C.UTF-8");  // use utf-8
  if (icall_temp == 0) {
     // enable history; use a NULL filename to not persist history to disk
     ic_set_history("history.txt", -1 /* default entries (= 200) */);
  }



  progname = "Dataplot";

  /* defaults */
  fd = nch = 0;
  nchmax=255;

  input = ic_readline(">");
  /* printf("input: %s\n",input); */

  /* For Dataplot, convert to ASCII Decimal Equivalents and return
   * the number of characters.
   */

  int i;
  int itemp;
  *nchar = 0;
  i = 0;
  while (input[i] != 0 && i < nchmax) {
       itemp = input[i];
       istring[i] = itemp;
       *nchar=i+1;
       i++;
  }
  free(input);

}

/*
 * dprl - This subroutine provides an interface to the
 *        readline library for the Dataplot program.
 *        It is adapted from the "rl.c" and "histexamp.c"
 *        programs.
 *
 *        We use readline in a basic way (just to get command line
 *        editing and history recall).  We do not add any custom
 *        features.  Also, we are only interested in supporting standard
 *        input.  Command line arguments are not currently supported.
 *
 *        For Dataplot return the character string as an integer
 *        array of ASCII decimal equivalents.  This is done for
 *        portability reasons (with some loss in efficiency).  Compilers
 *        are not consistent in how strings are passed when calling C
 *        from Fortran.
 *
 */
/* Copyright (C) 1987-2002 Free Software Foundation, Inc.

   This file is part of the GNU Readline Library, a library for
   reading lines of text with interactive input and history editing.

   The GNU Readline Library is free software; you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2, or
   (at your option) any later version.

   The GNU Readline Library is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   The GNU General Public License is often shipped with GNU software, and
   is generally kept in a file called COPYING or LICENSE.  If you do not
   have a copy of the license, write to the Free Software Foundation,
   59 Temple Place, Suite 330, Boston, MA 02111 USA. */

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

#ifdef NOUNDERSCORE
#define APPEND_UNDERSCORE 0
#else
#define APPEND_UNDERSCORE 1
#endif
#ifdef UPPERCASE
#define SUBROUTINE_CASE 0
#else
#define SUBROUTINE_CASE 1
#endif

#ifdef INTEGER8
#define INTEGER_PRECISION 1
#else
#define INTEGER_PRECISION 0
#endif

#if defined (HAVE_CONFIG_H)
#  include <config.h>
#endif

#include <stdio.h>
#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#  include <stdlib.h>
#else 
extern void exit();
#endif

#if defined (READLINE_LIBRARY)
#  include "posixstat.h"
#  include "readline.h"
#  include "history.h"
#else
#  include <sys/stat.h>
#  include <readline/readline.h>
#  include <readline/history.h>
#endif

extern int optind;
extern char *optarg;

#if !defined (strchr) && !defined (__STDC__)
extern char *strrchr();
#endif

static char *progname;
static char *deftext;

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  rldp_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  RLDP_();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  rldp();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  RLDP();
#endif

static int set_deftext ()
{
  if (deftext)
    {
      rl_insert_text (deftext);
      deftext = (char *)NULL;
      rl_startup_hook = (rl_hook_func_t *)NULL;
    }
  return 0;
}

/* For Dataplot purposes, just return an error flag, do not
 * print a message.  So comment out this routine.
 */
/*
static void usage()
{
  fprintf (stderr, "%s: usage: %s [-p prompt] [-u unit] [-d default] [-n nchars]\n",
		progname, progname);
}
*/

/*
 *  rldp is the routine that Datatplot calls.
 *
 *  A few comments:
 *
 *  1) Dataplot only uses readline for reading terminal input.
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
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void rldp_(nchar,ierror,istring)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void RLDP_(nchar,ierror,istring)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void rldp(nchar,ierror,istring)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void RLDP(nchar,ierror,istring)
#endif

int istring[];
#if INTEGER_PRECISION == 0
int *nchar;
int *ierror;
#else
int nchar[2];
int ierror[2];
#endif
{
#if INTEGER_PRECISION == 0
     *ierror = 0;
     *nchar = 0;
#else
     ierror[0] = 0;
     nchar[0] = 0;
#endif

  char *temp, *prompt;
  struct stat sb;
  int opt, fd, nch, nchmax;

  progname = "Dataplot";

  /* defaults */
  prompt = ">";
  fd = nch = 0;
  deftext = (char *)0;
  nchmax=255;

  if (deftext && *deftext)
    rl_startup_hook = set_deftext;

  if (nch > 0)
    rl_num_chars_to_read = nch;

  temp = readline (prompt);

  /* For Dataplot, convert to ASCII Decimal Equivalents and return
   * the number of characters.
   */

  int i;
  int itemp;
#if INTEGER_PRECISION == 0
  *nchar = 0;
#else
  nchar[0] = 0;
#endif
  i = 0;
  while (temp[i] != 0 && i < nchmax) {
       itemp = temp[i];
       istring[i] = itemp;
#if INTEGER_PRECISION == 0
       *nchar=i+1;
#else
       nchar[0]=i+1;
#endif
       i++;
  }

#if INTEGER_PRECISION == 0
 if (*nchar > 0)
    add_history(temp);
#else
 if (nchar[0] > 0)
    add_history(temp);
#endif

}

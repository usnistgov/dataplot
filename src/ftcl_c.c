/* DOC
 *
 *  ftcl_c.c  - routines to interface between Fortran and Tcl/Tk
 *
 *  Copyright (C) 1999 Arjen Markus
 *
 *  Arjen Markus
 *
 *
 *  General information:
 *  This file contains the following routines:
 *  - Ftcl_Init():        Initialisation for Ftcl package
 *  - Ftcl_ExecCmd():     Implement the Tcl "ftcl_exec" command
 *  - Ftcl_ConfigCmd():   Implement the Tcl "ftcl_config" command
 *  - ftcl_get_arg_*():   Get the integer/real/.. value from an argument
 *  - ftcl_get_*():       Get the value of an integer/real/.. Tcl variable
 *  - ftcl_put_*():       Put the value into an integer/real/.. Tcl variable
 *  - ftcl_set_result_*() Set the result to an integer/real/.. value
 *  - ftcl_script():      Execute a Tcl script
 *  - ftcl_start():       Initialise in case of embedded Tcl
 *  - fctl_conv_str_from_fort(): Convert a string from Fortran format
 *                        to C format
 *  - fctl_conv_str_to_fort(): Convert a string from C format to Fortran
 *                        format
 *  - ftcl_debug_message: Present a short description of what went wrong
 *  - ftcl_trace_*():     Present a short description of the calls and
 *                        their results
 *  - fctl_init_log():    Register the Fortran logical values
 *
 *  Note:
 *  The code uses the following macros to control the checks:
 *
 * ENDDOC
 */

/*  $Author$
 *  $Date$
 *  $Source$
 *  $Log$
 */

/* Include files - see "ftcl.h" for details
*/
#include "ftcl.h"

/* Prototypes of static routines
*/
static char *ftcl_conv_str_from_fort( char *string,  FTNLEN   length  ) ;
static void  ftcl_conv_str_to_fort(   char *string,  char    *fstr,
                                      int   length                    ) ;
static void  ftcl_debug_message(      char *string1, char    *string2 ) ;
static void  ftcl_trace_int(          char *routine, char    *var,
                                                     integer  value   ) ;
static void  ftcl_trace_real(         char *routine, char    *var,
                                                     real     value   ) ;
static void  ftcl_trace_double(       char *routine, char    *var,
                                               doublereal     value   ) ;
static void  ftcl_trace_string(       char *routine, char    *var,
                                                     char    *value   ) ;
static void  ftcl_trace_log(          char *routine, char    *var,
                                                     logical  value   ) ;
FOR_RETURN FOR_CALL ftcl_init_log(    logical *ftcl_true,
                                      logical *ftcl_false ) ;
static void  ftcl_trace_args(         char *routine, int      objc,
                                                     Tcl_Obj *CONST objv[] ) ;


static int   Ftcl_ConfigCmd( ClientData client_data, Tcl_Interp *interp,
                            int objc, struct Tcl_Obj * CONST objv[] )  ;
static int   Ftcl_ExecCmd(   ClientData client_data, Tcl_Interp *interp,
                            int objc, struct Tcl_Obj * CONST objv[] )  ;
static int   Ftcl_GenericCmd(ClientData client_data, Tcl_Interp *interp,
                            int objc, struct Tcl_Obj * CONST objv[] )  ;

/* Local macro definitions
*/
#define FTCL_BUFSIZE 256

/* Static variables
*/
static logical     ftcl_false              ; /* Fortran false value             */
static logical     ftcl_true               ; /* Fortran true value              */

static int         ftcl_debug          = 0 ; /* Is debugging required           */
static int         ftcl_exec_is_active = 0 ; /* Preclude recursive calls        */
static int         ftcl_trace              ; /* Trace on or off                 */
static int         ftcl_err                ; /* Error status                    */
static integer     ftcl_number_args        ; /* Number of arguments to ftcl_exec */
static FILE       *ftcl_outfile            ; /* Output file for debug/trace      */
static Tcl_Obj    *CONST *ftcl_args        ; /* Arguments to ftcl_exec           */
static Tcl_Interp *ftcl_interp             ; /* Current interpreter              */

/* --------------------------------------------------------------------
    Routine:  Ftcl_Init()
    Author:   Arjen Markus
    Purpose:  Initialisation for Ftcl package
    Context:  Used by Tcl interpreter
    Summary:
              Check if the right number of arguments is given.
              Check if this is not a recursive call
              Call the Fortran routine ftcl_exec for the actual
              work
-------------------------------------------------------------------- */
int Ftcl_Init( Tcl_Interp *interp )
{
   int retcode ;

/* Register the Fortran logical values
*/
   ftcl_init_log( &ftcl_true, &ftcl_false ) ;

/* Initialise the stubs
*/
#if 0
    if (Tcl_InitStubs(interp, "8.0", 0) == NULL) {
       return TCL_ERROR;
    }
#endif
/* Inquire about the package's version
*/
    if (Tcl_PkgRequire(interp, "Tcl", TCL_VERSION, 0) == NULL)
    {
       if (TCL_VERSION[0] == '7')
       {
          if (Tcl_PkgRequire(interp, "Tcl", "8.0", 0) == NULL)
          {
             return TCL_ERROR;
          }
       }
    }

    if (Tcl_PkgProvide(interp, "Ftcl", VERSION) != TCL_OK)
    {
       return TCL_ERROR;
    }

/* Register the Ftcl commands
*/
   retcode = TCL_OK ;

   if ( Tcl_CreateObjCommand( interp, "ftcl_exec", Ftcl_ExecCmd,
           (ClientData) 0L, NULL ) == NULL )
   {
      retcode = TCL_ERROR ;
   }

   if ( Tcl_CreateObjCommand( interp, "ftcl_config",
           Ftcl_ConfigCmd, (ClientData) 0L, NULL )  == NULL )
   {
      retcode = TCL_ERROR ;
   }

   return retcode ;
}

/* --------------------------------------------------------------------
    Routine:  Ftcl_ExecCmd()
    Author:   Arjen Markus
    Purpose:  Implement the Tcl "ftcl_exec" command
    Context:  Used by Tcl interpreter
    Summary:
              Check if the right number of arguments is given.
              Check if this is not a recursive call
              Call the Fortran routine ftcl_exec for the actual
              work
-------------------------------------------------------------------- */
static int Ftcl_ExecCmd( ClientData client_data, Tcl_Interp *interp,
                         int objc, struct Tcl_Obj * CONST objv[] )
{
   integer  ierror             ; /* Passed to Fortran */
   int      length             ;
   char     first_arg[FTCL_BUFSIZE] ;
   char    *pstr                    ;

/* Argument count includes the command name
*/
   if ( objc < 2 )
   {
      Tcl_SetResult( interp, "wrong # args: ftcl_exec service ?args" ,
         NULL ) ;
      return TCL_ERROR ;
   }
   if ( ftcl_exec_is_active != 0 )
   {
      Tcl_SetResult( interp, "recursive use of ftcl_exec not supported" ,
         NULL ) ;
      return TCL_ERROR ;
   }

   ftcl_exec_is_active ++         ;
   ftcl_number_args    = objc - 2 ;
   ftcl_args           = objv     ;
   ftcl_interp         = interp   ;

/* Get the first argument and call the Fortran routine
*/
   pstr = Tcl_GetStringFromObj( objv[1], &length ) ;
   ftcl_conv_str_to_fort( pstr, first_arg, sizeof(first_arg) );

   if ( ftcl_trace )
   {
      ftcl_trace_args( "ftcl_exec", objc, objv ) ;
   }

/* ---- Quik and dirty ----
   ftcl_exec( first_arg,
#ifdef IN_BETWEEN
      (FTNLEN) sizeof( first_arg),
#endif
      &ftcl_number_args,
      &ierror
#ifndef IN_BETWEEN
     ,(FTNLEN) sizeof( first_arg)
#endif
                                  ) ;
---- Quik and dirty ---- */

   ftcl_exec_is_active -- ;

   if ( ierror != 0 )
   {
      Tcl_SetResult( interp, "error in ftcl_exec" , NULL ) ;
      return TCL_ERROR ;
   }
   else
   {
      return TCL_OK ;
   }
}

/* --------------------------------------------------------------------
    Routine:  Ftcl_GenericCmd()
    Author:   Arjen Markus
    Purpose:  Wrapper for Tcl commands implemented via Fortran
    Context:  Used by Tcl interpreter
    Summary:
              Check if this is not a recursive call
              Call the actual Fortran routine for the actual
              work
-------------------------------------------------------------------- */
static int Ftcl_GenericCmd( ClientData client_data, Tcl_Interp *interp,
                            int objc, struct Tcl_Obj * CONST objv[] )
{
   integer  ierror                ; /* Passed to Fortran */
   int      length                ;
   char     cmdname[FTCL_BUFSIZE] ;
   char    *pstr                  ;
   FortProc procedure             ;

/* We do not support recursive calls ...
*/
   if ( ftcl_exec_is_active != 0 )
   {
      Tcl_SetResult( interp, "recursive use of ftcl_exec not supported" ,
         NULL ) ;
      return TCL_ERROR ;
   }

   ftcl_exec_is_active ++         ;
   ftcl_number_args    = objc - 1 ;
   ftcl_args           = objv     ;
   ftcl_interp         = interp   ;

/* Get the first argument and call the Fortran routine
*/
   pstr = Tcl_GetStringFromObj( objv[0], &length ) ;
   ftcl_conv_str_to_fort( pstr, cmdname, sizeof(cmdname) );

   if ( ftcl_trace )
   {
      ftcl_trace_args( pstr, objc, objv ) ;
   }

   procedure = (FortProc) client_data ;
   (*procedure)( cmdname,
#ifdef IN_BETWEEN
      (FTNLEN) sizeof( cmdname ),
#endif
      &ftcl_number_args,
      &ierror
#ifndef IN_BETWEEN
     ,(FTNLEN) sizeof( cmdname )
#endif
                                  ) ;
   ftcl_exec_is_active -- ;

   if ( ierror != 0 )
   {
      Tcl_SetResult( interp, "error in ", NULL ) ;
      Tcl_AppendResult( interp, pstr, NULL ) ;
      return TCL_ERROR ;
   }
   else
   {
      return TCL_OK ;
   }
}

/* --------------------------------------------------------------------
    Routine:  Ftcl_ConfigCmd()
    Author:   Arjen Markus
    Purpose:  Implement the Tcl "ftcl_config" command
    Context:  Used by Tcl interpreter
    Summary:
              Check the number of arguments (must be two). Also check
              if the first is a known option. Then call the actual
              routine to set them.
-------------------------------------------------------------------- */
static int Ftcl_ConfigCmd( ClientData client_data, Tcl_Interp *interp,
                           int objc, struct Tcl_Obj * CONST objv[] )
{
   integer  ierror             ; /* Passed to Fortran */
   int      length             ;
   char     first_arg[FTCL_BUFSIZE] ;
   char    *pstr                    ;

   if ( objc < 2 )
   {
      Tcl_SetResult( interp, "wrong # args: ftcl_config option ?value" , NULL ) ;
      return TCL_ERROR ;
   }
   return TCL_OK ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_get_arg_*()
    Author:   Arjen Markus
    Purpose:  Get the integer/real/... value from an argument
    Context:  Used by applications
    Summary:
              Check if the argument exists. If so, return the
              value converted to an integer, real, double, logical
              or string
              integer
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_get_arg_int(
   integer       *iarg    ,         /* I index of argument */
   integer       *value             /* O integer value     */
                          )         /* Returns nothing     */

{
   char  buffer[FTCL_BUFSIZE] ;

   if ( *iarg < 1 || *iarg > ftcl_number_args )
   {
      sprintf( buffer, "%ld", (long) *iarg ) ;
      ftcl_debug_message( "ftcl_get_arg: Argument index out of range",
         buffer ) ;
      ftcl_err = TCL_ERROR ;
   }
   else
   {
      ftcl_err = Tcl_GetLongFromObj( ftcl_interp, ftcl_args[*iarg], value ) ;
      if ( ftcl_err != TCL_OK )
      {
         ftcl_debug_message( "ftcl_get_arg: not an integer?" , "" ) ;
      }
   }

   if ( ftcl_trace )
   {
      sprintf( buffer, "argument %ld", (long) *iarg ) ;
      ftcl_trace_int( "ftcl_get_arg", buffer, *value ) ;
   }

   RETURN ;
}

FOR_RETURN FOR_CALL ftcl_get_arg_real(
   integer       *iarg    ,         /* I index of argument */
   real          *value             /* O real value        */
                          )         /* Returns nothing     */

{
   char    buffer[FTCL_BUFSIZE] ;
   double  dbl_value            ;

   if ( *iarg < 1 || *iarg > ftcl_number_args )
   {
      sprintf( buffer, "%ld", (long) *iarg ) ;
      ftcl_debug_message( "ftcl_get_arg: Argument index out of range",
         buffer ) ;
      ftcl_err = TCL_ERROR ;
   }
   else
   {
      ftcl_err = Tcl_GetDoubleFromObj( ftcl_interp, ftcl_args[*iarg],
                    &dbl_value )  ;
      *value   = (real) dbl_value ;
      if ( ftcl_err != TCL_OK )
      {
         ftcl_debug_message( "ftcl_get_arg: not a real?" , "" ) ;
      }
   }

   if ( ftcl_trace )
   {
      sprintf( buffer, "argument %ld", (long) *iarg ) ;
      ftcl_trace_real( "ftcl_get_arg", buffer, *value ) ;
   }

   RETURN ;
}


FOR_RETURN FOR_CALL ftcl_get_arg_double(
   integer       *iarg    ,         /* I index of argument */
   doublereal    *value             /* O real value        */
                          )         /* Returns nothing     */

{
   char  buffer[FTCL_BUFSIZE] ;

   if ( *iarg < 1 || *iarg > ftcl_number_args )
   {
      sprintf( buffer, "%ld", (long) *iarg ) ;
      ftcl_debug_message( "ftcl_get_arg: Argument index out of range",
         buffer ) ;
      ftcl_err = TCL_ERROR ;
   }
   else
   {
      ftcl_err = Tcl_GetDoubleFromObj( ftcl_interp, ftcl_args[*iarg],
                    value ) ;
      if ( ftcl_err != TCL_OK )
      {
         ftcl_debug_message( "ftcl_get_arg: not a double precision real?",
            "" ) ;
      }
   }

   if ( ftcl_trace )
   {
      sprintf( buffer, "argument %ld", (long) *iarg ) ;
      ftcl_trace_double( "ftcl_get_arg", buffer, *value ) ;
   }

   RETURN ;
}

FOR_RETURN FOR_CALL ftcl_get_arg_log(
   integer       *iarg    ,         /* I index of argument */
   logical       *value             /* O double value      */
                          )         /* Returns nothing     */

{
   char  buffer[FTCL_BUFSIZE] ;
   int   val                  ;

   if ( *iarg < 1 || *iarg > ftcl_number_args )
   {
      sprintf( buffer, "%ld", (long) *iarg ) ;
      ftcl_debug_message( "ftcl_get_arg: Argument index out of range",
         buffer ) ;
      ftcl_err = TCL_ERROR ;
   }
   else
   {
      ftcl_err = Tcl_GetBooleanFromObj( ftcl_interp, ftcl_args[*iarg],
                    &val ) ;
      if ( val == 1 )
      {
         *value = ftcl_true                  ;
      }
      else
      {
         *value = ftcl_false                 ;
      }
      if ( ftcl_err != TCL_OK )
      {
         ftcl_debug_message( "ftcl_get_arg: not a boolean?" , "" ) ;
      }
   }

   if ( ftcl_trace )
   {
      sprintf( &buffer[0], "argument %ld", (long) *iarg ) ;
      ftcl_trace_log( "ftcl_get_arg", buffer, *value ) ;
   }

   RETURN ;
}

FOR_RETURN FOR_CALL ftcl_get_arg_string(
   integer       *iarg    ,         /* I index of argument */
   char          *value             /* O string value    */
  ,FTNLEN         lenval            /* I (implicit) length of string value */
                          )         /* Returns nothing */

{
   int    length              ;
   char  *pval                ;
   char  buffer[FTCL_BUFSIZE] ;

   if ( *iarg < 1 || *iarg > ftcl_number_args )
   {
      sprintf( &buffer[0], "%ld", (long) *iarg ) ;
      ftcl_debug_message( "ftcl_get_arg: Argument index out of range",
         buffer ) ;
      ftcl_err = TCL_ERROR ;
   }
   else
   {
      pval = Tcl_GetStringFromObj( ftcl_args[*iarg], &length ) ;
      if ( pval != NULL )
      {
         ftcl_conv_str_to_fort( pval, value, lenval ) ;
      }
      else
      {
         ftcl_conv_str_to_fort( " ", value, lenval ) ;
         ftcl_debug_message( "ftcl_get_arg: not a string?", "" ) ;
      }
   }

   if ( ftcl_trace )
   {
      sprintf( &buffer[0], "argument %ld", (long) *iarg ) ;
      ftcl_trace_string( "ftcl_get_arg", buffer, pval ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_get_int()
    Author:   Arjen Markus
    Purpose:  Get the integer value from a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Get the value of that variable, interpreted as
              integer
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_get_int(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   integer       *value             /* O integer value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   int   val  ;
   char *pstr ;
   char *pval ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   pval = Tcl_GetVar( ftcl_interp, pstr, 0 )         ;
   if ( pval != NULL )
   {
      Tcl_GetInt( ftcl_interp, pval, &val ) ;
      *value = (integer) val                ;
   }
   else
   {
      *value = 0 ; /* Variable does not exist? */
      ftcl_debug_message( "ftcl_get", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_int( "ftcl_get", pstr, *value ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_get_real()
    Author:   Arjen Markus
    Purpose:  Get the real value from a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Get the value of that variable, interpreted as
              real
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_get_real(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   real          *value             /* O real value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   double  val  ;
   char   *pstr ;
   char   *pval ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   pval = Tcl_GetVar( ftcl_interp, pstr, 0 )         ;
   if ( pval != NULL )
   {
      Tcl_GetDouble( ftcl_interp, pval, &val ) ;
      *value = (real) val                      ;
   }
   else
   {
      *value = (real) 0.0 ; /* Variable does not exist? */
      ftcl_debug_message( "ftcl_get", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_real( "ftcl_get", pstr, *value ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_get_double()
    Author:   Arjen Markus
    Purpose:  Get the double precision value from a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Get the value of that variable, interpreted as
              double precision real
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_get_double(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   doublereal    *value             /* O real value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   double  val  ;
   char   *pstr ;
   char   *pval ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   pval = Tcl_GetVar( ftcl_interp, pstr, 0 )         ;
   if ( pval != NULL )
   {
      Tcl_GetDouble( ftcl_interp, pval, &val )       ;
      *value = (doublereal) val                      ;
   }
   else
   {
      *value = (doublereal) 0.0 ; /* Variable does not exist? */
      ftcl_debug_message( "ftcl_get", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_double( "ftcl_get", pstr, *value ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_get_log()
    Author:   Arjen Markus
    Purpose:  Get the logical value from a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Get the value of that variable, interpreted as
              logical
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_get_log(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   logical       *value             /* O logical value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   int    val  ;
   char  *pstr ;
   char  *pval ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   pval = Tcl_GetVar( ftcl_interp, pstr, 0 )         ;
   if ( pval != NULL )
   {
      Tcl_GetBoolean( ftcl_interp, pval, &val ) ;
      if ( val == 1 )
      {
         *value = ftcl_true                  ;
      }
      else
      {
         *value = ftcl_false                 ;
      }
   }
   else
   {
      *value = ftcl_false ; /* Variable does not exist? */
      ftcl_debug_message( "ftcl_get", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_log( "ftcl_get", pstr, *value ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_get_string()
    Author:   Arjen Markus
    Purpose:  Get the string value from a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Get the string value of that variable. Convert it to
              a proper Fortran string
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_get_string(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   char          *value             /* O string value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
  ,FTNLEN         lenval            /* I (implicit) length of string value */
                          )         /* Returns nothing */

{
   char  *pstr ;
   char  *pval ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   pval = Tcl_GetVar( ftcl_interp, pstr, 0 ) ;

   if ( pval != NULL )
   {
      ftcl_conv_str_to_fort( pval, value, lenval ) ;
   }
   else
   {
      ftcl_conv_str_to_fort( " ", value, lenval ) ; /* Variable does not exist? */
      ftcl_debug_message( "ftcl_get", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_get", pstr, pval ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_put_int()
    Author:   Arjen Markus
    Purpose:  Put the integer value into a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Give this variable a new (integer) value.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_put_int(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   integer       *value             /* I integer value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   char *pstr       ;
   char *pval       ;
   char  buffer[40] ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   sprintf( buffer, "%d", *value )                   ;
   pval = Tcl_SetVar( ftcl_interp, pstr, buffer, 0 ) ;

   if ( pval == NULL )
   {
      ftcl_debug_message( "ftcl_put", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_put", pstr, buffer ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_put_log()
    Author:   Arjen Markus
    Purpose:  Put the logical value into a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Give this variable a new (logical) value.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_put_log(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   logical       *value             /* I logical value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   char *pstr       ;
   char *pval       ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;

   if ( *value == ftcl_true )
   {
      pval = Tcl_SetVar( ftcl_interp, pstr, "1", 0 ) ;
   }
   else
   {
      pval = Tcl_SetVar( ftcl_interp, pstr, "0", 0 ) ;
   }

   if ( pval == NULL )
   {
      ftcl_debug_message( "ftcl_put", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_put", pstr, pval ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_put_real()
    Author:   Arjen Markus
    Purpose:  Put the real value into a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Give this variable a new (real) value.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_put_real(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   real          *value             /* I real value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   char *pstr       ;
   char *pval       ;
   char  buffer[40] ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   sprintf( buffer, "%g", *value )                   ;
   pval = Tcl_SetVar( ftcl_interp, pstr, buffer, 0 ) ;

   if ( pval == NULL )
   {
      ftcl_debug_message( "ftcl_put", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_put", pstr, buffer ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_put_double()
    Author:   Arjen Markus
    Purpose:  Put the double value into a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Give this variable a new (double) value.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_put_double(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   doublereal    *value             /* I double value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
                          )         /* Returns nothing */

{
   char *pstr       ;
   char *pval       ;
   char  buffer[40] ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   sprintf( buffer, "%12lg", (double) *value )       ;
   pval = Tcl_SetVar( ftcl_interp, pstr, buffer, 0 ) ;

   if ( pval == NULL )
   {
      ftcl_debug_message( "ftcl_put", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_put", pstr, buffer ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_put_string()
    Author:   Arjen Markus
    Purpose:  Put the string value into a Tcl variable
    Context:  Used by applications
    Summary:
              Convert the name into a Tcl variable.
              Give this variable the value of the string. But
              convert it to a proper C string first.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_put_string(
   char          *varname ,         /* I name of variable */
#ifdef IN_BETWEEN
   FTNLEN         length  ,         /* I (implicit) length of varname */
#endif
   char          *value             /* I string value    */
#ifndef IN_BETWEEN
  ,FTNLEN         length
#endif
  ,FTNLEN         lenval            /* I (implicit) length of string value */
                          )         /* Returns nothing */

{
   char  *pstr ;
   char  *pbuf ;
   char  *pval ;

   pbuf = (char *) malloc( sizeof(char)*(lenval+1) ) ;
   pval = ftcl_conv_str_from_fort( value, lenval   ) ;
   strcpy( pbuf, pval ) ;

   pstr = ftcl_conv_str_from_fort( varname, length ) ;
   pval = Tcl_SetVar( ftcl_interp, pstr, pbuf, 0   ) ;

   if ( pval == NULL )
   {
      ftcl_debug_message( "ftcl_put", pstr ) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_put", pstr, pval ) ;
   }

   free( pbuf ) ;

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_set_result_real()
    Author:   Arjen Markus
    Purpose:  Set the result to a real value
    Context:  Used by applications
    Summary:
              Set the result to the value that was passed
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_set_result_real(
   real          *value             /* I real value    */
                          )         /* Returns nothing */

{
   char buffer[100] ;
   Tcl_SetObjResult( ftcl_interp, Tcl_NewDoubleObj( (double) *value ) ) ;

   if ( ftcl_trace )
   {
      sprintf( buffer, "%g", *value ) ;
      ftcl_trace_string( "ftcl_set_result", "", buffer ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_script()
    Author:   Arjen Markus
    Purpose:  Execute a Tcl script
    Context:  Used by applications
    Summary:
              Convert the Fortran string. Execute the Tcl script
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_script(
   char          *string  ,         /* I string representing script */
   FTNLEN         length            /* I (implicit) length of string */
                          )         /* Returns nothing */

{
   char *pstr       ;
   int   retval     ;

   pstr   = ftcl_conv_str_from_fort( string, length ) ;
   retval = Tcl_Eval( ftcl_interp, pstr ) ;

   if ( retval != TCL_OK )
   {
      ftcl_debug_message( "ftcl_script", Tcl_GetVar(ftcl_interp, "errorInfo", TCL_GLOBAL_ONLY)) ;
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_script", pstr,
         Tcl_GetStringResult( ftcl_interp ) ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_start()
    Author:   Arjen Markus
    Purpose:  Initialise in case of embedded Tcl
    Context:  Used by applications
    Summary:
              Create a new Tcl interpreter. Then convert the Fortran
              string, which is regarded as a filename.
              Execute the Tcl script contained in this file.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_start(
   char          *filename,         /* I name of start up file */
   FTNLEN         length            /* I (implicit) length of string */
                          )         /* Returns nothing */

{
   char *pstr       ;
   int   retval     ;

   pstr   = ftcl_conv_str_from_fort( filename, length ) ;

   Tcl_FindExecutable("UNUSED");

   ftcl_interp = Tcl_CreateInterp() ;
   if ( ftcl_interp == NULL )
   {
      fprintf( stderr, "Could not create interpreter!\n" ) ;
      exit( 1 ) ;
   }

   if (Tcl_Init(ftcl_interp) == TCL_ERROR) {
       fprintf( stderr, "Could not initialise interpreter!\nReason: %s\n",
           Tcl_GetStringResult(ftcl_interp) ) ;
       RETURN ;
   }

   if ( strlen( pstr ) != 0 ) {
      retval = Tcl_EvalFile( ftcl_interp, pstr ) ;

      if ( retval != TCL_OK )
      {
         /*ftcl_debug_message( "ftcl_start", Tcl_GetVar(ftcl_interp, "errorInfo", TCL_GLOBAL_ONLY)) ; */
         ftcl_debug_message( "ftcl_start", Tcl_GetStringResult(ftcl_interp));
         exit( 1 ) ;
      }
   }

   if ( ftcl_trace )
   {
      ftcl_trace_string( "ftcl_start", pstr,
         Tcl_GetStringResult( ftcl_interp ) ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_main_loop()
    Author:   Arjen Markus
    Purpose:  Start the main event loop
    Context:  Used by applications
    Summary:
              Simply start the main event loop and let the GUI take
              care of the rest.
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_main_loop( void )
{
   while ( 1 ) {
      Tcl_DoOneEvent( 0 ) ;
   }

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_make_command()
    Author:   Arjen Markus
    Purpose:  Register a new Tcl command
    Context:  Used by applications
    Summary:
              Create a new Tcl command that actually calls the
              Fortran routine that does the work. The trick is
              to use the clientData argument to store the pointer
              to the procedure. Complications: the actual
              interface to the function ...
-------------------------------------------------------------------- */
FOR_RETURN FOR_CALL ftcl_make_command(
   FortProc       procedure,        /* I (pointer to) the Fortran procedure */
   char          *cmdname,          /* I name of the associated Tcl command */
   FTNLEN         length            /* I (implicit length of the string */
                          )         /* Returns nothing */
{
   char *pstr ;

   pstr = ftcl_conv_str_from_fort( cmdname, length ) ;
   Tcl_CreateObjCommand( ftcl_interp, pstr, Ftcl_GenericCmd,
      (ClientData) procedure, NULL ) ;

   RETURN ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_conv_str_from_fort()
    Author:   Arjen Markus
    Purpose:  Convert a string from Fortran format to C format
    Context:  Used by routines in this file
    Summary:
              Look for the last significant character. Insert a '\0'
              Return a pointer to the static buffer.
-------------------------------------------------------------------- */
static char *ftcl_conv_str_from_fort(
   char          *string  ,         /* I string to be converted */
   FTNLEN         length  )         /* I length to be examined */
                                    /* Returns pointer to static buffer */
{
   int         i      ;
   int         buflen ;
   static char buffer[FTCL_BUFSIZE] ;

   buflen = sizeof(buffer) - 1 ;
   if (length <  (sizeof(buffer) - 1 ) ) {
       buflen = length;
   }

   strncpy( buffer, string, buflen )          ;
   buffer[buflen] = '\0'                      ;

   for ( i = buflen-1 ; i >= 0 ; i -- )
   {
      if ( buffer[i] != ' ' )
      {
         buffer[i+1] = '\0' ;
         break              ;
      } else {
         buffer[i]   = '\0' ;
      }
   }

   return &buffer[0] ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_conv_str_to_fort()
    Author:   Arjen Markus
    Purpose:  Convert a string from C format to Fortran format
    Context:  Used by routines in this file
    Summary:
              Replace the null character by a space and fill up
              with spaces.
    Note:
              The destination string must have the length "length"
              or more! Pass the same value for the length to the
              Fortran routines.
-------------------------------------------------------------------- */
static void ftcl_conv_str_to_fort(
   char          *string  ,         /* I string to be converted */
   char          *fstr    ,         /* O Fortran string that results */
   int            length  )         /* I length for Fortran string */
                                    /* Returns nothing */
{
   int         i      ;
   int         actlen ;

   strncpy( fstr, string, length ) ;

   actlen = strlen( fstr ) ;

   for ( i = actlen ; i < length ; i ++ )
   {
      fstr[i] = ' ' ;
   }

   return ;
}

/* --------------------------------------------------------------------
    Routine:  ftcl_debug_message()
    Author:   Arjen Markus
    Purpose:  Present a short description of what went wrong
    Context:  Used by routines in this file
    Summary:
              Print a message like: "Error in 'ftcl_get': variable"
              Print to ftcl_outfile (defaults to stdout)
-------------------------------------------------------------------- */
static void ftcl_debug_message(
   char          *string1 ,         /* I first part of string */
   char          *string2     )     /* I second part of string */
                                    /* Returns nothing */
{
   if ( ftcl_debug )   {
      fprintf( ftcl_outfile, "Error in '%s': %s\n", string1, string2 ) ;
   } else {
      printf( "Error in '%s': %s\n", string1, string2 ) ;
   }
      exit(-1);


   return ;
}

/* --------------------------------------------------------------------
    Routines: ftcl_trace_int, _real, _double, _log, _string, _args
    Author:   Arjen Markus
    Purpose:  Present a short description of the calls and their results
    Context:  Used by routines in this file
    Summary:
              Print a message like:
                "Trace of 'ftcl_get': variable - 'value'"
              Print to ftcl_outfile (defaults to stdout)
-------------------------------------------------------------------- */
static void ftcl_trace_int(
   char          *routine,          /* I Name of called routine */
   char          *var,              /* I Name of variable */
   integer        value       )     /* I Value of variable */
                                    /* Returns nothing */
{
   fprintf( ftcl_outfile, "Trace of '%s': %s - '%ld'\n",
      routine, var, (long int) value ) ;

   return ;
}

static void ftcl_trace_real(
   char          *routine,          /* I Name of called routine */
   char          *var,              /* I Name of variable */
   real           value       )     /* I Value of variable */
                                    /* Returns nothing */
{
   fprintf( ftcl_outfile, "Trace of '%s': %s - '%g'\n",
      routine, var, (float) value ) ;

   return ;
}

static void ftcl_trace_double(
   char          *routine,          /* I Name of called routine */
   char          *var,              /* I Name of variable */
   doublereal     value       )     /* I Value of variable */
                                    /* Returns nothing */
{
   fprintf( ftcl_outfile, "Trace of '%s': %s - '%12lg'\n",
      routine, var, (double) value ) ;

   return ;
}

static void ftcl_trace_log(
   char          *routine,          /* I Name of called routine */
   char          *var,              /* I Name of variable */
   logical        value       )     /* I Value of variable */
                                    /* Returns nothing */
{
   fprintf( ftcl_outfile, "Trace of '%s': %s - '%s'\n",
      routine, var, ( value == ftcl_true ? "True" : "False" ) ) ;

   return ;
}

static void ftcl_trace_string(
   char          *routine,          /* I Name of called routine */
   char          *var,              /* I Name of variable */
   char          *value       )     /* I Value of variable */
                                    /* Returns nothing */
{
   fprintf( ftcl_outfile, "Trace of '%s': %s - '%s'\n",
      routine, var, value ) ;

   return ;
}

static void ftcl_trace_args(
   char          *routine,          /* I Name of called routine */
   int            objc,             /* I Number of arguments */
   Tcl_Obj       *CONST objv[] )    /* I Value of arguments */
                                    /* Returns nothing */
{
   int i ;

   fprintf( ftcl_outfile, "Trace of '%s':\n   %s", routine, routine ) ;
   for ( i = 0 ; i < objc ; i ++ )
   {
      fprintf( ftcl_outfile, " %s", Tcl_GetStringFromObj( objv[i], NULL ) ) ;
   }
   fprintf( ftcl_outfile, "\n" ) ;

   return ;
}

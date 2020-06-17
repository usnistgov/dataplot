/*  libplot.c
 *
 *  The purpose of this library is to provide easy access from
 *  a Fortran 77 program to the Unix/Linux libplot library.
 *
 *  Although many of the supported devices in libplot are redundant
 *  to devices already supported in Dataplot, it does support a
 *  metafile output format that is used by a number of common
 *  Unix/Linux graphics filter program (e.g., xfig).
 *
 *  This code assumes that the following libraries are installed on
 *  your system:
 *
 *       xlib
 *       xt
 *
 *  Note that calling C from Fortran is not standard.  I have
 *  provided the following compiler defintions to enhance portability.
 *
 *  1) The default is to assume that the Fortran compiler appends an
 *     underscore to the routine name.  Use -DNOUNDERSCORE if your
 *     compiler does not append the underscore.
 *  2) The default is to assume that the Fortran compiler converts
 *     routine names to lower case.  Use -DUPPERCASE if your
 *     Fortran compiler does not do this (e.g., the Cray).
 *  3) Many Unix compilers support a "-r8", or something similar,
 *     to make single precision 64-bit.  Use -DDOUBLE if you
 *     compile your Fortran with this option.
 *  4) Character strings are the most troublesome issue for
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
 *  The following routines are included:
 *
 *  plinit      - initialize libplot library
 *  plend       - close libplot library
 *  pleras      - start a new graph (close currently open one as well)
 *  pllatr      - set line attributes
 *  pldraw      - draw a polyline
 *  plseco      - set foreground color
 *  plpoin      - draw a point (i.e., a pixel)
 *  plrgfl      - solid fill of a region
 *  pltxth      - draw a horizontal character string
 *  pltxtv      - draw a vertical character string
 *  i_to_s_9    - utility routine to convert array of ADE's to string
 *                array
 *
 */

/*  Site dependent definitions (see comments above) */
/*  Default is an underscore and lower case.  The compiler specified
 *  definitions -DNOUNDERSCORE and -DUPPERCASE can be specified to
 *  override these defaults. */

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
#ifdef DOUBLE
#define PRECISION 1
#else
#define PRECISION 0
#endif
#ifdef INTEGER8
#define INTEGER_PRECISION 1
#else
#define INTEGER_PRECISION 0
#endif

/*  include files */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <plot.h>

/* global definitions */

#define DEFAULT_X_SIZE  600
#define DEFAULT_Y_SIZE  465
#define MIN_X_SIZE      100
#define MIN_Y_SIZE      100

/* common parameters */

/* flags for current attribute settings */
static int    OPEN_FLAG_CAIRO = 0;            /* 0 - libplot closed,
                                           1 - libplot open */
int           PL_DEVICE_TYPE = 0;       /* define device */
                                        /*  1 - X */
                                        /*  2 - pnm */
                                        /*  3 - gif */
                                        /*  4 - ai */
                                        /*  5 - ps */
                                        /*  6 - fig */
                                        /*  7 - pcl */
                                        /*  8 - hpgl */
                                        /*  9 - tek */
                                        /* 10 - meta */
                                        /* 11 - svg */
                                        /* 12 - png */
int           BITMAP = 0;               /* selected device is a bitmap */
int           LINE_STYLE_CURRENT_CAIRO = -1;  /* current line style */
int           JOIN_STYLE_CURRENT_CAIRO = -1;  /* current line style */
int           CAP_STYLE_CURRENT_CAIRO = -1;   /* current line style */

FILE              *outfile;
FILE              *outfile_null;
FILE              *infile_null;
plPlotter         *plotter;
plPlotterParams   *plotter_params;

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  plend_(), pldraw_(), plpoin_(), plrgfl_(), plseco();
void  plinit_(), pleras_(), pltxth_(), pltxtv_(), pllatr_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  PLEND_(), PLDRAW_(), PLPOIN_(), PLRGFL_(), PLRGFL_();
void  PLINIT_(), PLERAS_(), PLTXTH_(), PLTXTV_(), PLLATR()_;
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  plend(), pldraw(), plpoin(), plrgfl(), plseco();
void  plinit(), pleras(), pltxth(), pltxtv(), pllatr();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  PLEND(), PLDRAW(), PLPOIN(), PLRGFL(), PLRGFL();
void  PLINIT(), PLERAS(), PLTXTH(), PLTXTV(), PLLATR();
#endif
void  i_to_s_9();

/* PLINIT  - routine to initialize libplot.
 *           For libplot device, set flag saying this routine
 *           has been called and specify specific device being
 *           used.
 *
 *           Include parameters for:
 *
 *           1) Bitmap size
 *           2) Display name for x11
 *           3) Rotation for PCL devices
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void plinit_(itype, ierror, string1, prot, string2)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLINIT_(itype, ierror, string1, prot, string2)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void plinit(itype, ierror, string1, prot, string2)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLINIT(itype, ierror, string1, prot, string2)
#endif
double prot;
int    string1[], string2[];
#if INTEGER_PRECISION == 0
int  *itype, *ierror;
#else
int  itype[2], ierror[2];
#endif
{

   int   itype_temp;
   int   len, irot;
   char  string3[80];
   char  string4[160];

#if INTEGER_PRECISION == 0
   itype_temp = *itype;
#else
   itype_temp = itype[0];
#endif
#if INTEGER_PRECISION == 0
   i_to_s_9(string1, string3, 40, &len);
   i_to_s_9(string2, string4, 80, &len);
#else
   i_to_s_9(string2, string4, 80, &len);
   i_to_s_9(font, font_name, 160, &len2);
#endif
/*
 *           itype = 1   => X
 *                 = 2   => pnm  (netPBM binary format)
 *                 = 14  => pnm  (netPBM ASCII format)
 *                 = 3   => gif
 *                 = 4   => ai   (Adobe Illustrator)
 *                 = 5   => ps   (Postscript)
 *                 = 6   => fig  (xfig)
 *                 = 7   => pcl  (HP PCL format)
 *                 = 8   => hpgl (HP-GL format)
 *                 = 9   => tek  (Tektronix 4014 - screen)
 *                 = 19  => tek  (Tektronix 4014 - file)
 *                 = 10  => meta (libplot metafile binary format)
 *                 = 13  => meta (libplot metafile ASCII format)
 *                 = 11  => svg  (Scalable Vector Graphics)
 *                 = 12  => png  (Portable Network Graphics)
 *                 = 15  => regis (Regis - screen)
 *                 = 16  => regis (Regis - file)
 *                 = 17  => cgm  (webcgm binary format)
 *                 = 18  => cgm  (webcgm ascii format)
 */

    outfile_null = fopen("libplot.out","w");
    plotter_params = pl_newplparams ();
    if (string1[0] > 0)
    {
       pl_setplparam(plotter_params,"BITMAPSIZE",string3);
       /* pl_setplparam(plotter_params,"BITMAPSIZE","600x400"); */
    }
    if (itype_temp == 1) {
       pl_setplparam(plotter_params,"VANISH_ON_DELETE","yes");
       /*
       if (string2[0] > 0)
       {
          pl_setplparam(plotter_params,"DISPLAY",string4);
       }
       */
       if ((plotter = pl_newpl_r ("X", stdin, outfile_null, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
       BITMAP = 1;
    } else if (itype_temp == 2) {
       outfile = fopen("libplot.pnm","wb");
       if ((plotter = pl_newpl_r ("pnm", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
       BITMAP = 1;
    } else if (itype_temp == 14) {
       outfile = fopen("libplot.pnm","w");
       pl_setplparam (plotter_params, "META_PORTABLE", "yes");
       if ((plotter = pl_newpl_r ("pnm", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
       BITMAP = 1;
    } else if (itype_temp == 3) {
       outfile = fopen("libplot.gif","wb");
       if ((plotter = pl_newpl_r ("gif", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
       BITMAP = 1;
    } else if (itype_temp == 4) {
       outfile = fopen("libplot.ai","wb");
       if ((plotter = pl_newpl_r ("ai", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 5) {
       pl_setplparam(plotter_params,"PAGESIZE","letter");
       outfile = fopen("libplot.ps","w");
       if ((plotter = pl_newpl_r ("ps", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 6) {
       outfile = fopen("libplot.fig","wb");
       if ((plotter = pl_newpl_r ("fig", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 7) {
       outfile = fopen("libplot.pcl","w");
       if (prot == 0.) {
          pl_setplparam (plotter_params, "PCL_ROTATE", "0");
       }
       if (prot == 90.) {
          pl_setplparam (plotter_params, "PCL_ROTATE", "90");
       }
       if ((plotter = pl_newpl_r ("pcl", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 8) {
       outfile = fopen("libplot.hpgl","w");
       if ((plotter = pl_newpl_r ("hpgl", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 9) {
       if ((plotter = pl_newpl_r ("tek", stdin, stdout, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 19) {
       outfile = fopen("libplot.meta","wb");
       if ((plotter = pl_newpl_r ("tek", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 10) {
       outfile = fopen("libplot.meta","wb");
       if ((plotter = pl_newpl_r ("meta", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 13) {
       outfile = fopen("libplot.meta","w");
       pl_setplparam (plotter_params, "META_PORTABLE", "yes");
       if ((plotter = pl_newpl_r ("meta", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 11) {
       outfile = fopen("libplot.svg","w");
       if ((plotter = pl_newpl_r ("svg", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 12) {
       outfile = fopen("libplot.png","wb");
       if ((plotter = pl_newpl_r ("png", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
       BITMAP = 1;
    } else if (itype_temp == 15) {
       if ((plotter = pl_newpl_r ("regis", stdin, stdout, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 16) {
       outfile = fopen("libplot.regis","wb");
       if ((plotter = pl_newpl_r ("regis", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 17) {
       outfile = fopen("libplot.cgm","wb");
       if ((plotter = pl_newpl_r ("cgm", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    } else if (itype_temp == 18) {
       outfile = fopen("libplot.cgm","w");
       pl_setplparam (plotter_params, "CGM_PORTABLE", "yes");
       if ((plotter = pl_newpl_r ("cgm", stdin, outfile, outfile_null,
			     plotter_params)) == NULL)
       {
#if INTEGER_PRECISION == 0
           *ierror  = 1;
#else
           ierror[0] = 1;
#endif
           return;
       }
    }
    PL_DEVICE_TYPE = itype_temp;

    pl_openpl_r (plotter);
    OPEN_FLAG_CAIRO = 1;

}


/* PLERAS  - routine to clear the screen.
 *
 *  iade           - integer array containing string that
 *                   specifies the desired number of pixels
 *                   in both the horizontal and vertical positions
 *  back_col_red   - background color red component
 *  back_col_blue  - background color blue component
 *  back_col_green - background color blue component
 *
 *  Notes:
 *
 *  1) specifying screen size in pixels only applicable to
 *     bit-mapped devices
 *  2) background colors only apply to ..
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void pleras_(iade, red, green, blue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLERAS_(iade, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void pleras(iade, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLERAS(iade, red, green, blue)
#endif

#if INTEGER_PRECISION == 0
int  iade[];
int  *red, *green, *blue;
#else
int  iade[];
int  red[2], green[2], blue[2];
#endif

{

   int   red_temp, blue_temp, green_temp;
   int   len;
   int   i;
   char  pixel_size[20];

#if INTEGER_PRECISION == 0
   red_temp   = *red;
   blue_temp  = *blue;
   green_temp = *green;
#else
   red_temp   = red[0];
   blue_temp  = blue[0];
   green_temp = green[0];
#endif

   /* First, check if a graph is currently open, if so write it
      to the current file name. */
   if (OPEN_FLAG_CAIRO == 1) {

      if (BITMAP == 1) {
         strcpy(pixel_size," ");
         i_to_s_9(iade,pixel_size,20,&len);
      /* With thread safe interface, need to set plotter parameters when open
         pl_parampl("BITMAPSIZE",pixel_size);
      */
         pl_bgcolor_r(plotter,red_temp,green_temp,blue_temp);
      }
      pl_erase_r(plotter);
      pl_fspace_r(plotter,0.0,0.0,100.0,100.0);
   }

}

/* PLEND   - routine to end libplot.  Close the display.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void plend_(ierror)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLEND_(ierror)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void plend(ierror)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLEND(ierror)
#endif
#if INTEGER_PRECISION == 0
int    *ierror;
#else
int    ierror[2];
#endif
{

#if INTEGER_PRECISION == 0
   *ierror = 0;
#else
   ierror[0] = 0;
#endif
   pl_closepl_r(plotter);
   if (pl_deletepl_r(plotter) < 0) {
#if INTEGER_PRECISION == 0
      *ierror = 2;
#else
      ierror[0] = 2;
#endif
   }
   OPEN_FLAG_CAIRO = 0;
   if (PL_DEVICE_TYPE == 1) {
   } else if (PL_DEVICE_TYPE == 9) {
   } else {
      fclose(outfile);
      fclose(outfile_null);
   }

}

/* PLLATTR - set line attributes.  Note that the attribute will only be
 *           set if it is being changed (i.e., test against current
 *           value of the attribute).
 *
 * index - parameter that sets the value
 * icode - identify which attribute to set
 *         1 - set the line width
 *         2 - set the line style (i.e., solid or dash).  Currently only
 *             three dash patterns are supported.  However, additional
 *             ones may be added.
 *             0 - solid line
 *             1 - dash line      (DASH    => shortdashed)
 *             2 - dotted line    (DOTTED  => dotted)
 *             3 - dash2          (DASH2   => longdashed)
 *             4 - dash3          (DASH3   => dot-dashed)
 *             5 - dash4          (DASH4   => dotdotdashed)
 *             6 - dash5          (DASH5   => dotdotdotdashed)
 *         3 - set the line cap
 *             0 - cap butt (end-caps squared off at endoints perpindicular
 *                 to the slope of the line)
 *             1 - cap round (end-caps are circles with diameter equal to
 *                 line width)
 *             2 - cap projecting (end caps are squared off similar to cap
 *                 butt, however they project half the line width beyond
 *                 the end points)
 *         4 - set the line join
 *             0 - miter join (outer edges of wide line extended so that
 *                 they meet at same angle as would narow lines)
 *             1 - round join (corners are rounded off using a circle of
 *                 diameter equal to line width centered at join point)
 *             2 - bevel join (intersecting endpoints of lines both drawn
 *                 as if they were end points with cap butt style.  The
 *                 small triangle created is filled).
 * avalue - numeric value (currently only used by line width)
 *
 */
#define MAX_WIDTH  15
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void pllatr_(index, icode, avalue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLLATR_(index, icode, avalue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void pllatr(index, icode, avalue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLLATR(index, icode, avalue)
#endif

double *avalue;
#if INTEGER_PRECISION == 0
int    *index, *icode;
#else
int    index[2], icode[2];
#endif
{
unsigned long  valuemask;
int            dash_offset;
int            width_temp;
int            temp;
int            icode_temp, index_temp;
double         avalue_temp;

#if INTEGER_PRECISION == 0
   icode_temp  = *icode;
   index_temp  = *index;
#else
   icode_temp  = icode[0];
   index_temp  = index[0];
#endif
   switch (index_temp) {
      case 1:         /* set the line width */
          pl_flinewidth_r(plotter,avalue_temp);
          break;
      case 2:         /* set the line style */
          if (icode_temp == LINE_STYLE_CURRENT_CAIRO) break;
          switch (icode_temp) {    /* index determines the style */
             case 0:         /* solid line */
                 pl_linemod_r(plotter,"solid");
                 break;
             case 1:         /* dashed line */
                 pl_linemod_r(plotter,"shortdashed");
                 break;
             case 2:         /* dotted line */
                 pl_linemod_r(plotter,"dotted");
                 break;
             case 3:         /* dash2 line */
                 pl_linemod_r(plotter,"longdashed");
                 break;
             case 4:         /* dash3 */
                 pl_linemod_r(plotter,"dotdashed");
                 break;
             case 5:         /* dash4 line */
                 pl_linemod_r(plotter,"dotdotdashed");
                 break;
             case 6:         /* dash5 line */
                 pl_linemod_r(plotter,"dotdotdotdashed");
                 break;
              default:
                 index_temp = 0;
                 pl_linemod_r(plotter,"solid");
                 break;
          }
          LINE_STYLE_CURRENT_CAIRO = icode_temp;
          break;
      case 3:         /* set the line cap style */
          if (icode_temp == CAP_STYLE_CURRENT_CAIRO) break;
          switch (icode_temp) {  /* index determines the style */
             case 0:         /* cap butt */
                 pl_capmod_r(plotter,"butt");
                 break;
             case 1:         /* cap round */
                 pl_capmod_r(plotter,"round");
                 break;
             case 2:         /* cap projecting */
                 pl_capmod_r(plotter,"projecting");
                 break;
             default:
                 icode_temp = 0;
                 pl_capmod_r(plotter,"butt");
                 break;
          }
          CAP_STYLE_CURRENT_CAIRO = icode_temp;
          break;
      case 4:         /* set the join style */
          if (icode_temp == JOIN_STYLE_CURRENT_CAIRO) break;
          switch (icode_temp) {  /* index determines the style */
             case 0:         /* miter join */
                 pl_joinmod_r(plotter,"miter");
                 break;
             case 1:         /* round join */
                 pl_joinmod_r(plotter,"round");
                 break;
             case 2:         /* bevel join */
                 pl_joinmod_r(plotter,"bevel");
                 break;
             default:
                 index_temp = 0;
                 pl_joinmod_r(plotter,"miter");
                 break;
          }
          JOIN_STYLE_CURRENT_CAIRO = icode_temp;
          break;
      default:
          break;
   }
}

/* PLDRAW  - draw a polyline.
 *
 *           We are using a 0 to 100 coordinate system, so
 *           coordinates are given in real units.
 * xpts    - contains the x coordinates
 * ypts    - contains the y coordinates
 * npts    - the number of points to plot
 *
 */
#define MAX_LINE_POINTS  500
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void pldraw_(xpts,ypts,npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLDRAW_(xpts,ypts,npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void pldraw(xpts,ypts,npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLDRAW(xpts,ypts,npts)
#endif
double   xpts[], ypts[];
#if INTEGER_PRECISION == 0
int   *npts;
#else
int   npts[2];
#endif
{
   int     npts_temp;

#if INTEGER_PRECISION == 0
   npts_temp = *npts;
#else
   npts_temp = npts[0];
#endif

   double x1, x2, y1, y2;
   if (npts_temp == 2) {   /* draw exactly 2 points */
      x1 = xpts[0];
      x2 = xpts[1];
      y1 = ypts[0];
      y2 = ypts[1];
      pl_fline_r(plotter,x1,y1,x2,y2);
   } else {
      int i;
      x1 = xpts[0];
      y1 = ypts[0];
      pl_fmove_r(plotter,x1,y1);
      for (i = 1; i < npts_temp; i++) {
          x2 = xpts[i];
          y2 = ypts[i];
          pl_fcont_r(plotter,x2,y2);
      }
      pl_endpath_r(plotter);
   }
   
}

/* PLSECO  - set the color
 *
 * jcol   - index for desired color
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void plseco_(red, green, blue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLSECO_(red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void plseco(red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLSECO(red, green, blue)
#endif
#if INTEGER_PRECISION == 0
int   *red, *green, *blue;
#else
int   red[2], green[2], blue[2];
#endif
{
   int     red_temp;
   int     green_temp;
   int     blue_temp;

#if INTEGER_PRECISION == 0
   red_temp   = *red;
   green_temp = *green;
   blue_temp  = *blue;
#else
   red_temp   = red[0];
   green_temp = green[0];
   blue_temp  = blue[0];
#endif

   if (OPEN_FLAG_CAIRO > 0) {
      pl_color_r(plotter,red_temp, green_temp, blue_temp);
   }

}

/* PLPOIN - draw a point.
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void plpoin_(x, y)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLPOIN_(x, y)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void plpoin(x, y)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLPOIN(x, y)
#endif
double   *x, *y;
{
   double xtemp, ytemp;
   xtemp   = *x;
   ytemp   = *y;
   pl_fpoint_r(plotter,xtemp, ytemp);

}

/* PLRGFL - solid fill a rectangular region.
 *
 * x1     - contains the lower left x coordinate
 * y1     - contains the lower left y coordinates
 * x2     - contains the upper right x coordinate
 * y2     - contains the upper right y coordinates
 *
 */
#define MAX_REG_POINTS  1000
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void plrgfl_(x1, y1, x2, y2,red, green, blue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLRGFL_(x1, y1, x2, y2, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void plrgfl(x1, y1, x2, y2, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLRGFL(x1, y1, x2, y2, red, green, blue)
#endif
double   *x1, *y1, *x2, *y2;
#if INTEGER_PRECISION == 0
int   *red, *green, *blue;
#else
int   red[2], green[2], blue[2];
#endif
{
   double   x1temp, y1temp, x2temp, y2temp;
   int     red_temp;
   int     green_temp;
   int     blue_temp;

   x1temp = *x1;
   y1temp = *y1;
   x2temp = *x2;
   y2temp = *y2;
#if INTEGER_PRECISION == 0
   red_temp   = *red;
   green_temp = *green;
   blue_temp  = *blue;
#else
   red_temp   = red[0];
   green_temp = green[0];
   blue_temp  = blue[0];
#endif

   /* printf("red = %d, green = %d, blue = %d \n",red_temp,green_temp,blue_temp); */
   /* Note: Fill color is always coming out black.  Not sure why.  */
   pl_fillcolor_r(plotter,red_temp, green_temp, blue_temp);
   pl_filltype_r(plotter,1);
   pl_fbox_r(plotter,x1temp, y1temp, x2temp, y2temp);
   /* pl_filltype_r(plotter,0); */

}

/* PLTXTH - draw a horizontal text string.
 *
 * string - text string to draw
 * xpos   - x position
 * ypos   - y position
 * ijusth - justification (horizontal)
 *          0 - left justified
 *          1 - center justified
 *          2 - right justified
 * ijustv - justiciation (vertical)
 *          0 - center justified
 *          1 - bottom justified
 *          2 - top justified
 * height - character height
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void pltxth_(font, string, xpos, ypos, ijusth, ijustv, height, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLTXTH_(font, string, xpos, ypos, ijusth, ijustv, height, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void pltxth(font, string, xpos, ypos, ijusth, ijustv, height, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLTXTH(font, string, xpos, ypos, ijusth, ijustv, height, error)
#endif
int    font[];
int    string[];
double *xpos, *ypos, *height;
#if INTEGER_PRECISION == 0
int    *ijusth, *ijustv, *error;
#else
int    ijusth[2], ijustv[2], error[2];
#endif
{

   int    len;                  /* number of characters in string */
   int    len2;                 /* number of characters in font name */
   int    string_width;         /* width of string in pixels */
   char   string2[130];         /* converted string */
   char   font_name[130];       /* string for font name */
   int    i;
   int    ijusth_temp, ijustv_temp;
   int    height_temp;
   double xpos_temp, ypos_temp;
   char   *err;

   xpos_temp = *xpos;
   ypos_temp = *ypos;
   height_temp = *height;
#if INTEGER_PRECISION == 0
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
#else
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
#endif

#if INTEGER_PRECISION == 0
   i_to_s_9(string, string2, 130, &len);
   i_to_s_9(font, font_name, 80, &len2);
#else
   i_to_s_9(string, string2, 260, &len);
   i_to_s_9(font, font_name, 160, &len2);
#endif

   int ijust;
   pl_fmove_r(plotter,xpos_temp, ypos_temp);
   pl_ffontsize_r(plotter,height_temp);
   pl_ftextangle_r(plotter,0.0);
   pl_fontname_r(plotter,font_name);
   ijust = 1;
   if (ijusth_temp == 0 && ijustv_temp == 1) ijust=2;
   if (ijusth_temp == 0 && ijustv_temp == 2) ijust=3;
   if (ijusth_temp == 1 && ijustv_temp == 0) ijust=4;
   if (ijusth_temp == 1 && ijustv_temp == 1) ijust=5;
   if (ijusth_temp == 1 && ijustv_temp == 2) ijust=6;
   if (ijusth_temp == 2 && ijustv_temp == 0) ijust=7;
   if (ijusth_temp == 2 && ijustv_temp == 1) ijust=8;
   if (ijusth_temp == 2 && ijustv_temp == 2) ijust=9;

   switch (ijust) {
      case 1:
          pl_alabel_r(plotter,'l','c',string2);
          break;
      case 2:
          pl_alabel_r(plotter,'l','b',string2);
          break;
      case 3:
          pl_alabel_r(plotter,'l','t',string2);
          break;
      case 4:
          pl_alabel_r(plotter,'c','c',string2);
          break;
      case 5:
          pl_alabel_r(plotter,'c','b',string2);
          break;
      case 6:
          pl_alabel_r(plotter,'c','t',string2);
          break;
      case 7:
          pl_alabel_r(plotter,'r','c',string2);
          break;
      case 8:
          pl_alabel_r(plotter,'r','b',string2);
          break;
      case 9:
          pl_alabel_r(plotter,'r','t',string2);
          break;
      default:
          break;
   }

}

/* PLTXTV - draw a vertical text string.
 *
 * string - text string to draw
 * xpos   - x position
 * ypos   - y position
 * ijusth - justification (horizontal)
 *          0 - left justified
 *          1 - center justified
 *          2 - right justified
 * ijustv - justiciation (vertical)
 *          0 - center justified
 *          1 - bottom justified
 *          2 - top justified
 * height - character height
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void pltxtv_(font, string, xpos, ypos, ijusth, ijustv, height, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void PLTXTV_(font, string, xpos, ypos, ijusth, ijustv, height, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void pltxtv(font, string, xpos, ypos, ijusth, ijustv, height, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void PLTXTV(font, string, xpos, ypos, ijusth, ijustv, height, error)
#endif
int    font[];
int    string[];
double *xpos, *ypos, *height;
#if INTEGER_PRECISION == 0
int    *ijusth, *ijustv, *error;
#else
int    ijusth[2], ijustv[2], error[2];
#endif
{

   int    len;                  /* number of characters in string */
   int    len2;                 /* number of characters in font name */
   int    string_width;         /* width of string in pixels */
   char   string2[130];         /* converted string */
   char   font_name[130];       /* string for font name */
   int    i;
   int    ijusth_temp, ijustv_temp;
   int    height_temp;
   double xpos_temp, ypos_temp;
   char   *err;

   xpos_temp = *xpos;
   ypos_temp = *ypos;
   height_temp = *height;
#if INTEGER_PRECISION == 0
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
#else
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
#endif

#if INTEGER_PRECISION == 0
   i_to_s_9(string, string2, 130, &len);
   i_to_s_9(font, font_name, 80, &len2);
#else
   i_to_s_9(string, string2, 260, &len);
   i_to_s_9(font, font_name, 160, &len2);
#endif

   int ijust;
   pl_fmove_r(plotter,xpos_temp, ypos_temp);
   pl_ffontsize_r(plotter,height_temp);
   pl_ftextangle_r(plotter,90.0);
   pl_fontname_r(plotter,font_name);
   ijust = 1;
   if (ijusth_temp == 0 && ijustv_temp == 1) ijust=2;
   if (ijusth_temp == 0 && ijustv_temp == 2) ijust=3;
   if (ijusth_temp == 1 && ijustv_temp == 0) ijust=4;
   if (ijusth_temp == 1 && ijustv_temp == 1) ijust=5;
   if (ijusth_temp == 1 && ijustv_temp == 2) ijust=6;
   if (ijusth_temp == 2 && ijustv_temp == 0) ijust=7;
   if (ijusth_temp == 2 && ijustv_temp == 1) ijust=8;
   if (ijusth_temp == 2 && ijustv_temp == 2) ijust=9;

   switch (ijust) {
      case 1:
          pl_alabel_r(plotter,'l','c',string2);
          break;
      case 2:
          pl_alabel_r(plotter,'l','b',string2);
          break;
      case 3:
          pl_alabel_r(plotter,'l','t',string2);
          break;
      case 4:
          pl_alabel_r(plotter,'c','c',string2);
          break;
      case 5:
          pl_alabel_r(plotter,'c','b',string2);
          break;
      case 6:
          pl_alabel_r(plotter,'c','t',string2);
          break;
      case 7:
          pl_alabel_r(plotter,'r','c',string2);
          break;
      case 8:
          pl_alabel_r(plotter,'r','b',string2);
          break;
      case 9:
          pl_alabel_r(plotter,'r','t',string2);
          break;
      default:
          break;
   }

}


/* i_to_s_9  - utitlity routine to convert an integer array containing
 *           Ascii Decimal Equivalents to a character string array.  The
 *           Fortran routines pass character type data as an array of
 *           ADE's, which this routine then converts to C's character
 *           type.  Note that the input array is assumed to be correct
 *           (i.e., a value between 0 and 127) and no error checking is
 *           done on it.
 *
 * string1 - input array containing ADE's.
 * string2 - output array in C character format.
 * maxlen  - maximum length for string2
 * ilen    - length of character string
 *
 */
void i_to_s_9(string1, string2, maxlen, ilen)
int   string1[], maxlen, *ilen;
char  string2[];

{
     int  i;
     int  itemp;
     i = 0;
#if INTEGER_PRECISION == 0
     while (string1[i] != 0 && i < (maxlen - 1) ) {
         itemp = string1[i];
         string2[i] = string1[i];
         i++;
     }
     *ilen = i;
     string2[i]='\0';
}
#else
     while (string1[2*i] != 0 && i < (maxlen - 1) ) {
         itemp = string1[2*i];
         string2[i] = string1[2*i];
         i++;
     }
     *ilen = i;
     string2[i]='\0';
}
#endif

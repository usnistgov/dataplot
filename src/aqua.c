/*  aqua.c
 *
 *  Initial Implementation: 10/2007
 *  UPDATED  - March      2008.  Add AQSEC2 routine to support "imaging" routine
 *  UPDATED  - May        2009.  When using the "-DDOUBLE" option, need
 *                               to make a distinction between real
 *                               numbers and integers since the default
 *                               byte size can be set independently
 *                               for these.  Add the option "-DINTEGER8"
 *                               to handle the case where integers set
 *                               to 8 bytes on a 32-bit machine.
 *  UPDATED  - June       2014.  With the new versions of the Mac OSX and
 *                               Aquaterm, need to modify how real numbers
 *                               are passed between the Fortran,
 *                               aqua.c, and the Aquaterm library.
 *
 *  The purpose of this library is to provide easy access from
 *  a Fortran 77 program to the AquaTerm library on MacOSX.
 *
 *  Aquaterm provides a native Mac OSX terminal.  This can be
 *  used as an alternative to using X11 under Mac OSX.  The
 *  advantage of Aquaterm is that is does not require the user
 *  to have X11 installed (X11 installation is optional under
 *  Mac OSX).  The Aquaterm terminal also has the "look" of the
 *  Mac OSX window system (the X11 windows do not).
 *
 *  A few commets:
 *
 *     1) It is assumed that the Mac user has installed the
 *        underlying Aquaterm software (in particular, the
 *        Aquaterm library is a shared library that is expected
 *        to be in /usr/local/lib.
 *
 *     2) Although Aquaterm provides a Fortran-based "adapter",
 *        I am unable to take advantage of this.  The Fortran
 *        adapter requires that the Fortran code be compiled to
 *        preserve case in names.  This causes problems with
 *        Dataplot since I do not want to do this for some other
 *        routines that I am calling.
 *
 *     3) Aquaterm is a dynamic library.  With Dataplot, I try
 *        to link libraries statically whenever possible.
 *
 *        This seems to cause a conflict with the zlib compression
 *        library.  Dataplot uses the GD library for PNG and JPEG
 *        graphics.  GD in turn uses the jpeg, png, and zlib
 *        libraries.  For the Mac OSX, I have built static versions
 *        of these libraries.  However, Aquaterm seems to need
 *        a dynamic version of zlib.
 *
 *  For these reasons, I have choosen to use the C-based version
 *  of Aquaterm.  I have provided this subroutine as an intermediate
 *  wrapper between the Dataplot Fortran and the Aquaterm C-based
 *  library.
 *
 *  Given my problem, I have not supported the full Aquaterm library.
 *  Instead, I have supported a basic set of calls to support a
 *  device driver for Aquaterm.  This set of routines provides
 *  a wrapper layer between Fortran and the C based GD libraries.
 *  That is, these routines use only integer and real arguments with
 *  no C specific structures.  This makes the calling sequence from
 *  Fortran easy.
 * 
 *  Although I wrote this wrapper with a specific application in
 *  mind, I believe it may well be useful for other Fortran
 *  codes.  This code may be used and modified by anyone without
 *  restriction.  
 *
 *  A dummy version of this routine is maintained for non-Mac OSX
 *  systems.  Since the dummy library is coded in Fortran, routine
 *  names will be limited to six characters.
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
 *  3) Dataplot is compiled with options to make both single precision
 *     and double precision 64-bit.  So real numbers will be passed
 *     as "double" but need to be converted to "float" when calling
 *     the Aquaterm routines.
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
 *
 *  The following routines are included:
 *
 *  aqinit      - initialize Aquaterm
 *  aqend       - close Aquaterm
 *  aqrend      - render the current plot
 *  aqeras      - start a new graph (close currently open one as well)
 *  aqdraw      - draw a polyline
 *  aqmove      - move to a point
 *  aqseco      - set foreground color (based on color-map table)
 *  aqsec2      - set foreground color (based on RGB triplet)
 *  aqsepa      - set line pattern
 *  aqpoin      - draw a point (i.e., a pixel)
 *  aqcirc      - draw a circle
 *  aqrgfl      - solid fill of a region
 *  aqtxth      - draw a horizontal character string
 *  aqtxtv      - draw a vertical character string
 *  aqrelo      - read mouse position
 *  i_to_s_4    - utility routine to convert array of ADE's to string
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

/*  include files */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
/* #include <aquaterm/aquaterm.h> */
#include <aquaterm.h>

/* global definitions */

int decodeEvent(char *event, int *x, int *y);

/* flags for current attribute settings */
static int    OPEN_FLAG_AQUA = 0;          /* 0 - closed, 1 - open */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  aqend_(), aqdraw_(), aqpoin_(), aqcirc_(), aqrgfl_();
void  aqinit_(), aqeras_(), aqtxth_(), aqtxtv_();
void  aqseco_(), aqsec2(), aqsepa_(), aqrend_(), aaqrelo_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  AQEND_(), AQINIT_(), AQDRAW_(), AQPOIN_(), AQCIRC_(), AQRGFL_();
void  AQINIT_(), AQERAS_(), AQTXTH_(), AQTXTV_();
void  AQSECO_(), AQSEC2(), AQSEPA_(), AQREND_(), AQRELO_(),;
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  aqend(), aqdraw(), aqpoin(), aqcirc(), aqrgfl();
void  aqinit(), aqeras(), aqtxth(), aqtxtv(),gdtatt();
void  aqseco(), aqsec2(), aqsepa(), aqrend(), aqrelo();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  AQEND(),  AQDRAW(), AQPOIN(), AQCIRC(), AQRGFL();
void  AQINIT(), AQERAS(), AQTXTH(), AQTXTV();
void  AQSECO(), AQSEC2(), AQSEPA(), AQRELO();
#endif
void  i_to_s_4();

/* AQINIT  - routine to initialize Aquaterm.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqinit_(nplot,anumhp,anumvp,ired,igreen,iblue,maxclr)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQINIT_(nplot,anumhp,anumvp,ired,igreen,iblue,maxclr)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqinit(nplot,anumhp,anumvp,ired,igreen,iblue,maxclr)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQINIT(nplot,anumhp,anumvp,ired,igreen,iblue,maxclr)
#endif
int  ired[];
int  igreen[];
int  iblue[];
int  *maxclr;
int  *nplot, *anumhp, *anumvp;

{
int    nplot_temp, anumhp_temp, anumvp_temp;
int    maxclr_temp;
int    ival1, ival2, ival3;
int    i;
float  val1, val2, val3;

    maxclr_temp = *maxclr;
    nplot_temp  = *nplot;
    anumhp_temp = *anumhp;
    anumvp_temp = *anumvp;
    
    if (OPEN_FLAG_AQUA == 0) {           /*  Device currently closed */
       OPEN_FLAG_AQUA = 1;
       aqtInit();
       aqtOpenPlot(nplot_temp);
       aqtSetPlotSize(anumhp_temp,anumvp_temp);
       aqtSetPlotTitle("Dataplot Graphics Window");
       for (i = 0; i < maxclr_temp; i++) {
           ival1 = ired[i];
           ival2 = igreen[i];
           ival3 = iblue[i];
           val1 = (float)(ival1)/255.0;
           val2 = (float)(ival2)/255.0;
           val3 = (float)(ival3)/255.0;
           aqtSetColormapEntry(i,val1,val2,val3);
       }
    }
}

/* AQERAS  - routine to clear the screen.
 *
 *           1) Check if a plot is currently open.  If yes, write
 *              it to a file and destroy the current image.
 *           2) Create a new image with the specified size specified
 *              in pixels.  Note that orientation (landscape, portrait,
 *              is implicit in the pixel dimensions).  Note that
 *              this routine does not modify the values.
 *           3) Set all colors to be undefined and then set
 *              background and foreground colors.
 *
 *  xpixels   - width (in pixels) for graphics window
 *  ypixels   - height (in pixels) for graphics window
 *  back_col  - background color
 *  file name - file name (in integer ascii decimal equivalents)
 *
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqeras_(nplot, anumhp, anumvp, iback)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQERAS_(nplot, anumhp, anumvp, iback)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqeras(nplot, anumhp, anumvp, iback)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQERAS(nplot, anumhp, anumvp, iback)
#endif
int  *nplot, *anumhp, *anumvp, *iback;
{

   int   nplot_temp, anumhp_temp, anumvp_temp, iback_temp;

   nplot_temp  = *nplot;
   anumhp_temp = *anumhp;
   anumvp_temp = *anumvp;
   iback_temp  = *iback;

   /* Use "erase rectangle" method to clear screen  */
   if (OPEN_FLAG_AQUA > 0) {
      /* aqtTakeBackgroundColorFromColormapEntry(iback_temp); */
      /* aqtOpenPlot(nplot_temp); */
      /* aqtSetPlotSize(anumhp_temp,anumvp_temp); */
      /* aqtClearPlot(); */
      /* aqtSetPlotTitle("Dataplot Graphics Window"); */

      aqtTakeColorFromColormapEntry(iback_temp-1);
      aqtEraseRect(0,0,anumhp_temp,anumvp_temp);
      aqtAddFilledRect(0,0,anumhp_temp,anumvp_temp);

      aqtSetFontname("Helvetica");
      aqtSetFontsize(12.0);
   }
}

/* AQEND   - routine to end Aquaterm display and close the display
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqend_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQEND_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqend()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQEND()
#endif

{
   if (OPEN_FLAG_AQUA > 0) {
      aqtClosePlot();
      aqtTerminate();
      OPEN_FLAG_AQUA = 0;
   }
}

/* AQREND   - routine to render current plot
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqrend_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQREND_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqrend()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQREND()
#endif

{
   if (OPEN_FLAG_AQUA > 0) {
      aqtRenderPlot();
   }
}

/* AQDRAW  - draw a polyline.  The line attributes are set in
 *           other routines.
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points to plot
 * icap   - cap style: 1 = Butt
 *                     2 = Round
 *                     3 = Square
 *
 *          Calling routine should send points in increments of
 *          1,000.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqdraw_(xpts, ypts, npts, icap)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQDRAW_(xpts, ypts, npts, icap)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqdraw(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQDRAW(xpts, ypts, npts)
#endif
double   xpts[], ypts[];
int   *npts;
int   *icap;
{
   int     i;
   int     iindx;
   int     npts_temp;
   int     icap_temp;
   int     nlast;
   float   xpts_temp[1000];
   float   ypts_temp[1000];

   npts_temp = *npts;
   icap_temp = *icap;

   if (OPEN_FLAG_AQUA > 0) {
      if (icap_temp == 2) {
        aqtSetLineCapStyle(AQTRoundLineCapStyle);
      } else if (icap_temp == 3) {
        aqtSetLineCapStyle(AQTSquareLineCapStyle);
      } else {
        aqtSetLineCapStyle(AQTButtLineCapStyle);
      }

      nlast = npts_temp;
      if (nlast > 1000) nlast = 1000;
      for (i = 0; i < nlast; i++) {
          /* iindx=2*i; */
          iindx=i;
          xpts_temp[i] = xpts[iindx];
          ypts_temp[i] = ypts[iindx];
      }

      if (npts_temp > 0) {
         aqtAddPolyline(xpts_temp, ypts_temp, npts_temp);
      }
   }

}

/* AQMOVE  - move to a point
 *
 * ax1    - contains the x coordinate
 * ay1    - contains the y coordinate
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqmove_(ax1, ay1)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQMOVE_(ax1,a1)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqmove(ax1,ay1)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQMOVE(ax1,ay1)
#endif
double   *ax1, *ay1;
{
   float   ax1_temp;
   float   ay1_temp;

   ax1_temp = *ax1;
   ay1_temp = *ay1;

  aqtMoveTo(ax1_temp,ay1_temp);

}

/* AQSECO  - set the color
 *
 * jcol   - index for desired color
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqseco_(jcol)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQSECO_(jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqseco(jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQSECO(jcol)
#endif
int   *jcol;
{
   int    jcol_temp;
   float  avalue;

   jcol_temp = *jcol;

   if (OPEN_FLAG_AQUA > 0) {
      if (jcol_temp > 0) {
         aqtTakeColorFromColormapEntry(jcol_temp-1);
      } else {
         avalue = -(float)jcol_temp/100.;
         aqtSetColor(avalue,avalue,avalue);
      }
   }

}

/* AQSEC2  - set the color using RGB specification
 *
 * jred   - index for red component
 * jgreen - index for green component
 * jblue  - index for blue component
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqsec2_(jred,jgreen,jblue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQSEC2_(jred,jgreen,jblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqsec2(jred,jgreen,jblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQSEC2(jred,jgreen,jblue)
#endif
float   *jred;
float   *jgreen;
float   *jblue;
{
   float    avalue1;
   float    avalue2;
   float    avalue3;

   avalue1 = *jred;
   avalue2 = *jgreen;
   avalue3 = *jblue;

   if (avalue1 < 0) {
      avalue1 = 0.0;
   }
   if (avalue1 > 1) {
      avalue1=1.0;
   }
   if (avalue2 < 0) {
      avalue2 = 0.0;
   }
   if (avalue2 > 1) {
      avalue2=1.0;
   }
   if (avalue3 < 0) {
      avalue3 = 0.0;
   }
   if (avalue3 > 1) {
      avalue3=1.0;
   }

   if (OPEN_FLAG_AQUA > 0) {
      aqtSetColor(avalue1,avalue2,avalue3);
   }

}

/* AQSEPA  - set line attribute (color set in AQSECO):
 *
 * jpatt   - the line pattern
 * pthick  - the line thickness (in points)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqsepa_(xpatt,npatt,pthick,iopt)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQSEPA_(xpatt,npatt,pthick,iopt)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqsepa(xpatt,npatt,pthick,iopt)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQSEPA(xpatt,npatt,pthick,iopt)
#endif
double  xpatt[];
double *pthick;
int    *npatt;
int    *iopt;
{
   int     npatt_temp;
   int     iopt_temp;
   int     iindx;
   int     i;
   int     nlast;
   float   pthick_temp;
   float   xpatt_temp[8];

   pthick_temp = *pthick;
   npatt_temp  = *npatt;
   iopt_temp   = *iopt;

   nlast = npatt_temp;
   if (nlast > 7) nlast = 7;
   for (i = 0; i < nlast; i++) {
       /* iindx=2*i; */
       iindx=i;
       xpatt_temp[i] = xpatt[iindx];
   }

   if (OPEN_FLAG_AQUA > 0) {
      if (iopt_temp == 1) {
         if (npatt_temp <= 1) {
            aqtSetLinestyleSolid();
         } else {
            aqtSetLinestylePattern(xpatt_temp,npatt_temp,0.0);
         }
      }
      if (iopt_temp == 2) {
         aqtSetLinewidth(pthick_temp);
      }
   }

}

/* AQSESI  - set character size
 *
 * pheigh   - the desired point size
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqsesi_(pheigh)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQSESI_(pheigh)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqsesi(pheigh)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQSESI(pheigh)
#endif
double *pheigh;
{
   float   pheigh_temp;

   pheigh_temp = *pheigh;

   if (OPEN_FLAG_AQUA > 0) {
      aqtSetFontsize(pheigh_temp);
   }

}

/* AQPOIN - draw a point.
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 * jcol   - color to use in drawing the point
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqpoin_(ix, iy, ired, igreen, iblue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQPOIN_(ix, iy, ired, igreen, iblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqpoin(ix, iy, ired, igreen, iblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQPOIN(ix, iy, ired, igreen, iblue)
#endif
int   *ix, *iy;
int   *ired, *igreen, *iblue;
{

   unsigned char rgbImage[3] = {255, 255, 255};
   int    ixtemp;
   int    iytemp;
   int    ired_temp;
   int    igreen_temp;
   int    iblue_temp;

   ixtemp = *ix;
   iytemp = *iy;
   ired_temp   = *ired;
   igreen_temp = *igreen;
   iblue_temp  = *iblue;

   rgbImage[0] = ired_temp;
   rgbImage[1] = igreen_temp;
   rgbImage[2] = iblue_temp;

   aqtAddImageWithBitmap(rgbImage,1,1,ixtemp,iytemp,4,4);

}

/* AQRECT - fill a rectangle.
 *
 * ix1    - start x position
 * iy1    - start y position
 * ix2    - length of x
 * iy2    - length of y
 *          otherwise, a convex polygon)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqrect_(ix1, iy1, ix2, iy2)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQRECT_(ix1, iy1, ix2, iy2)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqrect(ix1, iy1, ix2, iy2)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQRECT(ix1, iy1, ix2, iy2)
#endif
int   *ix1;
int   *iy1;
int   *ix2;
int   *iy2;
{
   int     ix1_temp;
   int     iy1_temp;
   int     ix2_temp;
   int     iy2_temp;

   ix1_temp = *ix1;
   iy1_temp = *iy1;
   ix2_temp = *ix2;
   iy2_temp = *iy2;

   if (OPEN_FLAG_AQUA > 0) {
      aqtAddFilledRect(ix1_temp, iy1_temp, ix2_temp, iy2_temp);
   }

}

/* AQRGFL - fill a region.  Rectangular regions will be filled differently
 *          non-rectangular regions.  Dataplot only handles convex polygons,
 *          so set this (for faster performance).  This routine only does
 *          solid fills.  Hatch patterns must be drawn
 *          by the calling program (i.e., send the individual lines to
 *          the AQDRAW routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points in the polygon (if 2, assume a rectangle,
 *          otherwise, a convex polygon)
 *
 */
#define MAX_REG_POINTS  100
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqrgfl_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQRGFL_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqrgfl(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQRGFL(xpts, ypts, npts)
#endif
double   xpts[], ypts[];
int   *npts;
{
   int     i;
   int     iindx;
   int     npts_temp;
   int     nlast;
   float   xpts_temp[MAX_REG_POINTS];
   float   ypts_temp[MAX_REG_POINTS];

   npts_temp = *npts;
   nlast = MAX_REG_POINTS;
   if (nlast > MAX_REG_POINTS) nlast = MAX_REG_POINTS;
   for (i = 0; i < nlast; i++) {
       /* iindx=2*i; */
       iindx=i;
       xpts_temp[i] = xpts[iindx];
       ypts_temp[i] = ypts[iindx];
   }

   if (OPEN_FLAG_AQUA > 0) {
      aqtAddPolygon(xpts_temp, ypts_temp, npts_temp);
   }

}

/* AQTXTH - draw a horizontal text string.
 *
 * string - text string to draw
 * ixpos  - x position
 * iypos  - y position
 * ijusth - justification (horizontal)
 *          0 - left justified
 *          1 - center justified
 *          2 - right justified
 * ijustv - justiciation (vertical)
 *          0 - center justified
 *          1 - bottom justified
 *          2 - top justified
 * font   - font name
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqtxth_(string, ixpos, iypos, ijusth, ijustv, font, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQTXTH_(string, ixpos, iypos, ijusth, ijustv, font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqtxth(string, ixpos, iypos, ijusth, ijustv, font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQTXTH(string, ixpos, iypos, ijusth, ijustv, font, error)
#endif
int    string[];
int    font[];
int    *ixpos;
int    *iypos;
int    *ijusth;
int    *ijustv;
int    *error;
{

   int    len;                     /* number of characters in string */
   int    len2;                    /* number of characters in string */
   int    string_width;            /* width of string in pixels */
   char   string2[130];            /* converted string */
   char   font2[130];              /* font name */
   int    i;
   int    ixpos_temp;
   int    iypos_temp;
   int    ijusth_temp;
   int    ijustv_temp;
   int    error_temp;
   float  xpos;
   float  ypos;

   ixpos_temp = *ixpos;
   iypos_temp = *iypos;
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;

   i_to_s_4(string, string2, 130, &len);
   i_to_s_4(font, font2, 130, &len2);

   if (len2 > 1) {
      aqtSetFontname(font2);
   }

   xpos = (float)ixpos_temp;
   ypos = (float)iypos_temp;
   if (ijusth_temp == 0 && ijustv_temp == 0) {   /* Left-bottom */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignLeft | AQTAlignBottom));
   } else if (ijusth_temp == 0 && ijustv_temp == 1) {   /* Left-center */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignLeft | AQTAlignMiddle));
   } else if (ijusth_temp == 0 && ijustv_temp == 2) {   /* Left-Top */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignLeft | AQTAlignTop));
   } else if (ijusth_temp == 1 && ijustv_temp == 0) {   /* Center-bottom */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignCenter | AQTAlignBottom));
   } else if (ijusth_temp == 1 && ijustv_temp == 1) {   /* Center-Center */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignCenter | AQTAlignMiddle));
   } else if (ijusth_temp == 1 && ijustv_temp == 2) {   /* Center-Top */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignCenter | AQTAlignTop));
   } else if (ijusth_temp == 2 && ijustv_temp == 0) {   /* Right-bottom */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignRight | AQTAlignBottom));
   } else if (ijusth_temp == 2 && ijustv_temp == 1) {   /* Right-Center */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignRight | AQTAlignMiddle));
   } else if (ijusth_temp == 2 && ijustv_temp == 2) {   /* Right-Top */
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignRight | AQTAlignTop));
   } else {
      aqtAddLabel(string2,xpos,ypos,0.0,(AQTAlignLeft | AQTAlignBottom));
   }

}

/* AQTXTV - draw a vertical text string.
 *
 * string - text string to draw
 * ixpos  - x position
 * iypos  - y position
 * ijusth - justification (horizontal)
 *          0 - left justified
 *          1 - center justified
 *          2 - right justified
 * ijustv - justiciation (vertical)
 *          0 - center justified
 *          1 - bottom justified
 *          2 - top justified
 * font   - font name
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqtxtv_(string, ixpos, iypos, ijusth, ijustv, font, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQTXTV_(string, ixpos, iypos, ijusth, ijustv, font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqtxtv(string, ixpos, iypos, ijusth, ijustv, font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQTXTV(string, ixpos, iypos, ijusth, ijustv, font, error)
#endif
int    string[];
int    font[];
int    *ixpos;
int    *iypos;
int    *ijusth;
int    *ijustv;
int    *error;
{

   int    len;                     /* number of characters in string */
   int    string_width;            /* width of string in pixels */
   char   string2[130];            /* converted string */
   char   font2[130];              /* font name */
   int    i;
   int    ixpos_temp;
   int    iypos_temp;
   int    ijusth_temp;
   int    ijustv_temp;
   int    error_temp;
   float  xpos;
   float  ypos;

   ixpos_temp = *ixpos;
   iypos_temp = *iypos;
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;

   i_to_s_4(string, string2, 130, &len);
   i_to_s_4(font, font2, 130, &len);

   xpos = (float)ixpos_temp;
   ypos = (float)iypos_temp;
   if (ijusth_temp == 0 && ijustv_temp == 0) {   /* Left-bottom */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignLeft | AQTAlignBottom));
   } else if (ijusth_temp == 0 && ijustv_temp == 1) {   /* Left-center */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignLeft | AQTAlignMiddle));
   } else if (ijusth_temp == 0 && ijustv_temp == 2) {   /* Left-Top */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignLeft | AQTAlignTop));
   } else if (ijusth_temp == 1 && ijustv_temp == 0) {   /* Center-bottom */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignCenter | AQTAlignBottom));
   } else if (ijusth_temp == 1 && ijustv_temp == 1) {   /* Center-Center */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignCenter | AQTAlignMiddle));
   } else if (ijusth_temp == 1 && ijustv_temp == 2) {   /* Center-Top */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignCenter | AQTAlignTop));
   } else if (ijusth_temp == 2 && ijustv_temp == 0) {   /* Right-bottom */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignRight | AQTAlignBottom));
   } else if (ijusth_temp == 2 && ijustv_temp == 1) {   /* Right-Center */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignRight | AQTAlignMiddle));
   } else if (ijusth_temp == 2 && ijustv_temp == 2) {   /* Right-Top */
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignRight | AQTAlignTop));
   } else {
      aqtAddLabel(string2,xpos,ypos,90.0,(AQTAlignLeft | AQTAlignBottom));
   }

}

/* AQRDLO  - routine to read a position from the graphics window (used
 *           by the Dataplot cross-hair command).  The mouse position at
 *           the next mouse click will be determined.
 *
 *           This routine will wait until a mouse button is pressed.
 *
 * ixret   - x coordinate of mouse position when button pressed
 * iyret   - y coordinate of mouse position when button pressed
 * error   - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void aqrdlo_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void AQRDLO_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void aqrdlo(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void AQRDLO(ixret, iyret, error)
#endif
int  *ixret, *iyret, *error;
{
     int running = 0;
     char buffer[AQT_EVENTBUF_SIZE];
     int      x, y;


       *ixret = -1;
       *iyret = -1;
       *error = 0;

       running = 0;
       while (running == 0) {         /* loop until mouse click */

          aqtSelectPlot(1);
          aqtWaitNextEvent(buffer);
          switch (decodeEvent(buffer, &x, &y)) {

          case 0:            /* Null Event */
              *error = 1;
              running = 1;
              break;

          case -1:            /* Error */
              *error = 1;
              running = 1;
              break;

          case 1:            /* Mouse Click */
              *ixret = x;
              *iyret = y;
              running = 1;
              break;

          default:
              *error = 1;
              running = 1;
              break;
          }       /* end switch */
       }          /* end while */

}

/* i_to_s_4  - utitlity routine to convert an integer array containing
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
void i_to_s_4(string1, string2, maxlen, ilen)
int   string1[], maxlen, *ilen;
char  string2[];

{
     int  i;
     int  itemp;
     i = 0;
     while (string1[i] != 0 && i < (maxlen - 1) ) {
         itemp = string1[i];
         string2[i] = string1[i];
         i++;
     }
     *ilen = i;
     string2[i]='\0';
}

int decodeEvent(char *event, int *x, int *y)
{
   /* int x, y; */
   char **ap, *argv[10];
   
   /* Split arguments separated by a ':'  */
   for (ap = argv; (*ap = strsep(&event, ":")) != NULL;)
      if (**ap != '\0')
         if (++ap >= &argv[10])
            break;

   if (strcmp(argv[0], "42") >= 0)        /* Check for server error */
      return -1; 

   if (argv[0][0] != '1')                 /* Only check for mouse clicks */
      return 0;
   
   /* sscanf(argv[1], "{%d ,%d}", &x, &y);  */  /* Decode Position */
   return 1;
}


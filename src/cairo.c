/*  cairo.c
 *
 *  Initial Implementation: 2017/10
 *
 *  The purpose of this library is to provide easy access from
 *  a Fortran program to the Cairo library.  Cairo is a graphics
 *  library that supports a number of common devices.  It has a
 *  structure that is modeled on Postscript.
 *
 *  Note that since one of our motivations for supporting Cairo
 *  in Dataplot is to have a consistent appearance for graphs
 *  on the screen and to Postscript/PDF files, we allow for
 *  two Cairo devices to be open at the same time.
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
 *
 *  The following routines are included:
 *
 *  cachec      - check for expose and configure events
 *  cainit      - initialize Cairo
 *  caend       - close Cairo
 *  carend      - render the current plot
 *  caflsh      - flush current plot
 *  caeras      - start a new graph (close currently open one as well)
 *  cadraw      - draw a polyline
 *  camove      - move to a point
 *  caseco      - set foreground color (based on RGB triplet)
 *  casepa      - set line pattern
 *  capoin      - draw a point (i.e., a pixel)
 *  cacirc      - draw a circle
 *  cargfl      - solid fill of a region
 *  catxth      - draw a horizontal character string
 *  catxtv      - draw a vertical character string
 *  carelo      - read mouse position
 *  i_to_s_5    - utility routine to convert array of ADE's to string
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
#include <strings.h>
#
#ifdef HAVE_X11
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#endif
#
#include <cairo/cairo.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-svg.h>
#
#ifdef HAVE_MAC_OSX
#include <cairo/cairo-quartz.h>
#endif
#
#ifdef HAVE_X11
#include <cairo/cairo-xlib.h>
#include <cairo/cairo-xlib-xrender.h>
#endif
#
#include <cairo/cairo-svg.h>

/* global definitions */

cairo_surface_t *surface1;
cairo_t *cr1;
FILE *file1;
cairo_surface_t *surface2;
cairo_t *cr2;
FILE *file2;
cairo_surface_t *surface3;
cairo_t *cr3;
FILE *file3;

#if HAVE_X11
Display *dsp;
Drawable da;
Screen *scr;
int screen;
unsigned long white;       /* values for black and white */
unsigned long black;       /* values for black and white */
XEvent        event;       /* holds X server events */
int           expose_flag; /* 0 - expose status has not changed,
                              1 - expose status has changed */
int           configure_flag; /* 0 - configuration up to date,
                                 1 - configuration has been changed */
XTextProperty  WindowName, IconName;
char *window_name = "Dataplot";
char *icon_name = "Dataplot";
XSetWindowAttributes  window_attributes;    /* attributes structure */
XWindowAttributes     returned_attributes;  /* returned attributes */
unsigned long value_mask;         /* value mask for window attributes */
#define DEFAULT_X_SIZE  550
#define DEFAULT_Y_SIZE  425
#define DEFAULT_X        50
#define DEFAULT_Y        50
#define MIN_X_SIZE      200
#define MIN_Y_SIZE      200
static XSizeHints                 /* size hints for window manager */
    xsh = {
       (PPosition | PSize | PMinSize),   /* flags */
       DEFAULT_X_SIZE,                   /* height */
       DEFAULT_Y_SIZE,                   /* width */
       MIN_Y_SIZE,                       /* minimum height */
       MIN_X_SIZE,                       /* minimum width */
       DEFAULT_X,                        /* x coordinate */
       DEFAULT_Y,                        /* y coordinate */
    };
static XWMHints       /* more hints for window manager */
    xwmh = {
       (InputHint | StateHint),         /* flags */
       False,                           /* input */
       NormalState,                     /* initial state */
       0,                               /* icon pixmap */
       0,                               /* icon window */
       0, 0,                            /* icon location */
       0,                               /* icon mask */
       0                                /* window group */
    };
static XClassHint    /* class hints for window manager */
    xch = {
        "dataplot",                      /* name */
        "Graphics",                      /* class */
    };
static char  *argv[] = {                 /* dummy command line arguments */
                          (char *)NULL   /* required by a few calls */
                        };
static int   argc = 0;

#endif

/* Save file name for PNG device */
char     file_string_png_2[160];
char     file_string_png_3[160];

/* int decodeEvent(char *event, int *x, int *y); */

/* flags for current attribute settings */
static int    OPEN_FLAG_1 = 0;        /* 0 - closed, 1 - open */
static int    OPEN_FLAG_2 = 0;        /* 0 - closed, 1 - open */
static int    OPEN_FLAG_3 = 0;        /* 0 - closed, 1 - open */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  caend_(),  cadraw_(), capoin_(), cacirc_(), cargfl_();
void  cainit_(), caeras_(), catxth_(), catxtv_();
void  caseco_(), casepa_(), carend_(), caqrelo_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  CAEND_(),  CAINIT_(), CADRAW_(), CAPOIN_(), CACIRC_(), CARGFL_();
void  CAINIT_(), CAERAS_(), CATXTH_(), CATXTV_();
void  CASECO_(), CASEPA_(), CAREND_(), CARELO_(),;
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  caend(),  cadraw(), capoin(), cacirc(), cargfl();
void  cainit(), caeras(), catxth(), catxtv(), catatt();
void  caseco(), casepa(), carend(), carelo();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  CAEND(),  CADRAW(), CAPOIN(), CACIRC(), CARGFL();
void  CAINIT(), CAERAS(), CATXTH(), CATXTV();
void  CASECO(), CASEPA(), CARELO();
#endif
void  i_to_s_5();

/* CACHEC  - routine to check for X expose and configuration events.
 *           Specifically, resizing of the graphics window by the window
 *           manager (usually initiated by the user) generates a
 *           configuration event.  Placing windows on top of the graphics
 *           window and then re-raising it will generate an expose event.
 *           Dataplot makes no attempt to check if the part of the
 *           graphics window is not visible.  It simply draws and lets the
 *           window manager do any required clipping.
 *
 *           Note that this routine will not wait for any events.  It will
 *           simply check if there are any currently in the event queue.
 *
 *           Also, no action is taken for expose and configure events.
 *           This routine simply sets a flag indicating that these events
 *           were found.  It is the calling programs option as to what
 *           should be done about them.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void cachec_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CACHEC_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void cachec(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CACHEC(expose_flag_2, error_flag)
#endif
#if INTEGER_PRECISION == 0
int  *error_flag;
int  *expose_flag_2;
#else
int  error_flag[2];
int  expose_flag_2[2];
#endif
{
#ifdef HAVE_X11
     /* XEvent   event; */           /* holds Xserver events */
#endif

     *error_flag = 0;
     *expose_flag_2 = 0;
/*
#if INTEGER_PRECISION == 0
     *error_flag = 0;
     *expose_flag_2 = 0;
#else
     error_flag[0] = 0;
     expose_flag_2[0] = 0;
#endif
 */
#ifdef HAVE_X11
     while (XEventsQueued(dsp, QueuedAfterReading) != 0) {/* check queue */
          XNextEvent(dsp, &event);        /* get next event from queue */
          switch (event.type) {
          case DestroyNotify:   /* window has been destroyed */
              cairo_surface_destroy(surface1);
              cairo_destroy(cr1);
              XCloseDisplay(dsp);
              OPEN_FLAG_2 = 0;
              *error_flag = 1;
#if INTEGER_PRECISION == 0
              *error_flag = 1;
#else
              error_flag[0] = 1;
#endif
              break;
          case Expose:          /* portion of window has become visible */
              if(event.xexpose.count == 0) {
                expose_flag = 1;
              }
              *expose_flag_2 = 1;
#if INTEGER_PRECISION == 0
              *expose_flag_2 = 1;
#else
              expose_flag_2[0] = 1;
#endif
              break;
          case ConfigureNotify: /* window has been reconfigured */
              configure_flag = 1;
              break;
          default:
              break;
          }                     /* end switch */
     }                          /* end while */
#endif
}

/* CAINIT  - routine to initialize Cairo.
 *
 * Dataplot currently supports the following Cairo devices:
 *
 *    1 - X11
 *    2 - Postscript
 *    3 - PDF
 *    4 - SVG
 *    5 - Quartz
 *    6 - PNG
 *    7 - WIN32
 *    8 - Encapsulated Postscript
 *
 *  Note that Dataplot can support 3 devices simultaneously, so
 *  allow each of these to support a Cairo driver.
 *
 *  The parameter idev can be set to either 1, 2 or 3 (if outside this
 *  range, then set it to 1).  The imodel parameter identifies
 *  which device is being opened.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void cainit_(idev,imodel,file_name,anumhp,anumvp,jred,jgreen,jblue,error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAINIT_(idev,imodel,file_name,anumhp,anumvp,jred,jgreen,jblue,error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void cainit(idev,imodel,file_name,anumhp,anumvp,jred,jgreen,jblue,error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAINIT(idev,imodel,fname,anumhp,anumvp,jred,jgreen,jblue,error_flag)
#endif

int  file_name[];
#if INTEGER_PRECISION == 0
int  *idev;
int  *imodel;
int  *error_flag;
#else
int   idev[2];
int   imodel[2];
int   ierror_flag[2];
#endif
#if PRECISION == 0
double  *anumhp;
double  *anumvp;
double  *jred;
double  *jblue;
double  *jgreen;
#else
double  anumhp[2];
double  anumvp[2];
double  jred[2];
double  jblue[2];
double  jgreen[2];
#endif

{
   double   anumhp_temp;
   double   anumvp_temp;
   double   jred_temp;
   double   jblue_temp;
   double   jgreen_temp;
   double   avalue1;
   double   avalue2;
   double   avalue3;
   int      idev_temp;
   int      imodel_temp;
   int      len;
   int      numhp;
   int      numvp;
   char     file_string[160];

#if INTEGER_PRECISION == 0
   idev_temp   = *idev;
   imodel_temp = *imodel;
   *error_flag = 0;
   i_to_s_5(file_name, file_string, 80, &len);
#else
   idev_temp   = idev[0];
   imodel_temp = imodel[0];
   error_flag[0] = 0;
   i_to_s_2(file_name, file_string, 160, &len);
#endif

#if PRECISION == 0
   anumhp_temp = *anumhp;
   anumvp_temp = *anumvp;
   avalue1     = *jred;
   avalue2     = *jgreen;
   avalue3     = *jblue;
#else
   anumhp_temp = anumhp[0];
   anumvp_temp = anumvp[0];
   avalue1     = jred[0];
   avalue2     = jgreen[0];
   avalue3     = jblue[0];
#endif

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

    if (idev_temp < 1) idev_temp = 1;
    if (idev_temp > 3) idev_temp = 1;

    if (idev_temp == 1) {                /*  Device 1 */
       if (OPEN_FLAG_1 == 0) {           /*  Device currently closed */
          OPEN_FLAG_1 = 1;
          if (imodel_temp == 1) {        /*  X11 device */
             numhp = (int)anumhp_temp;
             numvp = (int)anumvp_temp;
#ifdef HAVE_X11
             if ((dsp = XOpenDisplay(NULL)) == NULL) {
                OPEN_FLAG_1 = 0;
#if INTEGER_PRECISION == 0
                *error_flag = 1;
#else
                error_flag[0] = 1;
#endif
             } else {
                double  x1, y1, width, height;
                screen = DefaultScreen(dsp);
                scr = DefaultScreenOfDisplay(dsp);
                white = WhitePixel(dsp,screen);
                black = BlackPixel(dsp,screen);
                da = XCreateSimpleWindow(dsp, DefaultRootWindow(dsp), 0, 0, numhp, numvp, 0, black, white);
                window_attributes.bit_gravity = NorthWestGravity;
                value_mask = CWBitGravity;
                XChangeWindowAttributes(dsp, da, value_mask, &window_attributes);
                if (XStringListToTextProperty(&window_name, 1, &WindowName) == 0) {
                }
                if (XStringListToTextProperty(&icon_name, 1, &IconName) == 0) {
                }
                XSetWMProperties(dsp, da, &WindowName, &IconName, 
                                 argv, argc, &xsh, &xwmh, &xch);
                XSelectInput(dsp, da, StructureNotifyMask | ExposureMask );
                XMapWindow(dsp, da);
                while (XEventsQueued(dsp, QueuedAfterReading) != 0) {/* check queue */
                   XFlush(dsp);
                   do {
                      XNextEvent(dsp, &event);
                   } while (event.type != MapNotify || event.xmap.window != da);
                   XFlush(dsp);
                }
                surface1 = cairo_xlib_surface_create(dsp, da, DefaultVisual(dsp, screen), numhp, numvp);
                cairo_xlib_surface_set_size(surface1, numhp, numvp);
                cr1 = cairo_create(surface1);
                x1 = 0.;
                y1 = 0.;
                width=anumhp_temp;
                height=anumvp_temp;
                cairo_set_line_width(cr1,2.);
                cairo_set_source_rgb(cr1,avalue1,avalue2,avalue3);
                cairo_rectangle(cr1,x1,y1,width,height);
                cairo_stroke_preserve(cr1);
                cairo_fill(cr1);
             }
#endif
          } else if (imodel_temp == 5) {    /*  Quartz device */
#ifdef CAIRO_HAS_QUARTZ_SURFACE
             double  x1, y1, width, height;
             int  width2, height2;
             width2=anumhp_temp;
             height2=anumvp_temp;
             surface1 = cairo_quartz_surface_create (cr1,width2,height2);
             cr1 = cairo_create (surface1);
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr1,2.);
             cairo_set_source_rgb(cr1,avalue1,avalue2,avalue3);
             cairo_rectangle(cr1,x1,y1,width,height);
             cairo_stroke_preserve(cr1);
             cairo_fill(cr1);
#endif
          } else if (imodel_temp == 7) {    /*  WIN32 GDI device */
             /*
             double  x1, y1, width, height;
             surface1 = cairo_win32_surface_create (hdc);
             cr1 = cairo_create (surface1);
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr1,2.);
             cairo_set_source_rgb(cr1,avalue1,avalue2,avalue3);
             cairo_rectangle(cr1,x1,y1,width,height);
             cairo_stroke_preserve(cr1);
             cairo_fill(cr1);
             */
          } else {
#if INTEGER_PRECISION == 0
     *error_flag = 2;
#else
     error_flag[0] = 2;
#endif
          }
       }
    } else if (idev_temp == 2) {           /*  Device 2 */
       if (OPEN_FLAG_2 == 0) {             /*  Device currently closed */
          OPEN_FLAG_2 = 1;
          if (imodel_temp == 2) {    /*  Postscript device */
             double  x1, y1, width, height;
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             surface2 = cairo_ps_surface_create (file_string, anumhp_temp, anumvp_temp);
             cr2 = cairo_create (surface2);
             /* Set background color */
             cairo_set_line_width(cr2,2.);
             cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
             cairo_rectangle(cr2,x1,y1,anumhp_temp,anumvp_temp);
             cairo_stroke_preserve(cr2);
             cairo_fill(cr2);
          } else if (imodel_temp == 3) {        /*  PDF device */
             double  x1, y1, width, height;
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             surface2 = cairo_pdf_surface_create (file_string, width, height);
             cr2 = cairo_create (surface2);
             /* Set background color */
             cairo_set_line_width(cr2,2.);
             cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
             cairo_rectangle(cr2,x1,y1,width,height);
             cairo_stroke_preserve(cr2);
             cairo_fill(cr2);
          } else if (imodel_temp == 4) {    /*  SVG device */
             double  x1, y1, width, height;
             surface2 = cairo_svg_surface_create (file_string, anumhp_temp, anumvp_temp);
             cr2 = cairo_create (surface2);
             /* Set background color */
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr2,2.);
             cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
             cairo_rectangle(cr2,x1,y1,width,height);
             cairo_stroke_preserve(cr2);
             cairo_fill(cr2);
          } else if (imodel_temp == 5) {    /*  Quartz device */
          } else if (imodel_temp == 6) {    /*  PNG device */
             double  x1, y1, width, height;
             int i;
             for (i = 0; i < 160; i++ ) {
                 file_string_png_2[i] = file_string[i];
             }
             surface2 = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                       anumhp_temp, anumvp_temp);
             cr2 = cairo_create (surface2);
             /* Set background color */
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr2,2.);
             cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
             cairo_rectangle(cr2,x1,y1,width,height);
             cairo_stroke_preserve(cr2);
             cairo_fill(cr2);
          } else if (imodel_temp == 7) {    /*  WIN32 GDI device */
             /*
             double  x1, y1, width, height;
             surface2 = cairo_win32_surface_create (hdc);
             cr2 = cairo_create (surface2);
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr2,2.);
             cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
             cairo_rectangle(cr2,x1,y1,width,height);
             cairo_stroke_preserve(cr2);
             cairo_fill(cr2);
             */
          } else if (imodel_temp == 8) {   /*  encapsulated Postscript device */
             double  x1, y1, width, height;
             cairo_bool_t eps;
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             surface2 = cairo_ps_surface_create (file_string, width, height);
             eps = 1;
             cairo_ps_surface_set_eps(surface2, eps);
             cr2 = cairo_create (surface2);
             /* Set background color */
             cairo_set_line_width(cr2,2.);
             cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
             cairo_rectangle(cr2,x1,y1,width,height);
             cairo_stroke_preserve(cr2);
             cairo_fill(cr2);
          } else {
#if INTEGER_PRECISION == 0
     *error_flag = 2;
#else
     error_flag[0] = 1;
#endif
          }
       }
    } else if (idev_temp == 3) {          /*  Device 2 */
       if (OPEN_FLAG_3 == 0) {           /*  Device currently closed */
          OPEN_FLAG_3 = 1;
          if (imodel_temp == 2) {  /*  Postscript device */
             double  x1, y1, width, height;
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             surface3 = cairo_ps_surface_create (file_string, width,height);
             cr3 = cairo_create (surface3);
             /* Set background color */
             cairo_set_line_width(cr3,2.);
             cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
             cairo_rectangle(cr3,x1,y1,width,height);
             cairo_stroke_preserve(cr3);
             cairo_fill(cr3);
          } else if (imodel_temp == 3) {        /*  PDF device */
             double  x1, y1, width, height;
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             surface3 = cairo_pdf_surface_create (file_string, width,height);
             cr3 = cairo_create (surface3);
             /* Set background color */
             cairo_set_line_width(cr3,2.);
             cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
             cairo_rectangle(cr3,x1,y1,width,height);
             cairo_stroke_preserve(cr3);
             cairo_fill(cr3);
          } else if (imodel_temp == 4) {    /*  SVG device */
             double  x1, y1, width, height;
             surface3 = cairo_svg_surface_create (file_string, anumhp_temp, anumvp_temp);
             cr3 = cairo_create (surface3);
             /* Set background color */
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr3,2.);
             cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
             cairo_rectangle(cr3,x1,y1,width,height);
             cairo_stroke_preserve(cr3);
             cairo_fill(cr3);
          } else if (imodel_temp == 5) {    /*  Quartz device */
          } else if (imodel_temp == 6) {    /*  PNG device */
             double  x1, y1, width, height;
             int i;
             for (i = 0; i < 160; i++ ) {
                 file_string_png_3[i] = file_string[i];
             }
             surface3 = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                       anumhp_temp, anumvp_temp);
             cr3 = cairo_create (surface3);
             /* Set background color */
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr3,2.);
             cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
             cairo_rectangle(cr3,x1,y1,width,height);
             cairo_stroke_preserve(cr3);
             cairo_fill(cr3);
          } else if (imodel_temp == 7) {    /*  WIN32 GDI device */
             /*
             double  x1, y1, width, height;
             surface3 = cairo_win32_surface_create (hdc);
             cr3 = cairo_create (surface3);
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             cairo_set_line_width(cr3,2.);
             cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
             cairo_rectangle(cr3,x1,y1,width,height);
             cairo_stroke_preserve(cr3);
             cairo_fill(cr3);
             */
          } else if (imodel_temp == 8) {        /*  encapsulated Postscript device */
             double  x1, y1, width, height;
             _Bool eps;
             x1 = 0.;
             y1 = 0.;
             width=anumhp_temp;
             height=anumvp_temp;
             surface3 = cairo_pdf_surface_create (file_string, width, height);
             eps=1;
             cairo_ps_surface_set_eps(surface3, eps);
             cr3 = cairo_create (surface3);
             /* Set background color */
             cairo_set_line_width(cr3,2.);
             cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
             cairo_rectangle(cr3,x1,y1,width,height);
             cairo_stroke_preserve(cr3);
             cairo_fill(cr3);
          } else {
#if INTEGER_PRECISION == 0
     *error_flag = 2;
#else
     error_flag[0] = 1;
#endif
          }
       }
    }
}

/* CAERAS  - routine to clear the screen.
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
 *  i
 *  xpixels   - width (in pixels) for graphics window
 *  ypixels   - height (in pixels) for graphics window
 *  back_col  - background color
 *  file name - file name (in integer ascii decimal equivalents)
 *
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void caeras_(idev, imodel, anumhp, anumvp, jred, jgreen, jblue, ixret, iyret)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAERAS_(idev, imodel, anumhp, anumvp, jred, jgreen, jblue, ixret, iyret)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void caeras(idev, imodel, anumhp, anumvp, jred, jgreen, jblue, ixret, iyret)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAERAS(idev, imodel, anumhp, anumvp, jred, jgreen, jblue, ixret, iyret)
#endif
#if INTEGER_PRECISION == 0
int  *idev;
int  *imodel;
int  *ixret;
int  *iyret;
#else
int   idev[2];
int   imodel[2];
int   ixret[2];
int   iyret[2];
#endif
#if PRECISION == 0
double  *anumhp;
double  *anumvp;
double  *jred;
double  *jblue;
double  *jgreen;
#else
double  anumhp[2];
double  anumvp[2];
double  jred[2];
double  jblue[2];
double  jgreen[2];
#endif
{

   int      idev_temp;
   int      imodel_temp;
   int      iflag_white;
   int      ixret_temp;
   int      iyret_temp;
   double   anumhp_temp;
   double   anumvp_temp;
   double   jred_temp;
   double   jblue_temp;
   double   jgreen_temp;
   double   avalue1;
   double   avalue2;
   double   avalue3;

#if INTEGER_PRECISION == 0
   idev_temp   = *idev;
   imodel_temp = *imodel;
#else
   idev_temp   = idev[0];
   imodel_temp = imodel[0];
   iback_temp  = iback[0];
#endif

#if PRECISION == 0
   anumhp_temp = *anumhp;
   anumvp_temp = *anumvp;
   avalue1     = *jred;
   avalue2     = *jgreen;
   avalue3     = *jblue;
#else
   anumhp_temp = anumhp[0];
   anumvp_temp = anumvp[0];
   avalue1     = jred[0];
   avalue2     = jgreen[0];
   avalue3     = jblue[0];
#endif

   /* If the background is white, skip the filled rectangle
    * to set the background. */
   iflag_white = 1;
   if (avalue1 == 1. && avalue2 == 1. && avalue3 == 1.) iflag_white = 0;

   /* Use "erase rectangle" method to clear screen  */
   if (idev_temp == 1) {                /*  Device 1 */
      if (OPEN_FLAG_1 > 0) {            /*  Device currently open */
         double  x1, y1, width, height, xscale, yscale;
         x1 = 0.;
         y1 = 0.;
#ifdef HAVE_X11
         if (imodel_temp == 1) {
            XFlush(dsp);
            XGetWindowAttributes(dsp,da,&returned_attributes);
            ixret_temp = returned_attributes.width;
            iyret_temp = returned_attributes.height;
         }
#else
         ixret_temp=cairo_xlib_surface_get_width(surface1);
         iyret_temp=cairo_xlib_surface_get_height(surface1);
#endif
         if (ixret_temp != (int)anumhp_temp || iyret_temp != (int)height) {
            width=(float)ixret_temp;
            height=(float)iyret_temp;
            xscale = width/anumhp_temp;
            yscale = height/anumvp_temp;
#ifdef HAVE_X11
            if (imodel_temp == 1) {
               cairo_xlib_surface_set_size(surface1,ixret_temp,iyret_temp);
            }
#endif
         } else {
            width=anumhp_temp;
            height=anumvp_temp;
         }
#if PRECISION == 0
         *ixret=(int)width;
         *iyret=(int)height;
#else
         ixret[0]=(int)width;
         iyret[0]=(int)height;
#endif
         /* if (iflag_white == 1) { */
            cairo_set_line_width(cr1,2.);
            cairo_set_source_rgb(cr1,avalue1,avalue2,avalue3);
            cairo_rectangle(cr1,x1,y1,width,height);
            cairo_stroke_preserve(cr1);
            cairo_fill(cr1);
         /* } */
      }
   } else if (idev_temp == 2) {            /*  Device 2 */
      if (OPEN_FLAG_2 > 0) {            /*  Device currently open */
         double  x1, y1, width, height;
         x1 = 0.;
         y1 = 0.;
         width=anumhp_temp;
         height=anumvp_temp;
         if (iflag_white == 1) {
            cairo_set_line_width(cr2,2.);
            cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
            cairo_rectangle(cr2,x1,y1,width,height);
            cairo_stroke_preserve(cr2);
            cairo_fill(cr2);
         }
      }
   } else if (idev_temp == 3) {            /*  Device 2 */
      if (OPEN_FLAG_3 > 0) {            /*  Device currently open */
         double  x1, y1, width, height;
         x1 = 0.;
         y1 = 0.;
         width=anumhp_temp;
         height=anumvp_temp;
         if (iflag_white == 1) {
            cairo_set_line_width(cr3,2.);
            cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
            cairo_rectangle(cr3,x1,y1,width,height);
            cairo_stroke_preserve(cr3);
            cairo_fill(cr3);
         }
      }
   }
}

/* CAEND   - routine to end Cairo display and close the display
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void caend_(idev,imodel)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAEND_(idev,imodel)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void caend(idev,imodel)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAEND(idev,imodel)
#endif
#if INTEGER_PRECISION == 0
int  *idev;
int  *imodel;
#else
int  idev[2];
int  imodel[2];
#endif

{

   int   idev_temp;
   int   imodel_temp;

#if INTEGER_PRECISION == 0
   idev_temp   = *idev;
   imodel_temp = *imodel;
#else
   idev_temp   = idev[0];
   imodel_temp = imodel[0];
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
#if HAVE_X11
         if (imodel_temp == 1) {        /*  X11 device */
            Display *dsp = cairo_xlib_surface_get_display(surface1);
         }
#endif
         cairo_surface_destroy(surface1);
         cairo_destroy(cr1);
#if HAVE_X11
         if (imodel_temp == 1) {        /*  X11 device */
            XCloseDisplay(dsp);
         }
#endif
         OPEN_FLAG_1 = 0;
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_surface_destroy(surface2);
         cairo_destroy(cr2);
         OPEN_FLAG_2 = 0;
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_surface_destroy(surface3);
         cairo_destroy(cr3);
         OPEN_FLAG_3 = 0;
      }
   }
}

/* CAFLSH - routine to flush Cairo display (called by GRCLDE,
 *          use to make sure current plot is completed)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void caflsh_(idev,imodel)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAFLSH_(idev,imodel)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void caflsh(idev,imodel)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAFLSH(idev,imodel)
#endif
#if INTEGER_PRECISION == 0
int  *idev;
int  *imodel;
#else
int  idev[2];
int  imodel[2];
#endif

{

   int   idev_temp;
   int   imodel_temp;

#if INTEGER_PRECISION == 0
   idev_temp   = *idev;
   imodel_temp = *imodel;
#else
   idev_temp   = idev[0];
   imodel_temp = imodel[0];
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
#ifdef HAVE_X11
         XFlush(dsp);
#endif
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_surface_flush(surface2);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_surface_flush(surface3);
      }
   }
}

/* CAREND   - routine to render current plot
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void carend_(idev,imodel)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAREND_(idev,imodel)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void carend(idev,imodel)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAREND(idev,imodel)
#endif

#if INTEGER_PRECISION == 0
int  *idev;
int  *imodel;
#else
int  idev[2];
int  imodel[2];
#endif

{

   int   idev_temp;
   int   imodel_temp;

#if INTEGER_PRECISION == 0
   idev_temp   = *idev;
   imodel_temp = *imodel;
#else
   idev_temp   = idev[0];
   imodel_temp = imodel[0];
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         cairo_surface_flush(surface1);
         /* cairo_show_page(cr1); */
         cairo_surface_show_page(surface1);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         /* cairo_surface_flush(surface2); */
         if (imodel_temp == 2 || imodel_temp == 3) {
            cairo_surface_show_page(surface2);
         } else if (imodel_temp == 8) {
            cairo_show_page(cr2);
         } else if (imodel_temp == 4) {    /* SVG device */
            cairo_show_page(cr2);
         } else if (imodel_temp == 6) {    /* PNG device */
            cairo_surface_write_to_png(surface2,file_string_png_2);
         }
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         /* cairo_surface_flush(surface3); */
         if (imodel_temp == 2 || imodel_temp == 3 || imodel_temp == 8) {
            cairo_surface_show_page(surface3);
         } else if (imodel_temp == 4 ) {
            cairo_show_page(cr3);
         } else if (imodel_temp == 6) {    /* PNG device */
            cairo_surface_write_to_png(surface3,file_string_png_3);
         }
      }
   }
}

/* CADRAW  - draw a polyline.  The line attributes are set in
 *           other routines.
 *
 * idev   - device 1, device 2, or device 3
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points to plot
 * icap   - cap style:  2 = Butt
 *                      3 = Round
 *                      4 = Square
 *                      1 = None
 * ijoin  - join style: 2 = Miter
 *                      3 = Round
 *                      4 = Bevel
 *                      1 = None
 * ipatt  - line pattern:  1 = Solid
 *                         2 = Dash
 *                         3 = Dotted
 *                         4 = Da1
 *                         5 = Da2
 *                         6 = Da3
 *                         7 = Da4
 *                         8 = Da5
 *       
 * pthick - line thickness
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void cadraw_(idev, xpts, ypts, npts, icap, ijoin, ipatt, pthick)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CADRAW_(idev, xpts, ypts, npts, icap, ijoin, ipatt, pthick)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void cadraw(idev, xpts, ypts, npts, icap, ijoin, ipatt, pthick)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CADRAW(idev, xpts, ypts, npts, icap, ijoin, ipatt, pthick)
#endif
double   xpts[], ypts[];
#if INTEGER_PRECISION == 0
int   *idev;
int   *npts;
int   *icap;
int   *ijoin;
int   *ipatt;
#else
int   idev[2];
int   npts[2];
int   icap[2];
int   ijoin[2];
int   ipatt[2];
#endif
#if PRECISION == 0
double  *pthick;
#else
double  pthick[2];
#endif
{
   int     npts_temp;
   int     icap_temp;
   int     ijoin_temp;
   int     ipatt_temp;
   int     idev_temp;
   int     i;
   int     nlast;
   int     iindx;
   float   xpts_temp[1000];
   float   ypts_temp[1000];
   double  pthick_temp;

#if INTEGER_PRECISION == 0
   npts_temp  = *npts;
   icap_temp  = *icap;
   ijoin_temp = *ijoin;
   ipatt_temp = *ipatt;
   idev_temp  = *idev;
#else
   npts_temp  = *npts[0];
   icap_temp  = *icap[0];
   ijoin_temp = *ijoin[0];
   ipatt_temp = *ipatt[0];
   idev_temp = *idev[0];
#endif

   nlast = npts_temp;
   if (nlast > 1000) nlast = 1000;
#if PRECISION == 0
   pthick_temp = *pthick;
   for (i=0; i < npts_temp; i++) {
       iindx=i;
       xpts_temp[i] = xpts[iindx];
       ypts_temp[i] = ypts[iindx];
   }
#else
   pthick_temp = *pthick[0];
   for (i=0; i < npts_temp; i++) {
       iindx=2*i;
       xpts_temp[i] = xpts[iindx];
       ypts_temp[i] = ypts[iindx];
#endif


   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {

         if (icap_temp == 1) {
           cairo_set_line_cap(cr1, CAIRO_LINE_CAP_BUTT);
         } else if (icap_temp == 2) {
           cairo_set_line_cap(cr1, CAIRO_LINE_CAP_BUTT);
         } else if (icap_temp == 3) {
           cairo_set_line_cap(cr1, CAIRO_LINE_CAP_ROUND);
         } else if (icap_temp == 4) {
           cairo_set_line_cap(cr1, CAIRO_LINE_CAP_SQUARE);
         }

         if (ijoin_temp == 1) {
           cairo_set_line_join(cr1, CAIRO_LINE_JOIN_MITER);
         } else if (ijoin_temp == 2) {
           cairo_set_line_join(cr1, CAIRO_LINE_JOIN_MITER);
         } else if (ijoin_temp == 3) {
           cairo_set_line_join(cr1, CAIRO_LINE_JOIN_ROUND);
         } else if (ijoin_temp == 4) {
           cairo_set_line_join(cr1, CAIRO_LINE_JOIN_BEVEL);
         }

         if (ipatt_temp == 1) {               /*  Solid Line */
            double dashes[] = {10.};
            int ndash = 0;
            double offset = 0.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 2) {       /*  Dashed Line */
            double dashes[] = {10., 10.0};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 3) {       /*  Dotted */
            double dashes[] = {2.,6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -2.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 4) {       /*  Dash 1  */
            double dashes[] = {10., 10., 5., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -20.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 5) {       /*  Dash 2  */
            double dashes[] = {10., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 6) {       /*  Dash 3  */
            double dashes[] = {5., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -5.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 7) {       /*  Dash 4 */
            double dashes[] = {10., 10., 2., 6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else if (ipatt_temp == 8) {      /*  Dash 5  */
            double dashes[] = {10., 5., 2., 6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         } else {                          /* No Match (Solid) */
            double dashes[] = {10};
            int ndash = 0;
            double offset = 0.0;
            cairo_set_dash(cr1, dashes, ndash, offset);
         }
         cairo_set_line_width(cr1,pthick_temp);

         cairo_move_to(cr1,xpts_temp[0],ypts_temp[0]);
         for (i=1; i < npts_temp; i++) {
             cairo_line_to(cr1,xpts_temp[i],ypts_temp[i]);
         }
         cairo_stroke(cr1);
         cairo_surface_flush(surface1);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {

         if (icap_temp == 1) {
           cairo_set_line_cap(cr2, CAIRO_LINE_CAP_BUTT);
         } else if (icap_temp == 2) {
           cairo_set_line_cap(cr2, CAIRO_LINE_CAP_BUTT);
         } else if (icap_temp == 3) {
           cairo_set_line_cap(cr2, CAIRO_LINE_CAP_ROUND);
         } else if (icap_temp == 4) {
           cairo_set_line_cap(cr2, CAIRO_LINE_CAP_SQUARE);
         }

         if (ijoin_temp == 1) {
           cairo_set_line_join(cr2, CAIRO_LINE_JOIN_MITER);
         } else if (ijoin_temp == 2) {
           cairo_set_line_join(cr2, CAIRO_LINE_JOIN_MITER);
         } else if (ijoin_temp == 3) {
           cairo_set_line_join(cr2, CAIRO_LINE_JOIN_ROUND);
         } else if (ijoin_temp == 4) {
           cairo_set_line_join(cr2, CAIRO_LINE_JOIN_BEVEL);
         }

         if (ipatt_temp == 1) {               /*  Solid Line */
            double dashes[] = {10.};
            int ndash = 0;
            double offset = 0.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 2) {       /*  Dashed Line */
            double dashes[] = {10., 10.0};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 3) {       /*  Dotted */
            double dashes[] = {2.,6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -2.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 4) {       /*  Dash 1  */
            double dashes[] = {10., 10., 5., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -20.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 5) {       /*  Dash 2  */
            double dashes[] = {10., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 6) {       /*  Dash 3  */
            double dashes[] = {5., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -5.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 7) {       /*  Dash 4 */
            double dashes[] = {10., 10., 2., 6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else if (ipatt_temp == 8) {      /*  Dash 5  */
            double dashes[] = {10., 5., 2., 6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         } else {                          /* No Match (Solid) */
            double dashes[] = {10};
            int ndash = 0;
            double offset = 0.0;
            cairo_set_dash(cr2, dashes, ndash, offset);
         }
         cairo_set_line_width(cr2,pthick_temp);

         cairo_move_to(cr2,xpts_temp[0],ypts_temp[0]);
         for (i=1; i < npts_temp; i++) {
             cairo_line_to(cr2,xpts_temp[i],ypts_temp[i]);
         }
         cairo_stroke(cr2);

         cairo_set_line_width(cr2,2.0);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {

         if (icap_temp == 1) {
           cairo_set_line_cap(cr3, CAIRO_LINE_CAP_BUTT);
         } else if (icap_temp == 2) {
           cairo_set_line_cap(cr3, CAIRO_LINE_CAP_BUTT);
         } else if (icap_temp == 3) {
           cairo_set_line_cap(cr3, CAIRO_LINE_CAP_ROUND);
         } else if (icap_temp == 4) {
           cairo_set_line_cap(cr3, CAIRO_LINE_CAP_SQUARE);
         }

         if (ijoin_temp == 1) {
           cairo_set_line_join(cr3, CAIRO_LINE_JOIN_MITER);
         } else if (ijoin_temp == 2) {
           cairo_set_line_join(cr3, CAIRO_LINE_JOIN_MITER);
         } else if (ijoin_temp == 3) {
           cairo_set_line_join(cr3, CAIRO_LINE_JOIN_ROUND);
         } else if (ijoin_temp == 4) {
           cairo_set_line_join(cr3, CAIRO_LINE_JOIN_BEVEL);
         }

         if (ipatt_temp == 1) {               /*  Solid Line */
            double dashes[] = {10.};
            int ndash = 0;
            double offset = 0.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 2) {       /*  Dashed Line */
            double dashes[] = {10., 10.0};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 3) {       /*  Dotted */
            double dashes[] = {2.,6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -2.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 4) {       /*  Dash 1  */
            double dashes[] = {10., 10., 5., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -20.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 5) {       /*  Dash 2  */
            double dashes[] = {10., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 6) {       /*  Dash 3  */
            double dashes[] = {5., 5.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -5.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 7) {       /*  Dash 4 */
            double dashes[] = {10., 10., 2., 6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else if (ipatt_temp == 8) {      /*  Dash 5  */
            double dashes[] = {10., 5., 2., 6.};
            int ndash = sizeof (dashes)/sizeof(dashes[0]);
            double offset = -10.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         } else {                          /* No Match (Solid) */
            double dashes[] = {10};
            int ndash = 0;
            double offset = 0.0;
            cairo_set_dash(cr3, dashes, ndash, offset);
         }
         cairo_set_line_width(cr3,pthick_temp);

         cairo_move_to(cr3,xpts[0],ypts[0]);
         for (i=1; i < npts_temp+1; i++) {
             cairo_line_to(cr3,xpts[i],ypts[i]);
         }
         cairo_stroke(cr3);
      }
   }
}

/* CAMOVE  - move to a point
 *
 * ax1    - contains the x coordinate
 * ay1    - contains the y coordinate
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void camove_(idev, ax1, ay1)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAMOVE_(idev, ax1,a1)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void camove(idev, ax1,ay1)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAMOVE(idev, ax1,ay1)
#endif
#if PRECISION == 0
double   *ax1, *ay1;
#else
double ax1[2], ay1[2];
#endif
#if INTEGER_PRECISION == 0
int   *idev;
#else
int   idev[2];
#endif
{
   double   ax1_temp;
   double   ay1_temp;
   int      idev_temp;

#if PRECISION == 0
   ax1_temp = *ax1;
   ay1_temp = *ay1;
#else
   ax1_temp = ax1[0];
   ay1_temp = ay1[0];
#endif

#if INTEGER_PRECISION == 0
   idev_temp = *idev;
#else
   idev_temp = *idev[0];
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         cairo_move_to(cr1,ax1_temp,ay1_temp);
         cairo_stroke(cr1);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_move_to(cr2,ax1_temp,ay1_temp);
         cairo_stroke(cr2);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_move_to(cr3,ax1_temp,ay1_temp);
         cairo_stroke(cr3);
      }
   }

}

/* CASECO  - set the color using RGB specification
 *
 * idev   - identify whether device 1, 2, or 3
 * jred   - value for red component (0 to 1)
 * jgreen - value for green component (0 to 1)
 * jblue  - value for blue component (0 to 1)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void caseco_(idev, jred,jgreen,jblue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CASECO_(idev, jred,jgreen,jblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void caseco(idev, jred,jgreen,jblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CASECO(idev, jred,jgreen,jblue)
#endif
#if PRECISION == 0
double   *jred;
double   *jgreen;
double   *jblue;
#else
double   jred[2];
double   jgreen[2];
double   jblue[2];
#endif
#if INTEGER_PRECISION == 0
int   *idev;
#else
int   idev[2];
#endif
{
   float    avalue1;
   float    avalue2;
   float    avalue3;
   int      idev_temp;

#if PRECISION == 0
   avalue1 = *jred;
   avalue2 = *jgreen;
   avalue3 = *jblue;
#else
   avalue1 = jred[0];
   avalue2 = jgreen[0];
   avalue3 = jblue[0];
#endif

#if INTEGER_PRECISION == 0
   idev_temp = *idev;
#else
   idev_temp = *idev[0];
#endif

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

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         cairo_set_source_rgb(cr1,avalue1,avalue2,avalue3);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_set_source_rgb(cr2,avalue1,avalue2,avalue3);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_set_source_rgb(cr3,avalue1,avalue2,avalue3);
      }
   }

}

/* CASEPA  - set line attribute (color set in AQSECO):
 *
 * jpatt   - the line pattern
 * pthick  - the line thickness (in points)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void casepa_(idev,xpatt,npatt,pthick,iopt)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CASEPA_(idev,xpatt,npatt,pthick,iopt)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void casepa(idev,xpatt,npatt,pthick,iopt)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CASEPA(idev,xpatt,npatt,pthick,iopt)
#endif
float  xpatt[];
#if PRECISION == 0
float *pthick;
#else
float pthick[2];
#endif
#if INTEGER_PRECISION == 0
int   *npatt;
int   *iopt;
int   *idev;
#else
int   npatt[2];
int   iopt[2];
int   idev[2];
#endif
{
   int     npatt_temp;
   float   pthick_temp;
   int     iopt_temp;
   int     idev_temp;

#if PRECISION == 0
   pthick_temp = *pthick;
#else
   pthick_temp = pthick[0];
#endif
#if INTEGER_PRECISION == 0
   npatt_temp  = *npatt;
   iopt_temp   = *iopt;
   idev_temp   = *idev;
#else
   npatt_temp  = npatt[0];
   iopt_temp   = iopt[0];
   idev_temp   = idev[0];
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
      }
   }

}

/* CASESI  - set character size
 *
 * pheigh   - the desired point size
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void casesi_(idev,pheigh)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CASESI_(idev,pheigh)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void casesi(idev,pheigh)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CASESI(idev,pheigh)
#endif
#if PRECISION == 0
float *pheigh;
#else
float pheigh[2];
#endif
#if INTEGER_PRECISION == 0
int   *idev;
#else
int   idev[2];
#endif
{
   float   pheigh_temp;
   int     idev_temp;

#if PRECISION == 0
   pheigh_temp = *pheigh;
#else
   pheigh_temp = pheigh[0];
#endif
#if INTEGER_PRECISION == 0
   idev_temp = *idev;
#else
   idev_temp = *idev[0];
#endif


   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
      }
   }

}

/* CAPOIN - draw a point.
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 * jcol   - color to use in drawing the point
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void capoin_(idev, ix, iy)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CAPOIN_(idev, ix, iy)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void capoin(idev, ix, iy)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CAPOIN(idev, ix, iy)
#endif
#if INTEGER_PRECISION == 0
int   *ix, *iy;
int   *idev;
#else
int   ix[2], iy[2];
int   idev[2];
#endif
{

   int    ixtemp;
   int    iytemp;
   int    idev_temp;

#if INTEGER_PRECISION == 0
   ixtemp      = *ix;
   iytemp      = *iy;
   idev_temp   = *idev;
#else
   ixtemp = ix[0];
   iytemp = iy[0];
   idev_temp == ired[0];
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         cairo_rectangle(cr1,ixtemp,iytemp,1,1);
         cairo_stroke_preserve(cr1);
         cairo_fill(cr1);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_rectangle(cr2,ixtemp,iytemp,1,1);
         cairo_stroke_preserve(cr2);
         cairo_fill(cr2);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_rectangle(cr3,ixtemp,iytemp,1,1);
         cairo_stroke_preserve(cr3);
         cairo_fill(cr3);
      }
   }

}

/* CARGFL - fill a region.  Rectangular regions will be filled differently
 *          non-rectangular regions.  Dataplot only handles convex polygons,
 *          so set this (for faster performance).  This routine only does
 *          solid fills.  Hatch patterns must be drawn
 *          by the calling program (i.e., send the individual lines to
 *          the CADRAW routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points in the polygon (if 2, assume a rectangle,
 *          otherwise, a convex polygon)
 *
 */
#define MAX_REG_POINTS  1000
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void cargfl_(idev, xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CARGFL_(idev, xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void cargfl(idev, xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CARGFL(idev, xpts, ypts, npts)
#endif
double   xpts[], ypts[];
#if INTEGER_PRECISION == 0
int   *npts;
int   *idev;
#else
int   npts[2];
int   idev[2];
#endif
{
   int     npts_temp;
   int     idev_temp;
   int     indx;

#if INTEGER_PRECISION == 0
   npts_temp = *npts;
   idev_temp = *idev;
   indx = 1;
#else
   npts_temp = npts[0];
   idev_temp = idev[0];
   indx = 2;
#endif

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         if (npts_temp == 2) {      /* rectangle */
            double  x1, y1, x2, y2, width, height, atemp;
            x1 = xpts[0];
            x2 = xpts[indx];
            y1 = ypts[0];
            y2 = ypts[indx];
            if (x1 > x2) {
               atemp = x1;
               x1 = x2;
               x2 = atemp;
            }
            if (y1 > y2) {
               atemp = y1;
               y1 = y2;
               y2 = atemp;
            }
            width=x2 - x1;
            height=y2 - y1;
            cairo_set_line_width(cr1,1);
            cairo_rectangle(cr1,x1,y1,width,height);
            cairo_stroke_preserve(cr1);
            cairo_fill(cr1);
         }
         else if (npts_temp > 2) {     /* convex polygon */
            int  i, temp;
            double ix,iy;
            temp = npts_temp;
            cairo_new_path(cr1);
            ix = xpts[0];
            iy = ypts[0];
            cairo_move_to(cr1,ix,iy);
            for (i = 1; i < temp; i++) {
#if INTEGER_PRECISION == 0
                ix = xpts[i];
                iy = ypts[i];
#else
                ix = xpts[2*i];
                iy = ypts[2*i];
#endif
                cairo_line_to(cr1,ix,iy);
            }
            cairo_close_path(cr1);
            cairo_fill_preserve(cr1);
            cairo_stroke(cr1);
         }
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         if (npts_temp == 2) {      /* rectangle */
            double  x1, y1, x2, y2, width, height, atemp;
            x1 = xpts[0];
            x2 = xpts[indx];
            y1 = ypts[0];
            y2 = ypts[indx];
            if (x1 > x2) {
               atemp = x1;
               x1 = x2;
               x2 = atemp;
            }
            if (y1 > y2) {
               atemp = y1;
               y1 = y2;
               y2 = atemp;
            }
            width=x2 - x1;
            height=y2 - y1;
            cairo_set_line_width(cr2,1);
            cairo_rectangle(cr2,x1,y1,width,height);
            cairo_stroke_preserve(cr2);
            cairo_fill(cr2);
         }
         else if (npts_temp > 2) {     /* convex polygon */
            int  i;
            double ax,ay;
            cairo_new_path(cr2);
            ax = xpts[0];
            ay = ypts[0];
            cairo_move_to(cr2,ax,ay);
            for (i = 1; i < npts_temp; i++) {
#if PRECISION == 0
                ax = xpts[i];
                ay = ypts[i];
#else
                ax = xpts[2*i];
                ay = ypts[2*i];
#endif
                cairo_line_to(cr2,ax,ay);
            }
            cairo_close_path(cr2);
            cairo_fill_preserve(cr2);
            cairo_stroke(cr2);
         }
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         if (npts_temp == 2) {      /* rectangle */
            double  x1, y1, x2, y2, width, height, atemp;
            x1 = xpts[0];
            x2 = xpts[indx];
            y1 = ypts[0];
            y2 = ypts[indx];
            if (x1 > x2) {
               atemp = x1;
               x1 = x2;
               x2 = atemp;
            }
            if (y1 > y2) {
               atemp = y1;
               y1 = y2;
               y2 = atemp;
            }
            width=x2 - x1;
            height=y2 - y1;
            cairo_set_line_width(cr3,1);
            cairo_rectangle(cr3,x1,y1,width,height);
            cairo_stroke_preserve(cr3);
            cairo_fill(cr3);
         }
         else if (npts_temp > 2) {     /* convex polygon */
            int  i, temp;
            double ix,iy;
            temp = npts_temp;
            cairo_new_path(cr3);
            ix = xpts[0];
            iy = ypts[0];
            cairo_move_to(cr3,ix,iy);
            for (i = 1; i < temp; i++) {
#if INTEGER_PRECISION == 0
                ix = xpts[i];
                iy = ypts[i];
#else
                ix = xpts[2*i];
                iy = ypts[2*i];
#endif
                cairo_line_to(cr3,ix,iy);
            }
            cairo_close_path(cr3);
            cairo_fill_preserve(cr3);
            cairo_stroke(cr3);
         }
      }
   }

}

/* CATXTH - draw a horizontal text string.
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
void catxth_(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CATXTH_(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void catxth(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CATXTH(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#endif
int    string[];
int    font[];
#if PRECISION == 0
double    *ixpos;
double    *iypos;
double    *fontsize;
#else
double    ixpos[2];
double    iypos[2];
double    fontsize[2];
#endif
#if INTEGER_PRECISION == 0
int    *idev;
int    *ijusth;
int    *ijustv;
int    *islant;
int    *iweight;
int    *error;
#else
int    idev[2];
int    ijusth[2];
int    ijustv[2];
int    islant[2];
int    iweight[2];
int    error[2];
#endif
{

   float  ixpos_temp;
   float  iypos_temp;
   float  xpos;
   float  ypos;
   float  size_temp;
   int    len;                     /* number of characters in string */
   int    len2;                    /* number of characters in string */
   int    string_width;            /* width of string in pixels */
   int    i;
   int    ijusth_temp;
   int    ijustv_temp;
   int    islant_temp;
   int    iweight_temp;
   int    error_temp;
   int    idev_temp;
   int    ival1;
   int    ival2;
   char   string2[130];            /* converted string */
   char   font2[33];               /* font name */

   cairo_text_extents_t  extents;  /* use for justifying text */

#if PRECISION == 0
   xpos       = *ixpos;
   ypos       = *iypos;
   size_temp  = *fontsize;
#else
   xpos       = ixpos[0];
   ypos       = iypos[0];
   size_temp  = fontsize[0];
#endif

#if INTEGER_PRECISION == 0
   ijusth_temp  = *ijusth;
   ijustv_temp  = *ijustv;
   islant_temp  = *islant;
   iweight_temp = *iweight;
   idev_temp    = *idev;
   i_to_s_5(string, string2, 130, &len);
   i_to_s_5(font, font2, 33, &len2);
#else
   ijusth_temp  = ijusth[0];
   ijustv_temp  = ijustv[0];
   islant_temp  = islant[0];
   iweight_temp = iweight[0];
   idev_temp    = *idev[0];
   i_to_s_5(string, string2, 260, &len);
   i_to_s_5(font, font2, 66, &len2);
#endif

   if (islant_temp == 1) {
      ival1 = CAIRO_FONT_SLANT_NORMAL;
   } else {
      ival1 = CAIRO_FONT_SLANT_ITALIC;
   }

   if (iweight_temp == 1) {
      ival2 = CAIRO_FONT_WEIGHT_NORMAL;
   } else {
      ival2 = CAIRO_FONT_WEIGHT_BOLD;
   }

   /* For center or right horizontallly justified text, adjust xpos */
   if (idev_temp == 1) {
      cairo_text_extents(cr1,string2,&extents);
      if (ijusth_temp == 1) {        /*  center justify horizontally */
         xpos = xpos - (extents.width/2 + extents.x_bearing);
      } else if (ijusth_temp == 2) { /* right justify horizontally */
         xpos = xpos - (extents.width + extents.x_bearing);
      }
   } else if (idev_temp == 2) {
      cairo_text_extents(cr2,string2,&extents);
      if (ijusth_temp == 1) {        /*  center justify horizontally */
         xpos = xpos - (extents.width/2 + extents.x_bearing);
      } else if (ijusth_temp == 2) { /* right justify horizontally */
         xpos = xpos - (extents.width + extents.x_bearing);
      }
   } else if (idev_temp == 3) {
      cairo_text_extents(cr3,string2,&extents);
      if (ijusth_temp == 1) {        /*  center justify horizontally */
         xpos = xpos - (extents.width/2 + extents.x_bearing);
      } else if (ijusth_temp == 2) { /* right justify horizontally */
         xpos = xpos - (extents.width + extents.x_bearing);
      }
   }

   /* For center or top vertically justified text, adjust ypos */
   if (idev_temp == 1) {
      if (ijustv_temp == 1) {        /*  center justify horizontally */
         ypos = ypos + size_temp/2;
      } else if (ijustv_temp == 2) { /* right justify horizontally */
         ypos = ypos + size_temp;
      }
   } else if (idev_temp == 2) {
      if (ijustv_temp == 0) {        /*  center justify vertically */
         ypos = ypos + size_temp/2;
      } else if (ijustv_temp == 2) { /* right justify horizontally */
         ypos = ypos + size_temp;
      }
   } else if (idev_temp == 3) {
      if (ijustv_temp == 0) {        /*  center justify horizontally */
         ypos = ypos + (extents.width/2 + extents.x_bearing);
      } else if (ijustv_temp == 2) { /* right justify horizontally */
         ypos = ypos + size_temp;
      }
   }

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         cairo_select_font_face(cr1,font2,ival1,ival2);
         cairo_set_font_size(cr1,size_temp);
         cairo_move_to(cr1,xpos,ypos);
         cairo_show_text(cr1,string2);
         cairo_surface_flush(surface1);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_select_font_face(cr2,font2,ival1,ival2);
         cairo_set_font_size(cr2,size_temp);
         cairo_move_to(cr2,xpos,ypos);
         cairo_show_text(cr2,string2);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_select_font_face(cr3,font2,ival1,ival2);
         cairo_set_font_size(cr3,size_temp);
         cairo_move_to(cr3,xpos,ypos);
         cairo_show_text(cr3,string2);
      }
   }

}
/* CATXTV - draw a vertical text string.
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
void catxtv_(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CATXTV_(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void catxtv(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CATXTV(idev, string, ixpos, iypos, ijusth, ijustv, fontsize, font, islant, iweight, error)
#endif
int    string[];
int    font[];
#if PRECISION == 0
double    *ixpos;
double    *iypos;
double    *fontsize;
#else
double    ixpos[2];
double    iypos[2];
double    fontsize[2];
#endif
#if INTEGER_PRECISION == 0
int    *idev;
int    *ijusth;
int    *ijustv;
int    *islant;
int    *iweight;
int    *error;
#else
int    idev[2];
int    ijusth[2];
int    ijustv[2];
int    islant[2];
int    iweight[2];
int    error[2];
#endif
{

   float  ixpos_temp;
   float  iypos_temp;
   float  xpos;
   float  ypos;
   float  size_temp;
   float  angle;
   int    len;                     /* number of characters in string */
   int    len2;                    /* number of characters in string */
   int    string_width;            /* width of string in pixels */
   int    i;
   int    ijusth_temp;
   int    ijustv_temp;
   int    islant_temp;
   int    iweight_temp;
   int    error_temp;
   int    idev_temp;
   int    ival1;
   int    ival2;
   char   string2[130];            /* converted string */
   char   font2[130];              /* font name */

   cairo_text_extents_t  extents;  /* use for justifying text */

#if PRECISION == 0
   xpos  = *ixpos;
   ypos  = *iypos;
   size_temp = *fontsize;
#else
   xpos  = ixpos[0];
   ypos  = iypos[0];
   size_temp = fontsize[0];
#endif

#if INTEGER_PRECISION == 0
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
   islant_temp = *islant;
   iweight_temp = *iweight;
   idev_temp = *idev;
   i_to_s_5(string, string2, 130, &len);
   i_to_s_5(font, font2, 33, &len2);
#else
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
   idev_temp = *idev[0];
   i_to_s_5(string, string2, 260, &len);
   i_to_s_5(font, font2, 66, &len2);
#endif

   if (islant_temp == 1) {
      ival1 = CAIRO_FONT_SLANT_NORMAL;
   } else {
      ival1 = CAIRO_FONT_SLANT_ITALIC;
   }

   if (iweight_temp == 1) {
      ival2 = CAIRO_FONT_WEIGHT_NORMAL;
   } else {
      ival2 = CAIRO_FONT_WEIGHT_BOLD;
   }

   size_temp = size_temp;
   angle=1.570796;
   /* For center or right horizontallly justified text, adjust xpos */
   if (idev_temp == 1) {
      if (ijusth_temp == 1) {        /*  center justify horizontally */
         xpos = xpos + size_temp/2;
      } else if (ijusth_temp == 2) { /* right justify horizontally */
         xpos = xpos + size_temp;
      }
   } else if (idev_temp == 2) {
      if (ijusth_temp == 1) {        /*  center justify horizontally */
         xpos = xpos + size_temp/2;
      } else if (ijusth_temp == 2) { /* right justify horizontally */
         xpos = xpos + size_temp;
      }
   } else if (idev_temp == 3) {
      if (ijusth_temp == 1) {        /*  center justify horizontally */
         xpos = xpos + size_temp/2;
      } else if (ijusth_temp == 2) { /* right justify horizontally */
         xpos = xpos + size_temp;
      }
   }

   /* For center or top vertically justified text, adjust ypos */
   if (idev_temp == 1) {
      cairo_text_extents(cr1,string2,&extents);
      if (ijustv_temp == 1) {        /*  center justify vertically */
         ypos = ypos + (extents.width/2 + extents.x_bearing);
      } else if (ijustv_temp == 2) { /* right justify vertically */
         ypos = ypos + (extents.width + extents.x_bearing);
      }
   } else if (idev_temp == 2) {
      cairo_text_extents(cr2,string2,&extents);
      if (ijustv_temp == 0) {        /*  center justify vertically */
         ypos = ypos + (extents.width/2 + extents.x_bearing);
      } else if (ijustv_temp == 2) { /* right justify vertically */
         ypos = ypos + (extents.width + extents.x_bearing);
      }
   } else if (idev_temp == 3) {
      cairo_text_extents(cr3,string2,&extents);
      if (ijustv_temp == 0) {        /*  center justify vertically */
         ypos = ypos + (extents.width/2 + extents.x_bearing);
      } else if (ijustv_temp == 2) { /* right justify vertically */
         ypos = ypos + (extents.width + extents.x_bearing);
      }
   }

   if (idev_temp == 1) {
      if (OPEN_FLAG_1 > 0) {
         cairo_select_font_face(cr1,font2,ival1,ival2);
         cairo_set_font_size(cr1,size_temp);
         cairo_move_to(cr1,xpos,ypos);
         cairo_rotate(cr1,-angle);
         cairo_show_text(cr1,string2);
         cairo_rotate(cr1,angle);
         cairo_surface_flush(surface1);
      }
   } else if (idev_temp == 2) {
      if (OPEN_FLAG_2 > 0) {
         cairo_select_font_face(cr2,font2,ival1,ival2);
         cairo_set_font_size(cr2,size_temp);
         cairo_move_to(cr2,xpos,ypos);
         cairo_rotate(cr2,-angle);
         cairo_show_text(cr2,string2);
         cairo_rotate(cr2,angle);
      }
   } else if (idev_temp == 3) {
      if (OPEN_FLAG_3 > 0) {
         cairo_select_font_face(cr3,font2,ival1,ival2);
         cairo_set_font_size(cr3,size_temp);
         cairo_move_to(cr3,xpos,ypos);
         cairo_rotate(cr3,-angle);
         cairo_show_text(cr3,string2);
         cairo_rotate(cr3,angle);
      }
   }
}

/* CARDLO  - routine to read a position from the graphics window (used
 *           by the Dataplot cross-hair command).  The mouse position at
 *           the next mouse click will be determined.
 *
 *           Currently this is only supported for screen devices and
 *           specifically X11 (may extend to WIN32 and QUARTZ drivers
 *           if these support some type of graphics input).
 *
 *           This routine will wait until a mouse button is pressed.
 *           Note that expose and configure events will still be collected.
 *
 *           Dataplot's cross-hair simply wants a single coordinate position
 *           when any of the mouse buttons is pressed.
 *
 * ixret   - x coordinate of mouse position when button pressed
 * iyret   - y coordinate of mouse position when button pressed
 * error   - error flag (for window destroy event)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void cardlo_(idev, imodel, ixret, iyret, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void CARDLO_(idev, inodel, ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void cardlo(idev, imodel, ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void CARDLO(idev, imodel, ixret, iyret, error)
#endif
#if INTEGER_PRECISION == 0
int  *idev;
int  *imodel;
int  *ixret;
int  *iyret;
int  *error;
#else
int  idev[2];
int  imodel[2];
int  ixret[2];
int  iyret[2];
int  error[2];
#endif
{
#ifdef HAVE_X11
     XEvent   event;            /* holds Xserver events */
#endif
     int      idev_temp;
     int      imodel_temp;
     int      x, y;             /* the last X pointer position */
     int      new_x, new_y;     /* a new X pointer position */
     unsigned int dummy;        /* placeholder for unwanted return value */
     int      done;
     int      expose_flag;
     int      configure_flag;

#ifdef HAVE_X11
     /* Types of graphic input to accept */
     /* Window notification events */
     /* Expose events */
     /* Button press */
     /* Pointer moves with button pressed */
     /* Pointer moves in window */
     XSelectInput(dsp, da,   
       StructureNotifyMask |
       ExposureMask |  
       ButtonPressMask | 
       ButtonMotionMask |
       PointerMotionMask);
#endif

#if INTEGER_PRECISION == 0
       idev_temp   = *idev;
       imodel_temp = *imodel;
       *ixret      = -1;
       *iyret      = -1;
       *error      = 0;
#else
       idev_temp   = idev[0];
       imodel_temp = imodel[0];
       ixret[0] = -1;
       iyret[0] = -1;
       error[0] = 0;
#endif

       if (idev_temp == 1) {
          if (OPEN_FLAG_1 > 0) {
             if (imodel_temp == 1) {
#ifdef HAVE_X11
                done = 0;
                while (done == 0) {      /* loop until button press event */

                XNextEvent(dsp, &event); /* get next event from queue */
                switch (event.type) {
                case DestroyNotify:      /* window has been destroyed */
                   cairo_surface_destroy(surface1);
                   cairo_destroy(cr1);
                   XCloseDisplay(dsp);
                   OPEN_FLAG_2 = 0;
#if INTEGER_PRECISION == 0
                   *error = 1;
#else
                   error[0] = 1;
#endif
                  break;
                case Expose:          /* portion of window has become visible */
                   if (event.xexpose.count == 0) {
                      expose_flag = 1;
                   }
                   break;
                case ConfigureNotify: /* window has been reconfigured */
                   configure_flag = 1;
                   break;
                case ButtonPress:     /* Button event */
#if INTEGER_PRECISION == 0
                   *ixret = event.xbutton.x;
                   *iyret = event.xbutton.y;
#else
                   ixret[0] = event.xbutton.x;
                   iyret[0] = event.xbutton.y;
#endif
                   done = 1;
                   break;
                default:
                   break;
                }       /* end switch */
                }          /* end while */

               XSelectInput(dsp, da,    /* Reset default event selection */
               StructureNotifyMask |          /* Window notification events */
               ExposureMask);                 /* Expose events */

#endif
         }
      }
   }
}


/* i_to_s_5  - utitlity routine to convert an integer array containing
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
void i_to_s_5(string1, string2, maxlen, ilen)
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
#else
     while (string1[2*i] != 0 && i < (maxlen - 1) ) {
         itemp = string1[2*i];
         string2[i] = string1[2*i];
         i++;
     }
     *ilen = i;
     string2[i]='\0';
#endif
}

/* int decodeEvent(char *event, int *x, int *y)
{
   int x, y;
   char **ap, *argv[10];
   
   return;
} */


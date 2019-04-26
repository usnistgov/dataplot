/*  gl.c  - c routines for Dataplot OpenGL driver.
 *
 *  This is a device driver for OpenGL.  It uses the GL and
 *  GLU libraries for graphics.  Window management uses the
 *  auxillary library (this is GLX and XLIB on Unix, and the 
 *  AUX library on the PC).  Note that it does not use the
 *  GLUT library.  This is due to the fact that GLUT requires
 *  an event based GUI type operation.  However, since we
 *  implement the Dataplot GUI in Tcl/Tk, we actually simply
 *  want a graphics window created from a console (or a terminal
 *  window in Unix).  Also, since the f90gl library of Bill Mitchell
 *  supports GLUT for windowing, we have chosen to use this
 *  C interface instead.
 *
 *  A dummy version of this library is maintained for those systems
 *  that do not support OpenGL or that do not allow C routines to be
 *  called from Fortran.  Since the dummy library is coded in
 *  Fortran, routine names will be limited to six characters.
 *
 *  Note that calling C from Fortran is not standard.  Therefore,
 *  these routines may require some tweaking for some operating
 *  systems.  This version was tested on the Sun (using Unix).  The
 *  following is a list of portability issues.
 *
 *  1) The Sun Unix system appends an underscore ("_") to the Fortran
 *     name.  For example, if the Fortran routine calls GLEND, the C
 *     name will be GLEND_.
 *
 *     The global variable APPEND_UNDERSCORE is used to handle this
 *     issue.  If your system appends the underscore, set this
 *     variable to 1.  Otherwise set it to 0.  Conditional
 *     compilation statements will insert the correct function name.
 *
 *     Most Unix Fortran compilers automatically convert routine
 *     names to lower case.  However, the Cray does not.  Add the
 *     global variable SUBROUTINE_CASE to handle this.
 *     For lower case, set this variable to 1.  For upper case, set
 *     this variable to 0.
 *
 *     The above 2 definitions are controlled with the
 *     -DNOUNDERSCORE and -DUPPERCASE compile definitions.
 *     The default is an underscore and lower case.  This allows
 *     the changes to be made without changes in the source code.
 *
 *  2) The primary portability problem will be in how arguments are
 *     passed back and forth between Fortran and C.  Note that
 *     Fortran passes arguments by reference (i.e., it sends the
 *     address) while C passes by value (i.e., a local copy is made
 *     of the variable).  On the Sun (and most Unix systems), real
 *     and integer arguments can be sent via the function arguments.
 *     Be aware that the C subroutine must declare the function
 *     arguments to be pointers since Fortran is sending an address
 *     rather than a value.  The function argument must
 *     be declared as a pointer even if its value is not being
 *     returned to the calling Fortran program.
 *
 *     However, note that arrays are treated as pointers in C, so
 *     array names are not declared as pointers in C.
 *
 *     Character strings should be sent as an array of Ascii Decimal
 *     Equivalents (i.e., "A" should be sent as the integer 65, "0"
 *     should be sent as the integer 48) to correspond to how C
 *     defines character strings.  Note that the string should end
 *     with the null character (integer 0).
 *
 *     If your system passes data differently (e.g., through some
 *     type of common), both the calling Fortran program and the C
 *     code here will have to be modified.
 *
 *  3) Window management varies between platforms.  On the
 *     compile, use one of "-DUNIX", "-DPC", or "-DMAC" to
 *     implement the right window manager code.
 *
 *  The primary references for writing this driver were:
 *
 *     "OpenGL Programming Guide", OpenGL ARB, Addison-Wesley, 1993.
 *
 *     "OpenGL: Programming Guide for the X Window System",
 *     Mark Kilgard, Addison-Wesley, 1996.
 *
 *     "OpenGL Superbible", Richard S. Wright Jr. and Michael
 *     Sweet, Waite Group Press, (1996).
 *
 *  The following routines are included:
 *
 *  glchec     - check for expose and configure events
 *  glrdlo     - read mouse position when button pressed
 *  glinit     - initialize OpenGL
 *  glend      - close OpenGL
 *  glclea     - flush the buffer
 *  gleras     - clear the screen
 *  glupda     - copy pixmap to screen
 *  glseco     - set foreground color
 *  glopde     - start new display list
 *  glclde     - close current display list
 *  glattr     - set line attributes (width, style, cap, join)
 *  gldraw     - draw a polyline
 *  glpoin     - draw a point
 *  glrefl     - solid fill of a region
 *  gltexh     - draw a horizontal character string
 *  gltexv     - draw a vertical character string
 *  gltatt     - set text attributes
 *  glsavg     - copy pixmap to a specified file
 *  glresg     - copy pixmap from a specified file to the screen
 *  glcycl     - cycle through previously saved pixmaps
 *  i_to_s_3   - utility routine to convert array of ADE's to
 *               string array
 *  gl_screen  - utility routine to set screen height and width
 *
 */

/*  Site dependent definitions (see comments above).
 *  Default is an underscore and lower case running under Unix.
 *  The compiler specified definitions -DNOUNDERSCORE, -DUPPERCASE,
 *   and DPC can be specified to override these defaults.
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

#ifdef DOUBLE
#define PRECISION 1
#else
#define PRECISION 0
#endif

#ifdef MAC_OS
#define MAC 0
#else
#define MAC 1
#endif
#ifdef PC_OS
#define PC 0
#else
#define PC 1
#endif
#ifdef UNIX_OS
#define UNIX 0
#else
#define UNIX 1
#endif

/*  include files */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

#if UNIX == 0
#include <X11/Xlib.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>
#endif
#if PC == 0
#include <windows.h>
#include <conio.h>
#include <GL/gl.h>
#include <GL/glu.h>
#endif
#if MAC == 0
#include <GL/gl.h>
#include <GL/glu.h>
#endif

/* global definitions */

#define MAX_COLORS    89
#define MAX_GRAY         100
#define BORDER_WIDTH      3
#define DEFAULT_X_SIZE  550
#define DEFAULT_Y_SIZE  425
#define DEFAULT_X        50
#define DEFAULT_Y        50
#define MIN_X_SIZE      200
#define MIN_Y_SIZE      200

#if UNIX == 0
/* X11 declarations */
Display       *display;    /* display connection */
Window        window;      /* window identifier */
Window        window2;     /* window identifier for repeat graph */
GLXContext    gc;          /* graphics context */
GLXContext    gc2;         /* graphics context for repeat graph */
XFontStruct   *font_struct;   /* returned font structure pointer */
Colormap      color_map;   /* default color map */
XVisualInfo   *vis;        /* pointer to visual structure */
XEvent        event;       /* holds X server events */
XSetWindowAttributes  window_attributes;   /* attributes structure */
XWindowAttributes     returned_attributes; /* returned attributes */
unsigned long value_mask;  /* value mask for window attributes */
static XSizeHints          /* size hints for window manager */
    xsh = {
       (PPosition | PSize | PMinSize),   /* flags */
       DEFAULT_X_SIZE,                   /* height */
       DEFAULT_Y_SIZE,                   /* width */
       MIN_Y_SIZE,                       /* minimum height */
       MIN_X_SIZE,                       /* minimum width */
       DEFAULT_X,                        /* x coordinate */
       DEFAULT_Y,                        /* y coordinate */
    };
static XWMHints            /* more hints for window manager */
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
static XClassHint         /* class hints for window manager */
    xch = {
        "dataplot",                      /* name */
        "Graphics",                      /* class */
    };
static char  *argv[] = {  /* dummy command line arguments */
       (char *)NULL       /* required by a few calls */
       };
static int   argc = 0;
#endif


/* common parameters */
unsigned int  width, height;      /* last known window size */
unsigned int  width2, height2;    /* width, height of 2nd window */
unsigned long black, white;       /* values for black and white */
int           list_id;            /* current display list */
int           screen;             /* default screen */
int           screen2;            /* repeat graph screen */
int           num_cells;          /* number of color cells available */
int           num_planes;         /* number of planes available */
int           depth;              /* depth (number of planes or bits/pixel */
int           color_flag;         /* 0 - monochrome, 1 - color */
int           max_colors;         /* maximum colors actually allocated */
int           configure_flag;     /* 0 - configuration up to date,
                                     1 - configuration has been changed */
int           expose_flag;        /* 0 - expose status has not changed,
                                     1 - expose status has changed */
int           expose_flag_aux;    /* 0 - expose status has not changed for repeat graph,
                                     1 - expose status has changed */
unsigned int root_width, root_height, root_depth, root_bw;
Window       root_id;
int          root_x, root_y;

/* flags for current attribute settings */
static int    OPEN_FLAG = 0;      /* 0 - X11 closed, 1 - X11 open */
static int    OPEN_FLAG_2 = 0;    /* 0 - repeat graph window closed, 1 - open */
int           GRAPH_FLAG = 0;     /* 0 - gleras has not been called, 1 - gleras has been called  */
int           WIDTH_CURRENT;      /* current line width */
int           LINE_STYLE_CURRENT; /* current line style */
char          FONT_NAME_CURRENT[80];  /* name of current font */
char          FONT_NULL[80];       /* null font */
char          FONT_NAME_DEFAULT[80];  /* name of default font */
int           FONT_HEIGHT_CURRENT; /* pixel ascent of current font */
int           FONT_DESCENT_CURRENT;/* descent of current font */
int           FONT_GAP_CURRENT;    /* vertical gap of current font */
int           BACKGROUND_CURRENT;  /* current background color */
int           COLOR_CURRENT;       /* current color */
int           ORIENTATION_CURRENT; /* current orientation */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  glclea_(), glend_(), glchec_();
void  glinit_(), glattr_(), gldraw_();
void  glpoin_(), glrefl_(), glrdlo_();
void  glseco_(), glopde_(), glclde_(),  gleras_(), glupda_();
void  glsavg_(), glresg_(), glcycl_();
void  gltexh_(), gltexv_(),gltatt_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  GLCLEA_(), GLEND_(), GLCHEC_();
void  GLINIT_(), GLATTR_(), GLDRAW_();
void  GLPOIN_(), GLRDLO_();
void  GLSECO_(), GLOPDE_(), GLCLDE_(), GLERAS_(), GLUPDA_();
void  GLSAVG_(), GLRESG_(), GLCYC_();
void  GLTEXH_(), GLTEXV_(), GLTATT_();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  glclea(), glend(), glchec();
void  glinit(), glattr(), gldraw();
void  glpoin(), glrdlo();
void  glseco(), glopde(), glclde(), gleras(), glupda();
void  glsavg(), glresg(), glcycl();
void  gltexh(), gltexv(),gltatt();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  GLCLEA(), GLEND(), GLCHEC();
void  GLINIT(), GLATTR(), GLDRAW();
void  GLPOIN(), GLREFL(), GLRDLO();
void  GLSECO(), GLOPDE(), GLCLDE(), GLERAS(), GLUPDA();
void  GLSAVG(), GLRESG(), GLCYC();
void  GLTEXH(), GLTEXV(), GLTATT();
#endif
void  i_to_s_3(), gl_screen();

/* GLCHEC  - routine to check for X expose and configuration events.
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
void glchec_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLCHEC_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glchec(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLCHEC(expose_flag_2, error_flag)
#endif
#if PRECISION == 0
int  *error_flag;
int  *expose_flag_2;
#else
int  error_flag[2];
int  expose_flag_2[2];
#endif
{
#if UNIX == 0
     XEvent   event;            /* holds Xserver events */
#endif

#if PRECISION == 0
     *error_flag = 0;
     *expose_flag_2 = 0;
#else
     error_flag[0] = 0;
     expose_flag_2[0] = 0;
#endif

/* Unix (i.e., X11 case) */
#if UNIX == 0
     /* Check Queue, get next event */
     while (XEventsQueued(display, QueuedAfterReading) != 0) {
          XNextEvent(display, &event);
          switch (event.type) {
          case DestroyNotify:   /* window has been destroyed */
              if (event.xdestroywindow.window == window2) {
                glXDestroyContext(display, gc2);
                XDestroyWindow(display,window2);
                OPEN_FLAG_2 = 0;
              }
              else {
                glXDestroyContext(display, gc);
                XDestroyWindow(display,window);
                XCloseDisplay(display);
                OPEN_FLAG = 0;
                OPEN_FLAG_2 = 0;
              }
#if PRECISION == 0
              *error_flag = 1;
#else
              error_flag[0] = 1;
#endif
              break;
          case Expose:    /* portion of window has become visible */
              if (event.xexpose.window == window2) {
                if(event.xexpose.count == 0) {
                  expose_flag_aux = 1;
                }
              }
              else {
                if(event.xexpose.count == 0) {
                  expose_flag = 1;
                }
              }
#if PRECISION == 0
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

/* PC */
#if PC == 0

#endif

/* MAC */
#if MAC == 0

#endif

}

/* GLRDLO  - routine to read a position from the graphics window (used
 *           by the Dataplot cross-hair command).  The mouse position at
 *           the next mouse click will be determined.
 *
 *           This routine will wait until a mouse button is pressed.
 *
 *           Note that expose and configure events will still be collected.
 *           Also, the pointer motion events are included for future
 *           development.  Currently, the code is commented out
 *           (although it should valid).  It will be activated if Dataplot
 *           develops a need for it later.  Dataplot's cross-hair simply
 *           wants a single coordinate position when any of the mouse buttons
 *           is pressed.
 *
 * ixret   - x coordinate of mouse position when button pressed
 * iyret   - y coordinate of mouse position when button pressed
 * error   - error flag (for window destroy event)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glrdlo_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLRDLO_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glrdlo(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLRDLO(ixret, iyret, error)
#endif
#if PRECISION == 0
int  *ixret, *iyret, *error;
#else
int  ixret[2], iyret[2], error[2];
#endif
{
#if UNIX == 0
     XEvent   event;            /* holds Xserver events */
     int      x, y;             /* the last X pointer position */
     int      new_x, new_y;     /* a new X pointer position */
     unsigned int dummy;        /* placeholder for unwanted return value */
     int      done;

     XSelectInput(display, window,    /* Types of graphic input to accept */
       StructureNotifyMask |          /* Window notification events */
       ExposureMask |                 /* Expose events */
       ButtonPressMask |              /* Button press */
       ButtonMotionMask |             /* Pointer moves with button pressed */
       PointerMotionMask);            /* Pointer moves in window */

#if PRECISION == 0
       *ixret = -1;
       *iyret = -1;
       *error = 0;
#else
       ixret[0] = -1;
       iyret[0] = -1;
       error[0] = 0;
#endif
       done = 0;
       while (done == 0) {               /* loop until button press event */

          XNextEvent(display, &event);   /* get next event from queue */
          switch (event.type) {
          case DestroyNotify:            /* window has been destroyed */
              glXDestroyContext(display, gc);
              XDestroyWindow(display,window);
              XCloseDisplay(display);
#if PRECISION == 0
              *error = 1;
#else
              error[0] = 1;
#endif
              done = 1;
              break;
          case Expose:                   /* expose event */
              if(event.xexpose.count == 0) {
                expose_flag = 1;
              }
              break;
          case ConfigureNotify:          /* window has been reconfigured */
              configure_flag = 1;
              break;
          case MotionNotify:             /* pointer motion */
          /*  XQueryPointer(display, window,
                            &dummy, &dummy,
                            &dummy, &dummy,
                            &new_x, &new_y,
                            &mask);
              x = new_x;
              y = new_y;    */
              break;
          case ButtonPress:     /* Button event */
#if PRECISION == 0
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

       XSelectInput(display, window,    /* Reset default event selection */
         StructureNotifyMask |          /* Window notification events */
         ExposureMask);                 /* Expose events */
#endif

/* PC Case */
#if PC == 0

#endif

/* MAC Case */
#if MAC == 0

#endif

}

/* GLINIT  - routine to initialize X11.
 *
 * xp          - suggested width of graphics window in pixels (0 for default)
 * yp          - suggested height of graphics window in pixels (0 for default)
 * or          - 0 for landscape, 1 for portrait, 2 for use pixel sizes
 *               3 for square
 * ixret       - acutal width returned by window manager (as opposed to
 *               suggested width sent to window manager)
 * iyret       - actual height returned by window manager (as opposed to
 *               suggested height sent to window manager)
 * display_name- display name (NULL means use the default)
 * error_flag  - following error codes:
 *               0 - normal
 *               1 - unable to open display connection
 *               2 - X11 already open
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glinit_(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag) 
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLINIT_(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glinit(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLINIT(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag)
#endif
#if PRECISION == 0
int    *xp, *yp, *or;
int    *error_flag, *ixret, *iyret;
int    display_name[];
int    iwind[], iwindn[2];
#else
int    xp[2], yp[2], or[2];
int    error_flag[2], ixret[2], iyret[2];
int    display_name[];
int    iwind[], iwindn[2];
#endif
{
     char *window_name = "Dataplot";
     char *icon_name = "Dataplot";
     int          xpixels, ypixels, orien, iwindn_temp;
     int          temp, i;
     int          len, itest, counter;
     int          window_flag;
     int          dummy,ierror, iplace;
     long         itemp;
     float        atemp;
     double       atemp1;
     Status       result;
     char         display_string[80];

#if UNIX == 0
     XTextProperty  WindowName, IconName;
     static int snglBuf[]={GLX_RGBA, GLX_RED_SIZE, 1, 
                          GLX_GREEN_SIZE, 1, GLX_BLUE_SIZE, 1,
                          GLX_DEPTH_SIZE, 1, GLX_DOUBLEBUFFER, None};
#endif

#if PC == 0

#endif

#if MAC == 0

#endif

#if PRECISION == 0
     *error_flag = 0;
     xpixels = *xp;
     ypixels = *yp;
     orien = *or;
     iwindn_temp = *iwindn;
#else
     error_flag[0] = 0;
     xpixels = xp[0];
     ypixels = yp[0];
     orien = or[0];
     iwindn_temp = iwindn[0];
#endif
     strcpy(FONT_NULL,"ZZZZ");
     strcpy(FONT_NAME_DEFAULT,"8X13");
     if(OPEN_FLAG != 0) {             /* X11 already open */
       ierror = 2;
       goto endX11;
     }

/* Unix (GLX) case */
#if UNIX == 0
     strcpy(display_string," ");
#if PRECISION == 0
     i_to_s_3(display_name, display_string, 80, &len);
#else
     i_to_s_3(display_name, display_string, 160, &len);
#endif
     itest = strncmp(display_string, "DEFAULT", 7);
     if (itest == 0) {    /* Open default display connection */
       if((display = XOpenDisplay(NULL)) == NULL) {
         ierror = 1;
         OPEN_FLAG = 0;
         goto endX11;
        }
     }
     else {           /* user specified display name */
       if((display = XOpenDisplay(display_string)) == NULL) {
         ierror = 1;
         OPEN_FLAG = 0;
         goto endX11;
        }
     }

     if (!glXQueryExtension(display, &dummy, &dummy)) {
         ierror = 3;
         OPEN_FLAG = 0;
         goto endX11;
     }
     OPEN_FLAG = 1;
     screen = DefaultScreen(display);        /* Set default screen */
     white = WhitePixel(display,screen);     /* Set white */
     black = BlackPixel(display,screen);     /* Set black */

     XGetGeometry(              /* Get root window geometry */
         display, DefaultRootWindow(display),
         &root_id,                  /* root id */
         &root_x, &root_y,          /* root position */
         &root_width, &root_height, /* root width and height */
         &root_bw, &root_depth);    /* root border width and depth */

     vis = glXChooseVisual(display, screen, snglBuf);
     if (vis ==  NULL) {
         ierror = 4;
         OPEN_FLAG = 0;
         goto endX11;
     }
     gc = glXCreateContext(display, vis,None, True);
     if (gc ==  NULL) {
         ierror = 5;
         OPEN_FLAG = 0;
         goto endX11;
     }
     color_map = XDefaultColormap(display, screen);

     /* If window id passed from front-end, define window variable
      * from string in iwind and skip some code below.
      */

     gl_screen(xpixels, ypixels, orien);
     ORIENTATION_CURRENT = orien;

     window_flag = 0;
     if ( iwindn_temp > 0 ) {
       window_flag = 1;
#if PRECISION == 0
       itemp = 0;
       for ( counter = 0; counter < iwindn_temp; counter++ ) {
          atemp1 = pow(16., (double) counter);
          itemp = itemp + (long) iwind[counter]* (long) atemp1;
       }
#else
       iplace = 0;
       itemp = 0;
       for ( counter = 0; counter < iwindn_temp; counter++ ) {
          atemp1 = pow(16., (double) counter);
          itemp = itemp + (long) iwind[iplace]* (long) atemp1;
          iplace = iplace + 2;
       }
#endif
       window = (Window) itemp;
       goto frontend;
     }

     window = XCreateSimpleWindow(      /* Open graphics window */
                display,                /* Pointer to Display structure */
                DefaultRootWindow(display), /* New window Parent */
                xsh.x,                  /* X position of window */
                xsh.y,                  /* Y position of window */
                xsh.width,              /* Width of window */
                xsh.height,             /* Height of window */
                BORDER_WIDTH,           /* Pixel width of border */
                black,                  /* Border color */
                white);                 /* Background color */

    /* Come here if front-end opened the graphics window.  */
    frontend:

    if ( window_flag == 0) {     /*  Set Bit Gravity */
       window_attributes.bit_gravity = NorthWestGravity;
       value_mask = CWBitGravity;
       XChangeWindowAttributes(display, window, value_mask,
                               &window_attributes);
       if (XStringListToTextProperty(&window_name, 1,
           &WindowName) == 0) {
          ierror = 6;
          OPEN_FLAG = 0;
          goto endX11;
       }
       if (XStringListToTextProperty(&icon_name, 1, &IconName) == 0) {
          ierror = 7;
          OPEN_FLAG = 0;
          goto endX11;
       }
       XSetWMProperties(display, window, &WindowName, &IconName, 
                        argv, argc, &xsh, &xwmh, &xch);
       XSelectInput(display, window,        /* Graphics input */
                    StructureNotifyMask |   /* Notification events */
                    ExposureMask );         /* Expose events */
       glXMakeCurrent(display, window, gc); /* Bind Context */
       XMapWindow(display, window);         /* Map the window */
       XFlush(display);                     /* Flush the buffer */

    }

    /* Get actual height and width set by the window manager */
    XGetWindowAttributes(display, window, &returned_attributes);
    width = returned_attributes.width;
    height = returned_attributes.height;
#if PRECISION == 0
    *ixret = width;
    *iyret = height;
#else
    ixret[0] = width;
    iyret[0] = height;
#endif

    if (window_flag == 0 ) {
      do {
          XNextEvent(display, &event);
      } while (event.type != MapNotify || event.xmap.window != window);
    }

    strcpy(FONT_NAME_CURRENT,FONT_NULL);
    WIDTH_CURRENT = -1;
    LINE_STYLE_CURRENT = -1;
    BACKGROUND_CURRENT = 1;

    /* end of X11 initialization */
    endX11:
#if PRECISION == 0
    *error_flag = ierror;
#else
    error_flag[0] = ierror;
#endif
    return;
    
#endif

#if PC == 0
    auxInitDisplayMode(AUX_SINGLE | AUX_RGBA);
    auxInitPosition(50, 50, xpixels, ypixels);
    auxInitWindow("Dataplot Graphics - OpenGL");
#endif

#if MAC == 2

#endif

}

/* GLERAS  - routine to clear the screen.  Check to see if the window
 *           configuration has been changed by the window manager (or
 *           by the user).  If so, reset the screen size.  Resizes
 *           done by the window manager will take precedence over resizes
 *           requested by the calling program (assume user resized window
 *           via the window manager).
 *
 *           Clear the pixmap as well as the graphics window.
 *
 *           Note that this routine assumes that XFLUSH has already
 *           been called to flush the buffer and that GLCHEC has been
 *           called to check for expose and configure events.
 *
 *  xpixels   - width (in pixels) for graphics window
 *  ypixels   - height (in pixels) for graphics window
 *  orien     - window orientation (i.e., landscape, portrait, or none)
 *  ixret     - width returned to calling program
 *  iyret     - height returned to calling program
 *  red       - red component of background color
 *  green     - green component of background color
 *  blue      - blue component of background color
 *  pix_flag  - 0 = no pixmap generated, 1 = pixmap generated
 *
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gleras_(xpixels, ypixels, orien, ixret, iyret, red, green, blue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLERAS_(xpixels, ypixels, orien, ixret, iyret, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gleras(xpixels, ypixels, orien, ixret, iyret, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLERAS(xpixels, ypixels, orien, ixret, iyret, red, green, blue)
#endif
#if PRECISION == 0
int  *xpixels, *ypixels, *orien;
int  *ixret, *iyret;
float *red, *green, *blue;
#else
int  xpixels[2], ypixels[2], orien[2];
int  ixret[2], iyret[2];
float red[2], green[2], blue[2];
#endif
{

   int   temp_color, temp_color_2;
   int   xpixels_temp, ypixels_temp, orien_temp;
   float red_temp, green_temp, blue_temp;

#if PRECISION == 0
   xpixels_temp = *xpixels;
   ypixels_temp = *ypixels;
   orien_temp = *orien;
   red_temp = *red;
   green_temp = *green;
   blue_temp = *blue;
#else
   xpixels_temp = xpixels[0];
   ypixels_temp = ypixels[0];
   orien_temp = orien[0];
   red_temp = red[0];
   green_temp = green[0];
   blue_temp = blue[0];
#endif

/* UNIX (GLX) Case */
#if UNIX == 0
   if (configure_flag == 1) {    /* graphics window re-configured by user */
      /* Get actual height and width set by the window manager */
      XGetWindowAttributes(display, window, &returned_attributes);
      width = returned_attributes.width;
      height = returned_attributes.height;
#if PRECISION == 0
      *ixret = width;
      *iyret = height;
#else
      ixret[0] = width;
      iyret[0] = height;
#endif


   }
   else if (    /* calling program changing width or height of window */
            ((xpixels_temp > 0) && (ypixels_temp > 0) &&
            (xpixels_temp != width || ypixels_temp != height)) ||
            (orien_temp != ORIENTATION_CURRENT)) {
      gl_screen(xpixels_temp, ypixels_temp, orien_temp);
      ORIENTATION_CURRENT = orien_temp;
      XUnmapWindow(display,window);
      XResizeWindow(display, window, xsh.width, xsh.height);

      XMapWindow(display, window);
      do {     /* make sure correct window is in place */
          XNextEvent(display, &event);
      } while (event.type != MapNotify || event.xmap.window != window);
      expose_flag = 0;
      configure_flag = 0;

      /* Get actual height and width set by the window manager */
      XGetWindowAttributes(display, window, &returned_attributes);
      width = returned_attributes.width;
      height = returned_attributes.height;
#if PRECISION == 0
      *ixret = width;
      *iyret = height;
#else
      ixret[0] = width;
      iyret[0] = height;
#endif

   }
   else {
#if PRECISION == 0
     *ixret = width;
     *iyret = height;
#else
     ixret[0] = width;
     iyret[0] = height;
#endif
   }
   configure_flag = 0;
#endif

   glEndList();
   glDeleteLists(1, list_id);

   list_id = 1;
   glNewList(list_id, GL_COMPILE_AND_EXECUTE);

   glClearColor(red_temp,green_temp,blue_temp,1.0);
   glclear(GL_COLOR_BUFFER_BIT);

   GRAPH_FLAG = 1;
}

/* GLUPDA - routine to redraw current screen
 *
 * If an expose event has been detected, update the graphics
 * window with the current display list.
 *
 *  If frontend active, automatically update graphics
 *  window regardless of expose_flag.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glupda_(frontend)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLUPDA_(frontend)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glupda(frontend)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLUPDA(frontend)
#endif
#if PRECISION == 0
int     *frontend;
#else
int     frontend[2];
#endif
{
   int i;
     
#if PRECISION == 0
   if (*frontend == 0) {
#else
   if (frontend[0] == 0) {
#endif
     if (expose_flag == 1) { /* update screen  if expose event */
         for (i=0; i < list_id; i++) {
            glCallList(i);
         }
         glFlush(); 
         expose_flag = 0;
     }
   }

#if PRECISION == 0
   if (*frontend == 1) {
#else
   if (frontend[0] == 1) {
#endif
     if (GRAPH_FLAG == 1) {
         for (i=0; i < list_id; i++) {
            glCallList(i);
         }
         glFlush(); 
         expose_flag = 0;
     }
   }

/* update second screen  if expose event */
#if PRECISION == 0
   if (*frontend == 0) {
#else
   if (frontend[0] == 0) {
#endif
     if (expose_flag_aux == 1 ) {
         for (i=0; i < list_id; i++) {
            glCallList(i);
         }
         glFlush(); 
         expose_flag_aux = 0;
     }
   }

}

/* GLCLEA  - routine to flush the buffer
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glclea_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLCLEA_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glclea()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLCLEA()
#endif
{
     glFlush();

}

/* GLOPDE  - start a new display list.  Currently, the
 *           display list is used to regenerate the current
 *           plot when expose events are detected.  Note
 *           that a new display list is created for each
 *           "GROPDE/GRCLDE" pair within Dataplot and 
 *           a GRERSC clears the current display list
 *           (this logic accomodates the fact that various
 *           diagrammatic graphics can be used to augment
 *           the current plot.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glopde_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLOPDE_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glopde()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLOPDE()
#endif
{
   list_id = list_id + 1;
   glNewList(list_id, GL_COMPILE_AND_EXECUTE);

}

/* GLCLDE  - close the current display list.  Currently, the
 *           display list is used to regenerate the current
 *           plot when expose events are detected.  Note
 *           that a new display list is created for each
 *           "GROPDE/GRCLDE" pair within Dataplot and 
 *           a GRERSC clears the current display list
 *           (this logic accomodates the fact that various
 *           diagrammatic graphics can be used to augment
 *           the current plot.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glclde_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLCLDE_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glclde()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLCLDE()
#endif
{
   glEndList();

}

/* glsavg - routine to copy pixmap to a specified file
 *
 * Save the current pixmap to the specified file.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glsavg_(file_name, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void glsavg_(file_name, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glsavg(file_name, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void glsavg(file_name, error_flag)
#endif
#if PRECISION == 0
int     *error_flag;
int     file_name[];
#else
int     *error_flag[2];
int     file_name[];
#endif
{
#if PRECISION == 0
#define MAX_STRING_LEN  128
#else
#define MAX_STRING_LEN  256
#endif
   int      len, istatus, ierror;
   char     file_name_string[MAX_STRING_LEN];

   if ( OPEN_FLAG == 0 ){
     ierror = 3;
     goto end;
   }

   strcpy(file_name_string," ");
   i_to_s_3(file_name, file_name_string, MAX_STRING_LEN, &len);

   /* Unix (GLX) Case, this needs to be modified for GL */
#if UNIX == 0
/*
   istatus = XWriteBitmapFile(display, file_name_string, pixmap,
                              width, height, -1, -1);
   XFlush(display);
   if (istatus != BitmapSuccess) {
      ierror = 2;
      goto end;
   }
 */
#endif

   /* Set error flag and return */
   end:
#if PRECISION == 0
     *error_flag = ierror;
#else
     error_flag[0] = ierror;
#endif
     return;

}

/* GLRESG - routine to copy pixmap from a specified file
 *          The pixmap will be restored to a special window
 *
 * Save the current pixmap to the specified file.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glresg_(file_name, title, iwind, iwindn, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLRESG_(file_name, title, iwind, iwindn, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glresg(file_name, title, iwind, iwindn, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLRESG(file_name, title, iwind, iwindn, error_flag)
#endif
#if PRECISION == 0
int     *error_flag;
int     file_name[];
int     iwind[], iwindn[2];
#else
int     *error_flag[2];
int     file_name[];
int     iwind[], iwindn[2];
int     title[];
#endif

{

#if PRECISION == 0
#define MAX_STRING_LEN  128
#else
#define MAX_STRING_LEN  256
#endif

#if UNIX == 0
   XTextProperty  WindowName, IconName;
   XWindowAttributes     window_attributes_tmp;
   static   XSizeHints  xsh2;
#endif
   int      len, istatus;
   int      x_hot, y_hot, iwindn_temp;
   unsigned int  width3, height3;
   char     file_name_string[MAX_STRING_LEN];
   char     title_string[MAX_STRING_LEN+10];
   char     *window_name = "1- Pixmap";
   long     itemp;
   int      counter;
   int      iplace;
   int      window_flag;
   int      ierror;
   double   atemp1;
   FILE     *ijunk;

#if PRECISION == 0
  iwindn_temp = *iwindn;
#else
  iwindn_temp = iwindn[0];
#endif
   if ( OPEN_FLAG == 0 ){
#if PRECISION == 0
     *error_flag = 3;
#else
     error_flag[0] = 3;
#endif
     return;
   }

   strcpy(file_name_string," ");
   i_to_s_3(file_name, file_name_string, MAX_STRING_LEN, &len);
   strcpy(title_string," ");
   i_to_s_3(title, title_string, MAX_STRING_LEN+10, &len);

/* Unix (GLX) case, needs to be changed for OpenGL */
#if UNIX == 0

/*
   istatus = XReadBitmapFile(display, window, file_name_string,
             &width2, &height2, &pixmap3, &x_hot, &y_hot);
   XFlush(display);
   if (istatus != BitmapSuccess) {
      ierror = 2;
      goto end;
   }

   pixmap2 = XCreatePixmap(display, window, width, height, depth);
   PIXMAP_CURRENT_2 = 1;
   if ( OPEN_FLAG_2 == 0 ){
      OPEN_FLAG_2 = 1;

      window_flag = 0;
      if ( iwindn_temp > 0 ) {
        window_flag = 1;
#if PRECISION == 0
        itemp = 0;
        for ( counter = 0; counter < iwindn_temp; counter++ ) {
           atemp1 = pow(16., (double) counter);
           itemp = itemp + (long) iwind[counter]* (long) atemp1;
        }
#else
        iplace = 0;
        itemp = 0;
        for ( counter = 0; counter < iwindn_temp; counter++ ) {
           atemp1 = pow(16., (double) counter);
           itemp = itemp + (long) iwind[iplace]* (long) atemp1;
           iplace = iplace + 2;
        }
#endif
        window2 = (Window) itemp;

        xsh2 =xsh;
        XGetWindowAttributes(display, window2, &returned_attributes);
        width3 = returned_attributes.width;
        height3 = returned_attributes.height;
        xsh2.x = returned_attributes.x;
        xsh2.y = returned_attributes.y;
        xsh2.width = width3;
        xsh2.height = height3;
        if (width3 < width2 || height3 < height2) {
           XUnmapWindow(display,window2);
           XResizeWindow(display, window2, width2, height2);
           XMapWindow(display, window2);
           XFlush(display);
           expose_flag_aux = 0;
           xsh2.width = width2;
           xsh2.height = height2;
        }
        XSetStandardProperties(display, window2, title_string, title_string,
                              None, argv, argc, &xsh2);
        XSelectInput(display, window2,
           StructureNotifyMask | 
           ExposureMask );     
      }  

      else {

        xsh2 = xsh;
        xsh2.x = DEFAULT_X - 100;
        xsh2.y = DEFAULT_Y - 50;
        xsh2.width = width2;
        xsh2.height = height2;

        window2 = XCreateSimpleWindow( 
                display,
                DefaultRootWindow(display),
                xsh2.x,
                xsh2.y,
                xsh2.width,
                xsh2.height,
                BORDER_WIDTH,
                black,
                white);

        value_mask = CWBitGravity;
        XChangeWindowAttributes(display, window2, value_mask,
                 &window_attributes);

       if (XStringListToTextProperty(&window_name, 1, &WindowName)
           == 0) {
           ierror = 1;
           goto end;
       }
       XSetWMProperties(display, window2, &WindowName, &WindowName, 
                        argv, argc, &xsh2, &xwmh, &xch);
       XSelectInput(display, window2,
           StructureNotifyMask | 
           ExposureMask );     
       XMapWindow(display, window2);
       XFlush(display);
       do {
            XNextEvent(display, &event);
          } while (event.type != MapNotify || event.xmap.window != window2);
     }

   }
   else {
       xsh2 =xsh;
       XGetWindowAttributes(display, window2, &returned_attributes);
       xsh2.x = returned_attributes.x;
       xsh2.y = returned_attributes.y;
       width3 = returned_attributes.width;
       height3 = returned_attributes.height;
       xsh2.width = width3;
       xsh2.height = height3;
       if (width3 < width2 || height3 < height2) {
          XUnmapWindow(display,window2);
          XResizeWindow(display, window2, width2, height2);
          XMapWindow(display, window2);
          XFlush(display);
          expose_flag_aux = 0;
          xsh2.width = width2;
          xsh2.height = height2;
       }
       XSetStandardProperties(display, window2, title_string,
                              title_string, None, argv, argc, &xsh2);
   }

   gc2 = XCreateGC(display, window2, 0, &gcvalues);
   XSetState(display, gc2, black, white, GXcopy, AllPlanes);
   XSetForeground(display,gc2,BlackPixel(display,DefaultScreen(display)));
   XSetBackground(display,gc2,WhitePixel(display,DefaultScreen(display)));
   XSetGraphicsExposures(display,gc2,False);
   XCopyPlane(display,pixmap3,pixmap2,gc2,0,0,width2,height2,0,0,1);
   XFreePixmap(display,pixmap3);
   XCopyArea(display, pixmap2, window2, gc2, 0, 0, width2, height2, 0, 0);
   XFlush(display);
   expose_flag_aux = 0;
 */
#endif

   /* Set error flag and return */
   end:
#if PRECISION == 0
     *error_flag = ierror;
#else
     error_flag[0] = ierror;
#endif
     return;
}

/* GLCYC - routine to cycle through previously created pixmaps
 *          The button pressed is stored in ibutton
 *          (1=left, 2=middle, 3=right)
 *          This routine only processes a SINGLE button event.  The
 *          calling routine reads ibutton and either redraws another
 *          pixmap or exits.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glcycl_(error_flag, ibutton)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLCYC_(error_flag, ibutton)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glcycl(error_flag, ibutton)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLCYC(error_flag, ibuttton)
#endif
#if PRECISION == 0
int     *error_flag;
int     *ibutton;
#else
int     *error_flag[2];
int     *ibutton[2];
#endif
{
#if UNIX == 0
     XEvent   event;        /* holds Xserver events */
#endif
     int      x, y;         /* the last X pointer position */
     int      new_x, new_y; /* a new X pointer position */
     unsigned int dummy;    /* placeholder for unwanted return value */
     int      done, ierror;

     if ( OPEN_FLAG == 0 ){
        ierror = 3;
        goto end;
     }

/* Unix (GLX) Case, needs to be modified for OpenGl */
#if UNIX == 0
/* 
     if ( PIXMAP_CURRENT == 0 ){
        ierror = 2;
        goto end;
     }

     XSelectInput(display, window2,
                  StructureNotifyMask |
                  ExposureMask |
                  ButtonPressMask |
                  ButtonMotionMask |
                  PointerMotionMask);

     done = 0;
     while (done == 0) {
          XNextEvent(display, &event);
          switch (event.type) {
          case DestroyNotify:
              glXDestroyContext(display, gc2);
              XDestroyWindow(display,window2);
              ierror = 1;
              done = 1;
              goto end;
              break;
          case Expose:
              if(event.xexpose.count == 0) {
                expose_flag_aux = 1;
              }
              break;
          case ConfigureNotify:
              break;
          case MotionNotify:
              break;
          case ButtonPress: 
#if PRECISION == 0
              *ibutton = event.xbutton.button;
#else
              ibutton[0] = event.xbutton.button;
#endif
              done = 1;
              break;
          default:
              break;
        }
     }

     XSelectInput(display, window2,
                  StructureNotifyMask |
                  ExposureMask);
 */
#endif

   /* Set error flag and return */
   end:
#if PRECISION == 0
     *error_flag = ierror;
#else
     error_flag[0] = ierror;
#endif
     return;
}

/* GLEND   - routine to end OpenGL.  Close the display.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glend_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLEND_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glend()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLEND()
#endif

{
/* Unix (GLX) Case */
#if UNIX == 0
     if (OPEN_FLAG_2 != 0) {
        glXDestroyContext(display,gc2);
        XDestroyWindow(display,window2);
     }
     OPEN_FLAG_2 = 0;

     if (OPEN_FLAG != 0) {
        glXDestroyContext(display,gc);
        XDestroyWindow(display,window);
        XCloseDisplay(display);
     }
     OPEN_FLAG = 0;
#endif

}

/* GLSECO  - routine to set the foreground color.
 *           Note that the RGB values are set in Dataplot
 *           and simply passed here (i.e., send RGB
 *           components as values between 0.0 and 1.0).
 *           Pass index to test against current color.
 *           Color only set if being changed.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glseco(jcol, red, green, blue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLSECO_(jcol, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glseco(jcol, red, green, blue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLSECO(jcol, red, green, blue)
#endif
#if PRECISION == 0
int   *jcol;
float *red, *green, *blue;
#else
int   jcol[2];
int   red[2], green[2], blue[2];
#endif
{
      int      jcol_temp;
      float    red_temp, green_temp, blue_temp;

#if PRECISION == 0
      jcol_temp = *jcol;
      red_temp = *red;
      green_temp = *green;
      blue_temp = *blue;
#else
      jcol_temp = jcol[0];
      red_temp = red[0];
      green_temp = green[0];
      blue_temp = blue[0];
#endif

      if (jcol_temp != COLOR_CURRENT) {
         COLOR_CURRENT = jcol_temp;
         glColor3f(red_temp, green_temp, blue_temp);
      }

}


/* GLATTR  - set line attributes.
 *
 * icode - identify which attribute to set
 *         1 - set the line width in pixels (in range 1 to 15)
 *         2 - set the line style
 * istyle - parameter that sets the line style
 *             0 - solid line
 *             1 - dash line
 *             2 - dotted line
 *             3 - dash2
 *             4 - dash3
 *             5 - dash4
 *             6 - dash5
 * width  - parameter that sets the line width (in pixels)
 *
 */
#define MAX_WIDTH  15
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glattr_(icode, istyle, width)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLATTR_(icode, istyle, width)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glattr(icode, istyle, width)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLATTR(icode, istyle, wdith)
#endif
#if PRECISION == 0
int    *istyle, *icode;
float  *width;
#else
int    istyle[2], icode[2];
float  width[2];
#endif
{
int            width_temp;
int            icode_temp, istyle_temp;

#if PRECISION == 0
   icode_temp = *icode;
   istyle_temp = *istyle;
   width_temp = *width;
#else
   icode_temp = icode[0];
   istyle_temp = istyle[0];
#endif
   switch (icode_temp) {
      case 1:         /* set the line width */
          if(width_temp < 1.0) width_temp = 1.0;
          if(width_temp > 15.0) width_temp = 15.0;
          glLineWidth(width_temp);
          break;
      case 2:         /* set the line style */
          switch (istyle_temp) {  /* index determines the style */
             case 0:         /* solid line */
                 glDisable(GL_LINE_STIPPLE);
                 break;
             case 1:         /* dashed line */
                 glLineStipple(1,0x00FF);
                 glEnable(GL_LINE_STIPPLE);
                 break;
             case 2:         /* dotted line */
                 glLineStipple(1,0x0101);
                 glEnable(GL_LINE_STIPPLE);
                 break;
             case 3:         /* dash-dot line */
                 glLineStipple(1,0x1C47);
                 glEnable(GL_LINE_STIPPLE);
                 break;
             case 4:         /* dash3 */
                 glLineStipple(1,0xAAAA);
                 glEnable(GL_LINE_STIPPLE);
                 break;
             case 5:         /* dash4 line */
                 glLineStipple(2,0x00FF);
                 glEnable(GL_LINE_STIPPLE);
                 break;
             case 6:         /* dash5 line */
                 glLineStipple(2,0xAAAA);
                 glEnable(GL_LINE_STIPPLE);
                 break;
              default:
                 glDisable(GL_LINE_STIPPLE);
                 break;
          }
          break;
      default:
          break;
   }
}

/* GLDRAW  - draw a polyline.  The attributes of the line have been
 *          previously set (by the GLATTR routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points to plot
 *
 */
#define MAX_LINE_POINTS  500
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gldraw_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLDRAW_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gldraw(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLDRAW(xpts, ypts, npts)
#endif
float   xpts[], ypts[];
#if PRECISION == 0
int   *npts;
#else
int   npts[2];
#endif
{
   int     npts_temp, i, indx;
#if PRECISION == 0
   npts_temp = *npts;
#else
   npts_temp = npts[0];
#endif

   glBegin(GL_LINE_STRIP);
   for (i = 0; i < npts_temp; i++) {
#if PRECISION == 0
      indx = i;
#else
      indx = 2*i;
#endif
      glVertex2f(xpts[indx], ypts[indx]);
   }
   glEnd();

}

/* GLPOIN - draw a point.
 *          previously set (by the GLATTR routine).
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 * isize  - size (in pixels) of point
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glpoin_(ix, iy, isize)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLPOIN_(ix, iy, isize)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glpoin(ix, iy, isize)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLPOIN(ix, iy, isize)
#endif
#if PRECISION == 0
float   *ix, *iy, *isize;
#else
float   ix[2], iy[2], isize[2];
#endif
{
   int i;

#if PRECISION == 0
   width = *isize;
#else
   width = isize[0];
#endif
   if (width < 1.0) width = 1.0;
   glPointsize(width);
   glBegin(GL_POINTS);
#if PRECISION == 0
   glVertex2f(*ix, *iy);
#else
   glVertex2f(ix[0], iy[0]);
#endif
   glEnd();

}


/* GLREFL - fill a region.
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points in the polygon (if 2, assume a
 *          rectangle, otherwise, a convex polygon)
 *
 */
#define MAX_REG_POINTS  1000
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void glrefl_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLREFL_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void glrefl(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLREFL(xpts, ypts, npts)
#endif
float xpts[], ypts[];
#if PRECISION == 0
int   *npts;
#else
int   npts[2];
#endif
{
   int     i, indx, npts_temp;

#if PRECISION == 0
   npts_temp = *npts;
#else
   npts_temp = npts[0];
#endif

   /* Rectangle Case */
   if (npts_temp == 2) {        /* rectangle */
#if PRECISION == 0
      glRectf(xpts[0],ypts[0],xpts[1],ypts[1]);
#else
      glRectf(xpts[0],ypts[0],xpts[2],ypts[1]);
#endif
   }
   else if (npts_temp > 2) {    /* convex polygon */
      glPolygonMode(GL_FRONT, GL_FILL);
      glPolygonMode(GL_BACK, GL_FILL);
      glBegin(GL_POLYGON);
      for (i = 0; i < npts_temp; i++) {
#if PRECISION == 0
         indx = i;
#else
         indx = 2*i;
#endif
         glVertex2f(xpts[indx], ypts[indx]);
      }
   }
   glEnd();

}

/* GLTATT - set the font for drawing text strings.  Note that the fonts
 *          available on a particular workstation can vary, so a test
 *          is made to see if the font exists.  If not, the 8x13 font
 *          will be used (this should be available on all X
 *          workstations.
 *
 *          It is the calling programs responsibility to provide a
 *          valid font name.  This routine does no checking for valid
 *          names since it is possible for local sites to have their own
 *          predefined font names.  The calling program should be sure
 *          to end the font name string with a null (i.e., ascii 0).
 *
 *          In order to avoid excessive reloading of the same font, the
 *          current font name is stored.  A change is made only if the
 *          requested font differs from the current font.
 *
 * font        - name of font
 * error       - 0 - valid font,
 *               1 - invalid font, leave current font in place
 *               2 - invalid font, set default
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gltatt_(font, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLTATT_(font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gltatt(font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLTATT(font, error)
#endif
int    font[];
#if PRECISION == 0
int    *error;
#else
int    error[2];
#endif
{

   int          itest;          /* temporary variables */
   int          len;            /* number of characters in font name */
   char         font_name[80];  /* converted font name */
   char         *font_name_2;   
   int          i;
   int          direction_hint, font_ascent, font_descent, font_gap;
#if UNIX == 0
   XCharStruct  overall;
#endif

   strcpy(font_name," ");
#if PRECISION == 0
   i_to_s_3(font, font_name, 80, &len);
#else
   i_to_s_3(font, font_name, 160, &len);
#endif

/* Unix (GLX) Case */
#if UNIX == 0
   /* Check current font against requested font */
   itest = strcmp(font_name, FONT_NAME_CURRENT);
   if (itest != 0) {
     if (strcmp(FONT_NAME_CURRENT,FONT_NULL) != 0) {
       XFreeFont(display, font_struct);      /* free default font */
     }
     font_name_2 = &font_name[0];
     font_struct = XLoadQueryFont (display, font_name_2);
     if (font_struct == 0) {
       if (strcmp(FONT_NAME_CURRENT,FONT_NULL) == 0)  { 
          strcpy(font_name,FONT_NAME_DEFAULT); /* load default font */
#if PRECISION == 0
         *error = 2;
#else
         error[0] = 2;
#endif
       }
       else {                              /* leave current font */
         strcpy(font_name, FONT_NAME_CURRENT);
#if PRECISION == 0
         *error = 1;
#else
         error[0] = 1;
#endif
       }
       font_name_2 = &font_name[0];
       font_struct = XLoadQueryFont(display, font_name_2);
     }
     strcpy(FONT_NAME_CURRENT, font_name);
/*
     XSetFont(display, gc, font_struct->fid);
     XTextExtents(font_struct, " ", 0, &direction_hint, &font_ascent,
          &font_descent, &overall);
     font_gap = (font_ascent + font_descent) * .20;
     FONT_HEIGHT_CURRENT = font_ascent + font_descent;
     FONT_GAP_CURRENT = font_gap;
 */
   }
#endif

}

/* GLTEXH - draw a horizontal text string.
 *
 *          Use XDrawString rather than XDrawText so that background pixels
 *          are not changed.  This is so that other lines that may intersect
 *          the character box will not be blanked out.
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
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gltexh_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLTEXH_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gltexh(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLTEXH(string, ixpos, iypos, ijusth, ijustv, error)
#endif
int    string[];
#if PRECISION == 0
int    *ixpos, *iypos, *ijusth, *ijustv, *error;
#else
int    ixpos[2], iypos[2], ijusth[2], ijustv[2], error[2];
#endif
{

   int          itest, itempx, itempy;   /* temporary variables */
   int          len;                     /* number of characters in string */
   int          string_width;            /* width of string in pixels */
   char         string2[130];            /* converted string */
   int          i;
   int          ixpos_temp, iypos_temp, ijusth_temp, ijustv_temp;

#if PRECISION == 0
   ixpos_temp = *ixpos;
   iypos_temp = *iypos;
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
#else
   ixpos_temp = ixpos[0];
   iypos_temp = iypos[0];
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
#endif

#if PRECISION == 0
   i_to_s_3(string, string2, 130, &len);
#else
   i_to_s_3(string, string2, 260, &len);
#endif
   string_width = XTextWidth(font_struct, string2, len);

   switch (ijusth_temp) {
      case 0:                       /* Left justified string */
          itempx = ixpos_temp;
          break;
      case 1:                       /* Center justified string */
          itempx = ixpos_temp - (string_width/2);
          break;
      case 2:                       /* Right justified string */
          itempx = ixpos_temp - string_width;
          break;
      default:
          itempx = ixpos_temp;
          break;
   }
   switch (ijustv_temp) {
      case 0:                       /* Center justified string */
          itempy = iypos_temp + (FONT_HEIGHT_CURRENT/2.0);
          break;
      case 1:                       /* Bottom justified string */
          itempy = iypos_temp;
          break;
      case 2:                       /* Top justified string */
          itempy = iypos_temp + FONT_HEIGHT_CURRENT;
          break;
      default:
          itempy = iypos_temp + (FONT_HEIGHT_CURRENT/2.0);
          break;
    }


#if UNIX == 0
/*
    XDrawString(display, window, gc, itempx, itempy, string2, len);
    if (PIXMAP_CURRENT == 1) {
      XDrawString(display, pixmap, gc, itempx, itempy, string2, len);
    }
 */
#endif

}

/* GLTEXV - draw a horizontal text string.
 *
 *          Use XDrawString rather than XDrawText so that background pixels
 *          are not changed.  This is so that other lines that may intersect
 *          the character box will not be blanked out.
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
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gltexv_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GLTEXV_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gltexv(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GLTEXV(string, ixpos, iypos, ijusth, ijustv, error)
#endif
int    string[];
#if PRECISION == 0
int    *ixpos, *iypos, *ijusth, *ijustv, *error;
#else
int    ixpos[2], iypos[2], ijusth[2], ijustv[2], error[2];
#endif
{

   int          itest, itempx, itempy;   /* temporary variables */
   int          len;                     /* number of characters in string */
   int          y_pix_len;               /* height of entire string */
   int          string_width;            /* width of string in pixels */
   char         string2[130];            /* converted string */
   int          i, ijunk;
#if PRECISION == 0
   int          string3[2];              /* one character at a time */
#else
   int          string3[4];              /* one character at a time */
#endif
   int          ixpos_temp, iypos_temp, ijusth_temp, ijustv_temp;

#if PRECISION == 0
   ixpos_temp = *ixpos;
   iypos_temp = *iypos;
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
#else
   ixpos_temp = ixpos[0];
   iypos_temp = iypos[0];
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
#endif


#if PRECISION == 0
   i_to_s_3(string, string2, 130, &len);
#else
   i_to_s_3(string, string2, 260, &len);
#endif
   y_pix_len = len * (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);

   switch (ijustv_temp) {
      case 0:                       /* Center justified string */
          itempy = -(y_pix_len/2) + FONT_HEIGHT_CURRENT;
          break;
      case 1:                       /* Bottom justified string */
          itempy = -y_pix_len + FONT_HEIGHT_CURRENT;
          break;
      case 2:                       /* Top justified string */
          itempy = FONT_HEIGHT_CURRENT;
          break;
      default:
          itempy = -(y_pix_len/2) + FONT_HEIGHT_CURRENT;
          break;
    }
    itempy = iypos_temp + itempy;

#if PRECISION == 0
   string3[1] = 0;
   for (i = 0; i < len; i++) {  /* plot each character one at a time */
      string3[0] = string[i];
      i_to_s_3(string3,string2, 2, &ijunk);
      string_width = XTextWidth(font_struct, string2, 1);

     switch (ijusth_temp) {
        case 0:                       /* Left justified string */
            itempx = 0;
            break;
        case 1:                       /* Center justified string */
            itempx = (string_width/2);
            break;
        case 2:                       /* Right justified string */
            itempx = string_width;
            break;
        default:
            itempx = ixpos_temp;
            break;
     }

/*
     itempx = ixpos_temp - itempx;
     XDrawString(display, window, gc, itempx, itempy, string2, 1);
     if (PIXMAP_CURRENT) {
       XDrawString(display, pixmap, gc, itempx, itempy, string2, 1);
     }
     itempy = itempy + (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);
 */

   }
#else
   string3[1] = 0;
   string3[2] = 0;
   string3[3] = 0;
   for (i = 0; i < len; i++) {  /* plot each character one at a time */
      string3[0] = string[2*i];
      i_to_s_3(string3,string2, 4, &ijunk);
      string_width = XTextWidth(font_struct, string2, 1);

     switch (ijusth_temp) {
        case 0:                       /* Left justified string */
            itempx = 0;
            break;
        case 1:                       /* Center justified string */
            itempx = (string_width/2);
            break;
        case 2:                       /* Right justified string */
            itempx = string_width;
            break;
        default:
            itempx = ixpos_temp;
            break;
     }

/*
     itempx = ixpos_temp - itempx;
     XDrawString(display, window, gc, itempx, itempy, string2, 1);
     if (PIXMAP_CURRENT) {
       XDrawString(display, pixmap, gc, itempx, itempy, string2, 1);
     }
     itempy = itempy + (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);
 */

   }
#endif

}

/* i_to_s_3  - utitlity routine to convert an integer array containing
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
void i_to_s_3(string1, string2, maxlen, ilen)
int   string1[], maxlen, *ilen;
char  string2[];

{
     int  i;
     int  itemp;
     i = 0;
#if PRECISION == 0
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

/* Set the screen size, use following steps:
 *
 * orien = 2:
 *
 * if xpixels and ypixels are zero, use the default sizes given
 * by this routine.  Otherwise, use xpixels and ypixels.  However,
 * compare them to the number of pixels available on the screen
 * (i.e., the root window).  If not within a certain tolerance, use
 * a half the screen width and 2/3 the screen height.
 *
 * orien = 0:
 *
 * This specifies a landscape orientation.  If the calling routine
 * specifies a positive number of x pixels, this will be used as the
 * x size.  The y size will then be (8.5/11.) of the x size.  If no
 * x size is given (i.e., xpixels = 0), then the default x size will
 * be used.  In either case, the aspect ratio will be constrained.
 *
 * orien = 1:
 *
 * This specifies a portrait orientation.  This works similarly to
 * the landscape orientation, except now the x size will be (8.5/11.)
 * of the y size.  If ypixels = 0, base the y size on the default x
 * size.
 *
 * orien = 3:
 *
 * This specifies a square orientation.  If ypixels = 0, then base on
 * default y size, otherwise use the ypixels value.
 */

void gl_screen(xpixels, ypixels, orien)
int   xpixels;  /* suggested width in pixels */
int   ypixels;  /* suggested height in pixels */
int   orien;    /* orientation (landscape, portrait) */

{
   int   temp;
   float atemp;

   switch (orien) {
   case 2:              /* User specified height and width (in pixels) */
       if (xpixels > 0 && ypixels > 0) {
         temp = xpixels;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       else {            /* Use default size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = DEFAULT_Y_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       xsh.min_aspect.x = 2;
       xsh.min_aspect.y = 3;
       xsh.max_aspect.x = 4;
       xsh.max_aspect.y = 3;
       break;
   case 0:                     /* Landscape orientation */
       if (xpixels > 0 ) {     /* User specified x size */
         temp = xpixels;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         atemp = xpixels * (8.5/11.0);
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
       }
       else {                 /* Use default x size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         atemp = temp * (8.5/11.0);
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
       }
       xsh.min_aspect.x = 11;
       xsh.min_aspect.y = 8;
       xsh.max_aspect.x = 11;
       xsh.max_aspect.y = 9;
       break;
   case 1:                      /* Portrait orientation */
       if (ypixels > 0) {       /* User specified y size */
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
         atemp = temp * (8.5/11.);
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
         }
         temp = atemp + 0.5;
         xsh.width = temp;
       }
       else {                   /* Use default size (base on X default) */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
         atemp = temp * (8.5/11.0);
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
         }
         temp = atemp + 0.5;
         xsh.width = temp;
       }
       xsh.min_aspect.x = 8;
       xsh.min_aspect.y = 11;
       xsh.max_aspect.x = 9;
       xsh.max_aspect.y = 11;
       break;
   case 3:                     /* Square orientation */
       if (ypixels > 0 ) {     /* User specified y size */
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.5*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
         xsh.width = temp;
       }
       else {                 /* Use default x size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.5*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
         xsh.width = temp;
       }
       xsh.min_aspect.x = 1;
       xsh.min_aspect.y = 1;
       xsh.max_aspect.x = 1;
       xsh.max_aspect.y = 1;
       break;
   default:           /* Orientation flag not specified */
       if (xpixels > 0 && ypixels > 0) {
         temp = xpixels;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       else {            /* Use default size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = DEFAULT_Y_SIZE;
         atemp = temp;
         if (atemp < 0.3*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       xsh.min_aspect.x = 2;
       xsh.min_aspect.y = 3;
       xsh.max_aspect.x = 4;
       xsh.max_aspect.y = 3;
       break;
    }

}

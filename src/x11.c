/*  x11.c  - c routines for Dataplot X11 driver */
/*  UPDATED  - June     1991.  Fixed bug in color handling. */
/*  UPDATED  - June     1991.  Use color names rather than specific
 *                             RGB values.  Had a problem with PC 
 *                             implementations not having enough free
 *                             color cells.  The global variable 
 *                             COLOR_TYPE is used to control which 
 *                             method is used.
 *  UPDATED  - January  1992.  Problem with pixmap on black-white
 *                             devices, modified "xerase" routine.
 *  UPDATED  - January  1992.  Variable FONT_NAME_DEFAULT to softcode
 *                             default X11 font.
 *  UPDATED  - April    1992.  Handle routine names as upper case (for
 *                             the Cray).  Use SUBROUTINE_CASE.
 *  UPDATED  - August   1992.  Minor bug fix.  Also, add support for
 *                             gray scale and a color to the list of
 *                             supported colors.
 *  UPDATED  - October  1993.  Definitions can now be specified on the
 *                             compile line as -D options rather than
 *                             requiring source changes.
 *  UPDATED  - May      1994.  XREGFL routine modified.  DATAPLOT
 *                             now allows more complicated fills.  Note
 *                             that there still seems to be a problem
 *                             if curve is both above and below the
 *                             the region base (e.g., PLOT SIN(X) FOR
 *                             X = 0 .1 25  (may have to revert to 
 *                             software fill for complex regions).
 *  UPDATED  - November 1994.  Modify i_to_s usage to work under both
 *                             ANSI C and K&R C.
 *  UPDATED  - May      1995.  Modify xinit to allow window id to be
 *                             passed from front-end.
 *  UPDATED  - July     1995.  Modify xupdat for front-end
 *  UPDATED  - June     1996.  Option to specify double precision for
 *                             real numbers instead of float (intended
 *                             for when the Fortran is compiled in
 *                             double precision, e.g., use of -r8 on
 *                             Unix Fortran.  Do not use for Cray
 *                             since their single precision is 64-bit
 *                             for both Fortran and C.
 *                             To turn on, use -DDOUBLE on C compile.
 *                             Introduces complications in that C
 *                             still uses 32-bit for both int and long.
 *  UPDATED  - April    1997.  Support for the following Dataplot
 *                             commands:
 *                                SAVE PLOT <file name>
 *                                SAVE PLOT AUTOMATIC
 *
 *                                REPEAT GRAPH <file>
 *                                CYCLE GRAPH
 *                             Add the following X11 routines:
 *                                xsaveg - routine to save the current
 *                                         pixmap to the specified file
 *                                xrestg - routine to restore a pixmap
 *                                         that was previously saved.
 *                                xcycle - cycle through previously
 *                                         created graphs
 *                             For performance reasons, the saving and
 *                             restoring of pixmaps is limited to
 *                             bitmaps rather than images (i.e.,
 *                             bitmaps have a depth of 1 = no color).
 *   
 *                             Change method for setting window
 *                             properties fro X11R3 to X11R4.  Add
 *                             a switch to determine which method to
 *                             use.
 *  UPDATED  - October  1997.  A few minor tweaks to "xrestg" to
 *                             make the April 1997 modifications
 *                             work with the GUI.
 *  UPDATED  - August   2007.  Dataplot restricts user requests for
 *                             the size of the window to be between
 *                             0.3 times root width/height and
 *                             0.8 times root width/height.  For some
 *                             of the newer "large" screens, this lower
 *                             limit may result in too large a window.
 *                             So change the lower proportion to 0.1.
 *  UPDATED  - April    2009.  Change <strings.h> to <string.h>
 *  UPDATED  - May      2009.  When using the "-DDOUBLE" option, need
 *                             to make a distinction between real
 *                             numbers and integers since the default
 *                             byte size can be set independently
 *                             for these.  Add the option "-DINTEGER8"
 *                             to handle the case where integers set
 *                             to 8 bytes on a 32-bit machine.
 *  UPDATED  - JANUARY  2015.  Implement routines for cut and paste
 *                             operations.  Use routines from "xclip"
 *                             package to implement.
 *
 *                             Use -DXCLIP to indicate "xclib" from xclip
 *                             is available.  If not, then copy/paste
 *                             operations will not be available.
 */
/*  This driver has been tested on Suns (both monochrome and color),
 *  Silicon Graphics Iris, HP-9000, a Cray Y-MP, DEC workstation, 
 *  and a PC implementation.   */
/*
 *  The Dataplot command can be entered from any currently open window.
 *  Dataplot's normal alphanumeric I/O (e.g., entering commands, output
 *  from FIT's) will occur in this window.  Dataplot will create a separate
 *  graphics window where the graphical output will be generated.  Note
 *  that although Dataplot will send a recommended size and position for
 *  the graphics window, the user has control over this through the window
 *  manager.  Dataplot will check for the actual dimensions whenever a clear
 *  screen command is entered.  The bit gravity is set to NorthWest so that
 *  drawing coordinates will be valid until the next erase page command.
 *
 *  A dummy version of this library is maintained for those systems
 *  that do not support X11 or that do not allow C routines to be called
 *  from Fortran.  Since the dummy library is coded in Fortran, routine
 *  names will be limited to six characters.
 *
 *  Note that calling C from Fortran is not standard.  Therefore, these
 *  routines may require some tweaking for some operating systems.  This
 *  version was tested on the Sun (using Unix).  The following is a list
 *  of portability issues.
 *
 *  1) The Sun Unix system appends an underscore ("_") to the Fortran
 *     name.  For example, if the Fortran routine calls XEND, the C name
 *     will be XEND_.
 *
 *     NOTE: May, 1990.  The global variable APPEND_UNDERSCORE is now
 *           used to handle this problem.  If your system appends the
 *           underscore, set this variable to 1.  Otherwise set it to
 *           0.  Conditional compilation statements will insert the correct
 *           function name.
 *
 *     NOTE: April, 1992.  Most Unix Fortran compilers automatically 
 *           convert routine names to lower case.  However, the Cray does
 *           not.  Add the global variable SUBROUTINE_CASE to handle this.
 *           For lower case, set this variable to 1.  For upper case, set
 *           this variable to 0.
 *
 *     NOTE: October, 1993.  The above 2 definitions are now controlled
 *           with the -DNOUNDERSCORE and -DUPPERCASE compile definitions.
 *           The default is an underscore and lower case.  This new way allows
 *           the changes to be made without changes in the source code.
 *
 *  2) The primary portability problem will be in how arguments are passed
 *     back and forth between Fortran and C.  Note that Fortran passes
 *     arguments by reference (i.e., it sends the address) while C passes
 *     by value (i.e., a local copy is made of the variable).  On the
 *     Sun (and most Unix systems), real and integer arguments can be sent
 *     via the function arguments.  Be aware that the C subroutine must
 *     declare the function arguments to be pointers since Fortran is
 *     sending an address rather than a value.  The function argument must
 *     be declared as a pointer even if its value is not being returned to
 *     the calling Fortran program.
 *
 *     However, note that arrays are treated as pointers in C, so array
 *     names are not declared as pointers in C.
 *
 *     Character strings should be sent as an array of Ascii Decimal
 *     Equivalents (i.e., "A" should be sent as the integer 65, "0"
 *     should be sent as the integer 48) to correspond to how C defines
 *     character strings.  Note that the string should end with the null
 *     character (integer 0).
 *
 *     If your system passes data differently (e.g., through some type of
 *     common), both the calling Fortran program and the C code here will
 *     have to be modified.
 *
 *  3) The Unix makefile may need to be modified for some Unix systems.
 *     This should be straightforward (may need to specify different
 *     directories for X11 or system libraries).
 *
 *  This initial version is strictly a "device driver".  Development to
 *  utilize the windowing capabilities of X is under consideration, but is
 *  not yet implemented.
 *
 *  The primary references for writing this driver were:
 *
 *     "Introduction to the X Window System" by Oliver Jones (1989).
 *     "Xlib Programming Manual for Version 11" by Adrian Nye (1990).
 *
 *  The following routines are included:
 *
 *  xcheck     - check for expose and configure events
 *  xrdloc     - read mouse position when button pressed
 *  xinit      - initialize X11
 *  xend       - close X11
 *  xclear     - flush the buffer
 *  xerase     - clear the screen
 *  xupdat     - copy pixmap to screen
 *  xfore      - set the foreground color
 *  xback      - set the background color
 *  xlattr     - set line attributes (width, style, cap, join)
 *  xdraw      - draw a polyline
 *  xpoint     - draw a point
 *  xcirc      - draw a circle
 *  xregfl     - solid fill of a region
 *  xtexth     - draw a horizontal character string
 *  xtextv     - draw a vertical character string
 *  xtattr     - set text attributes
 *  xsaveg     - copy pixmap to a specified file
 *  xrestg     - copy pixmap from a specified file to the screen
 *  xcycle     - cycle through previously saved pixmaps
 *  xfetch     - retrieve data from the clipboard
 *  i_to_s     - utility routine to convert array of ADE's to string array
 *  set_screen - utility routine to set screen height and width
 *
 */

/*  Site dependent definitions (see comments above) */
/*  Default is an underscore and lower case.  The compiler specified
 *  definitions -DNOUNDERSCORE and -DUPPERCASE can be specified to override
 *  these defaults. */

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
/* Following 5 lines added June, 1996.  */
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
#ifdef XCLIP
#define HAVE_XCLIP 1
#else
#define HAVE_XCLIP 0
#endif
#define COLOR_TYPE 1

/*  include files */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

#if HAVE_XCLIP
#include <unistd.h>
#include <ctype.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Atoms.h>
#include <X11/Xresource.h>
#include <X11/keysym.h>  */
#include <xcdef.h>
#include <xcprint.h>
#include <xclib.h>
#endif

/* add following 2 lines MAY 1995   */
#include <stdlib.h>
#include <math.h>
/* #include <strings.h> */
#include <string.h>

/* global definitions */

#if COLOR_TYPE
#define MAX_COLORS    163
#else
#define MAX_COLORS    16
#endif
#define MAX_GRAY         100
#define BORDER_WIDTH      3
/*
#define DEFAULT_X_SIZE  600
#define DEFAULT_Y_SIZE  465
*/
#define DEFAULT_X_SIZE  550
#define DEFAULT_Y_SIZE  425
#define DEFAULT_X        50
#define DEFAULT_Y        50
#define MIN_X_SIZE      200
#define MIN_Y_SIZE      200

/* X11 declarations */
Display       *display;           /* display connection */
Window        window;             /* window identifier */
Window        window2;            /* window identifier for repeat graph */
Pixmap        pixmap;             /* pixmap identifier */
Pixmap        pixmap2, pixmap3;   /* pixmap identifier for repeat graph */
GC            gc;                 /* graphics context */
GC            gc2;                /* graphics context for repeat graph */
XGCValues     gcvalues;           /* set graphics context attributes */
XColor        colors[MAX_COLORS]; /* colors to draw with */
XColor        grays[MAX_GRAY];    /* gray scales to draw with */
XColor        exact_def;          /* returns exact RGB values */
XFontStruct   *font_struct;       /* returned font structure pointer */
Colormap      color_map;          /* default color map */
Visual        *vis;               /* pointer to visual structure */
XEvent        event;              /* holds X server events */
XSetWindowAttributes  window_attributes;    /* attributes structure */
XWindowAttributes     returned_attributes;  /* returned attributes */
unsigned long value_mask;         /* value mask for window attributes */
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

/* common parameters */
unsigned int  width, height;      /* last known window size */
unsigned int  width2, height2;    /* width, height for second window */
unsigned long black, white;       /* values for black and white */
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
int          pixmap_flag;         /* 0 - do not draw to pixmap
                                     1 - draw to pixmap */
int          color_list[MAX_COLORS]; /* Color availability */
int          color_list_gray[MAX_GRAY]; /* Gray scale availability */
char         *color_names[] = {   /* define color names (for Release */
 /*  0 */        "black",         /* 3, p. 184 of Adrian Nye book,   */
 /*  1 */        "white",         /* these should be recognized by   */
 /*  2 */        "green",         /* all Release 3 implementations.  */
 /*  3 */        "yellow",        /* If color not physically         */
 /*  4 */        "red",           /* available, should be mapped to  */
 /*  5 */        "blue",          /* closest possible alternative.   */
 /*  6 */        "magenta",       /* Updated August 1992 to add      */
 /*  7 */        "cyan",          /* some colors from R4 color       */
 /*  8 */        "orange",        /* database (Appendix D of Xlib    */
 /*  9 */        "yellow green",  /* Programmer's Reference from     */
 /* 10 */        "dark green",    /* O'Reilly and Associates (only a */
 /* 11 */        "light blue",    /* few of the additions for R4 are */
 /* 12 */        "blue violet",   /* supported, may add more later). */
 /* 13 */        "violet red",
 /* 14 */        "dark slate gray",
 /* 15 */        "light gray",
 /* 16 */        "aquamarine",
 /* 17 */        "brown",
 /* 18 */        "cadet blue",
 /* 19 */        "coral",
 /* 20 */        "cornflower blue",
 /* 21 */        "dark olive green",
 /* 22 */        "dark orchid",
 /* 23 */        "dark slate blue",
 /* 24 */        "dark turquoise",
 /* 25 */        "firebrick",
 /* 26 */        "forest green",
 /* 27 */        "gold",
 /* 28 */        "goldenrod",
 /* 29 */        "gray",
 /* 30 */        "indian red",
 /* 31 */        "khaki",
 /* 32 */        "dim gray",
 /* 33 */        "light steel blue",
 /* 34 */        "lime green",
 /* 35 */        "maroon",
 /* 36 */        "medium aquamarine",
 /* 37 */        "medium blue",
 /* 38 */        "medium forest green",
 /* 39 */        "light goldenrod yellow",
 /* 40 */        "medium orchid",
 /* 41 */        "medium sea green",
 /* 42 */        "medium slate blue",
 /* 43 */        "medium spring green",
 /* 44 */        "medium turquoise",
 /* 45 */        "medium violet red",
 /* 46 */        "midnight blue",
 /* 47 */        "navy",
 /* 48 */        "orange red",
 /* 49 */        "orchid",
 /* 50 */        "pale green",
 /* 51 */        "pink",
 /* 52 */        "plum",
 /* 53 */        "purple",
 /* 54 */        "salmon",
 /* 55 */        "sea green",
 /* 56 */        "sienna",
 /* 57 */        "sky blue",
 /* 58 */        "slate blue",
 /* 59 */        "spring green",
 /* 60 */        "steel blue",
 /* 61 */        "tan",
 /* 62 */        "thistle",
 /* 63 */        "turquoise",
 /* 64 */        "violet",
 /* 65 */        "wheat",
 /* 66 */        "green yellow",
 /* 67 */        "light cyan",    /* This starts additions for R4 */
 /* 68 */        "blue2",
 /* 69 */        "blue3",
 /* 70 */        "blue4",
 /* 71 */        "cyan2",
 /* 72 */        "cyan3",
 /* 73 */        "cyan4",
 /* 74 */        "green2",
 /* 75 */        "green3",
 /* 76 */        "green2",
 /* 77 */        "yellow2",
 /* 78 */        "yellow3",
 /* 79 */        "yellow4",
 /* 80 */        "orange2",
 /* 81 */        "orange3",
 /* 82 */        "orange4",
 /* 83 */        "red2",
 /* 84 */        "red3",
 /* 85 */        "red4",
 /* 86 */        "magenta2",
 /* 87 */        "magenta3",
 /* 88 */        "magenta4",
 /* 89 */        "LightCoral",
 /* 90 */        "DarkSalmon",
 /* 91 */        "LightSalmon",
 /* 92 */        "Crimson",
 /* 93 */        "DarkRed",
 /* 94 */        "LightPink",
 /* 95 */        "HotPink",
 /* 96 */        "DeepPink",
 /* 97 */        "PaleVioletRed",
 /* 98 */        "Tomato",
 /* 99 */        "DarkOrange",
 /* 100 */       "LightYellow",
 /* 101 */       "LemonChifon",
 /* 102 */       "PapayaWhip",
 /* 103 */       "Mocasin",
 /* 104 */       "PeachPuff",
 /* 105 */       "PaleGoldenrod",
 /* 106 */       "DarkKhaki",
 /* 107 */       "Lavender",
 /* 108 */       "Fuchsia",
 /* 109 */       "MediumPurple",
 /* 110 */       "Amethyst",
 /* 111 */       "DarkViolet",
 /* 112 */       "DarkMagenta",
 /* 113 */       "Indigo",
 /* 114 */       "Chartreuse",
 /* 115 */       "LawnGreen",
 /* 116 */       "Lime",
 /* 117 */       "LightGreen",
 /* 118 */       "OliveDrab",
 /* 119 */       "Olive",
 /* 120 */       "DarkSeaGreen",
 /* 121 */       "LightSeaGreen",
 /* 122 */       "DarkCyan",
 /* 123 */       "Teal",
 /* 124 */       "PaleTurquoise",
 /* 125 */       "Aqua",
 /* 126 */       "PowderBlue",
 /* 127 */       "LightSkyBlue",
 /* 128 */       "DeepSkyBlue",
 /* 129 */       "DodgerBlue",
 /* 130 */       "RoyalBlue",
 /* 131 */       "DarkBlue",
 /* 132 */       "Cornsilk",
 /* 133 */       "BlanchedAlmond",
 /* 134 */       "Bisque",
 /* 135 */       "Navajo White",
 /* 136 */       "Burly Wood",
 /* 137 */       "Rosy Brown",
 /* 138 */       "Sandy Brown",
 /* 139 */       "DarkGoldenrod",
 /* 140 */       "Peru",
 /* 141 */       "Chocolate",
 /* 142 */       "SaddieBrown",
 /* 143 */       "Snow",
 /* 144 */       "Honeydew",
 /* 145 */       "MintCream",
 /* 146 */       "Azure",
 /* 147 */       "AliceBlue",
 /* 148 */       "GhostWhite",
 /* 149 */       "WhiteSmoke",
 /* 150 */       "SeaShell",
 /* 151 */       "Beige",
 /* 152 */       "OldLace",
 /* 153 */       "FloralWhite",
 /* 154 */       "Ivory",
 /* 155 */       "AntiqueWhite",
 /* 156 */       "Linen",
 /* 157 */       "MistyRose",
 /* 158 */       "Gainsboro",
 /* 159 */       "Silver",
 /* 160 */       "DarkGray",
 /* 161 */       "LightSlateGray",
 /* 162 */       "SlateGray"
              };
char         *gray_names[] = {    /* define gray scale names         */
                 "gray0",
                 "gray1", 
                 "gray2", 
                 "gray3", 
                 "gray4", 
                 "gray5", 
                 "gray6", 
                 "gray7", 
                 "gray8", 
                 "gray9", 
                 "gray10", 
                 "gray11", 
                 "gray12", 
                 "gray13", 
                 "gray14", 
                 "gray15", 
                 "gray16", 
                 "gray17", 
                 "gray18", 
                 "gray19", 
                 "gray20", 
                 "gray21", 
                 "gray22", 
                 "gray23", 
                 "gray24", 
                 "gray25", 
                 "gray26", 
                 "gray27", 
                 "gray28", 
                 "gray29", 
                 "gray30", 
                 "gray31", 
                 "gray32", 
                 "gray33", 
                 "gray34", 
                 "gray35", 
                 "gray36", 
                 "gray37", 
                 "gray38", 
                 "gray39", 
                 "gray40", 
                 "gray41", 
                 "gray42", 
                 "gray43", 
                 "gray44", 
                 "gray45", 
                 "gray46", 
                 "gray47", 
                 "gray48", 
                 "gray49", 
                 "gray50", 
                 "gray51", 
                 "gray52", 
                 "gray53", 
                 "gray54", 
                 "gray55", 
                 "gray56", 
                 "gray57", 
                 "gray58", 
                 "gray59", 
                 "gray60", 
                 "gray61", 
                 "gray62", 
                 "gray63", 
                 "gray64", 
                 "gray65", 
                 "gray66", 
                 "gray67", 
                 "gray68", 
                 "gray69", 
                 "gray70", 
                 "gray71", 
                 "gray72", 
                 "gray73", 
                 "gray74", 
                 "gray75", 
                 "gray76", 
                 "gray77", 
                 "gray78", 
                 "gray79", 
                 "gray80", 
                 "gray81", 
                 "gray82", 
                 "gray83", 
                 "gray84", 
                 "gray85", 
                 "gray86", 
                 "gray87", 
                 "gray88", 
                 "gray89", 
                 "gray90", 
                 "gray91", 
                 "gray92", 
                 "gray93", 
                 "gray94", 
                 "gray95", 
                 "gray96", 
                 "gray97", 
                 "gray98", 
                 "gray99" 
              };

/* flags for current attribute settings */
static int    OPEN_FLAG = 0;          /* 0 - X11 closed, 1 - X11 open */
static int    OPEN_FLAG_2 = 0;        /* 0 - repeat graph window closed, 1 - open */
int           GRAPH_FLAG = 0;         /* 0 - xerase has not been called, 1 - xerase has been called  */
int           WIDTH_CURRENT;          /* current line width */
int           LINE_STYLE_CURRENT;     /* current line style */
int           CAP_STYLE_CURRENT;      /* current cap style */
int           JOIN_STYLE_CURRENT;     /* current join style */
char          FONT_NAME_CURRENT[80];  /* name of current font */
char          FONT_NULL[80];          /* null font */
char          FONT_NAME_DEFAULT[80];  /* name of default font */
int           FONT_HEIGHT_CURRENT;    /* pixel ascent of current font */
int           FONT_DESCENT_CURRENT;   /* descent of current font */
int           FONT_GAP_CURRENT;       /* vertical gap of current font */
int           BACKGROUND_CURRENT;     /* current background color */
int           COLOR_CURRENT;          /* current color */
int           PIXMAP_CURRENT;         /* 0 - closed, 1 - open */
int           PIXMAP_CURRENT_2;       /* 0 - repeat graph closed, 1 - open */
int           ORIENTATION_CURRENT;    /* current orientation */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  xclear_(), xend_(), xcheck_();
void  xinit_(), xlattr_(), xdraw_();
void  xpoint_(), xcirc_(), xregfl_(), xrdloc_();
void  xfore_(),  xerase_(), xupdat_();
void  xsaveg_(),  xrestg_(), xcycle_();
void  xtexth_(), xtextv_(),xtattr_();
void  xfetch_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  XCLEAR_(), XEND_(), XCHECK_();
void  XINIT_(), XLATTR_(), XDRAW_();
void  XPOINT_(), XCIRC_(), XREGFL_(), XRDLOC_();
void  XFORE_(),  XERASE_(), XUPDAT_();
void  XSAVEG_(),  XRESTG_(), XCYCLE_();
void  XTEXTH_(), XTEXTV_(), XTATTR_();
void  XFETCH_();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  xclear(), xend(), xcheck();
void  xinit(), xlattr(), xdraw();
void  xpoint(), xcirc(), xregfl(), xrdloc();
void  xfore(),  xerase(), xupdat();
void  xsaveg(),  xrestg(), xcycle();
void  xtexth(), xtextv(),xtattr();
void  xfetch();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  XCLEAR(), XEND(), XCHECK();
void  XINIT(), XLATTR(), XDRAW();
void  XPOINT(), XCIRC(), XREGFL(), XRDLOC();
void  XFORE(),  XERASE(), XUPDAT();
void  XSAVEG(), XRESTG(), XCYCLE();
void  XTEXTH(), XTEXTV(), XTATTR();
void  XFETCH();
#endif
void  i_to_s(), set_screen(), xback();

/* XCHECK  - routine to check for X expose and configuration events.
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
void xcheck_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCHECK_(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xcheck(expose_flag_2, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCHECK(expose_flag_2, error_flag)
#endif
#if INTEGER_PRECISION == 0
int  *error_flag;
int  *expose_flag_2;
#else
int  error_flag[2];
int  expose_flag_2[2];
#endif
{
     XEvent   event;            /* holds Xserver events */

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
     while (XEventsQueued(display, QueuedAfterReading) != 0) {/* check queue */
          XNextEvent(display, &event);        /* get next event from queue */
          switch (event.type) {
          case DestroyNotify:   /* window has been destroyed */
              if (event.xdestroywindow.window == window2) {
                XFreeGC(display, gc2);
                XDestroyWindow(display,window2);
                OPEN_FLAG_2 = 0;
              }
              else {
                XFreeGC(display, gc);
                XDestroyWindow(display,window);
                XCloseDisplay(display);
                OPEN_FLAG = 0;
                OPEN_FLAG_2 = 0;
              }
              *error_flag = 1;
#if INTEGER_PRECISION == 0
              *error_flag = 1;
#else
              error_flag[0] = 1;
#endif
              break;
          case Expose:          /* portion of window has become visible */
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
}

/* XRDLOC  - routine to read a position from the graphics window (used
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
void xrdloc_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XRDLOC_(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xrdloc(ixret, iyret, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XRDLOC(ixret, iyret, error)
#endif
#if INTEGER_PRECISION == 0
int  *ixret, *iyret, *error;
#else
int  ixret[2], iyret[2], error[2];
#endif
{
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

#if INTEGER_PRECISION == 0
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
              XFreeGC(display, gc);
              XDestroyWindow(display,window);
              XCloseDisplay(display);
#if INTEGER_PRECISION == 0
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

       XSelectInput(display, window,    /* Reset default event selection */
         StructureNotifyMask |          /* Window notification events */
         ExposureMask);                 /* Expose events */

}

/* XINIT  - routine to initialize X11.
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
void xinit_(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag) 
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XINIT_(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xinit(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XINIT(xp, yp, or, ixret, iyret, display_name, iwind, iwindn,
            error_flag)
#endif
#if INTEGER_PRECISION == 0
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
     XTextProperty  WindowName, IconName;
     char *window_name = "Dataplot";
     char *icon_name = "Dataplot";
     int          xpixels, ypixels, orien, iwindn_temp;
     int          temp, i;
     int          class;
     int          bits_per_rgb;
     float        atemp;
     Status       result;
     int          len, itest;
     char         display_string[80];
     long         itemp;
     int          counter;
     double       atemp1;
     int          window_flag;
#if INTEGER_PRECISION == 1
     int          iplace;
#endif

#if INTEGER_PRECISION == 0
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
     if(OPEN_FLAG != 0) {                         /* X11 already open */
#if INTEGER_PRECISION == 0
       *error_flag = 2;
#else
       error_flag[0] = 2;
#endif
       return;
     }

     strcpy(display_string," ");
#if INTEGER_PRECISION == 0
     i_to_s(display_name, display_string, 80, &len);
#else
     i_to_s(display_name, display_string, 160, &len);
#endif
     itest = strncmp(display_string, "DEFAULT", 7);
     if (itest == 0) {          /* default display name */
       if((display = XOpenDisplay(NULL)) == NULL) { /*open display connection*/
#if INTEGER_PRECISION == 0
         *error_flag = 1;
#else
         error_flag[0] = 1;
#endif
         OPEN_FLAG = 0;
         return;
        }
     }
     else {           /* user specified display name */
       if((display = XOpenDisplay(display_string)) == NULL) {
#if INTEGER_PRECISION == 0
         *error_flag = 1;
#else
         error_flag[0] = 1;
#endif
         OPEN_FLAG = 0;
         return;
        }
     }

     OPEN_FLAG = 1;
     screen = DefaultScreen(display);        /* Set default screen */
     white = WhitePixel(display,screen);     /* Set white */
     black = BlackPixel(display,screen);     /* Set black */

     XGetGeometry(                           /* Get root window geometry */
                  display, DefaultRootWindow(display),
                  &root_id,                  /* root id */
                  &root_x, &root_y,          /* root position */
                  &root_width, &root_height, /* root width and height */
                  &root_bw, &root_depth);    /* root border width and depth */

     /* If window id passed from front-end, define window variable
      * from string in iwind and skip some code below.
      */

     set_screen(xpixels, ypixels, orien);
     ORIENTATION_CURRENT = orien;

     window_flag = 0;
     if ( iwindn_temp > 0 ) {
       window_flag = 1;
#if INTEGER_PRECISION == 0
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

     window = XCreateSimpleWindow(           /* Open graphics window */
                display,                     /* Pointer to Display structure */
                DefaultRootWindow(display),  /* Parent for new window */
                xsh.x,                       /* X position of window */
                xsh.y,                       /* Y position of window */
                xsh.width,                   /* Width of window */
                xsh.height,                  /* Height of window */
                BORDER_WIDTH,                /* Pixel width of border */
                black,                       /* Border color */
                white);                      /* Background color */

     /* Set the bit gravity to NorthWest.  This means that if the
      * window is resized or repositioned by the window manager, the
      * window will be redrawn from top left corner.  This means we
      * can use current scale units until next page erase (i.e., do not
      * change units until start a new graph).  This is best solution
      * since Dataplot is vector rather than raster oriented, so not
      * possible to redraw what was previously drawn.  Transformed window
      * may not use full window or it may clip current graph, but should
      * otherwise be OK.  Next erase page will start drawing in new units.
      */
     
    /* Come here if front-end opened the graphics window.  */

    frontend:

 if ( window_flag == 0) {
     window_attributes.bit_gravity = NorthWestGravity; /* Set bit gravity */
     value_mask = CWBitGravity;
     XChangeWindowAttributes(display, window, value_mask,
         &window_attributes);

#ifdef X11R3  /* Use following for X11 Release 3 and earlier */
     XSetStandardProperties(display, window, "Dataplot", "Dataplot",
       None, argv, argc, &xsh);
     XSetWMHints(display, window, &xwmh);
     XSetClassHint(display, window, &xch);
#else  /* X11R4 or later */
     if (XStringListToTextProperty(&window_name, 1, &WindowName) == 0) {
#if INTEGER_PRECISION == 0
        *error_flag = 1;
#else
        error_flag[0] = 1;
#endif
        return;
     }
     if (XStringListToTextProperty(&icon_name, 1, &IconName) == 0) {
#if INTEGER_PRECISION == 0
        *error_flag = 1;
#else
        error_flag[0] = 1;
#endif
        return;
     }
     XSetWMProperties(display, window, &WindowName, &IconName, 
                      argv, argc, &xsh, &xwmh, &xch);
#endif

     XSelectInput(display, window,    /* Types of graphic input to accept */
       StructureNotifyMask |          /* Window notification events */
       ExposureMask );                /* Expose events */

    XMapWindow(display, window);      /* Map the window (make visible) */
    XFlush(display);                  /* Flush the buffer */

 }

    /* Get actual height and width set by the window manager */
    XGetWindowAttributes(display, window, &returned_attributes);
    width = returned_attributes.width;
    height = returned_attributes.height;
#if INTEGER_PRECISION == 0
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

    gc = XCreateGC(display, window, 0, &gcvalues);/* Create graphics context */
    XSetState(display,                /* the current display */
      gc,                             /* the current graphics context */
      black,                          /* set default foreground color */
      white,                          /* set default background color */
      GXcopy,                         /* set default to "overwrite" */
      AllPlanes);
    XSetLineAttributes(display, gc,   /* current display , graphics context */
      0,                              /* default line width */
      LineSolid,                      /* default soild line */
      CapButt,                        /* default end-cap style */
      JoinMiter);                     /* default line join style */
    WIDTH_CURRENT = 0;
    LINE_STYLE_CURRENT = 0;
    CAP_STYLE_CURRENT = 0;
    JOIN_STYLE_CURRENT = 0;

    strcpy(FONT_NAME_CURRENT,FONT_NULL);
    PIXMAP_CURRENT = 0;
    pixmap_flag = 0;

    num_cells = XDisplayCells(display,screen);    /* Color inquiry calls */
    num_planes = XDisplayPlanes(display, screen);
    color_map = XDefaultColormap(display, screen);
    vis = XDefaultVisual(display, screen);
    class = vis->class;
    bits_per_rgb = vis->bits_per_rgb;
    depth = XDefaultDepth(display, screen);


    if (depth == 1) {                 /* one-plane monochrome */
      color_flag = 0;
    }
    else if (class == PseudoColor) {  /* multi-plane color */
      color_flag = 2;
    }
    else if (class == GrayScale)   {  /* multi-plane monochrome */
       color_flag = 1;
    }
    else if (class == StaticGray)  {  /* multi-plane monochrome, unchangeable
                                         color map */
       color_flag = 1;
    }
    else if (class == DirectColor) {  /* direct color */
       color_flag = 2;
    }
    else if (class == TrueColor)   {  /* direct color, unchangeable color map*/
       color_flag = 2;
    }
    else if (class == StaticColor) {  /* multi-plane color, unchangeable
                                         color map */
       color_flag = 2;
    }
    else {
       color_flag = 0;
    }

/* June, 1991.  Switch from specifying colors by specific RGB values
 * to using the named colors (as defined by the R3 Color Database).
 * This avoids the problem of running out of color cell entries
 * (occured on a PC implementation).  In addition, all implementations
 * at R3 or higher should support all the colors in the database 
 * (or at least map the color to the closest possible alternative
 * available on the particular workstation).  It has the drawback
 * that RGB values cannot be specified exactly, but this is not
 * not really a problem since most of the colors we defined for
 * Dataplot are defined in the database.  Release 4 and some       
 * implementations of X11 support a broader database.  We will limit
 * it to more conservative one since this should be supported on
 * all implementations (R3 or higher).  The global variable COLOR_TYPE
 * is used to control whether the old method or the new method is used.
 * A possible future enhancement might be to allow a user-specified
 * color map based on RGB values.  This would work better with the
 * old implementation.
 */
/* August 1992.  Allocate gray scale colors (for both monochrome
 * and color workstations). */
    max_colors = 0;
#if COLOR_TYPE
    if (color_flag > 1) {  /* define colors */
       max_colors = MAX_COLORS;
       for (i=0; i < MAX_COLORS; i++) {
           color_list[i] = 1;
           if (!XAllocNamedColor (display, color_map, color_names[i], 
               &colors[i], &exact_def)) {
             color_list[i] = 0;
           } 
        } 
    }
    color_list_gray[0] = 0;
    for (i=1; i < MAX_GRAY; i++) {
        color_list_gray[i] = 1;
        if (!XAllocNamedColor (display, color_map, gray_names[i], 
            &grays[i], &exact_def)) {
          color_list_gray[i] = 0;
        } 
    } 
#else
    if (color_flag > 1) {  /* define colors */
      max_colors = MAX_COLORS;
      colors[0].red = 0;   /* define black */
      colors[0].green = 0;
      colors[0].blue = 0;

      colors[1].red = 65535; /* define white */
      colors[1].green = 65535;
      colors[1].blue = 65535;

      colors[2].red = 0; /* define green */
      colors[2].green = 40000;
      colors[2].blue = 0;

      colors[3].red = 65535; /* define yellow */
      colors[3].green = 65535;
      colors[3].blue = 0;

      colors[4].red = 65535;   /* define red */
      colors[4].green = 0;
      colors[4].blue = 0;

      colors[5].red = 0;   /* define blue */
      colors[5].green = 0;
      colors[5].blue = 65535;

      colors[6].red = 65535;   /* define magenta */
      colors[6].green = 0;
      colors[6].blue = 65535;

      colors[7].red = 0; /* define cyan */
      colors[7].green = 65535;
      colors[7].blue = 65535;

      colors[8].red = 65535; /* define Yellow Orange  (or Orange) */
      colors[8].green = 32767;
      colors[8].blue = 0;

      colors[9].red = 32767; /* define Yellow Green */
      colors[9].green = 65535;
      colors[9].blue = 0;

      colors[10].red = 0; /* define Blue Green */
      colors[10].green = 65535;
      colors[10].blue = 32767;

      colors[11].red = 0; /* define Green Blue */
      colors[11].green = 32767;
      colors[11].blue = 65535;

      colors[12].red = 32767; /* define Blue Violet (or purple) */
      colors[12].green = 0;
      colors[12].blue = 65535;

      colors[13].red = 65535; /* define Red violet */
      colors[13].green = 0;
      colors[13].blue = 32767;

      colors[14].red = 21845; /* define Dark Grey */
      colors[14].green = 21845;
      colors[14].blue = 21845;

      colors[15].red = 43690; /* Define Light Grey */
      colors[15].green = 43690;
      colors[15].blue = 43690;

      for (i = 0; i < 15; i++) {   /* allocate the 16 colors */
         color_list[i] = 1;
         if (! XAllocColor(display, color_map, &colors[i])) {
           color_list[i] = 0;      /* color allocation failed */
         }
      }
    }
#endif

    BACKGROUND_CURRENT = -1;
    COLOR_CURRENT = -1;
    temp = 0;
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
    xfore_(&temp);
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
    XFORE_(&temp);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
    xfore(&temp);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
    XFORE(&temp);
#endif
    BACKGROUND_CURRENT = 1;

}

/* XERASE  - routine to clear the screen.  Check to see if the window
 *           configuration has been changed by the window manager (or
 *           by the user).  If so, reset the screen size.  Resizes
 *           done by the window manager will take precedence over resizes
 *           requested by the calling program (assume user resized window
 *           via the window manager).
 *
 *           Clear the pixmap as well as the graphics window.
 *
 *           Note that this routine assumes that XFLUSH has already
 *           been called to flush the buffer and that XCHECK has been
 *           called to check for expose and configure events.
 *
 *  xpixels   - width (in pixels) for graphics window
 *  ypixels   - height (in pixels) for graphics window
 *  orien     - window orientation (i.e., landscape, portrait, or none)
 *  ixret     - width returned to calling program
 *  iyret     - height returned to calling program
 *  back_col  - background color
 *  pix_flag  - 0 = no pixmap generated, 1 = pixmap generated
 *
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xerase_(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XERASE_(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xerase(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XERASE(xpixels, ypixels, orien, ixret, iyret, back_col, pix_flag)
#endif
#if INTEGER_PRECISION == 0
int  *xpixels, *ypixels, *orien, *back_col, *pix_flag;
int  *ixret, *iyret;
#else
int  xpixels[2], ypixels[2], orien[2], back_col[2], pix_flag[2];
int  ixret[2], iyret[2];
#endif
{

   int   temp_color, temp_color_2;
   int   xpixels_temp, ypixels_temp, orien_temp, back_col_temp;
   int   pix_flag_temp;

#if INTEGER_PRECISION == 0
   xpixels_temp = *xpixels;
   ypixels_temp = *ypixels;
   orien_temp = *orien;
   back_col_temp = *back_col;
   pix_flag_temp = *pix_flag;
#else
   xpixels_temp = xpixels[0];
   ypixels_temp = ypixels[0];
   orien_temp = orien[0];
   back_col_temp = back_col[0];
   pix_flag_temp = pix_flag[0];
#endif
   if (configure_flag == 1) {    /* graphics window re-configured by user */
      /* Get actual height and width set by the window manager */
      XGetWindowAttributes(display, window, &returned_attributes);
      width = returned_attributes.width;
      height = returned_attributes.height;
#if INTEGER_PRECISION == 0
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
      set_screen(xpixels_temp, ypixels_temp, orien_temp);
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
#if INTEGER_PRECISION == 0
      *ixret = width;
      *iyret = height;
#else
      ixret[0] = width;
      iyret[0] = height;
#endif

   }
   else {
#if INTEGER_PRECISION == 0
     *ixret = width;
     *iyret = height;
#else
     ixret[0] = width;
     iyret[0] = height;
#endif
   }
   configure_flag = 0;

   switch (pix_flag_temp) {
      case 0:                         /* user does not want pixmap */
          if (PIXMAP_CURRENT != 0) {  /* get rid of current pixmap */
            PIXMAP_CURRENT = 0;
            XFreePixmap(display, pixmap);
          }
          break;
      case 1:                          /* user wants a pixmap */
          if (PIXMAP_CURRENT != 0) {   /* pixmap already exists */
            XFreePixmap(display, pixmap);
          }
          else {                      /* pixmap does not yet exist */
            PIXMAP_CURRENT = 1;
          }
          pixmap = XCreatePixmap(display, window, width, height, depth);
          break;
      default:
          break;
   }

   xback(back_col_temp);                         /* set background color */
   XClearWindow(display, window);            /* clear the window */
   temp_color = BACKGROUND_CURRENT;
   temp_color_2 = COLOR_CURRENT;
   BACKGROUND_CURRENT = -1;
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
   xfore_(&temp_color);
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
   XFORE_(&temp_color);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
   xfore(&temp_color);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
   XFORE(&temp_color);
#endif

   XFillRectangle(display, window, gc, 0, 0, width, height);
/* January 1992.  Follwoing 3 lines added.  */
   if (PIXMAP_CURRENT == 1) {                /* set pixmap to background */
     XFillRectangle(display, pixmap, gc, 0, 0, width, height);
   }
/* End change.  */
   BACKGROUND_CURRENT = temp_color;
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
   xfore_(&temp_color_2);
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
   XFORE_(&temp_color_2);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
   xfore(&temp_color_2);
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
   XFORE(&temp_color_2);
#endif
/* January 1992.  Following 5 lines commented out */
/* if (PIXMAP_CURRENT == 1) {   */             /* set pixmap to background */
/*   XSetForeground(display, gc, colors[BACKGROUND_CURRENT].pixel); */
/*   XFillRectangle(display, pixmap, gc, 0, 0, width, height);      */
/*   XSetForeground(display, gc, colors[COLOR_CURRENT].pixel);      */
/* }  */
/* End change.  */

   /*  July 1995.  Set flag that xerase routine has been called.
    *  needed by front-end (so won't copy background pixmap before
    *  any graphs have been drawn.
    */

    GRAPH_FLAG = 1;
}

/* XUPDAT - routine to copy pixmap to screen
 *
 * If an expose event has been detected, update the graphics
 * window with the pixmap (if calling program has requested it).
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xupdat_(frontend)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XUPDAT_(frontend)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xupdat(frontend)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XUPDAT(frontend)
#endif
#if INTEGER_PRECISION == 0
int     *frontend;
#else
int     frontend[2];
#endif
{
/*  July 1995.  Add frontend argument.  If frontend active,
 *  automatically update graphics window regardless of expose_flag.
 */

#if INTEGER_PRECISION == 0
   if (*frontend == 0) {
#else
   if (frontend[0] == 0) {
#endif
     if (expose_flag == 1 &&         /* update screen  if expose event */
         PIXMAP_CURRENT == 1) {      /* copy pixmap to screen to update */
         XCopyArea(display, pixmap, window, gc, 0, 0, width, height, 0, 0);
         XFlush(display);           /* do copy immediately */
         expose_flag = 0;
     }
   }

#if INTEGER_PRECISION == 0
   if (*frontend == 1) {
#else
   if (frontend[0] == 1) {
#endif
     if (PIXMAP_CURRENT == 1 &&
         GRAPH_FLAG == 1) {      /* copy pixmap to screen to update */
         XCopyArea(display, pixmap, window, gc, 0, 0, width, height, 0, 0);
         XFlush(display);           /* do copy immediately */
     }
   }

#if INTEGER_PRECISION == 0
   if (*frontend == 0) {
#else
   if (frontend[0] == 0) {
#endif
     if (expose_flag_aux == 1 &&       /* update second screen  if expose event */
         PIXMAP_CURRENT_2 == 1) {    /* copy pixmap to screen to update */
         XCopyArea(display, pixmap2, window2, gc2, 0, 0, width2, height2, 0, 0);
         XFlush(display);           /* do copy immediately */
         expose_flag_aux = 0;
     }
   }

}

/* XCLEAR  - routine to flush the buffer
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xclear_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCLEAR_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xclear()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCLEAR()
#endif
{
   XFlush(display);
}

/* XSAVEG - routine to copy pixmap to a specified file
 *
 * Save the current pixmap to the specified file.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xsaveg_(file_name, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XSAVEG_(file_name, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xsaveg(file_name, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XSAVEG(file_name, error_flag)
#endif
#if INTEGER_PRECISION == 0
int     *error_flag;
int     file_name[];
#else
int     *error_flag[2];
int     file_name[];
#endif
{
#if INTEGER_PRECISION == 0
#define MAX_STRING_LEN  128
#else
#define MAX_STRING_LEN  256
#endif
   int      len, istatus;
   char     file_name_string[MAX_STRING_LEN];

   if ( OPEN_FLAG == 0 ){
#if INTEGER_PRECISION == 0
     *error_flag = 3;
#else
     error_flag[0] = 3;
#endif
     return;
   }

   if ( PIXMAP_CURRENT == 0 ){
#if INTEGER_PRECISION == 0
     *error_flag = 2;
#else
     error_flag[0] = 2;
#endif
     return;
   }

   strcpy(file_name_string," ");
   i_to_s(file_name, file_name_string, MAX_STRING_LEN, &len);

   istatus = XWriteBitmapFile(display, file_name_string, pixmap, width, height, -1, -1);
   XFlush(display);           /* do copy immediately */
#if INTEGER_PRECISION == 0
   if (istatus != BitmapSuccess) *error_flag = 2;
#else
   if (istatus != BitmapSuccess) error_flag[0] = 2;
#endif

}

/* XRESTG - routine to copy pixmap from a specified file
 *          The pixmap will be restored to a special window
 *
 * Save the current pixmap to the specified file.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xrestg_(file_name, title, iwind, iwindn, error_flag)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XRESTG_(file_name, title, iwind, iwindn, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xrestg(file_name, title, iwind, iwindn, error_flag)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XRESTG(file_name, title, iwind, iwindn, error_flag)
#endif
#if INTEGER_PRECISION == 0
int     *error_flag;
int     file_name[];
int     iwind[], iwindn[2], title[];
#else
int     *error_flag[2];
int     file_name[];
int     iwind[], iwindn[2];
int     title[];
#endif

{

#if INTEGER_PRECISION == 0
#define MAX_STRING_LEN  128
#else
#define MAX_STRING_LEN  256
#endif
   XTextProperty  WindowName, IconName;
   XWindowAttributes     window_attributes_tmp;
   int      len, istatus;
   int      x_hot, y_hot, iwindn_temp;
   unsigned int  width3, height3;
   char     file_name_string[MAX_STRING_LEN];
/* char     *title_string_2; */
   char     title_string[MAX_STRING_LEN+10];
   static   XSizeHints  xsh2;
   char     *window_name = "1- Pixmap";
   long         itemp;
   int          counter;
   double       atemp1;
   int          window_flag;
   FILE          *ijunk;
#if INTEGER_PRECISION == 1
   int          iplace;
#endif

#if INTEGER_PRECISION == 0
  iwindn_temp = *iwindn;
#else
  iwindn_temp = iwindn[0];
#endif
   if ( OPEN_FLAG == 0 ){
#if INTEGER_PRECISION == 0
     *error_flag = 3;
#else
     error_flag[0] = 3;
#endif
     return;
   }

   strcpy(file_name_string," ");
   i_to_s(file_name, file_name_string, MAX_STRING_LEN, &len);
   strcpy(title_string," ");
   i_to_s(title, title_string, MAX_STRING_LEN+10, &len);

   if (PIXMAP_CURRENT_2 != 0) {    /* Clear current pixmap */
     PIXMAP_CURRENT_2 = 0;
     XFreePixmap(display, pixmap2);
   }

   istatus = XReadBitmapFile(display, window, file_name_string,
             &width2, &height2, &pixmap3, &x_hot, &y_hot);
   XFlush(display);           /* do copy immediately */
#if INTEGER_PRECISION == 0
   if (istatus != BitmapSuccess) {
      *error_flag = 2;
      return;
   }
#else
   if (istatus != BitmapSuccess) {
      error_flag[0] = 2;
      return;
   }
#endif

   pixmap2 = XCreatePixmap(display, window, width, height, depth);
   PIXMAP_CURRENT_2 = 1;
   /*  Now open (or use currently open window) to display the
    *  successfully read pixmap */
   if ( OPEN_FLAG_2 == 0 ){
      OPEN_FLAG_2 = 1;

      window_flag = 0;
      if ( iwindn_temp > 0 ) {
        window_flag = 1;
#if INTEGER_PRECISION == 0
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

        window2 = XCreateSimpleWindow(      /* Open graphics window */
                display,                    /* Pointer to Display structure */
                DefaultRootWindow(display), /* Parent for new window */
                xsh2.x,                     /* X position of window */
                xsh2.y,                     /* Y position of window */
                xsh2.width,                 /* Width of window */
                xsh2.height,                /* Height of window */
                BORDER_WIDTH,               /* Pixel width of border */
                black,                      /* Border color */
                white);                     /* Background color */

        value_mask = CWBitGravity;
        XChangeWindowAttributes(display, window2, value_mask,
                 &window_attributes);

#ifdef X11R3  /* Use following for X11 Release 3 and earlier */
        XSetStandardProperties(display, window2, title_string, title_string,
                               None, argv, argc, &xsh);
        XSetWMHints(display, window2, &xwmh);
        XSetClassHint(display, window2, &xch);
#else  /* X11R4 or later */
    
/*     *title_string_2 = &title_string;  */
       if (XStringListToTextProperty(&window_name, 1, &WindowName) == 0) {
#if PRECISION == 0
          *error_flag = 1;
#else
          error_flag[0] = 1;
#endif
          return;
       }
       XSetWMProperties(display, window2, &WindowName, &WindowName, 
                        argv, argc, &xsh2, &xwmh, &xch);
#endif

       XSelectInput(display, window2,
           StructureNotifyMask | 
           ExposureMask );     
       XMapWindow(display, window2); /* Map the window (make visible) */
       XFlush(display);              /* Flush the buffer */
       do {
            XNextEvent(display, &event);
          } while (event.type != MapNotify || event.xmap.window != window2);
     }

   }
   /* If window exists, only need to check if the current width and
    * height are appropriate.  */

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
         XSetStandardProperties(display, window2, title_string, title_string,
                              None, argv, argc, &xsh2);
   }

   /* Following code is same for either a new window or a previously
    * created window.  */
   gc2 = XCreateGC(display, window2, 0, &gcvalues);/* Create graphics context */
   XSetState(display, gc2, black, white, GXcopy, AllPlanes);
   XSetForeground(display,gc2,BlackPixel(display,DefaultScreen(display)));
   XSetBackground(display,gc2,WhitePixel(display,DefaultScreen(display)));
   XSetGraphicsExposures(display,gc2,False);
   XCopyPlane(display,pixmap3,pixmap2,gc2,0,0,width2,height2,0,0,1);
   XFreePixmap(display,pixmap3);
   XCopyArea(display, pixmap2, window2, gc2, 0, 0, width2, height2, 0, 0);
   XFlush(display);
   expose_flag_aux = 0;

}

/* XCYCLE - routine to cycle through previously created pixmaps
 *          The button pressed is stored in ibutton
 *          (1=left, 2=middle, 3=right)
 *          This routine only processes a SINGLE button event.  The
 *          calling routine reads ibutton and either redraws another
 *          pixmap or exits.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xcycle_(error_flag, ibutton)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCYCLE_(error_flag, ibutton)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xcycle(error_flag, ibutton)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCYCLE(error_flag, ibuttton)
#endif
#if INTEGER_PRECISION == 0
int     *error_flag;
int     *ibutton;
#else
int     *error_flag[2];
int     *ibutton[2];
#endif
{
     XEvent   event;        /* holds Xserver events */
     int      x, y;         /* the last X pointer position */
     int      new_x, new_y; /* a new X pointer position */
     unsigned int dummy;    /* placeholder for unwanted return value */
     int      done;

   if ( OPEN_FLAG == 0 ){
#if INTEGER_PRECISION == 0
     *error_flag = 3;
#else
     error_flag[0] = 3;
#endif
     return;
   }

   if ( PIXMAP_CURRENT == 0 ){
#if INTEGER_PRECISION == 0
     *error_flag = 2;
#else
     error_flag[0] = 2;
#endif
     return;
   }

     XSelectInput(display, window2,    /* Types of graphic input to accept */
       StructureNotifyMask |          /* Window notification events */
       ExposureMask |                 /* Expose events */
       ButtonPressMask |              /* Button press */
       ButtonMotionMask |             /* Pointer moves with button pressed */
       PointerMotionMask);            /* Pointer moves in window */

       done = 0;
       while (done == 0) {         /* loop until button press event */

          XNextEvent(display, &event);   /* get next event from queue */
          switch (event.type) {
          case DestroyNotify:            /* window has been destroyed */
              XFreeGC(display, gc2);
              XDestroyWindow(display,window2);
#if INTEGER_PRECISION == 0
              *error_flag = 1;
#else
              error_flag[0] = 1;
#endif
              done = 1;
              break;
          case Expose:                   /* expose event */
              if(event.xexpose.count == 0) {
                expose_flag_aux = 1;
              }
              break;
          case ConfigureNotify:          /* window has been reconfigured */
              break;
          case MotionNotify:             /* pointer motion */
          /*  XQueryPointer(display, window2,
                            &dummy, &dummy,
                            &dummy, &dummy,
                            &new_x, &new_y,
                            &mask);
              x = new_x;
              y = new_y;    */
              break;
          case ButtonPress:     /* Button event */
#if INTEGER_PRECISION == 0
              *ibutton = event.xbutton.button;
        /*    *ixret = event.xbutton.x;
              *iyret = event.xbutton.y;   */
#else
              ibutton[0] = event.xbutton.button;
       /*     ixret[0] = event.xbutton.x;
              iyret[0] = event.xbutton.y; */
#endif
              done = 1;
              break;
          default:
              break;
          }       /* end switch */
       }          /* end while */

       XSelectInput(display, window2,    /* Reset default event selection */
         StructureNotifyMask |          /* Window notification events */
         ExposureMask);                 /* Expose events */


}
/* XEND   - routine to end X11.  Close the display.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xend_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XEND_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xend()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XEND()
#endif

{
     if (OPEN_FLAG_2 != 0) {
        XFreeGC(display,gc2);
        XDestroyWindow(display,window2);
     }
     OPEN_FLAG_2 = 0;

     if (OPEN_FLAG != 0) {
        XFreeGC(display,gc);
        XDestroyWindow(display,window);
        XCloseDisplay(display);
     }
     OPEN_FLAG = 0;

}

/* XFORE  - routine to set the foreground color.  Specify the color as
 *          an index.
 *       INDEX   COLOR_TYPE = 0        COLOR_TYPE = 1
 *       =====   ==============        ==============
 *           0 - black                 black
 *           1 - white                 white
 *           2 - green                 green
 *           3 - yellow                yellow
 *           4 - red                   red
 *           5 - blue                  blue
 *           6 - magenta               magenta
 *           7 - cyan                  cyan
 *           8 - yellow orange         orange
 *           9 - yellow green          yellow green
 *          10 - blue green            dark green
 *          11 - green blue            light blue
 *          12 - blue violet           blue violet
 *          13 - red violet            violet red
 *          14 - dark grey             dark slate gray
 *          15 - light grey            light gray
 *          16 -                       aquamarine
 *          17 -                       brown
 *          18 -                       cadet blue
 *          19 -                       coral
 *          20 -                       cornflower blue
 *          21 -                       dark olive green
 *          22 -                       dark orchid
 *          23 -                       dark slate blue
 *          24 -                       dark turquoise
 *          25 -                       firebrick       
 *          26 -                       forest green 
 *          27 -                       gold
 *          28 -                       goldenrod
 *          29 -                       gray
 *          30 -                       indian red 
 *          31 -                       khaki
 *          32 -                       dim gray   
 *          33 -                       light blue steel 
 *          34 -                       lime green
 *          35 -                       maroon
 *          36 -                       medium aquamarine 
 *          37 -                       medium blue
 *          38 -                       medium forest green
 *          39 -                       medium goldenrod
 *          40 -                       medium orchid
 *          41 -                       medium sea green
 *          42 -                       medium slate blue
 *          43 -                       medium spring green
 *          44 -                       medium turquoise
 *          45 -                       medium violet red
 *          46 -                       midnight blue
 *          47 -                       navy
 *          48 -                       orange red
 *          49 -                       orchid
 *          50 -                       pale green 
 *          51 -                       pink 
 *          52 -                       plum
 *          53 -                       purple
 *          54 -                       salmon
 *          55 -                       sea green 
 *          56 -                       sienna
 *          57 -                       sky blue 
 *          58 -                       slate blue
 *          59 -                       spring green
 *          60 -                       steel blue 
 *          61 -                       tan  
 *          62 -                       thistle    
 *          63 -                       turquoise
 *          64 -                       violet
 *          65 -                       wheat 
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xfore_(index)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XFORE_(index)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xfore(index)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XFORE(index)
#endif
#if INTEGER_PRECISION == 0
int   *index;
#else
int   index[2];
#endif
{
      unsigned long  temp;
      int            temp2;
      int            index_temp;

#if INTEGER_PRECISION == 0
      index_temp = *index;
#else
      index_temp = index[0];
#endif
      if (index_temp == COLOR_CURRENT) return;
      COLOR_CURRENT = index_temp;

      if (index_temp == BACKGROUND_CURRENT) {    /* test if equal background */
        if (index_temp == 0) {                   /* set to white */
          COLOR_CURRENT = 1;
          XSetForeground(display, gc, white);
        }
        else {                               /* set to black */
          COLOR_CURRENT = 0;
          XSetForeground(display, gc, black);
        }
        return;
      }

      if (index_temp < 0) {       /* Gray scale requested */
        temp2 = index_temp;
        temp2 = -temp2;
        if (temp2 > MAX_GRAY) temp2 = MAX_GRAY;
        if (color_list_gray[temp2] == 0) {   /* Color unavailable */
           if (BACKGROUND_CURRENT != 0) {  /* set to black */
             temp2 = 0;
           }
           else {                          /* set to white */
             temp2 = 1;
           }
           COLOR_CURRENT = temp2;
           XSetForeground(display, gc, colors[temp2].pixel);
        }
        else {
           XSetForeground(display, gc, grays[temp2].pixel);
        }
        return;
      } 

      if (color_flag < 2) {   /* monochrome devices */
        switch (index_temp) {
           case 0:       /* black foreground color */
               temp = black;
               COLOR_CURRENT = 0;
               break;
           case 1:       /* white foreground color */
               temp = white;
               COLOR_CURRENT = 1;
               break;
           default:      /* default, set to black */
               temp = black;
               COLOR_CURRENT = 0;
               break;
         }
         XSetForeground(display, gc, temp);
       }
       else {          /* color devices */
         temp2 = index_temp;
         if (temp2 > max_colors - 1) {    /* index out of range */
           if (BACKGROUND_CURRENT != 0) {  /* set to black */
             temp2 = 0;
           }
           else {                          /* set to white */
             temp2 = 1;
           }
         }
         else if (color_list[temp2] == 0) { /* color unavailable */
           if (BACKGROUND_CURRENT != 0) {  /* set to black */
             temp2 = 0;
           }
           else {                          /* set to white */
             temp2 = 1;
           }
         }
         COLOR_CURRENT = temp2;
         XSetForeground(display, gc, colors[temp2].pixel);
       }

}


/* xback  - routine to set the background color.  Specify the color as
 *          an index.  See comments for xfore routine above for the 
 *          index to color mapping.
 *
 * Note: the background color only applies in the following 4 cases:
 *          XDrawImageString
 *          XCopyPlane
 *          drawing lines with line style LineDoubleDash
 *          filling with fill style of FillOpaqueStippled
 *       Although Dataplot does not currently use any of these features,
 *       the background value will be set (in case any of these are
 *       implemented at a future date).  However, the background will
 *       be set by doing a solid fill rectangle of the entire screen
 *       (setting the foreground color to the background color).
 *
 */
void xback(index)
int  index;
{

      unsigned long temp;
      int           temp2;

      if (index == BACKGROUND_CURRENT) return;

      if (color_flag < 2) {   /* monochrome devices */
        temp2 = index;
        if (temp2 < 0 || temp2 > 1) temp2 = 2;
        switch (temp2) {
           case 0:       /* black background color */
               temp = black;
               BACKGROUND_CURRENT = 0;
               break;
           case 1:       /* white background color */
               temp = white;
               BACKGROUND_CURRENT = 1;
               break;
           case 2:
               temp = white;
               BACKGROUND_CURRENT = 1;
               break;
           default:      /* default, set to white */
               temp = white;
               BACKGROUND_CURRENT = 1;
               break;
         }
         XSetBackground(display, gc, temp);
       }
       else {          /* color devices */
         temp2 = index;
         if (temp2 < 0) temp2 = 1;
         if (temp2 > max_colors - 1) {    /* index out of range */ 
           if (BACKGROUND_CURRENT != 1) {  /* set to white */
             temp2 = 1;
             BACKGROUND_CURRENT = 1;
           }
           else {                          /* set to black */
             temp2 = 0;
             BACKGROUND_CURRENT = 0;
           }
         }
         else if (color_list[temp2] == 0) { /* color unavailable */
           if (BACKGROUND_CURRENT != 1) {  /* set to white */
             temp2 = 1;
             BACKGROUND_CURRENT = 1;
           }
           else {                          /* set to black */
             temp2 = 0;
             BACKGROUND_CURRENT = 0;
           }
         }
         BACKGROUND_CURRENT = temp2;
         XSetBackground(display, gc, colors[temp2].pixel);
       }
}

/* XLATTR  - set line attributes.  Note that the attribute will only be
 *           set if it is being changed (i.e., test against current
 *           value of the attribute).
 *
 * index - parameter that sets the value
 * icode - identify which attribute to set
 *         1 - set the line width in pixels
 *         2 - set the line style (i.e., solid or dash).  Currently only
 *             three dash patterns are supported.  However, additional ones
 *             may be added.
 *             0 - solid line
 *             1 - dash line
 *             2 - dotted line
 *             3 - dot-dash line (dash2 pattern)
 *             4 - dash3
 *             5 - dash4
 *         3 - set the line cap
 *             0 - cap butt (end-caps squared off at endoints perpindicular
 *                 to the slope of the line)
 *             1-  cap not last (similar to cap butt, but for 1 pixel wide
 *                 line, do not draw last point of line)
 *             2 - cap round (end-caps are circles with diameter equal to
 *                 line width)
 *             3 - cap projecting (end caps are squared off similar to cap
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
 *
 */
#define MAX_WIDTH  15
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xlattr_(index, icode)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XLATTR_(index, icode)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xlattr(index, icode)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XLATTR(index, icode)
#endif
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
static char    dot [] = {1,2};
static char    dash [] = {6,3};
static char    dash3 [] = {3,3};
static char    dash4 [] = {2,4};
static char    dash_dot [] = {9,3,3,3};

#if INTEGER_PRECISION == 0
   icode_temp = *icode;
   index_temp = *index;
#else
   icode_temp = icode[0];
   index_temp = index[0];
#endif
   switch (icode_temp) {
      case 1:         /* set the line width */
          if (index_temp == WIDTH_CURRENT) break;
          valuemask = GCLineWidth;
          if (index_temp < 2) {
             gcvalues.line_width = 0;
          }
          else {
             width_temp = index_temp;
             if (width_temp > MAX_WIDTH) width_temp = MAX_WIDTH;
             gcvalues.line_width = width_temp;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          WIDTH_CURRENT = index_temp;
          break;
      case 2:         /* set the line style */
          if (index_temp == LINE_STYLE_CURRENT) break;
          valuemask = GCLineStyle;
          dash_offset = 0;
          temp = index_temp;
          if (temp > 3) temp = 3;
          switch (temp) {    /* index determines the style */
             case 0:         /* solid line */
                 gcvalues.line_style = LineSolid;
                 break;
             case 1:         /* dashed line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash, 2);
                 break;
             case 2:         /* dotted line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dot, 2);
                 break;
             case 3:         /* dash-dot line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash_dot, 4);
                 break;
             case 4:         /* dash3 */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash3, 2);
                 break;
             case 5:         /* dash4 line */
                 gcvalues.line_style = LineOnOffDash;
                 XSetDashes(display, gc, dash_offset, dash4, 2);
                 break;
              default:
                 gcvalues.line_style = LineSolid;
                 break;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          LINE_STYLE_CURRENT = index_temp;
          break;
      case 3:         /* set the line cap style */
          if (index_temp == CAP_STYLE_CURRENT) break;
          valuemask = GCCapStyle;
          switch (index_temp) {  /* index determines the style */
             case 0:         /* cap butt */
                 gcvalues.cap_style = CapButt;
                 break;
             case 1:         /* cap round */
                 gcvalues.cap_style = CapRound;
                 break;
             case 2:         /* cap not last */
                 gcvalues.cap_style = CapNotLast;
                 break;
             case 3:         /* cap projecting */
                 gcvalues.cap_style = CapProjecting;
                 break;
             default:
                 gcvalues.cap_style = CapButt;
                 break;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          CAP_STYLE_CURRENT = index_temp;
          break;
      case 4:         /* set the join style */
          if (index_temp == JOIN_STYLE_CURRENT) break;
          valuemask = GCJoinStyle;
          switch (index_temp) {  /* index determines the style */
             case 0:         /* miter join */
                 gcvalues.join_style = JoinMiter;
                 break;
             case 1:         /* round join */
                 gcvalues.join_style = JoinRound;
                 break;
             case 2:         /* bevel join */
                 gcvalues.join_style = JoinBevel;
                 break;
             default:
                 gcvalues.join_style = JoinMiter;
                 break;
          }
          XChangeGC(display, gc, valuemask, &gcvalues);
          JOIN_STYLE_CURRENT = index_temp;
          break;
      default:
          break;
   }
}

/* XDRAW  - draw a polyline.  The attributes of the line have been
 *          previously set (by the XLATTR routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points to plot
 *
 */
#define MAX_LINE_POINTS  500
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xdraw_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XDRAW_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xdraw(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XDRAW(xpts, ypts, npts)
#endif
int   xpts[], ypts[];
#if INTEGER_PRECISION == 0
int   *npts;
#else
int   npts[2];
#endif
{
   XPoint  points[MAX_LINE_POINTS];
   int     npts_temp;
#if INTEGER_PRECISION == 0
   npts_temp = *npts;
#else
   npts_temp = npts[0];
#endif

   if (npts_temp == 2) {      /* draw 2 points */
      int  x1, y1, x2, y2;
#if INTEGER_PRECISION == 0
      x1 = xpts[0];
      x2 = xpts[1];
      y1 = ypts[0];
      y2 = ypts[1];
#else
      x1 = xpts[0];
      x2 = xpts[2];
      y1 = ypts[0];
      y2 = ypts[2];
#endif
      XDrawLine(display, window, gc, x1, y1, x2, y2);
      if (PIXMAP_CURRENT == 1) {
        XDrawLine(display, pixmap, gc, x1, y1, x2, y2);
      }
      return;
   }
   else if (npts_temp > 2) {     /* more than 2 points */
#if INTEGER_PRECISION == 0
     int  i, ilower, iupper, nloops, temp, itimes;
     iupper = 0;
     nloops = 0;
     while (nloops != 1) {  /* do MAX_LINE_POINTS per loop */
        ilower = iupper;
        iupper = ilower + MAX_LINE_POINTS - 1;
        if (iupper > npts_temp) {
          iupper = npts_temp;
          nloops = 1;
        }
        temp = 0;
        itimes = iupper - ilower;
        for (i = 0; i < itimes; i++) {
           points[i].x = xpts[i + ilower];
           points[i].y = ypts[i + ilower];
           temp = temp + 1;
        }
        XDrawLines(display, window, gc, points, temp, CoordModeOrigin);
        if (PIXMAP_CURRENT == 1) {
          XDrawLines(display, pixmap, gc, points, temp, CoordModeOrigin);
        }
     }  /* end while loop */
   }    /* end of npts loop */
#else
     int  i, ilower, iupper, nloops, temp, itimes;
     iupper = 0;
     nloops = 0;
     while (nloops != 1) {  /* do MAX_LINE_POINTS per loop */
        ilower = iupper;
        iupper = ilower + MAX_LINE_POINTS - 1;
        if (iupper > npts_temp) {
          iupper = npts_temp;
          nloops = 1;
        }
        temp = 0;
        itimes = iupper - ilower;
        for (i = 0; i < itimes; i++) {
           points[i].x = xpts[2*(i + ilower)];
           points[i].y = ypts[2*(i + ilower)];
           temp = temp + 1;
        }
        XDrawLines(display, window, gc, points, temp, CoordModeOrigin);
        if (PIXMAP_CURRENT == 1) {
          XDrawLines(display, pixmap, gc, points, temp, CoordModeOrigin);
        }
     }  /* end while loop */
   }    /* end of npts loop */
#endif

}

/* XPOINT - draw a point.
 *          previously set (by the XLATTR routine).
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xpoint_(ix, iy)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XPOINT_(ix, iy)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xpoint(ix, iy)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XPOINT(ix, iy)
#endif
#if INTEGER_PRECISION == 0
int   *ix, *iy;
#else
int   ix[2], iy[2];
#endif
{
#if INTEGER_PRECISION == 0
   XDrawPoint(display, window, gc, *ix, *iy);
   if (PIXMAP_CURRENT == 1) {
     XDrawPoint(display, pixmap, gc, *ix, *iy);
#else
   XDrawPoint(display, window, gc, ix[0], iy[0]);
   if (PIXMAP_CURRENT == 1) {
     XDrawPoint(display, pixmap, gc, ix[0], iy[0]);
#endif
   }

}

/* XCIRC  - draw a circle.  Note that the circle may be either filled or 
 *          unfilled.  For a filled circle, do twice, (once for the 
 *          outline, once to fill the interior).
 *
 * ix     - contains the x coordinate for the center of the circle
 * iy     - contains the y coordinate for the center of the circle 
 * irad   - radius
 * ifill  - 0 for unfilled circle, 1 for filled circle
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xcirc_(ix, iy, irad, ifill)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XCIRC_(ix, iy, irad, ifill)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xcirc(ix, iy, irad, ifill)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XCIRC(ix, iy, irad, ifill)
#endif
#if INTEGER_PRECISION == 0
int   *ix, *iy, *irad, *ifill;
#else
int   ix[2], iy[2], irad[2], ifill[2];
#endif
{

   int   xpos, ypos, iwidth, iheight, iang1, iang2, ir;

   iang1 = 0;
   iang2 = 23040;
#if INTEGER_PRECISION == 0
   ir = *irad;
   xpos = *ix - *irad;
   ypos = *iy - *irad;
#else
   ir = irad[0];
   xpos = ix[0] - irad[0];
   ypos = iy[0] - irad[0];
#endif
   iwidth = 2 * ir;
   iheight = 2 * ir;
   XDrawArc(display, window, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
   if (PIXMAP_CURRENT == 1) {
     XDrawArc(display, pixmap, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
   }
#if INTEGER_PRECISION == 0
   if (*ifill != 0) {
#else
   if (ifill[0] != 0) {
#endif
     XFillArc(display, window, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
     if (PIXMAP_CURRENT == 1) {
       XDrawArc(display, pixmap, gc, xpos, ypos, iwidth, iheight, iang1, iang2);
     }
   }

}

/* XREGFL - fill a region.  Rectangular regions will be filled differently
 *          non-rectangular regions.  Dataplot only handles convex polygons,
 *          so set this (for faster performance).  This routine only does
 *          solid fills.  Hatch patterns must be drawn
 *          by the calling program (i.e., send the individual lines to
 *          the XDRAW routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points in the polygon (if 2, assume a rectangle,
 *          otherwise, a convex polygon)
 *
 */
#define MAX_REG_POINTS  1000
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xregfl_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XREGFL_(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xregfl(xpts, ypts, npts)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XREGFL(xpts, ypts, npts)
#endif
int   xpts[], ypts[];
#if INTEGER_PRECISION == 0
int   *npts;
#else
int   npts[2];
#endif
{
   XPoint  points[MAX_REG_POINTS];
   int     npts_temp;

#if INTEGER_PRECISION == 0
   npts_temp = *npts;
#else
   npts_temp = npts[0];
#endif
   if (npts_temp == 2) {      /* rectangle */
      int  x1, y1, x2, y2, rect_height, rect_width;
      if (xpts[0] <= xpts[1]) {
#if INTEGER_PRECISION == 0
        x1 = xpts[0];
        x2 = xpts[1];
#else
        x1 = xpts[0];
        x2 = xpts[2];
#endif
        rect_width = x2 - x1 + 1;
      }
      else {
#if INTEGER_PRECISION == 0
        x1 = xpts[1];
        x2 = xpts[0];
#else
        x1 = xpts[2];
        x2 = xpts[0];
#endif
        rect_width = x2 - x1 + 1;
      }
      if (ypts[0] <= ypts[1]) {
#if INTEGER_PRECISION == 0
        y1 = ypts[0];
        y2 = ypts[1];
#else
        y1 = ypts[0];
        y2 = ypts[2];
#endif
        rect_height = y2 - y1 + 1;
      }
      else {
#if INTEGER_PRECISION == 0
        y1 = ypts[1];
        y2 = ypts[0];
#else
        y1 = ypts[2];
        y2 = ypts[0];
#endif
        rect_height = y2 - y1 + 1;
      }
      XFillRectangle(display, window, gc, x1, y1, rect_width, rect_height);
      if (PIXMAP_CURRENT == 1) {
        XFillRectangle(display, pixmap, gc, x1, y1, rect_width, rect_height);
      }
   }
   else if (npts_temp > 2) {     /* convex polygon */
      int  i, temp;
      temp = npts_temp;
      if( npts_temp > MAX_REG_POINTS) temp = MAX_REG_POINTS;
      for (i = 0; i < temp; i++) {
#if INTEGER_PRECISION == 0
         points[i].x = xpts[i];
         points[i].y = ypts[i];
#else
         points[i].x = xpts[2*i];
         points[i].y = ypts[2*i];
#endif
      }
      /* June 1994.  Use Complex instead of Convex (for possibly
         self-intersecting polygons.  X11 has problem with
         PLOT SIN(X) FOR X = 0 0.1 22 (i.e., more than 1 cycle).  
         The choice of fill rule does not seem to make a difference, 
         so leave as default.  Added a SET X11 HARDWARE FILL ON/OFF. */
   /* XSetFillRule(display,gc,EvenOddRule);  */
   /* XSetFillRule(display,gc,WindingRule);  */
      XFillPolygon(display,window,gc,points,temp,Convex,CoordModeOrigin);
      if (PIXMAP_CURRENT == 1) {
        XFillPolygon(display,pixmap,gc,points,temp,Convex,CoordModeOrigin);
      }
   }

}
/* XTATTR - set the font for drawing text strings.  Note that the fonts
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
void xtattr_(font, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XTATTR_(font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xtattr(font, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XTATTR(font, error)
#endif
int    font[];
#if INTEGER_PRECISION == 0
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
   XCharStruct  overall;

   strcpy(font_name," ");
#if INTEGER_PRECISION == 0
   i_to_s(font, font_name, 80, &len);
#else
   i_to_s(font, font_name, 160, &len);
#endif

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
#if INTEGER_PRECISION == 0
         *error = 2;
#else
         error[0] = 2;
#endif
       }
       else {                              /* leave current font */
         strcpy(font_name, FONT_NAME_CURRENT);
#if INTEGER_PRECISION == 0
         *error = 1;
#else
         error[0] = 1;
#endif
       }
       font_name_2 = &font_name[0];
       font_struct = XLoadQueryFont(display, font_name_2);
     }
     strcpy(FONT_NAME_CURRENT, font_name);
     XSetFont(display, gc, font_struct->fid);  /* Set the font */
     XTextExtents(font_struct, " ", 0, &direction_hint, &font_ascent,
          &font_descent, &overall);
     font_gap = (font_ascent + font_descent) * .20;
     FONT_HEIGHT_CURRENT = font_ascent + font_descent;
     FONT_GAP_CURRENT = font_gap;
   }
}

/* XTEXTH - draw a horizontal text string.
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
void xtexth_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XTEXTH_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xtexth(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XTEXTH(string, ixpos, iypos, ijusth, ijustv, error)
#endif
int    string[];
#if INTEGER_PRECISION == 0
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

#if INTEGER_PRECISION == 0
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

#if INTEGER_PRECISION == 0
   i_to_s(string, string2, 130, &len);
#else
   i_to_s(string, string2, 260, &len);
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


    XDrawString(display, window, gc, itempx, itempy, string2, len);
    if (PIXMAP_CURRENT == 1) {
      XDrawString(display, pixmap, gc, itempx, itempy, string2, len);
    }

}

/* XTEXTV - draw a horizontal text string.
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
void xtextv_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XTEXTV_(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xtextv(string, ixpos, iypos, ijusth, ijustv, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XTEXTV(string, ixpos, iypos, ijusth, ijustv, error)
#endif
int    string[];
#if INTEGER_PRECISION == 0
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
#if INTEGER_PRECISION == 0
   int          string3[2];              /* one character at a time */
#else
   int          string3[4];              /* one character at a time */
#endif
   int          ixpos_temp, iypos_temp, ijusth_temp, ijustv_temp;

#if INTEGER_PRECISION == 0
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


#if INTEGER_PRECISION == 0
   i_to_s(string, string2, 130, &len);
#else
   i_to_s(string, string2, 260, &len);
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

#if INTEGER_PRECISION == 0
   string3[1] = 0;
   for (i = 0; i < len; i++) {  /* plot each character one at a time */
      string3[0] = string[i];
      i_to_s(string3,string2, 2, &ijunk);
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

     itempx = ixpos_temp - itempx;
     XDrawString(display, window, gc, itempx, itempy, string2, 1);
     if (PIXMAP_CURRENT) {
       XDrawString(display, pixmap, gc, itempx, itempy, string2, 1);
     }
     itempy = itempy + (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);

   }
#else
   string3[1] = 0;
   string3[2] = 0;
   string3[3] = 0;
   for (i = 0; i < len; i++) {  /* plot each character one at a time */
      string3[0] = string[2*i];
      i_to_s(string3,string2, 4, &ijunk);
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

     itempx = ixpos_temp - itempx;
     XDrawString(display, window, gc, itempx, itempy, string2, 1);
     if (PIXMAP_CURRENT) {
       XDrawString(display, pixmap, gc, itempx, itempy, string2, 1);
     }
     itempy = itempy + (FONT_HEIGHT_CURRENT + FONT_GAP_CURRENT);

   }
#endif

}

/* XFETCH  - routine to retrieve contents of clipboard.
 *
 *           Note that copy/paste is somewhat complicated under Linux/Unix
 *           systems.  There is not a single system clipboard.  Furthermore
 *           this capability is tied to the X11 system (i.e., you have to
 *           do a device 1 X11 in Dataplot before you can access the
 *           clipboard).
 *
 *           There are several mechanisms for communicating between
 *           applications in X11:
 *
 *             1. X10 implemented "cut buffers".  Although up to 8
 *                cut buffers can be utilized, "cut buffer 0" is
 *                the default and is all we attempt to support.
 *
 *                Although the cut buffer is simple and more than adequate
 *                for Dataplot purposes (i.e., Dataplot only supports
 *                text, no images or binary data), it is basically
 *                considered obsolete.  It is still supported by some
 *                older X11 applications (most significantly, xterm
 *                implements cut/paste via cut buffer 0).  However, the
 *                use of cut buffers is generally discouraged these days.
 *
 *             2. X11 introduced the concept of "selections".  Selections
 *                are a general way for X11 applications to communicate
 *                with clipboard cut and paste being a subset of the
 *                selection capability.
 *
 *                There are several relevant selections:
 *
 *                  i. XA_PRIMARY - this is the "primary" selection and it
 *                                  contains the last text you highlighted.
 *
 *                 ii. XA_SECONDARY - this contains a "secondary" selection.
 *
 *                iii. XA_CLIPBOARD - contains text explicitly copied with
 *                                    Edit | Copy, Cntrl-C, etc.  That is,
 *                                    this is the system clipboard.
 *
 *                For Dataplot, we are primarily interested in XA_CLIPBOARD.
 *                However, the user can explicitly request any of the 3
 *                selections or the cut buffer 0.  If the xclip library is
 *                not available, only the cut buffer 0 will be available.
 *
 *           Note that the X11 device has to be open for the clipboard
 *           to be available.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void xfetch_(istr, nval, maxchr, ierror)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void XFETCH_(istr, nval, maxchr, ierror)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void xfetch(istr, nval, maxchr, ierror)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void XFETCH(istr, nval, maxchr, ierror)
#endif
#if INTEGER_PRECISION == 0
int    *nval;
int    *maxchr;
int    *ierror;
#else
int    nval[2];
int    maxchr[2];
int    ierror[2];
#endif
int    istr[];

{
   int buffer_open;
   int iptr;
   int nbytes_return;
   int nbytes_max;
   int nval_temp;
   int maxchr_temp;
   int ierror_temp;
   int ival;
   int ilen;
   int counter;
   char chrtemp[1];
   char *bytes_return = NULL;
   char *bytes_return_save = NULL;

   /* Step 0: Initialize */
#if INTEGER_PRECISION == 0
   maxchr_temp = *maxchr;
   ierror_temp = *ierror;
#else
   maxchr_temp = maxchr[0];
   ierror_temp = ierror[0];
#endif

   *ierror = 0;
   if ( OPEN_FLAG == 0 ){
#if INTEGER_PRECISION == 0
      *ierror = 1;
#else
      ierror[0] = 1;
#endif
      return;
   }

   /* Step 1: fetch contents of buffer */
   buffer_open = 0;
   nbytes_return = 0;
   if (bytes_return) XFree(bytes_return);
   bytes_return = XFetchBytes(display, &nbytes_return);
   nbytes_max = nbytes_return;
   if (nbytes_return > 0) {
      buffer_open = 1;
      iptr= 0;
      nbytes_max = nbytes_return;
   } else {
      if (bytes_return) XFree(bytes_return);
#if INTEGER_PRECISION == 0
      *nval = -1;
#else
      nval[0] = -1;
#endif
      return;
   }

   /* Step 2: Convert contents of buffer to an array of ASCII Decimal
    *         Equivalents */
   nval_temp = 0;
   iptr = 0;
   bytes_return_save = bytes_return;

   for ( counter = 0; counter < nbytes_return; counter++ ) {
       ilen = 1;
       strncpy(chrtemp, bytes_return, ilen);
       ival = chrtemp[0];
       istr[iptr] = ival;
       nval_temp = nval_temp + 1;
       iptr = iptr + 1;
       bytes_return = bytes_return + 1;
       if (ival == 0) {
          break;
       }
   }
#if INTEGER_PRECISION == 0
   *nval = nval_temp;
#else
   nval[0] = nval_temp;
#endif

   /* Step 3: Free contents of buffer */
   bytes_return = bytes_return_save;
   if (bytes_return) XFree(bytes_return);

}

/* i_to_s  - utitlity routine to convert an integer array containing
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
void i_to_s(string1, string2, maxlen, ilen)
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
 *
 * To accomodate "large" screens, change the lower bound from
 * 0.3 times root width/height to 0.1 time root width/height.
 */

void set_screen(xpixels, ypixels, orien)
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
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       else {            /* Use default size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = DEFAULT_Y_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
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
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         atemp = xpixels * (8.5/11.0);
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
       }
       else {                 /* Use default x size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         atemp = temp * (8.5/11.0);
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
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
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
         atemp = temp * (8.5/11.);
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
         }
         temp = atemp + 0.5;
         xsh.width = temp;
       }
       else {                   /* Use default size (base on X default) */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
         }
         temp = atemp + 0.5;
         xsh.height = temp;
         atemp = temp * (8.5/11.0);
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
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
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
            atemp = 0.5*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
         xsh.width = temp;
       }
       else {                 /* Use default x size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
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
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = ypixels;
         atemp = temp;
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
            atemp = 0.67*root_height;
            temp = atemp + 0.5;
         }
         xsh.height = temp;
       }
       else {            /* Use default size */
         temp = DEFAULT_X_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_width || atemp > 0.8*root_width) {
            atemp = 0.5*root_width;
            temp = atemp + 0.5;
         }
         xsh.width = temp;
         temp = DEFAULT_Y_SIZE;
         atemp = temp;
         if (atemp < 0.1*root_height || atemp > 0.8*root_height) {
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

/*  gd.c
 *
 *  Updated:  2/2006
 *            1) Upgrade to current versions of GD (and appropriate
 *               auxillary libraries).
 *            2) New GD library (version 2.033) now supports GIF,
 *               so add GIF support
 *            3) I've been having some compilation problems with
 *               the new version of GD on a few platforms, so make
 *               the GIF driver switchable with a define option.
 *  Updated:  3/2008
 *            1) In order to better support image files, convert
 *               from using "color palette" (which supports a
 *               maximum of 256 colors) to using "true color"
 *               (which supports full RGB support, with each
 *               component supporting intensities from 0 to 255).
 *               NOTE: This is a user-settable option.  In
 *                     Dataplot, the command
 *                          SET GD COLOR <TRUE/FIXED>
 *                     specifies which color model will be used.
 *            2) Add support for text.  Note that GD supports 5
 *               built-in fonts and in addition supports free type
 *               fonts using the FreeType library.  Note that
 *               Dataplot does not provide any true type fonts.
 *            3) Add support for reading images.
 *
 *  Updated:  5/2009
 *            When using the "-DDOUBLE" option, need
 *            to make a distinction between real
 *            numbers and integers since the default
 *            byte size can be set independently
 *            for these.  Add the option "-DINTEGER8"
 *            to handle the case where integers set
 *            to 8 bytes on a 32-bit machine.
 *
 *  Updated:  7/2014
 *            Upgraded to the 2.1.0 version.  Added support for
 *            additional devices (JPEG, PNG and GIF were the
 *            originally supported devices).
 *
 *                1 - added support for BPM and WBPM devices.  These
 *                    were available in previous versions of GD,
 *                    but I had not activated them.
 *
 *                2 - added support for TGA (Targa format).
 *
 *                3 - added support for TIFF.  Requires the "LIBTIFF"
 *                    library to be installed.
 *
 *                4 - the webp format is supported if the VPX
 *                    library is available.  This is a relatively
 *                    new format championed by Google.  Primarily
 *                    intended for viewing videos on the web.
 *
 *  Updated:  11/2020
 *            Increased the supported number of fixed colors from
 *            89 to 163.
 *
 *  Updated:  10/2023
 *            Made some tweaks to address a few issues.  Specifically,
 *            dashed/dotted lines were not working, character fill not
 *            working, unable to set background color in "true color"
 *            mode.  These issues have been corrected.
 *
 *  The purpose of this library is to provide easy access from
 *  a Fortran 77 program to the GD library.
 *
 *  Specifically, I was interested in providing support for PNG and
 *  JPEG images for generating web viewable graphics directly from
 *  Dataplot.  We previously had supported this by generating
 *  Postscript and then using various image conversion software.
 *  Although workable, the GD library allows us to build the support
 *  for web viewable graphics directly into Dataplot.  Dataplot
 *  is a Fortran 77 program that uses a vector graphics model (as
 *  oppossed to a bit-map model) to generate graphics.  The GD
 *  library provides a vector based interface to these formats
 *  with a C library.
 *
 *  Given my problem, I have not supported the full GD library.
 *  Instead, I have supported a basic set of calls to support a
 *  device driver for PNG or JPEG.  This set of routines provides
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
 *  This code assumes that the following libraries are installed on
 *  your system:
 *
 *       libgd
 *       libpng
 *       jpeg-6b
 *       zlib
 *
 *  These libraries are all freely available and they are downloadable
 *  from the web.  A dummy version of this library is maintained for
 *  those systems that do not support gd or that do not allow C
 *  routines to be called from Fortran.  Since the dummy library is
 *  coded in Fortran, routine names will be limited to six characters.
 *
 *  Note that calling C from Fortran is not standard.  I have
 *  provided the following compiler defintions to enhance portability.
 *
 *  1) The default is to assume that the Fortran compiler appends an
 *         underscore to the routine name.  Use -DNOUNDERSCORE if your
 *         compiler does not append the underscore.
 *  2) The default is to assume that the Fortran compiler converts
 *        routine names to lower case.  Use -DUPPERCASE if your
 *        Fortran compiler does not do this (e.g., the Cray).
 *  3) Many Unix compilers support a "-r8", or something similar,
 *        to make single precision 64-bit.  Use -DDOUBLE if you
 *        compile your Fortran with this option.
 *  4) Character strings are the most troublesome issue for
 *        portability.  Passing character strings from Fortran to C
 *        is very compiler dependent.  I have addressed this issue
 *        by passing character strings as arrays of ASCII Decimal
 *        Equivalents (ADE's).  That is, the Fortran converts a
 *        character string to an array of the integer values where
 *        the integer is the ASCII collating sequence (i.e., A = 65,
 *        B = 66, etc.).  The end of the string is denoted by setting
 *        the value to 0.  This is easily accomplished on the Fortran
 *        side by using the ICHAR function.  The C code here then
 *        calls an internal routine to covnert the integer array to
 *        a C string.   Although a bit convoluted, this avoids a lot
 *        of messy portability issues.
 *
 *
 *  The following routines are included:
 *
 *  gdinit      - initialize gd library
 *  gdend       - close gd library
 *  gderas      - start a new graph (close currently open one as well)
 *  gddraw      - draw a polyline
 *  gdseco      - set foreground color (for "fixed" option)
 *  gdsec2      - set foreground color (for "true" option)
 *  gdsepa      - set line pattern
 *  gdpoin      - draw a point (i.e., a pixel)
 *  gdcirc      - draw a circle
 *  gdrgfl      - solid fill of a region
 *  gdtxth      - draw a horizontal character string
 *  gdtxtv      - draw a vertical character string
 *  gdload      - load a pre-existing image
 *  gdpixe      - read a specified pixel position
 *  gdunlo      - unload a pre-existing image
 *  i_to_s_2    - utility routine to convert array of ADE's to string
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
#ifdef NO_GIF
#define GIF 0
#else
#define GIF 1
#endif

/*  include files */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include <gd.h>
#include <gdfonts.h>
#include <gdfontl.h>
#include <gdfontmb.h>
#include <gdfontg.h>
#include <gdfontt.h>

/* global definitions */

/* 2021/03/12: with recent upgrade of color model, need to
 *             increase max colors to 163 from 89.
 *
 */
#define MAX_COLORS_GD   163
#define MAX_GRAY        100
#define BORDER_WIDTH      3
#define DEFAULT_X_SIZE  600
#define DEFAULT_Y_SIZE  465
#define MIN_X_SIZE      100
#define MIN_Y_SIZE      100

/* GD declarations */
gdImagePtr    im;                 /* Declare Image for writing */
gdFontPtr     im_font;            /* Image Font Pointer */
FILE          *jpegout;           /* File ID for Image (writing) */
char          file_string[160];   /* Name of current file (writing) */
gdImagePtr    im2;                /* Declare Image for reading */
FILE          *jpegin;            /* File ID for Image (reading) */
char          file_string_2[160]; /* Name of current file (reading) */

/* common parameters */
int    COLOR_OPTION_GD = 0;            /* Specify fixed/true color */
int    CURRENT_COLOR_GD;               /* Define current color */
int    CURRENT_LINE_STYLE_GD[12];      /* Define current line style */
int    NPTS_STYLE_GD = 0;              /* Number of points in style */
int    color_table[MAX_COLORS_GD + 100];  /* color table */
int red[MAX_COLORS_GD] = {
    /*   0 -   7 */ 255,   0, 255,   0,   0, 255, 255,   0,
    /*   8 -  15 */ 255, 154,   0, 173, 138, 208,  47, 211,
    /*  16 -  23 */ 127, 165,  95, 255, 100,  85, 153,  72,
    /*  24 -  31 */   0, 178,  34, 255, 218, 192, 205, 240,
    /*  32 -  39 */ 105, 176,  50, 176, 102,   0, 107, 250,
    /*  40 -  47 */ 186,  60, 123,   0,  72, 199,  25,   0,
    /*  48 -  55 */ 255, 218, 152, 255, 221, 160, 250,  46,
    /*  56 -  63 */ 160, 125, 106,   0,  70, 210, 216,  64,
    /*  64 -  71 */ 238, 245, 173, 224,   0,   0,   0,   0,
    /*  72 -  79 */   0,   0,   0,   0,   0, 238, 205, 139,
    /*  80 -  87 */ 238, 205, 139, 238, 205, 139, 238, 205,
    /*  88 -  95 */ 139, 240, 233, 255, 220, 139, 255, 255,
    /*  96 - 103 */ 255, 219, 255, 255, 255, 255, 255, 255,
    /* 104 - 111 */ 255, 238, 189, 230, 255, 147, 153, 148,
    /* 112 - 119 */ 139,  75, 127, 124,   0, 144, 107, 128,
    /* 120 - 127 */ 143,  32,   0,   0, 175,   0, 176, 135,
    /* 128 - 135 */   0,  30,  65,   0, 255, 255, 255, 255,
    /* 136 - 143 */ 222, 188, 244, 184, 205, 210, 139, 255,
    /* 144 - 151 */ 240, 245, 240, 240, 248, 245, 255, 245,
    /* 152 - 159 */ 253, 255, 255, 255, 250, 250, 255, 192,
    /* 160 - 162 */ 169, 119, 112
};
int green[MAX_COLORS_GD] = {
    /*   0 -   7 */ 255,   0,   0,   0, 255,   0, 165, 255,
    /*   8 -  15 */ 255, 205, 100, 216,  43,  32,  79, 211,
    /*  16 -  23 */ 255,  42, 158, 127, 149, 107,  50,  61,
    /*  24 -  31 */ 206,  34, 139, 215, 165, 192,  92, 230,
    /*  32 -  39 */ 105, 196, 205,  48, 205,   0, 142, 250,
    /*  40 -  47 */  85, 179, 104, 250, 209,  21,  25,   0,
    /*  48 -  55 */  69, 112, 251, 192, 160,  32, 128, 139,
    /*  56 -  63 */  82, 206,  90, 255, 130, 180, 191, 224,
    /*  64 -  71 */ 130, 222, 255, 255,   0,   0,   0, 238,
    /*  72 -  79 */ 205, 139, 238, 205, 139, 238, 205, 139,
    /*  80 -  87 */ 154, 133,  90,   0,   0,   0,   0,   0,
    /*  88 -  95 */   0, 128, 150, 160,  20,   0, 182, 105,
    /*  96 - 103 */  20, 112,  99, 140, 255, 250, 239, 228,
    /* 104 - 111 */ 218, 232, 183, 230,   0, 112, 102,   0,
    /* 112 - 119 */   0,   0, 255, 252, 255, 238, 142, 128,
    /* 120 - 127 */ 188, 178, 139, 128, 238, 255, 224, 206,
    /* 128 - 135 */ 191, 144, 105,   0, 248, 235, 228, 222,
    /* 136 - 143 */ 184, 143, 164, 134, 133, 105,  69, 250,
    /* 144 - 151 */ 255, 255, 255, 248, 248, 245, 245, 245,
    /* 152 - 159 */ 245, 250, 255, 235, 240, 228, 220, 192,
    /* 160 - 162 */ 169, 136, 128
};
int blue[MAX_COLORS_GD] = {
    /*   0 -   7 */ 255,   0,   0, 255,   0, 255,   0, 255,
    /*   8 -  15 */   0,  50,   0, 230, 226, 144,  79, 211,
    /*  16 -  23 */ 212,  42, 160,  80, 237,  47, 204, 139,
    /*  24 -  31 */ 209,  34,  34,   0,  32, 192,  92, 140,
    /*  32 -  39 */ 105, 222,  50,  96, 170, 205,  35, 210,
    /*  40 -  47 */  85, 179, 104, 250, 209,  21,  25,   0,
    /*  48 -  55 */   0, 214, 152, 203, 221, 240, 114,  87,
    /*  56 -  63 */  45, 235, 205, 127, 180, 140, 216, 208,
    /*  64 -  71 */ 238, 179,  47, 255, 238, 205, 139, 238,
    /*  72 -  79 */ 205, 139,   0,   0,   0,   0,   0,   0,
    /*  80 -  87 */   0,   0,   0,   0,   0,   0, 238, 205,
    /*  88 -  95 */ 139, 128, 122, 122,  60,   0, 193, 180,
    /*  96 - 103 */ 147, 147,  71,   0, 224, 205, 213, 181,
    /* 104 - 111 */ 185, 170, 107, 250, 255, 219, 204, 211,
    /* 112 - 119 */ 139, 130,   0,   0,   0, 144,  35,   0,
    /* 120 - 127 */ 143, 170, 139, 128, 238, 255, 230, 250,
    /* 128 - 135 */ 255, 255, 225, 139, 220, 205, 196, 173,
    /* 136 - 143 */ 135, 143,  96,  11,  63,  30,  19, 250,
    /* 144 - 151 */ 240, 250, 255, 255, 255, 245, 238, 220,
    /* 152 - 159 */ 230, 240, 240, 215, 230, 225, 220, 192,
    /* 160 - 162 */ 169, 153, 144
};

/* flags for current attribute settings */
static int    OPEN_FLAG_GD = 0;       /* 0 - GD closed, 1 - GD open */
int           DEVICE_TYPE_GD = 0;     /* define device */
                                      /* 1 - jpeg */
                                      /* 2 - png */
                                      /* 3 - windows bmp */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void  gdend_(), gddraw_(), gdpoin_(), gdcirc_(), gdrgfl_();
void  gdinit_(), gderas_(), gdtxth_(), gdtxtv_();
void  gdseco_(), gdsec2_(), gdsepa_();
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void  GDEND_(), GDINIT_(), GDDRAW_(), GDPOIN_(), GDCIRC_(), GDRGFL_();
void  GDINIT_(), GDERAS_(), GDTXTH_(), GDTXTV_();
void  GDSECO_(), GDSEC2_(), GDSEPA_();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void  gdend(), gddraw(), gdpoin(), gdcirc(), gdrgfl();
void  gdinit(), gderas(), gdtxth(), gdtxtv(),gdtatt();
void  gdseco(), gdsec2(), gdsepa();
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void  GDEND(),  GDDRAW(), GDPOIN(), GDCIRC(), GDRGFL();
void  GDINIT(), GDERAS(), GDTXTH(), GDTXTV();
void  GDSECO(), GDSEC2(), GDSEPA();
#endif
void  i_to_s_2();

/* GDINIT  - routine to initialize GD.
 *           For GD device, simply set flag saying this routine
 *           has been called, initialize a few variables.
 *
 *           DEVICE_TYPE_GD = 1 -- jpeg
 *           DEVICE_TYPE_GD = 2 -- png
 *           DEVICE_TYPE_GD = 3 -- wbmp
 *           DEVICE_TYPE_GD = 4 -- gif
 *           DEVICE_TYPE_GD = 5 -- tiff (requires Tiff library)
 *           DEVICE_TYPE_GD = 6 -- bmp
 *           DEVICE_TYPE_GD = 7 -- webp (requires VPX library)
 *           DEVICE_TYPE_GD = 8 -- tga (this code currently not active)
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdinit_(itype)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDINIT_(itype)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdinit(itype)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDINIT(itype)
#endif
#if INTEGER_PRECISION == 0
int  *itype;
#else
int  itype[2];
#endif
{

   int   itype_temp;

#if INTEGER_PRECISION == 0
   itype_temp = *itype;
#else
   itype_temp = itype[0];
#endif
    DEVICE_TYPE_GD = itype_temp;
    OPEN_FLAG_GD = 0;

}


/* GDERAS  - routine to clear the screen.  Since PNG and JPEG devices
 *           treat each plot as a separate file, initialization
 *           functions occur here rather than GDINIT.  Do the
 *           following:
 *           1) Check if a plot is currently open.  If yes, write
 *              it to a file and destroy the current image.
 *           2) Create a new image with the specified size specified
 *              in pixels.  Note that orientation (landscape, portrait,
 *              is implicit in the pixel dimensions).  Note that
 *              this routine does not modify the values.
 *           3) Set all colors to be undefined and then set
 *              background and foreground colors.
 *
 *           3/2008: User can now request either "fixed" (i.e., a
 *                   pallette of pre-defined colors) or "true"
 *                   (i.e., colors are specified as RGB triplets).
 *
 *  xpixels   - width (in pixels) for graphics window
 *  ypixels   - height (in pixels) for graphics window
 *  back_col  - background color
 *  icol_typ  - color model (0 => fixed, 1 => true)
 *  file name - file name (in integer ascii decimal equivalents)
 *
 */

#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gderas_(xpixels, ypixels, back_col, icol_type, file_name)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDERAS_(xpixels, ypixels, back_col, icol_type, file_name)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gderas(xpixels, ypixels, back_col, icol_type, file_name)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDERAS(xpixels, ypixels, back_col, icol_type, file_name)
#endif
#if INTEGER_PRECISION == 0
int  *xpixels, *ypixels, *back_col;
int  *icol_type;
int file_name[];
#else
int  xpixels[2], ypixels[2], back_col[2];
int  icol_type[2];
int file_name[];
#endif
{

   int   back_col_temp, temp_color, temp_color_2;
   int   xpixels_temp, ypixels_temp;
   int   len;
   int   itemp, quality;
   int   i;
   int   itemp1, itemp2;
   int   icol_temp;
   int   jcol_temp;
   int   bitdepth;
   int   compression;
   float ai, atemp2;

#if INTEGER_PRECISION == 0
   back_col_temp = *back_col;
   xpixels_temp = *xpixels;
   ypixels_temp = *ypixels;
   icol_temp = *icol_type;
#else
   back_col_temp = back_col[0];
   xpixels_temp = xpixels[0];
   ypixels_temp = ypixels[0];
   icol_temp = icol_type[0];
#endif

   /* First, check if a graph is currently open, if so write it
      to the current file name. */
   /*  Currently supported image formats:
    *    1 - JPEG
    *    2 - PNG
    *    3 - WBMP
    *    4 - GIF
    *    5 - TIFF
    *    6 - BMP
    *    7 - WEBP
    */
   if (OPEN_FLAG_GD == 1) {
      /* interlaced images load faster on web, but leave for now */
      /* gdImageInterlace(im,1); */
      jpegout = fopen(file_string,"wb");
      if (DEVICE_TYPE_GD == 1) {
         quality = -1;
         gdImageJpeg(im,jpegout,quality);
      }
      else if (DEVICE_TYPE_GD == 2) {
         gdImagePng(im,jpegout);
      }
      else if (DEVICE_TYPE_GD == 3) {
         itemp=1;
         gdImageWBMP(im,itemp,jpegout);
      }
      else if (DEVICE_TYPE_GD == 4) {
         gdImageGif(im,jpegout);
      }
      else if (DEVICE_TYPE_GD == 5) {
         bitdepth = 8;
#ifdef HAVE_GD_TIFF
         gdImageTiff(im,jpegout);
#endif
      }
      else if (DEVICE_TYPE_GD == 6) {
         compression=1;
#ifdef HAVE_GD_BMP
         gdImageBmp(im,jpegout,compression);
#endif
      }
      else if (DEVICE_TYPE_GD == 7) {
#ifdef HAVE_GD_VPX
         gdImageWebp(im,jpegout);
#else
         quality = -1;
         gdImageJpeg(im,jpegout,quality);
#endif
      }
      else if (DEVICE_TYPE_GD == 8) {
         /*
          * printf("%s \n","gd.c: Before TGA");
          * gdImageTga(im,jpegout);
          */
      }
      else {
         quality = -1;
         gdImageJpeg(im,jpegout,quality);
      }
      fclose(jpegout);
      gdImageDestroy(im);
   }

   /* Now, start new image */
#if INTEGER_PRECISION == 0
     i_to_s_2(file_name, file_string, 80, &len);
#else
     i_to_s_2(file_name, file_string, 160, &len);
#endif

   /* Open image.  Different code is used for the "fixed"
    *              (i.e., a maximum of 255 distinct colors) and
    *              "true" color (colors specified as RGB triplets)
    *              cases.
    */

   if (icol_temp == 0) {           /* Fixed color case */
      /*  Step 1: Open Image */
      im = gdImageCreate(xpixels_temp, ypixels_temp);
      /*  Step 2: Allocate colors */
      back_col_temp = back_col_temp - 1;
      gdImageColorAllocate(im,red[back_col_temp],green[back_col_temp],
                           blue[back_col_temp]);
      for (i=0; i < MAX_COLORS_GD; i++) {
         color_table[i] = gdImageColorAllocate(im,red[i],green[i],
                          blue[i]);
      }
      for (i=0; i < 100; i++) {
         itemp1 = i + MAX_COLORS_GD;
         ai = i;
         atemp2 = 255.*(ai/100.);
         itemp2 = atemp2;
         color_table[itemp1] = gdImageColorAllocate(im,itemp2,itemp2,
                               itemp2);
      }
   }
   else {                  /* True color case */
      /*  Step 1: Open Image */
      im = gdImageCreateTrueColor(xpixels_temp, ypixels_temp);
      /*  Step 2: Allocate colors for Dataplot's basic color map */
      for (i=0; i < MAX_COLORS_GD; i++) {
         color_table[i] = gdImageColorAllocate(im,red[i],green[i],
                          blue[i]);
      }
      for (i=0; i < 100; i++) {
         itemp1 = i + MAX_COLORS_GD;
         ai = i;
         atemp2 = 255.*(ai/100.);
         itemp2 = atemp2;
         color_table[itemp1] = gdImageColorAllocate(im,itemp2,itemp2,
                               itemp2);
         COLOR_OPTION_GD = 1;
      }
   }
   OPEN_FLAG_GD = 1;

}

/* GDEND   - routine to end GD.  Close the display.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdend_(file_name)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDEND_(file_name)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdend(file_name)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDEND(file_name)
#endif
int file_name[];

{
   int     quality;
   int     itemp;
   int     len;
   int     bitdepth;
   int     compression;

#if INTEGER_PRECISION == 0
     i_to_s_2(file_name, file_string, 80, &len);
#else
     i_to_s_2(file_name, file_string, 160, &len);
#endif
   /*  Currently supported image formats:
    *    1 - JPEG
    *    2 - PNG
    *    3 - WBMP
    *    4 - GIF
    *    5 - TIFF (not activated)
    *    6 - BMP
    *    7 - WEBP
    *    8 - TGA
    */
   if (OPEN_FLAG_GD == 1) {
      /* interlaced images load faster on web, but leave for now */
      /* gdImageInterlace(im,1); */
      jpegout = fopen(file_string,"wb");
      if (DEVICE_TYPE_GD == 1) {
         quality = -1;
         gdImageJpeg(im,jpegout,quality);
      }
      else if (DEVICE_TYPE_GD == 2) {
         gdImagePng(im,jpegout);
      }
      else if (DEVICE_TYPE_GD == 3) {
         itemp=1;
         gdImageWBMP(im,itemp,jpegout);
      }
      else if (DEVICE_TYPE_GD == 4) {
         gdImageGif(im,jpegout);
      }
      else if (DEVICE_TYPE_GD == 5) {
         /* If libtiff not installed, default to jpeg */
#ifdef HAVE_GD_TIFF
         gdImageTiff(im,jpegout);
#endif
      }
      else if (DEVICE_TYPE_GD == 6) {
         compression=1;
#ifdef HAVE_GD_BMP
         gdImageBmp(im,jpegout,compression);
#endif
      }
      else if (DEVICE_TYPE_GD == 7) {
#ifdef HAVE_GD_VPX
         gdImageWebp(im,jpegout);
#else
         quality = -1;
         gdImageJpeg(im,jpegout,quality);
#endif
      }
      else if (DEVICE_TYPE_GD == 8) {
         /*
          * printf("%s \n","gd.c: Before TGA");
          * gdImageTga(im,jpegout);
          */
      }
      else {
         quality = -1;
         gdImageJpeg(im,jpegout,quality);
      }
      fclose(jpegout);
      gdImageDestroy(im);
   }
   OPEN_FLAG_GD = 0;

}

/* GDDRAW  - draw a polyline.  The color and line style have
 *           been set in GDSECO and GDSEPA (line thickness is
 *           generated in software)
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points to plot
 *
 */
#define MAX_LINE_POINTS  500
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gddraw_(ix1,iy1,ix2,iy2)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDDRAW_(ix1,iy1,ix2,iy2)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gddraw(ix1,iy1,ix2,iy2)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDDRAW(ix1,iy1,ix2,iy2)
#endif
#if INTEGER_PRECISION == 0
int   *ix1, *iy1, *ix2, *iy2;
#else
int   ix1[2], iy1[2], ix2[2], iy2[2];
#endif
{
   int     ix1_temp;
   int     iy1_temp;
   int     ix2_temp;
   int     iy2_temp;

#if INTEGER_PRECISION == 0
   ix1_temp = *ix1;
   iy1_temp = *iy1;
   ix2_temp = *ix2;
   iy2_temp = *iy2;
#else
   ix1_temp = ix1[0];
   iy1_temp = iy1[0];
   ix2_temp = ix2[0];
   iy2_temp = iy2[0];
#endif

   if (NPTS_STYLE_GD <= 0) {            /* Solid Line */
      if (COLOR_OPTION_GD == 0) {
         gdImageLine(im, ix1_temp, iy1_temp, ix2_temp, iy2_temp,
                     color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageLine(im, ix1_temp, iy1_temp, ix2_temp, iy2_temp,
                     CURRENT_COLOR_GD);
      }
   }
   else {      /* Dashed or Dotted Line */
      gdImageSetStyle(im, CURRENT_LINE_STYLE_GD, NPTS_STYLE_GD);
      gdImageLine(im,ix1_temp,iy1_temp,ix2_temp,iy2_temp,gdStyled);
   }

}

/* GDSECO  - set the color
 *
 * jcol   - index for desired color
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdseco_(jcol)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDSECO_(jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdseco(jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDSECO(jcol)
#endif
#if INTEGER_PRECISION == 0
int   *jcol;
#else
int   jcol[2];
#endif
{
   int     jcol_temp;
   int     jred_temp;
   int     jgreen_temp;
   int     jblue_temp;

#if INTEGER_PRECISION == 0
   jcol_temp = *jcol;
#else
   jcol_temp = jcol[0];
#endif

   if (COLOR_OPTION_GD == 0) {
      if (jcol_temp >= 0 ) {
         jcol_temp = jcol_temp - 1;
         CURRENT_COLOR_GD = jcol_temp;
      } else {
         jcol_temp = -jcol_temp;
         jcol_temp = jcol_temp + MAX_COLORS_GD - 1;
         CURRENT_COLOR_GD = jcol_temp;
      }
   } else {
      jred_temp = red[jcol_temp-1];
      jgreen_temp = green[jcol_temp-1];
      jblue_temp = blue[jcol_temp-1];
      jcol_temp = gdImageColorExact(im,jred_temp,jgreen_temp,jblue_temp);
      if (jcol_temp == (-1)) {
         jcol_temp = gdImageColorAllocate(im,jred_temp,jgreen_temp,jblue_temp);
         jcol_temp = gdImageColorClosest(im,jred_temp,jgreen_temp,jblue_temp);
      }
      CURRENT_COLOR_GD = jcol_temp;
   }

}

/* GDSEC2  - set the color based on RGB triplet
 *
 * jred   - value for red component
 * jgreen - value for green component
 * jblue  - value for blue component
 *
 * Each of these components is assumed to be an integer in
 * the range 0 - 255.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdsec2_(jred,jgreen,jblue,iretco)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDSEC2_(jred,jgreen,jblue,iretco)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdsec2(jred,jgreen,jblue,iretco)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDSEC2(jcol,iretco)
#endif
#if INTEGER_PRECISION == 0
int   *jred, *jgreen, *jblue;
int   *iretco;
#else
int   jred[2], jgreen[2], jblue[2];
int   iretco[2];
#endif
{
   int     jred_temp;
   int     jgreen_temp;
   int     jblue_temp;
   int     jcol_temp;

#if INTEGER_PRECISION == 0
   jred_temp = *jred;
   jgreen_temp = *jgreen;
   jblue_temp = *jblue;
#else
   jred_temp = jred[0];
   jgreen_temp = jgreen[0];
   jblue_temp = jblue[0];
#endif

   if (jred_temp < 0) {
      jred_temp = 0;
   } else if (jred_temp > 255) {
      jred_temp = 255;
   }
   if (jgreen_temp < 0) {
      jgreen_temp = 0;
   } else if (jgreen_temp > 255) {
      jgreen_temp = 255;
   }
   if (jblue_temp < 0) {
      jblue_temp = 0;
   } else if (jblue_temp > 255) {
      jblue_temp = 255;
   }

   if (COLOR_OPTION_GD == 0) {  /* For fixed color, find closest match */
      jcol_temp = gdImageColorClosest(im,jred_temp,jgreen_temp,jblue_temp);
   }
   else {
      jcol_temp = gdImageColorExact(im,jred_temp,jgreen_temp,jblue_temp);
      if (jcol_temp == (-1)) {
         jcol_temp = gdImageColorAllocate(im,jred_temp,jgreen_temp,jblue_temp);
         jcol_temp = gdImageColorClosest(im,jred_temp,jgreen_temp,jblue_temp);
      }
      CURRENT_COLOR_GD = jcol_temp;
#if INTEGER_PRECISION == 0
       *iretco = jcol_temp;
#else
       iretco[0] = jcol_temp;
#endif
   }

}

/* GDSEPA  - set pattern for lines
 *
 * jpatt  - the line pattern
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdsepa_(jpatt)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDSEPA_(jpatt)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdsepa(jpatt)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDSEPA(jpatt)
#endif
#if INTEGER_PRECISION == 0
int   *jpatt;
#else
int   jpatt[2];
#endif
{
   int     jpatt_temp;

#if INTEGER_PRECISION == 0
   jpatt_temp = *jpatt;
#else
   jpatt_temp = jpatt[0];
#endif

   if (jpatt_temp == 1) {            /* Solid Line */
      CURRENT_LINE_STYLE_GD[0] = 0;
      NPTS_STYLE_GD = 0;
   }
   else if (jpatt_temp == 2) {      /* Dashed Line */
      if (COLOR_OPTION_GD == 0) {
         CURRENT_LINE_STYLE_GD[0] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[1] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[2] = color_table[CURRENT_COLOR_GD];
      } else {
         CURRENT_LINE_STYLE_GD[0] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[1] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[2] = CURRENT_COLOR_GD;
      }
      CURRENT_LINE_STYLE_GD[3] = gdTransparent;
      CURRENT_LINE_STYLE_GD[4] = gdTransparent;
      CURRENT_LINE_STYLE_GD[5] = gdTransparent;
      NPTS_STYLE_GD = 6;
   }
   else if (jpatt_temp == 3) {      /* Dotted Line */
      CURRENT_LINE_STYLE_GD[0] = color_table[CURRENT_COLOR_GD];
      CURRENT_LINE_STYLE_GD[1] = gdTransparent;
      NPTS_STYLE_GD = 2;
   }
   else if (jpatt_temp == 4) {      /* DA2 Line */
      if (COLOR_OPTION_GD == 0) {
         CURRENT_LINE_STYLE_GD[0] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[1] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[2] = color_table[CURRENT_COLOR_GD];
      } else {
         CURRENT_LINE_STYLE_GD[0] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[1] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[2] = CURRENT_COLOR_GD;
      }
      CURRENT_LINE_STYLE_GD[3] = color_table[CURRENT_COLOR_GD];
      CURRENT_LINE_STYLE_GD[4] = gdTransparent;
      CURRENT_LINE_STYLE_GD[5] = gdTransparent;
      NPTS_STYLE_GD = 6;
   }
   else if (jpatt_temp == 5) {      /* DA3 Line */
      if (COLOR_OPTION_GD == 0) {
         CURRENT_LINE_STYLE_GD[0] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[1] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[2] = color_table[CURRENT_COLOR_GD];
      } else {
         CURRENT_LINE_STYLE_GD[0] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[1] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[2] = CURRENT_COLOR_GD;
      }
      CURRENT_LINE_STYLE_GD[3] = gdTransparent;
      CURRENT_LINE_STYLE_GD[4] = gdTransparent;
      CURRENT_LINE_STYLE_GD[5] = gdTransparent;
      CURRENT_LINE_STYLE_GD[6] = color_table[CURRENT_COLOR_GD];
      CURRENT_LINE_STYLE_GD[7] = gdTransparent;
      NPTS_STYLE_GD = 8;
   }
   else if (jpatt_temp == 6) {      /* DA4 Line */
      if (COLOR_OPTION_GD == 0) {
         CURRENT_LINE_STYLE_GD[0] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[1] = color_table[CURRENT_COLOR_GD];
      } else {
         CURRENT_LINE_STYLE_GD[0] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[1] = CURRENT_COLOR_GD;
      }
      CURRENT_LINE_STYLE_GD[2] = gdTransparent;
      CURRENT_LINE_STYLE_GD[3] = gdTransparent;
      NPTS_STYLE_GD = 4;
   }
   else if (jpatt_temp == 7) {      /* DA5 Line */
      if (COLOR_OPTION_GD == 0) {
         CURRENT_LINE_STYLE_GD[0] = color_table[CURRENT_COLOR_GD];
         CURRENT_LINE_STYLE_GD[1] = color_table[CURRENT_COLOR_GD];
      } else {
         CURRENT_LINE_STYLE_GD[0] = CURRENT_COLOR_GD;
         CURRENT_LINE_STYLE_GD[1] = CURRENT_COLOR_GD;
      }
      CURRENT_LINE_STYLE_GD[2] = gdTransparent;
      CURRENT_LINE_STYLE_GD[3] = color_table[CURRENT_COLOR_GD];
      CURRENT_LINE_STYLE_GD[4] = gdTransparent;
      NPTS_STYLE_GD = 5;
   }
   else {
      CURRENT_LINE_STYLE_GD[0] = 0;
      NPTS_STYLE_GD = 0;
   }

}

/* GDPOIN - draw a point.
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 * jcol   - color to use in drawing the point
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdpoin_(ix, iy, jcol)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDPOIN_(ix, iy, jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdpoin(ix, iy, jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDPOIN(ix, iy, jcol)
#endif
#if INTEGER_PRECISION == 0
int   *ix, *iy, *jcol;
#else
int   ix[2], iy[2], jcol[2];
#endif
{
#if INTEGER_PRECISION == 0
   if (COLOR_OPTION_GD == 0) {
      gdImageSetPixel(im, *ix, *iy, color_table[*jcol]);
   } else {
      gdImageSetPixel(im, *ix, *iy, CURRENT_COLOR_GD);
   }
#else
   if (COLOR_OPTION_GD == 0) {
      gdImageSetPixel(im, ix[0], iy[0], color_table[jcol[0]]);
   } else {
      gdImageSetPixel(im, ix[0], iy[0], CURRENT_COLOR_GD);
   }
#endif

}

/* GDCIRC  - draw a circle.  Note that the circle may be either filled or 
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
void gdcirc_(ix, iy, irad, ifill, jcol)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDCIRC_(ix, iy, irad, ifill, jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdcirc(ix, iy, irad, ifill, jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDCIRC(ix, iy, irad, ifill, jcol)
#endif
#if INTEGER_PRECISION == 0
int   *ix, *iy, *irad, *ifill, *jcol;
#else
int   ix[2], iy[2], irad[2], ifill[2], jcol[2];
#endif
{

   int   xpos, ypos, iwidth, iheight, iang1, iang2, ir, jcol_t;

   iang1 = 0;
   iang2 = 360;
#if INTEGER_PRECISION == 0
   ir = *irad;
   jcol_t = *jcol;
   xpos = *ix - *irad;
   ypos = *iy - *irad;
#else
   ir = irad[0];
   jcol_t = jcol[0];
   xpos = ix[0] - irad[0];
   ypos = iy[0] - irad[0];
#endif
   iwidth = 2 * ir;
   iheight = 2 * ir;
   if (COLOR_OPTION_GD == 0) {
      gdImageArc(im, xpos, ypos, iwidth, iheight, iang1, iang2,
                 color_table[jcol_t]);
   } else {
      gdImageArc(im, xpos, ypos, iwidth, iheight, iang1, iang2,
                 CURRENT_COLOR_GD);
   }

}

/* GDRGFL - fill a region.  Rectangular regions will be filled differently
 *          non-rectangular regions.  Dataplot only handles convex polygons,
 *          so set this (for faster performance).  This routine only does
 *          solid fills.  Hatch patterns must be drawn
 *          by the calling program (i.e., send the individual lines to
 *          the GDDRAW routine).
 *
 * xpts   - contains the x coordinates
 * ypts   - contains the y coordinates
 * npts   - the number of points in the polygon (if 2, assume a rectangle,
 *          otherwise, a convex polygon)
 *
 */
#define MAX_REG_POINTS  1000
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdrgfl_(xpts, ypts, npts, jcol)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDRGFL_(xpts, ypts, npts, jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdrgfl(xpts, ypts, npts, jcol)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDRGFL(xpts, ypts, npts, jcol)
#endif
int   xpts[], ypts[];
#if INTEGER_PRECISION == 0
int   *npts, *jcol;
#else
int   npts[2], jcol[2];
#endif
{
   int     points[MAX_REG_POINTS];
   int     npts_temp, jcol_temp, indx;
   int     jred_temp;
   int     jgreen_temp;
   int     jblue_temp;

#if INTEGER_PRECISION == 0
   npts_temp = *npts;
   jcol_temp = *jcol;
#else
   npts_temp = npts[0];
   jcol_temp = jcol[0];
#endif
#if INTEGER_PRECISION == 0
    indx = 1;
#else
    indx = 2;
#endif

   /* Color set separately (just use CURRENT_COLOR_GD) 
   if (COLOR_OPTION_GD == 0) {
      if (jcol_temp >= 0 ) {
         jcol_temp = jcol_temp - 1;
         CURRENT_COLOR_GD = jcol_temp;
      } else {
         jcol_temp = -jcol_temp;
         jcol_temp = jcol_temp + MAX_COLORS_GD - 1;
         CURRENT_COLOR_GD = jcol_temp;
      }
   } else {
      jred_temp = red[jcol_temp-1];
      jgreen_temp = green[jcol_temp-1];
      jblue_temp = blue[jcol_temp-1];
      jcol_temp = gdImageColorExact(im,jred_temp,jgreen_temp,jblue_temp);
      CURRENT_COLOR_GD = jcol_temp;
   }
   */

   if (npts_temp == 2) {      /* rectangle */
      int  x1, y1, x2, y2;
      if (xpts[0] <= xpts[indx]) {
        x1 = xpts[0];
        x2 = xpts[indx];
      }
      else {
        x1 = xpts[indx];
        x2 = xpts[0];
      }
      if (ypts[0] <= ypts[indx]) {
        y1 = ypts[0];
        y2 = ypts[indx];
      }
      else {
        y1 = ypts[indx];
        y2 = ypts[0];
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageFilledRectangle(im, x1, y1, x2, y2, 
                                color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageFilledRectangle(im, x1, y1, x2, y2, CURRENT_COLOR_GD);
      }
   }
   else if (npts_temp > 2) {     /* convex polygon */
/* Do filled non-rectangular regions in software
      int  i, temp;
      temp = npts_temp;
      if(npts_temp > MAX_REG_POINTS) temp = MAX_REG_POINTS;
      for (i = 0; i < temp; i++) {
#if INTEGER_PRECISION == 0
         points[i].x = xpts[i];
         points[i].y = ypts[i];
#else
         points[i].x = xpts[2*i];
         points[i].y = ypts[2*i];
#endif
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageFilledPolygon(im,points,temp,color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageFilledPolygon(im,points,temp,CURRENT_COLOR_GD);
      }
*/
   }

}

/* GDTXTH - draw a horizontal text string.
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
 * jcol   - color
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdtxth_(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
             jheigh,error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDTXTH_(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
             jheigh, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdtxth(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
            jheigh, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDTXTH(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
            jheigh, error)
#endif
/*
 *  ijusth  = 0  =>  left justified
 *  ijusth  = 1  =>  center justified
 *  ijusth  = 2  =>  right justified
 *  ijustv  = 0  =>  bottom justified
 *  ijustv  = 1  =>  center justified
 *  ijustv  = 2  =>  right justified
 */
int    font[];
int    string[];
#if INTEGER_PRECISION == 0
int    *ixpos, *iypos, *ijusth, *ijustv, *jcol, *jheigh, *error;
int    *ifontz;
#else
int    ixpos[2], iypos[2], ijusth[2], ijustv[2], jcol[2], error[2];
int    jheigh[2], ifontz[2];
#endif
{

   int    itest, itempx, itempy;   /* temporary variables */
   int    len;                  /* number of characters in string */
   int    len2;                 /* number of characters in font name */
   int    string_width;         /* width of string in pixels */
   char   string2[130];         /* converted string */
   char   font_name[130];       /* string for font name */
   int    i;
   int    ixpos_temp, iypos_temp, ijusth_temp, ijustv_temp;
   int    jcol_temp;
   int    ptsize;
   int    height;
   int    width;
   int    brect[8];
   double angle;
   char   *err;
   int    ifontz_temp;

#if INTEGER_PRECISION == 0
   ixpos_temp = *ixpos;
   iypos_temp = *iypos;
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
   jcol_temp = *jcol;
   ptsize = *jheigh;
   ifontz_temp = *ifontz;
#else
   ixpos_temp = ixpos[0];
   iypos_temp = iypos[0];
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
   jcol_temp = jcol[0];
   ptsize = jheigh[0];
   ifontz_temp = ifontz[0];
#endif

#if INTEGER_PRECISION == 0
   i_to_s_2(string, string2, 130, &len);
   i_to_s_2(font, font_name, 80, &len2);
#else
   i_to_s_2(string, string2, 260, &len);
   i_to_s_2(font, font_name, 160, &len2);
#endif

   if (ifontz_temp == 1) {
      im_font = gdFontGetSmall();
      height = im_font->h;
      width = im_font->w;
      /* GD fonts seem to be vertically top justified by default */
      if (ijustv_temp == 0) {                        /* Center */
         iypos_temp = iypos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         iypos_temp = iypos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         iypos_temp = iypos_temp;
      }
      if (ijusth_temp == 0) {
         ixpos_temp = ixpos_temp;
      } else if (ijusth_temp == 1) {
         ixpos_temp = ixpos_temp - len*(width/2);
      } else if (ijusth_temp == 2) {
         ixpos_temp = ixpos_temp - len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 2) {
      im_font = gdFontGetLarge();
      height = im_font->h;
      width = im_font->w;
      /* GD fonts seem to be vertically top justified by default */
      if (ijustv_temp == 0) {                        /* Center */
         iypos_temp = iypos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         iypos_temp = iypos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         iypos_temp = iypos_temp;
      }
      if (ijusth_temp == 0) {
         ixpos_temp = ixpos_temp;
      } else if (ijusth_temp == 1) {
         ixpos_temp = ixpos_temp - len*(width/2);
      } else if (ijusth_temp == 2) {
         ixpos_temp = ixpos_temp - len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 3) {
      im_font = gdFontGetMediumBold();
      height = im_font->h;
      width = im_font->w;
      /* GD fonts seem to be vertically top justified by default */
      if (ijustv_temp == 0) {                        /* Center */
         iypos_temp = iypos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         iypos_temp = iypos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         iypos_temp = iypos_temp;
      }
      if (ijusth_temp == 0) {
         ixpos_temp = ixpos_temp;
      } else if (ijusth_temp == 1) {
         ixpos_temp = ixpos_temp - len*(width/2);
      } else if (ijusth_temp == 2) {
         ixpos_temp = ixpos_temp - len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 4) {
      im_font = gdFontGetGiant();
      height = im_font->h;
      width = im_font->w;
      /* GD fonts seem to be vertically top justified by default */
      if (ijustv_temp == 0) {                        /* Center */
         iypos_temp = iypos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         iypos_temp = iypos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         iypos_temp = iypos_temp;
      }
      if (ijusth_temp == 0) {
         ixpos_temp = ixpos_temp;
      } else if (ijusth_temp == 1) {
         ixpos_temp = ixpos_temp - len*(width/2);
      } else if (ijusth_temp == 2) {
         ixpos_temp = ixpos_temp - len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 5) {
      im_font = gdFontGetTiny();
      height = im_font->h;
      width = im_font->w;
      /* GD fonts seem to be vertically top justified by default */
      if (ijustv_temp == 0) {                        /* Center */
         iypos_temp = iypos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         iypos_temp = iypos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         iypos_temp = iypos_temp;
      }
      if (ijusth_temp == 0) {
         ixpos_temp = ixpos_temp;
      } else if (ijusth_temp == 1) {
         ixpos_temp = ixpos_temp - len*(width/2);
      } else if (ijusth_temp == 2) {
         ixpos_temp = ixpos_temp - len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageString(im, im_font, ixpos_temp, iypos_temp, string2,
                       CURRENT_COLOR_GD);
      }
   } else {
      /*  Obtain brect to set the justification */
      err = gdImageStringFT(NULL, &brect[0],jcol_temp,font_name,ptsize,
                            0.,ixpos_temp,iypos_temp,string2);
      width = abs(brect[2] - brect[0]);
      height = abs(brect[1] - brect[7]);
      if (ijustv_temp == 0) {                        /* Center */
         iypos_temp = iypos_temp + (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         iypos_temp = iypos_temp;
      } else if (ijustv_temp == 2) {                 /* Top */
         iypos_temp = iypos_temp + height;
      }
      if (ijusth_temp == 0) {
         ixpos_temp = ixpos_temp;
      } else if (ijusth_temp == 1) {
         ixpos_temp = ixpos_temp - (width/2);
      } else if (ijusth_temp == 2) {
         ixpos_temp = ixpos_temp - width;
      }
      if (COLOR_OPTION_GD == 0) {
         err = gdImageStringFT(im, &brect[0],color_table[CURRENT_COLOR_GD],
               font_name,ptsize,0.,ixpos_temp,iypos_temp,string2);
      } else {
         err = gdImageStringFT(im, &brect[0],CURRENT_COLOR_GD,
               font_name,ptsize,0.,ixpos_temp,iypos_temp,string2);
      }
   }

}

/* GDTXTV - draw a vertical text string.
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
 * jcol   - color
 * error  - error flag
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdtxtv_(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
             jheigh,error)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDTXTV_(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
             jheigh, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdtxtv(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
            jheigh, error)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDTXTV(font, string, ifontz, ixpos, iypos, ijusth, ijustv, jcol,
            jheigh, error)
#endif
/*
 *  ijusth  = 0  =>  left justified
 *  ijusth  = 1  =>  center justified
 *  ijusth  = 2  =>  right justified
 *  ijustv  = 0  =>  bottom justified
 *  ijustv  = 1  =>  center justified
 *  ijustv  = 2  =>  right justified
 */
int    font[];
int    string[];
#if INTEGER_PRECISION == 0
int    *ixpos, *iypos, *ijusth, *ijustv, *jcol, *jheigh, *error;
int    *ifontz;
#else
int    ixpos[2], iypos[2], ijusth[2], ijustv[2], jcol[2], error[2];
int    jheigh[2], ifontz[2];
#endif
{

   int    itest, itempx, itempy;   /* temporary variables */
   int    len;                  /* number of characters in string */
   int    len2;                 /* number of characters in font name */
   int    string_width;         /* width of string in pixels */
   char   string2[130];         /* converted string */
   char   font_name[130];       /* string for font name */
   int    i;
   int    ixpos_temp, iypos_temp, ijusth_temp, ijustv_temp;
   int    jcol_temp;
   int    ptsize;
   int    height;
   int    width;
   int    brect[8];
   double angle;
   char   *err;
   int    ifontz_temp;

#if INTEGER_PRECISION == 0
   ixpos_temp = *ixpos;
   iypos_temp = *iypos;
   ijusth_temp = *ijusth;
   ijustv_temp = *ijustv;
   jcol_temp = *jcol;
   ptsize = *jheigh;
   ifontz_temp = *ifontz;
#else
   ixpos_temp = ixpos[0];
   iypos_temp = iypos[0];
   ijusth_temp = ijusth[0];
   ijustv_temp = ijustv[0];
   jcol_temp = jcol[0];
   ptsize = jheigh[0];
   ifontz_temp = ifontz[0];
#endif

#if INTEGER_PRECISION == 0
   i_to_s_2(string, string2, 130, &len);
   i_to_s_2(font, font_name, 80, &len2);
#else
   i_to_s_2(string, string2, 260, &len);
   i_to_s_2(font, font_name, 160, &len2);
#endif

   if (ifontz_temp == 1) {
      im_font = gdFontGetSmall();
      height = im_font->h;
      width = im_font->w;
      /* GD fonts seem to be vertically top justified by default */
      if (ijustv_temp == 0) {                        /* Center */
         ixpos_temp = ixpos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         ixpos_temp = ixpos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         ixpos_temp = ixpos_temp;
      }
      if (ijusth_temp == 0) {
         iypos_temp = iypos_temp;
      } else if (ijusth_temp == 1) {
         iypos_temp = iypos_temp + len*(width/2);
      } else if (ijusth_temp == 2) {
         iypos_temp = iypos_temp + len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 2) {
      im_font = gdFontGetLarge();
      height = im_font->h;
      width = im_font->w;
      if (ijustv_temp == 0) {                        /* Center */
         ixpos_temp = ixpos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         ixpos_temp = ixpos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         ixpos_temp = ixpos_temp;
      }
      if (ijusth_temp == 0) {
         iypos_temp = iypos_temp;
      } else if (ijusth_temp == 1) {
         iypos_temp = iypos_temp + len*(width/2);
      } else if (ijusth_temp == 2) {
         iypos_temp = iypos_temp + len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 3) {
      im_font = gdFontGetMediumBold();
      height = im_font->h;
      width = im_font->w;
      if (ijustv_temp == 0) {                        /* Center */
         ixpos_temp = ixpos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         ixpos_temp = ixpos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         ixpos_temp = ixpos_temp;
      }
      if (ijusth_temp == 0) {                        /* Left */
         iypos_temp = iypos_temp;
      } else if (ijusth_temp == 1) {                 /* Center */
         iypos_temp = iypos_temp + len*(width/2);
      } else if (ijusth_temp == 2) {
         iypos_temp = iypos_temp + len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 4) {
      im_font = gdFontGetGiant();
      height = im_font->h;
      width = im_font->w;
      if (ijustv_temp == 0) {                        /* Center */
         ixpos_temp = ixpos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         ixpos_temp = ixpos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         ixpos_temp = ixpos_temp;
      }
      if (ijusth_temp == 0) {
         iypos_temp = iypos_temp;
      } else if (ijusth_temp == 1) {
         iypos_temp = iypos_temp + len*(width/2);
      } else if (ijusth_temp == 2) {
         iypos_temp = iypos_temp + len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         CURRENT_COLOR_GD);
      }
   } else if (ifontz_temp == 5) {
      im_font = gdFontGetTiny();
      height = im_font->h;
      width = im_font->w;
      if (ijustv_temp == 0) {                        /* Center */
         ixpos_temp = ixpos_temp - (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         ixpos_temp = ixpos_temp - height;
      } else if (ijustv_temp == 2) {                 /* Top */
         ixpos_temp = ixpos_temp;
      }
      if (ijusth_temp == 0) {
         iypos_temp = iypos_temp;
      } else if (ijusth_temp == 1) {
         iypos_temp = iypos_temp + len*(width/2);
      } else if (ijusth_temp == 2) {
         iypos_temp = iypos_temp + len*width;
      }
      if (COLOR_OPTION_GD == 0) {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         color_table[CURRENT_COLOR_GD]);
      } else {
         gdImageStringUp(im, im_font, ixpos_temp, iypos_temp, string2,
                         CURRENT_COLOR_GD);
      }
   } else {
      /*  Obtain brect to set the justification */
      err = gdImageStringFT(NULL, &brect[0],jcol_temp,font_name,ptsize,
                            0.,ixpos_temp,iypos_temp,string2);
      width = brect[2] - brect[0];
      height = brect[7] - brect[1];
      if (ijustv_temp == 0) {                        /* Center */
         ixpos_temp = ixpos_temp + (height/2);
      } else if (ijustv_temp == 1) {                 /* Bottom */
         ixpos_temp = ixpos_temp;
      } else if (ijustv_temp == 2) {                 /* Top */
         ixpos_temp = ixpos_temp + height;
      }
      if (ijusth_temp == 0) {                       /* Left */
         iypos_temp = iypos_temp;
      } else if (ijusth_temp == 1) {                /* Center */
         iypos_temp = iypos_temp + (width/2);
      } else if (ijusth_temp == 2) {                /* Top */
         iypos_temp = iypos_temp + width;
      }
      if (COLOR_OPTION_GD == 0) {
         err = gdImageStringFT(im, &brect[0],color_table[CURRENT_COLOR_GD],
               font_name,ptsize,1.5708,ixpos_temp,iypos_temp,string2);
      } else {
         err = gdImageStringFT(im, &brect[0],CURRENT_COLOR_GD,
               font_name,ptsize,1.5708,ixpos_temp,iypos_temp,string2);
      }
   }

}

/* GDLOAD   - routine to open an existing JPEG/PNG/GIF/BMP image.
 *            This is used for reading existing images as
 *            oppossed to creating images.  Use a different image
 *            pointer to avoid potential conflicts with
 *            Dataplot creating an image.
 *
 *            The GDRROW routine is used to read one row of the
 *            image and the GDUNLO routine is used to close
 *            the image that is open for reading.
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdload_(itype, xpixels, ypixels, file_name, ierror)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDLOAD_(itype, xpixels, ypixels, file_name, ierror)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdload(itype, xpixels, ypixels, file_name, ierror)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDLOAD(itype, xpixels, ypixels, file_name, ierror)
#endif
#if INTEGER_PRECISION == 0
int  *xpixels, *ypixels;
int  *itype;
int  *ierror;
int file_name[];
#else
int  xpixels[2], ypixels[2];
int  itype[2];
int  ierror[2];
int file_name[];
#endif
{

   int   xpixels_temp, ypixels_temp;
   int   itype_temp;
   int   ierror_temp;
   int   len;
   int   itemp, quality;
   int   i;
   int   itemp1, itemp2;

#if INTEGER_PRECISION == 0
   xpixels_temp = *xpixels;
   ypixels_temp = *ypixels;
   itype_temp = *itype;
   ierror_temp = *ierror;
#else
   xpixels_temp = xpixels[0];
   ypixels_temp = ypixels[0];
   itype_temp = itype[0];
   ierror_temp = ierror[0];
#endif

#if INTEGER_PRECISION == 0
     i_to_s_2(file_name, file_string_2, 80, &len);
#else
     i_to_s_2(file_name, file_string_2, 160, &len);
#endif
     jpegin = fopen(file_string_2,"rb");
     if (!jpegin) {
        ierror_temp = 1;                /*  Unable to open file */
     } else {
        /* Open image. */
        /* Following supported:
         *
         *   1  - JPEG
         *   2  - PNG
         *   3  - GIF
         *   4  - BMP
         *   5  - WBMP
         *   6  - WEBP
         *   7  - TGA (or TARGA)
         *   8  - TIFF
         *   9  - XPM (X Pixmap)
         */
        if (itype_temp == 1) {
           im2 = gdImageCreateFromJpeg(jpegin);
        } else if (itype_temp == 2) {
           im2 = gdImageCreateFromPng(jpegin);
        } else if (itype_temp == 3) {
           im2 = gdImageCreateFromGif(jpegin);
        } else if (itype_temp == 4) {
#ifdef HAVE_GD_BMP
           im2 = gdImageCreateFromBmp(jpegin);
#endif
        } else if (itype_temp == 5) {
           im2 = gdImageCreateFromWBMP(jpegin);
        } else if (itype_temp == 6) {
#ifdef HAVE_GD_VPX
           im2 = gdImageCreateFromWBMP(jpegin);
#endif
        } else if (itype_temp == 7) {
#ifdef HAVE_GD_TGA
           im2 = gdImageCreateFromTga(jpegin);
#endif
        } else if (itype_temp == 8) {
#ifdef HAVE_GD_TIFF
           im2 = gdImageCreateFromTiff(jpegin);
#endif
        } else if (itype_temp == 9) {
           /* File name has to be specified differently
            * for this device.
            * im2 = gdImageCreateFromXpm(jpegin);
           */
        }
        fclose(jpegin);
        if (!im2) {
           ierror_temp = 2;
        } else {
           xpixels_temp = im2->sx;
           ypixels_temp = im2->sy;
        }
     }
#if INTEGER_PRECISION == 0
     *ierror = ierror_temp;
     *xpixels = xpixels_temp;
     *ypixels = ypixels_temp;
#else
     ierror[0] = ierror_temp;
     xpixels[0] = xpixels_temp;
     ypixels[0] = ypixels_temp;
#endif

}

/* GDUNLO  - routine to unload an image that was loaded for reading.
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdunlo_()
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDUNLO_()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdunlo()
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDUNLO()
#endif

{
   gdImageDestroy(im2);
}

/* GDPIXE - read a specified pixel position
 *
 * ix     - contains the x coordinate
 * iy     - contains the y coordinate
 * ired   - value of red component
 * iblue  - value of blue component
 * igreen - value of green component
 *
 */
#if APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 1
void gdpixe_(ix, iy, ired, igreen, iblue)
#elif APPEND_UNDERSCORE == 1 && SUBROUTINE_CASE == 0
void GDPIXE_(ix, iy, ired, igreen, iblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 1
void gdpixe(ix, iy, ired, igreen, iblue)
#elif APPEND_UNDERSCORE == 0 && SUBROUTINE_CASE == 0
void GDPIXE(ix, iy, ired, igreen, iblue)
#endif
#if INTEGER_PRECISION == 0
int   *ix, *iy, *ired, *igreen, *iblue;
#else
int   ix[2], iy[2], ired[2], igreen[2], iblue[2];
#endif

{
   int ix_temp;
   int iy_temp;
   int ired_temp;
   int igreen_temp;
   int iblue_temp;
   int jcol;

#if INTEGER_PRECISION == 0
   ix_temp = *ix;
   iy_temp = *iy;
#else
   ix_temp = ix[0];
   iy_temp = iy[0];
#endif

   jcol = gdImageGetPixel(im2, ix_temp, iy_temp);
   /* jcol = gdImageGetPixel(im2, iy_temp, ix_temp); */
   ired_temp = gdImageRed(im2,jcol);
   igreen_temp = gdImageGreen(im2,jcol);
   iblue_temp = gdImageBlue(im2,jcol);

#if INTEGER_PRECISION == 0
   *ired = ired_temp;
   *igreen = igreen_temp;
   *iblue = iblue_temp;
#else
   ired[0] = ired_temp;
   igreen[0] = igreen_temp;
   iblue[0] = iblue_temp;
#endif

}

/* i_to_s_2  - utitlity routine to convert an integer array containing
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
void i_to_s_2(string1, string2, maxlen, ilen)
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

/************************************************************************
 *
 *  display.c, X11 interface for tmndecode (H.263 decoder)
 *  Copyright (C) 1996  Telenor R&D, Norway
 *        Karl Olav Lillevold <Karl.Lillevold@nta.no>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Karl Olav Lillevold               <Karl.Lillevold@nta.no>
 *  Telenor Research and Development
 *  P.O.Box 83                        tel.:   +47 63 84 84 00
 *  N-2007 Kjeller, Norway            fax.:   +47 63 81 00 76
 *
 *  Robert Danielsen                  e-mail: Robert.Danielsen@nta.no
 *  Telenor Research and Development  www:    http://www.nta.no/brukere/DVC/
 *  P.O.Box 83                        tel.:   +47 63 84 84 00
 *  N-2007 Kjeller, Norway            fax.:   +47 63 81 00 76
 *  
 ************************************************************************/

/*
 * based on mpeg2decode, (C) 1994, MPEG Software Simulation Group
 * and mpeg2play, (C) 1994 Stefan Eckart
 *                         <stefan@lis.e-technik.tu-muenchen.de>
 *
 */

 /* the Xlib interface is closely modeled after
  * mpeg_play 2.0 by the Berkeley Plateau Research Group
  */

#ifdef DISPLAY

#include <stdio.h>
#include <stdlib.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "config.h"
#include "tmndec.h"
#include "global.h"

/* private prototypes */
static void display_image _ANSI_ARGS_((XImage *ximage, unsigned char *dithered_image));

/* display related data */
unsigned long wpixel[3];
static unsigned char *dithered_image;

/* X11 related variables */
static Display *display;
static Window window;
static GC gc;
static int dpy_depth;

XImage *ximage;

unsigned char pixel[256];

#ifdef SH_MEM

#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

extern int XShmQueryExtension _ANSI_ARGS_((Display *dpy));
extern int XShmGetEventBase _ANSI_ARGS_((Display *dpy));

static int HandleXError _ANSI_ARGS_((Display *dpy, XErrorEvent *event));
static void InstallXErrorHandler _ANSI_ARGS_((void));
static void DeInstallXErrorHandler _ANSI_ARGS_((void));

static int shmem_flag;
static XShmSegmentInfo shminfo1, shminfo2;
static int gXErrorFlag;
static int CompletionType = -1;

static int HandleXError(dpy, event)
Display *dpy;
XErrorEvent *event;
{
  gXErrorFlag = 1;

  return 0;
}

static void InstallXErrorHandler()
{
  XSetErrorHandler(HandleXError);
  XFlush(display);
}

static void DeInstallXErrorHandler()
{
  XSetErrorHandler(NULL);
  XFlush(display);
}

#endif

/* connect to server, create and map window,
 * allocate colors and (shared) memory
 */
void init_display(name)
char *name;
{
  int crv, cbu, cgu, cgv;
  int y, u, v, r, g, b;
  int i;
  char dummy;
  int screen;
  Visual *visual;
  int dpy_class;
  Colormap cmap;
  int private;
  XColor xcolor;
  unsigned int fg, bg;
  char *hello = "H.263 Display";
  XSizeHints hint;
  XEvent xev;
  XSetWindowAttributes xswa;
  unsigned long tmp_pixel;
  unsigned int mask;

  display = XOpenDisplay(name);

  if (display == NULL)
    error("Can not open display\n");

  screen = DefaultScreen(display);

  visual = DefaultVisual (display, screen);
  dpy_depth = DefaultDepth (display, screen);
  dpy_class = visual->class;

  if (!((dpy_class == TrueColor && dpy_depth == 32)
        || (dpy_class == TrueColor && dpy_depth == 24)
        || (dpy_class == TrueColor && dpy_depth == 16)
        || (dpy_class == PseudoColor && dpy_depth == 8)))
    error ("requires 8 bit PseudoColor or 16/24/32 bit TrueColor display\n");

  if (dpy_class == TrueColor && dpy_depth == 32)
    printf("TrueColor : 32 bit colordepth\n");
  if (dpy_class == TrueColor && dpy_depth == 24)
    printf("TrueColor : 24 bit colordepth\n");
  if (dpy_class == TrueColor && dpy_depth == 16)
    printf("TrueColor : 16 bit colordepth\n");
  if (dpy_class == PseudoColor && dpy_depth == 8)
    printf("PseudoColor : 8 bit colordepth, 4x4 ordered dither\n");

  /* width and height of the display window */
  if (expand) {
    hint.min_width = hint.max_width = hint.width = 2*horizontal_size;
    hint.min_height = hint.max_height = hint.height = 2*vertical_size;
  }
  else {
    hint.min_width = hint.max_width = hint.width = horizontal_size;
    hint.min_height = hint.max_height = hint.height = vertical_size;
  }

  hint.flags = PSize | PMinSize | PMaxSize;

  /* Get some colors */

  bg = WhitePixel (display, screen);
  fg = BlackPixel (display, screen);

  /* Make the window */
  mask = CWBackPixel | CWBorderPixel;
  if (dpy_depth == 32 || dpy_depth == 24 || dpy_depth == 16) {
    mask |= CWColormap;
    xswa.colormap = XCreateColormap (display, DefaultRootWindow (display),
             visual, AllocNone);
  }
  xswa.background_pixel = bg;
  xswa.border_pixel = fg;
  window = XCreateWindow (display, DefaultRootWindow (display),
          hint.x, hint.y, hint.width, hint.height,
          1, dpy_depth, InputOutput, visual, mask, &xswa);


  XSelectInput(display, window, StructureNotifyMask);

  /* Tell other applications about this window */

  XSetStandardProperties (display, window, hello, hello, None, NULL, 0, &hint);

  /* Map window. */

  XMapWindow(display, window);

  /* Wait for map. */
  do
  {
    XNextEvent(display, &xev);
  }
  while (xev.type != MapNotify || xev.xmap.event != window);

  XSelectInput(display, window, NoEventMask);

  /* allocate colors */

  gc = DefaultGC(display, screen);

  if (dpy_depth == 8) {
    XWindowAttributes xwa;

    cmap = DefaultColormap(display, screen);
    private = 0;

    /* matrix coefficients */
    crv = convmat[matrix_coefficients][0];
    cbu = convmat[matrix_coefficients][1];
    cgu = convmat[matrix_coefficients][2];
    cgv = convmat[matrix_coefficients][3];

    /* color allocation:
     * i is the (internal) 8 bit color number, it consists of separate
     * bit fields for Y, U and V: i = (yyyyuuvv), we don't use yyyy=0000
     * yyyy=0001 and yyyy=1111, this leaves 48 colors for other applications
     *
     * the allocated colors correspond to the following Y, U and V values:
     * Y:   40, 56, 72, 88, 104, 120, 136, 152, 168, 184, 200, 216, 232
     * U,V: -48, -16, 16, 48
     *
     * U and V values span only about half the color space; this gives
     * usually much better quality, although highly saturated colors can
     * not be displayed properly
     *
     * translation to R,G,B is implicitly done by the color look-up table
     */
    for (i=32; i<240; i++) {
      /* color space conversion */
      y = 16*((i>>4)&15) + 8;
      u = 32*((i>>2)&3)  - 48;
      v = 32*(i&3)       - 48;

      y = 76309 * (y - 16); /* (255/219)*65536 */
      
      r = clp[(y + crv*v + 32768)>>16];
      g = clp[(y - cgu*u -cgv*v + 32768)>>16];
      b = clp[(y + cbu*u + 32786)>>16];

      /* X11 colors are 16 bit */
      xcolor.red   = r << 8;
      xcolor.green = g << 8;
      xcolor.blue  = b << 8;

      if (XAllocColor(display, cmap, &xcolor) != 0)
        pixel[i] = xcolor.pixel;
      else {
        /* allocation failed, have to use a private colormap */

        if (private)
          error("Couldn't allocate private colormap");

        private = 1;

        if (!quiet)
          fprintf(stderr, "Using private colormap (%d colors were "
          "available).\n", i-32);

        /* Free colors. */
        while (--i >= 32)
        {
          tmp_pixel = pixel[i]; /* because XFreeColors expects unsigned long */
          XFreeColors(display, cmap, &tmp_pixel, 1, 0);
        }

        /* i is now 31, this restarts the outer loop */

        /* create private colormap */

        XGetWindowAttributes(display, window, &xwa);
        cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
        XSetWindowColormap(display, window, cmap);
      }
    }
  }

#ifdef SH_MEM
  if (XShmQueryExtension(display))
    shmem_flag = 1;
  else
  {
    shmem_flag = 0;
    if (!quiet)
      fprintf(stderr, "Shared memory not supported\nReverting to normal "
              "Xlib\n");
  }

  if (shmem_flag)
    CompletionType = XShmGetEventBase(display) + ShmCompletion;

  InstallXErrorHandler();

  if (shmem_flag)
  {

    if (expand)
      ximage = XShmCreateImage(display, visual, dpy_depth, ZPixmap, NULL,
               &shminfo1,
               2*coded_picture_width, 2*coded_picture_height);
    else
      ximage = XShmCreateImage(display, visual, dpy_depth, ZPixmap, NULL,
               &shminfo1,
               coded_picture_width, coded_picture_height);
    

    /* If no go, then revert to normal Xlib calls. */

    if (ximage==NULL)
    {
      if (ximage!=NULL)
        XDestroyImage(ximage);
      if (!quiet)
        fprintf(stderr, "Shared memory error, disabling (Ximage error)\n");
      goto shmemerror;
    }

    /* Success here, continue. */

    shminfo1.shmid = shmget(IPC_PRIVATE, 
                            ximage->bytes_per_line * ximage->height,
                            IPC_CREAT | 0777);

    if (shminfo1.shmid<0)
    {
      XDestroyImage(ximage);
      if (!quiet)
        fprintf(stderr, "Shared memory error, disabling (seg id error)\n");
      goto shmemerror;
    }

    shminfo1.shmaddr = (char *) shmat(shminfo1.shmid, 0, 0);
    shminfo2.shmaddr = (char *) shmat(shminfo2.shmid, 0, 0);

    if (shminfo1.shmaddr==((char *) -1))
    {
      XDestroyImage(ximage);
      if (shminfo1.shmaddr!=((char *) -1))
        shmdt(shminfo1.shmaddr);
      if (!quiet)
      {
        fprintf(stderr, "Shared memory error, disabling (address error)\n");
      }
      goto shmemerror;
    }

    ximage->data = shminfo1.shmaddr;
    dithered_image = (unsigned char *)ximage->data;
    shminfo1.readOnly = False;
    XShmAttach(display, &shminfo1);

    XSync(display, False);

    if (gXErrorFlag)
    {
      /* Ultimate failure here. */
      XDestroyImage(ximage);
      shmdt(shminfo1.shmaddr);
      if (!quiet)
        fprintf(stderr, "Shared memory error, disabling.\n");
      gXErrorFlag = 0;
      goto shmemerror;
    }
    else
    {
      shmctl(shminfo1.shmid, IPC_RMID, 0);
    }

    if (!quiet)
    {
      fprintf(stderr, "Sharing memory.\n");
    }
  }
  else
  {
shmemerror:
    shmem_flag = 0;
#endif


    if (expand) {
      ximage = XCreateImage(display,visual,dpy_depth,ZPixmap,0,&dummy,
            2*coded_picture_width,2*coded_picture_height,8,0);
      if (!(dithered_image =
            (unsigned char *)malloc(coded_picture_width*coded_picture_height*
            (dpy_depth > 8 ? sizeof (int)*4 : 
             sizeof (unsigned char))*4)))
        error("malloc failed");
    }
    else {
      ximage = XCreateImage(display,visual,dpy_depth,ZPixmap,0,&dummy,
            coded_picture_width,coded_picture_height,8,0);
      if (!(dithered_image =
            (unsigned char *)malloc(coded_picture_width*coded_picture_height*
            (dpy_depth > 8 ? sizeof (int) : 
             sizeof (unsigned char)))))
        error("malloc failed");
    }

#ifdef SH_MEM
  }

  DeInstallXErrorHandler();
#endif


  if (dpy_depth == 32 || dpy_depth == 24 || dpy_depth == 16) {
    XWindowAttributes xwa;

    XGetWindowAttributes(display, window, &xwa);


    wpixel[0] = xwa.visual->red_mask;
    wpixel[1] = xwa.visual->green_mask;
    wpixel[2] = xwa.visual->blue_mask;

    /* If the colors in 16/24/32-bit mode are wrong, try this instead
       of the above three lines */
    /*
    wpixel[2] = xwa.visual->red_mask;
    wpixel[1] = xwa.visual->green_mask;
    wpixel[0] = xwa.visual->blue_mask;
    */

    InitColorDither(dpy_depth == 24 || dpy_depth == 32);
  }
  else {
    ord4x4_dither_init ();
  }
}

void exit_display()
{
#ifdef SH_MEM
  if (shmem_flag)
  {
    XShmDetach(display, &shminfo1);
    XDestroyImage(ximage);
    shmdt(shminfo1.shmaddr);
  }
#endif
}

static void display_image(ximage,dithered_image)
XImage *ximage;
unsigned char *dithered_image;
{
  int t = 1;

 /* Always work in native bit and byte order. This tells Xlib to
    reverse bit and byte order if necessary when crossing a
    network. Frankly, this part of XImages is somewhat
    underdocumented, so this may not be exactly correct.  */

  if (*(char *)&t == 1) {
    ximage->byte_order = LSBFirst;
    ximage->bitmap_bit_order = LSBFirst;
  }
  else {
    ximage->byte_order = MSBFirst;
    ximage->bitmap_bit_order = MSBFirst;
  }    

  /* display dithered image */
#ifdef SH_MEM
  if (shmem_flag)
  {
    XShmPutImage(display, window, gc, ximage, 
                 0, 0, 0, 0, ximage->width, ximage->height, True);
    XFlush(display);
      
    while (1)
    {
      XEvent xev;
        
      XNextEvent(display, &xev);
      if (xev.type == CompletionType)
        break;
    }
  }
  else 
#endif
  {
    ximage->data = (char *) dithered_image; 
    XPutImage(display, window, gc, ximage, 0, 0, 0, 0, ximage->width, ximage->height);
  }
}


void dither(src)
unsigned char *src[];
{
  if (dpy_depth == 24 || dpy_depth == 32) {
    if (ximage->bits_per_pixel == 24)
      ConvertYUVtoRGB(src[0],src[1],src[2], dithered_image,
		      coded_picture_width,
		      coded_picture_height);
    else
      Color32DitherImage(src, dithered_image);
  }  
  else if (dpy_depth == 16) {
    Color16DitherImage(src, dithered_image);
  }
  else {
    ord4x4_dither_frame (src, dithered_image);
  }

  display_image (ximage, dithered_image);
}

#endif

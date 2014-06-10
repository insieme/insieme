/************************************************************************
 *
 *  store.c, picture output routines for tmndecode (H.263 decoder)
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#ifdef WIN32
#include <io.h>
#endif

#include "config.h"
#include "tmndec.h"
#include "global.h"
#ifdef WINDOWS
int displayImage (unsigned char *lum, unsigned char *Cr, unsigned char *Cb);
#endif

/* private prototypes */
static void store_one _ANSI_ARGS_((char *outname, unsigned char *src[],
  int offset, int incr, int height));
static void store_yuv _ANSI_ARGS_((char *outname, unsigned char *src[],
  int offset, int incr, int height));
static void store_yuv_append _ANSI_ARGS_((char *outname, unsigned char *src[],
  int offset, int incr, int height));
static void store_sif _ANSI_ARGS_((char *outname, unsigned char *src[],
  int offset, int incr, int height));
static void store_ppm_tga _ANSI_ARGS_((char *outname, unsigned char *src[],
  int offset, int incr, int height, int tgaflag));
static void store_yuv1 _ANSI_ARGS_((char *name, unsigned char *src,
  int offset, int incr, int width, int height, int append));
static void putbyte _ANSI_ARGS_((int c));
static void putword _ANSI_ARGS_((int w));
static void conv422to444 _ANSI_ARGS_((unsigned char *src, unsigned char *dst));
static void conv420to422 _ANSI_ARGS_((unsigned char *src, unsigned char *dst));

#define OBFRSIZE 4096
static unsigned char obfr[OBFRSIZE];
static unsigned char *optr;
static int outfile;

/*
 * store a picture as either one frame or two fields
 */
void storeframe(src,frame)
unsigned char *src[];
int frame;
{
  char outname[32];

  /* progressive */
  sprintf(outname,outputname,frame,'f');
  store_one(outname,src,0,coded_picture_width,vertical_size);
}

/*
 * store one frame or one field
 */
static void store_one(outname,src,offset,incr,height)
char *outname;
unsigned char *src[];
int offset, incr, height;
{
  switch (outtype)
  {
  case T_YUV:
    store_yuv(outname,src,offset,incr,height);
    break;
  case T_YUV_CONC:
    store_yuv_append(outname,src,offset,incr,height);
    break;
  case T_SIF:
    store_sif(outname,src,offset,incr,height);
    break;
  case T_TGA:
    store_ppm_tga(outname,src,offset,incr,height,1);
    break;
  case T_PPM:
    store_ppm_tga(outname,src,offset,incr,height,0);
    break;
#ifdef DISPLAY
  case T_X11:
    dither(src);
    break;
#endif
#ifdef WINDOWS
  case T_WIN:
    displayImage(src[0],src[2],src[1]);
    break;
#endif
  default:
    break;
  }
}

/* separate headerless files for y, u and v */
static void store_yuv(outname,src,offset,incr,height)
char *outname;
unsigned char *src[];
int offset,incr,height;
{
  int hsize;
  char tmpname[32];

  hsize = horizontal_size;

  sprintf(tmpname,"%s.Y",outname);
  store_yuv1(tmpname,src[0],offset,incr,hsize,height,0);

  offset>>=1; incr>>=1; hsize>>=1;
  height>>=1;

  sprintf(tmpname,"%s.U",outname);
  store_yuv1(tmpname,src[1],offset,incr,hsize,height,0);

  sprintf(tmpname,"%s.V",outname);
  store_yuv1(tmpname,src[2],offset,incr,hsize,height,0);
}

/* concatenated headerless file for y, u and v */
static void store_yuv_append(outname,src,offset,incr,height)
char *outname;
unsigned char *src[];
int offset,incr,height;
{
  int hsize;
  hsize = horizontal_size;

  store_yuv1(outname,src[0],offset,incr,hsize,height,1);

  offset>>=1; incr>>=1; hsize>>=1;
  height>>=1;

  store_yuv1(outname,src[1],offset,incr,hsize,height,1);
  store_yuv1(outname,src[2],offset,incr,hsize,height,1);
}

/* auxiliary routine */
static void store_yuv1(name,src,offset,incr,width,height,append)
char *name;
unsigned char *src;
int offset,incr,width,height,append;
{
  int i, j;
  unsigned char *p;

  if (append) {
    if ((outfile = open(name,O_APPEND|O_WRONLY|O_BINARY,0666))==-1) {
      sprintf(errortext,"Couldn't append to %s\n",name);
      error(errortext);
    }
  }
  else {
    if ((outfile = open(name,O_CREAT|O_TRUNC|O_WRONLY|O_BINARY,0666))==-1) {
      sprintf(errortext,"Couldn't create %s\n",name);
      error(errortext);
    }
  }
  if (!quiet)
    fprintf(stderr,"saving %s\n",name);

  optr=obfr;

  for (i=0; i<height; i++) {
    p = src + offset + incr*i;
    for (j=0; j<width; j++)
      putbyte(*p++);
  }

  if (optr!=obfr)
    write(outfile,obfr,optr-obfr);

  close(outfile);
}

/*
 * store as headerless file in U,Y,V,Y format
 */
static void store_sif (outname,src,offset,incr,height)
char *outname;
unsigned char *src[];
int offset, incr, height;
{
  int i,j;
  unsigned char *py, *pu, *pv;
  static unsigned char *u422, *v422;

  if (!u422) {
    if (!(u422 = (unsigned char *)malloc((coded_picture_width>>1)
         *coded_picture_height)))
      error("malloc failed");
    if (!(v422 = (unsigned char *)malloc((coded_picture_width>>1)
         *coded_picture_height)))
      error("malloc failed");
  }
  
  conv420to422(src[1],u422);
  conv420to422(src[2],v422);

  strcat(outname,".SIF");

  if (!quiet)
    fprintf(stderr,"saving %s\n",outname);

  if ((outfile = open(outname,O_CREAT|O_TRUNC|O_WRONLY|O_BINARY,0666))==-1)
  {
    sprintf(errortext,"Couldn't create %s\n",outname);
    error(errortext);
  }

  optr = obfr;

  for (i=0; i<height; i++)
  {
    py = src[0] + offset + incr*i;
    pu = u422 + (offset>>1) + (incr>>1)*i;
    pv = v422 + (offset>>1) + (incr>>1)*i;

    for (j=0; j<horizontal_size; j+=2)
    {
      putbyte(*pu++);
      putbyte(*py++);
      putbyte(*pv++);
      putbyte(*py++);
    }
  }

  if (optr!=obfr)
    write(outfile,obfr,optr-obfr);

  close(outfile);
}

/*
 * store as PPM (PBMPLUS) or uncompressed Truevision TGA ('Targa') file
 */
static void store_ppm_tga(outname,src,offset,incr,height,tgaflag)
char *outname;
unsigned char *src[];
int offset, incr, height;
int tgaflag;
{
  int i, j;
  int y, u, v, r, g, b;
  int crv, cbu, cgu, cgv;
  unsigned char *py, *pu, *pv;
  static unsigned char tga24[14] = {0,0,2,0,0,0,0, 0,0,0,0,0,24,32};
  char header[32];
  static unsigned char *u422, *v422, *u444, *v444;

  if (!u444) {
    if (!(u422 = (unsigned char *)malloc((coded_picture_width>>1)
         *coded_picture_height)))
      error("malloc failed");
    if (!(v422 = (unsigned char *)malloc((coded_picture_width>>1)
         *coded_picture_height)))
      error("malloc failed");

    if (!(u444 = (unsigned char *)malloc(coded_picture_width
         *coded_picture_height)))
      error("malloc failed");

    if (!(v444 = (unsigned char *)malloc(coded_picture_width
         *coded_picture_height)))
      error("malloc failed");
  }

  conv420to422(src[1],u422);
  conv420to422(src[2],v422);
  conv422to444(u422,u444);
  conv422to444(v422,v444);

  strcat(outname,tgaflag ? ".tga" : ".ppm");

  if (!quiet)
    fprintf(stderr,"saving %s\n",outname);

  if ((outfile = open(outname,O_CREAT|O_TRUNC|O_WRONLY|O_BINARY,0666))==-1)
    {
      sprintf(errortext,"Couldn't create %s\n",outname);
      error(errortext);
    }

  optr = obfr;

  if (tgaflag) {
    /* TGA header */
    for (i=0; i<12; i++)
      putbyte(tga24[i]);

    putword(horizontal_size); putword(height);
    putbyte(tga24[12]); putbyte(tga24[13]);
  }
  else {
    /* PPM header */
    sprintf(header,"P6\n%d %d\n255\n",horizontal_size,height);

    for (i=0; header[i]!=0; i++)
      putbyte(header[i]);
  }

  /* matrix coefficients */
  crv = convmat[matrix_coefficients][0];
  cbu = convmat[matrix_coefficients][1];
  cgu = convmat[matrix_coefficients][2];
  cgv = convmat[matrix_coefficients][3];
  
  for (i=0; i<height; i++) {
    py = src[0] + offset + incr*i;
    pu = u444 + offset + incr*i;
    pv = v444 + offset + incr*i;

    for (j=0; j<horizontal_size; j++) {
      u = *pu++ - 128;
      v = *pv++ - 128;
      y = 76309 * (*py++ - 16); /* (255/219)*65536 */
      r = clp[(y + crv*v + 32768)>>16];
      g = clp[(y - cgu*u - cgv*v + 32768)>>16];
      b = clp[(y + cbu*u + 32786)>>16];

      if (tgaflag) {
        putbyte(b); putbyte(g); putbyte(r);
      }
      else {
        putbyte(r); putbyte(g); putbyte(b);
      }
    }
  }

  if (optr!=obfr)
    write(outfile,obfr,optr-obfr);

  close(outfile);
}

static void putbyte(c)
int c;
{
  *optr++ = c;

  if (optr == obfr+OBFRSIZE)
  {
    write(outfile,obfr,OBFRSIZE);
    optr = obfr;
  }
}

static void putword(w)
int w;
{
  putbyte(w); putbyte(w>>8);
}

/* horizontal 1:2 interpolation filter */
static void conv422to444(src,dst)
unsigned char *src,*dst;
{
  int i, i2, w, j, im3, im2, im1, ip1, ip2, ip3;

  w = coded_picture_width>>1;

  for (j=0; j<coded_picture_height; j++) {
    for (i=0; i<w; i++) {

      i2 = i<<1;
      im3 = (i<3) ? 0 : i-3;
      im2 = (i<2) ? 0 : i-2;
      im1 = (i<1) ? 0 : i-1;
      ip1 = (i<w-1) ? i+1 : w-1;
      ip2 = (i<w-2) ? i+2 : w-1;
      ip3 = (i<w-3) ? i+3 : w-1;

      /* FIR filter coefficients (*256): 5 -21 70 228 -37 11 */
      dst[i2] =   clp[(int)(  5*src[im3]
              -21*src[im2]
              +70*src[im1]
              +228*src[i]
              -37*src[ip1]
              +11*src[ip2]+128)>>8];

      dst[i2+1] = clp[(int)(  5*src[ip3]
              -21*src[ip2]
              +70*src[ip1]
              +228*src[i]
              -37*src[im1]
              +11*src[im2]+128)>>8];
    }
    src+= w;
    dst+= coded_picture_width;
  }
}

/* vertical 1:2 interpolation filter */
static void conv420to422(src,dst)
unsigned char *src,*dst;
{
  int w, h, i, j, j2;
  int jm3, jm2, jm1, jp1, jp2, jp3;

  w = coded_picture_width>>1;
  h = coded_picture_height>>1;

  /* intra frame */
  for (i=0; i<w; i++) {
    for (j=0; j<h; j++) {
      j2 = j<<1;
      jm3 = (j<3) ? 0 : j-3;
      jm2 = (j<2) ? 0 : j-2;
      jm1 = (j<1) ? 0 : j-1;
      jp1 = (j<h-1) ? j+1 : h-1;
      jp2 = (j<h-2) ? j+2 : h-1;
      jp3 = (j<h-3) ? j+3 : h-1;
      
      /* FIR filter coefficients (*256): 5 -21 70 228 -37 11 */
      /* New FIR filter coefficients (*256): 3 -16 67 227 -32 7 */
      dst[w*j2] =     clp[(int)(  3*src[w*jm3]
          -16*src[w*jm2]
          +67*src[w*jm1]
          +227*src[w*j]
          -32*src[w*jp1]
          +7*src[w*jp2]+128)>>8];

      dst[w*(j2+1)] = clp[(int)(  3*src[w*jp3]
          -16*src[w*jp2]
          +67*src[w*jp1]
          +227*src[w*j]
          -32*src[w*jm1]
          +7*src[w*jm2]+128)>>8];
    }
    src++;
    dst++;
  }
}


/************************************************************************
 *
 *  yuv2rgb.c, colour space conversion for tmndecode (H.263 decoder)
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

/* This software was written by Erik Corry, who agreed to place this
 * file under the same licensing terms as the rest of the tmndec
 * software.  See the accompanying COPYING file for details.  The
 * original copyright notice from Erik Corry is included below for
 * completeness.  */
/*
 * Copyright (c) 1995 Erik Corry
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL ERIK CORRY BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF
 * THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF ERIK CORRY HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * ERIK CORRY SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS"
 * BASIS, AND ERIK CORRY HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT,
 * UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */


#include "config.h"
#include "tmndec.h"
#include "global.h"

#ifdef DISPLAY
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif


#ifdef DISPLAY

#undef INTERPOLATE

/*
 * Erik Corry's multi-byte dither routines.
 *
 * The basic idea is that the Init generates all the necessary tables.
 * The tables incorporate the information about the layout of pixels
 * in the XImage, so that it should be able to cope with 15-bit, 16-bit
 * 24-bit (non-packed) and 32-bit (10-11 bits per color!) screens.
 * At present it cannot cope with 24-bit packed mode, since this involves
 * getting down to byte level again. It is assumed that the bits for each
 * color are contiguous in the longword.
 * 
 * Writing to memory is done in shorts or ints. (Unfortunately, short is not
 * very fast on Alpha, so there is room for improvement here). There is no
 * dither time check for overflow - instead the tables have slack at
 * each end. This is likely to be faster than an 'if' test as many modern
 * architectures are really bad at ifs. Potentially, each '&&' causes a 
 * pipeline flush!
 *
 * There is no shifting and fixed point arithmetic, as I really doubt you
 * can see the difference, and it costs. This may be just my bias, since I
 * heard that Intel is really bad at shifting.
 */

/* Gamma correction stuff */

#define GAMMA_CORRECTION(x) ((int)(pow((x) / 255.0, 1.0 / gammaCorrect) * 255.0))
#define CHROMA_CORRECTION256(x) ((x) >= 128 \
                    ? 128 + mmin(127, (int)(((x) - 128.0) * chromaCorrect)) \
                    : 128 - mmin(128, (int)((128.0 - (x)) * chromaCorrect)))
#define CHROMA_CORRECTION128(x) ((x) >= 0 \
                    ? mmin(127,  (int)(((x) * chromaCorrect))) \
                    : mmax(-128, (int)(((x) * chromaCorrect))))
#define CHROMA_CORRECTION256D(x) ((x) >= 128 \
                    ? 128.0 + mmin(127.0, (((x) - 128.0) * chromaCorrect)) \
                    : 128.0 - mmin(128.0, (((128.0 - (x)) * chromaCorrect))))
#define CHROMA_CORRECTION128D(x) ((x) >= 0 \
                    ? mmin(127.0,  ((x) * chromaCorrect)) \
                    : mmax(-128.0, ((x) * chromaCorrect)))


/* Flag for gamma correction */
int gammaCorrectFlag = 0;
double gammaCorrect = 1.0;

/* Flag for chroma correction */
int chromaCorrectFlag = 0;
double chromaCorrect = 1.0;

/*
 * How many 1 bits are there in the longword.
 * Low performance, do not call often.
 */
static int
number_of_bits_set(a)
unsigned long a;
{
    if(!a) return 0;
    if(a & 1) return 1 + number_of_bits_set(a >> 1);
    return(number_of_bits_set(a >> 1));
}

/*
 * Shift the 0s in the least significant end out of the longword.
 * Low performance, do not call often.
 */
static unsigned long
shifted_down(a)
unsigned long a;
{
    if(!a) return 0;
    if(a & 1) return a;
    return a >> 1;
}

/*
 * How many 0 bits are there at most significant end of longword.
 * Low performance, do not call often.
 */
static int
free_bits_at_top(a)
unsigned long a;
{
      /* assume char is 8 bits */
    if(!a) return sizeof(unsigned long) * 8;
        /* assume twos complement */
    if(((long)a) < 0l) return 0;
    return 1 + free_bits_at_top ( a << 1);
}

/*
 * How many 0 bits are there at least significant end of longword.
 * Low performance, do not call often.
 */
static int
free_bits_at_bottom(a)
unsigned long a;
{
      /* assume char is 8 bits */
    if(!a) return sizeof(unsigned long) * 8;
    if(((long)a) & 1l) return 0;
    return 1 + free_bits_at_bottom ( a >> 1);
}

static int *L_tab, *Cr_r_tab, *Cr_g_tab, *Cb_g_tab, *Cb_b_tab;

/*
 * We define tables that convert a color value between -256 and 512
 * into the R, G and B parts of the pixel. The normal range is 0-255.
 */

static long *r_2_pix;
static long *g_2_pix;
static long *b_2_pix;
static long *r_2_pix_alloc;
static long *g_2_pix_alloc;
static long *b_2_pix_alloc;



/*
 *--------------------------------------------------------------
 *
 * InitColor16Dither --
 *
 *	To get rid of the multiply and other conversions in color
 *	dither, we use a lookup table.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The lookup tables are initialized.
 *
 *--------------------------------------------------------------
 */

void
InitColorDither(thirty2)
int thirty2;
{
    extern XImage *ximage;
    extern unsigned long wpixel[3];
    /*
     * misuse of the wpixel array for the pixel masks. Note that this
     * implies that the window is created before this routine is called
     */

     
    unsigned long red_mask = wpixel[0];
    unsigned long green_mask = wpixel[1];
    unsigned long blue_mask = wpixel[2];

    int CR, CB, i;

    if (ximage->bits_per_pixel == 24) /* not necessary in non-packed mode */
      init_dither_tab();

    L_tab    = (int *)malloc(256*sizeof(int)); 
    Cr_r_tab = (int *)malloc(256*sizeof(int));
    Cr_g_tab = (int *)malloc(256*sizeof(int));
    Cb_g_tab = (int *)malloc(256*sizeof(int));
    Cb_b_tab = (int *)malloc(256*sizeof(int));

    r_2_pix_alloc = (long *)malloc(768*sizeof(long));
    g_2_pix_alloc = (long *)malloc(768*sizeof(long));
    b_2_pix_alloc = (long *)malloc(768*sizeof(long));

    if (L_tab == NULL ||
        Cr_r_tab == NULL ||
        Cr_g_tab == NULL ||
        Cb_g_tab == NULL ||
        Cb_b_tab == NULL ||
        r_2_pix_alloc == NULL ||
        g_2_pix_alloc == NULL ||
        b_2_pix_alloc == NULL) {
      fprintf(stderr, "Could not get enough memory in InitColorDither\n");
      exit(1);
    }

    for (i=0; i<256; i++) {
      L_tab[i] = i;
      if (gammaCorrectFlag) {
        L_tab[i] = GAMMA_CORRECTION(i);
      }
      
      CB = CR = i;

      if (chromaCorrectFlag) {
        CB -= 128; 
        CB = CHROMA_CORRECTION128(CB);
        CR -= 128;
        CR = CHROMA_CORRECTION128(CR);
      } else {
        CB -= 128; CR -= 128;
      }
/* was
      Cr_r_tab[i] =  1.596 * CR;
      Cr_g_tab[i] = -0.813 * CR;
      Cb_g_tab[i] = -0.391 * CB;   
      Cb_b_tab[i] =  2.018 * CB;
  but they were just messed up.
  Then was (_Video Deymstified_):
      Cr_r_tab[i] =  1.366 * CR;
      Cr_g_tab[i] = -0.700 * CR;
      Cb_g_tab[i] = -0.334 * CB;   
      Cb_b_tab[i] =  1.732 * CB;
  but really should be:
   (from ITU-R BT.470-2 System B, G and SMPTE 170M )
*/
      Cr_r_tab[i] =  (0.419/0.299) * CR;
      Cr_g_tab[i] = -(0.299/0.419) * CR;
      Cb_g_tab[i] = -(0.114/0.331) * CB; 
      Cb_b_tab[i] =  (0.587/0.331) * CB;

/*
  though you could argue for:
    SMPTE 240M
      Cr_r_tab[i] =  (0.445/0.212) * CR;
      Cr_g_tab[i] = -(0.212/0.445) * CR;
      Cb_g_tab[i] = -(0.087/0.384) * CB; 
      Cb_b_tab[i] =  (0.701/0.384) * CB;
    FCC 
      Cr_r_tab[i] =  (0.421/0.30) * CR;
      Cr_g_tab[i] = -(0.30/0.421) * CR;
      Cb_g_tab[i] = -(0.11/0.331) * CB; 
      Cb_b_tab[i] =  (0.59/0.331) * CB;
    ITU-R BT.709 
      Cr_r_tab[i] =  (0.454/0.2125) * CR;
      Cr_g_tab[i] = -(0.2125/0.454) * CR;
      Cb_g_tab[i] = -(0.0721/0.386) * CB; 
      Cb_b_tab[i] =  (0.7154/0.386) * CB;
*/
    }

    /* 
     * Set up entries 0-255 in rgb-to-pixel value tables.
     */
    for (i = 0; i < 256; i++) {
      r_2_pix_alloc[i + 256] = i >> (8 - number_of_bits_set(red_mask));
      r_2_pix_alloc[i + 256] <<= free_bits_at_bottom(red_mask);
      g_2_pix_alloc[i + 256] = i >> (8 - number_of_bits_set(green_mask));
      g_2_pix_alloc[i + 256] <<= free_bits_at_bottom(green_mask);
      b_2_pix_alloc[i + 256] = i >> (8 - number_of_bits_set(blue_mask));
      b_2_pix_alloc[i + 256] <<= free_bits_at_bottom(blue_mask);
      /*
       * If we have 16-bit output depth, then we double the value
       * in the top word. This means that we can write out both
       * pixels in the pixel doubling mode with one op. It is 
       * harmless in the normal case as storing a 32-bit value
       * through a short pointer will lose the top bits anyway.
       * A similar optimisation for Alpha for 64 bit has been
       * prepared for, but is not yet implemented.
       */
      if(!thirty2) {

        r_2_pix_alloc[i + 256] |= (r_2_pix_alloc[i + 256]) << 16;
        g_2_pix_alloc[i + 256] |= (g_2_pix_alloc[i + 256]) << 16;
        b_2_pix_alloc[i + 256] |= (b_2_pix_alloc[i + 256]) << 16;

      }
#ifdef SIXTYFOUR_BIT
      if(thirty2) {

        r_2_pix_alloc[i + 256] |= (r_2_pix_alloc[i + 256]) << 32;
        g_2_pix_alloc[i + 256] |= (g_2_pix_alloc[i + 256]) << 32;
        b_2_pix_alloc[i + 256] |= (b_2_pix_alloc[i + 256]) << 32;

      }
#endif
    }

    /*
     * Spread out the values we have to the rest of the array so that
     * we do not need to check for overflow.
     */
    for (i = 0; i < 256; i++) {
      r_2_pix_alloc[i] = r_2_pix_alloc[256];
      r_2_pix_alloc[i+ 512] = r_2_pix_alloc[511];
      g_2_pix_alloc[i] = g_2_pix_alloc[256];
      g_2_pix_alloc[i+ 512] = g_2_pix_alloc[511];
      b_2_pix_alloc[i] = b_2_pix_alloc[256];
      b_2_pix_alloc[i+ 512] = b_2_pix_alloc[511];
    }

    r_2_pix = r_2_pix_alloc + 256;
    g_2_pix = g_2_pix_alloc + 256;
    b_2_pix = b_2_pix_alloc + 256;

}



/*
 *--------------------------------------------------------------
 *
 * Color16DitherImage --
 *
 *	Converts image into 16 bit color.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void 
Color16DitherImage(src, out)
unsigned char *src[];
unsigned char *out;
{
    unsigned char *lum = src[0];
    unsigned char *cb = src[1];
    unsigned char *cr = src[2];
    int cols;
    int rows;
    
    int L, CR, CB;
    unsigned short *row1, *row2;
    unsigned char *lum2;
    int x, y;
    int cr_r;
    int cr_g;
    int cb_g;
    int cb_b;
    int cols_2;

    cols = coded_picture_width;
    rows = coded_picture_height;
    if (expand) {
      cols *= 2;
      rows *= 2;
    }
    cols_2 = cols/2;

    row1 = (unsigned short *)out;
    row2 = row1 + cols_2 + cols_2;
    lum2 = lum + cols_2 + cols_2;

    for (y=0; y<rows; y+=2) {
        for (x=0; x<cols_2; x++) {
            int R, G, B;

            CR = *cr++;
            CB = *cb++;
            cr_r = Cr_r_tab[CR];
            cr_g = Cr_g_tab[CR];
            cb_g = Cb_g_tab[CB];
            cb_b = Cb_b_tab[CB];

            L = L_tab[(int) *lum++];

            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

#ifdef INTERPOLATE
            if(x != cols_2 - 1) {
              CR = (CR + *cr) >> 1;
              CB = (CB + *cb) >> 1;
              cr_r = Cr_r_tab[CR];
              cr_g = Cr_g_tab[CR];
              cb_g = Cb_g_tab[CB];
              cb_b = Cb_b_tab[CB];
            }
#endif

            L = L_tab[(int) *lum++];

            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

            /*
             * Now, do second row.
             */
#ifdef INTERPOLATE
            if(y != rows - 2) {
              CR = (CR + *(cr + cols_2 - 1)) >> 1;
              CB = (CB + *(cb + cols_2 - 1)) >> 1;
              cr_r = Cr_r_tab[CR];
              cr_g = Cr_g_tab[CR];
              cb_g = Cb_g_tab[CB];
              cb_b = Cb_b_tab[CB];
            }
#endif

            L = L_tab[(int) *lum2++];
            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

            L = L_tab[(int) *lum2++];
            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
        }
        /*
         * These values are at the start of the next line, (due
         * to the ++'s above),but they need to be at the start
         * of the line after that.
         */
        lum += cols_2 + cols_2;
        lum2 += cols_2 + cols_2;
        row1 += cols_2 + cols_2;
        row2 += cols_2 + cols_2;
    }
}




/*
 *--------------------------------------------------------------
 *
 * Color32DitherImage --
 *
 *	Converts image into 32 bit color (or 24-bit non-packed).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

/*
 * This is a copysoft version of the function above with ints instead
 * of shorts to cause a 4-byte pixel size
 */

void
Color32DitherImage(src, out)
unsigned char *src[];
unsigned char *out;
{
    unsigned char *lum = src[0];
    unsigned char *cb = src[1];
    unsigned char *cr = src[2];
    int cols;
    int rows;

    int L, CR, CB;
    unsigned int *row1, *row2;
    unsigned char *lum2;
    int x, y;
    int cr_r;
    int cr_g;
    int cb_g;
    int cb_b;
    int cols_2;

    cols = coded_picture_width;
    rows = coded_picture_height;
    if (expand) {
      cols *= 2;
      rows *= 2;
    }
    cols_2 = cols/2;

    row1 = (unsigned int *)out;
    row2 = row1 + cols_2 + cols_2;
    lum2 = lum + cols_2 + cols_2;
    for (y=0; y<rows; y+=2) {
        for (x=0; x<cols_2; x++) {
            int R, G, B;

            CR = *cr++;
            CB = *cb++;
            cr_r = Cr_r_tab[CR];
            cr_g = Cr_g_tab[CR];
            cb_g = Cb_g_tab[CB];
            cb_b = Cb_b_tab[CB];

            L = L_tab[(int) *lum++];

            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

#ifdef INTERPOLATE
            if(x != cols_2 - 1) {
              CR = (CR + *cr) >> 1;
              CB = (CB + *cb) >> 1;
              cr_r = Cr_r_tab[CR];
              cr_g = Cr_g_tab[CR];
              cb_g = Cb_g_tab[CB];
              cb_b = Cb_b_tab[CB];
            }
#endif

            L = L_tab[(int) *lum++];

            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

            /*
             * Now, do second row.
             */

#ifdef INTERPOLATE
            if(y != rows - 2) {
              CR = (CR + *(cr + cols_2 - 1)) >> 1;
              CB = (CB + *(cb + cols_2 - 1)) >> 1;
              cr_r = Cr_r_tab[CR];
              cr_g = Cr_g_tab[CR];
              cb_g = Cb_g_tab[CB];
              cb_b = Cb_b_tab[CB];
            }
#endif

            L = L_tab [(int) *lum2++];
            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

            L = L_tab [(int) *lum2++];
            R = L + cr_r;
            G = L + cr_g + cb_g;
            B = L + cb_b;

            *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
        }
        lum += cols_2 + cols_2;
        lum2 += cols_2 + cols_2;
        row1 += cols_2 + cols_2;
        row2 += cols_2 + cols_2;
    }
}





#endif



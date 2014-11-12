/************************************************************************
 *
 *  global.h, global variables for tmndecode (H.263 decoder)
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


/* choose between declaration (GLOBAL undefined)
 * and definition (GLOBAL defined)
 * GLOBAL is defined in exactly one file (tmndec.c)
 */

#ifndef GLOBAL
#define EXTERN extern
#else
#define EXTERN
#endif

#include <unistd.h>
#ifdef USE_TIME
#ifndef WIN32
#include <sys/time.h>
#else
#include <windows.h>
#endif
#endif
#ifdef WIN32
#include <io.h>
#endif

#ifdef DISPLAY
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xlibint.h>
#endif



/* Some macros */
#define mmax(a, b)        ((a) > (b) ? (a) : (b))
#define mmin(a, b)        ((a) < (b) ? (a) : (b))
#define mnint(a)        ((a) < 0 ? (int)(a - 0.5) : (int)(a + 0.5))
#define sign(a)         ((a) < 0 ? -1 : 1)

/* prototypes of global functions */

/* getbits.c */
void initbits _ANSI_ARGS_((void));
void fillbfr _ANSI_ARGS_((void));
unsigned int showbits _ANSI_ARGS_((int n));
unsigned int getbits1 _ANSI_ARGS_((void));
void flushbits _ANSI_ARGS_((int n));
unsigned int getbits _ANSI_ARGS_((int n));

/* getblk.c */
void getblock _ANSI_ARGS_((int comp, int mode));
void get_sac_block _ANSI_ARGS_((int comp, int mode));

/* gethdr.c */
int getheader _ANSI_ARGS_((void));
void startcode _ANSI_ARGS_((void));

/* getpic.c */
void getpicture _ANSI_ARGS_((int *framenum));
void putlast _ANSI_ARGS_((int framenum));

/* getvlc.c */
int getTMNMV _ANSI_ARGS_((void));
int getMCBPC _ANSI_ARGS_((void));
int getMODB _ANSI_ARGS_((void));
int getMCBPCintra _ANSI_ARGS_((void));
int getCBPY _ANSI_ARGS_((void));

/* idct.c */
void idct _ANSI_ARGS_((short *block));
void init_idct _ANSI_ARGS_((void));

/* idctref.c */
void init_idctref _ANSI_ARGS_((void));
void idctref _ANSI_ARGS_((short *block));

/* tmndec.c */
void error _ANSI_ARGS_((char *text));
void printbits _ANSI_ARGS_((int code, int bits, int len));
#ifdef USE_TIME
void doframerate(int pb);
#endif

/* recon.c */
void reconstruct _ANSI_ARGS_((int bx, int by, int P, int bdx, int bdy));

/* store.c */
void storeframe _ANSI_ARGS_((unsigned char *src[], int frame));

/* sac.c */
int decode_a_symbol _ANSI_ARGS_((int cumul_freq[ ]));
void decoder_reset( );

#ifdef DISPLAY
/* display.c */
void init_display _ANSI_ARGS_((char *name));
void exit_display _ANSI_ARGS_((void));
void dither _ANSI_ARGS_((unsigned char *src[]));
void init_dither _ANSI_ARGS_((void));

/* dither.c */
void ord4x4_dither_init _ANSI_ARGS_((void));
void ord4x4_dither_frame _ANSI_ARGS_((unsigned char *[], unsigned char *));

/* yuv2rgb.c */
void Color16DitherImage _ANSI_ARGS_((unsigned char *[], unsigned char *));
void Color32DitherImage _ANSI_ARGS_((unsigned char *[], unsigned char *));
void InitColorDither _ANSI_ARGS_((int));
/* yuvrgb24.c */
void ConvertYUVtoRGB(
  unsigned char *src0,
  unsigned char *src1,
  unsigned char *src2,
  unsigned char *dst_ori,
  int width,
  int height
);
void init_dither_tab();
#endif



/* global variables */

EXTERN char version[]
#ifdef GLOBAL
  ="tmndecode v1.7\n(C) 1995, 1996 Telenor R&D\n"
#endif
;

/* zig-zag scan */
EXTERN unsigned char zig_zag_scan[64]
#ifdef GLOBAL
=
{
  0,1,8,16,9,2,3,10,17,24,32,25,18,11,4,5,
  12,19,26,33,40,48,41,34,27,20,13,6,7,14,21,28,
  35,42,49,56,57,50,43,36,29,22,15,23,30,37,44,51,
  58,59,52,45,38,31,39,46,53,60,61,54,47,55,62,63
}
#endif
;


/* color space conversion coefficients
 *
 * entries are {crv,cbu,cgu,cgv}
 *
 * crv=(255/224)*65536*(1-cr)/0.5
 * cbu=(255/224)*65536*(1-cb)/0.5
 * cgu=(255/224)*65536*(cb/cg)*(1-cb)/0.5
 * cgv=(255/224)*65536*(cr/cg)*(1-cr)/0.5
 *
 * where Y=cr*R+cg*G+cb*B (cr+cg+cb=1)
 */

EXTERN int convmat[8][4]
#ifdef GLOBAL
=
{
  {117504, 138453, 13954, 34903}, /* no sequence_display_extension */
  {117504, 138453, 13954, 34903}, /* ITU-R Rec. 709 (1990) */
  {104597, 132201, 25675, 53279}, /* unspecified */
  {104597, 132201, 25675, 53279}, /* reserved */
  {104448, 132798, 24759, 53109}, /* FCC */
  {104597, 132201, 25675, 53279}, /* ITU-R Rec. 624-4 System B, G */
  {104597, 132201, 25675, 53279}, /* SMPTE 170M */
  {117579, 136230, 16907, 35559}  /* SMPTE 240M (1987) */
}
#endif
;

EXTERN int quiet;
EXTERN int trace;
EXTERN char errortext[256];
EXTERN unsigned char *refframe[3],*oldrefframe[3],*bframe[3],*newframe[3];
EXTERN unsigned char *edgeframe[3], *edgeframeorig[3], *exnewframe[3];
EXTERN int MV[2][5][MBR+1][MBC+2];
EXTERN int modemap[MBR+1][MBC+2];
EXTERN unsigned char *clp;
EXTERN int horizontal_size,vertical_size,mb_width,mb_height;
EXTERN int coded_picture_width, coded_picture_height;
EXTERN int chrom_width,chrom_height,blk_cnt;
EXTERN int pict_type,newgob;
EXTERN int mv_outside_frame,syntax_arith_coding,adv_pred_mode,pb_frame;
EXTERN int long_vectors;
EXTERN int fault,expand, deposterizeV, deposterizeH;
EXTERN int verbose;
EXTERN int refidct;
EXTERN int matrix_coefficients;
EXTERN int temp_ref, prev_temp_ref, quant, source_format;
#ifdef USE_TIME
EXTERN int framerate;
#ifndef WIN32
EXTERN struct timeval tftarget;
#else
EXTERN unsigned int targetTime;
#endif
#endif


EXTERN int trd, trb, bscan, bquant;
EXTERN int bscan_tab[]
#ifdef GLOBAL
= {2,4,6,8}
#endif
;
EXTERN int bquant_tab[]
#ifdef GLOBAL
= {5,6,7,8}
#endif
;

EXTERN int OM[5][8][8]
#ifdef GLOBAL
= {
{
  {4,5,5,5,5,5,5,4},
  {5,5,5,5,5,5,5,5},
  {5,5,6,6,6,6,5,5},
  {5,5,6,6,6,6,5,5},
  {5,5,6,6,6,6,5,5},
  {5,5,6,6,6,6,5,5},
  {5,5,5,5,5,5,5,5},
  {4,5,5,5,5,5,5,4},
},{
  {2,2,2,2,2,2,2,2},
  {1,1,2,2,2,2,1,1},
  {1,1,1,1,1,1,1,1},
  {1,1,1,1,1,1,1,1},
  {0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0},
},{
  {0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0},
  {0,0,0,0,0,0,0,0},
  {1,1,1,1,1,1,1,1},
  {1,1,1,1,1,1,1,1},
  {1,1,2,2,2,2,1,1},
  {2,2,2,2,2,2,2,2},
},{
  {0,0,0,0,1,1,1,2},
  {0,0,0,0,1,1,2,2},
  {0,0,0,0,1,1,2,2},
  {0,0,0,0,1,1,2,2},
  {0,0,0,0,1,1,2,2},
  {0,0,0,0,1,1,2,2},
  {0,0,0,0,1,1,2,2},
  {0,0,0,0,1,1,1,2},
},{
  {2,1,1,1,0,0,0,0},
  {2,2,1,1,0,0,0,0},
  {2,2,1,1,0,0,0,0},
  {2,2,1,1,0,0,0,0},
  {2,2,1,1,0,0,0,0},
  {2,2,1,1,0,0,0,0},
  {2,2,1,1,0,0,0,0},
  {2,1,1,1,0,0,0,0},
}}
#endif
;

EXTERN int roundtab[16]
#ifdef GLOBAL
=  {0,0,0,1,1,1,1,1,1,1,1,1,1,1,2,2}
#endif
;

/* output */
EXTERN char *outputname;
EXTERN int outtype;
#define T_YUV      0
#define T_SIF      1
#define T_TGA      2
#define T_PPM      3
#define T_X11      4
#define T_YUV_CONC 5
#define T_WIN      6

EXTERN struct ld {
  /* bit input */
  int infile;
  unsigned char rdbfr[2051];
  unsigned char *rdptr;
  unsigned char inbfr[16];
  int incnt;
  int bitcnt;
  /* block data */
  short block[12][64];
} base,*ld;




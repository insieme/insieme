/* ***********************************************************************
  This program is part of the
	OpenMP Source Code Repository

	http://www.pcg.ull.es/ompscr/
	e-mail: ompscr@etsii.ull.es

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License 
  (LICENSE file) along with this program; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place, Suite 330, 
  Boston, MA  02111-1307  USA
	
	FILE:              c_mandel.c
  VERSION:           1.0
  DATE:              May 2004
  AUTHOR:            F. de Sande
  COMMENTS TO:       sande@csi.ull.es
  DESCRIPTION:       This program computes an estimation to the 
	                   Mandelbrot Set area using MonteCarlo sampling.
										 The best known estimate so far is 1.50659177 +- 0.00000008.
  COMMENTS:	         The Mandelbrot set is a fractal that is defined as the set of points c
	                   in the complex plane for which the sequence z_{n+1} = z_n^2 + c
                     with z_0 = 0 does not tend to infinity.
                     The area of the Mandelbrot set is an open question that has been 
										 discussed in the recent past.
                   	 It is not easy to obtain an accurate analytical estimate, and therefore 
										 statistical methods have been used.
	                   The program explores the rectangle ranging over (-2.0, 0.5) in the 
	                   real axis and (0.0, 1.125) in the imaginary axis.
										 This rectangle covers the top half of the Mandelbrot Set.  
  REFERENCES:        http://www.fractalus.com/kerry/articles/area/mandelbrot-area.html
                     http://www.matthiasbook.de/papers/parallelfractals/mandelbrot.html
                     http://www.mrob.com/pub/muency/areaofthemandelbrotset.html
                     http://en.wikipedia.org/wiki/Mandelbrot_set
  BASIC PRAGMAS:     parallel for
	USAGE:             ./c_mandel.par 8192
  INPUT:             The number of points to explore
  OUTPUT:            An estimation of the area and the error. 
	FILE FORMATS:      -
	RESTRICTIONS:      -
	REVISION HISTORY:
**************************************************************************/
//#include "OmpSCR.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define OSCR_RAND_MAX 2147483647

#ifndef N
	#define N 1000
#endif

#define MAXITER 100000
#define DEFAULT_NPOINTS 4092
#define THRESOLD 2.0

#define	NUM_ARGS		1
#define	NUM_TIMERS	1
typedef struct { double re, im; } complex;

int NPOINTS;   /* Total no. of points  */
complex *points;

/* -----------------------------------------------------------------------
                          IMPLEMENTATION
 * ----------------------------------------------------------------------- */
int main(int argc, char **argv) {
  int i, j, NUMTHREADS;
  long inside,  /* no. of points inside the Mandelbrot set */
			 outside; /* no. of points outside the Mandelbrot set */
  double area, error, ztemp, total_time;
  complex z;
  char *PARAM_NAMES[NUM_ARGS] = {"Number of points"};
  char *TIMERS_NAMES[NUM_TIMERS] = {"Total_time"};
  char *DEFAULT_VALUES[NUM_ARGS] = {"4092"};


   //NUMTHREADS = omp_get_max_threads();
   /*OSCR_init (NUMTHREADS, "Mandelbrot set area", "Use 'mandel' <Number of points>", NUM_ARGS,
                PARAM_NAMES, DEFAULT_VALUES , NUM_TIMERS, NUM_TIMERS, TIMERS_NAMES,
                argc, argv);
 */
	//NPOINTS = OSCR_getarg_int(1);
	NPOINTS = N;
  /* Default: DEFAULT_NPOINTS */

  points = (complex *)calloc(NPOINTS, sizeof(complex));
  NUMTHREADS = omp_get_max_threads();

/*1. Generate NPOINTS random points in the complex plane */
  srandom(31416);
  for (i = 0; i < NPOINTS; i++) {
    points[i].re = -2.0 + 2.5 * random() / OSCR_RAND_MAX;
    points[i].im = 1.125 * random() / OSCR_RAND_MAX;
  }

/* *  2. Monte Carlo sampling 
 *    2a. Outer loop runs over NPOINTS, initialise z=c
 *    2b. Inner loop has the iteration z=z*z+c, and threshold test
 */
//	OSCR_timer_start(0);
  outside = 0;
#pragma omp parallel for default(none) reduction(+:outside)       \
	                       private(i, j, ztemp, z) shared(NPOINTS, points)
  for(i = 0; i < NPOINTS; i++) {
    z.re = points[i].re;
    z.im = points[i].im;
    for (j = 0; j < MAXITER; j++) {
      ztemp = (z.re * z.re) - (z.im * z.im) + points[i].re;
      z.im = z.re * z.im * 2 + points[i].im;
      z.re = ztemp;
      if (z.re * z.re + z.im * z.im > THRESOLD) {
        outside++;
        break;
      } 
    } /* for j */
  } /* for i */
  inside = (long)NPOINTS - outside;

  /*3. Calculate area and error */
	/* The area is proportional to 2 * the area of the rectangle * no. of points inside it */
	/* The error is inversely proportional to the square root of the number of test cases */
  area = 2.0 * (2.5 * 1.125) * inside / NPOINTS;
  error = area / sqrt(NPOINTS);  
//  OSCR_timer_stop(0);
//  total_time = OSCR_timer_read(0);

  /* 4. Output the Results */
//	OSCR_report(1, TIMERS_NAMES);
//	printf("\n \t# NPOINTS AREA \t\t\tERROR\n");
	printf("N=%d \tAREA=%16.12f ERROR=%16.12f\n", NPOINTS, area, error);
	return 0;

}

/*
 * vim:ts=2:sw=2:
 */

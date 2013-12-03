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

	FILE:              c_qsort.c
  VERSION:           1.0
  DATE:              May 2004
  AUTHOR:            F. de Sande
  COMMENTS TO:       sande@csi.ull.es
  DESCRIPTION:       Parallel implementation of Quicksort using OpenMP
	                   Sorts an integer array
  COMMENTS:          The code requires nested Parallelism.
  REFERENCES:        C. A. R. Hoare,
                     ACM Algorithm 64}: Quicksort",
                     Communications of the ACM",
                     vol. 4, no. 7, pg. 321. Jul 1961
                     http://en.wikipedia.org/wiki/Quicksort
  BASIC PRAGMAS:     parallel for
  USAGE:             ./c_qsort.par 2000000
  INPUT:             The size (in K) of the vector to sort
  OUTPUT:            The code tests that the vector is sorted
	FILE FORMATS:      -
	RESTRICTIONS:      -
	REVISION HISTORY:
**************************************************************************/
#include "OmpSCR.h"

#define NUM_ARGS	1
#define NUM_TIMERS	1
#define KILO (1024)
#define MEGA (1024 * 1024)
#define DEFAULT_SIZE  (2 * MEGA)
#define MAXSIZE  (9 * MEGA)
#define NUM_STEPS 10             /* No. of iterations (number of vectors to sort) */

char USAGE_STR[] = "<size_in_Kb>";
int SIZE;
int array[MAXSIZE];


/* -----------------------------------------------------------------------
                          PROTOTYPES
 * ----------------------------------------------------------------------- */

void initialize(int *v, int seed);
void testit(int *v);
void qs(int *v, int first, int last);

/* -----------------------------------------------------------------------
                          IMPLEMENTATION
 * ----------------------------------------------------------------------- */
/* -----------------------------------------------------------------------
   Sets randomly the values for the array
 * ----------------------------------------------------------------------- */
void initialize(int *v, int seed) {
  unsigned i;

   srandom(seed);
   for(i = 0; i < SIZE; i++)
     v[i] = (int)random();
}
/* -----------------------------------------------------------------------
   Tests the result
 * ----------------------------------------------------------------------- */
void testit(int *v) {
  register int k;
	int not_sorted = 0;

  for (k = 0; k < SIZE - 1; k++)
    if (v[k] > v[k + 1]) {
      not_sorted = 1;
      break;
    }
  if (not_sorted)
    printf("Array NOT sorted.\n");
	else
    printf("Array sorted.\n");
}
/* ----------------------------------------------------------------------- */
void qs(int *v, int first, int last) {
  int start[2], end[2], pivot, i, temp;

  if (first < last) {
     start[1] = first;
     end[0] = last;
     pivot = v[(first + last) / 2];
     while (start[1] <= end[0]) {
       while (v[start[1]] < pivot)
         start[1]++;
       while (pivot < v[end[0]])
         end[0]--;
       if (start[1] <= end[0]) {
         temp = v[start[1]];
         v[start[1]] = v[end[0]];
         v[end[0]] = temp;
         start[1]++;
         end[0]--;
       }
     }
     start[0] = first;
     end[1]   = last;

#pragma omp parallel
{
#pragma omp for nowait
     for(i = 0; i <= 1; i++) {
       qs(v, start[i], end[i]);
     }
}

   }
}
/* ----------------------------------------------------------------------- */
int main(int argc, char *argv[]) {
  int STEP, NUMTHREADS;
  double total_time;
  char *PARAM_NAMES[NUM_ARGS] = {"Size (in K)"};
  char *TIMERS_NAMES[NUM_TIMERS] = {"Total_time" };
  char *DEFAULT_VALUES[NUM_ARGS] = {"2048 K"};


  NUMTHREADS = omp_get_max_threads();
  OSCR_init (NUMTHREADS, "Quicksort", "Use 'qsort' <size (in K)>", NUM_ARGS,
    PARAM_NAMES, DEFAULT_VALUES , NUM_TIMERS, NUM_TIMERS, TIMERS_NAMES,
    argc, argv);

  SIZE = OSCR_getarg_int(1);
  if (SIZE > MAXSIZE) {
    printf("Size: %d Maximum size: %d\n", SIZE, MAXSIZE);
    exit(-1);
  }
	/* Default: DEFAULT_SIZE */
  for (STEP = 0; STEP < NUM_STEPS; STEP++) {
    initialize(array, STEP);
	  OSCR_timer_start(0);
    qs(array, 0, SIZE-1);
		OSCR_timer_stop(0);
    testit(array);
  }
	total_time = OSCR_timer_read(0);
	OSCR_report(1, TIMERS_NAMES);
	printf("\n \t# THREADS \tSIZE \tSTEPS \tTIME (secs.) \n");
	//printf("\t%d \t\t%d \t%d \t%14.6lf \n", NUMTHREADS, SIZE, NUM_STEPS, total_time);
    printf("\t%d \t\t%d \t%d \n", NUMTHREADS, SIZE, NUM_STEPS);

} /* main */


/*
 * vim:ts=2:sw=2:
 */

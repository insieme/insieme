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
	
FILE:		c_loopA.solution3.c
VERSION:	1.0
DATE:
AUTHOR:		Arturo González-Escribano
COMMENTS TO:	arturo@infor.uva.es
DESCRIPTION:       
		Parallelizing an inner loop with dependences
		Backward dependency

		for (iter=0; iter<numiter; iter++) {
			for (i=0; i<size-1; i++) {
				V[i] = f( V[i], V[i+1] );
			}
		}

		Method: Eliminate dependences by duplicating data
		Optimization: Copy only the border element
		Version: Parallel regions inside the outer loop, only 1 barrier,
			the other synchronization is associated with the end 
			of the parallel region
COMMENTS:          
REFERENCES:     
BASIC PRAGMAS:	parallel, barrier
USAGE: 		c_loopA.solution3 <size> <numiter>
INPUT:		The array has fixed innitial values:  V[i]=i
OUTPUT:		Compile with -DDEBUG to see final array values
FILE FORMATS:
RESTRICTIONS:
REVISION HISTORY:
**************************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include"OmpSCR.h"


/* PROTOYPES */
void loop(int, int, int);


/* MAIN: PROCESS PARAMETERS */
int main(int argc, char *argv[]) {
int nthreads, size, numiter;
char *argNames[2] = { "size", "numiter" };
char *defaultValues[2] = { "1000", "100" };
char *timerNames[1] = { "EXE_TIME" };

nthreads = omp_get_max_threads();
OSCR_init( nthreads,
	"Sinthetic loops experiment.",
	NULL,
	2,
	argNames,
	defaultValues,
	1,
	1,
	timerNames,
	argc,
	argv );

/* 1. GET PARAMETERS */
size = OSCR_getarg_int(1);
numiter = OSCR_getarg_int(2);

/* 2. CALL COMPUTATION */
loop(nthreads, size, numiter);

/* 3. REPORT */
OSCR_report();

return 0;
}


/*
* DUMMY FUCNTION
*/
#define f(x,y)	((x+y)/2.0)

/*
*
* PARALLEL LOOP
*
*/
void loop(int nthreads, int size, int numiter) {
/* VARIABLES */
int i,iter;
int thread;
int limitL, limitR;

/* DECLARE VECTOR AND ANCILLARY DATA STRUCTURES */
double *V=NULL;
double border;
int totalSize = size*nthreads;

V = (double *)OSCR_calloc(totalSize, sizeof(double));

/* 1. INITIALIZE VECTOR */
for (i=0; i<totalSize; i++) {
	V[i]= 0.0 + i;
	}

/* 2. GET TIMER */
OSCR_timer_start(0);

/* 3. ITERATIONS LOOP */
for(iter=0; iter<numiter; iter++) {

	/* 3.1. PROCESS IN PARALLEL */
#pragma omp parallel default(none) shared(V,size,nthreads,numiter) private(iter,thread,limitL,limitR,border,i)
		{
		/* 3.1.1. GET NUMBER OF THREAD */
		thread = omp_get_thread_num();

		/* 3.1.2. COMPUTE LIMIT INDEX */
		limitL = thread*size;
		limitR = (thread+1)*size-1;

		/* 3.1.3. COPY OTHER THREADS's NEIGHBOR ELEMENT */
		if (thread != nthreads) border = V[limitR+1];

		/* 3.1.4. SYNCHRONIZE BEFORE UPDATING LOCAL PART */
#pragma omp 	barrier

		/* 3.1.5. COMPUTE LOCAL UPDATES */
		for (i=limitL; i<limitR; i++) {
			V[i] = f( V[i], V[i+1] );
			}
		/* 3.1.6. COMPUTE LAST ELEMENT (EXCEPT LAST THREAD) */
		if (thread != nthreads-1)
			V[limitR] = f( V[limitR], border );
		
		/* 3.1.7. END PARALLEL REGION */
		}

	/* 3.2. END ITERATIONS LOOP */
	}


/* 4. STOP TIMER */
OSCR_timer_stop(0);

/* 5. WRITE VECTOR (DEBUG) */
#ifdef DEBUG
#include "debug_V.c"
#endif

/* 6. END */
}

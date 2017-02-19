/**********************************************************************************************/
/*  This program is part of the Barcelona OpenMP Tasks Suite                                  */
/*  Copyright (C) 2009 Barcelona Supercomputing Center - Centro Nacional de Supercomputacion  */
/*  Copyright (C) 2009 Universitat Politecnica de Catalunya                                   */
/*                                                                                            */
/*  This program is free software; you can redistribute it and/or modify                      */
/*  it under the terms of the GNU General Public License as published by                      */
/*  the Free Software Foundation; either version 2 of the License, or                         */
/*  (at your option) any later version.                                                       */
/*                                                                                            */
/*  This program is distributed in the hope that it will be useful,                           */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of                            */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                             */
/*  GNU General Public License for more details.                                              */
/*                                                                                            */
/*  You should have received a copy of the GNU General Public License                         */
/*  along with this program; if not, write to the Free Software                               */
/*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA            */
/**********************************************************************************************/

/*
 * Original code from the Cilk project (by Keith Randall)
 *
 * Copyright (c) 2000 Massachusetts Institute of Technology
 * Copyright (c) 2000 Matteo Frigo
 */


#include <cilk/cilk.h>

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>

#include <alloca.h>



/* Checking information */

static int solutions[] = {
        1,
        0,
        0,
        2,
        10, /* 5 */
        4,
        40,
        92,
        352,
        724, /* 10 */
        2680,
        14200,
        73712,
        365596,
};
#define MAX_SOLUTIONS sizeof(solutions)/sizeof(int)

int total_count;


/*
 * <a> contains array of <n> queen positions.  Returns 1
 * if none of the queens conflict, and returns 0 otherwise.
 */
int ok(int n, char *a)
{
	int i, j;
	char p, q;

	for (i = 0; i < n; i++) {
		p = a[i];

		for (j = i + 1; j < n; j++) {
			q = a[j];
			if (q == p || q == p - (j - i) || q == p + (j - i))
				return 0;
		}
	}
	return 1;
}

void nqueens_ser (int n, int j, char *a, int *solutions)
{
	int res;
	int i;

	if (n == j) {
		/* good solution, count it */
		*solutions = 1;
		return;
	}

	*solutions = 0;

	/* try each possible position for queen <j> */
	for (i = 0; i < n; i++) {
		{
			/* allocate a temporary array and copy <a> into it */
			a[j] = (char) i;
			if (ok(j + 1, a)) {
				nqueens_ser(n, j + 1, a,&res);
				*solutions += res;
			}
		}
	}
}

void nqueens(int n, int j, char *a, int *solutions, int depth);

void nqueens_helper(int n, char* a, int i, int j, int* csols, int depth) {
	/* allocate a temporary array and copy <a> into it */
	char * b = alloca(n * sizeof(char));
	memcpy(b, a, j * sizeof(char));
	b[j] = (char) i;
	if (ok(j + 1, b))
		nqueens(n, j + 1, b,&csols[i],depth); //FIXME: depth or depth+1 ???
}

void nqueens(int n, int j, char *a, int *solutions, int depth)
{
	//printf("nqueens n: %d, j: %d, a: %p, solutions: %p, depth: %d\n", n, j, a, solutions, depth);
	int *csols;
	int i;


	if (n == j) {
		/* good solution, count it */
		*solutions = 1;
		return;
	}


	*solutions = 0;
	csols = alloca(n*sizeof(int));
	memset(csols,0,n*sizeof(int));

	/* try each possible position for queen <j> */
	for (i = 0; i < n; i++) {
		cilk_spawn nqueens_helper(n, a, i, j, csols, depth);
	}

	cilk_sync;
	for ( i = 0; i < n; i++) *solutions += csols[i];
}

void find_queens (int size)
{
	total_count=0;

	printf("Computing N-Queens algorithm (n=%d) ", size);
	char *a;

	a = alloca(size * sizeof(char));
	nqueens(size, 0, a, &total_count,0);
	printf(" completed!\n");
}


int verify_queens (int size)
{
	if ( size > MAX_SOLUTIONS ) return 1;
	if ( total_count == solutions[size-1]) return 0;
	printf("Verification failed!\n");
	return 1;
}

int main(int argc, char** argv) {
	if (argc != 2) {
		printf("Usage: nqueens <n>\n");
		return 1;
	}

	int size = atoi(argv[1]);
	find_queens(size);
	return verify_queens(size);
}

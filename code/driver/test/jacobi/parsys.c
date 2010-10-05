/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

//============================================================================
// Name        : parsys.h
// Author      : Simone Pellegrini
// Version     :
// Copyright   :
// Description : Library for parallel systems laboratory
//============================================================================

#include "parsys.h"

void create_matrix(float*** m, int N, int M){
	int i;
	float* matrix = (float*) malloc(sizeof(float) * N * M); // create the main matrix
	*m = (float**) malloc(sizeof(float*) * N); // create and indexes vector
	for(i=0; i<N; i++)
		(*m)[i] = &matrix[i*M]; // make the intex vector to point to the matrix rows
}

void free_matrix(float** m){
	free(m[0]); // free the main matrix
	free(m); // free the indexes vector
}

// Initialize a vector of size N with random numbers in the range [0, max_val]
void init_vector(float* m, int N, int max_val){
	int i;
	for(i=0; i<N; i++)
		m[i] = ((float) rand() / RAND_MAX) * max_val;
}

// Initialize a matrix of size NxM with random numbers in the range [0, max_val]
void init_matrix(float** m, int N, int M, int max_val){
	init_vector(m[0], N*M, max_val);
}

void print_matrix(double** m, int N, int M){
	int i,j;
	for(i=0; i<N; i++){
		printf("[ ");
		for(j=0; j<M; j++){
			printf("%0.2f", m[i][j]);
			if(j != M-1)
				printf(", ");
		}
		printf(" ]\n");
	}
}

void print_vector(double* v, int N){
	int i;
	printf("[ ");
	for(i=0; i<N; i++){
		printf("%0.2f", v[i]);
		if(i != N-1)
			printf(", ");
	}
	printf(" ]\n");
}


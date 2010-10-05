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

#ifndef PARSYS_LIB_H
#define PARSYS_LIB_H

#include <stdlib.h>
#include <stdio.h>

// This function creates a matrix of size NxM
// The matrix can be accessed using the [][] operator
// The memory MUST be free by the user in the following way:
// 	free(m[0]);
//		free(m);
void create_matrix(float*** m, int N, int M);

// free the matrix created with the create_matrix function
void free_matrix(float** m);

// Initialize a vector with random numbers within the range [0, max_val]
void init_vector(float* m, int N, int max_val);

// Initialize a matrix with random numbers within the range [0, max_val]
void init_matrix(float** m, int N, int M, int max_val);

void print_matrix(double** m, int N, int M);

void print_vector(double* v, int N);

#endif

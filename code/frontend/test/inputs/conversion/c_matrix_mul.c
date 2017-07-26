/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <stdio.h>

// allows the user to specify the problem size at compile time
#ifndef N
#define N 1000
#endif

#define M N
#define K N

#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#define VALUE double

VALUE A[N][M];
VALUE B[M][K];
VALUE C[N][K];

int main() {
	// A contains real values
	for(int i = 0; i < N; i++) {
		for(int j = 0; j < M; j++) {
			A[i][j] = i * j;
		}
	}

	// B is the identity matrix
	for(int i = 0; i < M; i++) {
		for(int j = 0; j < K; j++) {
			B[i][j] = (i == j) ? 1 : 0;
		}
	}

	// conduct multiplication
	for(int i = 0; i < N; i++) {
		for(int j = 0; j < K; j++) {
			for(int k = 0; k < M; k++) {
				C[i][j] += A[i][k] * B[k][j];
			}
		}
	}

	// verify result
	int success = 1;
	for(int i = 0; i < N; i++) {
		for(int j = 0; j < MIN(M, K); j++) {
			if(A[i][j] != C[i][j]) { success = 0; }
		}
		for(int j = MIN(M, K); j < MAX(M, K); j++) {
			if(C[i][j] != 0) { success = 0; }
		}
	}

	return !success;
}

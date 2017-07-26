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

#include <gtest/gtest.h>

// enable both phases in the capture mechanism
#define RECORD
#define RESTORE

#include "context/capture.h"

#include "impl/error_handling.impl.h"
#include "context/impl/capture.impl.h"
#include "standalone.h"


#define CREATE_BLOCK(SIZE) REG_BLOCK(malloc(SIZE), (SIZE));


class ContextCapturing : public ::testing::Test {
	std::string dumpFile;

  protected:
	/**
	 * Adds a setup which is redirecting the capture-file generation
	 * to the temp-directory.
	 */
	void SetUp() {
		char sourceFile[] = P_tmpdir "/contextXXXXXX";
		setenv("IRT_CONTEXT_FILE", sourceFile, 1);
		dumpFile = std::string(sourceFile);
	}

	void TearDown() {
		// delete the capture file
		remove(dumpFile.c_str());
	}
};


TEST_F(ContextCapturing, SimpleBlock) {
	// a simple block, just reading the value 5 ...

	// --- recording ---

	{
		// initializing the mechanism
		INIT();

		// create a variable
		int x = 5;
		uint64 c = 7;

		// start the block
		START(0);

		// register and tag local variables
		TAG_BLOCK(x, 0);
		TAG_BLOCK(c, 1);

		// operate on the local variable
		EXPECT_EQ(5, READ(x));
		EXPECT_EQ(7, READ(c));

		// end block
		STOP(0);

		// finish processing => profile should be created
		FINISH();
	}

	// --- restoring ---

	{
		// reload profile
		LOAD(int, a, 0, 0);
		LOAD(uint64, b, 0, 1);

		// check reloaded content
		EXPECT_EQ(5, a);
		EXPECT_EQ(7, b);

		// done
		FINALIZE();
	}
}


TEST_F(ContextCapturing, Matrix) {
	// test restoring a matrix

	// --- recording ---

	const int N = 40;

	int sumA = 0;
	int sumB = 0;

	{
		// initializing the mechanism
		INIT();

		// create a variable
		int* A = (int*)CREATE_BLOCK(sizeof(int) * N * N);

		// fill matrix
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				A[i * N + j] = i * j;
			}
		}

		// start the block
		START(0);

		// tag block to be accessed
		TAG_BLOCK(A, 0);

		// sum up data
		sumA = 0;
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				sumA += READ(READ_PTR(A)[i * N + j]);
			}
		}

		// end block
		STOP(0);

		free(A);

		// finish processing => profile should be created
		FINISH();
	}

	// --- restoring ---

	{
		// reload profile
		LOAD(int*, A, 0, 0);

		// check reloaded content
		sumB = 0;
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				sumB += A[i * N + j];
			}
		}

		// done
		FINALIZE();
	}

	EXPECT_EQ(sumA, sumB);
}


TEST_F(ContextCapturing, LinkedList) {
	// the type of list node to be used
	typedef struct _list {
		int value;
		struct _list* next;
	} list;


	// test restoring a linked list

	// --- recording ---

	const int N = 40;

	int sumA = 0;
	int sumB = 0;

	{
		// initializing the mechanism
		INIT();

		// create a list
		list* l = NULL;
		for(int i = 0; i < N; i++) {
			list* cur = (list*)CREATE_BLOCK(sizeof(list));
			cur->value = i;
			cur->next = l;
			l = cur;
		}

		// start the block
		START(0);

		// tag list
		TAG_BLOCK(l, 0);

		// sum up list elements
		sumA = 0;
		list* cur = READ_PTR(l);
		while(cur != NULL) {
			sumA += READ(READ_PTR(cur)->value);
			cur = READ_PTR(READ_PTR(cur)->next);
		}

		// end block
		STOP(0);

		// delete list
		while(l != NULL) {
			cur = l->next;
			free(l);
			l = cur;
		}

		// finish processing => profile should be created
		FINISH();
	}

	// --- restoring ---

	{
		// reload profile
		LOAD(list*, l, 0, 0);

		// sum up list elements
		sumB = 0;
		list* cur = l;
		while(cur != NULL) {
			sumB += cur->value;
			cur = cur->next;
		}

		// extra test - order of values
		int i = N;
		while(l != NULL) {
			EXPECT_EQ(--i, l->value);
			l = l->next;
		}

		// done
		FINALIZE();
	}

	EXPECT_EQ(sumA, sumB);
}


TEST_F(ContextCapturing, ParallelMatrixMul) {
	// test restoring a matrix multiplication within

	// --- recording ---

	const int N = 40;

	int sumA = 0;
	int sumB = 0;

	{
		// initializing the mechanism
		INIT();

		// create a variable
		char* A = (char*)CREATE_BLOCK(sizeof(char) * N * N);

		// fill matrix
		#pragma omp parallel for
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				A[i * N + j] = i * j;
			}
		}

		// start the block
		START(0);

		// tag block to be accessed
		TAG_BLOCK(A, 0);

		// sum up data
		sumA = 0;
		#pragma omp parallel for reduction(+ : sumA)
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				sumA += READ(READ_PTR(A)[i * N + j]);
			}
		}

		// end block
		STOP(0);

		free(A);

		// finish processing => profile should be created
		FINISH();
	}

	// --- restoring ---

	{
		// reload profile
		LOAD(char*, A, 0, 0);

		// check reloaded content
		sumB = 0;
		#pragma omp parallel for reduction(+ : sumB)
		for(int i = 0; i < N; i++) {
			for(int j = 0; j < N; j++) {
				sumB += A[i * N + j];
			}
		}

		// done
		FINALIZE();
	}

	EXPECT_EQ(sumA, sumB);
}

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

#include <gtest/gtest.h>

#include "insieme/utils/matrix.h"

using namespace insieme::utils;

TEST(Matrix, SimpleMatrix) {
	
	Matrix<int> m(3, 3);

	EXPECT_EQ(3u, m.rows());
	EXPECT_EQ(3u, m.cols());

	EXPECT_EQ(0, m[0][1]);

	m[0][1] = 1;

	EXPECT_EQ(1, m[0][1]);
}

TEST(Matrix, IdentityMatrix) {

	auto&& m = makeIdentity<int>(3);

	EXPECT_EQ(3u, m.rows());
	EXPECT_EQ(3u, m.cols());

	EXPECT_EQ(m[0][0], 1);
	EXPECT_EQ(m[1][1], 1);
	EXPECT_EQ(m[2][2], 1);

	EXPECT_EQ(m[0][1], 0);
	EXPECT_EQ(m[0][2], 0);

	EXPECT_EQ(m[1][0], 0);
	EXPECT_EQ(m[1][2], 0);

	EXPECT_EQ(m[2][0], 0);
	EXPECT_EQ(m[2][1], 0);
}

TEST(Matrix, SwapRows) {

	Matrix<int>&& m = makeIdentity<int>(4);

	m.swapRows( 0, 2 );

	EXPECT_EQ( m[0][0], 0 );
	EXPECT_EQ( m[0][2], 1 );

	EXPECT_EQ( m[2][0], 1 );
	EXPECT_EQ( m[2][2], 0 );
}

TEST(Matrix, SwapSwapRows) {

	Matrix<int>&& m = makeIdentity<int>(4);

	m.swapRows( 2, 3 );

	EXPECT_EQ( 1, m[3][2] );
	EXPECT_EQ( 1, m[2][3] );

	EXPECT_EQ( 0, m[3][3] );
	EXPECT_EQ( 0, m[2][2] );

	m.swapRows( 1, 2 );
	EXPECT_EQ( 1, m[2][1] );
	EXPECT_EQ( 1, m[1][3] );

}

TEST(Matrix, SwapCols) {

	Matrix<int>&& m = makeIdentity<int>(4);

	m.swapCols( 1, 3 );

	EXPECT_EQ( m[1][1], 0 );
	EXPECT_EQ( m[1][3], 1 );

	EXPECT_EQ( m[3][1], 1 );
	EXPECT_EQ( m[3][3], 0 );

}

TEST(Matrix, SwapRowsAndCols) {

	Matrix<int> m(2,3);

	m[0] = { 1,2,3 };
	m[1] = { 4,5,6 };

	m.swapRows(0,1);
	m.swapCols(1,2);

	EXPECT_EQ( m[0][1], 6 );
	EXPECT_EQ( m[0][2], 5 );

	EXPECT_EQ( m[1][1], 3 );
	EXPECT_EQ( m[1][2], 2 );

}

TEST(Matrix, SwapColsAndRows) {

	Matrix<int> m(2,3);

	m[0] = { 1,2,3 };
	m[1] = { 4,5,6 };

	m.swapCols(1,2);
	m.swapRows(0,1);

	EXPECT_EQ( m[0][1], 6 );
	EXPECT_EQ( m[0][2], 5 );

	EXPECT_EQ( m[1][1], 3 );
	EXPECT_EQ( m[1][2], 2 );

}

TEST(Matrix, SetRow) {

	Matrix<double> md(2,4);

	md[0] = { 2.3, 2.1, 4.5, 1 };
	md[1] = { 3.5, 2.3, 3.3, 4.4 };

	EXPECT_EQ(md[0][0], 2.3);
	EXPECT_EQ(md[1][3], 4.4);
}

TEST(Matrix, MatMul) {

	Matrix<int> m1(1,4);
	Matrix<int> m2(4,2);

	m1[0] = { 1, 2, 3, 4 };

	m2[0] = { 1, 2 };
	m2[1] = { 3, 4 };
	m2[2] = { 5, 6 };
	m2[3] = { 7, 8 };

	Matrix<int>&& ret = m1 * m2;
	EXPECT_EQ(1u, ret.rows());
	EXPECT_EQ(2u, ret.cols());

	EXPECT_TRUE( std::equal(ret[0].begin(), ret[0].end(), (int[]) { 50, 60 }) );
}

TEST(Matrix, MatSum) {

	Matrix<int> m1(3,3);
	m1[0] = { 1, 2, 3 };
	m1[1] = { 4, 5, 6 };
	m1[2] = { 7, 8, 9 };

	Matrix<int>&& m2 = makeIdentity<int>(3);

	Matrix<int>&& ret = m1 + m2;

	EXPECT_EQ(ret[0][0], 2);
	EXPECT_EQ(ret[1][1], 6);
	EXPECT_EQ(ret[2][2], 10);

}

TEST(Matrix, MatDiff) {

	Matrix<int> m1(3,3);
	m1[0] = { 1, 2, 3 };
	m1[1] = { 4, 5, 6 };
	m1[2] = { 7, 8, 9 };

	Matrix<int>&& m2 = makeIdentity<int>(3);

	Matrix<int>&& ret = m1 - m2;

	EXPECT_EQ(ret[0][0], 0);
	EXPECT_EQ(ret[1][1], 4);
	EXPECT_EQ(ret[2][2], 8);

}

TEST(Matrix, Equal) {

	Matrix<int>&& m1 = makeIdentity<int>(3);
	Matrix<int>&& m2 = makeIdentity<int>(3);
	EXPECT_TRUE(m1 == m2);

}

TEST(Matrix, Equal2) {

	Matrix<int> m1(2,2);
	m1[0] = { 1, 2 };
	m1[1] = { 3, 4 };

	Matrix<int> m2(2,2);
	m2[0] = { 1, 2 };
	m2[1] = { 3, 5 };

	EXPECT_FALSE( m1 == m2 );
}

TEST(Matrix, Equal3) {

	Matrix<int> m1(2,2);
	m1[0] = { 2, 1 };
	m1[1] = { 4, 3 };

	m1.swapCols(0, 1);

	Matrix<int> m2(2,2);
	m2[0] = { 1, 2 };
	m2[1] = { 3, 4 };

	EXPECT_TRUE( m1 == m2 );
}

TEST(Matrix, FromArray) {

	Matrix<int> m1 ( { {1,2,3}, {4, 5, 6} } );

	EXPECT_EQ(2u, m1.rows());
	EXPECT_EQ(3u, m1.cols());

	EXPECT_EQ(m1[0][0], 1);
	EXPECT_EQ(m1[0][1], 2);
	EXPECT_EQ(m1[0][2], 3);

	EXPECT_EQ(m1[1][0], 4);
	EXPECT_EQ(m1[1][1], 5);
	EXPECT_EQ(m1[1][2], 6);

}


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
 *
 */
#include <gtest/gtest.h>

#include <vector>

#include "insieme/core/arithmetic/cudd/cudd.h"

#include "insieme/utils/string_utils.h"

/**
 * A simple test experimenting with the CUDD external library. It also tries
 * to verify its correct operation.
 */

namespace std {
	namespace {

		void ownPrintRec(std::ostream& out, DdManager* mgr, DdNode* node, std::vector<int> path, DdNode* zero) {
			// terminal case - a
			DdNode* N = Cudd_Regular(node);

			if(cuddIsConstant(N)) {
				// Terminal case: print the path if one has been reached
				if(node != zero) {
					out << " ";
					// print path
					for(auto it = path.begin(); it != path.end(); ++it) {
						int v = *it;
						if(0 <= v && v <= 1) {
							out << v;
						} else {
							out << '-';
						}
					}
				}
				return;
			}

			DdNode* Nv = cuddT(N);  // true path
			DdNode* Nnv = cuddE(N); // false path
			if(Cudd_IsComplement(node)) {
				Nv = Cudd_Not(Nv);
				Nnv = Cudd_Not(Nnv);
			}

			int index = N->index;

			// false path
			path[index] = 0;
			ownPrintRec(out, mgr, Nnv, path, zero);

			// true path
			path[index] = 1;
			ownPrintRec(out, mgr, Nv, path, zero);

			// clear path step
			path[index] = 2;

			return;
		}
	}
	std::ostream& operator<<(std::ostream& out, const BDD& bdd) {
		// create a path
		int size = bdd.manager()->ReadSize();
		std::vector<int> path(size, 2);

		// use recursive print
		out << "(";
		ownPrintRec(out, bdd.manager()->getManager(), bdd.getNode(), path, bdd.manager()->bddZero().getNode());
		out << " )";

		// done
		return out;
	}
}


namespace insieme {
namespace core {
namespace arithmetic {


	TEST(CUDD, SimpleRelations) {
		// creating a BDD
		Cudd mgr;

		// true constant
		BDD T = mgr.bddOne();
		BDD F = mgr.bddZero();

		BDD v1 = mgr.bddVar();
		BDD v2 = mgr.bddVar();
		BDD v3 = mgr.bddVar();

		EXPECT_EQ("( --- )", toString(T));
		EXPECT_EQ("( )", toString(F));

		EXPECT_EQ("( 1-- )", toString(v1));
		EXPECT_EQ("( -1- )", toString(v2));
		EXPECT_EQ("( --1 )", toString(v3));

		EXPECT_EQ("( 0-- )", toString(!v1));
		EXPECT_EQ("( -0- )", toString(!v2));
		EXPECT_EQ("( --0 )", toString(!v3));

		EXPECT_EQ("( 01- 1-- )", toString(v1 + v2));
		EXPECT_EQ("( 11- )", toString(v1 * v2));

		EXPECT_EQ("( 00- )", toString(!(v1 + v2)));


		EXPECT_EQ("( 0-- )", toString((!v1 * !v2 * !v3) + (!v1 * v2 * !v3) + (!v1 * !v2 * v3) + (!v1 * v2 * v3)));
		EXPECT_EQ("( -0- )", toString((!v1 * !v2 * !v3) + (v1 * !v2 * !v3) + (!v1 * !v2 * v3) + (v1 * !v2 * v3)));
		EXPECT_EQ("( --0 )", toString((!v1 * !v2 * !v3) + (v1 * !v2 * !v3) + (!v1 * v2 * !v3) + (v1 * v2 * !v3)));


		// -- test some tautologies

		EXPECT_EQ(T, T);
		EXPECT_EQ(F, F);
		EXPECT_NE(T, F);

		EXPECT_EQ(T, v1 + !v1);
		EXPECT_EQ(F, v1 * !v1);

		EXPECT_NE(F, v1 + !v1);
		EXPECT_NE(T, v1 * !v1);


		// -- test multiple managers

		Cudd mgr1;
		Cudd mgr2;

		BDD m1v1 = mgr1.bddVar(1);
		BDD m2v1 = mgr2.bddVar(1);

		EXPECT_EQ(m1v1, m2v1.Transfer(mgr1));
		EXPECT_EQ(m1v1.Transfer(mgr2), m2v1);
	}

} // end namespace arithmetic
} // end namespace core
} // end namespace insieme

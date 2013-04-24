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

#include <vector>

#include <cuddObj.hh>
#include <cuddInt.h>

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

			if (cuddIsConstant(N)) {

				// Terminal case: print the path if one has been reached
				if (node != zero) {
					out << " ";
					// print path
					for (auto it=path.begin(); it!=path.end(); ++it) {
						int v = *it;
						if (0 <= v && v <=1) {
							out << v;
						} else {
							out << '-';
						}
					}
				}
				return;

			}

			DdNode* Nv = cuddT(N);   // true path
			DdNode* Nnv = cuddE(N);  // false path
			if (Cudd_IsComplement(node)) {
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


	EXPECT_EQ("( 0-- )", toString((!v1*!v2*!v3) + (!v1*v2*!v3) + (!v1*!v2*v3) + (!v1*v2*v3)));
	EXPECT_EQ("( -0- )", toString((!v1*!v2*!v3) + (v1*!v2*!v3) + (!v1*!v2*v3) + (v1*!v2*v3)));
	EXPECT_EQ("( --0 )", toString((!v1*!v2*!v3) + (v1*!v2*!v3) + (!v1*v2*!v3) + (v1*v2*!v3)));


	// -- test some tautologies

	EXPECT_EQ(T,T);
	EXPECT_EQ(F,F);
	EXPECT_NE(T,F);

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


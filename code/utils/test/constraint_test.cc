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

#include "insieme/utils/constraint.h"
#include "insieme/utils/string_utils.h"

using namespace insieme::utils;

typedef Constraint<int> IntConstraint;
typedef CombinerPtr<int> IntConstraintPtr;

namespace insieme { namespace utils {

template <>
int asConstant(const int& val) { return val; }

IntConstraint normalize(const IntConstraint& c) { 
	ConstraintType type = c.getType();

	if (type == ConstraintType::GE || type == ConstraintType::EQ)  { return c; }

	if (type == ConstraintType::LE) { 
		return IntConstraint(-c.getFunction()); 
	}

	if (type == ConstraintType::LT) { 
		return IntConstraint(-c.getFunction()-1); 
	}

	assert (type == ConstraintType::GT);
	return IntConstraint(c.getFunction()-1); 
}

} } // end insieme::utils namespace 

TEST(Constraint, Creation) {

	IntConstraint c(4, ConstraintType::GT);
	EXPECT_EQ("4 > 0", toString(c));
	EXPECT_TRUE(c.isTrue());

	IntConstraint c1(0, ConstraintType::EQ);
	EXPECT_EQ("0 == 0", toString(c1));
	EXPECT_TRUE(c1.isTrue());

	IntConstraint c2(-2, ConstraintType::LT);
	EXPECT_EQ("-2 < 0", toString(c2));
	EXPECT_TRUE(c2.isTrue());

	IntConstraint c3(3, ConstraintType::GE);
	EXPECT_EQ("3 >= 0", toString(c3));
	EXPECT_TRUE(c3.isTrue());
}


TEST(Combiner, Creation) {

	IntConstraintPtr comb = IntConstraint(2) and IntConstraint(0, ConstraintType::EQ);

	EXPECT_EQ("((2 >= 0) ^ (0 == 0))", toString(*comb));
	EXPECT_TRUE( comb->isTrue() );
}


TEST(Constraint, Normalization) {
	IntConstraint c(10, ConstraintType::GT);
	IntConstraintPtr nc = normalize(makeCombiner(c));
	EXPECT_EQ("(9 >= 0)", toString(*nc));

	IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) and 
							IntConstraint(0, ConstraintType::EQ);

	EXPECT_EQ("((-3 >= 0) ^ (0 == 0))", toString(*normalize(comb)));
	EXPECT_FALSE( comb->isTrue() );

}

TEST(Constraint, DNF) {
	IntConstraint c(10, ConstraintType::GT);
	IntConstraintPtr nc = toDNF(makeCombiner(c));
	EXPECT_EQ("(9 >= 0)", toString(*nc));

	{
		IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) and 
								(IntConstraint(0, ConstraintType::EQ) or 
								 IntConstraint(3, ConstraintType::GT));

		EXPECT_EQ("((-3 >= 0) ^ ((0 == 0) v (2 >= 0)))", toString(*normalize(comb)));
		EXPECT_EQ("(((-3 >= 0) ^ (0 == 0)) v ((-3 >= 0) ^ (2 >= 0)))", toString(*toDNF(comb)));
		EXPECT_FALSE( comb->isTrue() );
	}

	{
		IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) and 
								not_(IntConstraint(0, ConstraintType::EQ) or 
								 	 IntConstraint(3, ConstraintType::GT));

		EXPECT_EQ("((-3 >= 0) ^ !((0 == 0) v (2 >= 0)))", toString(*normalize(comb)));
		EXPECT_EQ("((-3 >= 0) ^ (!(0 == 0) ^ !(2 >= 0)))", toString(*toDNF(comb)));
		EXPECT_FALSE( comb->isTrue() );
	}

	{
		IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) or
								not_(IntConstraint(0, ConstraintType::EQ) and 
								 	 IntConstraint(3, ConstraintType::GT));

		EXPECT_EQ("((-3 >= 0) v !((0 == 0) ^ (2 >= 0)))", toString(*normalize(comb)));
		EXPECT_EQ("((-3 >= 0) v (!(0 == 0) v !(2 >= 0)))", toString(*toDNF(comb)));
		EXPECT_FALSE( comb->isTrue() );
	}

	{
		IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) or
								not_(IntConstraint(0, ConstraintType::EQ) and 
								 	 IntConstraint(3, ConstraintType::GT)) or 
								(IntConstraint(0, ConstraintType::EQ) and 
								 	 IntConstraint(3, ConstraintType::GT));

		EXPECT_EQ("(((-3 >= 0) v !((0 == 0) ^ (2 >= 0))) v ((0 == 0) ^ (2 >= 0)))", toString(*normalize(comb)));
		EXPECT_EQ("(((-3 >= 0) v (!(0 == 0) v !(2 >= 0))) v ((0 == 0) ^ (2 >= 0)))", toString(*toDNF(comb)));
		EXPECT_TRUE( comb->isTrue() );
	}

	{
		IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) and
								not_(IntConstraint(0, ConstraintType::EQ) or
								 	 IntConstraint(3, ConstraintType::GT)) and 
								(IntConstraint(0, ConstraintType::EQ) or
								 	 IntConstraint(3, ConstraintType::GT));

		EXPECT_EQ("(((-3 >= 0) ^ !((0 == 0) v (2 >= 0))) ^ ((0 == 0) v (2 >= 0)))", toString(*normalize(comb)));
		EXPECT_EQ("(((0 == 0) ^ ((-3 >= 0) ^ (!(0 == 0) ^ !(2 >= 0)))) v "
				  "((2 >= 0) ^ ((-3 >= 0) ^ (!(0 == 0) ^ !(2 >= 0)))))", toString(*toDNF(comb)));
		EXPECT_FALSE( comb->isTrue() );
	}
}


TEST(Constraint, ExtractList) {
		IntConstraintPtr comb = IntConstraint(2, ConstraintType::LT) and
								not_(IntConstraint(0, ConstraintType::EQ) or
								 	 IntConstraint(3, ConstraintType::GT)) and 
								(IntConstraint(0, ConstraintType::EQ) or
								 	 IntConstraint(3, ConstraintType::GT));

		comb = normalize(comb);
		EXPECT_EQ("(((-3 >= 0) ^ !((0 == 0) v (2 >= 0))) ^ ((0 == 0) v (2 >= 0)))", toString(*comb));
		comb = toDNF(comb);
		EXPECT_EQ("(((0 == 0) ^ ((-3 >= 0) ^ (!(0 == 0) ^ !(2 >= 0)))) v "
				  "((2 >= 0) ^ ((-3 >= 0) ^ (!(0 == 0) ^ !(2 >= 0)))))", toString(*comb));

		EXPECT_FALSE( comb->isTrue() );

		std::vector<std::vector<IntConstraintPtr>>&& conj = getConjunctions(comb);
		EXPECT_EQ(2u, conj.size());
		EXPECT_EQ(4u, conj[0].size());
		EXPECT_EQ(4u, conj[1].size());
}

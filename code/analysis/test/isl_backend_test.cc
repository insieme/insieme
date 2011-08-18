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

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/core/expressions.h"

using namespace insieme;
using namespace insieme::analysis;
using namespace insieme::core;

void printIslSet(std::ostream& out, isl_ctx* ctx, isl_set* set) {
	isl_printer* printer = isl_printer_to_str(ctx);
	isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
	isl_printer_set_indent(printer, 1);
	isl_printer_print_set(printer, set);
	isl_printer_flush(printer);
	char* str = isl_printer_get_str(printer);
	out << str;
	free(str); // free the allocated string by the library
	isl_printer_free(printer);
}

#define CREATE_ITER_VECTOR \
	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); \
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); \
	\
	poly::IterationVector iterVec; \
	\
	iterVec.add( poly::Iterator(iter1) ); \
	EXPECT_EQ(static_cast<size_t>(2), iterVec.size()); \
	iterVec.add( poly::Parameter(param) ); \
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size()); \

TEST(IslBackend, SetCreation) {
	
	NodeManager mgr;	
	CREATE_ITER_VECTOR; 

	poly::backend::IslContext ctx;

	poly::backend::IslSet set(ctx, iterVec);
	
}

TEST(IslBackend, SetConstraint) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,3,10} );
	poly::Constraint c(af, poly::Constraint::LT);

	poly::backend::IslContext ctx;
	poly::backend::IslSet set(ctx, iterVec);

	set.applyConstraint( makeCombiner(c) );

	std::ostringstream ss;
	ss << set;
	EXPECT_EQ("[v3] -> { [v1] : v3 <= -4 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx.getRawContext(), 
			"[v3] -> {[v1] : 3v3 + 10 < 0}", -1
		);

	// check for equality
	EXPECT_TRUE(isl_set_is_equal(refSet, const_cast<isl_set*>(set.getAsIslSet())));
	
	isl_set_free(refSet);
}

TEST(IslBackend, SetConstraintNormalized) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {1,0,10});
	
	// 1*v1 + 0*v2  + 10 != 0
	poly::Constraint c(af, poly::Constraint::NE);

	// 1*v1 + 10 > 0 && 1*v1 +10 < 0
	// 1*v1 + 9 >= 0 & -1*v1 -11 >= 0
	poly::backend::IslContext ctx;
	poly::backend::IslSet set(ctx, iterVec);
	set.applyConstraint( makeCombiner(c) );

	std::ostringstream ss;
	ss << set;
	EXPECT_EQ("[v3] -> { [v1] : v1 <= -11 or v1 >= -9 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx.getRawContext(), 
			"[v3] -> {[v1] : v1 + 10 < 0 or v1 + 10 > 0}", -1
		);

	// check for equality
	EXPECT_TRUE(isl_set_is_equal(refSet, const_cast<isl_set*>(set.getAsIslSet())));
	
	isl_set_free(refSet);
}

TEST(IslBackend, FromCombiner) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	// 0*v1 + 2*v2 + 10
	poly::AffineFunction af(iterVec, {0,2,10});

	// 0*v1 + 2*v3 + 10 == 0
	poly::Constraint c1(af, poly::Constraint::EQ);

	// 2*v1 + 3*v3 +10 
	poly::AffineFunction af2(iterVec, {2,3,10});
	
	// 2*v1 + 3*v3 +10 < 0
	poly::Constraint c2(af2, poly::Constraint::LT);

	// 2v3+10 == 0 OR !(2v1 + 3v3 +10 < 0)
	poly::ConstraintCombinerPtr ptr =  c1 or (not_(c2));
	
// 	std::cout << *ptr << std::endl;

	poly::backend::IslContext ctx;
	poly::backend::IslSet set(ctx, iterVec);
	set.applyConstraint(ptr);

	std::ostringstream ss;
	ss << set;
	EXPECT_EQ("[v3] -> { [v1] : v3 = -5 or 2v1 >= -10 - 3v3 }", ss.str());

	// Build directly the ISL set
	isl_set* refSet = isl_set_read_from_str(ctx.getRawContext(), 
			"[v3] -> {[v1] : 2*v3 + 10 = 0 or 2*v1 +3*v3 +10 >= 0}", -1
		);
	
	printIslSet(std::cout, ctx.getRawContext(), refSet);
	// check for equality
	EXPECT_TRUE( isl_set_is_equal(refSet, const_cast<isl_set*>(set.getAsIslSet())) );
	
	isl_set_free(refSet);
}




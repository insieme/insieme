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

#include <fstream>

#include "insieme/analysis/cba/cba.h"

#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/context.h"
#include "insieme/analysis/cba/framework/constraint_resolver.h"
#include "insieme/analysis/cba/framework/cba.h"

//#include "insieme/analysis/cba/analysis/arithmetic.h"
//#include "insieme/analysis/cba/analysis/boolean.h"
#include "insieme/analysis/cba/analysis/simple_constant.h"
//#include "insieme/analysis/cba/analysis/callables.h"
//#include "insieme/analysis/cba/analysis/call_context_predecessor.h"
//#include "insieme/analysis/cba/analysis/reachability.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	template<typename C> struct DummyResolver : public ConstraintResolver<C> {
		DummyResolver(CBA& cba) : ConstraintResolver<C>(cba) {}
	};

	TypedSetType<int, DummyResolver> Dummy("Dummy");

	/**
	 * This is not really a test case and should be deleted when no longer used.
	 * It is simpley used for gradually fixing the refactored CBA infrastructure.
	 */

	TEST(CBA, Refactoring) {

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		core::ExpressionAddress code(builder.parseExpr("1"));

		// let's start simple
		CBA cba(code);

		EXPECT_EQ("s1", toString(cba.getSet(Dummy, code)));
		EXPECT_EQ("s2", toString(cba.getSet(Dummy, code, Context<1,1,0>())));
		EXPECT_EQ("s3", toString(cba.getSet(Dummy, code, Context<1,1,1>())));


		cba.getValuesOf(code, D);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

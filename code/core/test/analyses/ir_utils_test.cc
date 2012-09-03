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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/normalize.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(FreeVariables, BindTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: free variables within binds have not been recognized correctly
		// reason: recursive call for bound parameters was wrong =>

		// add some free variables
		std::map<string, NodePtr> symbols;
		symbols["v"] = builder.variable(manager.getLangBasic().getInt4(), 77);

		CallExprPtr call = analysis::normalize(builder.parseExpr(
				"let int = int<4> in (((int)=>int a)->int { return a(2); } ((int x)=> (2+v) + x))",
				symbols
		)).as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int.add(int.add(2, v77), v0)})", toString(*call));
		EXPECT_EQ("[AP(v77)]", toString(getFreeVariables(call)));
	}

	TEST(AllVariables, BindTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: free variables within binds have not been recognized correctly
		// reason: recursive call for bound parameters was wrong =>

		// add some free variables
		TypePtr int4 = manager.getLangBasic().getInt4();
		std::map<string, NodePtr> symbols;
		symbols["v"] = builder.variable(int4, 77);

		CallExprPtr call = analysis::normalize(builder.parseExpr(
				"let int = int<4> in (((int)=>int a)->int { return a(2); } ((int x)=> (2+v) + x))",
				symbols
		)).as<CallExprPtr>();
		ASSERT_TRUE(call);

		// check free variables
		EXPECT_EQ("rec v0.{v0=fun(((int<4>)=>int<4>) v1) {return v1(2);}}(bind(v0){int.add(int.add(2, v77), v0)})", toString(*call));
		EXPECT_EQ(utils::set::toSet<VariableSet>(
			builder.variable(int4, 0),
			builder.variable(builder.functionType(int4, int4, false), 1),
			builder.variable(int4, 77),
			builder.variable(builder.functionType(builder.functionType(int4, int4, false), int4), 0)
		), getAllVariables(call));
	}


} // end namespace analysis
} // end namespace core
} // end namespace insieme

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


#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/analysis/channels.h"

#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, ChannelFlow) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		map<string,NodePtr> symbols;
		symbols["e"] = builder.parse("lit(\"e\":channel<int<4>,1>)");

		auto in = builder.parseStmt(
				"{"
				"	let channel = channel<int<4>,1>;"
				"	"
				"	auto c = channel.create(type(int<4>), param(1));"
				"	auto c2 = c;"
				"	ref<channel> rc = var(c);"
				"	"
				"	c;"
				"	c2;"
				"	*rc;"
				"	e;"
				"	"
				"	rc = e;"
				"	*rc;"
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		const auto& C = Ch;

		EXPECT_EQ("{(0-0-1,[[0,0],[<0,[],0>,<0,[],0>]])}", toString(analysis.getValuesOf(code[3].as<ExpressionAddress>(), C)));
		EXPECT_EQ("{(0-0-1,[[0,0],[<0,[],0>,<0,[],0>]])}", toString(analysis.getValuesOf(code[4].as<ExpressionAddress>(), C)));
		EXPECT_EQ("{(0-0-1,[[0,0],[<0,[],0>,<0,[],0>]])}", toString(analysis.getValuesOf(code[5].as<ExpressionAddress>(), C)));
		EXPECT_EQ("{(0,[[0,0],[<0,[],0>,<0,[],0>]])}", toString(analysis.getValuesOf(code[6].as<ExpressionAddress>(), C)));

		EXPECT_EQ("{(0,[[0,0],[<0,[],0>,<0,[],0>]])}", toString(analysis.getValuesOf(code[8].as<ExpressionAddress>(), C)));

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

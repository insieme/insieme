/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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


#include "insieme/analysis/cba/cba.h"

#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/analysis/references.h"
#include "insieme/analysis/cba/framework/generator/reaching_definitions.h"

#include "insieme/core/ir_builder.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/test/test_utils.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;


	TEST(CBA, SimpleValue) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	decl auto a = var(0);"
				" 	a = 1;"
				"	a = 2;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get memory location referenced by the variable a
		const std::set<Reference<DefaultContext>>& refs = analysis.getValuesOf(code[0].as<DeclarationStmtAddress>()->getVariable(), R);
		ASSERT_EQ(1,refs.size());
		auto loc = refs.begin()->getLocation();

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[0], RDout, DefaultContext(), loc)));

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[1], RDin, DefaultContext(), loc)));
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[1], RDtmp, DefaultContext(), loc)));
		EXPECT_EQ("{(0-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[1], RDout, DefaultContext(), loc)));

		EXPECT_EQ("{(0-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[2], RDin, DefaultContext(), loc)));
		EXPECT_EQ("{(0-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[2], RDtmp, DefaultContext(), loc)));
		EXPECT_EQ("{(0-2,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[2], RDout, DefaultContext(), loc)));

	}

	TEST(CBA, UncertainValue) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		map<string,NodePtr> symbols;
		symbols["c"] = builder.variable(builder.getLangBasic().getInt4());

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	decl auto a = var(0);"
				"	if ( c == 0 ) {"
				" 		a = 1;"
				"	} else {"
				"		a = 2;"
				"	}"
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		// get memory location referenced by the variable a
		const std::set<Reference<DefaultContext>>& refs = analysis.getValuesOf(code[0].as<DeclarationStmtAddress>()->getVariable(), R);
		ASSERT_EQ(1,refs.size());
		auto loc = refs.begin()->getLocation();

		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[0], RDout, DefaultContext(), loc)));

		// before if
		EXPECT_EQ("{(0-0-1-1-2-0-1-2-1,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[1], RDin, DefaultContext(), loc)));

		// after if
		EXPECT_EQ("{(0-1-1-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]]),(0-1-2-0,[[0,0],[<0,[0,0],0>,<0,[0,0],0>]])}", toString(analysis.getValuesOf(code[1], RDout, DefaultContext(), loc)));

	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

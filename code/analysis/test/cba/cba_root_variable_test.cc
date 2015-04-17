/* Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/core/checks/full_check.h"

#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/analysis/uninterpreted_symbols.h"
#include "insieme/utils/set_utils.h"

#include "insieme/core/ir_builder.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, RootVariable) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);
		const lang::BasicGenerator& gen = mgr.getLangBasic();

		DeclarationStmtPtr declA = builder.declarationStmt(builder.refType(gen.getInt4()), builder.refVar(builder.intLit(1)));
		VariablePtr varA = declA->getVariable();
		DeclarationStmtPtr declB = builder.declarationStmt(gen.getInt4(), builder.deref(varA));
		VariablePtr varB = declB->getVariable();
		DeclarationStmtPtr declC = builder.declarationStmt(builder.refType(gen.getInt4()), builder.refVar(builder.undefined(gen.getInt4())));
		VariablePtr varC = declC->getVariable();
		LiteralPtr globalVar = builder.literal("global", builder.refType(gen.getInt4()));
		DeclarationStmtPtr declD = builder.declarationStmt(builder.refType(gen.getInt4()), globalVar);
		VariablePtr varD = declD->getVariable();


		std::map<string,NodePtr> symbols;
		symbols["declA"] = declA;
		symbols["declB"] = declB;
		symbols["declC"] = declC;
		symbols["declD"] = declD;
		symbols["A"] = varA;
		symbols["B"] = varB;
		symbols["C"] = varC;
		symbols["D"] = varD;
		symbols["globalVar"] = globalVar;


		auto in = builder.parseStmt(
				"{"
				"	let sA = (real<4> arg1)->ref<real<4>> { ref<real<4>> local1 = var(arg1); return local1;};"
				"	let sB = (ref<uint<8>> arg2)->unit { ref<uint<8>> local2; local2 = ref_deref(arg2); };"
				"	let sC = (int<4> arg3, ref<int<4>> arg4)->unit { arg4 = arg3;};"
				"	globalVar = 7;"
				"	"
				"	declA;"
				"	declB;"
				"	declC;"
				"	declD;"
				"	"
				"	sA((real<4>)(ref_deref(A)));"
				"	sB(var(int_to_uint(ref_deref(D), param(8))));"
				"	sC(ref_deref(A), C);"
				"}", symbols
		).as<CompoundStmtPtr>();

		auto semantic = core::checks::check(in);
		auto warnings = semantic.getWarnings();
		std::sort(warnings.begin(), warnings.end());
		for_each(warnings, [](const core::checks::Message& cur) {
			std::cout << cur << std::endl;
		});

		auto errors = semantic.getErrors();
		EXPECT_EQ(0u, errors.size()) ;
		std::sort(errors.begin(), errors.end());
		for_each(errors, [](const core::checks::Message& cur) {
			std::cout << cur << std::endl;
		});

//		dumpPretty(in);
		VariablePtr arg1, arg2, arg3, arg4;
		visitDepthFirst(in, [&](const ParametersPtr& params) {
			if(params[0].getType() == gen.getReal4())
				arg1 = params[0];
			if(params[0].getType() == builder.refType(gen.getUInt8()))
				arg2 = params[0];
			if(params[0].getType() == gen.getInt4())
				arg3 = params[0];
			if(params[0].getType() == builder.refType(gen.getInt4()))
				arg4 = params[0];
		});
		VariablePtr local1, local2;
		visitDepthFirst(in, [&](const DeclarationStmtPtr& decl) {
			if(decl->getVariable()->toString().compare("v10") == 0)
				local1 = decl->getVariable();
			if(decl->getVariable()->toString().compare("v13") == 0)
				local2 = decl->getVariable();
		});

//		EXPECT_EQ(getRootVariable(varA), varA);
//		EXPECT_EQ(getRootVariable(varB), varA);
//		EXPECT_EQ(getRootVariable(varC), varA);
//		EXPECT_EQ(getRootVariable(varB), varA);
//		EXPECT_EQ(getRootVariable(local1), varA);
//		EXPECT_EQ(getRootVariable(arg1), varA);
//		EXPECT_EQ(getRootVariable(arg3), varA);
//		EXPECT_EQ(getRootVariable(arg4), varA);
//		EXPECT_EQ(getRootVariable(globalVar), globalVar);
//		EXPECT_EQ(getRootVariable(varD), globalVar);
//		EXPECT_EQ(getRootVariable(arg2), globalVar);
//		EXPECT_EQ(getRootVariable(local2), globalVar);
//		EXPECT_NE(getRootVariable(varA), globalVar);

}

}
}
}

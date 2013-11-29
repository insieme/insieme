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
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/parser2/ir_parser.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/encoder/lists.h"

namespace insieme {
namespace core {
namespace analysis {

	bool hasNoFreeTypeVariables(const core::TypePtr& type) {
		return !hasFreeTypeVariables(type);
	}

	TEST(FreeTypeVariables, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		// test some cases with free variables
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("'a"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("set<'a>"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("array<'a,1>"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("struct { 'a data; }"));
		EXPECT_PRED1(hasFreeTypeVariables, builder.parseType("struct { struct { 'a data; } x }"));

		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("int<4>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("set<int<4>>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("array<int<4>,1>"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("struct { int<4> data; }"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("struct { struct { int<4> data; } x }"));
		EXPECT_PRED1(hasNoFreeTypeVariables, builder.parseType("('a)->'a"));

	}

	/*
	TEST(GlobalRec, InitBug) {
		NodeManager manager;
		IRBuilder builder(manager);
		std::map<string, NodePtr> symbols;
		auto recType = builder.parseType("let type0 = struct { ref<array<type0,1>> s; } in type0");
		symbols["recTy"] = recType;	

		auto init = builder.parseExpr("ref.reinterpret(ref.null, lit(type<array<recTy,1>>))",symbols);
		//std::cout << init << std::endl;

		auto structExpr = builder.structExpr(toVector(builder.namedValue("s",init)));
		//std::cout << structExpr << std::endl;

		ExpressionList elements;
		elements.push_back(structExpr);

		auto vecPartialInit = builder.callExpr(
			builder.getLangBasic().getVectorInitPartial(),
			core::encoder::toIR(manager, elements),
			builder.getIntParamLiteral(3));
		//std::cout << vecPartialInit<< std::endl;

		auto global = builder.literal("global", builder.parseType("ref<vector<recTy,3>>", symbols));
		//std::cout << global << std::endl;

		auto assign= builder.assign(global,vecPartialInit);
		//std::cout << assign << std::endl;

		auto semanticErrors = insieme::core::checks::check(assign);
		std::cout << semanticErrors << std::endl;
		EXPECT_TRUE(semanticErrors.empty());
	}
	*/


} // end namespace analysis
} // end namespace core
} // end namespace insieme

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

#include "backend_convert.h"
#include "program.h"
#include "ast_builder.h"
#include "lang_basic.h"
#include "set_utils.h"

using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::utils::set;
using namespace insieme::simple_backend;

ProgramPtr setupSampleProgram(ASTBuilder& build) {

	TypePtr stringType = build.genericType("string");
	TypePtr varArgType = build.genericType("var_list");
	TypePtr printfArgType = build.tupleType(toVector(stringType, varArgType));
	TypePtr unitType = build.getUnitType();
	TypePtr printfType = build.functionType(printfArgType, unitType);

	auto printfDefinition = build.definition("printf", printfType, NULL, true);

	TypePtr emptyTupleType = build.tupleType();
	TypePtr voidNullaryFunctionType = build.functionType(emptyTupleType, unitType);

	ExpressionPtr intLiteral = build.literal("4", TYPE_INT_GEN_PTR);
	auto invocation = build.callExpr(unitType, build.varExpr(printfType, "printf"), toVector(intLiteral));
	auto mainBody = build.lambdaExpr(voidNullaryFunctionType, LambdaExpr::ParamList(), invocation);

	auto mainDefinition = build.definition("main", voidNullaryFunctionType, mainBody, false);

	return build.createProgram(
		toSet<Program::DefinitionSet>(printfDefinition, mainDefinition),
		toSet<Program::EntryPointSet>(build.varExpr(voidNullaryFunctionType, "main"))
		);
}


TEST(SimpleBackend, Basic) {

	ConvertVisitor converter;

	ASTBuilder build;
	ProgramPtr prog = setupSampleProgram(build);

	converter.visit(prog);
	
	//EXPECT_EQ(*compound2, *compound);
}

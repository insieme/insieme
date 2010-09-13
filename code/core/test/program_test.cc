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

#include <iostream>

#include "program.h"
#include "container_utils.h"
#include "set_utils.h"
#include "types.h"
#include "ast_builder.h"
#include "lang_basic.h"

using namespace std;
using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::utils::set;


TEST(Program, HelloWorld) {

	ASTBuilder build;

	TypePtr stringType = build.genericType("string");
	TypePtr varArgType = build.genericType("var_list");
	TypePtr printfArgType = build.tupleType(toVector(stringType, varArgType));
	TypePtr unitType = lang::TYPE_UNIT_PTR;
	TypePtr printfType = build.functionType(printfArgType, unitType);

	auto printfDefinition = build.literal("printf", printfType);

	TypePtr emptyTupleType = build.tupleType();
	TypePtr voidNullaryFunctionType = build.functionType(emptyTupleType, unitType);

	ExpressionPtr intLiteral = build.literal("4", TYPE_INT_GEN_PTR);
	auto invocation = build.callExpr(unitType, build.varExpr(printfType, "printf"), toVector(intLiteral));
	auto mainBody = build.lambdaExpr(voidNullaryFunctionType, LambdaExpr::ParamList(), invocation);

	auto mainDefinition = build.lambdaExpr(voidNullaryFunctionType, LambdaExpr::ParamList(), mainBody);
	
	ProgramPtr pro = build.createProgram(
		toSet<Program::EntryPointSet>(build.varExpr(voidNullaryFunctionType, "main"))
	);

	cout << pro;
}

TEST(Program, ProgramData) {

	// create local manager
	NodeManager manager;

	// start with empty program
	ProgramPtr program = Program::create();
	NodeManager& programManager = *program->getNodeManager();

	// check some basic properties
	EXPECT_EQ ( 0, manager.size() );
	EXPECT_EQ ( 0, programManager.size() );

	EXPECT_TRUE (program->getEntryPoints().empty());

	TypePtr typeInt = GenericType::get(manager, "int");
	TypePtr typeDouble = GenericType::get(manager, "double");

	// ------------- Entry Points ------------
	ExpressionPtr entryA = VarExpr::get(manager, typeInt, "a");
	ExpressionPtr entryB = VarExpr::get(manager, typeInt, "b");
	ExpressionPtr entryC = VarExpr::get(manager, typeDouble, "c");

	program = program->addEntryPoint(entryA);
	EXPECT_NE (entryA , *program->getEntryPoints().begin());
	EXPECT_EQ (toSet<Program::EntryPointSet>(programManager.get(entryA)), program->getEntryPoints());

	Program::EntryPointSet entrySet;
	entrySet.insert(entryA);
	entrySet.insert(entryB);
	entrySet.insert(entryC);

	program = program->addEntryPoints(entrySet);
	EXPECT_EQ( (std::size_t)3, program->getEntryPoints().size());

	const Program::EntryPointSet& points = program->getEntryPoints();
	std::for_each(points.cbegin(), points.cend(),
		[&manager, &programManager](const ExpressionPtr& cur) {
			EXPECT_FALSE( manager.addressesLocal(cur) );
			EXPECT_TRUE( programManager.addressesLocal(cur) );
	});

	// print resulting program
	cout << *program << endl;
}



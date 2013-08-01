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

#include "insieme/analysis/cba/prototype.h"

#include "insieme/core/ir_builder.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, Development) {

		CBAContext context;



	}


	TEST(CBA, GetDefinitionPoint) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto w = builder.variable(builder.parseType("A"), 123);
		map<string, NodePtr> symbols;
		symbols["w"] = w;

		auto pos = builder.parseAddresses("(int x, int y)->unit { "
				"	$x$;"
				"	$y$;"
				"	$w$;"
				"	int<4> z = 123;"
				"	$z$;"
				"	{"
				"		$z$;"
				"		auto z = 1234;"
				"		$z$;"
				"	};"
				"	$z$;"
				"	int<4> w = 123;"
				"	$w$;"
				"	int<4> w = 123;"
				"	$w$;"
				"}",
				symbols
		);

		EXPECT_EQ(9u, pos.size());

		vector<VariableAddress> vars;
		for (auto cur : pos) { vars.push_back(cur.as<VariableAddress>()); }

		auto root = LambdaExprAddress(pos[0].getRootNode().as<LambdaExprPtr>());

		auto paramX = root->getParameterList()[0];
		auto paramY = root->getParameterList()[1];
		auto freeW = VariableAddress(w);
		auto localZ1 = pos[0].getParentAddress().getAddressOfChild(3,0).as<VariableAddress>();
		auto localZ2 = pos[0].getParentAddress().getAddressOfChild(5,1,0).as<VariableAddress>();
		auto localW1 = pos[0].getParentAddress().getAddressOfChild(7,0).as<VariableAddress>();
		auto localW2 = pos[0].getParentAddress().getAddressOfChild(9,0).as<VariableAddress>();

		// simple cases
		EXPECT_EQ(paramX,  getDefinitionPoint(paramX));
		EXPECT_EQ(paramY,  getDefinitionPoint(paramY));
		EXPECT_EQ(freeW,   getDefinitionPoint(freeW));
		EXPECT_EQ(localZ1, getDefinitionPoint(localZ1));
		EXPECT_EQ(localZ2, getDefinitionPoint(localZ2));
		EXPECT_EQ(localW1, getDefinitionPoint(localW1));
		EXPECT_EQ(localW2, getDefinitionPoint(localW2));

		EXPECT_EQ(paramX,  getDefinitionPoint(vars[0]));
		EXPECT_EQ(paramY,  getDefinitionPoint(vars[1]));
		EXPECT_EQ(freeW,   getDefinitionPoint(vars[2]));
		EXPECT_EQ(localZ1, getDefinitionPoint(vars[3]));
		EXPECT_EQ(localZ1, getDefinitionPoint(vars[4]));
		EXPECT_EQ(localZ2, getDefinitionPoint(vars[5]));
		EXPECT_EQ(localZ1, getDefinitionPoint(vars[6]));
		EXPECT_EQ(localW1, getDefinitionPoint(vars[7]));
		EXPECT_EQ(localW2, getDefinitionPoint(vars[8]));


		// check within CBA context
		CBAContext context;
		auto varX = context.getVariable(paramX);
		EXPECT_EQ(varX, context.getVariable(paramX));
		EXPECT_EQ(varX, context.getVariable(vars[0]));

		auto varY = context.getVariable(vars[1]);
		EXPECT_EQ(varY, context.getVariable(paramY));
		EXPECT_NE(varX, varY);

		auto varW = context.getVariable(freeW);
		EXPECT_EQ(varW, context.getVariable(vars[2]));

		auto varW1 = context.getVariable(localW1);
		EXPECT_EQ(varW1, context.getVariable(vars[7]));

		auto varW2 = context.getVariable(vars[8]);
		EXPECT_EQ(varW2, context.getVariable(localW2));

		auto varZ1 = context.getVariable(localZ1);
		auto varZ2 = context.getVariable(localZ2);

		EXPECT_NE(varZ1, varZ2);
		EXPECT_EQ(varZ1, context.getVariable(vars[3]));
		EXPECT_EQ(varZ1, context.getVariable(vars[4]));
		EXPECT_EQ(varZ2, context.getVariable(vars[5]));
		EXPECT_EQ(varZ1, context.getVariable(vars[6]));

		// check that all variables are distinct
		std::set<Variable> allVars = { varX, varY, varZ1, varZ2, varW, varW1, varW2 };
		EXPECT_EQ(7u, allVars.size()) << allVars;

	}

	TEST(CBA, MemoryConstructor) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr A = builder.genericType("A");
		TypePtr refType = builder.refType(A);

		auto litA = builder.literal("const", A);
		auto litB = builder.literal("var", refType);

		auto expr = builder.integerLit(12);
		auto alloc = builder.parseExpr("ref.alloc(  lit(int<4>), memloc.stack)");

		EXPECT_FALSE(isMemoryConstructor(StatementAddress(litA)));
		EXPECT_TRUE(isMemoryConstructor(StatementAddress(litB)));

		EXPECT_FALSE(isMemoryConstructor(StatementAddress(expr)));
		EXPECT_TRUE(isMemoryConstructor(StatementAddress(alloc)));

	}

	TEST(CBA, BasicControlFlow) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
				"{"
				"	int<4> x = 12;"
				"	auto y = (int z)->unit {};"
				"	y(14);"
				"	y(16);"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(code);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
		// std::cout << "Constraint: " << constraints << "\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
		// std::cout << "Solutions:  " << solution << "\n";

		auto declX = CompoundStmtAddress(code)[0].as<DeclarationStmtAddress>();
		VariableAddress varX = declX->getVariable();
		ExpressionAddress initX = declX->getInitialization();

//		std::cout << *varX << " = " << cba::getValuesOf(context, solution, varX) << "\n";
		EXPECT_EQ("{AP(12)}", toString(cba::getValuesOf(context, solution, varX)));

//		std::cout << *initX << " = " << cba::getValuesOf(context, solution, initX) << "\n";
		EXPECT_EQ("{AP(12)}", toString(cba::getValuesOf(context, solution, initX)));

		auto declY = CompoundStmtAddress(code)[1].as<DeclarationStmtAddress>();
		VariableAddress varY = declY->getVariable();
		ExpressionAddress initY = declY->getInitialization();

//		std::cout << *varY << " = " << cba::getValuesOf(context, solution, varY) << "\n";
//		std::cout << *initY << " = " << cba::getValuesOf(context, solution, initY) << "\n";
		EXPECT_EQ("{0-1-1}", toString(cba::getValuesOf(context, solution, varY, c)));
		EXPECT_EQ("{0-1-1}", toString(cba::getValuesOf(context, solution, initY, C)));


		auto varZ = initY.as<LambdaExprAddress>()->getParameterList()[0];
//		std::cout << *varZ << " = " << cba::getValuesOf(context, solution, varZ) << "\n";

		std::set<ExpressionPtr> should;
		should.insert(builder.intLit(14));
		should.insert(builder.intLit(16));
		auto valZ = cba::getValuesOf(context, solution, varZ);
		EXPECT_EQ(should, valZ);

	}

	TEST(CBA, ReturnValue) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
				"{"
				"	int<4> x = 12;"
				"	auto y = (int z)->int<4> { return 10; };"
				"	int<4> z = y(x);"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(code);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
		// std::cout << "Constraint: " << constraints << "\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
		// std::cout << "Solutions:  " << solution << "\n";

		auto declZ = CompoundStmtAddress(code)[2].as<DeclarationStmtAddress>();
		VariableAddress varZ = declZ->getVariable();

		// std::cout << *varZ << " = " << cba::getValuesOf(context, solution, varZ) << "\n";
		EXPECT_EQ("{AP(10)}", toString(cba::getValuesOf(context, solution, varZ)));
	}

	TEST(CBA, ReturnValue2) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
				"{"
				"	let id = ('a x)->'a { return x; };"
				"	auto x = 1;"
				"	auto x = id(2);"
				"	auto x = id(x);"
				"	auto x = id(id(3));"
				"	auto x = id(id(x));"
				"	auto x = id(id(id(4)));"
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(code);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
//		std::cout << "Solutions:  " << solution << "\n";

		vector<DeclarationStmtAddress> decls;
		for(auto cur : CompoundStmtAddress(code)) { decls.push_back(cur.as<DeclarationStmtAddress>()); }

		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, decls[0]->getVariable())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, decls[1]->getVariable())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, decls[2]->getVariable())));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, decls[3]->getVariable())));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, decls[4]->getVariable())));
		EXPECT_EQ("{AP(4)}", toString(cba::getValuesOf(context, solution, decls[5]->getVariable())));

	}

	namespace {

		void createDotDump(const CBAContext& context, const Constraints& constraints) {
			{
				// open file
				std::ofstream out("constraints.dot", std::ios::out );

				// write file
				context.plot(constraints, out);
			}

			// create pdf
//			system("dot -Tpdf constraints.dot -o constraints.pdf");
			system("dot -Tsvg constraints.dot -o constraints.svg");
		}

	}

	TEST(CBA, References1) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
				"{"
				"	ref<int<4>> x = var(1);"		// set x to 1
				"	*x;"							// should be 1
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(code);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
//		std::cout << "Solutions:  " << solution << "\n";

		auto decl = CompoundStmtAddress(code)[0].as<DeclarationStmtAddress>();
//		auto R_decl = context.getSet(R, context.getLabel(decl->getInitialization()));
//		std::cout << "R[decl] = s" << R_decl << " = " << solution[R_decl] << "\n";


		auto val = CompoundStmtAddress(code)[1].as<ExpressionAddress>();

		// this would be the ideal case
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, val)));

	}

	TEST(CBA, References2) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
				"{"
				"	ref<int<4>> x = var(1);"		// set x to 1
				"	auto y = *x;"					// y should be 1
				"	x = 2;"							// set x to 2
				"	auto z = *x;"					// z should be 2
				"	x = 3;"							// set x to 3
				"	auto w = *x;"					// z should be 3
				"}"
		).as<CompoundStmtPtr>();

		EXPECT_TRUE(code);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
//		std::cout << "Solutions:  " << solution << "\n";

		auto varY = CompoundStmtAddress(code)[1].as<DeclarationStmtAddress>()->getVariable();
		auto varZ = CompoundStmtAddress(code)[3].as<DeclarationStmtAddress>()->getVariable();
		auto varW = CompoundStmtAddress(code)[5].as<DeclarationStmtAddress>()->getVariable();

		// this would be the ideal case
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, varY)));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, varZ)));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, varW)));

	}

	TEST(CBA, References3) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"

				// init variables
				"	ref<int<4>> x = var(1);"		// set x to 1
				"	ref<int<4>> y = var(2);"		// set y to 2
				"	ref<int<4>> z = x;"				// z is an alias of x
				"	ref<ref<int<4>>> p = var(y);"	// p is a pointer on y

				// read values
				"	*x;"
				"	*y;"
				"	*z;"
				"	**p;"

				// update values
				"	x = 3;"							// x and z should be 3

				// read values
				"	*x;"
				"	*y;"
				"	*z;"
				"	**p;"

				// update pointer
				"	p = z;"							// p should now reference z = x

				// read values
				"	*x;"
				"	*y;"
				"	*z;"
				"	**p;"

				// update pointer
				"	*p = 4;"							// *x = *z = **p should now be 4

				// read values
				"	*x;"
				"	*y;"
				"	*z;"
				"	**p;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBAContext context;
		Constraints constraints;
		auto time = TIME(constraints = generateConstraints(context, code));
		std::cout << "Constraint generation took: " << (time*1000) << "ms\n";
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		Solution solution;
		time = TIME(solution = cba::solve(constraints));
		std::cout << "Solving constraints took: " << (time*1000) << "ms\n";
//		std::cout << "Solutions:  " << solution << "\n";

		// read first set of values
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code[4].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code[5].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code[6].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code[7].as<ExpressionAddress>())));

		// read second set of values
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code[ 9].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code[10].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code[11].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code[12].as<ExpressionAddress>())));

		// read third set of values
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code[14].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code[15].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code[16].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code[17].as<ExpressionAddress>())));

		// read fourth set of values
		EXPECT_EQ("{AP(4)}", toString(cba::getValuesOf(context, solution, code[19].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code[20].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(4)}", toString(cba::getValuesOf(context, solution, code[21].as<ExpressionAddress>())));
		EXPECT_EQ("{AP(4)}", toString(cba::getValuesOf(context, solution, code[22].as<ExpressionAddress>())));

	}

	TEST(CBA, IfStmt1) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"

				// init variables
				"	ref<int<4>> x = var(1);"		// set x to 1
				"	*x;"							// should be 1
				"	if (x > 0) {"
				"		*x;"						// should be 1
				"		x = 2;"
				"		*x;"						// should be 2
				"	} else {"
				"		*x;"						// should be 1
				"		x = 3;"
				"		*x;"						// should be 3
				"	}"
				"	*x;"							// what is x? - conservative: {2,3}
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
//		std::cout << "Solutions:  " << solution << "\n";

		// check value of condition
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,0).as<ExpressionAddress>(), B)));

		// check value of *x
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(1).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,1,0).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,1,2).as<ExpressionAddress>())));
		EXPECT_EQ("{}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,2,0).as<ExpressionAddress>())));		// never evaluated!
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,2,2).as<ExpressionAddress>())));

		// the last is fixed since we know the condition
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(3).as<ExpressionAddress>())));
	}

	TEST(CBA, IfStmt2) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string, NodePtr> symbols;
		symbols["e"] = builder.literal("e", builder.getLangBasic().getInt4());

		auto in = builder.parseStmt(
				"{"

				// init variables
				"	ref<int<4>> x = var(1);"		// set x to 1
				"	*x;"							// should be 1
				"	if (x > e) {"
				"		*x;"						// should be 1
				"		x = 2;"
				"		*x;"						// should be 2
				"	} else {"
				"		*x;"						// should be 1
				"		x = 3;"
				"		*x;"						// should be 3
				"	}"
				"	*x;"							// what is x? - conservative: {2,3}
				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
//		std::cout << "Solutions:  " << solution << "\n";

		// check value of condition
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,0).as<ExpressionAddress>(), B)));

		// check value of *x
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(1).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,1,0).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(2)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,1,2).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,2,0).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(3)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,2,2).as<ExpressionAddress>())));

		// the last one may be both
		std::set<ExpressionPtr> should;
		should.insert(builder.intLit(2));
		should.insert(builder.intLit(3));
		EXPECT_EQ(should, cba::getValuesOf(context, solution, code.getAddressOfChild(3).as<ExpressionAddress>()));

	}

	TEST(CBA, WhileStmt) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	ref<int<4>> x = var(1);"		// set x to 1
				"	*x;"							// should be 1
				"	while (x > 0) {"
				"		*x;"						// should be 1 or unknown
				"		x = x - 1;"
				"		*x;"						// should be unknown
				"	}"
				"	*x;"							// what is x? - should be unknown
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBAContext context;
		auto constraints = generateConstraints(context, code);
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		auto solution = cba::solve(constraints);
//		std::cout << "Solutions:  " << solution << "\n";

		// check value of *x
		EXPECT_EQ("{AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(1).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(NULL),AP(1)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,1,0).as<ExpressionAddress>())));
		EXPECT_EQ("{AP(NULL)}", toString(cba::getValuesOf(context, solution, code.getAddressOfChild(2,1,2).as<ExpressionAddress>())));

		// the last one may be both
		std::set<ExpressionPtr> should;
		should.insert(ExpressionPtr());
		should.insert(builder.intLit(1));
		EXPECT_EQ(should, cba::getValuesOf(context, solution, code.getAddressOfChild(3).as<ExpressionAddress>()));

	}

	TEST(CBA, Arithmetic_101) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string, NodePtr> symbols;
		symbols["e"] = builder.literal("e", builder.getLangBasic().getInt4());

		auto in = builder.parseStmt(
				"{"
				// constants
				"	0;"						// should be 0
				"	1;"						// should be 1
				"	e;"						// should be unknown

				// constant expressions
				"	1+1;"
				"	1+1+1;"
				"	1+2*3;"
				"	17+4;"

				// constant expressions with variables
				"	auto x = 2;"
				"	auto y = 3;"
				"	x;"
				"	2*x+1;"
				"	2*x+4*y;"
				"	2*x+4*y+e;"

				"	ref<int<4>> z = var(10);"
				"	ref<int<4>> w = var(5);"
				"	ref<int<4>> a = z;"
				"	ref<ref<int<4>>> p = var(z);"

				"	*z;"
				"	*p+2*z;"

				"	p = w;"
				"	*p+2*z+4*e*(*p)+a;"

				// boolean constraints
				"	1 < 2;"
				"	2*x+1 < 2;"
				"	2*x+1 > 2*x;"
				"	2*x+1 == (2+1)*x;"
				"	2*x < e;"

//				// ternary operator
//				"	(x<2)?1:2;"				// x is 2 => should be 2
//				"	(x<3)?1:2;"				// x is 2 => should be 1
//				"	(x<y)?1:2;"				// y is 3 => should be 1
//				"	(x<e)?1:2;"				// unknown => should be {1,2}

				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBAContext context;
		Constraints constraints;
		auto time = TIME(constraints = generateConstraints(context, code));
		std::cout << "Constraint generation took: " << (time*1000) << "ms\n";
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		Solution solution;
		time = TIME(solution = cba::solve(constraints));
		std::cout << "Solving constraints took: " << (time*1000) << "ms\n";
//		std::cout << "Solutions:  " << solution << "\n";

		// check values
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[0].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[1].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{e}", toString(cba::getValuesOf(context, solution, code[2].as<ExpressionAddress>(), A)));

		EXPECT_EQ("{2}", toString(cba::getValuesOf(context, solution, code[3].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{3}", toString(cba::getValuesOf(context, solution, code[4].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{7}", toString(cba::getValuesOf(context, solution, code[5].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{21}", toString(cba::getValuesOf(context, solution, code[6].as<ExpressionAddress>(), A)));

		EXPECT_EQ("{2}", toString(cba::getValuesOf(context, solution, code[9].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{5}", toString(cba::getValuesOf(context, solution, code[10].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{16}", toString(cba::getValuesOf(context, solution, code[11].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{e+16}", toString(cba::getValuesOf(context, solution, code[12].as<ExpressionAddress>(), A)));

		EXPECT_EQ("{10}", toString(cba::getValuesOf(context, solution, code[17].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{30}", toString(cba::getValuesOf(context, solution, code[18].as<ExpressionAddress>(), A)));

		EXPECT_EQ("{20*e+35}", toString(cba::getValuesOf(context, solution, code[20].as<ExpressionAddress>(), A)));

		// boolean constraints
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[21].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[22].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[23].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[24].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[25].as<ExpressionAddress>(), B)));
	}


	TEST(CBA, Boolean_101) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<string, NodePtr> symbols;
		symbols["b"] = builder.literal("b", builder.getLangBasic().getBool());
		symbols["e"] = builder.literal("e", builder.getLangBasic().getInt4());

		auto in = builder.parseStmt(
				"{"
				// constants
				"	true;"						// should be 0
				"	false;"						// should be 1
				"	b;"							// should be unknown

				// boolean relations
				"	true == false;"
				"	true == true;"
				"	true != false;"
				"	true != true;"

				"	true == b;"
				"	false == b;"
				"	true != b;"
				"	false != b;"
				"	b == b;"
				"	b != b;"


//				// arithmetic comparison
				"	1<2;"
				"	2<1;"
				"	2<2;"
				"	1<e;"
				"	e<1;"

				"	1<=2;"
				"	2<=1;"
				"	2<=2;"
				"	1<=e;"
				"	e<=1;"

				"	1>=2;"
				"	2>=1;"
				"	2>=2;"
				"	1>=e;"
				"	e>=1;"

				"	1>2;"
				"	2>1;"
				"	2>2;"
				"	1>e;"
				"	e>1;"

				"	1==2;"
				"	2==2;"
				"	1==e;"

				"	1!=2;"
				"	2!=2;"
				"	1!=e;"

				"}",
				symbols
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBAContext context;
		Constraints constraints;
		auto time = TIME(constraints = generateConstraints(context, code));
		std::cout << "Constraint generation took: " << (time*1000) << "ms\n";
//		std::cout << "Constraint: {\n\t" << join("\n\t",constraints) << "\n}\n";
//		createDotDump(context, constraints);

		Solution solution;
		time = TIME(solution = cba::solve(constraints));
		std::cout << "Solving constraints took: " << (time*1000) << "ms\n";
//		std::cout << "Solutions:  " << solution << "\n";

		int i = 0;
		// check constants
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		// check boolean relations
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		// arithmetic comparison
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

		EXPECT_EQ("{1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));
		EXPECT_EQ("{0,1}", toString(cba::getValuesOf(context, solution, code[i++].as<ExpressionAddress>(), B)));

	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/cba_interface.h"

#include "insieme/core/ir_builder.h"

#include <iostream>

namespace insieme {
namespace analysis {

	using namespace core;
	using testing::Types;



	/**
	 * GTest-specific class to enable parametrized tests.
	 * The type-parametrized constructor fetches a function pointer to the
	 * analysis from the appropriate CBA backend to be used in the tests below.
	 */
	template <typename Backend>
	class CBA_Interface : public testing::Test {
	protected:
		CBA_Interface() : getDefinitionPoint(&(analysis<getDefinitionPointAnalysis,Backend>())) {}
		core::VariableAddress (*getDefinitionPoint)(const core::VariableAddress&);
	};

	/**
	 * Type list of backends to be tested
	 */
	using CBA_Backends = Types<datalogEngine,haskellEngine>;

	/**
	 * Tell GTest that CBA_Interface tests shall be type-parametrized with CBA_Backends
	 */
	TYPED_TEST_CASE(CBA_Interface, CBA_Backends);



	/**
	 * Test the definition point interface
	 */
	TYPED_TEST(CBA_Interface, DefinitionPoint_DeclarationStmt) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
			"{ var int<4> x = 12; $x$; }"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<VariableAddress>();
		auto param = var.getRootAddress().as<CompoundStmtAddress>()[0].as<DeclarationStmtAddress>()->getVariable();

		//std::cout << "Parameter: " << param << "\n";
		//std::cout << "Variable:  " << var << "\n";

		auto find = this->getDefinitionPoint(var);
		ASSERT_TRUE(find);
		ASSERT_EQ(param, find);
	}

	/**
	 * Test the definition point interface
	 */
	TYPED_TEST(CBA_Interface, DefinitionPoint_DeclarationStmt_2) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string,core::NodePtr> symbols;
		symbols["w"] = builder.variable(builder.parseType("int<4>"));

		auto addresses = builder.parseAddressesStatement(
			"{ "
			"	var int<4> x = 12; "
			"	var int<4> y = 12; "
			"	var int<4> z = 12; "
			"	$x$; "
			"	$y$; "
			"	$z$; "
			"   $w$; "
			"}", symbols
		);

		ASSERT_EQ(4, addresses.size());

		auto varX = addresses[0].as<VariableAddress>();
		auto varY = addresses[1].as<VariableAddress>();
		auto varZ = addresses[2].as<VariableAddress>();
		auto varW = addresses[3].as<VariableAddress>();

		auto comp = varX.getRootAddress().as<CompoundStmtAddress>();

		auto declX = comp[0].as<DeclarationStmtAddress>()->getVariable();
		auto declY = comp[1].as<DeclarationStmtAddress>()->getVariable();
		auto declZ = comp[2].as<DeclarationStmtAddress>()->getVariable();

		EXPECT_EQ(declX, this->getDefinitionPoint(varX));
		EXPECT_EQ(declY, this->getDefinitionPoint(varY));
		EXPECT_EQ(declZ, this->getDefinitionPoint(varZ));
		EXPECT_FALSE(this->getDefinitionPoint(varW));
	}

	/**
	 * Test the definition point interface
	 */
	TYPED_TEST(CBA_Interface, DefinitionPoint_DeclarationStmt_3) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string,core::NodePtr> symbols;
		symbols["w"] = builder.variable(builder.parseType("int<4>"));

		auto addresses = builder.parseAddressesStatement(
			"{ "
			"	var int<4> x = 12; "
			"	{"
			"		var int<4> y = 12; "
			"		$x$;"
			"		$y$;"
			"	}"
			"	var int<4> y = 12; "
			"	$x$; "
			"	$y$; "
			"}", symbols
		);

		ASSERT_EQ(4, addresses.size());

		auto varX1 = addresses[0].as<VariableAddress>();
		auto varY1 = addresses[1].as<VariableAddress>();
		auto varX2 = addresses[2].as<VariableAddress>();
		auto varY2 = addresses[3].as<VariableAddress>();

		auto comp1 = varX1.getRootAddress().as<CompoundStmtAddress>();
		auto comp2 = comp1[1].as<CompoundStmtAddress>();

		auto declX1 = comp1[0].as<DeclarationStmtAddress>()->getVariable();
		auto declY1 = comp2[0].as<DeclarationStmtAddress>()->getVariable();
		auto declY2 = comp1[2].as<DeclarationStmtAddress>()->getVariable();

		EXPECT_EQ(declX1, this->getDefinitionPoint(varX1));
		EXPECT_EQ(declY1, this->getDefinitionPoint(varY1));
		EXPECT_EQ(declX1, this->getDefinitionPoint(varX2));
		EXPECT_EQ(declY2, this->getDefinitionPoint(varY2));
	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_LambdaParameter) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesExpression(
			"( x : int<4> ) -> int<4> { return $x$; }"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<CallExprAddress>().getArgument(0).as<VariableAddress>();
		auto param = var.getRootAddress().as<LambdaExprAddress>()->getParameterList()[0];

		std::cout << "Parameter: " << param << "\n";
		std::cout << "Variable:  " << var << "\n";

		EXPECT_EQ(param, this->getDefinitionPoint(var));

	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_LambdaParameter_2) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
			"def f = (x : int<4>, y : int<4>) -> unit { $y$; };"
			"{"
			"  f(1, 2);"
			"}"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<CallExprAddress>()->getArgument(0).as<VariableAddress>();
		auto param = var.getParentAddress(4).as<LambdaAddress>()->getParameterList()[1];

		auto find = this->getDefinitionPoint(var);
		ASSERT_TRUE(find);
		ASSERT_EQ(param, find);
	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_BindParameter) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesExpression(
			"( x : int<4> ) => $x$"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<VariableAddress>();
		auto param = var.getRootAddress().as<BindExprAddress>()->getParameters()[0];

		std::cout << "Parameter: " << param << "\n";
		std::cout << "Variable:  " << var << "\n";

		auto definition = this->getDefinitionPoint(var);
		EXPECT_TRUE(definition);
		EXPECT_EQ(param, definition);

	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_BindParameter_2) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string,core::NodePtr> symbols;
		symbols["z"] = builder.variable(builder.parseType("int<4>"));

		auto addresses = builder.parseAddressesStatement(
			"{ var int<4> y = 4; ( x : int<4>, w : int<4> ) => $x$ + $y$ + $z$ + $w$; }",
			symbols
		);

		ASSERT_EQ(4, addresses.size());

		auto x = addresses[0].as<VariableAddress>();
		auto y = addresses[1].as<VariableAddress>();
		auto z = addresses[2].as<VariableAddress>();
		auto w = addresses[3].as<VariableAddress>();

		auto decl = x.getRootAddress().as<CompoundStmtAddress>()[0].as<DeclarationStmtAddress>();
		auto bind = x.getRootAddress().as<CompoundStmtAddress>()[1].as<BindExprAddress>();

		auto defX = bind->getParameters()[0];
		auto defY = decl->getVariable();
		auto defW = bind->getParameters()[1];

		EXPECT_EQ(defX,this->getDefinitionPoint(x));
		EXPECT_EQ(defY,this->getDefinitionPoint(y));
		EXPECT_FALSE(this->getDefinitionPoint(z));
		EXPECT_EQ(defW,this->getDefinitionPoint(w));

	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_BindParameter_3) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string,core::NodePtr> symbols;
		symbols["y"] = builder.variable(builder.parseType("bool"));

		auto addresses = builder.parseAddressesExpression(
			"(x:bool)=>($x$ && $y$)",
			symbols
		);

		ASSERT_EQ(2, addresses.size());

		auto x = addresses[0].as<VariableAddress>();
		auto y = addresses[1].as<CallExprAddress>()->getArgument(0).as<VariableAddress>();

		// there is a bug in the parser, marking the wrong y => fix this
		y = y.getParentAddress(10).as<CallExprAddress>().getArgument(0).as<VariableAddress>();

		auto bind = x.getRootAddress().as<BindExprAddress>();
		auto defX = bind->getParameters()[0];

		EXPECT_EQ(defX,this->getDefinitionPoint(x));
		EXPECT_FALSE(this->getDefinitionPoint(y));

	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_BindParameter_4) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string,core::NodePtr> symbols;
		symbols["x"] = builder.variable(builder.parseType("bool"));

		auto addresses = builder.parseAddressesExpression(
			"(y:bool)=>($x$ && $y$)",
			symbols
		);

		ASSERT_EQ(2, addresses.size());

		auto x = addresses[0].as<VariableAddress>();
		auto y = addresses[1].as<CallExprAddress>()->getArgument(0).as<VariableAddress>();

		// there is a bug in the parser, marking the wrong y => fix this
		y = y.getParentAddress(10).as<CallExprAddress>().getArgument(0).as<VariableAddress>();

		auto bind = x.getRootAddress().as<BindExprAddress>();
		auto defY = bind->getParameters()[0];

		EXPECT_FALSE(this->getDefinitionPoint(x));
		EXPECT_EQ(defY,this->getDefinitionPoint(y));

	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_Fail) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
			"{ var int<4> x = 12; $x$; }"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<VariableAddress>();
		var = var.switchRoot(var);

		EXPECT_FALSE(this->getDefinitionPoint(var));
	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_ForLoop) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
			"{ for (int<4> x = 0 .. 4) { $x$; } }"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<VariableAddress>();
		auto param = var.getRootAddress().as<CompoundStmtAddress>()[0].as<ForStmtAddress>()->getDeclaration()->getVariable();

		auto find = this->getDefinitionPoint(var);
		ASSERT_TRUE(find);
		ASSERT_EQ(param, find);
	}

	TYPED_TEST(CBA_Interface, DefinitionPoint_ForLoop_2) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto addresses = builder.parseAddressesStatement(
			"{ for (int<4> x = 0 .. 4) { for(int<4> y = 0 .. 4) { $x$; } } }"
		);

		ASSERT_EQ(1, addresses.size());

		auto var = addresses[0].as<VariableAddress>();
		auto param = var.getRootAddress().as<CompoundStmtAddress>()[0].as<ForStmtAddress>()->getDeclaration()->getVariable();

		auto find = this->getDefinitionPoint(var);
		ASSERT_TRUE(find);
		ASSERT_EQ(param, find);
	}

} // end namespace analysis
} // end namespace insieme

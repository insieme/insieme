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
#include <limits>
#include <algorithm>

#include "insieme/analysis/access.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/analysis/polyhedral/scop.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

TEST(Access, Scalars) {

	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

	{
		auto code = parser.parseExpression("ref<int<4>>:a");

		auto access = getImmediateAccess( ExpressionAddress(code) );
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_TRUE(access.isRef());
	}

	{
		auto code = parser.parseExpression("int<4>:a");

		auto access = getImmediateAccess( ExpressionAddress(code) );
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_FALSE(access.isRef());
	}

	{
		auto code = parser.parseExpression("(op<ref.deref>(ref<int<4>>:a))");

		auto access = getImmediateAccess( ExpressionAddress(code) );
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_FALSE(access.isRef());
	}


	{
		auto code = parser.parseExpression("ref<struct<a:int<4>,b:int<4>>>:s");

		auto access = getImmediateAccess( ExpressionAddress(code) );
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::SCALAR, access.getType());
		EXPECT_TRUE(access.isRef());
	}


}

TEST(Access, MemberAccess) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

	{
		auto code = parser.parseExpression(
			"(op<composite.ref.elem>(ref<struct<a:int<4>, b:int<4>>>:s, lit<identifier,a>, lit<type<int<4>>, int<4>))"
		);

		auto access = getImmediateAccess(ExpressionAddress(code));
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::MEMBER, access.getType());
		EXPECT_EQ(1u, access.getAccessedVariable()->getId());
		EXPECT_TRUE(access.isRef());
	}

	{
		auto code = parser.parseExpression(
			"(op<composite.member.access>(struct<a:int<4>, b:int<4>>:s, lit<identifier,a>, lit<type<int<4>>, int<4>))"
		);
		
		auto access = getImmediateAccess(ExpressionAddress(code));
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::MEMBER, access.getType());
		EXPECT_EQ(2u, access.getAccessedVariable()->getId());
		EXPECT_FALSE(access.isRef());
	}

}

// Wait for new parser 
TEST(Access, ArrayAccess) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);
	IRBuilder builder(mgr);

	{
		auto code = parser.parseExpression(
			"(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, 2))"
		);
		// std::cout << code << " " << *code->getType() << std::endl;
		auto access = getImmediateAccess( ExpressionAddress(code) );
		//std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		// EXPECT_TRUE(access.isRef());
	}

	{
		auto code = parser.parseExpression(
			"(op<array.subscript.1D>(array<int<4>,1>:v, 2))"
		);
		auto access = getImmediateAccess( ExpressionAddress(code) );
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		// EXPECT_FALSE(access.isRef());
	}

	{
		auto code = parser.parseExpression(
			"(op<vector.ref.elem>(ref<vector<int<4>,4>>:v, 2))"
		);
		// std::cout << code << " " << *code->getType() << std::endl;
		auto access = getImmediateAccess( ExpressionAddress(code) );
		// std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		// EXPECT_TRUE(access.isRef());
	}

	{
		auto code = parser.parseExpression(
			"(op<vector.subscript>(vector<int<4>,8>:v, 2))"
		);
		auto access = getImmediateAccess( ExpressionAddress(code) );
		std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		// EXPECT_FALSE(access.isRef());
	}

	{
		auto code = parser.parseExpression(
			"(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, uint<4>:b))"
		);

		auto access = getImmediateAccess( ExpressionAddress(code) );
		std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		// std::cout << access.getConstraint() << std::endl;
	}

	{
		auto code = parser.parseStatement(
			"if( (uint<4>:b>10) )"
			"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, b))"
		);

		// perform the polyhedral analysis 
		polyhedral::scop::mark(code);

		auto access = getImmediateAccess( StatementAddress(code).as<IfStmtAddress>()->getThenBody()->getStatement(0).
										  as<ExpressionAddress>() );
		 std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		EXPECT_TRUE(!!access.getConstraint());
		EXPECT_EQ("(v7 + -11*1 >= 0)", toString(*access.getConstraint()));

		EXPECT_EQ(code, access.getContext().getAddressedNode()); 
	}

	{
		auto code = parser.parseStatement(
			"if( (uint<4>:b>10) )"
			"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (b+5)))"
		);

		// perform the polyhedral analysis 
		polyhedral::scop::mark(code);

		auto access = getImmediateAccess( StatementAddress(code).as<IfStmtAddress>()->getThenBody()->getStatement(0).
				  						  as<ExpressionAddress>() );
		std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		EXPECT_TRUE(!!access.getConstraint());
		EXPECT_EQ("(v9 + -11*1 >= 0)", toString(*access.getConstraint()));

		EXPECT_EQ(code, access.getContext().getAddressedNode()); 
	}

{
		auto code = parser.parseStatement(
			"if( ((uint<4>:b>10) && (uint<4>:a<20)) )"
			"	(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (b+a)))"
		);

		// perform the polyhedral analysis 
		polyhedral::scop::mark(code);

		auto access = getImmediateAccess( StatementAddress(code).as<IfStmtAddress>()->getThenBody()->getStatement(0).
										  as<ExpressionAddress>() );
		std::cout << access << std::endl;
		EXPECT_EQ(VarType::ARRAY, access.getType());
		EXPECT_TRUE(access.isRef());
		EXPECT_TRUE(!!access.getConstraint());
		EXPECT_EQ("((-v12 + 19*1 >= 0) ^ (v11 + -11*1 >= 0))", toString(*access.getConstraint()));

		EXPECT_EQ(code, access.getContext().getAddressedNode()); 
	}

}

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

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

namespace insieme {
namespace backend {
namespace c_ast {

// a small test verifying that the given substr is contained within the given string
bool containsSubString(const string& str, const string& substr) {
	return str.find(substr) != string::npos;
}

TEST(C_AST, Basic) {

	// create a simple expression
	CNodeManager manager;

	TypePtr intType = manager.create<PrimitiveType>(PrimitiveType::Int32);
	EXPECT_TRUE(intType);
	EXPECT_EQ("int32_t", toC(intType));

	VariablePtr x = var(intType, "x");
	VariablePtr y = var(intType, "y");
	ExpressionPtr sum = add(x,y);

	EXPECT_EQ("x", x->name->name);

	EXPECT_EQ("x", toC(x));
	EXPECT_EQ("y", toC(y));
	EXPECT_EQ("x+y", toC(sum));

	sum = add(sum, x);
	EXPECT_EQ("x+y+x", toC(sum));

	sum = add(sum, parenthese(add(x,y)));
	EXPECT_EQ("x+y+x+(x+y)", toC(sum));

}

TEST(C_AST_Printer, ParameterFormatting) {

	CNodeManager manager;

	TypePtr type;

	StructTypePtr structType = manager.create<StructType>(manager.create("XY"));
	TypePtr intType = manager.create<PrimitiveType>(PrimitiveType::Int32);

	type = intType;
	structType->elements.push_back(var(type, "x1"));

	type = manager.create<PointerType>(type);
	structType->elements.push_back(var(type, "x2"));

	structType->elements.push_back(var(manager.create<PointerType>(type), "x3"));

	structType->elements.push_back(var(manager.create<VectorType>(intType, manager.create<Literal>("3")), "x4"));

	type = manager.create<VectorType>(type, manager.create<Literal>("3"));
	structType->elements.push_back(var(type, "x5"));

	type = manager.create<VectorType>(type, manager.create<Literal>("5"));
	structType->elements.push_back(var(type, "x6"));

	type = manager.create<PointerType>(type);
	structType->elements.push_back(var(type, "x7"));

	type = manager.create<PointerType>(type);
	structType->elements.push_back(var(type, "x8"));

	type = manager.create<VectorType>(type, manager.create<Literal>("7"));
	structType->elements.push_back(var(type, "x9"));

	type = manager.create<PointerType>(manager.create<FunctionType>(intType, toVector(intType)));
	structType->elements.push_back(var(type, "x10"));

	NodePtr def = manager.create<TypeDefinition>(structType);

	auto code = toC(def);
	EXPECT_PRED2(containsSubString, code, "int32_t x1");
	EXPECT_PRED2(containsSubString, code, "int32_t* x2");
	EXPECT_PRED2(containsSubString, code, "int32_t** x3");
	EXPECT_PRED2(containsSubString, code, "int32_t x4[3]");
	EXPECT_PRED2(containsSubString, code, "int32_t* x5[3]");
	EXPECT_PRED2(containsSubString, code, "int32_t* x6[5][3]");
	EXPECT_PRED2(containsSubString, code, "int32_t*(* x7)[5][3]");
	EXPECT_PRED2(containsSubString, code, "int32_t*(** x8)[5][3]");
	EXPECT_PRED2(containsSubString, code, "int32_t*(** x9[7])[5][3]");
	EXPECT_PRED2(containsSubString, code, "int32_t(* x10)(int32_t)");

}

TEST(C_AST_Printer, ConstPointerParameterPrinter) {

	CNodeManager manager;

	TypePtr type;

	StructTypePtr structType = manager.create<StructType>(manager.create("XY"));
	TypePtr intType = manager.create<PrimitiveType>(PrimitiveType::Int32);

	structType->elements.push_back(var(intType, "x1"));

	structType->elements.push_back(var(c_ast::ptr(intType), "x2"));

	structType->elements.push_back(var(c_ast::ptr(intType, true), "x3"));

	structType->elements.push_back(var(c_ast::ptr(c_ast::ptr(intType, true), true), "x4"));

	structType->elements.push_back(var(c_ast::ptr(c_ast::ptr(intType, true), false), "x5"));

	structType->elements.push_back(var(c_ast::ptr(c_ast::ptr(intType, false), true), "x6"));

	structType->elements.push_back(var(c_ast::ptr(c_ast::ptr(c_ast::ptr(intType, false), true), false), "x7"));

	structType->elements.push_back(var(c_ast::ptr(c_ast::ptr(c_ast::ptr(intType, true), false), true), "x8"));

	NodePtr def = manager.create<TypeDefinition>(structType);

	auto code = toC(def);
	EXPECT_PRED2(containsSubString, code, "int32_t x1");
	EXPECT_PRED2(containsSubString, code, "int32_t* x2");
	EXPECT_PRED2(containsSubString, code, "int32_t*const x3");
	EXPECT_PRED2(containsSubString, code, "int32_t*const*const x4");
	EXPECT_PRED2(containsSubString, code, "int32_t*const* x5");
	EXPECT_PRED2(containsSubString, code, "int32_t**const x6");
	EXPECT_PRED2(containsSubString, code, "int32_t**const* x7");
	EXPECT_PRED2(containsSubString, code, "int32_t*const**const x8");

}


int f(int(*fun)(int, int)) {
	return fun(1,2);
}

int f(int(*(*fun)[4])(int, int)) {
	return (*fun)[1](1,2);
}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme


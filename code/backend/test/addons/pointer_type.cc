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

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/pointer.h"

namespace insieme {
namespace backend {
namespace runtime {

	using namespace core;
	using namespace core::lang;


	TEST(PointerAddon, Types) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// create a backend instance
		Converter converter(mgr);
		ConversionContext context(converter);

		// install pointer addon
		addons::PointerType().installOn(converter);

		// get type manager
		TypeManager& typeManager = converter.getTypeManager();

		TypeInfo info = typeManager.getTypeInfo(context, builder.parseType("ptr<char>"));
		EXPECT_EQ("char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("char*", c_ast::toC(info.externalType));

		info = typeManager.getTypeInfo(context, builder.parseType("ptr<char,t,f>"));
		EXPECT_EQ("const char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("const char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("const char*", c_ast::toC(info.externalType));

		info = typeManager.getTypeInfo(context, builder.parseType("ptr<char,f,t>"));
		EXPECT_EQ("volatile char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("volatile char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("volatile char*", c_ast::toC(info.externalType));

		info = typeManager.getTypeInfo(context, builder.parseType("ptr<char,t,t>"));
		EXPECT_EQ("const volatile char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("const volatile char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("const volatile char*", c_ast::toC(info.externalType));


		// -- nested pointers --

		info = typeManager.getTypeInfo(context, builder.parseType("ptr<ptr<char,t,f>,f,t>"));
		EXPECT_EQ("const char* volatile*", c_ast::toC(info.lValueType));
		EXPECT_EQ("const char* volatile*", c_ast::toC(info.rValueType));
		EXPECT_EQ("const char* volatile*", c_ast::toC(info.externalType));

	}

	TEST(PointerAddon, Operators) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		// create a backend instance
		Converter converter(mgr);

		// install pointer addon
		addons::PointerType().installOn(converter);

		// get type manager
		ConversionContext ctxt(converter);
		StmtConverter& stmtConverter = converter.getStmtConverter();


		// --- build a parser utility supporting pointer constructs ---

		// import symbols into a symbol map
		lang::symbol_map symbols = mgr.getLangExtension<core::lang::PointerExtension>().getSymbols();

		// add a few extra symbols for the test cases
		symbols["a"] = [&]()->NodePtr { return builder.parseExpr("lit(\"a\":int<4>)"); };
		symbols["x"] = [&]()->NodePtr { return builder.parseExpr("lit(\"x\":ptr<char,f,f>)"); };
		symbols["y"] = [&]()->NodePtr { return builder.parseExpr("lit(\"y\":ptr<char,f,f>)"); };
		symbols["r"] = [&]()->NodePtr { return builder.parseExpr("lit(\"r\":ref<ptr<char,f,f>>)"); };
		symbols["m"] = [&]()->NodePtr { return builder.parseExpr("lit(\"m\":ref<array<char,12>>)"); };
		symbols["n"] = [&]()->NodePtr { return builder.parseExpr("lit(\"n\":ref<array<char,inf>>)"); };


		// load pointer extension
		auto convert = [&](const std::string& ir) {
			return toC(stmtConverter.convertExpression(ctxt, builder.parseExpr(ir, symbols)));
		};


		// --- run tests --

		// check constants
		EXPECT_EQ("x", convert("x"));

		// check constructors
		EXPECT_EQ(               "(char*)0", convert("ptr_null(type_lit(char),type_lit(f),type_lit(f))"));
		EXPECT_EQ(         "(const char*)0", convert("ptr_null(type_lit(char),type_lit(t),type_lit(f))"));
		EXPECT_EQ(      "(volatile char*)0", convert("ptr_null(type_lit(char),type_lit(f),type_lit(t))"));
		EXPECT_EQ("(const volatile char*)0", convert("ptr_null(type_lit(char),type_lit(t),type_lit(t))"));

		// check casts
		EXPECT_EQ(                      "x", convert("ptr_cast(x,type_lit(f),type_lit(f))"));
		EXPECT_EQ(         "(const char*)x", convert("ptr_cast(x,type_lit(t),type_lit(f))"));
		EXPECT_EQ(      "(volatile char*)x", convert("ptr_cast(x,type_lit(f),type_lit(t))"));
		EXPECT_EQ("(const volatile char*)x", convert("ptr_cast(x,type_lit(t),type_lit(t))"));

		EXPECT_EQ("m.data", convert("ptr_from_array(m)"));
		EXPECT_EQ("n", convert("ptr_from_array(n)"));

		// check pointer subscript
		EXPECT_EQ("&x[5]", convert("ptr_subscript(x,5)"));
		EXPECT_EQ("&x[a]", convert("ptr_subscript(x,a)"));

		// check pointer operators
		EXPECT_EQ("*x", convert("ptr_deref(x)"));

		// check pointer comparisons
		EXPECT_EQ("x == y", convert("ptr_eq(x,y)"));
		EXPECT_EQ("x != y", convert("ptr_ne(x,y)"));
		EXPECT_EQ( "x < y", convert("ptr_lt(x,y)"));
		EXPECT_EQ("x <= y", convert("ptr_le(x,y)"));
		EXPECT_EQ( "x > y", convert("ptr_gt(x,y)"));
		EXPECT_EQ("x >= y", convert("ptr_ge(x,y)"));

		// check pointer arithmetic
		EXPECT_EQ("x + 5", convert("ptr_add(x,5)"));
		EXPECT_EQ("x + a", convert("ptr_add(x,a)"));
		EXPECT_EQ("x - 5", convert("ptr_sub(x,5)"));
		EXPECT_EQ("x - a", convert("ptr_sub(x,a)"));
		EXPECT_EQ(  "r++", convert("ptr_post_inc(r)"));
		EXPECT_EQ(  "r--", convert("ptr_post_dec(r)"));
		EXPECT_EQ(  "++r", convert("ptr_pre_inc(r)"));
		EXPECT_EQ(  "--r", convert("ptr_pre_dec(r)"));
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

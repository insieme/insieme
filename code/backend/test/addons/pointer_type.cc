/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

		// install pointer addon
		addons::PointerType().installOn(converter);

		// get type manager
		TypeManager& typeManager = converter.getTypeManager();

		TypeInfo info = typeManager.getTypeInfo(builder.parseType("ptr<char>"));
		EXPECT_EQ("char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("char*", c_ast::toC(info.externalType));

		info = typeManager.getTypeInfo(builder.parseType("ptr<char,t,f>"));
		EXPECT_EQ("const char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("const char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("const char*", c_ast::toC(info.externalType));

		info = typeManager.getTypeInfo(builder.parseType("ptr<char,f,t>"));
		EXPECT_EQ("volatile char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("volatile char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("volatile char*", c_ast::toC(info.externalType));

		info = typeManager.getTypeInfo(builder.parseType("ptr<char,t,t>"));
		EXPECT_EQ("const volatile char*", c_ast::toC(info.lValueType));
		EXPECT_EQ("const volatile char*", c_ast::toC(info.rValueType));
		EXPECT_EQ("const volatile char*", c_ast::toC(info.externalType));


		// -- nested pointers --

		info = typeManager.getTypeInfo(builder.parseType("ptr<ptr<char,t,f>,f,t>"));
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
		symbols["x"] = [&]()->NodePtr { return builder.parseExpr("lit(\"x\":ptr<char,f,f>)"); };

		// load pointer extension
		auto convert = [&](const std::string& ir) {
			return toC(stmtConverter.convertExpression(ctxt, builder.parseExpr(ir, symbols)));
		};


		// --- run tests --

		// check constants
		EXPECT_EQ("x", convert("x"));

		// check constructors
//		EXPECT_EQ(  "(char*)0",  convert("ptr_null(lit(char),lit(f),lit(f))"));

		// check casts
		EXPECT_EQ(                      "x", convert("ptr_cast(x,lit(f),lit(f))"));
		EXPECT_EQ(         "(const char*)x", convert("ptr_cast(x,lit(t),lit(f))"));
		EXPECT_EQ(      "(volatile char*)x", convert("ptr_cast(x,lit(f),lit(t))"));
		EXPECT_EQ("(const volatile char*)x", convert("ptr_cast(x,lit(t),lit(t))"));

		// check pointer operators
//
//		EXPECT_EQ(        "*x",  convert("ptr_deref(x)"));

	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

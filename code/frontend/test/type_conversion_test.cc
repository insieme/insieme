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

#include "program.h"
#include "clang_compiler.h"
#include "conversion.h"

#include "clang/AST/Stmt.h"
#include "clang/AST/Type.h"

using namespace insieme::frontend;
using namespace insieme::core;

#define CHECK_BUILTIN_TYPE(TypeName, InsiemeTypeDesc) \
	{ TypePtr convType = convFactory.ConvertType( clang::BuiltinType(clang::BuiltinType::TypeName) ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, convType->getName()); }

TEST(TypeConversion, HandleBuildinTypes) {

	ProgramPtr prog = Program::create();
	insieme::ConversionFactory convFactory( prog->getNodeManager() );

	// VOID
	CHECK_BUILTIN_TYPE(Void, "unit");
	// BOOL
	CHECK_BUILTIN_TYPE(Bool, "bool");

	// UChar
	CHECK_BUILTIN_TYPE(UChar, "uchar");
	// Char
	CHECK_BUILTIN_TYPE(SChar, "char");
	// Char16
	CHECK_BUILTIN_TYPE(Char16, "char<2>");
	// Char32
	CHECK_BUILTIN_TYPE(Char32, "char<4>");
	// WChar
	CHECK_BUILTIN_TYPE(WChar, "wchar");

	// UShort
	CHECK_BUILTIN_TYPE(UShort, "uint<2>");
	// Short
	CHECK_BUILTIN_TYPE(Short, "int<2>");
	// UInt
	CHECK_BUILTIN_TYPE(UInt, "uint<4>");
	// INT
	CHECK_BUILTIN_TYPE(Int, "int<4>");

	// ULong
	CHECK_BUILTIN_TYPE(ULong, "uint<8>");
	CHECK_BUILTIN_TYPE(ULongLong, "uint<8>");

	CHECK_BUILTIN_TYPE(Long, "int<8>");
	CHECK_BUILTIN_TYPE(LongLong, "int<8>");

	// UInt128
	CHECK_BUILTIN_TYPE(UInt128, "uint<16>");

	// Float
	CHECK_BUILTIN_TYPE(Float, "real<4>");
	// Double
	CHECK_BUILTIN_TYPE(Double, "real<8>");
	// LongDouble
	CHECK_BUILTIN_TYPE(LongDouble, "real<16>");

}

TEST(TypeConversion, HandlePointerTypes) {

	ProgramPtr prog = Program::create();
	insieme::ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	clang::BuiltinType intTy(clang::BuiltinType::Int);
	clang::QualType pointerTy = clang.getASTContext().getPointerType(clang::QualType(&intTy, 0));

	TypePtr insiemeTy = convFactory.ConvertType( *pointerTy.getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("ref<int<4>>", insiemeTy->toString());

}

TEST(TypeConversion, HandleReferenceTypes) {

	ProgramPtr prog = Program::create();
	insieme::ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	clang::BuiltinType intTy(clang::BuiltinType::Int);
	clang::QualType refTy = clang.getASTContext().getLValueReferenceType(clang::QualType(&intTy, 0));

	TypePtr insiemeTy = convFactory.ConvertType( *refTy.getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("ref<int<4>>", insiemeTy->toString());

}

TEST(TypeConversion, HandleStructTypes) {

}


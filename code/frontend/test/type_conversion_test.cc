/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#include <map>

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <clang/AST/ASTContext.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Type.h>
#include <clang/AST/Decl.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/translation_unit.h"
#include "insieme/utils/config.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/type_converter.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/logging.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"

#include "test_utils.inc"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::utils::log;
using namespace insieme::frontend::conversion;

namespace fe = insieme::frontend;

#define CHECK_BUILTIN_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	TypePtr convType = convFactory.convertType( builtin.getCanonicalTypeInternal() ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }



TEST(TypeConversion, HandleBuildinType) {

	Logger::get(std::cerr, INFO);

	NodeManager manager;
	fe::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/emptyFile.cpp");		// just using some dummy file ..

	// VOID
	CHECK_BUILTIN_TYPE(Void, "unit");

	// BOOL
	CHECK_BUILTIN_TYPE(Bool, "bool");

	// UChar
	CHECK_BUILTIN_TYPE(UChar, "uint<1>");
	// Char
	CHECK_BUILTIN_TYPE(SChar, "char");
	// Char16
	CHECK_BUILTIN_TYPE(Char16, "wchar<16>");
	// Char32
	CHECK_BUILTIN_TYPE(Char32, "wchar<32>");
	// WChar
	// CHECK_BUILTIN_TYPE(WChar, "wchar");  removed during port to clang2.9

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
	CHECK_BUILTIN_TYPE(ULongLong, "uint<16>");

	CHECK_BUILTIN_TYPE(Long, "int<8>");
	CHECK_BUILTIN_TYPE(LongLong, "int<16>");

	// UInt128
	CHECK_BUILTIN_TYPE(UInt128, "uint<16>");

	// Float
	CHECK_BUILTIN_TYPE(Float, "real<4>");
	// Double
	CHECK_BUILTIN_TYPE(Double, "real<8>");
	// LongDouble
	CHECK_BUILTIN_TYPE(LongDouble, "real<16>");

}

#define CHECK_POINTER_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	auto ptrType = ASTctx.getPointerType ( builtin.getCanonicalTypeInternal() );\
	TypePtr convType = convFactory.convertType( ptrType ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

#define CHECK_CONST_POINTER_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	auto ptrType = ASTctx.getPointerType ( builtin.getCanonicalTypeInternal() ).withConst();\
	TypePtr convType = convFactory.convertType( ptrType ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

#define CHECK_POINTER_CONST_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	auto ptrType = ASTctx.getPointerType ( builtin.getCanonicalTypeInternal().withConst() );\
	TypePtr convType = convFactory.convertType( ptrType ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

#define CHECK_CONST_POINTER_CONST_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	auto ptrType = ASTctx.getPointerType ( builtin.getCanonicalTypeInternal().withConst() ).withConst();\
	TypePtr convType = convFactory.convertType( ptrType ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

TEST(TypeConversion, PointerToType) {
	Logger::get(std::cerr, INFO);

	NodeManager manager;
	fe::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/emptyFile.cpp");		// just using some dummy file ..

	const fe::ClangCompiler& clang = tu.getCompiler();
	auto& ASTctx = clang.getASTContext();

	CHECK_POINTER_TYPE(Void, 	"ref<any>");
	CHECK_POINTER_TYPE(Bool, 	"ref<array<bool,1>>");
	CHECK_POINTER_TYPE(UChar, 	"ref<array<uint<1>,1>>");
	CHECK_POINTER_TYPE(SChar, 	"ref<array<char,1>>");
	CHECK_POINTER_TYPE(Char16, 	"ref<array<wchar<16>,1>>");
	CHECK_POINTER_TYPE(Char32, 	"ref<array<wchar<32>,1>>");
	CHECK_POINTER_TYPE(UShort, 	"ref<array<uint<2>,1>>");
	CHECK_POINTER_TYPE(Short, 	"ref<array<int<2>,1>>");
	CHECK_POINTER_TYPE(UInt, 	"ref<array<uint<4>,1>>");
	CHECK_POINTER_TYPE(Int, 	"ref<array<int<4>,1>>");
	CHECK_POINTER_TYPE(ULong, 	"ref<array<uint<8>,1>>");
	CHECK_POINTER_TYPE(ULongLong, "ref<array<uint<16>,1>>");
	CHECK_POINTER_TYPE(Long, 	"ref<array<int<8>,1>>");
	CHECK_POINTER_TYPE(LongLong, "ref<array<int<16>,1>>");
	CHECK_POINTER_TYPE(UInt128, "ref<array<uint<16>,1>>");
	CHECK_POINTER_TYPE(Float, 	"ref<array<real<4>,1>>");
	CHECK_POINTER_TYPE(Double, 	"ref<array<real<8>,1>>");
	CHECK_POINTER_TYPE(LongDouble, "ref<array<real<16>,1>>");

	CHECK_CONST_POINTER_TYPE(Void, 		"ref<any>");
	CHECK_CONST_POINTER_TYPE(Bool, 		"ref<array<bool,1>>");
	CHECK_CONST_POINTER_TYPE(UChar, 	"ref<array<uint<1>,1>>");
	CHECK_CONST_POINTER_TYPE(SChar, 	"ref<array<char,1>>");
	CHECK_CONST_POINTER_TYPE(Char16, 	"ref<array<wchar<16>,1>>");
	CHECK_CONST_POINTER_TYPE(Char32, 	"ref<array<wchar<32>,1>>");
	CHECK_CONST_POINTER_TYPE(UShort, 	"ref<array<uint<2>,1>>");
	CHECK_CONST_POINTER_TYPE(Short, 	"ref<array<int<2>,1>>");
	CHECK_CONST_POINTER_TYPE(UInt, 		"ref<array<uint<4>,1>>");
	CHECK_CONST_POINTER_TYPE(Int, 		"ref<array<int<4>,1>>");
	CHECK_CONST_POINTER_TYPE(ULong, 	"ref<array<uint<8>,1>>");
	CHECK_CONST_POINTER_TYPE(ULongLong, "ref<array<uint<16>,1>>");
	CHECK_CONST_POINTER_TYPE(Long, 		"ref<array<int<8>,1>>");
	CHECK_CONST_POINTER_TYPE(LongLong, 	"ref<array<int<16>,1>>");
	CHECK_CONST_POINTER_TYPE(UInt128, 	"ref<array<uint<16>,1>>");
	CHECK_CONST_POINTER_TYPE(Float, 	"ref<array<real<4>,1>>");
	CHECK_CONST_POINTER_TYPE(Double, 	"ref<array<real<8>,1>>");
	CHECK_CONST_POINTER_TYPE(LongDouble,"ref<array<real<16>,1>>");

	CHECK_POINTER_CONST_TYPE(Void, 		"ref<any>");
	CHECK_POINTER_CONST_TYPE(Bool, 		"src<array<bool,1>>");
	CHECK_POINTER_CONST_TYPE(UChar, 	"src<array<uint<1>,1>>");
	CHECK_POINTER_CONST_TYPE(SChar, 	"src<array<char,1>>");
	CHECK_POINTER_CONST_TYPE(Char16, 	"src<array<wchar<16>,1>>");
	CHECK_POINTER_CONST_TYPE(Char32, 	"src<array<wchar<32>,1>>");
	CHECK_POINTER_CONST_TYPE(UShort, 	"src<array<uint<2>,1>>");
	CHECK_POINTER_CONST_TYPE(Short, 	"src<array<int<2>,1>>");
	CHECK_POINTER_CONST_TYPE(UInt, 		"src<array<uint<4>,1>>");
	CHECK_POINTER_CONST_TYPE(Int, 		"src<array<int<4>,1>>");
	CHECK_POINTER_CONST_TYPE(ULong, 	"src<array<uint<8>,1>>");
	CHECK_POINTER_CONST_TYPE(ULongLong, "src<array<uint<16>,1>>");
	CHECK_POINTER_CONST_TYPE(Long, 		"src<array<int<8>,1>>");
	CHECK_POINTER_CONST_TYPE(LongLong, 	"src<array<int<16>,1>>");
	CHECK_POINTER_CONST_TYPE(UInt128, 	"src<array<uint<16>,1>>");
	CHECK_POINTER_CONST_TYPE(Float, 	"src<array<real<4>,1>>");
	CHECK_POINTER_CONST_TYPE(Double, 	"src<array<real<8>,1>>");
	CHECK_POINTER_CONST_TYPE(LongDouble,"src<array<real<16>,1>>");

	CHECK_CONST_POINTER_CONST_TYPE(Void, 	  "ref<any>");
	CHECK_CONST_POINTER_CONST_TYPE(Bool, 	  "src<array<bool,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(UChar, 	  "src<array<uint<1>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(SChar, 	  "src<array<char,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Char16, 	  "src<array<wchar<16>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Char32, 	  "src<array<wchar<32>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(UShort, 	  "src<array<uint<2>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Short, 	  "src<array<int<2>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(UInt, 	  "src<array<uint<4>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Int, 	  "src<array<int<4>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(ULong, 	  "src<array<uint<8>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(ULongLong, "src<array<uint<16>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Long, 	  "src<array<int<8>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(LongLong,  "src<array<int<16>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(UInt128,   "src<array<uint<16>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Float, 	  "src<array<real<4>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(Double, 	  "src<array<real<8>,1>>");
	CHECK_CONST_POINTER_CONST_TYPE(LongDouble,"src<array<real<16>,1>>");
}

#define CHECK_REFERENCE_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	auto ptrType = ASTctx.getLValueReferenceType ( builtin.getCanonicalTypeInternal() );\
	TypePtr convType = convFactory.convertType( ptrType ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

#define CHECK_REFERENCE_CONST_TYPE(TypeName, InsiemeTypeDesc) \
	{ Converter convFactory( manager, tu);\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	auto ptrType = ASTctx.getLValueReferenceType ( builtin.getCanonicalTypeInternal().withConst() );\
	TypePtr convType = convFactory.convertType( ptrType ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

TEST(TypeConversion, References) {
	Logger::get(std::cerr, INFO);

	NodeManager manager;
	fe::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/emptyFile.cpp");		// just using some dummy file ..

	const fe::ClangCompiler& clang = tu.getCompiler();
	auto& ASTctx = clang.getASTContext();

	CHECK_REFERENCE_TYPE(Void, 		"struct<_cpp_ref:ref<unit>>");  // <== this is actually not a type...
	CHECK_REFERENCE_TYPE(Bool, 		"struct<_cpp_ref:ref<bool>>");
	CHECK_REFERENCE_TYPE(UChar, 	"struct<_cpp_ref:ref<uint<1>>>");
	CHECK_REFERENCE_TYPE(SChar, 	"struct<_cpp_ref:ref<char>>");
	CHECK_REFERENCE_TYPE(Char16, 	"struct<_cpp_ref:ref<wchar<16>>>");
	CHECK_REFERENCE_TYPE(Char32, 	"struct<_cpp_ref:ref<wchar<32>>>");
	CHECK_REFERENCE_TYPE(UShort, 	"struct<_cpp_ref:ref<uint<2>>>");
	CHECK_REFERENCE_TYPE(Short, 	"struct<_cpp_ref:ref<int<2>>>");
	CHECK_REFERENCE_TYPE(UInt, 		"struct<_cpp_ref:ref<uint<4>>>");
	CHECK_REFERENCE_TYPE(Int, 		"struct<_cpp_ref:ref<int<4>>>");
	CHECK_REFERENCE_TYPE(ULong, 	"struct<_cpp_ref:ref<uint<8>>>");
	CHECK_REFERENCE_TYPE(ULongLong, "struct<_cpp_ref:ref<uint<16>>>");
	CHECK_REFERENCE_TYPE(Long, 		"struct<_cpp_ref:ref<int<8>>>");
	CHECK_REFERENCE_TYPE(LongLong, 	"struct<_cpp_ref:ref<int<16>>>");
	CHECK_REFERENCE_TYPE(UInt128, 	"struct<_cpp_ref:ref<uint<16>>>");
	CHECK_REFERENCE_TYPE(Float, 	"struct<_cpp_ref:ref<real<4>>>");
	CHECK_REFERENCE_TYPE(Double, 	"struct<_cpp_ref:ref<real<8>>>");
	CHECK_REFERENCE_TYPE(LongDouble,"struct<_cpp_ref:ref<real<16>>>");

	CHECK_REFERENCE_CONST_TYPE(Void, 	  "struct<_const_cpp_ref:src<unit>>");  // <== this is actually not a type...
	CHECK_REFERENCE_CONST_TYPE(Bool, 	  "struct<_const_cpp_ref:src<bool>>");
	CHECK_REFERENCE_CONST_TYPE(UChar, 	  "struct<_const_cpp_ref:src<uint<1>>>");
	CHECK_REFERENCE_CONST_TYPE(SChar, 	  "struct<_const_cpp_ref:src<char>>");
	CHECK_REFERENCE_CONST_TYPE(Char16, 	  "struct<_const_cpp_ref:src<wchar<16>>>");
	CHECK_REFERENCE_CONST_TYPE(Char32, 	  "struct<_const_cpp_ref:src<wchar<32>>>");
	CHECK_REFERENCE_CONST_TYPE(UShort, 	  "struct<_const_cpp_ref:src<uint<2>>>");
	CHECK_REFERENCE_CONST_TYPE(Short, 	  "struct<_const_cpp_ref:src<int<2>>>");
	CHECK_REFERENCE_CONST_TYPE(UInt, 	  "struct<_const_cpp_ref:src<uint<4>>>");
	CHECK_REFERENCE_CONST_TYPE(Int, 	  "struct<_const_cpp_ref:src<int<4>>>");
	CHECK_REFERENCE_CONST_TYPE(ULong, 	  "struct<_const_cpp_ref:src<uint<8>>>");
	CHECK_REFERENCE_CONST_TYPE(ULongLong, "struct<_const_cpp_ref:src<uint<16>>>");
	CHECK_REFERENCE_CONST_TYPE(Long, 	  "struct<_const_cpp_ref:src<int<8>>>");
	CHECK_REFERENCE_CONST_TYPE(LongLong,  "struct<_const_cpp_ref:src<int<16>>>");
	CHECK_REFERENCE_CONST_TYPE(UInt128,   "struct<_const_cpp_ref:src<uint<16>>>");
	CHECK_REFERENCE_CONST_TYPE(Float, 	  "struct<_const_cpp_ref:src<real<4>>>");
	CHECK_REFERENCE_CONST_TYPE(Double, 	  "struct<_const_cpp_ref:src<real<8>>>");
	CHECK_REFERENCE_CONST_TYPE(LongDouble,"struct<_const_cpp_ref:src<real<16>>>");
}

#define CREATE_TYPE(ClangType) \
	 convFactory.convertType( ClangType)

#define CREATE_PTR(ClangType) \
	 convFactory.convertType( ASTctx.getPointerType (ClangType) )

#define CREATE_REF(ClangType) \
	 convFactory.convertType( ASTctx.getLValueReferenceType (ClangType) )

#define CHECK_STRING_VALUE(IRType, StringValue) \
{\
	EXPECT_TRUE(IRType); \
	EXPECT_EQ(StringValue, toString(*IRType)); \
}

#define CHECK_TYPE(ClangType, IRValue, fullIRType) \
{\
	TypePtr convType = CREATE_TYPE( ClangType ); \
	CHECK_STRING_VALUE (convType, IRValue); \
 	convType = convFactory.getIRTranslationUnit().resolve(convType).as<TypePtr>(); \
	CHECK_STRING_VALUE (convType, fullIRType);  \
}

#define CHECK_POINTER(ClangType, IRValue, fullIRType) \
{\
	TypePtr convType = CREATE_PTR ( ClangType );\
	CHECK_STRING_VALUE (convType, IRValue); \
 	convType = convFactory.getIRTranslationUnit().resolve(convType).as<TypePtr>(); \
	CHECK_STRING_VALUE (convType, fullIRType);  \
}

#define CHECK_REFERENCE(ClangType, IRValue, fullIRType) \
{\
	TypePtr convType = CREATE_REF ( ClangType );\
	CHECK_STRING_VALUE (convType, IRValue); \
 	convType = convFactory.getIRTranslationUnit().resolve(convType).as<TypePtr>(); \
	CHECK_STRING_VALUE (convType, fullIRType);  \
}


TEST(TypeConversion, CombinedTypes) {
	Logger::get(std::cerr, INFO);

	NodeManager manager;
	fe::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/emptyFile.cpp");		// just using some dummy file ..
	Converter convFactory( manager, tu);

	const fe::ClangCompiler& clang = tu.getCompiler();
	auto& ASTctx = clang.getASTContext();

	////////////////////////////////////
	//  Create a class
	clang::SourceLocation loc;
	clang::RecordDecl *classDecl  = clang::CXXRecordDecl::Create( ASTctx,  clang::TagTypeKind::TTK_Class,
																ASTctx.getTranslationUnitDecl(), loc,
	                                     						loc, &ASTctx.Idents.get("BaseClass"));
	clang::BuiltinType fieldType (clang::BuiltinType::UShort);
	clang::FieldDecl *  fieldDecl = clang::FieldDecl::Create( ASTctx, classDecl, loc, loc,
															  &ASTctx.Idents.get("fieldA"), fieldType.getCanonicalTypeInternal(),
								 							  0, 0, false, clang::InClassInitStyle::ICIS_NoInit );
	fieldDecl->setAccess(clang::AccessSpecifier::AS_public );
	classDecl->startDefinition ();
	classDecl->addDeclInternal (fieldDecl);
	classDecl->setCompleteDefinition (true);

	// some check to make sure that TU does as expeced
	auto  irType = convFactory.convertType( ASTctx.getRecordType(classDecl)  );
	EXPECT_TRUE(irType);
	EXPECT_TRUE(irType.isa<core::GenericTypePtr>());
	EXPECT_TRUE(convFactory.getIRTranslationUnit()[irType.as<core::GenericTypePtr>()]);

	CHECK_TYPE		(ASTctx.getRecordType(classDecl),"BaseClass" ,
												   	 "struct BaseClass <fieldA:uint<2>>");
	CHECK_POINTER	(ASTctx.getRecordType(classDecl),"ref<array<BaseClass,1>>",
												   	 "ref<array<struct BaseClass <fieldA:uint<2>>,1>>");
	CHECK_REFERENCE	(ASTctx.getRecordType(classDecl),"struct<_cpp_ref:ref<BaseClass>>",
												     "struct<_cpp_ref:ref<struct BaseClass <fieldA:uint<2>>>>");

	CHECK_TYPE		(ASTctx.getRecordType(classDecl).withConst(),"BaseClass",
															     "struct BaseClass <fieldA:uint<2>>");
	CHECK_POINTER	(ASTctx.getRecordType(classDecl).withConst(),"src<array<BaseClass,1>>",
															     "src<array<struct BaseClass <fieldA:uint<2>>,1>>");
	CHECK_REFERENCE	(ASTctx.getRecordType(classDecl).withConst(),"struct<_const_cpp_ref:src<BaseClass>>",
															     "struct<_const_cpp_ref:src<struct BaseClass <fieldA:uint<2>>>>");

	////////////////////////////////////////////////////////
	//Now that we have a class type, lets make functions
	{
		clang::QualType resultType = clang::BuiltinType(clang::BuiltinType::Long).getCanonicalTypeInternal();
		std::vector<clang::QualType> args;
		args.push_back( resultType);
		args.push_back( ASTctx.getLValueReferenceType(resultType));
		args.push_back( ASTctx.getPointerType(resultType));
		auto funcTy = ASTctx.getFunctionType (resultType, args, clang::FunctionProtoType::ExtProtoInfo() );
		CHECK_TYPE		(funcTy, "((int<8>,struct<_cpp_ref:ref<int<8>>>,ref<array<int<8>,1>>)->int<8>)" ,
								 "((int<8>,struct<_cpp_ref:ref<int<8>>>,ref<array<int<8>,1>>)->int<8>)");
		CHECK_POINTER	(funcTy, "((int<8>,struct<_cpp_ref:ref<int<8>>>,ref<array<int<8>,1>>)->int<8>)" ,
								 "((int<8>,struct<_cpp_ref:ref<int<8>>>,ref<array<int<8>,1>>)->int<8>)");
		CHECK_REFERENCE	(funcTy, "struct<_cpp_ref:ref<((int<8>,struct<_cpp_ref:ref<int<8>>>,ref<array<int<8>,1>>)->int<8>)>>" ,
								 "struct<_cpp_ref:ref<((int<8>,struct<_cpp_ref:ref<int<8>>>,ref<array<int<8>,1>>)->int<8>)>>");
	}

	/////////////////////////////////////////////////////////
	// function with objects
	{
		clang::QualType resultType = ASTctx.getRecordType(classDecl);
		std::vector<clang::QualType> args;
		args.push_back( resultType);
		args.push_back( ASTctx.getLValueReferenceType( resultType));
		args.push_back( ASTctx.getPointerType( resultType));
		args.push_back( ASTctx.getLValueReferenceType( ASTctx.getPointerType( resultType)));
		auto funcTy = ASTctx.getFunctionType (resultType, args, clang::FunctionProtoType::ExtProtoInfo() );
		CHECK_TYPE		(funcTy, "((BaseClass,struct<_cpp_ref:ref<BaseClass>>,ref<array<BaseClass,1>>,struct<_cpp_ref:ref<ref<array<BaseClass,1>>>>)->BaseClass)" ,
								 "((struct BaseClass <fieldA:uint<2>>,struct<_cpp_ref:ref<struct BaseClass <fieldA:uint<2>>>>,ref<array<struct BaseClass <fieldA:uint<2>>,1>>,struct<_cpp_ref:ref<ref<array<struct BaseClass <fieldA:uint<2>>,1>>>>)->struct BaseClass <fieldA:uint<2>>)");
		CHECK_POINTER	(funcTy, "((BaseClass,struct<_cpp_ref:ref<BaseClass>>,ref<array<BaseClass,1>>,struct<_cpp_ref:ref<ref<array<BaseClass,1>>>>)->BaseClass)" ,
								 "((struct BaseClass <fieldA:uint<2>>,struct<_cpp_ref:ref<struct BaseClass <fieldA:uint<2>>>>,ref<array<struct BaseClass <fieldA:uint<2>>,1>>,struct<_cpp_ref:ref<ref<array<struct BaseClass <fieldA:uint<2>>,1>>>>)->struct BaseClass <fieldA:uint<2>>)");
		CHECK_REFERENCE	(funcTy, "struct<_cpp_ref:ref<((BaseClass,struct<_cpp_ref:ref<BaseClass>>,ref<array<BaseClass,1>>,struct<_cpp_ref:ref<ref<array<BaseClass,1>>>>)->BaseClass)>>" ,
								 "struct<_cpp_ref:ref<((struct BaseClass <fieldA:uint<2>>,struct<_cpp_ref:ref<struct BaseClass <fieldA:uint<2>>>>,ref<array<struct BaseClass <fieldA:uint<2>>,1>>,struct<_cpp_ref:ref<ref<array<struct BaseClass <fieldA:uint<2>>,1>>>>)->struct BaseClass <fieldA:uint<2>>)>>");
	}
}

TEST(TypeConversion, HandleRecursiveStructType) {

	Logger::get(std::cerr, INFO);

	NodeManager manager;
	fe::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/emptyFile.cpp");		// just using some dummy file ..
	Converter convFactory( manager, tu);

	const fe::ClangCompiler& clang = tu.getCompiler();
	auto& ASTctx = clang.getASTContext();

	// create rec struct
	clang::SourceLocation loc;
	clang::BuiltinType charTy (clang::BuiltinType::SChar);
	clang::RecordDecl *classDecl  = clang::CXXRecordDecl::Create( ASTctx,  clang::TagTypeKind::TTK_Class,
																ASTctx.getTranslationUnitDecl(), loc,
	                                     						loc, &ASTctx.Idents.get("RecClass"));

	clang::FieldDecl *  fieldOne = clang::FieldDecl::Create( ASTctx, classDecl, loc, loc,
															 &ASTctx.Idents.get("one"), charTy.getCanonicalTypeInternal(),
							 								 0, 0, false, clang::InClassInitStyle::ICIS_NoInit );
	fieldOne->setAccess(clang::AccessSpecifier::AS_public );

	auto  classTy = ASTctx.getRecordType(classDecl) ;
	clang::FieldDecl *  fieldPtr = clang::FieldDecl::Create( ASTctx, classDecl, loc, loc,
															 &ASTctx.Idents.get("rec_ptr"), ASTctx.getPointerType (classTy),
							 								 0, 0, false, clang::InClassInitStyle::ICIS_NoInit );
	fieldPtr->setAccess(clang::AccessSpecifier::AS_public );

	clang::FieldDecl *  fieldRef = clang::FieldDecl::Create( ASTctx, classDecl, loc, loc,
															 &ASTctx.Idents.get("ref"), ASTctx.getLValueReferenceType (classTy),
							 								 0, 0, false, clang::InClassInitStyle::ICIS_NoInit );
	fieldRef->setAccess(clang::AccessSpecifier::AS_public );

	clang::FieldDecl *  fieldConstRef = clang::FieldDecl::Create( ASTctx, classDecl, loc, loc,
															 &ASTctx.Idents.get("const_ref"), ASTctx.getLValueReferenceType (classTy.withConst()),
							 								 0, 0, false, clang::InClassInitStyle::ICIS_NoInit );
	fieldConstRef->setAccess(clang::AccessSpecifier::AS_public );

	classDecl->startDefinition ();
	classDecl->addDeclInternal (fieldOne);
	classDecl->addDeclInternal (fieldPtr);
	classDecl->addDeclInternal (fieldRef);
	classDecl->addDeclInternal (fieldConstRef);
	classDecl->setCompleteDefinition (true);

	CHECK_TYPE		(classTy, "RecClass",
							  "rec 'RecClass.{'RecClass=struct RecClass <one:char,rec_ptr:ref<array<'RecClass,1>>,ref:struct<_cpp_ref:ref<'RecClass>>,const_ref:struct<_const_cpp_ref:src<'RecClass>>>}");
	CHECK_POINTER	(classTy, "ref<array<RecClass,1>>",
							  "ref<array<rec 'RecClass.{'RecClass=struct RecClass <one:char,rec_ptr:ref<array<'RecClass,1>>,ref:struct<_cpp_ref:ref<'RecClass>>,const_ref:struct<_const_cpp_ref:src<'RecClass>>>},1>>");
	CHECK_REFERENCE	(classTy, "struct<_cpp_ref:ref<RecClass>>",
							  "struct<_cpp_ref:ref<rec 'RecClass.{'RecClass=struct RecClass <one:char,rec_ptr:ref<array<'RecClass,1>>,ref:struct<_cpp_ref:ref<'RecClass>>,const_ref:struct<_const_cpp_ref:src<'RecClass>>>}>>");
}


TEST(TypeConversion, FileTest) {
	Logger::get(std::cerr, INFO, 2);

	NodeManager manager;
	fe::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/types.c");

	auto filter = [](const fe::pragma::Pragma& curr){ return curr.getType() == "test"; };

	// we use an internal manager to have private counter for variables so we can write independent tests
	NodeManager mgr;


	fe::ConversionSetup setup;
	setup.frontendPluginInit();
	fe::conversion::Converter convFactory( mgr, tu, setup);
	convFactory.convert();

	auto resolve = [&](const core::NodePtr& cur) {
		return convFactory.getIRTranslationUnit().resolve(cur);
	};

	for(auto it = tu.pragmas_begin(filter), end = tu.pragmas_end(); it != end; ++it) {

		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*(*it));

		if(tp.isStatement()) {
            StatementPtr stmt = fe::fixVariableIDs(resolve(convFactory.convertStmt( tp.getStatement() ))).as<StatementPtr>();
            std::string match = toString(printer::PrettyPrinter(stmt, printer::PrettyPrinter::PRINT_SINGLE_LINE));
   			EXPECT_EQ(tp.getExpected(), '\"' + toString(printer::PrettyPrinter(stmt, printer::PrettyPrinter::PRINT_SINGLE_LINE)) + '\"' );
		} else {
			if(const clang::TypeDecl* td = llvm::dyn_cast<const clang::TypeDecl>( tp.getDecl() )) {
   				EXPECT_EQ(tp.getExpected(), '\"' + resolve(convFactory.convertType( td->getTypeForDecl()->getCanonicalTypeInternal() ))->toString() + '\"' );
			} else if(const clang::VarDecl* vd = llvm::dyn_cast<const clang::VarDecl>( tp.getDecl() )) {
				EXPECT_EQ(tp.getExpected(), '\"' + resolve(convFactory.convertVarDecl( vd ))->toString() + '\"' );
			}
		}
	}
}

TEST(TypeConversion, EnumTest) {
		insieme::frontend::Source file(
				R"(
                    enum Color {RED, BLUE=10};
                    int main() {
                        enum Color a;
                        a=RED;
                    }
				)"
		);
        core::NodeManager manager;
		core::IRBuilder builder(manager);
		auto irtu = insieme::frontend::ConversionJob(file).toIRTranslationUnit(manager);
		auto program = insieme::frontend::tu::toProgram(manager, irtu);
		//check for some enum features
		EXPECT_TRUE(toString(program).find("ref<__insieme_enum<_enum_") != std::string::npos);
		EXPECT_TRUE(toString(program).find("c221_45,__insieme_enum_constant__<_enumCtnt") != std::string::npos);
		EXPECT_TRUE(toString(program).find("c233_33,0>,__insieme_enum_constant__<_enumCtnt") != std::string::npos);
}

TEST(TypeConversion, FunctionPtr) {
		insieme::frontend::Source file(
				R"(
					int f() { return 1; }

					typedef int (f_t) ();
					int main() {
						f_t* a = f;
					}
				)"
		);
        core::NodeManager manager;
		core::IRBuilder builder(manager);
		auto irtu = insieme::frontend::ConversionJob(file).toIRTranslationUnit(manager);
		auto program = insieme::frontend::tu::toProgram(manager, irtu);

		dumpPretty(program);

		//check for some enum features
   		EXPECT_EQ("PROGRAM { \n// Entry Points: \n\trec v0.{v0=fun() {ref<(()->int<4>)> v1 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return 'a;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(rec v0.{v0=fun() {return 1;}});}}\n", toString(*program));
}

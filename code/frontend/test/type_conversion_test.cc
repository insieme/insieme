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

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <clang/AST/ASTContext.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Type.h>
#include <clang/AST/Decl.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/program.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/convert.h"
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
	{ Converter convFactory( manager, prog );\
	clang::BuiltinType builtin(clang::BuiltinType::TypeName); \
	TypePtr convType = convFactory.convertType( &builtin ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, toString(*convType)); }

TEST(TypeConversion, HandleBuildinType) {

	Logger::get(std::cerr, INFO);

	NodeManager manager;
	fe::Program prog(manager, SRC_DIR "/inputs/stmt.c");		// just using some dummy file ..

	// VOID
	CHECK_BUILTIN_TYPE(Void, "unit");

	// BOOL
	CHECK_BUILTIN_TYPE(Bool, "bool");

	// UChar
	CHECK_BUILTIN_TYPE(UChar, "uint<1>");
	// Char
	CHECK_BUILTIN_TYPE(SChar, "char");
	// Char16
	CHECK_BUILTIN_TYPE(Char16, "wchar16");
	// Char32
	CHECK_BUILTIN_TYPE(Char32, "wchar32");
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

//CXX Reference Type -- NOT SUPPORTED IN C
//TEST(TypeConversion, HandleReferenceType) {
//	using namespace clang;
//
//	NodeManager manager;
//	fe::Program prog(manager);
//	fe::TranslationUnit& tu = prog.createEmptyTranslationUnit();
//	const fe::ClangCompiler& clang = tu.getCompiler();
//	CXXConversionFactory convFactory( manager, prog );
//
//	clang::Type* intTy = new clang::BuiltinType(clang::BuiltinType::Int);
//	QualType refTy = clang.getASTContext().getLValueReferenceType(QualType(intTy, 0));
//
//	TypePtr insiemeTy = convFactory.convertType( refTy.getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("ref<int<4>>", insiemeTy->toString());
//
//	operator delete (intTy);
//}

//TEST(TypeConversion, HandleStructType) {
//	using namespace clang;
//
//	NodeManager manager;
//	fe::Program prog(manager);
//	fe::TranslationUnit& tu = prog.createEmptyTranslationUnit();
//	const fe::ClangCompiler& clang = tu.getCompiler();
//	ConversionFactory convFactory( manager, prog );
//	convFactory.setTranslationUnit(tu);
//
//	SourceLocation emptyLoc;
//
//	// cppcheck-suppress exceptNew
//	BuiltinType* charTy = new BuiltinType(BuiltinType::SChar);
//	// cppcheck-suppress exceptNew
//	BuiltinType* ushortTy = new BuiltinType(BuiltinType::UShort);
//
//	// create a struct:
//	// struct Person {
//	//	char* name;
//	//	unsigned short age;
//	// };
//	RecordDecl* decl = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
//			emptyLoc, clang.getPreprocessor().getIdentifierInfo("Person"));
//
//	// creates 'char* name' field
//	decl->addDecl(FieldDecl::Create(clang.getASTContext(), decl, emptyLoc,
//			clang.getPreprocessor().getIdentifierInfo("name"), clang.getASTContext().getPointerType(QualType(charTy, 0)), 0, 0, false));
//
//	// creates 'unsigned short age' field
//	decl->addDecl(FieldDecl::Create(clang.getASTContext(), decl, emptyLoc,
//			clang.getPreprocessor().getIdentifierInfo("age"), QualType(ushortTy,0), 0, 0, false));
//
//	decl->completeDefinition ();
//
//	// Gets the type for the record declaration
//	QualType type = clang.getASTContext().getTagDeclType(decl);
//
//	// convert the type into an IR type
//	TypePtr insiemeTy = convFactory.convertType( type.getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("struct<name:array<char,1>,age:uint<2>>", insiemeTy->toString());
//
//	operator delete (charTy);
//	operator delete (ushortTy);
//}

//TEST(TypeConversion, HandleRecursiveStructType) {
//
//	NodeManager manager;
//	fe::Program prog(manager);
//	fe::TranslationUnit& tu = prog.createEmptyTranslationUnit();
//	const fe::ClangCompiler& clang = tu.getCompiler();
//	ConversionFactory convFactory( manager, prog );
//
//	// cppcheck-suppress exceptNew
//	clang::BuiltinType* charTy = new clang::BuiltinType(clang::BuiltinType::SChar);
//	// cppcheck-suppress exceptNew
//	clang::BuiltinType* longTy = new clang::BuiltinType(clang::BuiltinType::Long);
//
//	clang::RecordDecl* decl = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
//			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("Person"));
//
//	clang::QualType declType = clang.getASTContext().getTagDeclType (decl);
//
//	decl->addDecl(clang::FieldDecl::Create(clang.getASTContext(), decl, clang::SourceLocation(),
//			clang.getPreprocessor().getIdentifierInfo("name"), clang.getASTContext().getPointerType(clang::QualType(charTy, 0)), 0, 0, false));
//
//	decl->addDecl(clang::FieldDecl::Create(clang.getASTContext(), decl, clang::SourceLocation(),
//			clang.getPreprocessor().getIdentifierInfo("age"), clang::QualType(longTy,0), 0, 0, false));
//
//	decl->addDecl(clang::FieldDecl::Create(clang.getASTContext(), decl, clang::SourceLocation(),
//			clang.getPreprocessor().getIdentifierInfo("mate"), clang.getASTContext().getPointerType(declType), 0, 0, false));
//	decl->completeDefinition();
//
//	TypePtr insiemeTy = convFactory.convertType( declType.getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("rec 'Person.{'Person=struct<name:array<char,1>,age:int<8>,mate:array<'Person,1>>}", insiemeTy->toString());
//
//	operator delete (charTy);
//	operator delete (longTy);
//}

//TEST(TypeConversion, HandleMutualRecursiveStructType) {
//	using namespace clang;
//
//	NodeManager manager;
//	fe::Program prog(manager);
//	fe::TranslationUnit& tu = prog.createEmptyTranslationUnit();
//	const fe::ClangCompiler& clang = tu.getCompiler();
//	ConversionFactory convFactory( manager, prog );
//
//	SourceLocation emptyLoc;
//
//	RecordDecl* declA = RecordDecl::Create(clang.getASTContext(), TTK_Struct, NULL, emptyLoc, clang.getPreprocessor().getIdentifierInfo("A"));
//	RecordDecl* declB = RecordDecl::Create(clang.getASTContext(), TTK_Struct, NULL,	emptyLoc, clang.getPreprocessor().getIdentifierInfo("B"));
//	RecordDecl* declC = RecordDecl::Create(clang.getASTContext(), TTK_Struct, NULL,	emptyLoc, clang.getPreprocessor().getIdentifierInfo("C"));
//	RecordDecl* declD = RecordDecl::Create(clang.getASTContext(), TTK_Struct, NULL,	emptyLoc, clang.getPreprocessor().getIdentifierInfo("D"));
//	RecordDecl* declE = RecordDecl::Create(clang.getASTContext(), TTK_Struct, NULL, emptyLoc, clang.getPreprocessor().getIdentifierInfo("E"));
//
//	declA->addDecl(FieldDecl::Create(clang.getASTContext(), declA, emptyLoc, clang.getPreprocessor().getIdentifierInfo("b"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declB)), 0, 0, false));
//
//	declA->completeDefinition();
//
//	declB->addDecl(FieldDecl::Create(clang.getASTContext(), declB, emptyLoc, clang.getPreprocessor().getIdentifierInfo("c"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declC)), 0, 0, false));
//
//	declB->completeDefinition();
//
//	declC->addDecl(FieldDecl::Create(clang.getASTContext(), declC, emptyLoc, clang.getPreprocessor().getIdentifierInfo("b"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declB)), 0, 0, false));
//
//	declC->addDecl(FieldDecl::Create(clang.getASTContext(), declC, emptyLoc, clang.getPreprocessor().getIdentifierInfo("a"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declA)), 0, 0, false));
//
//	declC->addDecl(FieldDecl::Create(clang.getASTContext(), declC, emptyLoc, clang.getPreprocessor().getIdentifierInfo("d"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declD)), 0, 0, false));
//
//	declC->completeDefinition();
//
//	declD->addDecl(FieldDecl::Create(clang.getASTContext(), declD, emptyLoc, clang.getPreprocessor().getIdentifierInfo("e"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declE)), 0, 0, false));
//	declD->completeDefinition();
//
////	declE->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declE, clang::SourceLocation(),
////			clang.getPreprocessor().getIdentifierInfo("b"),
////			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declB)), 0, 0, false));
////	declE->completeDefinition();
//
//	TypePtr insiemeTy = convFactory.convertType( clang.getASTContext().getTagDeclType(declA).getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("rec 'A.{'A=struct<b:array<'B,1>>, 'B=struct<c:array<'C,1>>, 'C=struct<b:array<'B,1>,a:array<'A,1>,d:array<struct<e:array<E,1>>,1>>}",
//			insiemeTy->toString());
//
//	insiemeTy = convFactory.convertType( clang.getASTContext().getTagDeclType(declB).getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("rec 'B.{'A=struct<b:array<'B,1>>, 'B=struct<c:array<'C,1>>, 'C=struct<b:array<'B,1>,a:array<'A,1>,d:array<struct<e:array<E,1>>,1>>}",
//			insiemeTy->toString());
//
//	insiemeTy = convFactory.convertType( clang.getASTContext().getTagDeclType(declC).getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("rec 'C.{'A=struct<b:array<'B,1>>, 'B=struct<c:array<'C,1>>, 'C=struct<b:array<'B,1>,a:array<'A,1>,d:array<struct<e:array<E,1>>,1>>}",
//			insiemeTy->toString());
//
//	insiemeTy = convFactory.convertType( clang.getASTContext().getTagDeclType(declD).getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("struct<e:array<E,1>>", insiemeTy->toString());
//
//	insiemeTy = convFactory.convertType( clang.getASTContext().getTagDeclType(declE).getTypePtr() );
//	EXPECT_TRUE(insiemeTy);
//	EXPECT_EQ("E", insiemeTy->toString());
//}


TEST(TypeConversion, HandleFunctionType) {
//	using namespace clang;
//
//	NodeManager manager;
//	fe::Program prog(manager);
//	fe::TranslationUnit& tu = prog.createEmptyTranslationUnit();
//	const fe::ClangCompiler& clang = tu.getCompiler();
//	ConversionFactory convFactory( manager, prog );
//
//	ASTContext& ctx = clang.getASTContext();
//	// Defines a function with the following prototype:
//	// int f(double a, float* b)
//	// cppcheck-suppress exceptNew
//	BuiltinType* intTy = new BuiltinType(BuiltinType::Int);
//	// cppcheck-suppress exceptNew
//	BuiltinType* doubleTy = new BuiltinType(BuiltinType::Double);
//	// cppcheck-suppress exceptNew
//	BuiltinType* floatTy = new BuiltinType(BuiltinType::Float);
//	{
//		QualType argTy[] = { QualType(doubleTy, 0), ctx.getPointerType(QualType(floatTy, 0)) };
//		QualType funcTy = ctx.getFunctionType(QualType(intTy, 0), argTy, 2, false, 0, false, false, 0, NULL,
//				clang::FunctionType::ExtInfo(false, 0, CallingConv::CC_Default));
//
//		// convert into IR type
//		TypePtr insiemeTy = convFactory.convertType( funcTy.getTypePtr() );
//		EXPECT_TRUE(insiemeTy);
//		EXPECT_EQ("((real<8>,ref<array<real<4>,1>>)->int<4>)", insiemeTy->toString());
//	}
//	// check conversion of function with no prototype
//	// int f()
//	{
//		QualType funcTy = ctx.getFunctionNoProtoType(QualType(intTy, 0));
//
//		// convert into IR type
//		TypePtr insiemeTy = convFactory.convertType( funcTy.getTypePtr() );
//		EXPECT_TRUE(insiemeTy);
//		EXPECT_EQ("(()->int<4>)", insiemeTy->toString());
//	}
//
//	operator delete (intTy);
//	operator delete (doubleTy);
//	operator delete (floatTy);
}

TEST(TypeConversion, FileTest) {
	Logger::get(std::cerr, INFO, 2);

	NodeManager manager;
	fe::Program prog(manager, SRC_DIR "/inputs/types.c");

	auto filter = [](const fe::pragma::Pragma& curr){ return curr.getType() == "test"; };

	// we use an internal manager to have private counter for variables so we can write independent tests
	NodeManager mgr;

	fe::conversion::Converter convFactory( mgr, prog );
	convFactory.convert();

	auto resolve = [&](const core::NodePtr& cur) {
		return convFactory.getIRTranslationUnit().resolve(cur);
	};

	for(auto it = prog.pragmas_begin(filter), end = prog.pragmas_end(); it != end; ++it) {

		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*(*it));

		if(tp.isStatement()) {
            StatementPtr stmt = fe::fixVariableIDs(resolve(convFactory.convertStmt( tp.getStatement() ))).as<StatementPtr>();
			EXPECT_EQ(tp.getExpected(), '\"' + toString(printer::PrettyPrinter(stmt, printer::PrettyPrinter::PRINT_SINGLE_LINE)) + '\"' );
		} else {
			if(const clang::TypeDecl* td = llvm::dyn_cast<const clang::TypeDecl>( tp.getDecl() )) {
				EXPECT_EQ(tp.getExpected(), '\"' + resolve(convFactory.convertType( td->getTypeForDecl() ))->toString() + '\"' );
			} else if(const clang::VarDecl* vd = llvm::dyn_cast<const clang::VarDecl>( tp.getDecl() )) {
				EXPECT_EQ(tp.getExpected(), '\"' + resolve(convFactory.convertVarDecl( vd ))->toString() + '\"' );
			}
		}
	}
}

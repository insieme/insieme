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

using namespace insieme::core;

using namespace insieme::frontend;
using namespace insieme::frontend::conversion;

#define CHECK_BUILTIN_TYPE(TypeName, InsiemeTypeDesc) \
	{ TypePtr convType = convFactory.ConvertType( clang::BuiltinType(clang::BuiltinType::TypeName) ); \
	EXPECT_TRUE(convType); \
	EXPECT_EQ(InsiemeTypeDesc, convType->getName()); }

TEST(TypeConversion, HandleBuildinType) {

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

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

TEST(TypeConversion, HandlePointerType) {

	using namespace clang;

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	BuiltinType intTy(BuiltinType::Int);
	QualType pointerTy = clang.getASTContext().getPointerType(QualType(&intTy, 0));

	TypePtr insiemeTy = convFactory.ConvertType( *pointerTy.getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("ref<int<4>>", insiemeTy->toString());

}

TEST(TypeConversion, HandleReferenceType) {

	using namespace clang;

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	BuiltinType intTy(BuiltinType::Int);
	QualType refTy = clang.getASTContext().getLValueReferenceType(QualType(&intTy, 0));

	TypePtr insiemeTy = convFactory.ConvertType( *refTy.getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("ref<int<4>>", insiemeTy->toString());

}

TEST(TypeConversion, HandleStructType) {

	using namespace clang;

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	SourceLocation emptyLoc;

	BuiltinType charTy(BuiltinType::SChar);
	BuiltinType ushortTy(BuiltinType::UShort);

	// create a struct:
	// struct Person {
	//	char* name;
	//	unsigned short age;
	// };
	RecordDecl* decl = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			emptyLoc, clang.getPreprocessor().getIdentifierInfo("Person"));

	// creates 'char* name' field
	decl->addDecl(FieldDecl::Create(clang.getASTContext(), decl, emptyLoc,
			clang.getPreprocessor().getIdentifierInfo("name"), clang.getASTContext().getPointerType(QualType(&charTy, 0)), 0, 0, false));

	// creates 'unsigned short age' field
	decl->addDecl(FieldDecl::Create(clang.getASTContext(), decl, emptyLoc,
			clang.getPreprocessor().getIdentifierInfo("age"), QualType(&ushortTy,0), 0, 0, false));

	decl->completeDefinition ();

	// Gets the type for the record declaration
	QualType type = clang.getASTContext().getTagDeclType(decl);

	// convert the type into an IR type
	TypePtr insiemeTy = convFactory.ConvertType( *type.getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("struct<name:ref<char>,age:uint<2>>", insiemeTy->toString());

}

TEST(TypeConversion, HandleRecursiveStructType) {

	ProgramPtr prog = Program::create();
	insieme::frontend::conversion::ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	clang::BuiltinType charTy(clang::BuiltinType::SChar);
	clang::BuiltinType longTy(clang::BuiltinType::Long);

	clang::RecordDecl* decl = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("Person"));

	clang::QualType declType = clang.getASTContext().getTagDeclType (decl);

	decl->addDecl(clang::FieldDecl::Create(clang.getASTContext(), decl, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("name"), clang.getASTContext().getPointerType(clang::QualType(&charTy, 0)), 0, 0, false));

	decl->addDecl(clang::FieldDecl::Create(clang.getASTContext(), decl, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("age"), clang::QualType(&longTy,0), 0, 0, false));

	decl->addDecl(clang::FieldDecl::Create(clang.getASTContext(), decl, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("mate"), clang.getASTContext().getPointerType(declType), 0, 0, false));
	decl->completeDefinition();

	TypePtr insiemeTy = convFactory.ConvertType( *declType.getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("rec 'Person.{'Person=struct<name:ref<char>,age:int<8>,mate:ref<'Person>>}", insiemeTy->toString());
}

TEST(TypeConversion, HandleMutualRecursiveStructType) {

	ProgramPtr prog = Program::create();
	insieme::frontend::conversion::ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	clang::RecordDecl* declA = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("A"));
	clang::RecordDecl* declB = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("B"));
	clang::RecordDecl* declC = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("C"));
	clang::RecordDecl* declD = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("D"));
	clang::RecordDecl* declE = clang::RecordDecl::Create(clang.getASTContext(), clang::TTK_Struct, NULL,
			clang::SourceLocation(), clang.getPreprocessor().getIdentifierInfo("E"));

	declA->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declA, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("b"),
			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declB)), 0, 0, false));

	declA->completeDefinition();

	declB->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declB, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("c"),
			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declC)), 0, 0, false));

	declB->completeDefinition();

	declC->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declC, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("b"),
			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declB)), 0, 0, false));

	declC->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declC, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("a"),
			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declA)), 0, 0, false));

	declC->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declC, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("d"),
			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declD)), 0, 0, false));

	declC->completeDefinition();

	declD->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declD, clang::SourceLocation(),
			clang.getPreprocessor().getIdentifierInfo("e"),
			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declE)), 0, 0, false));
	declD->completeDefinition();

//	declE->addDecl(clang::FieldDecl::Create(clang.getASTContext(), declE, clang::SourceLocation(),
//			clang.getPreprocessor().getIdentifierInfo("b"),
//			clang.getASTContext().getPointerType(clang.getASTContext().getTagDeclType(declB)), 0, 0, false));
//	declE->completeDefinition();

	TypePtr insiemeTy = convFactory.ConvertType( *clang.getASTContext().getTagDeclType(declA).getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("rec 'A.{'A=struct<b:ref<'B>>, 'B=struct<c:ref<'C>>, 'C=struct<b:ref<'B>,a:ref<'A>,d:ref<struct<e:ref<E>>>>}", insiemeTy->toString());

	insiemeTy = convFactory.ConvertType( *clang.getASTContext().getTagDeclType(declB).getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("rec 'B.{'A=struct<b:ref<'B>>, 'B=struct<c:ref<'C>>, 'C=struct<b:ref<'B>,a:ref<'A>,d:ref<struct<e:ref<E>>>>}", insiemeTy->toString());

	insiemeTy = convFactory.ConvertType( *clang.getASTContext().getTagDeclType(declC).getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("rec 'C.{'A=struct<b:ref<'B>>, 'B=struct<c:ref<'C>>, 'C=struct<b:ref<'B>,a:ref<'A>,d:ref<struct<e:ref<E>>>>}", insiemeTy->toString());

	insiemeTy = convFactory.ConvertType( *clang.getASTContext().getTagDeclType(declD).getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("struct<e:ref<E>>", insiemeTy->toString());

	insiemeTy = convFactory.ConvertType( *clang.getASTContext().getTagDeclType(declE).getTypePtr() );
	EXPECT_TRUE(insiemeTy);
	EXPECT_EQ("E", insiemeTy->toString());
}


TEST(TypeConversion, HandleFunctionType) {
	using namespace clang;

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	ASTContext& ctx = clang.getASTContext();

	// Defines a function with the following prototype:
	// int f(double a, float* b)

	BuiltinType intTy(BuiltinType::Int);
	BuiltinType doubleTy(BuiltinType::Double);
	BuiltinType floatTy(BuiltinType::Float);
	{
		QualType argTy[] = { QualType(&doubleTy, 0), ctx.getPointerType(QualType(&floatTy, 0)) };
		//QualType funcTy = ctx.getFunctionType(QualType(&intTy, 0), argTy, 2, false, 0, false, false, 0, NULL, CallingConv::CC_Default);

		// convert into IR type
//		TypePtr insiemeTy = convFactory.ConvertType( *funcTy.getTypePtr() );
//		EXPECT_TRUE(insiemeTy);
//		EXPECT_EQ("((real<8>,ref<real<4>>)->int<4>)", insiemeTy->toString());
	}
	// check conversion of function with no prototype
	// int f()
	{
		QualType funcTy = ctx.getFunctionNoProtoType(QualType(&intTy, 0));

		// convert into IR type
		TypePtr insiemeTy = convFactory.ConvertType( *funcTy.getTypePtr() );
		EXPECT_TRUE(insiemeTy);
		EXPECT_EQ("(()->int<4>)", insiemeTy->toString());
	}
}

TEST(TypeConversion, HandleArrayType) {
	using namespace clang;

	ProgramPtr prog = Program::create();
	ConversionFactory convFactory( prog->getNodeManager() );

	ClangCompiler clang;
	ASTContext& ctx = clang.getASTContext();

	// Check constant arrays: i.e. int a[4];
	BuiltinType intTy(BuiltinType::Int);
	{
		QualType arrayTy = ctx.getConstantArrayType(QualType(&intTy, 0), llvm::APInt(16,8,false), clang::ArrayType::Normal, 0);
		TypePtr insiemeTy = convFactory.ConvertType( *arrayTy.getTypePtr() );
		EXPECT_TRUE(insiemeTy);
		EXPECT_EQ("vector<ref<int<4>>,8>", insiemeTy->toString());
	}

	// check incomplete array types: char* arr[]
	BuiltinType charTy(BuiltinType::SChar);
	{
		QualType arrayTy = ctx.getIncompleteArrayType(ctx.getPointerType(QualType(&charTy, 0)), clang::ArrayType::Normal, 0);
		TypePtr insiemeTy = convFactory.ConvertType( *arrayTy.getTypePtr() );
		EXPECT_TRUE(insiemeTy);
		EXPECT_EQ("ref<array<ref<ref<char>>,1>>", insiemeTy->toString());
	}
	// ... check variable array and dependent array sizes

}



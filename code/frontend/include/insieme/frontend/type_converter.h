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

#pragma once

#include "insieme/frontend/convert.h"

#include <clang/AST/TypeVisitor.h>

namespace insieme {
namespace frontend {

namespace conversion {

#define DECLARE_TYPE_VISIT(Base, TypeTy) \
	virtual core::TypePtr Visit##TypeTy(const clang::TypeTy* type );

#define CALL_BASE_TYPE_VISIT(Base, TypeTy) \
	core::TypePtr Visit##TypeTy(const clang::TypeTy* type ) { return Base::Visit##TypeTy( type ); }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Type converter: Common Interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class Converter::TypeConverter {


protected:
	Converter& 							convFact;
	core::NodeManager& 					mgr;
	const core::IRBuilder& 				builder;
	const core::lang::BasicGenerator& 	gen;

public:
	TypeConverter(Converter& fact);

	virtual ~TypeConverter() { }

	DECLARE_TYPE_VISIT(TypeConverter, BuiltinType)
	DECLARE_TYPE_VISIT(TypeConverter, ComplexType)
	DECLARE_TYPE_VISIT(TypeConverter, ConstantArrayType)
	DECLARE_TYPE_VISIT(TypeConverter, IncompleteArrayType)
	DECLARE_TYPE_VISIT(TypeConverter, VariableArrayType)
	DECLARE_TYPE_VISIT(TypeConverter, FunctionProtoType)
	DECLARE_TYPE_VISIT(TypeConverter, FunctionNoProtoType)
	DECLARE_TYPE_VISIT(TypeConverter, VectorType)
	DECLARE_TYPE_VISIT(TypeConverter, ExtVectorType)
	DECLARE_TYPE_VISIT(TypeConverter, TypedefType)
	DECLARE_TYPE_VISIT(TypeConverter, TypeOfType)
	DECLARE_TYPE_VISIT(TypeConverter, TypeOfExprType)
	DECLARE_TYPE_VISIT(TypeConverter, TagType)
	DECLARE_TYPE_VISIT(TypeConverter, ElaboratedType)
	DECLARE_TYPE_VISIT(TypeConverter, ParenType)
	DECLARE_TYPE_VISIT(TypeConverter, PointerType)

	// main entry point
	core::TypePtr convert(const clang::Type* type);

protected:

	virtual core::TypePtr convertInternal(const clang::Type* type) = 0;

	virtual void postConvertionAction(const clang::Type* src, const core::TypePtr& res) { };

	core::TypePtr handleTagType(const clang::TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements);
};




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Type converter: C types
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class Converter::CTypeConverter:
	public Converter::TypeConverter,
	public clang::TypeVisitor<Converter::CTypeConverter, core::TypePtr>
{

public:
	CTypeConverter(Converter& fact)
		: TypeConverter(fact) { }

	virtual ~CTypeConverter() {}

	CALL_BASE_TYPE_VISIT(TypeConverter, BuiltinType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ComplexType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ConstantArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, IncompleteArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, VariableArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionNoProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, VectorType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ExtVectorType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypedefType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfExprType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TagType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ElaboratedType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ParenType)
	CALL_BASE_TYPE_VISIT(TypeConverter, PointerType)

protected:

	// main entry point
	virtual core::TypePtr convertInternal(const clang::Type* type);
};




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Type converter: C++ types
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class Converter::CXXTypeConverter :
	public Converter::TypeConverter,
	public clang::TypeVisitor<Converter::CXXTypeConverter, core::TypePtr>{

public:
	CXXTypeConverter(Converter& fact)
		: TypeConverter(fact) {}

	virtual ~CXXTypeConverter() {};

	vector<clang::RecordDecl*> getAllBases(const clang::CXXRecordDecl* recDeclCXX );

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  COMMON TYPES
	CALL_BASE_TYPE_VISIT(TypeConverter, BuiltinType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ComplexType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ConstantArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, IncompleteArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, VariableArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionNoProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, VectorType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ExtVectorType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypedefType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfExprType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ElaboratedType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ParenType)

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  C++ specific types
	core::TypePtr VisitPointerType(const clang::PointerType* ptrTy);
	core::TypePtr VisitTagType(const clang::TagType* tagType);
	core::TypePtr VisitDependentSizedArrayType(const clang::DependentSizedArrayType* arrTy);
	core::TypePtr VisitReferenceType(const clang::ReferenceType* refTy);
	core::TypePtr VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy) ;
	core::TypePtr VisitDependentTemplateSpecializationType(const clang::DependentTemplateSpecializationType* tempTy);
	core::TypePtr VisitInjectedClassNameType(const clang::InjectedClassNameType* tempTy);
	core::TypePtr VisitSubstTemplateTypeParmType(const clang::SubstTemplateTypeParmType* substTy);
	core::TypePtr VisitTemplateTypeParmType(const clang::TemplateTypeParmType* templParamTy);
//	core::TypePtr VisitDecltypeType(const clang::DecltypeType* declTy);
    //core::TypePtr VisitAutoType(const clang::AutoType* autoTy);
    core::TypePtr VisitMemberPointerType(const clang::MemberPointerType* memPointerTy);

protected:

	// main entry point
	virtual core::TypePtr convertInternal(const clang::Type* type);

	virtual void postConvertionAction(const clang::Type* src, const core::TypePtr& res);
};

}
}
}

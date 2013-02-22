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

#include "clang/AST/TypeVisitor.h"

#include "insieme/frontend/utils/dep_graph.h"

namespace insieme {
namespace frontend {

namespace conversion {

#define DECLARE_TYPE_VISIT(Base, TypeTy) \
	virtual core::TypePtr Visit##TypeTy(const clang::TypeTy* type );

#define CALL_BASE_TYPE_VISIT(Base, TypeTy) \
	core::TypePtr Visit##TypeTy(const clang::TypeTy* type ) { return Base::Visit##TypeTy( type ); }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 											Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define MAKE_SIZE(n)	toVector(core::IntTypeParam::getConcreteIntParam(n))
#define EMPTY_TYPE_LIST	vector<core::TypePtr>()

#define START_LOG_TYPE_CONVERSION(type) \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting type [class: '" << (type)->getTypeClassName() << "']"; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang type: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		type->dump(); \
	}

#define END_LOG_TYPE_CONVERSION(type) \
	VLOG(1) << "Converted 'type' into IR type: "; \
	VLOG(1) << "\t" << *type;





//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Type converter: Common Interface
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ConversionFactory::TypeConverter {

protected:
	ConversionFactory& 					convFact;
	core::NodeManager& 					mgr;
	const core::IRBuilder& 				builder;
	const core::lang::BasicGenerator& 	gen;

	utils::DependencyGraph<const clang::TagDecl*> typeGraph;

public:
	TypeConverter(ConversionFactory& fact, Program& program);

	virtual ~TypeConverter() { }

	DECLARE_TYPE_VISIT(TypeConverter, BuiltinType)
	DECLARE_TYPE_VISIT(TypeConverter, ComplexType)
	DECLARE_TYPE_VISIT(TypeConverter, ConstantArrayType)
	DECLARE_TYPE_VISIT(TypeConverter, IncompleteArrayType)
	DECLARE_TYPE_VISIT(TypeConverter, VariableArrayType)
	DECLARE_TYPE_VISIT(TypeConverter, FunctionProtoType)
	DECLARE_TYPE_VISIT(TypeConverter, FunctionNoProtoType)
	DECLARE_TYPE_VISIT(TypeConverter, ExtVectorType)
	DECLARE_TYPE_VISIT(TypeConverter, TypedefType)
	DECLARE_TYPE_VISIT(TypeConverter, TypeOfType)
	DECLARE_TYPE_VISIT(TypeConverter, TypeOfExprType)
	DECLARE_TYPE_VISIT(TypeConverter, TagType)
	DECLARE_TYPE_VISIT(TypeConverter, ElaboratedType)
	DECLARE_TYPE_VISIT(TypeConverter, ParenType)
	DECLARE_TYPE_VISIT(TypeConverter, PointerType)

	virtual core::TypePtr Visit(const clang::Type* type) = 0;
protected:

	virtual core::TypePtr handleTagType(const clang::TagDecl* tagDecl, 
			const core::NamedCompositeType::Entries& structElements);
};




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Type converter: C types
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ConversionFactory::CTypeConverter: 
	public ConversionFactory::TypeConverter, 
	public clang::TypeVisitor<ConversionFactory::CTypeConverter, core::TypePtr>
{

protected:
	//ConversionFactory& convFact;
	//utils::DependencyGraph<const clang::Type*> typeGraph;

public:
	CTypeConverter(ConversionFactory& fact, Program& program) :
		TypeConverter(fact, program) {
	}
	virtual ~CTypeConverter() {}

	CALL_BASE_TYPE_VISIT(TypeConverter, BuiltinType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ComplexType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ConstantArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, IncompleteArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, VariableArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionNoProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ExtVectorType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypedefType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfExprType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TagType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ElaboratedType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ParenType)
	CALL_BASE_TYPE_VISIT(TypeConverter, PointerType)

	core::TypePtr Visit(const clang::Type* type);

protected:

	virtual core::TypePtr handleTagType(const clang::TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements);

};




//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Type converter: C++ types
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ConversionFactory::CXXTypeConverter :
	public ConversionFactory::TypeConverter, 
	public clang::TypeVisitor<ConversionFactory::CXXTypeConverter, core::TypePtr>{

	ConversionFactory& convFact;

	/** 
	 * caches classes definitions
	 * this boost conversion but also avoids infinite recursion while resolving class member
	 * function
	 */
	std::map<const clang::TagType*, core::TypePtr> mClassTypeMap;

protected:
	core::TypePtr handleTagType(const clang::TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements);

public:
	CXXTypeConverter(ConversionFactory& fact, Program& program) :
		TypeConverter(fact, program),
		convFact(fact)
	{}

	virtual ~CXXTypeConverter() {};

	vector<clang::RecordDecl*> getAllBases(const clang::CXXRecordDecl* recDeclCXX );

	core::FunctionTypePtr addCXXThisToFunctionType(const core::IRBuilder& builder,
												   const core::TypePtr& globals,
												   const core::FunctionTypePtr& funcType);

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  COMMON TYPES
	CALL_BASE_TYPE_VISIT(TypeConverter, ComplexType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ConstantArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, IncompleteArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, VariableArrayType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, FunctionNoProtoType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ExtVectorType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypedefType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfType)
	CALL_BASE_TYPE_VISIT(TypeConverter, TypeOfExprType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ElaboratedType)
	CALL_BASE_TYPE_VISIT(TypeConverter, ParenType)
	CALL_BASE_TYPE_VISIT(TypeConverter, PointerType)

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//  C++ specific types
	core::TypePtr VisitBuiltinType(const clang::BuiltinType* buldInTy);
	core::TypePtr VisitTagType(const clang::TagType* tagType);
	core::TypePtr VisitDependentSizedArrayType(const clang::DependentSizedArrayType* arrTy);
	core::TypePtr VisitReferenceType(const clang::ReferenceType* refTy);
	core::TypePtr VisitTemplateSpecializationType(const clang::TemplateSpecializationType* templTy) ;
	core::TypePtr VisitDependentTemplateSpecializationType(const clang::DependentTemplateSpecializationType* tempTy);
	core::TypePtr VisitInjectedClassNameType(const clang::InjectedClassNameType* tempTy);
	core::TypePtr VisitSubstTemplateTypeParmType(const clang::SubstTemplateTypeParmType* substTy);
	core::TypePtr Visit(const clang::Type* type);
};

}
}
}

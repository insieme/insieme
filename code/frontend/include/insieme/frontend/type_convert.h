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

#include "insieme/frontend/convert.h"

#include "clang/AST/TypeVisitor.h"

#include "insieme/frontend/utils/dep_graph.h"

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {

namespace conversion {

#define FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(TypeTy) \
	core::TypePtr Visit##TypeTy( TypeTy* type ) { return convFact.convertCXXType(type); }

class ConversionFactory::ClangTypeConverter: public TypeVisitor<ConversionFactory::ClangTypeConverter, core::TypePtr>{

protected:
	ConversionFactory& convFact;
	utils::DependencyGraph<const clang::Type*> typeGraph;

public:
	ClangTypeConverter(ConversionFactory& fact, Program& program);
	virtual ~ClangTypeConverter();
	virtual core::TypePtr VisitBuiltinType(const BuiltinType* buldInTy);
	core::TypePtr VisitComplexType(const ComplexType* bulinTy);
	core::TypePtr VisitConstantArrayType(const ConstantArrayType* arrTy);
	core::TypePtr VisitIncompleteArrayType(const IncompleteArrayType* arrTy);
	core::TypePtr VisitVariableArrayType(const VariableArrayType* arrTy);
	core::TypePtr VisitFunctionProtoType(const FunctionProtoType* funcTy);
	core::TypePtr VisitFunctionNoProtoType(const FunctionNoProtoType* funcTy);
	core::TypePtr VisitExtVectorType(const ExtVectorType* vecTy);
	core::TypePtr VisitTypedefType(const TypedefType* typedefType);
	core::TypePtr VisitTypeOfType(const TypeOfType* typeOfType);
	core::TypePtr VisitTypeOfExprType(const TypeOfExprType* typeOfType);
	virtual core::TypePtr VisitTagType(const TagType* tagType);
	core::TypePtr VisitElaboratedType(const ElaboratedType* elabType);
	core::TypePtr VisitParenType(const ParenType* parenTy);
	core::TypePtr VisitPointerType(const PointerType* pointerTy);
protected:

	virtual core::TypePtr handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements);

};

class CXXConversionFactory::CXXExtTypeConverter: public ConversionFactory::ClangTypeConverter {

	CXXConversionFactory& convFact;

public:
	CXXExtTypeConverter(CXXConversionFactory& fact, Program& program);

	virtual ~CXXExtTypeConverter();

	core::TypePtr VisitBuiltinType(const BuiltinType* buldInTy);

	core::TypePtr VisitTagType(const TagType* tagType);

	vector<RecordDecl*> getAllBases(const clang::CXXRecordDecl* recDeclCXX );

	core::FunctionTypePtr addCXXThisToFunctionType(const core::IRBuilder& builder,
								 	 	 	 	 	   const core::TypePtr& globals,
								 	 	 	 	 	   const core::FunctionTypePtr& funcType);


	FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(DependentSizedArrayType)
	FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(ReferenceType)
	FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(TemplateSpecializationType)
	FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(DependentTemplateSpecializationType)
	FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(InjectedClassNameType)
	FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(SubstTemplateTypeParmType)

protected:
	core::TypePtr handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements);

};

class CXXConversionFactory::CXXTypeConverter : public TypeVisitor<CXXConversionFactory::CXXTypeConverter, core::TypePtr>{

	CXXConversionFactory& cxxConvFact;

public:
	CXXTypeConverter(CXXConversionFactory& fact, Program& program);

	virtual ~CXXTypeConverter();

	core::TypePtr VisitDependentSizedArrayType(const DependentSizedArrayType* arrTy);

	core::TypePtr VisitReferenceType(const ReferenceType* refTy);

	core::TypePtr VisitTemplateSpecializationType(const TemplateSpecializationType* templTy) ;

	core::TypePtr VisitDependentTemplateSpecializationType(const DependentTemplateSpecializationType* tempTy);

	core::TypePtr VisitInjectedClassNameType(const InjectedClassNameType* tempTy);

	core::TypePtr VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType* substTy);

	core::TypePtr Visit(clang::Type* type);

};

}
}
}

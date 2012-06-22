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
#include "insieme/frontend/type_convert.h"

#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"

#include "insieme/annotations/c/naming.h"

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/DeclTemplate.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::Type* type) {
	if(const clang::TagType* tagType = dyn_cast<const clang::TagType>(type))
		return out << tagType->getDecl()->getNameAsString();
	return out;
}

} // end std namespace

namespace insieme {
namespace frontend {

namespace utils {

template <>
void DependencyGraph<const clang::Type*>::Handle(const clang::Type* type,
												 const DependencyGraph<const clang::Type*>::VertexTy& v) {
	using namespace clang;

	assert(llvm::isa<const TagType>(type));
	const TagType* tagType = llvm::dyn_cast<const TagType>(type);
	assert(tagType && "Type not of TagType class");

	RecordDecl* tag = llvm::dyn_cast<RecordDecl>(tagType->getDecl());
	// if the tag type is not a struct but otherwise an enum, there will be no declaration 
	// therefore we can safely return as there is no risk of recursion 
	if (!tag) { return; }

	for(RecordDecl::field_iterator it=tag->field_begin(), end=tag->field_end(); it != end; ++it) {
		const Type* fieldType = (*it)->getType().getTypePtr();
		if( const PointerType *ptrTy = dyn_cast<PointerType>(fieldType) )
			fieldType = ptrTy->getPointeeType().getTypePtr();
		else if( const ReferenceType *refTy = dyn_cast<ReferenceType>(fieldType) )
			fieldType = refTy->getPointeeType().getTypePtr();

		// Elaborated types shoud be recursively visited 
		if( const ElaboratedType* elabTy = llvm::dyn_cast<ElaboratedType>(fieldType) ) {
			addNode(elabTy->getNamedType().getTypePtr(), &v);
		}

		if( const TagType* tagTy = llvm::dyn_cast<TagType>(fieldType) ) {
			if ( llvm::isa<RecordDecl>(tagTy->getDecl()) ) {
				addNode( tagTy, &v );
			}
		}
	}
}

} // end utils namespace

namespace conversion {

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

#define FORWARD_TYPE_TO_CXX_TYPE_VISITOR_CALL(TypeTy) \
	core::TypePtr Visit##TypeTy( TypeTy* type ) { return convFact.convertCXXType(type); }


//---------------------------------------------------------------------------------------------------------------------
//											CLANG TYPE CONVERTER
//---------------------------------------------------------------------------------------------------------------------

ConversionFactory::TypeConverter::TypeConverter(ConversionFactory& fact, Program& program): convFact( fact ) { }
ConversionFactory::TypeConverter::~TypeConverter() {};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								BUILTIN TYPES
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitBuiltinType(const BuiltinType* buldInTy) {
	START_LOG_TYPE_CONVERSION( buldInTy );
	const core::lang::BasicGenerator& gen = convFact.mgr.getLangBasic();

	switch(buldInTy->getKind()) {
	case BuiltinType::Void:			return gen.getUnit();
	case BuiltinType::Bool:			return gen.getBool();

	// char types
	case BuiltinType::Char_U:
	case BuiltinType::UChar:		return gen.getUInt1();
	case BuiltinType::Char16:		return gen.getInt2();
	case BuiltinType::Char32:		return gen.getInt4();
	case BuiltinType::Char_S:
	case BuiltinType::SChar:		return gen.getChar();
	// case BuiltinType::WChar:		return gen.getWChar();

	// integer types
	case BuiltinType::UShort:		return gen.getUInt2();
	case BuiltinType::Short:		return gen.getInt2();
	case BuiltinType::UInt:			return gen.getUInt4();
	case BuiltinType::Int:			return gen.getInt4();
	case BuiltinType::UInt128:		return gen.getUInt16();
	case BuiltinType::Int128:		return gen.getInt16();
	case BuiltinType::ULong:		return gen.getUInt8();
	case BuiltinType::ULongLong:	return gen.getUInt8();
	case BuiltinType::Long:			return gen.getInt8();
	case BuiltinType::LongLong:		return gen.getInt8();

	// real types
	case BuiltinType::Float:		return gen.getFloat();
	case BuiltinType::Double:		return gen.getDouble();
	case BuiltinType::LongDouble:	return gen.getDouble(); // unsopported FIXME

	// not supported types
	case BuiltinType::NullPtr:
	case BuiltinType::Overload:
	case BuiltinType::Dependent:
	default:
		throw "type not supported"; //todo introduce exception class
	}
	assert(false && "Built-in type conversion not supported!");
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								COMPLEX TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitComplexType(const ComplexType* bulinTy) {
	assert(false && "ComplexType not yet handled!");
}

// ------------------------   ARRAYS  -------------------------------------
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 					CONSTANT ARRAY TYPE
//
// This method handles the canonical version of C arrays with a specified
// constant size. For example, the canonical type for 'int A[4 + 4*100]' is
// a ConstantArrayType where the element type is 'int' and the size is 404
//
// The IR representation for such array will be: vector<int<4>,404>
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitConstantArrayType(const ConstantArrayType* arrTy) {
	START_LOG_TYPE_CONVERSION( arrTy );
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return convFact.convertType( arrTy->desugar().getTypePtr() );

	size_t arrSize = *arrTy->getSize().getRawData();
	core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
	assert(elemTy && "Conversion of array element type failed.");

	// we need to check if the element type for this not a vector (or array) type
	// if(!((core::dynamic_pointer_cast<const core::VectorType>(elemTy) ||
	//		core::dynamic_pointer_cast<const core::ArrayType>(elemTy)) &&
	//		!arrTy->getElementType().getTypePtr()->isExtVectorType())) {
	//	elemTy = convFact.builder.refType(elemTy);
	// }

	core::TypePtr&& retTy = convFact.builder.vectorType(
			elemTy, core::ConcreteIntTypeParam::get(convFact.mgr, arrSize)
		);
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						INCOMPLETE ARRAY TYPE
// This method handles C arrays with an unspecified size. For example
// 'int A[]' has an IncompleteArrayType where the element type is 'int'
// and the size is unspecified.
//
// The representation for such array will be: ref<array<int<4>,1>>
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitIncompleteArrayType(const IncompleteArrayType* arrTy) {
	START_LOG_TYPE_CONVERSION( arrTy );
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return Visit( arrTy->desugar().getTypePtr() );

	const core::IRBuilder& builder = convFact.builder;
	core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
	assert(elemTy && "Conversion of array element type failed.");

	// we need to check if the element type for this not a vector (or array) type
	// if(!(core::dynamic_pointer_cast<const core::VectorType>(elemTy) ||
	//		core::dynamic_pointer_cast<const core::ArrayType>(elemTy))) {
	//	elemTy = convFact.builder.refType(elemTy);
	//}

	core::TypePtr&& retTy = builder.arrayType( elemTy );
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VARIABLE ARRAY TYPE
// This class represents C arrays with a specified size which is not an
// integer-constant-expression. For example, 'int s[x+foo()]'. Since the
// size expression is an arbitrary expression, we store it as such.
// Note: VariableArrayType's aren't uniqued (since the expressions aren't)
// and should not be: two lexically equivalent variable array types could
// mean different things, for example, these variables do not have the same
// type dynamically:
//				void foo(int x) { int Y[x]; ++x; int Z[x]; }
//
// he representation for such array will be: array<int<4>,1>( expr() )
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitVariableArrayType(const VariableArrayType* arrTy) {
	START_LOG_TYPE_CONVERSION( arrTy );
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return Visit( arrTy->desugar().getTypePtr() );

	const core::IRBuilder& builder = convFact.builder;
	core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
	assert(elemTy && "Conversion of array element type failed.");

	// we need to check if the element type for this not a vector (or array) type
	// if(!(core::dynamic_pointer_cast<const core::VectorType>(elemTy) || core::dynamic_pointer_cast<const core::ArrayType>(elemTy))) {
	//	 elemTy = convFact.builder.refType(elemTy);
	// }

	core::TypePtr retTy = builder.arrayType( elemTy );
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

// --------------------  FUNCTIONS  ---------------------------------------
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//				FUNCTION PROTO TYPE
// Represents a prototype with argument type info, e.g. 'int foo(int)' or
// 'int foo(void)'. 'void' is represented as having no arguments, not as
// having a single void argument. Such a type can have an exception
// specification, but this specification is not part of the canonical type.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitFunctionProtoType(const FunctionProtoType* funcTy) {
	START_LOG_TYPE_CONVERSION(funcTy);

	const core::IRBuilder& builder = convFact.builder;
	core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );

	assert(retTy && "Function has no return type!");

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is maintained
	if((retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)) {
		// only exception are OpenCL vectors
		if(!dyn_cast<const ExtVectorType>(funcTy->getResultType()->getUnqualifiedDesugaredType()))
			retTy = convFact.builder.refType(retTy);
	}

	assert(retTy && "Function has no return type!");

	core::TypeList argTypes;
	std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
		[ &argTypes, this ] (const QualType& currArgType) {
			this->convFact.ctx.isResolvingFunctionType = true;
			core::TypePtr&& argTy = this->Visit( currArgType.getTypePtr() );

			// If the argument is of type vector or array we need to add a reference
			if(argTy->getNodeType() == core::NT_VectorType || argTy->getNodeType() == core::NT_ArrayType) {
				// only exception are OpenCL vectors
				if(!dyn_cast<const ExtVectorType>(currArgType->getUnqualifiedDesugaredType()))
					argTy = this->convFact.builder.refType(argTy);
			}

			argTypes.push_back( argTy );
			this->convFact.ctx.isResolvingFunctionType = false;
		}
	);

	if( argTypes.size() == 1 && convFact.mgr.getLangBasic().isUnit(argTypes.front())) {
		// we have only 1 argument, and it is a unit type (void), remove it from the list
		argTypes.clear();
	}

	if( funcTy->isVariadic() )
		argTypes.push_back( convFact.mgr.getLangBasic().getVarList() );

	retTy = builder.functionType( argTypes, retTy);
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					FUNCTION NO PROTO TYPE
// Represents a K&R-style 'int foo()' function, which has no information
// available about its arguments.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitFunctionNoProtoType(const FunctionNoProtoType* funcTy) {
	START_LOG_TYPE_CONVERSION( funcTy );
	core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is mantained
	if(retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)
		retTy = convFact.builder.refType(retTy);

	assert(retTy && "Function has no return type!");

	retTy = convFact.builder.functionType( core::TypeList(), retTy);
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

// TBD
//	TypeWrapper VisitVectorType(VectorType* vecTy) {	}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							EXTENDEND VECTOR TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitExtVectorType(const ExtVectorType* vecTy) {
   // get vector datatype
	const QualType qt = vecTy->getElementType();
	const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
	core::TypePtr&& subType = Visit(const_cast<BuiltinType*>(buildInTy));

	// get the number of elements
	size_t num = vecTy->getNumElements();
	core::IntTypeParamPtr numElem = core::ConcreteIntTypeParam::get(convFact.mgr, num);

	//note: members of OpenCL vectors are never refs
	return convFact.builder.vectorType( subType, numElem);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 								TYPEDEF TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitTypedefType(const TypedefType* typedefType) {
	START_LOG_TYPE_CONVERSION( typedefType );

	core::TypePtr&& subType = Visit( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
	// Adding the name of the typedef as annotation
	subType->addAnnotation(std::make_shared<annotations::c::CNameAnnotation>(typedefType->getDecl()->getNameAsString()));

	END_LOG_TYPE_CONVERSION( subType );
	return  subType;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 								TYPE OF TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitTypeOfType(const TypeOfType* typeOfType) {
	START_LOG_TYPE_CONVERSION(typeOfType);
	core::TypePtr retTy = convFact.mgr.getLangBasic().getUnit();
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							TYPE OF EXPRESSION TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitTypeOfExprType(const TypeOfExprType* typeOfType) {
	START_LOG_TYPE_CONVERSION( typeOfType );
	core::TypePtr&& retTy = Visit( GET_TYPE_PTR(typeOfType->getUnderlyingExpr()) );
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 core::TypePtr ConversionFactory::TypeConverter::VisitTagType(const TagType* tagType) {
	VLOG(2) << "VisitTagType " << tagType  <<  std::endl;
	START_LOG_TYPE_CONVERSION( tagType );

	if(!convFact.ctx.recVarMap.empty()) {
		// check if this type has a typevar already associated, in such case return it
		ConversionContext::TypeRecVarMap::const_iterator fit = convFact.ctx.recVarMap.find(tagType);
		if( fit != convFact.ctx.recVarMap.end() ) {
			// we are resolving a parent recursive type, so we shouldn't
			return fit->second;
		}
	}

	// check if the type is in the cache of already solved recursive types
	// this is done only if we are not resolving a recursive sub type
	if(!convFact.ctx.isRecSubType) {
		ConversionContext::RecTypeMap::const_iterator rit = convFact.ctx.recTypeCache.find(tagType);
		if(rit != convFact.ctx.recTypeCache.end())
			return rit->second;
	}

	// will store the converted type
	core::TypePtr retTy;
	VLOG(1) << "~ Converting TagType: " << tagType->getDecl()->getName().str();

	const TagDecl* tagDecl = tagType->getDecl()->getCanonicalDecl();
	ConversionContext::ClassDeclMap::const_iterator cit = convFact.ctx.classDeclMap.find(tagDecl);
	if(cit != convFact.ctx.classDeclMap.end()){
		return cit->second;
	}

	// iterate through all the re-declarations to see if one of them provides a definition
	TagDecl::redecl_iterator i,e = tagDecl->redecls_end();
	for(i = tagDecl->redecls_begin(); i != e && !i->isCompleteDefinition(); ++i) ;
	if(i != e) {
		tagDecl = i->getDefinition();
		// we found a definition for this declaration, use it
		assert(tagDecl->isCompleteDefinition() && "TagType is not a definition");

		if(tagDecl->getTagKind() == clang::TTK_Enum) {
			// Enums are converted into integers
			return convFact.builder.getLangBasic().getInt4();
		} else {
			// handle struct/union/class
			const RecordDecl* recDecl = dyn_cast<const RecordDecl>(tagDecl);
			assert(recDecl && "TagType decl is not of a RecordDecl type!");

			if(!convFact.ctx.isRecSubType) {
				// add this type to the type graph (if not present)
				typeGraph.addNode(tagDecl->getTypeForDecl());
			}

			// retrieve the strongly connected componenets for this type
			std::set<const Type*>&& components =
				typeGraph.getStronglyConnectedComponents(tagDecl->getTypeForDecl());

			if( !components.empty() ) {
				if(VLOG_IS_ON(2)) {
					// we are dealing with a recursive type
					VLOG(2) << "Analyzing RecordDecl: " << recDecl->getNameAsString() << std::endl
							<< "Number of components in the cycle: " << components.size();
					std::for_each(components.begin(), components.end(),
						[] (std::set<const Type*>::value_type c) {
							assert(isa<const TagType>(c));
							VLOG(2) << "\t" << dyn_cast<const TagType>(c)->getDecl()->getNameAsString();
						}
					);
					typeGraph.print(std::cerr);
				}

				// we create a TypeVar for each type in the mutual dependence
				convFact.ctx.recVarMap.insert(
						std::make_pair(tagType, convFact.builder.typeVariable(recDecl->getName()))
					);

				// when a subtype is resolved we aspect to already have these variables in the map
				if(!convFact.ctx.isRecSubType) {
					std::for_each(components.begin(), components.end(),
						[ this ] (std::set<const Type*>::value_type ty) {
							const TagType* tagTy = dyn_cast<const TagType>(ty);
							assert(tagTy && "Type is not of TagType type");

							this->convFact.ctx.recVarMap.insert(
									std::make_pair(ty, convFact.builder.typeVariable(tagTy->getDecl()->getName()))
								);
						}
					);
				}
			}

			// Visit the type of the fields recursively
			// Note: if a field is referring one of the type in the cyclic dependency, a reference
			//       to the TypeVar will be returned.
			core::NamedCompositeType::Entries structElements;

			// TODO
			// c++ constructors
			const CXXRecordDecl* recDeclCXX = dyn_cast<const CXXRecordDecl>(recDecl);
			VLOG(2)<<recDeclCXX;

			if(recDeclCXX){
				bool hasPolymorphicBaseClass = false;
				// add only direct baseclasses as member
				for(CXXRecordDecl::base_class_const_iterator bit=recDeclCXX->bases_begin(),
								bend=recDeclCXX->bases_end(); bit != bend; ++bit) {
					const CXXBaseSpecifier * base = bit;
					RecordDecl *baseRecord = base->getType()->getAs<RecordType>()->getDecl();

					// put for every direct base-class a member to the derived class
					core::TypePtr&& fieldType = Visit( const_cast<Type*>(baseRecord->getTypeForDecl()) );
					VLOG(2) << "BaseClass is: " << baseRecord->getNameAsString() << " type: " << fieldType;
					core::StringValuePtr id = convFact.builder.stringValue(baseRecord->getNameAsString());
					structElements.push_back(convFact.builder.namedType(id, fieldType ));

					hasPolymorphicBaseClass |= base->getType()->getAsCXXRecordDecl()->isPolymorphic();
				}

//					for(CXXRecordDecl::ctor_iterator xit=recDeclCXX->ctor_begin(),
//							xend=recDeclCXX->ctor_end(); xit != xend; ++xit) {
//						CXXConstructorDecl * ctorDecl = *xit;
//						VLOG(1) << "~ Converting constructor: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;
//
//						core::TypePtr convertedType = convFact.convertType( GET_TYPE_PTR(ctorDecl) );
//						assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
//						core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);
//
//						//TODO funcType = addGlobalsToFunctionType(convFact.builder, convFact.ctx.globalStruct.first, funcType);
//
//						convFact.convertFunctionDecl(ctorDecl);
//						//std::cerr<<"dumpconstr: "<< curr->getNameAsString() << " ";
//						//curr->dumpDeclContext(); // on cerr
//						//std::cerr<<"enddumpconstr\n";
//						//core::StatementPtr&& body = convFact.convertStmt(curr->getBody());
//						//core::IdentifierPtr id = convFact.builder.identifier(curr->getNameAsString());
//					}
//
//					for(CXXRecordDecl::method_iterator mit=recDeclCXX->method_begin(),
//							mend=recDeclCXX->method_end(); mit != mend; ++mit) {
//						CXXMethodDecl * curr = *mit;
//						//convFact.convertFunctionDecl(curr, false);
//
//						//std::cerr<<"dumpconstr: "<< curr->getNameAsString() << " ";
//						//curr->dumpDeclContext(); // on cerr
//						//std::cerr<<"enddumpconstr\n";
//						//core::StatementPtr&& body = convFact.convertStmt(curr->getBody());
//						//core::IdentifierPtr id = convFact.builder.identifier(curr->getNameAsString());
//					}

				// add __class member to support virtual functions at highest polymorphic baseclass
				if( recDeclCXX->isPolymorphic() && !hasPolymorphicBaseClass) {
					VLOG(2) << recDeclCXX->getName().data() << " polymorphic class";

					core::StringValuePtr id = convFact.builder.stringValue("__class");
					structElements.push_back(convFact.builder.namedType(id, convFact.builder.getLangBasic().getUInt4()));
				}

			}  // end if recDeclCXX

			unsigned mid = 0;
			for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
				RecordDecl::field_iterator::value_type curr = *it;
				core::TypePtr&& fieldType = Visit( const_cast<Type*>(GET_TYPE_PTR(curr)) );
				// if the type is not const we have to add a ref because the value could be accessed and changed
				//if(!(curr->getType().isConstQualified() || core::dynamic_pointer_cast<const core::VectorType>(fieldType)))
				//	fieldType = convFact.builder.refType(fieldType);

				core::StringValuePtr id = convFact.builder.stringValue(
						curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));

				structElements.push_back(convFact.builder.namedType(id, fieldType));
				mid++;
			}

			// build a struct or union IR type
			retTy = handleTagType(tagDecl, structElements);

			if( !components.empty() ) {
				// if we are visiting a nested recursive type it means someone else will take care
				// of building the rectype node, we just return an intermediate type
				if(convFact.ctx.isRecSubType)
					return retTy;

				// we have to create a recursive type
				ConversionContext::TypeRecVarMap::const_iterator tit = convFact.ctx.recVarMap.find(tagType);
				assert(tit != convFact.ctx.recVarMap.end() &&
						"Recursive type has not TypeVar associated to himself");
				core::TypeVariablePtr recTypeVar = tit->second;

				vector<core::RecTypeBindingPtr> definitions;
				definitions.push_back( convFact.builder.recTypeBinding(recTypeVar, handleTagType(tagDecl, structElements) ) );

				// We start building the recursive type. In order to avoid loop the visitor
				// we have to change its behaviour and let him returns temporarely types
				// when a sub recursive type is visited.
				convFact.ctx.isRecSubType = true;

				std::for_each(components.begin(), components.end(),
					[ this, &definitions, &convFact, &recTypeVar ] (std::set<const Type*>::value_type ty) {
						const TagType* tagTy = dyn_cast<const TagType>(ty);
						assert(tagTy && "Type is not of TagType type");

						//Visual Studio 2010 fix: full namespace
						insieme::frontend::conversion::ConversionFactory::ConversionContext::TypeRecVarMap::const_iterator tit =
								this->convFact.ctx.recVarMap.find(ty);

						assert(tit != this->convFact.ctx.recVarMap.end() && "Recursive type has no TypeVar associated");
						core::TypeVariablePtr var = tit->second;

						// test whether this variable has already been handled
						if (*var == *recTypeVar) { return; }

						// we remove the variable from the list in order to fool the solver,
						// in this way it will create a descriptor for this type (and he will not return the TypeVar
						// associated with this recursive type). This behaviour is enabled only when the isRecSubType
						// flag is true
						this->convFact.ctx.recVarMap.erase(ty);

						definitions.push_back( convFact.builder.recTypeBinding(var, this->Visit(const_cast<Type*>(ty))) );
						var->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(tagTy->getDecl()->getNameAsString()) );

						// reinsert the TypeVar in the map in order to solve the other recursive types
						this->convFact.ctx.recVarMap.insert( std::make_pair(tagTy, var) );
					}
				);

				// sort definitions - this will produce the same list of definitions for each of the related types => shared structure
				if (definitions.size() > 1) {
					std::sort(definitions.begin(), definitions.end(), [](const core::RecTypeBindingPtr& a, const core::RecTypeBindingPtr& b){
						return a->getVariable()->getVarName()->getValue() < b->getVariable()->getVarName()->getValue();
					});
				}

				// we reset the behavior of the solver
				convFact.ctx.isRecSubType = false;
				// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
				convFact.ctx.recVarMap.clear();

				core::RecTypeDefinitionPtr&& definition = convFact.builder.recTypeDefinition(definitions);
				retTy = convFact.builder.recType(recTypeVar, definition);

				// Once we solved this recursive type, we add to a cache of recursive types
				// so next time we encounter it, we don't need to compute the graph
				convFact.ctx.recTypeCache.insert(std::make_pair(tagType, retTy));
			}

			// Adding the name of the C struct as annotation
			if (!recDecl->getName().empty())
				retTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(recDecl->getName()) );
			convFact.ctx.classDeclMap.insert(std::make_pair(tagDecl, retTy));
		}
	} else {
		// We didn't find any definition for this type, so we use a name and define it as a generic type
		retTy = convFact.builder.genericType( tagDecl->getNameAsString() );
	}
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							ELABORATED TYPE (TODO)
//
// Represents a type that was referred to using an elaborated type keyword, e.g.,
// struct S, or via a qualified name, e.g., N::M::type, or both
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitElaboratedType(const ElaboratedType* elabType) {

	// elabType->dump();
	//elabType->desugar().getTypePtr()->dump();
	//std::cerr << elabType->getBaseElementTypeUnsafe() << std::endl <<"ElaboratedType not yet handled!!!!\n";

	VLOG(2) << "elabtype " << elabType << "\n";
	return Visit( elabType->getNamedType().getTypePtr() );
//		assert(false && "ElaboratedType not yet handled!");
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							   PAREN TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitParenType(const ParenType* parenTy) {
	START_LOG_TYPE_CONVERSION(parenTy);
	core::TypePtr&& retTy = Visit( parenTy->getInnerType().getTypePtr() );
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							POINTER TYPE (FIXME)
// Pointer types need to be converted into reference types within the IR.
// Essentially there are two options. If the pointer is pointing to a single
// element, e.g. int* pointing to one integer, the resulting type should be
//		pointer to scalar (rvalue):   int*  ---->   ref<int<4>>
//		pointer to scalar (lvalue):   int*  ---->   ref<ref<int<4>>>
//
// However, if the target is an array of values, the result should be
//		pointer to array (rvalue):   int*  ---->   ref<array<int<4>,1>>
//		pointer to array (lvalue):   int*  ---->   ref<ref<array<int<4>,1>>>
//
// Since the actual case can not be determined based on the type, the
// more general case (the array case) has to be conservatively considered.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitPointerType(const PointerType* pointerTy) {
	START_LOG_TYPE_CONVERSION(pointerTy);

	core::TypePtr&& subTy = Visit( pointerTy->getPointeeType().getTypePtr() );
	// ~~~~~ Handling of special cases ~~~~~~~
	// void* -> array<'a>
	if( convFact.mgr.getLangBasic().isUnit(subTy) ) {
		return convFact.mgr.getLangBasic().getAnyRef();
	}
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr&& retTy =
		(subTy->getNodeType() == core::NT_FunctionType)
		? subTy
		: convFact.builder.refType(convFact.builder.arrayType( subTy ));

	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}


core::TypePtr ConversionFactory::TypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return convFact.builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return convFact.builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
}


core::TypePtr ConversionFactory::CTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return convFact.builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return convFact.builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
}

ConversionFactory::CTypeConverter* ConversionFactory::makeTypeConvert(ConversionFactory& fact, Program& program) {
	return new ConversionFactory::CTypeConverter(fact, program);
}

void ConversionFactory::cleanTypeConvert(TypeConverter* typeConv) {
	delete typeConv;
}

core::TypePtr ConversionFactory::convertType(const clang::Type* type) {
	assert(type && "Calling convertType with a NULL pointer");
	auto fit = ctx.typeCache.find(type);
	if(fit == ctx.typeCache.end()) {
		core::TypePtr&& retTy = typeConv->Visit( const_cast<Type*>(type) );
		ctx.typeCache.insert( std::make_pair(type,retTy) );
		return retTy;
	}

	return fit->second;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								BUILTIN TYPES
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter::VisitBuiltinType(const BuiltinType* buldInTy) {
	START_LOG_TYPE_CONVERSION( buldInTy );
	const core::lang::BasicGenerator& gen = convFact.mgr.getLangBasic();

	switch(buldInTy->getKind()) {
	case BuiltinType::Void:			return gen.getUnit();
	case BuiltinType::Bool:			return gen.getBool();

	// char types
	case BuiltinType::Char_U:
	case BuiltinType::UChar:		return gen.getUInt1();
	case BuiltinType::Char16:		return gen.getInt2();
	case BuiltinType::Char32:		return gen.getInt4();
	case BuiltinType::Char_S:
	case BuiltinType::SChar:		return gen.getChar();
	// case BuiltinType::WChar:		return gen.getWChar();

	// integer types
	case BuiltinType::UShort:		return gen.getUInt2();
	case BuiltinType::Short:		return gen.getInt2();
	case BuiltinType::UInt:			return gen.getUInt4();
	case BuiltinType::Int:			return gen.getInt4();
	case BuiltinType::UInt128:		return gen.getUInt16();
	case BuiltinType::Int128:		return gen.getInt16();
	case BuiltinType::ULong:		return gen.getUInt8();
	case BuiltinType::ULongLong:	return gen.getUInt8();
	case BuiltinType::Long:			return gen.getInt8();
	case BuiltinType::LongLong:		return gen.getInt8();

	// real types
	case BuiltinType::Float:		return gen.getFloat();
	case BuiltinType::Double:		return gen.getDouble();
	case BuiltinType::LongDouble:	return gen.getDouble(); // unsopported FIXME

	// not supported types
	case BuiltinType::NullPtr:
	case BuiltinType::Overload:
	case BuiltinType::Dependent:
	default:
		throw "type not supported"; //todo introduce exception class
	}
	assert(false && "Built-in type conversion not supported!");
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter::VisitTagType(const TagType* tagType) {
	VLOG(2) << "VisitTagType " << tagType  <<  std::endl;
	START_LOG_TYPE_CONVERSION( tagType );

	if(!convFact.ctx.recVarMap.empty()) {
		// check if this type has a typevar already associated, in such case return it
		ConversionContext::TypeRecVarMap::const_iterator fit = convFact.ctx.recVarMap.find(tagType);
		if( fit != convFact.ctx.recVarMap.end() ) {
			// we are resolving a parent recursive type, so we shouldn't
			return fit->second;
		}
	}

	// check if the type is in the cache of already solved recursive types
	// this is done only if we are not resolving a recursive sub type
	if(!convFact.ctx.isRecSubType) {
		ConversionContext::RecTypeMap::const_iterator rit = convFact.ctx.recTypeCache.find(tagType);
		if(rit != convFact.ctx.recTypeCache.end())
			return rit->second;
	}

	// will store the converted type
	core::TypePtr retTy;
	VLOG(1) << "~ Converting TagType: " << tagType->getDecl()->getName().str();

	const TagDecl* tagDecl = tagType->getDecl()->getCanonicalDecl();
	ConversionContext::ClassDeclMap::const_iterator cit = convFact.ctx.classDeclMap.find(tagDecl);
	if(cit != convFact.ctx.classDeclMap.end()){
		return cit->second;
	}

	// iterate through all the re-declarations to see if one of them provides a definition
	TagDecl::redecl_iterator i,e = tagDecl->redecls_end();
	for(i = tagDecl->redecls_begin(); i != e && !i->isCompleteDefinition(); ++i) ;
	if(i != e) {
		tagDecl = i->getDefinition();
		// we found a definition for this declaration, use it
		assert(tagDecl->isCompleteDefinition() && "TagType is not a definition");

		if(tagDecl->getTagKind() == clang::TTK_Enum) {
			// Enums are converted into integers
			return convFact.builder.getLangBasic().getInt4();
		} else {
			// handle struct/union/class
			const RecordDecl* recDecl = dyn_cast<const RecordDecl>(tagDecl);
			assert(recDecl && "TagType decl is not of a RecordDecl type!");

			if(!convFact.ctx.isRecSubType) {
				// add this type to the type graph (if not present)
				typeGraph.addNode(tagDecl->getTypeForDecl());
			}

			// retrieve the strongly connected componenets for this type
			std::set<const Type*>&& components =
				typeGraph.getStronglyConnectedComponents(tagDecl->getTypeForDecl());

			if( !components.empty() ) {
				if(VLOG_IS_ON(2)) {
					// we are dealing with a recursive type
					VLOG(2) << "Analyzing RecordDecl: " << recDecl->getNameAsString() << std::endl
							<< "Number of components in the cycle: " << components.size();
					std::for_each(components.begin(), components.end(),
						[] (std::set<const Type*>::value_type c) {
							assert(isa<const TagType>(c));
							VLOG(2) << "\t" << dyn_cast<const TagType>(c)->getDecl()->getNameAsString();
						}
					);
					typeGraph.print(std::cerr);
				}

				// we create a TypeVar for each type in the mutual dependence
				convFact.ctx.recVarMap.insert(
						std::make_pair(tagType, convFact.builder.typeVariable(recDecl->getName()))
					);

				// when a subtype is resolved we aspect to already have these variables in the map
				if(!convFact.ctx.isRecSubType) {
					std::for_each(components.begin(), components.end(),
						[ this ] (std::set<const Type*>::value_type ty) {
							const TagType* tagTy = dyn_cast<const TagType>(ty);
							assert(tagTy && "Type is not of TagType type");

							this->convFact.ctx.recVarMap.insert(
									std::make_pair(ty, convFact.builder.typeVariable(tagTy->getDecl()->getName()))
								);
						}
					);
				}
			}

			// Visit the type of the fields recursively
			// Note: if a field is referring one of the type in the cyclic dependency, a reference
			//       to the TypeVar will be returned.
			core::NamedCompositeType::Entries structElements;

			// TODO
			// c++ constructors
			const CXXRecordDecl* recDeclCXX = dyn_cast<const CXXRecordDecl>(recDecl);
			VLOG(2)<<recDeclCXX;

			if(recDeclCXX){
				bool hasPolymorphicBaseClass = false;
				// add only direct baseclasses as member
				for(CXXRecordDecl::base_class_const_iterator bit=recDeclCXX->bases_begin(),
								bend=recDeclCXX->bases_end(); bit != bend; ++bit) {
					const CXXBaseSpecifier * base = bit;
					RecordDecl *baseRecord = base->getType()->getAs<RecordType>()->getDecl();

					// put for every direct base-class a member to the derived class
					core::TypePtr&& fieldType = Visit( const_cast<Type*>(baseRecord->getTypeForDecl()) );
					VLOG(2) << "BaseClass is: " << baseRecord->getNameAsString() << " type: " << fieldType;
					core::StringValuePtr id = convFact.builder.stringValue(baseRecord->getNameAsString());
					structElements.push_back(convFact.builder.namedType(id, fieldType ));

					hasPolymorphicBaseClass |= base->getType()->getAsCXXRecordDecl()->isPolymorphic();
				}

//					for(CXXRecordDecl::ctor_iterator xit=recDeclCXX->ctor_begin(),
//							xend=recDeclCXX->ctor_end(); xit != xend; ++xit) {
//						CXXConstructorDecl * ctorDecl = *xit;
//						VLOG(1) << "~ Converting constructor: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;
//
//						core::TypePtr convertedType = convFact.convertType( GET_TYPE_PTR(ctorDecl) );
//						assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
//						core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);
//
//						//TODO funcType = addGlobalsToFunctionType(convFact.builder, convFact.ctx.globalStruct.first, funcType);
//
//						convFact.convertFunctionDecl(ctorDecl);
//						//std::cerr<<"dumpconstr: "<< curr->getNameAsString() << " ";
//						//curr->dumpDeclContext(); // on cerr
//						//std::cerr<<"enddumpconstr\n";
//						//core::StatementPtr&& body = convFact.convertStmt(curr->getBody());
//						//core::IdentifierPtr id = convFact.builder.identifier(curr->getNameAsString());
//					}
//
//					for(CXXRecordDecl::method_iterator mit=recDeclCXX->method_begin(),
//							mend=recDeclCXX->method_end(); mit != mend; ++mit) {
//						CXXMethodDecl * curr = *mit;
//						//convFact.convertFunctionDecl(curr, false);
//
//						//std::cerr<<"dumpconstr: "<< curr->getNameAsString() << " ";
//						//curr->dumpDeclContext(); // on cerr
//						//std::cerr<<"enddumpconstr\n";
//						//core::StatementPtr&& body = convFact.convertStmt(curr->getBody());
//						//core::IdentifierPtr id = convFact.builder.identifier(curr->getNameAsString());
//					}

				// add __class member to support virtual functions at highest polymorphic baseclass
				if( recDeclCXX->isPolymorphic() && !hasPolymorphicBaseClass) {
					VLOG(2) << recDeclCXX->getName().data() << " polymorphic class";

					core::StringValuePtr id = convFact.builder.stringValue("__class");
					structElements.push_back(convFact.builder.namedType(id, convFact.builder.getLangBasic().getUInt4()));
				}

			}  // end if recDeclCXX

			unsigned mid = 0;
			for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
				RecordDecl::field_iterator::value_type curr = *it;
				core::TypePtr&& fieldType = Visit( const_cast<Type*>(GET_TYPE_PTR(curr)) );
				// if the type is not const we have to add a ref because the value could be accessed and changed
				//if(!(curr->getType().isConstQualified() || core::dynamic_pointer_cast<const core::VectorType>(fieldType)))
				//	fieldType = convFact.builder.refType(fieldType);

				core::StringValuePtr id = convFact.builder.stringValue(
						curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));

				structElements.push_back(convFact.builder.namedType(id, fieldType));
				mid++;
			}

			// build a struct or union IR type
			retTy = handleTagType(tagDecl, structElements);

			if( !components.empty() ) {
				// if we are visiting a nested recursive type it means someone else will take care
				// of building the rectype node, we just return an intermediate type
				if(convFact.ctx.isRecSubType)
					return retTy;

				// we have to create a recursive type
				ConversionContext::TypeRecVarMap::const_iterator tit = convFact.ctx.recVarMap.find(tagType);
				assert(tit != convFact.ctx.recVarMap.end() &&
						"Recursive type has not TypeVar associated to himself");
				core::TypeVariablePtr recTypeVar = tit->second;

				vector<core::RecTypeBindingPtr> definitions;
				definitions.push_back( convFact.builder.recTypeBinding(recTypeVar, handleTagType(tagDecl, structElements) ) );

				// We start building the recursive type. In order to avoid loop the visitor
				// we have to change its behaviour and let him returns temporarely types
				// when a sub recursive type is visited.
				convFact.ctx.isRecSubType = true;

				std::for_each(components.begin(), components.end(),
					[ this, &definitions, &convFact, &recTypeVar ] (std::set<const Type*>::value_type ty) {
						const TagType* tagTy = dyn_cast<const TagType>(ty);
						assert(tagTy && "Type is not of TagType type");

						//Visual Studio 2010 fix: full namespace
						insieme::frontend::conversion::CXXConversionFactory::CXXConversionContext::TypeRecVarMap::const_iterator tit =
								this->convFact.ctx.recVarMap.find(ty);

						assert(tit != this->convFact.ctx.recVarMap.end() && "Recursive type has no TypeVar associated");
						core::TypeVariablePtr var = tit->second;

						// test whether this variable has already been handled
						if (*var == *recTypeVar) { return; }

						// we remove the variable from the list in order to fool the solver,
						// in this way it will create a descriptor for this type (and he will not return the TypeVar
						// associated with this recursive type). This behaviour is enabled only when the isRecSubType
						// flag is true
						this->convFact.ctx.recVarMap.erase(ty);

						definitions.push_back( convFact.builder.recTypeBinding(var, this->Visit(const_cast<Type*>(ty))) );
						var->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(tagTy->getDecl()->getNameAsString()) );

						// reinsert the TypeVar in the map in order to solve the other recursive types
						this->convFact.ctx.recVarMap.insert( std::make_pair(tagTy, var) );
					}
				);

				// sort definitions - this will produce the same list of definitions for each of the related types => shared structure
				if (definitions.size() > 1) {
					std::sort(definitions.begin(), definitions.end(), [](const core::RecTypeBindingPtr& a, const core::RecTypeBindingPtr& b){
						return a->getVariable()->getVarName()->getValue() < b->getVariable()->getVarName()->getValue();
					});
				}

				// we reset the behavior of the solver
				convFact.ctx.isRecSubType = false;
				// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
				convFact.ctx.recVarMap.clear();

				core::RecTypeDefinitionPtr&& definition = convFact.builder.recTypeDefinition(definitions);
				retTy = convFact.builder.recType(recTypeVar, definition);

				// Once we solved this recursive type, we add to a cache of recursive types
				// so next time we encounter it, we don't need to compute the graph
				convFact.ctx.recTypeCache.insert(std::make_pair(tagType, retTy));
			}

			// Adding the name of the C struct as annotation
			if (!recDecl->getName().empty())
				retTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(recDecl->getName()) );
			convFact.ctx.classDeclMap.insert(std::make_pair(tagDecl, retTy));
		}
	} else {
		// We didn't find any definition for this type, so we use a name and define it as a generic type
		retTy = convFact.builder.genericType( tagDecl->getNameAsString() );
	}
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

// Returns all bases of a c++ record declaration
vector<RecordDecl*> CXXConversionFactory::CXXTypeConverter::getAllBases(const clang::CXXRecordDecl* recDeclCXX ){
	vector<RecordDecl*> bases;

	for(CXXRecordDecl::base_class_const_iterator bit=recDeclCXX->bases_begin(),
			bend=recDeclCXX->bases_end(); bit != bend; ++bit) {
		const CXXBaseSpecifier * base = bit;
		RecordDecl *baseRecord = base->getType()->getAs<RecordType>()->getDecl();
		bases.push_back(baseRecord);
		vector<RecordDecl*> subBases = getAllBases(dyn_cast<clang::CXXRecordDecl>(baseRecord));
		bases.insert(bases.end(), subBases.begin(), subBases.end());
	}
	return bases;
}

//TODO
core::FunctionTypePtr CXXConversionFactory::CXXTypeConverter::addCXXThisToFunctionType(const core::IRBuilder& builder,
											   const core::TypePtr& globals,
											   const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size()+1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin()+1);
	// function is receiving a reference to the global struct as the first argument
	argTypes[0] = builder.refType(globals);
	return builder.functionType( argTypes, funcType->getReturnType() );

}

core::TypePtr CXXConversionFactory::CXXTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return convFact.builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return convFact.builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
}

//---------------------------------------------------------------------------------------------------------------------
//										CXX CLANG TYPE CONVERTER
//---------------------------------------------------------------------------------------------------------------------

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 						DEPENDENT SIZED ARRAY TYPE
// This type represents an array type in C++ whose size is a value-dependent
// expression. For example:
//
//  template<typename T, int Size>
//  class array {
//     T data[Size];
//  };
//
// For these types, we won't actually know what the array bound is until
// template instantiation occurs, at which point this will become either
// a ConstantArrayType or a VariableArrayType.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter ::VisitDependentSizedArrayType(const DependentSizedArrayType* arrTy) {
	assert(false && "DependentSizedArrayType not yet handled!");
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						REFERENCE TYPE (FIXME)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter ::VisitReferenceType(const ReferenceType* refTy) {
	return convFact.builder.refType( convFact.convertType( refTy->getPointeeType().getTypePtr()) );
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter ::VisitTemplateSpecializationType(const TemplateSpecializationType* templTy) {
	VLOG(2) << "TemplateName: " << templTy->getTemplateName().getAsTemplateDecl()->getNameAsString();
	VLOG(2) << "numTemplateArg: " << templTy->getNumArgs();
	for(size_t argId=0, end=templTy->getNumArgs(); argId < end; argId++) {
		assert(templTy->getArg(argId).getAsType().getTypePtr());
		VLOG(2) << "TemplateArguments: " << templTy->getArg(argId).getAsType().getTypePtr()->getTypeClassName();
	}
	VLOG(2) << "isSugared: " << templTy->isSugared();

	START_LOG_TYPE_CONVERSION(templTy);
	core::TypePtr retTy;
	if(templTy->isSugared()) {
		//convert Template arguments (template < ActualClass >) -> ActualClass has to be converted
		for(TemplateSpecializationType::iterator ait=templTy->begin(), ait_end=templTy->end(); ait!=ait_end; ait++) {
			VLOG(2) << "Converting TemplateArg";
			convFact.convertType(ait->getAsType().getTypePtr());
		}

		retTy = convFact.convertType(templTy->desugar().getTypePtr());
	}
	//assert(false && "TemplateSpecializationType not yet handled!");
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter ::VisitDependentTemplateSpecializationType(const DependentTemplateSpecializationType* tempTy) {
	core::TypePtr retTy;

	START_LOG_TYPE_CONVERSION(tempTy);
	assert(false && "DependentTemplateSpecializationType not yet handled!");
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					DEPENDENT TEMPLATE SPECIALIZATION TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter ::VisitInjectedClassNameType(const InjectedClassNameType* tempTy) {
	core::TypePtr retTy;

	START_LOG_TYPE_CONVERSION(tempTy);
	assert(false && "InjectedClassNameType not yet handled!");
	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					SUBSTITUTE TEMPLATE TYPE PARAMETER TYPE (TODO)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr CXXConversionFactory::CXXTypeConverter ::VisitSubstTemplateTypeParmType(const SubstTemplateTypeParmType* substTy) {
	core::TypePtr retTy;

//		VLOG(2) << "resultType: " << funcTy->getResultType().getTypePtr()->getTypeClassName();
//		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
//			[ this ] (const QualType& currArgType) {
//				VLOG(2) << "argType: " << currArgType.getTypePtr()->getTypeClassName();
//			}
//		);

//		VLOG(2) << "CLANG Type Classname: " << substTy->getReplacedParameter()->getTypeClassName();
	//TODO SHOULD WORK IN NEWER CLANG VERSION???
	//VLOG(2) << "Replaced Template Name: " << substTy->getReplacedParameter()->getDecl()->getNameAsString();
	//VLOG(2) << "Replacement Type: " << substTy->getReplacementType().getTypePtr();

	START_LOG_TYPE_CONVERSION(substTy);
	//assert(false && "SubstTemplateTypeParmType not yet handled!");
	retTy = convFact.convertType( substTy->getReplacementType().getTypePtr() );

	END_LOG_TYPE_CONVERSION( retTy );
	return retTy;
}

core::TypePtr CXXConversionFactory::CXXTypeConverter::Visit(const clang::Type* type) {
	return TypeVisitor<CXXTypeConverter, core::TypePtr>::Visit(type);
}

CXXConversionFactory::CXXTypeConverter*
CXXConversionFactory::makeCXXTypeConvert(CXXConversionFactory& fact, Program& program) {
	return new CXXConversionFactory::CXXTypeConverter(fact, program);
}

void CXXConversionFactory::cleanCXXTypeConvert(CXXTypeConverter* typeConv) {
	delete typeConv;
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

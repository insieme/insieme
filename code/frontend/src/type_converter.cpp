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

#include "insieme/frontend/type_converter.h"

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

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::TagDecl* decl) {
	return out << decl->getNameAsString();
}

} // end std namespace


namespace {

const clang::TagDecl* findDefinition(const clang::TagType* tagType) {

	// typedef std::map<std::pair<std::string, unsigned>, const clang::TagDecl*> DeclLocMap;
	// static DeclLocMap locMap;

	const clang::TagDecl* decl = tagType->getDecl();

	TagDecl::redecl_iterator i,e = decl->redecls_end();
	for(i = decl->redecls_begin(); i != e && !i->isCompleteDefinition(); ++i) ;

	if (i!=e) {
		const clang::TagDecl* def = (*i)->getDefinition();
	//	clang::SourceLocation loc = def->getLocation();

	//	auto fit = locMap.find({ def->getNameAsString(), loc.getRawEncoding() });
	//	if (fit != locMap.end()) {
	//		return fit->second;
	//	}
		// add this definition to the map
	//	locMap.insert({ { def->getNameAsString(), loc.getRawEncoding()}, def });
		return def;
	}

	return NULL;
}

} // end anonymous namespace

namespace insieme {
namespace frontend {


namespace utils {

	void addType ( DependencyGraph<const clang::TagDecl*>& obj, const clang::Type* type, const DependencyGraph<const clang::TagDecl*>::VertexTy& v) {

		auto purifyType = [](const clang::Type* type) -> const clang::Type* {

			if( const PointerType *ptrTy = dyn_cast<PointerType>(type) )
				return ptrTy->getPointeeType().getTypePtr();

			if( const ReferenceType *refTy = dyn_cast<ReferenceType>(type) )
				return refTy->getPointeeType().getTypePtr();

			if( const TypedefType* typeDefTy = llvm::dyn_cast<TypedefType>(type) ) {
				 return typeDefTy->getDecl()->getUnderlyingType().getTypePtr();
			}

			if( const ParenType* parTy = llvm::dyn_cast<ParenType>(type) ) {
				 return parTy->getInnerType().getTypePtr();
			}

			if( const ElaboratedType* elabTy = llvm::dyn_cast<ElaboratedType>(type) ) {
				 return elabTy->getNamedType().getTypePtr();
			}

			if( const ArrayType* arrTy = llvm::dyn_cast<ArrayType>(type) ) {
				 return arrTy->getElementType().getTypePtr();
			}

			return type;
		};

		// purify the type until a fixpoint is reached
		const Type* purified = type;
		while( (purified = purifyType(type)) != type )
			type = purified;

		if (VLOG_IS_ON(2))
			purified->dump();
		VLOG(2) << purified->getTypeClassName();

		if( const TagType* tagTy = llvm::dyn_cast<TagType>(purified) ) {
			// LOG(DEBUG) << "Adding " << tagTy->getDecl()->getNameAsString();
			if ( llvm::isa<RecordDecl>(tagTy->getDecl()) ) {
				// find the definition
				auto def = findDefinition(tagTy);

				// we may have no definition for the type
				if (!def) { return; }

				obj.addNode( def, &v );
			}
		}

		// if the filed is a function pointer then we need to examine both the return type and the
		// argument list
		if (const FunctionType* funcType = llvm::dyn_cast<FunctionType>(type)) {

			addType(obj, funcType->getResultType().getTypePtr(), v);

			// If this is a function proto then look for the arguments type
			if (const FunctionProtoType* funcProtType = llvm::dyn_cast<FunctionProtoType>(funcType)) {

				std::for_each(funcProtType->arg_type_begin(), funcProtType->arg_type_end(),
					[ & ] (const QualType& currArgType) {
						addType(obj, currArgType.getTypePtr(), v);
					}
				);
			}
		}

	};


template <>
void DependencyGraph<const clang::TagDecl*>::Handle(
		const clang::TagDecl* tagDecl,
		const DependencyGraph<const clang::TagDecl*>::VertexTy& v)
{
	using namespace clang;

	assert(tagDecl && "Type not of TagType class");

	const RecordDecl* tag = llvm::dyn_cast<const RecordDecl>(tagDecl);

	// if the tag type is not a struct but otherwise an enum, there will be no declaration
	// therefore we can safely return as there is no risk of recursion
	if (!tag) { return; }




	for(RecordDecl::field_iterator it=tag->field_begin(), end=tag->field_end(); it != end; ++it) {
		addType(*this, (*it)->getType().getTypePtr(), v);
	}

}

} // end utils namespace

namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//											CLANG TYPE CONVERTER
//---------------------------------------------------------------------------------------------------------------------

ConversionFactory::TypeConverter::TypeConverter(ConversionFactory& fact, Program& program):
	convFact( fact ), mgr(fact.mgr), builder(fact.builder), gen(fact.mgr.getLangBasic()) { }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								BUILTIN TYPES
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitBuiltinType(const BuiltinType* buldInTy) {
    LOG_BUILTIN_TYPE_CONVERSION(buldInTy);

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
	case BuiltinType::WChar_S:		return gen.getWChar();
	case BuiltinType::WChar_U:		return gen.getWChar();

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
	case BuiltinType::NullPtr:		return builder.typeVariable("nullptr_t"); //gen.getAnyRef(); //FIXME how do we handle the std::nullptr_t??
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
	// FIXME
	assert(false && "ComplexType not yet handled!");
	return core::TypePtr();
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
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return convFact.convertType( arrTy->desugar().getTypePtr() );

	size_t arrSize = *arrTy->getSize().getRawData();
	core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
	assert(elemTy && "Conversion of array element type failed.");

	core::TypePtr&& retTy = builder.vectorType(
			elemTy, core::ConcreteIntTypeParam::get(mgr, arrSize)
		);
	LOG_TYPE_CONVERSION( arrTy, retTy );
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
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return Visit( arrTy->desugar().getTypePtr() );

	auto elemTy = Visit( arrTy->getElementType().getTypePtr() );
	assert(elemTy && "Conversion of array element type failed.");

	auto retTy = builder.arrayType( elemTy );
	LOG_TYPE_CONVERSION( arrTy, retTy );
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
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return Visit( arrTy->desugar().getTypePtr() );

	core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
	assert(elemTy && "Conversion of array element type failed.");

	core::TypePtr retTy = builder.arrayType( elemTy );
	LOG_TYPE_CONVERSION( arrTy, retTy );
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

	core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );
	LOG_TYPE_CONVERSION( funcTy, retTy );

	assert(retTy && "Function has no return type!");

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is maintained
	if((retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)) {
		// only exception are OpenCL vectors
		if(!dyn_cast<const ExtVectorType>(funcTy->getResultType()->getUnqualifiedDesugaredType()))
			retTy = builder.refType(retTy);
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
					argTy = this->builder.refType(argTy);
			}

			argTypes.push_back( argTy );
			this->convFact.ctx.isResolvingFunctionType = false;
		}
	);

	if( argTypes.size() == 1 && mgr.getLangBasic().isUnit(argTypes.front())) {
		// we have only 1 argument, and it is a unit type (void), remove it from the list
		argTypes.clear();
	}

	if( funcTy->isVariadic() )
		argTypes.push_back( gen.getVarList() );

	retTy = builder.functionType( argTypes, retTy);
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					FUNCTION NO PROTO TYPE
// Represents a K&R-style 'int foo()' function, which has no information
// available about its arguments.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitFunctionNoProtoType(const FunctionNoProtoType* funcTy) {
	core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );
	LOG_TYPE_CONVERSION( funcTy, retTy );

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is mantained
	if(retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)
		retTy = builder.refType(retTy);

	assert(retTy && "Function has no return type!");

	retTy = builder.functionType( core::TypeList(), retTy);
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
	core::IntTypeParamPtr numElem = core::ConcreteIntTypeParam::get(mgr, num);

	//note: members of OpenCL vectors are never refs
	return builder.vectorType( subType, numElem);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 								TYPEDEF TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitTypedefType(const TypedefType* typedefType) {

	core::TypePtr subType = Visit( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
	LOG_TYPE_CONVERSION( typedefType, subType );
	assert(subType);

	// Adding the name of the typedef as annotation
	subType->addAnnotation(
		std::make_shared<annotations::c::CNameAnnotation>(typedefType->getDecl()->getNameAsString())
	);

    return  subType;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 								TYPE OF TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitTypeOfType(const TypeOfType* typeOfType) {
	core::TypePtr retTy = gen.getUnit();
	LOG_TYPE_CONVERSION( typeOfType, retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							TYPE OF EXPRESSION TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr ConversionFactory::TypeConverter::VisitTypeOfExprType(const TypeOfExprType* typeOfType) {
	core::TypePtr&& retTy = Visit( GET_TYPE_PTR(typeOfType->getUnderlyingExpr()) );
	LOG_TYPE_CONVERSION( typeOfType, retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 core::TypePtr ConversionFactory::TypeConverter::VisitTagType(const TagType* tagType) {
	auto tagDecl = findDefinition(tagType);

	if (tagDecl) {
		VLOG(2) << "VisitTagType " << tagDecl->getNameAsString() <<  std::endl;
	}

	if(!convFact.ctx.recVarMap.empty() && tagDecl) {
		// check if this type has a typevar already associated, in such case return it
		ConversionContext::TypeRecVarMap::const_iterator fit = convFact.ctx.recVarMap.find(tagDecl);
		if( fit != convFact.ctx.recVarMap.end() ) {
			// we are resolving a parent recursive type, so we shouldn't
			return fit->second;
		}
	}

	// check if the type is in the cache of already solved recursive types
	// this is done only if we are not resolving a recursive sub type
	if(!convFact.ctx.isRecSubType && tagDecl) {
		ConversionContext::RecTypeMap::const_iterator rit = convFact.ctx.recTypeCache.find(tagDecl);
		if(rit != convFact.ctx.recTypeCache.end())
			return rit->second;
	}

	// will store the converted type
	core::TypePtr retTy;
	VLOG(1) << "~ Converting TagType: " << tagType->getDecl()->getName().str();
    LOG_TYPE_CONVERSION( tagType, retTy );

	if( tagDecl ) {

		// we found a definition for this declaration, use it
		assert(tagDecl->isCompleteDefinition() && "TagType is not a definition");

		if(tagDecl->getTagKind() == clang::TTK_Enum) {
			// Enums are converted into integer
			return gen.getInt4();
		} else {
			// handle struct/union/class
			const RecordDecl* recDecl = dyn_cast<const RecordDecl>(tagDecl);
			assert(recDecl && "TagType decl is not of a RecordDecl type!");

			if(!convFact.ctx.isRecSubType) {
				// add this type to the type graph (if not present)
				typeGraph.addNode( tagDecl );
			}

			// retrieve the strongly connected componenets for this type
			std::set<const TagDecl*>&& components = typeGraph.getStronglyConnectedComponents(tagDecl);

			if( !components.empty() ) {

				std::set<const clang::TagDecl*>&& subComponents = typeGraph.getSubComponents( tagDecl );
				for(const auto& cur : subComponents) {
					TagDecl* decl = const_cast<TagDecl*>(cur);

					VLOG(2) << "Analyzing TagDecl as sub component: " << decl->getNameAsString();

					auto fit = convFact.ctx.recTypeCache.find(decl);
					if ( fit == convFact.ctx.recTypeCache.end() ) {
						// perform the conversion only if this is the first time this
						// function is encountred
						VisitTagType(cast<clang::TagType>(decl->getTypeForDecl()));
					}
				}

				if(VLOG_IS_ON(2)) {
					// we are dealing with a recursive type
					VLOG(2) << "Analyzing RecordDecl: " << recDecl->getNameAsString() << std::endl
							<< "Number of components in the cycle: " << components.size();
					std::for_each(components.begin(), components.end(),
						[] (std::set<const TagDecl*>::value_type c) {
							VLOG(2) << "\t" << c->getNameAsString();
						}
					);
					typeGraph.print(std::cerr);
				}

				// get the name of the struct which are converting into IR
				auto decl_name = recDecl->getName();
				static int cnt = 0;

				if (decl_name.empty()) {
					// this is an anonymous struct, we need to generate a name
					decl_name = "__insieme_struct_" + std::to_string(cnt++);
				}
				
				// we create a TypeVar for each type in the mutual dependence
				convFact.ctx.recVarMap.insert(
						std::make_pair(tagDecl, builder.typeVariable(decl_name))
					);

				// when a subtype is resolved we aspect to already have these variables in the map
				if(!convFact.ctx.isRecSubType) {
					std::for_each(components.begin(), components.end(),
						[ this ] (std::set<const TagDecl*>::value_type cur) {
							auto decl_name = cur->getName();

							if (decl_name.empty()) {
								// this is an anonymous struct, we need to generate a name
								decl_name = "__insieme_struct_" + std::to_string(cnt++);
							}

							this->convFact.ctx.recVarMap.insert(
									std::make_pair(cur, builder.typeVariable(decl_name))
								);
						}
					);
				}
				
			}

			// Visit the type of the fields recursively
			// Note: if a field is referring one of the type in the cyclic dependency, a reference
			//       to the TypeVar will be returned.
			core::NamedCompositeType::Entries structElements;

			unsigned mid = 0;
			for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
				RecordDecl::field_iterator::value_type curr = *it;
				core::TypePtr&& fieldType = Visit( const_cast<Type*>(GET_TYPE_PTR(curr)) );
				// if the type is not const we have to add a ref because the value could be accessed and changed
				//if(!(curr->getType().isConstQualified() || core::dynamic_pointer_cast<const core::VectorType>(fieldType)))
				//	fieldType = convFact.builder.refType(fieldType);

				core::StringValuePtr id = builder.stringValue(
						curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));

				structElements.push_back(builder.namedType(id, fieldType));
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
				ConversionContext::TypeRecVarMap::const_iterator tit = convFact.ctx.recVarMap.find(tagDecl);
				assert(tit != convFact.ctx.recVarMap.end() &&
						"Recursive type has not TypeVar associated to himself");
				core::TypeVariablePtr recTypeVar = tit->second;

				vector<core::RecTypeBindingPtr> definitions;
				definitions.push_back( builder.recTypeBinding(recTypeVar, handleTagType(tagDecl, structElements) ) );

				// We start building the recursive type. In order to avoid loop the visitor
				// we have to change its behaviour and let him returns temporarely types
				// when a sub recursive type is visited.
				convFact.ctx.isRecSubType = true;

				std::for_each(components.begin(), components.end(),
					[ this, &definitions, &recTypeVar ] (std::set<const TagDecl*>::value_type decl) {

						//Visual Studio 2010 fix: full namespace
						auto tit = this->convFact.ctx.recVarMap.find(decl);

						assert(tit != this->convFact.ctx.recVarMap.end() && "Recursive type has no TypeVar associated");
						core::TypeVariablePtr var = tit->second;

						// test whether this variable has already been handled
						if (*var == *recTypeVar) { return; }

						// we remove the variable from the list in order to fool the solver,
						// in this way it will create a descriptor for this type (and he will not return the TypeVar
						// associated with this recursive type). This behaviour is enabled only when the isRecSubType
						// flag is true
						this->convFact.ctx.recVarMap.erase(decl);

						definitions.push_back( this->builder.recTypeBinding(var, this->Visit(const_cast<Type*>(decl->getTypeForDecl()))) );
						var->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(var->getVarName()->getValue()) );

						// reinsert the TypeVar in the map in order to solve the other recursive types
						this->convFact.ctx.recVarMap.insert( std::make_pair(decl, var) );
					}
				);

				// sort definitions - this will produce the same list of definitions for each of the
				// related types => shared structure
				if (definitions.size() > 1) {
					std::sort(definitions.begin(), definitions.end(), [](const core::RecTypeBindingPtr& a,
																		 const core::RecTypeBindingPtr& b){
						return a->getVariable()->getVarName()->getValue() < b->getVariable()->getVarName()->getValue();
					});
				}

				// we reset the behavior of the solver
				convFact.ctx.isRecSubType = false;
				// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
				convFact.ctx.recVarMap.clear();

				core::RecTypeDefinitionPtr&& definition = builder.recTypeDefinition(definitions);
				retTy = builder.recType(recTypeVar, definition);

				// Once we solved this recursive type, we add to a cache of recursive types
				// so next time we encounter it, we don't need to compute the graph
				convFact.ctx.recTypeCache.insert( {tagDecl, retTy} );
			}

			// Adding the name of the C struct as annotation
			if (!recDecl->getName().empty())
				retTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(recDecl->getName()) );
		}
	} else {
		// We didn't find any definition for this type, so we use a name and define it as a generic type
		retTy = builder.genericType( tagType->getDecl()->getNameAsString() );
	}
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
	core::TypePtr&& retTy = Visit( parenTy->getInnerType().getTypePtr() );
	LOG_TYPE_CONVERSION( parenTy, retTy );
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
	core::TypePtr&& subTy = Visit( pointerTy->getPointeeType().getTypePtr() );
	// ~~~~~ Handling of special cases ~~~~~~~
	// void* -> array<'a>
	if( gen.isUnit(subTy) ) {
		return gen.getAnyRef();
	}
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr&& retTy = (subTy->getNodeType() == core::NT_FunctionType)? 
		subTy : builder.refType(builder.arrayType( subTy ));

	LOG_TYPE_CONVERSION( pointerTy, retTy );
	return retTy;
}


core::TypePtr ConversionFactory::TypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
	return core::TypePtr();
}

core::TypePtr ConversionFactory::CTypeConverter::Visit(const clang::Type* type) {
	return TypeVisitor<CTypeConverter, core::TypePtr>::Visit(type);
}

core::TypePtr ConversionFactory::CTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
	return core::TypePtr();
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

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

#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/types.h"

#include "insieme/c_info/naming.h"

#include "clang/AST/TypeVisitor.h"
#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace insieme {
namespace frontend {
namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define MAKE_SIZE(n)	toVector(core::IntTypeParam::getConcreteIntParam(n))
#define EMPTY_TYPE_LIST	vector<core::TypePtr>()

#define START_LOG_TYPE_CONVERSION(type) \
	DVLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting type [class: '" << (type)->getTypeClassName() << "']"; \
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang type: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		type->dump(); \
	}

#define END_LOG_TYPE_CONVERSION(type) \
	DVLOG(1) << "Converted 'type' into IR type: "; \
	DVLOG(1) << "\t" << *type;


//#############################################################################
//
//							CLANG TYPE CONVERTER
//
//############################################################################
class ConversionFactory::ClangTypeConverter: public TypeVisitor<ClangTypeConverter, core::TypePtr> {
	ConversionFactory& convFact;

	utils::DependencyGraph<const clang::Type*> typeGraph;
public:
	ClangTypeConverter(ConversionFactory& fact): convFact( fact ) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								BUILTIN TYPES
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitBuiltinType(BuiltinType* buldInTy) {
		START_LOG_TYPE_CONVERSION( buldInTy );
		const core::lang::BasicGenerator& gen = convFact.mgr.basic;
		
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
		case BuiltinType::WChar:		return gen.getWChar();

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
		case BuiltinType::LongDouble:	// unsopported FIXME

		// not supported types
		case BuiltinType::NullPtr:		
		case BuiltinType::Overload:
		case BuiltinType::Dependent:
		case BuiltinType::UndeducedAuto:
		default:
			throw "type not supported"; //todo introduce exception class
		}
		assert(false && "Built-in type conversion not supported!");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								COMPLEX TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitComplexType(ComplexType* bulinTy) {
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
	// The IR representation for such array will be: vector<ref<int<4>>,404>
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitConstantArrayType(ConstantArrayType* arrTy) {
		START_LOG_TYPE_CONVERSION( arrTy );
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return convFact.convertType( arrTy->desugar().getTypePtr() );

		size_t arrSize = *arrTy->getSize().getRawData();
		core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy && "Conversion of array element type failed.");

		// we need to check if the element type for this not a vector (or array) type
		if(!(core::dynamic_pointer_cast<const core::VectorType>(elemTy) || 
				core::dynamic_pointer_cast<const core::ArrayType>(elemTy))) {
			elemTy = convFact.builder.refType(elemTy);
		}
		core::TypePtr&& retTy = convFact.builder.vectorType( elemTy, 
			core::IntTypeParam::getConcreteIntParam(arrSize) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						INCOMPLETE ARRAT TYPE
	// This method handles C arrays with an unspecified size. For example
	// 'int A[]' has an IncompleteArrayType where the element type is 'int'
	// and the size is unspecified.
	//
	// The representation for such array will be: ref<array<ref<int<4>>>>
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitIncompleteArrayType(IncompleteArrayType* arrTy) {
		START_LOG_TYPE_CONVERSION( arrTy );
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy && "Conversion of array element type failed.");

		// we need to check if the element type for this not a vector (or array) type
		if(!(core::dynamic_pointer_cast<const core::VectorType>(elemTy) || 
				core::dynamic_pointer_cast<const core::ArrayType>(elemTy))) {
			elemTy = convFact.builder.refType(elemTy);
		}

		core::TypePtr&& retTy = builder.arrayType( elemTy );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VARIABLE ARRAT TYPE
	// This class represents C arrays with a specified size which is not an
	// integer-constant-expression. For example, 'int s[x+foo()]'. Since the
	// size expression is an arbitrary expression, we store it as such.
	// Note: VariableArrayType's aren't uniqued (since the expressions aren't)
	// and should not be: two lexically equivalent variable array types could
	// mean different things, for example, these variables do not have the same
	// type dynamically:
	//				void foo(int x) { int Y[x]; ++x; int Z[x]; }
	//
	// he representation for such array will be: array<ref<int<4>>>( expr() )
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitVariableArrayType(VariableArrayType* arrTy) {
		START_LOG_TYPE_CONVERSION( arrTy );
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy && "Conversion of array element type failed.");

		// we need to check if the element type for this not a vector (or array) type
		if(!(core::dynamic_pointer_cast<const core::VectorType>(elemTy) || 
				core::dynamic_pointer_cast<const core::ArrayType>(elemTy))) {
			elemTy = convFact.builder.refType(elemTy);
		}

		core::TypePtr retTy = builder.arrayType( elemTy );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

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
	core::TypePtr VisitDependentSizedArrayType(DependentSizedArrayType* arrTy) {
		assert(false && "DependentSizedArrayType not yet handled!");
	}

	// --------------------  FUNCTIONS  ---------------------------------------
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//				FUNCTION PROTO TYPE
	// Represents a prototype with argument type info, e.g. 'int foo(int)' or
	// 'int foo(void)'. 'void' is represented as having no arguments, not as
	// having a single void argument. Such a type can have an exception
	// specification, but this specification is not part of the canonical type.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitFunctionProtoType(FunctionProtoType* funcTy) {
		START_LOG_TYPE_CONVERSION(funcTy);

		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );
		assert(retTy && "Function has no return type!");

		core::TypeList argTypes;
		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
			[ &argTypes, this ] (const QualType& currArgType) {
				this->convFact.ctx.isResolvingFunctionType = true;
				core::TypePtr&& argTy = this->Visit( currArgType.getTypePtr() );
				argTypes.push_back( argTy );
				this->convFact.ctx.isResolvingFunctionType = false;
			}
		);

		if( argTypes.size() == 1 && *argTypes.front() == *convFact.mgr.basic.getUnit()) {
			// we have only 1 argument, and it is a unit type (void), remove it from the list
			argTypes.clear();
		}

		if( funcTy->isVariadic() )
			argTypes.push_back( convFact.mgr.basic.getVarList() );

		retTy = builder.functionType( argTypes, retTy);
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					FUNCTION NO PROTO TYPE
	// Represents a K&R-style 'int foo()' function, which has no information
	// available about its arguments.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitFunctionNoProtoType(FunctionNoProtoType* funcTy) {
		START_LOG_TYPE_CONVERSION( funcTy );
		core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );
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
	core::TypePtr VisitExtVectorType(ExtVectorType* vecTy) {
       // get vector datatype
        const QualType qt = vecTy->getElementType();
        const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
        core::TypePtr&& subType = Visit(const_cast<BuiltinType*>(buildInTy));

        // get the number of elements
        size_t num = vecTy->getNumElements();
        core::IntTypeParam numElem = core::IntTypeParam::getConcreteIntParam(num);

        //note: members of OpenCL vectors are always modifiable
        return convFact.builder.vectorType( convFact.builder.refType(subType), numElem);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 								TYPEDEF TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTypedefType(TypedefType* typedefType) {
		START_LOG_TYPE_CONVERSION( typedefType );

        core::TypePtr&& subType = Visit( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
        // Adding the name of the typedef as annotation
        subType->addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(typedefType->getDecl()->getNameAsString()));

        END_LOG_TYPE_CONVERSION( subType );
        return  subType;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 								TYPE OF TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTypeOfType(TypeOfType* typeOfType) {
		START_LOG_TYPE_CONVERSION(typeOfType);
		core::TypePtr retTy = convFact.mgr.basic.getUnit();
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 							TYPE OF EXPRESSION TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTypeOfExprType(TypeOfExprType* typeOfType) {
		START_LOG_TYPE_CONVERSION( typeOfType );
		core::TypePtr&& retTy = Visit( GET_TYPE_PTR(typeOfType->getUnderlyingExpr()) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTagType(TagType* tagType) {
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

		START_LOG_TYPE_CONVERSION(tagType);

		// will store the converted type
		core::TypePtr retTy;
		VLOG(1) << "~ Converting TagType: " << tagType->getDecl()->getName().str();

		const TagDecl* tagDecl = tagType->getDecl()->getCanonicalDecl();
		// iterate through all the re-declarations to see if one of them provides a definition
		TagDecl::redecl_iterator i,e = tagDecl->redecls_end();
		for(i = tagDecl->redecls_begin(); i != e && !i->isDefinition(); ++i) ;
		if(i != e) {
			tagDecl = tagDecl->getDefinition();
			// we found a definition for this declaration, use it
			assert(tagDecl->isDefinition() && "TagType is not a definition");

			if(tagDecl->getTagKind() == clang::TTK_Enum) {
				assert(false && "Enum types not supported yet");
			} else {
				// handle struct/union/class
				const RecordDecl* recDecl = dyn_cast<const RecordDecl>(tagDecl);
				assert(recDecl && "TagType decl is not of a RecordDecl type!");

				if(!convFact.ctx.isRecSubType) {
					// add this type to the type graph (if not present)
					typeGraph.addNode(tagDecl->getTypeForDecl());
				}

				// retrieve the strongly connected componenets for this type
				std::set<const Type*>&& components = typeGraph.getStronglyConnectedComponents(tagDecl->getTypeForDecl());

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
					convFact.ctx.recVarMap.insert( std::make_pair(tagType, convFact.builder.typeVariable(recDecl->getName())) );

					// when a subtype is resolved we aspect to already have these variables in the map
					if(!convFact.ctx.isRecSubType) {
						std::for_each(components.begin(), components.end(),
							[ this ] (std::set<const Type*>::value_type ty) {
								const TagType* tagTy = dyn_cast<const TagType>(ty);
								assert(tagTy && "Type is not of TagType type");

								this->convFact.ctx.recVarMap.insert( std::make_pair(ty, convFact.builder.typeVariable(tagTy->getDecl()->getName())) );
							}
						);
					}
				}

				// Visit the type of the fields recursively
				// Note: if a field is referring one of the type in the cyclic dependency, a reference
				//       to the TypeVar will be returned.
				core::NamedCompositeType::Entries structElements;
				for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
					RecordDecl::field_iterator::value_type curr = *it;
					core::TypePtr&& fieldType = Visit( const_cast<Type*>(GET_TYPE_PTR(curr)) );
					// if the type is not const we have to add a ref because the value could be accessed and changed
					if(!(curr->getType().isConstQualified() || core::dynamic_pointer_cast<const core::VectorType>(fieldType)))
						fieldType = convFact.builder.refType(fieldType);

					structElements.push_back( core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), fieldType ) );
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
					assert(tit != convFact.ctx.recVarMap.end() && "Recursive type has not TypeVar associated to himself");
					core::TypeVariablePtr recTypeVar = tit->second;

					core::RecTypeDefinition::RecTypeDefs definitions;
					definitions.insert( std::make_pair(recTypeVar, handleTagType(tagDecl, structElements) ) );

					// We start building the recursive type. In order to avoid loop the visitor
					// we have to change its behaviour and let him returns temporarely types
					// when a sub recursive type is visited.
					convFact.ctx.isRecSubType = true;

					std::for_each(components.begin(), components.end(),
						[ this, &definitions ] (std::set<const Type*>::value_type ty) {
							const TagType* tagTy = dyn_cast<const TagType>(ty);
							assert(tagTy && "Type is not of TagType type");

							//Visual Studio 2010 fix: full namespace
							insieme::frontend::conversion::ConversionFactory::ConversionContext::TypeRecVarMap::const_iterator tit =
									this->convFact.ctx.recVarMap.find(ty);

							assert(tit != this->convFact.ctx.recVarMap.end() && "Recursive type has no TypeVar associated");
							core::TypeVariablePtr var = tit->second;

							// we remove the variable from the list in order to fool the solver,
							// in this way it will create a descriptor for this type (and he will not return the TypeVar
							// associated with this recursive type). This behaviour is enabled only when the isRecSubType
							// flag is true
							this->convFact.ctx.recVarMap.erase(ty);

							definitions.insert( std::make_pair(var, this->Visit(const_cast<Type*>(ty))) );
							var->addAnnotation( std::make_shared<insieme::c_info::CNameAnnotation>(tagTy->getDecl()->getNameAsString()) );

							// reinsert the TypeVar in the map in order to solve the other recursive types
							this->convFact.ctx.recVarMap.insert( std::make_pair(tagTy, var) );
						}
					);
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
				retTy->addAnnotation( std::make_shared<insieme::c_info::CNameAnnotation>(recDecl->getName()) );
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
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitElaboratedType(ElaboratedType* elabType) {
		assert(false && "ElaboratedType not yet handled!");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							POINTER TYPE (FIXME)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitPointerType(PointerType* pointerTy) {
		START_LOG_TYPE_CONVERSION(pointerTy);

		core::TypePtr&& subTy = Visit(pointerTy->getPointeeType().getTypePtr());
		// ~~~~~ Handling of special cases ~~~~~~~
		// void* -> ref<'a>
		if(*subTy == *convFact.mgr.basic.getUnit()) {
			subTy = convFact.mgr.basic.getRefAlpha();
			END_LOG_TYPE_CONVERSION( subTy );
			return subTy;
		}
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		core::TypePtr&& retTy = convFact.builder.arrayType( convFact.builder.refType(subTy) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						REFERENCE TYPE (FIXME)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitReferenceType(ReferenceType* refTy) {
		return convFact.builder.refType( Visit( refTy->getPointeeType().getTypePtr()) );
	}

private:

	core::TypePtr handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
		if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
			return convFact.builder.structType( structElements );
		} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
			return convFact.builder.unionType( structElements );
		}
		assert(false && "TagType not supported");
	}
};

ConversionFactory::ClangTypeConverter* ConversionFactory::makeTypeConverter(ConversionFactory& fact) {
	return new ClangTypeConverter(fact);
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

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

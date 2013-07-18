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
#include "insieme/frontend/utils/debug.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/node_replacer.h"

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

Converter::TypeConverter::TypeConverter(Converter& fact):
	convFact( fact ), mgr(fact.mgr), builder(fact.builder), gen(fact.mgr.getLangBasic()) { }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								BUILTIN TYPES
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitBuiltinType(const BuiltinType* buldInTy) {
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
core::TypePtr Converter::TypeConverter::VisitComplexType(const ComplexType* bulinTy) {
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
core::TypePtr Converter::TypeConverter::VisitConstantArrayType(const ConstantArrayType* arrTy) {
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return convFact.convertType( arrTy->desugar().getTypePtr() );

	size_t arrSize = *arrTy->getSize().getRawData();
	core::TypePtr&& elemTy = convert( arrTy->getElementType().getTypePtr() );
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
core::TypePtr Converter::TypeConverter::VisitIncompleteArrayType(const IncompleteArrayType* arrTy) {
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return convert( arrTy->desugar().getTypePtr() );

	auto elemTy = convert( arrTy->getElementType().getTypePtr() );
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
core::TypePtr Converter::TypeConverter::VisitVariableArrayType(const VariableArrayType* arrTy) {
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return convert( arrTy->desugar().getTypePtr() );

	core::TypePtr&& elemTy = convert( arrTy->getElementType().getTypePtr() );
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
core::TypePtr Converter::TypeConverter::VisitFunctionProtoType(const FunctionProtoType* funcTy) {

	core::TypePtr&& retTy = convert( funcTy->getResultType().getTypePtr() );
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
			core::TypePtr&& argTy = this->convert( currArgType.getTypePtr() );

			// If the argument is of type vector or array we need to add a reference
			if(argTy->getNodeType() == core::NT_VectorType || argTy->getNodeType() == core::NT_ArrayType) {
				// only exception are OpenCL vectors
				if(!dyn_cast<const ExtVectorType>(currArgType->getUnqualifiedDesugaredType()))
					argTy = this->builder.refType(argTy);
			}

			argTypes.push_back( argTy );
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
core::TypePtr Converter::TypeConverter::VisitFunctionNoProtoType(const FunctionNoProtoType* funcTy) {
	core::TypePtr&& retTy = convert( funcTy->getResultType().getTypePtr() );
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
core::TypePtr Converter::TypeConverter::VisitExtVectorType(const ExtVectorType* vecTy) {
   // get vector datatype
	const QualType qt = vecTy->getElementType();
	const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
	core::TypePtr&& subType = convert(const_cast<BuiltinType*>(buildInTy));

	// get the number of elements
	size_t num = vecTy->getNumElements();
	core::IntTypeParamPtr numElem = core::ConcreteIntTypeParam::get(mgr, num);

	//note: members of OpenCL vectors are never refs
	return builder.vectorType( subType, numElem);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 								TYPEDEF TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitTypedefType(const TypedefType* typedefType) {
	core::TypePtr subType = convert( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
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
core::TypePtr Converter::TypeConverter::VisitTypeOfType(const TypeOfType* typeOfType) {
	core::TypePtr retTy = gen.getUnit();
	LOG_TYPE_CONVERSION( typeOfType, retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							TYPE OF EXPRESSION TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitTypeOfExprType(const TypeOfExprType* typeOfType) {
	core::TypePtr&& retTy = convert( GET_TYPE_PTR(typeOfType->getUnderlyingExpr()) );
	LOG_TYPE_CONVERSION( typeOfType, retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 core::TypePtr Converter::TypeConverter::VisitTagType(const TagType* tagType) {

	// test whether we can get a definiton
	auto def = findDefinition(tagType);

	if (!def) {
		// We didn't find any definition for this type, so we use a name and define it as a generic type
		return builder.genericType( tagType->getDecl()->getNameAsString() );
	}

	// handle enums => always just integers
	if(def->getTagKind() == clang::TTK_Enum) {
		return gen.getInt4();
	}

	// handle struct/union/class
	const RecordDecl* recDecl = dyn_cast<const RecordDecl>(def);
	assert(recDecl && "TagType decl is not of a RecordDecl type!");


	// Visit the type of the fields recursively
	// Note: if a field is referring one of the type in the cyclic dependency, a reference
	//       to the TypeVar will be returned.
	core::NamedCompositeType::Entries structElements;

	unsigned mid = 0;
	for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
		RecordDecl::field_iterator::value_type curr = *it;
		core::TypePtr&& fieldType = convert( GET_TYPE_PTR(curr) );

		core::StringValuePtr id = builder.stringValue(
				curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));

		structElements.push_back(builder.namedType(id, fieldType));
		mid++;
	}

	// build a struct or union IR type
	core::TypePtr retTy = handleTagType(def, structElements);

	// Adding the name of the C struct as annotation
	if (!recDecl->getName().empty())
		retTy->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>(recDecl->getName()) );

	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							ELABORATED TYPE (TODO)
//
// Represents a type that was referred to using an elaborated type keyword, e.g.,
// struct S, or via a qualified name, e.g., N::M::type, or both
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitElaboratedType(const ElaboratedType* elabType) {

	// elabType->dump();
	//elabType->desugar().getTypePtr()->dump();
	//std::cerr << elabType->getBaseElementTypeUnsafe() << std::endl <<"ElaboratedType not yet handled!!!!\n";

	VLOG(2) << "elabtype " << elabType << "\n";
	return convert( elabType->getNamedType().getTypePtr() );
//		assert(false && "ElaboratedType not yet handled!");
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							   PAREN TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitParenType(const ParenType* parenTy) {
	core::TypePtr&& retTy = convert( parenTy->getInnerType().getTypePtr() );
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
core::TypePtr Converter::TypeConverter::VisitPointerType(const PointerType* pointerTy) {
	core::TypePtr&& subTy = convert( pointerTy->getPointeeType().getTypePtr() );
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


core::TypePtr Converter::TypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return builder.structType( structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return builder.unionType( structElements );
	}
	assert(false && "TagType not supported");
	return core::TypePtr();
}

core::TypePtr Converter::CTypeConverter::convertInternal(const clang::Type* type) {
	return TypeVisitor<CTypeConverter, core::TypePtr>::Visit(type);
}


namespace {

	const RecordDecl* toRecordDecl(const clang::Type* type) {
		if (auto tagType = dyn_cast<const clang::TagType>(type)) {
			if (auto def = findDefinition(tagType)) {
				return dyn_cast<const RecordDecl>(def);
			}
		}
		return nullptr;
	}

	bool hasFreeTypeVariables(const core::NodePtr& node) {
		return core::analysis::hasFreeTypeVariables(node.isa<core::TypePtr>());
	}

	bool hasFreeTypeVariables(const core::TypePtr& type) {
		return core::analysis::hasFreeTypeVariables(type);
	}

	core::TypePtr closeRecursiveType(const core::TypePtr type, const core::TypeVariablePtr var) {
		// if it is a direct recursion, be done
		core::NodeManager& mgr = type.getNodeManager();
		core::IRBuilder builder(mgr);

		// make sure it is handling a struct or union type
		assert(type.isa<core::StructTypePtr>() || type.isa<core::UnionTypePtr>());

		// see whether there is any free type variable
		if (!hasFreeTypeVariables(type)) return type;

		// 1) check nested recursive types - those include this type

		// check whether there is nested recursive type specification that equals the current type
		std::vector<core::RecTypePtr> recTypes;
		core::visitDepthFirstOnce(type, [&](const core::RecTypePtr& cur) {
			if (cur->getDefinition()->getDefinitionOf(var)) recTypes.push_back(cur);
		}, true, true);

		// see whether one of these is matching
		for(auto cur : recTypes) {
			// TODO: here it should actually be checked whether the inner one is structurally identical
			//		 at the moment we relay on the fact that it has the same name
			return builder.recType(var, cur->getDefinition());
		}


		// 2) normalize recursive type

		// collect all struct types within the given type
		core::TypeList structs;
		core::visitDepthFirstOncePrunable(type, [&](const core::TypePtr& cur) {
			//if (containsVarFree(cur)) ;
			if (cur.isa<core::RecTypePtr>()) return !hasFreeTypeVariables(cur);
			if (cur.isa<core::NamedCompositeTypePtr>() && hasFreeTypeVariables(cur)) {
				structs.push_back(cur.as<core::TypePtr>());
			}
			return false;
		}, true);

		// check whether there is a recursion at all
		if (structs.empty()) return type;

		// create de-normalized recursive bindings
		vector<core::RecTypeBindingPtr> bindings;
		for(auto cur : structs) {
			bindings.push_back(builder.recTypeBinding(builder.typeVariable(annotations::c::getCName(cur)), cur));
		}

		// sort according to variable names
		std::sort(bindings.begin(), bindings.end(), [](const core::RecTypeBindingPtr& a, const core::RecTypeBindingPtr& b) {
			return a->getVariable()->getVarName()->getValue() < b->getVariable()->getVarName()->getValue();
		});

		// create definitions
		core::RecTypeDefinitionPtr def = builder.recTypeDefinition(bindings);

		// test whether this is actually a closed type ..
		if(hasFreeTypeVariables(def.as<core::NodePtr>())) return type;

		// normalize recursive representation
		core::RecTypeDefinitionPtr old;
		while(old != def) {
			old = def;

			// set up current variable -> struct definition replacement map
			core::NodeMap replacements;
			for (auto cur : def) {
				replacements[cur->getType()] = cur->getVariable();
			}

			// wrap into node mapper
			auto mapper = core::makeLambdaMapper([&](int, const core::NodePtr& cur) {
				return core::transform::replaceAllGen(mgr, cur, replacements);
			});

			// apply mapper to defintions
			vector<core::RecTypeBindingPtr> newBindings;
			for (core::RecTypeBindingPtr& cur : bindings) {
				auto newBinding = builder.recTypeBinding(cur->getVariable(), cur->getType()->substitute(mgr, mapper));
				if (!contains(newBindings, newBinding)) newBindings.push_back(newBinding);
			}
			bindings = newBindings;

			// update definitions
			def = builder.recTypeDefinition(bindings);
		}

		// convert structs into list of definitions

		// build up new recursive type (only if it is closed)
		auto res = builder.recType(var, def);
		return hasFreeTypeVariables(res.as<core::TypePtr>())?type:res;
	}

}


core::TypePtr Converter::TypeConverter::convert(const clang::Type* type) {
	assert(type && "Calling TypeConverter::Visit with a NULL pointer");
	auto& cache = convFact.typeCache;

	// look up type within cache
	auto pos = cache.find(type);
	if (pos != cache.end()) {
		return pos->second;
	}

	// create result location
	core::TypePtr res;

	//check if type is intercepted
	if(convFact.program.getInterceptor().isIntercepted(type)) {
		VLOG(2) << type << " isIntercepted";
		res = convFact.program.getInterceptor().intercept(type, convFact);
		cache[type] = res;
		return res;
	}

	// assume a recursive construct for record declarations
	if (auto recDecl = toRecordDecl(type)) {

		// create a (temporary) type variable for this type
		core::TypeVariablePtr var = builder.typeVariable(recDecl->getName());

		// bind recursive variable within the cache
		cache[type] = var;

		// resolve the type recursively
		res = convertInternal(type);

		// fix recursive type if necessary
		res = closeRecursiveType(res, var);

		// update type cache
		assert(cache[type] == var); // should not change in the meantime

		// update cache
		if (!hasFreeTypeVariables(res)) {
			// update cache
			cache[type] = res;
		} else {
			// remove temporary from cache
			cache.erase(type);
		}

	} else {

		// for all others no recursive definitions need to be considered
		res = convertInternal(type);

		// update cache
		if (!hasFreeTypeVariables(res)) cache[type] = res;
	}

	// run post-conversion actions
	postConvertionAction(type, res);

	// be done
	return res;
}

core::TypePtr Converter::CTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
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

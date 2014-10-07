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

#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/annotations/c/include.h"
#include "insieme/annotations/c/decl_only.h"

#include "insieme/core/lang/complex_extension.h"
#include "insieme/core/lang/simd_vector.h"
#include "insieme/core/lang/enum_extension.h"

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/DeclTemplate.h>


using namespace clang;
using namespace insieme;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::TagDecl* decl) {
	if (decl)
		return out << decl->getNameAsString();
	else
		return out << "[UNNAMED]";
}

} // end std namespace


namespace {

	const clang::TagDecl* findDefinition(const clang::TagType* tagType) {

		const clang::TagDecl* decl = tagType->getDecl();
		clang::TagDecl* res = nullptr;

		TagDecl::redecl_iterator i,e = decl->redecls_end();
		for(i = decl->redecls_begin(); i != e; ++i) {

			if (llvm::isa<clang::TypedefDecl> (*i)) {
				std::cerr << "this is a typedef aliased type" << std::endl;
				assert(false);
			}

			if (i->isCompleteDefinition()){
				res =  i->getDefinition();
			}
			if ( llvm::isa<clang::ClassTemplatePartialSpecializationDecl> (*i)) {
				continue;
			}

			if ( llvm::isa<clang::ClassTemplateSpecializationDecl> (*i)) {
				if (i->isCompleteDefinitionRequired () )
					res =  *i;
			}
		}


		if (res){
			return res;
		}

		return NULL;
	}

} // end anonymous namespace

namespace insieme {
namespace frontend {
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
	case BuiltinType::Char16:		return gen.getWChar16(); //TODO c++11 specific builtin
	case BuiltinType::Char32:		return gen.getWChar32(); //TODO c++11 specific builtin
	case BuiltinType::Char_S:
	case BuiltinType::SChar:		return gen.getChar();
	case BuiltinType::WChar_S:		return gen.getWChar32();
	case BuiltinType::WChar_U:		return gen.getWChar32();

	// integer types
	case BuiltinType::UShort:		return gen.getUInt2();
	case BuiltinType::Short:		return gen.getInt2();
	case BuiltinType::UInt:			return gen.getUInt4();
	case BuiltinType::Int:			return gen.getInt4();
	case BuiltinType::UInt128:		return gen.getUInt16();
	case BuiltinType::Int128:		return gen.getInt16();
	case BuiltinType::ULong:		return gen.getUInt8();
	case BuiltinType::Long:			return gen.getInt8();

	case BuiltinType::LongLong:		return gen.getInt16();
	case BuiltinType::ULongLong:	return gen.getUInt16();

	// real types
	case BuiltinType::Float:		return gen.getFloat();
	case BuiltinType::Double:		return gen.getDouble();
	case BuiltinType::LongDouble:	return gen.getLongDouble();

	// not supported types
	case BuiltinType::NullPtr:		return builder.typeVariable("nullptr_t"); //TODO c++11 specific builtin type for nullptr literal
	case BuiltinType::Overload:
	case BuiltinType::Dependent:
	default:
		throw "type not supported"; //todo introduce exception class
	}
	frontend_assert(false) << "Built-in type conversion not supported!\n";
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								COMPLEX TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitComplexType(const ComplexType* bulinTy) {
    //extract the real type
    const core::TypePtr& type = convert(bulinTy->getElementType());
    frontend_assert(type) << "Conversion of complex element type failed.\n";
    return  mgr.getLangExtension<core::lang::ComplexExtension>().getComplexType(type);
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
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( arrTy, retTy );

	// FIXME: check why we should desugar stuff
	//if(arrTy->isSugared())
	//	// if the type is sugared, we Visit the desugared type
	//	return (retTy = convFact.convertType( arrTy->desugar() ));

	size_t arrSize = *arrTy->getSize().getRawData();
	core::TypePtr&& elemTy = convert( arrTy->getElementType());
	frontend_assert(elemTy) << "Conversion of array element type failed.\n";

	 retTy = builder.vectorType(
			elemTy, core::ConcreteIntTypeParam::get(mgr, arrSize)
		);
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
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( arrTy, retTy );
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return (retTy = convert( arrTy->desugar() ));

	auto elemTy = convert( arrTy->getElementType() );
	frontend_assert(elemTy ) << "Conversion of array element type failed.\n";

	retTy = builder.arrayType( elemTy );
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
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( arrTy, retTy );
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return (retTy = convert( arrTy->desugar() ));

	core::TypePtr&& elemTy = convert( arrTy->getElementType() );
	frontend_assert(elemTy ) << "Conversion of array element type failed.\n";

	retTy = builder.arrayType( elemTy );
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

	core::TypePtr&& retTy = convert( funcTy->getResultType() );
	LOG_TYPE_CONVERSION( funcTy, retTy );

	frontend_assert(retTy) <<  "Function has no return type!\n";

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is maintained
	if((retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)) {
		// exceptions are OpenCL vectors and gcc-vectors
		// this applies also for OpenCL ExtVectorType. If this is moved, take care it still works also for them.
		if(!funcTy->getResultType()->getUnqualifiedDesugaredType()->isVectorType())
		{
			retTy = builder.refType(retTy);
		}
	}

	frontend_assert(retTy ) <<  "Function has no return type!\n";

	core::TypeList argTypes;
	std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
		[ &argTypes, this ] (const QualType& currArgType) {


			core::TypePtr&& argTy = this->convert( currArgType );


			// If the argument is of type vector or array we need to add a reference
			if(argTy->getNodeType() == core::NT_VectorType || argTy->getNodeType() == core::NT_ArrayType) {
				// exceptions are OpenCL vectors and gcc-vectors
				// this applies also for OpenCL ExtVectorType. If this is moved, take care it still works also for them.
				if(!currArgType->getUnqualifiedDesugaredType()->isVectorType())
				{
					argTy = builder.refType(argTy);
				}
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
	core::TypePtr&& retTy = convert( funcTy->getResultType());
	LOG_TYPE_CONVERSION( funcTy, retTy );

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is mantained
	if(retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)
		retTy = builder.refType(retTy);

	frontend_assert(retTy) << "Function has no return type!\n";

	retTy = builder.functionType( core::TypeList(), retTy);
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							VECTOR TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitVectorType(const VectorType* vecTy) {
	//VectorType - GCC generic vector type
	//This type is created using __attribute__((vector_size(n)) where "n" specifies the vector size in bytes
	//or from an Altivec __vector or vector declaration
	//Since the constructor takes the number of vector elements, the client is responsible for converting the size into the number of elements.
	core::TypePtr retIr;
	LOG_TYPE_CONVERSION( vecTy, retIr);

    // get vector datatype
	const QualType qt = vecTy->getElementType();
	//const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
	core::TypePtr&& subType = convert(qt);

	// get the number of elements
	size_t num = vecTy->getNumElements();
	core::IntTypeParamPtr numElem = core::ConcreteIntTypeParam::get(mgr, num);

	auto irVecTy = builder.vectorType( subType, numElem);
	return (retIr = core::lang::toSIMDVector(irVecTy));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 								TYPEDEF TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitTypedefType(const TypedefType* typedefType) {
	auto underType = typedefType->getDecl()->getUnderlyingType();
	core::TypePtr retTy = convert(underType);
	LOG_TYPE_CONVERSION( typedefType, retTy );
	frontend_assert(retTy);
    return  retTy;
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
	core::TypePtr&& retTy = convert(typeOfType->getUnderlyingExpr()->getType() );
	LOG_TYPE_CONVERSION( typeOfType, retTy );
	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 core::TypePtr Converter::TypeConverter::VisitTagType(const TagType* tagType) {
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( tagType, retTy );

	// test whether we can get a definiton
//	auto def = tagType->getDecl();

	auto def = findDefinition(tagType);
	if (!def) {
		//tag type isonly declared
		auto decl = tagType->getDecl();
		std::string name = utils::getNameForRecord(decl, tagType->getCanonicalTypeInternal(), convFact.getSourceManager());

		// We didn't find any definition for this type, so we use a name and define it as a generic type
		retTy = builder.genericType( name );

		if (!decl->getNameAsString().empty()) {
			core::annotations::attachName(retTy,decl->getNameAsString());
		}

		//we mark the declaration only tagTypes to reconstruct them in the backend
		if(decl->isStruct())
			annotations::c::markDeclOnlyStruct(retTy.as<core::GenericTypePtr>());
		else if(decl->isClass())
			annotations::c::markDeclOnlyClass(retTy.as<core::GenericTypePtr>());
		else if(decl->isEnum())
			annotations::c::markDeclOnlyEnum(retTy.as<core::GenericTypePtr>());
		else if(decl->isUnion())
			annotations::c::markDeclOnlyUnion(retTy.as<core::GenericTypePtr>());

		return retTy;
	}

	//TODO splitup TagType visitor into EnumType-visitor and RecordType-visitor
	// handle enums => always just integers
	if(def->getTagKind() == clang::TTK_Enum) {
		const EnumDecl* enumDecl = llvm::cast<clang::EnumDecl>(def);
		frontend_assert(enumDecl) << "TagType decl is a EnumDecl type!\n";
		const auto& ext= mgr.getLangExtension<core::lang::EnumExtension>();
        std::vector<core::TypePtr> enumCtants;
        for(EnumDecl::enumerator_iterator it=enumDecl->enumerator_begin(), end=enumDecl->enumerator_end();it!=end;it++) {
			const string& enumConstantName = insieme::frontend::utils::buildNameForEnumConstant(*it, convFact.getSourceManager());
			enumCtants.push_back(ext.getEnumConstantType(enumConstantName, (*it)->getInitVal().toString(10)));
		};

        const string& enumTypeName = utils::buildNameForEnum(enumDecl, convFact.getSourceManager());
        auto enumType = ext.getEnumType(enumTypeName, enumCtants);
		core::annotations::attachName(enumType, enumDecl->getNameAsString());
		return enumType;
   	}

	// handle struct/union/class
	const RecordDecl* recDecl = dyn_cast<const RecordDecl>(def);
	frontend_assert(recDecl) << "TagType decl is not of a RecordDecl type!\n";


	// Visit the type of the fields recursively
	// Note: if a field is referring one of the type in the cyclic dependency, a reference
	//       to the TypeVar will be returned.
	core::NamedCompositeType::Entries structElements;

	unsigned mid = 0;
	for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {

		RecordDecl::field_iterator::value_type curr = *it;
		core::TypePtr&& fieldType = convert(curr->getType() );

		core::StringValuePtr id = builder.stringValue(
				curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));
		structElements.push_back(builder.namedType(id, fieldType));
		mid++;
	}


	// build a struct or union IR type
	retTy = handleTagType(def, structElements);

	// Adding the name of the C struct as annotation
	if (!recDecl->getNameAsString().empty()) {
        core::annotations::attachName(retTy,recDecl->getName());
	}

	return retTy;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							ELABORATED TYPE (TODO)
//
// Represents a type that was referred to using an elaborated type keyword, e.g.,
// struct S, or via a qualified name, e.g., N::M::type, or both
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitElaboratedType(const ElaboratedType* elabType) {
	core::TypePtr retTy;
	LOG_TYPE_CONVERSION( elabType, retTy );

	return (retTy = convert( elabType->getNamedType()));
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							   PAREN TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitParenType(const ParenType* parenTy) {
	core::TypePtr&& retTy = convert( parenTy->getInnerType());
	LOG_TYPE_CONVERSION( parenTy, retTy );
	return retTy;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                             ATOMIC TYPE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::TypePtr Converter::TypeConverter::VisitAtomicType(const AtomicType* atomicTy) {
    core::TypePtr ret = convert(atomicTy->getValueType());
    convFact.warnings.insert("Atomic type lost in INSPIRE");
    return ret;
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
	core::TypePtr&& subTy = convert( pointerTy->getPointeeType());
	// ~~~~~ Handling of special cases ~~~~~~~
	// void* -> array<'a>
	if( gen.isUnit(subTy) ) {
		return gen.getAnyRef();
	}
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr retTy ;
	if (subTy->getNodeType() == core::NT_FunctionType){
		retTy = subTy;
	}
	else{
		if(pointerTy->getPointeeType().isConstQualified ())
			retTy = builder.refType(builder.arrayType( subTy ), core::RK_SOURCE );
		else
			retTy = builder.refType(builder.arrayType( subTy ));
	}

    // Function pointers are IR function but
    // pointers of function pointers should be IR array of functions
    // hence we must stop recursion
    auto innerPtr = pointerTy->getPointeeType();
    if(innerPtr->isPointerType()) {
            if(innerPtr->getPointeeType().getTypePtr()->isFunctionType()) {
                subTy = convert(innerPtr->getPointeeType());
	            retTy = builder.refType(builder.arrayType( subTy ));
            }
    }

	LOG_TYPE_CONVERSION( pointerTy, retTy );
	return retTy;
}


core::TypePtr Converter::TypeConverter::VisitDecayedType(const DecayedType* decTy) {

	core::TypePtr&& subTy = convert( decTy->getPointeeType() );
	// ~~~~~ Handling of special cases ~~~~~~~
	// void* -> array<'a>
	if( gen.isUnit(subTy) ) {
		return gen.getAnyRef();
	}
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr&& retTy = (subTy->getNodeType() == core::NT_FunctionType)?
		subTy : builder.refType(builder.arrayType( subTy ));

    // Function pointers are IR function but
    // pointers of function pointers should be IR array of functions
    // hence we must stop recursion
    auto innerPtr = decTy->getPointeeType();
    if(innerPtr->isPointerType()) {
            if(innerPtr->getPointeeType().getTypePtr()->isFunctionType()) {
                subTy = convert(innerPtr->getPointeeType());
	            retTy = builder.refType(builder.arrayType( subTy ));
            }
    }

	LOG_TYPE_CONVERSION( decTy, retTy );
	return retTy;
}

core::TypePtr Converter::TypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {

	std::string name;
	if (tagDecl->getName() != ""){
		name = utils::getNameForRecord(llvm::cast<clang::RecordDecl>(tagDecl), 
									   tagDecl->getTypeForDecl()->getCanonicalTypeInternal(), 
									   convFact.getSourceManager());
	}
	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
		return builder.structType( builder.stringValue(name), structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return builder.unionType( builder.stringValue(name), structElements );
	}
	frontend_assert(false && "TagType not supported");
	return core::TypePtr();
}

core::TypePtr Converter::CTypeConverter::convertInternal(const clang::QualType& type) {
    return TypeVisitor<CTypeConverter, core::TypePtr>::Visit(type.getTypePtr());
}


core::TypePtr Converter::TypeConverter::convertImpl(const clang::QualType& type) {
	auto& typeCache = convFact.typeCache;

	// look up type within typeCache
	auto pos = typeCache.find(type);
	if (pos != typeCache.end()) {
		return pos->second;
	}

	// create result location
	core::TypePtr res;

	// assume a recursive construct for record declarations
	if (auto tagType = llvm::dyn_cast<clang::TagType>( type)) {
		auto recDecl = tagType->getDecl();
		std::string name = utils::getNameForRecord(recDecl, type, convFact.getSourceManager());

		// create a (temporary) type variable for this type
		core::GenericTypePtr symbol = builder.genericType(name);
		if (!recDecl->getNameAsString().empty()) {
			core::annotations::attachName(symbol,name);
		}

		// bind recursive variable within the typeCache
		typeCache[type] = symbol;

		// resolve the type recursively
		res = convertInternal(type);

		//check if type is defined in a system header --> if so add includeAnnotation which is used
		//in backend to avoid redeclaration of type

		if( convFact.getHeaderTagger().isDefinedInSystemHeader(recDecl) ) {
			bool systemHeaderOrigin = convFact.getSourceManager().isInSystemHeader(recDecl->getCanonicalDecl()->getSourceRange().getBegin());
			if(systemHeaderOrigin) {
				VLOG(2) << "isDefinedInSystemHeaders " << name << " " << res;
				if( core::annotations::hasNameAttached(symbol) ) {
                    if(recDecl->isStruct())
					    name  = "struct " + core::annotations::getAttachedName(symbol);
                    else if(recDecl->isUnion())
					    name  = "union " + core::annotations::getAttachedName(symbol);
                    else
					    name  = core::annotations::getAttachedName(symbol);
					core::annotations::attachName(symbol,name);
				}
				convFact.getHeaderTagger().addHeaderForDecl(res, recDecl);
			}
		}

		frontend_assert(res) << "it seems that no type was converted, this is not ok";


	//	// give the type a name to structs
	//	if (core::StructTypePtr strTy = res.isa<core::StructTypePtr>())
	//		res = core::transform::replaceNode(mgr, core::StructTypeAddress(strTy)->getName(), builder.stringValue(name)).as<core::TypePtr>();

		// check typeCache consistency
		frontend_assert(typeCache[type] == symbol); // should not change in the meantime

		// it might be that type was not complete, keep just the symbol and the name will be forwarded to be linked
		if (!res.isa<core::GenericTypePtr>()) {
			frontend_assert(res.isa<core::StructTypePtr>() || res.isa<core::UnionTypePtr>());

			// run post-conversion actions
			postConvertionAction(type, res);

			// register type within resulting translation unit
			convFact.getIRTranslationUnit().addType(symbol, res);
		}

		// is an enum, return it as it is
		if (mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(res)){
			typeCache[type] = res;
			return res;
		}

		// but the result is just the symbol
		return symbol;


	} else {

		// for all others no recursive definitions need to be considered
		res = convertInternal(type);

		// update typeCache
		typeCache[type] = res;
	}

	// run post-conversion actions
	postConvertionAction(type, res);

	// be done
	return res;
}

core::TypePtr Converter::TypeConverter::convert(const clang::QualType& type) {
    core::TypePtr irType;

    //iterate clang handler list and check if a handler wants to convert the type
	for(auto plugin : convFact.getConversionSetup().getPlugins()) {
	    irType = plugin->Visit(type, convFact);
        if(irType) break;
	}

    if(!irType)
        irType = convertImpl(type);

    for(auto plugin : convFact.getConversionSetup().getPlugins()) {
        irType = plugin->PostVisit(type, irType, convFact);
    }

	return irType;
}

//core::TypePtr Converter::CTypeConverter::handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
//	if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
//		return builder.structType( structElements );
//	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
//		return builder.unionType( structElements );
//	}
//	frontend_assert(false && "TagType not supported");
//	return core::TypePtr();
//}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

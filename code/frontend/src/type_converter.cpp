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

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/macros.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/annotations/c/include.h"
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
namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//											CLANG TYPE CONVERTER
//---------------------------------------------------------------------------------------------------------------------

Converter::TypeConverter::TypeConverter(Converter& fact):
	convFact( fact ), mgr(fact.mgr), builder(fact.builder), gen(fact.mgr.getLangBasic()) { }

core::TypePtr Converter::TypeConverter::pluginPostTypeVisit(const clang::Type* type, core::TypePtr& irType) {
    for(auto plugin : convFact.getConversionSetup().getPlugins()) {
        irType = plugin->PostVisit(type, irType, convFact);
    }
    return irType;
}

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
	case BuiltinType::Char16:		return gen.getWChar16();
	case BuiltinType::Char32:		return gen.getWChar32();
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

									// long long is packed in a struct to avoid aliases with just long
	case BuiltinType::LongLong:		return builder.structType(toVector( builder.namedType("longlong_val", gen.getInt8()))); 
	case BuiltinType::ULongLong:	return builder.structType(toVector( builder.namedType("longlong_val", gen.getUInt8()))); 

	// real types
	case BuiltinType::Float:		return gen.getFloat();
	case BuiltinType::Double:		return gen.getDouble();
	case BuiltinType::LongDouble:	return gen.getLongDouble();

	// not supported types
	case BuiltinType::NullPtr:		return builder.typeVariable("nullptr_t"); //gen.getAnyRef(); //FIXME how do we handle the std::nullptr_t??
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
    const core::TypePtr& type = convert(bulinTy->getElementType().getTypePtr());
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
	if(arrTy->isSugared())
		// if the type is sugared, we Visit the desugared type
		return convFact.convertType( arrTy->desugar().getTypePtr() );

	size_t arrSize = *arrTy->getSize().getRawData();
	core::TypePtr&& elemTy = convert( arrTy->getElementType().getTypePtr() );
	frontend_assert(elemTy) << "Conversion of array element type failed.\n";

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
	frontend_assert(elemTy ) << "Conversion of array element type failed.\n";

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
	frontend_assert(elemTy ) << "Conversion of array element type failed.\n";

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

	frontend_assert(retTy) <<  "Function has no return type!\n";

	// If the return type is of type vector or array we need to add a reference
	// so that the semantics of C argument passing is maintained
	if((retTy->getNodeType() == core::NT_VectorType || retTy->getNodeType() == core::NT_ArrayType)) {
		// exceptions are OpenCL vectors and gcc-vectors
		if( !funcTy->getResultType()->getUnqualifiedDesugaredType()->isExtVectorType()
			&& !funcTy->getResultType()->getUnqualifiedDesugaredType()->isVectorType())
		{
			retTy = builder.refType(retTy);
		}
	}

	frontend_assert(retTy ) <<  "Function has no return type!\n";

	core::TypeList argTypes;
	std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
		[ &argTypes, this ] (const QualType& currArgType) {
			core::TypePtr&& argTy = this->convert( currArgType.getTypePtr() );

			// If the argument is of type vector or array we need to add a reference
			if(argTy->getNodeType() == core::NT_VectorType || argTy->getNodeType() == core::NT_ArrayType) {
				// exceptions are OpenCL vectors and gcc-vectors
				if( !currArgType->getUnqualifiedDesugaredType()->isExtVectorType()
					&& !currArgType->getUnqualifiedDesugaredType()->isVectorType())
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
	core::TypePtr&& retTy = convert( funcTy->getResultType().getTypePtr() );
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
	const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
	core::TypePtr&& subType = convert(const_cast<BuiltinType*>(buildInTy));

	// get the number of elements
	size_t num = vecTy->getNumElements();
	core::IntTypeParamPtr numElem = core::ConcreteIntTypeParam::get(mgr, num);

	auto irVecTy = builder.vectorType( subType, numElem);
	return (retIr = core::lang::toSIMDVector(irVecTy));
}

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
	frontend_assert(subType);

	// typedefs take ownership of the inner type, this way, same anonimous types along translation units will
	// be named as the typdef if any
	if (core::GenericTypePtr symb = subType.isa<core::GenericTypePtr>()){
		core::TypePtr trgty =  convFact.lookupTypeDetails(symb);

		// a new generic type will point to the previous translation unit decl
		if (trgty.isa<core::StructTypePtr>()){
			std::string name = utils::getNameForRecord(typedefType->getDecl(), typedefType);
			core::GenericTypePtr gen = builder.genericType(name);

			core::TypePtr impl = symb;
			// if target is an annonymous type, we create a new type with the name of the typedef
			if (trgty.as<core::StructTypePtr>()->getName()->getValue().substr(0,4) == "_anon"){
				impl = builder.structType ( builder.stringValue( name), trgty.as<core::StructTypePtr>()->getParents(), trgty.as<core::StructTypePtr>()->getEntries());
			}

			convFact.getIRTranslationUnit().addType(gen, impl);
			return gen;
		}
	}

    //it may happen that we have something like 'typedef enum {...} name;'
    //in this case we can forget the annotation
    //typedef name annotation for struct/union messes with recursive type resolution
    //as typeDef is syntactic sugar drop the name annotation
    /*
    if( !mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(subType)
		&& !subType.isa<core::StructTypePtr>() && !subType.isa<core::UnionTypePtr>()) {
        // Adding the name of the typedef as annotation
        core::annotations::attachName(subType,typedefType->getDecl()->getNameAsString());
    }
    */
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
		core::TypePtr retTy = builder.genericType( tagType->getDecl()->getNameAsString() );
		if (!tagType->getDecl()->getNameAsString().empty()) {
			core::annotations::attachName(retTy,tagType->getDecl()->getNameAsString());
		}
        return retTy;
	}

	// handle enums => always just integers
	if(def->getTagKind() == clang::TTK_Enum) {
		//TODO splitup TagType visitor into EnumType-visitor and RecordType-visitor

		const EnumDecl* enumDecl = llvm::cast<clang::EnumDecl>(def);
		frontend_assert(enumDecl) << "TagType decl is a EnumDecl type!\n";

       	bool systemHeaderOrigin = convFact.getSourceManager().isInSystemHeader(enumDecl->getSourceRange().getBegin());


        const string& enumTypeName = utils::buildNameForEnum(enumDecl);
		const auto& ext= mgr.getLangExtension<core::lang::EnumExtension>();

		core::TypePtr enumTy = ext.getEnumType(enumTypeName);
		LOG_TYPE_CONVERSION( tagType, enumTy );

		if(systemHeaderOrigin) {
			//if the enumType comes from a system provided header we don't convert it
			//TODO let interceptor take care of
       		return enumTy;
		}

		//takes a enumConstantDecl and appends "enumConstantName=enumConstantInitValue" to the stream
		auto enumConstantPrinter = [&](std::ostream& out, const clang::EnumConstantDecl* ecd) {
					const string& enumConstantName = utils::buildNameForEnumConstant(ecd);
					const string& enumConstantInitVal = ecd->getInitVal().toString(10);
					out << enumConstantName << "=" << enumConstantInitVal;
				};

		//enumTypes can be shadowed if redeclared in different scope (similar to variables)
		//we add all enumTypes to the global scope so the enumconstants need a distinguishable name
		//(delivered by buildNameForEnumConstant)
		if(!core::annotations::hasNameAttached(enumTy)) {
			std::stringstream ss;
			ss << join(", ", enumDecl->enumerator_begin(), enumDecl->enumerator_end(), enumConstantPrinter);
			core::annotations::attachName(enumTy, ss.str());
		} else {
			std::stringstream annotation;
			annotation << core::annotations::getAttachedName(enumTy);
			// go through all possible enumConstants and attach them to the enumTy, if they aren't
			// there already
			for(EnumDecl::enumerator_iterator it=enumDecl->enumerator_begin(), end=enumDecl->enumerator_end();it!=end;it++) {
				const clang::EnumConstantDecl* enumConstant = *it;
				std::stringstream attachment;
                enumConstantPrinter(attachment, enumConstant);

                //check if element is already in enum list
                if(annotation.str().find(attachment.str()) == std::string::npos) {
                    annotation << ", " << attachment.str();
                }
			}
			core::annotations::attachName(enumTy, annotation.str());
		}
		VLOG(2) << "enumType " << enumTy << " attachedName: " << core::annotations::getAttachedName(enumTy) << "(" << tagType << ")";

		//return enum type
        return enumTy;
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
		core::TypePtr&& fieldType = convert( GET_TYPE_PTR(curr) );

		core::StringValuePtr id = builder.stringValue(
				curr->getIdentifier() ? curr->getNameAsString() : "__m"+insieme::utils::numeric_cast<std::string>(mid));
		structElements.push_back(builder.namedType(id, fieldType));
		mid++;
	}

	// build a struct or union IR type
	core::TypePtr retTy = handleTagType(def, structElements);

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

	// elabType->dump();
	//elabType->desugar().getTypePtr()->dump();
	//std::cerr << elabType->getBaseElementTypeUnsafe() << std::endl <<"ElaboratedType not yet handled!!!!\n";

	VLOG(2) << "elabtype " << elabType << "\n";
	return convert( elabType->getNamedType().getTypePtr() );
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
		std::string name = utils::getNameForRecord(llvm::cast<clang::RecordDecl>(tagDecl), tagDecl->getTypeForDecl());
		return builder.structType( builder.stringValue(name), structElements );
	} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
		return builder.unionType( structElements );
	}
	frontend_assert(false && "TagType not supported");
	return core::TypePtr();
}

core::TypePtr Converter::CTypeConverter::convertInternal(const clang::Type* type) {
   	assert(type && "Calling TypeConverter::Visit with a NULL pointer");
   	core::TypePtr retIr;
   	//iterate clang handler list and check if a handler wants to convert the type
	for(auto plugin : convFact.getConversionSetup().getPlugins()) {
		retIr = plugin->Visit(type, convFact);
		if(retIr)
			return pluginPostTypeVisit(type, retIr);
	}
	//check if the insieme type converter should be called
	if(!retIr) {
        retIr = TypeVisitor<CTypeConverter, core::TypePtr>::Visit(type);
	}
    //return post visited type
	return pluginPostTypeVisit(type, retIr);
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

}


core::TypePtr Converter::TypeConverter::convert(const clang::Type* type) {
	auto& typeCache = convFact.typeCache;

	// look up type within typeCache
	auto pos = typeCache.find(type);
	if (pos != typeCache.end()) {
		return pos->second;
	}

	// create result location
	core::TypePtr res;

	//check if type is intercepted
	if(convFact.program.getInterceptor().isIntercepted(type)) {
		VLOG(2) << type << " isIntercepted";
		res = convFact.program.getInterceptor().intercept(type, convFact);
		typeCache[type] = res;
		return res;
	}

	// assume a recursive construct for record declarations
	if (auto recDecl = toRecordDecl(type)) {
		std::string name = utils::getNameForRecord(recDecl, type);

		// create a (temporary) type variable for this type
		core::GenericTypePtr symbol = builder.genericType(name);
		if (!name.empty()) {
			core::annotations::attachName(symbol,name);
		}

		// bind recursive variable within the typeCache
		typeCache[type] = symbol;

		// resolve the type recursively
		res = convertInternal(type);

		//check if type is defined in a system header --> if so add includeAnnotation which is used
		//in backend to avoid redeclaration of type
		if( utils::isDefinedInSystemHeader(recDecl, convFact.getProgram().getStdLibDirs(), convFact.getProgram().getUserIncludeDirs()) ) {
			VLOG(2) << "isDefinedInSystemHeaders " << name << " " << res;
			utils::addHeaderForDecl(res, recDecl, convFact.getProgram().getStdLibDirs());
		}

		frontend_assert(res.isa<core::StructTypePtr>() || res.isa<core::UnionTypePtr>());

	//	// give the type a name to structs
	//	if (core::StructTypePtr strTy = res.isa<core::StructTypePtr>())
	//		res = core::transform::replaceNode(mgr, core::StructTypeAddress(strTy)->getName(), builder.stringValue(name)).as<core::TypePtr>();

		// check typeCache consistency
		frontend_assert(typeCache[type] == symbol); // should not change in the meantime

		// register type within resulting translation unit
		convFact.getIRTranslationUnit().addType(symbol, res);

		// run post-conversion actions
		postConvertionAction(type, res);

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

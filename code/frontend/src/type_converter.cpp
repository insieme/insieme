/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/name_manager.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/c/declaration.h"
#include "insieme/annotations/c/include.h"
#include "insieme/annotations/std_init_list.h"

using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//											CLANG TYPE CONVERTER
	//---------------------------------------------------------------------------------------------------------------------

	Converter::TypeConverter::TypeConverter(Converter& fact) : converter(fact), mgr(fact.mgr), builder(fact.builder), basic(fact.mgr.getLangBasic()) {}

	core::TypePtr Converter::CTypeConverter::convertInternal(const clang::QualType& type) {
		return TypeVisitor<CTypeConverter, core::TypePtr>::Visit(type.getTypePtr());
	}

	core::TypePtr Converter::TypeConverter::convert(const clang::QualType& type) {
		auto retTy = converter.applyExtensions<core::TypePtr>(type, [&](const clang::QualType& param) {
			return convertInternal(param);
		});

		if(!retTy) type->dump();
		assert_true(retTy) << "^^^^^^^^^^^^^^^^^ Type conversion to null\n";
		return retTy;
	}

	core::TypePtr Converter::TypeConverter::convertVarType(const clang::QualType& type) {
		auto irt = convert(type);
		// if it's already a ref, we come from C++ and are fine
		if(core::lang::isReference(irt)) return irt;
		// add correctly qualified "ref" to inner type
		return builder.refType(irt, type.isConstQualified(), type.isVolatileQualified());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								BUILTIN TYPES
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitBuiltinType(const BuiltinType* buldInTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(buldInTy, retTy);

		switch(buldInTy->getKind()) {
		case BuiltinType::Void: retTy = basic.getUnit(); break;
		case BuiltinType::Bool: retTy = basic.getBool(); break;

		// char types
		case BuiltinType::Char_U:
		case BuiltinType::UChar: retTy = basic.getUInt1(); break;
		case BuiltinType::Char16: retTy = basic.getWChar16(); break; // TODO c++11 specific builtin
		case BuiltinType::Char32: retTy = basic.getWChar32(); break; // TODO c++11 specific builtin
		case BuiltinType::Char_S: retTy = basic.getChar(); break;
		case BuiltinType::SChar: retTy = basic.getInt1(); break; // Signed char, remember: char, unsigned char, signed char are distinct types
		case BuiltinType::WChar_S: retTy = basic.getWChar32(); break;
		case BuiltinType::WChar_U: retTy = basic.getWChar32(); break;

		// integer types
		case BuiltinType::UShort: retTy = basic.getUInt2(); break;
		case BuiltinType::Short: retTy = basic.getInt2(); break;
		case BuiltinType::UInt: retTy = basic.getUInt4(); break;
		case BuiltinType::Int: retTy = basic.getInt4(); break;
		case BuiltinType::UInt128: retTy = basic.getUInt16(); break;
		case BuiltinType::Int128: retTy = basic.getInt16(); break;
		case BuiltinType::ULong: retTy = basic.getUInt8(); break;
		case BuiltinType::Long: retTy = basic.getInt8(); break;

		case BuiltinType::LongLong: retTy = basic.getInt16(); break;
		case BuiltinType::ULongLong: retTy = basic.getUInt16(); break;

		// real types
		case BuiltinType::Float: retTy = basic.getFloat(); break;
		case BuiltinType::Double: retTy = basic.getDouble(); break;
		case BuiltinType::LongDouble: retTy = basic.getLongDouble(); break;

		// not supported types
		case BuiltinType::NullPtr: retTy = builder.typeVariable("nullptr_t"); break; // TODO c++11 specific builtin type for nullptr literal
		default:
			buldInTy->dump();
			frontend_assert(false) << "Built-in type conversion not supported for this type.";
			break;
		}
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								COMPLEX TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitComplexType(const ComplexType* complexType) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(complexType, retTy);
		frontend_assert(false) << "ComplexType not implemented.";
		return retTy;
	}

	// ------------------------   ARRAYS  -------------------------------------
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 					CONSTANT ARRAY TYPE
	//
	// This method handles the canonical version of C arrays with a specified
	// constant size. For example, the canonical type for 'int A[4 + 4*100]' is
	// a ConstantArrayType where the element type is 'int' and the size is 404
	//
	// The IR representation for such array will be: array<int<4>,"404">
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitConstantArrayType(const ConstantArrayType* arrTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(arrTy, retTy);

		size_t arrSize = *arrTy->getSize().getRawData();
		core::TypePtr&& elemTy = convert(arrTy->getElementType());
		frontend_assert(elemTy) << "Conversion of array element type failed.";

		retTy = builder.arrayType(elemTy, arrSize);
		frontend_assert(elemTy) << "Conversion of ConstantArrayType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						INCOMPLETE ARRAY TYPE
	// This method handles C arrays with an unspecified size. For example
	// 'int A[]' has an IncompleteArrayType where the element type is 'int'
	// and the size is unspecified.
	//
	// The representation for such array will be: array<int<4>,inf>
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitIncompleteArrayType(const IncompleteArrayType* arrTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(arrTy, retTy);

		auto elemTy = convert(arrTy->getElementType());
		frontend_assert(elemTy) << "Conversion of array element type failed.";

		retTy = builder.arrayType(elemTy);
		frontend_assert(elemTy) << "Conversion of IncompleteArrayType failed.";
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
	// The representation for such arrays will be:
	// v0 = x;
	// array<int<4>,v0>
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitVariableArrayType(const VariableArrayType* arrTy) {
		frontend_assert(false) << "Variable arrays are handled in a separate FE extension, which does not appear to be loaded.";
		return core::TypePtr();
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
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(funcTy, retTy);

		core::TypePtr funRetTy = convert(funcTy->getReturnType());
		frontend_assert(funRetTy) << "Function has no return type.";

		core::TypeList argTypes;
		for(const QualType& currArgType : funcTy->param_types()) {
			core::TypePtr argTy = convert(currArgType);
			argTypes.push_back(argTy);
		}

		if(argTypes.size() == 1 && mgr.getLangBasic().isUnit(argTypes.front())) {
			// we have only 1 argument, and it is a unit type (void), remove it from the list
			argTypes.clear();
		}

		retTy = builder.functionType(argTypes, funRetTy);
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					FUNCTION NO PROTO TYPE
	// Represents a K&R-style 'int foo()' function, which has no information
	// available about its arguments.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitFunctionNoProtoType(const FunctionNoProtoType* funcTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(funcTy, retTy);
		core::TypePtr funRetTy = convert(funcTy->getReturnType());
		frontend_assert(funRetTy) << "Function has no return type.";

		retTy = builder.functionType(core::TypeList(), funRetTy);
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 							VECTOR TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitVectorType(const VectorType* vecTy) {
		// VectorType - GCC generic vector type
		// This type is created using __attribute__((vector_size(n)) where "n" specifies the vector size in bytes
		// or from an Altivec __vector or vector declaration
		// Since the constructor takes the number of vector elements, the client is responsible for converting the size into the number of elements.
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(vecTy, retTy);

		frontend_assert(false) << "VectorType not implemented.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 								TYPEDEF TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitTypedefType(const TypedefType* typedefType) {
		auto underType = typedefType->getDecl()->getUnderlyingType();
		core::TypePtr retTy = convert(underType);
		LOG_TYPE_CONVERSION(typedefType, retTy);
		frontend_assert(retTy) << "Conversion of TypedefType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 								TYPE OF TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitTypeOfType(const TypeOfType* typeOfType) {
		core::TypePtr retTy = basic.getUnit();
		LOG_TYPE_CONVERSION(typeOfType, retTy);
		frontend_assert(retTy) << "Conversion of TypeOfType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 							TYPE OF EXPRESSION TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitTypeOfExprType(const TypeOfExprType* typeOfExprType) {
		core::TypePtr retTy = convert(typeOfExprType->getUnderlyingExpr()->getType());
		LOG_TYPE_CONVERSION(typeOfExprType, retTy);
		frontend_assert(retTy) << "Conversion of TypeOfExprType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	namespace {
		core::TypePtr handleEnumType(const Converter& converter, const EnumType* clangEnumTy) {
			core::NodeManager& mgr = converter.getNodeManager();
			core::IRBuilder builder(mgr);
			auto enumDecl = clangEnumTy->getDecl();

			// determine correct integral type
			auto enumIntType = builder.getLangBasic().getInt4();
			if(enumDecl->isFixed()) {
				enumIntType = converter.convertType(enumDecl->getIntegerType());
			}

			std::string enumName = utils::getNameForTagDecl(converter, enumDecl).first;

			// collect enum constant decls
			std::vector<core::GenericTypePtr> enumElements;
			for(auto m : enumDecl->enumerators()) {
				// get value of enum element
				core::ExpressionPtr val = builder.literal(builder.getLangBasic().getInt8(), m->getInitVal().toString(10));
				std::string enumConstantDeclName = insieme::utils::mangle(m->getQualifiedNameAsString());
				// create enum element
				enumElements.push_back(core::lang::EnumEntry::create(builder.genericType(enumConstantDeclName), val));
			}

			// generate enum type
			core::GenericTypePtr definition = core::lang::EnumDefinition::create(builder.genericType(enumName), enumIntType, enumElements);
			auto enumResTy = core::lang::buildEnumType(definition);

			// attach necessary information for types defined in library headers
			converter.applyHeaderTagging(enumResTy, enumDecl);
			if(!enumDecl->getNameAsString().empty()) core::annotations::attachName(enumResTy, enumDecl->getNameAsString());
			return enumResTy;
		}

		void tryEnsuruseFullClangType(Converter& converter, const RecordType* clangRecordTy) {
			auto clangDecl = clangRecordTy->getDecl();

			// Try to get clang to fully instantiate template types for us before we start handling the type
			converter.getTranslationUnit().getInsiemeSema().RequireCompleteType(clangDecl->getLocStart(), clang::QualType(clangRecordTy, 0), 0);

			// if this type represents a ClassTemplateSpecialization, we ask clang to instantiate it explicitly
			if(auto ctsd = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(clangDecl)) {
				auto& instantiatedDecls = converter.getInstantiatedDecls();

				// though we only do so if we didn't already instantiate the type already
				if(instantiatedDecls.insert(ctsd).second) {
					// we have to cast away the const-ness of the decl in order to ask clang to instantiate the type specialization for us
					auto nonConstClassDecl = const_cast<clang::ClassTemplateSpecializationDecl*>(ctsd);
					converter.getTranslationUnit().getInsiemeSema().InstantiateClassTemplateSpecializationMembers(nonConstClassDecl->getLocation(), nonConstClassDecl,
					                                                                                              clang::TemplateSpecializationKind::TSK_ExplicitInstantiationDefinition);
					// and after that, we ask clang to perform all pending implicit instantiations
					converter.getTranslationUnit().getInsiemeSema().PerformPendingInstantiations();
				}
			}
		}

		core::TypePtr handleRecordType(Converter& converter, const RecordType* clangRecordTy) {

			// try to get the full type from clang - instantiating templates where possible
			tryEnsuruseFullClangType(converter, clangRecordTy);

			core::NodeManager& mgr = converter.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& rMan = *converter.getRecordMan();
			auto clangDecl = clangRecordTy->getDecl();
			core::GenericTypePtr genTy;
			string mangledName;
			bool useName;
			std::tie(mangledName, useName) = utils::getNameForTagDecl(converter, clangDecl);
			if(rMan.contains(clangDecl)) {
				genTy = rMan.lookup(clangDecl);
			} else {
				// first time we encounter this type
				genTy = builder.genericType(mangledName);
				converter.applyHeaderTagging(genTy, clangDecl);
				// make sure the generic type will be declared by the backend even if there is no visible definition
				annotations::c::markDeclaration(genTy);
				rMan.insert(clangDecl, genTy);
			}
			if(!clangDecl->isThisDeclarationADefinition() || converter.getIRTranslationUnit().hasType(genTy)) { return genTy; }

			// build actual struct type (after insert!)
			// insert dummy type in IR TU in case of recursive type definition, to be replaced with actual type
			converter.getIRTranslationUnit().addType(genTy, builder.structType());
			core::FieldList recordMembers;
			for(auto mem : clangDecl->fields()) {
				recordMembers.push_back(builder.field(insieme::frontend::utils::getNameForField(mem, converter.getSourceManager()), converter.convertType(mem->getType())));
			}
			auto compoundName = useName ? builder.stringValue(mangledName) : builder.stringValue("");
			core::TagTypePtr recordType = clangRecordTy->isUnionType() ? builder.unionType(compoundName, recordMembers)
				                                                       : builder.structType(compoundName, recordMembers);
			// attach necessary information for types defined in library headers
			converter.applyHeaderTagging(recordType, clangDecl);
			// we'll attach name only if available, this could be a problem if anonymous names are used
			if(!clangDecl->getNameAsString().empty()) core::annotations::attachName(recordType, clangDecl->getNameAsString());

			// attach the element type if the type is a std::initializer_list type
			if(clangDecl->getQualifiedNameAsString() == "std::initializer_list") {
				auto tempSpecDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(clangDecl);
				assert_true(tempSpecDecl) << "RecordDecl of an object of type std::initializer_list has to be of type ClassTemplateSpecializationDecl";
				assert_eq(1, tempSpecDecl->getTemplateArgs().size());
				auto templateArg = tempSpecDecl->getTemplateArgs().get(0);
				assert_eq(clang::TemplateArgument::Type, templateArg.getKind());
				annotations::markStdInitList(genTy, converter.convertType(templateArg.getAsType()));
			}

			// replace dummy type with actual type
			converter.getIRTranslationUnit().replaceType(genTy, recordType);

			return genTy;
		}
	} // anonymous namespace

	core::TypePtr Converter::TypeConverter::VisitTagType(const TagType* tagType) {
		VLOG(2) << "Converter::TypeConverter::VisitTagType " << tagType << std::endl;
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(tagType, retTy);

		if(auto clangRecTy = llvm::dyn_cast<RecordType>(tagType)) {
			retTy = handleRecordType(converter, clangRecTy);
		} else if(auto clangEnumTy = llvm::dyn_cast<EnumType>(tagType)) {
			retTy = handleEnumType(converter, clangEnumTy);
		} else {
			frontend_assert(false) << "TagType not implemented.";
		}

		converter.applyHeaderTagging(retTy, tagType->getDecl());

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
		LOG_TYPE_CONVERSION(elabType, retTy);

		retTy = convert(elabType->getNamedType());
		frontend_assert(retTy) << "Conversion of ElaboratedType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							   PAREN TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitParenType(const ParenType* parenTy) {
		core::TypePtr retTy = convert(parenTy->getInnerType());
		LOG_TYPE_CONVERSION(parenTy, retTy);
		frontend_assert(retTy) << "Conversion of ParenType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                             ATOMIC TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitAtomicType(const AtomicType* atomicTy) {
		core::TypePtr retTy = convert(atomicTy->getValueType());
		LOG_TYPE_CONVERSION(atomicTy, retTy);
		frontend_assert(false) << "Atomic types not implemented!\n";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							POINTER TYPE
	// Pointer types need to be converted into pointer types within the IR.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitPointerType(const PointerType* pointerTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(pointerTy, retTy);

		auto clangSubTy = pointerTy->getPointeeType();
		core::TypePtr subTy = convert(clangSubTy);

		bool isConst = clangSubTy.isConstQualified();
		// INSPIRE function references must be const
		if(clangSubTy->isFunctionType()) isConst = true;

		retTy = builder.ptrType(subTy, isConst, clangSubTy.isVolatileQualified());
		frontend_assert(retTy) << "Conversion of PointerType failed.";
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECAYED TYPE
	// A pointer type decayed from an array or function type.
	// Handled like a pointer type for the purpose of Inspire.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr Converter::TypeConverter::VisitDecayedType(const DecayedType* decTy) {
		core::TypePtr retTy;
		LOG_TYPE_CONVERSION(decTy, retTy);

		core::TypePtr subTy = convert(decTy->getPointeeType());

		retTy = builder.ptrType(subTy);
		frontend_assert(retTy) << "Conversion of DecayedType failed.";
		return retTy;
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

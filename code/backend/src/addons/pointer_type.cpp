/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/backend/addons/pointer_type.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* PointerTypeHandler(const Converter& converter, const core::TypePtr& type) {
			// only interested in pointers
			if(!core::lang::isPointer(type)) return nullptr;

			// parse the pointer type
			core::lang::PointerType ptr(type);

			// build up TypeInfo for the pointer type
			TypeManager& typeManager = converter.getTypeManager();

			// load type information of base type
			const TypeInfo& elementInfo = typeManager.getTypeInfo(ptr.getElementType());

			// create pointer type
			auto cType = c_ast::ptr(c_ast::qualify(elementInfo.lValueType, ptr.isConst(), ptr.isVolatile()));

			if(auto fun = ptr.getElementType().isa<core::FunctionTypePtr>()) {
				// function pointers must not be qualified (calls to const function pointers are ignored by GCC)
				if(fun.isPlain()) cType = c_ast::ptr(elementInfo.lValueType);
				// function pointers must be typedef'ed in case they are used in C++ in a conversion operator definition
				c_ast::IdentifierPtr funcTypeName = converter.getCNodeManager()->create(converter.getNameManager().getName(type));
				c_ast::TypeDefinitionPtr def = converter.getCNodeManager()->create<c_ast::TypeDefinition>(cType, funcTypeName);
				auto typedefCode = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), def);
				typedefCode->addDependency(elementInfo.declaration);
				return type_info_utils::createInfo(converter.getCNodeManager()->create<c_ast::NamedType>(funcTypeName), typedefCode);
			}

			// build up and return resulting type information
			return type_info_utils::createInfo(cType, elementInfo.declaration);
		}


		OperatorConverterTable getPointerTypeOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res;
			const auto& ext = manager.getLangExtension<core::lang::PointerExtension>();

			#include "insieme/backend/operator_converter_begin.inc"

			// ------------------ point from / to ref ---------------

			res[ext.getPtrToRef()] = OP_CONVERTER { return CONVERT_ARG(0); };
			res[ext.getPtrFromRef()] = OP_CONVERTER { return CONVERT_ARG(0); };
			res[ext.getPtrOfFunction()] = OP_CONVERTER { return CONVERT_ARG(0); };

			res[ext.getPtrFromArray()] = OP_CONVERTER {
				// operator type: (ref<array<'a,'s>,'c,'v>) -> ptr<'a,'c,'v>

				core::lang::ReferenceType refType(ARG(0)->getType());
				core::TypePtr elementType = refType.getElementType();

				// get array type
				core::lang::ArrayType arrT(elementType);

				// helper to add element type dependency if required
				auto convertAndAddDep = [&](const core::TypePtr& t) {
					const TypeInfo& info = GET_TYPE_INFO(t);
					context.getDependencies().insert(info.definition);
					return CONVERT_TYPE(t);
				};

				// handle array new undefined
				if(LANG_EXT_REF.isCallOfRefNew(ARG(0))) {
					return c_ast::newArrayCall(convertAndAddDep(arrT.getElementType()), CONVERT_EXPR(arrT.getSize().as<core::ExpressionPtr>()), nullptr);
				}
				// handle array new with init
				if(auto initExp = ARG(0).isa<core::InitExprPtr>()) {
					if(LANG_EXT_REF.isCallOfRefNew(initExp->getMemoryExpr())) {
						c_ast::InitializerPtr cInit = nullptr;
						// create init expression
						if(!initExp->getInitExprs().empty()) {
							auto initElems = ::transform(initExp->getInitExprList(),
								[&](const core::ExpressionPtr& cur) { return CONVERT_EXPR(cur).as<c_ast::NodePtr>(); });
							cInit = c_ast::init(initElems);
						}
						return c_ast::newArrayCall(convertAndAddDep(arrT.getElementType()), CONVERT_EXPR(arrT.getSize().as<core::ExpressionPtr>()), cInit);
					}
				}

				// add dependency to element type
				convertAndAddDep(elementType);

				// access source
				if(core::lang::isFixedSizedArray(elementType)) {
					// special handling for string-literals ..
					if (auto lit = ARG(0).isa<core::LiteralPtr>()) {
						if (lit->getStringValue()[0] == '"') {
							return CONVERT_ARG(0);   // the literal is already a pointer
						}
					}
					auto converted = CONVERT_ARG(0);
					// if directly nested init expression, we need to get its address
					if(ARG(0).isa<core::InitExprPtr>()) converted = c_ast::ref(converted);
					//TODO Think about a nicer solution here. This line replaces the following one to also support arrays in system defined structs
					return c_ast::cast(CONVERT_TYPE(call->getType()), converted);
					//return c_ast::access(c_ast::deref(CONVERT_ARG(0)), "data");
				}

				// otherwise references and pointers are the same
				return c_ast::deref(CONVERT_ARG(0));
			};


			// ------------------------ casts -----------------------

			auto cast = OP_CONVERTER {
				if(ARG(0)->getType() == call->getType()) return CONVERT_ARG(0);
				return c_ast::cast(CONVERT_TYPE(call->getType()), CONVERT_ARG(0));
			};

			res[ext.getPtrCast()] = cast;
			res[ext.getPtrReinterpret()] = cast;
			res[ext.getPtrConstCast()] = cast;
			res[ext.getPtrVolatileCast()] = cast;

			res[ext.getPtrFromIntegral()] = cast;
			res[ext.getPtrToIntegral()] = cast;


			// ------------------------ sub-referencing ------------------------

			res[ext.getPtrNarrow()] =           OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrExpand()] =           OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrArrayElement()] =     OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrMemberAccess()] =     OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrComponentAccess()] =  OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };
			res[ext.getPtrScalarToPtrArray()] = OP_CONVERTER { assert_not_implemented(); return c_ast::ExpressionPtr(); };

			res[ext.getPtrSubscript()] = OP_CONVERTER {
				// operator type: (ptr<'a,'c,'v>, int<8>) -> ref<'a,'c,'v>

				// add dependency to element type
				core::lang::PointerType ptrType(ARG(0)->getType());
				core::TypePtr elementType = ptrType.getElementType();
				const TypeInfo& info = GET_TYPE_INFO(elementType);
				context.getDependencies().insert(info.definition);

				// generated code &(X[Y])
				return c_ast::ref(c_ast::subscript(CONVERT_ARG(0), CONVERT_ARG(1)));
			};


			// ------------------------ de-referencing ------------------------

			res[ext.getPtrDeref()] = OP_CONVERTER { return c_ast::deref(CONVERT_ARG(0)); };


			// ------------------------ null ------------------------

			res[ext.getPtrNull()] = OP_CONVERTER { return c_ast::cast(CONVERT_TYPE(call->getType()), C_NODE_MANAGER->create<c_ast::Literal>("0")); };


			// ------------------------ comparison operators ------------------------

			res[ext.getPtrEqual()] =        OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrNotEqual()] =     OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrLessThan()] =     OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrLessEqual()] =    OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrGreaterThan()] =  OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrGreaterEqual()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };


			// ------------------------ pointer arithmetic ------------------------

			res[ext.getPtrAdd()] = OP_CONVERTER { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrSub()] = OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrDiff()] = OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
			res[ext.getPtrPostInc()] = OP_CONVERTER { return c_ast::postInc(c_ast::deref(CONVERT_ARG(0))); };
			res[ext.getPtrPostDec()] = OP_CONVERTER { return c_ast::postDec(c_ast::deref(CONVERT_ARG(0))); };
			res[ext.getPtrPreInc()] = OP_CONVERTER { return c_ast::preInc(c_ast::deref(CONVERT_ARG(0))); };
			res[ext.getPtrPreDec()] = OP_CONVERTER { return c_ast::preDec(c_ast::deref(CONVERT_ARG(0))); };


            #include "insieme/backend/operator_converter_end.inc"

			return res;
		}
	}

	void PointerType::installOn(Converter& converter) const {
		// registers type handler
		converter.getTypeManager().addTypeHandler(PointerTypeHandler);

		// register additional operators
		converter.getFunctionManager().getOperatorConverterTable().insertAll(getPointerTypeOperatorTable(converter.getNodeManager()));
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

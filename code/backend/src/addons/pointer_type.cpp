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

#include "insieme/backend/addons/pointer_type.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/operator_converter.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/annotations/c/include.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"


namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* PointerTypeHandler(ConversionContext& context, const core::TypePtr& type) {
			const auto& converter = context.getConverter();

			// only interested in pointers
			if(!core::lang::isPointer(type)) return nullptr;

			// parse the pointer type
			core::lang::PointerType ptr(type);

			// build up TypeInfo for the pointer type
			TypeManager& typeManager = converter.getTypeManager();

			// load type information of base type
			const TypeInfo& elementInfo = typeManager.getTypeInfo(context, ptr.getElementType());

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
					return c_ast::mallocCall(context, arrT.getElementType(), arrT.getSize().as<core::ExpressionPtr>());
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

					// If we have an intercepted type or ref_member_access of intercepted types, we must not access the "data" member of the insieme structs
					auto arg = ARG(0);
					if(annotations::c::hasIncludeAttached(arg)
							|| (LANG_EXT_REF.isCallOfRefMemberAccess(arg) && annotations::c::hasIncludeAttached(
									core::analysis::getReferencedType(core::analysis::getArgument(arg, 0)->getType())))) {
						return c_ast::cast(CONVERT_TYPE(call->getType()), converted);

						// every other access is a normal fixed size array and we actually should access the data member of the struct we created for it
					} else {
						return c_ast::access(c_ast::derefIfNotImplicit(converted, ARG(0)), "data");
					}
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
			res[ext.getPtrParentCast()] = cast;
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

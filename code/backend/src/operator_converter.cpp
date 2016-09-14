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

#include "insieme/backend/operator_converter.h"

#include <functional>

#include "insieme/backend/addons/cpp_casts.h"
#include "insieme/backend/c_ast/c_ast_printer.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"

#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/complex.h"
#include "insieme/core/lang/io.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/simplify.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	namespace {

		bool isPrimitiveType(const core::TypePtr& type) {
			auto& basic = type->getNodeManager().getLangBasic();
			return basic.isChar(type) || basic.isBool(type) || basic.isScalarType(type);
		}

		c_ast::ExpressionPtr getAssignmentTarget(ConversionContext& context, const core::ExpressionPtr& expr) {
			// convert expression
			c_ast::ExpressionPtr res = context.getConverter().getStmtConverter().convertExpression(context, expr);

			return c_ast::derefIfNotImplicit(res, expr);
		}

		core::ExpressionPtr inlineLazy(const core::NodePtr& lazy) {
			core::NodeManager& manager = lazy->getNodeManager();

			core::ExpressionPtr exprPtr = dynamic_pointer_cast<const core::Expression>(lazy);
			assert_true(exprPtr) << "Lazy is not an expression!";

			// use core functionality
			auto res = core::transform::evalLazy(manager, exprPtr);

			// return evaluated lazy expression
			return res;
		}

		core::ExpressionPtr wrapNarrow(const core::ExpressionPtr& root, const core::ExpressionPtr& dataPath) {
			core::NodeManager& mgr = dataPath.getNodeManager();
			auto& dpExt = mgr.getLangExtension<core::lang::DatapathExtension>();
			core::IRBuilder builder(mgr);

			// check for the terminal case
			if(dpExt.isCallOfDataPathRoot(dataPath)) { return root; }

			// checks the remaining data path
			assert_eq(dataPath->getNodeType(), core::NT_CallExpr) << "Data Path is neither root nor call: " << *dataPath;

			// resolve data path recursively
			core::CallExprPtr call = dataPath.as<core::CallExprPtr>();
			auto args = core::transform::extractArgExprsFromCall(call);
			core::ExpressionPtr res = wrapNarrow(root, args[0]);
			auto fun = call->getFunctionExpr();
			if(dpExt.isDataPathMember(fun)) {
				// access the member of the struct / union
				return builder.refMember(res, args[1].as<core::LiteralPtr>()->getStringValue());
			} else if(dpExt.isDataPathElement(fun)) {
				// access element of vector
				return builder.arrayRefElem(res, args[1]);
			} else if(dpExt.isDataPathComponent(fun)) {
				// access tuple component
				return builder.refComponent(res, args[1]);
			} else if(dpExt.isDataPathParent(fun)) {
				// cast to parent type using a static cast
				const auto& ext = mgr.getLangExtension<addons::CppCastExtension>();
				core::lang::ReferenceType refType(res);
				refType.setElementType(core::analysis::getRepresentedType(args[1]));
				return builder.callExpr(ext.getStaticCast(), res, builder.getTypeLiteral((core::GenericTypePtr)refType));
			}

			// this is not a valid data path
			LOG(ERROR) << "Invalid data path encoding: " << *dataPath << "\n";
			assert_fail() << "Unknown Data Path encoding!";
			return res;
		}

		/**
		 * Converts a root expression + data path into a C-AST construct navigating from the root to the addressed
		 * data element.
		 */
		c_ast::ExpressionPtr narrow(ConversionContext& context, const core::ExpressionPtr& root, const core::ExpressionPtr& dataPath) {
			// convert data path to access operations and use standard conversion
			return context.getConverter().getStmtConverter().convertExpression(context, wrapNarrow(root, dataPath));
		}

		/**
		 * Converts a leaf expression and a data path into a C-AST expression navigating from the leaf to the root
		 * expression.
		 */
		c_ast::ExpressionPtr expand(ConversionContext& context, const c_ast::ExpressionPtr& src, const core::ExpressionPtr& dataPath,
		                            const core::TypePtr& resType, core::TypePtr& curType) {
			auto& dpExt = dataPath.getNodeManager().getLangExtension<core::lang::DatapathExtension>();

			// expansion is not supported in C => requires manual pointer arithmetic

			// process remaining data-path components
			assert_eq(dataPath->getNodeType(), core::NT_CallExpr) << "Data Path is not a call: " << *dataPath;

			// resolve data path recursively
			core::CallExprPtr call = dataPath.as<core::CallExprPtr>();
			auto fun = call->getFunctionExpr();

			// check for the terminal case
			if(dpExt.isDataPathRoot(fun)) {
				curType = resType; // update currently processed type
				return src;        // done, no offset required
			}

			// compute offsets of remaining data path recursively
			c_ast::ExpressionPtr res = expand(context, src, call->getArgument(0), resType, curType);

			// compute access expression
			auto& cNodeManager = context.getConverter().getCNodeManager();
			if(dpExt.isDataPathMember(fun)) {
				// extract element type from current result type
				core::LiteralPtr memberName = call->getArgument(1).as<core::LiteralPtr>();

				// obtain reference to struct type
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, curType);

				// add definition of type
				context.addDependency(info.definition);

				// update current type for following steps
				curType = curType.as<core::TagTypePtr>()->getFieldType(memberName->getValue());

				// compute offset using offsetof macro
				c_ast::ExpressionPtr offset =
				    c_ast::call(cNodeManager->create("offsetof"), info.rValueType, cNodeManager->create(memberName->getStringValue()));

				return c_ast::sub(res, offset);

			} else if(dpExt.isDataPathElement(fun)) {
				// update current type for next steps
				curType = core::lang::ArrayType(curType).getElementType();

				// get type-info of element type
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, curType);

				// add dependency
				context.addDependency(info.definition);

				c_ast::ExpressionPtr offset =
				    c_ast::mul(c_ast::sizeOf(info.rValueType), context.getConverter().getStmtConverter().convertExpression(context, call->getArgument(1)));

				// subtract offset from pointer
				return c_ast::sub(res, offset);

			} else if(dpExt.isDataPathComponent(fun)) {
				// obtain reference to struct type
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, curType);

				//  --- update current type for next steps ---

				// get element type of selected component
				core::arithmetic::Formula index = core::arithmetic::toFormula(call->getArgument(1));
				assert_true(index.isInteger()) << "Non-constant tuple element access encountered!";

				// extract component index
				int componentIndex = index.getConstantValue().getNumerator();

				// extract element type from tuple
				curType = resType.as<core::TupleTypePtr>()->getElement(componentIndex);


				// --- compute offset ---

				// add definition of type
				context.addDependency(info.definition);

				// compute offset using offsetof macro
				c_ast::ExpressionPtr offset =
				    c_ast::call(cNodeManager->create("offsetof"), info.rValueType, cNodeManager->create(format("c%d", componentIndex)));

				return c_ast::sub(res, offset);

			} else {
				assert_fail() << "Unknown data path setup!";
			}

			return src;
		}
	}


	OperatorConverterTable getBasicOperatorTable(core::NodeManager& manager) {
		// TODO: distribute this among multiple addons
		const core::lang::BasicGenerator& basic = manager.getLangBasic();
		const core::lang::ArrayExtension& arrayExt = manager.getLangExtension<core::lang::ArrayExtension>();
		const core::lang::ReferenceExtension& refExt = manager.getLangExtension<core::lang::ReferenceExtension>();

		OperatorConverterTable res;

		#include "insieme/backend/operator_converter_begin.inc"

		// -- booleans --

		res[basic.getBoolLAnd()] = OP_CONVERTER { return c_ast::logicAnd(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1)))); };
		res[basic.getBoolLOr()] = OP_CONVERTER { return c_ast::logicOr(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1)))); };
		res[basic.getBoolLNot()] = OP_CONVERTER { return c_ast::logicNot(CONVERT_ARG(0)); };

		res[basic.getBoolEq()] = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getBoolNe()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };


		// -- unsigned integers --

		res[basic.getUnsignedIntAdd()] = OP_CONVERTER { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntSub()] = OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntMul()] = OP_CONVERTER { return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntDiv()] = OP_CONVERTER { return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntMod()] = OP_CONVERTER { return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getUnsignedIntAnd()] = OP_CONVERTER { return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntOr()] = OP_CONVERTER { return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntXor()] = OP_CONVERTER { return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntNot()] = OP_CONVERTER { return c_ast::bitwiseNot(CONVERT_ARG(0)); };

		res[basic.getUnsignedIntLShift()] = OP_CONVERTER { return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntRShift()] = OP_CONVERTER { return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[refExt.getGenPreInc()]  = OP_CONVERTER { return c_ast::preInc(getAssignmentTarget(context, ARG(0))); };
		res[refExt.getGenPostInc()] = OP_CONVERTER { return c_ast::postInc(getAssignmentTarget(context, ARG(0))); };
		res[refExt.getGenPreDec()]  = OP_CONVERTER { return c_ast::preDec(getAssignmentTarget(context, ARG(0))); };
		res[refExt.getGenPostDec()] = OP_CONVERTER { return c_ast::postDec(getAssignmentTarget(context, ARG(0))); };

		res[basic.getUnsignedIntEq()] = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntNe()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntGe()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntGt()] = OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntLt()] = OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getUnsignedIntLe()] = OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };

		// -- unsigned integers --

		res[basic.getSignedIntAdd()] = OP_CONVERTER { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntSub()] = OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntMul()] = OP_CONVERTER { return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntDiv()] = OP_CONVERTER { return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntMod()] = OP_CONVERTER { return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getSignedIntAnd()] = OP_CONVERTER { return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntOr()] = OP_CONVERTER { return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntXor()] = OP_CONVERTER { return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntNot()] = OP_CONVERTER { return c_ast::bitwiseNot(CONVERT_ARG(0)); };

		res[basic.getSignedIntLShift()] = OP_CONVERTER { return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntRShift()] = OP_CONVERTER { return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getSignedIntEq()] = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntNe()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntGe()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntGt()] = OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntLt()] = OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getSignedIntLe()] = OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };

		// -- casts --

		res[basic.getNumericCast()] = OP_CONVERTER { return c_ast::cast(CONVERT_RES_TYPE, CONVERT_ARG(0)); };
		res[basic.getTypeInstantiation()] = OP_CONVERTER {
			auto lit = ARG(1).as<core::LiteralPtr>();
			if(!lit) assert_fail() << "type instantiation should either be handled at call site or apply to function pointer literal";
			auto replacementLit = core::IRBuilder(NODE_MANAGER).literal(lit->getValue(), call->getType());
			core::transform::utils::migrateAnnotations(lit, replacementLit);
			auto ret = CONVERT_EXPR(replacementLit);
			// perform explicit instantiation if required
			auto explicitInstantiationList = ::transform(call->getType().as<core::FunctionTypePtr>()->getInstantiationTypeList(), [&](const core::TypePtr& t){
				return CONVERT_TYPE(t);
			});
			if(!explicitInstantiationList.empty()) ret = C_NODE_MANAGER->create<c_ast::ExplicitInstantiation>(ret, explicitInstantiationList);
			return ret;
		};

		// -- reals --

		res[basic.getRealAdd()] = OP_CONVERTER { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealSub()] = OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealMul()] = OP_CONVERTER { return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealDiv()] = OP_CONVERTER { return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getRealEq()] = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealNe()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealGe()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealGt()] = OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealLt()] = OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getRealLe()] = OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };

		// -- characters --

		res[basic.getCharEq()] = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getCharNe()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getCharGe()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getCharGt()] = OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getCharLt()] = OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getCharLe()] = OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };


		// -- references --

		res[refExt.getRefEqual()]    = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[refExt.getRefNotEqual()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };

		// -- generic --

		res[basic.getGenAdd()] = OP_CONVERTER { return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenSub()] = OP_CONVERTER { return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenMul()] = OP_CONVERTER { return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenDiv()] = OP_CONVERTER { return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getGenNot()] = OP_CONVERTER { return c_ast::bitwiseNot(CONVERT_ARG(0)); };
		res[basic.getGenAnd()] = OP_CONVERTER { return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenOr()] = OP_CONVERTER { return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenXor()] = OP_CONVERTER { return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenRShift()] = OP_CONVERTER { return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenLShift()] = OP_CONVERTER { return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getGenEq()] = OP_CONVERTER { return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenNe()] = OP_CONVERTER { return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenGe()] = OP_CONVERTER { return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenGt()] = OP_CONVERTER { return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenLt()] = OP_CONVERTER { return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); };
		res[basic.getGenLe()] = OP_CONVERTER { return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); };

		// -- undefined --

		res[basic.getZero()] = OP_CONVERTER {
			auto type = call->getType();
			// special case: intercepted object
			if(type.isa<core::GenericTypePtr>()) {
				// write a string witch is the whole type
				auto cType = CONVERT_TYPE(type);
				return c_ast::call(cType);
			}
			return nullptr;
		};

		res[basic.getNumTypeToInt()] = OP_CONVERTER {
			auto type = call->getType().isa<core::GenericTypePtr>();
			assert_true(type) << "Invalid result type: " << *type;
			return C_NODE_MANAGER->create<c_ast::Literal>(toString(*type->getTypeParameter(0)));
		};

		res[basic.getUnitConsume()] = OP_CONVERTER { return c_ast::cast(CONVERT_TYPE(LANG_BASIC.getUnit()), CONVERT_ARG(0)); };

		// -- references --

		res[refExt.getRefDeref()] = OP_CONVERTER {

			// check type
			assert_pred1(core::lang::isReference, ARG(0)->getType());

			// special handling of derefing result of ref.new or ref.var => bogus
			core::ExpressionPtr arg = ARG(0);
			if(core::analysis::isCallOf(arg, LANG_EXT_REF.getRefTempInit()) || core::analysis::isCallOf(arg, LANG_EXT_REF.getRefNewInit())) {
				// skip ref.var / ref.new => stupid
				core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(arg);
				return CONVERT_EXPR(call->getArgument(0));
			}

			// extract resulting type
			const core::TypePtr elementType = core::analysis::getReferencedType(ARG(0)->getType());
			assert_true(elementType) << "Unable to extract element type from: " << ARG(0)->getType();
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, elementType);
			context.getDependencies().insert(info.definition);

			// special handling for string literals
			if(ARG(0)->getNodeType() == core::NT_Literal) {
				core::LiteralPtr literal = ARG(0).as<core::LiteralPtr>();
				if(literal->getStringValue()[0] == '\"') {
					// the cast to a vector element is implemented at this point
					// instead of the actual literal conversion to save unnecessary casts when
					// strings are forwarded to external functions (printf) directly
					return c_ast::deref(c_ast::cast(c_ast::ptr(info.rValueType), CONVERT_ARG(0)));
				}
			}

			// deref of an assigment, do not
			if(core::analysis::isCallOf(ARG(0), LANG_EXT_REF.getRefAssign())) { return CONVERT_ARG(0); }

			// deref of init expression is implicit in C/++
			if(ARG(0).isa<core::InitExprPtr>()) {
				return CONVERT_ARG(0);
			}

			return c_ast::derefIfNotImplicit(CONVERT_ARG(0), ARG(0));
		};

		res[refExt.getRefAssign()] = OP_CONVERTER {

			// extract type
			core::ExpressionPtr initValue = call->getArgument(0);
			core::TypePtr type = initValue->getType();
			const TypeInfo& valueTypeInfo = GET_TYPE_INFO(type);

			// fix dependency
			context.getDependencies().insert(valueTypeInfo.definition);


			return c_ast::assign(getAssignmentTarget(context, ARG(0)), CONVERT_ARG(1));
		};

		res[refExt.getRefTemp()] = OP_CONVERTER {
			ADD_HEADER("alloca.h"); // for 'alloca'
			core::GenericTypePtr resType = call->getType().as<core::GenericTypePtr>();
			c_ast::ExpressionPtr size = c_ast::sizeOf(CONVERT_TYPE(core::analysis::getReferencedType(resType)));
			auto cType = CONVERT_TYPE(resType);
			auto allocaNode = c_ast::call(C_NODE_MANAGER->create("alloca"), size);
			return c_ast::cast(cType, allocaNode);
		};

		res[refExt.getRefDecl()] = OP_CONVERTER {
			return C_NODE_MANAGER->create<c_ast::Literal>("ref_decl_node__should_not_appear_in_final_code");
		};

		res[refExt.getRefTempInit()] = OP_CONVERTER {

			// extract type
			core::ExpressionPtr initValue = call->getArgument(0);
			core::TypePtr type = initValue->getType();
			const TypeInfo& valueTypeInfo = GET_TYPE_INFO(type);

			// fix dependency
			context.getDependencies().insert(valueTypeInfo.definition);

			// special handling for arrays
			if(core::lang::isArray(type)) {
				// no out allocation required!
				return c_ast::ref(CONVERT_EXPR(initValue));
			}

			auto res = CONVERT_EXPR(initValue);
			if(res->getNodeType() == c_ast::NT_Initializer) { return c_ast::ref(res); }

			// moderm C++ does not allow the usage of the syntax (int[1]{x})
			// the value of a class instance is implicitly materialized to
			// be used as reference parameter in function calls,
			// lets try just to write the expression and let the backend compiler
			// materialize it
			// 	Classes && generic types (which are intercepted objects)
			if(type.isa<core::TagTypePtr>() || (type.isa<core::GenericTypePtr>() && !isPrimitiveType(type))) { return c_ast::ref(res); }

			// creates a something of the format "(int[1]){x}"
			return c_ast::init(c_ast::vec(valueTypeInfo.rValueType, 1), res);
		};

		res[refExt.getRefNew()] = OP_CONVERTER {
			return c_ast::mallocCall(context, core::analysis::getReferencedType(call->getType()));
		};

		res[refExt.getRefNewInit()] = OP_CONVERTER {

			// get result type information
			core::GenericTypePtr resType = call->getType().as<core::GenericTypePtr>();
			const RefTypeInfo& info = context.getConverter().getTypeManager().getRefTypeInfo(context, resType);

			// extract type
			core::ExpressionPtr initValue = call->getArgument(0);
			core::TypePtr type = initValue->getType();
			const TypeInfo& valueTypeInfo = GET_TYPE_INFO(type);

			// fix dependency
			context.getDependencies().insert(valueTypeInfo.definition);

			// special handling for variable sized structs
			if(auto structType = core::analysis::isStruct(initValue->getType())) {
				if(core::lang::isUnknownSizedArray(structType->getFields().back()->getType())) {
					// Create code similar to this:
					// 		(A*)memcpy(malloc(sizeof(A) + sizeof(float) * v2), &(struct A){ v2 }, sizeof(A))

					__insieme_unused auto& arrayExt = LANG_EXT(core::lang::ArrayExtension);

					assert_eq(ARG(0)->getNodeType(), core::NT_InitExpr) << "Only supporting struct expressions as initializer value so far!";
					core::InitExprPtr initValue = ARG(0).as<core::InitExprPtr>();

					// get types of struct and element
					auto structType = core::analysis::getReferencedType(initValue->getType());

					// get size of variable part
					auto arrayInitValue = initValue->getInitExprList().back();
					// FIXME
					assert_not_implemented() << "Variable sized struct unimplemented";
//					assert_true(core::analysis::isCallOf(arrayInitValue, arrayExt.getArrayCreate())) << "Array not properly initialized!";
//					auto size = arrayInitValue.as<core::CallExprPtr>()->getArgument(1);
//
//					// add header dependencies
//					ADD_HEADER("stdlib.h"); // for 'malloc'
//					ADD_HEADER("string.h"); // for 'memcpy'
//
//					auto c_struct_type = CONVERT_TYPE(structType);
//					auto c_element_type = CONVERT_TYPE(core::lang::ArrayType(arrayInitValue).getElementType());
//
//					// build call
//					auto malloc = c_ast::call(C_NODE_MANAGER->create("malloc"),
//											  c_ast::add(c_ast::sizeOf(c_struct_type), c_ast::mul(c_ast::sizeOf(c_element_type), CONVERT_EXPR(size))));
//					return c_ast::cast(CONVERT_TYPE(resType),
//									   c_ast::call(C_NODE_MANAGER->create("memcpy"), malloc, c_ast::ref(CONVERT_EXPR(initValue)), c_ast::sizeOf(c_struct_type)));
				}
			}

			// use a call to the ref_new operator of the ref type
			context.getDependencies().insert(info.newOperator);
			return c_ast::call(info.newOperatorName, CONVERT_ARG(0));

		};

		res[refExt.getRefDelete()] = OP_CONVERTER {

			// do not free non-heap variables
			if(ARG(0)->getNodeType() == core::NT_Variable) {
				core::VariablePtr var = static_pointer_cast<const core::Variable>(ARG(0));
				if(GET_VAR_INFO(var).location != VariableInfo::INDIRECT) {
					// return NULL pointer => no op
					return c_ast::ExpressionPtr();
				}
			}

			// ensure correct type
			assert_true(core::analysis::hasRefType(ARG(0))) << "Cannot free a non-ref type!";

			// handle destructor call
			if(core::CallExprPtr dtorCall = ARG(0).isa<core::CallExprPtr>()) {
				if(dtorCall->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->isDestructor()) {
					return c_ast::deleteCall(CONVERT_EXPR(core::transform::extractInitExprFromDecl(dtorCall[0])));
				}
			}

			// handle array delete
			if(core::lang::isArray(core::analysis::getReferencedType(ARG(0)->getType()))) {
				assert_true(LANG_EXT_PTR.isCallOfPtrToArray(ARG(0)));
				return c_ast::deleteArrayCall(CONVERT_EXPR(core::analysis::getArgument(ARG(0), 0)));
			}

			// everything else will be free'd with a call to stdlib's free function
			return c_ast::freeCall(context, CONVERT_ARG(0));
		};

		res[refExt.getRefReinterpret()] = OP_CONVERTER {
			c_ast::TypePtr type;

			if(core::analysis::getReferencedType(call->getType()).isa<core::FunctionTypePtr>()) {
				// function pointers
				type = CONVERT_TYPE(core::analysis::getReferencedType(call->getType()));
			} else {
				type = CONVERT_TYPE(call->getType());
			}

			c_ast::ExpressionPtr value = GET_TYPE_INFO(ARG(0)->getType()).externalize(C_NODE_MANAGER, CONVERT_ARG(0));
			return GET_TYPE_INFO(call->getType()).internalize(C_NODE_MANAGER, c_ast::cast(type, value));
		};

		res[refExt.getRefCast()] = OP_CONVERTER {

			// in C, this should always be implicit
			auto in = CONVERT_ARG(0);
			if (*call->getType() == *ARG(0)->getType()) {
				return in;
			}

			// parse source and target types
			auto src = core::lang::ReferenceType(ARG(0)->getType());
			auto trg = core::lang::ReferenceType(call->getType());

			// if it is a plain to cpp reference cast => deref source
			if(src.isPlain() && !trg.isPlain()) {
				in = c_ast::deref(in);
			}

			// if it is a cpp reference to plain cast => ref source
			if(!src.isPlain() && trg.isPlain()) {
				in = c_ast::ref(in);
			}

			auto res_type = GET_TYPE_INFO(call->getType()).rValueType;
			return c_ast::cast(res_type, in);
		};

		res[refExt.getRefKindCast()] = OP_CONVERTER {
			// get source and target reference kinds
			auto srcRefKind = core::lang::getReferenceKind(ARG(0));
			auto trgRefKind = core::lang::getReferenceKind(ARG(1));

			if(ARG(0).isa<core::InitExprPtr>()) return CONVERT_ARG(0);

			if(srcRefKind == core::lang::ReferenceType::Kind::Plain) {
				if(trgRefKind == core::lang::ReferenceType::Kind::CppReference || trgRefKind == core::lang::ReferenceType::Kind::CppRValueReference) {
					// special handling for string literal arguments
					if(auto lit = ARG(0).isa<core::LiteralPtr>()) {
						if(lit->getStringValue()[0] == '"') {
							return c_ast::deref(c_ast::cast(CONVERT_TYPE(lit->getType()), CONVERT_ARG(0)));
						}
					}
					return c_ast::deref(CONVERT_ARG(0));
				}
			}

			if(srcRefKind == core::lang::ReferenceType::Kind::CppReference || trgRefKind == core::lang::ReferenceType::Kind::CppRValueReference) {
				if(trgRefKind == core::lang::ReferenceType::Kind::Plain) {
					return c_ast::ref(CONVERT_ARG(0));
				}
			}

			return CONVERT_ARG(0);
		};

		res[refExt.getRefParentCast()] = OP_CONVERTER {
			c_ast::TypePtr type = CONVERT_TYPE(call->getType());
			return c_ast::cast(type, CONVERT_ARG(0));
		};

		// -- support narrow and expand --

		res[refExt.getRefNarrow()] = OP_CONVERTER {
			// narrow starting position step by step
			return narrow(context, ARG(0), ARG(1));
		};

		res[refExt.getRefExpand()] = OP_CONVERTER {
			ADD_HEADER("stddef.h") // for 'offsetof';

			auto charPtrType = c_ast::ptr(C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Char));

			// navigate up by re-computing pionter address
			auto res = CONVERT_ARG(0); // get address of nested

			// cast to char* (to match byte order)
			res = c_ast::cast(charPtrType, res);

			// subtract offsets
			core::TypePtr curType;
			res = expand(context, res, ARG(1), core::lang::ReferenceType(call).getElementType(), curType);

			// cast to target type and return result
			return c_ast::cast(CONVERT_TYPE(call->getType()), res);
		};

		res[refExt.getRefNull()] = OP_CONVERTER {
			// just cast the 0 constant to the proper pointer type
			return c_ast::cast(CONVERT_TYPE(call->getType()), C_NODE_MANAGER->create<c_ast::Literal>("0"));
		};

		// -- arrays --

		res[arrayExt.getArraySubscript()] = OP_CONVERTER {
			// skip deref => included subscript operator
			c_ast::ExpressionPtr src = CONVERT_ARG(0);

			// access vector struct if necessary
			if (core::lang::isFixedSizedArray(ARG(0))) {
				src = c_ast::access(src, "data");
			}

			// access field
			return c_ast::subscript(src, CONVERT_ARG(1));
		};

		res[refExt.getRefArrayElement()] = OP_CONVERTER {
			// add dependency to element type
			core::TypePtr arrayType = core::analysis::getReferencedType(ARG(0)->getType());
			core::TypePtr elementType = core::lang::ArrayType(arrayType).getElementType();
			const TypeInfo& info = GET_TYPE_INFO(elementType);
			context.getDependencies().insert(info.definition);

			// access source
			auto src = CONVERT_ARG(0);
			if (core::lang::isFixedSizedArray(arrayType)) {
				src = c_ast::access(c_ast::derefIfNotImplicit(src, ARG(0)), "data");
			}

			// generated code &(X[Y])
			return c_ast::ref(c_ast::subscript(src, CONVERT_ARG(1)));
		};

		res[refExt.getRefScalarToRefArray()] = OP_CONVERTER {
			// initialize an array instance
			//   Operator Type: (ref<'a>) -> ref<array<'a,1>>
			// => requires no special treatment
			return CONVERT_ARG(0);
		};


		// -- structs --

		res[basic.getCompositeMemberAccess()] = OP_CONVERTER {
			// signature of operation:
			//		('a, identifier, type<'b>) -> 'b

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr structType = ARG(0)->getType();
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, structType);
			context.getDependencies().insert(info.definition);

			// create member access
			assert_eq(ARG(1)->getNodeType(), core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(static_pointer_cast<const core::Literal>(ARG(1))->getStringValue());
			return c_ast::access(CONVERT_ARG(0), field);
		};

		res[refExt.getRefMemberAccess()] = OP_CONVERTER {
			// signature of operation:
			//		(ref<'a>, identifier, type<'b>) -> ref<'b>

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr structType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, structType);
			context.getDependencies().insert(info.definition);

			assert_eq(ARG(1)->getNodeType(), core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(static_pointer_cast<const core::Literal>(ARG(1))->getStringValue());

			auto converted = CONVERT_ARG(0);
			auto thisObject = c_ast::derefIfNotImplicit(converted, ARG(0));
			auto access = c_ast::access(thisObject, field);

			// handle inner initExpr
			if(ARG(0).isa<core::InitExprPtr>()) {
				access = c_ast::access(converted, field);
			}

			// handle implicit C array / pointer duality
			if(core::lang::isVariableSizedArray(core::analysis::getReferencedType(call->getType()))) { return access; }

			// access the type
			return c_ast::ref(access);
		};

		// -- tuples --

		res[basic.getTupleMemberAccess()] = OP_CONVERTER {
			// signature of operation:
			//		('a, uint<8>, type<'b>) -> 'b

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr tupleType = ARG(0)->getType();
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, tupleType);
			context.getDependencies().insert(info.definition);

			// create member access
			core::ExpressionPtr index = ARG(1);
			while(index->getNodeType() == core::NT_CastExpr) {
				index = static_pointer_cast<const core::CastExpr>(index)->getSubExpression();
			}
			assert_eq(index->getNodeType(), core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(string("c") + static_pointer_cast<const core::Literal>(index)->getStringValue());
			return c_ast::access(CONVERT_ARG(0), field);
		};

		res[refExt.getRefComponentAccess()] = OP_CONVERTER {
			// signature of operation:
			//		(ref<'a>, uint<8>, type<'b>) -> ref<'b>

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr tupleType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, tupleType);
			context.getDependencies().insert(info.definition);

			core::ExpressionPtr index = ARG(1);
			while(index->getNodeType() == core::NT_CastExpr) {
				index = static_pointer_cast<const core::CastExpr>(index)->getSubExpression();
			}
			assert_eq(index->getNodeType(), core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(string("c") + static_pointer_cast<const core::Literal>(index)->getStringValue());

			// access the type
			return c_ast::ref(c_ast::access(c_ast::derefIfNotImplicit(CONVERT_ARG(0), ARG(0)), field));
		};

		// -- others --

		res[basic.getId()] = OP_CONVERTER {
			return CONVERT_ARG(0); // simply forward input argument
		};

		res[basic.getIfThenElse()] = OP_CONVERTER {
			// IF-THEN-ELSE literal: (bool, () -> 'b, () -> 'b) -> 'b")
			return c_ast::ite(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1))), CONVERT_EXPR(inlineLazy(ARG(2))));
		};

		res[basic.getSizeof()] = OP_CONVERTER {
			// extract type sizeof is applied to
			core::GenericTypePtr type = dynamic_pointer_cast<const core::GenericType>(ARG(0)->getType());
			assert_true(type) << "Illegal argument to sizeof operator";
			core::TypePtr target = type->getTypeParameter()[0];

			// return size-of operator call
			return c_ast::sizeOf(CONVERT_TYPE(target));
		};

		res[basic.getSelect()] = OP_CONVERTER {
			//  Implements the select operation
			//   Operator Type: ( comp(arg0, arg1) ? arg0 : arg1)

			core::IRBuilder builder(ARG(0)->getNodeManager());
			return c_ast::ite(CONVERT_EXPR(builder.callExpr(ARG(2), ARG(0), ARG(1))), CONVERT_ARG(0), CONVERT_ARG(1));
		};

		res[basic.getCloogFloor()] = OP_CONVERTER {
			//			ADD_HEADER_FOR("floor");
			//			auto floatType = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Float);
			//			return c_ast::call( C_NODE_MANAGER->create("floor"),
			//					c_ast::div(
			//						c_ast::cast(floatType, CONVERT_ARG(0)),
			//						c_ast::cast(floatType, CONVERT_ARG(1))
			//					)
			//				);

			auto intType = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UInt8);

			// ((a*b>0)?(a/b):(-(-a/b+(-a%b!=0))))
			auto a = CONVERT_ARG(0);
			auto na = c_ast::minus(a);
			auto b = CONVERT_ARG(1);
			auto z = c_ast::lit(intType, "0");

			return c_ast::ite(c_ast::gt(c_ast::mul(a, b), z), c_ast::div(a, b), c_ast::minus(c_ast::add(c_ast::div(na, b), c_ast::ne(c_ast::mod(na, b), z))));

		};

		res[basic.getCloogCeil()] = OP_CONVERTER {
			//			ADD_HEADER_FOR("ceil");
			//			auto floatType = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Float);
			//			return c_ast::call( C_NODE_MANAGER->create("ceil"),
			//					c_ast::div(
			//						c_ast::cast(floatType, CONVERT_ARG(0)),
			//						c_ast::cast(floatType, CONVERT_ARG(1))
			//					)
			//				);

			auto intType = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UInt8);

			// ((a*b>0)?(a/b + (a%b!=0)):(-(-a/b)))
			auto a = CONVERT_ARG(0);
			auto b = CONVERT_ARG(1);
			auto z = c_ast::lit(intType, "0");

			return c_ast::ite(c_ast::gt(c_ast::mul(a, b), z), c_ast::add(c_ast::div(a, b), c_ast::ne(c_ast::mod(a, b), z)),
			                  c_ast::minus(c_ast::div(c_ast::minus(a), b)));
		};

		res[basic.getCloogMod()] = OP_CONVERTER { return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); };

		res[basic.getExit()] = OP_CONVERTER {
			ADD_HEADER("stdlib.h"); // for 'exit'
			return c_ast::call(C_NODE_MANAGER->create("exit"), CONVERT_ARG(0));
		};

		res[basic.getGenInit()] = OP_CONVERTER {
			// this is a blind initialization, used to initialize intercepted objects

			// we need a little introspection over whatever we find inside, it might be that we find another expression list.
			// the last thing we want to do is to cast those into any type (clasical compound initializer: (type){...} )
			// since we know NOTHING about inner types, we'll create a compound without
			auto tupleExpr = ARG(1).as<core::TupleExprPtr>();

			vector<c_ast::NodePtr> v;
			for(auto cur : tupleExpr->getExpressions()) {
				v.push_back(CONVERT_EXPR(cur));
			}

			return C_NODE_MANAGER->create<c_ast::Initializer>(CONVERT_TYPE(call->getType()), v);
		};

		// ----------------------------  Attribute extension --------------------------

		auto& attrExt = manager.getLangExtension<core::analysis::AttributeExtension>();
		res[attrExt.getAttr()] = OP_CONVERTER {
			// just skip this attribute
			return CONVERT_ARG(0);
		};


		// ---------------------------- IR++ / C++ --------------------------


//		{
//			// core C++ extensions
//
//			const auto& irppExt = manager.getLangExtension<core::lang::IRppExtensions>();
//
//			res[irppExt.getArrayCtor()] = OP_CONVERTER {
//
//				// init array using a vector expression
//				auto objType = ARG(1)->getType().as<core::FunctionTypePtr>()->getObjectType();
//				auto type = CONVERT_TYPE(objType);
//				auto size = CONVERT_ARG(2);
//				c_ast::ExpressionPtr res = c_ast::initArray(type, size);
//
//				// add dependency to class declaration
//				context.addDependency(GET_TYPE_INFO(objType).declaration);
//
//				// convert default constructor
//				auto ctor = CONVERT_ARG(1);
//
//				// add new if required
//				const auto& basic = LANG_BASIC;
//				core::IRBuilder builder(NODE_MANAGER);
//				auto normArg0 = builder.normalize(ARG(0));
//				if(basic.isRefVar(normArg0)) {
//					// nothing to do
//				} else if(basic.isRefNew(normArg0)) {
//					res = c_ast::newCall(res);
//				} else {
//					assert_fail() << "Creating Arrays of objects neither on heap nor stack isn't supported!";
//				}
//				return res;
//			};
//
//			res[irppExt.getVectorCtor()] = OP_CONVERTER {
//
//				// init vector using a vector expression
//				auto objType = ARG(1)->getType().as<core::FunctionTypePtr>()->getObjectType();
//				auto type = CONVERT_TYPE(objType);
//				auto size = CONVERT_ARG(2);
//				c_ast::ExpressionPtr res = c_ast::initArray(type, size);
//
//				// add dependency to class declaration
//				context.addDependency(GET_TYPE_INFO(objType).declaration);
//
//				// convert default constructor
//				auto ctor = CONVERT_ARG(1);
//
//				// add new if required
//				const auto& basic = LANG_BASIC;
//				if(basic.isRefVar(ARG(0))) {
//					// nothing to do
//				} else if(basic.isRefNew(ARG(0))) {
//					res = c_ast::newCall(res);
//				} else {
//					assert_fail() << "Creating Arrays of objects neither on heap nor stack isn't supported!";
//				}
//				return res;
//			};
//
//			res[irppExt.getVectorCtor2D()] = OP_CONVERTER {
//
//				// init vector using a vector expression
//				auto objType = ARG(1)->getType().as<core::FunctionTypePtr>()->getObjectType();
//				auto type = CONVERT_TYPE(objType);
//				auto sizeA = CONVERT_ARG(2);
//				auto sizeB = CONVERT_ARG(3);
//				c_ast::ExpressionPtr res = c_ast::initArray(type, sizeB, sizeA);
//
//				// add dependency to class declaration
//				context.addDependency(GET_TYPE_INFO(objType).declaration);
//
//				// convert default constructor
//				auto ctor = CONVERT_ARG(1);
//
//				// add new if required
//				const auto& basic = LANG_BASIC;
//				if(refExt.isRefVar(ARG(0))) {
//					// nothing to do
//				} else if(refExt.isRefNew(ARG(0))) {
//					res = c_ast::newCall(res);
//				} else {
//					assert_fail() << "Creating Arrays of objects neither on heap nor stack isn't supported!";
//				}
//				return res;
//			};
//
//			res[irppExt.getArrayDtor()] = OP_CONVERTER {
//				// create a node representing a delete operation
//				return c_ast::deleteArrayCall(CONVERT_ARG(0));
//			};
//		}

//		{
//			// backend C++ extensions
//
//			const auto& irppExt = manager.getLangExtension<core::lang::IRppExtensions>();
//
//			res[irppExt.getStaticCast()] = OP_CONVERTER {
//				// build up a static cast operator
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				if(!targetTy.isa<core::ArrayTypePtr>() && !core::analysis::isCppRef(targetTy)) { targetCType = c_ast::ptr(targetCType); }
//
//				// cast needs full definition of target type to be done
//				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, targetTy);
//				context.addDependency(info.definition);
//
//				// and the definition of the source type (to retrieve sub-type relations)
//				const TypeInfo& srcTypeInfo = context.getConverter().getTypeManager().getTypeInfo(context, ARG(0)->getType().as<core::RefTypePtr>()->getElementType());
//				context.addDependency(srcTypeInfo.definition);
//
//				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getStaticCastRefCppToRefCpp()] = OP_CONVERTER {
//				// build up a static cast operator for cpp_ref to cpp_ref
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				assert_true(core::analysis::isCppRef(targetTy)) << "targetType not a reference type";
//				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getStaticCastConstCppToConstCpp()] = OP_CONVERTER {
//				// build up a static cast operator for const_cpp_ref to const_cpp_ref
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				assert_true(core::analysis::isConstCppRef(targetTy)) << "targetType not a reference type";
//				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
//			};
//			res[irppExt.getStaticCastRefCppToConstCpp()] = OP_CONVERTER {
//				// build up a static cast operator for cpp_ref to const_cpp_ref
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				assert_true(core::analysis::isConstCppRef(targetTy)) << "targetType not a reference type";
//				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getDynamicCast()] = OP_CONVERTER {
//				// build up a dynamic cast operator
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				if(!targetTy.isa<core::ArrayTypePtr>() && !core::analysis::isCppRef(targetTy)) { targetCType = c_ast::ptr(targetCType); }
//
//				// cast needs full definition of target type to be done
//				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, targetTy);
//				context.addDependency(info.definition);
//
//				// and the definition of the source type (to retrieve sub-type relations)
//				const TypeInfo& srcTypeInfo = context.getConverter().getTypeManager().getTypeInfo(context, ARG(0)->getType().as<core::RefTypePtr>()->getElementType());
//				context.addDependency(srcTypeInfo.definition);
//
//				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getDynamicCastRefCppToRefCpp()] = OP_CONVERTER {
//				// build up a dynamic cast operator for cpp_ref to cpp_ref
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				assert_true(core::analysis::isCppRef(targetTy)) << "targetType not a reference type";
//				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getDynamicCastConstCppToConstCpp()] = OP_CONVERTER {
//				// build up a dynamic cast operator for const_cpp_ref to const_cpp_ref
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				assert_true(core::analysis::isConstCppRef(targetTy)) << "targetType not a reference type";
//				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getDynamicCastRefCppToConstCpp()] = OP_CONVERTER {
//				// build up a dynamic cast operator for cpp_ref to const_cpp_ref
//
//				auto targetTy = core::analysis::getRepresentedType(ARG(1));
//				auto targetCType = CONVERT_TYPE(targetTy);
//
//				assert_true(core::analysis::isConstCppRef(targetTy)) << "targetType not a reference type";
//				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
//			};
//
//			res[irppExt.getTypeid()] = OP_CONVERTER {
//				// extract typeinfo
//				core::GenericTypePtr type = dynamic_pointer_cast<const core::GenericType>(ARG(0)->getType());
//				if(type) {
//					core::TypePtr target = type->getTypeParameter()[0];
//					return c_ast::typeId(CONVERT_TYPE(target));
//				} else {
//					return c_ast::typeId(c_ast::deref(CONVERT_ARG(0)));
//				}
//			};
//
//			res[irppExt.getAlignof()] = OP_CONVERTER {
//				c_ast::CallPtr res = c_ast::call(C_NODE_MANAGER->create("__alignof__"));
//				res->arguments.push_back(CONVERT_TYPE(core::analysis::getRepresentedType(ARG(0))));
//				return res;
//			};
//
//			res[irppExt.getMemberPointerCheck()] = OP_CONVERTER { return CONVERT_ARG(0); };
//		}

		#include "insieme/backend/operator_converter_end.inc"

		// table complete => return table
		return res;
	}


} // end namespace backend
} // end namespace insieme

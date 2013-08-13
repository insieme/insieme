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

#include "insieme/backend/operator_converter.h"

#include <functional>

#include "insieme/backend/converter.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/ir_extensions.h"

#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/types/variable_sized_struct_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	namespace {


		c_ast::ExpressionPtr getAssignmentTarget(ConversionContext& context, const core::ExpressionPtr& expr) {

			// convert expression
			c_ast::ExpressionPtr res = context.getConverter().getStmtConverter().convertExpression(context, expr);

			// a deref is required (implicit in C)
			return c_ast::deref(res);
		}


		core::ExpressionPtr inlineLazy(const core::NodePtr& lazy) {
			core::NodeManager& manager = lazy->getNodeManager();

			core::ExpressionPtr exprPtr = dynamic_pointer_cast<const core::Expression>(lazy);
			assert(exprPtr && "Lazy is not an expression!");

			// use core functionality
			auto res = core::transform::evalLazy(manager, exprPtr);

			// simplify evaluated lazy expression
			return core::transform::simplify(manager, res);
		}

		core::ExpressionPtr wrapNarrow(const core::ExpressionPtr& root, const core::ExpressionPtr& dataPath) {
			core::NodeManager& mgr = dataPath.getNodeManager();
			auto& basic = mgr.getLangBasic();
			core::IRBuilder builder(mgr);

			// check for the terminal case
			if (basic.isDataPathRoot(dataPath)) {
				return root;
			}

			// checks the remaining data path
			assert(dataPath->getNodeType() == core::NT_CallExpr && "Data Path is neither root nor call!");

			// resolve data path recursively
			core::CallExprPtr call = dataPath.as<core::CallExprPtr>();
			core::ExpressionPtr res = wrapNarrow(root, call->getArgument(0));
			auto fun = call->getFunctionExpr();
			if (basic.isDataPathMember(fun)) {
				// access the member of the struct / union
				return builder.refMember(res, call->getArgument(1).as<core::LiteralPtr>()->getStringValue());
			} else if (basic.isDataPathElement(fun)) {
				// access element of vector
				return builder.arrayRefElem(res, call->getArgument(1));
			} else if (basic.isDataPathComponent(fun)) {
				// access tuple component
				return builder.refComponent(res, call->getArgument(1));
			} else if (basic.isDataPathParent(fun)) {
				// cast to parent type using a static cast
				const auto& ext = mgr.getLangExtension<core::lang::IRppExtensions>();
				return builder.callExpr(ext.getStaticCast(), res, call->getArgument(1));
			}

			// this is not a valid data path
			LOG(ERROR) << "Invalid data path encoding: " << *dataPath << "\n";
			assert(false && "Unknown Data Path encoding!");
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
		c_ast::ExpressionPtr expand(ConversionContext& context, const c_ast::ExpressionPtr& src, const core::ExpressionPtr& dataPath, const core::TypePtr& resType, core::TypePtr& curType) {
			auto& basic = dataPath.getNodeManager().getLangBasic();

			// expansion is not supported in C => requires manual pointer arithmetic

			// check for the terminal case
			if (basic.isDataPathRoot(dataPath)) {
				curType = resType;	// update currently processed type
				return src;		// done, no offset required
			}

			// process remaining data-path components
			assert(dataPath->getNodeType() == core::NT_CallExpr && "Data Path is neither root nor call!");

			// resolve data path recursively
			core::CallExprPtr call = dataPath.as<core::CallExprPtr>();
			auto fun = call->getFunctionExpr();


			// compute offsets of remaining data path recursively
			c_ast::ExpressionPtr res = expand(context, src, call->getArgument(0), resType, curType);

			// compute access expression
			auto& cNodeManager = context.getConverter().getCNodeManager();
			if (basic.isDataPathMember(fun)) {

				// extract element type from current result type
				core::LiteralPtr memberName = call->getArgument(1).as<core::LiteralPtr>();

				// obtain reference to struct type
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(curType);

				// add definition of type
				context.addDependency(info.definition);

				// update current type for following steps
				curType = curType.as<core::NamedCompositeTypePtr>()->getNamedTypeEntryOf(memberName->getValue())->getType();

				// compute offset using offsetof macro
				c_ast::ExpressionPtr offset = c_ast::call(
						cNodeManager->create("offsetof"),
						info.rValueType,
						cNodeManager->create(memberName->getStringValue())
				);

				return c_ast::sub(res, offset);

			} else if (basic.isDataPathElement(fun)) {

				// update current type for next steps
				curType = curType.getChild(0).as<core::TypePtr>();

				// get type-info of element type
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(curType);

				// add dependency
				context.addDependency(info.definition);

				c_ast::ExpressionPtr offset = c_ast::mul(
						c_ast::sizeOf(info.rValueType),
						context.getConverter().getStmtConverter().convertExpression(context, call->getArgument(1))
				);

				// subtract offset from pointer
				return c_ast::sub(res, offset);

			} else if (basic.isDataPathComponent(fun)) {

				// obtain reference to struct type
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(curType);

				//  --- update current type for next steps ---

				// get element type of selected component
				core::arithmetic::Formula index = core::arithmetic::toFormula(call->getArgument(1));
				assert(index.isInteger() && "Non-constant tuple element access encountered!");

				// extract component index
				int componentIndex = index.getConstantValue().getNumerator();

				// extract element type from tuple
				curType = resType.as<core::TupleTypePtr>()->getElement(componentIndex);


				// --- compute offset ---

				// add definition of type
				context.addDependency(info.definition);

				// compute offset using offsetof macro
				c_ast::ExpressionPtr offset = c_ast::call(
						cNodeManager->create("offsetof"),
						info.rValueType,
						cNodeManager->create(format("c%d", componentIndex))
				);

				return c_ast::sub(res, offset);

			} else {
				assert(false && "Unknown data path setup!");
			}

			return src;
		}
	}


	OperatorConverterTable getBasicOperatorTable(core::NodeManager& manager) {
		const core::lang::BasicGenerator& basic = manager.getLangBasic();

		OperatorConverterTable res;

		#include "insieme/backend/operator_converter_begin.inc"

		// -- booleans --
		res[basic.getBoolLAnd()] = OP_CONVERTER({ return c_ast::logicAnd(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1)))); });
		res[basic.getBoolLOr()]  = OP_CONVERTER({ return c_ast::logicOr(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1)))); });
		res[basic.getBoolLNot()] = OP_CONVERTER({ return c_ast::logicNot(CONVERT_ARG(0)); });

		res[basic.getBoolEq()]   = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getBoolNe()]   = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });

		//moved whit the new cast operations
		//res[basic.getBoolToInt()] = OP_CONVERTER({ return CONVERT_ARG(0); });


		// -- unsigned integers --

		res[basic.getUnsignedIntAdd()] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntSub()] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntMul()] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntDiv()] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntMod()] = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getUnsignedIntAnd()] = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntOr()] =  OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntXor()] = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntNot()] = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });

		res[basic.getUnsignedIntLShift()] = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntRShift()] = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getGenPreInc()]  = OP_CONVERTER({ return c_ast::preInc(getAssignmentTarget(context, ARG(0))); });
		res[basic.getGenPostInc()] = OP_CONVERTER({ return c_ast::postInc(getAssignmentTarget(context, ARG(0))); });
		res[basic.getGenPreDec()]  = OP_CONVERTER({ return c_ast::preDec(getAssignmentTarget(context, ARG(0))); });
		res[basic.getGenPostDec()] = OP_CONVERTER({ return c_ast::postDec(getAssignmentTarget(context, ARG(0))); });

		res[basic.getUnsignedIntEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });


		// -- unsigned integers --

		res[basic.getSignedIntAdd()] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntSub()] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntMul()] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntDiv()] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntMod()] = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getSignedIntAnd()] = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntOr()] =  OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntXor()] = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntNot()] = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });

		res[basic.getSignedIntLShift()] = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntRShift()] = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getSignedIntEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		// -- CASTS --
		auto cast = OP_CONVERTER({ return c_ast::cast(CONVERT_RES_TYPE, CONVERT_ARG(0)); });

		res[basic.getUnsignedToInt()] = cast; 
		res[basic.getRealToInt()] 	  = cast; 
		res[basic.getCharToInt()] 	  = cast; 
		res[basic.getBoolToInt()] 	  = OP_CONVERTER({ return CONVERT_ARG(0); });

		res[basic.getSignedToUnsigned()]= cast; 
		res[basic.getRealToUnsigned()] 	= cast; 
		res[basic.getCharToUnsigned()] 	= cast; 
		res[basic.getBoolToUnsigned()] 	= OP_CONVERTER({ return CONVERT_ARG(0); });

		res[basic.getSignedToReal()]  = cast; 
		res[basic.getUnsignedToReal()]= cast; 
		res[basic.getCharToReal()] 	  = cast; 
		res[basic.getBoolToReal()] 	  = cast; 

		res[basic.getSignedToChar()]  = cast;
		res[basic.getUnsignedToChar()]= cast;
		res[basic.getRealToChar()] 	  = cast;
		res[basic.getBoolToChar()] 	  = cast;

		res[basic.getSignedToBool()]  = cast;
		res[basic.getUnsignedToBool()]= cast;
		res[basic.getRealToBool()] 	  = cast;
		res[basic.getCharToBool()] 	  = cast;

		res[basic.getIntPrecisionFix()]   = cast;
		res[basic.getUintPrecisionFix()]  = cast;
		res[basic.getRealPrecisionFix()] = cast;

		// -- reals --

		res[basic.getRealAdd()] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealSub()] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealMul()] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealDiv()] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getRealEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getRealToInt()] = OP_CONVERTER({ return c_ast::cast(CONVERT_RES_TYPE, CONVERT_ARG(0)); });

		// -- characters --

		res[basic.getCharEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });


		// -- references --

		res[basic.getRefEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRefNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRefGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRefGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRefLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRefLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });


		// -- undefined --

		res[basic.getUndefined()] = OP_CONVERTER({

			// one special case: an empty struct
			auto type = call->getType();
			if (const core::StructTypePtr& structType = type.isa<core::StructTypePtr>()) {
				if (structType.empty() && structType->getParents().empty()) {
					auto cType = CONVERT_TYPE(type);
					auto zero = C_NODE_MANAGER->create("0");
					return c_ast::deref(c_ast::cast(c_ast::ptr(cType), zero));
				}
			}

			// the rest is handled like a *var(undefined(..))
			return c_ast::deref(CONVERT_EXPR(core::IRBuilder(call->getNodeManager()).refVar(call)));
		});

		// -- volatile --

		res[basic.getVolatileMake()] = OP_CONVERTER({ return CONVERT_ARG(0); });
		res[basic.getVolatileRead()] = OP_CONVERTER({ return CONVERT_ARG(0); });
		

		// -- references --

		res[basic.getRefDeref()] = OP_CONVERTER({

			// special handling of derefing result of ref.new or ref.var => bogus
			core::ExpressionPtr arg = ARG(0);
			if (core::analysis::isCallOf(arg, LANG_BASIC.getRefVar()) || core::analysis::isCallOf(arg, LANG_BASIC.getRefNew())) {
				// skip ref.var / ref.new => stupid
				core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(arg);
				return CONVERT_EXPR(call->getArgument(0));
			}

			// extract resulting type
			const core::TypePtr elementType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(elementType);
			context.getDependencies().insert(info.definition);

			// special handling for string literals
			if (ARG(0)->getNodeType() == core::NT_Literal) {
				core::LiteralPtr literal = ARG(0).as<core::LiteralPtr>();
				if (literal->getStringValue()[0] == '\"') {
					// the cast to a vector element is implemented at this point
					// instead of the actual literal conversion to save unnecessary casts when
					// strings are forwarded to external functions (printf) directly
					return c_ast::deref(c_ast::cast(c_ast::ptr(info.rValueType), CONVERT_ARG(0)));
				}
			}

			// deref of an assigment, do not
			if (core::analysis::isCallOf (ARG(0), LANG_BASIC.getRefAssign()) ){
				return CONVERT_ARG(0);
			}

			return c_ast::deref(CONVERT_ARG(0));
		});

		res[basic.getRefAssign()] = OP_CONVERTER({
			return c_ast::assign(getAssignmentTarget(context, ARG(0)), CONVERT_ARG(1));
		});

		res[basic.getRefVar()] = OP_CONVERTER({

			// get some manager
			const core::lang::BasicGenerator& basic = LANG_BASIC;

			// extract type
			core::ExpressionPtr initValue = call->getArgument(0);
			core::TypePtr type = initValue->getType();
			const TypeInfo& valueTypeInfo = GET_TYPE_INFO(type);

			// fix dependency
			context.getDependencies().insert(valueTypeInfo.definition);


			// special handling for arrays
			if (type->getNodeType() == core::NT_ArrayType) {
				// no out allocation required!
				return CONVERT_EXPR(initValue);
			}

			// use a initializer to realize the ref var locally
			if (core::analysis::isCallOf(initValue, basic.getVectorInitUndefined()) ||
					core::analysis::isCallOf(initValue, basic.getUndefined())) {
				return c_ast::ref(c_ast::init(valueTypeInfo.rValueType, c_ast::lit(valueTypeInfo.rValueType, "0")));
			}

			// add support for partial vector initialization
			if (core::analysis::isCallOf(initValue, basic.getVectorInitPartial())) {
				return CONVERT_ARG(0);
			}

			auto res = CONVERT_EXPR(initValue);
			if (res->getNodeType() == c_ast::NT_Initializer) {
				return c_ast::ref(res);
			}
			// creates a something of the format "(int[1]){x}"
			return c_ast::init(c_ast::vec(valueTypeInfo.rValueType, 1), res);
		});

		res[basic.getRefNew()] = OP_CONVERTER({

			// get result type information
			core::RefTypePtr resType = static_pointer_cast<const core::RefType>(call->getType());
			const RefTypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(resType);

			// extract type
			core::ExpressionPtr initValue = call->getArgument(0);
			core::TypePtr type = initValue->getType();
			const TypeInfo& valueTypeInfo = GET_TYPE_INFO(type);

			// fix dependency
			context.getDependencies().insert(valueTypeInfo.definition);

			// special handling for requesting a un-initialized memory cell
			if (core::analysis::isCallOf(ARG(0), LANG_BASIC.getUndefined())) {
				ADD_HEADER_FOR("malloc");

				c_ast::ExpressionPtr size = c_ast::sizeOf(CONVERT_TYPE(resType->getElementType()));
				return c_ast::call(C_NODE_MANAGER->create("malloc"), size);
			}

			// special handling for arrays
			if (core::analysis::isCallOf(ARG(0), LANG_BASIC.getArrayCreate1D())) {
				// array-init is allocating data on stack using alloca => switch to malloc
				ADD_HEADER_FOR("malloc");

				auto res = CONVERT_ARG(0);
				return c_ast::call(C_NODE_MANAGER->create("malloc"), static_pointer_cast<const c_ast::Call>(res)->arguments[0]);
			}

			// special handling for variable sized structs
			if (core::types::isVariableSized(resType->getElementType())) {
				// Create code similar to this:
				// 		(A*)memcpy(malloc(sizeof(A) + sizeof(float) * v2), &(struct A){ v2 }, sizeof(A))

				assert(ARG(0)->getNodeType() == core::NT_StructExpr && "Only supporting struct expressions as initializer value so far!");
				core::StructExprPtr initValue = ARG(0).as<core::StructExprPtr>();

				// get types of struct and element
				auto structType = initValue->getType();
				auto elementType = core::types::getRepeatedType(structType);

				// get size of variable part
				auto arrayInitValue = initValue->getMembers().back()->getValue();
				assert(core::analysis::isCallOf(arrayInitValue, LANG_BASIC.getArrayCreate1D()) && "Array not properly initialized!");
				auto size = arrayInitValue.as<core::CallExprPtr>()->getArgument(1);

				// add header dependencies
				ADD_HEADER_FOR("malloc");
				ADD_HEADER_FOR("memcpy");

				auto c_struct_type = CONVERT_TYPE(structType);
				auto c_element_type = CONVERT_TYPE(elementType);

				// build call
				auto malloc = c_ast::call(C_NODE_MANAGER->create("malloc"),
						c_ast::add(c_ast::sizeOf(c_struct_type), c_ast::mul(c_ast::sizeOf(c_element_type), CONVERT_EXPR(size)))
				);
				return c_ast::cast(CONVERT_TYPE(resType), c_ast::call(C_NODE_MANAGER->create("memcpy"), malloc, c_ast::ref(CONVERT_EXPR(initValue)), c_ast::sizeOf(c_struct_type)));
			}

			// use a call to the ref_new operator of the ref type
			context.getDependencies().insert(info.newOperator);
			return c_ast::call(info.newOperatorName, CONVERT_ARG(0));

		});

		res[basic.getRefDelete()] = OP_CONVERTER({
			// TODO: fix when frontend is producing correct code

			// do not free non-heap variables
			if (ARG(0)->getNodeType() == core::NT_Variable) {
				core::VariablePtr var = static_pointer_cast<const core::Variable>(ARG(0));
				if (GET_VAR_INFO(var).location != VariableInfo::INDIRECT) {
					// return NULL pointer => no op
					return c_ast::ExpressionPtr();
				}
			}

			// ensure correct type
			assert(core::analysis::hasRefType(ARG(0)) && "Cannot free a non-ref type!");

			// handle destructor call
			if (core::CallExprPtr dtorCall = ARG(0).isa<core::CallExprPtr>()) {
				if (dtorCall->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->isDestructor()) {
					return c_ast::deleteCall(CONVERT_EXPR(dtorCall[0]));
				}
			}

			// add dependency to stdlib.h (contains the free)
			ADD_HEADER_FOR("free");

			// construct argument
			c_ast::ExpressionPtr arg = CONVERT_ARG(0);

			if (core::analysis::getReferencedType(ARG(0)->getType())->getNodeType() == core::NT_ArrayType) {
				// TODO: call array destructor instead!!
			}

			return c_ast::call(C_NODE_MANAGER->create("free"), arg);
		});

		res[basic.getRefReinterpret()] = OP_CONVERTER({
			c_ast::TypePtr type = CONVERT_TYPE(call->getType());
			c_ast::ExpressionPtr value = GET_TYPE_INFO(ARG(0)->getType()).externalize(C_NODE_MANAGER, CONVERT_ARG(0));
			return GET_TYPE_INFO(call->getType()).internalize(C_NODE_MANAGER, c_ast::cast(type, value));
		});

		// -- support narrow and expand --

		res[basic.getRefNarrow()] = OP_CONVERTER({
			// narrow starting position step by step
			return narrow(context, ARG(0), ARG(1));
		});

		res[basic.getRefExpand()] = OP_CONVERTER({
			ADD_HEADER_FOR("offsetof");

			auto charPtrType = c_ast::ptr(C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Char));

			// navigate up by re-computing pionter address
			auto res = CONVERT_ARG(0);	// get address of nested

			// cast to char* (to match byte order)
			res = c_ast::cast(charPtrType,res);

			// subtract offsets
			core::TypePtr curType;
			res = expand(context, res, ARG(1), call->getType().as<core::RefTypePtr>()->getElementType(), curType);

			// cast to target type and return result
			return c_ast::cast(CONVERT_TYPE(call->getType()), res);
		});


		// -- arrays --

		res[basic.getArraySubscript1D()] = OP_CONVERTER({
			// skip deref => included subscript operator
			c_ast::ExpressionPtr target;
			if (core::analysis::isCallOf(ARG(0), LANG_BASIC.getRefDeref())) {
				target = CONVERT_EXPR(core::analysis::getArgument(ARG(0), 0));
			} else {
				target = CONVERT_ARG(0);
			}
			return c_ast::subscript(target, CONVERT_ARG(1));
		});

		res[basic.getArrayRefElem1D()] = OP_CONVERTER({
			// add dependency to element type
			core::TypePtr elementType = core::analysis::getReferencedType(ARG(0)->getType()); \
			elementType = elementType.as<core::ArrayTypePtr>()->getElementType(); \
			const TypeInfo& info = GET_TYPE_INFO(elementType); \
			context.getDependencies().insert(info.definition);

			// generated code &(X[Y])
			return c_ast::ref(c_ast::subscript(CONVERT_ARG(0), CONVERT_ARG(1)));
		});

		res[basic.getScalarToArray()] = OP_CONVERTER({
			// initialize an array instance
			//   Operator Type: (ref<'a>) -> ref<array<'a,1>>
			// => requires no special treatment
			return CONVERT_ARG(0);
		});

		res[basic.getArrayCreate1D()] = OP_CONVERTER({
			// type of Operator: (type<'elem>, uint<8>) -> array<'elem,1>
			// create new array on the heap using alloca
			ADD_HEADER_FOR("alloca");

			const core::ArrayTypePtr& resType = static_pointer_cast<const core::ArrayType>(call->getType());
			c_ast::ExpressionPtr size = c_ast::mul(c_ast::sizeOf(CONVERT_TYPE(resType->getElementType())), CONVERT_ARG(1));
			return c_ast::call(C_NODE_MANAGER->create("alloca"), size);
		});

		#define ADD_ELEMENT_TYPE_DEPENDENCY() \
				core::TypePtr elementType = core::analysis::getReferencedType(call->getType()); \
				elementType = elementType.as<core::ArrayTypePtr>()->getElementType(); \
				const TypeInfo& info = GET_TYPE_INFO(elementType); \
				context.getDependencies().insert(info.definition);

		res[basic.getArrayView()] = OP_CONVERTER({
			// add dependency to element type definition
			ADD_ELEMENT_TYPE_DEPENDENCY();
			return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1));
		});

		res[basic.getArrayViewPreInc()]  = OP_CONVERTER({
			// add dependency to element type definition
			ADD_ELEMENT_TYPE_DEPENDENCY();
			return c_ast::preInc(getAssignmentTarget(context, ARG(0)));
		});

		res[basic.getArrayViewPostInc()] = OP_CONVERTER({
			// add dependency to element type definition
			ADD_ELEMENT_TYPE_DEPENDENCY();
			return c_ast::postInc(getAssignmentTarget(context, ARG(0)));
		});

		res[basic.getArrayViewPreDec()]  = OP_CONVERTER({
			// add dependency to element type definition
			ADD_ELEMENT_TYPE_DEPENDENCY();
			return c_ast::preDec(getAssignmentTarget(context, ARG(0)));
		});

		res[basic.getArrayViewPostDec()] = OP_CONVERTER({
			// add dependency to element type definition
			ADD_ELEMENT_TYPE_DEPENDENCY();
			return c_ast::postDec(getAssignmentTarget(context, ARG(0)));
		});

		#undef ADD_ELEMENT_TYPE_DEPENDENCY
		
		res[basic.getArrayRefDistance()] = OP_CONVERTER({
			return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1));
		});

		// -- vectors --

		res[basic.getVectorRefElem()] = OP_CONVERTER({
			//   operator type:  (ref<vector<'elem,#l>>, uint<8>) -> ref<'elem>

			// fix dependency
			const TypeInfo& infoSrc = GET_TYPE_INFO(core::analysis::getReferencedType(call->getArgument(0)->getType()));
			context.getDependencies().insert(infoSrc.definition);

			//   generated code: &((*X).data[Y])
			return c_ast::ref(c_ast::subscript(c_ast::access(c_ast::deref(CONVERT_ARG(0)), "data"), CONVERT_ARG(1)));
		});

		res[basic.getVectorSubscript()] = OP_CONVERTER({
			//   operator type:  (vector<'elem,#l>, uint<#a>) -> 'elem
			//   generated code: ((X).data[Y])
			return c_ast::subscript(c_ast::access(CONVERT_ARG(0), "data"), CONVERT_ARG(1));
		});

		res[basic.getVectorInitUniform()] = OP_CONVERTER({
			//  operator type:  ('elem, intTypeParam<#a>) -> vector<'elem,#a>
			//  generated code: <init_uniform_function>(ARG_0)

			// obtain information regarding vector type (including required functionality)
			const core::VectorTypePtr vectorType = static_pointer_cast<const core::VectorType>(call->getType());
			const VectorTypeInfo& info = GET_TYPE_INFO(vectorType);

			// add dependency
			context.getDependencies().insert(info.initUniform);

			return c_ast::call(info.initUniformName, CONVERT_ARG(0));
		});

		res[basic.getVectorInitPartial()] = OP_CONVERTER({

			// obtain information regarding vector type
			const core::VectorTypePtr vectorType = call->getType().as<core::VectorTypePtr>();
			const VectorTypeInfo& info = GET_TYPE_INFO(vectorType);

			// add dependency
			context.getDependencies().insert(info.definition);

			// create data vector to fill struct
			auto values = core::encoder::toValue<vector<core::ExpressionPtr>>(ARG(0));
			auto converted = ::transform(values, [&](const core::ExpressionPtr& cur)->c_ast::NodePtr { return CONVERT_EXPR(cur); });
			auto data = C_NODE_MANAGER->create<c_ast::VectorInit>(converted);

			// create compound-init expression filing struct
			return c_ast::init(info.rValueType, data);
		});

		res[basic.getRefVectorToRefArray()] = OP_CONVERTER({
			// Operator type: (ref<vector<'elem,#l>>) -> ref<array<'elem,1>>
			const TypeInfo& infoSrc = GET_TYPE_INFO(core::analysis::getReferencedType(call->getArgument(0)->getType()));
			context.getDependencies().insert(infoSrc.definition);

			const TypeInfo& infoRes = GET_TYPE_INFO(core::analysis::getReferencedType(call->getType()));
			context.getDependencies().insert(infoRes.definition);

			// special handling for string literals
			if (call->getArgument(0)->getNodeType() == core::NT_Literal) {
				core::LiteralPtr literal = call->getArgument(0).as<core::LiteralPtr>();
				if (literal->getStringValue()[0] == '\"') {
					return CONVERT_ARG(0);
				}
			}

			return c_ast::cast(infoRes.rValueType, CONVERT_ARG(0));
		});

		res[basic.getVectorReduction()] = OP_CONVERTER({
			// type of the operation:
			// (vector<'elem,#l>, 'res, ('elem, 'res) -> 'res) -> 'res

			// get vector size
			core::ExpressionPtr vector = ARG(0);
			assert(
				( vector->getNodeType() == core::NT_VectorExpr
				|| vector->getNodeType() == core::NT_Variable )
						&& "Only supporting vector expression or variable!");

			core::VectorExprPtr vectorExpr = dynamic_pointer_cast<const core::VectorExpr>(vector);
			core::VectorTypePtr vectorType = static_pointer_cast<const core::VectorType>(vector->getType());

			core::IntTypeParamPtr vectorSize = vectorType->getSize();
			assert(vectorSize->getNodeType() == core::NT_ConcreteIntTypeParam && "Only supported for fixed vector sizes!");
			std::size_t size = static_pointer_cast<const core::ConcreteIntTypeParam>(vectorSize)->getValue();

			// compose unfolded reduction expression
			core::ExpressionPtr res = ARG(1);
			core::ExpressionPtr op = ARG(2);
			core::ExpressionPtr subscript = op->getNodeManager().getLangBasic().getVectorSubscript();
			core::TypePtr elementType = vectorType->getElementType();

			core::IRBuilder builder(op->getNodeManager());
			for (std::size_t i = 0; i<size; i++) {
				core::ExpressionPtr element;
				if (vectorExpr) {
					element = vectorExpr->getExpressions()[i];
				} else {
					element = builder.callExpr(elementType, subscript, toVector(vector, builder.uintLit(i)));
				}

				res = builder.callExpr(res->getType(), op, toVector(res, element));
			}

			// add code of reduction expression
			return CONVERT_EXPR(res);
		});


		// -- structs --

		res[basic.getCompositeMemberAccess()] = OP_CONVERTER({
			// signature of operation:
			//		('a, identifier, type<'b>) -> 'b

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr structType = ARG(0)->getType();
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(structType);
			context.getDependencies().insert(info.definition);

			// create member access
			assert(ARG(1)->getNodeType() == core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(static_pointer_cast<const core::Literal>(ARG(1))->getStringValue());
			return c_ast::access(CONVERT_ARG(0), field);
		});

		res[basic.getCompositeRefElem()] = OP_CONVERTER({
			// signature of operation:
			//		(ref<'a>, identifier, type<'b>) -> ref<'b>

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr structType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(structType);
			context.getDependencies().insert(info.definition);

			assert(ARG(1)->getNodeType() == core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(static_pointer_cast<const core::Literal>(ARG(1))->getStringValue());

			// special handling for accessing variable array within struct
			auto access = c_ast::access(c_ast::deref(CONVERT_ARG(0)), field);
			if (core::analysis::isRefOf(call->getType(), core::NT_ArrayType)) {
				return access;
			}

			// access the type
			return c_ast::ref(access);
		});



		// -- tuples --

		res[basic.getTupleMemberAccess()] = OP_CONVERTER({
			// signature of operation:
			//		('a, uint<8>, type<'b>) -> 'b

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr tupleType = ARG(0)->getType();
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(tupleType);
			context.getDependencies().insert(info.definition);

			// create member access
			core::ExpressionPtr index = ARG(1);
			while(index->getNodeType() == core::NT_CastExpr) {
				index = static_pointer_cast<const core::CastExpr>(index)->getSubExpression();
			}
			assert(index->getNodeType() == core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(string("c") + static_pointer_cast<const core::Literal>(index)->getStringValue());
			return c_ast::access(CONVERT_ARG(0), field);
		});

		res[basic.getTupleRefElem()] = OP_CONVERTER({
			// signature of operation:
			//		(ref<'a>, uint<8>, type<'b>) -> ref<'b>

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr tupleType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(tupleType);
			context.getDependencies().insert(info.definition);

			core::ExpressionPtr index = ARG(1);
			while(index->getNodeType() == core::NT_CastExpr) {
				index = static_pointer_cast<const core::CastExpr>(index)->getSubExpression();
			}
			assert(index->getNodeType() == core::NT_Literal);
			c_ast::IdentifierPtr field = C_NODE_MANAGER->create(string("c") + static_pointer_cast<const core::Literal>(index)->getStringValue());

			// access the type
			return c_ast::ref(c_ast::access(c_ast::deref(CONVERT_ARG(0)), field));
		});



		// -- pointer --

/*
		res[basic.getRefIsNull()] = OP_CONVERTER({
			// Operator Type:  (array<'a,1>) -> bool
			// generated code: X == 0
			auto intType = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Int32);
			return c_ast::eq(CONVERT_ARG(0), c_ast::lit(intType,"0"));
		});
*/

		// -- others --

		res[basic.getIfThenElse()] = OP_CONVERTER({
			// IF-THEN-ELSE literal: (bool, () -> 'b, () -> 'b) -> 'b")
			return c_ast::ite(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1))), CONVERT_EXPR(inlineLazy(ARG(2))));
		});

		res[basic.getSizeof()] = OP_CONVERTER({
			// extract type sizeof is applied to
			core::GenericTypePtr type = dynamic_pointer_cast<const core::GenericType>(ARG(0)->getType());
			assert(type && "Illegal argument to sizeof operator");
			core::TypePtr target = type->getTypeParameter()[0];

			// return size-of operator call
			return c_ast::sizeOf(CONVERT_TYPE(target));
		});

		res[basic.getPrint()] = OP_CONVERTER({
			// map to invoking the external function printf
			core::IRBuilder builder(NODE_MANAGER);
			auto printf = builder.literal("printf", call->getFunctionExpr()->getType());
			return CONVERT_EXPR(builder.callExpr(LANG_BASIC.getUnit(), printf, ARG(0), ARG(1)));
		});

		// -- IR extensions --

		auto& ext = manager.getLangExtension<IRExtensions>();

		res[ext.registerGlobal] = OP_CONVERTER({

			// obtain access to global fragment
			c_ast::CCodeFragmentPtr globals = static_pointer_cast<c_ast::CCodeFragment>(FRAGMENT_MANAGER->getFragment(IRExtensions::GLOBAL_ID));
			if (!globals) {
				// create and bind a new global fragment
				globals = c_ast::CCodeFragment::createNew(FRAGMENT_MANAGER);
				FRAGMENT_MANAGER->bindFragment(IRExtensions::GLOBAL_ID, globals);
			}

			string name = static_pointer_cast<core::LiteralPtr>(ARG(0))->getStringValue();
			core::TypePtr globalType = core::analysis::getRepresentedType(ARG(1)->getType());
			if (globalType->getNodeType() == core::NT_RefType) {
				globalType = core::analysis::getReferencedType(globalType);
			}

			// get type of global struct
			const TypeInfo& info = GET_TYPE_INFO(globalType);

			// append new declaration to global struct
			c_ast::NodePtr decl = C_NODE_MANAGER->create<c_ast::VarDecl>(c_ast::var(info.lValueType, name));
			globals->getCode().push_back(decl);

			// add dependencies
			globals->addDependency(info.definition);
			context.getDependencies().insert(globals);

			// => no actual expression required her ...
			return c_ast::ExpressionPtr();
		});

		res[basic.getSelect()] = OP_CONVERTER({
			//  Implements the select operation
			//   Operator Type: ( comp(arg0, arg1) ? arg0 : arg1)

			core::IRBuilder builder(ARG(0)->getNodeManager());
			return c_ast::ite( CONVERT_EXPR( builder.callExpr(ARG(2), ARG(0), ARG(1)) ),
							   CONVERT_ARG(0),
							   CONVERT_ARG(1)
							 );
		});

		res[basic.getCloogFloor()] = OP_CONVERTER({
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

			return c_ast::ite(
					c_ast::gt(c_ast::mul(a,b), z),
					c_ast::div(a,b),
					c_ast::minus(c_ast::add(c_ast::div(na,b), c_ast::ne(c_ast::mod(na, b), z)))
			);

		});

		res[basic.getCloogCeil()] = OP_CONVERTER({
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

			return c_ast::ite(
					c_ast::gt(c_ast::mul(a,b), z),
					c_ast::add(c_ast::div(a,b), c_ast::ne(c_ast::mod(a,b),z)),
					c_ast::minus(c_ast::div(c_ast::minus(a),b))
			);
		});

		res[basic.getCloogMod()] = OP_CONVERTER({
			return c_ast::mod( CONVERT_ARG(0), CONVERT_ARG(1) );
		});

		res[basic.getExit()] = OP_CONVERTER({
			ADD_HEADER_FOR("exit");
			return c_ast::call( C_NODE_MANAGER->create("exit"), CONVERT_ARG(0));
		});


		auto& attrExt = manager.getLangExtension<core::analysis::AttributeExtension>();
		res[attrExt.getAttr()] = OP_CONVERTER({
			// just skip this attribute
			return CONVERT_ARG(0);
		});



		// ---------------------------- IR++ / C++ --------------------------


		{	// core C++ extensions

			const auto& irppExt = manager.getLangExtension<core::lang::IRppExtensions>();

			res[irppExt.getArrayCtor()] = OP_CONVERTER({

				// init array using a vector expression
				auto objType = ARG(1)->getType().as<core::FunctionTypePtr>()->getObjectType();
				auto type = CONVERT_TYPE(objType);
				auto size = CONVERT_ARG(2);
				c_ast::ExpressionPtr res = c_ast::initArray(type, size);

				// add dependency to class declaration
				context.addDependency(GET_TYPE_INFO(objType).declaration);

				// convert default constructor
				auto ctor = CONVERT_ARG(1);

				// add new if required
				const auto& basic = LANG_BASIC;
				if (basic.isRefVar(ARG(0))) {
					// nothing to do
				} else if (basic.isRefNew(ARG(0))) {
					res = c_ast::newCall(res);
				} else {
					assert(false && "Creating Arrays of objects neither on heap nor stack isn't supported!");
				}
				return res;
			});

			res[irppExt.getVectorCtor()] = OP_CONVERTER({

				// init vector using a vector expression
				auto objType = ARG(1)->getType().as<core::FunctionTypePtr>()->getObjectType();
				auto type = CONVERT_TYPE(objType);
				auto size = CONVERT_ARG(2);
				c_ast::ExpressionPtr res = c_ast::initArray(type, size);

				// add dependency to class declaration
				context.addDependency(GET_TYPE_INFO(objType).declaration);

				// convert default constructor
				auto ctor = CONVERT_ARG(1);

				// add new if required
				const auto& basic = LANG_BASIC;
				if (basic.isRefVar(ARG(0))) {
					// nothing to do
				} else if (basic.isRefNew(ARG(0))) {
					res = c_ast::newCall(res);
				} else {
					assert(false && "Creating Arrays of objects neither on heap nor stack isn't supported!");
				}
				return res;
			});

			res[irppExt.getArrayDtor()] = OP_CONVERTER({
				// create a node representing a delete operation
				return c_ast::deleteArrayCall(CONVERT_ARG(0));
			});

		}

		{	// backend C++ extensions

			const auto& irppExt = manager.getLangExtension<core::lang::IRppExtensions>();

			res[irppExt.getStaticCast()] = OP_CONVERTER({
				// build up a static cast operator
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
				
				if(!targetTy.isa<core::ArrayTypePtr>()
					&& !core::analysis::isCppRef(targetTy) ) {
					targetCType = c_ast::ptr(targetCType);	
				}

				return c_ast::staticCast( targetCType, CONVERT_ARG(0));
			});

			res[irppExt.getStaticCastRefCppToRefCpp()] = OP_CONVERTER({
				// build up a static cast operator for cpp_ref to cpp_ref
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
			
				assert(core::analysis::isCppRef(targetTy) && "targetType not a reference type");
				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
			});

			res[irppExt.getStaticCastConstCppToConstCpp()] = OP_CONVERTER({
				// build up a static cast operator for const_cpp_ref to const_cpp_ref
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
				
				assert(core::analysis::isConstCppRef(targetTy) && "targetType not a reference type");
				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
			});
			res[irppExt.getStaticCastRefCppToConstCpp()] = OP_CONVERTER({
				// build up a static cast operator for cpp_ref to const_cpp_ref
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
				
				assert(core::analysis::isConstCppRef(targetTy) && "targetType not a reference type");
				return c_ast::staticCast(targetCType, CONVERT_ARG(0));
			});

			res[irppExt.getDynamicCast()] = OP_CONVERTER({
				// build up a dynamic cast operator
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
				
				if(!targetTy.isa<core::ArrayTypePtr>()
					&& !core::analysis::isCppRef(targetTy) ) {
					targetCType = c_ast::ptr(targetCType);	
				}

				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
			});
				
			res[irppExt.getDynamicCastRefCppToRefCpp()] = OP_CONVERTER({
				// build up a dynamic cast operator for cpp_ref to cpp_ref
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
			
				assert(core::analysis::isCppRef(targetTy) && "targetType not a reference type");
				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
			});

			res[irppExt.getDynamicCastConstCppToConstCpp()] = OP_CONVERTER({
				// build up a dynamic cast operator for const_cpp_ref to const_cpp_ref
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
				
				assert(core::analysis::isConstCppRef(targetTy) && "targetType not a reference type");
				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
			});
			res[irppExt.getDynamicCastRefCppToConstCpp()] = OP_CONVERTER({
				// build up a dynamic cast operator for cpp_ref to const_cpp_ref
				
				auto targetTy = core::analysis::getRepresentedType(ARG(1));
				auto targetCType = CONVERT_TYPE(targetTy);
				
				assert(core::analysis::isConstCppRef(targetTy) && "targetType not a reference type");
				return c_ast::dynamicCast(targetCType, CONVERT_ARG(0));
			});
			res[irppExt.getTypeid()] = OP_CONVERTER({
				// extract typeinfo
				core::GenericTypePtr type = dynamic_pointer_cast<const core::GenericType>(ARG(0)->getType());
				if(type) {
                    			core::TypePtr target = type->getTypeParameter()[0];
					return c_ast::typeId(CONVERT_TYPE(target));
				} else {
					return c_ast::typeId(CONVERT_ARG(0));
				}
			});
		}

		#include "insieme/backend/operator_converter_end.inc"

		// table complete => return table
		return res;
	}

	



} // end namespace backend
} // end namespace insieme

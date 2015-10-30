/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/backend/preprocessor.h"

#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/function_manager.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/encoder/encoder.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/static_vars.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/attributes.h"

#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/instantiate.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {


	PreProcessorPtr getBasicPreProcessorSequence(BasicPreprocessorFlags options) {
		vector<PreProcessorPtr> steps;
		if(!(options & SKIP_POINTWISE_EXPANSION)) { steps.push_back(makePreProcessor<InlinePointwise>()); }
		steps.push_back(makePreProcessor<InitGlobals>());
		// steps.push_back(makePreProcessor<RedundancyElimination>());		// optional - disabled for performance reasons
		steps.push_back(makePreProcessor<CorrectRecVariableUsage>());
		steps.push_back(makePreProcessor<RecursiveLambdaInstantiator>());
		return makePreProcessor<PreProcessingSequence>(steps);
	}


	core::NodePtr PreProcessingSequence::process(const Converter& converter, const core::NodePtr& code) {
		auto& manager = converter.getNodeManager();

		// start by copying code to given target manager
		core::NodePtr res = manager.get(code);

		// apply sequence of pre-processing steps
		for_each(preprocessor, [&](const PreProcessorPtr& cur) { res = cur->process(converter, res); });

		// return final result
		return res;
	}


	// ------- concrete pre-processing step implementations ---------

	core::NodePtr NoPreProcessing::process(const Converter& converter, const core::NodePtr& code) {
		// just copy to target manager
		return converter.getNodeManager().get(code);
	}


	// --------------------------------------------------------------------------------------------------------------
	//      PreProcessor InlinePointwise => replaces invocations of pointwise operators with in-lined code
	// --------------------------------------------------------------------------------------------------------------

	class PointwiseReplacer : public core::transform::CachedNodeMapping {
		core::NodeManager& manager;
		const core::lang::BasicGenerator& basic;

	  public:
		PointwiseReplacer(core::NodeManager& manager) : manager(manager), basic(manager.getLangBasic()){};

		const core::NodePtr resolveElement(const core::NodePtr& ptr) {

			// check whether current node is a target of interest
			auto call = ptr.isa<core::CallExprPtr>();
			if (!call) return ptr->substitute(manager, *this);		// not of interest

			// get the (potential) pointwise operator
			auto pointwiseOp = call->getFunctionExpr();

			// check whether it is a call to the right function
			auto& arrayModule = manager.getLangExtension<core::lang::ArrayExtension>();
			if (!arrayModule.isCallOfArrayPointwise(pointwiseOp)) return ptr->substitute(manager, *this);

			// extract argument types
			core::TypePtr arg0Type = call[0]->getType();
			core::TypePtr arg1Type = call[1]->getType();
			core::TypePtr resType = call->getType();

			// check argument and result types!
			assert_pred1(core::lang::isFixedSizedArray, arg0Type) << "First argument should be a fixed-sized array!";
			assert_pred1(core::lang::isFixedSizedArray, arg1Type) << "First argument should be a fixed-sized array!";
			assert_pred1(core::lang::isFixedSizedArray, resType) << "Result should be a fixed-sized array!";

			// obtain the vector size
			unsigned size = core::lang::ArrayType(arg0Type).getNumElements();

			// obtain element types
			core::TypePtr arg0ElementType = core::lang::ArrayType(arg0Type).getElementType();
			core::TypePtr arg1ElementType = core::lang::ArrayType(arg1Type).getElementType();

			// extract operator
			core::ExpressionPtr op = pointwiseOp.as<core::CallExprPtr>()[0];

			// create new lambda, realizing the point-wise operation
			core::IRBuilder builder(manager);
			core::FunctionTypePtr funType = builder.functionType(toVector<core::TypePtr>(arg0Type, arg1Type), resType);

			core::VariablePtr v1 = builder.variable(builder.refType(arg0Type));
			core::VariablePtr v2 = builder.variable(builder.refType(arg1Type));
			core::VariablePtr res = builder.variable(resType);

			// create vector init expression
			vector<core::ExpressionPtr> elements;

			// unroll the pointwise operation
			core::TypePtr unitType = basic.getUnit();
			core::TypePtr longType = basic.getInt8();
			core::ExpressionPtr arraySubscript = arrayModule.getArraySubscript();
			for(std::size_t i = 0; i < size; i++) {
				core::LiteralPtr index = builder.literal(longType, boost::lexical_cast<std::string>(i));
				core::ExpressionPtr a = builder.callExpr(arraySubscript, builder.deref(v1), index);
				core::ExpressionPtr b = builder.callExpr(arraySubscript, builder.deref(v2), index);
				elements.push_back(builder.callExpr(op, a, b));
			}

			// return result
			auto sizeType = builder.numericType(core::lang::ArrayType(arg0Type).getSize().as<core::LiteralPtr>());
			core::StatementPtr body = builder.returnStmt(core::lang::buildArrayCreate(sizeType, elements));

			// construct substitute ...
			core::LambdaExprPtr substitute = builder.lambdaExpr(funType, toVector(v1, v2), body);
			return builder.callExpr(resType, substitute, call->getArguments())->substitute(manager, *this);
		}
	};

	core::NodePtr InlinePointwise::process(const Converter& converter, const core::NodePtr& code) {
		// the converter does the magic
		return PointwiseReplacer(converter.getNodeManager()).map(code);
	}


	// --------------------------------------------------------------------------------------------------------------
	//      Restore Globals
	// --------------------------------------------------------------------------------------------------------------

	namespace {

		core::CompoundStmtAddress getMainBody(const core::NodePtr& code) {
			static const core::CompoundStmtAddress fail;

			// check for the program - only works on the global level
			if(code->getNodeType() != core::NT_Program) { return fail; }

			// check whether it is a main program ...
			core::NodeAddress root(code);
			const core::ProgramAddress& program = core::static_address_cast<const core::Program>(root);
			if(!(program->getEntryPoints().size() == static_cast<std::size_t>(1))) { return fail; }

			// extract body of main
			const core::ExpressionAddress& mainExpr = program->getEntryPoints()[0];
			if(mainExpr->getNodeType() != core::NT_LambdaExpr) { return fail; }
			const core::LambdaExprAddress& main = core::static_address_cast<const core::LambdaExpr>(mainExpr);
			const core::StatementAddress& bodyStmt = main->getBody();
			if(bodyStmt->getNodeType() != core::NT_CompoundStmt) { return fail; }
			return core::static_address_cast<const core::CompoundStmt>(bodyStmt);
		}
	}


	// --------------------------------------------------------------------------------------------------------------
	//      Turn initial assignments of global variables into values to be assigned to init values.
	// --------------------------------------------------------------------------------------------------------------

	core::NodePtr InitGlobals::process(const Converter& converter, const core::NodePtr& code) {
		// get body of main
		auto body = getMainBody(code);
		if(!body) {
			return code; // nothing to do if this is not a full program
		}

		auto& mgr = code->getNodeManager();
		const auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();

		// search for globals initialized in main body
		bool stop = false;
		std::map<core::LiteralPtr, core::CallExprAddress> inits;
		core::visitDepthFirstOncePrunable(body, [&](const core::StatementAddress& stmt) -> bool {
			// check out
			if(auto call = stmt.isa<core::CallExprAddress>()) {
				// check if the function call is not a builtin.
				// this is done because a function call can cause
				// a modification of the global variable
				if(stop) { return true; }
				if(!core::lang::isBuiltIn(call.as<core::CallExprPtr>()->getFunctionExpr())) { stop = true; }
				// check whether it is an assignment
				if(core::analysis::isCallOf(call.as<core::CallExprPtr>(), refExt.getRefAssign())) {
					// check whether target is a literal
					if(auto trg = call[0].isa<core::LiteralPtr>()) {
						// check whether literal is already known
						auto pos = inits.find(trg);
						if(pos == inits.end()) {
							// found a new one
							inits[trg] = call;
						}
					}
				}
			}

			// decent into nested compound statements
			if(stmt.isa<core::CompoundStmtAddress>()) { return false; }

			// but nothing else
			return true;

		});

		// check if anything has been found
		if(inits.empty()) { return code; }

		// prepare a utility to check whether some expression is depending on globals
		auto isDependingOnGlobals = [&](const core::ExpressionPtr& expr) {
			return core::visitDepthFirstOnceInterruptible(expr, [&](const core::LiteralPtr& lit) -> bool {
				// every global variable is a literal of a ref-type ... that's how we identify those
				return core::lang::isReference(lit);
			});
		};

		// build up replacement map
		const auto& ext = mgr.getLangExtension<IRExtensions>();
		core::IRBuilder builder(mgr);
		std::map<core::NodeAddress, core::NodePtr> replacements;
		for(const auto& cur : inits) {
			// no free variables shell be moved to the global space
			if(core::analysis::hasFreeVariables(cur.second[1])) { continue; }

			// skip initialization expressions if they are depending on globals
			if(isDependingOnGlobals(cur.second[1])) { continue; }

			replacements[cur.second] = builder.callExpr(ext.getInitGlobal(), cur.second[0], cur.second[1]);
		}

		// if there is nothing to do => done
		if(replacements.empty()) { return code; }

		// conduct replacements
		return core::transform::replaceAll(mgr, replacements);
	}



	core::NodePtr CorrectRecVariableUsage::process(const Converter& converter, const core::NodePtr& code) {
		core::NodeManager& manager = converter.getNodeManager();

		// this pass has been implemented as part of the core manipulation utils
		return core::transform::makeCachedLambdaMapper([&](const core::NodePtr& code) -> core::NodePtr {
			       // only consider lambdas
			       if(code->getNodeType() != core::NT_LambdaExpr) return code;
			       // use core library utility to fix recursive variable usage
			       return core::transform::correctRecursiveLambdaVariableUsage(manager, code.as<core::LambdaExprPtr>());
			   })
		    .map(code);
	}

	core::NodePtr RecursiveLambdaInstantiator::process(const Converter& converter, const core::NodePtr& code) {
		auto isCovered = [&](const core::NodePtr& node) { return converter.getFunctionManager().isBuiltIn(node); };
		return core::transform::instantiateTypes(code, isCovered);
	}

} // end namespace backend
} // end namespace insieme

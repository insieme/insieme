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
#include "insieme/core/ir_class_info.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/ir_class_info.h"

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
#include "insieme/transform/ir_cleanup.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {


	PreProcessorPtr getBasicPreProcessorSequence(BasicPreprocessorFlags options) {
		vector<PreProcessorPtr> steps;
		if (!(options & SKIP_POINTWISE_EXPANSION)) {
			steps.push_back(makePreProcessor<InlinePointwise>());
		}
		steps.push_back(makePreProcessor<InitGlobals>());
		steps.push_back(makePreProcessor<MakeVectorArrayCastsExplicit>());
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
		for_each(preprocessor, [&](const PreProcessorPtr& cur) {
			res = cur->process(converter, res);
		});

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

		PointwiseReplacer(core::NodeManager& manager) : manager(manager), basic(manager.getLangBasic()) {};

		const core::NodePtr resolveElement(const core::NodePtr& ptr) {

			// check types => abort
			if (ptr->getNodeCategory() == core::NC_Type) {
				return ptr;
			}

			// look for call expressions
			if (ptr->getNodeType() == core::NT_CallExpr) {
				// extract the call
				core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(ptr);

				// only care about calls to pointwise operations
				if (core::analysis::isCallOf(call->getFunctionExpr(), basic.getVectorPointwise())) {

					// get argument and result types!
					assert_eq(call->getType()->getNodeType(), core::NT_VectorType) << "Result should be a vector!";
					assert_eq(call->getArgument(0)->getType()->getNodeType(), core::NT_VectorType) << "Argument should be a vector!";

					core::VectorTypePtr argType = static_pointer_cast<const core::VectorType>(call->getArgument(0)->getType());
					core::VectorTypePtr resType = static_pointer_cast<const core::VectorType>(call->getType());

					// extract generic parameter types
					core::TypePtr in = argType->getElementType();
					core::TypePtr out = resType->getElementType();

					assert_eq(resType->getSize()->getNodeType(), core::NT_ConcreteIntTypeParam) << "Result should be of fixed size!";
					core::ConcreteIntTypeParamPtr size = static_pointer_cast<const core::ConcreteIntTypeParam>(resType->getSize());

					// extract operator
					core::ExpressionPtr op = static_pointer_cast<const core::CallExpr>(call->getFunctionExpr())->getArgument(0);

					// create new lambda, realizing the point-wise operation
					core::IRBuilder builder(manager);

					core::FunctionTypePtr funType = builder.functionType(toVector<core::TypePtr>(argType, argType), resType);

					core::VariablePtr v1 = builder.variable(argType);
					core::VariablePtr v2 = builder.variable(argType);
					core::VariablePtr res = builder.variable(resType);

					// create vector init expression
					vector<core::ExpressionPtr> fields;

					// unroll the pointwise operation
					core::TypePtr unitType = basic.getUnit();
					core::TypePtr longType = basic.getUInt8();
					core::ExpressionPtr vectorSubscript = basic.getVectorSubscript();
					for(std::size_t i=0; i<size->getValue(); i++) {
						core::LiteralPtr index = builder.literal(longType, boost::lexical_cast<std::string>(i));

						core::ExpressionPtr a = builder.callExpr(in, vectorSubscript, v1, index);
						core::ExpressionPtr b = builder.callExpr(in, vectorSubscript, v2, index);

						fields.push_back(builder.callExpr(out, op, a, b));
					}

					// return result
					core::StatementPtr body = builder.returnStmt(builder.vectorExpr(resType, fields));

					// construct substitute ...
					core::LambdaExprPtr substitute = builder.lambdaExpr(funType, toVector(v1,v2), body);
					return builder.callExpr(resType, substitute, call->getArguments());
				}
			}

			// decent recursively
			return ptr->substitute(manager, *this);
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
			if (code->getNodeType() != core::NT_Program) {
				return fail;
			}

			// check whether it is a main program ...
			core::NodeAddress root(code);
			const core::ProgramAddress& program = core::static_address_cast<const core::Program>(root);
			if (!(program->getEntryPoints().size() == static_cast<std::size_t>(1))) {
				return fail;
			}

			// extract body of main
			const core::ExpressionAddress& mainExpr = program->getEntryPoints()[0];
			if (mainExpr->getNodeType() != core::NT_LambdaExpr) {
				return fail;
			}
			const core::LambdaExprAddress& main = core::static_address_cast<const core::LambdaExpr>(mainExpr);
			const core::StatementAddress& bodyStmt = main->getBody();
			if (bodyStmt->getNodeType() != core::NT_CompoundStmt) {
				return fail;
			}
			return core::static_address_cast<const core::CompoundStmt>(bodyStmt);
		}

	}



	// --------------------------------------------------------------------------------------------------------------
	//      Turn initial assignments of global variables into values to be assigned to init values.
	// --------------------------------------------------------------------------------------------------------------

	core::NodePtr InitGlobals::process(const Converter& converter, const core::NodePtr& code) {

		// get body of main
		auto body = getMainBody(code);
		if (!body) return code;			// nothing to do if this is not a full program

		auto& mgr = code->getNodeManager();
		const auto& base = mgr.getLangBasic();

		// search for globals initialized in main body
		bool stop = false;
		std::map<core::LiteralPtr, core::CallExprAddress> inits;
		core::visitDepthFirstOncePrunable(body, [&](const core::StatementAddress& stmt)->bool {
			// check out
			if (auto call = stmt.isa<core::CallExprAddress>()) {
                // check if the function call is not a builtin.
                // this is done because a function call can cause
                // a modification of the global variable
                if (stop) {
                    return true;
                }
                if (!core::lang::isBuiltIn(call.as<core::CallExprPtr>()->getFunctionExpr())) {
                    stop = true;
                }
				// check whether it is an assignment
				if (core::analysis::isCallOf(call.as<core::CallExprPtr>(), base.getRefAssign())) {

					// check whether target is a literal
					if (auto trg = call[0].isa<core::LiteralPtr>()) {

						// check whether literal is already known
						auto pos = inits.find(trg);
						if (pos == inits.end()) {
							// found a new one
							inits[trg] = call;
						}
					}
				}
			}

			// decent into nested compound statements
			if (stmt.isa<core::CompoundStmtAddress>()) {
				return false;
			}

			// but nothing else
			return true;

		});

		// check if anything has been found
		if (inits.empty()) return code;

		// prepare a utility to check whether some expression is depending on globals
		auto isDependingOnGlobals = [&](const core::ExpressionPtr& expr) {
			return core::visitDepthFirstOnceInterruptible(expr, [&](const core::LiteralPtr& lit)->bool {
				// every global variable is a literal of a ref-type ... that's how we identify those
				return lit->getType().isa<core::RefTypePtr>();
			});
		};

		// build up replacement map
		const auto& ext = mgr.getLangExtension<IRExtensions>();
		core::IRBuilder builder(mgr);
		std::map<core::NodeAddress, core::NodePtr> replacements;
		for(const auto& cur : inits) {
			// no free variables shell be moved to the global space
			if (core::analysis::hasFreeVariables(cur.second[1])) {
				continue;
			}

			// skip initialization expressions if they are depending on globals
			if (isDependingOnGlobals(cur.second[1])) {
				continue;
			}

			// if it is initializing a vector
			if (cur.second[1].isa<core::VectorExprPtr>()) {
				continue;
			}
			replacements[cur.second] = builder.callExpr(ext.getInitGlobal(), cur.second[0], cur.second[1]);
		}

		// if there is nothing to do => done
		if (replacements.empty()) {
			return code;
		}

		// conduct replacements
		return core::transform::replaceAll(mgr, replacements);
	}


	// --------------------------------------------------------------------------------------------------------------
	//      Adding explicit Vector to Array casts
	// --------------------------------------------------------------------------------------------------------------

	class VectorToArrayConverter : public core::transform::CachedNodeMapping {

		core::NodeManager& manager;

	public:

		VectorToArrayConverter(core::NodeManager& manager) : manager(manager) {}

		const core::NodePtr resolveElement(const core::NodePtr& ptr) {

			// do not touch types ...
			if (ptr->getNodeCategory() == core::NC_Type) {
				return ptr;
			}

			// apply recursively - bottom up
			core::NodePtr res = ptr->substitute(ptr->getNodeManager(), *this);

			// handle calls
			if (ptr->getNodeType() == core::NT_CallExpr) {
				res = handleCallExpr(core::static_pointer_cast<const core::CallExpr>(res));
			}

			// handle rest
			return res;
		}

	private:

		/**
		 * This method replaces vector arguments with vector2array conversions whenever necessary.
		 */
		core::CallExprPtr handleCallExpr(core::CallExprPtr call) {

			// extract node manager
			core::IRBuilder builder(manager);
			const core::lang::BasicGenerator& basic = builder.getLangBasic();


			// check whether there is a argument which is a vector but the parameter is not
			const core::TypePtr& type = call->getFunctionExpr()->getType();
			assert_eq(type->getNodeType(), core::NT_FunctionType) << "Function should be of a function type!";
			const core::FunctionTypePtr& funType = core::static_pointer_cast<const core::FunctionType>(type);

			const core::TypeList& paramTypes = funType->getParameterTypes()->getElements();
			const core::ExpressionList& args = call->getArguments();

			// check number of arguments
			if (paramTypes.size() != args.size()) {
				// => invalid call, don't touch this
				return call;
			}

			bool conversionRequired = false;
			std::size_t size = args.size();
			for (std::size_t i = 0; !conversionRequired && i < size; i++) {
				conversionRequired = conversionRequired || (args[i]->getType()->getNodeType() == core::NT_VectorType && paramTypes[i]->getNodeType() != core::NT_VectorType);
			}

			// check whether a vector / non-vector argument/parameter pair has been found
			if (!conversionRequired) {
				// => no deduction required
				return call;
			}

			// derive type variable instantiation
			auto instantiation = core::types::getTypeVariableInstantiation(manager, call);
			if (!instantiation) {
				// => invalid call, don't touch this
				return call;
			}

			// obtain argument list
			vector<core::TypePtr> argTypes;
			::transform(args, std::back_inserter(argTypes), [](const core::ExpressionPtr& cur) { return cur->getType(); });

			// apply match on parameter list
			core::TypeList newParamTypes = paramTypes;
			for (std::size_t i=0; i<newParamTypes.size(); i++) {
				newParamTypes[i] = instantiation->applyTo(manager, newParamTypes[i]);
			}

			// generate new argument list
			bool changed = false;
			core::ExpressionList newArgs = call->getArguments();
			for (unsigned i=0; i<size; i++) {

				// ignore identical types
				if (*newParamTypes[i] == *argTypes[i]) {
					continue;
				}

				core::TypePtr argType = argTypes[i];
				core::TypePtr paramType = newParamTypes[i];

				// strip references
				bool ref = false;
				if (argType->getNodeType() == core::NT_RefType && paramType->getNodeType() == core::NT_RefType) {
					ref = true;
					argType = core::static_pointer_cast<const core::RefType>(argType)->getElementType();
					paramType = core::static_pointer_cast<const core::RefType>(paramType)->getElementType();
				}

				// handle vector->array
				if (argType->getNodeType() == core::NT_VectorType && paramType->getNodeType() == core::NT_ArrayType) {
					if (!ref) { assert_true(ref) << "Cannot convert implicitly to array value!"; }
					// conversion needed
					newArgs[i] = builder.callExpr(basic.getRefVectorToRefArray(), newArgs[i]);
					changed = true;
				}
			}
			if (!changed) {
				// return original call
				return call;
			}

			// exchange parameters and done
			return core::CallExpr::get(manager, instantiation->applyTo(call->getType()), call->getFunctionExpr(), newArgs);
		}

	};

	core::NodePtr MakeVectorArrayCastsExplicit::process(const Converter& converter, const core::NodePtr& code) {
		// the converter does the magic
		return VectorToArrayConverter(converter.getNodeManager()).map(code);
	}

	core::NodePtr RedundancyElimination::process(const Converter& converter, const core::NodePtr& code) {
		// this pass has been implemented as part of the core manipulation utils
		return transform::eliminateRedundantAssignments(code);
	}

	core::NodePtr CorrectRecVariableUsage::process(const Converter& converter, const core::NodePtr& code) {
		core::NodeManager& manager = converter.getNodeManager();

		// this pass has been implemented as part of the core manipulation utils
		return core::transform::makeCachedLambdaMapper([&](const core::NodePtr& code)->core::NodePtr {
			// only consider lambdas
			if (code->getNodeType() != core::NT_LambdaExpr) return code;
			// use core library utility to fix recursive variable usage
			return core::transform::correctRecursiveLambdaVariableUsage(manager, code.as<core::LambdaExprPtr>());
		}).map(code);
	}

	core::NodePtr RecursiveLambdaInstantiator::process(const Converter& converter, const core::NodePtr& code) {
		auto elem = core::IRBuilder(converter.getNodeManager()).typeVariable("elem");
		LOG(DEBUG) << "PRE ============ elem? " << core::analysis::contains(code, elem) << "\n"; 
		LOG(DEBUG) << dumpColor(code) << "\n";
		LOG(DEBUG) << "PRE DETAIL ============\n"; 
		LOG(DEBUG) << dumpDetailColored(code) << "\n";

		auto isLangOrOpBuiltin = [&](const core::NodePtr& node) {
			return core::lang::isBuiltIn(node) || converter.getFunctionManager().isBuiltIn(node);
		};
		auto ret = core::transform::instantiateTypes(code, isLangOrOpBuiltin);

		visitDepthFirstOnce(code, [&](const core::TypePtr& t) {
			if(core::hasMetaInfo(t)) {
				auto meta = core::getMetaInfo(t);
				auto metaEnc = core::encoder::toIR(t->getNodeManager(), meta);
				auto pre = metaEnc;
				LOG(DEBUG) << "META INFO PRE: elem? " 
					<< core::analysis::contains(metaEnc, elem) << "\n" << dumpColor(metaEnc) 
					<< "META INFO PRE TEXT:\n" << dumpText(metaEnc) << "\n";
				metaEnc = core::transform::instantiateTypes(metaEnc, isLangOrOpBuiltin).as<core::ExpressionPtr>();
				LOG(DEBUG) << "META INFO POST: elem? " 
					<< core::analysis::contains(metaEnc, elem) << "\n" << dumpColor(metaEnc) 
					<< "META INFO POST TEXT:\n" << dumpText(metaEnc) << "\n";
				LOG(DEBUG) << "META INFO POST=PRE? " << (*metaEnc == *pre) << "\n";
				core::setMetaInfo(t, core::encoder::toValue<core::ClassMetaInfo>(metaEnc));
			}
		}, true, true);


		LOG(DEBUG) << "RET ============ elem? " << core::analysis::contains(ret, elem) << "\n";
		LOG(DEBUG) << dumpColor(ret) << "\n";
		LOG(DEBUG) << "RET DETAIL ============\n"; 
		LOG(DEBUG) << dumpDetailColored(ret) << "\n";
		return ret;
	}

} // end namespace backend
} // end namespace insieme

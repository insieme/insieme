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

#include "insieme/backend/preprocessor.h"

#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/function_manager.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
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
		if (!(options & SKIP_RESTORE_GLOBALS)) {
			steps.push_back(makePreProcessor<RestoreGlobals>());
		}
		steps.push_back(makePreProcessor<MakeVectorArrayCastsExplicit>());
		// steps.push_back(makePreProcessor<RedundancyElimination>());		// optional - disabled for performance reasons
		steps.push_back(makePreProcessor<CorrectRecVariableUsage>());
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
					assert(call->getType()->getNodeType() == core::NT_VectorType && "Result should be a vector!");
					assert(call->getArgument(0)->getType()->getNodeType() == core::NT_VectorType && "Argument should be a vector!");

					core::VectorTypePtr argType = static_pointer_cast<const core::VectorType>(call->getArgument(0)->getType());
					core::VectorTypePtr resType = static_pointer_cast<const core::VectorType>(call->getType());

					// extract generic parameter types
					core::TypePtr in = argType->getElementType();
					core::TypePtr out = resType->getElementType();

					assert(resType->getSize()->getNodeType() == core::NT_ConcreteIntTypeParam && "Result should be of fixed size!");
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

	bool isZero(const core::ExpressionPtr& value) {

		const core::lang::BasicGenerator& basic = value->getNodeManager().getLangBasic();
		core::IRBuilder builder(value->getNodeManager());

		// if initialization is zero ...
		if (value == builder.getZero(value->getType())) {
			// no initialization required
			return true;
		}

		// ... or a zero literal ..
		if (value->getNodeType() == core::NT_Literal) {
			const string& strValue = static_pointer_cast<const core::Literal>(value)->getStringValue();
			if (strValue == "0" || strValue == "0.0") {
				return true;
			}
		}

		// ... or the ref.null literal
		if (basic.isRefNull(value)) {
			return true;
		}

		// ... or a vector initialization with a zero value
		if (core::analysis::isCallOf(value, basic.getVectorInitUniform())) {
			return isZero(core::analysis::getArgument(value, 0));
		}

		// TODO: remove this when frontend is fixed!!
		// => compensate for silly stuff like var(*getNull())
		if (core::analysis::isCallOf(value, basic.getRefVar())) {
			core::ExpressionPtr arg = core::analysis::getArgument(value, 0);
			if (core::analysis::isCallOf(arg, basic.getRefDeref())) {
				return isZero(core::analysis::getArgument(arg, 0));
			}
		}

		// otherwise, it is not zero
		return false;
	}

	namespace {


		struct GlobalDeclarationCollector : public core::IRVisitor<bool, core::Address> {
			vector<core::DeclarationStmtAddress> decls;

			// do not visit types
			GlobalDeclarationCollector() : IRVisitor<bool, core::Address>(false) {}

			bool visitNode(const core::NodeAddress& node) { return true; }	// does not need to decent deeper

			bool visitDeclarationStmt(const core::DeclarationStmtAddress& cur) {
				core::DeclarationStmtPtr decl = cur.getAddressedNode();

				// check the type
				core::TypePtr type = decl->getVariable()->getType();

				// check for references
				if (type->getNodeType() != core::NT_RefType) {
					return true;   // not a global struct
				}

				type = static_pointer_cast<core::RefTypePtr>(type)->getElementType();

				// the element type has to be a struct type
				if (type->getNodeType() != core::NT_StructType) {
					return true;    // also, not a global
				}


				// check initalization
				auto& basic = decl->getNodeManager().getLangBasic();
				core::ExpressionPtr init = decl->getInitialization();
				if (!(core::analysis::isCallOf(init, basic.getRefNew()) || core::analysis::isCallOf(init, basic.getRefVar()))) {
					return true; 	// again, not a global
				}

				init = core::analysis::getArgument(init, 0);

				// check whether the initialization is based on a struct expression
				if (init->getNodeType() != core::NT_StructExpr) {
					return true; 	// guess what, not a global!
				}

				// well, this is a global
				decls.push_back(cur);
				return true;
			}

			bool visitCompoundStmt(const core::CompoundStmtAddress& cmp) {
				return false; // keep descending into those!
			}

		};

		vector<core::DeclarationStmtAddress> getGlobalDeclarations(const core::CompoundStmtPtr& mainBody) {
			GlobalDeclarationCollector collector;
			core::visitDepthFirstPrunable(core::NodeAddress(mainBody), collector);
			return collector.decls;
		}

	}


	namespace {

		core::StatementPtr splitUpAndRegisterGlobals(core::NodeManager& manager, const core::StatementPtr& code, const vector<core::LiteralPtr>& globals) {

			core::IRBuilder builder(manager);
			const auto& extensions = manager.getLangExtension<IRExtensions>();
			const auto& basic = manager.getLangBasic();
			const auto& unit = basic.getUnit();


			unsigned counter = globals.size();
			vector<core::LiteralPtr> res_globals;		// the list of new globals introduced by this function
			utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;

			for_each(globals, [&](const core::LiteralPtr& cur) {
				if (!cur->getType()->getNodeType() == core::NT_RefType) {
					return; 	// only operates interested in ref<struct< ... >> literals
				}

				core::TypePtr type = core::analysis::getReferencedType(cur->getType());
				if (type->getNodeType() != core::NT_StructType) {
					return; 	// only operates interested in ref<struct< ... >> literals
				}

				core::StructTypePtr structType = type.as<core::StructTypePtr>();

				// collect list of large structures (vectors) and the rest
				vector<core::NamedTypePtr> remaining;
				vector<core::NamedTypePtr> blocks;

				for_each(structType, [&](const core::NamedTypePtr& cur) {
					// only interested in vectors
					if (cur->getType()->getNodeType() != core::NT_VectorType) {
						remaining.push_back(cur);
					} else {
						blocks.push_back(cur);
					}
				});

				// check whether there is something to be isolated into a stand-alone global
				if (blocks.empty()) {
					res_globals.push_back(cur);
					return; // nothing found
				}

				// alter type of existing global
				core::LiteralPtr altered_global = builder.literal(cur->getValue(), builder.refType(builder.structType(remaining)));
				replacements[cur] = altered_global;
				res_globals.push_back(altered_global);

				// add new globals
				for_each(blocks, [&](const core::NamedTypePtr& curMember) {
					core::LiteralPtr new_global = builder.literal(builder.refType(curMember->getType()), IRExtensions::GLOBAL_ID + toString(counter++));
					replacements[builder.refMember(cur, curMember->getName())] = new_global;
					replacements[builder.accessMember(builder.deref(cur), curMember->getName())] = builder.deref(new_global);
					res_globals.push_back(new_global);
				});

			});

			// check whether larger blocks have been found
			if (res_globals.empty()) {
				return code;
			}

			// replace the globals
			core::StatementPtr res = core::transform::replaceAll(manager, code, replacements).as<core::StatementPtr>();

			// register the new globals
			vector<core::StatementPtr> body;
			for_each(res_globals, [&](const core::LiteralPtr& cur) {
				core::ExpressionPtr typeLiteral = builder.getTypeLiteral(cur->getType());
				core::LiteralPtr nameLiteral = builder.getIdentifierLiteral(cur->getStringValue());
				core::StatementPtr registerGlobal = builder.callExpr(unit, extensions.registerGlobal, nameLiteral, typeLiteral);
				body.push_back(registerGlobal);
			});
			body.push_back(res);

			// build resulting body
			return builder.compoundStmt(body);
		}

	}


	core::NodePtr RestoreGlobals::process(const Converter& converter, const core::NodePtr& code) {
		return process(converter.getNodeManager(), code);
	}

	core::NodePtr RestoreGlobals::process(core::NodeManager& manager, const core::NodePtr& code) {

		// check for the program - only works on the global level
		if (code->getNodeType() != core::NT_Program) {
			return code;
		}

		// check whether it is a main program ...
		core::NodeAddress root(code);
		const core::ProgramAddress& program = core::static_address_cast<const core::Program>(root);
		if (!(program->getEntryPoints().size() == static_cast<std::size_t>(1))) {
			return code;
		}

		// extract body of main
		const core::ExpressionAddress& mainExpr = program->getEntryPoints()[0];
		if (mainExpr->getNodeType() != core::NT_LambdaExpr) {
			return code;
		}
		const core::LambdaExprAddress& main = core::static_address_cast<const core::LambdaExpr>(mainExpr);
		const core::StatementAddress& bodyStmt = main->getBody();
		if (bodyStmt->getNodeType() != core::NT_CompoundStmt) {
			return code;
		}
		core::CompoundStmtAddress body = core::static_address_cast<const core::CompoundStmt>(bodyStmt);


		// search for global structs
		vector<core::DeclarationStmtAddress> globals = getGlobalDeclarations(body.getAddressedNode());
		if (globals.empty()) {
			return code;
		}

		// create global_literals
		utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> replacements;
		vector<core::LiteralPtr> globalLiterals;

		int i = 0;
		for_each(globals, [&](const core::DeclarationStmtAddress& cur) {
			core::DeclarationStmtPtr decl = cur.getAddressedNode();
			core::LiteralPtr global = core::Literal::get(manager, decl->getVariable()->getType(), IRExtensions::GLOBAL_ID + toString(i++));
			replacements[decl->getVariable()] = global;
			globalLiterals.push_back(global);
		});


		// replace declarations with pure initializations
		core::IRBuilder builder(manager);
		const core::lang::BasicGenerator& basic = manager.getLangBasic();

		// get some functions used for the initialization
		core::ExpressionPtr initUniform = basic.getVectorInitUniform();

		// a property used to determine whether an initial value is undefined
		auto isUndefined = [&](const core::NodePtr& cur) {
			return   core::analysis::isCallOf(cur, basic.getUndefined()) ||
					(core::analysis::isCallOf(cur, basic.getRefVar()) &&
					 core::analysis::isCallOf(core::analysis::getArgument(cur, 0), basic.getUndefined())) ||
				    (core::analysis::isCallOf(cur, basic.getRefNew()) &&
					 core::analysis::isCallOf(core::analysis::getArgument(cur, 0), basic.getUndefined()));
		};

		// create an initializing block for each global value
		i = 0;
		std::map<core::NodeAddress, core::NodePtr> initializations;
		for_each(globals, [&](const core::DeclarationStmtAddress& cur) {
			core::DeclarationStmtPtr decl = cur.getAddressedNode();

			vector<core::StatementPtr> initExpressions;

			// initialize remaining fields of global struct
			core::ExpressionPtr initValue = core::analysis::getArgument(decl->getInitialization(), 0);
			assert(initValue->getNodeType() == core::NT_StructExpr);
			core::StructExprPtr initStruct = static_pointer_cast<const core::StructExpr>(initValue);

			for_each(initStruct->getMembers()->getElements(), [&](const core::NamedValuePtr& member) {

				// ignore zero values => default initialization
				if (isZero(member->getValue())) {
					return;
				}

				// ignore initalizations of undefined functions
				if (isUndefined(member->getValue())) {
					return;
				}

				core::ExpressionPtr access = builder.refMember(replacements[decl->getVariable()], member->getName());
				core::ExpressionPtr assign = builder.assign(access, member->getValue());
				initExpressions.push_back(assign);
			});

			// aggregate initializations to compound stmt
			initializations[cur] = builder.compoundStmt(initExpressions);
		});

		// create new body
		core::StatementPtr newBody = static_pointer_cast<core::StatementPtr>(core::transform::replaceAll(manager, initializations));

		// propagate new global variables
		for_each(replacements, [&](const std::pair<core::VariablePtr, core::ExpressionPtr>& cur) {
			newBody = core::transform::fixVariable(manager, newBody, cur.first, cur.second);
		});

		// split up globals (e.g. every array it's own global)
		newBody = splitUpAndRegisterGlobals(manager, newBody, globalLiterals);

		// replace old and new body
		return core::transform::replaceNode(manager, body, newBody);
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
			assert(type->getNodeType() == core::NT_FunctionType && "Function should be of a function type!");
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
					if (!ref) { assert(ref && "Cannot convert implicitly to array value!"); }
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

} // end namespace backend
} // end namespace insieme

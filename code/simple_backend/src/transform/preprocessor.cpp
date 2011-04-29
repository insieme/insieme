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

#include "insieme/simple_backend/transform/preprocessor.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ast_builder.h"
#include "insieme/core/expressions.h"
#include "insieme/core/type_utils.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_variable_deduction.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/simple_backend/ir_extensions.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace simple_backend {
namespace transform {

	namespace {

		/**
		 * Converts all string literals into vectors to eliminate unsupported string types.
		 *
		 * @param manager the manager to be responsible for maintaining the intermediate and the final result
		 * @param code the code to be processed
		 * @return the same DAG, however, string literals will be convereted into char vector literals
		 */
		core::NodePtr convertStringLiterals(core::NodeManager& manager, const core::NodePtr& code);

		/**
		 * Replaces all occurrences of ITE calls with lazy-ITE calls. Lazy-ITE calls correspond to the C-equivalent,
		 * where the if / then branch is only evaluated after evaluating the boolean condition.
		 *
		 * @param manager the manager to be responsible for maintaining the intermediate and the final result
		 * @param code the code to be processed
		 * @return the same DAG, however, ITE calls will be replaced with lazy ITE calls
		 */
		core::NodePtr convertITE(core::NodeManager& manager, const core::NodePtr& code);

		/**
		 * This pass is introducing vector->array conversions within the given program DAG wherever necessary.
		 *
		 * @param manager the manager to be responsible for maintaining the intermediate and the final result
		 * @param code the code to be processed
		 * @return the same DAG, however, implicit vector->array conversions will be explicit
		 */
		core::NodePtr addImplicitVectorArrayCasts(core::NodeManager& manager, const core::NodePtr& code);

	}

	core::NodePtr preprocess(core::NodeManager& manager, const core::NodePtr& code) {
		// for starters - just mirror the code
		core::NodePtr res = manager.get(code);

		// replace ITEs with lazy ITEs
		res = convertITE(manager, res);

		// apply the vector/array conversion
		res = addImplicitVectorArrayCasts(manager, res);

		// done
		return res;
	}


	namespace {

		// --------------------------------------------------------------------------------------------------------------



		class ITEConverter : public core::NodeMapping {

			const core::LiteralPtr ITE;
			const IRExtensions extensions;

		public:

			ITEConverter(core::NodeManager& manager) :
				ITE(manager.basic.getIfThenElse()),  extensions(manager) {};

			/**
			 * Searches all ITE calls and replaces them by lazyITE calls. It also is aiming on inlining
			 * the resulting call.
			 */
			const core::NodePtr mapElement(unsigned index, const core::NodePtr& ptr) {
				// do not touch types ...
				if (ptr->getNodeCategory() == core::NC_Type) {
					return ptr;
				}

				// apply recursively - bottom up
				core::NodePtr res = ptr->substitute(ptr->getNodeManager(), *this, true);

				// check current node
				if (!core::analysis::isCallOf(res, ITE)) {
					// no change required
					return res;
				}

				// exchange ITE call
				core::ASTBuilder builder(res->getNodeManager());
				core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(res);
				const vector<core::ExpressionPtr>& args = call->getArguments();
				return builder.callExpr(extensions.lazyITE, args[0], evalLazy(args[1]), evalLazy(args[2]));
			}

		private:

			/**
			 * A utility method for inlining the evaluation of lazy functions.
			 */
			core::ExpressionPtr evalLazy(const core::ExpressionPtr& lazy) {

				core::NodeManager& manager = lazy->getNodeManager();

				core::FunctionTypePtr funType = core::dynamic_pointer_cast<const core::FunctionType>(lazy->getType());
				assert(funType && "Illegal lazy type!");

				// form call expression
				core::CallExprPtr call = core::CallExpr::get(manager, funType->getReturnType(), lazy, toVector<core::ExpressionPtr>());
				return core::transform::tryInlineToExpr(manager, call);
			}
		};

		core::NodePtr convertITE(core::NodeManager& manager, const core::NodePtr& code) {
			// the converter does the magic
			ITEConverter converter(manager);
			return converter.map(code);
		}



		// --------------------------------------------------------------------------------------------------------------



		class VectorToArrayConverter : public core::NodeMapping {

			/**
			 * A cache for converted call expressions - since each might be encountered multiple times.
			 */
			utils::map::PointerMap<core::NodePtr, core::NodePtr> resultCache;

		public:

			const core::NodePtr mapElement(unsigned index, const core::NodePtr& ptr) {
				// do not touch types ...
				if (ptr->getNodeCategory() == core::NC_Type) {
					return ptr;
				}

				// check result cache
				auto pos = resultCache.find(ptr);
				if (pos != resultCache.end()) {
					return pos->second;
				}

				// apply recursively - bottom up
				core::NodePtr res = ptr->substitute(ptr->getNodeManager(), *this, true);

				// handle calls
				if (ptr->getNodeType() == core::NT_CallExpr) {
					res = handleCallExpr(core::static_pointer_cast<const core::CallExpr>(res));
				}

				// handle declarations
				if (ptr->getNodeType() == core::NT_DeclarationStmt) {
					res = handleDeclarationStmt(core::static_pointer_cast<const core::DeclarationStmt>(res));
				}

				// handle rest
				resultCache.insert(std::make_pair(ptr, res));
				return res;
			}

		private:

			/**
			 * This method replaces vector arguments with vector2array conversions whenever necessary.
			 */
			core::CallExprPtr handleCallExpr(core::CallExprPtr call) {

				// extract node manager
				core::NodeManager& manager = call->getNodeManager();
				core::ASTBuilder builder(manager);
				const core::lang::BasicGenerator& basic = builder.getBasicGenerator();


				// check whether there is a argument which is a vector but the parameter is not
				const core::TypePtr& type = call->getFunctionExpr()->getType();
				assert(type->getNodeType() == core::NT_FunctionType && "Function should be of a function type!");
				const core::FunctionTypePtr& funType = core::static_pointer_cast<const core::FunctionType>(type);

				const core::TypeList& paramTypes = funType->getArgumentTypes();
				const core::ExpressionList& args = call->getArguments();

				if (paramTypes.size() != args.size()) {
					LOG(WARNING) << "Invalid call detected: " << call << " " << *(call->getFunctionExpr()->getType()) << " "
								<< join(", ", call->getArguments(), [](std::ostream& out, const core::ExpressionPtr& cur) { out << *(cur->getType()); });
					return call;
				}

				bool found = false;
				std::size_t size = args.size();
				for (std::size_t i = 0; !found && i < size; i++) {
					found = found || (args[i]->getType()->getNodeType() == core::NT_VectorType && paramTypes[i]->getNodeType() != core::NT_VectorType);
				}

				// check whether a vector / non-vector argument/parameter pair has been found
				if (!found) {
					// => no deduction required
					return call;
				}

				// derive type variable instantiation
				auto instantiation = core::analysis::getTypeVariableInstantiation(manager, call);
				if (!instantiation) {
					LOG(WARNING) << "Invalid call detected: " << call << " " << *(call->getFunctionExpr()->getType()) << " "
							<< join(", ", call->getArguments(), [](std::ostream& out, const core::ExpressionPtr& cur) { out << *(cur->getType()); });
					return call;
				}

				IRExtensions extensions(manager);
				if (*call->getFunctionExpr() == *extensions.lazyITE) {
					LOG(DEBUG) << "Lazy ITE encountered: " << *call << "  :=:  " << *instantiation;
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
						// conversion needed
						newArgs[i] = builder.callExpr((ref)?basic.getRefVector2RefArray():basic.getVector2Array(), newArgs[i]);
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

			/**
			 * This method replaces vector initialization values with vector2array conversions whenever necessary.
			 */
			core::DeclarationStmtPtr handleDeclarationStmt(core::DeclarationStmtPtr declaration) {

				// only important for array types
				core::TypePtr type = declaration->getVariable()->getType();
				if (type->getNodeType() != core::NT_ArrayType) {
					return declaration;
				}

				// get initialization value
				type = declaration->getInitialization()->getType();
				if (type->getNodeType() != core::NT_VectorType) {
					return declaration;
				}

				// extract some values from the declaration statement
				core::NodeManager& manager = declaration->getNodeManager();
				const core::VariablePtr& var = declaration->getVariable();
				const core::ExpressionPtr& oldInit = declaration->getInitialization();

				// construct a new init statement
				core::ExpressionPtr newInit = core::CallExpr::get(manager, var->getType(), manager.basic.getVector2Array(), toVector(oldInit));
				return core::DeclarationStmt::get(manager, declaration->getVariable(), newInit);
			}
		};


		core::NodePtr addImplicitVectorArrayCasts(core::NodeManager& manager, const core::NodePtr& code) {
			// the magic is done by the converter ...
			VectorToArrayConverter converter;
			return converter.map(code);
		}

	}

} // end: namespace transform
} // end: namespace simple_backend
} // end: namespace insieme

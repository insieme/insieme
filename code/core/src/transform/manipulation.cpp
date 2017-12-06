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

#include <algorithm>
#include <tuple>

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_cached_visitor.h"

#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/set_utils.h"

namespace insieme {
namespace core {
namespace transform {

	using namespace std;

	namespace us = insieme::utils::set;
	namespace um = insieme::utils::map;

	/**
	 * A utility function to apply an arbitrary manipulation to the statements within a compound statement.
	 *
	 * @param manager the manager used to create new nodes
	 * @param target the compound statement which should be manipulated
	 * @param manipulator the manipulation to be applied as a functor
	 * @return the root node of the modified AST tree (according to the root of the address)
	 */
	template <typename Manipulator>
	NodePtr manipulate(NodeManager& manager, const CompoundStmtAddress& target, Manipulator manipulator) {
		// get and manipulate statement list
		vector<StatementPtr> list = target.getAddressedNode()->getStatements();

		// apply manipulation
		manipulator(list);

		// create and apply replacement
		CompoundStmtPtr replacement = CompoundStmt::get(manager, list);
		return replaceNode(manager, target, replacement);
	}


	NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementPtr& statement, unsigned index) {
		// use generic manipulation function
		return manipulate(manager, target, [index, &statement](vector<StatementPtr>& list) {
			// limit index and insert element
			unsigned pos = min((unsigned)(list.size()), index);
			list.insert(list.begin() + pos, statement);
		});
	}

	NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementList& statements, unsigned index) {
		// use generic manipulation function
		return manipulate(manager, target, [index, &statements](vector<StatementPtr>& list) {
			// limit index and insert element
			unsigned pos = min((unsigned)(list.size()), index);
			list.insert(list.begin() + pos, statements.cbegin(), statements.cend());
		});
	}

	NodePtr append(NodeManager& manager, const CompoundStmtAddress& target, const StatementList& statements) {
		return insert(manager, target, statements, target->getChildList().size());
	}


	NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement) {
		// check whether target is within a compound statement
		if(auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress())) {
			return insert(manager, compoundParent, statement, target.getIndex());
		}

		IRBuilder build(manager);
		auto newCompound = build.compoundStmt(statement, target.getAddressedNode());
		return replaceNode(manager, target, newCompound);
	}

	NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementList& statements) {
		// check whether target is within a compound statement
		if(auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress())) {
			return insert(manager, compoundParent, statements, target.getIndex());
		}

		IRBuilder build(manager);
		StatementList allStatements;
		allStatements.insert(allStatements.begin(), statements.cbegin(), statements.cend());
		allStatements.push_back(target.getAddressedNode());
		auto newCompound = build.compoundStmt(allStatements);
		return replaceNode(manager, target, newCompound);
	}

	NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement) {
		auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
		if(compoundParent) {
			return insert(manager, compoundParent, statement, target.getIndex() + 1);
		} else {
			IRBuilder build(manager);
			auto newCompound = build.compoundStmt(target.getAddressedNode(), statement);
			return replaceNode(manager, target, newCompound);
		}
	}

	NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementList& statements) {
		auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
		if(compoundParent) {
			return insert(manager, compoundParent, statements, target.getIndex() + 1);
		} else {
			IRBuilder build(manager);
			StatementList allStatements;
			allStatements.push_back(target.getAddressedNode());
			allStatements.insert(allStatements.end(), statements.cbegin(), statements.cend());
			auto newCompound = build.compoundStmt(allStatements);
			return replaceNode(manager, target, newCompound);
		}
	}


	NodePtr remove(NodeManager& manager, const CompoundStmtAddress& target, unsigned index) {
		// use generic manipulation function
		return manipulate(manager, target, [index](vector<StatementPtr>& list) {
			// remove element
			assert_lt(index, list.size()) << "Index out of range!";
			list.erase(list.begin() + index);
		});
	}

	/// Remove the given list of statements, the NodeManger keeping track of removals.
	NodePtr remove(NodeManager& manager, const vector<StatementAddress>& stmts) {
		assert_false(stmts.empty()) << "List of statements to be removed must not be empty!";

		NodePtr res = stmts[0].getRootNode();

		// check whether the stmt list is not empty and all addresses have the same root.
		assert_false(stmts.empty()) << "Statements must not be empty!";
		assert(all(stmts, [&](const StatementAddress& cur) { return cur.getRootNode() == res; }));

		// create a sorted list of statements
		vector<StatementAddress> list = stmts;
		std::sort(list.begin(), list.end());

		// remove statements in reverse order (this does not effect earlier addresses)
		for_each(list.rbegin(), list.rend(), [&](StatementAddress cur) {

			// skip marker expressions (yes, they are used!!)
			while(cur.getParentAddress()->getNodeType() == NT_MarkerExpr) {
				cur = static_address_cast<StatementAddress>(cur.getParentAddress());
			}

			assert_eq(cur.getParentAddress()->getNodeType(), NT_CompoundStmt) << "Every stmt should be inside a compound stmt!";

			// update root of current stmt
			cur = cur.switchRoot(res);

			// remove current statement
			res = remove(manager, static_address_cast<CompoundStmtAddress>(cur.getParentAddress()), cur.getIndex());
		});

		return res;
	}

	NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementPtr& replacement) {
		// use generic manipulation function
		return manipulate(manager, target, [index, replacement](vector<StatementPtr>& list) {
			// remove element
			assert_lt(index, list.size()) << "Index out of range!";
			list[index] = replacement;
		});
	}

	NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementList& replacements) {
		// use generic manipulation function
		return manipulate(manager, target, [index, &replacements](vector<StatementPtr>& list) {
			// remove element
			assert_lt(index, list.size()) << "Index out of range!";
			list.erase(list.begin() + index);
			list.insert(list.begin() + index, replacements.cbegin(), replacements.cend());
		});
	}

	NodePtr move(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, int displacement) {
		// shortcut for no offset
		if(displacement == 0) { return target.getRootNode(); }

		// use generic manipulation function
		return manipulate(manager, target, [index, displacement](vector<StatementPtr>& list) {
			// check index
			assert_lt(index, list.size()) << "Index out of range!";

			// limit displacement
			int newPos = index + displacement;
			newPos = max(min((int)(list.size() - 1), newPos), 0);

			StatementPtr element = list[index];
			list.erase(list.begin() + index);
			list.insert(list.begin() + newPos, element);
		});
	}

	namespace {

		ExpressionPtr tryInlineBindToExpr(NodeManager& manager, const CallExprPtr& call) {
			IRBuilder builder(manager);

			// extract bind and call expression
			assert_eq(call->getFunctionExpr()->getNodeType(), NT_BindExpr) << "Illegal argument!";
			BindExprPtr bind = static_pointer_cast<const BindExpr>(call->getFunctionExpr());

			// process call recursively
			CallExprPtr innerCall = bind->getCall();
			ExpressionPtr inlined = tryInlineToExpr(manager, innerCall);

			// build intermediate lambda (result of bind)
			auto ingredients = transform::materialize({ bind->getParameters(), builder.returnStmt(inlined) });
			auto bindType = bind->getType().as<FunctionTypePtr>();
			auto funType = builder.functionType(bindType->getParameterTypes(), bindType->getReturnType());
			auto fun = builder.lambdaExpr(funType, ingredients.params, ingredients.body);
			auto intermediateCall = builder.callExpr(call->getType(), fun, call->getArgumentDeclarations());

			// try inlining the intermediate call
			auto res = tryInlineToExpr(manager, intermediateCall);

			// only if both steps worked, the bind could be inlined
			if(res != intermediateCall) {
				return res;
			}

			// fallback => stick to bind call
			return call;

		}

		ExpressionPtr tryInlineToExprInternal(NodeManager& manager, const CallExprPtr& call, bool inlineDerivedBuiltIns) {

			// Step 1 - get capture init and lambda expression
			ExpressionPtr target = call->getFunctionExpr();
			LambdaExprPtr lambda;

			// check for built-ins
			if(!inlineDerivedBuiltIns && core::lang::isDerived(target)) { return call; }

			// check for bind expression ...
			if(target->getNodeType() == NT_BindExpr) { return tryInlineBindToExpr(manager, call); }

			// check for lambda ...
			if(target->getNodeType() == NT_LambdaExpr) {
				lambda = static_pointer_cast<const LambdaExpr>(target);
			} else {
				// no in-lining possible
				return call;
			}

			// if recursive => inlining not possible
			if(lambda->isRecursive()) { return call; }

			// Step 2 - check body => has to be a return statement
			StatementPtr bodyStmt = lambda->getLambda()->getBody();

			while(CompoundStmtPtr compound = dynamic_pointer_cast<const CompoundStmt>(bodyStmt)) {
				const auto& stmts = compound->getStatements();
				if(stmts.size() == 1) {
					bodyStmt = stmts[0];
				} else {
					// no in-lining possible (to many statements)
					return call;
				}
			}

			// check for expression
			ExpressionPtr body;
			if(ReturnStmtPtr returnStmt = dynamic_pointer_cast<const ReturnStmt>(bodyStmt)) {
				body = returnStmt->getReturnExpr();
			} else if(bodyStmt->getNodeCategory() == core::NC_Expression) {
				// single expression => can also be inlined
				body = static_pointer_cast<const core::Expression>(bodyStmt);
			} else {
				// no in-lining possible (not a simple expression)
				return call;
			}

			// Step 3 - collect variables replacements
			std::set<std::size_t> accessedArguments;
			std::map<NodeAddress, NodePtr> replacements;

			bool success = true;
			const auto& params = lambda->getParameterList();
			auto& refExt = manager.getLangExtension<lang::ReferenceExtension>();
			IRBuilder builder(manager);
			visitDepthFirstPrunable(ExpressionAddress(body), [&](const NodeAddress& cur)->bool {

				// cut off nested lambdas
				if(cur.isa<LambdaExprPtr>()) return true; // prune

				auto handleParamAccess = [&](VariablePtr var) {
					// get position of parameter
					std::size_t idx = std::find(params.begin(), params.end(), var) - params.begin();

					// get the argument
					auto arg = call[idx];

					// added to accessed variables
					auto firstUsage = accessedArguments.insert(idx).second;

					// update success flag
					success &= (core::analysis::isSideEffectFree(arg) || firstUsage);

					// register replacement
					replacements[cur] = extractInitExprFromDecl(call[idx]);

					// if we have a ref -> ref parameter passing case, we need to deref again
					if(refExt.isCallOfRefDeref(cur) && replacements[cur].as<core::ExpressionPtr>()->getType() == var->getType()) {
						replacements[cur] = builder.deref(replacements[cur].as<core::ExpressionPtr>());
					}
				};

				// check if current node is a parameter access
				if(refExt.isCallOfRefDeref(cur)) {
					// check for parameters
					if(auto var = cur.as<CallExprPtr>()->getArgument(0).isa<VariablePtr>()) {
						if(::contains(params, var)) {
							handleParamAccess(var);
							// no more descent required here
							return true;
						}
					}
				} else if(auto var = cur.isa<VariablePtr>()) {
					if((lang::isCppReference(var) || lang::isCppRValueReference(var)) && ::contains(params, var)) {
						handleParamAccess(var);
						// no more descent required here
						return true;
					}
				}

				// otherwise => search deeper
				return false;
			});

			// get set of arguments with side effects
			std::set<std::size_t> argsWithSideEffects;
			for(std::size_t i = 0; i < call.size(); i++) {
				if (!analysis::isSideEffectFree(call[i])) {
					argsWithSideEffects.insert(i);
				}
			}

			// get set of accessed arguments with side effects
			std::set<std::size_t> accessedArgsWithSideEffects;
			for(const auto& arg : accessedArguments) {
				if (!analysis::isSideEffectFree(call[arg])) {
					accessedArgsWithSideEffects.insert(arg);
				}
			}

			// check whether parameters have only been accessed at most once
			if (!success || accessedArgsWithSideEffects != argsWithSideEffects) return call;

			// Step 4 - substitute variables within body
			return replacements.empty() ? body : replaceAll(manager, replacements).as<ExpressionPtr>();
		}
	}


	ExpressionPtr tryInlineToExpr(NodeManager& manager, const CallExprPtr& call, bool inlineDerivedBuiltIns, bool singleStep) {
		bool successful = true;
		ExpressionPtr res = call;
		while(successful && res->getNodeType() == NT_CallExpr) {
			ExpressionPtr tmp = tryInlineToExprInternal(manager, res.as<CallExprPtr>(), inlineDerivedBuiltIns);
			successful = (*tmp != *res);
			res = tmp;
			if(singleStep) { return res; }
		}
		return res;
	}

	StatementPtr tryInlineToStmt(NodeManager& manager, const CallExprPtr& callExpr, bool inlineDerivedBuiltIns) {
		// first use expression inlining
		ExpressionPtr res = tryInlineToExpr(manager, callExpr, inlineDerivedBuiltIns);
		if(res->getNodeType() != NT_CallExpr) {
			return res; // no more processing necessary
		}

		// call has to be a call to a lambda
		CallExprPtr call = static_pointer_cast<CallExprPtr>(res);
		if(call->getFunctionExpr()->getNodeType() != NT_LambdaExpr) {
			return call; // only known functions can be inlined
		}

		LambdaExprPtr fun = static_pointer_cast<LambdaExprPtr>(call->getFunctionExpr());
		if(fun->isRecursive() || !analysis::isOutlineAble(fun->getBody())) {
			return call; // recursive functions and free return / break / continue can not be supported
		}

		// do not touch derived built-ins if not explicitly requested
		if(!inlineDerivedBuiltIns && core::lang::isDerived(fun)) { return call; }

		// --- ok, some inline has to be done --
		IRBuilder builder(manager);

		// create substitution
		vector<StatementPtr> stmts;
		VariableMap varMap;

		// instantiate every variable within the parameter list with a fresh name
		const auto& params = fun->getParameterList().getElements();
		const auto& args = call->getArgumentDeclarations();

		assert_eq(params.size(), args.size()) << "Arguments do not fit parameters!!";

		for(std::size_t i = 0; i < params.size(); i++) {
			VariablePtr localVar = builder.variable(args[i]->getType());

			// copy value of parameter into a local variable
			stmts.push_back(builder.declarationStmt(args[i], localVar));

			// add to replacement map
			varMap[params[i]] = localVar;
		}

		// check whether all parameters are always dereferenced
		bool allParamsDerefed = true;
		auto deref = manager.getLangExtension<lang::ReferenceExtension>().getRefDeref();
		NodeMap derefReplacements;
		visitDepthFirstOncePrunable(NodeAddress(fun->getBody()), [&](const NodeAddress& cur) -> bool {
			// early exit ...
			if(!allParamsDerefed) return true;

			// prune nested lambdas
			if(cur.isa<LambdaExprPtr>()) return true;

			// check for parameters
			if(auto var = cur.isa<VariablePtr>()) {
				if(::contains(params, var)) {
					// if it is a parameter read ..
					if(analysis::isCallOf(cur.getParentAddress(2), deref)) {
						// .. replace it
						derefReplacements[cur.getParentNode(2)] = builder.deref(varMap[var]);
					} else {
						// invalid parameter access => no inlining possible
						allParamsDerefed = false;
					}
				}
			}

			// do not prune here
			return false;
		});

		// if there is a un-dereferenced parameter => no inlining supported
		if(!allParamsDerefed) return call;

		// add body of function to resulting inlined code
		for_each(fun->getBody()->getStatements(), [&](const core::StatementPtr& cur) { stmts.push_back(replaceAllGen(manager, cur, derefReplacements)); });

		// return compound stmt containing the entire stmt sequence
		return builder.compoundStmt(stmts);
	}


	namespace {

		class ConstantPropagator : public core::transform::CachedNodeMapping {
			NodeManager& manager;
			const ExpressionPtr target;
			const ExpressionPtr replacement;

		  public:
			ConstantPropagator(NodeManager& manager, const ExpressionPtr& target, const ExpressionPtr& replacement)
			    : manager(manager), target(target), replacement(replacement) {}

			LambdaExprPtr fixLambda(const LambdaExprPtr& original, const ExpressionList& args, ExpressionList& newArgs) {
				// start based on same lambda
				LambdaExprPtr lambda = original;

				// replace one parameter after another (back to front)
				std::size_t size = args.size();
				for(int i = size - 1; i >= 0; i--) {
					// check if it is the variable

					// try to fix the parameter for called function
					if(*args[i] == *target) {
						LambdaExprPtr newLambda = tryFixParameter(manager, lambda, i, replacement);
						if(*newLambda != *lambda) {
							// => nice, it worked!
							lambda = newLambda;
							continue;
						}
					}

					// exchange argument
					ExpressionPtr newArg = this->map(args[i]);
					newArgs.insert(newArgs.begin(), newArg);
				}

				// restore annotations
				utils::migrateAnnotations(original, lambda);
				utils::migrateAnnotations(original->getLambda(), lambda->getLambda());

				// return modified lambda
				return lambda;
			}

			const NodePtr resolveElement(const NodePtr& ptr) {
				// check for replacement (dereferenced parameter needs to be replaced)
				if(*ptr == *target) {
					return replacement;
				}

				// cut off types and stop if already unsuccessful
				if(ptr->getNodeCategory() == NC_Type) { return ptr; }

				// handle call expressions
				if(ptr->getNodeType() == NT_CallExpr) {
					CallExprPtr call = static_pointer_cast<const CallExpr>(ptr);
					if(::contains(call->getArgumentList(), target)) {
						const auto& args = call->getArgumentList();
						ExpressionList newArgs;

						bool resolved = false;

						// Push value through to sub-call ...
						ExpressionPtr fun = call->getFunctionExpr();
						if(fun->getNodeType() == NT_LambdaExpr) {
							// exchange function by fixing parameters within the lambda
							fun = fixLambda(fun.as<LambdaExprPtr>(), args, newArgs);
							resolved = true;

							// check whether it is a known literal
						} else if(analysis::isCallOf(fun, manager.getLangBasic().getPick())) {
							assert_eq(fun->getType()->getNodeType(), NT_FunctionType);

							// get list encapsulated options
							CallExprPtr pickCall = fun.as<CallExprPtr>();
							auto variants = encoder::toValue<vector<ExpressionPtr>>(pickCall->getArgument(0));

							if(all(variants, [](const ExpressionPtr& cur) { return cur->getNodeType() == NT_LambdaExpr; })) {
								// fix all lambdas offered to the pick
								for_each(variants, [&](ExpressionPtr& cur) {
									newArgs.clear(); // only update arguments once
									cur = fixLambda(cur.as<LambdaExprPtr>(), args, newArgs);
								});

								// replace function by customized pick call
								fun = IRBuilder(manager).pickVariant(variants);
								resolved = true;
							}
						} else {
							// process function definition
							fun = map(fun);
						}

						if(!resolved) {
							// cannot be pushed through ... just substitute within arguments and done
							::transform(args, std::back_inserter(newArgs), [&](const ExpressionPtr& cur) -> const ExpressionPtr {
								if(*cur == *target) { return replacement; }
								return this->map(cur);
							});
						}

						// create new call
						return CallExpr::get(manager, call->getType(), fun, newArgs);
					}
				}

				// handle lambda expressions ...
				if(ptr->getNodeType() == NT_LambdaExpr) {
					// .. end of replacement scope
					return ptr;
				}

				// else: process recursively
				return ptr->substitute(manager, *this);
			}
		};

		class ParameterFixer : public ConstantPropagator {
		public:
			ParameterFixer(NodeManager& manager, const VariablePtr& param, const ExpressionPtr& replacement)
				: ConstantPropagator(manager, IRBuilder(manager).deref(param), replacement) {}
		};

	}


	LambdaExprPtr tryFixParameter(NodeManager& manager, const LambdaExprPtr& lambda, unsigned index, const ExpressionPtr& value) {
		//	if (lambda->isRecursive()) {
		//		// use specialized function
		//		return tryFixRecursive(manager, lambda, index, value);
		//	}

		// do not touch derived operator definitions
		if(lang::isDerived(lambda)) { return lambda; }

		// check parameters
		const FunctionTypePtr& funType = lambda->getFunctionType();
		TypeList paramTypes = funType->getParameterTypes()->getTypes();
		assert_lt(index, paramTypes.size()) << "Index out of bound - no such parameter!";

		assert_true(types::isSubTypeOf(value->getType(), paramTypes[index])) << "Cannot substitute non-compatible value for specified parameter.";

		// make sure replacement value does not have any free variables except it is a variable itself (used for fixing recursive variables)
		assert_true((value->getNodeType() == NT_Variable || core::analysis::getFreeVariables(value).empty()))
		    << "Replacement value must not have free variables!";

		// conduct replacement

		const VariablePtr& param = lambda->getParameterList()[index];
		ParameterFixer fixer(manager, param, value);
		CompoundStmtPtr body = fixer.map(lambda->getBody());

		// create new function type
		paramTypes.erase(paramTypes.begin() + index);
		FunctionTypePtr newFunType = FunctionType::get(manager, paramTypes, funType->getReturnType(), funType->getKind());

		// create new lambda reference
		auto newLambdaRef = LambdaReference::get(manager, newFunType, lambda->getReference()->getName());

		// handle recursive functions
		if(lambda->isRecursive()) {
			if(lambda->getDefinition().size() > 1u) {
				assert_fail() << "Propagating parameters across mutual recursive functions not supported yet!";
				return lambda; // not supported yet
			}

			// collect all recursive calls (addresses to support a faster replacement later on)
			LambdaReferencePtr lambdaRef = lambda->getReference();
			vector<CallExprAddress> calls;
			visitDepthFirst(NodeAddress(body), [&](const CallExprAddress& call) {
				if(call.as<CallExprPtr>()->getFunctionExpr() == lambdaRef) { calls.push_back(call); }
			});

			// check whether parameter is propagated
			bool isPropagated = all(calls, [&](const CallExprPtr& call) -> bool {
				// check whether value is propagated along the recursion
				assert_lt(index, call->getNumArguments()) << "Invalid recursive call!";
				return call->getArgument(index) == value;
			});

			// value is not propagated along recursion
			if(!isPropagated) {
				// peel recursion once and use non-recursive parameter fixer
				return tryFixParameter(manager, lambda->peel(), index, value);
			}

			// update recursive variable

			// update recursive calls within body
			IRBuilder builder(manager);
			std::map<NodeAddress, NodePtr> replacements;
			for(const CallExprAddress& cur : calls) {
				// eliminate argument from call
				DeclarationList newArgs = cur.as<CallExprPtr>()->getArgumentDeclarations();
				newArgs.erase(newArgs.begin() + index);

				// build and register updated call
				CallExprPtr newCall = builder.callExpr(cur->getType(), newLambdaRef, newArgs);
				replacements.insert({cur, newCall});
			}

			// update body
			if(!replacements.empty()) { body = replaceAll(manager, replacements).as<CompoundStmtPtr>(); }
		}


		// create new parameter list
		vector<VariablePtr> params = lambda->getParameterList()->getParameters();
		params.erase(params.begin() + index);

		// build resulting lambda (preserving recursive variable ID)
		LambdaBindingMap bindings = { {newLambdaRef, Lambda::get(manager, newFunType, params, body)} };
		auto def = LambdaDefinition::get(manager, bindings);
		return LambdaExpr::get(manager, newLambdaRef, def);
	}

	NodePtr fixLambdaReference(NodeManager& manager, const NodePtr& node, const LambdaReferencePtr& ref, const ExpressionPtr& value) {
		return ConstantPropagator(manager, ref, value).map(node);
	}

	NodePtr fixParameter(NodeManager& manager, const NodePtr& node, const VariablePtr& param, const ExpressionPtr& value) {
		return ParameterFixer(manager, param, value).map(node);
	}

	CallExprPtr pushBindIntoLambda(NodeManager& manager, const CallExprPtr& call, unsigned index) {
		// ---------------- Pre-Conditions --------------------

		// check whether indexed parameter is in-deed a bind
		assert_lt(index, call->getNumArguments()) << "Invalid argument index!";
		assert_eq(call->getArgument(index)->getNodeType(), NT_BindExpr) << "Specified argument is not a bind!";

		// check whether function is in-deed a lambda
		assert_eq(call->getFunctionExpr()->getNodeType(), NT_LambdaExpr) << "Function has to be a lambda!";

		// so far it is only supported for non-recursive implementations
		assert(!call->getFunctionExpr().as<LambdaExprPtr>()->isRecursive() && "Not implemented for recursive functions!");

		// ----------------------------------------------------

		IRBuilder builder(manager);

		// get all free variables within the body of the lambda (those must not be used for new variables)
		LambdaExprPtr lambda = call->getFunctionExpr().as<LambdaExprPtr>();
		auto allVars = analysis::getAllVariables(lambda);

		// also consider variables within the bind node to be moved inside
		auto bindVars = analysis::getAllVariables(call->getArgument(index));
		allVars.insert(bindVars.begin(), bindVars.end());

		// get next free variable ID
		unsigned next_index = 0;
		for(const auto& cur : allVars) {
			next_index = std::max(next_index, cur->getId());
		}
		next_index++;

		// compute a map of free variables to
		vector<ExpressionPtr> newArgs;
		vector<VariablePtr> newParams;
		vector<ExpressionPtr> newBindCallArgs;

		// iterate through bound expressions => filter constants and others
		BindExprPtr bind = call->getArgument(index).as<BindExprPtr>();
		const VariableList bindParams = bind->getParameters().getParameters();
		CallExprPtr callWithinBind = bind->getCall();
		for(const ExpressionPtr& cur : callWithinBind->getArgumentList()) {
			// bind-parameters are just forwarded
			if(cur->getNodeType() == NT_Variable && contains(bindParams, cur.as<VariablePtr>())) {
				newBindCallArgs.push_back(cur);
				continue;
			}

			// this is a bound expression
			//  => if it is a constant it stays
			if(analysis::getFreeVariables(cur).empty()) {
				newBindCallArgs.push_back(cur);
				continue;
			}

			//  => otherwise it is evaluated and forwarded as a parameter
			VariablePtr newParam = builder.variable(builder.refType(cur->getType()), next_index++);
			newBindCallArgs.push_back(builder.deref(newParam));

			// add new parameter to list of parameters to be added to the lambda
			newParams.push_back(newParam);

			// add new argument to be passed by the call
			newArgs.push_back(cur);
		}

		// --- build up result ---

		// build new bind with modified call expression
		auto newBind = BindExpr::get(manager, bind->getType().as<FunctionTypePtr>(), bind->getParameters(),
		                             CallExpr::get(manager, callWithinBind->getType(), callWithinBind->getFunctionExpr(), newBindCallArgs));

		// fix parameter within body of lambda
		auto newBody = fixParameter(manager, lambda->getBody(), lambda->getParameterList()[index], newBind).as<StatementPtr>();

		// fix parameter list of lambda
		VariableList newLambdaParams = lambda->getParameterList()->getParameters();
		newLambdaParams.erase(newLambdaParams.begin() + index);                                      // remove parameter accepting bind
		newLambdaParams.insert(newLambdaParams.begin() + index, newParams.begin(), newParams.end()); // inject new parameters

		// build new lambda
		auto newLambda = builder.lambdaExpr(lambda->getFunctionType()->getReturnType(), newLambdaParams, newBody);

		// assemble new argument list
		ExpressionList newArgumentList = call->getArgumentList();
		newArgumentList.erase(newArgumentList.begin() + index);                                  // drop bind argument
		newArgumentList.insert(newArgumentList.begin() + index, newArgs.begin(), newArgs.end()); // inject new arguments

		// finally build new call expression
		return builder.callExpr(call->getType(), newLambda, newArgumentList);
	}


	// ------------------------------ lambda extraction -------------------------------------------------------------------

	CallExprPtr outline(NodeManager& manager, const StatementPtr& stmt, bool allowReturns) {
		// check whether it is allowed
		assert_true(analysis::isOutlineAble(stmt, allowReturns)) << "Cannot outline given code - it contains 'free' return, break or continue stmts.";

		// Obtain list of free variables
		VariableList free = analysis::getFreeVariables(manager.get(stmt));

		// sort to obtain stable results
		std::sort(free.begin(), free.end(), compare_target<VariablePtr>());

		// rename free variables within body using restricted scope
		IRBuilder builder(manager);
		um::PointerMap<VariablePtr, ExpressionPtr> replacements;
		VariableList parameters;
		ExpressionList arguments;
		TypeList paramTypes;
		for(const VariablePtr& cur : free) {
			auto curT = cur->getType();
			auto param = builder.variable(builder.refType(curT));
			replacements[cur] = builder.deref(param);
			// we don't want to wrap references to pointers into another level of reference
			if(lang::isReference(curT) && lang::isPointer(analysis::getReferencedType(curT))) {
				param = builder.variable(curT);
				replacements[cur] = param;
				paramTypes.push_back(analysis::getReferencedType(curT));
				arguments.push_back(builder.deref(cur));
			} else {
				paramTypes.push_back(curT);
				arguments.push_back(cur);
			}
			parameters.push_back(param);
		};
		StatementPtr body = replaceVarsGen(manager, stmt, replacements);

		// determine return type if necessary
		auto retType = manager.getLangBasic().getUnit();
		if(allowReturns) { retType = analysis::autoReturnType(manager, builder.compoundStmt(body)); }

		// create lambda accepting all free variables as arguments
		auto funType = builder.functionType(paramTypes, retType);
		LambdaExprPtr lambda = builder.lambdaExpr(funType, parameters, body);

		// create call to this lambda
		return builder.callExpr(retType, lambda, arguments);
	}

	CallExprPtr outline(NodeManager& manager, const ExpressionPtr& expr) {
		// Obtain list of free variables
		VariableList free = analysis::getFreeVariables(expr);

		// sort to obtain stable results
		std::sort(free.begin(), free.end(), compare_target<VariablePtr>());

		// rename free variables within body using restricted scope
		IRBuilder builder(manager);
		um::PointerMap<VariablePtr, VariablePtr> replacements;
		VariableList parameter;
		for_each(free, [&](const VariablePtr& cur) {
			auto var = builder.variable(cur->getType());
			replacements[cur] = var;
			parameter.push_back(var);
		});
		ExpressionPtr body = replaceVarsGen(manager, expr, replacements);

		// materialize parameters and body
		auto ingredients = core::transform::materialize({ parameter, builder.returnStmt(body) });
		auto funType = builder.functionType(extractTypes(parameter), body->getType());

		// create lambda accepting all free variables as arguments
		LambdaExprPtr lambda = builder.lambdaExpr(funType, ingredients.params, ingredients.body);

		// create call to this lambda
		return builder.callExpr(body->getType(), lambda, convertList<Expression>(free));
	}

	ExpressionPtr evalLazy(NodeManager& manager, const ExpressionPtr& lazy, bool evalDerivedOps) {
		// check type of lazy expression
		core::FunctionTypePtr funType = dynamic_pointer_cast<const core::FunctionType>(lazy->getType());
		assert_true(funType) << "Illegal lazy type!";

		// form call expression
		core::CallExprPtr call = core::CallExpr::get(manager, funType->getReturnType(), lazy, toVector<core::ExpressionPtr>());

		// evaluated call by inlining it
		return core::transform::tryInlineToExpr(manager, call, evalDerivedOps);
	}

	BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root) {
		// if it is already extracted, skip operation
		if(root->getNodeType() == NT_BindExpr) { return root.as<BindExprPtr>(); }

		// use standard outlining utility
		return extractLambda(manager, root, toVector<VariablePtr>());
	}

	BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root, const std::vector<VariablePtr>& passAsArguments) {
		IRBuilder build(manager);

		// outline statement
		CallExprPtr outlined = outline(manager, root);

		// create new parameter list
		VariableList params;
		um::PointerMap<VariablePtr, VariablePtr> replacements;
		for_each(passAsArguments, [&](const VariablePtr& cur) {
			auto newVar = build.variable(cur->getType());
			replacements[cur] = newVar;
			params.push_back(newVar);
		});

		// update parameter list within call
		outlined = replaceVarsGen(manager, outlined, replacements);

		// create bind expression exposing requested arguments
		return build.bindExpr(params, outlined);
	}

	LambdaExprPtr instantiate(NodeManager& manager, const LambdaExprPtr& lambda, const types::SubstitutionOpt& substitution) {
		// check for early exit
		if(!substitution || substitution->empty()) { return lambda; }

		assert_false(lambda->isRecursive()) << "I owe you the support for recursive functions!";

		// update type
		const FunctionTypePtr funType = static_pointer_cast<FunctionTypePtr>(substitution->applyTo(lambda->getType()));

		// update body
		CompoundStmtPtr body = static_pointer_cast<CompoundStmtPtr>(transform::replaceTypeVars(manager, lambda->getBody(), substitution));

		// update parameters
		VariableList params;
		VariableMap paramMap;
		for(const VariablePtr& var : lambda->getParameterList()->getParameters()) {
			// create new parameter
			auto newVar = Variable::get(manager, substitution->applyTo(var->getType()), var->getId());
			// collect new parameters
			params.push_back(newVar);
			paramMap[var] = newVar;
		}

		// update variables in body
		body = transform::replaceVarsGen(manager, body, paramMap);

		// construct result
		return LambdaExpr::get(manager, funType, params, body);
	}


	namespace {

		namespace detail {

			/**
			 * Adds a new parameter to the given lambda and returns a pair of the modified lambda and an expression to
			 * access the value of the new parameter within the lambda's body.
			 */
			std::pair<LambdaExprPtr, ExpressionPtr> addNewParameter(const LambdaExprPtr& lambda, const TypePtr& paramType) {
				assert_false(lambda->isRecursive()) << "Recursive functions not supported yet!\n" << "Lambda: " << *lambda << "\n";

				IRBuilder builder(lambda.getNodeManager());

				// find name for new parameter
				auto variablesInScope = core::analysis::getAllVariablesInScope(lambda->getLambda());

				auto paramVarType = builder.refType(paramType);
				VariablePtr newVar = builder.variable(paramVarType);
				while(contains(variablesInScope, newVar, [](const VariablePtr& a, const VariablePtr& b) { return a->getId() == b->getId(); })) {
					newVar = builder.variable(paramVarType);
				}

				// add new parameter and return new lambda
				VariableList params = lambda->getParameterList().getParameters();
				params.push_back(newVar);
				return std::make_pair(
					builder.lambdaExpr(lambda->getFunctionType()->getReturnType(), params, lambda->getBody()),
					builder.deref(newVar)
				);
			}
		}


		/**
		 * Pushes the given value from the root-node context of target to the target node by passing it
		 * to every lambda encountered along this path. If the value has to be restructured due to variable renaming
		 * in nested scopes, the parameter value will be updated.
		 * After the call value will reference the expression containing the pushed in value in the context of the target.
		 */
		NodeAddress makeAvailable(NodeManager& manager, const NodeAddress& target, ExpressionPtr& value) {
			// we are at the root node, no modification is necessary
			if(target.isRoot()) { return target; }

			// process recursively first (bring the value to the current scope
			NodeAddress parent = makeAvailable(manager, target.getParentAddress(), value);
			NodeAddress res = parent.getAddressOfChild(target.getIndex());

			// handle standard calls to lambdas
			if(parent.isa<CallExprPtr>() && parent.as<CallExprAddress>()->getFunctionExpr() == res) {
				// if this is a call to a lambda => add parameter (if not already there)
				if(parent.getNodeType() == NT_CallExpr) {
					CallExprPtr call = parent.as<CallExprPtr>();
					if(*call->getFunctionExpr() == *target) {
						// we are following the function call => add a parameter
						if(!contains(call.getArgumentList(), value)) {
							IRBuilder builder(manager);
							ExpressionList newArgs = call.getArgumentList();
							newArgs.push_back(value);

							// build new call (function is fixed within recursive step one level up)
							CallExprPtr newCall = builder.callExpr(call->getType(), call->getFunctionExpr(), newArgs);
							res = replaceAddress(manager, parent, newCall).getAddressOfChild(target.getIndex());
						}
					}
				}

				// if we are crossing a lambda, check whether new parameter needs to be accepted
				if(res.getNodeType() == NT_LambdaExpr) {
					assert_eq(res.getParentNode()->getNodeType(), NT_CallExpr) << "Parent of lambda needs to be a call!";
					assert(*res.getParentNode().as<CallExprPtr>()->getFunctionExpr() == *res && "Only directly invoked lambdas supported!");

					LambdaExprPtr lambda = res.as<LambdaExprPtr>();
					CallExprPtr call = res.getParentNode().as<CallExprPtr>();

					// check whether a new argument needs to be added
					if(call->getNumArguments() != lambda->getParameterList().size()) {
						// add new parameter to lambda
						LambdaExprPtr newLambda;
						std::tie(newLambda, value) = detail::addNewParameter(lambda, value->getType());
						res = replaceAddress(manager, res, newLambda);

					} else {
						// update variable when it is already passed as an argument
						auto args = call->getArgumentList();
						for(int i = args.size() - 1; i > 0; i--) {
							if(args[i] == value) {
								value = IRBuilder(manager).deref(lambda->getParameterList()[i]);
								break;
							}
						}
					}
				}

				// otherwise: if the result is a lambda expression but the parent is not a function call it => capture local variable
			} else if(auto lambda = res.isa<LambdaExprAddress>()) {
				// in this case we have something like
				//   bool.and(..., fun, ...)
				// where fun is the next step. We need to capture the local variable and forward it to fun by
				//   bool.and(..., (<args>)=>fun(<args>,var), ...)

				// extend lambda by an extra parameter (and update variable to be pushed)
				ExpressionPtr innerValue = value;
				LambdaExprPtr newLambda;
				std::tie(newLambda, innerValue) = detail::addNewParameter(lambda, value->getType());

				// build bind expression
				IRBuilder builder(lambda->getNodeManager());

				// - new list of arguments
				ExpressionList newArgs;
				for(const auto& cur : lambda->getParameterList()) {
					newArgs.push_back(cur);
				}
				newArgs.push_back(value);

				// - new function type
				auto funTypeIn = lambda->getFunctionType();
				auto funTypeOut = builder.functionType(funTypeIn->getParameterTypes(), funTypeIn->getReturnType(), FK_CLOSURE);

				// - actual bind
				auto bind = builder.bindExpr(funTypeOut, lambda->getParameterList().as<ParametersPtr>(), builder.callExpr(newLambda, newArgs));

				// this is passed as an argument, and thus wrapped in a declaration
				assert_true(res.getParentNode().isa<DeclarationPtr>());

				// we have to replace the declaration too
				auto decl = builder.declaration(materialize(bind->getType()),bind);

				// build address to result for next step
				res = replaceAddress(manager, res.getParentAddress(), decl);
				res = res.as<DeclarationAddress>()->getInitialization().as<BindExprAddress>()->getCall()->getFunctionExpr();

				// update propagated variable
				value = innerValue;

			}

			// for all others: just return result
			return res;
		}
	}

	ExpressionAddress pushInto(NodeManager& manager, const ExpressionAddress& target, const VariablePtr& var) {
		ExpressionPtr inner = var;
		NodeAddress inserted = makeAvailable(manager, target, inner);
		return replaceAddress(manager, inserted, inner).as<ExpressionAddress>();
	}

	vector<ExpressionAddress> pushInto(NodeManager& manager, const vector<ExpressionAddress>& targets, const VariablePtr& var) {
		// check whether there is something to do
		if(targets.empty()) { return vector<ExpressionAddress>(); }

		// special handling for a single step
		if(targets.size() == 1u) { return toVector(pushInto(manager, targets[0], var)); }

		// implant variables in reverse order of addresses (avoid invalid addresses in case targets are nested)
		vector<ExpressionAddress> sorted = targets;
		std::sort(sorted.rbegin(), sorted.rend());

		// implant variables - one by one
		NodePtr curRoot = targets[0].getRootNode();
		vector<ExpressionAddress> res;
		for(const auto& cur : targets) {
			res.push_back(pushInto(manager, cur.switchRoot(curRoot), var));
			curRoot = res.back().getRootNode();
		}

		// update all root nodes
		for(auto& cur : res) {
			cur = cur.switchRoot(curRoot);
		}

		// done
		return res;
	}

	NodePtr pushInto(NodeManager& manager, const map<ExpressionAddress, VariablePtr>& elements) {
		// some pre-conditions
		assert_false(elements.empty()) << "Elements must not be empty!";
		assert(all(elements,
		           [&](const pair<ExpressionAddress, VariablePtr>& cur) -> bool { return cur.first.getRootNode() == elements.begin()->first.getRootNode(); }));


		// apply push-in operation iteratively
		NodePtr root = elements.begin()->first.getRootNode();
		for(auto it = elements.rbegin(); it != elements.rend(); ++it) {
			const pair<ExpressionAddress, VariablePtr>& cur = *it;

			ExpressionAddress addr = cur.first.switchRoot(root);
			root = pushInto(manager, addr, cur.second).getRootNode();
		}

		// done
		return root;
	}


	LambdaExprPtr correctRecursiveLambdaVariableUsage(NodeManager& manager, const LambdaExprPtr& lambda) {
		// short-cut for non-recursive functions
		if(!lambda->isRecursive()) { return manager.get(lambda); }

		LambdaDefinitionPtr defs = lambda->getDefinition();
		LambdaDefinitionPtr res = defs;
		for(auto def : defs->getDefinitions()) {
			auto ref = def->getReference();
			res = fixLambdaReference(manager, res, ref, ref).as<LambdaDefinitionPtr>();
		}
		return IRBuilder(manager).lambdaExpr(lambda->getReference(), res);
	}

	namespace {

		StatementPtr convertJobToPforBody(const StatementPtr& body, const VariablePtr& id, const VariablePtr& size) {
			// - reduce the ID within every getThreadID and getThreadNum call by 1
			// - if ID was 0, replace it by a constant
			// - replace constant by pushing in the variables

			// reduce the getThreadID values
			IRBuilder builder(body->getNodeManager());
			const LiteralPtr idConstant = builder.literal(id->getType(), "____TEMPORARY_PFOR_BODY_ID");
			const LiteralPtr sizeConstant = builder.literal(id->getType(), "____TEMPORARY_PFOR_BODY_SIZE");

			StatementPtr res = makeCachedLambdaMapper([&](const NodePtr& cur) -> NodePtr {
				                   if(cur->getNodeType() != NT_CallExpr) return cur;

				                   // some preparation
				                   NodeManager& mgr = cur->getNodeManager();
				                   auto& parExt = mgr.getLangExtension<lang::ParallelExtension>();
				                   IRBuilder builder(mgr);
				                   auto call = cur.as<CallExprPtr>();

				                   // a function reducing the level expression by 1
				                   auto decLevel = [](const ExpressionPtr& level) -> ExpressionPtr {
					                   auto formula = arithmetic::toFormula(level);
					                   assert_true(formula.isConstant()) << "Accessing thread-group using non-constant level index not supported!";
					                   if(formula.isZero()) return ExpressionPtr();
					                   return arithmetic::toIR(level->getNodeManager(), formula - 1);
					               };

				                   // handle getThreadID
				                   if(analysis::isCallOf(call, parExt.getGetThreadId())) {
					                   if(ExpressionPtr newLevel = decLevel(call.getArgument(0))) { return builder.getThreadId(newLevel); }
					                   return idConstant;
				                   }

				                   // handle group size
				                   if(analysis::isCallOf(call, parExt.getGetGroupSize())) {
					                   if(ExpressionPtr newLevel = decLevel(call.getArgument(0))) { return builder.getThreadGroupSize(newLevel); }
					                   return sizeConstant;
				                   }

				                   return cur;
				               }).map(body);

			// collect all references to the constants ...
			set<NodeAddress> idConstants;
			set<NodeAddress> sizeConstants;
			visitDepthFirstPrunable(NodeAddress(res), [&](const NodeAddress& cur) -> bool {
				if(cur->getNodeCategory() == NC_Type) return true;
				if(*cur == *idConstant) idConstants.insert(cur);
				if(*cur == *sizeConstant) sizeConstants.insert(cur);
				return false;
			});

			// start replacement
			NodeManager& manager = body->getNodeManager();
			for(auto it = idConstants.rbegin(); it != idConstants.rend(); ++it) {
				res = pushInto(manager, it->switchRoot(res).as<ExpressionAddress>(), id).getRootNode().as<StatementPtr>();
			}
			for(auto it = sizeConstants.rbegin(); it != sizeConstants.rend(); ++it) {
				res = pushInto(manager, it->switchRoot(res).as<ExpressionAddress>(), size).getRootNode().as<StatementPtr>();
			}

			// done
			return res;
		}


		ExpressionPtr jobToPforBody(const ExpressionPtr& jobBody, const VariablePtr& numIterations) {
			NodeManager& mgr = jobBody->getNodeManager();
			IRBuilder builder(mgr);

			/**
			 * build something similar to:
			 * 		(a,b,c)=> for (i = a .. b : c ) { f(i); }
			 * where f is derived from the job body
			 */

			TypePtr iterType = mgr.getLangBasic().getUInt8();
			VariablePtr i = builder.variable(iterType);
			VariablePtr a = builder.variable(iterType);
			VariablePtr b = builder.variable(iterType);
			VariablePtr c = builder.variable(iterType);


			// replace getThreadID(0) / getThreadNum(0) => iterator / limit, reduce others by 1

			// start extracting body by evaluating the job-body
			StatementPtr body = evalLazy(mgr, jobBody);

			// replace getThreadID / getThreadNum by iterator / limit
			body = convertJobToPforBody(body, i, numIterations);

			// build resulting loop
			ForStmtPtr loop = builder.forStmt(i, a, b, c, body);

			// build bind and the rest
			return extractLambda(mgr, loop, {a, b, c});
		}
	}

	ExpressionPtr tryToPFor(const JobExprPtr& job) {
		static const ExpressionPtr fail;
		NodeManager& mgr = job.getNodeManager();

		// also, there must not be a re-distribute call => can not be supported
		if(analysis::contains(job->getBody(), mgr.getLangExtension<lang::ParallelExtension>().getRedistribute())) { return fail; }

		/**
		 * Converts a job of the format
		 *
		 * 		job[x-y](A) f
		 *
		 * into something of the shape
		 *
		 * 		()=> {
		 * 			A;
		 * 			pfor(0,x,1,f');
		 * 			barrier();
		 * 		}
		 *
		 * 	where f' is the a modified version of f
		 */

		// prepare utilities
		IRBuilder builder(mgr);

		StatementList stmts;

		// create pfor-call
		{
			auto range = job->getThreadNumRange();
			auto lowerBound = analysis::getArgument(range, 0);
			auto iterType = lowerBound->getType();

			// build up range
			ExpressionPtr start = builder.getZero(lowerBound->getType());
			ExpressionPtr step = builder.literal(iterType, "1");

			// create iterator variable
			VariablePtr threadID = builder.variable(start->getType());

			// create limit for iteration (into extra variable)
			DeclarationStmtPtr limitDecl = builder.declarationStmt(lowerBound);
			VariablePtr end = limitDecl->getVariable();

			stmts.push_back(limitDecl);

			// get job body
			ExpressionPtr jobBody = job->getBody();

			// create and add pfor call
			stmts.push_back(builder.pfor(jobToPforBody(jobBody, end), start, end, step));
		}

		// finish by adding the barrier
		stmts.push_back(builder.barrier());

		// build resulting function
		return extractLambda(mgr, builder.compoundStmt(stmts));
	}

	ExpressionPtr extractInitExprFromDecl(const DeclarationPtr& decl) {
		auto type = decl->getType();

		auto& refExt = decl->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		auto ret = transformBottomUpGen(decl->getInitialization(), [&refExt](const core::CallExprPtr& call) {
			if(refExt.isCallOfRefDecl(call)) {
				return core::lang::buildRefTemp(core::analysis::getReferencedType(call->getType())).as<core::CallExprPtr>();
			}
			return call;
		}, [](const core::NodePtr& node) {
			// we don't want to convert within deeper calls
			if(node.isa<CallExprPtr>()) return core::transform::ReplaceAction::Prune;
			return core::transform::ReplaceAction::Process;
		});
		return ret;
	}

	ExpressionList extractArgExprsFromCall(const NodePtr& call) {
		assert_eq(call->getNodeType(), NT_CallExpr);
		auto decls = call.as<core::CallExprPtr>()->getArgumentDeclarations();
		ExpressionList out;
		std::transform(decls.begin(), decls.end(), std::back_inserter(out), [](const DeclarationPtr& d) { return extractInitExprFromDecl(d); });
		return out;
	}

	ExpressionPtr extractArg(const NodePtr& call, size_t num) {
		assert_eq(call->getNodeType(), NT_CallExpr);
		return extractInitExprFromDecl(call.as<core::CallExprPtr>().getArgumentDeclaration(num));
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme

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

#include <algorithm>

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_cached_visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/set_utils.h"

#include "insieme/core/printer/pretty_printer.h"

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
template<typename Manipulator>
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
	return manipulate(manager, target, [index, &statement](vector<StatementPtr>& list){
		// limit index and insert element
		unsigned pos = min((unsigned)(list.size()), index);
		list.insert(list.begin() + pos, statement);
	});
}

NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementList& statements, unsigned index) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &statements](vector<StatementPtr>& list){
		// limit index and insert element
		unsigned pos = min((unsigned)(list.size()), index);
		list.insert(list.begin() + pos, statements.cbegin(), statements.cend());
	});
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
		return insert(manager, compoundParent, statement, target.getIndex()+1);
	} else {
		IRBuilder build(manager);
		auto newCompound = build.compoundStmt(target.getAddressedNode(), statement);
		return replaceNode(manager, target, newCompound);
	}
}

NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementList& statements) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
		return insert(manager, compoundParent, statements, target.getIndex()+1);
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
	return manipulate(manager, target, [index](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list.erase(list.begin() + index);
	});
}

NodePtr remove(NodeManager& manager, const vector<StatementAddress>& stmts) {

	assert(!stmts.empty() && "List of statements to be removed must not be empty!");

	NodePtr res = stmts[0].getRootNode();

	// check whether the stmt list is not empty and all addresses have the same root.
	assert(!stmts.empty() && "Statements must not be empty!");
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

		assert(cur.getParentAddress()->getNodeType() == NT_CompoundStmt && "Every stmt should be inside a compound stmt!");

		// update root of current stmt
		cur = cur.switchRoot(res);

		// remove current statement
		res = remove(manager, static_address_cast<CompoundStmtAddress>(cur.getParentAddress()), cur.getIndex());
	});

	return res;
}

NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementPtr& replacement) {
	// use generic manipulation function
	return manipulate(manager, target, [index, replacement](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list[index] = replacement;
	});
}

NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementList& replacements) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &replacements](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list.erase(list.begin() + index);
		list.insert(list.begin() + index, replacements.cbegin(), replacements.cend());
	});
}

NodePtr move(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, int displacement) {

	// shortcut for no offset
	if (displacement == 0) {
		return target.getRootNode();
	}

	// use generic manipulation function
	return manipulate(manager, target, [index, displacement](vector<StatementPtr>& list){
		// check index
		assert( index < list.size() && "Index out of range!");

		// limit displacement
		int newPos = index + displacement;
		newPos = max(min((int)(list.size()-1), newPos), 0);

		StatementPtr element = list[index];
		list.erase(list.begin() + index);
		list.insert(list.begin() + newPos, element);
	});
}

namespace {

	class InlineSubstituter : public NodeMapping {

		bool successful;

		um::PointerMap<VariablePtr, ExpressionPtr>& replacements;
		us::PointerSet<VariablePtr> replacedOnce;

	public:

		InlineSubstituter(um::PointerMap<VariablePtr, ExpressionPtr>& replacements)
			: successful(true), replacements(replacements) { }

		const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if (!successful) {
				return ptr;
			}

			if (ptr->getNodeType() != NT_Variable) {
				if (ptr->getNodeType() == NT_LambdaExpr) {
					// entering new scope => ignore this
					return ptr;
				}
				return ptr->substitute(ptr->getNodeManager(), *this);
			}

			const VariablePtr& var = ptr.as<VariablePtr>();

			// check whether variable has been already encountered
			if (replacedOnce.find(var) != replacedOnce.end()) {
				// variable may only be replaced once!
				successful = false;
				return ptr;
			}

			auto pos = replacements.find(var);
			if (pos != replacements.end()) {
				ExpressionPtr res = pos->second;

				// to ensure a single evaluation!
				if (!isSideEffectFree(res)) {
					replacedOnce.insert(var);
				}
				return res;
			}

			// no modification required
			return ptr;
		}

		bool wasSuccessful() const {
			return successful;
		}

		static bool isSideEffectFree(const ExpressionPtr& expr) {
			// all variables and literals are side-effect free accessible
			NodeType type = expr->getNodeType();
			if (type == NT_Variable || type == NT_Literal) {
				return true;
			}

			// check for other operations
			if (type != NT_CallExpr) {
				return false;
			}

			// check whether function is side-effect free + all arguments are
			CallExprPtr call = expr.as<CallExprPtr>();
			auto& basic = expr->getNodeManager().getLangBasic();
			return basic.isPure(call->getFunctionExpr()) &&
					all(call->getArguments(), &isSideEffectFree);
		}
	};




	ExpressionPtr tryInlineBindToExpr(NodeManager& manager, const CallExprPtr& call) {

		// extract bind and call expression
		assert(call->getFunctionExpr()->getNodeType() == NT_BindExpr && "Illegal argument!");
		BindExprPtr bind = static_pointer_cast<const BindExpr>(call->getFunctionExpr());

		// process call recursively
		CallExprPtr innerCall = bind->getCall();
		ExpressionPtr inlined = tryInlineToExpr(manager, innerCall);

		// check for matching argument number
		auto parameter = bind->getParameters();
		auto arguments = call->getArguments();
		if (parameter.size() != arguments.size()) {
			return call;
		}

		// substituted call arguments with bind parameters
		um::PointerMap<VariablePtr, ExpressionPtr> replacements;

		replacements.insert(
				make_paired_iterator(parameter.begin(), arguments.begin()),
				make_paired_iterator(parameter.end(), arguments.end())
		);

		// substitute variables within body
		InlineSubstituter substituter(replacements);
		ExpressionPtr res = static_pointer_cast<const Expression>(substituter.mapElement(0, inlined));

		// check result
		if (!substituter.wasSuccessful()) {
			return call;
		}
		return res;
	}

	ExpressionPtr tryInlineToExprInternal(NodeManager& manager, const CallExprPtr& call) {

		// Step 1 - get capture init and lambda expression
		ExpressionPtr target = call->getFunctionExpr();
		LambdaExprPtr lambda;

		// check for bind expression ...
		if (target->getNodeType() == NT_BindExpr) {
			return tryInlineBindToExpr(manager, call);
		}

		// check for lambda ...
		if (target->getNodeType() == NT_LambdaExpr) {
			lambda = static_pointer_cast<const LambdaExpr>(target);
		} else {
			// no in-lining possible
			return call;
		}

		// if recursive => inlining not possible
		if (lambda->isRecursive()) {
			return call;
		}

		// Step 2 - check body => has to be a return statement
		StatementPtr bodyStmt = lambda->getLambda()->getBody();

		while (CompoundStmtPtr compound = dynamic_pointer_cast<const CompoundStmt>(bodyStmt)) {
			const auto& stmts = compound->getStatements();
			if (stmts.size() == 1) {
				bodyStmt = stmts[0];
			} else {
				// no in-lining possible (to many statements)
				return call;
			}
		}

		// check for expression
		ExpressionPtr body;
		if (ReturnStmtPtr returnStmt = dynamic_pointer_cast<const ReturnStmt>(bodyStmt)) {
			body = returnStmt->getReturnExpr();
		} else if (bodyStmt->getNodeCategory() == core::NC_Expression){
			// single expression => can also be inlined
			body = static_pointer_cast<const core::Expression>(bodyStmt);
		} else {
			// no in-lining possible (not a simple expression)
			return call;
		}

		// Step 3 - collect variables replacements
		const ParametersPtr& paramList = lambda->getParameterList();

		um::PointerMap<VariablePtr, ExpressionPtr> replacements;

		// add call parameters
		int index = 0;
		::for_each(call->getArguments(), [&](const ExpressionPtr& cur) {
			replacements.insert(std::make_pair(paramList[index++], cur));
		});

		// Step 4 - substitute variables within body
		InlineSubstituter substituter(replacements);
		ExpressionPtr res = substituter.mapElement(0, body).as<ExpressionPtr>();

		// check result
		if (substituter.wasSuccessful()) {
			return res;
		}
		return call;
	}

}


ExpressionPtr tryInlineToExpr(NodeManager& manager, const CallExprPtr& call) {

	bool successful = true;
	ExpressionPtr res = call;
	while(successful && res->getNodeType() == NT_CallExpr) {
		ExpressionPtr tmp = tryInlineToExprInternal(manager, res.as<CallExprPtr>());
		successful = (*tmp != *res);
		res = tmp;
	}
	return res;
}

StatementPtr tryInlineToStmt(NodeManager& manager, const CallExprPtr& callExpr) {

	// first use expression inlining
	ExpressionPtr res = tryInlineToExpr(manager, callExpr);
	if (res->getNodeType() != NT_CallExpr) {
		return res;		// no more processing necessary
	}

	// call has to be a call to a lambda
	CallExprPtr call = static_pointer_cast<CallExprPtr>(res);
	if (call->getFunctionExpr()->getNodeType() != NT_LambdaExpr) {
		return call;	// only known functions can be inlined
	}

	LambdaExprPtr fun = static_pointer_cast<LambdaExprPtr>(call->getFunctionExpr());
	if (fun->isRecursive() || !isOutlineAble(fun->getBody())) {
		return call;	// recursive functions and free return / break / continue can not be supported
	}

	// --- ok, some inline has to be done --

	IRBuilder builder(manager);

	// create substitution
	vector<StatementPtr> stmts;
	VariableMap varMap;

	// instantiate very variable within the parameter list with a fresh name
	const auto& params = fun->getParameterList().getElements();
	const auto& args = call->getArguments();

	assert(params.size() == args.size() && "Arguments do not fit parameters!!");

	for(std::size_t i =0; i<params.size(); i++) {

		VariablePtr localVar;
		if (args[i]->getNodeType() == NT_Variable) {
			// passing a pure variable => use variable everywhere
			localVar = static_pointer_cast<VariablePtr>(args[i]);
		} else {
			// temporary value is passed => create temporary variable
			localVar = builder.variable(args[i]->getType());

			// copy value of parameter into a local variable
			stmts.push_back(builder.declarationStmt(localVar, args[i]));
		}

		// add to replacement map
		varMap[params[i]] = localVar;
	}

	// add body of function to resulting inlined code
	for_each(fun->getBody()->getStatements(), [&](const core::StatementPtr& cur) {
		stmts.push_back(replaceVarsGen(manager, cur, varMap));
	});

	// return compound stmt containing the entire stmt sequence
	return builder.compoundStmt(stmts);
}


namespace {

	class ParameterFixer : public core::transform::CachedNodeMapping {

		NodeManager& manager;
		const VariablePtr var;
		const ExpressionPtr replacement;

	public:

		ParameterFixer(NodeManager& manager, const VariablePtr& var, const ExpressionPtr& replacement)
			: manager(manager), var(var), replacement(replacement) {}

		LambdaExprPtr fixLambda(const LambdaExprPtr& original, const ExpressionList& args, ExpressionList& newArgs) {

			// start based on same lambda
			LambdaExprPtr lambda = original;

			// replace one parameter after another (back to front)
			std::size_t size = args.size();
			for (int i = size-1; i>=0; i--) {
				// check if it is the variable

				// try to fix the parameter for called function
				if (*args[i] == *var) {
					LambdaExprPtr newLambda = tryFixParameter(manager, lambda, i, replacement);
					if (*newLambda != *lambda) {
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
			// check for replacement
			if (*ptr == *var) {
				return replacement;
			}

			// cut off types and stop if already unsuccessful
			if (ptr->getNodeCategory() == NC_Type) {
				return ptr;
			}

			// handle call expressions
			if (ptr->getNodeType() == NT_CallExpr) {
				CallExprPtr call = static_pointer_cast<const CallExpr>(ptr);
				if (::contains(call->getArguments(), var)) {

					const auto& args = call->getArguments();
					ExpressionList newArgs;

					bool resolved = false;

					// Push value through to sub-call ...
					ExpressionPtr fun = call->getFunctionExpr();
					if (fun->getNodeType() == NT_LambdaExpr) {

						// exchange function by fixing parameters within the lambda
						fun = fixLambda(fun.as<LambdaExprPtr>(), args, newArgs);
						resolved = true;

					// check whether it is a known literal
					} else if (analysis::isCallOf(fun, manager.getLangBasic().getPick())) {
						assert(fun->getType()->getNodeType() == NT_FunctionType);

						// get list encapsulated options
						CallExprPtr pickCall = fun.as<CallExprPtr>();
						auto variants = encoder::toValue<vector<ExpressionPtr>>(pickCall->getArgument(0));

						if (all(variants, [](const ExpressionPtr& cur) { return cur->getNodeType() == NT_LambdaExpr; })) {

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

					if (!resolved) {
						// cannot be pushed through ... just substitute within arguments and done
						::transform(args, std::back_inserter(newArgs), [&](const ExpressionPtr& cur)->const ExpressionPtr {
							if (*cur == *var) {
								return replacement;
							}
							return this->map(cur);
						});

					}

					// create new call
					return CallExpr::get(manager, call->getType(), fun, newArgs);
				}
			}

			// handle lambda expressions ...
			if (ptr->getNodeType() == NT_LambdaExpr) {
				// .. end of replacement scope
				return ptr;
			}

			// else: process recursively
			return ptr->substitute(manager, *this);

		}

	};



//	LambdaExprPtr tryFixRecursive(NodeManager& manager, const LambdaExprPtr& lambda, unsigned index, const ExpressionPtr& value) {
//		assert(lambda->isRecursive());
//
//		/**
//		 * The following procedure is applied:
//		 * 		- the parameter is fixed within the body of the lambda
//		 * 		- all recursive calls are searched within the lambda
//		 * 		- if value is passed along at the same position, unmodified
//		 * 			=> remove parameter from recursion
//		 * 		  otherwise: unroll once, fix parameter for non-recursive function
//		 */
//
//		// check for compatible target type
//		FunctionTypePtr funType = lambda->getFunctionType();
//		TypeList paramTypes = funType->getParameterTypeList();
//		assert(index < paramTypes.size() && "Index out of bound - no such parameter!");
//		assert(types::isSubTypeOf(value->getType(), paramTypes[index]) && "Cannot substitute non-compatible value for specified parameter.");
//
//		// replace parameter within body
//		const VariablePtr& param = lambda->getParameterList()[index];
//		CompoundStmtPtr newBody = fixVariable(manager, lambda->getBody(), param, value).as<CompoundStmtPtr>();
//
//		// check whether it can be propagated over the recursion
//		//  - search for recursive calls in new body
//		auto recVar = lambda->getVariable();
//
//
//
//		// value is propagated => eliminate parameter in recursion
//
//		// Step 1) fix recursive variable
//
//		// Step 1) fix body by replacing all calls with fixed calls
//		std::map<NodeAddress, NodePtr> replacements;
//		for(const CallExprAddress& cur : calls) {
//
//
//
//			replacements.insert({cur, newCall});
//		}
//
//
//		bool canPropagate = all();
//
//
//		return lambda;
//	}

}


LambdaExprPtr tryFixParameter(NodeManager& manager, const LambdaExprPtr& lambda, unsigned index, const ExpressionPtr& value) {

//	if (lambda->isRecursive()) {
//		// use specialized function
//		return tryFixRecursive(manager, lambda, index, value);
//	}

	// check parameters
	const FunctionTypePtr& funType = lambda->getFunctionType();
	TypeList paramTypes = funType->getParameterTypes()->getTypes();
	assert(index < paramTypes.size() && "Index out of bound - no such parameter!");

	assert(types::isSubTypeOf(value->getType(), paramTypes[index]) && "Cannot substitute non-compatible value for specified parameter.");

	// make sure replacement value does not have any free variables except it is a variable itself (used for fixing recursive variables)
	assert((value->getNodeType() == NT_Variable || core::analysis::getFreeVariables(value).empty()) && "Replacement value must not have free variables!");

	// conduct replacement

	const VariablePtr& param = lambda->getParameterList()[index];
	ParameterFixer fixer(manager, param, value);
	CompoundStmtPtr body = fixer.map(lambda->getBody());

	// create new function type
	paramTypes.erase(paramTypes.begin() + index);
	FunctionTypePtr newFunType = FunctionType::get(manager, paramTypes, funType->getReturnType(), funType->getKind());

	// create new recursive variable
	auto newRecVar = Variable::get(manager, newFunType, lambda->getVariable()->getId());

	// handle recursive functions
	if (lambda->isRecursive()) {

		if(lambda->getDefinition().size() > 1u) {
			assert(false && "Propagating parameters across mutual recursive functions not supported yet!");
			return lambda; // not supported yet
		}

		// collect all recursive calls (addresses to support a faster replacement later on)
		VariablePtr recVar = lambda->getVariable();
		vector<CallExprAddress> calls;
		visitDepthFirst(NodeAddress(body), [&](const CallExprAddress& call){
			if (call.as<CallExprPtr>()->getFunctionExpr() == recVar) calls.push_back(call);
		});

		// check whether parameter is propagated
		bool isPropagated = all(calls, [&](const CallExprPtr& call)->bool {
			// check whether value is propagated along the recursion
			assert(index < call->getArguments().size() && "Invalid recursive call!");
			return call->getArgument(index) == value;
		});

		// value is not propagated along recursion
		if (!isPropagated) {
			// peel recursion once and use non-recursive parameter fixer
			return tryFixParameter(manager, lambda->peel(), index, value);
		}

		// update recursive variable


		// update recursive calls within body
		IRBuilder builder(manager);
		std::map<NodeAddress, NodePtr> replacements;
		for(const CallExprAddress& cur : calls) {
			// eliminate argument from call
			ExpressionList newArgs = cur.as<CallExprPtr>()->getArguments();
			newArgs.erase(newArgs.begin() + index);


			// build and register updated call
			CallExprPtr newCall = builder.callExpr(cur->getType(), newRecVar, newArgs);
			replacements.insert({cur, newCall});
		}

		// update body
		if(!replacements.empty()) {
			body = replaceAll(manager, replacements).as<CompoundStmtPtr>();
		}
	}



	// create new parameter list
	vector<VariablePtr> params = lambda->getParameterList()->getParameters();
	params.erase(params.begin() + index);

	// build resulting lambda (preserving recursive variable ID)
	auto binding = LambdaBinding::get(manager, newRecVar, Lambda::get(manager, newFunType, params, body));
	auto def = LambdaDefinition::get(manager, toVector(binding));
	return LambdaExpr::get(manager, newRecVar, def);
}

NodePtr fixVariable(NodeManager& manager, const NodePtr& node, const VariablePtr& var, const ExpressionPtr& value) {
	ParameterFixer fixer(manager, var, value);
	return fixer.map(node);
}


CallExprPtr pushBindIntoLambda(NodeManager& manager, const CallExprPtr& call, unsigned index) {

	// ---------------- Pre-Conditions --------------------

	// check whether indexed parameter is in-deed a bind
	assert(index < call->getArguments().size() && "Invalid argument index!");
	assert(call->getArgument(index)->getNodeType() == NT_BindExpr && "Specified argument is not a bind!");

	// check whether function is in-deed a lambda
	assert(call->getFunctionExpr()->getNodeType() == NT_LambdaExpr && "Function has to be a lambda!");

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
	for(const ExpressionPtr& cur : callWithinBind->getArguments()) {

		// bind-parameters are just forwarded
		if (cur->getNodeType() == NT_Variable && contains(bindParams, cur.as<VariablePtr>())) {
			newBindCallArgs.push_back(cur);
			continue;
		}

		// this is a bound expression
		//  => if it is a constant it stays
		if (analysis::getFreeVariables(cur).empty()) {
			newBindCallArgs.push_back(cur);
			continue;
		}

		//  => otherwise it is evaluated and forwarded as a parameter
		VariablePtr newParam = builder.variable(cur->getType(), next_index++);
		newBindCallArgs.push_back(newParam);

		// add new parameter to list of parameters to be added to the lambda
		newParams.push_back(newParam);

		// add new argument to be passed by the call
		newArgs.push_back(cur);
	}

	// --- build up result ---

	// build new bind with modified call expression
	auto newBind = BindExpr::get(manager, bind->getType().as<FunctionTypePtr>(), bind->getParameters(),
			CallExpr::get(manager, callWithinBind->getType(), callWithinBind->getFunctionExpr(), newBindCallArgs)
	);

	// fix parameter within body of lambda
	auto newBody = fixVariable(manager, lambda->getBody(), lambda->getParameterList()[index], newBind);

	// fix parameter list of lambda
	VariableList newLambdaParams = lambda->getParameterList()->getParameters();
	newLambdaParams.erase(newLambdaParams.begin() + index); // remove parameter accepting bind
	newLambdaParams.insert(newLambdaParams.begin() + index, newParams.begin(), newParams.end()); // inject new parameters

	// build new lambda
	auto newLambda = builder.lambdaExpr(lambda->getFunctionType()->getReturnType(), newBody, newLambdaParams);

	// assemble new argument list
	ExpressionList newArgumentList = call->getArguments();
	newArgumentList.erase(newArgumentList.begin() + index); // drop bind argument
	newArgumentList.insert(newArgumentList.begin() + index, newArgs.begin(), newArgs.end()); // inject new arguments

	// finally build new call expression
	return builder.callExpr(call->getType(), newLambda, newArgumentList);
}


// ------------------------------ lambda extraction -------------------------------------------------------------------


bool hasFreeControlStatement(const StatementPtr& stmt, NodeType controlStmt, const vector<NodeType>& pruneStmts) {
	bool hasFree = false;
	visitDepthFirstOncePrunable(stmt, [&](const NodePtr& cur) {
		auto curType = cur->getNodeType();
		if(::contains(pruneStmts, curType)) {
			return true; // do not descent here
		}
		if(cur->getNodeType() == controlStmt) {
			hasFree = true;	// "bound" stmt found
		}
		return hasFree;
	});
	return hasFree;
}

bool isOutlineAble(const StatementPtr& stmt) {

	// the statement must not contain a "free" return
	bool hasFreeReturn = false;
	visitDepthFirstOncePrunable(stmt, [&](const NodePtr& cur) {
		if (cur->getNodeType() == NT_LambdaExpr) {
			return true; // do not decent here
		}
		if (cur->getNodeType() == NT_ReturnStmt) {
			hasFreeReturn = true;	// "bound" return found
		}
		return hasFreeReturn;
	});

	if (hasFreeReturn) {
		return false;
	}

	// search for "bound" break or continue statements
	vector<NodeType> pruneStmts;
	pruneStmts.push_back(NT_ForStmt);
	pruneStmts.push_back(NT_WhileStmt);
	bool hasFreeBreakOrContinue = hasFreeControlStatement(stmt, NT_BreakStmt, pruneStmts) ||	hasFreeControlStatement(stmt, NT_ContinueStmt, pruneStmts);

	return !hasFreeBreakOrContinue;
}


CallExprPtr outline(NodeManager& manager, const StatementPtr& stmt) {
	// check whether it is allowed
	assert(isOutlineAble(stmt) && "Cannot outline given code - it contains 'free' return, break or continue stmts.");

	// FIXME: no one cares at all about this
	//std::cout << "YOU ARE USING OUTLINE, we experienced some errors with this, try to use builder.createCallExprFromBody " << std::endl;

	// Obtain list of free variables
	VariableList free = analysis::getFreeVariables(manager.get(stmt));

	// sort to obtain stable results
	std::sort(free.begin(), free.end(), compare_target<VariablePtr>());

	// rename free variables within body using restricted scope
	IRBuilder builder(manager);
	um::PointerMap<VariablePtr, VariablePtr> replacements;
	VariableList parameter;
	for_each(free,[&](const VariablePtr& cur) {
		auto var = builder.variable(cur->getType());
		replacements[cur] = var;
		parameter.push_back(var);
	});
	StatementPtr body = replaceVarsGen(manager, stmt, replacements);

	// create lambda accepting all free variables as arguments
	LambdaExprPtr lambda = builder.lambdaExpr(body, parameter);

	// create call to this lambda
	return builder.callExpr(manager.getLangBasic().getUnit(), lambda, convertList<Expression>(free));
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
	for_each(free,[&](const VariablePtr& cur) {
		auto var = builder.variable(cur->getType());
		replacements[cur] = var;
		parameter.push_back(var);
	});
	ExpressionPtr body = replaceVarsGen(manager, expr, replacements);

	// create lambda accepting all free variables as arguments
	LambdaExprPtr lambda = builder.lambdaExpr(body->getType(), builder.returnStmt(body), parameter);

	// create call to this lambda
	return builder.callExpr(body->getType(), lambda, convertList<Expression>(free));
}

ExpressionPtr evalLazy(NodeManager& manager, const ExpressionPtr& lazy) {

	// check type of lazy expression
	core::FunctionTypePtr funType = dynamic_pointer_cast<const core::FunctionType>(lazy->getType());
	assert(funType && "Illegal lazy type!");

	// form call expression
	core::CallExprPtr call = core::CallExpr::get(manager, funType->getReturnType(), lazy, toVector<core::ExpressionPtr>());

	// evaluated call by inlining it
	return core::transform::tryInlineToExpr(manager, call);
}

BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root) {

	// if it is already extracted, skip operation
	if (root->getNodeType() == NT_BindExpr) {
		return root.as<BindExprPtr>();
	}

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

LambdaExprPtr privatizeVariables(NodeManager& manager, const LambdaExprPtr& root, const std::vector<VariablePtr>& varsToPrivatize) {
	
	auto body = root->getBody();

	IRBuilder build(manager);
	um::PointerMap<NodePtr, NodePtr> replacements;

	for_each(varsToPrivatize, [&](VariablePtr p) {
		auto var = build.variable(p->getType());
		replacements[p] = var;
	});
	auto newBody = replaceAll(manager, body, replacements);

	return dynamic_pointer_cast<const LambdaExpr>(newBody);
}

LambdaExprPtr instantiate(NodeManager& manager, const LambdaExprPtr& lambda, const types::SubstitutionOpt& substitution) {

	// check for early exit
	if (!substitution || substitution->empty()) {
		return lambda;
	}

	assert(!lambda->isRecursive() && "I owe you the support for recursive functions!");

	// update type
	const FunctionTypePtr funType = static_pointer_cast<FunctionTypePtr>(substitution->applyTo(lambda->getType()));

	// update body
	const CompoundStmtPtr body = static_pointer_cast<CompoundStmtPtr>(transform::replaceTypeVars(manager, lambda->getBody(), substitution));

	// update parameters
	VariableList params;
	::transform(lambda->getParameterList()->getParameters(), std::back_inserter(params), [&](const VariablePtr& cur)->VariablePtr {
		TypePtr newType = substitution->applyTo(cur->getType());
		if (newType == cur->getType()) {
			return cur;
		}
		return Variable::get(manager, newType, cur->getId());
	});

	// construct result
	return LambdaExpr::get(manager, funType, params, body);
}

DeclarationStmtPtr createGlobalStruct(NodeManager& manager, ProgramPtr& prog, const NamedValueList& globals) {
	//if(!prog->isMain()) {
	//	LOG(WARNING) << "createGlobalStruct called on non-main program.";
	//}

	LambdaExprPtr lambda = prog[0].as<LambdaExprPtr>(); //dynamic_pointer_cast<const LambdaExpr>(prog->getElement(0));
	auto compound = lambda->getBody();
	auto addr = CompoundStmtAddress::find(compound, prog);
	IRBuilder build(manager);

	// generate type list from initialization expression list in "globals"
	NamedTypeList entries = ::transform(globals, [&](const NamedValuePtr& val) { 
		auto type = val->getValue()->getType();
		return build.namedType(val->getName(), type); 
	});

	auto structType = build.refType(build.structType(entries));
	// TODO: fix backend problem with this
	auto declStmt = build.declarationStmt(structType, /*build.refVar(build.undefined(structType))*/build.refVar(build.structExpr(globals)));

	// update program
	int location = 0;
	if(addr->getStatement(location)->getNodeType() == NT_DeclarationStmt) ++location;
	auto newProg = static_pointer_cast<const Program>(insert(manager, addr, declStmt, location));
	utils::migrateAnnotations(prog, newProg);
	prog = newProg;

	// migrate annotations on body
	lambda = prog[0].as<LambdaExprPtr>();
	compound = lambda->getBody();
	utils::migrateAnnotations(addr.getAddressedNode(), compound);

	return declStmt;
}


namespace {

	/**
	 * Pushes the given variable from the root-node context of target to the target node by passing it
	 * to every lambda encountered along this path. In the variable has to be renamed since the same id is
	 * already present (types will not be considered to increase readability) the parameter var will be updated.
	 * After the call var will reference the name of the variable as it is named in the context of the target.
	 */
	NodeAddress makeAvailable(NodeManager& manager, const NodeAddress& target, VariablePtr& var) {

		// we are at the root node, no modification is necessary
		if (target.isRoot()) return target;

		// process recursively first (bring the variable to the current scope
		NodeAddress parent = makeAvailable(manager, target.getParentAddress(), var);
		NodeAddress res = parent.getAddressOfChild(target.getIndex());

		// if this is a call to a lambda => add parameter (if not already there)
		if (parent.getNodeType() == NT_CallExpr) {
			CallExprPtr call = parent.as<CallExprPtr>();
			if (*call->getFunctionExpr() == *target) {
				// we are following the function call => add a parameter
				if (!contains(call.getArguments(), var)) {

					IRBuilder builder(manager);
					ExpressionList newArgs = call.getArguments();
					newArgs.push_back(var);

					// build new call (function is fixed within recursive step one level up)
					CallExprPtr newCall = builder.callExpr(call->getType(), call->getFunctionExpr(), newArgs);
					res = replaceAddress(manager, parent, newCall).getAddressOfChild(target.getIndex());
				}
			}
		}

		// if we are crossing a lambda, check whether new parameter needs to be accepted
		if (res.getNodeType() == NT_LambdaExpr) {
			assert(res.getParentNode()->getNodeType() == NT_CallExpr && "Parent of lambda needs to be a call!");
			assert(*res.getParentNode().as<CallExprPtr>()->getFunctionExpr() == *res && "Only directly invoked lambdas supported!");

			LambdaExprPtr lambda = res.as<LambdaExprPtr>();
			CallExprPtr call = res.getParentNode().as<CallExprPtr>();

			// check whether a new argument needs to be added
			if (call->getArguments().size() != lambda->getParameterList().size()) {
				assert(!lambda->isRecursive() && "Recursive functions not supported yet!");
				IRBuilder builder(manager);

				// find name for new parameter
				auto variablesInScope = core::analysis::getAllVariablesInScope(lambda->getLambda());

				VariablePtr newVar = var;
				while(contains(variablesInScope, newVar, [](const VariablePtr& a, const VariablePtr& b) { return a->getId() == b->getId(); })) {
					newVar = builder.variable(newVar->getType());
				}

				// update propagated variable
				var = newVar;

				// add new parameter and return new lambda
				VariableList params = lambda->getParameterList().getParameters();
				params.push_back(newVar);
				LambdaExprPtr newLambda = builder.lambdaExpr(lambda->getFunctionType()->getReturnType(), lambda->getBody(), params);
				res = replaceAddress(manager, res, newLambda);

			} else {

				// update variable when it is already passed as an argument
				auto args = call->getArguments();
				for(int i = args.size() - 1; i>0; i--) {
					if (args[i] == var) {
						var = lambda->getParameterList()[i];
						break;
					}
				}

			}
		}

		// for all others: just return result
		return res;
	}

}

VariableAddress pushInto(NodeManager& manager, const ExpressionAddress& target, const VariablePtr& var) {
	VariablePtr inner = var;
	NodeAddress inserted = makeAvailable(manager, target, inner);
	return replaceAddress(manager, inserted, inner).as<VariableAddress>();
}

vector<VariableAddress> pushInto(NodeManager& manager, const vector<ExpressionAddress>& targets, const VariablePtr& var) {

	// check whether there is something to do
	if (targets.empty()) {
		return vector<VariableAddress>();
	}

	// special handling for a single step
	if (targets.size() == 1u) {
		return toVector(pushInto(manager, targets[0], var));
	}

	// implant variables in reverse order of addresses (avoid invalid addresses in case targets are nested)
	vector<ExpressionAddress> sorted = targets;
	std::sort(sorted.rbegin(), sorted.rend());

	// implant variables - one by one
	NodePtr curRoot = targets[0].getRootNode();
	vector<VariableAddress> res;
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
	assert(!elements.empty() && "Elements must not be empty!");
	assert(all(elements, [&](const pair<ExpressionAddress, VariablePtr>& cur)->bool {
		return cur.first.getRootNode() == elements.begin()->first.getRootNode();
	}));


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
	if (!lambda->isRecursive()) return manager.get(lambda);

	LambdaDefinitionPtr defs = lambda->getDefinition();
	LambdaDefinitionPtr res = defs;
	for(auto def : defs->getDefinitions()) {
		auto var = def->getVariable();
		res = fixVariable(manager, res, var, var).as<LambdaDefinitionPtr>();
	}
	return IRBuilder(manager).lambdaExpr(lambda->getVariable(), res);

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

		StatementPtr res = makeCachedLambdaMapper([&](const NodePtr& cur)->NodePtr{
			if (cur->getNodeType() != NT_CallExpr) return cur;

			// some preparation
			NodeManager& mgr = cur->getNodeManager();
			auto& basic = mgr.getLangBasic();
			IRBuilder builder(mgr);
			auto call = cur.as<CallExprPtr>();

			// a function reducing the level expression by 1
			auto decLevel = [](const ExpressionPtr& level)->ExpressionPtr {
				auto formula = arithmetic::toFormula(level);
				assert(formula.isConstant() && "Accessing thread-group using non-constant level index not supported!");
				if (formula.isZero()) return ExpressionPtr();
				return arithmetic::toIR(level->getNodeManager(), formula-1);
			};

			// handle getThreadID
			if (analysis::isCallOf(call, basic.getGetThreadId())) {
				if (ExpressionPtr newLevel = decLevel(call[0])) {
					return builder.getThreadId(newLevel);
				}
				return idConstant;
			}

			// handle group size
			if (analysis::isCallOf(call,basic.getGetGroupSize())) {
				if (ExpressionPtr newLevel = decLevel(call[0])) {
					return builder.getThreadGroupSize(newLevel);
				}
				return sizeConstant;
			}

			return cur;
		}).map(body);

		// collect all references to the constants ...
		set<NodeAddress> idConstants;
		set<NodeAddress> sizeConstants;
		visitDepthFirstPrunable(NodeAddress(res), [&](const NodeAddress& cur)->bool {
			if (cur->getNodeCategory() == NC_Type) return true;
			if (*cur == *idConstant) idConstants.insert(cur);
			if (*cur == *sizeConstant) sizeConstants.insert(cur);
			return false;
		});

		// start replacement
		NodeManager& manager = body->getNodeManager();
		for (auto it = idConstants.rbegin(); it != idConstants.rend(); ++it) {
			res = pushInto(manager, it->switchRoot(res).as<ExpressionAddress>(), id).getRootNode().as<StatementPtr>();
		}
		for (auto it = sizeConstants.rbegin(); it != sizeConstants.rend(); ++it) {
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
		return extractLambda(mgr, loop, {a,b,c});
	}


}

ExpressionPtr tryToPFor(const JobExprPtr& job) {
	static const ExpressionPtr fail;
	NodeManager& mgr = job.getNodeManager();

	// make sure there are no guarded statements
	if (!job->getGuardedExprs().empty()) return fail;

	// also, there must not be a re-distribute call => can not be supported
	if (analysis::contains(job->getDefaultExpr(), mgr.getLangBasic().getRedistribute())) return fail;

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

	// start list of statements by declarations A
	StatementList stmts;
	::copy(job.getLocalDecls(), std::back_inserter(stmts));

	// create pfor-call
	{
		auto range = job->getThreadNumRange();
		auto lowerBound = analysis::getArgument(range, 0);
		auto iterType = lowerBound->getType();

		// build up range
		ExpressionPtr start = builder.getZero(lowerBound->getType());
		ExpressionPtr step  = builder.literal(iterType, "1");

		// create iterator variable
		VariablePtr threadID = builder.variable(start->getType());

		// create limit for iteration (into extra variable)
		DeclarationStmtPtr limitDecl = builder.declarationStmt(lowerBound);
		VariablePtr end = limitDecl->getVariable();

		stmts.push_back(limitDecl);

		// get job body
		ExpressionPtr jobBody = job->getDefaultExpr();

		// create and add pfor call
		stmts.push_back(builder.pfor(jobToPforBody(jobBody, end), start, end, step));
	}

	// finish by adding the barrier
	stmts.push_back(builder.barrier());

	// build resulting function
	return extractLambda(mgr, builder.compoundStmt(stmts));
}

} // end namespace transform
} // end namespace core
} // end namespace insieme

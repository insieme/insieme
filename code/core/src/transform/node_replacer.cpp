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

#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/type_utils.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/type_utils.h"
#include "insieme/utils/logging.h"

namespace {

using namespace insieme::core;
using namespace insieme::core::transform;

using namespace insieme::utils::map;

/**
 * Visitor which replace a specific node of the IR starting from a root node.
 */
class NodeReplacer : public CachedNodeMapping {
	NodeManager& manager;
	const PointerMap<NodePtr, NodePtr>& replacements;
	const bool includesTypes;

public:

	NodeReplacer(NodeManager& manager, const PointerMap<NodePtr, NodePtr>& replacements)
		: manager(manager), replacements(replacements),
		  includesTypes(any(replacements, [](const std::pair<NodePtr, NodePtr>& cur) { auto cat = cur.first->getNodeCategory(); return cat == NC_Type || cat == NC_IntTypeParam; })) { }

private:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		// check whether the element has been found
		auto pos = replacements.find(ptr);
		if(pos != replacements.end()) {
			return pos->second;
		}

		// if element to be replaced is a not a type but the current node is,
		// the recursion can be pruned (since types only have other types as
		// sub-nodes)
		if (!includesTypes && ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// done
		return res;
	}
};

/**
 * Visitor which replace a specific node of the IR starting from a root node.
 */
class SingleNodeReplacer : public CachedNodeMapping {
	NodeManager& manager;
	const NodePtr& target;
	const NodePtr& replacement;
	const bool visitTypes;

public:

	SingleNodeReplacer(NodeManager& manager, const NodePtr& target, const NodePtr& replacement)
		: manager(manager), target(target), replacement(replacement), visitTypes(target->getNodeCategory() == NC_Type || target->getNodeCategory() == NC_IntTypeParam) { }

private:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {

		// handle replacement
		if (*ptr == *target) {
			return replacement;
		}

		// prune types if possible
		if (!visitTypes && ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);


		// done
		return res;
	}
};

class VariableReplacer : public CachedNodeMapping {

	NodeManager& manager;
	const VariablePtr variable;
	const NodePtr replacement;

public:

	VariableReplacer(NodeManager& manager, const VariablePtr& variable, const NodePtr& replacement)
		: manager(manager), variable(variable), replacement(replacement) { }

private:
	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		// check whether the element has been found
		if (*ptr == *variable) {
			return replacement;
		}

		// shortcut for types => will never be changed
		if (ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// handle scope limiting elements
		if (ptr->getNodeType() == NT_LambdaExpr) {
			// enters a new scope => variable will no longer occur
			return ptr;
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// done
		return res;
	}
};

template<typename T>
class VariableMapReplacer : public CachedNodeMapping {

	NodeManager& manager;
	const PointerMap<VariablePtr, T>& replacements;

public:

	VariableMapReplacer(NodeManager& manager, const PointerMap<VariablePtr, T>& replacements)
		: manager(manager), replacements(replacements) { }

private:
	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		// check whether the element has been found
		if (ptr->getNodeType() == NT_Variable) {
			auto pos = replacements.find(static_pointer_cast<const Variable>(ptr));
			if(pos != replacements.end()) {
				return pos->second;
			}
		}

		// shortcut for types => will never be changed
		if (ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// handle scope limiting elements
		if (ptr->getNodeType() == NT_LambdaExpr) {
			// enters a new scope => variable will no longer occur
			return ptr;
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// done
		return res;
	}
};


class RecVariableMapReplacer : public CachedNodeMapping {

	NodeManager& manager;
	IRBuilder builder;
	const PointerMap<VariablePtr, VariablePtr>& replacements;
	bool limitScope;
	const TypeRecoveryHandler& recoverTypes;
	const TypeHandler& typeHandler;

public:

	RecVariableMapReplacer(NodeManager& manager, const PointerMap<VariablePtr, VariablePtr>& replacements, bool limitScope,
			const TypeRecoveryHandler& recoverTypes, const TypeHandler& typeHandler)
		: manager(manager), builder(manager), replacements(replacements), limitScope(limitScope),
		  recoverTypes(recoverTypes), typeHandler(typeHandler) {}

private:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		// check whether the element has been found
		if (ptr->getNodeType() == NT_Variable) {
			auto pos = replacements.find(static_pointer_cast<const Variable>(ptr));
			if(pos != replacements.end()) {
				return pos->second;
			}
		}

		// shortcut for types => will never be changed
		if (ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// handle scope limiting elements
		if(ptr->getNodeType() == NT_LambdaExpr) {
			// enters a new scope => variable will no longer occur
			return ptr;
		}

		// compute result - bottom up
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed, nothing to check
			return ptr;
		}
		
		// check whether types have to be corrected in post-processing
		if (res->getNodeType() == NT_CallExpr) {
			res = handleCallExpr(res.as<CallExprPtr>());
		}

		// special handling for declaration statements
		if (res->getNodeType() == NT_DeclarationStmt) {
			res = handleDeclStmt(res.as<DeclarationStmtPtr>());
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// done
		return res;
	}

	NodePtr handleDeclStmt(const DeclarationStmtPtr& decl) {
		// check variable and value type
		TypePtr varType = decl->getVariable()->getType();
		TypePtr valType = decl->getInitialization()->getType();

		if (*varType != *valType) {
			// if types are not matching, try to fix it using type handler
			return typeHandler(decl);
		}
		return decl;
	}

	bool typesMatch(ExpressionList a, ExpressionList b) {
		if(a.size() != b.size())
			return false;


		for(unsigned int cnt = 0; cnt < a.size(); ++cnt) {
			if(a.at(cnt) != b.at(cnt)->getType()) {
				return false;
			}
		}

		return true;
	}

	ExpressionPtr handleCallExpr(const CallExprPtr& call) {

		// check whether it is a call to a literal
		const ExpressionPtr& fun = call->getFunctionExpr();

		if (fun->getNodeType() == NT_LambdaExpr) {
			return handleCallToLamba(call);
		}

		if (fun->getNodeType() != NT_Literal) {
			return call;
		}

		// run it through the type recovery handler
		return recoverTypes(call);
	}

	CallExprPtr handleCallToLamba(const CallExprPtr& call) {
		assert(call->getFunctionExpr()->getNodeType() == NT_LambdaExpr);

		const LambdaExprPtr& lambda = call->getFunctionExpr().as<LambdaExprPtr>();
		const ExpressionList& args = call->getArguments();

		const VariableList& params = lambda->getParameterList()->getParameters();
		if (params.size() != args.size()) {
			// wrong number of arguments => don't touch this!
			return call;
		}

		// test whether current lambda type is matching
		TypeList argumentTypes = extractTypes(call->getArguments());
		if (deduceReturnType(lambda->getType().as<FunctionTypePtr>(), argumentTypes, false)) {
			return call; // => no modification necessary
		}

		// create replacement map
		VariableList newParams;
		VariableMap map = limitScope ? VariableMap() : replacements;
		for_range(make_paired_range(params, args), [&](const std::pair<VariablePtr, ExpressionPtr>& cur) {
			VariablePtr param = cur.first;
			if (!isSubTypeOf(cur.second->getType(), param->getType())) {
				param = this->builder.variable(cur.second->getType());
				map[cur.first] = param;
			}
			newParams.push_back(param);
		});

		// check whether a modification is necessary at all
		if (map.empty()) {
			return call;
		}

		// construct new body
		CompoundStmtPtr newBody = replaceVarsRecursiveGen(manager, lambda->getBody(), map, limitScope, recoverTypes, typeHandler);

		// obtain return type
		TypeList returnTypes;
		visitDepthFirstPrunable(newBody, [&](const NodePtr& cur) {
			if (cur->getNodeType() == NT_ReturnStmt) {
				returnTypes.push_back(cur.as<ReturnStmtPtr>()->getReturnExpr()->getType());
			}
			return cur->getNodeType() == NT_LambdaExpr || cur->getNodeCategory() != NC_Statement;
		});

		// deduce return type
		TypePtr returnType = (returnTypes.empty())?
				manager.getLangBasic().getUnit() :
				getSmallestCommonSuperType(returnTypes);

		assert(returnType && "Cannot find a common supertype of all return statements");

		// construct new function type
		FunctionTypePtr funType = builder.functionType(extractTypes(newParams), returnType);
		LambdaExprPtr newLambda = builder.lambdaExpr(funType, newParams, newBody);

		// deduce type of resulting call
		TypePtr callType = deduceReturnType(funType, argumentTypes);

		// create replacing call (including the deduction of a new return type)
		return builder.callExpr(callType, newLambda, args);

	}
};

class TypeFixer : public CachedNodeMapping {

	NodeManager& manager;
	IRBuilder builder;
	const VariableMap& replacements;
	bool limitScope;
	const TypeHandler& typeHandler;

public:

	TypeFixer(NodeManager& manager, const VariableMap& replacements, bool limitScope,
			const TypeHandler& typeHandler)
		: manager(manager), builder(manager), replacements(replacements), limitScope(limitScope), typeHandler(typeHandler) {}

private:
	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		// check whether the element has been found
		if (ptr->getNodeType() == NT_Variable) {
			auto pos = replacements.find(static_pointer_cast<const Variable>(ptr));
			if(pos != replacements.end()) {
				return pos->second;
			}
		}

		// shortcut for types => will never be changed
		if (ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// handle scope limiting elements
		if(limitScope) {
			switch(ptr->getNodeType()) {
			case NT_LambdaExpr:
				// enters a new scope => variable will no longer occur
				return ptr;
			default: { }
			}
		}

		// compute result
		NodePtr res = ptr;

		// update calls to functions recursively
		if (res->getNodeType() == NT_CallExpr) {

			CallExprPtr call = handleCall(static_pointer_cast<const CallExpr>(res));

			const ExpressionList args = call->getArguments();
			// remove illegal deref operations
			if(builder.getNodeManager().getLangBasic().isRefDeref(call->getFunctionExpr()) &&
					(!dynamic_pointer_cast<const RefType>(args.at(0)->getType()) && args.at(0)->getType()->getNodeType() != NT_GenericType ))
				return args.at(0);

			// check return type
			assert(call->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Function expression is not a function!");

			// extract function type
			FunctionTypePtr funType = static_pointer_cast<const FunctionType>(call->getFunctionExpr()->getType());
			assert(funType->getParameterTypes().size() == call->getArguments().size() && "Invalid number of arguments!");
/*
			if (static_pointer_cast<const CallExpr>(call)->getFunctionExpr()->getNodeType() == NT_Literal) {
				std::cout << "ARRR " << call << std::endl;
			}*/
			TypeList argumentTypes;
			::transform(call->getArguments(), back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });
			try {
				tryDeduceReturnType(funType, argumentTypes);
			} catch(ReturnTypeDeductionException& rtde) {
				typeHandler(call);
			}

			res = call;
		} else if (res->getNodeType() == NT_DeclarationStmt) {
			res = handleDeclStmt(static_pointer_cast<const DeclarationStmt>(res));

		} else {
			// recursive replacement has to be continued
			res = res->substitute(manager, *this);
		}

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// done
		return res;
	}

	NodePtr handleDeclStmt(const DeclarationStmtPtr& decl) {
		auto pos = replacements.find(decl->getVariable());
		if(pos != replacements.end()) {
			ExpressionPtr newInit = static_pointer_cast<const Expression>(this->resolveElement(decl->getInitialization()));
			if(pos->second->getType() != newInit->getType())
				return typeHandler(builder.declarationStmt(pos->second, newInit));

			return builder.declarationStmt(pos->second, newInit);
		}

		// continue replacement recursively
		return decl->substitute(manager, *this);
	}

	bool typesMatch(ExpressionList a, ExpressionList b) {
		if(a.size() != b.size())
			return false;


		for(unsigned int cnt = 0; cnt < a.size(); ++cnt) {
			if(a.at(cnt) != b.at(cnt)->getType()) {
				return false;
			}
		}

		return true;
	}

	CallExprPtr handleCall(const CallExprPtr& call) {

		// handle recursive push-through-calls

		// investigate call
		const ExpressionPtr& fun = call->getFunctionExpr();
		const ExpressionList& args = call->getArguments();

		// test whether args contains something which changed
		ExpressionList newArgs = ::transform(args, [&](const ExpressionPtr& cur)->ExpressionPtr {
/*if(call->getType()->toString().find("_cl_kernel") != string::npos)
	std::cout << "\nARG " << call->getType() << " " << *call->getFunctionExpr() << "(" << *cur << " " << *cur->getType() << ")" << std::endl;
*/
			return static_pointer_cast<const Expression>(this->resolveElement(cur));
		});

		if (fun->getNodeType() == NT_LambdaExpr) {
			const CallExprPtr newCall = handleCallToLamba(call->getType(), static_pointer_cast<const LambdaExpr>(fun), newArgs);
/*			if(call->getType() != newCall->getType()) {
				std::cout << call->getType() << " " << call << std::endl << newCall->getType() << " " << newCall << "\n\n\n";
			}
*/
			return newCall;
		}
		// test whether there has been a change
		if (args == newArgs && fun->getNodeType() != NT_Literal) {
			return call;
		}

/*		if(const LiteralPtr& literal = dynamic_pointer_cast<const Literal>(call->getFunctionExpr()))
					if (manager.getLangBasic().isBuiltIn(literal))
			std::cout << " -> " << literal << " " << call->getArguments() << std::endl;*/
		if (fun->getNodeType() == NT_Literal) {
			return handleCallToLiteral(call, call->getType(), newArgs).as<CallExprPtr>();
		}

		// don't act on pointwise and accuracy functions
		if(CallExprPtr calledExpr = dynamic_pointer_cast<const CallExpr>(fun))
			if(manager.getLangBasic().isPointwise(calledExpr->getFunctionExpr()) || manager.getLangBasic().isAccuracy(calledExpr->getFunctionExpr()))
				return call;

		LOG(ERROR) << fun;
		for_each(call->getArguments(), [](ExpressionPtr arg){ std::cout << arg->getType() << " " << arg << std::endl; });
		assert(false && "Unsupported call-target encountered - sorry!");
		return call;
	}

	CallExprPtr handleCallToLamba(const TypePtr& resType, const LambdaExprPtr& lambda, const ExpressionList& args) {

		const VariableList& params = lambda->getParameterList()->getParameters();
		if (params.size() != args.size()) {
			// wrong number of arguments => don't touch this!
			return builder.callExpr(resType, lambda, args);
		}

		TypeList newParamTypes = ::transform(args, [](const ExpressionPtr& cur)->TypePtr { return cur->getType(); });
		TypePtr callTy = deduceReturnType(static_pointer_cast<const FunctionType>(lambda->getType()), newParamTypes, false);
		if(callTy) {
			return static_pointer_cast<const CallExpr>(builder.callExpr(callTy, lambda, args)->substitute(manager, *this));
		}

		// create replacement map
		VariableList newParams;
		TypeMap tyMap;
		VariableMap map = replacements;
		for_range(make_paired_range(params, args), [&](const std::pair<VariablePtr, ExpressionPtr>& cur) {
/*				bool foundTypeVariable = visitDepthFirstInterruptable(param->getType(), [&](const NodePtr& type) -> bool {
					if(type->getNodeType() == NT_TypeVariable) {
						std::cerr << param->getType() << " - " << type << std::endl;
						return true;
					}
					return false;
				}, true , true);
				if(foundTypeVariable) {
					TypePtr tyVars = unify(manager, param->getType(), cur.second->getType());
					if(tyVars) {
						tyVars->applyTo(manager, )
					}
				}*/

			VariablePtr param = cur.first;
			if (!isSubTypeOf(cur.second->getType(), param->getType())) {
				param = this->builder.variable(cur.second->getType());
				map[cur.first] = param;
			}

			newParams.push_back(param);
		});

		// construct new body
		CompoundStmtPtr newBody = fixTypesGen(manager, lambda->getBody(), map, limitScope, typeHandler);

		// obtain return type
		TypeList returnTypes;
		visitDepthFirstPrunable(newBody, [&](const NodePtr& cur) {
			if (cur->getNodeType() == NT_ReturnStmt) {
				returnTypes.push_back(cur.as<ReturnStmtPtr>()->getReturnExpr()->getType());
			}
			return cur->getNodeType() == NT_LambdaExpr || cur->getNodeCategory() != NC_Statement;
		});

		// deduce return type
		callTy = (returnTypes.empty())?
				manager.getLangBasic().getUnit() :
				getSmallestCommonSuperType(returnTypes);

		assert(callTy && "Cannot find a common supertype of all return statements");

		// assemble new lambda
		FunctionTypePtr funType = builder.functionType(newParamTypes, callTy);
		LambdaExprPtr newLambda = builder.lambdaExpr(funType, newParams, newBody);

		return builder.callExpr(callTy, newLambda, args);

	}

	ExpressionPtr handleCallToLiteral(const CallExprPtr& call, const TypePtr& resType, const ExpressionList& args) {
		const LiteralPtr literal = call->getFunctionExpr().as<LiteralPtr>();
		NodeManager& manager = call->getNodeManager();
		IRBuilder builder(manager);

		// only supported for function types
		assert(literal->getType()->getNodeType() == NT_FunctionType);
		// do not touch build-in literals
		if (manager.getLangBasic().isBuiltIn(literal)) {

			// use type inference for the return type
			if(manager.getLangBasic().isCompositeRefElem(literal)) {
				return static_pointer_cast<const CallExpr>(builder.refMember(args.at(0),
						static_pointer_cast<const Literal>(args.at(1))->getValue()));
			}
			if(manager.getLangBasic().isCompositeMemberAccess(literal)) {
				return static_pointer_cast<const CallExpr>(builder.accessMember(args.at(0),
						static_pointer_cast<const Literal>(args.at(1))->getValue()));
			}
			if(manager.getLangBasic().isTupleRefElem(literal)) {
	//std::cout << "TRRRRRRRRR " << args.at(0)->getType() << " < " << args << std::endl;
				return static_pointer_cast<const CallExpr>(builder.refComponent(args.at(0), args.at(1)));
			}
			if(manager.getLangBasic().isTupleMemberAccess(literal)) {
	//std::cout << "BAMBAM " << args.at(0)->getType() << " > " << args << std::endl;
				return static_pointer_cast<const CallExpr>(builder.accessComponent(args.at(0), args.at(1)));
			}

			CallExprPtr newCall = builder.callExpr(literal, args);

			return newCall;
		}

		// assemble new argument types
		TypeList newParamTypes = ::transform(args, [](const ExpressionPtr& cur)->TypePtr { return cur->getType(); });

		FunctionTypePtr newType = builder.functionType(newParamTypes, resType);
		return builder.callExpr(builder.literal(newType, literal->getValue()), args);

	}


};


class TypeVariableReplacer : public CachedNodeMapping {

	NodeManager& manager;
	const SubstitutionOpt& substitution;

public:

	TypeVariableReplacer(NodeManager& manager, const SubstitutionOpt& substitution)
		: manager(manager), substitution(substitution) {
		assert(substitution && !substitution->empty() && "Substitution must not be empty!");
	}

private:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {

		// check whether the element has been found
		if (ptr->getNodeType() == NT_TypeVariable || ptr->getNodeType() == NT_VariableIntTypeParam) {
			return substitution->applyTo(manager, static_pointer_cast<const Type>(ptr));
		}

		// handle scope limiting elements
		switch(ptr->getNodeType()) {
		case NT_LambdaExpr:
		case NT_FunctionType:
			// enters a new scope => variable will no longer occur
			return ptr;
		default: { }
		}

		// recursive replacement has to be continued
		NodePtr res = ptr->substitute(manager, *this);

		// check whether something has changed ...
		if (res == ptr) {
			// => nothing changed
			return ptr;
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// correct type literal name => cosmetic
		if (res->getNodeType() == NT_Literal) {
			const LiteralPtr& literal = static_pointer_cast<const Literal>(res);
			if (analysis::isTypeLiteralType(literal->getType())) {
				const TypePtr& type = analysis::getRepresentedType(literal->getType());

				// update type
				return IRBuilder(manager).getTypeLiteral(type);
			}
		}

		// done
		return res;
	}

};


class NodeAddressReplacer : public NodeMapping {
	const unsigned indexToReplace;
	const NodePtr& replacement;

	public:

		NodeAddressReplacer(unsigned index, const NodePtr& replacement)
			: indexToReplace(index), replacement(replacement) {}

	private:

		/**
		 * Represents an identity-operation except for the one element to be replaced,
		 * which is identified by its index.
		 *
		 * @param index the index of the element to be mapped
		 * @param ptr a pointer to the element to be mapped
		 * @return a pointer to the mapped element
		 */
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if (indexToReplace == index) {
				return replacement;
			}
			return ptr;
		}

};

}

namespace insieme {
namespace core {
namespace transform {

NodePtr applyReplacer(NodeManager& mgr, const NodePtr& root, NodeMapping& mapper) {
	if(!root) {
		return NodePtr(NULL);
	}

	// map root node element
	NodePtr res = mapper.map(0, root);

	// check whether something has changed
	if (res == root) {
		// nothing changed => return handed in node
		return root;
	}

	// preserve annotations
	utils::migrateAnnotations(root, res);

	return res;
}


NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodeMap& replacements, bool limitScope) {

	// shortcut for empty replacements
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// handle single element case
	if (replacements.size() == 1) {
		auto pair = *replacements.begin();
		return replaceAll(mgr, root, pair.first, pair.second, limitScope);
	}

	// handle entire map
	auto mapper = ::NodeReplacer(mgr, replacements);
	return applyReplacer(mgr, root, mapper);
}

NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const NodePtr& toReplace, const NodePtr& replacement, bool limitScope) {

	if (limitScope && toReplace->getNodeType() == NT_Variable) {
		return replaceAll(mgr, root, static_pointer_cast<const Variable>(toReplace), replacement);
	}

	auto mapper = ::SingleNodeReplacer(mgr, toReplace, replacement);
	return applyReplacer(mgr, root, mapper);
}

NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const VariablePtr& toReplace, const NodePtr& replacement, bool limitScope) {
	if(limitScope) {
		auto mapper = ::VariableReplacer(mgr, toReplace, replacement);
		return applyReplacer(mgr, root, mapper);
	} else {
		auto mapper = ::SingleNodeReplacer(mgr, toReplace, replacement);
		return applyReplacer(mgr, root, mapper);
	}
}


NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VariableMap& replacements) {
	// special handling for empty replacement map
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::VariableMapReplacer<VariablePtr>(mgr, replacements);
	return applyReplacer(mgr, root, mapper);
}

NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VarExprMap& replacements) {
	// special handling for empty replacement map
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::VariableMapReplacer<ExpressionPtr>(mgr, replacements);
	return applyReplacer(mgr, root, mapper);
}

namespace {

	CallExprPtr typeDeductionBasedTypeRecovery(const CallExprPtr& call) {

		// check type
		const ExpressionPtr fun = call->getFunctionExpr();
		const ExpressionList args = call->getArguments();

		// check whether the function type has been preserved
		assert(fun->getType()->getNodeType() == NT_FunctionType && "Call is no longer targeting function after replacement!");

		FunctionTypePtr funType = fun->getType().as<FunctionTypePtr>();

		TypePtr should = deduceReturnType(funType, extractTypes(call->getArguments()));

		// see whether a better type has been deduced
		if (!should || *should == *call->getType()) {
			return call;
		}

		// use deduced alternative
		return IRBuilder(call->getNodeManager()).callExpr(should, call->getFunctionExpr(), call->getArguments());
	}

}


ExpressionPtr defaultTypeRecovery(const CallExprPtr& call) {

	// check whether target of call is a literal
	if (call->getFunctionExpr()->getNodeType() == NT_Literal) {
		NodeManager& manager = call->getNodeManager();
		IRBuilder builder(manager);
		const auto& basic = manager.getLangBasic();

		auto args = call->getArguments();

		const LiteralPtr& literal = call->getFunctionExpr().as<LiteralPtr>();

		// deal with standard build-in literals
		if (basic.isCompositeRefElem(literal)) {
			return builder.refMember(args[0], args[1].as<LiteralPtr>()->getValue());
		}
		if (basic.isCompositeMemberAccess(literal)) {
			return builder.accessMember(args[0], args[1].as<LiteralPtr>()->getValue());
		}
		if (basic.isTupleRefElem(literal)) {
			return builder.refComponent(args[0], args[1]);
		}
		if (basic.isTupleMemberAccess(literal)) {
			return builder.accessComponent(args[0], args[1]);
		}

		// eliminate unnecessary dereferencing
		if (basic.isRefDeref(literal) && !analysis::isRefType(args[0]->getType())) {
			return args[0];
		}
	}

	// use generic fall-back solution
	return typeDeductionBasedTypeRecovery(call);

}

// functor which updates the type literal inside a call to undefined in a declareation
TypeHandler getVarInitUpdater(NodeManager& manager){
	return [&](const StatementPtr& node)->StatementPtr {
		IRBuilder builder(manager);
		StatementPtr res = node;
		const lang::BasicGenerator& basic = builder.getLangBasic();

		// update init undefined
		if(const DeclarationStmtPtr& decl = dynamic_pointer_cast<const DeclarationStmt>(node)) {
			if(const CallExprPtr& init = dynamic_pointer_cast<const CallExpr>(decl->getInitialization())) {
				const VariablePtr& var = decl->getVariable();
				const ExpressionPtr& fun = init->getFunctionExpr();
				// handle ref variables
				if((init->getType() != var->getType()) && (basic.isRefVar(fun) || basic.isRefNew(fun))) {
					const RefTypePtr varTy = static_pointer_cast<const RefType>(var->getType());
					if(const CallExprPtr& undefined = dynamic_pointer_cast<const CallExpr>(init->getArgument(0))) {
						if(basic.isUndefined(undefined->getFunctionExpr()))
							res = builder.declarationStmt(var, builder.callExpr(varTy, fun, builder.callExpr(varTy->getElementType(),
									basic.getUndefined(), builder.getTypeLiteral(varTy->getElementType()))));
					}
				}
				// handle non ref variables
				if((init->getType() != var->getType()) && basic.isUndefined(fun)) {
					const TypePtr varTy = var->getType();
					res = builder.declarationStmt(var, builder.callExpr(varTy, fun, builder.getTypeLiteral(varTy)));
				}
			}
		}

		// check whether something has changed ...
		if (res == node) {
			// => nothing changed
			return node;
		}

		// preserve annotations
		transform::utils::migrateAnnotations(node, res);

		// done
		return res;

	};

}


NodePtr replaceVarsRecursive(NodeManager& mgr, const NodePtr& root, const VariableMap& replacements,
		bool limitScope, const TypeRecoveryHandler& recoveryHandler, const TypeHandler& typeHandler) {
	// special handling for empty replacement maps
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::RecVariableMapReplacer(mgr, replacements, limitScope, recoveryHandler, typeHandler);
	return applyReplacer(mgr, root, mapper);
}

NodePtr fixTypes(NodeManager& mgr, NodePtr root, const VariableMap& replacements, bool limitScope,
		const TypeHandler& typeHandler) {
	// conduct actual substitutions
	auto mapper = ::TypeFixer(mgr, replacements, limitScope, typeHandler);
	return applyReplacer(mgr, root, mapper);
}

NodePtr replaceTypeVars(NodeManager& mgr, const NodePtr& root, const SubstitutionOpt& substitution) {
	assert(root && "Root must not be a null pointer!");

	// check whether there is something to do
	if (!substitution || substitution->empty()) {
		return root;
	}

	// conduct actual substitution
	auto mapper = ::TypeVariableReplacer(mgr, substitution);
	return applyReplacer(mgr, root, mapper);
}


NodePtr replaceAll(NodeManager& mgr, const std::map<NodeAddress, NodePtr>& replacements) {
	typedef std::pair<NodeAddress, NodePtr> Replacement;

	// check preconditions
	assert(!replacements.empty() && "Replacements must not be empty!");

	assert(all(replacements, [&](const Replacement& cur) {
		return cur.first.isValid() && cur.second;
	}) && "Replacements are no valid addresses / pointers!");

	assert(all(replacements, [&](const Replacement& cur) {
		return *cur.first.getRootNode() == *replacements.begin()->first.getRootNode();
	}) && "Replacements do not all have the same root node!");


	// convert replacements into edit-able list
	vector<Replacement> steps(replacements.begin(), replacements.end());

	NodePtr res = replaceNode(mgr, steps.front().first, steps.front().second);
	for(auto it = steps.begin()+1; it != steps.end(); ++it) {

		// conduct current replacements using updated address
		res = replaceNode(mgr, it->first.switchRoot(res), it->second);
	}

	// return final node version
	return res;
}

NodePtr replaceNode(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement) {
	assert( toReplace.isValid() && "Invalid node address provided!");

	// create result
	NodePtr res = replacement;

	// process the path bottom up => replace one node after another
	Path path = toReplace.getPath();

	// replace bottom up
	unsigned lastPos = path.getIndex();
	while (path.getLength() > 1) {
		// go to parent
		path = path.getPathToParent();

		// conduct replace operation
		auto mapper = NodeAddressReplacer(lastPos, res);
		const NodePtr& cur = path.getAddressedNode();
		res = cur->substitute(manager, mapper);

		// preserve annotations
		utils::migrateAnnotations(cur, res);

		// update last-pos
		lastPos = path.getIndex();
	}

	// done
	return res;
}

NodeAddress replaceAddress(NodeManager& manager, const NodeAddress& toReplace, const NodePtr& replacement) {
	NodePtr newRoot = replaceNode(manager, toReplace, replacement);
	return toReplace.switchRoot(newRoot);
}



} // End transform namespace
} // End core namespace
} // End insieme namespace

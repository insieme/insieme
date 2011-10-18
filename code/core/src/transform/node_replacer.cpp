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

#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_address.h"
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
		switch(ptr->getNodeType()) {
		case NT_LambdaExpr:
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
		switch(ptr->getNodeType()) {
		case NT_LambdaExpr:
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

		// done
		return res;
	}
};


class RecVariableMapReplacer : public CachedNodeMapping {

	NodeManager& manager;
	ASTBuilder builder;
	const PointerMap<VariablePtr, VariablePtr>& replacements;
	bool limitScope;
	const std::function<NodePtr (const NodePtr&)>& functor;

public:

	RecVariableMapReplacer(NodeManager& manager, const PointerMap<VariablePtr, VariablePtr>& replacements, bool limitScope,
			const std::function<NodePtr (const NodePtr&)>& functor)
		: manager(manager), builder(manager), replacements(replacements), limitScope(limitScope), functor(functor) {

//		std::cout << replacements << std::endl;
	}

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
				functor(call);
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
				return functor(builder.declarationStmt(pos->second, newInit));

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
					if (manager.getBasicGenerator().isBuiltIn(literal))
			std::cout << " -> " << literal << " " << call->getArguments() << std::endl;*/
		if (fun->getNodeType() == NT_Literal) {
			const CallExprPtr& newCall = handleCallToLiteral(call->getType(), static_pointer_cast<const Literal>(fun), newArgs);
/*			if(call->getType() != newCall->getType()) {
				std::cout << call->getType() << " " << call << std::endl << newCall->getType() << " " << newCall << "\n\n\n";
			}
*/
			return newCall;
		}
		LOG(ERROR) << call;
		assert(false && "Unsupported call-target encountered - sorry!");
		return call;
	}

	CallExprPtr handleCallToLamba(const TypePtr& resType, const LambdaExprPtr& lambda, const ExpressionList& args) {

		const Lambda::ParamList& params = lambda->getParameterList();
		if (params.size() != args.size()) {
			// wrong number of arguments => don't touch this!
			return builder.callExpr(resType, lambda, args);
		}

		TypeList newParamTypes = ::transform(args, [](const ExpressionPtr& cur) { return cur->getType(); });
		TypePtr callTy = deduceReturnType(static_pointer_cast<const FunctionType>(lambda->getType()), newParamTypes, false);
		if(callTy) {
			return static_pointer_cast<const CallExpr>(builder.callExpr(callTy, lambda, args)->substitute(manager, *this));
		}

		// create replacement map
		Lambda::ParamList newParams;
		insieme::utils::map::PointerMap<TypePtr, TypePtr> tyMap;
		insieme::utils::map::PointerMap<VariablePtr, VariablePtr> map = replacements;
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
		StatementPtr newBody = replaceVarsRecursiveGen(manager, lambda->getBody(), map, limitScope, functor);

		// obtain return type
		TypeList typeList;
		visitDepthFirst(newBody, [&](const ReturnStmtPtr& ret) {
			typeList.push_back(ret->getReturnExpr()->getType());
		});

		TypePtr returnType = getSmallestCommonSuperType(typeList);
		if (!returnType) {
			returnType = builder.getBasicGenerator().getUnit();
		}

		// assemble new lambda
		FunctionTypePtr funType = builder.functionType(newParamTypes, returnType);
		LambdaExprPtr newLambda = builder.lambdaExpr(funType, newParams, newBody);

		try {
			callTy = tryDeduceReturnType(static_pointer_cast<const FunctionType>(lambda->getType()), newParamTypes);
		} catch(ReturnTypeDeductionException& rtde) {
			callTy = returnType;
		}

		return builder.callExpr(callTy, newLambda, args);

	}

	CallExprPtr handleCallToLiteral(const TypePtr& resType, const LiteralPtr& literal, const ExpressionList& args) {
		// only supported for function types
		assert(literal->getType()->getNodeType() == NT_FunctionType);
		// do not touch build-in literals
		if (manager.getBasicGenerator().isBuiltIn(literal)) {
			// use type inference for the return type

			if(manager.getBasicGenerator().isCompositeRefElem(literal)) {
				return static_pointer_cast<const CallExpr>(builder.refMember(args.at(0),
						static_pointer_cast<const Literal>(args.at(1))->getValue()));
			}
			if(manager.getBasicGenerator().isCompositeMemberAccess(literal)) {
				return static_pointer_cast<const CallExpr>(builder.accessMember(args.at(0),
						static_pointer_cast<const Literal>(args.at(1))->getValue()));
			}
			if(manager.getBasicGenerator().isTupleRefElem(literal)) {
//std::cout << "TRRRRRRRRR " << args.at(0)->getType() << " < " << args << std::endl;
				return static_pointer_cast<const CallExpr>(builder.refComponent(args.at(0), args.at(1)));
			}
			if(manager.getBasicGenerator().isTupleMemberAccess(literal)) {
//std::cout << "BAMBAM " << args.at(0)->getType() << " > " << args << std::endl;
				return static_pointer_cast<const CallExpr>(builder.accessComponent(args.at(0), args.at(1)));
			}

if(resType->toString().find("_cl_kernel") != string::npos)
			CallExprPtr newCall = builder.callExpr(literal, args);

			return newCall;
		}

		// assemble new argument types
		TypeList newParamTypes = ::transform(args, [](const ExpressionPtr& cur) { return cur->getType(); });

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
				return manager.basic.getTypeLiteral(type);
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


NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const PointerMap<NodePtr, NodePtr>& replacements, bool limitScope) {

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


NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const insieme::utils::map::PointerMap<VariablePtr, VariablePtr>& replacements) {
	// special handling for empty replacement map
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::VariableMapReplacer<VariablePtr>(mgr, replacements);
	return applyReplacer(mgr, root, mapper);
}

NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const insieme::utils::map::PointerMap<VariablePtr, ExpressionPtr>& replacements) {
	// special handling for empty replacement map
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::VariableMapReplacer<ExpressionPtr>(mgr, replacements);
	return applyReplacer(mgr, root, mapper);
}

// functor which updates the type literal inside a call to undefined in a declareation
std::function<NodePtr (const NodePtr&)> getVarInitUpdater(const ASTBuilder& builder){
	return [&builder](const NodePtr& node)->NodePtr {
		NodePtr res = node;
		const lang::BasicGenerator& basic = builder.getBasicGenerator();

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
									basic.getUndefined(), basic.getTypeLiteral(varTy->getElementType()))));
					}
				}
				// handle non ref variables
				if((init->getType() != var->getType()) && basic.isUndefined(fun)) {
					const TypePtr varTy = var->getType();
					res = builder.declarationStmt(var, builder.callExpr(varTy, fun, basic.getTypeLiteral(varTy)));
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

NodePtr replaceVarsRecursive(NodeManager& mgr, const NodePtr& root, const insieme::utils::map::PointerMap<VariablePtr, VariablePtr>& replacements,
		bool limitScope, const std::function<NodePtr (const NodePtr&)>& functor) {
	// special handling for empty replacement maps
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::RecVariableMapReplacer(mgr, replacements, limitScope, functor);
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




} // End transform namespace
} // End core namespace
} // End insieme namespace

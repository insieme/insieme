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

#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/core/types/subtyping.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/utils/logging.h"

namespace {

using namespace insieme::core;
using namespace insieme::core::transform;
using namespace insieme::core::types;

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
	const bool limitScope;

public:

	SingleNodeReplacer(NodeManager& manager, const NodePtr& target, const NodePtr& replacement, bool limit)
		: manager(manager), target(target), replacement(replacement), visitTypes(target->getNodeCategory() == NC_Type ||
				target->getNodeCategory() == NC_IntTypeParam || target->getNodeCategory() == NC_Support), limitScope(limit){ }

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

		// handle scope limiting elements
		if (limitScope && ptr->getNodeType() == NT_LambdaExpr) {
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
		if (VariablePtr var = dynamic_pointer_cast<const Variable>(ptr)) {
			auto pos = replacements.find(var);
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
	ExpressionMap replacements;
	bool limitScope;
	const TypeRecoveryHandler& recoverTypes;
	const TypeHandler& typeHandler;
	const PointerMap<VariablePtr, ExpressionPtr>& declInitReplacements;

	mutable NodePtr root;

public:

	RecVariableMapReplacer(NodeManager& manager, const ExpressionMap& replacements, bool limitScope, const TypeRecoveryHandler& recoverTypes,
			const TypeHandler& typeHandler, const PointerMap<VariablePtr, ExpressionPtr>& declInitReplacements)
		: manager(manager), builder(manager), replacements(replacements), limitScope(limitScope),
		  recoverTypes(recoverTypes), typeHandler(typeHandler), declInitReplacements(declInitReplacements) {}

private:

	virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {

		// check whether this is the first entry ...
		if (root) {
			// it is not
			return CachedNodeMapping::mapElement(index, ptr);
		}

		// remember root element
		root = ptr;

		// the rest is the same
		auto res = CachedNodeMapping::mapElement(index, ptr);

		// delete root element
		root = NodePtr();

		return res;
	}

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {

		// if one of the replaced variables has been used as init expression for another one the type may changed.
		// In that case, update the type of the declared variable
		DeclarationStmtPtr decl = newVarFromInit(ptr);
		if(decl) return decl;

		// check whether the element has been found
        if (ExpressionPtr expr = dynamic_pointer_cast<const Expression>(ptr)) {
			auto pos = replacements.find(expr);
			if(pos != replacements.end()) {
				return pos->second;
			}
		}

		// shortcut for types => will never be changed
		if (ptr->getNodeCategory() == NC_Type) {
			return ptr;
		}

		// handle scope limiting elements
        if(limitScope && ptr->getNodeType() == NT_LambdaExpr && ptr != root) {
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

		// run it through the type recovery handler if applicable
		if (ptr->getNodeCategory() == NC_Expression && res->getNodeCategory() == NC_Expression) {
			res = recoverTypes(ptr.as<ExpressionPtr>(), res.as<ExpressionPtr>());
		}

		// preserve annotations
		utils::migrateAnnotations(ptr, res);

		// done
		return res;
	}

	NodePtr handleDeclStmt(const DeclarationStmtPtr& decl) {
		// check variable and value type
		VariablePtr var = decl->getVariable();
		TypePtr varType = var->getType();
		ExpressionPtr val = decl->getInitialization();
		TypePtr valType = val->getType();

		// check if the new variable is in declInitReplacements
		auto newInit = declInitReplacements.find(var);
		if(newInit != declInitReplacements.end()) {
			// use the provided init expression in declaration

			return builder.declarationStmt(var, newInit->second);
		}

		// if type has not changed we can stop here
		if(isSubTypeOf(valType, varType))
			return decl;

		// if types are not matching, try to fix it using type handler
		StatementPtr handled = typeHandler(decl);
		if(*handled != *decl)
			return handled;

		// if one of the replaced variables has been used as init expression for another one the type may changed.
		// In that case, update the type of the declared variable
/*		if(CallExprPtr initCall = val.isa<CallExprPtr>()) {
			if(builder.getLangBasic().isRefVar(initCall->getFunctionExpr())) {
				if(VariablePtr iVar = initCall->getArgument(0).isa<VariablePtr>()) {
					VariablePtr replaced;
					for_each(replacements, [&](std::pair<ExpressionPtr, ExpressionPtr> r) {
						if(r.second == iVar) {
							replaced = iVar;
							return;
						}
					});

					if(replaced) {
						// create new variable with new type
						VariablePtr newVar = builder.variable(initCall->getType());
						DeclarationStmtPtr newDecl = builder.declarationStmt(newVar, val);
						// add new variable to replacements
						replacements[var] = newVar;
		std::cout << "\nreplacint variable " << *var << " with " << *val << std::endl;
						return newDecl;
					}
				}
			}
		}
*/
		// reaching this point, the IR will probably have semantic errors
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

	DeclarationStmtPtr newVarFromInit(NodePtr ptr) {
		DeclarationStmtPtr newDecl;

		if(DeclarationStmtPtr decl = ptr.isa<DeclarationStmtPtr>()) {
			// check variable and value type
			ExpressionPtr val = decl->getInitialization();

			if(CallExprPtr initCall = val.isa<CallExprPtr>()) {
				if(builder.getLangBasic().isRefVar(initCall->getFunctionExpr())) {
					if(VariablePtr iVar = initCall->getArgument(0).isa<VariablePtr>()) {
						auto ding = replacements.find(iVar);
						if(ding != replacements.end()) {
							VariablePtr replaced = ding->second.isa<VariablePtr>();
							ExpressionPtr newInit = builder.refVar(replaced);
							TypePtr newType = newInit->getType();

							VariablePtr var = decl->getVariable();
							TypePtr varType = var->getType();
							if(replaced && varType != newType) {
								// create new variable with new type
								VariablePtr newVar = builder.variable(newType);
								newDecl = builder.declarationStmt(newVar, newInit);
								// add new variable to replacements
								replacements[var] = newVar;
							}
						}
					}
				}
			}
		}

		return newDecl;
	}

	ExpressionPtr handleCallExpr(const CallExprPtr& call) {

		// check whether it is a call to a literal
		const ExpressionPtr& fun = call->getFunctionExpr();
		if (fun->getNodeType() == NT_LambdaExpr) {
			return handleCallToLamba(call);
		}
		if(manager.getLangBasic().isRefAssign(fun)) {
			ExpressionPtr lhs = call.getArgument(0);
			ExpressionPtr rhs = call.getArgument(1);

			// some things (i.e. the OpenCL backend) replace variables with variables of the same type, wrapped in a generic wrapper type.
			// The unwrap is inserted in a later step. To allow this procedure, this hack had to be inserted.
			if(lhs->getType().isa<GenericTypePtr>())
				return call;

			assert(lhs->getType().isa<RefTypePtr>() && "Replacing variable of ref-type with variable of non-ref-type makes assignment impossible");

			TypePtr lhsTy = lhs->getType().as<RefTypePtr>()->getElementType();

			if(!isSubTypeOf(rhs->getType(), lhsTy)) {

				// if it's a ref.reinterpret or cast, fix type
				if(CastExprPtr rCast = rhs.isa<CastExprPtr>())
					return builder.callExpr(fun, lhs, builder.castExpr(lhsTy, rCast->getSubExpression()));
				if(CallExprPtr rCall = rhs.isa<CallExprPtr>()) {
					if(builder.getLangBasic().isRefReinterpret(rCall->getFunctionExpr())) {
						assert(lhsTy.isa<RefTypePtr>() &&
								"Replacing variable of ref-ref-type with variable of non-ref-ref-type makes assignment of ref.reinterpret impossible");

						return builder.callExpr(fun, lhs, builder.refReinterpret(rCall->getArgument(0), lhsTy.as<RefTypePtr>()->getElementType()));
					}
				}
				// else add cast
				return builder.callExpr(fun, lhs, builder.castExpr(lhsTy, rhs));
			}
		}

		return call;
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
        VariableMap map = VariableMap();
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
	const ExpressionMap& replacements;
	bool limitScope;
	const TypeHandler& typeHandler;

public:

	TypeFixer(NodeManager& manager, const ExpressionMap& replacements, bool limitScope,
			const TypeHandler& typeHandler)
		: manager(manager), builder(manager), replacements(replacements), limitScope(limitScope), typeHandler(typeHandler) {}

private:
	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		// check whether the element has been found
		if (ptr.isa<ExpressionPtr>()) {
			auto pos = replacements.find(ptr.as<ExpressionPtr>());
			if(pos != replacements.end()) {
				NodePtr res = pos->second;
				utils::migrateAnnotations(ptr, res);
				return res;
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
					(!dynamic_pointer_cast<const RefType>(args.at(0)->getType()) && args.at(0)->getType()->getNodeType() != NT_GenericType )) {
				res = args.at(0);
				utils::migrateAnnotations(ptr, res);
				return res;
			}

			// check return type
			assert(call->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Function expression is not a function!");

			// extract function type
			FunctionTypePtr funType = static_pointer_cast<const FunctionType>(call->getFunctionExpr()->getType());
			assert(funType->getParameterTypes().size() == call->getArguments().size() && "Invalid number of arguments!");
/*
			if (static_pointer_cast<const CallExpr>(call)->getFunctionExpr()->getNodeType() == NT_Literal) {
				std::cout << "ARRR " << call << std::endl;
			}*/

			res = call;

			TypeList argumentTypes;
			::transform(call->getArguments(), back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });
			try {
				// check if function type is consistent
				tryDeduceReturnType(funType, argumentTypes);
			} catch(ReturnTypeDeductionException& rtde) {
				// use type handler if function type is inconsistent
				res = typeHandler(call);
			}
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
				return typeHandler(builder.declarationStmt(pos->second.as<VariablePtr>(), newInit));

			return builder.declarationStmt(pos->second.as<VariablePtr>(), newInit);
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
			return static_pointer_cast<const Expression>(this->resolveElement(cur));
		});

		// fix call to known built-in functions
		if (manager.getLangBasic().isBuiltIn(call->getFunctionExpr())) {
			return handleCallToBuiltIn(call, newArgs);
		}

		if (fun->getNodeType() == NT_LambdaExpr) {
			const CallExprPtr newCall = handleCallToLamba(call->getType(), static_pointer_cast<const LambdaExpr>(fun), newArgs);

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
				return call->substitute(manager, *this);

		LOG(ERROR) << fun;
		for_each(call->getArguments(), [](ExpressionPtr arg){ std::cout << arg->getType() << " " << arg << std::endl; });
		assert(false && "Unsupported call-target encountered - sorry!");
		return call;
	}

	CallExprPtr handleCallToBuiltIn(const CallExprPtr& call, const ExpressionList& args) {
		// obtain function
		auto fun = call->getFunctionExpr();

		// should only be called for built-in functions
		assert(manager.getLangBasic().isBuiltIn(fun));

		// use type inference for the return type
		if(manager.getLangBasic().isCompositeRefElem(fun)) {
			return static_pointer_cast<const CallExpr>(builder.refMember(args.at(0),
					static_pointer_cast<const Literal>(args.at(1))->getValue()));
		}
		if(manager.getLangBasic().isCompositeMemberAccess(fun)) {
			return static_pointer_cast<const CallExpr>(builder.accessMember(args.at(0),
					static_pointer_cast<const Literal>(args.at(1))->getValue()));
		}
		if(manager.getLangBasic().isTupleRefElem(fun)) {
			return static_pointer_cast<const CallExpr>(builder.refComponent(args.at(0), args.at(1)));
		}
		if(manager.getLangBasic().isTupleMemberAccess(fun)) {
			return static_pointer_cast<const CallExpr>(builder.accessComponent(args.at(0), args.at(1)));
		}

		if(manager.getLangBasic().isRefAssign(fun)) {
			return builder.assign(args.at(0), args.at(1));
		}

		if(manager.getLangBasic().isRefNew(fun)) {
			return builder.refNew(args.at(0));
		}

		if(manager.getLangBasic().isRefVar(fun)) {
			return builder.refVar(args.at(0));
		}

		// otherwise standard treatment
		return builder.callExpr(fun, args);
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
		ExpressionMap map = replacements;
		for_range(make_paired_range(args, params), [&](const std::pair<ExpressionPtr, VariablePtr>& cur) {
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

			auto hasReplacement = replacements.find(cur.second);
			VariablePtr param = (hasReplacement == replacements.end()) ? cur.second : (*hasReplacement).second.as<VariablePtr>();
			if (!isSubTypeOf(cur.first->getType(), param->getType())) {
				param = this->builder.variable(cur.first->getType());

				map[cur.second] = param;
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
		// preserve annotations
		utils::migrateAnnotations(lambda, newLambda);


		return builder.callExpr(callTy, newLambda, args);

	}

	ExpressionPtr handleCallToLiteral(const CallExprPtr& call, const TypePtr& resType, const ExpressionList& args) {
		const LiteralPtr literal = call->getFunctionExpr().as<LiteralPtr>();
		NodeManager& manager = call->getNodeManager();
		IRBuilder builder(manager);

		// only supported for function types
		assert(literal->getType()->getNodeType() == NT_FunctionType);

		// assemble new argument types
		TypeList newParamTypes = ::transform(args, [](const ExpressionPtr& cur)->TypePtr { return cur->getType(); });

		FunctionTypePtr newType = builder.functionType(newParamTypes, resType);
		return builder.callExpr(builder.literal(newType, literal->getValue()), args);

	}


};

class InterfaceFixer : public CachedNodeMapping {

	NodeManager& manager;

public:
	InterfaceFixer(NodeManager& manager) : manager(manager) {}

private:
	virtual const NodePtr resolveElement(const NodePtr& ptr) {
		if(LambdaExprPtr lambdaExpr = ptr.isa<LambdaExprPtr>()) {
			ParametersPtr params = lambdaExpr->getParameterList();
			FunctionTypePtr funTy = lambdaExpr->getType().isa<FunctionTypePtr>();

			if(funTy) {
				TypeList newParamTys;
				bool changed = false;
				for_range(make_paired_range(params, funTy->getParameterTypeList()), [&](const std::pair<VariablePtr, TypePtr>& cur) {
					if(cur.first->getType() != cur.second) {
						changed = true;
						newParamTys.push_back(cur.first->getType());
					} else {
						newParamTys.push_back(cur.second);
					}
				});

				if(changed) {
					IRBuilder builder(manager);
					return builder.lambdaExpr(builder.functionType(newParamTys, funTy->getReturnType()), params,
							lambdaExpr->getBody()->substitute(manager, *this));
				}
			}
		}

		return ptr->substitute(manager, *this);
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

		// fix generic operators ... gen.eq => int.eq if parameters are properly instantiated
		if (res->getNodeType() == NT_CallExpr) {
			auto& basic = manager.getLangBasic();
			CallExprPtr call = res.as<CallExprPtr>();
			if (basic.isGenOp(call->getFunctionExpr())) {

				// see whether operation can be replaced
				auto oldOp = call->getFunctionExpr().as<LiteralPtr>();
				auto opCode = basic.getOperator(oldOp);
				auto newOp = basic.getOperator(call->getArgument(0)->getType(), opCode);

				// if there was a change
				if (newOp && newOp != oldOp) {
					// exchange operator
					IRBuilder builder(manager);
					res = builder.callExpr(call->getType(), newOp, call->getArguments());
				}
			}
		}

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

	// check whether there is something to do
	if (*toReplace == *replacement) return mgr.get(root);

	if (limitScope && toReplace->getNodeType() == NT_Variable) {
		return replaceAll(mgr, root, static_pointer_cast<const Variable>(toReplace), replacement);
	}

	auto mapper = ::SingleNodeReplacer(mgr, toReplace, replacement, limitScope);
	return applyReplacer(mgr, root, mapper);
}

NodePtr replaceAll(NodeManager& mgr, const NodePtr& root, const VariablePtr& toReplace, const NodePtr& replacement, bool limitScope) {
	// check whether there is something to do
	if (*toReplace == *replacement) return mgr.get(root);

	if(limitScope) {
		auto mapper = ::VariableReplacer(mgr, toReplace, replacement);
		return applyReplacer(mgr, root, mapper);
	} else {
		auto mapper = ::SingleNodeReplacer(mgr, toReplace, replacement, limitScope);
		return applyReplacer(mgr, root, mapper);
	}
}


NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VariableMap& replacements) {

	if (root){

		// special handling for empty replacement map
		if (replacements.empty()) {
			return mgr.get(root);
		}

		// conduct actual substitutions
		auto mapper = ::VariableMapReplacer<VariablePtr>(mgr, replacements);
		return applyReplacer(mgr, root, mapper);
	}
	else
		return root;
}

NodePtr replaceVars(NodeManager& mgr, const NodePtr& root, const VarExprMap& replacements) {

	if (root){

		// special handling for empty replacement map
		if (replacements.empty()) {
			return mgr.get(root);
		}

		// conduct actual substitutions
		auto mapper = ::VariableMapReplacer<ExpressionPtr>(mgr, replacements);
		return applyReplacer(mgr, root, mapper);
	}
	else
		return root;
}

namespace {

	CallExprPtr typeDeductionBasedTypeRecovery(const CallExprPtr& call) {

		// check type
		const ExpressionPtr fun = call->getFunctionExpr();
		const ExpressionList args = call->getArguments();

		// check whether the function type has been preserved
		assert(fun->getType()->getNodeType() == NT_FunctionType && "Call is no longer targeting function after replacement!");

		FunctionTypePtr funType = fun->getType().as<FunctionTypePtr>();

		TypePtr should = deduceReturnType(funType, extractTypes(call->getArguments()), false);

		// see whether a better type has been deduced
		if (!should || *should == *call->getType()) {
			return call;
		}

		// use deduced alternative
		return IRBuilder(call->getNodeManager()).callExpr(should, call->getFunctionExpr(), call->getArguments());
	}

}

namespace {

	ExpressionPtr defaultCallExprTypeRecovery(const CallExprPtr& call) {

		// check whether target of call is a literal
		NodeManager& manager = call->getNodeManager();
		const auto& basic = manager.getLangBasic();
		if (basic.isBuiltIn(call->getFunctionExpr())) {
			IRBuilder builder(manager);

			auto args = call->getArguments();

			const ExpressionPtr& fun = call->getFunctionExpr().as<ExpressionPtr>();

			// deal with standard build-in funs
			if (basic.isCompositeRefElem(fun) &&
				args[0]->getType().isa<RefTypePtr>() &&
				args[0]->getType().as<RefTypePtr>()->getElementType().isa<NamedCompositeTypePtr>()) {
				return builder.refMember(args[0], args[1].as<LiteralPtr>()->getValue());
			}
			if (basic.isCompositeMemberAccess(fun) &&
				args[0]->getType().isa<NamedCompositeTypePtr>()) {
				return builder.accessMember(args[0], args[1].as<LiteralPtr>()->getValue());
			}
			if (basic.isTupleRefElem(fun)) {
				return builder.refComponent(args[0], args[1]);
			}
			if (basic.isTupleMemberAccess(fun)) {
				return builder.accessComponent(args[0], args[1]);
			}

			// eliminate unnecessary dereferencing
			if (basic.isRefDeref(fun) && !analysis::isRefType(args[0]->getType())) {
				return args[0];
			}
		}

		// use generic fall-back solution
		return typeDeductionBasedTypeRecovery(call);
	}

}

ExpressionPtr no_type_fixes(const ExpressionPtr&, const ExpressionPtr& newExpr) {
	return newExpr;
}

ExpressionPtr defaultTypeRecovery(const ExpressionPtr& oldExpr, const ExpressionPtr& newExpr) {

	// rule out everything that is no change
	if (*oldExpr == *newExpr) return newExpr;

	// handle call expressions externally
	if (CallExprPtr call = newExpr.isa<CallExprPtr>()) {

		// check old function / argument types have changed
		if (CallExprPtr oldCall = oldExpr.isa<CallExprPtr>()) {

			bool changed = call.size() != oldCall.size();		// check number of parameters
			changed = changed || (oldCall->getFunctionExpr()->getType() != call->getFunctionExpr()->getType());

			// check argument types
			for(std::size_t i = 0; !changed && i < call.size(); i++) {
				changed = !types::isSubTypeOf(call[i]->getType(), oldCall[i]->getType());
			}

			// if nothing has changed, consider new type to be accurate
			if (!changed) return newExpr;
		}

		return defaultCallExprTypeRecovery(call);
	}

	// handle lambda expressions
	if (LambdaExprPtr lambda = newExpr.isa<LambdaExprPtr>()) {

		auto newParams = lambda->getLambda()->getParameters();

		// see whether old version also has been a lambda
		if (LambdaExprPtr oldLambda = oldExpr.isa<LambdaExprPtr>()) {

			// check whether any of the lambdas in the definition has changed its parameter list or type
			LambdaDefinitionPtr oldDef = oldLambda->getDefinition();
			LambdaDefinitionPtr newDef = lambda->getDefinition();

			// if there is no change or a fundamental change => abort
			if (oldDef == newDef || oldDef.size() != newDef.size()) {
				return newExpr;
			}

			// see whether types are altered
			bool altered = false;
			for(decltype(oldDef.size()) i = 0; !altered && i<oldDef.size(); i++) {

				LambdaPtr oldEntry = oldDef[i]->getLambda();
				LambdaPtr newEntry = newDef[i]->getLambda();

				auto oldParams = oldEntry->getParameters();
				auto newParams = newEntry->getParameters();

				// test whether the types of the parameters have changed
				if (*oldParams == *newParams || extractTypes(oldParams->getElements()) == extractTypes(newParams->getElements())) {
					continue;
				}

				// types have changed => lambdas need to be updated
				altered = true;
			}

			// if there is no change in the types => nothing to fix
			if (!altered) return newExpr;


			// ---------- re-build lambda -------------
			IRBuilder builder(newExpr->getNodeManager());

			// 1. rebuild definition
			NodeMap recVarMap;
			vector<LambdaBindingPtr> bindings;

			for(decltype(oldDef.size()) i = 0; i<oldDef.size(); i++) {

				LambdaBindingPtr oldBinding = oldDef[i];
				LambdaBindingPtr newBinding = newDef[i];

				// check whether something has changed
				if (oldBinding == newBinding) {
					bindings.push_back(newBinding);
					continue;
				}

				// rebuild lambda using parameters and body ..
				auto newLambda = newBinding->getLambda();

				// re-build function type
				FunctionKind kind = newLambda->getType()->getKind();
				FunctionTypePtr funType;
				{
					auto paramTypes = extractTypes(newLambda->getParameters()->getElements());
					if (kind == FK_CONSTRUCTOR || kind == FK_DESTRUCTOR) {
						funType = builder.functionType(paramTypes, paramTypes[0], kind);
					} else {
						funType = builder.functionType(paramTypes, newLambda->getType()->getReturnType(), kind);
					}
				}

				// re-build lambda
				LambdaPtr lambda = builder.lambda(funType, newLambda->getParameters(), newLambda->getBody());

				// re-build recursive variable
				VariablePtr recVar = builder.variable(funType, newBinding->getVariable()->getID());
				recVarMap[newBinding->getVariable()] = recVar;

				// add new binding
				bindings.push_back(builder.lambdaBinding(recVar, lambda));
			}

			// 2. update recursive variables
			if (oldLambda->isRecursive()) {

				// update all lambda bodies to reflect new recursive variables
				for (auto cur : bindings) {
					auto curLambda = cur->getLambda();
					auto curBody = curLambda->getBody();

					auto newBody = replaceAllGen(newExpr->getNodeManager(), curBody, recVarMap, false);

					cur = builder.lambdaBinding(
							cur->getVariable(),
							builder.lambda(curLambda->getType(), curLambda->getParameters(), newBody)
					);
				}
			}

			// re-build definition
			LambdaDefinitionPtr resDef = builder.lambdaDefinition(bindings);

			// 3. re-build lambda expression
			return builder.lambdaExpr(recVarMap[lambda->getVariable()].as<VariablePtr>(), resDef);
		}

	}

	return newExpr;
}

// functor which updates the type literal inside a call to undefined in a declaration
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


NodePtr replaceVarsRecursive(NodeManager& mgr, const NodePtr& root, const ExpressionMap& replacements,
		bool limitScope, const TypeRecoveryHandler& recoveryHandler, const TypeHandler& typeHandler,
		const PointerMap<VariablePtr, ExpressionPtr>& declInitReplacements) {
	// special handling for empty replacement maps
	if (replacements.empty()) {
		return mgr.get(root);
	}

	// conduct actual substitutions
	auto mapper = ::RecVariableMapReplacer(mgr, replacements, limitScope, recoveryHandler, typeHandler, declInitReplacements);
	return applyReplacer(mgr, root, mapper);
}

NodePtr fixTypes(NodeManager& mgr, NodePtr root, const ExpressionMap& replacements, bool limitScope,
		const TypeHandler& typeHandler) {
	// conduct actual substitutions
	auto mapper = ::TypeFixer(mgr, replacements, limitScope, typeHandler);
	return applyReplacer(mgr, root, mapper);
}

NodePtr fixTypes(NodeManager& mgr, NodePtr root,  const ExpressionPtr& toReplace, const ExpressionPtr& replacement, bool limitScope,
		const TypeHandler& typeHandler ) {
	ExpressionMap replacements;
	replacements[toReplace] = replacement;
	return fixTypes(mgr, root, replacements, limitScope, typeHandler);
}

NodePtr fixInterfaces(NodeManager& mgr, NodePtr root) {
	auto mapper = ::InterfaceFixer(mgr);
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
	vector<Replacement> steps(replacements.rbegin(), replacements.rend());

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

	// short-cut for replacing the root
	if (toReplace.isRoot()) return replacement;

	// create result
	NodePtr res = replacement;

	// process the path bottom up => replace one node after another
	auto path = toReplace.getPath();

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
	if (toReplace.isRoot()) return NodeAddress(replacement);
	NodePtr newRoot = replaceNode(manager, toReplace, replacement);
	return toReplace.switchRoot(newRoot);
}



} // End transform namespace
} // End core namespace
} // End insieme namespace

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

#include "insieme/core/analysis/ir_utils.h"

#include <set>
#include <map>

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/analysis/attributes.h"

#include "insieme/core/lang/basic.h"

#include "insieme/utils/logging.h"

// WARNING: this file is only preliminary and might be heavily modified or moved ...


namespace insieme {
namespace core {
namespace analysis {

using std::map;

bool isCallOf(const CallExprPtr& candidate, const NodePtr& function) {

	// check for null
	if (!candidate) {
		return false;
	}

	// check invoked function
	return *(stripAttributes(candidate->getFunctionExpr())) == *function;
}

bool isCallOf(const NodePtr& candidate, const NodePtr& function) {

	// check for null
	if (!candidate) {
		return false;
	}

	// check type of node => has to be a call expression
	if (candidate->getNodeType() != NT_CallExpr) {
		return false;
	}

	// check invoked function
	return isCallOf(static_pointer_cast<const CallExpr>(candidate), function);
}

bool isNoOp(const StatementPtr& candidate) {

	// check for null
	if (!candidate) {
		return false;
	}

	// check type of statement => must be a compound statement
	if (candidate->getNodeType() != NT_CompoundStmt) {
		return false;
	}

	// must be an empty compound statement
	return candidate.as<CompoundStmtPtr>().empty();
}

// ------------------------------------ Begin :: isGeneric ----------------------------

namespace {

	/**
	 * Defines a set of type variables required to be passed as a context
	 * through the IsGeneric visitor.
	 */
	typedef std::set<TypeVariablePtr> TypeVariableSet;

	/**
	 * A visitor searching for free type variables or variable int type parameters.
	 * The visitor is used by the isGeneric(..) function. If one of those is present,
	 * the type is generic.
	 */
	class IsGeneric : private IRVisitor<bool, Pointer, TypeVariableSet&> {

	public:

		IsGeneric() : IRVisitor<bool, Pointer, TypeVariableSet&>(true) {}

		/**
		 * An entry point for this visitor testing whether the given type
		 * is a generic type or not.
		 */
		bool test(const TypePtr& type) {
			TypeVariableSet boundVars;
			return visit(type, boundVars);
		}

	private:

		/**
		 * When encountering a variable, check whether it is bound.
		 */
		bool visitTypeVariable(const TypeVariablePtr& var, TypeVariableSet& boundVars) {
			return boundVars.find(var) == boundVars.end();	// if not, there is a free generic variable
		}

		bool visitRefType(const RefTypePtr& type, TypeVariableSet& boundVars) {
			return visit(type->getElementType(), boundVars);
		}

		bool visitFunctionType(const FunctionTypePtr& funType, TypeVariableSet& boundVars) {
			return any(funType->getParameterTypes(), [&](const TypePtr& type) { return visit(type, boundVars); }) ||
					visit(funType->getReturnType(), boundVars);
		}

		bool visitNamedCompositeType(const NamedCompositeTypePtr& type, TypeVariableSet& boundVars) {
			return any(type->getElements(), [&](const NamedTypePtr& element) { return visit(element->getType(), boundVars); });
		}

		bool visitTupleType(const TupleTypePtr& tuple, TypeVariableSet& boundVars) {
			return any(tuple->getElements(), [&](const TypePtr& type) { return visit(type, boundVars); });
		}

		bool visitGenericType(const GenericTypePtr& type, TypeVariableSet& boundVars) {
			// generic type is generic if one of its parameters is
			return
				any(
					type->getTypeParameter()->getTypes(),
					[&](const TypePtr& type) { return visit(type, boundVars); }
				) ||
				any(
					type->getIntTypeParameter()->getParameters(),
					[&](const IntTypeParamPtr& param) { return visit(param, boundVars); }
				);
		}

		bool visitRecType(const RecTypePtr& recType, TypeVariableSet& boundVars) {
			TypeVariableSet local = boundVars;
			for(const RecTypeBindingPtr& binding : recType->getDefinition()) {
				local.insert(binding->getVariable());
			}

			// check types within recursive bindings
			return any(recType->getDefinition(), [&](const RecTypeBindingPtr& binding) { return visit(binding->getType(), local); });
		}

		bool visitType(const TypePtr& type, TypeVariableSet& boundVars) {
			return any(type->getChildList(), [&](const NodePtr& node) { return visit(node, boundVars); });
		}


		// -- int type parameters --

		bool visitVariableIntTypeParam(const VariableIntTypeParamPtr& var, TypeVariableSet& boundVars) {
			return true;	// if there are free int type parameter => it is generic
		}

		bool visitIntTypeParam(const IntTypeParamPtr& param, TypeVariableSet& boundVars) {
			return false;	// int-type params are not causing a type to be generic
		}


		/**
		 * A terminal visit capturing all non-covered types. This one should
		 * never be reached since all cases need to be covered within specialized
		 * visit members.
		 */
		bool visitNode(const NodePtr& node, TypeVariableSet& boundVars) {
			LOG(FATAL) << "Reaching " << *node << " within IsGeneric visitor!";
			assert(false && "Should not be reached!");
			return false;
		}

	};

}


bool isGeneric(const TypePtr& type) {
	// just use the IsGeneric recursive visitor based test
	return IsGeneric().test(type);
}

// ------------------------------------ End :: isGeneric ----------------------------

TypeList getElementTypes(const TypePtr& type) {
	TypeList res;
	visitDepthFirstPrunable(type, [&](const TypePtr& cur)->bool {
		if (cur == type) { return false; } // exclude root
		res.push_back(cur);
		return true;
	}, true);
	return res;
}

bool isRefOf(const NodePtr& candidate, const NodePtr& type) {

	// check for null
	if (!candidate) {
		return false;
	}

	NodePtr adjustedCandidate = candidate;
	// check if expression, if so use type
	if(ExpressionPtr expr = dynamic_pointer_cast<ExpressionPtr>(candidate)) {
		adjustedCandidate = expr->getType();
	}

	// check type of node
	if (adjustedCandidate->getNodeType() != NT_RefType) {
		return false;
	}

	// check element type 
	return *(adjustedCandidate.as<RefTypePtr>()->getElementType()) == *type;
}

bool isRefOf(const NodePtr& candidate, const NodeType kind) {

	// check for null
	if (!candidate) {
		return false;
	}
	
	NodePtr adjustedCandidate = candidate;
	// check if expression, if so use type
	if(ExpressionPtr expr = dynamic_pointer_cast<ExpressionPtr>(candidate)) {
		adjustedCandidate = expr->getType();
	}

	// check type of node
	if (adjustedCandidate->getNodeType() != NT_RefType) {
		return false;
	}

	// check element type (kind)
	return adjustedCandidate.as<RefTypePtr>()->getElementType()->getNodeType() == kind;
}

bool isTypeLiteralType(const GenericTypePtr& type) {
	// check family name as well as type and name of parameters
	return type->getName()->getValue() == "type"
			&& type->getTypeParameter().size() == static_cast<std::size_t>(1)
			&& type->getIntTypeParameter().empty();
}

bool isTypeLiteralType(const TypePtr& type) {

	// check node type
	if (type->getNodeType() != core::NT_GenericType) {
		return false;
	}

	// forward test
	return isTypeLiteralType(static_pointer_cast<const core::GenericType>(type));
}

bool isIntTypeParamType(const GenericTypePtr& type) {
	// check family name as well as type of parameters
	return type->getName()->getValue() == "intTypeParam"
			&& type->getTypeParameter().empty()
			&& type->getIntTypeParameter().size() == static_cast<std::size_t>(1);
}

bool isIntTypeParamType(const TypePtr& type) {
	// check node type
	return type && type->getNodeType() == core::NT_GenericType && isIntTypeParamType(type.as<GenericTypePtr>());
}

bool isConstructorExpr(const NodePtr& node) {
	NodeType pnt = node->getNodeType();
	return pnt == NT_VectorExpr || pnt == NT_StructExpr || pnt == NT_UnionExpr || pnt == NT_TupleExpr || pnt == NT_JobExpr;
}


bool isVolatileType(const TypePtr& type) {
	core::GenericTypePtr gt;
	return type->getNodeType() == core::NT_GenericType && 
		   (gt = static_pointer_cast<const core::GenericType>(type), 
				gt->getName()->getValue() == "volatile" && 
				gt->getTypeParameter().size() == 1u && 
				gt->getIntTypeParameter().empty()
		   );
}

TypePtr getVolatileType(const TypePtr& type) {
	assert( isVolatileType(type) );
	return core::static_pointer_cast<const core::GenericType>( type )->getTypeParameter()[0];
}

// ------ Free Variable Extraction ----------

namespace {

	/**
	 * A functor template for the free-variable collector handling the critical part distinguishing
	 * the collection of pointers and nodes.
	 */
	template<template<class Target> class Ptr>
	struct rec_free_var_collector;

	/**
	 * The specialization of the rec_free_var_collector template for pointers.
	 */
	template<>
	struct rec_free_var_collector<Pointer> {
		vector<VariablePtr> operator()(const NodePtr& cur) const { return analysis::getFreeVariables(cur); }
		VariablePtr extend(const NodePtr&, const VariablePtr& res) const { return res; }
	};

	/**
	 * The specialization of the rec_free_var_collector template for addresses.
	 */
	template<>
	struct rec_free_var_collector<Address> {
		vector<VariableAddress> operator()(const NodeAddress& cur) const {
			return getFreeVariableAddresses(cur);
		}
		VariableAddress extend(const NodeAddress& head, const VariableAddress& tail) const {
			return concat(head, tail);
		}
	};

	/**
	 * Will certainly determine the declaration status of variables inside a block.
	 */
	template<template<class Target> class Ptr>
	struct FreeVariableCollector : private IRVisitor<void, Ptr, VariableSet&, std::set<Ptr<const Variable>>&> {

		typedef std::set<Ptr<const Variable>> ResultSet;
		typedef vector<Ptr<const Variable>> ResultList;

		// do not visit types
		FreeVariableCollector() : IRVisitor<void,Ptr, VariableSet&, std::set<Ptr<const Variable>>&>(false) {}

		ResultSet run(const Ptr<const Node>& root) {

			// run visitor
			VariableSet bound;
			ResultSet free;

			// run visit
			visit(root, bound, free);

			// return result set
			return free;
		}

	private:

		void visitNode(const Ptr<const Node>& node, VariableSet& bound, ResultSet& free) {
			// ignore types
			if (node->getNodeCategory() == NC_Type) return;

			// visit all sub-nodes
			visitAll(node->getChildList(), bound, free);
		}

		void visitDeclarationStmt(const Ptr<const DeclarationStmt>& decl, VariableSet& bound, ResultSet& free) {
			// first add variable to set of bound variables
			bound.insert(decl->getVariable());

			// then visit the defining expression
			visit(decl->getInitialization(), bound, free);
		}

		void visitCompoundStmt(const Ptr<const CompoundStmt>& compound, VariableSet& bound, ResultSet& free) {

			// a compound statement creates a new scope
			VariableSet innerBound = bound;

			// continue visiting with the limited scope
			visitNode(compound, innerBound, free);
		}

		void visitCatchClause(const Ptr<const CatchClause>& clause, VariableSet& bound, ResultSet& free) {

			// a catch clause creates a new scope
			VariableSet innerBound = bound;

			// the catch-variable is a bound one
			innerBound.insert(clause->getVariable());

			// continue visiting with the limited scope
			visitNode(clause, innerBound, free);
		}

		void visitVariable(const Ptr<const Variable>& var, VariableSet& bound, ResultSet& free) {
			if(bound.find(var) == bound.end()) {
				free.insert(var);
			}
		}

		void visitLambda(const Ptr<const Lambda>& lambda, VariableSet& bound, ResultSet& free) {

			// a lambda creates a new scope
			VariableSet innerBound = bound;

			// all parameters are bound within this scope
			const auto& params = lambda->getParameters();
			innerBound.insert(params.begin(), params.end());

			// continue visiting with the limited scope
			visitNode(lambda, innerBound, free);
		}

		void visitLambdaDefinition(const Ptr<const LambdaDefinition>& definition, VariableSet& bound, ResultSet& free) {
			// a lambda creates a new scope
			VariableSet innerBound = bound;

			// register recursive lambda variables
			for(const LambdaBindingPtr& bind : definition) {
				innerBound.insert(bind->getVariable());
			}

			// process child nodes
			visitNode(definition, innerBound, free);
		}


		// due to the structure of the IR, nested lambdas can never reuse outer variables
		//  - also prevents variables in LamdaDefinition from being inadvertently captured
		void visitLambdaExpr(const Ptr<const LambdaExpr>& lambda, VariableSet& bound, ResultSet& free) {
			static const rec_free_var_collector<Ptr> collectRecursive;


			// The type used to annotate free variable lists.
			struct FreeVariableSet : public ResultSet, public value_annotation::cloneable {
				FreeVariableSet(const ResultSet& set) : ResultSet(set) {}
				FreeVariableSet(const ResultList& list) : ResultSet(list.begin(), list.end()) {}

				static VariablePtr cloneTo(NodeManager& manager, const VariablePtr& ptr) { return manager.get(ptr); }
				static VariableAddress cloneTo(NodeManager& manager, const VariableAddress& addr) { return addr.cloneTo(manager); }

				// clone free variable set to new node manager during clone operations
				void cloneTo(const NodePtr& target) const {
					ResultSet newSet;
					for(auto cur : *this) { newSet.insert(cloneTo(target->getNodeManager(), cur)); }
					target->attachValue(FreeVariableSet(newSet));
				}
			};

			// check annotation
			auto definition = lambda->getDefinition();
			if (!definition->template hasAttachedValue<FreeVariableSet>()) {
				// evaluate recursively
				definition->attachValue(FreeVariableSet(collectRecursive(definition)));
			}

			// should be fixed now
			assert(definition->template hasAttachedValue<FreeVariableSet>());
			assert(all(definition->template getAttachedValue<FreeVariableSet>(), [&](const VariablePtr& cur)->bool { return definition->getNodeManager().contains(cur); }));

			// add free variables to result set
			for(const auto& cur : definition->template getAttachedValue<FreeVariableSet>()) {
				// if variable is not defined by the enclosed lambda definition block
				if (!definition->getDefinitionOf(cur)) {
					free.insert(collectRecursive.extend(definition, cur));
				}
			}
		}

		void visitBindExpr(const Ptr<const BindExpr>& bindExpr, VariableSet& bound, ResultSet& free) {

			// first search for free variables within bound expressions
			auto expressions = bindExpr->getBoundExpressions();
			for_each(expressions, [&](const Ptr<const Expression>& e) {
				this->visit(e, bound, free);
			} );

			// add free variables encountered within call target
			visit(bindExpr->getCall()->getFunctionExpr(), bound, free);
		}
	};

}

bool hasFreeVariables(const NodePtr& code) {
	return !getFreeVariables(code).empty();
}

VariableList getFreeVariables(const NodePtr& code) {

	// collect free variables
	auto set = FreeVariableCollector<Pointer>().run(code);

	// convert result into list
	VariableList res(set.begin(), set.end());
	std::sort(res.begin(), res.end(), compare_target<VariablePtr>());
	return res;
}

vector<VariableAddress> getFreeVariableAddresses(const NodePtr& code) {

	// collect free variables
	auto set = FreeVariableCollector<Address>().run(NodeAddress(code));

	// convert result into list
	auto res = vector<VariableAddress>(set.begin(), set.end());
	std::sort(res.begin(), res.end());
	return res;
}

vector<StatementAddress> getExitPoints(const StatementPtr& stmt) {
	// delegate request to address-based implementation
	return getExitPoints(StatementAddress(stmt));
}

vector<StatementAddress> getExitPoints(const StatementAddress& stmt) {

	// list of exit points
	vector<StatementAddress> res;

	// the statement must not contain a "free" return
	visitDepthFirstOncePrunable(stmt, [&](const NodeAddress& cur) {
		if (cur->getNodeType() == NT_LambdaExpr) {
			return true; // do not decent here
		}
		if (cur->getNodeType() == NT_ReturnStmt) {
			// free return identified
			res.push_back(cur.as<StatementAddress>());
			return true;
		}
		// skip non-full expression
		if (StatementAddress stmt = cur.isa<StatementAddress>()) {
			return !stmt.isRoot() && !stmt.isa<CompoundStmtAddress>() && !stmt.getParentAddress().isa<CompoundStmtAddress>();
		}

		return false;		// check the rest
	});

	// search for "bound" break or continue statements
	visitDepthFirstOncePrunable(stmt, [&](const NodeAddress& cur) {
		if (cur->getNodeType() == NT_ForStmt || cur->getNodeType() == NT_WhileStmt) {
			return true; // do not decent here
		}
		if (cur->getNodeType() == NT_BreakStmt || cur->getNodeType() == NT_ContinueStmt) {
			// free break / continue found
			res.push_back(cur.as<StatementAddress>());
			return true;
		}

		// skip non-full expression
		if (StatementAddress stmt = cur.isa<StatementAddress>()) {
			return !stmt.isRoot() && !stmt.isa<CompoundStmtAddress>() && !stmt.getParentAddress().isa<CompoundStmtAddress>();
		}

		return false;		// check the rest
	});

	// done
	return res;
}


namespace {

	struct VariableCollector : public IRVisitor<> {

		struct VariableSetAnnotation : public VariableSet {
			VariableSetAnnotation(const VariableSet& varSet) : VariableSet(varSet) {}
		};

		VariableSet varSet;

		void visitVariable(const VariablePtr& var) {
			varSet.insert(var);		// collect value, that's all
		}

		void visitLambda(const LambdaPtr& lambda) {
			// add a cut-off at lambdas and cache results
			if (!lambda->hasAttachedValue<VariableSetAnnotation>()) {
				VariableSet local = getAllVariables(lambda->getBody());
				local.insert(lambda->getParameters().begin(),lambda->getParameters().end());
				lambda->attachValue<VariableSetAnnotation>(local);
			}
			assert(lambda->hasAttachedValue<VariableSetAnnotation>());
			const VariableSetAnnotation& set = lambda->getAttachedValue<VariableSetAnnotation>();
			varSet.insert(set.begin(), set.end());
		}

		void visitNode(const NodePtr& node) {
			// collect recursively
			visitAll(node->getChildList());
		}

	};

}


VariableSet getAllVariables(const NodePtr& code) {
	VariableCollector collector;
	collector.visit(code);
	return collector.varSet;
}


VariableSet getAllVariablesInScope(const NodePtr& code) {

	// Collect variables in current scope
	VariableSet res;
	visitDepthFirstOncePrunable(code, [&](const NodePtr& cur)->bool {
		auto type = cur->getNodeType();
		if (type == NT_Variable) {
			res.insert(cur.as<VariablePtr>());
		}

		// prune search
		return cur->getNodeCategory() == NC_Type ||
				type == NT_Variable ||
				type == NT_LambdaExpr;
	});

	// + add free variables (recursive function variables)
	for(const VariablePtr& var : getFreeVariables(code)) {
		res.insert(var);
	}

	// done
	return res;
}

namespace {
class RenamingVarVisitor: public core::IRVisitor<void, Address> {
	core::VariableAddress varAddr;

	void visitCallExpr(const CallExprAddress& call) {
		if(LambdaExprAddress lambda = dynamic_address_cast<const LambdaExpr>(call->getFunctionExpr())) {

			for_range(make_paired_range(call->getArguments(), lambda->getLambda()->getParameters()),
					[&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
					  if (*varAddr == *pair.second) {
							if(VariableAddress tmp = dynamic_address_cast<const Variable>(extractVariable(pair.first)))
								varAddr = tmp;
//							std::cout << "First->" << *pair.first << "   Second->" << *pair.second << std::endl;
						}
			});
		}
	}

	ExpressionAddress extractVariable(ExpressionAddress exp){
		if(VariableAddress var = dynamic_address_cast<const Variable>(exp))
			return var;

		if(CastExprAddress cast = dynamic_address_cast<const CastExpr>(exp))
			return extractVariable(cast->getSubExpression());

		if(CallExprAddress call = dynamic_address_cast<const CallExpr>(exp)){
			NodeManager& manager = exp->getNodeManager();
			if (manager.getLangBasic().isRefDeref(call->getFunctionExpr())){
				return extractVariable(call->getArgument(0));
			}

		}

		return exp;
	}

public:

	VariableAddress& getVariableAddr(){
		return varAddr;
	}

	RenamingVarVisitor(const core::VariableAddress& va): IRVisitor<void, Address>(false), varAddr(va) {}
};

}

utils::map::PointerMap<VariableAddress, VariableAddress> getRenamedVariableMap(const std::vector<VariableAddress> varlist){
	utils::map::PointerMap<VariableAddress, VariableAddress> varMap;
	for_each(varlist, [&](const VariableAddress& add) {
			RenamingVarVisitor rvv(add);
			visitPathBottomUp(add, rvv);
			if(VariableAddress source = rvv.getVariableAddr()) {
				if(source) {
					varMap[add] = source;
				}
			}
	});


	return varMap;
}

void getRenamedVariableMap(utils::map::PointerMap<VariableAddress, VariableAddress>& varMap){
	for_each(varMap, [&](std::pair<VariableAddress, VariableAddress> add) {
			RenamingVarVisitor rvv(add.second);
			visitPathBottomUp(add.second, rvv);
			if(VariableAddress source = rvv.getVariableAddr()) {
				if(source)
					varMap[add.first] = source;
			}
	});
}


CallExprAddress findLeftMostOutermostCallOf(const NodeAddress& root, const ExpressionPtr& fun) {
		CallExprAddress res;
		core::visitDepthFirstInterruptible(root, [&](const CallExprAddress& call)->bool {
			if (isCallOf(call.getAddressedNode(), fun)) {
				res = call; return true;
			}
			return false;
		});
		return res;
}


bool contains(const NodePtr& code, const NodePtr& element) {
	assert(element && "Element to be searched must not be empty!");
	bool checkTypes = element->getNodeCategory() == NC_Type || element->getNodeCategory() == NC_IntTypeParam;
	return code && makeCachedLambdaVisitor([&](const NodePtr& cur, const rec_call<bool>::type& rec)->bool { return *cur == *element || any(cur->getChildList(), rec); }, checkTypes)(code);
}


namespace {


	/**
	 * A container storing results and assumptions for read-only parameters
	 * in a recursive context.
	 */
	struct ReadOnlyCheck {

		/**
		 * A map mapping recursive variables and parameter positions to a flag
		 * indicating whether the corresponding parameter is a read-only parameter.
		 */
		map<pair<VariablePtr, int>, bool> cache;

		map<VariablePtr, LambdaPtr> bodyMap;

		bool isReadOnly(const VariablePtr& var, int paramPos) {

			// check whether this is a known variable
			auto bodyPair = bodyMap.find(var);
			if (bodyPair == bodyMap.end()) return false;

			// check cache
			auto pos = cache.find(std::make_pair(var, paramPos));
			if (pos != cache.end()) {
				return pos->second;
			}

			// evaluation is necessary
			LambdaPtr lambda = bodyPair->second;
			VariablePtr param = lambda->getParameters()[paramPos];
			bool res = isReadOnly(lambda, param);

			// safe result in cache
			cache.insert({{var, paramPos}, res});

			return res;
		}


		bool isReadOnly(const LambdaExprPtr& lambda, const VariablePtr& param) {

			// check the parameter
			assert(::contains(lambda->getParameterList(), param) && "Asking for non-existing parameter.");

			// simple case for non-recursive lambdas
			if (!lambda->isRecursive()) {
				// just check body
				return isReadOnly(lambda->getLambda(), param);
			}

			// ---- recursive lambdas ----

			// remember bodies
			for(const LambdaBindingPtr& cur : lambda->getDefinition()) {

				// add body
				bodyMap[cur->getVariable()] = cur->getLambda();

			}

			// assume parameter is save (co-induction)
			int pos = 0;
			ParametersPtr params = lambda->getParameterList();
			while (*params[pos] != *param) pos++;
			cache[std::make_pair(lambda->getVariable(), pos)] = true;

			// compute result
			bool res = isReadOnly(lambda->getLambda(), param);

			// update cache
			cache[std::make_pair(lambda->getVariable(), pos)] = res;

			// done
			return res;
		}

		bool isReadOnly(const LambdaPtr& lambda, const VariablePtr& param) {
			return isReadOnly(lambda->getBody(), param);
		}

		bool isReadOnly(const StatementPtr& stmt, const VariablePtr& var) {

			// non-ref values are always read-only
			if (var->getType()->getNodeType() != NT_RefType) return true;

			// get deref token
			auto deref = var->getNodeManager().getLangBasic().getRefDeref();

			bool readOnly = true;
			visitDepthFirstPrunable(NodeAddress(stmt), [&](const NodeAddress& cur) {
				// already violated => abort
				if (!readOnly) return true;

				// prune inner scopes
				if (cur->getNodeType() == NT_LambdaExpr) return true;

				// only interested in the given variable
				if (*cur != *var) { return false; }

				// check whether value is used
				if (cur.getParentNode()->getNodeType() == NT_CompoundStmt) return true;

				// if it is a call to a lambda, check the lambda
				if (CallExprPtr call = cur.getParentNode().isa<CallExprPtr>()) {

					// check calls to nested functions
					if (LambdaExprPtr fun = call->getFunctionExpr().isa<LambdaExprPtr>()) {
						if (!isReadOnly(fun, fun->getParameterList()[cur.getIndex()-2])) {		// -1 for type, -1 for function expr
							readOnly = false;		// it is no longer read-only
						}
						// we can stop the decent here
						return true;
					}

					// check calls to recursive functions
					if (VariablePtr var = call->getFunctionExpr().isa<VariablePtr>()) {
						if (!isReadOnly(var, cur.getIndex()-2)) {		// -1 for type, -1 for function expr
							readOnly = false;		// it is no longer read-only
						}
						// we can stop the decent here
						return true;
					}
				}

				// check whether variable is dereferenced at this location
				if (!isCallOf(cur.getParentNode(), deref)) {
					// => it is not, so it is used by reference
					readOnly = false;		// it is no longer read-only
					return true;			// we can stop the decent here
				}

				// continue search
				return false;
			});

			// done
			return readOnly;
		}
	};

}


bool isReadOnly(const LambdaExprPtr& lambda, const VariablePtr& param) {
	return ReadOnlyCheck().isReadOnly(lambda, param);
}

bool isReadOnly(const StatementPtr& stmt, const VariablePtr& var) {
	return ReadOnlyCheck().isReadOnly(stmt, var);
}

bool isStaticVar (const ExpressionPtr& var){
	if (const LiteralPtr lit = var.as<LiteralPtr>()){
		const lang::StaticVariableExtension& ext = var->getNodeManager().getLangExtension<lang::StaticVariableExtension>();
		return(ext.isStaticType(lit->getType().as<core::RefTypePtr>().getElementType()));
	}
	else
		return false;
}

bool compareTypes(const TypePtr& a, const TypePtr& b) {
	if( *a == *b) { return true; }
	
	//FunctionType
	if (a->getNodeType() == NT_FunctionType && b->getNodeType() == NT_FunctionType) {
		FunctionTypePtr funTypeA = static_pointer_cast<const FunctionType>(a);
		FunctionTypePtr funTypeB = static_pointer_cast<const FunctionType>(b);

		// check kind of functions
		if (funTypeA->getKind() != funTypeB->getKind()) { return false; }

		bool res = true;
		res = res && funTypeA->getParameterTypes().size() == funTypeB->getParameterTypes().size();
		res = res && compareTypes(funTypeA->getReturnType(), funTypeB->getReturnType());
		for(std::size_t i = 0; res && i<funTypeB->getParameterTypes().size(); i++) {
			res = res && compareTypes(funTypeB->getParameterTypes()[i], funTypeA->getParameterTypes()[i]);
		}
		return res;
	}
	
	//RecType
	if (a->getNodeType() == NT_RecType || b->getNodeType() == NT_RecType) {

		// if both are they have to be equivalent
		if (a->getNodeType() == NT_RecType && b->getNodeType() == NT_RecType) { return a == b; }

		// if only one is, it needs to be unrolled
		if (a->getNodeType() == NT_RecType) {
			assert(b->getNodeType() != NT_RecType);
			return compareTypes(a.as<RecTypePtr>()->unroll(), b);
		}

		if (b->getNodeType() == NT_RecType) {
			assert(a->getNodeType() != NT_RecType);
			return compareTypes(a, b.as<RecTypePtr>()->unroll());
		}
		assert(false && "How could you get here?");
	}	
	
	//RefType
	if(a.isa<RefTypePtr>() && b.isa<RefTypePtr>()) {
		return	compareTypes(a.as<RefTypePtr>()->getElementType(), b.as<RefTypePtr>()->getElementType());
	}

	//ArrayType
	if(a.isa<ArrayTypePtr>() && b.isa<ArrayTypePtr>()) {
		return	(a.as<ArrayTypePtr>()->getDimension() == b.as<ArrayTypePtr>()->getDimension()) &&
				compareTypes(a.as<ArrayTypePtr>()->getElementType(), b.as<ArrayTypePtr>()->getElementType());
	}

	//VectorType
	if(a.isa<VectorTypePtr>() && b.isa<VectorTypePtr>()) {
		return	(a.as<VectorTypePtr>()->getSize() == b.as<VectorTypePtr>()->getSize()) &&
				compareTypes(a.as<VectorTypePtr>()->getElementType(), b.as<VectorTypePtr>()->getElementType());
	}
	
	//TypeVariable
	if(a.isa<TypeVariablePtr>() || b.isa<TypeVariablePtr>() ) { return a == b; }

	//GenericType
	if(a.isa<GenericTypePtr>() && b.isa<GenericTypePtr>()) { return a == b; }
	
	//ChannelType
	if(a.isa<ChannelTypePtr>() && b.isa<ChannelTypePtr>()) { return a==b; }

	//TupleType
	if(a.isa<TupleTypePtr>() && b.isa<TupleTypePtr>()) { return a==b; }
	//StructType
	//if(a.isa<StructTypePtr>() && b.isa<StructTypePtr>()) { return a==b; }
	//UnionType
	//if(a.isa<UnionTypePtr>() && b.isa<UnionTypePtr>()) { return a==b; }

	//StructType
	//UnionType
	if(a.isa<NamedCompositeTypePtr>() && b.isa<NamedCompositeTypePtr>()) { 
		// names and sub-types have to be checked
		auto entriesA = static_pointer_cast<const NamedCompositeType>(a)->getEntries();
		auto entriesB = static_pointer_cast<const NamedCompositeType>(b)->getEntries();

		// check number of entries
		if (entriesA.size() != entriesB.size()) { return false; }

		// check all child nodes
		bool res = true;
		for (auto it = make_paired_iterator(entriesA.begin(), entriesB.begin());
				it != make_paired_iterator(entriesA.end(), entriesB.end()); ++it) {

			auto entryA = (*it).first;
			auto entryB = (*it).second;
			if (*entryA->getName() != *entryB->getName()) { return false; }

			res = res && compareTypes(entryA->getType(), entryB->getType());
		}
		return res;
	}

	return false;
}

} // end namespace utils
} // end namespace core
} // end namespace insieme

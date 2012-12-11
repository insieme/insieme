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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/analysis/attributes.h"

#include "insieme/core/lang/basic.h"

// WARNING: this file is only preliminary and might be heavily modified or moved ...


namespace insieme {
namespace core {
namespace analysis {


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
	struct FreeVariableCollector : public IRVisitor<bool, Ptr> {

		typedef std::set<Ptr<const Variable>> ResultSet;
		typedef vector<Ptr<const Variable>> ResultList;

		VariableSet bound;
		ResultSet free;

		// do not visit types
		FreeVariableCollector() : IRVisitor<bool,Ptr>(false) {}

		bool visitNode(const Ptr<const Node>& node) {
			return node->getNodeCategory() == NC_Type;
		} // default behavior: continue visiting

		bool visitDeclarationStmt(const Ptr<const DeclarationStmt>& decl) {
			bound.insert(decl->getVariable());
			return false;
		}

		bool visitVariable(const Ptr<const Variable>& var) {
			if(bound.find(var) == bound.end()) {
				free.insert(var);
			}
			return false;
		}

		bool visitLambda(const Ptr<const Lambda>& lambda) {
			// register lambda parameters to be bound
			const auto& params = lambda->getParameters();
			bound.insert(params.begin(), params.end());
			return false;
		}

		bool visitLambdaDefinition(const Ptr<const LambdaDefinition>& definition) {
			// register recursive lambda variables
			for(const LambdaBindingPtr& bind : definition) {
				bound.insert(bind->getVariable());
			}
			return false;
		}


		// due to the structure of the IR, nested lambdas can never reuse outer variables
		//  - also prevents variables in LamdaDefinition from being inadvertently captured
		bool visitLambdaExpr(const Ptr<const LambdaExpr>& lambda) {
			static const rec_free_var_collector<Ptr> collectRecursive;


			// The type used to annotate free variable lists.
			struct FreeVariableSet : public ResultSet {
				FreeVariableSet(const ResultSet& set) : ResultSet(set) {}
				FreeVariableSet(const ResultList& list) : ResultSet(list.begin(), list.end()) {}
			};

			// check annotation
			auto definition = lambda->getDefinition();
			if (!definition->template hasAttachedValue<FreeVariableSet>()) {
				// evaluate recursively
				definition->attachValue(FreeVariableSet(collectRecursive(definition)));
			}

			// should be fixed now
			assert(definition->template hasAttachedValue<FreeVariableSet>());

			// add free variables to result set
			for(const auto& cur : definition->template getAttachedValue<FreeVariableSet>()) {
				free.insert(collectRecursive.extend(definition, cur));
			}

			return true;
		}

		bool visitBindExpr(const Ptr<const BindExpr>& bindExpr) {

			// first search for free variables within bound expressions
			auto expressions = bindExpr->getBoundExpressions();
			for_each(expressions, [&](const Ptr<const Expression>& e) {
				visitDepthFirstPrunable(e, *this);
			} );

			// add free variables encountered within call target
			visitDepthFirstPrunable(bindExpr->getCall()->getFunctionExpr(), *this);

			// that's all within this branch
			return true;
		}
	};

}

VariableList getFreeVariables(const NodePtr& code) {
	FreeVariableCollector<Pointer> collector;
	visitDepthFirstOncePrunable(code, collector);

	// convert result into list
	return VariableList(collector.free.begin(), collector.free.end());
}

vector<VariableAddress> getFreeVariableAddresses(const NodePtr& code) {
	FreeVariableCollector<Address> collector;
	visitDepthFirstPrunable(NodeAddress(code), collector);

	// convert result into list
	auto res = vector<VariableAddress>(collector.free.begin(), collector.free.end());
	std::sort(res.begin(), res.end());
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

} // end namespace utils
} // end namespace core
} // end namespace insieme

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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
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

	// check type of node
	if (candidate->getNodeType() != NT_RefType) {
		return false;
	}

	// check element type
	return *(static_pointer_cast<const RefType>(candidate)->getElementType()) == *type;
}

bool isRefOf(const NodePtr& candidate, const NodeType kind) {

	// check for null
	if (!candidate) {
		return false;
	}

	// check type of node
	if (candidate->getNodeType() != NT_RefType) {
		return false;
	}

	// check element type (kind)
	return static_pointer_cast<const RefType>(candidate)->getElementType()->getNodeType() == kind;
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
	 * Will certainly determine the declaration status of variables inside a block.
	 */
	struct LambdaDeltaVisitor : public IRVisitor<bool> {
		VariableSet bound;
		VariableSet free;

		// do not visit types
		LambdaDeltaVisitor() : IRVisitor<bool>(false) {}

		bool visitNode(const NodePtr& node) {
			return node->getNodeCategory() == NC_Type;
		} // default behavior: continue visiting

		bool visitDeclarationStmt(const DeclarationStmtPtr &decl) {
			bound.insert(decl->getVariable());
			return false;
		}

		bool visitVariable(const VariablePtr& var) {
			if(bound.find(var) == bound.end()) free.insert(var);
			return false;
		}

		bool visitLambda(const LambdaPtr& lambda) {
			// register lambda parameters to be bound
			const auto& params = lambda->getParameters();
			bound.insert(params.begin(), params.end());
			return false;
		}

		bool visitLambdaDefinition(const LambdaDefinitionPtr& definition) {
			// register recursive lambda variables
			for(const LambdaBindingPtr& bind : definition) {
				bound.insert(bind->getVariable());
			}
			return false;
		}


		// due to the structure of the IR, nested lambdas can never reuse outer variables
		//  - also prevents variables in LamdaDefinition from being inadvertently captured
		bool visitLambdaExpr(const LambdaExprPtr& lambda) {

			// The type used to annotate free variable lists.
			struct FreeVariableSet : public VariableSet {
				FreeVariableSet(const VariableSet& set) : VariableSet(set) {}
				FreeVariableSet(const VariableList& list) : VariableSet(list.begin(), list.end()) {}
			};

			// check annotation
			if (!lambda->hasAttachedValue<FreeVariableSet>()) {
				// evaluate recursively
				lambda->attachValue(FreeVariableSet(getFreeVariables(lambda->getDefinition())));
			}

			// should be fixed now
			assert(lambda->hasAttachedValue<FreeVariableSet>());

			// add free variables to result set
			const FreeVariableSet& varset = lambda->getAttachedValue<FreeVariableSet>();
			free.insert(varset.begin(), varset.end());

			return true;
		}

		// for bind, just look at the variables being bound and ignore the call
		bool visitBindExpr(const BindExprPtr& bindExpr) {
			ExpressionList expressions = bindExpr->getBoundExpressions();
			for_each(expressions, [&](const ExpressionPtr& e) {
				visitDepthFirstOncePrunable(e, *this);
			} );
			return true;
		}
	};

}

VariableList getFreeVariables(const NodePtr& code) {
	LambdaDeltaVisitor ldv;
	visitDepthFirstOncePrunable(code, ldv);

	// convert result into list
	return VariableList(ldv.free.begin(), ldv.free.end());
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

} // end namespace utils
} // end namespace core
} // end namespace insieme

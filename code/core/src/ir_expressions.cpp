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

#include "insieme/core/ir_expressions.h"

#include <algorithm>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_mapper_utils.h"

namespace insieme {
namespace core {

	std::ostream& JobExpr::printTo(std::ostream& out) const {
		out << "job [" << join(", ", getLocalDecls()->getElements(), print<deref<NodePtr>>()) << "] ("
			<< join(", ", getGuardedExprs()->getElements(), print<deref<NodePtr>>())
			<< (getGuardedExprs()->empty()?"":", ") << "default: " << *getDefaultExpr() << ")";
		return out;
	}

	bool Variable::operator<(const Variable& var) const {
		VariablePtr other(&var);
		if (getId() < other->getId()) {
			return true;
		}
		return ::toString(*getType()) < ::toString(*other->getType());
	}

	bool Literal::operator<(const Literal& var) const {
		LiteralPtr other(&var);
		if (getStringValue() < other->getStringValue()) {
			return true;
		}
		return ::toString(*getType()) < ::toString(*other->getType());
	}

	bool LambdaDefinition::isRecursivelyDefined(const VariablePtr& variable) const {

		// obtain lambda definition
		const auto& lambda = getDefinitionOf(variable);

		// a detector which aborts a visiting in cased a recursive function invocation
		// is detected
		auto detector = makeLambdaVisitor([&](const NodePtr& node)->bool {
			// check node type
			if (node->getNodeType() != NT_Variable) {
				return false;
			}

			// check whether the variable is a recursive function
			return this->getDefinitionOf(static_pointer_cast<VariablePtr>(node));
		}, false);

		// run visitor => if interrupted, the definition is recursive
		return visitDepthFirstOnceInterruptible(lambda, detector);
	}


	std::ostream& LambdaExpr::printTo(std::ostream& out) const {
		return out << "rec " << *getVariable() << "." << *getDefinition();
	}

	LambdaExprPtr LambdaExpr::get(NodeManager& manager, const LambdaPtr& lambda) {
		VariablePtr var = Variable::get(manager, lambda->getType());
		LambdaBindingPtr binding = LambdaBinding::get(manager, var, lambda);
		LambdaDefinitionPtr def = LambdaDefinition::get(manager, toVector(binding));
		return get(manager, lambda->getType(), var, def);
	}

	LambdaExprPtr LambdaExpr::get(NodeManager& manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body) {
		return get(manager, Lambda::get(manager, type, params, body));
	}

	bool LambdaExpr::isRecursiveInternal() const {
		// evaluate lazily
		if (!recursive.isEvaluated()) {
			recursive.setValue(getDefinition()->isRecursive(getVariable()));
		}
		return recursive.getValue();
	}

	namespace {

		class RecLambdaUnroller : public transform::CachedNodeMapping {

			NodeManager& manager;
			const LambdaDefinitionPtr& definition;

			VariableSet recVars;


		public:

			RecLambdaUnroller(NodeManager& manager, const LambdaDefinitionPtr& definition)
				: manager(manager), definition(definition) {
				for(const LambdaBindingPtr& bind : definition->getDefinitions()) {
					recVars.insert(bind->getVariable());
				}
			}

			virtual const NodePtr resolveElement(const NodePtr& ptr) {
				// check whether it is a known variable
				if (ptr->getNodeType() == NT_Variable) {
					VariablePtr var = static_pointer_cast<const Variable>(ptr);
					if (!recVars.contains(var)) return var;
					return LambdaExpr::get(manager, var, definition);
				}

				// cut of types
				if (ptr->getNodeCategory() == NC_Type) {
					return ptr;
				}

				// special treatment of lambda expressions
				if (ptr->getNodeType() == NT_LambdaExpr) {
					// just substitute definition, but preserve variable
					auto lambda = ptr.as<LambdaExprPtr>();
					return LambdaExpr::get(manager, lambda->getVariable(), map(lambda->getDefinition()));
				}


				// cut-off nested lambda definitions
				if (ptr->getNodeType() == NT_LambdaDefinition) {

					// compute sub-set of recursive variables remaining in the unrolling
					VariableSet subRecVars;

					// filter recursive variables
					LambdaDefinitionPtr defs = ptr.as<LambdaDefinitionPtr>();
					for(const VariablePtr cur : recVars) {
						if (!defs->getDefinitionOf(cur)) {
							subRecVars.insert(cur);				// this one is not re-defined
						}
					}

					// check whether there are still recursions left
					if (subRecVars.empty()) {
						return ptr;		// no further descend necessary
					}

					// switch to sub-set of recursive variables
					VariableSet backup = recVars;
					recVars = subRecVars;

					// conduct substitution recursively
					NodePtr res = ptr->substitute(manager, *this);

					// restore backed up variable set
					recVars = backup;
					return res;
				}

				// replace recursively
				return ptr->substitute(manager, *this);
			}

			LambdaPtr apply(const LambdaPtr& node) {
				return static_pointer_cast<const Lambda>(node->substitute(manager, *this));
			}

		};

	}

	LambdaExprPtr LambdaDefinition::unrollDefinitionOnce(NodeManager& manager, const VariablePtr& variable) const {
		// unroll recursive lambda
		LambdaPtr lambda = RecLambdaUnroller(manager, LambdaDefinitionPtr(this)).apply(getDefinitionOf(variable));
		return LambdaExpr::get(manager, lambda);
	}

} // end namespace core
} // end namespace insieme

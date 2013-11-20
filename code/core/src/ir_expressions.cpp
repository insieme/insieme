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
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/ir_address.h"

namespace insieme {
namespace core {

	using std::map;

	std::ostream& JobExpr::printTo(std::ostream& out) const {
		out << "job [" << join(", ", getLocalDecls()->getElements(), print<deref<NodePtr>>()) << "] ("
			<< join(", ", getGuardedExprs()->getElements(), print<deref<NodePtr>>())
			<< (getGuardedExprs()->empty()?"":", ") << "default: " << *getDefaultExpr() << ")";
		return out;
	}

	bool Variable::operator<(const Variable& var) const {
		// smaller id or same id and "smaller" type
		return getId() < var.getId() || (getId() == var.getId() && *getType() < *var.getType());
	}

	bool Literal::operator<(const Literal& var) const {
		LiteralPtr other(&var);
		if (getStringValue() < other->getStringValue()) {
			return true;
		}
		return ::toString(*getType()) < ::toString(*other->getType());
	}


	namespace {

		/**
		 * An annotation used to store the addresses of recursive lambda calls.
		 * This annotation is attached to the LambdaBinding it is describing.
		 */
		struct RecursiveCallLocations {

			LambdaDefinitionPtr root;

			// the internal store for the recursive call locations
			map<LambdaBindingPtr, vector<VariableAddress>> bind2var;
			map<VariablePtr, vector<VariableAddress>> var2var;

			RecursiveCallLocations(const LambdaDefinitionPtr& root)
				: root(root), bind2var(), var2var() {};

			bool operator==(const RecursiveCallLocations& other) const {
				return this == &other || bind2var == other.bind2var;
			}

			bool empty() const {
				return bind2var.empty();
			}
		};


		class RecursiveCallCollector : private IRVisitor<void, Address, vector<VariableAddress>&, VariableSet&> {

			typedef IRVisitor<void, Address, vector<VariableAddress>&, VariableSet&> super;

		public:

			/**
			 * The entry point for the resolution of recursive call sides. This function finds all
			 * recursive calls within the lambda bodies.
			 *
			 * @param definition the recursive definition to be processed
			 * @param var the variable referencing the recursive function to be processed within the definition
			 * @return a list of addresses relative to the defining lambda referencing all recursive calls within
			 * 		the selected lambda.
			 */
			RecursiveCallLocations findLocations(const LambdaDefinitionPtr& definition) {

				// set up set of recursive variables
				VariableSet recVarSet;
				for(const LambdaBindingPtr& cur : definition) { recVarSet.insert(cur->getVariable()); }

				// search locations
				RecursiveCallLocations res(definition);
				for(auto cur : definition) {
					vector<VariableAddress>& curList = res.bind2var[cur];
					visit(NodeAddress(cur->getLambda()), curList, recVarSet);
				}

				// check whether some recursive calls have been
				if (res.empty()) return res;

				// get relative address from definition to the bindings
				map<LambdaBindingPtr, LambdaAddress> lambdas;
				for(auto cur : LambdaDefinitionAddress(definition)) {
					lambdas[cur.as<LambdaBindingPtr>()] = cur->getLambda();
				}

				// complete var2call index
				for(auto cur : res.bind2var) {
					auto head = lambdas[cur.first];
					for(auto var : cur.second) {
						res.var2var[var.as<VariablePtr>()].push_back(concat(head, var));
					}
				}

				// done
				return res;
			}

		private:

			void visitVariable(const VariableAddress& var, vector<VariableAddress>& res, VariableSet& recVars) {
				// if a recursive variable has been encountered => record the address
				if (recVars.contains(var)) res.push_back(var);
			}

			void visitLambdaDefinition(const LambdaDefinitionAddress& def, vector<VariableAddress>& res, VariableSet& recVars) {
				// eliminate re-defined recursive variables from recVar set
				VariableSet subSet;

				// filter recursive variables by eliminating re-defined variables
				for(const VariablePtr& var : recVars) {
					if (!def->getDefinitionOf(var)) {
						subSet.insert(var);
					}
				}

				// see whether there is still something to search
				if (subSet.empty()) return;

				// process recursively
				visitAll(def->getChildList(), res, subSet);
			}

			void visitLambdaExpr(const LambdaExprAddress& lambda, vector<VariableAddress>& res, VariableSet& recVars) {

				// check whether there are free variables matching the recursive variables
				VariableList freeVars = analysis::getFreeVariables(lambda);
				if (!any(freeVars, [&](const VariablePtr& cur) { return recVars.contains(cur); })) {
					// we can stop here (prune search space)
					return;
				}

				// continue by processing the definitions
				visitLambdaDefinition(lambda->getDefinition(), res, recVars);
			}

			void visitNode(const NodeAddress& node, vector<VariableAddress>& res, VariableSet& recVars) {
				if (node->getNodeCategory() == NC_Type) return;
				// a general forwarding to all child nodes
				visitAll(node->getChildList(), res, recVars);
			}

		};

		const RecursiveCallLocations& getRecursiveCallLocations(const LambdaDefinitionPtr& definition) {

			// compute recursive call locations if missing
			if (!definition->hasAttachedValue<RecursiveCallLocations>()) {
				definition->attachValue(RecursiveCallCollector().findLocations(definition));
			}

			// return a reference to the call location set
			const auto& res = definition->getAttachedValue<RecursiveCallLocations>();

			// this locations might be invalid if migrated => the following if is checking for this

			// check validity of annotation (not moved between node managers)
			if (res.root != definition) {
				definition->attachValue(RecursiveCallCollector().findLocations(definition));
				return definition->getAttachedValue<RecursiveCallLocations>();
			}

			return res;
		}

		const vector<VariableAddress>& getRecursiveCallLocationsInternal(const LambdaDefinitionPtr& definition, const VariablePtr& var) {
			static const vector<VariableAddress> empty;

			// get lambda binding
			auto binding = definition->getBindingOf(var);
			assert(binding && "Requesting recursive status of invalid rec-lambda variable!");

			// return a reference to the call location set
			const auto& locs = getRecursiveCallLocations(definition).bind2var;
			auto pos = locs.find(binding);
			return (pos == locs.end()) ? empty : pos->second;
		}

		const vector<VariableAddress>& getRecursiveCallsOfInternal(const LambdaDefinitionPtr& definition, const VariablePtr& var) {
			static const vector<VariableAddress> empty;

			// check if this variable is even covered
			if (!definition->getBindingOf(var)) return empty;

			// get all
			const auto& locs = getRecursiveCallLocations(definition).var2var;
			auto pos = locs.find(var);
			return (pos == locs.end()) ? empty : pos->second;
		}

	}

	std::ostream& LambdaExpr::printTo(std::ostream& out) const {
		return out << "rec " << *getVariable() << "." << *getDefinition();
	}

	LambdaExprPtr LambdaExpr::get(NodeManager& manager, const LambdaPtr& lambda) {
		VariablePtr var = Variable::get(manager, lambda->getType(), 0);
		LambdaBindingPtr binding = LambdaBinding::get(manager, var, lambda);
		LambdaDefinitionPtr def = LambdaDefinition::get(manager, toVector(binding));
		def->attachValue(RecursiveCallLocations(def));			// this is not a recursive function!
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

	LambdaExprPtr LambdaExpr::unroll(NodeManager& manager, unsigned numTimes) const {
		// TODO: check whether unrolled definitions are still mutual recursive!
		if (!isRecursive()) return manager.get(this);
		return LambdaExpr::get(manager, getVariable(), getDefinition()->unroll(manager, numTimes));
	}


	bool LambdaDefinition::isRecursivelyDefined(const VariablePtr& variable) const {
		return !getRecursiveCallLocationsInternal(this, variable).empty();
	}

	const vector<VariableAddress>& LambdaDefinition::getRecursiveCallsOf(const VariablePtr& variable) const {
		return getRecursiveCallsOfInternal(this, variable);
	}

	LambdaExprPtr LambdaDefinition::peel(NodeManager& manager, const VariablePtr& variable, unsigned numTimes) const {
		assert(getBindingOf(variable) && "Referencing undefined recursive lambda binding!");

		// terminal case => no peeling at all
		if (numTimes == 0 || !isRecursive(variable)) {
			return LambdaExpr::get(manager, variable, this);
		}
		// compute peeled code versions for each variable
		std::map<VariablePtr, LambdaExprPtr> peeled;
		for(const VariableAddress& cur : getRecursiveCallLocationsInternal(this, variable)) {
			auto pos = peeled.find(cur);
			if (pos == peeled.end()) {
				peeled[cur] = peel(manager, cur, numTimes - 1);
			}
		}

		// build up replacement map
		std::map<NodeAddress, NodePtr> replacements;
		for (const VariableAddress& cur : getRecursiveCallLocationsInternal(this, variable)) {
			replacements[cur] = peeled[cur];
		}

		// use replace-utility for peeling the lambda
		return LambdaExpr::get(manager, transform::replaceAll(manager, replacements).as<LambdaPtr>());
	}

	LambdaDefinitionPtr LambdaDefinition::unroll(NodeManager& manager, unsigned numTimes) const {

		// just return lambda definition as it is if no unrolling is requested
		if (numTimes < 2 || !isRecursive()) return this;

		// conduct the unrolling one time less => use results
		std::map<VariablePtr, LambdaExprPtr> unrolled;
		for(const LambdaBindingPtr& cur : unroll(manager, numTimes - 1)) {
			unrolled[cur->getVariable()] = LambdaExpr::get(manager, cur->getLambda());
		}

		vector<LambdaBindingPtr> newBindings;
		for(const LambdaBindingPtr& cur : *this) {

			// skip non-recursive definitions
			if (!isRecursive(cur->getVariable())) {
				newBindings.push_back(cur);
				continue;
			}

			// build up replacement map
			std::map<NodeAddress, NodePtr> replacements;
			for (const VariableAddress& var : getRecursiveCallLocationsInternal(this, cur->getVariable())) {
				replacements[var] = unrolled[var];
			}

			// convert current lambda
			newBindings.push_back(LambdaBinding::get(manager, cur->getVariable(), transform::replaceAll(manager, replacements).as<LambdaPtr>()));
		}

		// build up resulting definitions
		return LambdaDefinition::get(manager, newBindings);
	}

} // end namespace core
} // end namespace insieme

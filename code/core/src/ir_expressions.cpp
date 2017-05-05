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
 *
 */
#include "insieme/core/ir_expressions.h"

#include <algorithm>

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace core {

	using std::map;

	std::ostream& JobExpr::printTo(std::ostream& out) const {
		out << "job (" << *getBody() << ")";
		return out;
	}

	bool Variable::operator<(const Variable& var) const {
		// smaller id or same id and "smaller" type
		return getId() < var.getId() || (getId() == var.getId() && *getType() < *var.getType());
	}

	VariablePtr Variable::get(NodeManager & manager, const TypePtr& type) {
		unsigned id = manager.getFreshID();
		// Variable var(type,UIntValue::get(manager, id));
		while(manager.contains(Variable(type, UIntValue::get(manager, id)))) {
			id = manager.getFreshID();
		}
		return manager.get(Variable(type, UIntValue::get(manager, id)));
	}

	std::ostream& Literal::printTo(std::ostream & out) const {
		if(getValue()->getValue() == "type_literal") { return out << *getType(); }
		if(getValue().isa<StringValuePtr>()) { return out << *getValue() << printer::getLiteralTypeSuffix(getNodeManager().get(getNode())); }
		return out << *getValue();
	}

	bool Literal::operator<(const Literal& var) const {
		LiteralPtr other(&var);
		if(getStringValue() < other->getStringValue()) { return true; }
		return ::toString(*getType()) < ::toString(*other->getType());
	}


	LambdaPtr Lambda::get(NodeManager & manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body) {
		assert_true(::all(params, lang::isReference)) << "Unsupported non-reference parameter: " << extractTypes(params) << "\n";
		return manager.get(Lambda(type, params, body));
	}

	namespace {

		/**
		 * An annotation used to store the addresses of recursive lambda calls.
		 * This annotation is attached to the LambdaBinding it is describing.
		 */
		struct RecursiveCallLocations {
			LambdaDefinitionPtr root;

			// the internal store for the recursive call locations
			map<LambdaBindingPtr, vector<LambdaReferenceAddress>> bind2ref;
			map<LambdaReferencePtr, vector<LambdaReferenceAddress>> ref2ref;

			RecursiveCallLocations(const LambdaDefinitionPtr& root) : root(root) {};

			bool operator==(const RecursiveCallLocations& other) const {
				return this == &other || bind2ref == other.bind2ref;
			}

			bool empty() const {
				return bind2ref.empty();
			}
		};


		class RecursiveCallCollector : private IRVisitor<void, Address, vector<LambdaReferenceAddress>&, LambdaReferenceSet&> {
			typedef IRVisitor<void, Address, vector<LambdaReferenceAddress>&, LambdaReferenceSet&> super;

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
				LambdaReferenceSet recRefSet;
				for(const LambdaBindingPtr& cur : definition) {
					recRefSet.insert(cur->getReference());
				}

				// search locations
				RecursiveCallLocations res(definition);
				for(auto cur : definition) {
					vector<LambdaReferenceAddress>& curList = res.bind2ref[cur];
					visit(NodeAddress(cur->getLambda()), curList, recRefSet);
				}

				// check whether some recursive calls have been
				if(res.empty()) { return res; }

				// get relative address from definition to the bindings
				map<LambdaBindingPtr, LambdaAddress> lambdas;
				for(auto cur : LambdaDefinitionAddress(definition)) {
					lambdas[cur.as<LambdaBindingPtr>()] = cur->getLambda();
				}

				// complete var2call index
				for(auto cur : res.bind2ref) {
					auto head = lambdas[cur.first];
					for(auto ref : cur.second) {
						res.ref2ref[ref.as<LambdaReferencePtr>()].push_back(concat(head, ref));
					}
				}

				// done
				return res;
			}

		  private:

			void visitLambdaReference(const LambdaReferenceAddress& ref, vector<LambdaReferenceAddress>& res, LambdaReferenceSet& recRefs) override {
				// if a recursive variable has been encountered => record the address
				if(recRefs.contains(ref)) { res.push_back(ref); }
			}

			void visitLambdaExpr(const LambdaExprAddress& lambda, vector<LambdaReferenceAddress>& res, LambdaReferenceSet& recRefs) override {
				// skip over this reference
				visit(lambda->getDefinition(), res, recRefs);
			}

			void visitLambdaDefinition(const LambdaDefinitionAddress& def, vector<LambdaReferenceAddress>& res, LambdaReferenceSet& recRefs) override {
				// eliminate re-defined recursive variables from recVar set
				LambdaReferenceSet subSet;

				// filter recursive variables by eliminating re-defined variables
				for(const LambdaReferencePtr& ref : recRefs) {
					if(!def->getDefinitionOf(ref)) { subSet.insert(ref); }
				}

				// see whether there is still something to search
				if(subSet.empty()) { return; }

				// process recursively
				visitAll(def->getChildList(), res, subSet);
			}

			void visitNode(const NodeAddress& node, vector<LambdaReferenceAddress>& res, LambdaReferenceSet& recRefs) override {
				if(node->getNodeCategory() == NC_Type) { return; }
				// a general forwarding to all child nodes
				visitAll(node->getChildList(), res, recRefs);
			}
		};

		const RecursiveCallLocations& getRecursiveCallLocations(const LambdaDefinitionPtr& definition) {
			// compute recursive call locations if missing
			if(!definition->hasAttachedValue<RecursiveCallLocations>()) { definition->attachValue(RecursiveCallCollector().findLocations(definition)); }

			// return a reference to the call location set
			const auto& res = definition->getAttachedValue<RecursiveCallLocations>();

			// this locations might be invalid if migrated => the following if is checking for this

			// check validity of annotation (not moved between node managers)
			if(res.root != definition) {
				definition->attachValue(RecursiveCallCollector().findLocations(definition));
				return definition->getAttachedValue<RecursiveCallLocations>();
			}

			return res;
		}

		const vector<LambdaReferenceAddress>& getRecursiveCallLocationsInternal(const LambdaDefinitionPtr& definition, const LambdaReferencePtr& ref) {
			static const vector<LambdaReferenceAddress> empty;

			// get lambda binding
			auto binding = definition->getBindingOf(ref);
			assert_true(binding) << "Requesting recursive status of invalid rec-lambda variable!";

			// return a reference to the call location set
			const auto& locs = getRecursiveCallLocations(definition).bind2ref;
			auto pos = locs.find(binding);
			return (pos == locs.end()) ? empty : pos->second;
		}

		const vector<LambdaReferenceAddress>& getRecursiveCallsOfInternal(const LambdaDefinitionPtr& definition, const LambdaReferencePtr& ref) {
			static const vector<LambdaReferenceAddress> empty;

			// check if this variable is even covered
			if(!definition->getBindingOf(ref)) { return empty; }

			// get all
			const auto& locs = getRecursiveCallLocations(definition).ref2ref;
			auto pos = locs.find(ref);
			return (pos == locs.end()) ? empty : pos->second;
		}
	}

	std::ostream& LambdaExpr::printTo(std::ostream& out) const {
		return out << "rec " << *getReference() << "." << *getDefinition();
	}

	LambdaExprPtr LambdaExpr::get(NodeManager& manager, const LambdaPtr& lambda) {
		return get(manager, lambda, "_");
	}

	LambdaExprPtr LambdaExpr::get(NodeManager & manager, const LambdaPtr& lambda, const string& name) {
		LambdaReferencePtr ref = LambdaReference::get(manager, lambda->getType(), name);
		LambdaBindingMap bindings = { { ref, lambda } };
		LambdaDefinitionPtr def = LambdaDefinition::get(manager, bindings);
		def->attachValue(RecursiveCallLocations(def)); // this is not a recursive function!
		return get(manager, lambda->getType(), ref, def);
	}

	LambdaExprPtr LambdaExpr::get(NodeManager& manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body) {
		return get(manager, Lambda::get(manager, type, params, body));
	}


	bool LambdaExpr::isRecursiveInternal() const {
		// evaluate lazily
		if(!recursive.isEvaluated()) { recursive.setValue(getDefinition()->isRecursive(getReference())); }
		return recursive.getValue();
	}

	LambdaExprPtr LambdaExpr::unroll(NodeManager& manager, unsigned numTimes) const {
		// TODO: check whether unrolled definitions are still mutual recursive!
		if(!isRecursive()) { return manager.get(this); }
		return LambdaExpr::get(manager, getReference(), getDefinition()->unroll(manager, numTimes));
	}

	LambdaDefinitionPtr LambdaDefinition::get(NodeManager & manager, const LambdaBindingMap& bindings) {
		vector<LambdaBindingPtr> lambdaBindings;
		for(auto p : bindings) {
			lambdaBindings.push_back(LambdaBinding::get(manager, p.first, p.second));
		}
		std::sort(lambdaBindings.begin(), lambdaBindings.end(), [](const LambdaBindingPtr& a, const LambdaBindingPtr& b) {
			return a->getReference()->getNameAsString() < b->getReference()->getNameAsString();
		});
		return manager.get(LambdaDefinition(convertList(lambdaBindings)));
	}

	bool LambdaDefinition::isRecursivelyDefined(const LambdaReferencePtr& reference) const {
		return !getRecursiveCallLocationsInternal(this, reference).empty();
	}

	const vector<LambdaReferenceAddress>& LambdaDefinition::getRecursiveCallsOf(const LambdaReferencePtr& reference) const {
		return getRecursiveCallsOfInternal(this, reference);
	}

	LambdaExprPtr LambdaDefinition::peel(NodeManager& manager, const LambdaReferencePtr& reference, unsigned numTimes) const {
		assert_true(getBindingOf(reference)) << "Referencing undefined recursive lambda binding!";

		// terminal case => no peeling at all
		if(numTimes == 0 || !isRecursive(reference)) { return LambdaExpr::get(manager, reference, this); }
		// compute peeled code versions for each reference
		std::map<LambdaReferencePtr, LambdaExprPtr> peeled;
		for(const auto& cur : getRecursiveCallLocationsInternal(this, reference)) {
			auto pos = peeled.find(cur);
			if(pos == peeled.end()) { peeled[cur] = peel(manager, cur, numTimes - 1); }
		}

		// build up replacement map
		std::map<NodeAddress, NodePtr> replacements;
		for(const auto& cur : getRecursiveCallLocationsInternal(this, reference)) {
			replacements[cur] = peeled[cur];
		}

		// use replace-utility for peeling the lambda
		return LambdaExpr::get(manager, transform::replaceAll(manager, replacements).as<LambdaPtr>());
	}

	LambdaDefinitionPtr LambdaDefinition::unroll(NodeManager& manager, unsigned numTimes) const {
		// just return lambda definition as it is if no unrolling is requested
		if(numTimes < 2 || !isRecursive()) { return this; }

		// conduct the unrolling one time less => use results
		std::map<LambdaReferencePtr, LambdaExprPtr> unrolled;
		for(const LambdaBindingPtr& cur : unroll(manager, numTimes - 1)) {
			unrolled[cur->getReference()] = LambdaExpr::get(manager, cur->getLambda());
		}

		LambdaBindingMap newBindings;
		for(const LambdaBindingPtr& cur : *this) {
			// skip non-recursive definitions
			if(!isRecursive(cur->getReference())) {
				newBindings.insert({ cur->getReference(), cur->getLambda() });
				continue;
			}

			// build up replacement map
			std::map<NodeAddress, NodePtr> replacements;
			for(const auto& var : getRecursiveCallLocationsInternal(this, cur->getReference())) {
				replacements[var] = unrolled[var];
			}

			// convert current lambda
			newBindings.insert({ cur->getReference(), transform::replaceAll(manager, replacements).as<LambdaPtr>() });
		}

		// build up resulting definitions
		return LambdaDefinition::get(manager, newBindings);
	}

	CallExprPtr CallExpr::get(NodeManager & manager, const TypePtr& type, const ExpressionPtr& function, const NodeRange<ExpressionPtr>& arguments) {
		NodeList children;
		children.push_back(type);
		children.push_back(function);
		auto param_types = function->getType().as<FunctionTypePtr>()->getParameterTypeList();
		for(size_t i = 0; i < arguments.size(); ++i) {
			auto t = i < param_types.size() ? param_types[i] : arguments[i]->getType();
			children.push_back(Declaration::get(manager, transform::materialize(t), arguments[i]));
		}
		return manager.get(CallExpr(children));
	}

	CallExprPtr CallExpr::get(NodeManager & manager, const TypePtr& type, const ExpressionPtr& function, const NodeRange<DeclarationPtr>& argumentDecls) {
		NodeList children;
		children.push_back(type);
		children.push_back(function);
		std::copy(argumentDecls.cbegin(), argumentDecls.cend(), std::back_inserter(children));
		return manager.get(CallExpr(children));
	}

} // end namespace core
} // end namespace insieme

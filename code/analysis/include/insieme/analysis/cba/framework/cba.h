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

#pragma once

#include <set>
#include <map>
#include <tuple>
#include <unordered_map>

#include <boost/noncopyable.hpp>

#include "insieme/analysis/cba/framework/_forward_decl.h"
#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/context.h"
#include "insieme/analysis/cba/framework/entities.h"
#include "insieme/analysis/cba/framework/constraint_generator.h"

#include "insieme/analysis/cba/utils/cba_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/typed_map.h"
#include "insieme/utils/lazy.h"
#include "insieme/utils/set_constraint/solver2.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::tuple;
	using std::map;

	namespace sc = insieme::utils::set_constraint_2;



	// TODO: find a better place for this too

	// ----------------- imperative analysis ---------------

	// forward declaration
	class StateSetType;

	// since every state set type is a global constant we can use plain pointer
	typedef const StateSetType* StateSetTypePtr;

	/**
	 * The state set type is a special type of set type referencing
	 * sets attached to pairs of statements and locations (standard
	 * sets are only attached to statements).
	 */
	class StateSetType {

		/**
		 * The name of this set for printing and debugging issues.
		 */
		const string name;

	public:

		StateSetType(const string& name) : name(name) {}

	public:

		const string& getName() const {
			return name;
		}

		bool operator==(const StateSetType& other) const {
			// the identity of a set type is fixed by its address
			return this == &other;
		}

		bool operator!=(const StateSetType& other) const {
			return !(*this == other);
		}
	};

	// TODO: make those an enum
	extern const StateSetType Sin;		// in-state of statements
	extern const StateSetType Sout;		// out-state of statements
	extern const StateSetType Stmp;		// temporary states of statements (assignment only)

	class CBA : public boost::noncopyable {


		typedef sc::SetID SetID;
		typedef sc::LazySolver Solver;

		struct ContainerBase {
			virtual ~ContainerBase() {}
			virtual std::size_t getNumSets() const =0;

			virtual void addConstraintsFor(CBA& cba, const SetID& set, Constraints& res) const =0;
			virtual void plot(const CBA& cba, const std::map<SetID,string>& solution, std::ostream& out) const =0;
		};

		template<typename Context>
		struct Container : public ContainerBase {

			typedef tuple<SetTypePtr, int, Context> SetKey;
			typedef tuple<StateSetTypePtr, SetTypePtr, Location<Context>> LocationSetKey;
			typedef tuple<LocationSetKey, Label, Context> StateSetKey;

			typedef ConstraintGenerator<Context>* ConstraintGeneratorPtr;

			// a data structure managing constraint generators
			std::set<ConstraintGeneratorPtr> generator;
			std::map<SetTypePtr, ConstraintGeneratorPtr> settype2generator;			// maps a set type to a generator
			std::map<std::type_index, ConstraintGeneratorPtr> generatorIndex;			// to prevent the same type of generator being used multiple times
			std::map<LocationSetKey, ConstraintGeneratorPtr> locationGenerator;

			// set ID maps
			std::map<SetKey, SetID> sets;
			std::map<StateSetKey, SetID> stateSets;

			// reverse maps for sets (for resolution)
			std::unordered_map<SetID, SetKey> set2key;
			std::unordered_map<SetID, StateSetKey> set2statekey;

			utils::Lazy<std::vector<Context>> contexts;
			utils::Lazy<std::vector<Location<Context>>> locations;
			utils::Lazy<std::vector<Callable<Context>>> callables;

			~Container() {
				for(auto cur : generator) delete cur;
			}

			template<typename T, template<typename C> class R>
			sc::TypedSetID<T> getSet(CBA& cba, const TypedSetType<T,R>& type, int id, const Context& context) {
				SetKey key(&type, id, context);
				auto pos = sets.find(key);
				if (pos != sets.end()) {
					return sc::TypedSetID<T>(pos->second);
				}

				// make sure generator is installed
				getGenerator(cba, type);

				// create a new set
				SetID newSet(++cba.setCounter);		// reserve 0
				sets[key] = newSet;
				set2key[newSet] = key;
				return sc::TypedSetID<T>(newSet);
			}

			template<typename T, template<typename C> class G>
			sc::TypedSetID<T> getSet(CBA& cba, const StateSetType& type, Label label, const Context& context, const Location<Context>& loc, const TypedSetType<T,G>& type_loc) {
				StateSetKey key(LocationSetKey(&type, &type_loc, loc), label, context);
				auto pos = stateSets.find(key);
				if (pos != stateSets.end()) {
					return pos->second;
				}

				// TODO: make sure generator are present!

				// create new set
				sc::TypedSetID<T> newSet(++cba.setCounter);		// reserve 0
				stateSets[key] = newSet;
				set2statekey[newSet] = key;
				return newSet;
			}

			template<template<typename C> class G>
			G<Context>* getGenerator(CBA& cba) {
				std::type_index key = typeid(G<Context>);
				auto pos = generatorIndex.find(key);
				if (pos != generatorIndex.end()) {
					return static_cast<G<Context>*>(pos->second);
				}

				G<Context>* res = new G<Context>(cba);
				generator.insert(res);
				generatorIndex[key] = res;
				return res;
			}

			template<typename T, template<typename C> class G>
			G<Context>& getGenerator(CBA& cba, const TypedSetType<T,G>& type) {
				auto pos = settype2generator.find(&type);
				if (pos != settype2generator.end()) {
					return static_cast<G<Context>&>(*pos->second);
				}

				// create and register a new instance
				G<Context>* res = getGenerator<G>(cba);
				settype2generator[&type] = res;

				// also location generator instances
				for(const auto& loc : getAllLocations(cba)) {
					auto in  = new ImperativeInStateConstraintGenerator<Context,TypedSetType<T,G>>(cba, type, loc);
					auto out = new ImperativeOutStateConstraintGenerator<Context,TypedSetType<T,G>>(cba, type, loc);
					generator.insert(in);
					generator.insert(out);
					locationGenerator[LocationSetKey(&Sin,  &type, loc)] = in;
					locationGenerator[LocationSetKey(&Sout, &type, loc)] = out;
				}

				return *res;
			}

			ConstraintGeneratorPtr getGenerator(const SetType& type) const {
				auto pos = settype2generator.find(&type);
				return (pos != settype2generator.end()) ? pos->second : ConstraintGeneratorPtr();
			}

			virtual void addConstraintsFor(CBA& cba, const SetID& set, Constraints& res) const {
				// check standard set keys
				{
					auto pos = set2key.find(set);
					if (pos != set2key.end()) {

						const SetKey& key = pos->second;

						int id = std::get<1>(key);
						if (id == 0) return;		// nothing to do here
						const SetType& type = *std::get<0>(key);
						const Context& context = std::get<2>(key);

						auto generator = getGenerator(type);
						assert_true(generator) << "No generator registered for type " << type.getName() << "\n";

						// get targeted node
						core::StatementAddress trg = cba.getStmt(id);
						if (!trg) {
							// it is a variable
							trg = cba.getVariable(id);
						}

						// this should have worked
						assert(trg && "Unable to obtain target!");

						// run resolution
						generator->addConstraints(trg, context, res);

						// done
						return;
					}
				}

				// try a state formula
				{
					auto pos = set2statekey.find(set);
					if (pos != set2statekey.end()) {
						const StateSetKey& key = pos->second;

						// get targeted node
						core::StatementAddress trg = cba.getStmt(std::get<1>(key));

						// run resolution
						if (trg) {
							auto type = std::get<0>(key);

							// ignore temporaries
							if (std::get<0>(type) == &Stmp) return;

							// obtain generator
							auto pos = locationGenerator.find(type);
							assert_true(pos != locationGenerator.end()) << "No generator registered for type " << std::get<0>(type)->getName() << "/" << std::get<1>(type)->getName() << "\n";

							// add constraints
							pos->second->addConstraints(trg, std::get<2>(key), res);
						}

						// done
						return;
					}
				}

				// an unknown set?
				assert_true(false) << "Unknown set encountered: " << set << "\n";
			}

			const std::vector<Context>& getContexts(CBA& cba) {
				if (contexts) return contexts;

				// create list of valid call contexts
				vector<typename Context::call_context> callContexts = generateSequences<Context::call_ctxt_size>(cba.getDynamicCallLabels(),
						[&](const std::array<Label,Context::call_ctxt_size>& cur) {
							return cba.isValid(cur);
						}
				);

				contexts = std::vector<Context>();
				for(const auto& cur : callContexts) {
					contexts->push_back(Context(cur));
				}

				return contexts;
			}

			const std::vector<Location<Context>>& getLocations(CBA& cba) {
				if (locations) return locations;
				locations = getAllLocations(cba);
				return locations;
			}

			const std::vector<Callable<Context>>& getCallables(CBA& cba) {
				if (callables) return callables;
				callables = getAllCallables(cba);
				return callables;
			}

			virtual std::size_t getNumSets() const {
				return sets.size() + stateSets.size();
			}

			virtual void plot(const CBA& cba, const std::map<SetID,string>& solution, std::ostream& out) const {

				// a utility obtaining the address of a label
				auto getAddress = [&](const Label l)->StatementAddress {
					if (l == 0) return cba.root;
					auto res = cba.getStmt(l);
					return (res) ? res : cba.getVariable(l);
				};

				auto getSolution = [&](const SetID& set)->const string& {
					static const string& none = "";
					auto pos = solution.find(set);
					return (pos != solution.end()) ? pos->second : none;
				};

				// name sets
				for(auto cur : sets) {
					string setName = std::get<0>(cur.first)->getName();
					auto pos = getAddress(std::get<1>(cur.first));
					out << "\n\t" << cur.second
							<< " [label=\"" << cur.second << " = " << setName
								<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]"
							<< " = " << getSolution(cur.second) << "\""
							<< ((cba.solver.isResolved(cur.second)) ? " shape=box" : "") << "];";
				}

				for(auto cur : stateSets) {
					auto& loc = std::get<0>(cur.first);
					string setName = std::get<0>(loc)->getName();
					string dataName = std::get<1>(loc)->getName();
					auto pos = getAddress(std::get<1>(cur.first));
					out << "\n\t" << cur.second
							<< " [label=\"" << cur.second << " = " << setName << "-" << dataName << "@" << std::get<2>(loc)
								<< "[l" << std::get<1>(cur.first) << " = " << pos->getNodeType() << " : " << pos << " : " << std::get<2>(cur.first) << "]"
							<< " = " << getSolution(cur.second) << "\""
							<< ((cba.solver.isResolved(cur.second)) ? " shape=box" : "") << "];";
				}
			}

		private:

			// a utility function extracting a list of memory location constructors from the given code fragment
			vector<Location<Context>> getAllLocations(CBA& cba) {
				vector<Location<Context>> res;

				// TODO: cache constructor expressions in cba!

				// collect all memory location constructors
				core::visitDepthFirst(cba.getRoot(), [&](const core::ExpressionAddress& cur) {
					// TODO: add context info to locations
					if (isMemoryConstructor(cur)) {
						res.push_back(Location<Context>(cur));
					}
				});
				return res;
			}

			// a utility function extracting a list of callables
			vector<Callable<Context>> getAllCallables(CBA& cba) {
				vector<Callable<Context>> res;

				for(const auto& fun : cba.getFreeFunctions()) {

					if (fun->getNodeType() == core::NT_LambdaExpr) {

						auto lambda = fun.template as<core::LambdaExprAddress>();
						res.push_back(Callable<Context>(lambda));

					} else if (fun->getNodeType() == core::NT_BindExpr) {

						auto bind = fun.template as<core::BindExprAddress>();
						for(const auto& ctxt : getContexts(cba)) {
							res.push_back(Callable<Context>(bind, ctxt));
						}

					} else {

						assert_fail() << "Encountered unexpected function type: " << fun->getNodeType();
					}
				}

				return res;
			}
		};

		typedef utils::TypedMap<Container, ContainerBase> index_map_type;

		core::StatementAddress root;

		Solver solver;

		int setCounter;

		index_map_type indices;

		// two caches for resolving labels and variables
		int idCounter;
		std::unordered_map<core::StatementAddress, Label> labels;
		std::unordered_map<core::VariableAddress, Variable> vars;

		// a reverse lookup structure for labels
		std::unordered_map<Label, core::StatementAddress> reverseLabels;
		std::unordered_map<Variable, core::VariableAddress> reverseVars;


		// TODO: move this part to some plug-in system

		// a list of all dynamic calls within the targeted fragment - filled by the constructor
		utils::Lazy<std::vector<core::CallExprAddress>> dynamicCalls;
		utils::Lazy<std::vector<Label>> dynamicCallLabels;

		utils::Lazy<std::vector<ContextFreeCallable>> freeFunctions;

		std::unordered_map<SetID, ContainerBase*> set2container;

	public:

		CBA(const core::StatementAddress& root);

		// basic functionality

		const core::StatementAddress& getRoot() const {
			return root;
		}


		// -- main entry point for running analysis --

		template<typename T, template<typename C> class G, typename C = DefaultContext>
		const std::set<T>& getValuesOf(const core::ExpressionAddress& expr, const TypedSetType<T,G>& set, const C& ctxt = C()) {
			auto id = getSet(set, getLabel(expr), ctxt);
			return solver.solve(id)[id];
		}


		// -- set management --

		template<typename T, template<typename C> class G, typename Context = DefaultContext>
		sc::TypedSetID<T> getSet(const TypedSetType<T,G>& type, int id, const Context& context = Context()) {
			Container<Context>& container = getContainer<Context>();
			sc::TypedSetID<T> res = container.getSet(*this, type, id, context);
			set2container[res] = &container;
			return res;
		}

		template<typename T, template<typename C> class G, typename Context = DefaultContext>
		sc::TypedSetID<T> getSet(const StateSetType& type, Label label, const Context& context, const Location<Context>& loc, const TypedSetType<T,G>& type_loc) {
			Container<Context>& container = getContainer<Context>();
			sc::TypedSetID<T> res = container.getSet(*this, type, label, context, loc, type_loc);
			set2container[res] = &container;
			return res;
		}

		// -- label management --

		Label getLabel(const core::StatementAddress& expr) {
			auto pos = labels.find(expr);
			if (pos != labels.end()) {
				return pos->second;
			}
			Label l = ++idCounter;		// reserve 0 for the empty set
			labels[expr] = l;
			reverseLabels[l] = expr;
			return l;
		}

		Label getLabel(const core::StatementAddress& expr) const {
			auto pos = labels.find(expr);
			return (pos != labels.end()) ? pos->second : 0;
		}

		core::StatementAddress getStmt(Label label) const {
			auto pos = reverseLabels.find(label);
			return (pos != reverseLabels.end()) ? pos->second : core::StatementAddress();
		}


		// -- variable management --

		Variable getVariable(const core::VariableAddress& var) {
			auto pos = vars.find(var);
			if (pos != vars.end()) {
				return pos->second;
			}

			// get the definition point
			core::VariableAddress def = getDefinitionPoint(var);

			Variable res;
			if (def == var) {
				res = getLabel(def);		// use label of definition point
				reverseVars[res] = def;
			} else {
				res = getVariable(def);
			}
			vars[var] = res;
			return res;
		}

		Variable getVariable(const core::VariableAddress& var) const {
			auto pos = vars.find(var);
			return (pos != vars.end()) ? pos->second : 0;
		}

		core::VariableAddress getVariable(const Variable& var) const {
			auto pos = reverseVars.find(var);
			return (pos != reverseVars.end()) ? pos->second : core::VariableAddress();
		}


		// -------------- static analysis data input code -----------------

		template<typename Context>
		const std::vector<Location<Context>>& getLocations() {
			return getContainer<Context>().getLocations(*this);
		}

		// TODO: remove default values
		// TODO: think about moving this to the reference analysis module
		template<typename Context>
		Location<Context> getLocation(const core::ExpressionAddress& ctor) { // TODO: add support: , const CallContext& c = CallContext(), const ThreadContext& t = ThreadContext()) {
			Context context;

			assert(isMemoryConstructor(ctor));

			// obtain address of definition point
			auto def = getLocationDefinitionPoint(ctor);

			// for globals the call context and thread context is not relevant
			if (ctor.isa<core::LiteralPtr>()) {
				return Location<Context>(def, Context());
			}

			// create the location instance
			return Location<Context>(def, context);
		}

		template<typename Context>
		const std::vector<Callable<Context>>& getCallables() {
			return getContainer<Context>().getCallables(*this);
		}

		const std::vector<ContextFreeCallable>& getFreeFunctions() {
			if (freeFunctions) return freeFunctions;
			freeFunctions = getAllFreeFunctions(root);
			return freeFunctions;
		}

		const std::vector<core::CallExprAddress>& getDynamicCalls() {
			if (dynamicCalls) return dynamicCalls;

			dynamicCalls = std::vector<core::CallExprAddress>();

			// fill dynamicCalls
			core::visitDepthFirst(root, [&](const core::CallExprAddress& call) {
				auto fun = call->getFunctionExpr();
				if (fun.isa<core::LiteralPtr>() || fun.isa<core::LambdaExprPtr>() || fun.isa<core::BindExprPtr>()) return;
				this->dynamicCalls->push_back(call);
			});

			return dynamicCalls;
		}

		const std::vector<Label>& getDynamicCallLabels() {
			if (dynamicCallLabels) return dynamicCallLabels;
			dynamicCallLabels = ::transform(getDynamicCalls(), [&](const core::CallExprAddress& cur) { return getLabel(cur); });
			dynamicCallLabels->push_back(0);
			return dynamicCallLabels;
		}

		// -------------- static computation of call sites -----------------

		typedef boost::optional<std::vector<Label>> OptCallSiteList;
		std::map<core::ExpressionAddress, OptCallSiteList> callSiteCache;

		/**
		 * Tries to determine all uses of the given function (lambda or bind) and returns
		 * the vector of dynamic call labels accessing the function. If the uses can not
		 * be determined statically, the optional result will not be filled.
		 */
		const OptCallSiteList& getAllStaticUses(const core::ExpressionAddress& fun);

		/**
		 * Obtains a list of statically known predecessors of the given statement in
		 * potential call sequences. The optional result will be empty if there is none.
		 * If the uses can not be determined statically, the optional result will not be filled.
		 */
		OptCallSiteList getAllStaticPredecessors(const core::CallExprAddress& call);

		/**
		 * Obtains a list of statically known predecessors of the given label in
		 * potential call sequences. The optional result will be empty if there is none.
		 * If the uses can not be determined statically, the optional result will not be filled.
		 */
		OptCallSiteList getAllStaticPredecessors(const Label& label) {
			static const OptCallSiteList zero = toVector<Label>(0);
			if (label == 0) return zero;
			return getAllStaticPredecessors(getStmt(label).as<CallExprAddress>());
		}


		// -------------- Call-Candidate computation -----------------

		typedef std::vector<ContextFreeCallable> ContextFreeCallableList;
		typedef boost::optional<ContextFreeCallableList> OptContextFreeCallableList;
		std::map<core::CallExprAddress, OptContextFreeCallableList> contextFreeCallableCandidateCache;

		const ContextFreeCallableList& getContextFreeCallableCandidate(const core::CallExprAddress& call);


		// -------------- Static Context Filter -----------------

		bool isValid(const std::array<Label, 0>& call_ctxt) { return true; }
		bool isValid(const std::array<Label, 1>& call_ctxt) { return contains(getDynamicCallLabels(), call_ctxt.front()); }

		template<std::size_t size>
		bool isValid(const std::array<Label, size>& seq) {
			assert(size >= 2);

			// check sequence
			for(std::size_t i=0; i<size-1; i++) {
				const auto& list = getAllStaticPredecessors(seq[i+1]);
				if (list && !contains(*list, seq[i])) return false;
			}
			return true;
		}

		template<unsigned size>
		bool isValid(const Sequence<Label, size>& seq) {
			return isValid(seq.sequence);
		}

		template<unsigned a, unsigned b, unsigned c>
		bool isValid(const Context<a,b,c>& ctxt) {
			return isValid(ctxt.callContext);
		}

		template<typename Context = DefaultContext>
		const vector<Context>& getValidContexts() {
			return getContainer<Context>().getContexts(*this);
		}

		// ----------------------- some debugging utilities ---------------------------

		void plot(std::ostream& out = std::cout) const;

		std::size_t getNumSets() const {
			std::size_t sum = 0;
			for(auto cur : indices) {
				sum += cur.second->getNumSets();
			}
			return sum;
		}

		std::size_t getNumConstraints() const {
			return solver.getConstraints().size();
		}

	private:

		template<typename T>
		Container<T>& getContainer() { return indices.get<T>(); }

		void addConstraintsFor(const SetID& set, Constraints& res);

	};

	template<typename Context>
	bool isValidContext(CBA& cba, const Context& context) {
		return cba.isValid(context);
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

/**
 * This include has to follow the CBA class definition due to dependencies and needs to
 * be always included whenever the CBA class is included. That is why it is located at
 * the end of the file and must not be moved to the top.
 */
#include "insieme/analysis/cba/framework/generator/mutable_data.h"

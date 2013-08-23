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

#include "insieme/analysis/cba/framework/forward_decl.h"
#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/context.h"
#include "insieme/analysis/cba/framework/entitiey.h"
#include "insieme/analysis/cba/framework/constraint_resolver.h"

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

	extern const StateSetType Sin;		// in-state of statements
	extern const StateSetType Sout;		// out-state of statements
	extern const StateSetType Stmp;		// temporary states of statements (assignment only)

	class CBA : public boost::noncopyable {


		typedef sc::SetID SetID;
		typedef sc::LazySolver Solver;

		struct ContainerBase {
			virtual ~ContainerBase() {}
		};

		template<typename Context>
		struct Container : public ContainerBase {

			typedef tuple<SetTypePtr, int, Context> SetKey;
			typedef tuple<StateSetTypePtr, SetTypePtr, Location<Context>> LocationSetKey;
			typedef tuple<LocationSetKey, Label, Context> StateSetKey;

			typedef ConstraintResolver<Context>* ConstraintResolverPtr;

			// a data structure managing constraint resolvers
			std::set<ConstraintResolverPtr> resolver;
			std::map<SetTypePtr, ConstraintResolverPtr> setResolver;
			std::map<LocationSetKey, ConstraintResolverPtr> locationResolver;

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
				for(auto cur : resolver) delete cur;
			}

			template<typename T, template<typename C> class R>
			sc::TypedSetID<T> getSet(CBA& cba, const TypedSetType<T,R>& type, int id, const Context& context) {
				SetKey key(&type, id, context);
				auto pos = sets.find(key);
				if (pos != sets.end()) {
					return sc::TypedSetID<T>(pos->second);
				}

				// make sure resolver is installed
				getResolver(cba, type);

				// create a new set
				SetID newSet(++cba.setCounter);		// reserve 0
				sets[key] = newSet;
				set2key[newSet] = key;
				return sc::TypedSetID<T>(newSet);
			}

			template<typename T, template<typename C> class R>
			sc::TypedSetID<T> getSet(CBA& cba, const StateSetType& type, Label label, const Context& context, const Location<Context>& loc, const TypedSetType<T,R>& type_loc) {
				StateSetKey key(LocationSetKey(&type, &type_loc, loc), label, context);
				auto pos = stateSets.find(key);
				if (pos != stateSets.end()) {
					return pos->second;
				}

				// TODO: make sure resolver are present!

				// create new set
				sc::TypedSetID<T> newSet(++cba.setCounter);		// reserve 0
				stateSets[key] = newSet;
				set2statekey[newSet] = key;
				return newSet;
			}

			template<typename T, template<typename C> class R>
			R<Context>& getResolver(CBA& cba, const TypedSetType<T,R>& type) {
				auto pos = setResolver.find(&type);
				if (pos != setResolver.end()) {
					return static_cast<R<Context>&>(*pos->second);
				}

				// create and register a new instance
				R<Context>* res = new R<Context>(cba);
				resolver.insert(res);
				setResolver[&type] = res;
				return *res;
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

	public:

		CBA(const core::StatementAddress& root);

		// basic functionality

		const core::StatementAddress& getRoot() const {
			return root;
		}


		// -- main entry point for running analysis --

		template<typename T, template<typename C> class R, typename C = DefaultContext>
		const std::set<T>& getValuesOf(const core::ExpressionAddress& expr, const TypedSetType<T,R>& set, const C& ctxt = C()) {
			auto id = getSet(set, getLabel(expr), ctxt);
			return solver.solve(id)[id];
		}


		// -- set management --

		template<typename T, template<typename C> class R, typename Context = DefaultContext>
		sc::TypedSetID<T> getSet(const TypedSetType<T,R>& type, int id, const Context& context = Context()) {
			return getContainer<Context>().getSet(*this, type, id, context);
		}

		template<typename T, template<typename C> class R, typename Context = DefaultContext>
		sc::TypedSetID<T> getSet(const StateSetType& type, Label label, const Context& context, const Location<Context>& loc, const TypedSetType<T,R>& type_loc) {
			return getContainer<Context>().getSet(*this, type, label, context, loc, type_loc);
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
				res = ++idCounter; 		// reserve 0 for the empty set
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
		 */
		const OptCallSiteList& getAllStaticPredecessors(const core::StatementAddress& stmt);

		/**
		 * Obtains a list of statically known predecessors of the given label in
		 * potential call sequences. The optional result will be empty if there is none.
		 */
		const OptCallSiteList& getAllStaticPredecessors(const Label& label) {
			static const OptCallSiteList zero = toVector<Label>(0);
			if (label == 0) return zero;
			return getAllStaticPredecessors(getStmt(label));
		}


		// -------------- Call-Candidate computation -----------------

		typedef std::vector<ContextFreeCallable> ContextFreeCallableList;
		typedef boost::optional<ContextFreeCallableList> OptContextFreeCallableList;
		std::map<core::CallExprAddress, OptContextFreeCallableList> contextFreeCallableCandidateCache;

		const ContextFreeCallableList& getContextFreeCallableCandidate(const core::CallExprAddress& call);


//		template<typename Context>
//		bool isValid(const Context& ctxt) const {
//			// TODO: materialize all contexts?
//			if (Context::call_context::size < 2) return true;
//
//			// check sequence
//			const auto& seq = ctxt.callContext;
//			for(int i=0; i<Context::call_context::size-1; i++) {
//				const auto& list = getAllStaticPredecessors(seq[i+1]);
//				if (list && !contains(*list, seq[i])) return false;
//			}
//			return true;
//		}

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

	private:

		template<typename T>
		Container<T>& getContainer() { return indices.get<T>(); }

		void addConstraintsFor(const SetID& set, Constraints& res) {}

//
//		template<typename T>
//		const std::set<T>& getValuesOf(const core::ExpressionAddress& expr, const TypedSetType<T>& set, const Context& ctxt = Context()) {
//			auto id = getSet(set, getLabel(expr), ctxt);
//			return solver.solve(id)[id];
//		}
//
//		template<typename T>
//		const std::set<T>& getValuesOf(const core::VariableAddress& var, const TypedSetType<T>& set, const Context& ctxt = Context()) {
//			auto id = getSet(set, getVariable(var), ctxt);
//			return solver.solve(id)[id];
//		}
//
//		const std::set<core::ExpressionPtr>& getValuesOf(const core::ExpressionAddress& expr, const Context& ctxt = Context()) {
//			return getValuesOf(expr, D, ctxt);
//		}
//
//		const std::set<core::ExpressionPtr>& getValuesOf(const core::VariableAddress& var, const Context& ctxt = Context()) {
//			return getValuesOf(var, d, ctxt);
//		}
//
//		ConstraintResolverPtr getResolver(const SetType& set) {
//			auto& res = setResolver[&set];
//			return (res) ? res : set.getConstraintResolver(*this);
//		}
//
//		template<typename R>
//		typename std::enable_if<std::is_base_of<ConstraintResolver, R>::value, ConstraintResolver*>::type
//		getResolver(const SetType& set) {
//			auto& res = setResolver[&set];
//			if (!res) {
//				res = new R(*this);
//				resolver.insert(res);
//			}
//			return res;
//		}
//
//		template<typename R, typename T, typename ... Args>
//		typename std::enable_if<std::is_base_of<ConstraintResolver, R>::value, void>::type
//		registerLocationResolver(const StateSetType& stateSetType, const TypedSetType<T>& dataType, const Location& location, const Args& ... args) {
//			auto key = std::make_tuple(&stateSetType,  (const SetType*)(&dataType), location);
//
//			// if there is already a resolver installed => skip new one
//			auto& cur = locationResolver[key];
//			if (cur) return;
//
//			// create new resolver and register it
//			cur = new R(*this, dataType, location, args ...);
//			resolver.insert(cur);
//		}
//
//		const std::set<ConstraintResolverPtr>& getAllResolver() const {
//			return resolver;
//		}
//
//		void addConstraintsFor(const SetID& set, Constraints& res);
//
//		template<typename T>
//		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const Context& context = Context()) {
//			return utils::set_constraint_2::TypedSetID<T>(getSet(static_cast<const SetType&>(type), id, context));
//		}
//
//		utils::set_constraint_2::SetID getSet(const SetType& type, int id, const Context& context = Context()) {
//			SetKey key(&type, id, context);
//			auto pos = sets.find(key);
//			if (pos != sets.end()) {
//				return pos->second;
//			}
//			utils::set_constraint_2::SetID newSet(++setCounter);		// reserve 0
//			sets[key] = newSet;
//			set2key[newSet] = key;
//			return newSet;
//		}
//
//		template<typename T>
//		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const Context& context = Context()) const {
//			return utils::set_constraint_2::TypedSetID<T>(getSet(static_cast<const SetType&>(type), id, context));
//		}
//
//		utils::set_constraint_2::SetID getSet(const SetType& type, int id, const Context& context = Context()) const {
//			static const utils::set_constraint_2::SetID empty(0);
//
//			SetKey key(&type, id, context);
//			auto pos = sets.find(key);
//			if (pos != sets.end()) {
//				return pos->second;
//			}
//
//			// use empty set as the default
//			return empty;
//		}
//
//
//		template<typename T>
//		utils::set_constraint_2::TypedSetID<T> getSet(const StateSetType& type, Label label, const Context& context, const Location& loc, const TypedSetType<T>& type_loc) {
//			StateSetKey key(LocationSetKey(&type, &type_loc, loc), label, context);
//			auto pos = stateSets.find(key);
//			if (pos != stateSets.end()) {
//				return pos->second;
//			}
//			utils::set_constraint_2::TypedSetID<T> newSet(++setCounter);		// reserve 0
//			stateSets[key] = newSet;
//			set2statekey[newSet] = key;
//			return newSet;
//		}
//
//		Label getLabel(const core::StatementAddress& expr) {
//			auto pos = labels.find(expr);
//			if (pos != labels.end()) {
//				return pos->second;
//			}
//			Label l = ++idCounter;		// reserve 0 for the empty set
//			labels[expr] = l;
//			reverseLabels[l] = expr;
//			return l;
//		}
//
//		Label getLabel(const core::StatementAddress& expr) const {
//			auto pos = labels.find(expr);
//			return (pos != labels.end()) ? pos->second : 0;
//		}
//
//		core::StatementAddress getStmt(Label label) const {
//			auto pos = reverseLabels.find(label);
//			return (pos != reverseLabels.end()) ? pos->second : core::StatementAddress();
//		}
//
//		Variable getVariable(const core::VariableAddress& var) {
//			auto pos = vars.find(var);
//			if (pos != vars.end()) {
//				return pos->second;
//			}
//
//			// get the definition point
//			core::VariableAddress def = getDefinitionPoint(var);
//
//			Variable res;
//			if (def == var) {
//				res = ++idCounter; 		// reserve 0 for the empty set
//				reverseVars[res] = def;
//			} else {
//				res = getVariable(def);
//			}
//			vars[var] = res;
//			return res;
//		}
//
//		Variable getVariable(const core::VariableAddress& var) const {
//			auto pos = vars.find(var);
//			return (pos != vars.end()) ? pos->second : 0;
//		}
//
//		core::VariableAddress getVariable(const Variable& var) const {
//			auto pos = reverseVars.find(var);
//			return (pos != reverseVars.end()) ? pos->second : core::VariableAddress();
//		}
//
//		// TODO: remove default values
//		Location getLocation(const core::ExpressionAddress& ctor) { // TODO: add support: , const CallContext& c = CallContext(), const ThreadContext& t = ThreadContext()) {
//			Context context;
//
//			assert(isMemoryConstructor(ctor));
//
//			// obtain address of definition point
//			auto def = getLocationDefinitionPoint(ctor);
//
//			// for globals the call context and thread context is not relevant
//			if (ctor.isa<core::LiteralPtr>()) {
//				return Location(def, Context());
//			}
//
//			// create the location instance
//			return Location(def, context);
//		}
//
//		// only for debugging
//		core::ExpressionAddress getMemoryConstructor(const Location& loc) {
//			return std::get<0>(loc);
//		}
//
//		const std::vector<Location>& getLocations() const {
//			return locations;
//		}
//
//		const std::vector<ContextFreeCallable>& getFreeFunctions() const {
//			return freeFunctions;
//		}
//
//		const std::vector<core::CallExprAddress>& getDynamicCalls() const {
//			return dynamicCalls;
//		}
//
//		const std::vector<Label>& getDynamicCallLabels() const {
//			return dynamicCallLabels;
//		}
//
//		// -------------- static computation of call sites -----------------
//
//		typedef boost::optional<std::vector<Label>> OptCallSiteList;
//		std::map<core::ExpressionAddress, OptCallSiteList> callSiteCache;
//
//		/**
//		 * Tries to determine all uses of the given function (lambda or bind) and returns
//		 * the vector of dynamic call labels accessing the function. If the uses can not
//		 * be determined statically, the optional result will not be filled.
//		 */
//		const OptCallSiteList& getAllStaticUses(const core::ExpressionAddress& fun);
//
//		/**
//		 * Obtains a list of statically known predecessors of the given statement in
//		 * potential call sequences. The optional result will be empty if there is none.
//		 */
//		const OptCallSiteList& getAllStaticPredecessors(const core::StatementAddress& stmt);
//
//		/**
//		 * Obtains a list of statically known predecessors of the given label in
//		 * potential call sequences. The optional result will be empty if there is none.
//		 */
//		const OptCallSiteList& getAllStaticPredecessors(const Label& label) {
//			static const OptCallSiteList zero = toVector<Label>(0);
//			if (label == 0) return zero;
//			return getAllStaticPredecessors(getStmt(label));
//		}
//
//		bool isValid(const Context& ctxt) {
//			// TODO: materialize all contexts?
//			if (Context::CallContext::size < 2) return true;
//
//			// check sequence
//			const auto& seq = ctxt.callContext;
//			for(int i=0; i<Context::CallContext::size-1; i++) {
//				const auto& list = getAllStaticPredecessors(seq[i+1]);
//				if (list && !contains(*list, seq[i])) return false;
//			}
//			return true;
//		}
//
//		typedef std::vector<Callable> CallableList;
//		typedef boost::optional<CallableList> OptCallableList;
//		std::map<core::CallExprAddress, OptCallableList> callableCandidateCache;
//
//		/**
//		 * Obtains a full list of potential callables.
//		 */
//		const CallableList& getCallables() const {
//			return callables;
//		}
//
//		/**
//		 * Obtains a list of candidate callables to be called by the given call expression.
//		 */
//		const CallableList& getCallableCandidates(const core::CallExprAddress& call);
//
//		const CallableList& getCallableCandidates(const Label& callSite) {
//			return getCallableCandidates(getStmt(callSite).as<core::CallExprAddress>());
//		}
//
//		typedef std::vector<ContextFreeCallable> ContextFreeCallableList;
//		typedef boost::optional<ContextFreeCallableList> OptContextFreeCallableList;
//		std::map<core::CallExprAddress, OptContextFreeCallableList> contextFreeCallableCandidateCache;
//
//		const ContextFreeCallableList& getContextFreeCallableCandidate(const core::CallExprAddress& call);
//
//		// ----------------------- some debugging utilities ---------------------------
//
//		void plot(std::ostream& out = std::cout) const;
//		std::size_t getNumSets() const { return sets.size() + stateSets.size(); }
//		std::size_t getNumConstraints() const { return solver.getConstraints().size(); }

	};

	template<typename Context>
	bool isValidContext(CBA& cba, const Context& context) {
		return cba.isValid(context);
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

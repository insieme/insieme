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

#include <array>
#include <tuple>
#include <map>
#include <type_traits>

#include <boost/optional/optional.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_constraint/solver2.h"

#include <boost/optional.hpp>
#include "insieme/core/arithmetic/arithmetic.h"

namespace insieme {
namespace analysis {
namespace cba {
namespace prototype {

	using std::array;
	using std::tuple;
	using std::map;

	// forward declarations
	typedef int Label;										// the type used to label code locations
	typedef int Variable;									// the type used to identify variables

	class CBA;												// the main analysis entity

	class ConstraintResolver;								// a base class for resolving constraints, managed by CBA instances
	typedef ConstraintResolver* ConstraintResolverPtr;		// a pointer type for resolver instances - plain, since managed by CBA instances


	// ---------- context information ----------------


	template<typename T, unsigned s>
	struct Sequence : public utils::Printable {
		enum { size = s, empty = (s==0) };
		array<T, s> context;
		Sequence() : context() {}
		Sequence(const Sequence<T,s>& other) : context(other.context) {}
		Sequence(const array<T,s>& context) : context(context) {}
		template<typename ... E>
		Sequence(const E& ... e) : context((array<T,s>){{e...}}) {}
		bool operator==(const Sequence& other) const { return empty || this == &other || context == other.context; }
		bool operator!=(const Sequence& other) const { return !(*this == other); }
		bool operator<(const Sequence& other) const { return !empty && this != &other && context < other.context; }
		const T& operator[](std::size_t index) const { return context[index]; }
		bool startsWith(const T& e) const { return s == 0 || context.front() == e; }
		bool endsWith(const T& e) const { return s == 0 || context.back() == e; }
		const T& front() const { assert(s > 0u); return context.front(); }
		const T& back() const { assert(s > 0u); return context.back(); }
		Sequence<T,s>& operator<<=(const Label& label) {
			if (empty) return *this;
			for(unsigned i=0; i<(s-1); ++i) {
				context[i] = context[i+1];
			}
			context[s-1] = label;
			return *this;
		}
		Sequence<T,s>& operator>>=(const Label& label) {
			if (empty) return *this;
			for(int i=(s-1); i>=0; --i) {
				context[i] = context[i-1];
			}
			context[0] = label;
			return *this;
		}
		Sequence<T,s> operator<<(const Label& label) const { return Sequence(*this)<<=label; }
		Sequence<T,s> operator>>(const Label& label) const { return Sequence(*this)>>=label; }
		std::ostream& printTo(std::ostream& out) const { return out << context; };
	};

	template<unsigned s>
	struct ThreadID : public utils::Printable {
		Label spawn;
		Sequence<Label, s> spawnContext;
		int id;
		ThreadID() : spawn(0), spawnContext(), id(0) {}
		bool operator==(const ThreadID& other) const { return spawn == other.spawn && id == other.id && spawnContext == other.spawnContext; }
		bool operator<(const ThreadID& other) const {
			if (spawn != other.spawn) return spawn < other.spawn;
			if (id != other.id) return id < other.id;
			return spawnContext < other.spawnContext;
		}
		std::ostream& printTo(std::ostream& out) const { return out << "<" << spawn << "," << spawnContext << "," << id << ">"; };
	};

	/**
	 * The context class ...
	 * TODO: document
	 * TODO: make generic
	 */
//	template<
//		unsigned call_context_size = 2,
//		unsigned thread_context_length = 2,
//		unsigned thread_spawn_context_length = 0
//	>
	struct Context : public utils::Printable {

//		typedef Sequence<Label, call_context_size> CallContext;
//
//		typedef ThreadID<thread_spawn_context_length> ThreadID;
//		typedef Sequence<ThreadID, thread_context_length> ThreadContext;

		typedef Sequence<Label, 2> CallContext;
		typedef Sequence<ThreadID<0>, 2> ThreadContext;

		CallContext callContext;
		ThreadContext threadContext;

		Context()
			: callContext(), threadContext() {}

		Context(const CallContext& callContext, const ThreadContext& threadContext = ThreadContext())
			: callContext(callContext), threadContext(threadContext) {}

		Context(const Context& other)
			: callContext(other.callContext), threadContext(other.threadContext) {}

		bool operator==(const Context& other) const {
			return this == &other ||
					(callContext == other.callContext && threadContext == other.threadContext);
		}
		bool operator!=(const Context& other) const {
			return !(*this == other);
		}
		bool operator<(const Context& other) const {
			if (*this == other) return false;
			if (callContext != other.callContext) return callContext < other.callContext;
			if (threadContext != other.threadContext) return threadContext < other.threadContext;
			assert(false && "How did you get here?");
			return false;
		}
		std::ostream& printTo(std::ostream& out) const {
			return out << "[" << callContext << "," << threadContext << "]";
		};
	};

//	typedef Context<0,0,0> NoContext;
//	typedef Context<2,2,0> DefaultContext;
//	typedef Context<2,2,0> Context;

	// ----------- set types ------------------

	/**
	 * An abstract base class for set types to be handled by the analysis.
	 * Instances are expected to be immutable global constants.
	 */
	class SetType : public boost::noncopyable {

	public:

		// the type of the internally stored resolver factory
		typedef std::function<ConstraintResolverPtr(CBA&)> resolver_factory;

	private:

		/**
		 * The name of this set for printing and debugging issues.
		 */
		const string name;

		/**
		 * The factory utilized for obtaining resolver instances for the represented set type.
		 */
		const resolver_factory factory;

	protected:

		SetType(const string& name, const resolver_factory& factory) : name(name), factory(factory) {}

	public:

		const string& getName() const {
			return name;
		}

		bool operator==(const SetType& other) const {
			// the identity of a set type is fixed by its address
			return this == &other;
		}

		bool operator!=(const SetType& other) const {
			return !(*this == other);
		}

		/**
		 * Requests this set type to install a constraint resolver instance
		 * within the given CBA instance.
		 */
		ConstraintResolverPtr getConstraintResolver(CBA& analysis) const {
			return factory(analysis);
		}

	};

	/**
	 * A special type of set type fixing the element type of the represented type.
	 */
	template<typename E>
	struct TypedSetType : public SetType {
		TypedSetType(const string& name, const resolver_factory& factory) : SetType(name, factory) {}
	};

	// all set types are global constants => plain pointers can be used safely
	typedef const SetType* SetTypePtr;

	// a type for set of types
	typedef std::set<SetTypePtr> SetTypeSet;


	// ------------------- reachable code ------------------

	struct Reachable : public utils::Printable {
		bool operator<(const Reachable& other) const { return false; }
		bool operator==(const Reachable& other) const { return true; }
		std::ostream& printTo(std::ostream& out) const { return out << "reachable"; };
	};

	extern const TypedSetType<Reachable> Rin;		// the associated term is reached
	extern const TypedSetType<Reachable> Rout;		// the associated term is left


	// ----------------- inter-procedural control flow ------------------

	// the type used to represent functions / closures
	struct Callable : public utils::Printable {
		core::ExpressionAddress definition;
		Context context;

		Callable(const core::LiteralAddress& lit)
			: definition(lit.getAddressedNode()), context() {}
		Callable(const core::LambdaExprAddress& fun)
			: definition(fun), context() {}
		Callable(const core::BindExprAddress& bind, const Context& context)
			: definition(bind), context(context) {}
		Callable(const core::ExpressionAddress& expr, const Context& context = Context())
			: definition(expr), context(context) { assert(isBind() || isLambda()); }
		Callable(const Callable& other)
			: definition(other.definition), context(other.context) {}

		bool operator<(const Callable& other) const {
			if (definition != other.definition) return definition < other.definition;
			if (context != other.context) return context < other.context;
			return false;
		}
		bool operator==(const Callable& other) const {
			if (this == &other) return true;
			return definition == other.definition && context == other.context;
		}

		bool operator!=(const Callable& other) const { return !(*this == other); }

		bool isBind() const { return definition->getNodeType() == core::NT_BindExpr; }
		bool isLambda() const { return definition->getNodeType() == core::NT_LambdaExpr; };

		std::size_t getNumParams() const { return definition->getType().as<core::FunctionTypePtr>()->getParameterTypes().size(); }
		core::StatementAddress getBody() const {
			assert(isBind() || isLambda());
			return (isBind())
					? definition.as<core::BindExprAddress>()->getCall().as<core::StatementAddress>()
					: definition.as<core::LambdaExprAddress>()->getBody().as<core::StatementAddress>();
		}

		std::ostream& printTo(std::ostream& out) const {
			if (auto lit = definition.isa<core::LiteralPtr>()) return out << *lit;
			return out << "(" << definition->getNodeType() << "@" << definition << "," << context << ")";
		}
	};

	extern const TypedSetType<Callable> C;
	extern const TypedSetType<Callable> c;

	// a light version only tracking functions, no context
	typedef core::ExpressionAddress ContextFreeCallable;
	extern const TypedSetType<ContextFreeCallable> F;
	extern const TypedSetType<ContextFreeCallable> f;


	// ----------------- references ---------------

	typedef std::tuple<core::ExpressionAddress, Context> Location;
	extern const TypedSetType<Location> R;
	extern const TypedSetType<Location> r;


	// ----------------- simple constants ---------------

	extern const TypedSetType<core::ExpressionPtr> D;
	extern const TypedSetType<core::ExpressionPtr> d;


	// ----------------- arithmetic analysis ---------------

	struct Formula : public utils::Printable {
		typedef boost::optional<core::arithmetic::Formula> formula_type;
		formula_type formula;

		Formula() : formula() {};
		Formula(const core::arithmetic::Formula& formula) : formula(formula) {};

		bool operator<(const Formula& other) const {
			return (!formula && other.formula) || (other.formula && formula->lessThan(*other.formula));
		}

		operator bool() const {
			return formula;
		}

		std::ostream& printTo(std::ostream& out) const {
			if (formula) return out << *formula;
			return out << "-unknown-";
		}
	};

	extern const TypedSetType<Formula> A;
	extern const TypedSetType<Formula> a;


	// ----------------- booleans analysis ---------------

	extern const TypedSetType<bool> B;
	extern const TypedSetType<bool> b;



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


	// -------------------- Context Constraints ------------

	extern const TypedSetType<Label> pred;		// the set of pre-decessors in call contexts


	// -------------------- Constraint Resolver ---------------------------

	// the type used for lists of constraints
	typedef utils::set_constraint_2::Constraints Constraints;

	/**
	 * A base class for classes capable of lazily resolving set constraints while processing constraint
	 * based analysis.
	 *
	 * Essentially, a constraint resolver is an IR visitor generating for a given node (addressed by its
	 * address) and a call / thread context constraints. In general, every resolver is only supposed to
	 * generate in-constraints for the requested target set specified by the address and context parameter.
	 */
	class ConstraintResolver : public core::IRVisitor<void, core::Address, const Context&, Constraints&> {

		// a short-cut for the base class
		typedef core::IRVisitor<void, core::Address, const Context&, Constraints&> super;

		/**
		 * The base-implementation is preventing the same arguments to be processed multiple times.
		 */
		typedef std::tuple<core::NodeAddress, Context> Item;
		std::set<Item> processed;

	protected:

		/**
		 * The analysis context this resolver is working for. Every instance may only be utilized by
		 * a single CBA instance.
		 */
		CBA& context;

	public:

		ConstraintResolver(CBA& context)
			: processed(), context(context) {}

		/**
		 * The main entry function for resolving constraints for the given node and context. Constraints
		 * will be added to the given list.
		 *
		 * @param node the node for which constraints shell be generated - details regarding the set covered
		 * 			by this resolver are implementation specific.
		 * @param ctxt the call context to be considered for the constraint generation
		 */
		void addConstraints(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints) {
			// just forward call to visit-process
			visit(node, ctxt, constraints);
		}

		/**
		 * Overrides the standard visit function of the super type and realizes the guard avoiding the
		 * repeated evaluation of identical argument types.
		 */
		virtual void visit(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints);

	protected:

		/**
		 * Provides access to the context
		 */
		CBA& getContext() {
			return context;
		}

	};

	typedef ConstraintResolver* ConstraintResolverPtr;


	// allows to check whether a given statement is a memory location constructor (including globals)
	bool isMemoryConstructor(const core::StatementAddress& stmt);

	core::VariableAddress getDefinitionPoint(const core::VariableAddress& varAddress);

	core::ExpressionAddress getLocationDefinitionPoint(const core::StatementAddress& stmt);

	typedef utils::set_constraint_2::Assignment Solution;


	class CBA : public boost::noncopyable {

		typedef utils::set_constraint_2::SetID SetID;
		typedef utils::set_constraint_2::LazySolver Solver;

		typedef tuple<SetTypePtr, int, Context> SetKey;
		typedef tuple<StateSetTypePtr, SetTypePtr, Location> LocationSetKey;
		typedef tuple<LocationSetKey, Label, Context> StateSetKey;

		core::StatementAddress root;

		Solver solver;

		// a data structure managing constraint resolvers
		std::set<ConstraintResolverPtr> resolver;
		std::map<SetTypePtr, ConstraintResolverPtr> setResolver;
		std::map<LocationSetKey, ConstraintResolverPtr> locationResolver;

		// a list of all dynamic calls within the targeted fragment - filled by the constructor
		std::vector<core::CallExprAddress> dynamicCalls;
		std::vector<Label> dynamicCallLabels;

		std::vector<Location> locations;
		std::vector<Callable> callables;
		std::vector<ContextFreeCallable> freeFunctions;

		int setCounter;
		std::map<SetKey, SetID> sets;
		std::map<StateSetKey, SetID> stateSets;

		// two caches for resolving labels and variables
		int idCounter;
		std::unordered_map<core::StatementAddress, Label> labels;
		std::unordered_map<core::VariableAddress, Variable> vars;

		// a reverse lookup structure for labels
		std::unordered_map<Label, core::StatementAddress> reverseLabels;
		std::unordered_map<Variable, core::VariableAddress> reverseVars;


		// reverse maps for sets (for resolution)
		std::unordered_map<SetID, SetKey> set2key;
		std::unordered_map<SetID, StateSetKey> set2statekey;

	public:

		CBA(const core::StatementAddress& root);

		~CBA() {
			for(auto cur : resolver) delete cur;
		}

		const core::StatementAddress& getRoot() const {
			return root;
		}

		template<typename T>
		const std::set<T>& getValuesOf(const core::ExpressionAddress& expr, const TypedSetType<T>& set, const Context& ctxt = Context()) {
			auto id = getSet(set, getLabel(expr), ctxt);
			return solver.solve(id)[id];
		}

		template<typename T>
		const std::set<T>& getValuesOf(const core::VariableAddress& var, const TypedSetType<T>& set, const Context& ctxt = Context()) {
			auto id = getSet(set, getVariable(var), ctxt);
			return solver.solve(id)[id];
		}

		const std::set<core::ExpressionPtr>& getValuesOf(const core::ExpressionAddress& expr, const Context& ctxt = Context()) {
			return getValuesOf(expr, D, ctxt);
		}

		const std::set<core::ExpressionPtr>& getValuesOf(const core::VariableAddress& var, const Context& ctxt = Context()) {
			return getValuesOf(var, d, ctxt);
		}

		ConstraintResolverPtr getResolver(const SetType& set) {
			auto& res = setResolver[&set];
			return (res) ? res : set.getConstraintResolver(*this);
		}

		template<typename R>
		typename std::enable_if<std::is_base_of<ConstraintResolver, R>::value, ConstraintResolver*>::type
		getResolver(const SetType& set) {
			auto& res = setResolver[&set];
			if (!res) {
				res = new R(*this);
				resolver.insert(res);
			}
			return res;
		}

		template<typename R, typename T, typename ... Args>
		typename std::enable_if<std::is_base_of<ConstraintResolver, R>::value, void>::type
		registerLocationResolver(const StateSetType& stateSetType, const TypedSetType<T>& dataType, const Location& location, const Args& ... args) {
			auto key = std::make_tuple(&stateSetType,  (const SetType*)(&dataType), location);

			// if there is already a resolver installed => skip new one
			auto& cur = locationResolver[key];
			if (cur) return;			

			// create new resolver and register it
			cur = new R(*this, dataType, location, args ...);
			resolver.insert(cur);
		}

		const std::set<ConstraintResolverPtr>& getAllResolver() const {
			return resolver;
		}

		void addConstraintsFor(const SetID& set, Constraints& res);

		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const Context& context = Context()) {
			return utils::set_constraint_2::TypedSetID<T>(getSet(static_cast<const SetType&>(type), id, context));
		}

		utils::set_constraint_2::SetID getSet(const SetType& type, int id, const Context& context = Context()) {
			SetKey key(&type, id, context);
			auto pos = sets.find(key);
			if (pos != sets.end()) {
				return pos->second;
			}
			utils::set_constraint_2::SetID newSet(++setCounter);		// reserve 0
			sets[key] = newSet;
			set2key[newSet] = key;
			return newSet;
		}

		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const Context& context = Context()) const {
			return utils::set_constraint_2::TypedSetID<T>(getSet(static_cast<const SetType&>(type), id, context));
		}

		utils::set_constraint_2::SetID getSet(const SetType& type, int id, const Context& context = Context()) const {
			static const utils::set_constraint_2::SetID empty(0);

			SetKey key(&type, id, context);
			auto pos = sets.find(key);
			if (pos != sets.end()) {
				return pos->second;
			}

			// use empty set as the default
			return empty;
		}


		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(const StateSetType& type, Label label, const Context& context, const Location& loc, const TypedSetType<T>& type_loc) {
			StateSetKey key(LocationSetKey(&type, &type_loc, loc), label, context);
			auto pos = stateSets.find(key);
			if (pos != stateSets.end()) {
				return pos->second;
			}
			utils::set_constraint_2::TypedSetID<T> newSet(++setCounter);		// reserve 0
			stateSets[key] = newSet;
			set2statekey[newSet] = key;
			return newSet;
		}

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

		// TODO: remove default values
		Location getLocation(const core::ExpressionAddress& ctor) { // TODO: add support: , const CallContext& c = CallContext(), const ThreadContext& t = ThreadContext()) {
			Context context;

			assert(isMemoryConstructor(ctor));

			// obtain address of definition point
			auto def = getLocationDefinitionPoint(ctor);

			// for globals the call context and thread context is not relevant
			if (ctor.isa<core::LiteralPtr>()) {
				return Location(def, Context());
			}

			// create the location instance
			return Location(def, context);
		}

		// only for debugging
		core::ExpressionAddress getMemoryConstructor(const Location& loc) {
			return std::get<0>(loc);
		}

		const std::vector<Location>& getLocations() const {
			return locations;
		}

		const std::vector<ContextFreeCallable>& getFreeFunctions() const {
			return freeFunctions;
		}

		const std::vector<core::CallExprAddress>& getDynamicCalls() const {
			return dynamicCalls;
		}

		const std::vector<Label>& getDynamicCallLabels() const {
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

		bool isValid(const Context& ctxt) {
			// TODO: materialize all contexts?
			if (Context::CallContext::size < 2) return true;

			// check sequence
			const auto& seq = ctxt.callContext;
			for(int i=0; i<Context::CallContext::size-1; i++) {
				const auto& list = getAllStaticPredecessors(seq[i+1]);
				if (list && !contains(*list, seq[i])) return false;
			}
			return true;
		}

		typedef std::vector<Callable> CallableList;
		typedef boost::optional<CallableList> OptCallableList;
		std::map<core::CallExprAddress, OptCallableList> callableCandidateCache;

		/**
		 * Obtains a full list of potential callables.
		 */
		const CallableList& getCallables() const {
			return callables;
		}

		/**
		 * Obtains a list of candidate callables to be called by the given call expression.
		 */
		const CallableList& getCallableCandidates(const core::CallExprAddress& call);

		const CallableList& getCallableCandidates(const Label& callSite) {
			return getCallableCandidates(getStmt(callSite).as<core::CallExprAddress>());
		}

		typedef std::vector<ContextFreeCallable> ContextFreeCallableList;
		typedef boost::optional<ContextFreeCallableList> OptContextFreeCallableList;
		std::map<core::CallExprAddress, OptContextFreeCallableList> contextFreeCallableCandidateCache;

		const ContextFreeCallableList& getContextFreeCallableCandidate(const core::CallExprAddress& call);

		// ----------------------- some debugging utilities ---------------------------

		void plot(std::ostream& out = std::cout) const;
		std::size_t getNumSets() const { return sets.size() + stateSets.size(); }
		std::size_t getNumConstraints() const { return solver.getConstraints().size(); }

	};

	inline void ConstraintResolver::visit(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints) {
		// do not resolve the same nodes multiple times
		if (!processed.insert(Item(node,ctxt)).second) return;

		// filter out invalid contexts
		if (!context.isValid(ctxt)) return;

		// for valid content => std procedure
		super::visit(node, ctxt, constraints);
	}

} // end namespace prototype
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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

	using std::array;
	using std::tuple;
	using std::map;

	// forward declarations
	typedef int Label;		// the type used to label code locations
	typedef int Variable;	// the type used to identify variables


	// ---------- context information ----------------


	template<typename T, unsigned s>
	struct Sequence : public utils::Printable {
		enum { size = s };
		array<T, s> context;		// TODO: make length a generic parameter
		Sequence() : context() {}
		Sequence(const Sequence<T,s>& other) : context(other.context) {}
		Sequence(const array<T,s>& context) : context(context) {}
		template<typename ... E>
		Sequence(const E& ... e) : context((array<T,s>){{e...}}) {}
		bool operator==(const Sequence& other) const { return this == &other || context == other.context; }
		bool operator!=(const Sequence& other) const { return !(*this == other); }
		bool operator<(const Sequence& other) const { return this != &other && context < other.context; }
		bool startsWith(const T& e) const { return s == 0 || context[0] == e; }
		Sequence<T,s>& operator<<=(const Label& label) {
			for(unsigned i=0; i<(s-1); ++i) {
				context[i] = context[i+1];
			}
			context[s-1] = label;
			return *this;
		}
		Sequence<T,s>& operator>>=(const Label& label) {
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

	struct ThreadID : public utils::Printable {
		Label spawn;
		int id;
		ThreadID() : spawn(0), id(0) {}
		bool operator==(const ThreadID& other) const { return spawn == other.spawn && id == other.id; }
		bool operator<(const ThreadID& other) const { return spawn < other.spawn || (spawn == other.spawn && id < other.id); }
		std::ostream& printTo(std::ostream& out) const { return out << "<" << spawn << "," << id << ">"; };
	};

	/**
	 * The context class ...
	 * TODO: document
	 * TODO: make generic
	 */
	struct Context : public utils::Printable {

		typedef Sequence<Label, 2> CallContext;
		typedef Sequence<ThreadID, 2> ThreadContext;

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

	// ----------- set types ------------------

	class SetType : public boost::noncopyable {
		string name;
	protected:
		SetType(const string& name) : name(name) {}
	public:
		const string& getName() const { return name; }
		bool operator==(const SetType& other) const { return this == &other; }
	};

	// TODO: restrict creation of this kind of objectes somehow ...
	template<typename E>
	class TypedSetType : public SetType {
	public:
		TypedSetType(const string& name) : SetType(name) {}
	};

	// all set types are global constants => pointers can be used plain
	typedef const SetType* SetTypePtr;

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
	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (auto lit = definition.isa<core::LiteralPtr>()) return out << *lit;
			return out << "(" << definition->getNodeType() << "@" << definition << "," << context << ")";
		}
	};

	extern const TypedSetType<Callable> C;
	extern const TypedSetType<Callable> c;


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

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (formula) return out << *formula;
			return out << "-unknown-";
		}
	};

	extern const TypedSetType<Formula> A;
	extern const TypedSetType<Formula> a;


	// ----------------- booleans analysis ---------------

	extern const TypedSetType<bool> B;
	extern const TypedSetType<bool> b;



	// ----------------- state set analysis ---------------

	// the set-type tokens to be utilized for the state sets
	struct StateSetType : public TypedSetType<void> {
		StateSetType(const string& name) : TypedSetType<void>(name) {}
	};

	extern const StateSetType Sin;		// in-state of statements
	extern const StateSetType Sout;		// out-state of statements
	extern const StateSetType Stmp;		// temporary states of statements (assignment only)


	core::VariableAddress getDefinitionPoint(const core::VariableAddress& varAddress);




	// -------------------- Constraint Resolver ---------------------------

	typedef utils::set_constraint_2::Constraints Constraints;

	class CBA;
	class ConstraintResolver;
	class StateConstraintResolver;

	class ConstraintResolver : public core::IRVisitor<void, core::Address, const Context&, Constraints&> {

		typedef core::IRVisitor<void, core::Address, const Context&, Constraints&> super;

		// a cache recording resolved items
		typedef std::tuple<core::NodeAddress, Context> Item;
		std::set<Item> processed;

		const SetTypeSet coveredSets;

	protected:

		CBA& context;

	public:

		ConstraintResolver(CBA& context, const SetTypeSet& coveredSets)
			: processed(), coveredSets(coveredSets), context(context) {}

		void addConstraints(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints) {
			// just forward call to visit-process
			visit(node, ctxt, constraints);
		}

		virtual void visit(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints) {
			if (!processed.insert(Item(node,ctxt)).second) return;
			super::visit(node, ctxt, constraints);
		}

		const SetTypeSet& getCoveredSets() const {
			return coveredSets;
		}

	};

	typedef ConstraintResolver* ConstraintResolverPtr;


	// allows to check whether a given statement is a memory location constructor (including globals)
	bool isMemoryConstructor(const core::StatementAddress& stmt);

	core::ExpressionAddress getLocationDefinitionPoint(const core::StatementAddress& stmt);

	typedef utils::set_constraint_2::Assignment Solution;


	class CBA : public boost::noncopyable {

		typedef utils::set_constraint_2::SetID SetID;
		typedef utils::set_constraint_2::LazySolver Solver;

		typedef tuple<const SetType*, int, Context> SetKey;
		typedef tuple<const StateSetType*, Label, Context, Location, const SetType*> StateSetKey;

		Solver solver;

		// a list of all dynamic calls within the targeted fragment - filled by the constructor
		std::vector<core::CallExprAddress> dynamicCalls;
		std::vector<Label> dynamicCallLabels;

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

		// a data structure managing constraint resolvers
		std::set<ConstraintResolverPtr> resolver;
		std::map<const SetType*, ConstraintResolverPtr> setResolver;
		std::map<std::tuple<const StateSetType*, const SetType*, Location>, ConstraintResolverPtr> locationResolver;

		// reverse maps for sets (for resolution)
		std::unordered_map<SetID, SetKey> set2key;
		std::unordered_map<SetID, StateSetKey> set2statekey;

	public:

		CBA(const core::StatementAddress& root);

		~CBA() {
			for(auto cur : resolver) delete cur;
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

		template<typename R, typename ... Args>
		typename std::enable_if<std::is_base_of<ConstraintResolver, R>::value, void>::type
		registerResolver(const Args& ... args) {
			R* r = new R(*this, args ...);
			resolver.insert(r);
			for(auto type : r->getCoveredSets()) {
				assert(!setResolver[type] && "Must not bind two resolvers for the same set type!");
				setResolver[type] = r;
			}
		}

		template<typename R, typename T, typename ... Args>
		typename std::enable_if<std::is_base_of<ConstraintResolver, R>::value, void>::type
		registerLocationResolver(const TypedSetType<T>& dataType, const Location& location, const Args& ... args) {
			// one resolver for in and out set
			R* r = new R(*this, dataType, location, args ...);
			resolver.insert(r);

			for(auto type : r->getCoveredSets()) {
				locationResolver[std::make_tuple((const StateSetType*)(type),  (const SetType*)(&dataType), location)] = r;
			}
		}

		const std::set<ConstraintResolverPtr>& getAllResolver() const {
			return resolver;
		}

		void addConstraintsFor(const SetID& set, Constraints& res);

		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const Context& context) {
			return utils::set_constraint_2::TypedSetID<T>(getSet(static_cast<const SetType&>(type), id, context));
		}

		utils::set_constraint_2::SetID getSet(const SetType& type, int id, const Context& context) {
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
		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const Context& context) const {
			return utils::set_constraint_2::TypedSetID<T>(getSet(static_cast<const SetType&>(type), id, context));
		}

		utils::set_constraint_2::SetID getSet(const SetType& type, int id, const Context& context) const {
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
			StateSetKey key(&type, label, context, loc, &type_loc);
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

		void plot(std::ostream& out = std::cout) const;
		std::size_t getNumSets() const { return sets.size() + stateSets.size(); }

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

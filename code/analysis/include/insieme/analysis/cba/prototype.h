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

#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

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
	typedef int Value;		// the type used for values within the CBA

	typedef int Set;		// the type used to represent sets


	// ---------- context information ----------------

	template<typename T, unsigned s>
	struct Context : public utils::Printable {
		array<T, s> context;		// TODO: make length a generic parameter
		Context() : context() {}
		Context(const Context<T,s>& other) : context(other.context) {}
		template<typename ... E>
		Context(const E& ... e) : context((array<T,s>){{e...}}) {}
		bool operator==(const Context& other) const { return context == other.context; }
		bool operator<(const Context& other) const { return context < other.context; }
		Context<T,s>& operator<<=(const Label& label) {
			for(unsigned i=0; i<(s-1); ++i) {
				context[i] = context[i+1];
				context[s-1] = label;
			}
			return *this;
		}
		Context<T,s> operator<<(const Label& label) const { Context cpy(*this); return cpy <<= label; }
		std::ostream& printTo(std::ostream& out) const { return out << context; };
	};

	typedef Context<Label,2> CallContext;

	struct ThreadID : public utils::Printable {
		Label spawn;
		int id;
		ThreadID() : spawn(0), id(0) {}
		bool operator==(const ThreadID& other) const { return spawn == other.spawn && id == other.id; }
		bool operator<(const ThreadID& other) const { return spawn < other.spawn || (spawn == other.spawn && id < other.id); }
		std::ostream& printTo(std::ostream& out) const { return out << "<" << spawn << "," << id << ">"; };
	};

	typedef Context<ThreadID,2> ThreadContext;

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

	extern const TypedSetType<core::ExpressionAddress> C;
	extern const TypedSetType<core::ExpressionAddress> c;

	extern const TypedSetType<core::ExpressionPtr> D;
	extern const TypedSetType<core::ExpressionPtr> d;

	typedef std::tuple<core::ExpressionAddress, CallContext, ThreadContext> Location;
	extern const TypedSetType<Location> R;
	extern const TypedSetType<Location> r;


	// ------------------- reachable code ------------------

	struct Reachable : public utils::Printable {
		bool operator<(const Reachable& other) const { return false; }
		bool operator==(const Reachable& other) const { return true; }
		std::ostream& printTo(std::ostream& out) const { return out << "reachable"; };
	};

	extern const TypedSetType<Reachable> Rin;		// the associated term is reached
	extern const TypedSetType<Reachable> Rout;		// the associated term is left

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

	enum StateSetType {
		Sin, Stmp, Sout		// state sets before, within (only for assignments) and after statements
	};


	core::VariableAddress getDefinitionPoint(const core::VariableAddress& varAddress);


	// - references ------------


	// allows to check whether a given statement is a memory location constructor (including globals)
	bool isMemoryConstructor(const core::StatementAddress& stmt);

	core::ExpressionAddress getLocationDefinitionPoint(const core::StatementAddress& stmt);

	typedef utils::set_constraint_2::Constraints Constraints;

	typedef utils::set_constraint_2::Assignment Solution;


	class CBAContext {

		typedef utils::set_constraint_2::SetID SetID;

		typedef tuple<const SetType*, int, CallContext, ThreadContext> SetKey;
		typedef tuple<StateSetType, Label, CallContext, ThreadContext, Location, const SetType*, CallContext, ThreadContext> StateSetKey;

		int setCounter;
		std::map<SetKey, SetID> sets;
		std::map<StateSetKey, SetID> stateSets;

		// two caches for resolving labels and variables
		int idCounter;
		std::map<core::StatementAddress, Label> labels;
		std::map<core::VariableAddress, Variable> vars;

	public:

		CBAContext() : setCounter(0), idCounter(0) {};

		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const CallContext& c, const ThreadContext& t) {
			SetKey key(&type, id, c, t);
			auto pos = sets.find(key);
			if (pos != sets.end()) {
				return pos->second;
			}
			utils::set_constraint_2::TypedSetID<T> newSet(++setCounter);		// reserve 0
			sets[key] = newSet;
			return newSet;
		}

		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(const TypedSetType<T>& type, int id, const CallContext& c, const ThreadContext& t) const {
			static const utils::set_constraint_2::TypedSetID<T> empty(0);

			SetKey key(&type, id, c, t);
			auto pos = sets.find(key);
			if (pos != sets.end()) {
				return pos->second;
			}

			// use empty set as the default
			return empty;
		}


		template<typename T>
		utils::set_constraint_2::TypedSetID<T> getSet(StateSetType type, Label label, const CallContext& c, const ThreadContext& t, Location loc, const TypedSetType<T>& type_loc) { // TODO: add support: , const CallContext& c_loc = CallContext(), const ThreadContext& t_loc = ThreadContext()) {
//			StateSetKey key(type, label, c, t, loc, &type_loc, c_loc, t_loc);
			StateSetKey key(type, label, c, t, loc, &type_loc, CallContext(), ThreadContext());
			auto pos = stateSets.find(key);
			if (pos != stateSets.end()) {
				return pos->second;
			}
			utils::set_constraint_2::TypedSetID<T> newSet(++setCounter);		// reserve 0
			stateSets[key] = newSet;
			return newSet;
		}

		Label getLabel(const core::StatementAddress& expr) {
			auto pos = labels.find(expr);
			if (pos != labels.end()) {
				return pos->second;
			}
			Label l = ++idCounter;		// reserve 0 for the empty set
			labels[expr] = l;
			return l;
		}

		Label getLabel(const core::StatementAddress& expr) const {
			auto pos = labels.find(expr);
			return (pos != labels.end()) ? pos->second : 0;
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


		// TODO: remove default values
		Location getLocation(const core::ExpressionAddress& ctor) { // TODO: add support: , const CallContext& c = CallContext(), const ThreadContext& t = ThreadContext()) {
			CallContext c; ThreadContext t;

			assert(isMemoryConstructor(ctor));

			// obtain address of definition point
			auto def = getLocationDefinitionPoint(ctor);

			// for globals the call context and thread context is not relevant
			if (ctor.isa<core::LiteralPtr>()) {
				return Location(def, CallContext(), ThreadContext());
			}

			// create the location instance
			return Location(def, c, t);
		}

		// only for debugging
		core::ExpressionAddress getMemoryConstructor(const Location& loc) {
			return std::get<0>(loc);
		}

		void plot(const Constraints& constraints, std::ostream& out = std::cout) const;
		void plot(const Constraints& constraints, const Solution& ass, std::ostream& out = std::cout) const;

	};


	Constraints generateConstraints(CBAContext& context, const core::StatementPtr& root);

	Solution solve(const Constraints& constraints);

	template<typename T>
	const std::set<T>& getValuesOf(const CBAContext& context, const Solution& solution,
			const core::ExpressionAddress& expr, const TypedSetType<T>& set,
			const CallContext& callContext = CallContext(), const ThreadContext& threadContext = ThreadContext()) {

		auto label = context.getLabel(expr);
		return solution[context.getSet(set, label, callContext, threadContext)];
	}

	template<typename T>
	const std::set<T>& getValuesOf(const CBAContext& context, const Solution& solution,
			const core::VariableAddress& var, const TypedSetType<T>& set,
			const CallContext& callContext = CallContext(), const ThreadContext& threadContext = ThreadContext()) {

		auto id = context.getVariable(var);
		return solution[context.getSet(set, id, callContext, threadContext)];
	}

	const std::set<core::ExpressionPtr>& getValuesOf(const CBAContext& context, const Solution& solution, const core::ExpressionAddress& expr,
			const CallContext& callContext = CallContext(), const ThreadContext& threadContext = ThreadContext()) {
		return getValuesOf(context, solution, expr, D, callContext, threadContext);
	}

	const std::set<core::ExpressionPtr>& getValuesOf(const CBAContext& context, const Solution& solution, const core::VariableAddress& var,
			const CallContext& callContext = CallContext(), const ThreadContext& threadContext = ThreadContext()) {
		return getValuesOf(context, solution, var, d, callContext, threadContext);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

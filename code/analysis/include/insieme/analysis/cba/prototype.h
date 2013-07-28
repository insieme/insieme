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
#include "insieme/utils/set_constraint/solver.h"

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
	struct Context;			// the class to model calling contexts
	struct Thread;			// the class to model execution threads

	typedef int Set;		// the type used to represent sets

	struct Context : public utils::Printable {
		array<Label, 2> context;		// TODO: make length a generic parameter
		bool operator<(const Context& other) const { return context < other.context; }
		std::ostream& printTo(std::ostream& out) const { return out << context; };
	};

	struct Thread : public utils::Printable{
		struct Entry : public utils::Printable {
			Label spawn;
			int id;
			Entry() : spawn(0), id(0) {}
			bool operator<(const Entry& other) const { return spawn < other.spawn || (spawn == other.spawn && id < other.id); }
			std::ostream& printTo(std::ostream& out) const { return out << "<" << spawn << "," << id << ">"; };
		};

		array<Entry, 2> context;			// TODO: make length a generic parameter
		bool operator<(const Thread& other) const { return context < other.context; }
		std::ostream& printTo(std::ostream& out) const { return out << context; };
	};

	// TODO: make this expandable
	enum SetType {
		C, c,		// label / variable => function  (and value at the moment)
		D, d, 		// label / variable => value (not used yet, occupied by C and r)
		Sin, Sout,	// state before and after every expression
		R, r,		// label / variable => location pointing at
	};

	core::VariableAddress getDefinitionPoint(const core::VariableAddress& varAddress);


	// - references ------------

	typedef int Location;

	// a memory location is addressed by its creation point
//	typedef tuple<Label, Context, Thread> MemoryLocation;

	// allows to check whether a given statement is a memory location constructor (including globals)
	bool isMemoryConstructor(const core::StatementAddress& stmt);

	core::ExpressionAddress getLocationDefinitionPoint(const core::StatementAddress& stmt);

	class CBAContext {

		typedef tuple<SetType, int, Context, Thread> SetKey;

		int setCounter;
		std::map<SetKey, Set> sets;
		std::map<tuple<SetType, Label, Context, Thread, Location, Context, Thread>, Set> stateSets;

		// two caches for resolving labels and variables
		int varCounter;
		std::map<core::StatementAddress, Label> labels;
		std::map<core::VariableAddress, Variable> vars;

		// an index for expressions - in both directions
		std::map<core::ExpressionAddress, Value> e2i;				// TODO: think about using pointer ...
		std::unordered_map<Value, core::ExpressionAddress> i2e;

		// an index for locations
		std::map<tuple<Label, Context, Thread>,  Location> loc2i;
		std::map<Location, tuple<Label, Context, Thread>> i2loc;

	public:

		CBAContext() : setCounter(0), varCounter(0) {};

		Set getSet(SetType type, int id, const Context& c = Context(), const Thread& t = Thread()) {
			SetKey key(type, id, c, t);
			auto pos = sets.find(key);
			if (pos != sets.end()) {
				return pos->second;
			}
			Set newSet = ++setCounter;		// reserve 0
			sets[key] = newSet;
			return newSet;
		}

		Set getSet(SetType type, Label label, const Context& c, const Thread& t, Location loc, const Context& c_loc = Context(), const Thread& t_loc = Thread()) {
			auto key = std::make_tuple(type, label, c, t, loc, c_loc, t_loc);
			auto pos = stateSets.find(key);
			if (pos != stateSets.end()) {
				return pos->second;
			}
			Set newSet = ++setCounter;		// reserve 0
			stateSets[key] = newSet;
			return newSet;
		}

		Label getLabel(const core::StatementAddress& expr) {
			auto pos = labels.find(expr);
			if (pos != labels.end()) {
				return pos->second;
			}
			Label l = labels.size() + 1;		// reserve 0
			labels[expr] = l;
			return l;
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
				res = ++varCounter; 		// reserve 0
			} else {
				res = getVariable(def);
			}
			vars[var] = res;
			return res;
		}

		Variable getFreshVar() {
			return ++varCounter;
		}

		Value getValue(const core::ExpressionAddress& expr) {
			auto pos = e2i.find(expr);
			if (pos != e2i.end()) {
				return pos->second;
			}

			Value res = e2i.size();
			e2i[expr] = res;
			i2e[res] = expr;
			return res;
		}

		core::ExpressionPtr getExpr(const Value& value) {
			auto pos = i2e.find(value);
			assert(pos != i2e.end());
			return pos->second;
		}

		// TODO: remove default values
		Location getLocation(const core::ExpressionAddress& ctor, const Context& c = Context(), const Thread& t = Thread()) {
			assert(isMemoryConstructor(ctor));
			auto key = std::make_tuple(getLocationDefinitionPoint(ctor), c, t);

			if (ctor.isa<core::LiteralPtr>()) {
				// for globals the context and thread ID is not relevant
				key = std::make_tuple(std::get<0>(key), Context(), Thread());
			}

			auto pos = loc2i.find(key);
			if (pos != loc2i.end()) {
				return pos->second;
			}

			// create a new location index
			Location res = loc2i.size() + 1; 	// reserve 0
			loc2i[key] = res;
			i2loc[res] = key;
			return res;
		}

	};


	typedef utils::set_constraint::Constraints Constraints;

	Constraints generateConstraints(CBAContext& context, const core::StatementPtr& root);

	typedef utils::set_constraint::Assignment Solution;

	Solution solve(const Constraints& constraints);

	core::ExpressionSet getValuesOf(CBAContext& context, const Solution& solution, const core::ExpressionAddress& expr, SetType set = D);

	core::ExpressionSet getValuesOf(CBAContext& context, const Solution& solution, const core::VariableAddress& var, SetType set = d);

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
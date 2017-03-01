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
#pragma once

#include <set>

#include "insieme/utils/set_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace utils {
namespace constraint {

	// forward declarations
	template <typename T>
	struct default_meet_assign_op;
	template <typename meet_assign_op>
	struct meet_assign_based_meet_op;
	template <typename meet_assign_op>
	struct meet_assign_based_less_op;


	/**
	 * Defines a (meta-) type forming a lattice - a structure consisting
	 * of a set and an meet operator (and an optional less-operator which
	 * might be derived automatically from the meet operator or implemented
	 * explicitly for performance reasons).
	 *
	 * @tparam T the element type
	 * @tparam meet_op a functor implementing the meet operator
	 * @tparam less_op a functor implementing the less operator
	 */
	template <typename T, typename meet_assign_op = default_meet_assign_op<T>, typename less_op = meet_assign_based_less_op<meet_assign_op>,
	          typename meet_op = meet_assign_based_meet_op<meet_assign_op>>
	struct Lattice {
		typedef T value_type;
		typedef meet_assign_op meet_assign_op_type;
		typedef meet_op meet_op_type;
		typedef less_op less_op_type;
	};


	// forward declarations for set lattice operations
	template <typename E>
	struct set_union_meet_assign_op;
	template <typename E>
	struct set_union_less_op;

	template <typename E>
	struct set_intersect_meet_assign_op;
	template <typename E>
	struct set_intersect_less_op;

	template <typename E>
	struct SetUnionLattice : public Lattice<std::set<E>, set_union_meet_assign_op<E>, set_union_less_op<E>> {};

	// the special set type utilized for intersection lattices (supporting the representation of all elements)
	template <typename E>
	struct iset;

	template <typename E>
	struct SetIntersectLattice : public Lattice<iset<E>, set_intersect_meet_assign_op<E>, set_intersect_less_op<E>> {};

	/**
	 * A utility definition for set lattices defining the proper operations based
	 * on an element type. The partial order is defined by the sub-set relation.
	 */
	template <typename E>
	struct SetLattice : public SetUnionLattice<E> {};

	// ----------------------------------------------------------------
	// 						utility definition
	// ----------------------------------------------------------------

	template <typename T>
	struct default_meet_assign_op {
		default_meet_assign_op() {}
		bool operator()(T& trg, const T& src) const {
			T old = trg;
			trg += src;
			return old != trg;
		}
	};

	template <typename meet_assign_op>
	struct meet_assign_based_meet_op {
		meet_assign_based_meet_op() {}
		template <typename A, typename B>
		A operator()(const A& a, const B& b) const {
			static const meet_assign_op meet_assign;
			A r = a;
			meet_assign(r, b);
			return r;
		}
	};

	template <typename meet_assign_op>
	struct meet_assign_based_less_op {
		meet_assign_based_less_op() {}
		template <typename A, typename B>
		bool operator()(const A& a, const B& b) const {
			static const meet_assign_op meet_assign;
			if(a == b) { return true; }
			A c = a;
			meet_assign(c, b);
			return c == b;
		}
	};


	template <typename E>
	struct set_union_meet_assign_op {
		set_union_meet_assign_op() {}
		bool operator()(std::set<E>& trg, const E& element) const {
			return trg.insert(element).second;
		}
		bool operator()(std::set<E>& trg, const std::set<E>& src) const {
			// add values to target set
			bool newData = false;
			for(const auto& x : src) {
				newData = trg.insert(x).second || newData;
			}

			// if target set has changed
			return newData;
		}
	};

	template <typename E>
	struct set_union_less_op {
		set_union_less_op() {}
		bool operator()(const E& e, const std::set<E>& a) const {
			return a.find(e) != a.end();
		}
		bool operator()(const std::set<E>& a, const std::set<E>& b) const {
			return set::isSubset(a, b);
		}
	};


	template <typename E>
	struct iset : public std::set<E>, public utils::Printable {
		bool universal;
		iset(bool universal = true) : universal(universal) {}
		std::ostream& printTo(std::ostream& out) const {
			if(universal) { return out << "{-all-}"; }
			return out << static_cast<const std::set<E>&>(*this);
		}

		static iset<E> all() {
			return iset<E>(true);
		}
		static iset<E> empty() {
			return iset<E>(false);
		}
	};


	template <typename E>
	struct set_intersect_meet_assign_op {
		set_intersect_meet_assign_op() {}
		bool operator()(iset<E>& trg, const E& element) const {
			// deal with universe
			if(trg.universal) {
				trg.universal = false;
				assert_true(trg.empty()) << trg;
				trg.insert(element);
				return true;
			}

			// handle others
			if(trg.size() == 1u && *trg.begin() == element) { return false; }
			trg.clear();
			trg.insert(element);
			return true;
		}
		bool operator()(iset<E>& trg, const iset<E>& src) const {
			// handle universal sets
			if(trg.universal) {
				// if both are universal, there is nothing to do
				if(src.universal) { return false; }

				// otherwise the result will be the src
				trg = src;
				return true;
			}

			// compute the intersection
			bool changed = false;

			// this is strange code, but it is the simplest stack overflow could come up with
			// http://stackoverflow.com/questions/2874441/deleting-elements-from-stl-set-while-iterating
			for(auto it = trg.begin(); it != trg.end();) {
				if(src.find(*it) == src.end()) {
					trg.erase(it++);
					changed = true;
				} else {
					++it;
				}
			}

			return changed;
		}
	};

	template <typename E>
	struct set_intersect_less_op {
		set_intersect_less_op() {}
		bool operator()(const E& e, const iset<E>& a) const {
			return a.empty() || (a.size() == 1u && *a.begin() == e);
		}
		bool operator()(const iset<E>& a, const iset<E>& b) const {
			if(a.universal) { return true; }
			if(b.universal) { return false; }
			return set::isSubset(b, a); // note: reverse order!
		}
	};

} // end namespace constraint
} // end namespace utils
} // end namespace insieme

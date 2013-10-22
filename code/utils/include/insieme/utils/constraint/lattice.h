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

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace utils {
namespace constraint {

	// forward declarations
	template<typename T> struct default_meet_op;
	template<typename T, typename meet_op> struct meet_based_less_op;


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
	template<
		typename T,
		typename meet_op = default_meet_op<T>,
		typename less_op = meet_based_less_op<T,meet_op>
	>
	struct Lattice {
		typedef T value_type;
		typedef meet_op meet_op_type;
		typedef less_op less_op_type;
	};


	// forward declarations for set lattice
	template<typename E> struct set_meet_op;
	template<typename E> struct set_less_op;

	/**
	 * A utility definition for set lattices defining the proper operations based
	 * on an element type. The partial order is defined by the sub-set relation.
	 */
	template<typename E>
	struct SetLattice : public Lattice<std::set<E>, set_meet_op<E>, set_less_op<E>> {};


	// ----------------------------------------------------------------
	// 						utility definition
	// ----------------------------------------------------------------

	template<typename T>
	struct default_meet_op {
		bool operator()(T& trg, const T& src) const {
			T old = trg;
			trg += src;
			return old != trg;
		}
	};

	template<typename T, typename meet_op>
	struct meet_based_less_op {
		bool operator()(const T& a, const T& b) const {
			static const meet_op meet;
			if (a==b) return true;
			T c = a;
			meet(c,b);
			return c == b;
		}
	};


	template<typename E>
	struct set_meet_op {
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

	template<typename E>
	struct set_less_op {
		bool operator()(const std::set<E>& a, const std::set<E>& b) const {
			return set::isSubset(a, b);
		}
	};

} // end namespace constraint
} // end namespace utils
} // end namespace insieme

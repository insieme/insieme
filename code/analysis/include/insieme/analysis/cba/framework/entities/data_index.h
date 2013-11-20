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

#include <cstdint>

#include <boost/operators.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/arithmetic/arithmetic.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * A base class for indices to be utilized for analyzing operations
	 * on data structures within data values.
	 */
	template<typename DerivedIndex>
	struct Index :
			public boost::equality_comparable<DerivedIndex>,
			public boost::partially_ordered<DerivedIndex>,
			public utils::Printable,
			public utils::Hashable
	{};

	/**
	 * A base class for all indices actually modeling array indices.
	 */
	template<typename DerivedIndex>
	struct ArrayIndex : public Index<DerivedIndex> {};


	// -------------------------------------------------------------------------------------------------

	/**
	 * An index distinguishing different named fields.
	 */
	template<typename Key = string, typename less_op = std::less<Key>, typename equal_op = std::equal_to<Key>, typename hash_op = std::hash<Key>, typename print_op = print<id<Key>>>
	class NominalIndex : public Index<NominalIndex<Key, less_op, equal_op, hash_op, print_op>> {

		/**
		 * The name to be represented - as a reference to save the copy-overhead.
		 */
		Key name;

		std::size_t hashCode;

	public:

		NominalIndex(const Key& name = Key()) : name(name), hashCode(hash_op()(name)) {};

		NominalIndex(const NominalIndex<Key,less_op,equal_op,hash_op,print_op>& other) : name(other.name), hashCode(other.hashCode) {}

		const Key& getName() const {
			return name;
		}

		bool operator==(const NominalIndex<Key,less_op,equal_op,hash_op,print_op>& other) const {
			return this == &other || equal_op()(name, other.name);
		}

		bool operator<(const NominalIndex<Key,less_op,equal_op,hash_op,print_op>& other) const {
			return less_op()(name, other.name);
		}

		std::size_t hash() const {
			return hashCode;
		}

		std::ostream& printTo(std::ostream& out) const {
			return print_op()(out, name);
		}

	};

	/**
	 * The cross operator computing the ranges to be included when combining two indexed structures.
	 */
	template<typename K, typename L, typename E, typename H, typename P, typename I>
	std::set<NominalIndex<K,L,E,H,P>> cross(const std::map<NominalIndex<K,L,E,H,P>, I>& mapA, const std::map<NominalIndex<K,L,E,H,P>, I>& mapB) {
		std::set<NominalIndex<K,L,E,H,P>> res;
		for(const auto& cur : mapA) {
			res.insert(cur.first);
		}
		for(const auto& cur : mapB) {
			res.insert(cur.first);
		}
		return res;
	}

	/**
	 * The extract operator obtaining the values for a given index within a map (the index might not be present or
	 * explicitly covered).
	 */
	template<typename K, typename L, typename E, typename H, typename P, typename I>
	const I& extract(const std::map<NominalIndex<K,L,E,H,P>, I>& map, const NominalIndex<K,L,E,H,P>& i) {
		const static I empty = I();
		// check whether the requested index is present
		auto pos = map.find(i);
		return (pos == map.end()) ? empty : pos->second;
	}

	/**
	 * Checks whether the indices are covering overlapping areas
	 */
	template<typename K, typename L, typename E, typename H, typename P>
	bool overlap(const NominalIndex<K,L,E,H,P>& a, const NominalIndex<K,L,E,H,P>& b) {
		return a.getName() == b.getName();
	}

	// -------------------------------------------------------------------------------------------------

	/**
	 * The simplest form of an index - there is only a single instance, all array operations are
	 * applied to the same instance.
	 */
	class UnitIndex : public ArrayIndex<UnitIndex> {

	public:

		UnitIndex() {}
		UnitIndex(const core::arithmetic::Formula& f) {}

		bool operator==(const UnitIndex& other) const {
			return true;	// all instances are equivalent
		}

		bool operator<(const UnitIndex& other) const {
			return false;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "*";
		}

		std::size_t hash() const {
			return 0;	// all the same
		}

	};

	/**
	 * The cross operator computing the ranges to be included when combining two indexed structures.
	 */
	template<typename E>
	const std::set<UnitIndex>& cross(const std::map<UnitIndex, E>& mapA, const std::map<UnitIndex, E>& mapB) {
		static const std::set<UnitIndex> empty;
		static const std::set<UnitIndex> single = utils::set::toSet<std::set<UnitIndex>>(UnitIndex());
		return (mapA.empty() && mapB.empty()) ? empty : single;
	}

	/**
	 * The extract operator obtaining the values for a given index within a map (the index might not be present or
	 * explicitly covered).
	 */
	template<typename E>
	const E& extract(const std::map<UnitIndex, E>& map, const UnitIndex& i) {
		const static E empty = E();

		// check whether the requested index is present
		auto pos = map.find(i);
		return (pos == map.end()) ? empty : pos->second;
	}


	/**
	 * Checks whether the indices are covering overlapping areas
	 */
	inline bool overlap(const UnitIndex& a, const UnitIndex& b) {
		return true;
	}

	/**
	 * An index distinguishing concrete indices (if the value of a subscript is known) from cases
	 * where the index is unknown or too complex to be evaluated statically.
	 */
	class SingleIndex : public ArrayIndex<SingleIndex> {

		/**
		 * A flag determining whether this instances is representing a concrete index
		 * or an unknown index.
		 */
		bool concrete;

		/**
		 * The represented index - if it is known.
		 */
		std::uint64_t index;

	public:

		SingleIndex() : concrete(false), index(0) {}

		SingleIndex(std::uint64_t index) : concrete(true), index(index) {}

		SingleIndex(const core::arithmetic::Formula& f)
			: concrete(f.isInteger()), index((concrete)?f.getIntegerValue():0) {}

		bool operator==(const SingleIndex& other) const {
			if (this == &other) return true;
			return concrete == other.concrete && index == other.index;
		}

		bool operator<(const SingleIndex& other) const {
			if (!concrete) return false;
			if (!other.concrete) return true;
			return index < other.index;
		}

		bool isConcrete() const {
			return concrete;
		}

		std::uint64_t getIndex() const {
			assert(isConcrete());
			return index;
		}

		std::ostream& printTo(std::ostream& out) const {
			if (!concrete) return out << "*";
			return out << index;
		}

		std::size_t hash() const {
			return (isConcrete()) ? std::hash<std::uint64_t>()(getIndex()) : 743;
		}
	};

	/**
	 * The cross operator computing the ranges to be included when combining two indexed structures.
	 */
	template<typename E>
	std::set<SingleIndex> cross(const std::map<SingleIndex, E>& mapA, const std::map<SingleIndex, E>& mapB) {
		std::set<SingleIndex> res;
		for(const auto& cur : mapA) {
			res.insert(cur.first);
		}
		for(const auto& cur : mapB) {
			res.insert(cur.first);
		}
		return res;
	}

	/**
	 * The extract operator obtaining the values for a given index within a map (the index might not be present or
	 * explicitly covered).
	 */
	template<typename E>
	const E& extract(const std::map<SingleIndex, E>& map, const SingleIndex& i) {
		static const E empty = E();

		// check whether the requested index is present
		auto pos = map.find(i);
		if (pos != map.end()) {
			return pos->second;
		}

		// otherwise check whether the * element is present
		pos = map.find(SingleIndex());
		if (pos != map.end()) {
			return pos->second;
		}

		// no data present
		return empty;
	}

	/**
	 * Checks whether the indices are covering overlapping areas
	 */
	inline bool overlap(const SingleIndex& a, const SingleIndex& b) {
		return a == b || !a.isConcrete() || !b.isConcrete();
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

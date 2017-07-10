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

#include <algorithm>
#include <set>
#include <ostream>
#include <vector>

#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {
namespace cba {

	template<typename Elem, typename Compare = std::less<Elem>>
	class Set {

	public:
		using SetType = std::set<Elem, Compare>;

	private:
		SetType elements;

		bool all;

		Set(bool all)
			: elements(), all(all) {}

	public:

		Set()
			: elements(), all(false) {}

		Set(const SetType& set)
			: elements(set), all(false) {}

		Set(SetType&& set)
			: elements(std::move(set)), all(false) {}

		Set(const Set&) = default;
		Set(Set&&) = default;

		Set& operator=(const Set&) = default;
		Set& operator=(Set&&) = default;

		static Set getUniversal() {
			return Set(true);
		}

		bool empty() const {
			return !all && elements.empty();
		}

		std::size_t size() const {
			assert_false(isUniversal());
			return elements.size();
		}

		const SetType& getElements() const {
			assert_false(isUniversal());
			return elements;
		}

		bool isUniversal() const {
			return all;
		}

		void insert(const Elem& a) {
			if (all) return;
			elements.insert(a);
 		}

		bool contains(const Elem& a) const {
			return all || (elements.find(a) != elements.end());
		}

		auto begin() const -> decltype(elements.begin()) {
			assert_false(isUniversal());
			return elements.begin();
		}

		auto end() const -> decltype(elements.end()) {
			assert_false(isUniversal());
			return elements.end();
		}

		bool operator==(const Set& other) const {
			if (all && other.all) return true;
			if (all != other.all) return false;
			assert_true(!all && !other.all);
			return elements == other.elements;
		}

		bool operator!=(const Set& other) const {
			return !(*this == other);
		}

		bool operator<(const Set& other) const {
			if (all) return !other.all;
			if (other.all) return true;
			return elements < other.elements;
		}

		friend std::ostream& operator<<(std::ostream& out, const Set& set) {
			if (set.all) {
				return out << "{-all-}";
			}
			return out << set.elements;
		}

	};


	template <typename Elem, typename Compare>
	Set<Elem, Compare> merge(const Set<Elem, Compare>& a, const Set<Elem, Compare>& b) {
		if(a.isUniversal() || b.isUniversal()) return Set<Elem, Compare>::getUniversal();
		const auto& sa = a.getElements();
		const auto& sb = b.getElements();
		if(sa.empty()) return b;
		if(sb.empty()) return a;
		std::vector<Elem> res;
		std::set_union(sa.begin(), sa.end(), sb.begin(), sb.end(), std::back_inserter(res), Compare());
		return std::set<Elem, Compare>(res.begin(), res.end());
	}


	template <typename Elem, typename Compare>
	Set<Elem, Compare> intersect(const Set<Elem, Compare>& a, const Set<Elem, Compare>& b) {
		// handle universal sets
		if(a.isUniversal()) return b;
		if(b.isUniversal()) return a;

		// handle explicit sets
		const auto& sa = a.getElements();
		const auto& sb = b.getElements();
		if(sa.empty()) return a;
		if(sb.empty()) return b;

		// compute the intersection
		std::vector<Elem> res;
		std::set_intersection(sa.begin(), sa.end(), sb.begin(), sb.end(), std::back_inserter(res), Compare());
		return std::set<Elem, Compare>(res.begin(), res.end());
	}

	template<typename Elem, typename Compare>
	bool isSubsetOf(const Set<Elem,Compare>& a, const Set<Elem,Compare>& b) {
		if (b.isUniversal()) return true;
		if (a.isUniversal()) return false;
		for(const auto& e : a) {
			if (!b.contains(e)) return false;
		}
		return true;
	}

} //'end namespace cba
} // end namespace analysis
} // end namespace insieme


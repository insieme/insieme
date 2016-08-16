/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <algorithm>
#include <set>
#include <ostream>

#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {

	template<typename Elem, typename Compare = std::less<Elem>>
	class Set {

		std::set<Elem, Compare> elements;

		bool all;

		Set(bool all)
			: elements(), all(all) {}

	public:

		Set()
			: elements(), all(false) {}

		Set(const std::set<Elem, Compare>& set)
			: elements(set), all(false) {}

		Set(std::set<Elem, Compare>&& set)
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

		const std::set<Elem, Compare>& getElements() const {
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

		auto end() const -> decltype(elements.begin()) {
			assert_false(isUniversal());
			return elements.begin();
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
		std::set_union(sa.begin(), sa.end(), sb.begin(), sb.end(), res.begin(), Compare());
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
		std::set_intersection(sa.begin(), sa.end(), sb.begin(), sb.end(), res.begin(), Compare());
		return std::set<Elem, Compare>(res.begin(), res.end());
	}

} // end namespace analysis
} // end namespace insieme


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

namespace insieme {
namespace analysis {

	template<typename Elem, unsigned bound = 10>
	class BoundedSet {

		std::set<Elem> elements;

		bool all;

		BoundedSet(bool all)
			: elements(), all(all) {}

		BoundedSet(const std::set<Elem>& elements)
			: elements(elements), all(false) {}

		BoundedSet(std::set<Elem>&& elements)
			: elements(std::move(elements)), all(false) {}

	public:

		BoundedSet()
			: elements(), all(false) {}

		static BoundedSet getUniversal() {
			return BoundedSet(true);
		}

		static BoundedSet fromSet(const std::set<Elem>& a) {
			if (a.size() > bound) return getUniversal();
			return BoundedSet(a);
		}

		static BoundedSet fromSet(std::set<Elem>&& a) {
			if (a.size() > bound) return getUniversal();
			return BoundedSet(a);
		}

		bool empty() const {
			return !all && elements.empty();
		}

		std::size_t size() const {
			return (all) ? std::numeric_limits<std::size_t>::max() : elements.size();
		}

		const std::set<Elem>& getElements() const {
			return elements;
		}

		bool isUniversal() const {
			return all;
		}

		void insert(int a) {
			if (all) return;
			elements.insert(a);
 		}

		bool operator==(const BoundedSet& other) const {
			if (all && other.all) return true;
			if (all != other.all) return false;
			assert_true(!all && !other.all);
			return elements == other.elements;
		}

		bool operator!=(const BoundedSet& other) const {
			return !(*this == other);
		}

		friend std::ostream& operator<<(std::ostream& out, const BoundedSet& set) {
			if (set.all) {
				return out << "{-all-}";
			}
			return out << set.elements;
		}

	};

	template<typename Elem, unsigned bound>
	BoundedSet<Elem,bound> merge(const BoundedSet<Elem,bound>& a, const BoundedSet<Elem,bound>& b) {
		if (a.isUniversal() || b.isUniversal()) return BoundedSet<Elem,bound>::getUniversal();
		const auto& sa = a.getElements();
		const auto& sb = b.getElements();
		if (sa.empty()) return b;
		if (sb.empty()) return a;
		std::set<Elem> res;
		std::set_union(sa.begin(),sa.end(),sb.begin(),sb.end(),res.begin());
		return BoundedSet<Elem,bound>::fromSet(std::move(res));
	}

} // end namespace analysis
} // end namespace insieme


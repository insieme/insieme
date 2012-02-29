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

#include <utility>
#include <vector>
#include <algorithm>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	template<typename iter>
	struct range : public std::pair<iter, iter> {

		typedef iter iterator;
		typedef iter const_iterator;

		typedef typename iterator::value_type value_type;

		range(const iter& begin, const iter& end)
			: std::pair<iter,iter>(begin, end) {}

		iterator begin() const { return this->first; };
		iterator end() const { return this->second; };
		iterator cbegin() const { return this->first; };
		iterator cend() const { return this->second; };

		std::size_t size() const { return std::distance(begin(), end()); }
		bool empty() const { return begin() == end(); }

		const value_type& front() const {
			return *begin();
		}

		const value_type& back() const {
			return *(end()-1);
		}

		const value_type& operator[](unsigned index) const {
			return *(begin() + index);
		}

		template<typename Container>
		bool operator==(const Container& other) const {
			return size() == other.size() && equal(begin(), end(), other.begin());
		}

		template<typename Container>
		bool operator!=(const Container& other) const {
			return !(*this == other);
		}

		/**
		 * An implicit converter realizing this range within a vector.
		 */
		operator vector<value_type>() const {
			return vector<value_type>(begin(), end());
		}
	};


	template<typename iter>
	range<iter> make_range(const iter& a, const iter& b) {
		return range<iter>(a,b);
	}

	template<typename Container, typename Iter>
	bool operator==(const Container& other, const range<Iter>& range) {
		return range == other;
	}

	template<typename Container, typename Iter>
	bool operator!=(const Container& other, const range<Iter>& range) {
		return range != other;
	}

} // end namespace utils
} // end namespace insieme

namespace std {

	/**
	 * Allows to print ranges including printable elements.
	 *
	 * @param out the stream to which the given range should be printed to
	 * @param container the range to be printed
	 * @return the handed in ostream to chain operation invocations.
	 */
	template<typename Iter>
	ostream& operator<<(ostream& out, const insieme::utils::range<Iter>& container) {
		// print and done
		return out << "[" << join(",", container) << "]";
	}
}

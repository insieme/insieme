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

#include <utility>
#include <vector>
#include <algorithm>

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	/**
	 * A utility class modeling a container of elements, thereby being a view on a
	 * sub-range of an underlying data structure. A range is defined by a pair
	 * of iterators referencing the begin and end of the represented range.
	 *
	 * A range can be used like any other container within container utilities
	 * like for_each, any, all or join.
	 *
	 * If the underlying iterators is a random-access iterator, it is also supporting
	 * direct access to individual elements using the indexing operator or meta information
	 * like the size.
	 *
	 * Unlike other containers, this view does not prevent the underlying data structure from
	 * being destroyed. If the underlying container is removed, any range defined above it
	 * will be invalid.
	 *
	 * @param iter the kind of iterator used for setting up the boundaries of the range
	 */
	template <typename iter>
	class range {
	  public:
		/**
		 * The type of iterator used internally.
		 */
		typedef iter iterator;

		/**
		 * The constant iterator offered by this range to iterate over its elements.
		 */
		typedef iter const_iterator;

		/**
		 * The type of value contained within this range.
		 */
		typedef typename iterator::value_type value_type;

	  private:
		/**
		 * The iterator pointing to the begin of the covered range.
		 */
		iter a;

		/**
		 * The iterator pointing to the end of the covered range.
		 */
		iter b;

	  public:
		/**
		 * Creates a new range covering the given interval.
		 *
		 * @param begin the begin of the interval
		 * @param end the end of the interval
		 */
		range(const iter& begin, const iter& end) : a(begin), b(end) {}

		/**
		 * Obtains access to the begin / end of the range.
		 */
		iterator begin() const {
			return a;
		};
		iterator end() const {
			return b;
		};
		iterator cbegin() const {
			return a;
		};
		iterator cend() const {
			return b;
		};

		/**
		 * Obtains a reference to the first element of the range. This
		 * is only valid in case the range is not empty.
		 *
		 * @return a reference to the first element within the range
		 */
		const value_type& front() const {
			return *a;
		}

		/**
		 * Obtains a reference to the last element of the range. This
		 * is only valid in case the range is not empty.
		 *
		 * @return a reference to the last element within the range
		 */
		const value_type& back() const {
			return *(b - 1);
		}

		/**
		 * Determines the number of elements within the range.
		 */
		std::size_t size() const {
			return std::distance(a, b);
		}

		/**
		 * Tests whether the range is empty.
		 */
		bool empty() const {
			return a == b;
		}

		/**
		 * Tests whether the range is covering a single element.
		 */
		bool single() const {
			return a + 1 == b;
		}

		/**
		 * Obtains a range covering a sub-range of this range spreading
		 * from i to the end of the range.
		 */
		range<iter> subrange(int i) const {
			return subrange(i, size());
		}

		/**
		 * Obtains a range covering a sub-range of this range spreading
		 * from i to j.
		 */
		range<iter> subrange(int i, int j) const {
			// check for emptiness
			if(i >= j) { return range<iter>(b, b); }
			int k = size() - j;
			return range<iter>(a + i, b - k);
		}

		/**
		 * If the underlying iterator is a random-access iterator elements can
		 * be directly accessed using the indexing operator.
		 *
		 * @param index the index of the requested element
		 * @return a constant reference to the maintained element
		 */
		const value_type& operator[](unsigned index) const {
			return *(a + index);
		}

		/**
		 * Reduces this range be eliminating the given number of elements from
		 * the head of the range.
		 */
		range<iter>& operator+=(unsigned i) {
			a = (i < size()) ? a + i : b;
			return *this;
		}

		/**
		 * Reduces this range be eliminating the given number of elements from
		 * the tail of the range.
		 */
		range<iter>& operator-=(unsigned i) {
			b = (i < size()) ? b - i : a;
			return *this;
		}

		/**
		 * Reduces this range be eliminating the given number of elements from
		 * the head of the range.
		 */
		range<iter> operator+(unsigned i) const {
			return range<iter>(*this) += i;
		}

		/**
		 * Reduces this range be eliminating the given number of elements from
		 * the tail of the range.
		 */
		range<iter> operator-(unsigned i) const {
			return range<iter>(*this) -= i;
		}

		/**
		 * Compares this container with the given container. Two containers are
		 * equal in case the contain the same sequence of elements.
		 *
		 * @param other the container to compare with
		 * @return true if equal, false otherwise
		 */
		template <typename Container>
		bool operator==(const Container& other) const {
			return size() == other.size() && equal(a, b, other.begin());
		}

		/**
		 * Compares this container with the given container for unequality. Two
		 * containers are unequal if they are not equal.
		 *
		 * @param other the container to compare with
		 * @return true if unequal, false otherwise
		 */
		template <typename Container>
		bool operator!=(const Container& other) const {
			return !(*this == other);
		}

		/**
		 * Establishes a order on ranges to allow them to be used within tree-
		 * based structures (sets / maps).
		 */
		bool operator<(const range<iter>& other) const {
			return lexicographical_compare(*this, other);
		}

		/**
		 * An implicit converter realizing this range within a vector.
		 */
		operator std::vector<value_type>() const {
			return std::vector<value_type>(a, b);
		}

		/**
		 * An implicit converter to a pair of iterators.
		 */
		operator std::pair<iter, iter>() const {
			return std::make_pair(a, b);
		}
	};


	/**
	 * A utility constructor reducing the amount of types to be specified when creating
	 * a range.
	 *
	 * @param a the begin of the range
	 * @param b the end of the range
	 * @return the produced range
	 */
	template <typename iter>
	range<iter> make_range(const iter& a, const iter& b) {
		return range<iter>(a, b);
	}

	/**
	 * Compares an arbitrary container with a range for equality.
	 */
	template <typename Container, typename Iter>
	typename std::enable_if<!std::is_same<Container, range<Iter>>::value, bool>::type operator==(const Container& other, const range<Iter>& range) {
		return range.template operator==(other);
	}

	/**
	 * Compares an arbitrary container with a range for unequality.
	 */
	template <typename Container, typename Iter>
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
	template <typename Iter>
	ostream& operator<<(ostream& out, const insieme::utils::range<Iter>& container) {
		// print and done
		return out << "[" << join(",", container) << "]";
	}
}

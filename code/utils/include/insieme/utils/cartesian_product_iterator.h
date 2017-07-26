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
 */

#pragma once

#include <iterator>
#include <vector>

#include <boost/operators.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/range.h"
#include "insieme/utils/lazy.h"

namespace insieme {
namespace utils {

	namespace detail {

		/**
		 * An iterator implementation iterating through the Cartesian product of some nested input containers.
		 *
		 * Users should not use this class directly - the following utility function are offered bellow:
		 * 		- cartesian_product_begin(..)			.. get a begin-iterator
		 * 		- cartesian_product_end(..)				.. get a end-iterator
		 * 		- cartesian_product_range(..)			.. get a range of begin/end iterator to be utilized within a for-each loop
		 *
		 * @tparam the iterator type of the inner container type
		 * @tparam ElementType the element type of the inner container type, by default automatically deduced
		 */
		template <typename InnerIterator, typename ElementType = typename InnerIterator::value_type>
		class CartesianProductIterator : public std::iterator<std::input_iterator_tag, std::vector<ElementType>>,
		                                 public boost::equality_comparable<CartesianProductIterator<InnerIterator>> {
			/**
			 * Internal representation of the current state - using digits for counting.
			 */
			struct Digit {
				InnerIterator begin;
				InnerIterator end;
				InnerIterator cur;

				bool operator==(const Digit& other) const {
					return cur == other.cur; // equality is only determined by the current position
				}
			};

			/**
			 * The current state of the iterator - a list of iterators pointing to individual elements.
			 */
			std::vector<Digit> digits;

			/**
			 * The current value the iterator is pointing to - lazy assembled form the digit state.
			 */
			Lazy<std::vector<ElementType>> value;

			/**
			 * Marks this iterator to point to the end position.
			 */
			bool points_to_end;

			/**
			 * A private constructor creating an iterator referencing the end.
			 */
			CartesianProductIterator() : digits(), points_to_end(true) {}

			/**
			 * A private constructor for initializing a an iterator.
			 */
			CartesianProductIterator(const std::vector<Digit>& digits) : digits(digits), points_to_end(false) {}

		  public:
			bool operator==(const CartesianProductIterator& other) const {
				return (points_to_end && other.points_to_end) || (!points_to_end && !other.points_to_end && digits == other.digits);
			}

			std::vector<ElementType>& operator*() {
				// check lazy-evaluated value instance
				if(!value) {
					// build up currently represented value (lazily)
					value = std::vector<ElementType>();
					std::vector<ElementType>& data = value;
					for(const auto& cur : digits) {
						data.push_back(*cur.cur);
					}
				}

				// return a reference to the lazily evaluated value
				return value;
			}

			std::vector<ElementType>* operator->() {
				return &**this();
			}

			CartesianProductIterator& operator++() {
				// skip operation if already at the end
				if(points_to_end) { return *this; }

				// clear value
				value.reset();

				// increment digit 'counter'
				for(auto it = digits.begin(); it != digits.end(); ++it) {
					// increment current digit
					++(it->cur);

					// if we haven't reached an overflow we are done
					if(it->cur != it->end) { return *this; }

					// if it is not the last position but we have an overflow => continue
					if(it->cur == it->end) {
						// reset to the start and continue
						it->cur = it->begin;
					}
				}

				// if we reach this position we have reached the end
				points_to_end = true;

				// done
				return *this;
			}

			CartesianProductIterator operator++(int) {
				CartesianProductIterator res = *this;
				++(*this);
				return res;
			}

			/**
			 * A static factory function to create a iterator pointing to the first element of the
			 * Cartesian product.
			 */
			template <typename NestedDataContainer>
			static CartesianProductIterator begin(const NestedDataContainer& data) {
				std::vector<Digit> pos;
				for(const auto& cur : data) {
					if(cur.begin() == cur.end()) { return end(data); }
					pos.push_back(Digit{cur.begin(), cur.end(), cur.begin()});
				}
				return CartesianProductIterator(pos);
			}

			/**
			 * A static factory function to create a iterator pointing to the first element after
			 * the last element of the Cartesian product.
			 */
			template <typename NestedDataContainer>
			static CartesianProductIterator end(const NestedDataContainer& /*data*/) {
				return CartesianProductIterator();
			}
		};
	}


	/**
	 * A factory function to create a iterator pointing to the first element of the
	 * Cartesian product.
	 */
	template <typename NestedDataContainer, typename product_iter = detail::CartesianProductIterator<typename NestedDataContainer::value_type::const_iterator>>
	product_iter cartesian_product_begin(const NestedDataContainer& data) {
		return product_iter::begin(data);
	}

	/**
	 * A factory function to create a iterator pointing to the first element after
	 * the last element of the Cartesian product.
	 */
	template <typename NestedDataContainer, typename product_iter = detail::CartesianProductIterator<typename NestedDataContainer::value_type::const_iterator>>
	product_iter cartesian_product_end(const NestedDataContainer& data) {
		return product_iter::end(data);
	}

	/**
	 * A factory function to create a range covering the full Cartesian product spanned by the
	 * given nested data containers.
	 */
	template <typename NestedDataContainer, typename product_iter = detail::CartesianProductIterator<typename NestedDataContainer::value_type::const_iterator>>
	range<product_iter> cartesian_product(const NestedDataContainer& data) {
		return make_range(cartesian_product_begin(data), cartesian_product_end(data));
	}


} // end namespace utils
} // end namespace insieme

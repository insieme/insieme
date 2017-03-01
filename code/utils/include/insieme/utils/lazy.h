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

namespace insieme {
namespace utils {

	/**
	 * A small utility helper wrapping a value into a lazy closure.
	 */
	template <typename V>
	class Lazy {
		/**
		 * The flag recording whether the value is valid or not.
		 */
		bool evaluated;

		/**
		 * The value to be represented.
		 */
		V value;

	  public:
		/**
		 * A simple constructor initializing the instance unevaluated.
		 */
		Lazy() : evaluated(false) {}

		/**
		 * Obtains a reference to the represented value in case it has been evaluated.
		 */
		V& getValue() {
			assert_true(evaluated) << "Unsupported access to unevaluated value!";
			return value;
		}

		/**
		 * Obtains a reference to the represented value in case it has been evaluated.
		 */
		const V& getValue() const {
			assert_true(evaluated) << "Unsupported access to unevaluated value!";
			return value;
		}

		/**
		 * Update the represented value. This method may only be called once.
		 *
		 * @param value the value to be represented.
		 * @return a reference to the internally stored value
		 */
		const V& setValue(const V& newValue) {
			assert_false(evaluated) << "Cannot update value twice!";
			value = newValue;
			evaluated = true;
			return value;
		}

		/**
		 * Allows to determine whether the value has already been evaluated or not.
		 */
		bool isEvaluated() const {
			return evaluated;
		}

		/**
		 * Resets this lazy container to an unevaluated state.
		 */
		void reset() {
			if(evaluated) {
				evaluated = false;
				value = V();
			}
		}

		/**
		 * Get access to value using indirection operator.
		 */
		V& operator*() {
			return getValue();
		}

		/**
		 * Get access to value using indirection operator.
		 */
		const V& operator*() const {
			return getValue();
		}

		/**
		 * An implicit conversion to the value type.
		 */
		operator V&() {
			return getValue();
		}

		/**
		 * An implicit conversion to the value type.
		 */
		operator const V&() const {
			return getValue();
		}

		/**
		 * Provides access to the represented value.
		 */
		V* operator->() {
			return &getValue();
		}

		/**
		 * Provides access to the represented value.
		 */
		const V* operator->() const {
			return &getValue();
		}

		/**
		 * An implicit conversion to a boolean allowing to check
		 * whether the lazy object has been evaluated or not.
		 */
		operator bool() const {
			return isEvaluated();
		}

		/**
		 * An overloaded assignment operator to simplify the assignment
		 * of values to this lazy type.
		 */
		Lazy<V>& operator=(const V& value) {
			setValue(value);
			return *this;
		}
	};


} // end namespace utils
} // end namespace insieme

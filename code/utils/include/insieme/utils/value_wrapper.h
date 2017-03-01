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
	 * A simple template wrapping a type into another named type. It can be used
	 * to realize a type distinction although the actual value to be used is the same.
	 *
	 * E.g. to represent a distance between two points you might use a simple float.
	 * However, in some cases you would like to use a specialized type to avoid confusing
	 * distances with other float values.
	 *
	 * Another use case are value annotations. You can only annotate a single integer as a
	 * value annotation. However, in many cases you would like to annotate a analysis result
	 * that only consist of a single integer (loop nesting level, nested calls, ...). To
	 * distinguish those, classes inheriting from this class can be used.
	 */
	template <typename V>
	struct ValueWrapper {
		/**
		 * The value to be wrapped.
		 */
		V value;

		/**
		 * Creates a new, default instance of this class representing
		 * the default value of the underlying type.
		 */
		ValueWrapper() : value() {}

		/**
		 * Creates a new instance based on the given value.
		 *
		 * @param value the value to be represented.
		 */
		ValueWrapper(const V& value) : value(value) {}

		/**
		 * An implicit conversion form this value to the wrapped value.
		 */
		operator V() const {
			return value;
		}

		// some operators just forwarded to the underlying value

		bool operator==(const ValueWrapper<V>& other) const {
			return value == other.value;
		}
		bool operator!=(const ValueWrapper<V>& other) const {
			return value != other.value;
		}
		bool operator<=(const ValueWrapper<V>& other) const {
			return value <= other.value;
		}
		bool operator>=(const ValueWrapper<V>& other) const {
			return value >= other.value;
		}
		bool operator<(const ValueWrapper<V>& other) const {
			return value < other.value;
		}
		bool operator>(const ValueWrapper<V>& other) const {
			return value > other.value;
		}
	};

	/**
	 * A macro reducing the amount code required for defining a new type derived from
	 * the generic ValueWrapper by implementing the necessary constructors.
	 */
	#define VALUE_TYPE(NAME, TYPE)                                                                                                                             \
		struct NAME : public insieme::utils::ValueWrapper<TYPE> {                                                                                              \
			NAME() : insieme::utils::ValueWrapper<TYPE>() {}                                                                                                   \
			NAME(const TYPE& value) : insieme::utils::ValueWrapper<TYPE>(value) {}                                                                             \
		}


} // end namespace utils
} // end namespace insieme

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
	template<typename V>
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

		bool operator==(const ValueWrapper<V>& other) const { return value == other.value; }
		bool operator!=(const ValueWrapper<V>& other) const { return value != other.value; }
		bool operator<=(const ValueWrapper<V>& other) const { return value <= other.value; }
		bool operator>=(const ValueWrapper<V>& other) const { return value >= other.value; }
		bool operator<(const ValueWrapper<V>& other) const { return value < other.value; }
		bool operator>(const ValueWrapper<V>& other) const { return value > other.value; }

	};

	/**
	 * A macro reducing the amount code required for defining a new type derived from
	 * the generic ValueWrapper by implementing the necessary constructors.
	 */
	#define VALUE_TYPE(NAME,TYPE) \
		struct NAME : public insieme::utils::ValueWrapper<TYPE> { \
			NAME() : insieme::utils::ValueWrapper<TYPE>() {} \
			NAME(const TYPE& value) : insieme::utils::ValueWrapper<TYPE>(value) {} \
		}


} // end namespace utils
} // end namespace insieme

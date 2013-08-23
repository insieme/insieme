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
	 * A small utility helper wrapping a value into a lazy closure.
	 */
	template<typename V>
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
			assert(evaluated && "Unsupported access to unevaluated value!");
			return value;
		}

		/**
		 * Obtains a reference to the represented value in case it has been evaluated.
		 */
		const V& getValue() const {
			assert(evaluated && "Unsupported access to unevaluated value!");
			return value;
		}

		/**
		 * Update the represented value. This method may only be called once.
		 *
		 * @param value the value to be represented.
		 * @return a reference to the internally stored value
		 */
		const V& setValue(const V& newValue) {
			assert(!evaluated && "Cannot update value twice!");
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
			if (evaluated) {
				evaluated = false;
				value = V();
			}
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

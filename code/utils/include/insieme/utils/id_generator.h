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
	 * A generic class realizing a simple ID generator.
	 *
	 * @tparma T the type of ID to be generated by the instance.
	 * 				the type has to support the ++ operator (prefix).
	 */
	template <typename T = std::size_t>
	class SimpleIDGenerator {
		/**
		 * The last id produced by this generator.
		 */
		T last;

	  public:
		/**
		 * A member type representing the type of value generated by this generator.
		 */
		typedef T id_type;

		/**
		 * A default constructor initializing this ID generator with 0. The first
		 * ID to be generated will be the 1.
		 */
		SimpleIDGenerator() : last(0) {}

		/**
		 * A default constructor initializing this ID generator with the given value.
		 * The first ID to be returned will be the init + 1.
		 */
		SimpleIDGenerator(id_type init) : last(init) {}

		/**
		 * Produces the next ID.
		 */
		id_type getNext() {
			return ++last;
		}

		/**
		 * Updates the generator to continue with the given value.
		 */
		void setNext(id_type value) {
			last = value - 1;
		}
	};


} // end namespace utils
} // end namespace insieme

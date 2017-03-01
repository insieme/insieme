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

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace types {

	/**
	 * An exception type used if a return type could not successfully be deduced.
	 */
	class ReturnTypeDeductionException : public std::exception {
		// the message describing this type of exception
		const string msg;

	  public:
		ReturnTypeDeductionException(const string& msg = "Unable to deduce return type.") : msg(msg) {}
		virtual ~ReturnTypeDeductionException() throw() {}
		virtual const char* what() const throw() {
			return msg.c_str();
		}
	};

	/**
	 * This function is trying to deduce the type returned when calling a function having the
	 * given type by passing parameters of the given types. If the type cannot be deduced,
	 * an exception is raised.
	 *
	 * @param funType the type of the function to be invoked, for which the return type should be deduced
	 * @param argumentTypes the types of arguments passed to this function
	 * @return the deduced, most generic return type
	 * @throws ReturnTypeDeductionException if the requested type cannot be deduced
	 */
	TypePtr tryDeduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes);

	/**
	 * This function is deducing the type returned when calling a function having the given
	 * type by passing parameters of the given types.
	 *
	 * @param funType the type of the function to be invoked, for which the return type should be deduced
	 * @param argumentTypes the types of arguments passed to this function
	 * @return the deduced, most generic return type
	 */
	TypePtr deduceReturnType(const FunctionTypePtr& funType, const TypeList& argumentTypes, bool unitOnFail = true);


} // end namespace types
} // end namespace core
} // end namespace insieme

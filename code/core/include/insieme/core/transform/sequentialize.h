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

#include "insieme/core/forward_decls.h"

#include "insieme/core/ir_pointer.h"

namespace insieme {
namespace core {
namespace transform {

	/**
	 * Converts the given statement into an equivalent sequential code variant by replacing
	 * all parallel constructs inside with their sequential counterpart.
	 *
	 * @param manager the manager used to create new nodes
	 * @param stmt the statement to be sequentialized
	 * @param removeSyncOps enables the elimination of locks, atomics, barriers, flushes, ...
	 * @return a sequential version of the code
	 * @throws NotSequentializableException in case the conversion can not be completed
	 */
	NodePtr sequentialize(NodeManager& manager, const NodePtr& stmt, bool removeSyncOps = true);

	template <typename T>
	Pointer<const T> sequentialize(NodeManager& manager, const Pointer<const T>& code, bool removeSyncOps = true) {
		return sequentialize(manager, NodePtr(code), removeSyncOps).as<Pointer<const T>>();
	}

	/**
	 * Converts the given statement into an equivalent sequential code variant by replacing
	 * all parallel constructs inside with their sequential counterpart.
	 *
	 * @param manager the manager used to create new nodes
	 * @param stmt the statement to be sequentialized
	 * @param removeSyncOps enables the elimination of locks, atomics, barriers, flushes, ...
	 * @return a sequential version of the code or a null pointer if the given code can not be
	 * 			safely sequentialized.
	 */
	NodePtr trySequentialize(NodeManager& manager, const NodePtr& stmt, bool removeSyncOps = true);

	/**
	 * A generic form of the trySequentialize function preserving the type of the pointer being
	 * passed as an argument. The caller has to make sure that the result type is still valid after
	 * the sequentialization. If the sequentialization fails, a null pointer will be returned.
	 *
	 * @param manager the manager used to create new nodes
	 * @param stmt the statement to be sequentialized
	 * @param removeSyncOps enables the elimination of locks, atomics, barriers, flushes, ...
	 * @return a sequential version of the code or a null pointer if the given code can not be
	 * 			safely sequentialized.
	 */
	template <typename T>
	Pointer<const T> trySequentialize(NodeManager& manager, const Pointer<const T>& code, bool removeSyncOps = true) {
		return trySequentialize(manager, NodePtr(code), removeSyncOps).as<Pointer<const T>>();
	}

	class NotSequentializableException : public std::exception {
		std::string msg;

	  public:
		NotSequentializableException(const string& msg = "Unknown Error") : msg(msg){};
		virtual const char* what() const throw() {
			return msg.c_str();
		}
		virtual ~NotSequentializableException() throw() {}
	};

} // end namespace transform
} // end namespace core
} // end namespace insieme

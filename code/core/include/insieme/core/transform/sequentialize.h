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

	template<typename T>
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
	template<typename T>
	Pointer<const T> trySequentialize(NodeManager& manager, const Pointer<const T>& code, bool removeSyncOps = true) {
		return trySequentialize(manager, NodePtr(code), removeSyncOps).as<Pointer<const T>>();
	}

	class NotSequentializableException : public std::exception {
		std::string msg;
	public:
		NotSequentializableException(const string& msg = "Unknown Error") : msg(msg) {};
		virtual const char* what() const throw() { return msg.c_str(); }
		virtual ~NotSequentializableException() throw() { }
	};

} // end namespace transform
} // end namespace core
} // end namespace insieme

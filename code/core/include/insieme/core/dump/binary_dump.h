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

#include <ostream>
#include <istream>

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace dump {

	namespace binary {

		/**
		 * Writes a binary encoding of the given IR node into the given output stream.
		 *
		 * @param out the stream to be writing to
		 * @param ir the code fragment to be written
		 */
		void dumpIR(std::ostream& out, const NodePtr& ir);

		/**
		 * Restores an IR code fragment from the given input stream. For constructing
		 * the resulting nodes, the given manager will be used. In case the stream contains
		 * an illegal encoding, an InvalidEncodingException will be thrown.
		 *
		 * @param in the stream to be reading from
		 * @param manager the node manager to be used for creating nodes
		 * @return the resolved node
		 */
		NodePtr loadIR(std::istream& in, NodeManager& manager);

		/**
		 * An exception which will be raised in case the binary stream is not
		 * containing a valid encoding of an IR code fragment.
		 */
		class InvalidEncodingException : public std::exception {
			static const char* MSG;
		public:
			virtual const char* what() const throw() { return MSG; }
		};


	} // end namespace binary

} // end namespace dump
} // end namespace core
} // end namespace insieme

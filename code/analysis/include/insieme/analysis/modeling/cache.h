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
#include <stdexcept>

namespace insieme { 
namespace core {
class NodeManager;

template <class T>
class Pointer;

class Node;

} // end core namespace 

namespace analysis { namespace modeling {

/**
 * Used to quite analysis when was not possible to derive a cache model for a given IR code
 */
class CacheModelingError : public std::logic_error { 
public:
	CacheModelingError(const std::string& msg) : std::logic_error(msg) { }
};

/**
 * Determines the amount of cache misses for the given code. 
 */
void mapCache(const core::Pointer<const core::Node>& root, size_t block_size=32, size_t cache_size=32768);


/**
 * Compute the reuse distance of the given code. In the case the given code is not a SCoP an exception will be thrown
 */
size_t getReuseDistance(const core::Pointer<const core::Node>& root, size_t block_size=32, size_t cache_size=32768);


} } } // end insieme::analysis::modeling

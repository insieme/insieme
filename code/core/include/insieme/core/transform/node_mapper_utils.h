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

#include "insieme/core/ast_node.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace core {
namespace transform {

/**
 * A common base class for implementing node mappings which benefit from caching results
 */
class CachedNodeMapping : public NodeMapping {

	/**
	 * The cache to be used for reusing results.
	 */
	insieme::utils::cache::PointerCache<NodePtr, NodePtr> cache;

public:

	/**
	 * A default constructor initializing the factory method of the
	 * internally maintained cache.
	 */
	CachedNodeMapping() : cache(fun(*this, &CachedNodeMapping::resolveElement)) {};

	/**
	 * The mapping function which is checking whether the given node has already been
	 * processed. If so, the result of the previous processing is returned. Otherwise,
	 * the method resolveElement(...) is used to obtain a replacement and returned.
	 *
	 * NOTE: should not be overridden by sub-class
	 */
	virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
		// just look up content of cache (will be resolved automatically)
		return cache.get(ptr);
	}

	/**
	 * A pure virtual method to be implemented by sub-classes.
	 */
	virtual const NodePtr resolveElement(const NodePtr& ptr) = 0;


};


/**
 * A utility class mapping a child list of a node using some other node mapping. After
 * mapping all children, it verifies whether any modification has been applied.
 */
class ChildListMapping : public NodeMapping {

	/**
	 * The mapped list of children.
	 */
	vector<NodePtr> children;

	/**
	 * A flag indicating whether there has been any difference between the original
	 * and the mapped list of children.
	 */
	bool different;

public:

	ChildListMapping(const Node::ChildList& list, NodeMapping& mapping)
		: NodeMapping(), children(mapping.map(0, list)), different(!equals(children, list)) {}

	/**
	 * Determines whether this mapping would cause any modification when being applied
	 * to the child list it has been constructed for.
	 */
	bool isDifferent() const {
		return different;
	}

	/**
	 * Performs the actual mapping of a child node. The resulting node is identical to the
	 * node returned by the mapping passed to the constructor of this class.
	 *
	 * @param index the index of the child node to be mapped
	 * @param ptr the child to be mapped.
	 * @return the mapped child node.
	 */
	virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
		return children[index];
	}

};


} // end namespace transform
} // end namespace core
} // end namespace insieme

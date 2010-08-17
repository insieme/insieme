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

#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/is_const.hpp>
#include <boost/utility/enable_if.hpp>

#include "instance_manager.h"
#include "visitor.h"


// ---------------------------------- Type Manager ----------------------------------------

template<
	typename NodeType,
	typename PtrType,
	typename boost::enable_if<boost::is_const<NodeType>,int>::type = 0,
	typename boost::enable_if<boost::is_base_of<Visitable<PtrType >, NodeType>, int>::type = 0,
	typename boost::enable_if<boost::is_base_of<InstancePtr<NodeType>, PtrType>,int>::type = 0
	>
class TreeManager: public InstanceManager<NodeType, PtrType> {

	/**
	 * This method represents the internal implementation of the actual lookup method. The method
	 * is trying to look up the given node within the internal store. If the same
	 * data element is already present, a pointer to the present copy is returned. Otherwise,
	 * the given copy is cloned and added to the store. A pointer to the new element is
	 * then returned.
	 *
	 * @param node the node for which a master copy should be obtained
	 * @return the pointer to the obtained element
	 */
	PtrType getPointerInternal(const NodeType& node) {

		// get master copy
		std::pair<PtrType, bool> res = add(node);

		// check whether all child-nodes are within this manager
		assert( !res.second || containsAll(*(res.first->getChildren())));

		// return newly added or present node
		return res.first;
	}

public:

	/**
	 * A generic wrapper enclosing the internal implementation of the lookup method.
	 *
	 * @param node the node for which a master copy should be obtained
	 * @return the pointer to the obtained element
	 * @see TreeManager::getPointerInternal(const NodeType&)
	 */
	PtrType getPointer(const NodeType& node) {
		return getPointerInternal(node);
	}

	/**
	 * Obtains a type pointer pointing to an identical element as the given pointer.
	 * However, the referenced element will be maintained by this manager.
	 *
	 * @param pointer for which a local reference should be obtained
	 * @return a pointer pointing to a local copy of the given pointer
	 */
	PtrType getPointer(const PtrType& pointer) {
		if (!pointer) {
			return pointer;
		}
		return this->getPointer(*pointer);
	}

	/**
	 * Obtains a list of pointer referencing the same elements as the given list
	 * of pointers. However, the obtained one will reference elements managed by
	 * this manager.
	 *
	 * @param pointer the list of pointers to be covered looked up
	 * @return an equivalent list of pointers addressing elements within the local manager
	 */
	vector<PtrType> getPointer(const vector<PtrType>& pointers) {

		// just create resulting list ...
		vector<PtrType> res;
		res.reserve(pointers.size());

		// ... and look up all elements of the input list
		std::transform(pointers.cbegin(), pointers.cend(), back_inserter(res),
			[&](const PtrType& cur) {
				return this->getPointer(*cur);
		});

		return res;
	}
};

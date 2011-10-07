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

namespace insieme {
namespace core {

/**
 * This class constitutes an interface for utility class required for transforming AST nodes.
 * Instances of this class represent mappings between nodes. During the transformation process,
 * each referenced pointer is replaced by the element is mapped to.
 */
class NodeMapping {

protected:

	/**
	 * Implements the actual mapping operation by mapping the given pointer (and the context information)
	 * to a new pointer.
	 *
	 * @param index the index of the ptr within the parents child list
	 * @param ptr the pointer to be resolved
	 * @return the pointer the given pointer is mapped to by this mapper
	 */
	virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) =0;

public:

	/**
	 * A virtual destructor of the mapping for a proper cleanup.
	 */
	virtual ~NodeMapping() { };

	/**
	 * A generic version of the map operation to be applied on a root node.
	 */
	template<typename T>
	inline Pointer<T> map(const Pointer<T>& ptr) {
		return map<T>(0, ptr);
	}

	/**
	 * A generic version of the map operation handling pointer types properly.
	 */
	template<typename T>
	inline Pointer<T> map(unsigned index, const Pointer<T>& ptr) {
		// short-cut for null
		if (!ptr) {
			return static_pointer_cast<T> (ptr);
		}

		// map and cast
		const NodePtr res = mapElement(index, ptr);
		// during development, make cast secure
		assert(dynamic_pointer_cast<T> (res) && "Invalid conversion");
		return static_pointer_cast<T> (res);
	}

	/**
	 * Obtains a container of pointers referencing clones of nodes referenced by a given
	 * container of pointers. Thereby, annotations are properly preserved and isolated.
	 *
	 * @param container the container including the pointers to be cloned
	 * @return a new container including pointers referencing clones of the nodes referenced
	 * 		   by the original container.
	 */
	template<typename Container>
	Container map(unsigned offset, const Container& container) {
		Container res;

		auto first = container.begin();
		auto last = container.end();
		auto out = inserter(res, res.end());
		for (auto it = first; it != last; ++it) {
			*out = map(offset++, *it);
			out++;
		}

		return res;
	}

};

template<typename Lambda>
class LambdaNodeMapper: public NodeMapping {
	Lambda lambda;
public:
	LambdaNodeMapper(Lambda lambda)
		: lambda(lambda) { };

	const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
		return lambda(index, ptr);
	}
};

template<typename Lambda>
LambdaNodeMapper<Lambda> makeLambdaMapper(Lambda lambda) {
	return LambdaNodeMapper<Lambda> (lambda);
}

} // end namespace core
} // end namespace insieme

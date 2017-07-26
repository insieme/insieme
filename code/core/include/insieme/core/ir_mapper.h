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

#include "insieme/utils/unused.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_pointer.h"

namespace insieme {
namespace core {

	/**
	 * This class constitutes an interface for utility class required for transforming IR nodes.
	 * Instances of this class represent mappings between nodes. During the transformation process,
	 * each referenced pointer is replaced by the element it is mapped to.
	 */
	template <typename Context>
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
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr, Context& c) = 0;

	  public:
		/**
		 * A virtual destructor of the mapping for a proper cleanup.
		 */
		virtual ~NodeMapping(){};

		/**
		 * A generic version of the map operation to be applied on a root node.
		 */
		template <typename T>
		inline Pointer<T> map(const Pointer<T>& ptr, Context& c) {
			return map<T>(0, ptr, c);
		}

		/**
		 * A generic version of the map operation handling pointer types properly.
		 */
		template <typename T>
		inline Pointer<T> map(unsigned index, const Pointer<T>& ptr, Context& c) {
			// short-cut for null
			if(!ptr) { return Pointer<T>(); }

			// map and cast
			return mapElement(index, ptr, c).template as<Pointer<T>>();
		}

		/**
		 * Obtains a container of pointers referencing clones of nodes referenced by a given
		 * container of pointers. Thereby, annotations are properly preserved and isolated.
		 *
		 * @param container the container including the pointers to be cloned
		 * @return a new container including pointers referencing clones of the nodes referenced
		 * 		   by the original container.
		 */
		NodeList mapAll(const NodeList& container, Context& c) {
			NodeList res;

			auto first = container.begin();
			auto last = container.end();
			auto out = inserter(res, res.end());
			unsigned counter = 0;
			for(auto it = first; it != last; ++it) {
				*out = map(counter++, *it, c);
				out++;
			}

			return res;
		}
	};

	class SimpleNodeMapping : public NodeMapping<int> {
	  public:
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr, int&) final override {
			return mapElement(index, ptr);
		}
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) = 0;

		template <typename T>
		inline Pointer<T> map(const Pointer<T>& ptr) {
			int ctxt = 0;
			return NodeMapping<int>::map<T>(0, ptr, ctxt);
		}
		template <typename T>
		inline Pointer<T> map(unsigned index, const Pointer<T>& ptr) {
			int ctxt = 0;
			return NodeMapping<int>::map(index, ptr, ctxt);
		}
		NodeList mapAll(const NodeList& container) {
			int ctxt = 0;
			return NodeMapping<int>::mapAll(container, ctxt);
		}
	};

	template <typename Lambda>
	class LambdaNodeMapper : public SimpleNodeMapping {
		Lambda lambda;

	  public:
		LambdaNodeMapper(Lambda lambda) : lambda(lambda){};

		const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			return lambda(index, ptr);
		}
	};

	template <typename Lambda>
	LambdaNodeMapper<Lambda> makeLambdaMapper(Lambda lambda) {
		return LambdaNodeMapper<Lambda>(lambda);
	}

} // end namespace core
} // end namespace insieme

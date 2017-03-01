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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"

namespace insieme {
namespace core {
namespace transform {
namespace utils {

	/**
	 * A utility function handling the migration of annotations between nodes during
	 * transformations. It is invoking the migration handling routine of the node annotations
	 * to determine whether and how the individual annotations should be migrated.
	 */
	template <typename TypeA, typename TypeB, template <typename T> class Ptr>
	inline void migrateAnnotations(const Ptr<TypeA>& before, const Ptr<TypeB>& after) {
		typedef typename Node::annotation_container::annotation_map_type AnnotationMap;

		// check whether there is something to do
		if(before == after || !before->hasAnnotations()) { return; }

		// check whether before and after are the same, just in a different manager
		if(*before == *after) {
			// annotations need to be cloned
			for(auto& cur : before->getAnnotations()) {
				cur.second->clone(cur.second, after);
			}
			return;
		}

		// migrate annotations individually
		for_each(before->getAnnotations(), [&](const typename AnnotationMap::value_type& cur) { cur.second->migrate(cur.second, before, after); });
	}

	/**
	 * A utility function to migrate IR code fragments between node managers - thereby preserving
	 * potential migrateable annotations.
	 *
	 * @param src the fragment to be migrated
	 * @param trgMgr the targeted node manager
	 * @return a copy of src maintained by the target manager including all annotations
	 */
	template <typename Node>
	inline const Pointer<const Node> migrate(const Pointer<const Node>& src, NodeManager& trgMgr) {
		// copy node to target
		auto res = trgMgr.get(src);

		// migrate annotations
		visitDepthFirstOncePrunable(src, [&](const NodePtr& node) -> bool {

			// get copy of src node within target manager
			NodePtr cpy = trgMgr.get(node);

			// if copy is the same (at same address) we can stop here
			if(cpy == node) return true;

			// move annotations
			migrateAnnotations(node, cpy);
			return false;
		}, true);

		// done
		return res;
	}


} // end namespace utils
} // end namespace transform
} // end namespace core
} // end namespace insieme

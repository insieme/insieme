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

#include <type_traits>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node_types.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 									Node Type Traits
	// **********************************************************************************

	namespace detail {

		/**
		 * A helper for defining node traits ...
		 */
		template <typename T>
		struct node_type_helper {
			typedef T type;
			typedef Pointer<const T> ptr_type;
			typedef Address<const T> adr_type;
		};

		template <typename T, NodeType N>
		struct concrete_node_type_helper : public node_type_helper<T> {
			BOOST_STATIC_CONSTANT(NodeType, nt_value = N);
		};
	}

	/**
	 * A type trait linking node types to their properties.
	 */
	template <typename T>
	struct node_type;

	template <typename T>
	struct concrete_node_type;

	/**
	 * A trait struct linking node type values to the represented node's properties.
	 */
	template <NodeType typ>
	struct to_node_type;

	#define CONCRETE(NAME)                                                                                                                                     \
		template <>                                                                                                                                            \
		struct concrete_node_type<NAME> : public detail::concrete_node_type_helper<NAME, NT_##NAME> {};                                        \
		template <>                                                                                                                                            \
		struct to_node_type<NT_##NAME> : public concrete_node_type<NAME> {};
	#include "insieme/core/ir_nodes.def"
	#undef CONCRETE

	#define NODE(NAME)                                                                                                                                         \
		template <>                                                                                                                                            \
		struct node_type<NAME> : public detail::node_type_helper<NAME> {                                                                       \
			static const std::string& getName() {                                                                                                              \
				static const std::string name = #NAME;                                                                                                         \
				return name;                                                                                                                                   \
			}                                                                                                                                                  \
		};                                                                                                                                                     \
		template <>                                                                                                                                            \
		struct node_type<const NAME> : public node_type<NAME> {};
	#include "insieme/core/ir_nodes.def"
	#undef NODE

	/**
	 * Determines whether the given node type belongs to the requested category.
	 */
	template <NodeCategory category>
	bool isA(NodeType type) {
		switch(type) {
		#define CONCRETE(KIND)                                                                                                                                 \
			case NT_##KIND:                                                                                                                                    \
				return std::is_base_of<typename node_category_trait<category>::base_type, typename to_node_type<NT_##KIND>::type>::value;
		#include "insieme/core/ir_nodes.def"
		#undef CONCRETE
		}
		return false;
	}

} // end namespace core
} // end namespace insieme

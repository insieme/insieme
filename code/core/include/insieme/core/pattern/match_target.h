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
#include "insieme/core/ir_node_types.h"

#include "insieme/core/pattern/structure.h"

namespace insieme {
namespace core {
namespace pattern {

namespace details {

	struct target_info {};

	template <typename TargetType, typename ValueType, typename IDType, typename AtomType>
	struct match_target_info_helper : public target_info {
		typedef TargetType target_type;
		typedef ValueType value_type;
		typedef IDType id_type;
		typedef AtomType atom_type;

		typedef vector<ValueType> list_type;
		typedef typename list_type::const_iterator list_iterator;
	};
}

struct ptr_target : public details::match_target_info_helper<ptr_target, core::NodePtr, core::NodeType, core::NodePtr> {};
struct address_target : public details::match_target_info_helper<address_target, core::NodeAddress, core::NodeType, core::NodePtr> {};
struct tree_target : public details::match_target_info_helper<tree_target, TreePtr, unsigned, TreePtr> {};

template <typename T>
struct match_target_info;
template <>
struct match_target_info<core::NodePtr> : public ptr_target {};
template <>
struct match_target_info<core::NodeAddress> : public address_target {};
template <>
struct match_target_info<TreePtr> : public tree_target {};


} // end namespace pattern
} // end namespace core
} // end namespace insieme

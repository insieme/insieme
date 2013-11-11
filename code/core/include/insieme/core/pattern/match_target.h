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
#include "insieme/core/ir_node_types.h"

#include "insieme/core/pattern/structure.h"

namespace insieme {
namespace core {
namespace pattern {

	namespace details {

		struct target_info {};

		template<
			typename TargetType,
			typename ValueType,
			typename IDType,
			typename AtomType
		>
		struct match_target_info_helper : public target_info {

			typedef TargetType target_type;
			typedef ValueType value_type;
			typedef IDType id_type;
			typedef AtomType atom_type;

			typedef vector<ValueType> list_type;
			typedef typename list_type::const_iterator list_iterator;

		};

	}

	struct ptr_target
		: public details::match_target_info_helper<ptr_target, core::NodePtr, core::NodeType, core::NodePtr> {};
	struct address_target
		: public details::match_target_info_helper<address_target, core::NodeAddress, core::NodeType, core::NodePtr> {};
	struct tree_target
		: public details::match_target_info_helper<tree_target, TreePtr, unsigned, TreePtr> {};

	template<typename T> struct match_target_info;
	template<> struct match_target_info<core::NodePtr> : public ptr_target {};
	template<> struct match_target_info<core::NodeAddress> : public address_target {};
	template<> struct match_target_info<TreePtr> : public tree_target {};


} // end namespace pattern
} // end namespace core
} // end namespace insieme

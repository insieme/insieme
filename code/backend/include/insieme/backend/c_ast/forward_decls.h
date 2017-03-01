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

#include <set>
#include <memory>

#include "insieme/utils/pointer.h"

namespace insieme {
namespace backend {
namespace c_ast {

/**
 * Adds forward declarations for all C AST node types. Further, for each
 * type a type definition for a corresponding annotated pointer is added.
 */
#define NODE(NAME)                                                                                                                                             \
	struct NAME;                                                                                                                                                \
	typedef Ptr<NAME> NAME##Ptr;                                                                                                                               \
// take all nodes from within the definition file
#include "insieme/backend/c_ast/c_nodes.def"
#undef NODE

#define CONCRETE(name) NT_##name,
	enum NodeType {
	// the necessary information is obtained from the node-definition file
	#include "insieme/backend/c_ast/c_nodes.def"
	};
	#undef CONCRETE


	class CNodeManager;
	typedef std::shared_ptr<CNodeManager> SharedCNodeManager;

	class CodeFragmentManager;
	typedef std::shared_ptr<CodeFragmentManager> SharedCodeFragmentManager;

	class CCode;
	typedef std::shared_ptr<CCode> CCodePtr;

	class CodeFragment;
	typedef Ptr<CodeFragment> CodeFragmentPtr;

	class CCodeFragment;
	typedef Ptr<CCodeFragment> CCodeFragmentPtr;

	class DummyFragment;
	typedef Ptr<DummyFragment> DummyFragmentPtr;

	class IncludeFragment;
	typedef Ptr<IncludeFragment> IncludeFragmentPtr;

	typedef std::set<CodeFragmentPtr> FragmentSet;

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

namespace std {

	/**
	 * Allows node types to be printed using names.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::backend::c_ast::NodeType& type);

} // end namespace std

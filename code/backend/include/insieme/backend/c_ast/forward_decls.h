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
	#define NODE(NAME) \
	class NAME; \
	typedef Ptr<NAME> NAME ## Ptr; \
	// take all nodes from within the definition file
	#include "insieme/backend/c_ast/c_nodes.def"
	#undef NODE

	#define CONCRETE(name) NT_ ## name,
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


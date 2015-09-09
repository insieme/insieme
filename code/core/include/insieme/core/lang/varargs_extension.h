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

#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace lang {

	/**
		* Handling of C variable argument lists 
		* - e.g. bla(int x, ...)
		*/
	class VarArgsExtension : public core::lang::Extension {
		/**
			* Allow the node manager to create instances of this class.
			*/
		friend class core::NodeManager;

		/**
			* Creates a new instance based on the given node manager.
			*/
		VarArgsExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

		// this extension is based upon the symbols defined by the reference module
		IMPORT_MODULE(ReferenceExtension);

		public:
		// represents a packed list of arguments passed to a function taking a variable number of arguments (e.g. printf)
		LANG_EXT_TYPE(VarList, "var_list")
				
		// operation for packing a list of arguments into a var_list
		LANG_EXT_LITERAL(VarlistPack, "varlist_pack", "('alpha) -> var_list")

		// represents the type used as a parameter for the macros defined in <cstdarg> to retrieve the additional arguments of a function
		LANG_EXT_TYPE(Valist, "va_list");
			
		// mappings of the C functions used to handle va_lists
		LANG_EXT_LITERAL(Vaarg, "va_arg", "(va_list, type<'a>) -> 'a");
		LANG_EXT_LITERAL(Vastart, "va_start", "(ref<va_list>, var_list) -> unit");
	};

	static inline bool isVarList(const core::NodePtr& node) {
		if(auto exp = node.isa<ExpressionPtr>()) return isVarList(exp->getType());
		return node == node->getNodeManager().getLangExtension<VarArgsExtension>().getVarList();
	}
}
}
}

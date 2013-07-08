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


	class StaticVariableExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		StaticVariableExtension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}

	public:

		bool isStaticType(const TypePtr& type) const;

		TypePtr wrapStaticType(const TypePtr& type) const;

		TypePtr unwrapStaticType(const TypePtr& type) const;

		/**
		 * A function ..
		 */
		LANG_EXT_DERIVED(CreateStatic,
				"let type = struct __static_var { bool initialized; 'a value; }  in "
				""
				"(type<'a> t)->type { "
				"	ref<type> res = var(type);"
				"	res->initialized = false;"
				"	return *res;"
				"}"
		);

		/**
		 * A function ..
		 */
		LANG_EXT_DERIVED(InitStatic,
				"let type = struct __static_var { bool initialized; 'a value; }  in "
				""
				"(ref<type> var, 'a value)->unit { "
				"	if (*var->initialized) return;"
				"	var->initialized = true;"
				"	var->value = value;"
				"}"
		);

		/**
		 * A function ..
		 */
		LANG_EXT_DERIVED(AccessStatic,
				"let type = struct __static_var { bool initialized; 'a value; }  in "
				""
				"(ref<type> var)->ref<'a> { "
				"	return var->value;"
				"}"
		);

	};


} // end namespace lang
} // end namespace core
} // end namespace insieme

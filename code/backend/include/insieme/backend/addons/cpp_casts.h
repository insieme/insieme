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

#include "insieme/backend/addon.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/reference.h"

/**
 * This header file defines the components required to be registered within
 * a backend instance to handle C++ casts.
 */
namespace insieme {
namespace backend {
namespace addons {

	/**
	 * A language module defining C++ cast operators within the backend.
	 */
	class CppCastExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		CppCastExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		// import reference extension to utilize aliases
		IMPORT_MODULE(core::lang::ReferenceExtension);

		/**
		 * The literal utilized to represent static C++ casts.
		 */
		LANG_EXT_LITERAL(StaticCast, "static_cast", "(ref<'a,'c,'v>, type<ref<'b,'nc,'nv>>) -> ref<'b,'nc,'nv>")

	    /**
		 * The literal utilized to represent dynamic C++ casts.
		 */
	    LANG_EXT_LITERAL(DynamicCast, "dynamic_cast", "(ref<'a,'c,'v>, type<ref<'b,'nc,'nv>>) -> ref<'b,'nc,'nv>")

	};


	/**
	 * An Add-On realizing support for C++ casts.
	 */
	struct CppCastsAddon : public AddOn {
		/**
		 * Installs the this Add-On within the given converter.
		 */
		virtual void installOn(Converter& converter) const;
	};


} // end namespace addons
} // end namespace backend
} // end namespace insieme

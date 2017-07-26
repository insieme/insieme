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

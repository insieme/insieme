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

#include "insieme/core/expressions.h"

namespace insieme {
namespace backend {
namespace runtime {

	/**
	 * This class offers a list of IR extensions required to model concepts within the
	 * Insieme Runtime. The extensions include literals and types to model work items,
	 * data items and additional runtime functionality.
	 */
	class Extensions {
	public:

		/**
		 * Creates a new instance of this extension set. The given manager is used to construct
		 * the contained literals and types.
		 *
		 * @param manager the manager to be used to construct the required types and literals
		 */
		Extensions(core::NodeManager& manager);

		/**
		 * An invocation to this literal results in a no-op. However, context management routines
		 * like the init / cleanup context functions as well as the type/implementation tables
		 * required by the runtime will be initialized as a side effect. This instruction
		 * will be implanted as the first instruction of the newly generated entry point during
		 * the pre-processing.
		 */
		const core::LiteralPtr initContext;

		/**
		 * The type used internally to represent work items. The type is treated in an abstract
		 * way and its actual implementation is imported via a runtime-include file.
		 */
		const core::TypePtr workItemType;

		/**
		 * The literal used as a wrapper for the work-item creation function within the runtime.
		 */
		const core::LiteralPtr createWorkItem;

		const core::LiteralPtr submitWorkItem;

		const core::LiteralPtr joinWorkItem;
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme

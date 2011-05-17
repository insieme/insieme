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
namespace simple_backend {

	/**
	 * This class offers a list of IR extensions required within the simple backend. Such
	 * extensions include additional literals representing i.g. C operators or procedures of the
	 * runtime interface.
	 */
	class IRExtensions {
	public:

		/**
		 * The name of the global literal introduced by the preprocessor.
		 */
		static const string GLOBAL_ID;

		/**
		 * Creates a new instance of this IRExtension set. The given manager is used to construct
		 * the included literals.
		 *
		 * @param manager the manager to be used to construct the required types and literals
		 */
		IRExtensions(core::NodeManager& manager);

		/**
		 * A special literal representing a lazy-evaluating if-then-else operator.
		 */
		const core::LiteralPtr lazyITE;

		/**
		 * A special literal representing a function causing the initialization of the global variables.
		 */
		const core::LiteralPtr initGlobals;
	};


} // end namespace simple_backend
} // end namespace insieme

/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
namespace frontend {
namespace ir{

		class FrontendIr : public core::lang::Extension {

			bool isCXX;


		public:
			/**
			 * Creates a new instance based on the given node manager.
			 */
			FrontendIr(core::NodeManager& manager, bool cxx)
					: core::lang::Extension(manager), isCXX(cxx) {}
	
			/**
			 * 	frontend assignment operator
			 */
			LANG_EXT_LITERAL_WITH_NAME(RefAssign, "fe_ref_assign", "FE_RefAssign", isCXX? "(ref<'a>, 'a)->ref<'a>":"(ref<'a>, 'a)->'a");
		};


} // namespace ir
} // namespace frontend
} // namespace insieme

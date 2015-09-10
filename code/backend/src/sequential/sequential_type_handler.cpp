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

#include "insieme/backend/sequential/sequential_type_handler.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"

#include "insieme/core/lang/parallel.h"

namespace insieme {
namespace backend {
namespace sequential {

	namespace {

		const TypeInfo* handleType(const Converter& converter, const core::TypePtr& type) {
			// handle jobs
			const core::lang::ParallelExtension& ext = converter.getNodeManager().getLangExtension<core::lang::ParallelExtension>();

			// handle lock types
			if(ext.isLock(type)) { return type_info_utils::createInfo(converter.getFragmentManager(), "int32_t", "stdint.h"); }

			// it is not a special runtime type => let somebody else try
			return 0;
		}
	}

	TypeHandler SequentialTypeHandler = &handleType;

} // end namespace ocl_standalone
} // end namespace backend
} // end namespace insieme

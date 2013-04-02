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
#include "insieme/backend/preprocessor.h"

namespace insieme {
namespace backend {
namespace runtime {

	enum class PickImplementationHint { CALL, SWITCH };
	
	/**
	 * A pre-processor wrapping the entry point of the given code into a newly generated
	 * lambda instantiating and running a standalone version of the insieme runtime.
	 */
	class StandaloneWrapper : public PreProcessor {
	public:
		virtual core::NodePtr process(const backend::Converter& converter, const core::NodePtr& code);
	};

	/**
	 * A pre-processor converting all job expressions, calls to parallel and pfors into runtime
	 * equivalents. After this pass, the resulting program will no longer contain any of those
	 * primitives.
	 *
	 * Yes, the name is a working title ...
	 */
	class WorkItemizer : public PreProcessor {
		bool includeEffortEstimation;
	public:
		WorkItemizer(bool includeEffortEstimation = false) : includeEffortEstimation(includeEffortEstimation) {}
		virtual core::NodePtr process(const backend::Converter& converter, const core::NodePtr& code);
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme

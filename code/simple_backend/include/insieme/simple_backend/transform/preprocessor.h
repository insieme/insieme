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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace simple_backend {
namespace transform {

	/**
	 * An enumeration of optional preprocessing steps.
	 */
	enum PreProcessorSteps {
		INSTANTIATE_POINTWISE				= 1 << 0,
		INSTANTIATE_GENERIC_LAMBDAS 		= 1 << 1,
		INLINE_IF_THEN_ELSE					= 1 << 2,
		RESTORE_GLOBALS						= 1 << 3,
	};


	/**
	 * The default selection of steps => all steps.
	 */
	const int ALL_STEPS = INSTANTIATE_POINTWISE | INSTANTIATE_GENERIC_LAMBDAS | INLINE_IF_THEN_ELSE | RESTORE_GLOBALS;

	/**
	 * This function is applying pre-processing steps on the given code. The pre-processing includes
	 * for instance the introduction of explicit vector -> array casts.
	 *
	 * @param manager the manager used for maintaining the resulting IR DAG
	 * @param code the code fragment to be pre-processed
	 * @param steps allows to select a subset of the preprocessing steps to be conducted
	 * @return the result of the pre-processing step, maintained by the given manager
	 */
	core::NodePtr preprocess(core::NodeManager& manager, const core::NodePtr& code, const int steps = ALL_STEPS);

} // end: namespace transform
} // end: namespace simple_backend
} // end: namespace insieme

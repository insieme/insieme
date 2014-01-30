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

#include <set>

#include "insieme/analysis/cba/framework/context.h"
#include "insieme/analysis/cba/framework/entities/thread_region.h"

#include "insieme/core/forward_decls.h"

#include "insieme/utils/petri_net/petri_net.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * An execution network is a Petri net where:
	 * 		- places are thread phases and channel states
	 * 		- transitions are introduced by
	 * 				- non-deterministic choice
	 * 				- spawn / merge operations
	 * 				- channel operations
	 * 				- redistribute operations
	 */


	template<typename Context>
	class Place {

		/**
		 * A place can either be:
		 * 		- a thread region
		 * 		- a channel buffer
		 * 		- a auxiliary construct for non-deterministic choices
		 */

	};

	template<typename Context>
	class Transition {

		/**
		 * A transition can be:
		 * 		- a spawn
		 * 		- a merge
		 * 		- a redistribute
		 * 		- a channel send / recv operation
		 */
	};

	template<typename Context>
	class ExecutionNet :
			public utils::petri_net::PetriNet<Place<Context>, Transition<Context>>,
			public utils::Printable
	{

	};


	template<typename Context>
	ExecutionNet<Context> buildExecutionNet(const std::set<ThreadRegion<Context>>& regions) {

		ExecutionNet<Context> res;

		return res;
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

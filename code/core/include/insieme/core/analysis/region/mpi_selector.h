/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/analysis/region/region_selector.h"

#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/ir_pattern.h"

namespace insieme {
namespace core {
namespace analysis {
namespace region {

	namespace pt = insieme::core::pattern;

	/**
	 * This region selector is picking all MPI communication constructs.
	 */
	class MPISelector : public RegionSelector {
	  private:
		const bool matchRequestObjs;

	  public:
		MPISelector(bool matchRequestObjects = false) : matchRequestObjs(matchRequestObjects) {}

		/*
		 * Returns a pattern matching MPI_Isend and MPI_Ireceive calls
		 * @param mpiRequest a specific MPI request object for async calls to be matched against (any by default)
		 */
		pt::TreePattern getMPIAsyncTransport(pt::TreePattern mpiRequest = pt::any) const;

		/*
		* Returns a pattern matching MPI_Send and MPI_Receive calls
		*/
		pt::TreePattern getMPISyncTransport() const;

		/*
		* Returns a pattern matching MPI_Test calls
		* @param mpiRequest a specific MPI request object to be matched against (any by default)
		*/
		pt::TreePattern getMPITest(pt::TreePattern mpiRequest = pt::any) const;

		/*
		* Returns a pattern matching MPI_Wait calls
		* @param mpiRequest a specific MPI request object to be matched against (any by default)
		*/
		pt::TreePattern getMPIWait(pt::TreePattern mpiRequest = pt::any) const;

		/*
		* Returns a pattern matching while loops containing MPI_Test calls
		* @param mpiRequest a specific MPI request object for async calls to be matched against (any by default)
		*/
		pt::TreePattern getMPIWhileTest(pt::TreePattern mpiRequest = pt::any) const;

		/*
		* Returns a pattern matching an asynchronous MPI communication operation containing MPI_Isend or MPI_Ireceive primitives followed by
		* either MPI_Wait or a while loop holding MPI_Test
		* @param matchRequestObjects require that MPI request objects of communication calls match
		*/
		pt::ListPattern getMPIAsyncPattern(bool matchRequestObjects) const;

		/*
		* Returns a pattern matching a synchronous MPI communication operation containing MPI_Send or MPI_Receive primitives
		*/
		pt::TreePattern getMPISyncPattern() const;

		/*
		* Returns a pattern matching a synchronous or asynchronous MPI communication operation containing MPI primitives
		* @param matchRequestObjects require that MPI request objects of asynchronous communication calls match
		*/
		pt::ListPattern getMPIPattern(bool matchRequestObjects) const;

		/**
		 * Selects all MPI regions within the given code fragment.
		 */
		virtual RegionList getRegions(const core::NodeAddress& code) const;
	};

} // end namespace region
} // end namespace analysis
} // end namespace core
} // end namespace insieme

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

namespace insieme {
namespace core {
namespace analysis {
namespace region {

	/**
	 * This region selector is picking regions based on a estimated computation
	 * cost model.
	 */
	class SizeBasedRegionSelector : public RegionSelector {
		/**
		 * The lower limit for the cost of a code fragment to be classified
		 * as a region.
		 */
		unsigned minSize;

		/**
		 * The upper bound for the estimated costs a code fragment can have
		 * to be classified as a region.
		 */
		unsigned maxSize;

	  public:
		/**
		 * Creates a new selector identifying regions having a estimated execution
		 * cost within the given boundaries.
		 */
		SizeBasedRegionSelector(unsigned minSize, unsigned maxSize) : minSize(minSize), maxSize(maxSize) {}

		/**
		 * Selects all regions within the given code fragment.
		 */
		virtual RegionList getRegions(const core::NodeAddress& code) const;
	};

} // end namespace region
} // end namespace analysis
} // end namespace core
} // end namespace insieme

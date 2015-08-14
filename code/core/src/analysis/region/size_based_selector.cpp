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

#include "insieme/core/analysis/region/size_based_selector.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/parallel.h"

#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace core {
namespace analysis {
namespace region {

	namespace {

		/**
		 * The actual calculator computing the size of regions.
		 */
		class SizeCalculator {
			utils::cache::PointerCache<core::NodePtr, unsigned> cache;

		  public:
			SizeCalculator() : cache(fun(*this, &SizeCalculator::calcRegionSize)) {}

			unsigned estimateSize(const core::NodePtr& node) {
				if(node->getNodeCategory() == core::NC_Type) { return 0; }
				return cache.get(node);
			}

		  private:
			unsigned calcRegionSize(const core::NodePtr& node) {
				if(node->getNodeCategory() == core::NC_Type) { return 0; }

				// estimate size
				unsigned size = 1;
				for_each(node->getChildList(), [&](const core::NodePtr& child) { size += estimateSize(child); });

				// multiply with number of iterations (loops count twice)
				unsigned mul = 1;
				auto t = node->getNodeType();
				auto& b = node->getNodeManager().getLangExtension<lang::ParallelExtension>();
				if(t == core::NT_ForStmt || t == core::NT_WhileStmt) { mul = 2; }
				if(core::analysis::isCallOf(node, b.getPFor())) { mul = 2; }

				return size * mul;
			}
		};
	}

	RegionList SizeBasedRegionSelector::getRegions(const core::NodePtr& node) const {
		RegionList regions;

		SizeCalculator calculator;
		visitDepthFirstPrunable(core::NodeAddress(node), [&](const core::CompoundStmtAddress& comp) {
			unsigned size = calculator.estimateSize(comp.getAddressedNode());
			if(minSize < size && size < maxSize) {
				regions.push_back(comp);
				return true;
			}
			return false;
		});
		return regions;
	}

} // end namespace region
} // end namespace analysis
} // end namespace core
} // end namespace insieme

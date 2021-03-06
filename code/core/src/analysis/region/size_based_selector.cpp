/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

	RegionList SizeBasedRegionSelector::getRegions(const core::NodeAddress& code) const {
		RegionList regions;

		SizeCalculator calculator;
		visitDepthFirstPrunable(code, [&](const core::CompoundStmtAddress& comp) {
			unsigned size = calculator.estimateSize(comp.getAddressedNode());
			if(minSize < size && size < maxSize) {
				regions.push_back(Region(comp));
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

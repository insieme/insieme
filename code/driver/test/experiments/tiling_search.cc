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

#include <gtest/gtest.h>

#include "insieme/frontend/frontend.h"
#include "insieme/analysis/features/cache_features.h"

#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/polyhedral/transformations.h"
#include "insieme/transform/filter/filter.h"

#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/logging.h"

#include "insieme/core/printer/pretty_printer.h"


namespace insieme {

using namespace analysis::features;
using namespace utils::test;
using namespace transform;
using namespace transform::pattern;
using namespace transform::polyhedral;
using namespace core;
using namespace core::printer;


	TEST(TilingSearch, Dummy) {
		// no test-case is not supported
	}

	// -------- testing tiling and cache models -----------------

//	TEST(CacheOptimizerTest, Jacobi) {
//		Logger::setLevel(ERROR);
//
//		NodeManager manager;
//
//		// load test case
//		ProgramPtr prog = load(manager, "simple_jacobi");
//		ASSERT_TRUE(prog);
//
//		// find second-innermost loops to be tiled
//		auto secondInnermost = filter::allMatches(transform::pattern::irp::innerMostForLoopNest(2));
//
//		vector<NodeAddress> targets = secondInnermost(prog);
//		EXPECT_EQ(2u, targets.size());
//
//		for (unsigned i=0; i<targets.size(); i++) {
//
//			NodeAddress target = targets[i];
//			std::cout << "Processing Target: \n" << PrettyPrinter(target) << "\n";
//
//			try {
//
//				// try whether loop tiling can be applied
//				makeLoopTiling(4,4)->apply(target);
//
//			} catch (const InvalidTargetException& ite) {
//				std::cout << "Tiling cannot be applied!\n";
//				continue;
//			}
//
//			// compute cache misses
//			LRUCacheModel<64,64,8> L1;
//			LRUCacheModel<64,512,8> L2;
//			LRUCacheModel<64,4096,16> L3;
//
////			MultiLevelCache<
////				LRUCacheModel<64,64,8>,			// L1 cache
////				LRUCacheModel<64,512,8>,		// L2 cache
////				LRUCacheModel<64,4096,16>		// L3 cache
////			> all;
//
//			MultiLevelCache<
//				LRUCacheModel<64,512,2>,		// L1 cache
//				LRUCacheModel<64,512,16>,		// L2 cache
//				LRUCacheModel<64,1706,48>		// L3 cache
//			> all;
//
//			L1.eval(target);
//			std::cout << "L1 misses: " << L1.getFeatureValue() << " - " << L1.getMissRatio() << "\n";
//
//			L2.eval(target);
//			std::cout << "L2 misses: " << L2.getFeatureValue() << " - " << L2.getMissRatio() << "\n";
//
//			L3.eval(target);
//			std::cout << "L3 misses: " << L3.getFeatureValue() << " - " << L3.getMissRatio() << "\n";
//
//			all.eval(target);
//			std::cout << "Cache uses: " << all.getFeatureValue() << "\n";
//
//			CacheModel& model = all;
//			for(int i=1; i<=32; i<<=1) {
//				for (int j=1; j<=32; j<<=1) {
//					std::cout << format("(%2d,%2d)", i, j);
//					std::cout << " - " << model.eval(makeLoopTiling(i,j)->apply(target));
////					std::cout << " - " << model.getMissRatio();
////					std::cout << " - " << model.getAccesses();
//					std::cout << "\n";
//				}
//			}
//		}
//	}
//
//	TEST(CacheOptimizerTest, MatrixMultiplication) {
//		Logger::setLevel(ERROR);
//
//		NodeManager manager;
//
//		// load test case
//		ProgramPtr prog = load(manager, "matrix_mul_static");
//		ASSERT_TRUE(prog);
//
////		std::cout << "Pattern 1: \n" << transform::pattern::irp::innerMostForLoopNest(1) << "\n\n";
////		std::cout << "Pattern 2: \n" << transform::pattern::irp::innerMostForLoopNest(2) << "\n\n";
////		std::cout << "Pattern 3: \n" << transform::pattern::irp::innerMostForLoopNest(3) << "\n\n";
//
//		// find second-innermost loops to be tiled
//		auto targetFilter = filter::allMatches(transform::pattern::irp::innerMostForLoopNest(3));
//
//		vector<NodeAddress> targets = targetFilter(prog);
//		EXPECT_EQ(1u, targets.size());
//
//		NodeAddress target = targets[0];
//		std::cout << "Processing Target: \n" << PrettyPrinter(target) << "\n";
//
//		try {
//
//			// try whether loop tiling can be applied
//			makeLoopTiling(4,4)->apply(target);
//
//		} catch (const InvalidTargetException& ite) {
//			std::cout << "Tiling cannot be applied!\n";
//			return;
//		}
//
//		// compute cache misses
//		LRUCacheModel<64,64,8> L1;
//		LRUCacheModel<64,512,8> L2;
//		LRUCacheModel<64,4096,16> L3;
//
//		MultiLevelCache<
//			LRUCacheModel<64,64,8>,			// L1 cache
//			LRUCacheModel<64,512,8>,		// L2 cache
//			LRUCacheModel<64,4096,16>		// L3 cache
//		> all;
//
//		L1.eval(target);
//		std::cout << "L1 misses: " << L1.getFeatureValue() << " - " << L1.getMissRatio() << "\n";
//
//		L2.eval(target);
//		std::cout << "L2 misses: " << L2.getFeatureValue() << " - " << L2.getMissRatio() << "\n";
//
//		L3.eval(target);
//		std::cout << "L3 misses: " << L3.getFeatureValue() << " - " << L3.getMissRatio() << "\n";
//
//		all.eval(target);
//		std::cout << "Cache uses: " << all.getFeatureValue() << "\n";
//
//		double bestRes = 1;
//		int bestI = -1;
//		int bestJ = -1;
//
//		for(int i=1; i<=32; i<<=1) {
//			for (int j=1; j<=32; j<<=1) {
//				std::cout << format("(%2d,%2d)", i, j);
//				std::cout << " - " << L1.eval(makeLoopTiling(i,j)->apply(target));
//				std::cout << " - " << L1.getAccesses();
//				std::cout << " - " << L1.getMissRatio();
//				std::cout << "\n";
//
//				if (L1.getMissRatio() < bestRes) {
//					bestRes = L1.getMissRatio();
//					bestI = i; bestJ = j;
//				}
//			}
//		}
//
//		std::cout << "Best Tile-Size: " << format("(%2d,%2d)", bestI, bestJ) << "\n";
//	}

}

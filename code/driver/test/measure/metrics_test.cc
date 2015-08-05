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

#include <gtest/gtest.h>

#include <cstdlib>
#include <future>

#include <boost/filesystem.hpp>

#include "insieme/driver/measure/measure.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace driver {
namespace measure {

using namespace std;
using namespace core;

TEST(Measuring, Metrics) {
	// play a little using units
	auto time = Metric::CPU_TIME;
	
	EXPECT_EQ(time, Metric::CPU_TIME);
	
	EXPECT_EQ("cpu_time", toString(time));
	
	stringstream stream;
	stream << Metric::CPU_TIME->getName() << "(" << *(Metric::CPU_TIME->getUnit()) << ")";
	
	EXPECT_EQ(Metric::CPU_TIME, Metric::getForNameAndUnit(stream.str()));
}

TEST(Measuring, MetricsDependencies) {
	// test some parameter without dependency
	EXPECT_TRUE(Metric::PAPI_L1_DCM->getDependencies().empty());
	
	// check something with a single dependency
	//std::set<MetricPtr> dep;
	//dep.insert(Metric::PAPI_L3_TCM);
	//EXPECT_EQ(dep, Metric::TOTAL_L3_CACHE_MISS->getDependencies());
	
}


TEST(Measuring, MetricsDependencyClosure) {
	// the set of dependencies to compare with
	std::set<MetricPtr> dep;
	
	// check empty list
	EXPECT_EQ(dep, getDependencyClosureLeafs(toVector<MetricPtr>()));
	
	// check a leaf metric itself
	dep.insert(Metric::PAPI_L3_TCM);
	EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::PAPI_L3_TCM)));
	
	// check a derived metric
	//EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::TOTAL_L3_CACHE_MISS)));
	
	// check multiple metrics
	dep.insert(Metric::PAPI_L3_TCM);
	dep.insert(Metric::CPU_TIME);
	EXPECT_EQ(dep, getDependencyClosureLeafs(toVector(Metric::CPU_TIME, Metric::PAPI_L3_TCM)));
	
}

} // end namespace measure
} // end namespace driver
} // end namespace insieme

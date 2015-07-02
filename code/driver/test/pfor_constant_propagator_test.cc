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

#include "insieme/core/transform/sequentialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/inline.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/test/test_utils.h"

#include "insieme/driver/integration/tests.h"

#include "insieme/analysis/region/pfor_selector.h"

#include "insieme/transform/pfor_constant_propagator.h"


namespace insieme {
namespace driver {

using namespace insieme::core;

	TEST(PForConstantPropagator, Simple) {
		namespace icp = insieme::core::pattern;

		NodeManager mgr;
		IRBuilder builder(mgr);

		ProgramPtr program = insieme::driver::integration::loadIntegrationTest(mgr, "omp/simple_pfor", true);

		ASSERT_TRUE(program);

		icp::TreePattern forWithVariableStep = icp::irp::forStmt(icp::any, icp::any, icp::any, icp::irp::variable(), icp::any);
		icp::TreePattern forWithConstantStep = icp::irp::forStmt(icp::any, icp::any, icp::any, icp::irp::literal(), icp::any);

		auto resVar = icp::irp::collectAll(forWithVariableStep, program, false);
		auto resCon = icp::irp::collectAll(forWithConstantStep, program, false);
		ASSERT_EQ(1, resVar.size());
		EXPECT_TRUE(resVar.front());
		ASSERT_EQ(1, resCon.size());
		EXPECT_TRUE(resCon.front());

		insieme::transform::PForConstantPropagator propagator;

		const auto newProgram = propagator.map(program);

		ASSERT_TRUE(newProgram);

		auto resVar2 = icp::irp::collectAll(forWithVariableStep, newProgram, false);
		auto resCon2 = icp::irp::collectAll(forWithConstantStep, newProgram, false);
		ASSERT_EQ(0, resVar2.size());
		ASSERT_EQ(2, resCon2.size());
		EXPECT_TRUE(resCon2.front());
	}
	
	
} // end namespace driver
} // end namespace insieme




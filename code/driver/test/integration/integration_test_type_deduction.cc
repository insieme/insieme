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
#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/driver/driver_config.h"

#include "integration_tests.inc"

namespace insieme {

	// ---------------------------------- Check the type checker -------------------------------------

	// the type definition (specifying the parameter type)
	class TypeVariableDeductionTest : public ::testing::TestWithParam<IntegrationTestCase> { };

	// define the test case pattern
	TEST_P(TypeVariableDeductionTest, DeriveTypes) {
		core::NodeManager manager;

		// obtain test case
		utils::test::IntegrationTestCase testCase = GetParam();
		SCOPED_TRACE("Testing Case: " + testCase.getName());
		LOG(INFO) << "Testing Case: " + testCase.getName();
	
		// load the code using the frontend
		core::ProgramPtr code = load(manager, testCase);

		// and now, apply the check and see whether a solution could be found
		core::visitDepthFirstOnce(code, [&](const core::CallExprPtr& call){
			EXPECT_TRUE(types::getTypeVariableInstantiation(manager, call))
	//					<< "Processing:     " << core::printer::PrettyPrinter(call) << "\n"
					<< "FunctionType:   " << *(call->getFunctionExpr()->getType()) << "\n"
					<< "Argument Types: " << getTypes(call->getArguments());
		});
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(TypeVariableDeductionCheck, TypeVariableDeductionTest, ::testing::ValuesIn(getAllCases()));

}

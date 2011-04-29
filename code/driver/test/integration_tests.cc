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

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/analysis/type_variable_deduction.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/test/test_utils.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"

namespace insieme {

using namespace utils::test;
using namespace core;


// --------------- Some global utilities for this test program --------------------

namespace {

	// a node manager bound to the life cycle of the entire program
	NodeManager testGlobalManager;

	// a cache for already loaded Integreation tests
	std::map<IntegrationTestCase, ProgramPtr> loadedCodes;

	// a helper method for loading program code
	ProgramPtr load(NodeManager& manager, const IntegrationTestCase& testCase) {

		// check whether the code is already in the cache
		auto pos = loadedCodes.find(testCase);
		if (pos != loadedCodes.end()) {
			return manager.get(pos->second);
		}

		// not loaded yet => load and cache code
		insieme::utils::Timer timer("Loading TestCase " + testCase.getName());
		core::ProgramPtr code = frontend::ConversionJob(testGlobalManager, testCase.getFiles(), testCase.getIncludeDirs()).execute();
		timer.stop();
		std::cout << timer;

		loadedCodes.insert(std::make_pair(testCase, code));
		return manager.get(code);
	}

}


inline vector<TypePtr> getTypes(const vector<ExpressionPtr>& expressions) {
	vector<TypePtr> res;
	::transform(expressions, std::back_inserter(res), [](const ExpressionPtr& cur) { return cur->getType(); });
	return res;
}

// the type definition (specifying the parameter type)
class TypeVariableDeductionTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(TypeVariableDeductionTest, DeriveTypes) {
	core::NodeManager manager;
//	core::NodeManager& manager = testGlobalManager;

	// disable logger output
	Logger::get(std::cerr, ERROR, 0);

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();


	// load the code using the frontend
	std::cout << "Loading the first time ..." << std::endl;
	insieme::utils::Timer firstLoad("First Load ...");
	core::ProgramPtr code = load(manager, testCase);
	firstLoad.stop();
	std::cout << firstLoad;

	std::cout << "Loading the second time ..." << std::endl;
	insieme::utils::Timer secondLoad("Second Load ...");
	core::ProgramPtr code2 = load(manager, testCase);
	secondLoad.stop();
	std::cout << secondLoad;

	NodeManager manager2;
	std::cout << "Copying tree from one manager to another ..." << std::endl;
	insieme::utils::Timer copyTimer("Copying between Manager ...");
	core::ProgramPtr code3 = manager2.get(code2);
	copyTimer.stop();
	std::cout << copyTimer;


	// check presents ...
	std::cout << "Looking up same tree of different manager ..." << std::endl;
	insieme::utils::Timer lookup1("Lookup non-local ...");
	core::ProgramPtr code4 = manager2.get(code2);
	lookup1.stop();
	std::cout << lookup1;

	std::cout << "Looking up same tree of same manager ..." << std::endl;
	insieme::utils::Timer lookup2("Lookup local ...");
	core::ProgramPtr code5 = manager2.get(code3);
	lookup2.stop();
	std::cout << lookup2;


	std::cout << "Done" << std::endl;

//	// and now, apply the check
//	core::visitAll(code, core::makeLambdaPtrVisitor([&](const NodePtr& cur){
//		if (cur->getNodeType() == NT_CallExpr) {
//			CallExprPtr call = static_pointer_cast<const CallExpr>(cur);
//			EXPECT_TRUE(analysis::getTypeVariableInstantiation(manager, call))
////					<< "Processing:     " << core::printer::PrettyPrinter(call) << "\n"
//					<< "FunctionType:   " << *(call->getFunctionExpr()->getType()) << "\n"
//					<< "Argument Types: " << getTypes(call->getArguments());
//
//		}
//	}, false));


}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(TypeVariableDeductionCheck, TypeVariableDeductionTest, ::testing::ValuesIn(getAllCases()));

}

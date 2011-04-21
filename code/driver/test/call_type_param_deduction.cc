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

#include "insieme/utils/test/test_utils.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"

namespace insieme {

using namespace utils::test;
using namespace core;

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

	// disable logger output
	Logger::get(std::cerr, ERROR, 0);

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();

	// load the code using the frontend
	core::ProgramPtr code = frontend::ConversionJob(manager, testCase.getFiles(), testCase.getIncludeDirs()).execute();

	// and now, apply the check
	core::visitAll(code, core::makeLambdaPtrVisitor([&](const NodePtr& cur){
		if (cur->getNodeType() == NT_CallExpr) {
			CallExprPtr call = static_pointer_cast<const CallExpr>(cur);
			EXPECT_TRUE(analysis::getTypeVariableInstantiation(manager, call))
//					<< "Processing:     " << core::printer::PrettyPrinter(call) << "\n"
					<< "FunctionType:   " << *(call->getFunctionExpr()->getType()) << "\n"
					<< "Argument Types: " << getTypes(call->getArguments());

		}
	}, false));


}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(TypeVariableDeductionCheck, TypeVariableDeductionTest, ::testing::ValuesIn(getAllCases()));

}

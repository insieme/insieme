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
#include "insieme/core/analysis/type_variable_deduction.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/ir_check.h"

#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/typechecks.h"
#include "insieme/core/checks/imperativechecks.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/text_dump.h"

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
	// ---------------------------------- Check transfomations -------------------------------------

//	// the type definition (specifying the parameter type)
//	class TransformationIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };
//
//	// define the test case pattern
//	TEST_P(TransformationIntegrationTest, SemanticChecks) {
//		core::NodeManager mgr;
//
//		// obtain test case
//		utils::test::IntegrationTestCase testCase = GetParam();
//		SCOPED_TRACE("Testing Case: " + testCase.getName());
//
//		// load the code using the frontend
//		core::ProgramPtr code = load(mgr, testCase);
//
//		// find array declarations
//		auto& basic = mgr.basic;
//		IRBuilder builder(mgr);
//
//		// search for array variable declarations
//		//visitDepthFirstInterruptible(NodeAddress(code), [&](const DeclarationStmtAddress& curdecl) -> bool {
//		//	auto var = curdecl->getVariable();
//		//	auto type = var->getType();
//		//	auto init = curdecl->getInitialization();
//
//		//	unsigned numRefs = 0;
//		//	while(auto refType = dynamic_pointer_cast<const RefType>(type)) {
//		//		type = refType->getElementType();
//		//		numRefs++;
//		//	}
//
//		//	if(type->getNodeType() == NT_ArrayType) {
//		//		LOG(INFO) << "**************************************\n====\narray (" << numRefs
//		//			<< " refs) initialized using:\n" << printer::PrettyPrinter(init) << "\n*********************\n";
//		//	}
//
//		//	return false;
//		//});
//
//		// search for calls to scalar.to.array
//		visitDepthFirstInterruptible(NodeAddress(code), [&](const CallExprAddress& curcall) -> bool {
//			for(int argIndex = 0; argIndex < curcall->getArguments().size(); ++argIndex) {
//				if(CallExprPtr convertcall = dynamic_pointer_cast<const CallExpr>(curcall->getArgument(argIndex))) {
//					if(basic.isScalarToArray(convertcall->getFunctionExpr())) {
//						if(LambdaExprPtr called = dynamic_pointer_cast<const LambdaExpr>(curcall->getFunctionExpr())) {
//							VariablePtr param = called->getParameterList()[argIndex];
//							LOG(INFO) << "**************************************\n====\nparam:\n " << printer::PrettyPrinter(param) << "\n*********************\n";
//							visitDepthFirstInterruptible(NodeAddress(called->getBody()), [&](const VariableAddress& var) {
//								if(var.getAddressedNode() == param) {
//									if(CallExprPtr usecall = dynamic_pointer_cast<const CallExpr>(var.getParentNode())) {
//										if(basic.isArrayRefElem1D(usecall->getFunctionExpr())) {
//											try {
//												auto formula = arithmetic::toFormula(usecall->getArgument(1));
//												if(formula.isZero()) {
//													//LOG(INFO) << "- used in array.ref.elem.1D: OK";
//												} else {
//													LOG(INFO) << "- used in array.ref.elem.1D with: " << formula;
//													LOG(INFO) << "- context:\n" << printer::PrettyPrinter(called);
//												}
//											} catch(arithmetic::NotAFormulaException e) {
//												LOG(INFO) << "- used in array.ref.elem.1D with non-formula: " << usecall;
//											}
//										} else {
//											LOG(INFO) << "- used in unexpected call: " << usecall;
//										}
//									} else {
//										LOG(INFO) << "****\n- used in non-call: " << printer::PrettyPrinter(var.getParentNode());
//									}
//								}
//								return false;
//							});
//						}
//					}
//				}
//			}
//			return false;
//		});
//
//	}
//
//	// instantiate the test case
//	INSTANTIATE_TEST_CASE_P(TransformationIntegrationCheck, TransformationIntegrationTest, ::testing::ValuesIn(getAllCases()));

	TEST(TransformationIntegrationTest, Dummy) {
		// empty test cases may lead to problems
	}
}

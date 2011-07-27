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

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/analysis/type_variable_deduction.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ast_builder.h"

#include "insieme/core/ast_check.h"

#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/typechecks.h"
#include "insieme/core/checks/imperativechecks.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/simple_backend/simple_backend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"


#ifdef USE_XML
#include "insieme/xml/xml_utils.h"
#endif

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
		core::ProgramPtr code = frontend::ConversionJob(testGlobalManager, testCase.getFiles(), testCase.getIncludeDirs()).execute();

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
	Logger::get(std::cerr, DEBUG, 1);

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();


//	// load the code using the frontend
//	std::cout << "Loading the first time ..." << std::endl;
//	insieme::utils::Timer firstLoad("First Load ...");
//	core::ProgramPtr code = load(manager, testCase);
//	firstLoad.stop();
//	std::cout << firstLoad;
//
//	std::cout << "Loading the second time ..." << std::endl;
//	insieme::utils::Timer secondLoad("Second Load ...");
//	core::ProgramPtr code2 = load(manager, testCase);
//	secondLoad.stop();
//	std::cout << secondLoad;
//
//	NodeManager manager2;
//	std::cout << "Copying tree from one manager to another ..." << std::endl;
//	insieme::utils::Timer copyTimer("Copying between Manager ...");
//	core::ProgramPtr code3 = manager2.get(code2);
//	copyTimer.stop();
//	std::cout << copyTimer;
//
//
//	// check presents ...
//	std::cout << "Looking up same tree of different manager ..." << std::endl;
//	insieme::utils::Timer lookup1("Lookup non-local ...");
//	core::ProgramPtr code4 = manager2.get(code2);
//	lookup1.stop();
//	std::cout << lookup1;
//
//	std::cout << "Looking up same tree of same manager ..." << std::endl;
//	insieme::utils::Timer lookup2("Lookup local ...");
//	core::ProgramPtr code5 = manager2.get(code3);
//	lookup2.stop();
//	std::cout << lookup2;
//
//
//	std::cout << "Done" << std::endl;

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
//INSTANTIATE_TEST_CASE_P(TypeVariableDeductionCheck, TypeVariableDeductionTest, ::testing::ValuesIn(getAllCases()));


// ---------------------------------- Check the frontend -------------------------------------

// the type definition (specifying the parameter type)
class FrontendIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(FrontendIntegrationTest, SemanticChecks) {
	core::NodeManager manager;
	
	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();
	SCOPED_TRACE("Testing Case: " + testCase.getName());

	// load the code using the frontend
	core::ProgramPtr code = load(manager, testCase);

	// run semantic checks on loaded program
	auto errors = core::check(code, core::checks::getFullCheck());

	EXPECT_EQ(static_cast<std::size_t>(0), errors.size());
	if (!errors.empty()) {
		for_each(errors, [](const Message& cur) {
			LOG(INFO) << cur;
			NodeAddress address = cur.getAddress();
			std::stringstream ss;
			unsigned contextSize = 1;
			do {
				ss.str("");
				ss.clear();

				unsigned up = contextSize;
				if (contextSize > address.getDepth()) {
					up = address.getDepth();
				}
				NodePtr context = address.getParentNode(up);
				ss << insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 1+2*contextSize);
			} while(ss.str().length() < 50 && contextSize++ < 5);
			LOG(INFO) << "\t Context: " << ss.str() << std::endl;
		});
	}

}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(FrontendIntegrationCheck, FrontendIntegrationTest, ::testing::ValuesIn(getAllCases()));

// ---------------------------------- Check transfomations -------------------------------------

//// the type definition (specifying the parameter type)
//class TransformationIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };
//
//// define the test case pattern
//TEST_P(TransformationIntegrationTest, SemanticChecks) {
//	core::NodeManager mgr;
//
//	// obtain test case
//	utils::test::IntegrationTestCase testCase = GetParam();
//	SCOPED_TRACE("Testing Case: " + testCase.getName());
//
//	// load the code using the frontend
//	core::ProgramPtr code = load(mgr, testCase);
//
//	// find array declarations
//	auto& basic = mgr.basic;
//	ASTBuilder builder(mgr);
//
//	// search for array variable declarations
//	//visitDepthFirstInterruptable(NodeAddress(code), [&](const DeclarationStmtAddress& curdecl) -> bool {
//	//	auto var = curdecl->getVariable();
//	//	auto type = var->getType();
//	//	auto init = curdecl->getInitialization();
//
//	//	unsigned numRefs = 0;
//	//	while(auto refType = dynamic_pointer_cast<const RefType>(type)) {
//	//		type = refType->getElementType();
//	//		numRefs++;
//	//	}
//
//	//	if(type->getNodeType() == NT_ArrayType) {
//	//		LOG(INFO) << "**************************************\n====\narray (" << numRefs 
//	//			<< " refs) initialized using:\n" << printer::PrettyPrinter(init) << "\n*********************\n";
//	//	}
//
//	//	return false;
//	//});
//
//	// search for calls to scalar.to.array
//	visitDepthFirstInterruptable(NodeAddress(code), [&](const CallExprAddress& curcall) -> bool {
//		for(int argIndex = 0; argIndex < curcall->getArguments().size(); ++argIndex) {
//			if(CallExprPtr convertcall = dynamic_pointer_cast<const CallExpr>(curcall->getArgument(argIndex))) {
//				if(basic.isScalarToArray(convertcall->getFunctionExpr())) {
//					if(LambdaExprPtr called = dynamic_pointer_cast<const LambdaExpr>(curcall->getFunctionExpr())) {
//						VariablePtr param = called->getParameterList()[argIndex];
//						LOG(INFO) << "**************************************\n====\nparam:\n " << printer::PrettyPrinter(param) << "\n*********************\n";
//						visitDepthFirstInterruptable(NodeAddress(called->getBody()), [&](const VariableAddress& var) {
//							if(var.getAddressedNode() == param) {
//								if(CallExprPtr usecall = dynamic_pointer_cast<const CallExpr>(var.getParentNode())) {
//									if(basic.isArrayRefElem1D(usecall->getFunctionExpr())) {
//										try {
//											auto formula = arithmetic::toFormula(usecall->getArgument(1));
//											if(formula.isZero()) {
//												//LOG(INFO) << "- used in array.ref.elem.1D: OK";
//											} else {
//												LOG(INFO) << "- used in array.ref.elem.1D with: " << formula;
//												LOG(INFO) << "- context:\n" << printer::PrettyPrinter(called);
//											}
//										} catch(arithmetic::NotAFormulaException e) {
//											LOG(INFO) << "- used in array.ref.elem.1D with non-formula: " << usecall;
//										}
//									} else {
//										LOG(INFO) << "- used in unexpected call: " << usecall;
//									}
//								} else {
//									LOG(INFO) << "****\n- used in non-call: " << printer::PrettyPrinter(var.getParentNode());
//								}
//							}
//							return false;
//						});
//					}
//				}
//			}
//		}
//		return false;
//	});
//
//}
//
//// instantiate the test case
//INSTANTIATE_TEST_CASE_P(TransformationIntegrationCheck, TransformationIntegrationTest, ::testing::ValuesIn(getAllCases()));


// ---------------------------------- Check the simple backend -------------------------------------

// the type definition (specifying the parameter type)
class SimpleBackendIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(SimpleBackendIntegrationTest, CompileableCode) {
	core::NodeManager manager;

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();

	SCOPED_TRACE("Testing Case: " + testCase.getName());

	// load the code using the frontend
	core::ProgramPtr code = load(manager, testCase);

	// create target code using simple backend
	auto target = simple_backend::SimpleBackend::getDefault()->convert(code);

	// test whether result can be compiled
	auto compiler = utils::compiler::Compiler::getDefaultC99Compiler();
	compiler.addFlag("-I/home/herbert/insieme/code/simple_backend/include/insieme/simple_backend/runtime");
	EXPECT_TRUE(utils::compiler::compile(*target, compiler));
}

// instantiate the test case
//INSTANTIATE_TEST_CASE_P(SimpleBackendIntegrationCheck, SimpleBackendIntegrationTest, ::testing::ValuesIn(getAllCases()));


//// ------------------------------------ Semantic Checks -------------------------------------------
//
//// the type definition (specifying the parameter type)
//class SemanticCheckPerformanceTest : public ::testing::TestWithParam<IntegrationTestCase> { };
//
//// define the test case pattern
//TEST_P(SemanticCheckPerformanceTest, AllTests) {
//	core::NodeManager manager;
//
//	// disable logger output
//	Logger::get(std::cerr, ERROR, 0);
//
//	// obtain test case
//	utils::test::IntegrationTestCase testCase = GetParam();
//
//	// get program code
//	core::ProgramPtr program = load(manager, testCase);
//
//	core::CheckPtr check = core::checks::getFullCheck();
//
//	vector<std::pair<string, core::CheckPtr>> checks;
//	checks.push_back(std::make_pair("All", core::checks::getFullCheck()));
//	checks.push_back(std::make_pair("Keyword", makeVisitOnce(core::make_check<core::checks::KeywordCheck>())));
//	checks.push_back(std::make_pair("CallExprType", makeVisitOnce(core::make_check<core::checks::CallExprTypeCheck>())));
//	checks.push_back(std::make_pair("FunctionType", makeVisitOnce(core::make_check<core::checks::FunctionTypeCheck>())));
//	checks.push_back(std::make_pair("ReturnType", makeVisitOnce(core::make_check<core::checks::ReturnTypeCheck>())));
//	checks.push_back(std::make_pair("DeclarationStmtType", makeVisitOnce(core::make_check<core::checks::DeclarationStmtTypeCheck>())));
//
//	checks.push_back(std::make_pair("UndeclaredVariable", makeVisitOnce(core::make_check<core::checks::UndeclaredVariableCheck>())));
//	checks.push_back(std::make_pair("DeclaredOnce", core::make_check<core::checks::DeclaredOnceCheck>()));
//
//
//
//	for(auto it = checks.begin(); it != checks.end(); ++it ) {
//		// run all checks
//		insieme::utils::Timer checkTime("Running check " + it->first + " ...");
//		core::check(program, it->second);
//		checkTime.stop();
//		std::cout << checkTime;
//	}
//
//}
//
//// instantiate the test case
//INSTANTIATE_TEST_CASE_P(SemanticCheckPerformanceCheck, SemanticCheckPerformanceTest, ::testing::ValuesIn(getAllCases()));


#ifdef USE_XML

// ---------------------------------- Check the XML dump -------------------------------------

// the type definition (specifying the parameter type)
class XMLIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(XMLIntegrationTest, WriteReadTest) {
	core::NodeManager manager;

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();

	SCOPED_TRACE("Testing Case: " + testCase.getName());

	// load the code using the frontend
	core::ProgramPtr code = load(manager, testCase);

	// conduct XML conversion
	xml::XmlUtil xml;
	xml.convertIrToDom(code);
	string res = xml.convertDomToString();

	NodeManager manager2;
	xml::XmlUtil xml2;
	xml.convertStringToDom(res, true);
	core::NodePtr code2 = xml.convertDomToIr(manager2);

	EXPECT_EQ(*code, *code2);
	EXPECT_NE(code, code2);
//	EXPECT_TRUE(core::equalsWithAnnotations(code, code2));

}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(XMLIntegrationCheck, XMLIntegrationTest, ::testing::ValuesIn(getAllCases()));

#endif


}

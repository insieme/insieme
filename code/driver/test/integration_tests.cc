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
#include "insieme/simple_backend/simple_backend.h"

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/test/integration_tests.h"
#include "insieme/utils/cmd_line_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/driver/driver_config.h"


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
		frontend::ConversionJob job(testGlobalManager, testCase.getFiles(), testCase.getIncludeDirs());
		job.setOption(frontend::ConversionJob::OpenMP, testCase.isEnableOpenMP());
		core::ProgramPtr code = job.execute();

		loadedCodes.insert(std::make_pair(testCase, code));
		return manager.get(code);
	}

}


inline vector<TypePtr> getTypes(const vector<ExpressionPtr>& expressions) {
	vector<TypePtr> res;
	::transform(expressions, std::back_inserter(res), [](const ExpressionPtr& cur) { return cur->getType(); });
	return res;
}

// ---------------------------------- Check the frontend -------------------------------------

// the type definition (specifying the parameter type)
class FrontendIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(FrontendIntegrationTest, SemanticChecks) {
	core::NodeManager manager;
	
	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();
	SCOPED_TRACE("Testing Case: " + testCase.getName());
	LOG(INFO) << "Testing Case: " + testCase.getName();

	// load the code using the frontend
	core::ProgramPtr code = load(manager, testCase);

	// run semantic checks on loaded program
	auto errors = core::check(code, core::checks::getFullCheck()).getErrors();

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
		EXPECT_TRUE(analysis::getTypeVariableInstantiation(manager, call))
//					<< "Processing:     " << core::printer::PrettyPrinter(call) << "\n"
				<< "FunctionType:   " << *(call->getFunctionExpr()->getType()) << "\n"
				<< "Argument Types: " << getTypes(call->getArguments());
	});
}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(TypeVariableDeductionCheck, TypeVariableDeductionTest, ::testing::ValuesIn(getAllCases()));



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
//	IRBuilder builder(mgr);
//
//	// search for array variable declarations
//	//visitDepthFirstInterruptible(NodeAddress(code), [&](const DeclarationStmtAddress& curdecl) -> bool {
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
//	visitDepthFirstInterruptible(NodeAddress(code), [&](const CallExprAddress& curcall) -> bool {
//		for(int argIndex = 0; argIndex < curcall->getArguments().size(); ++argIndex) {
//			if(CallExprPtr convertcall = dynamic_pointer_cast<const CallExpr>(curcall->getArgument(argIndex))) {
//				if(basic.isScalarToArray(convertcall->getFunctionExpr())) {
//					if(LambdaExprPtr called = dynamic_pointer_cast<const LambdaExpr>(curcall->getFunctionExpr())) {
//						VariablePtr param = called->getParameterList()[argIndex];
//						LOG(INFO) << "**************************************\n====\nparam:\n " << printer::PrettyPrinter(param) << "\n*********************\n";
//						visitDepthFirstInterruptible(NodeAddress(called->getBody()), [&](const VariableAddress& var) {
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
	LOG(INFO) << "Testing Case: " + testCase.getName();

	// load the code using the frontend
	core::ProgramPtr code = load(manager, testCase);

	// create target code using simple backend
	auto target = simple_backend::SimpleBackend::getDefault()->convert(code);

	// test whether result can be compiled
	auto compiler = utils::compiler::Compiler::getDefaultC99Compiler();
	compiler.addFlag("-I" SRC_DIR "../../simple_backend/include/insieme/simple_backend/runtime");
	compiler.addFlag("-c");
	//compiler.addFlag("-L/home/herbert/insieme/build_all/code/simple_backend/");
	//compiler.addFlag("-lm");

	EXPECT_TRUE(utils::compiler::compile(*target, compiler));// << "Code: " << *target; // << core::printer::PrettyPrinter(code);
}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(SimpleBackendIntegrationCheck, SimpleBackendIntegrationTest, ::testing::ValuesIn(getAllCases()));




// ---------------------------------- Check the runtime backend -------------------------------------

// the type definition (specifying the parameter type)
class RuntimeBackendIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(RuntimeBackendIntegrationTest, CompileableCode) {
	core::NodeManager manager;

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();

	SCOPED_TRACE("Testing Case: " + testCase.getName());
	LOG(INFO) << "Testing Case: " + testCase.getName();

	if (testCase.getName() == "ocl/ocl_kernel") {
		LOG(INFO) << "Skipping kernel test ...";
		return;
	}

	// load the code using the frontend
	core::ProgramPtr code = load(manager, testCase);


	// create target code using the runtime backend
	auto target = backend::runtime::RuntimeBackend::getDefault()->convert(code);

	// see whether target code can be compiled
	utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultC99Compiler();
	compiler.addFlag("-I " SRC_DIR "../../runtime/include -D_XOPEN_SOURCE=700 -D_GNU_SOURCE -ldl -lrt -lpthread -lm");

	EXPECT_TRUE(utils::compiler::compile(*target, compiler));// << "Code: " << *target;


//	// test whether result can be compiled
//	auto compiler = utils::compiler::Compiler::getDefaultC99Compiler();
//	compiler.addFlag("-I/home/herbert/insieme/code/simple_backend/include/insieme/simple_backend/runtime");
//	EXPECT_TRUE(utils::compiler::compile(*target, compiler));
}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(RuntimeBackendIntegrationCheck, RuntimeBackendIntegrationTest, ::testing::ValuesIn(getAllCases()));


// ---------------------------------- Check the binary dump -------------------------------------

// the type definition (specifying the parameter type)
class BinaryDumpIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };

// define the test case pattern
TEST_P(BinaryDumpIntegrationTest, WriteReadTest) {
	core::NodeManager managerA;

	// obtain test case
	utils::test::IntegrationTestCase testCase = GetParam();

	SCOPED_TRACE("Testing Case: " + testCase.getName());
	LOG(INFO) << "Testing Case: " + testCase.getName();

	// load the code using the frontend
	core::ProgramPtr code = load(managerA, testCase);

	// create a in-memory stream
	std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);

	// dump IR using a binary format
	core::dump::binary::dumpIR(buffer, code);

	// reload IR using a different node manager
	core::NodeManager managerB;
	core::NodePtr restored = core::dump::binary::loadIR(buffer, managerB);

	EXPECT_NE(code, restored);
	EXPECT_EQ(*code, *restored);

	buffer.seekg(0); // reset stream

	core::NodePtr restored2 = core::dump::binary::loadIR(buffer, managerA);
	EXPECT_EQ(code, restored2);

}

// instantiate the test case
INSTANTIATE_TEST_CASE_P(BinaryDumpIntegrationCheck, BinaryDumpIntegrationTest, ::testing::ValuesIn(getAllCases()));



//// ---------------------------------- Check the text dump -------------------------------------
//
//// the type definition (specifying the parameter type)
//class TextDumpIntegrationTest : public ::testing::TestWithParam<IntegrationTestCase> { };
//
//// define the test case pattern
//TEST_P(TextDumpIntegrationTest, WriteReadTest) {
//	core::NodeManager managerA;
//
//	// obtain test case
//	utils::test::IntegrationTestCase testCase = GetParam();
//
//	SCOPED_TRACE("Testing Case: " + testCase.getName());
//
//	// load the code using the frontend
//	core::ProgramPtr code = load(managerA, testCase);
//
//	// create a in-memory stream
//	std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);
//
//	// dump IR using a binary format
//	core::dump::text::dumpIR(buffer, code);
//
//	// abort run in case dump is larger than 2GB
//	if (buffer.str().length() > (1<<31)) {
//		return;
//	}
//
//	// reload IR using a different node manager
//	core::NodeManager managerB;
//	core::NodePtr restored = core::dump::text::loadIR(buffer, managerB);
//
//	EXPECT_NE(code, restored);
//	EXPECT_EQ(*code, *restored);
//
//	buffer.seekg(0); // reset stream
//
//	core::NodePtr restored2 = core::dump::text::loadIR(buffer, managerA);
//	EXPECT_EQ(code, restored2);
//
//}
//
//// instantiate the test case
//INSTANTIATE_TEST_CASE_P(TextDumpIntegrationCheck, TextDumpIntegrationTest, ::testing::ValuesIn(getAllCases()));



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
	LOG(INFO) << "Testing Case: " + testCase.getName();

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

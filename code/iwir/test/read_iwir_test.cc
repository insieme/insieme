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

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/iwir/utils.h"
#include "insieme/iwir/test_config.h"

#include "insieme/utils/logging.h"

using namespace std;
using namespace insieme::utils;
using namespace insieme::utils::log;
using namespace insieme::core;
using namespace insieme::core::lang;

namespace insieme {
namespace iwir {
/*
bool check(NodePtr node) {

	XmlUtil xml;
	xml.convertIrToDom(node);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	std::cout << s1 << std::endl;
	NodeManager manager2;
	NodePtr restored = xml.convertDomToIr(manager2);

	EXPECT_TRUE(*node == *restored)
		<< "Orig: " << *node << "\n"
		<< "Rest: " << *restored << "\n";

	return node!=restored && *node == *restored;
}
*/

/*
 * FIXME do we need an explicit test for this?
 *
TEST(IWIRTest, XmlToDom) {
	XmlUtil xml;
	
	auto iwirSchema = IWIR_SCHEMA_DIR + "iwir_schema1.1.xsd";
	auto iwirInput = IWIR_TEST_DIR + "input/parallelForEachTest1.xml";

	xml.convertXmlToDom(iwirInput, iwirSchema, true);

	std::cout << "---" << std::endl;

	std::cout << xml.convertDomToString() << std::endl;

	std::cout << "---" << std::endl;

	//auto iwirAst = xml.convertDomToIWIR("someIWIRSchemaFile");

	//auto irprogram = iwirAST.convertIWIRtoInspire();

	//std::cout << irprogram << std::endl;
}
*/

TEST(IWIRTest, ReadIWIR_Atomic1) {
	auto iwirInput = IWIR_TEST_DIR + "input/atomicTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_Atomic2) {
	auto iwirInput = IWIR_TEST_DIR + "input/atomicTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_If1) {
	auto iwirInput = IWIR_TEST_DIR + "input/ifTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_If2) {
	auto iwirInput = IWIR_TEST_DIR + "input/ifTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_If3) {
	auto iwirInput = IWIR_TEST_DIR + "input/ifTest3.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_If4) {
	auto iwirInput = IWIR_TEST_DIR + "input/ifTest4.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_If5) {
	auto iwirInput = IWIR_TEST_DIR + "input/ifTest5.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS1) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS2) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS3) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest3.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS4) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest4.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS5) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest5.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS6) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest6.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_BS7) {
	auto iwirInput = IWIR_TEST_DIR + "input/blockScopeTest7.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_While1) {
	auto iwirInput = IWIR_TEST_DIR + "input/whileTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_While2) {
	auto iwirInput = IWIR_TEST_DIR + "input/whileTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_While3) {
	auto iwirInput = IWIR_TEST_DIR + "input/whileTest3.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For1) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For2) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For3) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest3.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For4) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest4.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For5) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest5.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For6) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest6.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For7) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest7.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_For8) {
	auto iwirInput = IWIR_TEST_DIR + "input/forTest8.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_ForEach1) {
	auto iwirInput = IWIR_TEST_DIR + "input/forEachTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_ForEach2) {
	auto iwirInput = IWIR_TEST_DIR + "input/forEachTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_ForEach3) {
	auto iwirInput = IWIR_TEST_DIR + "input/forEachTest3.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_ParallelForEach1) {
	auto iwirInput = IWIR_TEST_DIR + "input/parallelForEachTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}


TEST(IWIRTest, ReadIWIR_ParallelFor1) {
	auto iwirInput = IWIR_TEST_DIR + "input/parallelForTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_ParallelFor2) {
	auto iwirInput = IWIR_TEST_DIR + "input/parallelForTest2.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_ImplicitLinkCasts) {
	auto iwirInput = IWIR_TEST_DIR + "input/implicitLinkCasts.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_Wien2K) {
	auto iwirInput = IWIR_TEST_DIR + "input/wien2k.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

TEST(IWIRTest, ReadIWIR_SubWF1) {
	auto iwirInput = IWIR_TEST_DIR + "input/subWFTest1.xml";

	NodeManager mgr;
	VLOG(2) << "---" << std::endl;
	auto ir = readIWIR(mgr, iwirInput);
	VLOG(2) << "---" << std::endl;
}

} // end namespace iwir
} // end namespace insieme

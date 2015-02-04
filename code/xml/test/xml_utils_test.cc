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
#include <xercesc/util/XercesDefs.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/xml/xml_utils.h"
#include "insieme/xml/test_config.h"

#include "insieme/utils/logging.h"

using namespace std;
using namespace insieme::utils;
using namespace insieme::utils::log;
using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::xml;

namespace insieme {
namespace xml {
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

TEST(XmlTest, XmlToDom) {
	XmlUtil xml;
	
	auto iwirSchema = XML_SCHEMA_DIR + "iwir_schema1.1.xsd";
	auto iwirInput = XML_TEST_DIR + "input/parallelForEachTest1.xml";

	xml.convertXmlToDom(iwirInput, iwirSchema, true);

	std::cout << "---" << std::endl;

	std::cout << xml.convertDomToString() << std::endl;

	std::cout << "---" << std::endl;

	//auto iwirAst = xml.convertDomToIWIR("someIWIRSchemaFile");

	//auto irprogram = iwirAST.convertIWIRtoInspire();

	//std::cout << irprogram << std::endl;
}

TEST(XmlTest, ReadIWIR_Atomic1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/atomicTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_Atomic2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/atomicTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_If1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/ifTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_If2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/ifTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_If3) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/ifTest3.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_If4) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/ifTest4.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_If5) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/ifTest5.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS3) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest3.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS4) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest4.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS5) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest5.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS6) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest6.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_BS7) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/blockScopeTest7.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_While1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/whileTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_While2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/whileTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For3) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest3.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For4) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest4.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For5) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest5.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For6) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest6.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_For7) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forTest7.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_ForEach1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forEachTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_ForEach2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/forEachTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_ParallelForEach1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/parallelForEachTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}


TEST(XmlTest, ReadIWIR_ParallelFor1) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/parallelForTest1.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_ParallelFor2) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/parallelForTest2.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}

TEST(XmlTest, ReadIWIR_Wien2K) {
	Logger::get(std::cerr, DEBUG, 2);
	
	auto iwirInput = XML_TEST_DIR + "input/wien2k.xml";

	NodeManager mgr;
	std::cout << "---" << std::endl;
	XmlUtil::readIWIR(mgr, iwirInput);
	std::cout << "---" << std::endl;
}
} // end namespace xml
} // end namespace insieme

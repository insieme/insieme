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
#include <xml_utils.h>

using namespace insieme::core;

TEST(XmlTest, GenericTypeTest) {
	NodeManager manager;
	//GenericTypePtr type = GenericType::get(manager, "int");
	//GenericTypePtr type2 = GenericType::get(manager, "int", toVector<TypePtr>(type, type), toVector(IntTypeParam::getVariableIntParam('p')), type);
	//NodePtr root = type2;
	
	//TypeVariablePtr varA = TypeVariable::get(manager, "alpha");
	//GenericTypePtr type1 = GenericType::get(manager, "C", toVector<TypePtr>(varA));
	
	GenericTypePtr type1 = GenericType::get(manager, "int");
	GenericTypePtr type2 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type1), toVector(IntTypeParam::getVariableIntParam('p')), type1);
	FunctionTypePtr funType1 = FunctionType::get(manager, type1, type2);
	NodePtr root = funType1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	//std::cout << s1;
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	//std::cout << s2;
	EXPECT_EQ (s1, s2);
	//xmlWrite(root, "dump1.xml");
	//xmlRead("dump1.xml", false);
	//xmlValidate("dump1.xml");
}
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

#include <sstream>
#include <stdlib.h>


#include <gtest/gtest.h>

#define BOOST_SPIRIT_DEBUG

#include "insieme/core/identifier.h"
#include "insieme/core/types.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/string_utils.h"

using namespace insieme::core;
using namespace insieme::core::parse;


TEST(IRParser, TypeTests) {

	string testStr("testGenType");
	NodeManager manager;
	IRParser parser(manager);
	ASTBuilder builder(manager);
	
	auto intType = builder.genericType("int", vector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam('a')));
	EXPECT_EQ(intType, parser.parseType("int<#a>"));
	EXPECT_EQ(intType, parser.parseType("(|int<#a>|)"));

	auto intPairType = builder.tupleType(toVector<TypePtr>(intType, intType));
	EXPECT_EQ(intPairType, parser.parseType("(int<#a>, int<#a>)"));

	auto funType = builder.functionType(intPairType->getElementTypes(), intType);
	EXPECT_EQ(funType, parser.parseType("(int<#a>, int<#a>) -> int<#a>"));

	auto captureFunType = builder.functionType(intPairType->getElementTypes(), toVector<TypePtr>(intType, intType, intType), intType);
	EXPECT_EQ(captureFunType, parser.parseType("[int<#a>, int<#a>](int<#a>, int<#a>, int<#a>) -> int<#a>"));

	auto arrayType = builder.arrayType(intType);
	EXPECT_EQ(arrayType, parser.parseType("array<int<#a>, 1>"));

	auto vectorType = builder.vectorType(intType, IntTypeParam::getConcreteIntParam(10));
	EXPECT_EQ(vectorType, parser.parseType("vector<int<#a>, 10>"));

	auto refType = builder.refType(intType);
	EXPECT_EQ(refType, parser.parseType("ref<int<#a>>"));

	auto multiParamType = builder.genericType("multi", toVector<TypePtr>(builder.typeVariable("tvar"), intType, intPairType), 
		toVector(IntTypeParam::getConcreteIntParam(2), IntTypeParam::getInfiniteIntParam(), IntTypeParam::getVariableIntParam('v')));
	EXPECT_EQ(multiParamType, parser.parseType("multi<'tvar,int<#a>,(int<#a>,int<#a>),2,#inf,#v>"));

	EXPECT_THROW(parser.parseType("fail1<#1,'alpha>"), ParseException);
	EXPECT_THROW(parser.parseType("(fail2"), ParseException);
	EXPECT_THROW(parser.parseType("fail3)"), ParseException);
	EXPECT_THROW(parser.parseType("int -> bool"), ParseException);

}

TEST(IRParser, InteractiveTest) {

	string testStr("testGenType");
	if(getenv("IR_PARSE_STR")) testStr = string(getenv("IR_PARSE_STR"));
	NodeManager nm;
	try {
		TypePtr t = parseType(nm, testStr);
		std::cout << "--------------------------------------\n Parsing succeeded, "
			<< "result: \n" << t << std::endl << "--------------------------------------\n";
	} catch(ParseException e) {
		std::cout << "--------------------------------------\n Parsing failed \n"
			<< std::endl << "--------------------------------------\n";
	}
}

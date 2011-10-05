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

#include "insieme/transform/pattern/irconvert.h"
#include "insieme/transform/pattern/irpattern.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/logging.h"

#ifndef TEST
// avoiding warnings in eclipse, enabling auto completions
#define TEST void fun
#endif

using namespace insieme::utils::log;

namespace insieme {
using namespace core;
	
namespace transform {
namespace pattern {

bool match(const TreePatternPtr& pattern, const TreePtr& tree) {
	return pattern->match(tree);
}

bool notMatch(const TreePatternPtr& pattern, const TreePtr& tree) {
	return !match(pattern, tree);
}

TEST(IRConvert, Basic) {
	NodeManager manager;
	auto t = [&manager](string typespec) { return parse::parseType(manager, typespec); };
	
	TypePtr tupleA = t("(int<4>,float<8>,uint<1>)");
	TypePtr tupleB = t("(int<4>,float<8>)");

	//LOG(INFO) << *tupleA;

	ConversionVisitor converter;
	auto treeA = converter.visit(NodeAddress(tupleA));
	auto treeB = converter.visit(NodeAddress(tupleB));
	//LOG(INFO) << treeA;

	TreePatternPtr patternA = aT(atom(converter.visit(NodeAddress(t("float<8>")))));
	EXPECT_PRED2(match, patternA, treeA);
	TreePatternPtr patternB = aT(atom(converter.visit(NodeAddress(t("uint<8>")))));
	EXPECT_PRED2(notMatch, patternB, treeA);
	TreePatternPtr patternC = irp::tupleType(any << atom(converter.visit(NodeAddress(t("float<8>")))) << any);
	EXPECT_PRED2(match, patternC, treeA);
	EXPECT_PRED2(notMatch, patternC, treeB);

	TreePatternPtr patternD = irp::tupleType(*any << atom(converter.visit(NodeAddress(t("float<8>")))) << *any);
	EXPECT_PRED2(match, patternD, treeA);
	EXPECT_PRED2(match, patternD, treeB);

	TreePatternPtr patternE = irp::tupleType(*any << atom(converter.visit(NodeAddress(t("uint<1>")))) << *any);
	EXPECT_PRED2(match, patternE, treeA);
	EXPECT_PRED2(notMatch, patternE, treeB);
}


TEST(IRPattern, Types) {
	NodeManager manager;
	auto t = [&manager](string typespec) { return parse::parseType(manager, typespec); };
	
	TypePtr int8Type = t("int<8>");
	TypePtr genericA = t("megatype<ultratype<int<8>,666>>");
	
	ConversionVisitor converter;
	auto int8TypeTree = converter.visit(NodeAddress(int8Type));
	auto genericATypeTree = converter.visit(NodeAddress(genericA));

	TreePatternPtr patternA = irp::genericType("megatype", single(any));
	EXPECT_PRED2(notMatch, patternA, int8TypeTree); 
	EXPECT_PRED2(match, patternA, genericATypeTree);

	TreePatternPtr patternB = irp::genericType("ultratype", any << any);
	EXPECT_PRED2(notMatch, patternB, int8TypeTree);
	EXPECT_PRED2(notMatch, patternB, genericATypeTree);
	EXPECT_PRED2(notMatch, aT(patternB), int8TypeTree);
	EXPECT_PRED2(match, aT(patternB), genericATypeTree);
}


TEST(IRPattern, Expressions) {
	NodeManager manager;
	auto e = [&manager](string expressionSpec) { return parse::parseExpression(manager, expressionSpec); };
	
	ExpressionPtr realAddExp = e("(4.2 + 3.1)");
	ExpressionPtr intSubExp = e("(4 - 2)");
	ExpressionPtr nestedExp = e("((4.2 + 3.1) * (4 - 2))");
	
	auto realAddTree = convertIR(realAddExp);
	auto intSubTree = convertIR(intSubExp);
	auto nestedTree = convertIR(nestedExp);

	TreePatternPtr patternA = irp::call(manager.basic.getRealAdd(), *any);
	EXPECT_PRED2(match, patternA, realAddTree);
	EXPECT_PRED2(notMatch, patternA, intSubTree);
	EXPECT_PRED2(notMatch, patternA, nestedTree);
	
	TreePatternPtr patternB = node(irp::lit("int.sub", any) << *any);
	EXPECT_PRED2(notMatch, patternB, realAddTree);
	EXPECT_PRED2(match, patternB, intSubTree);
	EXPECT_PRED2(notMatch, patternB, nestedTree);
}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme


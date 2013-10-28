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


#include "insieme/analysis/cba/cba.h"

#include "insieme/analysis/cba/framework/cba.h"

#include "insieme/analysis/cba/analysis/arithmetic.h"
#include "insieme/analysis/cba/analysis/boolean.h"
#include "insieme/analysis/cba/analysis/simple_constant.h"
#include "insieme/analysis/cba/analysis/callables.h"
#include "insieme/analysis/cba/analysis/call_context.h"
#include "insieme/analysis/cba/analysis/reachability.h"

#include "insieme/core/ir_builder.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/test/test_utils.h"

#include "cba_test.inc.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, Locations) {
		NodeManager mgr;
		IRBuilder builder(mgr);


	}

	TEST(CBA, SimpleStruct) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	let point = struct { int a; int b; };"
				"	"
				"	point p1 = (point){ 1, 2 };"
				"	point p2 = (point){ 3, 4 };"
				"	"
				"	p1;"
				"	p2;"
				"	p1.a;"
				"	p1.b;"
				"	p2.a;"
				"	p2.b;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("[a={1},b={2}]", toString(analysis.getValuesOf(code[2].as<ExpressionAddress>(), A)));
		EXPECT_EQ("[a={3},b={4}]", toString(analysis.getValuesOf(code[3].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{1}", toString(analysis.getValuesOf(code[4].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{2}", toString(analysis.getValuesOf(code[5].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{3}", toString(analysis.getValuesOf(code[6].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{4}", toString(analysis.getValuesOf(code[7].as<ExpressionAddress>(), A)));

	}

	TEST(CBA, NestedStruct) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	let point = struct { int x; int y; };"
				"	let cycle = struct { point center; int r; };"
				"	"
				"	cycle c1 = (cycle){ (point) { 1, 2 } , 3 };"
				"	"
				"	c1;"
				"	c1.center;"
				"	c1.center.x;"
				"	c1.center.y;"
				"	c1.r;"
				"	"
				"	point p1 = c1.center;"
				"	p1;"
				"	p1.x;"
				"	p1.y;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		EXPECT_EQ("[center=[x={1},y={2}],r={3}]", toString(analysis.getValuesOf(code[1].as<ExpressionAddress>(), A)));
		EXPECT_EQ("[x={1},y={2}]", toString(analysis.getValuesOf(code[2].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{1}", toString(analysis.getValuesOf(code[3].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{2}", toString(analysis.getValuesOf(code[4].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{3}", toString(analysis.getValuesOf(code[5].as<ExpressionAddress>(), A)));

		EXPECT_EQ("[x={1},y={2}]", toString(analysis.getValuesOf(code[7].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{1}", toString(analysis.getValuesOf(code[8].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{2}", toString(analysis.getValuesOf(code[9].as<ExpressionAddress>(), A)));

	}

	TEST(CBA, MutableStruct) {

		// a simple test cases checking the handling of simple value structs
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto in = builder.parseStmt(
				"{"
				"	let int = int<4>;"
				"	let point = struct { int a; int b; };"
				"	"
				"	auto p1 = var((point){ 1, 2 });"
				"	auto p2 = var((point){ 3, 4 });"
				"	ref<ref<point>> p3 = var(p1);"
				"	"
				"	*p1;"
				"	*p2;"
				"	*p3;"
				"	*p1.a;"
				"	*p1.b;"
				"	*p2.a;"
				"	*p2.b;"
				"	*p3->a;"
				"	*p3->b;"
				"	"
				"	p1.a = 5;"
				"	"
				"	*p1;"
				"	*p2;"
				"	*p3;"
				"	*p1.a;"
				"	*p1.b;"
				"	*p2.a;"
				"	*p2.b;"
				"	*p3->a;"
				"	*p3->b;"
				"	"
				"	p1.b = 6;"
				"	"
				"	*p1;"
				"	*p2;"
				"	*p3;"
				"	*p1.a;"
				"	*p1.b;"
				"	*p2.a;"
				"	*p2.b;"
				"	*p3->a;"
				"	*p3->b;"
				"	"
				"	p3 = p2;"
				"	"
				"	*p1;"
				"	*p2;"
				"	*p3;"
				"	*p1.a;"
				"	*p1.b;"
				"	*p2.a;"
				"	*p2.b;"
				"	*p3->a;"
				"	*p3->b;"
				"	"
				"	p1 = *p2;"
				"	"
				"	*p1;"
				"	*p2;"
				"	*p3;"
				"	*p1.a;"
				"	*p1.b;"
				"	*p2.a;"
				"	*p2.b;"
				"	*p3->a;"
				"	*p3->b;"
				"}"
		).as<CompoundStmtPtr>();

		ASSERT_TRUE(in);
		CompoundStmtAddress code(in);

		CBA analysis(code);

		int pos = 3;
		EXPECT_EQ("[a={1},b={2}]", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		createDotDump(analysis);
		EXPECT_EQ("[a={3},b={4}]", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("[a={3},b={4}]", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{1}", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{2}", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{3}", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{4}", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{1}", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));
		EXPECT_EQ("{2}", toString(analysis.getValuesOf(code[pos++].as<ExpressionAddress>(), A)));

		pos++;

	}

//	TEST(CBA, SimpleArray) {
//
//		// a simple test cases checking the handling of simple value structs
//		NodeManager mgr;
//		IRBuilder builder(mgr);
//
//		auto in = builder.parseStmt(
//				"{"
//				"	let int = int<4>;"
//				"	let list = arra<int,1>;"
//				"	"
//				"	point p1 = (point){ 1, 2 };"
//				"	point p2 = (point){ 3, 4 };"
//				"	"
//				"	p1;"
//				"	p2;"
//				"	p1.a;"
//				"	p1.b;"
//				"	p2.a;"
//				"	p2.b;"
//				"}"
//		).as<CompoundStmtPtr>();
//
//		ASSERT_TRUE(in);
//		CompoundStmtAddress code(in);
//
//		CBA analysis(code);
//
//		EXPECT_EQ("[a={1},b={2}]", toString(analysis.getValuesOf(code[2].as<ExpressionAddress>(), A)));
//		EXPECT_EQ("[a={3},b={4}]", toString(analysis.getValuesOf(code[3].as<ExpressionAddress>(), A)));
//		EXPECT_EQ("{1}", toString(analysis.getValuesOf(code[4].as<ExpressionAddress>(), A)));
//		EXPECT_EQ("{2}", toString(analysis.getValuesOf(code[5].as<ExpressionAddress>(), A)));
//		EXPECT_EQ("{3}", toString(analysis.getValuesOf(code[6].as<ExpressionAddress>(), A)));
//		EXPECT_EQ("{4}", toString(analysis.getValuesOf(code[7].as<ExpressionAddress>(), A)));
//
//	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

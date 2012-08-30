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

#include "insieme/transform/polyhedral/transformations.h"

#include "insieme/transform/polyhedral/primitives.h"
#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/normalize.h"

using namespace insieme::analysis::polyhedral;
using namespace insieme::transform::polyhedral;

TEST(Transform, InterchangeManual) {

	Logger::get(std::cerr, DEBUG);

	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	TreePatternPtr pattern = rT ( irp::forStmt( var("i"), any, any, any, recurse | !irp::forStmt() ) );
	auto&& match = pattern->matchPointer(forStmt);

	VariablePtr i = 
		static_pointer_cast<const Variable>( match->getVarBinding("i").getList()[0] );

	VariablePtr j = 
		static_pointer_cast<const Variable>( match->getVarBinding("i").getList()[1] );
	
	EXPECT_TRUE(forStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);

	Scop& scop = ann.getScop();
	IntMatrix&& schedule = extractFrom(scop[0].getSchedule());
	// std::cout << schedule << std::endl;
	
	auto&& interMat = makeInterchangeMatrix( scop.getIterationVector(), i, j );
	// std::cout << interMat << std::endl;

	auto&& newSched = schedule * interMat;
	// std::cout << newSched << std::endl;

	scop[0].getSchedule().set( newSched );

	NodePtr newIR = analysis::normalize(scop.toIR(mgr));
	EXPECT_EQ( "for(int<4> v0 = 5 .. int.add(24, 1) : 1) {"
					"for(int<4> v1 = 10 .. int.add(49, 1) : 1) {"
						"array.ref.elem.1D(v3, uint.add(v1, v0));"
					"};"
				"}", toString(*newIR));
}

void checkSCoPCorrectness(const insieme::core::NodePtr& node) { 
	using namespace insieme::core;
	using namespace insieme::analysis;
	// Check for the generated SCoP
	auto scop2 = polyhedral::scop::ScopRegion::toScop(node);
	EXPECT_TRUE(scop2);

	// LOG(DEBUG) << *scop2;
	NodePtr res = analysis::normalize(scop2->toIR(node->getNodeManager()));

	// LOG(DEBUG) << printer::PrettyPrinter(newNode);
	// LOG(DEBUG) << printer::PrettyPrinter(res); 
	EXPECT_EQ(*node,*res);
}

TEST(Transform, InterchangeAuto) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopInterchange li(0, 1);
	NodePtr newIR = analysis::normalize(li.apply(forStmt));
	
	EXPECT_EQ( "for(int<4> v0 = 5 .. int.add(24, 1) : 1) {"
					"for(int<4> v1 = 10 .. int.add(49, 1) : 1) {"
						"array.ref.elem.1D(v3, uint.add(v1, v0));"
					"};"
				"}", toString(*newIR));

	checkSCoPCorrectness(newIR);
}

TEST(Transform, InterchangeAuto2) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i)); \
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopInterchange li(0, 1);
	NodePtr newIR = analysis::normalize(li.apply(forStmt));
	
	EXPECT_EQ( "{"
				"for(int<4> v0 = 10 .. int.add(49, 1) : 1) {"
					"array.ref.elem.1D(v2, v0);"
				"}; "
				"for(int<4> v1 = 5 .. int.add(24, 1) : 1) {"
					"for(int<4> v3 = 10 .. int.add(49, 1) : 1) {"
						"array.ref.elem.1D(v2, uint.add(v3, v1));"
					"};"
				"};"
			    "}", toString(*newIR));

	checkSCoPCorrectness(newIR);
}

TEST(Transform, StripMiningAuto) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i)); \
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopStripMining li(1, 7);
	NodePtr newIR = analysis::normalize(li.apply(forStmt));

	EXPECT_EQ( "for(int<4> v0 = 10 .. int.add(49, 1) : 1) {"
					"array.ref.elem.1D(v2, v0); "
					"for(int<4> v1 = 5 .. int.add(24, 1) : 7) {"
						"for(int<4> v3 = v1 .. int.add(select(int.add(cast<int<4>>(v1), cast<int<4>>(6)), 24, int.lt), 1) : 1) {"
							"array.ref.elem.1D(v2, uint.add(v0, v3));"
						"};"
					"};"
				"}", toString(*newIR));

	checkSCoPCorrectness(newIR);
}

TEST(Transform, LoopFusionAuto) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto stmt = parser.parseStatement("\
		{\
			for(decl uint<4>:i = 10 .. 50 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i)); \
			};\
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, j)); \
			}; \
		}");

	scop::mark(stmt);

	LoopFusion lf( {0,1} );
	NodePtr newIR = analysis::normalize(lf.apply(stmt));

	EXPECT_EQ( "{"
					"for(int<4> v0 = 5 .. int.add(9, 1) : 1) {"
						"array.ref.elem.1D(v2, v0);"
					"}; "
					"for(int<4> v1 = 10 .. int.add(24, 1) : 1) {"
						"array.ref.elem.1D(v2, v1); array.ref.elem.1D(v2, v1);"
					"}; "
					"for(int<4> v3 = 25 .. int.add(49, 1) : 1) {"
						"array.ref.elem.1D(v2, v3);"
					"};"
				"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);
}

TEST(Transform, TilingManual) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopStripMining lsm1(0, 7);
	NodePtr newIR = lsm1.apply(forStmt);

	LoopStripMining lsm2(2, 7);
	newIR = lsm2.apply(newIR);

	LoopInterchange li(1,2);
	newIR = analysis::normalize(li.apply(newIR));

	EXPECT_EQ( "for(int<4> v0 = 10 .. int.add(49, 1) : 7) {"
					"for(int<4> v1 = 5 .. int.add(24, 1) : 7) {"
						"for(int<4> v2 = v0 .. int.add(select(int.add(cast<int<4>>(v0), cast<int<4>>(6)), 49, int.lt), 1) : 1) {"
							"for(int<4> v4 = v1 .. int.add(select(int.add(cast<int<4>>(v1), cast<int<4>>(6)), 24, int.lt), 1) : 1) {"
								"array.ref.elem.1D(v3, uint.add(v2, v4));"
							"};"
						"};"
					"};"
				"}", newIR->toString() );

	checkSCoPCorrectness(newIR);
}

TEST(Transform, TilingAuto) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopTiling li({7,7});
	NodePtr newIR = analysis::normalize(li.apply(forStmt));

	EXPECT_EQ( "for(int<4> v0 = 10 .. int.add(49, 1) : 7) {"
					"for(int<4> v1 = 5 .. int.add(24, 1) : 7) {"
						"for(int<4> v2 = v0 .. int.add(select(int.add(cast<int<4>>(v0), cast<int<4>>(6)), 49, int.lt), 1) : 1) {"
							"for(int<4> v4 = v1 .. int.add(select(int.add(cast<int<4>>(v1), cast<int<4>>(6)), 24, int.lt), 1) : 1) {"
								"array.ref.elem.1D(v3, uint.add(v2, v4));"
							"};"
						"};"
					"};"
				"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);
}

TEST(Transform, TilingAuto2) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 3 .. 25 : 1) { \
				for(decl uint<4>:k = 2 .. 100 : 1) { \
					(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
				}; \
			}; \
		}") );

	scop::mark(forStmt);

	LoopTiling li({ 7,6,3 });
	NodePtr newIR = analysis::normalize(li.apply(forStmt));

	EXPECT_EQ( "for(int<4> v0 = 10 .. int.add(49, 1) : 7) {"
					"for(int<4> v1 = 3 .. int.add(24, 1) : 6) {"
						"for(int<4> v2 = 2 .. int.add(99, 1) : 3) {"
							"for(int<4> v3 = v0 .. int.add(select(int.add(cast<int<4>>(v0), cast<int<4>>(6)), 49, int.lt), 1) : 1) {"
								"for(int<4> v5 = v1 .. int.add(select(int.add(cast<int<4>>(v1), cast<int<4>>(5)), 24, int.lt), 1) : 1) {"
									"for(int<4> v6 = v2 .. int.add(select(int.add(cast<int<4>>(v2), cast<int<4>>(2)), 99, int.lt), 1) : 1) {"
										"array.ref.elem.1D(v4, uint.add(v3, v5));"
									"};"
								"};"
							"};"
						"};"
					"};"
				"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);
}

TEST(Transform, TilingAuto21) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 3 .. 25 : 1) { \
				for(decl uint<4>:k = 2 .. 100 : 1) { \
					(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
				}; \
			}; \
		}") );

	scop::mark(forStmt);

	LoopTiling li({ 5,5 }, {0,0});
	NodePtr newIR = analysis::normalize(li.apply(forStmt));

	EXPECT_EQ( "for(int<4> v0 = 10 .. int.add(49, 1) : 1) {"
					"for(int<4> v1 = 3 .. int.add(24, 1) : 5) {"
						"for(int<4> v2 = 2 .. int.add(99, 1) : 5) {"
							"for(int<4> v3 = v1 .. int.add(select(int.add(cast<int<4>>(v1), cast<int<4>>(4)), 24, int.lt), 1) : 1) {"
								"for(int<4> v5 = v2 .. int.add(select(int.add(cast<int<4>>(v2), cast<int<4>>(4)), 99, int.lt), 1) : 1) {"
									"array.ref.elem.1D(v4, uint.add(v0, v3));"
								"};"
							"};"
						"};"
					"};"
				"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);
}

TEST(Transform, TilingAuto3) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;
	
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. 50 : 1) { \
			for(decl uint<4>:j = 1 .. 25 : 1) { \
				for(decl uint<4>:k = i .. 100 : 1) { \
					(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
				};\
			}; \
		}") );

	scop::mark(forStmt);

	LoopTiling li({7,6,8});
	NodePtr newIR = analysis::normalize(li.apply(forStmt));

	EXPECT_EQ( "for(int<4> v0 = 10 .. int.add(49, 1) : 7) {"
					"for(int<4> v1 = 1 .. int.add(24, 1) : 6) {"
						"for(int<4> v2 = v0 .. int.add(99, 1) : 1) {"
							"for(int<4> v3 = int.add(cast<int<4>>(v2), cast<int<4>>(int.mul(cast<int<4>>(-8), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v0))), cast<int<4>>(v2)), 8))))) .. int.add(select(int.add(cast<int<4>>(v0), cast<int<4>>(6)), select(v2, 49, int.lt), int.lt), 1) : 8) {"
									"if(bool.and(int.le(v0, v3), bind(){rec v0.{v0=fun(int<4> v1, int<4> v2) {"
												"return int.ge(v1, int.add(cast<int<4>>(v2), cast<int<4>>(-7)));"
											"}"
										"}(v0, v3)})) {"
											"for(int<4> v5 = v1 .. int.add(int.add(cast<int<4>>(v1), cast<int<4>>(5)), 1) : 1) {"
												"for(int<4> v6 = v2 .. int.add(select(int.add(cast<int<4>>(v2), cast<int<4>>(7)), 99, int.lt), 1) : 1) {"
													"array.ref.elem.1D(v4, uint.add(v3, v5));"
												"};"
											"};"
									"} else {};"
								"};"
							"};"
						"};"
					"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);
}

TEST(Transform, LoopStamping) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;
	
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 0 .. 30 : 1) { \
			for(decl uint<4>:j = 0 .. 30 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopStamping ls( 7, { 0,0 } );
	NodePtr newIR = analysis::normalize(ls.apply(forStmt));

	EXPECT_EQ( "{"
					"for(int<4> v0 = 0 .. int.add(29, 1) : 1) {"
						"for(int<4> v1 = 0 .. int.add(27, 1) : 1) {"
							"array.ref.elem.1D(v3, uint.add(v0, v1));"
						"};"
					"}; "
					"for(int<4> v2 = 0 .. int.add(29, 1) : 1) {"
						"for(int<4> v4 = 28 .. int.add(29, 1) : 1) {"
							"array.ref.elem.1D(v3, uint.add(v2, v4));"
						"};"
					"};"
				"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);

}

TEST(Transform, LoopStamping2) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;
	
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 3 .. 30 : 1) { \
			for(decl uint<4>:j = 3 .. 30 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );

	scop::mark(forStmt);

	LoopStamping ls( 7 , { 0 } );
	NodePtr newIR = analysis::normalize(ls.apply(forStmt));

	EXPECT_EQ( "{"
					"for(int<4> v0 = 3 .. int.add(23, 1) : 1) {"
						"for(int<4> v1 = 3 .. int.add(29, 1) : 1) {"
							"array.ref.elem.1D(v3, uint.add(v0, v1));"
						"};"
					"}; "
					"for(int<4> v2 = 24 .. int.add(29, 1) : 1) {"
						"for(int<4> v4 = 3 .. int.add(29, 1) : 1) {"
							"array.ref.elem.1D(v3, uint.add(v2, v4));"
						"};"
					"};"
				"}", toString(*newIR) );

	checkSCoPCorrectness(newIR);

}

TEST(Transform, LoopStamping3) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;
	
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. int<4>:b : 1) { \
			for(decl uint<4>:j = 1 .. 25 : 1) { \
				for(decl uint<4>:k = 1 .. 100 : 1) { \
					(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
				};\
			}; \
		}") );

	scop::mark(forStmt);

	LoopStamping ls( 7, { 0 } );
	NodePtr newIR = ls.apply(forStmt);

	EXPECT_EQ( "if(int.ge(v2, 11)) {"
					"for(int<4> v12 = 10 .. int.add(int.add(cast<int<4>>(int.mul(cast<int<4>>(-7), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v2))), cast<int<4>>(2)), 7)))), cast<int<4>>(-5)), 1) : 1) {"
						"for(int<4> v13 = 1 .. int.add(24, 1) : 1) {"
							"for(int<4> v14 = 1 .. int.add(99, 1) : 1) {"
								"array.ref.elem.1D(v5, uint.add(v12, v13));"
							"};"
						"};"
					"}; "
					"for(int<4> v15 = int.add(cast<int<4>>(int.mul(cast<int<4>>(-7), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v2))), cast<int<4>>(2)), 7)))), cast<int<4>>(-4)) .. int.add(int.add(cast<int<4>>(v2), cast<int<4>>(-1)), 1) : 1) {"
						"for(int<4> v16 = 1 .. int.add(24, 1) : 1) {"
							"for(int<4> v17 = 1 .. int.add(99, 1) : 1) {"
								"array.ref.elem.1D(v5, uint.add(v15, v16));"
							"};"
						"};"
					"};"
				"} else {}", toString(*newIR) );

	//checkSCoPCorrectness(newIR);
}

TEST(Transform, LoopStamping4) {
	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;
	
	
	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl uint<4>:i = 10 .. int<4>:b : 1) { \
			for(decl uint<4>:j = 1 .. 25 : 1) { \
				for(decl uint<4>:k = i .. 100 : 1) { \
					(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
				};\
			}; \
		}") );

	scop::mark(forStmt);

	LoopStamping ls( 7, { 0 } );
	NodePtr newIR = analysis::normalize(ls.apply(forStmt));

	EXPECT_EQ( "if(int.ge(v2, 11)) {"
					"for(int<4> v0 = 10 .. int.add(select(int.add(cast<int<4>>(int.mul(cast<int<4>>(-7), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v2))), cast<int<4>>(2)), 7)))), cast<int<4>>(-5)), 99, int.lt), 1) : 1) {"
						"for(int<4> v1 = 1 .. int.add(24, 1) : 1) {for(int<4> v3 = v0 .. int.add(99, 1) : 1) {"
							"array.ref.elem.1D(v5, uint.add(v0, v1));"
						"};"
					"};"
				"}; "
				"for(int<4> v4 = int.add(cast<int<4>>(int.mul(cast<int<4>>(-7), cast<int<4>>(cloog.floor(int.add(cast<int<4>>(int.mul(cast<int<4>>(-1), cast<int<4>>(v2))), cast<int<4>>(2)), 7)))), cast<int<4>>(-4)) .. int.add(select(int.add(cast<int<4>>(v2), cast<int<4>>(-1)), 99, int.lt), 1) : 1) {"
					"for(int<4> v6 = 1 .. int.add(24, 1) : 1) {"
						"for(int<4> v7 = v4 .. int.add(99, 1) : 1) {"
							"array.ref.elem.1D(v5, uint.add(v4, v6));"
						"};"
					"};"
				"};"
			"} else {}", toString(*newIR) );

	// checkSCoPCorrectness(8,newIR);

}

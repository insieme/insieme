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

#include "insieme/transform/polyhedral/transform.h"
#include "insieme/transform/polyhedral/primitives.h"
#include "insieme/transform/pattern/irpattern.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/core/parser/ir_parse.h"

using namespace insieme::analysis::poly;
using namespace insieme::transform::polyhedral;

TEST(Transform, InterchangeManual) {

	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. 50 : 1) { \
			for(decl int<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	TreePatternPtr pattern = rT ( irp::forStmt( var("i"), any, any, any, recurse | !irp::forStmt() ) );
	auto&& match = pattern->matchPointer(forStmt);
std::cout << "\n Pattern: " << pattern << "\n";
std::cout << "\n Match: " << match << "\n";
	VariablePtr i = 
		static_pointer_cast<const Variable>( match->getVarBinding("i").getTreeList()[0] );

	VariablePtr j = 
		static_pointer_cast<const Variable>( match->getVarBinding("i").getTreeList()[1] );
	
	EXPECT_TRUE(forStmt->hasAnnotation(scop::ScopRegion::KEY));
	scop::ScopRegion& ann = *forStmt->getAnnotation(scop::ScopRegion::KEY);
	ann.resolve();

	poly::Scop& scop = ann.getScop();
	IntMatrix&& schedule = extractFrom(scop[0].getSchedule());
	// std::cout << schedule << std::endl;
	
	auto&& interMat = makeInterchangeMatrix( scop.getIterationVector(), i, j );
	// std::cout << interMat << std::endl;

	auto&& newSched = schedule * interMat;
	// std::cout << newSched << std::endl;

	scop[0].getSchedule().set( newSched );

	NodeManager mgr1;
	NodePtr newIR = scop.toIR(mgr1);
	// std::cout << *newIR << std::endl;
	
	NodeManager mgr2;
	parse::IRParser parser2(mgr2);
	auto forStmtInt = parser2.parseStatement("\
		{for(decl int<4>:j = 5 .. (24+1) : 1) { \
			for(decl int<4>:i = 10 .. (49+1) : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		};}");
	
	// std::cout << *forStmtInt << std::endl;

	EXPECT_EQ(*newIR, *forStmtInt);
}

TEST(Transform, InterchangeAuto) {

	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. 50 : 1) { \
			for(decl int<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	LoopInterchange li(0, 1);
	NodePtr newIR = li.apply(forStmt);

	NodeManager mgr2;
	parse::IRParser parser2(mgr2);
	auto forStmtInt = parser2.parseStatement("\
		{for(decl int<4>:j = 5 .. (24+1) : 1) { \
			for(decl int<4>:i = 10 .. (49+1) : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		};}");
	
	// std::cout << *forStmtInt << std::endl;

	EXPECT_EQ(*newIR, *forStmtInt);
}

TEST(Transform, StripMiningAuto) {

	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace insieme::transform::pattern;
	using insieme::transform::pattern::any;

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto forStmt = static_pointer_cast<const ForStmt>( parser.parseStatement("\
		for(decl int<4>:i = 10 .. 50 : 1) { \
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i)); \
			for(decl int<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j))); \
			}; \
		}") );
	// std::cout << *forStmt << std::endl;
	scop::mark(forStmt);

	LoopStripMining li(1, 7);
	NodePtr newIR = li.apply(forStmt);

	std::cout << *newIR << std::endl;
	EXPECT_EQ( "{for(int<4> v1 = 10 .. int.add(49, 1) : 1) {array.ref.elem.1D(v2, v1); for(int<4> v2 = 7 .. int.add(24, 1) : 7) {for(int<4> v3 = v2 .. int.add(ite(int.lt(24, int.add(v2, 7)), bind(){rec v6.{v6=fun() {return 24;}}()}, bind(){rec v5.{v5=fun(int<4> v4) {return int.add(v4, 7);}}(v2)}), 1) : 1) {array.ref.elem.1D(v2, int.add(v1, v3));};};};}", newIR->toString() );
	// std::cout << *forStmtInt << std::endl;
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
			for(decl int<4>:i = 10 .. 50 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i)); \
			};\
			for(decl int<4>:j = 5 .. 25 : 1) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, j)); \
			}; \
		}");
	// std::cout << *stmt << std::endl;
	scop::mark(stmt);

	LoopFusion lf(0,1);
	NodePtr newIR = lf.apply(stmt);

	// std::cout << *newIR << std::endl;
	EXPECT_EQ( "{for(int<4> v1 = 5 .. int.add(9, 1) : 1) {array.ref.elem.1D(v2, v1);}; for(int<4> v2 = 10 .. int.add(24, 1) : 1) {array.ref.elem.1D(v2, v2); array.ref.elem.1D(v2, v2);}; for(int<4> v3 = 25 .. int.add(49, 1) : 1) {array.ref.elem.1D(v2, v3);};}", newIR->toString() );
	// std::cout << *forStmtInt << std::endl;
}

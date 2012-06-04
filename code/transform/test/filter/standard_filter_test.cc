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

#include "insieme/transform/filter/standard_filter.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/parser/ir_parse.h"

namespace insieme {
namespace transform {
namespace filter {


	TEST(TargetFilter, OutermostLoops) {

		core::NodeManager manager;
		core::NodePtr node = core::parse::parseStatement(manager,""
			"for(decl uint<4>:i = 10 .. 50 : 1) {"
			"	for(decl uint<4>:j = 3 .. 25 : 1) {"
			"		for(decl uint<4>:k = 2 .. 100 : 1) {"
			"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
			"		};"
			"	};"
			"}");

		EXPECT_TRUE(node);

		auto isFor = [](const core::NodeAddress& cur) { return cur->getNodeType() == core::NT_ForStmt; };

		// get outermost loop filter
		TargetFilter filter = outermostLoops();

		auto res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(core::NodeAddress(node)), res);


		// try multiple outermost for loops
		node = core::parse::parseStatement(manager,"{"
			"10;"
			"for(decl uint<4>:i = 10 .. 50 : 1) {"
			"	for(decl uint<4>:j = 3 .. 25 : 1) {"
			"		for(decl uint<4>:k = 2 .. 100 : 1) {"
			"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
			"		};"
			"	};"
			"};"
			"12;"
			"for(decl uint<4>:i = 10 .. 50 : 1) {"
			"	for(decl uint<4>:j = 3 .. 25 : 1) {"
			"		for(decl uint<4>:k = 2 .. 100 : 1) {"
			"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
			"		};"
			"	};"
			"};"
		"}");

		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		auto root = core::NodeAddress(node);
		EXPECT_EQ(toVector(root.getAddressOfChild(1), root.getAddressOfChild(3)), res);

	}


	TEST(TargetFilter, InnermostLoops) {

		core::NodeManager manager;
		core::NodePtr node = core::parse::parseStatement(manager,"{"
				"10;"
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	for(decl uint<4>:j = 3 .. 25 : 1) {"
				"		for(decl uint<4>:k = 2 .. 100 : 1) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		};"
				"	};"
				"	for(decl uint<4>:k = 2 .. 100 : 1) {"
				"		(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"	};"
				"};"
				"12;"
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	for(decl uint<4>:j = 3 .. 25 : 1) {"
				"		for(decl uint<4>:k = 2 .. 100 : 1) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		};"
				"	};"
				"};"
			"}");

		EXPECT_TRUE(node);

		auto root = core::NodeAddress(node);
		auto isFor = [](const core::NodeAddress& cur) { return cur->getNodeType() == core::NT_ForStmt; };

		// try various innermost loop levels
		TargetFilter filter = innermostLoops();
		auto res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(
				root.getAddressOfChild(1,3,0,3,0),
				root.getAddressOfChild(1,3,1),
				root.getAddressOfChild(3,3,0,3,0)
			), res);

		// try various innermost loop levels
		filter = innermostLoops(2);
		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(
				root.getAddressOfChild(1,3,0),
				root.getAddressOfChild(3,3,0)
			), res);

		// try various innermost loop levels
		filter = innermostLoops(3);
		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector(
				root.getAddressOfChild(1),
				root.getAddressOfChild(3)
			), res);

		// try various innermost loop levels
		filter = innermostLoops(4);
		res = filter(node);
		EXPECT_TRUE(::all(res, isFor));
		EXPECT_EQ(toVector<core::NodeAddress>(), res);

	}


	TEST(TargetFilter, LoopPicker) {

		core::NodeManager manager;
		core::NodePtr node = core::parse::parseStatement(manager,"{"
				"10;"
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	for(decl uint<4>:j = 3 .. 25 : 1) {"
				"		for(decl uint<4>:k = 2 .. 100 : 1) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		};"
				"	};"
				"	for(decl uint<4>:k = 2 .. 100 : 1) {"
				"		(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"	};"
				"};"
				"12;"
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	for(decl uint<4>:j = 3 .. 25 : 1) {"
				"		for(decl uint<4>:k = 2 .. 100 : 1) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		};"
				"	};"
				"};"
			"}");

		EXPECT_TRUE(node);

		auto root = core::NodeAddress(node);

		// extract addresses of loops
		auto for0   = root.getAddressOfChild(1);
		auto for00  = root.getAddressOfChild(1,3,0);
		auto for000 = root.getAddressOfChild(1,3,0,3,0);
		auto for01  = root.getAddressOfChild(1,3,1);
		auto for1   = root.getAddressOfChild(3);
		auto for10  = root.getAddressOfChild(3,3,0);
		auto for100 = root.getAddressOfChild(3,3,0,3,0);


		// check loop picker
		EXPECT_EQ(toVector(for0),   pickLoop(0)(root));
		EXPECT_EQ(toVector(for00),  pickLoop(0,0)(root));
		EXPECT_EQ(toVector(for000), pickLoop(0,0,0)(root));
		EXPECT_EQ(toVector(for01),  pickLoop(0,1)(root));
		EXPECT_EQ(toVector(for1),   pickLoop(1)(root));
		EXPECT_EQ(toVector(for10),  pickLoop(1,0)(root));
		EXPECT_EQ(toVector(for100), pickLoop(1,0,0)(root));

		EXPECT_TRUE(pickLoop(0,0,1)(root).empty());
		EXPECT_TRUE(pickLoop(1,0,1)(root).empty());

	}

	TEST(TargetFilter, AddressPicker) {

		core::NodeManager manager;
		core::NodePtr node = core::parse::parseStatement(manager,"{"
				"10;"
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	for(decl uint<4>:j = 3 .. 25 : 1) {"
				"		for(decl uint<4>:k = 2 .. 100 : 1) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		};"
				"	};"
				"	for(decl uint<4>:k = 2 .. 100 : 1) {"
				"		(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"	};"
				"};"
				"12;"
				"for(decl uint<4>:i = 10 .. 50 : 1) {"
				"	for(decl uint<4>:j = 3 .. 25 : 1) {"
				"		for(decl uint<4>:k = 2 .. 100 : 1) {"
				"			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (i+j)));"
				"		};"
				"	};"
				"};"
			"}");

		EXPECT_TRUE(node);

		auto root = core::NodeAddress(node);

		// extract addresses of loops
		auto for0   = root.getAddressOfChild(1);
		auto for00  = root.getAddressOfChild(1,3,0);
		auto for000 = root.getAddressOfChild(1,3,0,3,0);
		auto for01  = root.getAddressOfChild(1,3,1);
		auto for1   = root.getAddressOfChild(3);
		auto for10  = root.getAddressOfChild(3,3,0);
		auto for100 = root.getAddressOfChild(3,3,0,3,0);

		core::NodeAddress decl0  = for0.as<core::ForStmtAddress>()->getDeclaration();
		core::NodeAddress decl100  = for100.as<core::ForStmtAddress>()->getDeclaration();


		// check loop picker
		EXPECT_EQ(toVector(for0),   pickRelative(for0)(root));
		EXPECT_EQ(toVector(for00),  pickRelative(for00)(root));
		EXPECT_EQ(toVector(for000), pickRelative(for000)(root));
		EXPECT_EQ(toVector(for01),  pickRelative(for01)(root));
		EXPECT_EQ(toVector(for1),   pickRelative(for1)(root));
		EXPECT_EQ(toVector(for10),  pickRelative(for10)(root));
		EXPECT_EQ(toVector(for100), pickRelative(for100)(root));
		EXPECT_EQ(toVector(decl0), pickRelative(decl0)(root));
		EXPECT_EQ(toVector(decl100), pickRelative(decl100)(root));

		// some applications which should not find anything
		EXPECT_TRUE(pickLoop(for0)(for0).empty());
		EXPECT_TRUE(pickLoop(for00)(for1).empty());

	}

} // end namespace filter
} // end namespace transform
} // end namespace insieme



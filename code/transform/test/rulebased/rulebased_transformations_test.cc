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

#include "insieme/transform/rulebased/transformations.h"
#include "insieme/transform/sequential/constant_folding.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace transform {
namespace rulebased {



	TEST(Transformations, CompoundElimination) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::StatementPtr one = builder.intLit(1);
		core::StatementPtr two = builder.intLit(2);

		CompoundElimination trans(parameter::emptyValue);


		core::StatementPtr stmt = builder.compoundStmt(builder.compoundStmt(one));
		EXPECT_EQ("{{1;};}", toString(*stmt));
		EXPECT_EQ("{1;}", toString(*trans.apply(core::NodeAddress(stmt))));

		stmt = builder.compoundStmt(
				one, two,
				builder.compoundStmt(),
				two
		);

		EXPECT_EQ("{1; 2; {}; 2;}", toString(*stmt));
		EXPECT_EQ("{1; 2; 2;}", toString(*trans.apply(core::NodeAddress(stmt))));

	}


	TEST(Transformations, LoopUnrolling) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.parseStmt(
			"for(int<4> i = 10 .. 50 : 2) { "
			"	i; "
			"}").as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);
		EXPECT_TRUE(core::checks::check(forStmt).empty()) << core::checks::check(forStmt);

		LoopUnrolling trans(parameter::makeValue<unsigned>(4));
		auto transformed = trans.apply(core::NodeAddress(forStmt));
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << res;

		// check transformed code
		// Old comparison - without simplification
//		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = 10 .. (10+(((((((50-10)-1)/2)+1)/4)*2)*4)) : (2*4))");
//		EXPECT_PRED2(containsSubString, res, "v1+(2*0)");
//		EXPECT_PRED2(containsSubString, res, "v1+(2*1)");
//		EXPECT_PRED2(containsSubString, res, "v1+(2*2)");
//		EXPECT_PRED2(containsSubString, res, "v1+(2*3)");
//		EXPECT_PRED2(notContainsSubString, res, "v1+(1*4)");
//
//		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = (10+(((((((50-10)-1)/2)+1)/4)*2)*4)) .. 50 : 2)");

		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = 10 .. 50 : 8)");
		EXPECT_PRED2(containsSubString, res, "v1;");
		EXPECT_PRED2(containsSubString, res, "v1+2");
		EXPECT_PRED2(containsSubString, res, "v1+4");
		EXPECT_PRED2(containsSubString, res, "v1+6");
		EXPECT_PRED2(notContainsSubString, res, "v1+3");

		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = 50 .. 50 : 2)");

//		auto list = core::checks::check(transformed);
//		EXPECT_TRUE(list.empty()) << list;

	}


	TEST(Transformations, LoopUnrolling_LargeUnrolleFactor) {

		// Try unrolling a loop like for( i = 0 .. 5 : 1 ) { ... } with a factor of 32 produces incorrect code

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.parseStmt("for(int<4> i = 0 .. 5) { i; }");

		EXPECT_TRUE(forStmt);

		TransformationPtr transform = makeLoopUnrolling(10);
		auto transformed = sequential::foldConstants(manager, transform->apply(forStmt));
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

		// check transformed code
		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = 0 .. 0 : 10)");
		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = 0 .. 5 : 1)");

		auto list = core::checks::check(transformed);
		EXPECT_TRUE(list.empty()) << list;
	}


	TEST(Transformations, TotalLoopUnrolling) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.parseStmt(
			"for(int<4> i = 10 .. 50 : 2) { "
			"	i; "
			"}").as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		TotalLoopUnrolling trans;
		auto transformed = trans.apply(core::NodeAddress(forStmt)).getAddressedNode();
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

		// check transformed code
		vector<core::StatementPtr> stmts;
		for(int i =10; i<50; i+=2) {
			stmts.push_back(builder.intLit(i));
		}
		EXPECT_EQ(builder.compoundStmt(stmts), transformed);

		auto list = core::checks::check(transformed);
		EXPECT_TRUE(list.empty()) << list;
	}

	TEST(Transformations, TotalLoopUnrollingVariableBoundary) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		std::map<std::string, core::NodePtr> symbols;
		symbols["x"] = builder.variable(builder.parseType("int<4>"));

		auto forStmt = builder.parseStmt(
			"for(int<4> i = 10+x .. 50+x : 2) { "
			"	i; "
			"}", symbols).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		TotalLoopUnrolling trans;
		auto transformed = core::analysis::normalize(trans.apply(core::NodeAddress(forStmt)).getAddressedNode());
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << res;

		// check transformed code
		vector<core::StatementPtr> stmts;
		for(int i =10; i<50; i+=2) {
			stmts.push_back(builder.add(builder.variable(manager.getLangBasic().getInt4(),1), builder.intLit(i)));
		}
		EXPECT_EQ(builder.compoundStmt(stmts), transformed);

		auto list = core::checks::check(transformed);
		EXPECT_TRUE(list.empty()) << list;

		symbols["y"] = builder.variable(builder.parseType("int<4>"));

		// something that should fail
		forStmt = builder.parseStmt(
			"for(int<4> i = 10+x .. 50+y : 2) { "
			"	i; "
			"}", symbols).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);
		EXPECT_THROW(trans.apply(core::NodeAddress(forStmt)), InvalidTargetException);

		forStmt = builder.parseStmt(
			"for(int<4> i = 10+x .. 50+x : 2*x) { "
			"	i; "
			"}", symbols).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);
		EXPECT_THROW(trans.apply(core::NodeAddress(forStmt)), InvalidTargetException);
	}

	TEST(Transformations, SimpleLoopTiling2D) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.normalize(builder.parseStmt(
			"for(int<4> i = 0 .. 50 : 2) {"
			"	for(int<4> j = 10 .. 80 : 3) {"
			"		i+j;"
			"	}"
			"}")).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		SimpleLoopTiling2D trans(parameter::combineValues(10u,15u));
		auto transformed = builder.normalize(trans.apply(core::NodeAddress(forStmt)).getAddressedNode());
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << res;

		EXPECT_TRUE(core::checks::check(transformed).empty()) << core::checks::check(transformed);

		EXPECT_PRED2(containsSubString, res, "v0 = 0 .. 50 : (10*2)");
		EXPECT_PRED2(containsSubString, res, "v1 = 10 .. 80 : (15*3)");
		EXPECT_PRED2(containsSubString, res, "v2 = v0 .. select((v0+(10*2)), 50, int.lt) : 2");
		EXPECT_PRED2(containsSubString, res, "v3 = v1 .. select((v1+(15*3)), 80, int.lt) : 3");
		EXPECT_PRED2(containsSubString, res, "v2+v3");

		EXPECT_EQ(vector<core::checks::Message>(),  core::checks::check(transformed).getAll());

	}

	TEST(Transformations, SimpleLoopTiling2D_Extended) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.normalize(builder.parseStmt(
			"for(int<4> i = 0 .. 50 : 2) {"
			"	auto x = i;"
			"	for(int<4> j = 10 .. 80 : 3) {"
			"		i + j + x;"
			"	}"
			"}")).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		SimpleLoopTiling2D trans(parameter::combineValues(10u,15u));
		auto transformed = builder.normalize(trans.apply(core::NodeAddress(forStmt)).getAddressedNode());
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << "Before:\n";
//		std::cout << dumpPretty(forStmt);
//		std::cout << "\n";
//
//		std::cout << "After:\n";
//		std::cout << res;
//		std::cout << "\n";

		EXPECT_TRUE(core::checks::check(transformed).empty()) << core::checks::check(transformed);

		EXPECT_PRED2(containsSubString, res, "v0 = 0 .. 50 : (10*2)");
		EXPECT_PRED2(containsSubString, res, "v1 = 10 .. 80 : (15*3)");
		EXPECT_PRED2(containsSubString, res, "v2 = v0 .. select((v0+(10*2)), 50, int.lt) : 2");
		EXPECT_PRED2(containsSubString, res, "v4 = v1 .. select((v1+(15*3)), 80, int.lt) : 3");
		EXPECT_PRED2(containsSubString, res, "decl int<4> v3 = v2;");
		EXPECT_PRED2(containsSubString, res, "v2+v4");

		EXPECT_EQ(vector<core::checks::Message>(),  core::checks::check(transformed).getAll());

	}

	TEST(Transformations, SimpleLoopTiling3D) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.normalize(builder.parseStmt(
			"for(int<4> i = 0 .. 50 : 2) {"
			"	for(int<4> j = 10 .. 80 : 3) {"
			"		for(int<4> k = 5 .. 20 : 4) {"
			"			i+j+k;"
			"		}"
			"	}"
			"}")).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		SimpleLoopTiling3D trans(parameter::combineValues(10u,15u,12u));
		auto transformed = builder.normalize(trans.apply(core::NodeAddress(forStmt)).getAddressedNode());
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << res;

		EXPECT_TRUE(core::checks::check(transformed).empty()) << core::checks::check(transformed);

		EXPECT_PRED2(containsSubString, res, "v0 = 0 .. 50 : (10*2)");
		EXPECT_PRED2(containsSubString, res, "v1 = 10 .. 80 : (15*3)");
		EXPECT_PRED2(containsSubString, res, "v2 = 5 .. 20 : (12*4)");
		EXPECT_PRED2(containsSubString, res, "v3 = v0 .. select((v0+(10*2)), 50, int.lt) : 2");
		EXPECT_PRED2(containsSubString, res, "v4 = v1 .. select((v1+(15*3)), 80, int.lt) : 3");
		EXPECT_PRED2(containsSubString, res, "v5 = v2 .. select((v2+(12*4)), 20, int.lt) : 4");
		EXPECT_PRED2(containsSubString, res, "((v3+v4)+v5)");

		EXPECT_EQ(vector<core::checks::Message>(),  core::checks::check(transformed).getAll());

	}

	TEST(Transformations, SimpleLoopTiling3D_Extended) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto forStmt = builder.normalize(builder.parseStmt(
			"for(int<4> i = 0 .. 50 : 2) {"
			"	auto x = i;"
			"	for(int<4> j = 10 .. 80 : 3) {"
			"		auto y = j;"
			"		i;"
			"		for(int<4> k = 5 .. 20 : 4) {"
			"			i+j+k+x+y;"
			"		}"
			"		y;"
			"	}"
			"	x;"
			"}")).as<core::ForStmtPtr>();

		EXPECT_TRUE(forStmt);

		SimpleLoopTiling3D trans(parameter::combineValues(10u,15u,12u));
		auto transformed = builder.normalize(trans.apply(core::NodeAddress(forStmt)).getAddressedNode());
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << "Before:\n";
//		std::cout << dumpPretty(forStmt);
//		std::cout << "\n";
//
//		std::cout << "After:\n";
//		std::cout << res;
//		std::cout << "\n";

		EXPECT_TRUE(core::checks::check(transformed).empty()) << core::checks::check(transformed);

		EXPECT_PRED2(containsSubString, res, "v0 = 0 .. 50 : (10*2)");
		EXPECT_PRED2(containsSubString, res, "v1 = 10 .. 80 : (15*3)");
		EXPECT_PRED2(containsSubString, res, "v2 = 5 .. 20 : (12*4)");
		EXPECT_PRED2(containsSubString, res, "v3 = v0 .. select((v0+(10*2)), 50, int.lt) : 2");
		EXPECT_PRED2(containsSubString, res, "v5 = v1 .. select((v1+(15*3)), 80, int.lt) : 3");
		EXPECT_PRED2(containsSubString, res, "v7 = v2 .. select((v2+(12*4)), 20, int.lt) : 4");
		EXPECT_PRED2(containsSubString, res, "decl int<4> v4 = v3;");
		EXPECT_PRED2(containsSubString, res, "decl int<4> v6 = v5;");
		EXPECT_PRED2(containsSubString, res, "((v3+v5)+v7)");

		EXPECT_EQ(vector<core::checks::Message>(),  core::checks::check(transformed).getAll());

	}

} // end namespace rulebased
} // end namespace transform
} // end namespace insieme



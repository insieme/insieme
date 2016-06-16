/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/utils/logging.h"
#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace core {
namespace checks {

	bool containsMSG(const MessageList& list, const Message& msg) {
		return contains(list.getAll(), msg);
	}

	TEST(MissingReturnStmtCheck, UnitLambda) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> unit {
					var ref<uint<8>,f,f,plain> i = 0u;
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";


		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());
		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_TRUE(checkResult.empty());
	}

	TEST(MissingReturnStmtCheck, SimpleError) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> uint {
					var ref<int<8>,f,f,plain> i = 0u;
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt).getAddressOfChild(0), EC_SEMANTIC_MISSING_RETURN_STMT, "", Message::ERROR));
	}

	TEST(MissingReturnStmtCheck, IfCorrect) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> uint {
					var bool a = true;
					if(a) {
						return 1u;
					} else {
						return 2u;
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	TEST(MissingReturnStmtCheck, IfError) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> uint {
					var bool a = true;
					if(a) {
						return 1u;
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt).getAddressOfChild(0), EC_SEMANTIC_MISSING_RETURN_STMT, "", Message::ERROR));
	}

	TEST(MissingReturnStmtCheck, WhileCorrect) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> uint {
					var bool a = true;
					while(true) {
						if(a) { return 1u; }
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	TEST(MissingReturnStmtCheck, WhileError) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				(b : bool) -> uint {
					var bool a = true;
					while(b) {
						if(a) { return 1u; }
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt).getAddressOfChild(0), EC_SEMANTIC_MISSING_RETURN_STMT, "", Message::ERROR));
	}

	TEST(MissingReturnStmtCheck, Throw) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> uint {
					throw 5;
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	TEST(MissingReturnStmtCheck, SwitchCorrect) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> int<4> {
					var ref<int<4>> a;
					switch(*a) {
					case 0: { return 5; }
					case 1: { return 10; }
					default: { throw "Ugh"; }
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	TEST(MissingReturnStmtCheck, SwitchError) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> int<4> {
					var ref<int<4>> a;
					switch(*a) {
					case 0: { return 5; }
					case 1: { 10; }
					default: { throw "Ugh"; }
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt).getAddressOfChild(0), EC_SEMANTIC_MISSING_RETURN_STMT, "", Message::ERROR));
	}

	TEST(MissingReturnStmtCheck, SwitchErrorDefaultMissing) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> int<4> {
					var ref<int<4>> a;
					switch(*a) {
					case 0: { return 5; }
					case 1: { return 10; }
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt).getAddressOfChild(0), EC_SEMANTIC_MISSING_RETURN_STMT, "", Message::ERROR));
	}

	TEST(MissingReturnStmtCheck, SwitchCorrectInLoop) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			alias uint = uint<8>;
			{
				() -> int<4> {
					var ref<int<4>> a;
					while(true) {
						switch(*a) {
						case 0: { return 5; }
						case 1: { 10; }
						default: { "Ugh"; }
						}
					}
				};
			}
			)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	TEST(ValidInitExprMemLocationCheck, Valid) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			def struct X { a: int<4>; };
			{
				<ref<X>>{ 5 };
				<ref<X>>(ref_temp(type_lit(X))){ 5 };
				var ref<X> v0 = <ref<X>>(v0){ 5 };
				<ref<X>>(v0){ 5 };
				<ref<X>>(lit("bla":ref<X>)){ 5 };
			})1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr validInitExprMemLocationCheck = makeRecursive(make_check<ValidInitExprMemLocationCheck>());

		auto checkResult = check(stmt, validInitExprMemLocationCheck);
		EXPECT_TRUE(checkResult.empty());
		EXPECT_EQ(toString(checkResult), "[]");
	}

	TEST(ValidInitExprMemLocationCheck, Invalid) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
			def struct X { a: int<4>; };
			{
				<ref<X>>(ref_new(type_lit(X))){ 5 };
				<ref<X>>(5+5){ 5 };
			})1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr validInitExprMemLocationCheck = makeRecursive(make_check<ValidInitExprMemLocationCheck>());

		auto checkResult = check(stmt, validInitExprMemLocationCheck);
		EXPECT_FALSE(checkResult.empty());
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt).getAddressOfChild(1), EC_SEMANTIC_INVALID_INIT_MEMLOC, "", Message::ERROR));
	}

} // end namespace checks
} // end namespace core
} // end namespace insieme

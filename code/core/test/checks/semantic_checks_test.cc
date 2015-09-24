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

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/utils/logging.h"
#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace core {
namespace checks {

	TEST(ScalarArrayIndexRangeCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		{
			StatementPtr stmt_err = builder.normalize(builder.parseStmt(R"1N5P1RE(
            {
                let uint = uint<8>;
                lambda () -> unit { 
                    decl ref<uint<8>,f,f> i = var(0u); 
                    lambda (ref<array<uint<8>,inf>,f,f> arr) -> unit { 
                        decl uint<8> b = 1u; 
                        arr[b]; 
                    } (ref_scalar_to_ref_array(i)); 
                 };
			}
            )1N5P1RE"));
			auto addrlist = builder.parseAddressesStatement(R"1N5P1RE(
            {
                let uint = uint<8>;
                lambda () -> unit { 
                    decl ref<uint<8>,f,f> i = var(0u); 
                    lambda (ref<array<uint<8>,inf>,f,f> arr) -> unit { 
                        decl uint<8> b = 1u; 
                        $ arr[b] $; 
                    } (ref_scalar_to_ref_array(i)); 
                 };
			}
            )1N5P1RE");

			EXPECT_TRUE(stmt_err) << "parsing error";
			EXPECT_EQ(addrlist.size(), 1) << "parsing error";

			CheckPtr scalarArrayIndexRangeCheck = makeRecursive(make_check<ScalarArrayIndexRangeCheck>());

			// NodeAddress errorAdr = NodeAddress(stmt_err).getAddressOfChild(2,0,1,2,1,1,2,0,1,2,1);
			NodeAddress errorAdr = addrlist[0];

			EXPECT_EQ("0-0-2-0-1-2-1-1-2-0-1-2-1", toString(errorAdr));
			EXPECT_TRUE(dynamic_pointer_cast<CallExprPtr>(errorAdr.getAddressedNode())) << errorAdr.getAddressedNode();

			ASSERT_FALSE(check(stmt_err, scalarArrayIndexRangeCheck).empty());

			EXPECT_EQ(toString(check(stmt_err, scalarArrayIndexRangeCheck)[0]),
			          toString(Message(errorAdr, EC_SEMANTIC_ARRAY_INDEX_OUT_OF_RANGE,
			                           "Potentially unsafe indexing of single-element array v1 using formula v2", Message::WARNING)));
		}

		{
			StatementPtr stmt_pass = builder.parseExpr("let uint = uint<8>;"
			                                           "lambda () -> unit { "
			                                           "	decl ref<uint<8>,f,f> i = var(0u); "
			                                           "	lambda (ref<array<uint<8>,inf>,f,f> arr) -> unit { "
			                                           "		decl uint<8> b = 1; "
			                                           "		arr[0u]; "
			                                           "	} (ref_scalar_to_ref_array(i)); "
			                                           "}");
			EXPECT_TRUE(stmt_pass) << "parsing error";

			CheckPtr scalarArrayIndexRangeCheck = makeRecursive(make_check<ScalarArrayIndexRangeCheck>());
			EXPECT_TRUE(check(stmt_pass, scalarArrayIndexRangeCheck).empty());
		}
	}

	TEST(MissingReturnStmtCheck, UnitLambda) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
	{
		let uint = uint<8>;
		lambda () -> unit { 
			decl ref<uint<8>,f,f> i = var(0u);
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
	{
		let uint = uint<8>;
		lambda () -> uint { 
			decl ref<int<8>,f,f> i = var(0u);
		};
	}
	)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_EQ(toString(checkResult[0]),
		          "ERROR:   [00047] - SEMANTIC / MISSING_RETURN_STMT @ (0-0) - MSG: Not all control paths of non-unit lambdaExpr return a value.");
	}

	TEST(MissingReturnStmtCheck, IfCorrect) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
	{
		let uint = uint<8>;
		lambda () -> uint { 
			decl bool a = var(true);
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
	{
		let uint = uint<8>;
		lambda () -> uint { 
			decl bool a = var(true);
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
		EXPECT_EQ(toString(checkResult[0]),
		          "ERROR:   [00047] - SEMANTIC / MISSING_RETURN_STMT @ (0-0) - MSG: Not all control paths of non-unit lambdaExpr return a value.");
	}

	TEST(MissingReturnStmtCheck, WhileCorrect) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
	{
		let uint = uint<8>;
		lambda () -> uint {
			decl bool a = var(true);			
			while(true) {
				if(a) return 1u;
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
	{
		let uint = uint<8>;
		lambda (bool b) -> uint {
			decl bool a = var(true);			
			while(b) {
				if(a) return 1u;
			}
		};
	}
	)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_EQ(toString(checkResult[0]),
		          "ERROR:   [00047] - SEMANTIC / MISSING_RETURN_STMT @ (0-0) - MSG: Not all control paths of non-unit lambdaExpr return a value.");
	}

	TEST(MissingReturnStmtCheck, Throw) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
	{
		let uint = uint<8>;
		lambda () -> uint {
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
	{
		let uint = uint<8>;
		lambda () -> uint {
			decl int<4> a;			
			switch(a) {
			case 0: return 5;
			case 1: return 10;
			default: throw "Ugh";
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
	{
		let uint = uint<8>;
		lambda () -> uint {
			decl int<4> a;			
			switch(a) {
			case 0: return 5;
			case 1: 10;
			default: throw "Ugh";
			}
		};
	}
	)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_EQ(toString(checkResult[0]),
		          "ERROR:   [00047] - SEMANTIC / MISSING_RETURN_STMT @ (0-0) - MSG: Not all control paths of non-unit lambdaExpr return a value.");
	}

	TEST(MissingReturnStmtCheck, SwitchErrorDefaultMissing) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
	{
		let uint = uint<8>;
		lambda () -> uint {
			decl int<4> a;			
			switch(a) {
			case 0: return 5;
			case 1: return 10;
			}
		};
	}
	)1N5P1RE");
		EXPECT_TRUE(stmt) << "parsing error";

		CheckPtr missingReturnStmtCheck = makeRecursive(make_check<MissingReturnStmtCheck>());

		auto checkResult = check(stmt, missingReturnStmtCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_EQ(toString(checkResult[0]),
		          "ERROR:   [00047] - SEMANTIC / MISSING_RETURN_STMT @ (0-0) - MSG: Not all control paths of non-unit lambdaExpr return a value.");
	}

	TEST(MissingReturnStmtCheck, SwitchCorrectInLoop) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto stmt = builder.parseStmt(R"1N5P1RE(
	{
		let uint = uint<8>;
		lambda () -> uint {
			decl int<4> a;
			while(true) {
				switch(a) {
				case 0: return 5;
				case 1: 10;
				default: "Ugh";
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

	TEST(ArrayCreateArgumentCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		{
			StatementPtr stmt_ok = builder.normalize(builder.parseStmt(R"1N5P1RE(
            {
                let uint = uint<8>;
                lambda () -> unit {
					decl ref<array<int<4>,3>,f,f> v0 = var(array_create(lit(3), [0,1,2]));
				};
			}
            )1N5P1RE"));
			EXPECT_TRUE(stmt_ok) << "parsing error";
			CheckPtr arrayCreateArgumentCheck = makeRecursive(make_check<ArrayCreateArgumentCheck>());
			auto checkResult = check(stmt_ok, arrayCreateArgumentCheck);
			EXPECT_TRUE(checkResult.empty());
			EXPECT_EQ(toString(checkResult), "[]");
		}

		{
			StatementPtr stmt_err = builder.normalize(builder.parseStmt(R"1N5P1RE(
            {
                let uint = uint<8>;
                lambda () -> unit {
					decl auto list = var([0,1,2]);
					decl ref<array<int<4>,3>,f,f> v0 = var(array_create(lit(3), *list));
				};
			}
            )1N5P1RE"));
			EXPECT_TRUE(stmt_err) << "parsing error";
			CheckPtr arrayCreateArgumentCheck = makeRecursive(make_check<ArrayCreateArgumentCheck>());
			auto checkResult = check(stmt_err, arrayCreateArgumentCheck);
			EXPECT_EQ(checkResult.size(), 1);
			EXPECT_EQ(toString(checkResult[0]),
					"ERROR:   [00044] - SEMANTIC / ARRAY_CREATE_INVALID_ARGUMENT @ (0-0-2-0-1-2-1-1-2) - MSG: Invalid initializer argument in array_create call.");
		}
	}

} // end namespace checks
} // end namespace core
} // end namespace insieme

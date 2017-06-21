/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/annotations/default_delete.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/utils/logging.h"
#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace core {
namespace checks {

	bool containsMSG(const MessageList& list, const Message& msg) {
		return contains(list.getAll(), msg);
	}

	TEST(DefaultedDeletedPreTUMarkerCheck, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		CheckPtr defaultedDeletedMarkerCheck = makeRecursive(make_check<DefaultedDeletedPreTUMarkerCheck>());

		auto lit = builder.intLit(5);
		auto checkResult = check(lit, defaultedDeletedMarkerCheck);
		EXPECT_TRUE(checkResult.empty());

		lit = builder.intLit(6);
		annotations::markDefaultedPreTU(lit);
		checkResult = check(lit, defaultedDeletedMarkerCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(lit), EC_SEMANTIC_DEFAULTED_BODY_MARKER, "", Message::ERROR));

		lit = builder.intLit(7);
		annotations::markDeletedPreTU(lit);
		checkResult = check(lit, defaultedDeletedMarkerCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(lit), EC_SEMANTIC_DELETED_BODY_MARKER, "", Message::ERROR));

		auto stmt = builder.getNoOp();
		checkResult = check(stmt, defaultedDeletedMarkerCheck);
		EXPECT_TRUE(checkResult.empty());

		stmt = builder.getDefaultedBodyPreTUMarker();
		checkResult = check(stmt, defaultedDeletedMarkerCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt), EC_SEMANTIC_DEFAULTED_BODY_MARKER, "", Message::ERROR));

		stmt = builder.getDeletedBodyPreTUMarker();
		checkResult = check(stmt, defaultedDeletedMarkerCheck);
		EXPECT_EQ(checkResult.size(), 1);
		EXPECT_PRED2(containsMSG, checkResult, Message(NodeAddress(stmt), EC_SEMANTIC_DELETED_BODY_MARKER, "", Message::ERROR));
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

	TEST(ValidMaterializingDeclarationCheck, Valid) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto mkDecl = [&](const std::string& declType, const std::string& initExpr) {
			return builder.declaration(
					builder.normalize(builder.parseType(declType)),
					builder.normalize(builder.parseExpr(initExpr))
			);
		};

		CheckPtr declCheck = makeRecursive(make_check<ValidMaterializingDeclarationCheck>());
		std::string structDef;
		core::DeclarationPtr decl;

		// -- some positive checks --

		// check non-materializing declarations
		decl = mkDecl("int<4>", "1");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("bool", "true");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>>", "lit(\"X\":ref<int<4>>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>,f,f,plain>", "lit(\"X\":ref<int<4>,f,f,plain>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>,t,f,plain>", "lit(\"X\":ref<int<4>,f,f,plain>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>,f,f,cpp_ref>", "lit(\"X\":ref<int<4>,f,f,cpp_ref>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>,t,f,cpp_ref>", "lit(\"X\":ref<int<4>,f,f,cpp_ref>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>,f,f,cpp_rref>", "lit(\"X\":ref<int<4>,f,f,cpp_rref>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<'s>,f,f,plain>", "lit(\"X\":ref<int<'s>,f,f,plain>)");
		EXPECT_FALSE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);


		// check materializing declarations

		// - primitive initialization -
		decl = mkDecl("ref<int<4>,f,f,plain>", "4");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<int<4>,f,f,plain>", "lit(\"X\":int<4>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - init expression -
		decl = mkDecl("ref<int<4>,f,f,plain>", "<ref<int<4>>>(ref_decl(type_lit(ref<int<4>>))){ 4 }");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - explicit constructor -
		decl = mkDecl("ref<A,f,f,plain>", "lit(\"X\":A::())(ref_decl(type_lit(ref<A>)))");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - implicit copy constructor -
		structDef = "def struct A {}; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "ref_cast(lit(\"X\" : ref<A>),type_lit(t),type_lit(f),type_lit(cpp_ref))");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		structDef = "def struct A {}; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "lit(\"X\" : ref<A,t,f,cpp_ref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - implicit move constructor -
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "lit(\"X\" : ref<A,f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);


		// - implicit constructor call -
		structDef = "def struct A { ctor ( o : int<4>) { } }; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "4");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);


		// - implicit base-type copy construction -
		decl = mkDecl("ref<int<4>,f,f,plain>", "lit(\"X\" : ref<int<4>,t,f,cpp_ref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - implicit copy constructor, where move is deleted -
		structDef = "def struct A { ctor ( o : ref<A,f,f,cpp_rref>) = delete; }; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "lit(\"X\" : ref<A,t,f,cpp_ref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - implicit move constructor, where copy is deleted -
		structDef = "def struct A { ctor ( o : ref<A,t,f,cpp_ref>) = delete; }; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "lit(\"X\" : ref<A,f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);


		// - tuples are implicitly copy-constructable
		decl = mkDecl("ref<(),f,f,plain>", "lit(\"X\" : ref<(),t,f,cpp_ref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<(int<4>),f,f,plain>", "lit(\"X\" : ref<(int<4>),t,f,cpp_ref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - tuples are implicitly move constructable
		decl = mkDecl("ref<(),f,f,plain>", "lit(\"X\" : ref<(),f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<(int<4>),f,f,plain>", "lit(\"X\" : ref<(int<4>),f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - the implicit declaration should also work with type variables
		decl = mkDecl("ref<('a,'b),f,f,plain>", "lit(\"X\" : ref<(int<4>,bool),f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		decl = mkDecl("ref<('ts...),f,f,plain>", "lit(\"X\" : ref<(int<4>,bool),f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_TRUE(check(decl,declCheck).empty()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);


		// -- invalid --

		// - unrelated types -
		decl = mkDecl("ref<int<4>,f,f,plain>", "lit(\"X\":A::())(ref_decl(type_lit(ref<A>)))");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_EQ(1,check(decl,declCheck).size()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - missing constructor -
		structDef = "def struct A { ctor ( o : bool ) { } }; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "4");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_EQ(1,check(decl,declCheck).size()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		structDef = "def struct A { ctor ( o : ref<A,t,f,cpp_ref>) = delete; }; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "lit(\"X\" : ref<A,t,f,cpp_ref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_EQ(1,check(decl,declCheck).size()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		structDef = "def struct A { ctor ( o : ref<A,f,f,cpp_rref>) = delete; }; ";
		decl = mkDecl(structDef + "ref<A,f,f,plain>", structDef + "lit(\"X\" : ref<A,f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_EQ(1,check(decl,declCheck).size()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

		// - type variables must be consistently instantiated
		decl = mkDecl("ref<('a,'a),f,f,plain>", "lit(\"X\" : ref<(int<4>,bool),f,f,cpp_rref>)");
		EXPECT_TRUE(analysis::isMaterializingDecl(decl));
		EXPECT_EQ(1,check(decl,declCheck).size()) << "Declaration:\n" << dumpReadable(decl) << "\nErrors: " << check(decl,declCheck);

	}

} // end namespace checks
} // end namespace core
} // end namespace insieme

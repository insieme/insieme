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

#include "insieme/core/parser2/detail/lexer.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace parser {
namespace detail {

	TEST(Lexer, HelloWorld) {

		auto tokens = lex("Hello World");

		EXPECT_EQ(2u, tokens.size());
		EXPECT_EQ("[(Ident:Hello),(Ident:World)]", toString(tokens));

		// check empty string
		EXPECT_EQ("[]", toString(lex("")));
		EXPECT_EQ("[]", toString(lex("  \t \n")));

//		EXPECT_EQ(testB, parse(manager, "test<12,#c,#inf,12>"));
//		EXPECT_EQ(testC, parse(manager, "test<A,test<A>,12,#inf>"));
	}

	TEST(Lexer, BoolLiterals) {

		// simply try true and false
		EXPECT_EQ("[(BoolLit:true)]", toString(lex("true")));
		EXPECT_EQ("[(BoolLit:true),(BoolLit:false)]", toString(lex("true false")));


	}

	TEST(Lexer, IntLiterals) {

		// two simple cases
		EXPECT_EQ("[(IntLit:12)]", toString(lex("12")));
		EXPECT_EQ("[(IntLit:1),(IntLit:2)]", toString(lex("1 2")));

		// test special cases
		EXPECT_EQ("[(IntLit:0)]", toString(lex("0")));

		EXPECT_EQ("[(IntLit:21u)]", toString(lex("21u")));
		EXPECT_EQ("[(IntLit:21ul)]", toString(lex("21ul")));

		// test hex values
		EXPECT_EQ("[(IntLit:0x012ABcd23)]", toString(lex(" 0x012ABcd23 ")));
		EXPECT_EQ("[(IntLit:0x012ABcd23)]", toString(lex(" 0x012ABcd23 ")));

		// test oct values
		EXPECT_EQ("[(IntLit:001277)]", toString(lex(" 001277 ")));

	}

	TEST(Lexer, RealLiterals) {

		EXPECT_EQ("[(DoubleLit:0.02)]", toString(lex("0.02")));
		EXPECT_EQ("[(FloatLit:0.02f)]", toString(lex("0.02f")));

		EXPECT_EQ("[(DoubleLit:0.02)]", toString(lex("0.02")));
		EXPECT_EQ("[(FloatLit:0.02f)]", toString(lex("0.02f")));

		EXPECT_EQ("[(DoubleLit:120.02)]", toString(lex("120.02")));
		EXPECT_EQ("[(FloatLit:120.02f)]", toString(lex("120.02f")));

	}

	TEST(Lexer, CharLiterals) {

		// two simple cases
		EXPECT_EQ("[(CharLit:'a')]", toString(lex("'a'")));
		EXPECT_EQ("[(CharLit:'a'),(CharLit:'b')]", toString(lex("'a' 'b'")));

		// something complex ...
		EXPECT_EQ(
			"[(CharLit:'a'),(CharLit:'\n'),(Ident:sadfd),(Symbol:'),(Ident:df),(Symbol:'),(CharLit:'d'),(Ident:dad),(Symbol:'),(Ident:d)]",
			toString(lex("'a' '\n' sadfd 'df'  'd' dad'd"))
		);

		// something that isn't anything
		EXPECT_EQ(
			"[(Symbol:'),(Symbol:')]",
			toString(lex("''"))
		);
	}

	TEST(Lexer, StringLiterals) {

		// two simple cases
		EXPECT_EQ("[(StrLit:\"a\")]", toString(lex("\"a\"")));
		EXPECT_EQ("[(StrLit:\"a\"),(StrLit:\"b\")]", toString(lex("\"a\" \"b\"")));

		// something complex ...
		EXPECT_EQ(
			"[(Ident:Some),(StrLit:\"String\"),(Ident:inside)]",
			toString(lex("Some \"String\" inside"))
		);

		// check the empty string
		EXPECT_EQ("[(StrLit:\"\")]",toString(lex("\"\"")));

		// check for escaped \"
		EXPECT_EQ("[(Ident:Some),(StrLit:\"str\\\"ing\"),(Ident:inside)]", toString(lex("Some \"str\\\"ing\" inside")));
	}

	TEST(Lexer, CommentsShort) {
		EXPECT_EQ("[(Ident:a)]", toString(lex("// comment A \n a // comment B \n")));
		EXPECT_EQ("[(Ident:b)]", toString(lex(" // comment A \n b // comment B")));
	}

	TEST(Lexer, CommentsLong) {
		EXPECT_EQ("[(Ident:a)]", toString(lex("/* comment A */ a /* comment B /* */")));
		EXPECT_EQ("[(Ident:b)]", toString(lex("/* comment A */ b /* comment B")));
	}

	TEST(Lexer, NonLiterals) {
		EXPECT_EQ("[(Ident:array),(Symbol:.),(Ident:create),(Symbol:.),(Ident:1d)]", toString(lex("array.create.1d")));
	}

} // end namespace detail
} // end namespace parser2
} // end namespace core
} // end namespace insieme

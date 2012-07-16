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

#include "insieme/core/parser2/simple_parser.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace parser {


	auto accept = [](const Context& cur)->Result {
		return IRBuilder(cur.manager).boolLit(true);
	};


	TEST(SimpleParser, SimpleStringValue) {

		/**
		 * Goal:
		 * 	- build a rule converting a string into a string-value node
		 */

		NodeManager manager;
		IRBuilder builder(manager);

		Grammar grammar = rule(any, [](const Context& cur)->Result {
			return IRBuilder(cur.manager).stringValue(*cur.begin);
		});


		NodePtr res = grammar.match(manager, "hello");

		// check whether result was parsed correctly
		EXPECT_EQ(builder.stringValue("hello"), res);


		// test some other cases
		EXPECT_EQ(builder.stringValue("world"), grammar.match(manager, "world"));
		EXPECT_EQ(builder.stringValue("world"), grammar.match(manager, "   world   \t  "));


		// test some negative cases too
		EXPECT_FALSE(grammar.match(manager, ""));
		EXPECT_FALSE(grammar.match(manager, " "));
		EXPECT_FALSE(grammar.match(manager, "Hello World"));
	}

	TEST(SimpleParser, Literals) {

		/**
		 * Goal:
		 * 	- build 2 rules for integer and floating point literals.
		 * 	- build a grammar covering both alternatives
		 */

		NodeManager manager;
		IRBuilder builder(manager);


		// the rule dealing with integers
		auto a = rule(any, [](const Context& cur)->Result {
			try {
				uint64_t value = utils::numeric_cast<uint64_t>(*cur.begin);
				return IRBuilder(cur.manager).integerLit(value, true);
			} catch (const boost::bad_lexical_cast&) {}
			return false;
		});

		auto b = rule(any, [](const Context& cur)->Result {
			try {
				utils::numeric_cast<double>(*cur.begin);
				return IRBuilder(cur.manager).doubleLit(*cur.begin);
			} catch (const boost::bad_lexical_cast&) {}
			return false;
		});



		Grammar g = a | b;


		NodePtr res;
		EXPECT_FALSE(res = g.match(manager, "hello"));
		EXPECT_TRUE(res = g.match(manager, "12"));
		EXPECT_EQ(builder.integerLit(12, true), res);

		EXPECT_TRUE(res = g.match(manager, "12.2"));
		EXPECT_EQ(builder.doubleLit("12.2"), res);

	}

	TEST(SimpleParser, Symbols) {
			NodeManager manager;
			IRBuilder builder(manager);

			/**
			 * Goal:
			 * 	- Test the correct operation of optional operators
			 */

			Grammar g;

			g = rule(lit("-"), accept);
			EXPECT_TRUE(g.match(manager, "-"));
			EXPECT_FALSE(g.match(manager, ""));

			g = rule(empty, accept);
			EXPECT_FALSE(g.match(manager, "-"));
			EXPECT_TRUE(g.match(manager, ""));
	}

	TEST(SimpleParser, Sequence) {

		/**
		 * Goal:
		 * 	- build a parser matching sequences of terminals
		 */

		NodeManager manager;
		IRBuilder builder(manager);

		auto pattern = rule(seq("+", "-", "+"), [&](const Context& cur)->Result {
			return builder.boolLit(true);
		});

		Grammar g = pattern;

		// test correct pattern first
		EXPECT_TRUE(g.match(manager, "+-+"));

		// test a lot of incorrect patterns
		EXPECT_FALSE(g.match(manager, "+++"));
		EXPECT_FALSE(g.match(manager, "+-"));
		EXPECT_FALSE(g.match(manager, "++"));
		EXPECT_FALSE(g.match(manager, ""));
		EXPECT_FALSE(g.match(manager, "+-+-"));
		EXPECT_FALSE(g.match(manager, "+-++"));

	}

	TEST(SimpleParser, Optional) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * Goal:
		 * 	- Test the correct operation of optional operators
		 */

		Grammar g = rule(opt("-"), accept);

		EXPECT_TRUE(g.match(manager, "-"));
		EXPECT_TRUE(g.match(manager, ""));

		EXPECT_FALSE(g.match(manager, "+"));


		g = rule(seq("+", opt("-"), "+"), accept);

		EXPECT_TRUE(g.match(manager, "++"));
		EXPECT_TRUE(g.match(manager, "+-+"));

		EXPECT_FALSE(g.match(manager, "+"));
		EXPECT_FALSE(g.match(manager, "+-"));
		EXPECT_FALSE(g.match(manager, "+++"));
	}

	TEST(SimpleParser, IntegerExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto num = rule(any, [](const Context& cur)->Result {
			try {
				uint64_t value = utils::numeric_cast<uint64_t>(*cur.begin);
				return IRBuilder(cur.manager).integerLit(value, true);
			} catch (const boost::bad_lexical_cast&) {}
			return false;
		});

		auto add = rule(seq(rec, "+", rec), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr a = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			ExpressionPtr b = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[1]);
			if (!a || !b) return false;
			return IRBuilder(cur.manager).add(a,b);
		});

		auto mul = rule(seq(rec, "*", rec), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr a = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			ExpressionPtr b = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[1]);
			if (!a || !b) return false;
			return IRBuilder(cur.manager).mul(a,b);
		});


		Grammar g = num | add | mul;

//		std::cout << g.match(manager, "1") << "\n";
//		std::cout << g.match(manager, "1+2") << "\n";
//		std::cout << g.match(manager, "1+2*3") << "\n";

		EXPECT_TRUE(g.match(manager, "1"));
		EXPECT_TRUE(g.match(manager, "1+2"));
		EXPECT_TRUE(g.match(manager, "1+2+3"));
		EXPECT_TRUE(g.match(manager, "1+2*3"));

		EXPECT_TRUE(g.match(manager, "1+2*3+4"));
		EXPECT_TRUE(g.match(manager, "1+2*3+4*5+6*7"));

		EXPECT_FALSE(g.match(manager, "1+2-3"));
		EXPECT_FALSE(g.match(manager, "1+2**3"));

	}


//	TEST(SimpleParser, Loops) {
//		NodeManager manager;
//		IRBuilder builder(manager);
//
//		/**
//		 * Goal: one rule including a loop => parse it
//		 */
//
//		auto token = rule(any, [](const Context& cur)->Result {
//			return IRBuilder(cur.manager).stringLit(*cur.begin);
//		});
//
//		auto compound = rule(seq("{", loop(seq(rec, ";")), "}"), [](const Context& cur)->Result {
//			StatementList stmts;
//			for(auto it=cur.getTerms().begin(); it != cur.getTerms().end(); ++it) {
//				StatementPtr stmt = dynamic_pointer_cast<StatementPtr>(*it);
//				if (!stmt) return false;
//				stmts.push_back(stmt);
//			}
//			return IRBuilder(cur.manager).compoundStmt(stmts);
//		});
//
//		EXPECT_EQ("_ => ...", toString(*token));
//		EXPECT_EQ("'{' ( <E> ';' )* '}' => ...", toString(*compound));
//
//		Grammar g = token | compound;
//
//		auto a = builder.stringLit("a");
//		auto b = builder.stringLit("b");
//		auto c = builder.stringLit("c");
//
//		EXPECT_TRUE(g.match(manager, "a"));
//		EXPECT_TRUE(g.match(manager, "b"));
//
//		EXPECT_EQ(a, g.match(manager, "a"));
//		EXPECT_EQ(b, g.match(manager, "b"));
//
//		EXPECT_EQ(builder.compoundStmt(), g.match(manager, "{}"));
//		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a;}"));
//		EXPECT_EQ(builder.compoundStmt(a,b,a), g.match(manager, "{a;b;a;}"));
//		EXPECT_EQ(builder.compoundStmt(a,a,b), g.match(manager, "{a ; a; b; }"));
//
//		// a large example (runtime test)
//		EXPECT_EQ(builder.compoundStmt(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a), g.match(manager, "{a ; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; }"));
//
//		// try nested
//		auto abc = builder.compoundStmt(a,b,c);
//		EXPECT_EQ(builder.compoundStmt(a, b, abc, a, c), g.match(manager, "{ a; b; { a; b; c; }; a; c; }"));
//
//		auto abcab = builder.compoundStmt(a,b,c,a,b);
//		EXPECT_EQ(builder.compoundStmt(a, b, abc, abcab, a, c), g.match(manager, "{ a; b; { a; b; c; }; { a; b; c; a; b; }; a; c; }"));
//
//
//		// some stuff that should not work
//		EXPECT_FALSE(g.match(manager, "{a ; a; a; a; a; a; a; a; a a ; a; a; a; a; a; a; a; }"));
//
//	}

	TEST(SimpleParser, LoopsAndOptionals) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * Goal: one rule including a loop => parse it
		 */

		auto token = rule(any, [](const Context& cur)->Result {
			// the list of terminals
			static const string terminals = "+-*/%=()<>{}[]&|,:;?!~^°'´\\";
			if (contains(terminals, (*cur.begin)[0])) return false;
			return IRBuilder(cur.manager).stringLit(*cur.begin);
		});

		auto compound = rule(seq("{", loop(seq(rec, opt(";"))), "}"), [](const Context& cur)->Result {
			StatementList stmts;
			for(auto it=cur.getTerms().begin(); it != cur.getTerms().end(); ++it) {
				StatementPtr stmt = dynamic_pointer_cast<StatementPtr>(*it);
				if (!stmt) {
					return false;
				}
				stmts.push_back(stmt);
			}
			return IRBuilder(cur.manager).compoundStmt(stmts);
		});

		EXPECT_EQ("_ => ...", toString(*token));
		EXPECT_EQ("'{' ( <E> ( ';' | _e ) )* '}' => ...", toString(*compound));

		Grammar g = token | compound;

		auto a = builder.stringLit("a");
		auto b = builder.stringLit("b");
		auto c = builder.stringLit("c");

//		EXPECT_TRUE(g.match(manager, "a"));
//		EXPECT_TRUE(g.match(manager, "b"));
//
//		EXPECT_EQ(a, g.match(manager, "a"));
//		EXPECT_EQ(b, g.match(manager, "b"));

		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a}"));

//		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a;}"));
//		EXPECT_EQ(builder.compoundStmt(b), g.match(manager, "{b;}"));
//
//		EXPECT_EQ(builder.compoundStmt(a,b), g.match(manager, "{a;b}"));
//		EXPECT_EQ(builder.compoundStmt(a,b), g.match(manager, "{a;b;}"));

//		EXPECT_EQ(builder.compoundStmt(), g.match(manager, "{}"));
//		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a}"));
//		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a;}"));
//		EXPECT_EQ(builder.compoundStmt(a,b,a), g.match(manager, "{a;b;a;}"));
//		EXPECT_EQ(builder.compoundStmt(a,a,b), g.match(manager, "{a ; a; b; }"));
//
//		// a large example (runtime test)
//		EXPECT_EQ(builder.compoundStmt(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a), g.match(manager, "{a ; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; }"));
//
//		// try nested
//		auto abc = builder.compoundStmt(a,b,c);
//		EXPECT_EQ(builder.compoundStmt(a, b, abc, a, c), g.match(manager, "{ a; b; { a; b; c; }; a; c; }"));
//
//		auto abcab = builder.compoundStmt(a,b,c,a,b);
//		EXPECT_EQ(builder.compoundStmt(a, b, abc, abcab, a, c), g.match(manager, "{ a; b; { a; b; c; }; { a; b; c; a; b; }; a; c; }"));
//
//
//		// some stuff that should not work
//		EXPECT_FALSE(g.match(manager, "{a ; a; a; a; a; a; a; a; a a ; a; a; a; a; a; a; a; }"));

	}


//
//	TEST(SimpleParser, IfLang) {
//		NodeManager manager;
//
//		/**
//		 * The goal: support a the following grammar:
//		 *
//		 * E = { (E ;?)* } | if E then E else E | if E then E | a | b | c | E + E | E * E
//		 */
//
//		auto token = rule(any, [](const Context& cur)->Result {
//			return IRBuilder(cur.manager).stringLit(*cur.begin);
//		});
//
//		auto if_then_rule = rule(seq("if", "(", rec, ")", rec), [](const Context& cur)->Result {
//			if (cur.getTerms().size() != 2u) return false;
//			ExpressionPtr condition = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
//			StatementPtr thenPart = dynamic_pointer_cast<StatementPtr>(cur.getTerms()[1]);
//			if (!condition || !thenPart) return false;
//			return IRBuilder(cur.manager).ifStmt(condition, thenPart);
//		});
//
//		auto if_then_else_rule = rule(seq("if", "(", rec, ")", rec, "else", rec), [](const Context& cur)->Result {
//			if (cur.getTerms().size() != 3u) return false;
//			ExpressionPtr condition = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
//			StatementPtr thenPart = dynamic_pointer_cast<StatementPtr>(cur.getTerms()[1]);
//			StatementPtr elsePart = dynamic_pointer_cast<StatementPtr>(cur.getTerms()[2]);
//			if (!condition || !thenPart || !elsePart) return false;
//			return IRBuilder(cur.manager).ifStmt(condition, thenPart, elsePart);
//		});
//
//		auto compound = rule(seq("{", loop(seq(rec, opt(";"))), "}"), [](const Context& cur)->Result {
//			StatementList stmts;
//			for(auto it=cur.getTerms().begin(); it != cur.getTerms().end(); ++it) {
//				StatementPtr stmt = dynamic_pointer_cast<StatementPtr>(*it);
//				if (!stmt) return false;
//				stmts.push_back(stmt);
//			}
//			return IRBuilder(cur.manager).compoundStmt(stmts);
//		});
//
//		EXPECT_EQ("if ( E ) E |-", toString(*if_then_rule));
//		EXPECT_EQ("if ( E ) E else E |-", toString(*if_then_else_rule));
//		EXPECT_EQ("{ ( E ; | _e )* } |-", toString(*compound));
//
//
//		Grammar g = token | if_then_rule | if_then_else_rule | compound;
//
//
//		EXPECT_FALSE(g.match(manager, ""));
//		EXPECT_TRUE(g.match(manager, "{ a; }"));
//
//		EXPECT_TRUE(g.match(manager,
//				"{"
//				"	a;"
//				"	b;"
//				"	if (a) b;"
//				"	if (a) {"
//				"		b;"
//				"	}"
//				"	c;"
//				"}"));
//
////		std::cout << dump::text::TextDump(g.match(manager, ""));
//
//		/**
//		 * To test:
//		 *   - correct parsing
//		 *   - operator priority
//		 *   - error reporting
//		 */
//
//
//
//	}

} // end namespace parser
} // end namespace core
} // end namespace insieme

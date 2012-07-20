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

#include "insieme/core/parser2/parser.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace parser {

	namespace {

		auto accept = [](const Context& cur)->Result {
			return IRBuilder(cur.manager).boolLit(true);
		};

	}

	TEST(Parser, SimpleStringValue) {

		/**
		 * Goal:
		 * 	- build a rule converting a string into a string-value node
		 */

		NodeManager manager;
		IRBuilder builder(manager);

		Grammar grammar = rule(any(), [](const Context& cur)->Result {
			//assert(cur.begin + 1 == cur.end);
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

	TEST(Parser, Literals) {

		/**
		 * Goal:
		 * 	- build 2 rules for integer and floating point literals.
		 * 	- build a grammar covering both alternatives
		 */

		NodeManager manager;
		IRBuilder builder(manager);


		// the rule dealing with integers
		auto a = rule(any(), [](const Context& cur)->Result {
			if (cur.begin->getType() != Token::Int_Literal) return false;
			try {
				int64_t value = utils::numeric_cast<int64_t>(cur.begin->getLexeme());
				return IRBuilder(cur.manager).integerLit(value, true);
			} catch (const boost::bad_lexical_cast&) {}
			return false;
		});

		auto b = rule(any(), [](const Context& cur)->Result {
			if (cur.begin->getType() != Token::Double_Literal) return false;
			return IRBuilder(cur.manager).doubleLit(cur.begin->getLexeme());
		});



		Grammar g(a, b);


		NodePtr res;
		EXPECT_FALSE(res = g.match(manager, "hello"));
		EXPECT_TRUE(res = g.match(manager, "12"));
		EXPECT_EQ(builder.integerLit(12, true), res);

		EXPECT_TRUE(res = g.match(manager, "12.2"));
		EXPECT_EQ(builder.doubleLit("12.2"), res);

	}

	TEST(Parser, Symbols) {
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

	TEST(Parser, Sequence) {

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

	TEST(Parser, Optional) {
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

	TEST(Parser, IntegerExpr) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto num = rule(any(), [](const Context& cur)->Result {
			try {
				uint64_t value = utils::numeric_cast<uint64_t>(cur.begin->getLexeme());
				return IRBuilder(cur.manager).integerLit(value, true);
			} catch (const boost::bad_lexical_cast&) {}
			return false;
		});

		auto add = rule(seq(rec(), "+", rec()), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr a = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			ExpressionPtr b = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[1]);
			if (!a || !b) return false;
			return IRBuilder(cur.manager).add(a,b);
		});

		auto mul = rule(seq(rec(), "*", rec()), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr a = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			ExpressionPtr b = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[1]);
			if (!a || !b) return false;
			return IRBuilder(cur.manager).mul(a,b);
		});

		auto par = rule(seq("(", rec(), ")"), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 1u) return false;
			return dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
		});

		// grammar - ordered by inverse priority
		Grammar g(num, add, mul, par);

		EXPECT_TRUE(g.match(manager, "1"));
		EXPECT_TRUE(g.match(manager, "1+2"));
		EXPECT_TRUE(g.match(manager, "1+2+3"));
		EXPECT_TRUE(g.match(manager, "1+2*3"));

		EXPECT_TRUE(g.match(manager, "(1)"));
		EXPECT_TRUE(g.match(manager, "((1))"));

		EXPECT_TRUE(g.match(manager, "1+2*3+4"));
		EXPECT_TRUE(g.match(manager, "1+2*3+4*5+6*7"));

		EXPECT_FALSE(g.match(manager, "1+2-3"));
		EXPECT_FALSE(g.match(manager, "1+2**3"));

		// check some priority rules
		auto n1 = builder.integerLit(1,true);
		auto n2 = builder.integerLit(2,true);
		auto n3 = builder.integerLit(3,true);

		EXPECT_EQ(n1, g.match(manager, "1"));
		EXPECT_EQ(n1, g.match(manager, "(1)"));
		EXPECT_EQ(n1, g.match(manager, "((1))"));
		EXPECT_EQ(builder.add(n1,n2), g.match(manager, "1+2"));
		EXPECT_EQ(builder.add(n1,builder.mul(n2,n3)), g.match(manager, "1+2*3"));
		EXPECT_EQ(builder.mul(builder.add(n1,n2),n3), g.match(manager, "(1+2)*3"));
		EXPECT_EQ(builder.add(builder.mul(n1,n2),n3), g.match(manager, "1*2+3"));
		EXPECT_EQ(builder.add(builder.add(n1,n2),n3), g.match(manager, "1+2+3"));

		EXPECT_EQ(
				builder.add(
					builder.add(
							n1,
							builder.mul(builder.mul(n2,n3),n1)
					),
					builder.mul(
							builder.mul(n2,builder.add(n3,n1)),
							n2
					)
				),
				g.match(manager, "1+2*3*1+2*(3+1)*2")
		);

	}


	TEST(Parser, Loops) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * Goal: one rule including a loop => parse it
		 */

		auto token = rule(any(), [](const Context& cur)->Result {
			return IRBuilder(cur.manager).stringLit(*cur.begin);
		});

		auto compound = rule(seq("{", loop(seq(rec(), ";")), "}"), [](const Context& cur)->Result {
			StatementList stmts;
			for(auto it=cur.getTerms().begin(); it != cur.getTerms().end(); ++it) {
				StatementPtr stmt = dynamic_pointer_cast<StatementPtr>(*it);
				if (!stmt) return false;
				stmts.push_back(stmt);
			}
			return IRBuilder(cur.manager).compoundStmt(stmts);
		});

		EXPECT_EQ("_ => ...", toString(*token));
		EXPECT_EQ("'{' ( <E> ';' )* '}' => ...", toString(*compound));

		Grammar g(token, compound);

		auto a = builder.stringLit("a");
		auto b = builder.stringLit("b");
		auto c = builder.stringLit("c");

		EXPECT_TRUE(g.match(manager, "a"));
		EXPECT_TRUE(g.match(manager, "b"));

		EXPECT_EQ(a, g.match(manager, "a"));
		EXPECT_EQ(b, g.match(manager, "b"));

		EXPECT_EQ(builder.compoundStmt(), g.match(manager, "{}"));
		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a;}"));
		EXPECT_EQ(builder.compoundStmt(a,b,a), g.match(manager, "{a;b;a;}"));
		EXPECT_EQ(builder.compoundStmt(a,a,b), g.match(manager, "{a ; a; b; }"));

		// a large example (runtime test)
		EXPECT_EQ(builder.compoundStmt(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a), g.match(manager, "{a ; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; }"));

		// try nested
		EXPECT_EQ(builder.compoundStmt(builder.compoundStmt()), g.match(manager, "{ {}; } "));

		auto abc = builder.compoundStmt(a,b,c);
		EXPECT_EQ(builder.compoundStmt(a, b, abc, a, c), g.match(manager, "{ a; b; { a; b; c; }; a; c; }"));

		auto abcab = builder.compoundStmt(a,b,c,a,b);
		EXPECT_EQ(builder.compoundStmt(a, b, abc, abcab, a, c), g.match(manager, "{ a; b; { a; b; c; }; { a; b; c; a; b; }; a; c; }"));


		// some stuff that should not work
		EXPECT_FALSE(g.match(manager, "{a ; a; a; a; a; a; a; a; a a ; a; a; a; a; a; a; a; }"));

	}

	TEST(Parser, LoopsAndOptionals) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * Goal: one rule including a loop => parse it
		 */

		auto token = rule(any(), [](const Context& cur)->Result {
			// the list of terminals
			if (cur.begin->getType() == Token::Symbol) return false;
			return IRBuilder(cur.manager).stringLit(*cur.begin);
		});

		auto compound = rule(seq("{", loop(seq(rec(), opt(";"))), "}"), [](const Context& cur)->Result {
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

		Grammar g(token, compound);

		auto a = builder.stringLit("a");
		auto b = builder.stringLit("b");
		auto c = builder.stringLit("c");

		EXPECT_TRUE(g.match(manager, "a"));
		EXPECT_TRUE(g.match(manager, "b"));

		EXPECT_EQ(a, g.match(manager, "a"));
		EXPECT_EQ(b, g.match(manager, "b"));

		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a}"));

		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a;}"));
		EXPECT_EQ(builder.compoundStmt(b), g.match(manager, "{b;}"));

		EXPECT_EQ(builder.compoundStmt(a,b), g.match(manager, "{a;b}"));
		EXPECT_EQ(builder.compoundStmt(a,b), g.match(manager, "{a;b;}"));

		EXPECT_EQ(builder.compoundStmt(), g.match(manager, "{}"));
		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a}"));
		EXPECT_EQ(builder.compoundStmt(a), g.match(manager, "{a;}"));
		EXPECT_EQ(builder.compoundStmt(a,b,a), g.match(manager, "{a;b;a;}"));
		EXPECT_EQ(builder.compoundStmt(a,a,b), g.match(manager, "{a ; a; b; }"));

		// a large example (runtime test)
		EXPECT_EQ(builder.compoundStmt(a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a), g.match(manager, "{a ; a; a; a; a; a; a; a; a; a; a; a; a; a; a; a; }"));

		// try nested
		auto abc = builder.compoundStmt(a,b,c);
		EXPECT_EQ(builder.compoundStmt(a, b, abc, a, c), g.match(manager, "{ a; b; { a; b; c; }; a; c; }"));

		auto abcab = builder.compoundStmt(a,b,c,a,b);
		EXPECT_EQ(builder.compoundStmt(a, b, abc, abcab, a, c), g.match(manager, "{ a; b; { a; b; c; }; { a; b; c; a; b; }; a; c; }"));

		// this should work now too ...
		EXPECT_TRUE(g.match(manager, "{a ; a; a; a; a; a; a; a; a a ; a; a; a; a; a; a; a; }"));

	}


	TEST(Parser, IfLang) {
		NodeManager manager;
		IRBuilder builder(manager);

		/**
		 * The goal: support a the following grammar:
		 *
		 * E = { (E ;?)* } | if E then E else E | if E then E | a | b | c | E + E | E * E
		 */

		auto token = rule(any(), [](const Context& cur)->Result {
			// the list of terminals
			if (cur.begin->getType() == Token::Symbol) return false;
			return IRBuilder(cur.manager).stringLit(*cur.begin);
		});

		auto if_then_rule = rule(seq("if", "(", rec(), ")", rec()), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr condition = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			StatementPtr thenPart = dynamic_pointer_cast<StatementPtr>(cur.getTerms()[1]);
			if (!condition || !thenPart) return false;
			return IRBuilder(cur.manager).ifStmt(condition, thenPart);
		});

		auto if_then_else_rule = rule(seq("if", "(", rec(), ")", rec(), "else", rec()), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 3u) return false;
			ExpressionPtr condition = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			StatementPtr thenPart = dynamic_pointer_cast<StatementPtr>(cur.getTerms()[1]);
			StatementPtr elsePart = dynamic_pointer_cast<StatementPtr>(cur.getTerms()[2]);
			if (!condition || !thenPart || !elsePart) return false;
			return IRBuilder(cur.manager).ifStmt(condition, thenPart, elsePart);
		});

		auto compound = rule(seq("{", loop(seq(rec(), opt(";"))), "}"), [](const Context& cur)->Result {
			StatementList stmts;
			for(auto it=cur.getTerms().begin(); it != cur.getTerms().end(); ++it) {
				StatementPtr stmt = dynamic_pointer_cast<StatementPtr>(*it);
				if (!stmt) return false;
				stmts.push_back(stmt);
			}
			return IRBuilder(cur.manager).compoundStmt(stmts);
		});

		EXPECT_EQ("'if' '(' <E> ')' <E> => ...", toString(*if_then_rule));
		EXPECT_EQ("'if' '(' <E> ')' <E> 'else' <E> => ...", toString(*if_then_else_rule));
		EXPECT_EQ("'{' ( <E> ( ';' | _e ) )* '}' => ...", toString(*compound));


		Grammar g(token, if_then_rule, if_then_else_rule, compound);


		EXPECT_FALSE(g.match(manager, ""));
		EXPECT_TRUE(g.match(manager, "{ a; }"));

		auto res = g.match(manager,
				"{"
				"	a;"
				"	b;"
				"	if (a) b;"
				"	if (a) {"
				"		b;"
				"	}"
				"	c;"
				"}");

		EXPECT_TRUE(res);

		auto a = builder.stringLit("a");
		auto b = builder.stringLit("b");
		auto c = builder.stringLit("c");

		auto iab = builder.ifStmt(a, b);

		EXPECT_EQ(builder.compoundStmt(a,b,iab,iab,c), res);

		// test something incorrect
		EXPECT_FALSE(g.match(manager,
				"{"
				"	a;"
				"	b;"
				"	if (a) b;"
				"	if (a {"
				"		b;"
				"	}"
				"	c;"
				"}")
		);

	}

	TEST(Parser, ErrorReporting) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create expression grammar with function call

		auto num = rule(any(), [](const Context& cur)->Result {
			try {
				uint64_t value = utils::numeric_cast<uint64_t>(*cur.begin);
				return IRBuilder(cur.manager).intLit(value);
			} catch (const boost::bad_lexical_cast&) {}
			return false;
		});

		auto add = rule(seq(rec(), "+", rec()), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr a = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			ExpressionPtr b = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[1]);
			if (!a || !b) return false;
			return IRBuilder(cur.manager).add(a,b);
		});

		auto mul = rule(seq(rec(), "*", rec()), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 2u) return false;
			ExpressionPtr a = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
			ExpressionPtr b = dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[1]);
			if (!a || !b) return false;
			return IRBuilder(cur.manager).mul(a,b);
		});

		auto par = rule(seq("(", rec(), ")"), [](const Context& cur)->Result {
			if (cur.getTerms().size() != 1u) return false;
			return dynamic_pointer_cast<ExpressionPtr>(cur.getTerms()[0]);
		});

		auto fun = rule(seq("f", "(", list(rec(),",") , ")"), [](const Context& cur)->Result {
			IRBuilder builder(cur.manager);
			TypePtr intType = builder.getLangBasic().getInt4();
			ExpressionList args;
			TypeList paramTypes;
			for(auto it=cur.getTerms().begin(); it != cur.getTerms().end(); ++it) {
				ExpressionPtr expr = dynamic_pointer_cast<ExpressionPtr>(*it);
				if (!expr) return false;
				args.push_back(expr);
				paramTypes.push_back(intType);
			}
			return builder.callExpr(
					builder.getLangBasic().getInt4(),
					builder.literal("f", builder.functionType(paramTypes, intType, true)),
					args
			);
		});

		Grammar g(num, add, mul, par, fun);

		EXPECT_TRUE(g.match(manager, "f()"));
		EXPECT_TRUE(g.match(manager, "f(1)"));
		EXPECT_TRUE(g.match(manager, "f(1,2)"));

		auto res = g.match(manager, "2+f(f(1,2),3,4,5+6*f(1))*3");
		EXPECT_TRUE(res);
		if (res) EXPECT_EQ("int.add(2, int.mul(f(f(1, 2), 3, 4, int.add(5, int.mul(6, f(1)))), 3))", toString(*res));

//		EXPECT_THROW(g.match(manager, "2**3", true), ParseException);

	}


	TEST(Parser, MultiSymbol) {
		NodeManager manager;

		Grammar g;
		EXPECT_EQ("(E,{})", toString(g));

		// build a simple grammar for even/odd values
		//		E = z | s(O)
		//		O = s(E)

		g.addRule("E", rule("z", accept));
		g.addRule("E", rule(seq("s(", rec("O"), ")"), accept));
		g.addRule("O", rule(seq("s(", rec("E"), ")"), accept));

		// test matching even numbers
		g.setStartSymbol("E");
		EXPECT_TRUE(g.match(manager, "z"));
		EXPECT_FALSE(g.match(manager, "s(z)"));
		EXPECT_TRUE(g.match(manager, "s(s(z))"));
		EXPECT_FALSE(g.match(manager, "s(s(s(z)))"));

		// now testing odd numbers
		g.setStartSymbol("O");
		EXPECT_FALSE(g.match(manager, "z"));
		EXPECT_TRUE(g.match(manager, "s(z)"));
		EXPECT_FALSE(g.match(manager, "s(s(z))"));
		EXPECT_TRUE(g.match(manager, "s(s(s(z)))"));

	}

} // end namespace parser
} // end namespace core
} // end namespace insieme

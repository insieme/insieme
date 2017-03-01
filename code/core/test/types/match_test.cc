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

#include "insieme/core/types/match.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace types {

	bool matchable(const TypePtr& typeA, const TypePtr& typeB) {
		return isMatchable(typeA, typeB);
	}

	bool notMatchable(const TypePtr& typeA, const TypePtr& typeB) {
		return !isMatchable(typeA, typeB);
	}


	TEST(Matchable, TypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};


		// --- simple steps ---

		// identities should be matchable
		EXPECT_PRED2(matchable, type("A"), type("A"));

		// constants to variables are ok
		EXPECT_PRED2(matchable, type("A"), type("'a"));

		// variables to constants not
		EXPECT_PRED2(notMatchable, type("'a"), type("A"));

		// variables to variables are also ok
		EXPECT_PRED2(matchable, type("'a"), type("'b"));


		// --- consistency ---

		EXPECT_PRED2(   matchable, type("(A,B)"), type("('a,'b)"));
		EXPECT_PRED2(   matchable, type("(A,A)"), type("('a,'b)"));
		EXPECT_PRED2(   matchable, type("(A,A)"), type("('a,'a)"));
		EXPECT_PRED2(notMatchable, type("(A,B)"), type("('a,'a)"));


		// -- test functions --
		EXPECT_PRED2(matchable, type("(A)->R"), type("('a)->'r"));
		EXPECT_PRED2(matchable, type("(A)->A"), type("('a)->'r"));
		EXPECT_PRED2(matchable, type("<A>(B)->R"), type("<'a>('b)->'r"));

		EXPECT_PRED2(matchable, type("A::(B)->R"), type("'a::('b)->'r"));
		EXPECT_PRED2(matchable, type("A::(B)"), type("'a::('b)"));
		EXPECT_PRED2(matchable, type("~A::()"), type("~'a::()"));

		EXPECT_PRED2(notMatchable, type("A::(B)->R"), type("(ref<'a>,'b)->'r"));
		EXPECT_PRED2(notMatchable, type("A::(B)"), type("(ref<'a>,'b)->ref<'a>"));

		// -- test instantiation types --
		EXPECT_PRED2(matchable, type("<A>(A)->R"), type("<'a>('a)->'r"));
		EXPECT_PRED2(notMatchable, type("<A>(B)->R"), type("<'a>('a)->'r"));
		EXPECT_PRED2(matchable, type("<A>(B)->R"), type("<'a>('b)->'r"));
		EXPECT_PRED2(matchable, type("<A>(A)->R"), type("<'a>('b)->'r"));

		EXPECT_PRED2(notMatchable, type("(A)->R"), type("<'a>('a)->'r"));
		EXPECT_PRED2(notMatchable, type("<A>(B)->R"), type("<'a>('a)->'r"));

	}

	TEST(Matchable, VariadicTypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};

		// -- some tests with simple type variables
		EXPECT_PRED2(   matchable, type("()"),    type("()"));
		EXPECT_PRED2(   matchable, type("(A)"),   type("('a)"));
		EXPECT_PRED2(   matchable, type("(A,A)"), type("('a,'b)"));
		EXPECT_PRED2(   matchable, type("(A,B)"), type("('a,'b)"));
		
		// -- test with variadic type variables
		EXPECT_PRED2(   matchable, type("()"),      type("('a...)"));
		EXPECT_PRED2(   matchable, type("(A)"),     type("('a...)"));
		EXPECT_PRED2(   matchable, type("(A,B)"),   type("('a...)"));
		EXPECT_PRED2(   matchable, type("(A,B,C)"), type("('a...)"));

		// -- some tests with a mixture
		EXPECT_PRED2(notMatchable, type("()"),      type("('a,'b...)"));
		EXPECT_PRED2(   matchable, type("(A)"),     type("('a,'b...)"));
		EXPECT_PRED2(   matchable, type("(A,B)"),   type("('a,'b...)"));
		EXPECT_PRED2(   matchable, type("(A,B,C)"), type("('a,'b...)"));

		// -- some tests with deeper mixes
		EXPECT_PRED2(notMatchable, type("()"),                      type("('a,('b,'c...),'d...)"));
		EXPECT_PRED2(notMatchable, type("(A)"),                     type("('a,('b,'c...),'d...)"));
		EXPECT_PRED2(   matchable, type("(A,(B))"),                 type("('a,('b,'c...),'d...)"));
		EXPECT_PRED2(   matchable, type("(A,(B),C)"),               type("('a,('b,'c...),'d...)"));
		EXPECT_PRED2(   matchable, type("(A,(B,C),D)"),             type("('a,('b,'c...),'d...)"));
		EXPECT_PRED2(   matchable, type("(A,(B,C),D,E)"),           type("('a,('b,'c...),'d...)"));
		EXPECT_PRED2(   matchable, type("(A,(B,C,F,F,F),D,E,F,F)"), type("('a,('b,'c...),'d...)"));

		// -- add constraints on variadic type variables
		EXPECT_PRED2(matchable, type("((),())"), type("(('a...),('a...))"));
		EXPECT_PRED2(matchable, type("((A),(A))"), type("(('a...),('a...))"));
		EXPECT_PRED2(matchable, type("((A,B),(A,B))"), type("(('a...),('a...))"));

		EXPECT_PRED2(notMatchable, type("((A,B),(A))"), type("(('a...),('a...))"));
		EXPECT_PRED2(notMatchable, type("((A,B),(A,B,C))"), type("(('a...),('a...))"));
		EXPECT_PRED2(notMatchable, type("((A,B),(B,A))"), type("(('a...),('a...))"));

		// -- test instantiation types --
		EXPECT_PRED2(matchable, type("<>()->A"), type("<'a...>('a...)->'b"));

		EXPECT_PRED2(matchable, type("<A>(A)->A"), type("<'a...>('a...)->'b"));
		EXPECT_PRED2(matchable, type("<A,B>(A,B)->A"), type("<'a...>('a...)->'b"));
		EXPECT_PRED2(matchable, type("<A,B,C>(A,B,C)->A"), type("<'a...>('a...)->'b"));

		EXPECT_PRED2(notMatchable, type("<A>(A,B)->A"), type("<'a...>('a...)->'b"));
		EXPECT_PRED2(notMatchable, type("<A,B,C>(A,B)->A"), type("<'a...>('a...)->'b"));
		EXPECT_PRED2(notMatchable, type("<A,B>(B,A)->A"), type("<'a...>('a...)->'b"));
	}

	TEST(Matchable, GenericTypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};

		// --- simple steps ---

		// identities should be matchable
		EXPECT_PRED2(matchable, type("A"), type("A"));

		// constants to variables are ok
		EXPECT_PRED2(matchable, type("A"), type("'a<>"));

		// variables to constants not
		EXPECT_PRED2(notMatchable, type("'a<>"), type("A"));

		// variables to variables are also ok
		EXPECT_PRED2(matchable, type("'a<>"), type("'b<>"));


		// --- parameter checks ---

		// variables to variables are also ok
		EXPECT_PRED2(matchable, type("A"), type("'a<>"));
		EXPECT_PRED2(matchable, type("A<B>"), type("'a<'b>"));

		EXPECT_PRED2(notMatchable, type("A<>"), type("'a<'b>"));
		EXPECT_PRED2(notMatchable, type("A<B,C>"), type("'a<'b>"));


		// --- deeper check ---

		EXPECT_PRED2(notMatchable, type("A"), type("'a<'x,'y,'z<'w>>"));
		EXPECT_PRED2(notMatchable, type("A<B,C,D>"), type("'a<'x,'y,'z<'w>>"));
		EXPECT_PRED2(   matchable, type("A<B,C,D<E>>"), type("'a<'x,'y,'z<'w>>"));
		
		EXPECT_PRED2(   matchable, type("A<B<B2>,C,D<E>>"), type("'a<'x,'y,'z<'w>>"));
		EXPECT_PRED2(notMatchable, type("A<B<B2>,C,D<E>>"), type("'a<'x<>,'y,'z<'w>>"));

		// ensure type capturing
		EXPECT_PRED2(matchable, type("(A<B>,B)"), type("('a<'c>,'c)"));
		EXPECT_PRED2(notMatchable, type("(A<B>,C)"), type("('a<'c>,'c)"));

		// make sure that nested variables in instantiation lists are not captured
		EXPECT_PRED2(notMatchable, type("(A<B>,B)->D"), type("<'a<'b>,'b>('a<'c>,'b)->'d"));
		EXPECT_PRED2(matchable, type("<A<B>,B>(A<B>,B)->D"), type("<'a<'b>,'b>('a<'c>,'b)->'d"));
		EXPECT_PRED2(matchable, type("<A<C>,B>(A<C>,B)->D"), type("<'a<'b>,'b>('a<'c>,'b)->'d"));
		EXPECT_PRED2(matchable, type("<A<C>,B>(A<D>,B)->D"), type("<'a<'b>,'b>('a<'c>,'b)->'d"));
		EXPECT_PRED2(notMatchable, type("<A<C>,B>(A<D>,E)->D"), type("<'a<'b>,'b>('a<'c>,'b)->'d"));
		EXPECT_PRED2(notMatchable, type("<A<C>,B>(A<C>,B)->D"), type("<'a<'b>,'b>('a<'b>,'b)->'d"));
		EXPECT_PRED2(notMatchable, type("<A<C>,B>(A<C>,B)->D"), type("<'a<'c>,'b>('a<'b>,'b)->'d"));

		// --- consistency ---

		EXPECT_PRED2(   matchable, type("(A<B>,A<C>)"), type("('a<'b>,'a<'c>)"));
		EXPECT_PRED2(notMatchable, type("(A<B>,B<C>)"), type("('a<'b>,'a<'c>)"));

		EXPECT_PRED2(   matchable, type("<A<C>>(A<B>)->D"), type("<'a<'c>>('a<'b>)->'d"));
		EXPECT_PRED2(notMatchable, type("<A<C>>(B<B>)->D"), type("<'a<'c>>('a<'b>)->'d"));


		EXPECT_PRED2(matchable, type("(A,B)"), type("('a,'b)"));
		EXPECT_PRED2(matchable, type("(A,A)"), type("('a,'b)"));
		EXPECT_PRED2(matchable, type("(A,A)"), type("('a,'a)"));
		EXPECT_PRED2(notMatchable, type("(A,B)"), type("('a,'a)"));

	}

	TEST(Matchable, VariadicGenericTypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);
		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};

		// -- some tests with simple type variables
		EXPECT_PRED2(   matchable, type("(A<B>,C<B>)"), type("('a...<'b>)"));
		EXPECT_PRED2(notMatchable, type("(A<B>,C<D>)"), type("('a...<'b>)"));

		EXPECT_PRED2(matchable, type("(A,B,C)"), type("('a...<>)"));

	}

} // end namespace types
} // end namespace core
} // end namespace insieme

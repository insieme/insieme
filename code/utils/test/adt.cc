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

#include <list>

#include "insieme/utils/adt.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace utils {

	struct List : public ADT<
		Variant<'N'>,
		Variant<'C',int, List>
	> {};


	TEST(ADT, List) {

		EXPECT_EQ(2,List::arity);

		List a = create<List>('N');
		List b = create<List>('C', 12, a);
		List c = create<List>('C', 10, b);

		// check the size
		EXPECT_EQ(sizeof(void*),sizeof(List));

		// check comparison
		EXPECT_EQ(a,a);
		EXPECT_EQ(b,b);

		EXPECT_NE(a,b);
		EXPECT_NE(b,c);

		EXPECT_LT(b,a);
		EXPECT_LT(c,b);

		// check printer
		EXPECT_EQ("N", toString(a));
		EXPECT_EQ("(C 12 N)", toString(b));
		EXPECT_EQ("(C 10 (C 12 N))", toString(c));

		// an example match
		c.match(
				on_case<'N'>([](){
					std::cout << "It is a N\n";
				}),
				on_case<'C'>([](int e, List rest){
					std::cout << "It is a C with " << e << " and " << rest << "\n";
				}),
				other([](){
					std::cout << "It is something else!\n";
				})
		);

		auto isEmpty = [](const List& l) {
			return l.match(
					on_case<'N'>([](){ return true; }),
					other([](){ return false; })
			);
		};

		auto notEmpty = [&](const List& l) { return !isEmpty(l); };


		EXPECT_PRED1(isEmpty, a);
		EXPECT_PRED1(notEmpty, b);
		EXPECT_PRED1(notEmpty, c);

	}

	struct Mix : public ADT<
		Variant<'M',bool,char,int,float,double,
			std::string,
			std::vector<int>,
			std::vector<List>,
			std::vector<std::vector<int>>,
			std::set<int>,
			std::set<std::vector<int>>
		>
	>{};

	TEST(ADT, Mix) {

		List l = create<List>('N');

		std::vector<int> il;
		il.push_back(12);
		std::vector<List> ll;
		// ll.push_back(l);

		std::vector<int> a;
		a.push_back(5);
		a.push_back(2);

		std::vector<int> b;
		b.push_back(7);
		b.push_back(8);

		std::vector<std::vector<int>> nested;
		nested.push_back(a);
		nested.push_back(b);

		std::set<int> s1 = { 10, 12 };
		std::set<std::vector<int>> s2 = { b, a };

		Mix m = create<Mix>('M',true,'c',1,2.0f,3.0,"bla",il,ll,nested,s1,s2);
		Mix n = create<Mix>('M',true,'c',1,2.0f,3.0,"bla",il,ll,nested,s1,s2);

		EXPECT_EQ(m,n);
		EXPECT_EQ("(M true c 1 2 3 \"bla\" [12] [] [[5,2],[7,8]] {10,12} {[5,2],[7,8]})", toString(m));
		EXPECT_EQ("(M true c 1 2 3 \"bla\" [12] [] [[5,2],[7,8]] {10,12} {[5,2],[7,8]})", toString(n));
	}


	struct BinaryTree : public ADT<
		Variant<'L'>,
		Variant<'N',BinaryTree,int,BinaryTree>
	> {};

	TEST(ADT, BinaryTree) {

		EXPECT_EQ(3,BinaryTree::arity);

		BinaryTree e = create<BinaryTree>('L');
		BinaryTree t = create<BinaryTree>('N',e,12,e);

		EXPECT_EQ("(N L 12 L)", toString(t));

	}


	struct WideTree : public ADT<
		Variant<'L'>,
		Variant<'N',std::vector<WideTree>>
	> {};

	TEST(ADT, WideTree) {

		EXPECT_EQ(1,WideTree::arity);

		WideTree e = create<WideTree>('L');
		WideTree a = create<WideTree>('N',std::vector<WideTree>{e,e});
		WideTree b = create<WideTree>('N',std::vector<WideTree>{e,a,e,a,e});

		EXPECT_EQ("L", toString(e));
		EXPECT_EQ("(N [L,L])", toString(a));
		EXPECT_EQ("(N [L,(N [L,L]),L,(N [L,L]),L])", toString(b));


		// match the empty tree
		EXPECT_TRUE(e.match(
				on_case<'L'>([](){ return true; }),
				on_case<'N'>([](const std::vector<WideTree>& list){
					return false;
				})
		));

		// match the small tree
		EXPECT_TRUE(a.match(
				on_case<'L'>([](){ return false; }),
				on_case<'N'>([&](const std::vector<WideTree>& list){
					EXPECT_EQ(2,list.size());
					EXPECT_EQ(e,list[0]);
					EXPECT_EQ(e,list[1]);
					return true;
				})
		));

		// match the big tree
		EXPECT_TRUE(b.match(
				on_case<'L'>([](){ return false; }),
				on_case<'N'>([&](const std::vector<WideTree>& list){
					EXPECT_EQ(5,list.size());
					EXPECT_EQ(e,list[0]);
					EXPECT_EQ(a,list[1]);
					EXPECT_EQ(e,list[2]);
					EXPECT_EQ(a,list[3]);
					EXPECT_EQ(e,list[4]);
					return true;
				})
		));

	}

	struct ForestTree : public ADT<
		Variant<'L'>,
		Variant<'N',std::set<ForestTree>>
	> {};

	TEST(ADT, ForestTree) {

		EXPECT_EQ(1,ForestTree::arity);

		ForestTree e = create<ForestTree>('L');
		ForestTree a = create<ForestTree>('N',std::set<ForestTree>{e,e});

		EXPECT_EQ("L", toString(e));
		EXPECT_EQ("(N {L})", toString(a));


		// match the empty tree
		EXPECT_TRUE(e.match(
				on_case<'L'>([](){ return true; }),
				on_case<'N'>([](const std::set<ForestTree>& list){
					return false;
				})
		));

		// match the small tree
		EXPECT_TRUE(a.match(
				on_case<'L'>([](){ return false; }),
				on_case<'N'>([&](const std::set<ForestTree>& list){
					EXPECT_EQ(1,list.size());
					EXPECT_EQ(e,*list.begin());
					return true;
				})
		));

	}


//	data Expression = Number Int
//	                | Add Expression Expression
//	                | Minus Expression
//	                | Mult Expression Expression
//	                | Divide Expression Expression


	struct Expr : public ADT<
		Variant<'N',int>,
		Variant<'+',Expr,Expr>,
		Variant<'-',Expr,Expr>,
		Variant<'*',Expr,Expr>,
		Variant<'/',Expr,Expr>
	>{};

	TEST(ADT, Expr) {

		EXPECT_EQ(2,Expr::arity);

		Expr e = create<Expr>('N',12);
		Expr t = create<Expr>('+',e,e);

		EXPECT_EQ("(+ (N 12) (N 12))", toString(t));

	}


	// --------------- Formula --------------------

	struct Variable : public ADT<
		Variant<'V',int>
	>{};

	struct Formula : public ADT<
		Variant<'F'>
	>{};



	// --------------- Benchmark --------------------

	const int N = 100 * 1000;
	const int M = 10;

	struct IntList : public ADT<
		Variant<'N'>,
		Variant<'C',int,IntList>
	> {};

	TEST(ADT, DISABLED_Benchmark) {

		for(int j=0; j<M; j++) {

			// fill the list
			IntList l = create<IntList>('N');
			for(int i=0; i<N; i++) {
				l = create<IntList>('C',i,l);
			}

		}
	}

	TEST(LinkedList, DISABLED_Benchmark) {

		for(int j=0; j<M; j++) {

			// fill the list
			std::list<int> l;
			for(int i=0; i<N; i++) {
				l.push_back(i);
			}

		}
	}


} // end namespace utils
} // end namespace insieme

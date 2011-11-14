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

#include "insieme/utils/functional_utils.h"





TEST(TypeListTrait, DealingWithListTraits) {


	EXPECT_TRUE(typeid(type_list<int,double>::head) == typeid(int));
	EXPECT_TRUE(typeid(type_list<int,double>::rest::head) == typeid(double));

	EXPECT_TRUE(typeid(element_type<0,int,double>::type) == typeid(int));
	EXPECT_TRUE(typeid(element_type<1,int,double>::type) == typeid(double));

	EXPECT_TRUE(typeid(type_at<0,type_list<int,double>>::type) == typeid(int));
	EXPECT_TRUE(typeid(type_at<1,type_list<int,double>>::type) == typeid(double));

	EXPECT_FALSE(typeid(type_at<0,type_list<int,double>>::type) == typeid(double));
	EXPECT_FALSE(typeid(type_at<1,type_list<int,double>>::type) == typeid(int));

}

// ------------------------- Some experimenting with std::function ----------------

#include <functional>
#include <vector>

int add(int a, int b) {
	return a + b;
}

struct mul {
	int operator()(int a, int b) {
		return a * b;
	};
};

std::vector<std::function<int(int,int)>> buildOperations(int step, int& akk) {
	std::vector<std::function<int(int,int)>> res;
	res.push_back(&add);
	res.push_back(mul());
	res.push_back([&akk, step](int a, int b) {
		akk += a + b + step;
		return akk;
	});

	return res;
}

int poluteStack(int depth) {
	if (depth == 0) {
		return 0;
	}
	return poluteStack(depth - 1) + depth;
}


TEST(FunctionExperiment, TryStdFunction) {

	std::function<int(int,int)> func;

	func = &add;
	EXPECT_EQ(3, func(1,2));

	mul op;
	func = op;
	EXPECT_EQ(2, func(1,2));

	func = [](int a, int b) {
		return a - b;
	};
	EXPECT_EQ(-1, func(1,2));

	int res = 0;
	auto funs = buildOperations(1, res);

	poluteStack(40);

	EXPECT_EQ(3, funs[0](1,2));
	EXPECT_EQ(2, funs[1](1,2));
	EXPECT_EQ(4, funs[2](1,2));
	EXPECT_EQ(4, res);
	EXPECT_EQ(8, funs[2](1,2));
	EXPECT_EQ(8, res);


	// check lambdas with states ....

	int counter = 0;
	auto l = [counter]() mutable {
		counter++;
		return counter;
	};

	std::function<int()> funcA;
	std::function<int()> funcB;

	// the actual lambda is copied
	funcA = l;
	funcB = l;

	EXPECT_EQ(1, funcA());
	EXPECT_EQ(2, funcA());
	EXPECT_EQ(3, funcA());

	// here shit gets real! => lambda was copied, not handled by reference
	EXPECT_EQ(1, funcB());
	EXPECT_EQ(2, funcB());

}

struct A {
	virtual int f() const { return 0; }
	virtual int g(int a) { return a + 1; };
};

struct B : public A {
	virtual int f() const { return 1; }
	virtual int g(int a) { return a + 2; };
};

TEST(FunctionExperiment, MemberFunctionWrapper) {

	A a;
	B b;

	// try a ordinary member function pointer
	int(A::*p)()const = &A::f;
	EXPECT_EQ(0, (a.*p)());
	EXPECT_EQ(1, (b.*p)());


	// invoke same operation using the member function functor
	auto f = fun(a, &A::f);
	EXPECT_EQ(0, f());

	f = fun(b, &A::f);
	EXPECT_EQ(1, f());

	auto g = fun(a, &A::g);
	EXPECT_EQ(4, g(3));

	g = fun(b, &A::g);
	EXPECT_EQ(5, g(3));


	// test compatibility to std::function
	std::function<int()> sf = f;
	EXPECT_EQ(1, sf());

	std::function<int(int)> sg = g;
	EXPECT_EQ(6, sg(4));

}


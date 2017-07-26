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
 */

#include <gtest/gtest.h>

#include "insieme/utils/functional_utils.h"


TEST(TypeListTrait, DealingWithListTraits) {
	EXPECT_TRUE(typeid(type_list<int, double>::head) == typeid(int));
	EXPECT_TRUE(typeid(type_list<int, double>::rest::head) == typeid(double));

	EXPECT_TRUE(typeid(element_type<0, int, double>::type) == typeid(int));
	EXPECT_TRUE(typeid(element_type<1, int, double>::type) == typeid(double));

	EXPECT_TRUE(typeid(type_at<0, type_list<int, double>>::type) == typeid(int));
	EXPECT_TRUE(typeid(type_at<1, type_list<int, double>>::type) == typeid(double));

	EXPECT_FALSE(typeid(type_at<0, type_list<int, double>>::type) == typeid(double));
	EXPECT_FALSE(typeid(type_at<1, type_list<int, double>>::type) == typeid(int));

	EXPECT_EQ(0, (index_of<int, int, float>::value));
	EXPECT_EQ(1, (index_of<float, int, float>::value));
}


TEST(LambdaTrait, PureFunctionTypes) {
	// test pure function types
	EXPECT_TRUE(typeid(lambda_traits<int()>::result_type) == typeid(int));
	EXPECT_TRUE(lambda_traits<int()>::arity == 0);

	EXPECT_TRUE(typeid(lambda_traits<int(float)>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int(float)>::argument_type) == typeid(float));
	EXPECT_TRUE(typeid(type_at<0, lambda_traits<int(float)>::argument_types>::type) == typeid(float));
	EXPECT_TRUE(lambda_traits<int(float)>::arity == 1);

	EXPECT_TRUE(typeid(lambda_traits<int(float, bool)>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int(float, bool)>::arg1_type) == typeid(float));
	EXPECT_TRUE(typeid(lambda_traits<int(float, bool)>::arg2_type) == typeid(bool));
	EXPECT_TRUE(typeid(type_at<0, lambda_traits<int(float, bool)>::argument_types>::type) == typeid(float));
	EXPECT_TRUE(typeid(type_at<1, lambda_traits<int(float, bool)>::argument_types>::type) == typeid(bool));
	EXPECT_TRUE(lambda_traits<int(float, bool)>::arity == 2);
}

TEST(LambdaTrait, FunctionPointerTypes) {
	// test function pointers
	EXPECT_TRUE(typeid(lambda_traits<int (*)()>::result_type) == typeid(int));
	EXPECT_TRUE(lambda_traits<int (*)()>::arity == 0);

	EXPECT_TRUE(typeid(lambda_traits<int (*)(float, bool)>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int (*)(float, bool)>::arg1_type) == typeid(float));
	EXPECT_TRUE(typeid(lambda_traits<int (*)(float, bool)>::arg2_type) == typeid(bool));
	EXPECT_TRUE(typeid(type_at<0, lambda_traits<int (*)(float, bool)>::argument_types>::type) == typeid(float));
	EXPECT_TRUE(typeid(type_at<1, lambda_traits<int (*)(float, bool)>::argument_types>::type) == typeid(bool));
	EXPECT_TRUE(lambda_traits<int (*)(float, bool)>::arity == 2);


	// test const function pointers
	EXPECT_TRUE(typeid(lambda_traits<int (*const)()>::result_type) == typeid(int));
	EXPECT_TRUE(lambda_traits<int (*const)()>::arity == 0);

	EXPECT_TRUE(typeid(lambda_traits<int (*const)(float, bool)>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int (*const)(float, bool)>::arg1_type) == typeid(float));
	EXPECT_TRUE(typeid(lambda_traits<int (*const)(float, bool)>::arg2_type) == typeid(bool));
	EXPECT_TRUE(typeid(type_at<0, lambda_traits<int (*const)(float, bool)>::argument_types>::type) == typeid(float));
	EXPECT_TRUE(typeid(type_at<1, lambda_traits<int (*const)(float, bool)>::argument_types>::type) == typeid(bool));
	EXPECT_TRUE(lambda_traits<int (*const)(float, bool)>::arity == 2);
}

TEST(LambdaTrait, MemberFunctionPointerTypes) {
	class A {};

	// test member-function pointers
	EXPECT_TRUE(typeid(lambda_traits<int (A::*)()>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*)()>::class_type) == typeid(A));
	EXPECT_TRUE(lambda_traits<int (A::*)()>::arity == 0);

	EXPECT_TRUE(typeid(lambda_traits<int (A::*)(float, bool)>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*)(float, bool)>::arg1_type) == typeid(float));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*)(float, bool)>::arg2_type) == typeid(bool));
	EXPECT_TRUE(typeid(type_at<0, lambda_traits<int (A::*)(float, bool)>::argument_types>::type) == typeid(float));
	EXPECT_TRUE(typeid(type_at<1, lambda_traits<int (A::*)(float, bool)>::argument_types>::type) == typeid(bool));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*)(float, bool)>::class_type) == typeid(A));
	EXPECT_TRUE(lambda_traits<int (A::*)(float, bool)>::arity == 2);


	// test const member-function pointers
	EXPECT_TRUE(typeid(lambda_traits<int (A::*const)()>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*const)()>::class_type) == typeid(A));
	EXPECT_TRUE(lambda_traits<int (A::*const)()>::arity == 0);

	EXPECT_TRUE(typeid(lambda_traits<int (A::*const)(float, bool)>::result_type) == typeid(int));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*const)(float, bool)>::arg1_type) == typeid(float));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*const)(float, bool)>::arg2_type) == typeid(bool));
	EXPECT_TRUE(typeid(type_at<0, lambda_traits<int (A::*const)(float, bool)>::argument_types>::type) == typeid(float));
	EXPECT_TRUE(typeid(type_at<1, lambda_traits<int (A::*const)(float, bool)>::argument_types>::type) == typeid(bool));
	EXPECT_TRUE(typeid(lambda_traits<int (A::*const)(float, bool)>::class_type) == typeid(A));
	EXPECT_TRUE(lambda_traits<int (A::*const)(float, bool)>::arity == 2);
}

TEST(LambdaTrait, LambdaTypes) {
	// test lambdas
	EXPECT_TRUE(typeid(decltype(&std::logical_not<bool>::operator())) == typeid(bool (std::logical_not<bool>::*)(const bool&) const));
	EXPECT_TRUE(typeid(lambda_traits<std::logical_not<bool>>::result_type) == typeid(bool));
	EXPECT_TRUE(typeid(lambda_traits<std::logical_not<bool>>::arg1_type) == typeid(bool));
	EXPECT_TRUE(typeid(lambda_traits<std::logical_not<bool>>::class_type) == typeid(std::logical_not<bool>));
	EXPECT_TRUE(lambda_traits<std::logical_not<bool>>::arity == 1);
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

std::vector<std::function<int(int, int)>> buildOperations(int step, int& akk) {
	std::vector<std::function<int(int, int)>> res;
	res.push_back(&add);
	res.push_back(mul());
	res.push_back([&akk, step](int a, int b) {
		akk += a + b + step;
		return akk;
	});

	return res;
}

int poluteStack(int depth) {
	if(depth == 0) { return 0; }
	return poluteStack(depth - 1) + depth;
}


TEST(FunctionExperiment, TryStdFunction) {
	std::function<int(int, int)> func;

	func = &add;
	EXPECT_EQ(3, func(1, 2));

	mul op;
	func = op;
	EXPECT_EQ(2, func(1, 2));

	func = [](int a, int b) { return a - b; };
	EXPECT_EQ(-1, func(1, 2));

	int res = 0;
	auto funs = buildOperations(1, res);

	poluteStack(40);

	EXPECT_EQ(3, funs[0](1, 2));
	EXPECT_EQ(2, funs[1](1, 2));
	EXPECT_EQ(4, funs[2](1, 2));
	EXPECT_EQ(4, res);
	EXPECT_EQ(8, funs[2](1, 2));
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
	virtual int f() const {
		return 0;
	}
	virtual int g(int a) {
		return a + 1;
	};
};

struct B : public A {
	virtual int f() const {
		return 1;
	}
	virtual int g(int a) {
		return a + 2;
	};
};

TEST(FunctionExperiment, MemberFunctionWrapper) {
	A a;
	B b;

	// try a ordinary member function pointer
	int (A::*p)() const = &A::f;
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

int finc(int v) {
	return ++v;
}
int fdec(int v) {
	return --v;
}


TEST(FunctionExperiment, FunctionComposition) {
	using namespace insieme::utils;

	EXPECT_TRUE(composeFunc(std::logical_not<bool>(), std::logical_not<bool>())(true));
	EXPECT_FALSE(composeFunc(std::logical_not<bool>(), std::logical_not<bool>())(false));

	EXPECT_FALSE(composeFunc(std::logical_not<bool>(), std::logical_not<bool>(), std::logical_not<bool>())(true));
	EXPECT_TRUE(composeFunc(std::logical_not<bool>(), std::logical_not<bool>(), std::logical_not<bool>())(false));

	auto&& dec = [](const int& val) { return val - 1; };
	auto&& inc = [](const int& val) { return val + 1; };

	EXPECT_EQ(4, composeFunc(inc, inc, inc, dec)(2));
	EXPECT_EQ(3, composeFunc(inc, dec, inc, dec)(3));

	int val = 10;
	EXPECT_EQ(11, composeFunc([](int& val) { return val + 1; })(val));
	EXPECT_EQ(7, composeFunc([](int& val) { return val - 1; },
	                         [](int& val) -> int& {
		                         val -= 2;
		                         return val;
		                     })(val));
	EXPECT_EQ(8, val);

	EXPECT_EQ(9, composeFunc(std::function<int(int)>(finc), std::function<int(int)>(fdec), std::function<int(int)>(finc))(val));
}

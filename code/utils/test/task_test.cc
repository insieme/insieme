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

#ifndef _GLIBCXX_USE_NANOSLEEP
#define _GLIBCXX_USE_NANOSLEEP
#endif
#include <gtest/gtest.h>
#include "insieme/utils/timer.h"
#include "insieme/utils/tasks/insieme_tasks.h"


namespace insieme {
namespace utils {
	TEST(Tasks, SimpleTest) {}

	//	void testFun() {
	//		std::cout << "Function Pointer Works!\n";
	//	}
	//
	//	void testFun2(int x) {
	//		std::cout << "Another test function - with parameters: " << x << "\n";
	//	}
	//
	//	TEST(Tasks, SimpleTest) {
	//

	//		// test the function type wrapper handling
	//		auto l = []() { std::cout << "Hello"; };
	//		EXPECT_EQ(typeid(std::function<void()>), typeid(fun_type<decltype(l)>::type));
	//
	//		auto t1 = task([]() {  std::cout << "Hello";  });
	//		auto t2 = task([]() {  std::cout << " ";  });
	//		auto t3 = task([]() {  std::cout << "World";  });
	//		auto t4 = task([]() {  std::cout << "\n";  });
	//
	//		t1 >> t2 >> t3 >> t4;
	//		t1 >> t4;
	//		t4();
	//
	//		auto tf = task(&testFun);
	//		tf();
	//
	//		auto tf2 = task(&testFun2, 123);
	//		tf2();
	//
	//		auto tn = task([](int x) { std::cout << "x=" << x << "\n"; }, 212);
	//		tn();
	//
	//		auto ts = task([](int a, int b) { std::cout << "a+b=" << a << "+" << b << "=" << (a+b) << "\n"; }, 14, 16);
	//		ts();
	//
	//	}
	//
	//	TEST(Tasks, ParallelTasks) {
	//
	//		auto sleep = []() {
	//			std::this_thread::sleep_for(std::chrono::seconds(1));
	//		};
	//
	//		auto t1 = task(sleep);
	//		auto t2 = task(sleep);
	//		auto t3 = task(sleep);
	//
	//		auto tn = task();
	//
	//		t1 >> tn;
	//		t2 >> tn;
	//		t3 >> tn;
	//
	//		tn();
	//
	//	}
	//
	//	TEST(Tasks, NestedTasks) {
	//
	//		auto sleep = []() {
	//			std::this_thread::sleep_for(std::chrono::seconds(1));
	//		};
	//
	//		auto t1 = task(sleep);
	//		auto t2 = task(sleep);
	//		auto t3 = task(sleep);
	//
	//		auto t4 = task([&](){
	//
	//			auto t1 = task(sleep);
	//			auto t2 = task(sleep);
	//			auto t3 = task(sleep);
	//
	//			auto tn = task();
	//			t1 >> tn;
	//			t2 >> tn;
	//			t3 >> tn;
	//
	//			tn();
	//		});
	//
	//		auto tn = task();
	//
	//		t1 >> tn;
	//		t2 >> tn;
	//		t3 >> tn;
	//
	//		t4 >> tn;
	//
	//		tn();
	//
	//	}
	//
	//	int fib_seq(int x) {
	//		if (x <= 1) return x;
	//		return fib_seq(x-1) + fib_seq(x-2);
	//	}
	//
	//
	//	int fib_par(int x) {
	//
	//		if (x <= 1) return x;
	//
	//		int a, b;
	//
	//		auto t1 = task([&a,x]() { a = fib_par(x-1); });
	//		auto t2 = task([&b,x]() { b = fib_par(x-2); });
	//
	//		auto tn = task();
	//		t1 >> tn;
	//		t2 >> tn;
	//		tn();
	//		return a + b;
	//	}
	//
	//	TEST(Tasks, Fib) {
	//
	//		int N = 10;
	//		int a,b;
	//		std::cout << "Time for a: " << TIME(a = fib_seq(N)) << "\n";
	//		std::cout << "Time for b: " << TIME(b = fib_par(N)) << "\n";
	//
	//		EXPECT_EQ(a,b);
	//	}
	//
	//
	//	TEST(Tasks, Scooped) {
	//
	//		int v = 0;
	//		{
	//			auto tn = task([&](int x) { v=x; }, 3);
	//			tn();
	//		}
	//		EXPECT_EQ(v,3);
	//
	//		{
	//			auto t1 = task([&]() { v++; });
	//			auto t2 = task(t1);
	//			t1();
	//			t2();
	//		}
	//		EXPECT_EQ(v,4);
	//
	//		insieme::utils::TaskManager::finalize();
	//	}
} // end namespace analysis
} // end namespace insieme

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

#include "insieme/utils/lua/lua.h"


namespace insieme {
namespace utils {
namespace lua {

	TEST(LuaTest, Basic) {
		// create a Lua instance
		Lua lua;

		// run a hello world
		lua.run("print('Hello World')");

		// run something invalid => there should be an exception
		EXPECT_THROW(lua.run("print("), LuaException);

		// do some computation
		EXPECT_EQ(7, lua.run<int>("return 3 + 4"));

		// some more extensive compuation
		EXPECT_EQ(55, lua.run<int>("sum = 0; for i=1, 10 do sum = sum + i; end ; return sum"));

		// done (cleanup is done be destructor)
	}


	TEST(LuaTest, GlobalState) {
		// create a Lua instance
		Lua lua;

		// set up some value
		lua.run("value = 5");

		// read value
		EXPECT_EQ(5, lua.run<int>("return value"));

		// use specialized function
		EXPECT_EQ(5, lua.eval<int>("value"));
		EXPECT_EQ(8, lua.eval<int>("value + 3"));

		// test strings
		lua.run("str = 'Hello'");
		EXPECT_EQ("Hello", lua.eval<std::string>("str"));
	}


	int one(lua_State* state) {
		lua_pushnumber(state, 1); // push result
		return 1;                 // number of results pushed
	}

	int two() {
		return 2;
	}

	TEST(LuaTest, Function) {
		// create a Lua instance
		Lua lua;

		// run a hello world
		lua.registerFunction("one", &one);

		// run the script using build in command
		EXPECT_EQ(3, lua.run<int>("sum = 0; for i = 0 , 2 do sum = sum + one();  end ; return sum;"));


		// test wrapped function
		lua.registerFunction("two", &two);

		// run script using 'two' function
		EXPECT_EQ(6, lua.run<int>("sum = 0; for i = 0 , 2 do sum = sum + two();  end ; return sum;"));
	}


	struct Counter {
		int value;
		Counter(int value = 0) : value(value) {}
		int inc() {
			return ++value;
		}
		int operator()() {
			return inc();
		}
	};


	TEST(LuaTest, MemberFunction) {
		// create a Lua instance
		Lua lua;

		Counter a(10);
		Counter b(20);

		// register a member function
		lua.registerFunction("incA", &a);
		lua.registerFunction("incB", &b);

		// run the script using build in command
		lua.run("for i = 0 , 3 do incB(); incA(); incB(); end");

		EXPECT_EQ(a.value, 14);
		EXPECT_EQ(b.value, 28);

		// done (cleanup is done be destructor)
	}


	class Subtractor {
	  public:
		int operator()(int a, int b) {
			return a - b;
		}
	};


	TEST(LuaTest, MemberFunctionWithArguments) {
		// create a Lua instance
		Lua lua;

		Subtractor a;

		// register a member function
		lua.registerFunction("sub", &a);

		// run the script using build in command
		EXPECT_EQ(5, lua.run<int>("return sub(10,5);"));
		EXPECT_EQ(5, lua.run<int>("return sub(20,15);"));
	}


	struct Counter2 {
		int value;
		void operator()(int a) {
			value += a;
		}
	};


	TEST(LuaTest, VoidFunction) {
		// create a Lua instance
		Lua lua;

		Counter2 a;
		a.value = 0;

		// register a member function
		lua.registerFunction("count", &a);

		// run the script using build in command
		lua.run("for i = 1 , 4 do count(i) end");

		EXPECT_EQ(10, a.value);
	}

} // end namespace lua
} // end namespace core
} // end namespace insieme

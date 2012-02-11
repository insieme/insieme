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

#pragma once

#include <cassert>
#include <string>
#include <luajit-2.0/lua.hpp>

#include <iostream>
#include "insieme/utils/container_utils.h"

/**
 * A common utility file providing a C++ interface to an underlying
 * Lua library.
 */

namespace insieme {
namespace utils {
namespace lua {

	// Tutorial: http://csl.sublevel3.org/lua/


	class Lua;
	class LuaState;
	class LuaExcpetion;

	/**
	 * A resource-acquisition-is-initialization wrapper for a lua_state
	 * struct.
	 */
	class LuaState {

		/**
		 * The wrapped lua state instance.
		 */
		lua_State* L;

		bool own;

	public:

		/**
		 * A default constructor initializing the lua_state.
		 */
		LuaState() : L(lua_open()), own(true) {}

		LuaState(lua_State* state) : L(state), own(false) {}

		/**
		 * The destructor cleaning up the wrapped lua state instance.
		 */
		~LuaState() { if(own) lua_close(L); }

		/**
		 * An operator supporting the implicit conversion of this object
		 * to a lua_state pointer (to be passed to the lua functions).
		 */
		operator lua_State*() { return L; }

	};

	namespace detail {

		template<typename T> T def() { return T(); }

		template<typename T> T extract_argument(lua_State* state, int pos);

		template<> bool extract_argument<bool>(lua_State* state, int pos) {
			return lua_toboolean(state, pos);
		}

		template<> int extract_argument<int>(lua_State* state, int pos) {
			assert(lua_isnumber(state, pos) && "Invalid argument passed!");
			return lua_tointeger(state, pos);
		}

		template<> double extract_argument<double>(lua_State* state, int pos) {
			assert(lua_isnumber(state, pos) && "Invalid argument passed!");
			return lua_tonumber(state, pos);
		}


		bool checkNumArgs(lua_State* state, int num) {
			if (lua_gettop(state) != num) {
				lua_pushstring(state, "Invalid number of arguments passed!");
				lua_error(state);
			}
			return true;
		}


		// structs realizing calls

		template<typename C, typename R, typename ... P> struct call;

		template<typename C, typename R>
		struct call<C,R> {
			R operator()(lua_State* state, C& obj) const {
				return obj();
			}
		};

		template<typename C, typename R, typename P1>
		struct call<C,R,P1> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>(state,1));
			}
		};

		template<typename C, typename R, typename P1, typename P2>
		struct call<C,R,P1,P2> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>(state,1), extract_argument<P2>(state,2));
			}
		};

		template<typename C, typename R, typename P1, typename P2, typename P3>
		struct call<C,R,P1,P2,P3> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>(state,1), extract_argument<P2>(state,2), extract_argument<P3>(state,3));
			}
		};


		// structs implementing executors

		template<typename F, typename R, typename ... P>
		struct executor {
			int operator()(lua_State* state, F& fun) {
				// check number of arguments (only in debug mode)
				assert(checkNumArgs(state, sizeof...(P)));

				lua_pushnumber(state, call<F,R,P...>()(state, fun));
				return 1;
			}
		};

		template<typename F, typename ... P>
		struct executor<F, void, P...> {
			int operator()(lua_State* state, F& fun) {
				// check number of arguments (only in debug mode)
				assert(checkNumArgs(state, sizeof...(P)));

				call<F,void,P...>()(state, fun);
				return 0;
			}
		};

		template<typename F, typename R, typename ... P>
		struct executor<F, R(F::*)(P...)> : public executor<F,R,P...> {};

		template<typename F, typename R, typename ... P>
		struct executor<F, R(*)(P...)> : public executor<F,R,P...> {};


		// the template function to be used for wrapping method invocations

		template<typename Object>
		int callBind(lua_State* state) {
			struct executor<Object, decltype(&Object::operator())> run;
			Object* obj = (Object*)lua_touserdata(state, lua_upvalueindex(1));
			return run(state, *obj);
		}

		template<typename R, typename ... P>
		int callPure(lua_State* state) {
			// get function pointer
			struct executor<R(*)(P...), R, P...> run;
			R(*f)(P...) = (R(*)(P...))lua_touserdata(state, lua_upvalueindex(1));
			return run(state, f);
		}
	}

	class Lua {

		typedef lua_CFunction Function;

		LuaState state;

	public:

		Lua();

		LuaState& getState() { return state; }

		void run(const std::string& script);

		template<typename ResType>
		ResType run(const std::string& script) {
			check(luaL_loadstring(state, script.c_str()));
			check(lua_pcall(state, 0, 1, 0));

			ResType res = detail::extract_argument<ResType>(state, 1);
			lua_pop(state, 1);
			return res;
		}

		void registerFunction(const std::string& name, Function f) {
			lua_register(state, name.c_str(), f);
		}

		template<typename R, typename ... P>
		void registerFunction(const std::string& name, R(*f)(P...)) {
			// push function pointer on stack
			lua_pushlightuserdata(state, (void*)f);

			// create c-closure
			lua_pushcclosure(state, &detail::callPure<R(*)(P...)>, 1);

			// bind closure
			lua_setglobal(state, name.c_str());
		}

		template<typename Object>
		void registerFunction(const std::string& name, Object* obj) {
			// push object and member function pointer on closure stack
			lua_pushlightuserdata(state, obj);

			// create c-closure
			lua_pushcclosure(state, &detail::callBind<Object>, 1);

			// bind closure
			lua_setglobal(state, name.c_str());
		}

	private:

		void check(int code);

	};


	class LuaException : public std::exception {
		std::string msg;

	public:
		LuaException(const std::string& msg) : msg(msg) {}
		virtual ~LuaException() throw() { }

		virtual const char* what() const throw() { return msg.c_str(); }
	};


} // lua
} // utils
} // insieme

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

#pragma once

#include <cassert>
#include <string>

#include <lua.hpp>

#include <boost/noncopyable.hpp>

#include "insieme/utils/assert.h"

/**
 * A common utility file providing a C++ interface to an underlying
 * Lua library.
 */

namespace insieme {
namespace utils {
namespace lua {

	/**
	 * This utility offers a simple mean to use the Lua scripting environment. The main
	 * class is the Lua class, which offers a simple, safe, C++ like interface for the Lua
	 * environment. It supports the execution of expressions and scripts as well as the
	 * customization of an environment by offering a simple mean for registering additional
	 * function symbols realizing C function callbacks.
	 */

	class Lua;
	class LuaState;
	class LuaExcpetion;

	/**
	 * A resource-acquisition-is-initialization wrapper for a lua_state
	 * pointer. Instances of this type maintain all the information required
	 * for processing Lua scripts.
	 *
	 * Tutorial: http://csl.sublevel3.org/lua/
	 */
	class LuaState : public boost::noncopyable {
		/**
		 * The wrapped lua state instance.
		 */
		lua_State* L;

		/**
		 * A flag indicating whether this state instance is just
		 * a wrapper or also an owner of the internal state.
		 */
		bool own;

	  public:
		/**
		 * A default constructor creating a fresh Lua environment instance.
		 */
		LuaState() : L(lua_open()), own(true) {}

		/**
		 * A constructor allowing to wrap an exteranl lua_State instance into
		 * an instance of this type. This constructor may be required when calling
		 * a C++ function from Lua.
		 */
		LuaState(lua_State* state) : L(state), own(false) {}

		/**
		 * The destructor cleaning up the wrapped lua state instance.
		 */
		~LuaState() {
			if(own) { lua_close(L); }
		}

		/**
		 * An operator supporting the implicit conversion of this object
		 * to a lua_state pointer (to be passed to the lua functions).
		 */
		operator lua_State*() {
			return L;
		}
	};

	namespace function_wrapper {

		/**
		 * To enable access to C/C++ functions from within Lua, those functions
		 * need to be registered. However, the interface for supported functions
		 * is fixed. Nevertheless, the interface is powerful enough to support
		 * simple wrappers between this predefined interface and the actual C/C++
		 * interface. The following template utilities are generating such
		 * an interface for arbitrary callables (functions, lambdas and functors).
		 */

		/**
		 * This template is realizing the extraction of an argument from the
		 * call stack surrounding the registrated C/C++ function (C-Closure).
		 *
		 * Each specialization of this struct is offering a call-operator implementation
		 * accepting a lua_state (to read from) and a position (the index of the argument
		 * to be extracted) as an argument.
		 */
		template <typename T>
		struct extract_argument;

		// support booleans

		template <>
		struct extract_argument<bool> {
			bool operator()(lua_State* state, int pos) const {
			// check type of argument in debug mode
			#ifndef NDEBUG
				if(!lua_isboolean(state, pos)) {
					lua_pushstring(state, "Invalid type of argument passed - expected bool!");
					lua_error(state);
				}
				#endif
				return lua_toboolean(state, pos);
			}
		};

		// support integer types

		template <typename Num>
		struct extract_integer_argument {
			Num operator()(lua_State* state, int pos) const {
			// check type of argument in debug mode
			#ifndef NDEBUG
				if(!lua_isnumber(state, pos)) {
					lua_pushstring(state, "Invalid type of argument passed - expected number!");
					lua_error(state);
				}
				#endif
				return (Num)lua_tointeger(state, pos);
			}
		};

		template <>
		struct extract_argument<uint8_t> : public extract_integer_argument<uint8_t> {};
		template <>
		struct extract_argument<uint16_t> : public extract_integer_argument<uint16_t> {};
		template <>
		struct extract_argument<uint32_t> : public extract_integer_argument<uint32_t> {};
		template <>
		struct extract_argument<uint64_t> : public extract_integer_argument<uint64_t> {};

		template <>
		struct extract_argument<int8_t> : public extract_integer_argument<int8_t> {};
		template <>
		struct extract_argument<int16_t> : public extract_integer_argument<int16_t> {};
		template <>
		struct extract_argument<int32_t> : public extract_integer_argument<int32_t> {};
		template <>
		struct extract_argument<int64_t> : public extract_integer_argument<int64_t> {};

		template <>
		struct extract_argument<float> : public extract_integer_argument<float> {};
		template <>
		struct extract_argument<double> : public extract_integer_argument<double> {};

		// support arbitrary pointers

		template <typename T>
		struct extract_argument<T*> {
			T* operator()(lua_State* state, int pos) const {
				return (T*)lua_topointer(state, pos);
			}
		};

		template <typename T>
		struct extract_argument<const T*> {
			const T* operator()(lua_State* state, int pos) const {
				return (const T*)lua_topointer(state, pos);
			}
		};

		template <>
		struct extract_argument<std::string> {
			std::string operator()(lua_State* state, int pos) const {
			// check type of argument in debug mode
			#ifndef NDEBUG
				if(!lua_isstring(state, pos)) {
					lua_pushstring(state, "Invalid type of argument passed - expected string!");
					lua_error(state);
				}
				#endif
				return std::string(lua_tostring(state, pos));
			}
		};


		/**
		 * Extracting result values works the same way as extracting arguments.
		 */
		template <typename T>
		struct extract_result : public extract_argument<T> {};


		/**
		 * The actual process of extracting arguments, invoking the function
		 * is realized within the call functor.
		 *
		 * Unfortunately it is not possible to generate a sequence of indices
		 * from a variadic template parameter pack. Hence, one specialization
		 * for every number of parameters is required.
		 *
		 * @tparam F the type of function to be called
		 * @tparam R the return type of the function
		 * @tparam P the parameters of the function to be called
		 */
		template <typename F, typename R, typename... P>
		struct call;

		template <typename F, typename R>
		struct call<F, R> {
			R operator()(lua_State* /*state*/, F& obj) const {
				return obj();
			}
		};

		template <typename C, typename R, typename P1>
		struct call<C, R, P1> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>()(state, 1));
			}
		};

		template <typename C, typename R, typename P1, typename P2>
		struct call<C, R, P1, P2> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>()(state, 1), extract_argument<P2>()(state, 2));
			}
		};

		template <typename C, typename R, typename P1, typename P2, typename P3>
		struct call<C, R, P1, P2, P3> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>()(state, 1), extract_argument<P2>()(state, 2), extract_argument<P3>()(state, 3));
			}
		};

		template <typename C, typename R, typename P1, typename P2, typename P3, typename P4>
		struct call<C, R, P1, P2, P3, P4> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>()(state, 1), extract_argument<P2>()(state, 2), extract_argument<P3>()(state, 3),
				           extract_argument<P4>()(state, 4));
			}
		};

		template <typename C, typename R, typename P1, typename P2, typename P3, typename P4, typename P5>
		struct call<C, R, P1, P2, P3, P4, P5> {
			R operator()(lua_State* state, C& obj) const {
				return obj(extract_argument<P1>()(state, 1), extract_argument<P2>()(state, 2), extract_argument<P3>()(state, 3),
				           extract_argument<P4>()(state, 4), extract_argument<P5>()(state, 5));
			}
		};

		// structs implementing executors


		/**
		 * Verifies the number of arguments being passed to a function.
		 */
		inline bool checkNumArgs(lua_State* state, int num) {
			if(lua_gettop(state) != num) {
				lua_pushstring(state, "Invalid number of arguments passed!");
				lua_error(state);
			}
			return true;
		}

		/**
		 * The executor functor is realizing the function call and the pushing
		 * of results on the Lua call stack.
		 */
		template <typename F, typename R, typename... P>
		struct executor;

		template <typename F, typename... P>
		struct executor<F, void, P...> {
			int operator()(lua_State* state, F& fun) {
				assert_true(checkNumArgs(state, sizeof...(P)));
				call<F, void, P...>()(state, fun);
				return 0;
			}
		};

		template <typename F, typename... P>
		struct executor<F, bool, P...> {
			int operator()(lua_State* state, F& fun) {
				assert_true(checkNumArgs(state, sizeof...(P)));
				lua_pushboolean(state, call<F, bool, P...>()(state, fun));
				return 1;
			}
		};

		template <typename F, typename R, typename... P>
		struct executor_numeric_res {
			int operator()(lua_State* state, F& fun) {
				assert_true(checkNumArgs(state, sizeof...(P)));
				lua_pushnumber(state, call<F, R, P...>()(state, fun));
				return 1;
			}
		};

		template <typename F, typename... P>
		struct executor<F, uint8_t, P...> : public executor_numeric_res<F, uint8_t, P...> {};
		template <typename F, typename... P>
		struct executor<F, uint16_t, P...> : public executor_numeric_res<F, uint16_t, P...> {};
		template <typename F, typename... P>
		struct executor<F, uint32_t, P...> : public executor_numeric_res<F, uint32_t, P...> {};
		template <typename F, typename... P>
		struct executor<F, uint64_t, P...> : public executor_numeric_res<F, uint64_t, P...> {};

		template <typename F, typename... P>
		struct executor<F, int8_t, P...> : public executor_numeric_res<F, int8_t, P...> {};
		template <typename F, typename... P>
		struct executor<F, int16_t, P...> : public executor_numeric_res<F, int16_t, P...> {};
		template <typename F, typename... P>
		struct executor<F, int32_t, P...> : public executor_numeric_res<F, int32_t, P...> {};
		template <typename F, typename... P>
		struct executor<F, int64_t, P...> : public executor_numeric_res<F, int64_t, P...> {};

		template <typename F, typename... P>
		struct executor<F, float, P...> : public executor_numeric_res<F, float, P...> {};
		template <typename F, typename... P>
		struct executor<F, double, P...> : public executor_numeric_res<F, double, P...> {};


		// strings and C-strings

		template <typename F, typename... P>
		struct executor<F, std::string, P...> {
			int operator()(lua_State* state, F& fun) {
				assert_true(checkNumArgs(state, sizeof...(P)));
				lua_pushstring(state, call<F, std::string, P...>()(state, fun).c_str());
				return 1;
			}
		};

		template <typename F, typename... P>
		struct executor<F, const char*, P...> {
			int operator()(lua_State* state, F& fun) {
				assert_true(checkNumArgs(state, sizeof...(P)));
				lua_pushstring(state, call<F, const char*, P...>()(state, fun));
				return 1;
			}
		};

		// two bridges for resolving function and member function pointers

		template <typename F, typename C, typename R, typename... P>
		struct executor<F, R (C::*)(P...)> : public executor<F, R, P...> {};

		template <typename F, typename C, typename R, typename... P>
		struct executor<F, R (C::*)(P...) const> : public executor<F, R, P...> {};

		template <typename F, typename R, typename... P>
		struct executor<F, R (*)(P...)> : public executor<F, R, P...> {};


		/**
		 * The wrapper function registered within the Lua environment in
		 * case a functor object should be called from inside a Lua script.
		 *
		 * @tparam Functor the functor type to be called.
		 */
		template <typename Functor>
		int callBind(lua_State* state) {
			struct executor<Functor, decltype(&Functor::operator())> run;
			Functor* obj = (Functor*)lua_touserdata(state, lua_upvalueindex(1));
			return run(state, *obj);
		}

		/**
		 * The wrapper function registered within the Lua environment in
		 * case a functor object should be called from inside a Lua script.
		 * This wrapper should be used if there are multiple overloaded call
		 * operators within the functor.
		 *
		 * @tparam Functor the functor type to be called.
		 * @tparam R the return type of the call operator
		 * @tparam P the parameters of the call operator
		 */
		template <typename Functor, typename R, typename... P>
		int callBind(lua_State* state) {
			struct executor<Functor, R, P...> run;
			Functor* obj = (Functor*)lua_touserdata(state, lua_upvalueindex(1));
			return run(state, *obj);
		}

		/**
		 * The wrapper function registered within the Lua environment in
		 * case a pure function pointer should be called from inside a Lua script.
		 */
		template <typename R, typename... P>
		int callPure(lua_State* state) {
			// get function pointer
			struct executor<R (*)(P...), R, P...> run;
			R (*f)(P...) = (R (*)(P...))lua_touserdata(state, lua_upvalueindex(1));
			return run(state, f);
		}
	}

	/**
	 * The Lua class is the main entity offered by this utility collection.
	 * Instances of this class represent full Lua environments. It supports
	 * the simple execution of Lua scripts and the customization of the
	 * Lua environment by registering additional function symbols linked to
	 * C/C++ functions.
	 */
	class Lua {
		/**
		 * A type definition referencing the native interface required by Lua
		 * for every function to be called from inside a Lua script.
		 */
		typedef lua_CFunction Function;

		/**
		 * The Lua environment state this Lua instance is managing.
		 */
		LuaState state;

	  public:
		/**
		 * Creates a new Lua environment supporting all the default libraries.
		 */
		Lua();

		/**
		 * Obtains access to the internally managed lua state instance. Users
		 * may access the state in case for specific customizations not supported
		 * by this interface.
		 *
		 * NOTE: if it is something common you want to do with this state, you
		 * may as well add this kind of operation to this wrapper.
		 *
		 * @return a reference to the internal managed Lua state
		 */
		LuaState& getState() {
			return state;
		}

		/**
		 * Runs the given script inside the managed Lua environment.
		 *
		 * @param script the script to be executed
		 */
		void run(const std::string& script);

		/**
		 * Runs the given script inside the managed Lua environment and
		 * returns the value returned by the script. The script has to include
		 * a return value!
		 *
		 * @tparam ResType the type of value produced by the script
		 * @param script the script to be executed (must contain a return!)
		 */
		template <typename ResType>
		ResType run(const std::string& script) {
			check(luaL_loadstring(state, script.c_str()));
			check(lua_pcall(state, 0, 1, 0));

			ResType res = function_wrapper::extract_result<ResType>()(state, 1);
			lua_pop(state, 1);
			return res;
		}

		/**
		 * Evaluates the given expression inside the Lua environment and obtains
		 * the corresponding result.
		 *
		 * This function is equivalent to calling run("return " + expr).
		 *
		 * @tparam expr the expression to be evaluated.
		 */
		template <typename ResType>
		ResType eval(const std::string& expr) {
			return run<ResType>("return " + expr);
		}


		// ---- function registration ----

		/**
		 * Registers a Lua-native C-function inside the managed lua environment.
		 *
		 * @param name the name to which the given function should be bound inside
		 * 				this environment
		 * @param f a pointer to the function to be registered.
		 */
		void registerFunction(const std::string& name, Function f) {
			lua_register(state, name.c_str(), f);
		}

		/**
		 * Registers a arbitrary typed function within this Lua environment. The
		 * wrappers bridging the parameter passing and return value handling between
		 * Lua and C/C++ is created automatically using templates.
		 *
		 * @tparam R the result type of the function to be registered
		 * @tparam P the parameters of the function to be registered
		 * @param name the name the given function should be bound to
		 * @param f the function to be bound reference by a pointer
		 */
		template <typename R, typename... P>
		void registerFunction(const std::string& name, R (*f)(P...)) {
			// push function pointer on stack
			lua_pushlightuserdata(state, (void*)f);

			// create c-closure
			lua_pushcclosure(state, &function_wrapper::callPure<R, P...>, 1);

			// bind closure
			lua_setglobal(state, name.c_str());
		}

		/**
		 * Registers a arbitrary typed functor within this Lua environment. The
		 * wrappers bridging the parameter passing and return value handling between
		 * Lua and C/C++ is created automatically using templates.
		 *
		 * @tparam Functor the type of the functor to be bound
		 * @param name the name the given function should be bound to
		 * @param fun the functor to be bound reference by a pointer. The referenced
		 * 			functor has to be alive while it is used inside the Lua environment.
		 */
		template <typename Functor>
		void registerFunction(const std::string& name, Functor* fun) {
			// push object and member function pointer on closure stack
			lua_pushlightuserdata(state, fun);

			// create c-closure
			lua_pushcclosure(state, &function_wrapper::callBind<Functor>, 1);

			// bind closure
			lua_setglobal(state, name.c_str());
		}

		/**
		 * The same as as the registrating function using a pointer to a functor. However,
		 * this function allows to resolve ambiguities in case the given functor is supporting
		 * multiple overloaded call operators.
		 *
		 * @tparam Functor the type of the functor to be bound
		 * @param name the name the given function should be bound to
		 * @param fun the functor to be bound reference by a pointer. The referenced
		 * 			functor has to be alive while it is used inside the Lua environment.
		 * @param op a reference to the call-operator to be used
		 */
		template <typename Functor, typename R, typename... P>
		void registerFunction(const std::string& name, Functor* fun, R (Functor::*op)(P...)) {
			// push object and member function pointer on closure stack
			lua_pushlightuserdata(state, fun);

			// create c-closure
			lua_pushcclosure(state, &function_wrapper::callBind<Functor, R, P...>, 1);

			// bind closure
			lua_setglobal(state, name.c_str());
		}

	  private:
		/**
		 * A utility used to handle return codes of the Lua library. In case an error
		 * occurred, a LuaException will be thrown.
		 *
		 * @param code the code to be processed.
		 */
		void check(int code);
	};

	/**
	 * The type of exception been thrown in case an error occurred while running a Lua
	 * script or conducting any other modification within a Lua environment.
	 */
	class LuaException : public std::exception {
		/**
		 * A brief description of the encountered problem.
		 */
		std::string msg;

	  public:
		LuaException(const std::string& msg) : msg(msg) {}
		virtual ~LuaException() throw() {}

		virtual const char* what() const throw() {
			return msg.c_str();
		}
		const std::string& getMessage() const {
			return msg;
		}
	};


} // lua
} // utils
} // insieme

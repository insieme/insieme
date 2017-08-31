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

// functions ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<class R>
R trivialTemplateFun() { }

template<class R>
R templateFunRet() {
	R r;
	return r;
}

template<class R, class P>
R templateFunRetParam(P p) {
	R r;
	return r + p;
}

template<class C>
C templateFun(C c) {
	return c+c;
}

struct Typer {
	using Bla = int;
};
template<class T>
void dependentNameFun(typename T::Bla param) { }

template <template <typename> class Container, typename T>
void templateTemplateFun(Container<T>& container, const T value) {
	container.field = value;
}

// variadic ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<typename T>
T variadicTemplateFun(T v) {
  return v;
}

template<typename T, typename... Args>
T variadicTemplateFun(T first, Args... args) {
  return first + variadicTemplateFun(args...);
}

template<typename... Args>
class VariadicClass {
};

template<typename T, int... Args>
int variadicNonTypeTemplateFun(T first) {
	return sizeof...(Args);
}

// variadic template template /////////////////////////////////////////////////////////////////////////////////////////////////////////

template<template<typename> class ... Name>
void variadicTemplateTemplateFun() {
}


// classes ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<class T>
struct TemplateWithMethod {
	T get() {
		T t;
		return t;
	}
};

class ClassWithTemplateMethod {
  public:
	template<class T>
	T get() {
		T t;
		return t;
	}
};

template<class D>
class TemplateClass {
  public:
	D field;
};

template<int I>
class IntTemplateClass {
};

template<int I>
void intTemplateClassFunction(IntTemplateClass<I> i) {}

template<int I>
struct IntTemplateClassContainer {
	IntTemplateClassContainer(IntTemplateClass<I> i) {}
};

template<template <class> class TT, class X>
class TemplateTemplateClass {
	TT<X> x;
};

// function pointers ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<typename T, typename S>
void templateFunPointerParam(T(*funPtr)(S)) {}

void specificFunPointerParam(int(*funPtr)(float)) {}

template<typename PT>
void modifier(TemplateClass<PT>) {}

template<typename T>
void dependentFunPointerParam(void(*paramFun)(TemplateClass<T>)) {}

template<class CharT> class basic_ostream {
public:
	basic_ostream& op(void (*func)(basic_ostream<CharT>&));
};

template<class CharT>
void endl(basic_ostream<CharT>& os);

using ostream = basic_ostream<char>;

// statics ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

template<typename _Value>
struct __numeric_traits_integer {
	static _Value __min;
};

template<typename _Value>
_Value __numeric_traits_integer<_Value>::__min;

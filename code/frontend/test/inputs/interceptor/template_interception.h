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

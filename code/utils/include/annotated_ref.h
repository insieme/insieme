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

#include <set>
#include <boost/type_traits/is_base_of.hpp>

// ------------------------------- replace w/ boost / move
//template<bool> struct is_true;
//template<> struct is_true<true> {
//    typedef bool flag;
//};
//template<> struct is_true<false> {
//};
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ replace w/ boost / move


class Annotation;

class Annotatable {
	std::set<Annotation> *annotations;
public:
	void addAnnotation(const Annotation& a) {};
};

template<typename T>
class AnnotatedRef : public Annotatable {
public:
	T& node;

	template <typename B, bool IsBaseOf>
	T& convert(B& b);

public:
	AnnotatedRef(const T& node) : node(node) { }
	
//	template<typename B>
//	AnnotatedRef(const AnnotatedRef<B>& from, typename is_true<boost::is_base_of<T,B>::value>::flag = 0) : node(from.node) { }

	template <class B>
	AnnotatedRef(const AnnotatedRef<B>& from) : node( convert<B,boost::is_base_of<T,B>::value>(from.node) ) { }

	T& operator->() {
		return node;
	}
	
	const T& operator->() const {
		return node;
	}
};

template <typename T>
template <typename B>
T& AnnotatedRef<T>::convert<B,true>(B& b) { return static_cast<T>(b); }

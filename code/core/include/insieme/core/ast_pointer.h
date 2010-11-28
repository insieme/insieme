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

#include <iostream>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/utility/enable_if.hpp>

#include "insieme/utils/instance_ptr.h"
#include "insieme/utils/type_traits_utils.h"


namespace insieme {
namespace core {

// Forward declaration of cast functor.
struct StaticPointerCast;
struct PointerChildFactory;

template<typename T>
class Pointer : public InstancePtr<T> {
public:

	typedef StaticPointerCast StaticCast;
	typedef PointerChildFactory ChildFactory;

	Pointer() : InstancePtr<T>(NULL) {}

	Pointer(T* ptr) : InstancePtr<T>(ptr) { }

	/**
	 * A conversion operator to a annotated pointer referencing a super type of the type
	 * pointed to by this instance can be efficiently realized using a reinterpret_cast. This
	 * operator is realizing this efficient conversion.
	 */
	template<typename B, typename boost::enable_if<boost::is_base_of<B,T>,int>::type = 0>
	operator const Pointer<B>() const {
		return *reinterpret_cast<const Pointer<B>* >(this);
	}
};

template<typename B, typename T>
inline typename boost::enable_if<boost::is_base_of<T,B>, Pointer<B>>::type
dynamic_pointer_cast(Pointer<T> src) {
	if(!src || dynamic_cast<B*>(&(*src))) {
		return *(reinterpret_cast<Pointer<B>* >(&src));
	}
	return NULL;
}

template<typename B, typename T>
inline typename boost::enable_if<boost::is_base_of<T,B>, Pointer<B>&>::type
static_pointer_cast(Pointer<T>& src) {
	assert((!src || dynamic_cast<B*>(&(*src))) && "Invalid static cast!");
	return reinterpret_cast<Pointer<B>&>(src);
}

template<typename B, typename T>
inline typename boost::enable_if<boost::is_base_of<T,B>, const Pointer<B>&>::type
static_pointer_cast(const Pointer<T>& src) {
	assert((!src || dynamic_cast<B*>(&(*src))) && "Invalid static cast!");
	return reinterpret_cast<const Pointer<B>&>(src);
}

/**
 * A template version for a functor performing static pointer casts on annotated pointer.
 * The purpose of this struct is to allow the static_pointer_cast function to be defined as
 * a pointer conversion function required as a template parameter of the AST Visitor class.
 */
struct StaticPointerCast {
	template<typename Target, typename Source>
	const Pointer<Target>& operator()(const Pointer<Source>& value) const {
		return static_pointer_cast<Target>(value);
	}
};


// forward declaration of the node type
class Node;

/**
 * A static functor object extracting child node pointer from given pointer.
 */
struct PointerChildFactory {
	template<typename Source>
	inline const Pointer<const Node> operator()(const Pointer<Source>& value, std::size_t childIndex) const {
		return value->getChildList()[childIndex];
	}
};

} // end namespace core
} // end namespace insieme

template<typename T>
std::ostream& operator<<(std::ostream& out, const insieme::core::Pointer<T>& ptr) {
//	out << "AP@" << (&ptr) << "->" << (&*ptr) << "(";
	out << "AP(";
	if (!!ptr) {
		out << *ptr;
	} else {
		out << "NULL";
	}
	out << ")";
	return out;
}


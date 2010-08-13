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
#include <boost/utility/enable_if.hpp>

#include "instance_ptr.h"
#include "type_traits_utils.h"

class Annotation;

class Annotatable {
	std::set<Annotation> *annotations;
public:
	void addAnnotation(const Annotation& a) {};
};


template<typename T>
class AnnotatedPtr : public InstancePtr<T>, Annotatable {
public:
	AnnotatedPtr(T* node) : InstancePtr<T>(node) { }

	template<typename B>
	AnnotatedPtr(const AnnotatedPtr<B>& from, typename boost::enable_if<boost::is_base_of<T,B>,int>::type = 0) : InstancePtr<T>(from.node) { }
};


template<typename B, typename T>
typename boost::enable_if<boost::is_base_of<T,B>, AnnotatedPtr<B>>::type 
dynamic_pointer_cast(AnnotatedPtr<T> src) {
	if (dynamic_cast<B*>(&(*src))) {
		return *(reinterpret_cast<AnnotatedPtr<B>* >(&src));
	}
	return NULL;
}

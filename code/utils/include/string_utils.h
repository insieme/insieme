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

#include <algorithm>
#include <functional>
#include <string>
#include <cstring>
#include <sstream>

#include "functional_utils.h"

using std::string;

string format(const char* formatString, ...);

template<typename T>
string toString(const T& value) {
	std::stringstream res;
	res << value;
	return res.str();
}

/**
 * This functor can be used to print elements to an output stream.
 */
template<typename Extractor>
struct print : public std::binary_function<std::ostream&, const typename Extractor::argument_type&, std::ostream&> {
	Extractor extractor;
	std::ostream& operator()(std::ostream& out, const typename Extractor::argument_type& cur) const {
		return out << extractor(cur);
	}
};

template<typename Container, typename Printer>
struct Joinable {

	const string& separator;
	const Container& container;
	const Printer& printer;

	Joinable(const string& separator, const Container& container, const Printer& printer) :
		separator(separator), container(container), printer(printer) {};

};


template<typename Container, typename Printer>
std::ostream& operator<<(std::ostream& out, const Joinable<Container, Printer>& joinable) {
	if(joinable.container.size() > 0) {
		auto iter = joinable.container.cbegin();
		joinable.printer(out, *iter);
		++iter;
		std::for_each(iter, joinable.container.cend(), [&](const typename Container::value_type& cur) {
			out << joinable.separator;
			joinable.printer(out, cur);
		});
	}
	return out;
}

/** Joins the values in the collection to the stream separated by a supplied separator.
 ** NOTE: When using C++0x lambdas to supply the printer *make sure* to provide the return type (usually std::ostream&) explicitly.
 ** */
template<typename Container, typename Printer>
Joinable<Container, Printer> join(const string& separator, const Container& container, const Printer& printer) {
	return Joinable<Container,Printer>(separator, container, printer);
}

template<typename Container>
Joinable<Container, print<id<const typename Container::value_type&>>> join(const string& separator, const Container& container) {
	return Joinable<Container, print<id<const typename Container::value_type&>>>(separator, container, print<id<const typename Container::value_type&>>());
}

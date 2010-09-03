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


template<typename Container, typename Extractor>
struct Joinable {

	const string& separator;
	const Container& container;
	const Extractor& extractor;

	Joinable(const string& separator, const Container& container, const Extractor& extractor) :
		separator(separator), container(container), extractor(extractor) {};

};


template<typename Container, typename Extractor>
std::ostream& operator<<(std::ostream& out, const Joinable<Container, Extractor>& joinable) {
	if(joinable.container.size() > 0) {
		out << joinable.extractor(*joinable.container.cbegin());
		std::for_each(joinable.container.cbegin()+1, joinable.container.cend(), [&](const typename Container::value_type& cur) {
			out << joinable.separator << joinable.extractor(cur);
		});
	}
	return out;
}

template<typename Container, typename Extractor>
Joinable<Container, Extractor> join(const string& separator, const Container& container, const Extractor& extractor) {
	return Joinable<Container,Extractor>(separator, container, extractor);
}

template<typename Container>
Joinable<Container, id<const typename Container::value_type&>> join(const string& separator, const Container& container) {
	return Joinable<Container, id<const typename Container::value_type&>>(separator, container, id<const typename Container::value_type&>());
}

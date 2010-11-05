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

#include "core/int_type_param.h"

#include "utils/container_utils.h"

namespace insieme {
namespace core {

// -------------------------------- Integer Type Parameter ----------------------------

const IntTypeParam IntTypeParam::ZERO = IntTypeParam::getConcreteIntParam(0);
const IntTypeParam IntTypeParam::ONE  = IntTypeParam::getConcreteIntParam(1);
const IntTypeParam IntTypeParam::INF  = IntTypeParam(INFINITE);

bool IntTypeParam::operator==(const IntTypeParam& param) const {
	// quick check on reference
	if (this == &param) {
		return true;
	}

	// different type => different
	if (type != param.type) {
		return false;
	}

	// check type dependent content
	switch (type) {
	case VARIABLE:
		return symbol == param.symbol;
	case CONCRETE:
		return value == param.value;
	case INFINITE:
		return true;
	}
	return false;
}

bool IntTypeParam::operator<(const IntTypeParam& param) const {
	// check type first ...
	if (type != param.type) {
		return type < param.type;
	}

	// so ... type is the same
	switch(type) {
	case INFINITE: return false; /* < infinite is always identical */
	case VARIABLE: return symbol < param.symbol;
	case CONCRETE: return value < param.value;
	}

	assert(false && "Illegal object state!");
	return false;
}

/**
 * Tests whether all of the given integer type parameter are concrete.
 *
 * @param intTypeParams the list of parameters to be tested
 * @return true if all are concrete, false otherwise
 */
bool IntTypeParam::allConcrete(const vector<IntTypeParam>& intTypeParams) {
	// just use all-functionality of container utils
	return all(intTypeParams, [](const IntTypeParam& param) { return param.isConcrete(); });
}

IntTypeParam IntTypeParam::getVariableIntParam(char symbol) {
	return IntTypeParam(symbol);
}

IntTypeParam IntTypeParam::getConcreteIntParam(std::size_t value) {
	return IntTypeParam(value);
}

IntTypeParam IntTypeParam::getInfiniteIntParam() {
	return IntTypeParam(INFINITE);
}


} // end namespace core
} // end namespace insieme

namespace std {
	ostream& operator<<(ostream& os, const insieme::core::IntTypeParam& p) {
		return (os << p.toString());
	}
}

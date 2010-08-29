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

#include <vector>

#include "container_utils.h"
#include "annotated_ptr.h"
#include "types.h"

namespace insieme {
namespace core {

DECLARE_NODE_TYPE(IntType);
DECLARE_NODE_TYPE(FloatType);
DECLARE_NODE_TYPE(BoolType);
DECLARE_NODE_TYPE(UnitType);


class IntType : public GenericType {

	IntType(const unsigned short& numBytes = 4) :
		GenericType("int", vector<TypePtr>(), toVector(IntTypeParam::getConcreteIntParam(numBytes))) {};

	virtual IntType* clone(TypeManager&) const {
		return new IntType(getNumBytes());
	}

public:
	static IntTypePtr get(TypeManager& manager, const unsigned short numBytes = 4) {
		return manager.get(IntType(numBytes));
	}

	const unsigned short getNumBytes() const {
		return getIntTypeParameter()[0].getValue();
	}
};

class FloatType : public GenericType {

	FloatType(const unsigned short& numBytes = 8) :
		GenericType("float", vector<TypePtr>(), toVector(IntTypeParam::getConcreteIntParam(numBytes))) {};

	virtual FloatType* clone(TypeManager&) const {
		return new FloatType(getNumBytes());
	}

public:
	static FloatTypePtr get(TypeManager& manager, const unsigned short numBytes = 8) {
		return manager.get(FloatType(numBytes));
	}

	const unsigned short getNumBytes() const {
		return getIntTypeParameter()[0].getValue();
	}
};

class BoolType : public GenericType {

	BoolType() : GenericType("bool") {};

	virtual BoolType* clone(TypeManager&) const {
		return new BoolType();
	}

public:
	static BoolTypePtr get(TypeManager& manager) {
		return manager.get(BoolType());
	}
};

class UnitType : public GenericType {

	UnitType() : GenericType("unit") {};

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual UnitType* clone(TypeManager&) const {
		return new UnitType();
	}

public:

	static UnitTypePtr get(TypeManager& manager) {
		return manager.get(UnitType());
	}

};

} // end namespace core
} // end namespace insieme

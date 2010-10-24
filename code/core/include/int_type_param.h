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

#include <string>
#include <vector>

#include "string_utils.h"

// ---------------------------------------- Integer Type Parameters ------------------------------

namespace insieme {
namespace core {

using std::string;
using std::vector;

/**
 * Instances of this class represent the integer-type parameters.
 *
 * The type system supports two types of generic type parameters - other types (or variables) and integers.
 * Integer parameters may be concrete values, variables (equal to type variables) or the infinite sigh.
 */
class IntTypeParam {
public:
	/**
	 * An enumeration to determine the actual type of the integer parameter.
	 */
	typedef enum {
		VARIABLE, CONCRETE, INFINITE
	} Type;

	/**
	 * A predefined int-type-parameter constant representing the value 0
	 */
	static const IntTypeParam ZERO;

	/**
	 * A predefined int-type-parameter constant representing the value 1
	 */
	static const IntTypeParam ONE;

	/**
	 * A predefined int-type-parameter constant representing an infinite value.
	 */
	static const IntTypeParam INF;

private:

	/**
	 * The type of the parameter represented by this instance.
	 * 3 bits for compilers with unsigned enum
	 */
	Type type :3;

	union {
		/**
		 * The value represented by the concrete type parameter.
		 */
		std::size_t value;

		/**
		 * The symbol used for the integer type variable.
		 */
		char symbol;
	};

private:

	/**
	 * A private constructor to create a variable integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 *
	 * @param symbol the symbol to be used for the integer type variable
	 */
	IntTypeParam(const char symbol) : type(VARIABLE), symbol(symbol) {
	}

	/**
	 * A private constructor to create a concrete integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 *
	 * @param value the value to be used for the concrete integer type parameter
	 */
	IntTypeParam(const std::size_t value) : type(CONCRETE), value(value) {}

	/**
	 * A private constructor to create a infinite integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 */
	IntTypeParam(const Type) :	type(INFINITE), value(0) {
	}

public:

	/**
	 * A factory method to obtain a integer type parameter variable.
	 *
	 * @param symbol the symbol to be used for the variable
	 * @return an IntTypeParam representing a token for this variable.
	 */
	static IntTypeParam getVariableIntParam(char symbol);

	/**
	 * A factory method to obtain a concrete integer type parameter.
	 *
	 * @param value the value to be represented
	 * @return an IntTypeParam representing a token for this value.
	 */
	static IntTypeParam getConcreteIntParam(std::size_t value);

	/**
	 * A factory method to obtain a integer type parameter representing
	 * the infinite value.
	 *
	 * @return an IntTypeParam representing a token for the infinite value.
	 */
	static IntTypeParam getInfiniteIntParam();

	/**
	 * Tests whether all of the given integer type parameter are concrete.
	 *
	 * @param intTypeParams the list of parameters to be tested
	 * @return true if all are concrete, false otherwise
	 */
	static bool allConcrete(const vector<IntTypeParam>& intTypeParams);

	/**
	 * Implements the equality operator for the IntTypeParam type.
	 */
	bool operator==(const IntTypeParam&) const;

	/**
	 * Implements the less-than relation by lexicographical comparison
	 * of the type / value tuple.
	 */
	bool operator<(const IntTypeParam&) const;

	/**
	 * Provides a string representation for this token type.
	 *
	 * @return a string representation for this type.
	 */
	const string toString() const {
		switch (type) {
		case VARIABLE:
			return ::toString(symbol);
		case CONCRETE:
			return ::toString(value);
		case INFINITE:
			return ::toString("Inf");
		default:
			assert(false && "Invalid parameter type discovered!");
			return "undefined";
		}
	}

	/**
	 * Determines whether this instance is representing a variable integer type
	 * parameter or a concrete value.
	 *
	 * @return false if variable, true otherwise
	 */
	bool isConcrete() const {
		return type != VARIABLE;
	}

	/**
	 * Obtains the type of parameter this instance is.
	 *
	 * @return the type of int-type parameter
	 *
	 * @see Type
	 */
	Type getType() const {
		return type;
	}

	/**
	 * Obtains the value of a concrete int-type parameter. The value is only
	 * properly defined in case the type is CONCRETE. Otherwise an assertion
	 * violation will be triggered.
	 */
	std::size_t getValue() const {
		// TODO: replace with an exception
		assert( type == CONCRETE );
		return value;
	}

	/**
	 * Obtains the symbol of a variable int-type parameter. The symbol is only
	 * properly defined in case the type is VARIABLE. Otherwise an assertion
	 * violation will be triggered.
	 */
	char getSymbol() const {
		// TODO: replace with an exception
		assert( type == VARIABLE );
		return symbol;
	}

};

} // end namespace core
} // end namespace insieme


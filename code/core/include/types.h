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
#include <iterator>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/algorithm/string/join.hpp>

#include "stringutils.h"
#include "containerutils.h"

using std::string;
using std::vector;
using std::map;

// ---------------------------------------- A token for an abstract type ------------------------------

/**
 * The base type for all type tokens. Type tokens are immutable instances of classes derived from this base
 * class and are used to represent the type of data elements and functions (generally types) within the IR.
 *
 * Each type is equipped with a unique name. The name makes them distinguishable. Further, there are two sorts
 * of types. Concrete types represent types which for which actual values exist. For instance, a value for the type
 * int<4> would be 7. Variable types however represent a family of types, e.g. the type int<p> represents all
 * types where the integer-type parameter p can be substituted by some arbitrary value between 0 and infinity (including
 * both). Variable types can only be used as the input/output types of functions.
 */
class Type {

	/**
	 * The name of this type. This name is used to uniquely identify the represented type. Since types
	 * are generally immutable, the type name is marked to be constant.
	 */
	const string name;
public:

	/**
	 * Creates a new type using the given name. The constructor is public, however, since
	 * this class is an abstract class, no actual instance can be created.
	 *
	 * @param name the unique name for this type
	 */
	Type(const std::string& name) : name(name) {}

	/**
	 * A simple, virtual destructor for this abstract class.
	 */
	virtual ~Type() {}

	/**
	 * Checks whether the represented type is a concrete type (hence, it does not have any unbound,
	 * variable type parameters). The method is marked abstract, hence the actual decision whether
	 * a type is concrete or not is made by actual sub-classes.
	 *
	 * @return true if it is a concrete type, false otherwise
	 */
	virtual bool isConcrete() const = 0;

	/**
	 * Retrieves the unique name identifying this type.
	 */
	const string& getName() const { return name; }

	/**
	 * Provides a string representation of this type, which is by default
	 * the actual name of the type. Specific sub-types may override this method
	 * to customize the representation.
	 */
	virtual string toString() const { return getName(); }
};

/**
 * To safely handle type tokens, instances should be maintained via shared pointers. Hence,
 * their life cycle should be handled using the internal shared pointer mechanisms.
 */
typedef std::shared_ptr<Type> TypeRef;


// ---------------------------------------- A class for type variables  ------------------------------

/**
 * Tokens of this type are used to represent type variables. Instances them-self represent types,
 * yet no concrete ones.
 */
class VariableType : public Type {

	/**
	 * Creates a new type variable using the given name.
	 *
	 * @param name the name of the type variable to be created
	 */
	VariableType(const string& name) : Type(format("'%s",name.c_str())){}
public:

	/**
	 * Simply returns the false since variable Types are never concrete.
	 *
	 * @return always false
	 *
	 * @see Type::isConcrete()
	 */
	virtual bool isConcrete() const { return false; }
};


// ---------------------------------------- A token for an abstract type ------------------------------

// pre-definition of abstract type class
class AbstractType;

/**
 * A type definition for a shared reference on an abstract type.
 */
typedef std::shared_ptr<AbstractType> AbstractTypeRef;

/**
 * The abstract type is a special (singleton) token representing the base class for types defined
 * in an abstract way. Hence, type definitions using this type as a base type have to be supported
 * by the code synthesizer.
 */
class AbstractType : public Type {

	/**
	 * The singleton instance of this type (shared among all type manager instances)
	 */
	static AbstractTypeRef instance;

	/**
	 * The default constructor of this type, fixing the name to "abstract".
	 */
	AbstractType() : Type("abstract") {}

public:

	/**
	 * Obtains a reference to the singleton instance of this type.
	 *
	 * @return the singleton instance of this type (accessed via a shared pointer to
	 * 		   be compatible with other type references).
	 */
	static AbstractTypeRef getInstance() { return instance; }

	/**
	 * Overrides the implementation of this method within the parent class (where it is abstract).
	 * The abstract type is always a concrete type.
	 */
	virtual bool isConcrete() const { return true; }
};


// ---------------------------------------- Integer Type Parameters ------------------------------

/**
 * Instances of this class represent the integer-type parameters.
 *
 * The type system supports two types of generic type parameters - other types (or variables) and integers.
 * Integer parameters may be concrete values, variables (equal to type variables) or the infinite sigh.
 */
class IntTypeParam {
private:
	/**
	 * An enumeration to determine the actual type of the integer parameter.
	 */
	typedef enum {
		VARIABLE,
		CONCRETE,
		INFINITE
	} Type;

	/**
	 * The type of the parameter represented by this instance.
	 */
	const Type type;

	/**
	 * A union containing additional information on the represented type parameter.
	 */
	union {
		/**
		 * The value represented by the concrete type parameter.
		 */
		int value;

		/**
		 * The symbol used for the integer type variable.
		 */
		char symbol;
	};

	/**
	 * A private constructor to create a variable integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 *
	 * @param symbol the symbol to be used for the integer type variable
	 */
	IntTypeParam(const char symbol) : type(VARIABLE), symbol(symbol) {};

	/**
	 * A private constructor to create a concrete integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 *
	 * @param value the value to be used for the concrete integer type parameter
	 */
	IntTypeParam(const int value) : type(CONCRETE), value(value)  {};

	/**
	 * A private constructor to create a infinite integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 */
	IntTypeParam() : type(INFINITE) {};

public:

	/**
	 * Provides a string representation for this token type.
	 *
	 * @return a string representation for this type.
	 */
	const string toString() const {
		switch(type) {
		case VARIABLE: return ::toString(symbol);
		case CONCRETE: return ::toString(value);
		case INFINITE: return ::toString("Inf");
		default: throw std::runtime_error("Invalid parameter type discovered!");
		}
	}

	/**
	 * Determines whether this instance is representing a variable integer type
	 * parameter or a concrete value.
	 *
	 * @return false if variable, true otherwise
	 */
	bool isConcrete() const {
		return type!=VARIABLE;
	}

private:

	/**
	 * The singleton instance of the infinite integer type parameter. Since
	 * there is no requirement to maintain multiple of those, this instance is shared
	 * among all places where it is required.
	 */
	static IntTypeParam infinite;

public:

	/**
	 * A factory method to obtain a integer type parameter variable.
	 *
	 * @param symbol the symbol to be used for the variable
	 * @return an IntTypeParam representing a token for this variable.
	 */
	static IntTypeParam getVariableIntParam(char symbol) {
		return IntTypeParam(symbol);
	}

	/**
	 * A factory method to obtain a concrete integer type parameter.
	 *
	 * @param value the value to be represented
	 * @return an IntTypeParam representing a token for this value.
	 */
	static IntTypeParam getConcreteIntParam(int value) {
		return IntTypeParam(value);
	}

	/**
	 * A factory method to obtain a integer type parameter representing
	 * the infinite value.
	 *
	 * @return an IntTypeParam representing a token for the infinite value.
	 */
	static IntTypeParam getInfiniteIntParam() {
		return infinite;
	}

};

// ---------------------------------------- User defined Type ------------------------------


class UserType : public Type {
	static string buildNameString(const string& name, 
			const vector<TypeRef>& typeParams, const vector<IntTypeParam>& intParams) {
		
		// create output buffer
		std::stringstream res;
		res << name;

		// check whether there are type parameters
		if (!typeParams.empty() || !intParams.empty()) {

			// convert type parameters to strings ...
			vector<string> list;
			std::transform(typeParams.cbegin(), typeParams.cend(), back_inserter(list), [](const TypeRef cur) { return cur->getName(); });
			std::transform(intParams.cbegin(), intParams.cend(), back_inserter(list), [](const IntTypeParam cur) { return cur.toString(); });

			res << "<" << boost::join(list, ",") << ">";
		}
		return res.str();
	}

	const vector<TypeRef> typeParams;
	const vector<IntTypeParam> intParams;
	const TypeRef baseType;

public:
	UserType(const string& name,
			vector<TypeRef> typeParams = vector<TypeRef>(),
			vector<IntTypeParam> intTypeParams = vector<IntTypeParam>(),
			TypeRef baseType = AbstractType::getInstance())
	   :
		Type(buildNameString(name, typeParams, intTypeParams)),
		typeParams(typeParams),
		intParams(intTypeParams),
		baseType(baseType) {}

	virtual bool isConcrete() const {
		// init result (to perform lazy evaluation)
		bool res = true;

		// check whether there is a variable type within the type-parameter list
		auto p = [](const TypeRef cur) { return cur->isConcrete(); };
		res = res && (std::find_if(typeParams.cbegin(), typeParams.cend(), p) != typeParams.end());

		// check whether there is a variable symbol within the integer-parameter list
		auto q = [](const IntTypeParam cur) { return cur.isConcrete(); };
		res = res && std::find_if(intParams.cbegin(), intParams.cend(), q) != intParams.end();

		return res;
	}
};

class TupleType : public Type {
	const vector<TypeRef> elementTypes;
};

class FunctionType : public Type {
	const TypeRef returnType;
	const TypeRef argumentType;
};

class NamedCompositeType : public Type {
	const map<const string, const TypeRef> elements;
};
class StructType : public NamedCompositeType {
};
class UnionType : public NamedCompositeType {
};

class ArrayType : public Type {
	friend class TypeManager;
	const TypeRef elementType;
	const unsigned dimensions;

	ArrayType(const TypeRef elementType, const unsigned dim) : 
		Type(format("array<%d,%s>", dim, elementType->getName().c_str())),
			elementType(elementType), dimensions(dim)  { };

public:
	virtual bool isConcrete() const { elementType->isConcrete(); }
};
typedef const std::shared_ptr<ArrayType> ArrayTypeRef;

class VectorType : public UserType {
	VectorType(TypeRef elementType, IntTypeParam size) : UserType("vector", singleton(elementType), singleton(size)) {}
};

class ReferenceType : public UserType {
	ReferenceType(TypeRef elementType) : UserType("ref", singleton(elementType), vector<IntTypeParam>()) {}
};

class ChannelType : public Type {
	TypeRef type;
	unsigned bufferLength;
};





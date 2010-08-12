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
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/algorithm/string/join.hpp>

#include "string_utils.h"
#include "container_utils.h"

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
	 * Tests whether the represented type is a function type.
	 * Since most types are not function types, the default implementation returns false.
	 */
	virtual bool isFunctionType() const { return false; }

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
typedef std::shared_ptr<Type> TypePtr;


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
typedef std::shared_ptr<AbstractType> AbstractTypePtr;

/**
 * The abstract type is a special (singleton) token representing the base class for types defined
 * in an abstract way. Hence, type definitions using this type as a base type have to be supported
 * by the code synthesizer.
 */
class AbstractType : public Type {

	/**
	 * The singleton instance of this type (shared among all type manager instances)
	 */
	static AbstractTypePtr instance;

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
	static AbstractTypePtr getInstance() { return instance; }

	/**
	 * Overrides the implementation of this method within the parent class (where it is abstract).
	 * The abstract type is always a concrete type.
	 */
	virtual bool isConcrete() const { return true; }
};


// ---------------------------------------- A tuple type ------------------------------

/**
 * The tuple type represents a special kind of type representing a simple aggregation
 * (cross-product) of other types. It thereby forms the foundation for functions
 * accepting multiple input parameters.
 */
class TupleType : public Type {
	/**
	 * The list of element types this tuple is consisting of.
	 */
	const vector<TypePtr> elementTypes;

	/**
	 * A private utility method building the name of a tuple type.
	 *
	 * @param elementTypes	the list of element types
	 * @return a string representation of the resulting tuple type
	 */
	static string buildNameString(const vector<TypePtr>& elementTypes);

public:
	/**
	 * Creates a new tuple type based on the given element types.
	 */
	TupleType(vector<TypePtr> elementTypes) :
		Type(buildNameString(elementTypes)),
		elementTypes(elementTypes) {};

	/**
	 * Tests whether this generic type instance represents a concrete or variable type.
	 *
	 * @return true if it is a concrete type, hence no type parameter is variable, false otherwise
	 */
	virtual bool isConcrete() const {
		// just test whether all sub-types are concrete
		auto p = [](const TypePtr cur) {return cur->isConcrete();};
		return (std::find_if(elementTypes.cbegin(), elementTypes.cend(), p) != elementTypes.end());
	}
};


// ---------------------------------------- Function Type ------------------------------

/**
 * This special type represents
 */
class FunctionType : public Type {
	const TypePtr argumentType;
	const TypePtr returnType;
public:

	FunctionType(TypePtr argumentType, TypePtr returnType)
		: Type(format("(%s -> %s)", argumentType->getName().c_str(), returnType->getName().c_str())),
		  argumentType(argumentType),
		  returnType(returnType) {};

	/**
	 * Ensures that a function type is considered to be concrete. Since
	 * generic functions are supported within the IR, type variables are
	 * not preventing an element from having a generic function type.
	 *
	 * @return true if it is a concrete type, hence no type parameter is variable, false otherwise
	 */
	virtual bool isConcrete() const {
		// all function types are inhabited
		return true;
	}

	/**
	 * Ensures that this type is identifiable as a function type by returning true.
	 */
	virtual bool isFunctionType() const { return true; }
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
	Type type : 2;

	union {
		/**
		 * The value represented by the concrete type parameter.
		 */
		unsigned short value;

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
	IntTypeParam(const unsigned short value) : type(CONCRETE), value(value)  {};

	/**
	 * A private constructor to create a infinite integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 */
	IntTypeParam(const Type type) : type(INFINITE) {};


public:

	/**
	 * Implements the equality operator for the IntTypeParam type.
	 */
	bool operator==(const IntTypeParam&);

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
	static IntTypeParam getConcreteIntParam(unsigned short value) {
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

// ---------------------------------------- Generic Type ------------------------------

/**
 * This type represents a generic type which can be used to represent arbitrary user defined
 * or derived types. Each generic type can be equipped with a number of generic type and integer
 * parameters. Those are represented using other types and IntTypeParam instances.
 */
class GenericType : public Type {

	/**
	 * The list of type parameters being part of this type specification.
	 */
	const vector<TypePtr> typeParams;

	/**
	 * The list of integer type parameter being part of this type specification.
	 */
	const vector<IntTypeParam> intParams;

	/**
	 * the base type of this type if there is any.
	 *
	 * TODO: is this actually required?
	 */
	const TypePtr baseType;

	/**
	 * A private utility method building the name of a generic type.
	 *
	 * @param name			the name of the generic type (only prefix, generic parameters are added automatically)
	 * @param typeParams 	the list of type parameters to be appended
	 * @param intParams		the list of integer type parameters to be appended
	 * @return a string representation of the type
	 */
	static string buildNameString(const string& name, const vector<TypePtr>& typeParams, const vector<IntTypeParam>& intParams);

public:

	/**
	 * Creates an new generic type instance based on the given parameters.
	 *
	 * @param name 			the name of the new type (only the prefix)
	 * @param typeParams	the type parameters of this type, concrete or variable
	 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
	 * @param baseType		the base type of this generic type
	 */
	GenericType(const string& name,
			vector<TypePtr> typeParams = vector<TypePtr>(),
			vector<IntTypeParam> intTypeParams = vector<IntTypeParam>(),
			TypePtr baseType = AbstractType::getInstance())
	   :
		Type(buildNameString(name, typeParams, intTypeParams)),
		typeParams(typeParams),
		intParams(intTypeParams),
		baseType(baseType) {}

	/**
	 * Tests whether this generic type instance represents a concrete or variable type.
	 *
	 * @return true if it is a concrete type, hence no type parameter is variable, false otherwise
	 */
	virtual bool isConcrete() const {
		// init result (to perform lazy evaluation)
		bool res = true;

		// check whether there is a variable type within the type-parameter list
		auto p = [](const TypePtr cur) { return cur->isConcrete(); };
		res = res && (std::find_if(typeParams.cbegin(), typeParams.cend(), p) != typeParams.end());

		// check whether there is a variable symbol within the integer-parameter list
		auto q = [](const IntTypeParam cur) { return cur.isConcrete(); };
		res = res && std::find_if(intParams.cbegin(), intParams.cend(), q) != intParams.end();

		return res;
	}
};


typedef std::shared_ptr<FunctionType> FunctionTypePtr;

class NamedCompositeType : public Type {
	const map<const string, const TypePtr> elements;
};
class StructType : public NamedCompositeType {
};
class UnionType : public NamedCompositeType {
};

class ArrayType : public Type {
	friend class TypeManager;
	const TypePtr elementType;
	const unsigned dimensions;

	ArrayType(const TypePtr elementType, const unsigned dim) : 
		Type(format("array<%d,%s>", dim, elementType->getName().c_str())),
			elementType(elementType), dimensions(dim)  { };

public:
	virtual bool isConcrete() const { elementType->isConcrete(); }
};
typedef const std::shared_ptr<ArrayType> ArrayTypeRef;

class VectorType : public GenericType {
	VectorType(TypePtr elementType, IntTypeParam size) : GenericType("vector", toVector(elementType), toVector(size)) {}
};

class ReferenceType : public GenericType {
	ReferenceType(TypePtr elementType) : GenericType("ref", toVector(elementType), vector<IntTypeParam>()) {}
};

class ChannelType : public Type {
	TypePtr type;
	unsigned bufferLength;
};





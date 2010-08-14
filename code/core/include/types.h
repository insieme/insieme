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
#include <set>
#include <stdexcept>
#include <string>
#include <ostream>
#include <vector>

#include <boost/algorithm/string/join.hpp>
#include <boost/functional/hash.hpp>

#include "annotated_ptr.h"
#include "container_utils.h"
#include "instance_manager.h"
#include "string_utils.h"
#include "visitor.h"

using std::string;
using std::vector;
using std::map;

enum {
	TYPE_HASH_ABSTRACT
};

// ------------------------------ Forward Declarations ------------------------------------

class Type;
typedef AnnotatedPtr<const Type> TypePtr;

class TypeVariable;
typedef AnnotatedPtr<const TypeVariable> TypeVariablePtr;

class FunctionType;
typedef AnnotatedPtr<const FunctionType> FunctionTypePtr;

class TupleType;
typedef AnnotatedPtr<const TupleType> TupleTypePtr;

class ArrayType;
typedef AnnotatedPtr<const ArrayType> ArrayTypePtr;

class VectorType;
typedef AnnotatedPtr<const VectorType> VectorTypePtr;

class RefType;
typedef AnnotatedPtr<const RefType> RefTypePtr;

class ChannelType;
typedef AnnotatedPtr<const ChannelType> ChannelTypePtr;

class GenericType;
typedef AnnotatedPtr<const GenericType> GenericTypePtr;

class NamedCompositeType;
typedef AnnotatedPtr<const NamedCompositeType> NamedCompositeTypePtr;

class StructType;
typedef AnnotatedPtr<const StructType> StructTypePtr;

class UnionType;
typedef AnnotatedPtr<const UnionType> UnionTypePtr;

// ---------------------------------------- Integer Type Parameters ------------------------------

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
		VARIABLE, CONCRETE, INFINITE
	} Type;

	/**
	 * The type of the parameter represented by this instance.
	 * 3 bits for compilers with unsigned enum
	 */
	Type type :3;

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
	IntTypeParam(const char symbol) : type(VARIABLE), symbol(symbol) {
	}

	/**
	 * A private constructor to create a concrete integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 *
	 * @param value the value to be used for the concrete integer type parameter
	 */
	IntTypeParam(const unsigned short value) : type(CONCRETE), value(value) {
	}

	/**
	 * A private constructor to create a infinite integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 */
	IntTypeParam(const Type type) :	type(INFINITE), value(0) {
	}

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
		switch (type) {
		case VARIABLE:
			return ::toString(symbol);
		case CONCRETE:
			return ::toString(value);
		case INFINITE:
			return ::toString("Inf");
		default:
			throw std::runtime_error("Invalid parameter type discovered!");
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
		return IntTypeParam(INFINITE);
	}


	/**
	 * Tests whether all of the given integer type parameter are concrete.
	 *
	 * @param intTypeParams the list of parameters to be tested
	 * @return true if all are concrete, false otherwise
	 */
	static bool allConcrete(const vector<IntTypeParam>& intTypeParams);


};

// ---------------------------------- Type Manager ----------------------------------------

class TypeManager: public InstanceManager<const Type, TypePtr> {

	/**
	 * The internal implementation of the actual lookup method. The method
	 * is trying to look up the given type within the internal store. If the same
	 * data element is already present, a pointer to the present copy is returned. Otherwise,
	 * the given copy is cloned and added to the store. A pointer to the new element is
	 * then returned.
	 *
	 * @param type the type node for which a master copy should be obtained
	 * @return the pointer to the obtained element
	 */
	TypePtr getTypePtrInternal(const Type& type);

public:

	/**
	 * A generic wrapper enclosing the internal implementation of the lookup method.
	 *
	 * @param type the type node for which a master copy should be obtained
	 * @return the pointer to the obtained element
	 * @see TypeManager::getTypePtrInternal(const Type&)
	 */
	template<typename T>
	AnnotatedPtr<const T> getTypePtr(const T& type) {
		return dynamic_pointer_cast<const T>(getTypePtrInternal(type));
	}
};


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
class Type: public Visitable<TypePtr> {

	/**
	 * The name of this type. This name is used to uniquely identify the represented type. Since types
	 * are generally immutable, the type name is marked to be constant.
	 *
	 * TODO: introduce lazy evaluation to speed up the creation of instances
	 */
	const string name;

	/**
	 * A flag indicating whether this type represents a concrete type (true) or a family of types
	 * based on type variables (false).
	 */
	const bool concrete;

	/**
	 * A flag indicating whether this type represents a function type (true) or a data type (false).
	 */
	const bool functionType;

	/**
	 * The hash value of this type derived once. This value will be required frequently,
	 * hence evaluating it once and reusing it helps reducing computational overhead. Since
	 * type tokens are immutable, the hash does not have to be altered after the creation of a type.
	 */
	const std::size_t hashCode;

public:

	/**
	 * Creates a new type using the given name. The constructor is public, however, since
	 * this class is an abstract class, no actual instance can be created.
	 *
	 * @param name the unique name for this type
	 * @param concrete a flag indicating whether this type is a concrete type or represents a family of types
	 * 					due to type variables. Default: true
	 * @param functionType a flag indicating whether this type is a function type or not. Default: false
	 */
	Type(const std::string& name, const bool concrete = true, const bool functionType = false)
		: name(name), concrete(concrete), functionType(functionType), hashCode(boost::hash_value(name)) {}

	/**
	 * A simple, virtual destructor for this abstract class.
	 */
	virtual ~Type() {
	}

	/**
	 * Checks whether the represented type is a concrete type (hence, it does not have any unbound,
	 * variable type parameters).
	 *
	 * @return true if it is a concrete type, false otherwise
	 */
	bool isConcrete() const {
		return concrete;
	}

	/**
	 * Tests whether the represented type is a function type.
	 * Since most types are not function types, the default implementation returns false.
	 */
	bool isFunctionType() const {
		return functionType;
	}

	/**
	 * Retrieves the unique name identifying this type.
	 */
	const string& getName() const {
		return name;
	}

	/**
	 * Provides a string representation of this type, which is by default
	 * the actual name of the type. Specific sub-types may override this method
	 * to customize the representation.
	 */
	virtual string toString() const {
		return getName();
	}

	/**
	 * A default implementation of the equals operator comparing the actual
	 * names of the types.
	 */
	bool operator==(const Type& other) const {
		// test for identity
		if (this == &other) {
			return true;
		}

		// fast hash code test
		if (hashCode != other.hashCode) {
			return false;
		}

		// slow name comparison
		return name == other.name;
	}

	/**
	 * Computes a hash code for this instance covering all constant fields making
	 * the type unique.
	 *
	 * Note: this function is not virtual, so it should not be overridden in sub-classes.
	 * Since every type is uniquely identified by its name, there should not be a need
	 * for doing so.
	 *
	 * @return the hash code derived for this type.
	 */
	std::size_t hash() const {
		// retrieve cached hash code
		return hashCode;
	}

	/**
	 * Retrieves a clone of this object (a newly allocated instance of this class)
	 */
	virtual Type* clone() const = 0;

	/**
	 * Retrieves references to types revered to by this type. The default implementation
	 * returns and empty list. Sub-classes may override the method to return sub-types.
	 *
	 * The method is mainly used to ensure that all types referred to by one type are present
	 * within the same type manager (to ensure their existence).
	 *
	 * @return a list containing all types this type is based on.
	 */
	virtual ChildList getChildren() const {
		return newChildList();
	}



	// ---------------------------------- Type Utils ----------------------------------------


	/**
	 * Tests whether this generic type instance represents a concrete or variable type.
	 *
	 * @return true if it is a concrete type, hence no type parameter is variable, false otherwise
	 */
	static bool allConcrete(const vector<TypePtr>& elementTypes);

};

// ---------------------------------------- A class for type variables  ------------------------------

/**
 * Tokens of this type are used to represent type variables. Instances them-self represent types,
 * yet no concrete ones.
 */
class TypeVariable: public Type {

	/**
	 * Creates a new type variable using the given name.
	 *
	 * @param name the name of the type variable to be created
	 */
	TypeVariable(const string& name) : Type(format("'%s", name.c_str()), false, false) { }

public:

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a type variable pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 */
	static TypeVariablePtr get(TypeManager& manager, const string& name) {
		return manager.getTypePtr(TypeVariable(name));
	}

	/**
	 * Creates a clone of this node.
	 */
	virtual TypeVariable* clone() const {
		return new TypeVariable(*this);
	}

};


// ---------------------------------------- A tuple type ------------------------------

/**
 * The tuple type represents a special kind of type representing a simple aggregation
 * (cross-product) of other types. It thereby forms the foundation for functions
 * accepting multiple input parameters.
 */
class TupleType: public Type {
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

	/**
	 * Creates a new tuple type based on the given element types.
	 */
	TupleType(vector<TypePtr> elementTypes) :
		Type(buildNameString(elementTypes), allConcrete(elementTypes)), elementTypes(elementTypes) {}

public:

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a tuple type pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 */
	static TupleTypePtr get(TypeManager& manager, vector<TypePtr> elementTypes) {
		return manager.getTypePtr(TupleType(elementTypes));
	}

	/**
	 * Creates a clone of this node.
	 */
	virtual TupleType* clone() const {
		return new TupleType(*this);
	}
};

// ---------------------------------------- Function Type ------------------------------
// FIXME: to new mode ...
/**
 * This special type represents
 */
class FunctionType: public Type {
	const TypePtr argumentType;
	const TypePtr returnType;
public:

	FunctionType(TypePtr argumentType, TypePtr returnType) :
		Type(format("(%s -> %s)", argumentType->getName().c_str(), returnType->getName().c_str())), argumentType(
				argumentType), returnType(returnType) {
	}
	;

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
	virtual bool isFunctionType() const {
		return true;
	}
};


// ---------------------------------------- Generic Type ------------------------------

/**
 * This type represents a generic type which can be used to represent arbitrary user defined
 * or derived types. Each generic type can be equipped with a number of generic type and integer
 * parameters. Those are represented using other types and IntTypeParam instances.
 */
class GenericType: public Type {

	/**
	 * The list of type parameters being part of this type specification.
	 */
	const vector<TypePtr> typeParams;

	/**
	 * The list of integer type parameter being part of this type specification.
	 */
	const vector<IntTypeParam> intParams;

	/**
	 * The base type of this type if there is any. The pointer is pointing toward
	 * the base type or is NULL in case there is no base type (hence, it would be
	 * an abstract type).
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
	static string buildNameString(const string& name, const vector<TypePtr>& typeParams,
			const vector<IntTypeParam>& intParams);

	/**
	 * Creates an new generic type instance based on the given parameters.
	 *
	 * @param name 			the name of the new type (only the prefix)
	 * @param typeParams	the type parameters of this type, concrete or variable
	 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
	 * @param baseType		the base type of this generic type
	 */
	GenericType(const string& name,
			const vector<TypePtr>& typeParams = vector<TypePtr> (),
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> (),
			const TypePtr& baseType = NULL)
		:
			Type(buildNameString(name, typeParams, intTypeParams), Type::allConcrete(typeParams) && IntTypeParam::allConcrete(intTypeParams)),
			typeParams(typeParams),
			intParams(intTypeParams),
			baseType(baseType) { }

public:

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a generic type pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 *
	 * @param name 			the name of the new type (only the prefix)
	 * @param typeParams	the type parameters of this type, concrete or variable
	 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
	 * @param baseType		the base type of this generic type
	 */
	static GenericTypePtr get(TypeManager& manager,
			const string& name,
			const vector<TypePtr>& typeParams = vector<TypePtr> (),
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> (),
			const TypePtr& baseType = NULL);

	/**
	 * Creates a clone of this node.
	 */
	virtual GenericType* clone() const {
		return new GenericType(*this);
	}

	/**
	 * Retrieves the child types referenced by this generic type.
	 *
	 * @return the
	 */
	virtual ChildList getChildren() const {
		auto res = newChildList(typeParams);

		// further add base types
		if (baseType) {
			res->push_back(baseType);
		}

		return res;
	}

};

//class NamedCompositeType: public Type {
//	const map<const string, const TypePtr> elements;
//};
//class StructType: public NamedCompositeType {
//};
//class UnionType: public NamedCompositeType {
//};
//
//class ArrayType: public Type {
//	friend class TypeManager;
//	const TypePtr elementType;
//	const unsigned dimensions;
//
//	ArrayType(const TypePtr elementType, const unsigned dim) :
//		Type(format("array<%d,%s>", dim, elementType->getName().c_str())), elementType(elementType), dimensions(dim) {
//	}
//	;
//
//public:
//	virtual bool isConcrete() const {
//		return elementType->isConcrete();
//	}
//};
//
//class VectorType: public GenericType {
//	VectorType(TypePtr elementType, IntTypeParam size) :
//		GenericType("vector", toVector(elementType), toVector(size)) {
//	}
//};
//
//class ReferenceType: public GenericType {
//	ReferenceType(TypePtr elementType) :
//		GenericType("ref", toVector(elementType), vector<IntTypeParam> ()) {
//	}
//};
//
//class ChannelType: public Type {
//	TypePtr type;
//	unsigned bufferLength;
//};



// ---------------------------------------------- Utility Functions ------------------------------------

/**
 * Allows to compute the hash value of a type.
 *
 * @param type the type for which a hash value should be computed
 * @return the computed hash value
 */
std::size_t hash_value(const Type& type);


/**
 * Allows this type to be printed to a stream (especially useful during debugging and
 * within test cases where equals values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Type& type);




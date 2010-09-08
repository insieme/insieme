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
#include <unordered_map>
#include <stdexcept>
#include <string>
#include <ostream>
#include <vector>

#include <boost/functional/hash.hpp>

#include "ast_node.h"
#include "annotated_ptr.h"
#include "container_utils.h"
#include "identifier.h"
#include "instance_manager.h"
#include "string_utils.h"

using std::string;
using std::vector;
using std::map;

namespace insieme {
namespace core {

// ------------------------------ Forward Declarations ------------------------------------

DECLARE_NODE_TYPE(Type);

DECLARE_NODE_TYPE(TypeVariable);

DECLARE_NODE_TYPE(FunctionType);
DECLARE_NODE_TYPE(TupleType);
DECLARE_NODE_TYPE(GenericType);
DECLARE_NODE_TYPE(NamedCompositeType);
DECLARE_NODE_TYPE(RecursiveType);

DECLARE_NODE_TYPE(ArrayType);
DECLARE_NODE_TYPE(VectorType);
DECLARE_NODE_TYPE(RefType);
DECLARE_NODE_TYPE(ChannelType);

DECLARE_NODE_TYPE(StructType);
DECLARE_NODE_TYPE(UnionType);

DECLARE_NODE_TYPE(RecursiveTypeDefinition);

/**
 * This class is used to represent integer parameters of generic types.
 */
class IntTypeParam;

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
class Type: public Node {

	/**
	 * Allow the test case to access private methods.
	 */
	template<typename PT>
	friend void basicTypeTests(PT, bool, bool, const Node::ChildList& children = Node::ChildList());

private:

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

	virtual Type* clone(NodeManager& manager) const = 0;

protected:

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
		: Node(TYPE,boost::hash_value(name)), name(name), concrete(concrete), functionType(functionType) {}

public:

	/**
	 * A simple, virtual destructor for this abstract class.
	 */
	virtual ~Type() { }

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
	std::ostream& printTo(std::ostream& out) const {
		return (out << getName());
	}

	/**
	 * A default implementation of the equals operator comparing the actual
	 * names of the types.
	 */
	bool equals(const Node& other) const {
		// precondition: other must be a type
		assert( dynamic_cast<const Type*>(&other) && "Type violation by base class!" );

		// convert (statically) and check the type name
		const Type& ref = static_cast<const Type&>(other);
		// TODO: improve this by eliminating the name!
		return name == ref.name;
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

	/**
	 * Creates a clone of this node.
	 */
	virtual TypeVariable* clone(NodeManager&) const {
		return new TypeVariable(*this);
	}

protected:

	/**
	 * Creates a empty child list since this node represents a terminal node.
	 */
	virtual OptionChildList getChildNodes() const {
		// return an option child list filled with an empty list
		return OptionChildList(new ChildList());
	}

public:

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a type variable pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 */
	static TypeVariablePtr get(NodeManager& manager, const string& name);

};


// ---------------------------------------- A tuple type ------------------------------

/**
 * The tuple type represents a special kind of type representing a simple aggregation
 * (cross-product) of other types. It thereby forms the foundation for functions
 * accepting multiple input parameters.
 */
class TupleType: public Type {

public:

	/**
	 * The type used to represent list of tuple elements.
	 */
	typedef vector<TypePtr> ElementTypeList;

private:

	/**
	 * The list of element types this tuple is consisting of.
	 */
	const ElementTypeList elementTypes;

	/**
	 * A private utility method building the name of a tuple type.
	 *
	 * @param elementTypes	the list of element types
	 * @return a string representation of the resulting tuple type
	 */
	static string buildNameString(const ElementTypeList& elementTypes);

	/**
	 * Creates a new tuple type based on the given element types.
	 */
	TupleType(const ElementTypeList& elementTypes) :
		Type(buildNameString(elementTypes), allConcrete(elementTypes)), elementTypes(elementTypes) {}

	/**
	 * Creates a clone of this node.
	 */
	virtual TupleType* clone(NodeManager& manager) const {
		return new TupleType(manager.getAll(elementTypes));
	}

protected:

	/**
	 * Creates a empty child list since this node represents a terminal node.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a tuple type pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 *
	 * @param manager the manager to obtain the new type reference from
	 * @param elementTypes the list of element types to be used to form the tuple
	 */
	static TupleTypePtr get(NodeManager& manager, const ElementTypeList& elementTypes = ElementTypeList());

};

// ---------------------------------------- Function Type ------------------------------

/**
 * This type corresponds to the type of a function. It specifies the argument types and the
 * value returned by the members of this type.
 */
class FunctionType: public Type {

	/**
	 * The type of element accepted as an argument by this function type.
	 */
	const TypePtr argumentType;

	/**
	 * The type of value produced by this function type.
	 */
	const TypePtr returnType;

	/**
	 * Creates a new instance of this type based on the given in and output types.
	 *
	 * @param argumentType a reference to the type used as argument
	 * @param returnType a reference to the type used as return type
	 */
	FunctionType(const TypePtr& argumentType, const TypePtr& returnType) :
		Type(format("(%s->%s)", argumentType->getName().c_str(), returnType->getName().c_str()), true, true),
		argumentType(argumentType), returnType(returnType) {
	}

	/**
	 * Creates a clone of this node.
	 */
	virtual FunctionType* clone(NodeManager& manager) const {
		return new FunctionType(manager.get(argumentType), manager.get(returnType));
	}

protected:

	/**
	 * Creates a empty child list since this node represents a terminal node.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a function type pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 *
	 * @param manager the manager to be used for handling the obtained type pointer
	 * @param argumentType the argument type of the type to be obtained
	 * @param returnType the type of value to be returned by the obtained function type
	 * @return a pointer to a instance of the required type maintained by the given manager
	 */
	static FunctionTypePtr get(NodeManager& manager, const TypePtr& argumentType, const TypePtr& returnType);

};


// ---------------------------------------- Generic Type ------------------------------

/**
 * This type represents a generic type which can be used to represent arbitrary user defined
 * or derived types. Each generic type can be equipped with a number of generic type and integer
 * parameters. Those are represented using other types and IntTypeParam instances.
 */
class GenericType: public Type {

	/**
	 * The name of this generic type.
	 */
	const Identifier familyName;

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

protected:

	/**
	 * Creates an new generic type instance based on the given parameters.
	 *
	 * @param name 			the name of the new type (only the prefix)
	 * @param typeParams	the type parameters of this type, concrete or variable
	 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
	 * @param baseType		the base type of this generic type
	 */
	GenericType(const Identifier& name,
			const vector<TypePtr>& typeParams = vector<TypePtr> (),
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> (),
			const TypePtr& baseType = NULL);

	/**
	 * Creates a clone of this node.
	 */
	virtual GenericType* clone(NodeManager& manager) const {
		return new GenericType(familyName, manager.getAll(typeParams), intParams, manager.get(baseType));
	}

	/**
	 * Obtains a list of all type parameters and the optional base type
	 * referenced by this generic type.
	 */
	virtual OptionChildList getChildNodes() const;

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
	static GenericTypePtr get(NodeManager& manager,
			const Identifier& name,
			const vector<TypePtr>& typeParams = vector<TypePtr> (),
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> (),
			const TypePtr& baseType = NULL);

	/**
	 * Retrieves all type parameter associated to this generic type.
	 *
	 * @return a const reference to the internally maintained type parameter list.
	 */
	const vector<TypePtr>& getTypeParameter() const {
		return typeParams;
	}

	/**
	 * Retrieves a list of all integer type parameters associated to this type.
	 *
	 * @return a const reference to the internally maintained integer type parameter list.
	 */
	const vector<IntTypeParam>& getIntTypeParameter() const {
		return intParams;
	}

	/**
	 * Retrieves a reference to the base type associated with this type.
	 *
	 * @return a reference to the base type of this type.
	 */
	const TypePtr& getBaseType() const {
		return baseType;
	}
};

// ---------------------------------------- Recursive Type ------------------------------


class RecursiveTypeDefinition : public Node {

public:

	typedef std::pair<TypeVariablePtr, TypePtr> Definition;
	typedef std::unordered_map<TypeVariablePtr, TypePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> Definitions;

private:

	/**
	 * The list of definitions this recursive type definition is consisting of.
	 */
	const Definitions definitions;

	RecursiveTypeDefinition(const Definitions& definitions);

	RecursiveTypeDefinition* clone(NodeManager& manager) const;

protected:

	virtual bool equals(const Node& other) const;

	virtual OptionChildList getChildNodes() const;

public:

	static RecursiveTypeDefinitionPtr get(NodeManager& manager, const Definitions& definitions);

	const Definitions& getDefinitions() {
		return definitions;
	}

	const TypePtr getDefinitionOf(const TypeVariablePtr& variable) const;

	virtual std::ostream& printTo(std::ostream& out) const;

};

/**
 * This type connector allows to define recursive type within the IR language. Recursive
 * types are types which are defined by referencing to their own type. The definition of
 * a list may be consisting of a pair, where the first element is corresponding to a head
 * element and the second to the remaining list. Therefore, a list is defined using its own
 * type.
 *
 * This implementation allows to define mutually recursive data types, hence, situations
 * in which the definition of multiple recursive types are interleaved.
 */
class RecursiveType: public Type {

	/**
	 * The name of the type variable describing this type.
	 */
	const TypeVariablePtr typeVariable;

	/**
	 * The definition body of this recursive type. Identical definitions may be
	 * shared among recursive type definitions.
	 */
	const RecursiveTypeDefinitionPtr definition;


	/**
	 * A constructor for creating a new recursive type.
	 */
	RecursiveType(const TypeVariablePtr& typeVariable, const RecursiveTypeDefinitionPtr& definition);

	/**
	 * Creates a clone of this node.
	 */
	virtual RecursiveType* clone(NodeManager& manager) const;

	/**
	 * Obtains a list of all sub-nodes referenced by this AST node.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	/**
	 * A factory method for obtaining a new recursive type instance.
	 *
	 * @param manager the manager which should be maintaining the new instance
	 * @param typeVariable the name of the variable used within the recursive type definition for representing the
	 * 					   recursive type to be defined by the resulting type instance.
	 * @param definition the definition of the recursive type.
	 */
	static RecursiveTypePtr get(NodeManager& manager, const TypeVariablePtr& typeVariable, const RecursiveTypeDefinitionPtr& definition);

};

// --------------------------------- Named Composite Type ----------------------------

/**
 * This abstract type is used to form a common basis for the struct and union types which
 * both consist of a list of typed elements.
 */
class NamedCompositeType: public Type {

public:

	/**
	 * Defines the type used for representing a element entry.
	 */
	typedef std::pair<Identifier, TypePtr> Entry;

	/**
	 * The type used to represent a list of element entries.
	 */
	typedef vector<Entry> Entries;

private:

	/**
	 * The type entries this composed type is based on.
	 */
	const Entries entries;

protected:

	/**
	 * Creates a new named composite type having the given name and set of entries.
	 *
	 * @param prefix the prefix to be used for the new type (union or struct)
	 * @param entries the entries of the new type
	 *
	 * @throws std::invalid_argument if the same identifier is used more than once within the type
	 */
	NamedCompositeType(const string& prefix, const Entries& entries);

	static Entries getEntriesFromManager(NodeManager& manager, Entries entries);


	/**
	 * Obtains a list of all child sub-types used within this struct.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	/**
	 * Retrieves the set of entries this composed type is based on.
	 *
	 * @return a reference to the internally maintained list of entries.
	 */
	const Entries& getEntries() const {
		return entries;
	}

private:

	/**
	 * A static utility function composing the name of this type.
	 *
	 * @param prefix the prefix of the type name (union or struct)
	 * @param entries the list of entries forming the content of this type
	 */
	static string buildNameString(const string& prefix, const Entries& entries);

	/**
	 * Checks whether all types within the given list of entries are concrete types. If so, true
	 * is returned, false otherwise.
	 *
	 * @param elements the list of elements which's types should be checked
	 * @return true if all are concrete, false otherwise
	 */
	static bool allConcrete(const Entries&);

};

// --------------------------------- Struct Type ----------------------------

/**
 * The type used to represent structs / records.
 */
class StructType: public NamedCompositeType {

	/**
	 * Creates a new instance of this type based on the given entries.
	 *
	 * @param entries the entries the new type should consist of
	 * @see NamedCompositeType::NamedCompositeType(const string&, const Entries&)
	 */
	StructType(const Entries& entries) : NamedCompositeType("struct", entries) {}

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual StructType* clone(NodeManager& manager) const {
		return new StructType(NamedCompositeType::getEntriesFromManager(manager, getEntries()));
	}

public:

	/**
	 * A factory method allowing to obtain a pointer to a struct type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager the manager which should be responsible for maintaining the new
	 * 				  type instance and all its referenced elements.
	 * @param entries the set of entries the new type should consist of
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static StructTypePtr get(NodeManager& manager, const Entries& entries);

};


// --------------------------------- Union Type ----------------------------

/**
 * The type used to represent unions.
 */
class UnionType: public NamedCompositeType {

	/**
	 * Creates a new instance of this type based on the given entries.
	 *
	 * @param entries the entries the new type should consist of
	 * @see NamedCompositeType::NamedCompositeType(const string&, const Entries&)
	 */
	UnionType(const Entries& elements) : NamedCompositeType("union", elements) {}

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual UnionType* clone(NodeManager& manager) const {
		return new UnionType(NamedCompositeType::getEntriesFromManager(manager, getEntries()));
	}

public:

	/**
	 * A factory method allowing to obtain a pointer to a union type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager the manager which should be responsible for maintaining the new
	 * 				  type instance and all its referenced elements.
	 * @param entries the set of entries the new type should consist of
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static UnionTypePtr get(NodeManager& manager, const Entries& entries);

};

// --------------------------------- Single Element Type ----------------------------

/**
 * This abstract type is used as a common base class for a serious of intrinsic types
 * which all require a single element type. It thereby represents a specialized version
 * of a generic type.
 */
class SingleElementType : public GenericType {
protected:

	/**
	 * Creates a new instance of this class using the given name, element type and
	 * integer parameters.
	 *
	 * @param name the (prefix) of the name of the new type
	 * @param elementType its single type parameter
	 * @param intTypeParams additional integer type parameters. By default this parameters
	 * 						will be set to an empty set.
	 */
	SingleElementType(const string& name,
			const TypePtr& elementType,
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> ()) :
		GenericType(name, toVector(elementType), intTypeParams) {};

public:

	/**
	 * Retrieves the one element type associated with this type.
	 *
	 * @return the element type associated with this type
	 */
	const TypePtr& getElementType() const {
		return getTypeParameter()[0];
	}
};

// --------------------------------- Array Type ----------------------------

/**
 * This intrinsic array type used to represent multidimensional rectangular arrays
 * within the type system.
 */
class ArrayType: public SingleElementType {

	/**
	 * Creates a new instance of this class using the given parameters.
	 *
	 * @param elementType the element type of this array
	 * @param dim the dimension of the represented array
	 */
	ArrayType(const TypePtr elementType, const unsigned short dim);

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual ArrayType* clone(NodeManager& manager) const;

public:

	/**
	 * A factory method allowing to obtain a pointer to a array type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager 		the manager which should be responsible for maintaining the new
	 * 				  		type instance and all its referenced elements.
	 * @param elementType 	the type of element to be maintained within the array
	 * @param dim 			the dimension of the requested array (default is set to 1)
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static ArrayTypePtr get(NodeManager& manager, const TypePtr& elementType, const unsigned short dim = 1);

	/**
	 * Retrieves the dimension of the represented array.
	 *
	 * @return the dimension of the represented array type
	 */
	const unsigned short getDimension() const;
};

// --------------------------------- Vector Type ----------------------------

/**
 * This intrinsic vector type used to represent fixed sized arrays (=vectors).
 */
class VectorType : public SingleElementType {

	/**
	 * Creates a new instance of a vector type token using the given element and size parameter.
	 *
	 * @param elementType the element type of the new vector
	 * @param size the size of the new vector
	 */
	VectorType(const TypePtr& elementType, const IntTypeParam& size) :
		SingleElementType("vector", elementType, toVector(size)) {}

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual VectorType* clone(NodeManager& manager) const {
		return new VectorType(manager.get(getElementType()), getIntTypeParameter()[0]);
	}

public:

	/**
	 * A factory method allowing to obtain a pointer to a vector type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager 		the manager which should be responsible for maintaining the new
	 * 				  		type instance and all its referenced elements.
	 * @param elementType 	the type of element to be maintained within the vector
	 * @param size 			the size of the requested vector
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static VectorTypePtr get(NodeManager& manager, const TypePtr& elementType, const unsigned short size);

	/**
	 * A factory method allowing to obtain a pointer to a vector type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager 		the manager which should be responsible for maintaining the new
	 * 				  		type instance and all its referenced elements.
	 * @param elementType 	the type of element to be maintained within the vector
	 * @param size 			the size of the requested vector
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static VectorTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size);

	/**
	 * Retrieves the size (=number of elements) of the represented vector type.
	 *
	 * @return the size of the represented array type
	 */
	const unsigned short getSize() const;
};


// --------------------------------- Reference Type ----------------------------

/**
 * This intrinsic reference type used to represent mutable memory locations.
 */
class RefType: public SingleElementType {

	/**
	 * A private constructor to create a new instance of this type class based on the
	 * given element type.
	 *
	 * @param elementType the type the new type should reference to.
	 */
	RefType(const TypePtr elementType) :
		SingleElementType("ref", elementType) {}

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual RefType* clone(NodeManager& manager) const {
		return new RefType(manager.get(getElementType()));
	}

public:

	/**
	 * A factory method allowing to obtain a pointer to a reference type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager 		the manager which should be responsible for maintaining the new
	 * 				  		type instance and all its referenced elements.
	 * @param elementType 	the type of element the requested reference is addressing
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static RefTypePtr get(NodeManager& manager, const TypePtr& elementType);

};

// --------------------------------- Channel Type ----------------------------

/**
 * This intrinsic reference type used to represent communication channels.
 */
class ChannelType: public SingleElementType {

	/**
	 * Creates a new channel.
	 *
	 * @param elementType	the type of data to be communicated through this channel
	 * @param size			the buffer size of this channel, hence the number of elements which can be
	 * 						obtained within this channel until it starts blocking writte operations. If
	 * 						set to 0, the channel will represent a handshake channel.
	 */
	ChannelType(const TypePtr elementType, const unsigned short size);
	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual ChannelType* clone(NodeManager& manager) const {
		return new ChannelType(manager.get(getElementType()), getSize());
	}

public:

	/**
	 * A factory method allowing to obtain a pointer to a reference type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager 		the manager which should be responsible for maintaining the new
	 * 				  		type instance and all its referenced elements.
	 * @param elementType 	the type of element the requested reference is addressing
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static ChannelTypePtr get(NodeManager& manager, const TypePtr& elementType, const unsigned short size);

	/**
	 * Retrieves the (buffer) size of this channel.
	 *
	 * @return the buffer size of the channel
	 */
	const unsigned short getSize() const;
};


// ---------------------------------------- Integer Type Parameters ------------------------------

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
		unsigned short value;

		/**
		 * The symbol used for the integer type variable.
		 */
		char symbol;
	};

public:

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

private:

	/**
	 * A private constructor to create a infinite integer type parameter.
	 * The constructor is private to enforce the usage of static factory methods.
	 */
	IntTypeParam(const Type) :	type(INFINITE), value(0) {
	}

public:

	/**
	 * Implements the equality operator for the IntTypeParam type.
	 */
	bool operator==(const IntTypeParam&) const;

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
	static IntTypeParam getVariableIntParam(char symbol);

	/**
	 * A factory method to obtain a concrete integer type parameter.
	 *
	 * @param value the value to be represented
	 * @return an IntTypeParam representing a token for this value.
	 */
	static IntTypeParam getConcreteIntParam(unsigned short value);

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
	 * violation will be caused.
	 */
	unsigned short getValue() const {
		// TODO: replace with an exception
		assert( type == CONCRETE );
		return value;
	}

};

} // end namespace core
} // end namespace insieme




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

#include "insieme/utils/container_utils.h"
#include "insieme/utils/instance_manager.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/annotated_ptr.h"
#include "insieme/core/identifier.h"

using std::string;
using std::vector;
using std::map;

namespace insieme {
namespace core {

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

	virtual Type* createCopyUsing(NodeMapping& mapper) const = 0;

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
	Type(NodeType type, const std::string& name, const bool concrete = true, const bool functionType = false)
		: Node(type, NC_Type, boost::hash_value(name)), name(name), concrete(concrete), functionType(functionType) { }

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


/**
 * A type definition for a list of types, frequently used throughout the type definitions.
 */
typedef std::vector<TypePtr> TypeList;


// ---------------------------------------- A class for type variables  ------------------------------

/**
 * Tokens of this type are used to represent type variables. Instances them-self represent types,
 * yet no concrete ones.
 */
class TypeVariable: public Type {

	/**
	 * The name of the represented variable.
	 */
	std::string varName;

public:

	/**
	 * Creates a new type variable using the given name.
	 *
	 * @param name the name of the type variable to be created
	 */
	TypeVariable(const string& name) : Type(NT_TypeVariable, format("'%s", name.c_str()), false, false), varName(name) { }

private:

	/**
	 * Creates a clone of this node.
	 */
	virtual TypeVariable* createCopyUsing(NodeMapping&) const;

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

	static TypeVariablePtr getFromId(NodeManager& manager, const Identifier& id);

	/**
	 * Obtains the name of the variable represented by this instance.
	 */
	const std::string getVarName() const {
		return varName;
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
	const TypeList elementTypes;

	/**
	 * A private utility method building the name of a tuple type.
	 *
	 * @param elementTypes	the list of element types
	 * @return a string representation of the resulting tuple type
	 */
	static string buildNameString(const TypeList& elementTypes);

public:

	/**
	 * Creates a new, empty tuple type.
	 */
	TupleType();

	/**
	 * Creates a new tuple type based on the given element type(s).
	 */
	TupleType(const TypePtr&);

	/**
	 * Creates a new tuple type based on the given element type(s).
	 */
	TupleType(const TypePtr&, const TypePtr&);

	/**
	 * Creates a new tuple type based on the given element types.
	 */
	TupleType(const TypeList& elementTypes);

private:

	/**
	 * Creates a clone of this node.
	 */
	virtual TupleType* createCopyUsing(NodeMapping& mapper) const;

protected:

	/**
	 * Creates a empty child list since this node represents a terminal node.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	const TypeList& getElementTypes() const { return elementTypes; }

	/**
	 * This method provides a static factory method for an empty tuple type.
	 *
	 * @param manager the manager to obtain the new type reference from
	 */
	static TupleTypePtr getEmpty(NodeManager& manager);

	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a tuple type pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 *
	 * @param manager the manager to obtain the new type reference from
	 * @param elementTypes the list of element types to be used to form the tuple
	 */
	static TupleTypePtr get(NodeManager& manager, const TypeList& elementTypes = TypeList());

};

// ---------------------------------------- Function Type ------------------------------

/**
 * This type corresponds to the type of a function. It specifies the argument types and the
 * value returned by the members of this type.
 */
class FunctionType: public Type {

	/**
	 * The list of captured types.
	 */
	const TypeList captureTypes;

	/**
	 * The list of argument types.
	 */
	const TypeList argumentTypes;

	/**
	 * The type of value produced by this function type.
	 */
	const TypePtr returnType;

public:

	/**
	 * Creates a new instance of this type based on the given in and output types.
	 *
	 * @param argumentTypes a reference to the type used as argument types
	 * @param returnType a reference to the type used as return type
	 */
	FunctionType(const TypeList& argumentTypes, const TypePtr& returnType);


	/**
	 * Creates a new instance of this type based on the given in and output types.
	 *
	 * @param captureTypes a reference to the type captured by this function type
	 * @param argumentTypes a reference to the type used as argument types
	 * @param returnType a reference to the type used as return type
	 */
	FunctionType(const TypeList& captureTypes, const TypeList& argumentTypes, const TypePtr& returnType);

protected:

	/**
	 * Creates a clone of this node.
	 */
	virtual FunctionType* createCopyUsing(NodeMapping& mapper) const;

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
	 * @param argumentTypes the arguments accepted by the resulting function type
	 * @param returnType the type of value to be returned by the obtained function type
	 * @return a pointer to a instance of the required type maintained by the given manager
	 */
	static FunctionTypePtr get(NodeManager& manager, const TypeList& argumentTypes, const TypePtr& returnType);


	/**
	 * This method provides a static factory method for this type of node. It will return
	 * a function type pointer pointing toward a variable with the given name maintained by the
	 * given manager.
	 *
	 * @param manager the manager to be used for handling the obtained type pointer
	 * @param captureTypes the list of capture arguments accepted by the resulting function type
	 * @param argumentTypes the arguments accepted by the resulting function type
	 * @param returnType the type of value to be returned by the obtained function type
	 * @return a pointer to a instance of the required type maintained by the given manager
	 */
	static FunctionTypePtr get(NodeManager& manager, const TypeList& captureTypes, const TypeList& argumentTypes, const TypePtr& returnType);

	/**
	 * Obtains a reference to the internally maintained list of capture types.
	 *
	 * @return a reference to the list of capture types.
	 */
	const TypeList& getCaptureTypes() const {
		return captureTypes;
	}

	/**
	 * Obtains a reference to the internally maintained list of argument types.
	 *
	 * @return a reference to the list of argument types.
	 */
	const TypeList& getArgumentTypes() const {
		return argumentTypes;
	}

	/**
	 * Obtains a reference to the internally maintained result type.
	 *
	 * @return a reference to the result type.
	 */
	const TypePtr& getReturnType() const {
		return returnType;
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

public:

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

protected:

	/**
	 * A special constructor which HAS to be used by all sub-classes to ensure
	 * that the node type token is matching the actual class type.
	 *
	 * @param nodeType		the token to be used to identify the type of this node
	 * @param name 			the name of the new type (only the prefix)
	 * @param typeParams	the type parameters of this type, concrete or variable
	 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
	 * @param baseType		the base type of this generic type
	 */
	GenericType(NodeType nodeType,
			const Identifier& name,
			const vector<TypePtr>& typeParams = vector<TypePtr> (),
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> (),
			const TypePtr& baseType = NULL);


	/**
	 * Creates a clone of this node.
	 */
	virtual GenericType* createCopyUsing(NodeMapping& mapper) const;

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
	 * @param manager		the manager to be used for creating the node (memory management)
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
	 * Obtains the family name of this generic type.
	 */
	const Identifier& getFamilyName() const {
		return familyName;
	}

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


class RecTypeDefinition : public Node {

public:

	typedef std::unordered_map<TypeVariablePtr, TypePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> RecTypeDefs;

private:

	/**
	 * The list of definitions this recursive type definition is consisting of.
	 */
	const RecTypeDefs definitions;

	RecTypeDefinition(const RecTypeDefs& definitions);

	RecTypeDefinition* createCopyUsing(NodeMapping& mapper) const;

protected:

	virtual bool equals(const Node& other) const;

	virtual OptionChildList getChildNodes() const;

public:

	static RecTypeDefinitionPtr get(NodeManager& manager, const RecTypeDefs& definitions);

	const RecTypeDefs& getDefinitions() const{
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
class RecType: public Type {

	/**
	 * The name of the type variable describing this type.
	 */
	const TypeVariablePtr typeVariable;

	/**
	 * The definition body of this recursive type. Identical definitions may be
	 * shared among recursive type definitions.
	 */
	const RecTypeDefinitionPtr definition;


	/**
	 * A constructor for creating a new recursive type.
	 */
	RecType(const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition);

	/**
	 * Creates a clone of this node.
	 */
	virtual RecType* createCopyUsing(NodeMapping& mapper) const;

	/**
	 * Obtains a list of all sub-nodes referenced by this AST node.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	const RecTypeDefinitionPtr getDefinition() const { return definition; }

	const TypeVariablePtr getTypeVariable() const { return typeVariable; }

	/**
	 * A factory method for obtaining a new recursive type instance.
	 *
	 * @param manager the manager which should be maintaining the new instance
	 * @param typeVariable the name of the variable used within the recursive type definition for representing the
	 * 					   recursive type to be defined by the resulting type instance.
	 * @param definition the definition of the recursive type.
	 */
	static RecTypePtr get(NodeManager& manager, const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition);

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
	 * @param nodeType the token to identify the actual type of the resulting node
	 * @param prefix the prefix to be used for the new type (union or struct)
	 * @param entries the entries of the new type
	 *
	 * @throws std::invalid_argument if the same identifier is used more than once within the type
	 */
	NamedCompositeType(NodeType nodeType, const string& prefix, const Entries& entries);

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

	/**
	 * Retrieves the type of a member of this struct or a null pointer if there is no
	 * such entry.
	 */
	const TypePtr getTypeOfMember(const Identifier& member) const;

private:

	/**
	 * A static utility function composing the name of this type.
	 *
	 * @param prefix the prefix of the type name (union or struct)
	 * @param entries the list of entries forming the content of this type
	 * @return the name to be used by this named composite type
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
	StructType(const Entries& entries) : NamedCompositeType(NT_StructType, "struct", entries) {}

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual StructType* createCopyUsing(NodeMapping& mapper) const;

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
	 * @param elements the elements the new type should consist of
	 * @see NamedCompositeType::NamedCompositeType(const string&, const Entries&)
	 */
	UnionType(const Entries& elements) : NamedCompositeType(NT_UnionType, "union", elements) {}

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual UnionType* createCopyUsing(NodeMapping& mapper) const;

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
	 * @param nodeType the node type of the concrete implementation of this abstract class
	 * @param name the (prefix) of the name of the new type
	 * @param elementType its single type parameter
	 * @param intTypeParams additional integer type parameters. By default this parameters
	 * 						will be set to an empty set.
	 */
	SingleElementType(NodeType nodeType, const string& name,
			const TypePtr& elementType,
			const vector<IntTypeParam>& intTypeParams = vector<IntTypeParam> ());

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

public:

	/**
	 * Creates a new instance of this class using the given parameters.
	 *
	 * @param elementType the element type of this array
	 * @param dim the dimension of the represented array
	 */
	ArrayType(const TypePtr& elementType, const IntTypeParam& dim = IntTypeParam::ONE);

private:

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual ArrayType* createCopyUsing(NodeMapping& mapper) const;

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
	static ArrayTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& dim = IntTypeParam::ONE);

	/**
	 * Retrieves the dimension of the represented array.
	 *
	 * @return the dimension of the represented array type
	 */
	const IntTypeParam getDimension() const;
};

// --------------------------------- Vector Type ----------------------------

/**
 * This intrinsic vector type used to represent fixed sized arrays (=vectors).
 */
class VectorType : public SingleElementType {

public:

	/**
	 * Creates a new instance of a vector type token using the given element and size parameter.
	 *
	 * @param elementType the element type of the new vector
	 * @param size the size of the new vector
	 */
	VectorType(const TypePtr& elementType, const IntTypeParam& size);

private:

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual VectorType* createCopyUsing(NodeMapping& mapper) const;

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
	static VectorTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size);

	/**
	 * Retrieves the size (=number of elements) of the represented vector type.
	 *
	 * @return the size of the represented array type
	 */
	const IntTypeParam getSize() const;
};


// --------------------------------- Reference Type ----------------------------

/**
 * This intrinsic reference type used to represent mutable memory locations.
 */
class RefType: public SingleElementType {

public:

	/**
	 * A private constructor to create a new instance of this type class based on the
	 * given element type.
	 *
	 * @param elementType the type the new type should reference to.
	 */
	RefType(const TypePtr& elementType);

private:

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual RefType* createCopyUsing(NodeMapping& mapper) const;

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

public:

	/**
	 * Creates a new channel.
	 *
	 * @param elementType	the type of data to be communicated through this channel
	 * @param size			the buffer size of this channel, hence the number of elements which can be
	 * 						obtained within this channel until it starts blocking writte operations. If
	 * 						set to 0, the channel will represent a handshake channel.
	 */
	ChannelType(const TypePtr& elementType, const IntTypeParam& size);

private:

	/**
	 * Creates a clone of this type within the given manager.
	 */
	virtual ChannelType* createCopyUsing(NodeMapping& mapper) const;

public:

	/**
	 * A factory method allowing to obtain a pointer to a reference type representing
	 * an instance managed by the given manager.
	 *
	 * @param manager 		the manager which should be responsible for maintaining the new
	 * 				  		type instance and all its referenced elements.
	 * @param elementType 	the type of element the requested reference is addressing
	 * @param size			the size of the channel to be created
	 * @return a pointer to a instance of the requested type. Multiple requests using
	 * 		   the same parameters will lead to pointers addressing the same instance.
	 */
	static ChannelTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParam& size);

	/**
	 * Retrieves the (buffer) size of this channel.
	 *
	 * @return the buffer size of the channel
	 */
	const IntTypeParam getSize() const;
};


} // end namespace core
} // end namespace insieme




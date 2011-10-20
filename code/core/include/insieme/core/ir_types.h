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

#include "insieme/core/ir_node.h"
#include "insieme/core/values.h"
#include "insieme/core/ir_int_type_param.h"

using std::string;
using std::vector;
using std::map;

// ---------------------------------------------- IR Types -----------------------------------------------

namespace insieme {
namespace core {
namespace new_core {


	// ---------------------------------------- An abstract base type ------------------------------

	/**
	 * The accessor for instances of type expressions.
	 */
	template<typename D,template<typename T> class P>
	struct TypeAccessor : public NodeAccessor<D,P> {};

	/**
	 * The base type for all type nodes. Type nodes are used to represent the type of data elements, literals
	 * and functions - hence all expressions - within the IR.
	 */
	class Type : public Node {
	protected:

			/**
			 * A constructor for this kind of nodes ensuring that every sub-class is a member of the
			 * Type node category.
			 *
			 * @param nodeType the actual type the resulting node will be representing
			 * @param children the child nodes to be contained
			 */
			template<typename ... Nodes>
			Type(const NodeType nodeType, const Pointer<const Nodes>& ... children)
				: Node(nodeType, NC_Type, children ...) { }

			/**
			 * A constructor creating a new instance of this type based on a given child-node list.
			 *
			 * @param nodeType the type of the newly created node
			 * @param children the child nodes to be used to create the new node
			 */
			Type(const NodeType nodeType, const NodeList& children)
				: Node(nodeType, NC_Type, children) { }

	};




	// ------------------------------------ A class representing type parameter lists  ------------------------------

	/**
	 * The accessor associated to an type parameter list.
	 */
	IR_LIST_NODE_ACCESSOR(TypeParamList, Support, Type)
	};

	/**
	 * A node type representing a list of type parameters.
	 */
	IR_NODE(TypeParamList, Support)
	public:

		/**
		 * This static factory method allows to construct a type parameter list based
		 * on the given type list.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param types the types to be included within the requested type parameter list
		 * @return the requested type instance managed by the given manager
		 */
		static TypeParamListPtr get(NodeManager& manager, const TypeList& types) {
			return manager.get(TypeParamList(convertList(types)));
		}
	};




	// ---------------------------------------- Generic Type ------------------------------

	/**
	 * The accessor associated to generic types.
	 */
	IR_NODE_ACCESSOR(GenericType, Type, StringValue, TypeParamList, IntTypeParamList)
		/**
		 * Obtains the name of this generic type.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the list of type parameters of this generic type.
		 */
		IR_NODE_PROPERTY(TypeParamList, TypeParameter, 1);

		/**
		 * Obtains the list of int-type parameters of this generic type.
		 */
		IR_NODE_PROPERTY(IntTypeParamList, IntTypeParameter, 2);
	};

	/**
	 * This type represents a generic type which can be used to represent arbitrary user defined
	 * or derived types. Each generic type can be equipped with a number of generic type and integer
	 * parameters. Those are represented using other types and IntTypeParam instances.
	 */
	IR_NODE(GenericType, Type)

		/**
		 * A simple constructor creating a new generic type based
		 * on the given parameters.
		 *
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
		 */
		GenericType(const StringValuePtr& name, const TypeParamListPtr& typeParams, const IntTypeParamListPtr& intTypeParams)
			: Type(NT_GenericType, name, typeParams, intTypeParams) {}

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
		 */
		static GenericTypePtr get(NodeManager& manager, const StringValuePtr& name,
				const TypeParamListPtr& typeParams, const IntTypeParamListPtr& intTypeParams) {
			return manager.get(GenericType(name, typeParams, intTypeParams));
		}


		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a generic type pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
		 */
		static GenericTypePtr get(NodeManager& manager,
				const string& name,
				const TypeList& typeParams = TypeList(),
				const IntParamList& intTypeParams = IntParamList()) {
			return get(manager, StringValue::get(manager, name),TypeParamList::get(manager, typeParams), IntTypeParamList::get(manager, intTypeParams));
		}

	};





	// ------------------------------------ A class representing type variables  ------------------------------

	/**
	 * The accessor associated to a type variable. Each type variable has a single child node - a string value representing
	 * the name of the variable.
	 */
	IR_NODE_ACCESSOR(TypeVariable, Type, StringValue)
		/**
		 * Obtains the name of this type variable.
		 */
		IR_NODE_PROPERTY(StringValue, VarName, 0);
	};

	/**
	 * A node type representing concrete int-type parameters.
	 */
	IR_NODE(TypeVariable, Type)

		/**
		 * A simple constructor creating a new type variable based
		 * on the given name.
		 *
		 * @param name the name of the new type variable
		 */
		TypeVariable(const StringValuePtr& name);

	public:

		/**
		 * This static factory method allows to construct a type variable based on a string value (its identifier).
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting type variable
		 * @return the requested type instance managed by the given manager
		 */
		static TypeVariablePtr get(NodeManager& manager, const StringValuePtr& name) {
			return manager.get(TypeVariable(name));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a type variable pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting type variable
		 * @return the requested type instance managed by the given manager
		 */
		static TypeVariablePtr get(NodeManager& manager, const string& name) {
			return get(manager, StringValue::get(manager, name));
		}

	};





	// ---------------------------------------- A tuple type ------------------------------

	/**
	 * The accessor associated to a tuple type. Each tuple type has a single child node - a type
	 * parameter list listing the types of the elements.
	 */
	IR_NODE_ACCESSOR(TupleType, Type, TypeParamList)
		/**
		 * Obtains the name of this type variable.
		 */
		IR_NODE_PROPERTY(TypeParamList, ElementTypes, 0);
	};

	/**
	 * A node type representing Tuple Type.
	 */
	IR_NODE(TupleType, Type)

		/**
		 * A simple constructor creating a new tuple type based
		 * on the given type parameter list.
		 *
		 * @param elementTypes the types of the elements of the new tuple type
		 */
		TupleType(const TypeParamListPtr& elementTypes);

	public:

		/**
		 * This static factory method allows to construct a type variable based on a string value (its identifier).
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting type variable
		 * @return the requested type instance managed by the given manager
		 */
		static TupleTypePtr get(NodeManager& manager, const TypeParamListPtr& types) {
			return manager.get(TupleType(types));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a tuple type pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager the manager to obtain the new type reference from
		 * @param elementTypes the list of element types to be used to form the tuple
		 */
		static TupleTypePtr get(NodeManager& manager, const TypeList& elementTypes = TypeList()) {
			return get(manager, TypeParamList::get(manager, elementTypes));
		}

	};


//
//	// ---------------------------------------- Function Type ------------------------------
//
//	/**
//	 * This type corresponds to the type of a function. It specifies the argument types and the
//	 * value returned by the members of this type.
//	 */
//	class FunctionType: public Type {
//
//		/**
//		 * The list of parameter types.
//		 */
//		const TypeList parameterTypes;
//
//		/**
//		 * The type of value produced by this function type.
//		 */
//		const TypePtr returnType;
//
//		/**
//		 * A flag determining whether this function type is representing a function value that
//		 * is plain (can be called without a context) or one that requires a closure context.
//		 */
//		const bool plain;
//
//	private:
//
//		/**
//		 * Creates a new instance of this type based on the given in and output types.
//		 *
//		 * @param parameterTypes a reference to the type used as argument types
//		 * @param returnType a reference to the type used as return type
//		 * @param plain a flag determining whether values of this function require a closure context when
//		 * being invoked or not.
//		 */
//		FunctionType(const TypeList& parameterTypes, const TypePtr& returnType, bool plane);
//
//	protected:
//
//		/**
//		 * Creates a empty child list since this node represents a terminal node.
//		 */
//		virtual NodeListOpt getChildNodes() const;
//
//		/**
//		 * Compares this type with the given type.
//		 */
//		virtual bool equalsType(const Type& type) const;
//
//	public:
//
//		/**
//		 * This method provides a static factory method for function types. It will return a pointer to
//		 * a function type instance representing the requested function type and being maintained
//		 * within the given manager.
//		 *
//		 * @param manager the manager to be used for handling the obtained type pointer
//		 * @param paramTypes the type of the single parameter accepted by the resulting function
//		 * @param resultType the type of value to be returned by the obtained function type
//		 * @param plane determining whether the resulting type covers closures or not
//		 * @return a pointer to a instance of the required type maintained by the given manager
//		 */
//		static FunctionTypePtr get(NodeManager& manager, const TypePtr& paramType, const TypePtr& resultType, bool plane = true);
//
//		/**
//		 * This method provides a static factory method for this type of node. It will return
//		 * a function type pointer pointing toward a variable with the given name maintained by the
//		 * given manager.
//		 *
//		 * @param manager the manager to be used for handling the obtained type pointer
//		 * @param parameterTypes the arguments accepted by the resulting function type
//		 * @param returnType the type of value to be returned by the obtained function type
//		 * @param plain determining whether the resulting type covers closures or not
//		 * @return a pointer to a instance of the required type maintained by the given manager
//		 */
//		static FunctionTypePtr get(NodeManager& manager, const TypeList& parameterTypes, const TypePtr& returnType, bool plain = true);
//
//		/**
//		 * Obtains a reference to the internally maintained list of parameter types.
//		 *
//		 * @return a reference to the list of parameter types.
//		 */
//		const TypeList& getParameterTypes() const {
//			return parameterTypes;
//		}
//
//		/**
//		 * Obtains a reference to the internally maintained result type.
//		 *
//		 * @return a reference to the result type.
//		 */
//		const TypePtr& getReturnType() const {
//			return returnType;
//		}
//
//		/**
//		 * Determines whether this type represents a plane function type (if true) or a
//		 * function type requiring a closure context when being invoked.
//		 */
//		const bool isPlain() const {
//			return plain;
//		}
//
//		/**
//		 * Prints a string-representation of this type to the given output stream.
//		 */
//		virtual std::ostream& printTypeTo(std::ostream& out) const;
//
//	private:
//
//		/**
//		 * Creates a clone of this node.
//		 */
//		virtual FunctionType* createCopyUsing(NodeMapping& mapper) const;
//
//	};
//
//	// ---------------------------------------- Recursive Type ------------------------------
//
//
//	class RecTypeDefinition : public Node {
//
//	public:
//
//		/**
//		 * The data type used for maintaining recursive type definitions. The tree based
//		 * std::map is required to fix the order of the contained elements.
//		 */
//		typedef std::map<TypeVariablePtr, TypePtr, compare_target<TypeVariablePtr>> RecTypeDefs;
//
//	private:
//
//		/**
//		 * The list of definitions this recursive type definition is consisting of.
//		 */
//		const RecTypeDefs definitions;
//
//		/**
//		 * A constructor for this type of node.
//		 */
//		RecTypeDefinition(const RecTypeDefs& definitions);
//
//		/**
//		 * Creates a clone of this node.
//		 */
//		RecTypeDefinition* createCopyUsing(NodeMapping& mapper) const;
//
//	protected:
//
//		/**
//		 * Compares this node with the given node.
//		 */
//		virtual bool equals(const Node& other) const;
//
//		/**
//		 * Obtains a list of child nodes.
//		 */
//		virtual NodeListOpt getChildNodes() const;
//
//	public:
//
//		/**
//		 * Constructs a new node of this type based on the given type definitions.
//		 */
//		static RecTypeDefinitionPtr get(NodeManager& manager, const RecTypeDefs& definitions);
//
//		/**
//		 * Obtains the definitions constituting this type of node.
//		 */
//		const RecTypeDefs& getDefinitions() const{
//			return definitions;
//		}
//
//		/**
//		 * Obtains a specific definition maintained within this node.
//		 */
//		const TypePtr getDefinitionOf(const TypeVariablePtr& variable) const;
//
//		/**
//		 * Unrolls this definition once for the given variable.
//		 *
//		 * @param manager the manager to be used for maintaining the resulting type pointer
//		 * @param variable the variable defining the definition to be unrolled once
//		 * @return the resulting, unrolled type
//		 */
//		TypePtr unrollOnce(NodeManager& manager, const TypeVariablePtr& variable) const;
//
//		/**
//		 * Prints a string representation of this node to the given output stream.
//		 */
//		virtual std::ostream& printTo(std::ostream& out) const;
//
//	};
//
//	/**
//	 * This type connector allows to define recursive type within the IR language. Recursive
//	 * types are types which are defined by referencing to their own type. The definition of
//	 * a list may be consisting of a pair, where the first element is corresponding to a head
//	 * element and the second to the remaining list. Therefore, a list is defined using its own
//	 * type.
//	 *
//	 * This implementation allows to define mutually recursive data types, hence, situations
//	 * in which the definition of multiple recursive types are interleaved.
//	 */
//	class RecType: public Type {
//
//		/**
//		 * The name of the type variable describing this type.
//		 */
//		const TypeVariablePtr typeVariable;
//
//		/**
//		 * The definition body of this recursive type. Identical definitions may be
//		 * shared among recursive type definitions.
//		 */
//		const RecTypeDefinitionPtr definition;
//
//
//		/**
//		 * A constructor for creating a new recursive type.
//		 */
//		RecType(const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition);
//
//	public:
//
//		/**
//		 * Obtains the definition-part of this recursive type.
//		 */
//		const RecTypeDefinitionPtr getDefinition() const {
//			return definition;
//		}
//
//		/**
//		 * Obtains the type variable referencing the represented representation within
//		 * the associated definition.
//		 */
//		const TypeVariablePtr getTypeVariable() const { return typeVariable; }
//
//		/**
//		 * A factory method for obtaining a new recursive type instance.
//		 *
//		 * @param manager the manager which should be maintaining the new instance
//		 * @param typeVariable the name of the variable used within the recursive type definition for representing the
//		 * 					   recursive type to be defined by the resulting type instance.
//		 * @param definition the definition of the recursive type.
//		 */
//		static RecTypePtr get(NodeManager& manager, const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition);
//
//		/**
//		 * Unrolls this recursive type once.
//		 */
//		TypePtr unroll() const {
//			return unroll(getNodeManager());
//		}
//
//		/**
//		 * Unrolls this recursive type.
//		 */
//		TypePtr unroll(NodeManager& manager) const {
//			return definition->unrollOnce(manager, typeVariable);
//		}
//
//		/**
//		 * Prints a string-representation of this type to the given output stream.
//		 */
//		virtual std::ostream& printTypeTo(std::ostream& out) const;
//
//	protected:
//
//		/**
//		 * Obtains a list of all sub-nodes referenced by this AST node.
//		 */
//		virtual NodeListOpt getChildNodes() const;
//
//		/**
//		 * Compares this type with the given type.
//		 */
//		virtual bool equalsType(const Type& type) const;
//
//	private:
//
//		/**
//		 * Creates a clone of this node.
//		 */
//		virtual RecType* createCopyUsing(NodeMapping& mapper) const;
//
//	};
//
//
//	// --------------------------------- Named Composite Type ----------------------------
//
//	/**
//	 * This abstract type is used to form a common basis for the struct and union types which
//	 * both consist of a list of typed elements.
//	 */
//	class NamedCompositeType: public Type {
//
//	public:
//
//		/**
//		 * Defines the type used for representing a element entry.
//		 */
//		typedef std::pair<IdentifierPtr, TypePtr> Entry;
//
//		/**
//		 * The type used to represent a list of element entries.
//		 */
//		typedef vector<Entry> Entries;
//
//	private:
//
//		/**
//		 * The type entries this composed type is based on.
//		 */
//		const Entries entries;
//
//	protected:
//
//		/**
//		 * Creates a new named composite type having the given name and set of entries.
//		 *
//		 * @param nodeType the token to identify the actual type of the resulting node
//		 * @param hashSeed	the seed to be used for computing a hash value
//		 * @param prefix the prefix to be used for the new type (union or struct)
//		 * @param entries the entries of the new type
//		 *
//		 * @throws std::invalid_argument if the same identifier is used more than once within the type
//		 */
//		NamedCompositeType(NodeType nodeType, std::size_t hashSeed, const string& prefix, const Entries& entries);
//
//		/**
//		 * Obtains a list of all child sub-types used within this struct.
//		 */
//		virtual NodeListOpt getChildNodes() const;
//
//		/**
//		 * Compares this type with the given type.
//		 */
//		virtual bool equalsType(const Type& type) const;
//
//	public:
//
//		/**
//		 * Retrieves the set of entries this composed type is based on.
//		 *
//		 * @return a reference to the internally maintained list of entries.
//		 */
//		const Entries& getEntries() const {
//			return entries;
//		}
//
//		/**
//		 * Retrieves the type of a member of this struct or a null pointer if there is no
//		 * such entry.
//		 */
//		const TypePtr getTypeOfMember(const IdentifierPtr& member) const;
//
//		/**
//		 * Prints a string-representation of this type to the given output stream.
//		 */
//		virtual std::ostream& printTypeTo(std::ostream& out) const;
//
//	};
//
//	// --------------------------------- Struct Type ----------------------------
//
//	/**
//	 * The type used to represent structs / records.
//	 */
//	class StructType: public NamedCompositeType {
//
//		/**
//		 * Creates a new instance of this type based on the given entries.
//		 *
//		 * @param entries the entries the new type should consist of
//		 * @see NamedCompositeType::NamedCompositeType(const string&, const Entries&)
//		 */
//		StructType(const Entries& entries);
//
//		/**
//		 * Creates a clone of this type within the given manager.
//		 */
//		virtual StructType* createCopyUsing(NodeMapping& mapper) const;
//
//	public:
//
//		/**
//		 * A factory method allowing to obtain a pointer to a struct type representing
//		 * an instance managed by the given manager.
//		 *
//		 * @param manager the manager which should be responsible for maintaining the new
//		 * 				  type instance and all its referenced elements.
//		 * @param entries the set of entries the new type should consist of
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static StructTypePtr get(NodeManager& manager, const Entries& entries);
//
//	};
//
//
//	// --------------------------------- Union Type ----------------------------
//
//	/**
//	 * The type used to represent unions.
//	 */
//	class UnionType: public NamedCompositeType {
//
//		/**
//		 * Creates a new instance of this type based on the given entries.
//		 *
//		 * @param elements the elements the new type should consist of
//		 * @see NamedCompositeType::NamedCompositeType(const string&, const Entries&)
//		 */
//		UnionType(const Entries& elements);
//
//		/**
//		 * Creates a clone of this type within the given manager.
//		 */
//		virtual UnionType* createCopyUsing(NodeMapping& mapper) const;
//
//	public:
//
//		/**
//		 * A factory method allowing to obtain a pointer to a union type representing
//		 * an instance managed by the given manager.
//		 *
//		 * @param manager the manager which should be responsible for maintaining the new
//		 * 				  type instance and all its referenced elements.
//		 * @param entries the set of entries the new type should consist of
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static UnionTypePtr get(NodeManager& manager, const Entries& entries);
//
//	};
//
//
//	// --------------------------------- Single Element Type ----------------------------
//
//	/**
//	 * This abstract type is used as a common base class for a serious of intrinsic types
//	 * which all require a single element type. It thereby represents a specialized version
//	 * of a generic type.
//	 */
//	class SingleElementType : public GenericType {
//	protected:
//
//		/**
//		 * Creates a new instance of this class using the given name, element type and
//		 * integer parameters.
//		 *
//		 * @param nodeType the node type of the concrete implementation of this abstract class
//		 * @param hashSeed	the seed to be used for computing a hash value
//		 * @param name the (prefix) of the name of the new type
//		 * @param elementType its single type parameter
//		 * @param intTypeParams additional integer type parameters. By default this parameters
//		 * 						will be set to an empty set.
//		 */
//		SingleElementType(NodeType nodeType, std::size_t hashSeed,
//				const string& name, const TypePtr& elementType,
//				const vector<IntTypeParamPtr>& intTypeParams = vector<IntTypeParamPtr> ());
//
//	public:
//
//		/**
//		 * Retrieves the one element type associated with this type.
//		 *
//		 * @return the element type associated with this type
//		 */
//		const TypePtr& getElementType() const {
//			return getTypeParameter()[0];
//		}
//	};
//
//	// --------------------------------- Array Type ----------------------------
//
//	/**
//	 * This intrinsic array type used to represent multidimensional rectangular arrays
//	 * within the type system.
//	 */
//	class ArrayType: public SingleElementType {
//
//	public:
//
//		/**
//		 * Creates a new instance of this class using the given parameters.
//		 *
//		 * @param elementType the element type of this array
//		 * @param dim the dimension of the represented array
//		 */
//		ArrayType(const TypePtr& elementType, const IntTypeParamPtr& dim);
//
//	private:
//
//		/**
//		 * Creates a clone of this type within the given manager.
//		 */
//		virtual ArrayType* createCopyUsing(NodeMapping& mapper) const;
//
//	public:
//
//		/**
//		 * A factory method allowing to obtain a pointer to a array type representing
//		 * an instance managed by the given manager. The dimension of the resulting array is
//		 * one.
//		 *
//		 * @param manager 		the manager which should be responsible for maintaining the new
//		 * 				  		type instance and all its referenced elements.
//		 * @param elementType 	the type of element to be maintained within the array
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static ArrayTypePtr get(NodeManager& manager, const TypePtr& elementType);
//
//
//		/**
//		 * A factory method allowing to obtain a pointer to a array type representing
//		 * an instance managed by the given manager.
//		 *
//		 * @param manager 		the manager which should be responsible for maintaining the new
//		 * 				  		type instance and all its referenced elements.
//		 * @param elementType 	the type of element to be maintained within the array
//		 * @param dim 			the dimension of the requested array
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static ArrayTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& dim);
//
//		/**
//		 * Retrieves the dimension of the represented array.
//		 *
//		 * @return the dimension of the represented array type
//		 */
//		const IntTypeParamPtr& getDimension() const;
//	};
//
//	// --------------------------------- Vector Type ----------------------------
//
//	/**
//	 * This intrinsic vector type used to represent fixed sized arrays (=vectors).
//	 */
//	class VectorType : public SingleElementType {
//
//	public:
//
//		/**
//		 * Creates a new instance of a vector type token using the given element and size parameter.
//		 *
//		 * @param elementType the element type of the new vector
//		 * @param size the size of the new vector
//		 */
//		VectorType(const TypePtr& elementType, const IntTypeParamPtr& size);
//
//	private:
//
//		/**
//		 * Creates a clone of this type within the given manager.
//		 */
//		virtual VectorType* createCopyUsing(NodeMapping& mapper) const;
//
//	public:
//
//		/**
//		 * A factory method allowing to obtain a pointer to a vector type representing
//		 * an instance managed by the given manager.
//		 *
//		 * @param manager 		the manager which should be responsible for maintaining the new
//		 * 				  		type instance and all its referenced elements.
//		 * @param elementType 	the type of element to be maintained within the vector
//		 * @param size 			the size of the requested vector
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static VectorTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& size);
//
//		/**
//		 * Retrieves the size (=number of elements) of the represented vector type.
//		 *
//		 * @return the size of the represented array type
//		 */
//		const IntTypeParamPtr& getSize() const;
//	};
//
//
//	// --------------------------------- Reference Type ----------------------------
//
//	/**
//	 * This intrinsic reference type used to represent mutable memory locations.
//	 */
//	class RefType: public SingleElementType {
//
//	public:
//
//		/**
//		 * A private constructor to create a new instance of this type class based on the
//		 * given element type.
//		 *
//		 * @param elementType the type the new type should reference to.
//		 */
//		RefType(const TypePtr& elementType);
//
//	private:
//
//		/**
//		 * Creates a clone of this type within the given manager.
//		 */
//		virtual RefType* createCopyUsing(NodeMapping& mapper) const;
//
//	public:
//
//		/**
//		 * A factory method allowing to obtain a pointer to a reference type representing
//		 * an instance managed by the given manager.
//		 *
//		 * @param manager 		the manager which should be responsible for maintaining the new
//		 * 				  		type instance and all its referenced elements.
//		 * @param elementType 	the type of element the requested reference is addressing
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static RefTypePtr get(NodeManager& manager, const TypePtr& elementType);
//
//	};
//
//	// --------------------------------- Channel Type ----------------------------
//
//	/**
//	 * This intrinsic reference type used to represent communication channels.
//	 */
//	class ChannelType: public SingleElementType {
//
//	public:
//
//		/**
//		 * Creates a new channel.
//		 *
//		 * @param elementType	the type of data to be communicated through this channel
//		 * @param size			the buffer size of this channel, hence the number of elements which can be
//		 * 						obtained within this channel until it starts blocking writte operations. If
//		 * 						set to 0, the channel will represent a handshake channel.
//		 */
//		ChannelType(const TypePtr& elementType, const IntTypeParamPtr& size);
//
//	private:
//
//		/**
//		 * Creates a clone of this type within the given manager.
//		 */
//		virtual ChannelType* createCopyUsing(NodeMapping& mapper) const;
//
//	public:
//
//		/**
//		 * A factory method allowing to obtain a pointer to a reference type representing
//		 * an instance managed by the given manager.
//		 *
//		 * @param manager 		the manager which should be responsible for maintaining the new
//		 * 				  		type instance and all its referenced elements.
//		 * @param elementType 	the type of element the requested reference is addressing
//		 * @param size			the size of the channel to be created
//		 * @return a pointer to a instance of the requested type. Multiple requests using
//		 * 		   the same parameters will lead to pointers addressing the same instance.
//		 */
//		static ChannelTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& size);
//
//		/**
//		 * Retrieves the (buffer) size of this channel.
//		 *
//		 * @return the buffer size of the channel
//		 */
//		const IntTypeParamPtr& getSize() const;
//	};

} // end namespace new_core
} // end namespace core
} // end namespace insieme




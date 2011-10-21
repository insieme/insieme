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
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

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

	protected:

		/**
		 * A constructor offered to sub-types allowing to specify the resulting node type.
		 *
		 * @param type			the type of the resulting node
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
		 */
		GenericType(const NodeType type, const StringValuePtr& name, const TypeParamListPtr& typeParams, const IntTypeParamListPtr& intTypeParams)
			: Type(type, name, typeParams, intTypeParams) {}

		/**
		 * A constructor offered to sub-types allowing to specify the resulting node type.
		 *
		 * @param type			the type of the resulting node
		 * @param children		the child list of the resulting node
		 */
		GenericType(const NodeType type, const NodeList& children)
			: Type(type, children) {}


		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

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

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "'" << *getVarName();
		}

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
		TupleType(const TypeParamListPtr& elementTypes)
			: Type(NT_TupleType, elementTypes) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << '(' << join(",", getElementTypes()->getChildList(), print<deref<NodePtr>>()) << ')';
		}

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




	// ---------------------------------------- Function Type ------------------------------


	/**
	 * The accessor associated to a function type. Each function type is composed of 3 sub-nodes.
	 * The first is forming a list of parameters, the second the return type and the last marks
	 * function types to be pure or closures.
	 */
	IR_NODE_ACCESSOR(FunctionType, Type, TypeParamList, Type, BoolValue)
		/**
		 * Obtains the list of parameters required for invoking functions of this type.
		 */
		IR_NODE_PROPERTY(TypeParamList, ParameterTypes, 0);

		/**
		 * Obtains the return type of the represented function type.
		 */
		IR_NODE_PROPERTY(Type, ReturnType, 1);

		/**
		 * Obtains a pointer to the child node determining whether this function type is a plain function type or not.
		 */
		IR_NODE_PROPERTY(BoolValue, Plain, 2);

		/**
		 * A utility function allowing to determine directly whether a function
		 * is plain or not.
		 */
		bool isPlain() const { return NodeAccessor<Derived,Ptr>::getNode().getPlain()->getValue(); }
	};

	/**
	 * This type corresponds to the type of a function. It specifies the argument types and the
	 * value returned by the members of this type.
	 */
	IR_NODE(FunctionType, Type)

		/**
		 * A simple constructor creating a new function type based on the given parameters.
		 *
		 * @param parameters the list of type of parameters accepted by the resulting function type
		 * @param returnType the type of value returned by the resulting function type
		 * @param plain a flag indicating whether this function type is a plain function or not
		 */
		FunctionType(const TypeParamListPtr& parameters, const TypePtr& returnType, const BoolValuePtr& plain)
			: Type(NT_FunctionType, parameters, returnType, plain) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	public:

		/**
		 * This method provides a static factory method for function types. It will return a pointer to
		 * a function type instance representing the requested function type and being maintained
		 * within the given manager.
		 *
		 * @param manager the manager to be used for handling the obtained type pointer
		 * @param paramTypes the type of the single parameter accepted by the resulting function
		 * @param returnType the type of value to be returned by the obtained function type
		 * @param plain determining whether the resulting type covers closures or not
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypePtr& paramType, const TypePtr& returnType, bool plain = true) {
			return manager.get(FunctionType(TypeParamList::get(manager, toVector(paramType)), returnType, BoolValue::get(manager, plain)));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a function type pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager the manager to be used for handling the obtained type pointer
		 * @param parameterTypes the arguments accepted by the resulting function type
		 * @param returnType the type of value to be returned by the obtained function type
		 * @param plain determining whether the resulting type covers closures or not
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypeList& parameterTypes, const TypePtr& returnType, bool plain = true){
			return manager.get(FunctionType(TypeParamList::get(manager, parameterTypes), returnType, BoolValue::get(manager, plain)));
		}

	};





	// ---------------------------------------- Recursive Type ------------------------------

	/**
	 * The accessor associated to a recursive type binding. Each binding maps a variable to
	 * a type - potentially including a recursive usage of the bound variable.
	 */
	IR_NODE_ACCESSOR(RecTypeBinding, Support, TypeVariable, Type)
		/**
		 * Obtains the variable being bound by this binding.
		 */
		IR_NODE_PROPERTY(TypeVariable, Variable, 0);

		/**
		 * Obtains a reference to the type being bound to the referenced variable.
		 */
		IR_NODE_PROPERTY(Type, Type, 1);
	};

	/**
	 * A node type used to represent recursive type variable bindings.
	 */
	IR_NODE(RecTypeBinding, Support)

		/**
		 * A simple constructor creating a new binding between the given variable
		 * and type.
		 *
		 * @param elementTypes the types of the elements of the new tuple type
		 */
		RecTypeBinding(const TypeVariablePtr& var, const TypePtr& type)
			: Support(NT_RecTypeBinding, var, type) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *getVariable() << "=" << *getType();
		}

	public:

		/**
		 * This static factory method allows to construct a new binding between the given
		 * variable and type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param var the variable to be bound
		 * @param type the type to be bound to the given variable
		 * @return the requested type instance managed by the given manager
		 */
		static RecTypeBindingPtr get(NodeManager& manager, const TypeVariablePtr& var, const TypePtr& type) {
			return manager.get(RecTypeBinding(var, type));
		}

	};



	/**
	 * The accessor associated to a recursive type binding. Each binding maps a variable to
	 * a type - potentially including a recursive usage of the bound variable.
	 */
	IR_LIST_NODE_ACCESSOR(RecTypeDefinition, Support, RecTypeBinding)
		const vector<RecTypeBindingPtr>& getDefinitions() {
			return convertList<RecTypeBinding>(NodeAccessor<Derived, Ptr>::getNode().getChildList());
		}

		/**
		 * Obtains a specific definition maintained within this node.
		 */
		TypePtr getDefinitionOf(const TypeVariablePtr& variable) const {
			auto list = convertList<RecTypeBinding>(NodeAccessor<Derived, Ptr>::getNode().getChildList());
			auto pos = std::find_if(list.begin(), list.end(), [&](const RecTypeBindingPtr& cur) {
				return *cur->getVariable() == *variable;
			});
			return (pos==list.end())?TypePtr():(*pos)->getType();
		}

	};

	/**
	 * A node type used to represent recursive type variable bindings.
	 */
	IR_NODE(RecTypeDefinition, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", getChildList(), print<deref<NodePtr>>()) << "}";
		}

	public:

		/**
		 * This static factory method constructing a new recursive type definition based
		 * on a given list of recursive-variable bindings.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param bindings the bindings to be included within this definition
		 * @return the requested type instance managed by the given manager
		 */
		static RecTypeDefinitionPtr get(NodeManager& manager, const vector<RecTypeBindingPtr>& bindings) {
			return manager.get(RecTypeDefinition(convertList(bindings)));
		}

		/**
		 * Unrolls this definition once for the given variable.
		 *
		 * @param manager the manager to be used for maintaining the resulting type pointer
		 * @param variable the variable defining the definition to be unrolled once
		 * @return the resulting, unrolled type
		 */
		TypePtr unrollOnce(NodeManager& manager, const TypeVariablePtr& variable) const;

	};




	/**
	 * The accessor associated to a recursive type. A recursive type is simply referencing
	 * a bound type variable within a recursive type definitions.
	 */
	IR_NODE_ACCESSOR(RecType, Type, TypeVariable, RecTypeDefinition)
		/**
		 * Obtains the variable picked by this recursive type.
		 */
		IR_NODE_PROPERTY(TypeVariable, TypeVariable, 0);

		/**
		 * Obtains a reference the underlying recursive type definition.
		 */
		IR_NODE_PROPERTY(RecTypeDefinition, Definition, 1);
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
	IR_NODE(RecType, Type)

		/**
		 * A simple constructor creating a new recursive type representing the
		 * recursive type bound to the given variable within the given type.
		 *
		 * @param var the variable defining the chosen definition
		 * @param definition the definition of the actual recursive type
		 */
		RecType(const TypeVariablePtr& var, const RecTypeDefinitionPtr& definition)
			: Type(NT_RecType, var, definition) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "rec " << *getTypeVariable() << "." << *getDefinition();
		}

	public:

		/**
		 * A factory method for obtaining a new recursive type instance.
		 *
		 * @param manager the manager which should be maintaining the new instance
		 * @param typeVariable the name of the variable used within the recursive type definition for representing the
		 * 					   recursive type to be defined by the resulting type instance.
		 * @param definition the definition of the recursive type.
		 */
		static RecTypePtr get(NodeManager& manager, const TypeVariablePtr& typeVariable, const RecTypeDefinitionPtr& definition) {
			return manager.get(RecType(typeVariable, definition));
		}

		/**
		 * Unrolls this recursive type once.
		 */
		TypePtr unroll() const {
			return unroll(getNodeManager());
		}

		/**
		 * Unrolls this recursive type.
		 */
		TypePtr unroll(NodeManager& manager) const {
			return getDefinition()->unrollOnce(manager, getTypeVariable());
		}

	};






	// --------------------------------- Named Composite Type ----------------------------


	/**
	 * The accessor associated to a named type. A named type is linking a name to
	 * a type. Named types are the components named composite types (structs and unions)
	 * are build form.
	 */
	IR_NODE_ACCESSOR(NamedType, Support, StringValue, Type)
		/**
		 * Obtains the name bound by this binding.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the bound type.
		 */
		IR_NODE_PROPERTY(Type, Type, 1);
	};

	/**
	 * A node type used to represent a named type within a struct or an union.
	 */
	IR_NODE(NamedType, Support)

		/**
		 * A simple constructor creating a new binding between the given variable
		 * and type.
		 *
		 * @param elementTypes the types of the elements of the new tuple type
		 */
		NamedType(const StringValuePtr& name, const TypePtr& type)
			: Support(NT_NamedType, name, type) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *getName() << ":" << *getType();
		}

	public:

		/**
		 * This static factory method allows to construct a new binding between the given
		 * name and type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the name to be bound
		 * @param type the type to be bound to the given variable
		 * @return the requested type instance managed by the given manager
		 */
		static NamedTypePtr get(NodeManager& manager, const StringValuePtr& name, const TypePtr& type) {
			return manager.get(NamedType(name, type));
		}

	};


	/**
	 * The accessor associated to a named type. A named type is linking a name to
	 * a type. Named types are the components named composite types (structs and unions)
	 * are build form.
	 */
	IR_LIST_NODE_ACCESSOR(NamedCompositeType, Type, NamedType)
		/**
		 * Retrieves the type of a member of this composite type or a null pointer if there is no
		 * such entry.
		 *
		 * @param name the name to be searching for
		 */
		TypePtr getTypeOfMember(const StringValuePtr& name) const {
			auto list = convertList<NamedType>(NodeAccessor<Derived, Ptr>::getNode().getChildList());
			auto pos = std::find_if(list.begin(), list.end(), [&](const NamedTypePtr& cur) {
				return *cur->getName() == *name;
			});
			return (pos==list.end())?TypePtr():(*pos)->getType();
		}
	};

	/**
	 * A node type used to represent a named type within a struct or an union.
	 */
	class NamedCompositeType : public Type,
		public NamedCompositeTypeAccessor<NamedCompositeType, Pointer>,
		public NamedCompositeTypeAccessor<NamedCompositeType, Pointer>::node_format_helper {
	public:

		/**
		 * A type definition defining the type of entries this composed type is consisting of.
		 */
		typedef vector<NamedTypePtr> Entries;

	protected:

		/**
		 * A simple constructor creating a new named composite type based on the
		 * given element types.
		 *
		 * @param elements the elements of the resulting composite type
		 */
		NamedCompositeType(const NodeType& type, const NodeList& elements);

		/**
		 * A simple constructor creating a new named composite type based on the
		 * given element types.
		 *
		 * @param elements the elements of the resulting composite type
		 */
		NamedCompositeType(const NodeType& type, const Entries& elements);

	};

	/**
	 * A macro creating single element nodes.
	 */
	#define IR_NAMED_COMPOSITE_TYPE(NAME) \
		class NAME : public NamedCompositeType { \
			NAME(const NodeList& children) : NamedCompositeType(NT_ ## NAME, children) { \
				assert(checkChildList(children) && "Invalid composition of Child-Nodes discovered!"); \
			} \
		\
		protected: \
			/* The function required for the clone process. */ \
			virtual NAME* createInstanceUsing(const NodeList& children) const { \
				return new NAME(children); \
			} \
		public: \
			/* A factory method creating instances based on a child list */ \
			static NAME ## Ptr get(NodeManager& manager, const NodeList& children) { \
				return manager.get(NAME(children)); \
			} \
		private: \


	// --------------------------------- Struct Type ----------------------------

	/**
	 * The type used to represent struct / records.
	 */
	IR_NAMED_COMPOSITE_TYPE(StructType)

		/**
		 * A simple constructor creating a new struct type based on the
		 * given element types.
		 *
		 * @param elements the elements of the resulting composite type
		 */
		StructType(const vector<NamedTypePtr>& elements)
			: NamedCompositeType(NT_StructType, elements) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "struct<" << join(",",getChildList(), print<deref<NodePtr>>()) << ">";
		}

	public:

		/**
		 * A factory method allowing to obtain a pointer to a struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param entries the list of entries the new type should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructTypePtr get(NodeManager& manager, const vector<NamedTypePtr>& entries) {
			return manager.get(StructType(entries));
		}

	};



	// --------------------------------- Union Type ----------------------------

	/**
	 * The type used to represent unions.
	 */
	IR_NAMED_COMPOSITE_TYPE(UnionType)

		/**
		 * A simple constructor creating a new union type based on the
		 * given element types.
		 *
		 * @param elements the elements of the resulting composite type
		 */
		UnionType(const vector<NamedTypePtr>& elements)
			: NamedCompositeType(NT_UnionType, elements) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "union<" << join(",",getChildList(), print<deref<NodePtr>>()) << ">";
		}

	public:

		/**
		 * A factory method allowing to obtain a pointer to a union type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param entries the list of entries the new type should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static UnionTypePtr get(NodeManager& manager, const vector<NamedTypePtr>& entries) {
			return manager.get(UnionType(entries));
		}

	};

	#undef IR_NAMED_COMPOSITE_TYPE




	// --------------------------------- Reference Type ----------------------------

	/**
	 * The accessor associated to the reference type.
	 */
	IR_NODE_ACCESSOR(RefType, Type, Type)

		/**
		 * Obtains the type of the referenced element type.
		 */
		IR_NODE_PROPERTY(Type, ElementType, 0);

	};

	/**
	 * This intrinsic reference type used to represent mutable memory locations.
	 */
	IR_NODE(RefType, Type)

		/**
		 * A private constructor to create a new instance of this type class based on the
		 * given element type.
		 *
		 * @param elementType the type the new type should reference to.
		 */
		RefType(const TypePtr& elementType) : Type(NT_RefType, elementType) {}

	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "ref<" << *getElementType() << ">";
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
		static RefTypePtr get(NodeManager& manager, const TypePtr& elementType) {
			return manager.get(RefType(elementType));
		}

	};


	// --------------------------------- Single Element Type ----------------------------

	/**
	 * The accessor to be used when accessing single element types.
	 */
	IR_NODE_ACCESSOR(SingleElementType, Type, Type, IntTypeParam)

		/**
		 * Obtains the associated element type.
		 */
		IR_NODE_PROPERTY(Type, ElementType, 0);

		/**
		 * Obtains the associated int-type parameter.
		 */
		IR_NODE_PROPERTY(IntTypeParam, IntTypeParameter, 1);

	};

	/**
	 * This abstract type is used as a common base class for a serious of intrinsic types
	 * which all require a single element type. It thereby represents a specialized version
	 * of a generic type.
	 */
	class SingleElementType : public Type {

	protected:

		/**
		 * A simple constructor creating a new single-element type.
		 *
		 * @param type the type of the resulting node
		 * @param typeParams the additional type parameters of the resulting type
		 * @param paramList the additional generic int-type parameters of the resulting type
		 */
		SingleElementType(const NodeType& type, const TypePtr& elementType, const IntTypeParamPtr& param)
			: Type(type, elementType, param) { }

		/**
		 * The get-node-from-list constructor required by all nodes.
		 *
		 * @param type the type of the resulting node
		 * @param children the list of children of the resulting nodes
		 */
		SingleElementType(const NodeType& type, const NodeList& children)
			: Type(type, children) {}

	};

	/**
	 * A macro creating single element nodes.
	 */
	#define IR_SINGLE_ELEMENT_TYPE(NAME) \
		class NAME : public SingleElementType, public NAME ## Accessor<NAME, Pointer>, public NAME ## Accessor<NAME, Pointer>::node_format_helper { \
			NAME(const NodeList& children) : SingleElementType(NT_ ## NAME, children) { \
				assert(checkChildList(children) && "Invalid composition of Child-Nodes discovered!"); \
			} \
		\
		protected: \
			/* The function required for the clone process. */ \
			virtual NAME* createInstanceUsing(const NodeList& children) const { \
				return new NAME(children); \
			} \
		public: \
			/* A factory method creating instances based on a child list */ \
			static NAME ## Ptr get(NodeManager& manager, const NodeList& children) { \
				return manager.get(NAME(children)); \
			} \
		private: \





	// --------------------------------- Array Type ----------------------------

	/**
	 * The accessor to be used when accessing array types.
	 */
	template<typename D,template<typename T> class P>
	struct ArrayTypeAccessor : public SingleElementTypeAccessor<D,P> {
		/**
		 * Retrieves the dimension of the represented array.
		 *
		 * @return the dimension of the represented array type
		 */
		const P<const IntTypeParam> getDimension() const {
			return SingleElementTypeAccessor<D,P>::getIntTypeParameter();
		}
	};

	/**
	 * This intrinsic array type used to represent multidimensional rectangular arrays
	 * within the type system.
	 */
	IR_SINGLE_ELEMENT_TYPE(ArrayType)

		/**
		 * Creates a new instance of this class using the given parameters.
		 *
		 * @param elementType the element type of this array
		 * @param dim the dimension of the represented array
		 */
		ArrayType(const TypePtr& elementType, const IntTypeParamPtr& dim)
			: SingleElementType(NT_ArrayType, elementType, dim) {}

	protected:

		/*
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "array<" << *getElementType() << "," << *getIntTypeParameter() << ">";
		}

	public:

		/**
		 * A factory method allowing to obtain a pointer to a array type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager 		the manager which should be responsible for maintaining the new
		 * 				  		type instance and all its referenced elements.
		 * @param elementType 	the type of element to be maintained within the array
		 * @param dim 			the dimension of the requested array
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static ArrayTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& dim) {
			return manager.get(ArrayType(elementType, dim));
		}

		/**
		 * A factory method allowing to obtain a pointer to a array type representing
		 * an instance managed by the given manager. The dimension of the resulting array is
		 * one.
		 *
		 * @param manager 		the manager which should be responsible for maintaining the new
		 * 				  		type instance and all its referenced elements.
		 * @param elementType 	the type of element to be maintained within the array
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static ArrayTypePtr get(NodeManager& manager, const TypePtr& elementType) {
			return get(manager, elementType, ConcreteIntTypeParam::get(manager, 1));
		}

	};



	// --------------------------------- Vector Type ----------------------------

	/**
	 * The accessor to be used when accessing vector types.
	 */
	template<typename D,template<typename T> class P>
	struct VectorTypeAccessor : public SingleElementTypeAccessor<D,P> {
		/**
		 * Retrieves the size (=number of elements) of the represented vector type.
		 *
		 * @return the size of the represented array type
		 */
		const P<const IntTypeParam> getSize() const {
			return SingleElementTypeAccessor<D,P>::getIntTypeParameter();
		}
	};

	/**
	 * This intrinsic vector type used to represent fixed sized arrays (=vectors).
	 */
	IR_SINGLE_ELEMENT_TYPE(VectorType)

		/**
		 * Creates a new instance of a vector type token using the given element and size parameter.
		 *
		 * @param elementType the element type of the new vector
		 * @param size the size of the new vector
		 */
		VectorType(const TypePtr& elementType, const IntTypeParamPtr& size)
			: SingleElementType(NT_VectorType, elementType, size) {}

	protected:

		/*
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "vector<" << *getElementType() << "," << *getIntTypeParameter() << ">";
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
		static VectorTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& size) {
			return manager.get(VectorType(elementType, size));
		}

	};



	// --------------------------------- Channel Type ----------------------------

	/**
	 * The accessor to be used when accessing vector types.
	 */
	template<typename D,template<typename T> class P>
	struct ChannelTypeAccessor : public SingleElementTypeAccessor<D,P> {
		/**
		 * Retrieves the size (=number of elements) of the represented vector type.
		 *
		 * @return the size of the represented array type
		 */
		const P<const IntTypeParam> getSize() const {
			return SingleElementTypeAccessor<D,P>::getIntTypeParameter();
		}
	};

	/**
	 * This intrinsic reference type used to represent communication channels.
	 */
	IR_SINGLE_ELEMENT_TYPE(ChannelType)

		/**
		 * Creates a new channel.
		 *
		 * @param elementType	the type of data to be communicated through this channel
		 * @param size			the buffer size of this channel, hence the number of elements which can be
		 * 						obtained within this channel until it starts blocking writte operations. If
		 * 						set to 0, the channel will represent a handshake channel.
		 */
		ChannelType(const TypePtr& elementType, const IntTypeParamPtr& size)
			: SingleElementType(NT_ChannelType, elementType, size) {}

	protected:

			/*
			 * Prints a string representation of this node to the given output stream.
			 */
			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "channel<" << *getElementType() << "," << *getIntTypeParameter() << ">";
			}

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
		static ChannelTypePtr get(NodeManager& manager, const TypePtr& elementType, const IntTypeParamPtr& size) {
			return manager.get(ChannelType(elementType, size));
		}
	};

	#undef IR_SINGLE_ELEMENT_TYPE

} // end namespace new_core
} // end namespace core
} // end namespace insieme




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
#include "insieme/core/ir_values.h"
#include "insieme/core/ir_int_type_param.h"

// ---------------------------------------------- IR Types -----------------------------------------------

namespace insieme {
namespace core {


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




	// ------------------------------------ A class representing a list of types  ------------------------------

	/**
	 * The accessor associated to an type parameter list.
	 */
	IR_LIST_NODE_ACCESSOR(Types, Support, Types, Type)
	};

	/**
	 * A node type representing a list of type parameters.
	 */
	IR_NODE(Types, Support)
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
		static TypesPtr get(NodeManager& manager, const TypeList& types) {
			return manager.get(Types(convertList(types)));
		}
	};


	// ------------------------------------ A class representing a parent type  ------------------------------

	/**
	 * The accessor associated to a parent type reference. A parent type reference is linking a
	 * base type to a parent type via inheritance. The inheritance relation may be virtual.
	 */
	IR_NODE_ACCESSOR(Parent, Support, BoolValue, Type)

		/**
		 * Obtains the flag determining whether this parent type is a virtual or non-virtual parent type.
		 */
		IR_NODE_PROPERTY(BoolValue, Virtual, 0);

		/**
		 * Obtains the parent type this parent node is referring to.
		 */
		IR_NODE_PROPERTY(Type, Type, 1);

		/**
		 * Determines whether this parent node is referring to a type via virtual inheritance.
		 */
		bool isVirtual() const {
			return getVirtual().getValue();
		}
	};

	/**
	 * The node type used to represent a link to a parent class within a base type.
	 */
	IR_NODE(Parent, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			if (isVirtual()) out << "virtual ";
			return out << *getType();
		}

	public:

		/**
		 * This static factory method allows to construct a new parent-link instance referencing
		 * the given type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul a flag determining whether the result should be a virtual inheritance link
		 * @param type the type to be referenced as a parent class
		 * @return the requested type instance managed by the given manager
		 */
		static ParentPtr get(NodeManager& manager, const BoolValuePtr& virtul, const TypePtr& type) {
			return manager.get(Parent(virtul, type));
		}

		/**
		 * This static factory method allows to construct a new parent-link instance referencing
		 * the given type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul a flag determining whether the result should be a virtual inheritance link
		 * @param type the type to be referenced as a parent class
		 * @return the requested type instance managed by the given manager
		 */
		static ParentPtr get(NodeManager& manager, bool virtul, const TypePtr& type) {
			return get(manager, BoolValue::get(manager, virtul), type);
		}

		/**
		 * This static factory method allows to construct a new parent-link instance referencing
		 * the given type as a  non-virtual parent
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type to be referenced as a parent class
		 * @return the requested type instance managed by the given manager
		 */
		static ParentPtr get(NodeManager& manager, const TypePtr& type) {
			return get(manager, false, type);
		}

	};

	// ------------------------------------ A class representing a list of parent types  ------------------------------

	/**
	 * The accessor associated to a list of parent types.
	 */
	IR_LIST_NODE_ACCESSOR(Parents, Support, Types, Parent)
	};

	/**
	 * A node type representing a list of parent types.
	 */
	IR_NODE(Parents, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	public:

		/**
		 * This static factory method allows to construct a parent type list based
		 * on a given parent list.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param parents the parents to be included within the resulting parents list
		 * @return the requested parents list instance managed by the given manager
		 */
		static ParentsPtr get(NodeManager& manager, const ParentList& parents = ParentList()) {
			return manager.get(Parents(convertList(parents)));
		}

		/**
		 * This static factory method allows to construct a parent type list based
		 * on the given type list.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param types the types to be included within the requested type parameter list
		 * @return the requested type instance managed by the given manager
		 */
		static ParentsPtr get(NodeManager& manager, const TypeList& types) {
			return get(manager, ::transform(types, [&](const TypePtr& type) { return Parent::get(manager, type); }));
		}
	};


	// ---------------------------------------- Generic Type ------------------------------

	/**
	 * The accessor associated to generic types.
	 */
	IR_NODE_ACCESSOR(GenericType, Type, StringValue, Parents, Types, IntTypeParams)
		/**
		 * Obtains the name of this generic type.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the list of types this type is derived from.
		 */
		IR_NODE_PROPERTY(Parents, Parents, 1);

		/**
		 * Obtains the list of type parameters of this generic type.
		 */
		IR_NODE_PROPERTY(Types, TypeParameter, 2);

		/**
		 * Obtains the list of int-type parameters of this generic type.
		 */
		IR_NODE_PROPERTY(IntTypeParams, IntTypeParameter, 3);

		/**
		 * Obtains the name portion of this generic type.
		 */
		const string& getFamilyName() const {
			return getName()->getValue();
		}

		/**
		 * Obtains a reference to the type parameter with the given index.
		 */
		Ptr<const Type> getTypeParameter(std::size_t index) const {
			return getTypeParameter()->getElement(index);
		}
	};

	/**
	 * This type represents a generic type which can be used to represent arbitrary user defined
	 * or derived types. Each generic type can be equipped with a number of generic type and integer
	 * parameters. Those are represented using other types and IntTypeParam instances.
	 */
	IR_NODE(GenericType, Type)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	public:


		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a generic type pointer pointing toward a variable with the given name, parents, type
		 * parameters and int-type parameters which is maintained by the given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param parents		the list of parent types (for the inheritance hierarchy)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
		 */
		static GenericTypePtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents,
				const TypesPtr& typeParams, const IntTypeParamsPtr& intTypeParams) {
			return manager.get(GenericType(name, parents, typeParams, intTypeParams));
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
		static GenericTypePtr get(NodeManager& manager, const StringValuePtr& name,
				const TypesPtr& typeParams, const IntTypeParamsPtr& intTypeParams) {
			return get(manager, name, Parents::get(manager), typeParams, intTypeParams);
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a generic type pointer pointing toward a variable with the given properties maintained by the
		 * given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param parentTypes	the list of parent types to be exhibited
		 * @param typeParams	the type parameters of this type, concrete or variable
		 * @param intTypeParams	the integer-type parameters of this type, concrete or variable
		 */
		static GenericTypePtr get(NodeManager& manager,
				const string& name,
				const ParentList& parentTypes,
				const TypeList& typeParams = TypeList(),
				const IntParamList& intTypeParams = IntParamList()) {
			return get(manager, StringValue::get(manager, name), Parents::get(manager, parentTypes), Types::get(manager, typeParams), IntTypeParams::get(manager, intTypeParams));
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
			return get(manager, StringValue::get(manager, name),Types::get(manager, typeParams), IntTypeParams::get(manager, intTypeParams));
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
	 * The accessor associated to a tuple type. Each tuple type is consisting of a list
	 * of type pointers.
	 */
	IR_LIST_NODE_ACCESSOR(TupleType, Type, ElementTypes, Type)
	};

	/**
	 * A node type representing Tuple Type.
	 */
	IR_NODE(TupleType, Type)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << '(' << join(",", getElementTypes(), print<deref<NodePtr>>()) << ')';
		}

	public:

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a tuple type pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager the manager to obtain the new type reference from
		 * @param elementTypes the list of element types to be used to form the tuple
		 */
		static TupleTypePtr get(NodeManager& manager, const TypeList& elementTypes = TypeList()) {
			return get(manager, convertList(elementTypes));
		}

	};




	// ---------------------------------------- Function Type ------------------------------

	/**
	 * An enumeration used for distinguishing the various kinds of function types.
	 */
	enum FunctionKind {
		FK_PLAIN = 1, 			/* < a plain function produced by a simple lambda */
		FK_CLOSURE, 			/* < a closure function produced by a binding */
		FK_CONSTRUCTOR, 		/* < a constructor used for creating object instances */
		FK_DESTRUCTOR, 			/* < a destructor used for destroying object instances */
		FK_MEMBER_FUNCTION		/* < a member function being associated to a class */
	};

	/**
	 * The accessor associated to a function type. Each function type is composed of 3 sub-nodes.
	 * The first is forming a list of parameters, the second the return type. The last node
	 * determines the kind of function - according to the FunctionKind enumeration.
	 */
	IR_NODE_ACCESSOR(FunctionType, Type, Types, Type, UIntValue)
		/**
		 * Obtains the list of parameters required for invoking functions of this type.
		 */
		IR_NODE_PROPERTY(Types, ParameterTypes, 0);

		/**
		 * Obtains the return type of the represented function type.
		 */
		IR_NODE_PROPERTY(Type, ReturnType, 1);

		/**
		 * Obtains a pointer to the child node determining the kind of this function type.
		 */
		IR_NODE_PROPERTY(UIntValue, FunctionKind, 2);

		/**
		 * A utility function allowing to determine directly whether a function
		 * is plain or not.
		 */
		bool isPlain() const { return FunctionTypeAccessor<Derived,Ptr>::getKind() == FK_PLAIN; }

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a closure or not.
		 */
		bool isClosure() const { return FunctionTypeAccessor<Derived,Ptr>::getKind() == FK_CLOSURE; }

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a constructor or not.
		 */
		bool isConstructor() const { return FunctionTypeAccessor<Derived,Ptr>::getKind() == FK_CONSTRUCTOR; }

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a destructor or not.
		 */
		bool isDestructor() const { return FunctionTypeAccessor<Derived,Ptr>::getKind() == FK_DESTRUCTOR; }

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a member function or not.
		 */
		bool isMemberFunction() const { return FunctionTypeAccessor<Derived,Ptr>::getKind() == FK_MEMBER_FUNCTION; }

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a constructor, destructor or member function.
		 */
		bool isMember() const {
			return FunctionTypeAccessor<Derived,Ptr>::isConstructor() ||
				   FunctionTypeAccessor<Derived,Ptr>::isDestructor() ||
				   FunctionTypeAccessor<Derived,Ptr>::isMemberFunction();
		}

		/**
		 * Obtains a list of types forming the parameter types of this function type.
		 */
		vector<Ptr<const Type>> getParameterTypeList() const {
			return getParameterTypes()->getElements();
		}

		/**
		 * Obtains a reference to the requested parameter type.
		 */
		Ptr<const Type> getParameterType(unsigned index) const {
			return FunctionTypeAccessor<Derived, Ptr>::getParameterTypes()->getElement(index);
		}

		/**
		 * Obtains the kind of function this function type is representing.
		 */
		FunctionKind getKind() const {
			return (FunctionKind)getFunctionKind()->getValue();
		}

		/**
		 * Obtains the object type this function is attached to in case it is a constructor, destructor
		 * or member function. In case it is a plain or closure function type a call to this function is
		 * invalid.
		 */
		Ptr<const Type> getObjectType() const {
			assert(isConstructor() || isDestructor() || isMemberFunction());
			assert(!getParameterTypes().empty());
			assert(getParameterType(0)->getNodeType() == NT_RefType);
			static const auto caster = typename Ptr<const RefType>::StaticCast();
			return caster.template operator()<const RefType>(getParameterType(0))->getElementType();
		}
	};

	/**
	 * This type corresponds to the type of a function. It specifies the argument types and the
	 * value returned by the members of this type.
	 */
	IR_NODE(FunctionType, Type)
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
		 * @param kind determining the kind of function type to be constructed
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypesPtr& paramType, const TypePtr& returnType, FunctionKind kind = FK_PLAIN) {
			return manager.get(FunctionType(paramType, returnType, UIntValue::get(manager, kind)));
		}

		/**
		 * This method provides a static factory method for function types. It will return a pointer to
		 * a function type instance representing the requested function type and being maintained
		 * within the given manager. Return type will be unit.
		 *
		 * @param manager the manager to be used for handling the obtained type pointer
		 * @param paramTypes the type of the single parameter accepted by the resulting function
		 * @param kind determining the kind of function type to be constructed
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypesPtr& paramType, FunctionKind kind = FK_PLAIN) {
			return get(manager, paramType, GenericType::get(manager, "unit"), kind);
		}

		/**
		 * This method provides a static factory method for function types. It will return a pointer to
		 * a function type instance representing the requested function type and being maintained
		 * within the given manager.
		 *
		 * @param manager the manager to be used for handling the obtained type pointer
		 * @param parameterTypes the type of the single parameter accepted by the resulting function
		 * @param returnType the type of value to be returned by the obtained function type
		 * @param kind determining the kind of function type to be constructed
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypeList& parameterTypes, const TypePtr& returnType, FunctionKind kind = FK_PLAIN) {
			return get(manager, Types::get(manager, parameterTypes), returnType, kind);
		}

		/**
		 * This method provides a static factory method for function types. It will return a pointer to
		 * a function type instance representing the requested function type and being maintained
		 * within the given manager. Return type will be unit.
		 *
		 * @param manager the manager to be used for handling the obtained type pointer
		 * @param parameterTypes the type of the single parameter accepted by the resulting function
		 * @param kind determining the kind of function type to be constructed
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypeList& parameterTypes, FunctionKind kind = FK_PLAIN) {
			return get(manager, Types::get(manager, parameterTypes), GenericType::get(manager, "unit"), kind);
		}

		/**
		 * This method provides a static factory method for function types. It will return a pointer to
		 * a function type instance representing the requested function type and being maintained
		 * within the given manager.
		 *
		 * @param manager the manager to be used for handling the obtained type pointer
		 * @param paramTypes the type of the single parameter accepted by the resulting function
		 * @param returnType the type of value to be returned by the obtained function type
		 * @param kind determining the kind of function type to be constructed
		 * @return a pointer to a instance of the required type maintained by the given manager
		 */
		static FunctionTypePtr get(NodeManager& manager, const TypePtr& paramType, const TypePtr& returnType, FunctionKind kind = FK_PLAIN) {
			return get(manager, Types::get(manager, toVector(paramType)), returnType, kind);
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
	IR_LIST_NODE_ACCESSOR(RecTypeDefinition, Support, Definitions, RecTypeBinding)

		/**
		 * Obtains a specific definition maintained within this node.
		 */
		TypePtr getDefinitionOf(const TypeVariablePtr& variable) const {
			auto list = convertList<RecTypeBinding>(RecTypeDefinitionAccessor<Derived, Ptr>::getNode().getChildList());
			auto pos = std::find_if(list.begin(), list.end(), [&](const RecTypeBindingPtr& cur) {
				return *cur->getVariable() == *variable;
			});
			return (pos==list.end())?TypePtr():(*pos)->getType();
		}

		/**
		 * Unrolls this definition once for the given variable.
		 *
		 * @param manager the manager to be used for maintaining the resulting type pointer
		 * @param variable the variable defining the definition to be unrolled once
		 * @return the resulting, unrolled type
		 */
		TypePtr unrollOnce(NodeManager& manager, const TypeVariablePtr& variable) const {
			return RecTypeDefinitionAccessor<Derived,Ptr>::getNode().unrollDefinitionOnce(manager, variable);
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
		TypePtr unrollDefinitionOnce(NodeManager& manager, const TypeVariablePtr& variable) const;

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

		/**
		 * Obtains the definition of the recursive type defined by this
		 * recursive type. It is accessing the internal recursive type definition
		 * and obtaining the type associated to the variable defined within this
		 * recursive type node.
		 */
		Ptr<const Type> getTypeDefinition() const {
			return getDefinition()->getDefinitionOf(getTypeVariable());
		}

		/**
		 * Unrolls this recursive type.
		 */
		TypePtr unroll(NodeManager& manager) const {
			return (*getDefinition()).unrollOnce(manager, getTypeVariable());
		}

		/**
		 * Unrolls this recursive type once.
		 */
		TypePtr unroll() const {
			return unroll(NodeAccessor<Derived,Ptr>::getNode().getNodeManager());
		}
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
	IR_LIST_NODE_ACCESSOR(NamedCompositeType, Type, Entries, StringValue, Parents, NamedType)

		/**
		 * Obtains the name of this type - might be empty. Names can be used to distinguish
		 * structurally identical types.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the list of parent classes associated to this named composite type (for unions
		 * this list shell always be empty).
		 */
		IR_NODE_PROPERTY(Parents, Parents, 1);

		/**
		 * Retrieves the named type entry referencing the given member name within this
		 * struct type or a null pointer if there is no such member.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const NamedType> getNamedTypeEntryOf(const StringValuePtr& name) const {
			auto list = getEntries();
			auto pos = std::find_if(list.begin(), list.end(), [&](const NamedTypePtr& cur) {
				return *cur->getName() == *name;
			});
			return (pos==list.end())?Ptr<const NamedType>():(*pos);
		}

		/**
		 * Retrieves the named type entry referencing the given member name within this
		 * struct type or a null pointer if there is no such member.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const NamedType> getNamedTypeEntryOf(const string& name) const {
			auto list = getEntries();
			auto pos = std::find_if(list.begin(), list.end(), [&](const NamedTypePtr& cur) {
				return cur->getName()->getValue() == name;
			});
			return (pos==list.end())?Ptr<const NamedType>():(*pos);
		}

		/**
		 * Retrieves the type of a member of this composite type or a null pointer if there is no
		 * such entry.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Type> getTypeOfMember(const StringValuePtr& name) const {
			Ptr<const NamedType> entry = getNamedTypeEntryOf(name);
			return (entry)?(entry->getType()):Ptr<const Type>();
		}

	};

	/**
	 * A node type used to represent a named type within a struct or an union.
	 */
	class NamedCompositeType : public Type,
		public NamedCompositeTypeAccessor<NamedCompositeType, Pointer>,
		public NamedCompositeTypeAccessor<NamedCompositeType, Pointer>::node_helper {

	public:

		/**
		 * A type definition defining the type of entries this composed type is consisting of.
		 */
		typedef NamedTypeList Entries;

	protected:

		/**
		 * A simple constructor creating a new named composite type based on the
		 * given element types.
		 *
		 * @param elements the elements of the resulting composite type
		 */
		NamedCompositeType(const NodeType& type, const NodeList& elements);

	};

	/**
	 * A macro creating named composite types.
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
	 * The accessor for instances of struct types.
	 */
	template<typename D,template<typename T> class P>
	struct StructTypeAccessor : public NamedCompositeTypeAccessor<D,P> {};

	/**
	 * The type used to represent struct / records.
	 */
	IR_NAMED_COMPOSITE_TYPE(StructType)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	public:

		/**
		 * A factory method allowing to obtain a pointer to a struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct type
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param entries the list of entries the new type should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructTypePtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents, const vector<NamedTypePtr>& entries = vector<NamedTypePtr>()) {
			NodeList children;
			children.push_back(name);
			children.push_back(parents);
			children.insert(children.end(), entries.begin(), entries.end());
			return manager.get(StructType(children));
		}

		/**
		 * A factory method allowing to obtain a pointer to an unnamed struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param entries the list of entries the new type should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructTypePtr get(NodeManager& manager, const ParentsPtr& parents, const vector<NamedTypePtr>& entries = vector<NamedTypePtr>()) {
			return get(manager, StringValue::get(manager, ""), parents, entries);
		}

		/**
		 * A factory method allowing to obtain a pointer to a named struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct type
		 * @param entries the list of entries the new type should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructTypePtr get(NodeManager& manager, const StringValuePtr& name, const vector<NamedTypePtr>& entries = vector<NamedTypePtr>()) {
			return get(manager, name, Parents::get(manager), entries);
		}

		/**
		 * A factory method allowing to obtain a pointer to an unnamed struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param entries the list of entries the new type should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructTypePtr get(NodeManager& manager, const vector<NamedTypePtr>& entries = vector<NamedTypePtr>()) {
			return get(manager, Parents::get(manager), entries);
		}

	};



	// --------------------------------- Union Type ----------------------------

	/**
	 * The accessor for instances of union types.
	 */
	template<typename D,template<typename T> class P>
	struct UnionTypeAccessor : public NamedCompositeTypeAccessor<D,P> {};

	/**
	 * The type used to represent unions.
	 */
	IR_NAMED_COMPOSITE_TYPE(UnionType)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			assert(getParents()->empty() && "Unions must not be derived!");
			return out << "union<" << join(",",getEntries(), print<deref<NodePtr>>()) << ">";
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
			NodeList children;
			children.push_back(StringValue::get(manager, ""));
			children.push_back(Parents::get(manager));
			children.insert(children.end(), entries.begin(), entries.end());
			return manager.get(UnionType(children));
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
		class NAME : public SingleElementType, public NAME ## Accessor<NAME, Pointer>, public NAME ## Accessor<NAME, Pointer>::node_helper { \
			NAME(const NodeList& children) : SingleElementType(NT_ ## NAME, children), NAME ## Accessor<NAME, Pointer>::node_helper(getChildNodeList()) {} \
			template<typename ... Children> \
			NAME(const Pointer<const Children>& ... children) : SingleElementType(NT_ ## NAME, children ...), NAME ## Accessor<NAME, Pointer>::node_helper(getChildNodeList()) {} \
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

} // end namespace core
} // end namespace insieme




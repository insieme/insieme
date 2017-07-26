/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_values.h"

// ---------------------------------------------- IR Types -----------------------------------------------

namespace insieme {
namespace core {

	namespace detail {
		/// Used to sort node lists in types, to enable equivalent representation of semantically equivalent types
		/// (e.g. class method and constructor order is irrelevant)
		bool semanticNodeLessThan(const core::NodePtr& a, const core::NodePtr& b);
	}


	// ---------------------------------------- An abstract base type ------------------------------

	/**
	 * The accessor for instances of type expressions.
	 */
	template <typename D, template <typename T> class P>
	struct Accessor<Type,D,P> : public Accessor<Node,D, P> {};

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
		template <typename... Nodes>
		Type(const NodeType nodeType, const Pointer<const Nodes>&... children)
		    : Node(nodeType, NC_Type, children...) {}

		/**
		 * A constructor creating a new instance of this type based on a given child-node list.
		 *
		 * @param nodeType the type of the newly created node
		 * @param children the child nodes to be used to create the new node
		 */
		Type(const NodeType nodeType, const NodeList& children) : Node(nodeType, NC_Type, children) {}
	};


	// ------------------------------------ A class representing a list of types  ------------------------------

	/**
	 * The accessor associated to an type parameter list.
	 */
	IR_LIST_NODE_ACCESSOR(Types, Support, Types, Type)
	IR_NODE_END()

	/**
	 * A node type representing a list of type parameters.
	 */
	IR_NODE(Types, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
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
	IR_NODE_END()


	// ------------------------------------ A class representing a parent type  ------------------------------


	/**
	 * An enumeration used for distinguishing different inheritance access specifiers.
	 */
	enum AccessSpecifier {
		AS_PUBLIC = 1,
		AS_PROTECTED,
		AS_PRIVATE
	};

	/**
	 * The accessor associated to a parent type reference. A parent type reference is linking a
	 * base type to a parent type via inheritance. The inheritance relation may be virtual.
	 */
	IR_NODE_ACCESSOR(Parent, Support, BoolValue, UIntValue, Type)

		/**
		 * Obtains the flag determining whether this parent type is a virtual or non-virtual parent type.
		 */
		IR_NODE_PROPERTY(BoolValue, Virtual, 0);

		/**
		 * Obtains the flag determining whether this parent type is a virtual or non-virtual parent type.
		 */
		IR_NODE_PROPERTY(UIntValue, AccessSpecifierKind, 1);

		/**
		 * Obtains the parent type this parent node is referring to.
		 */
		IR_NODE_PROPERTY(Type, Type, 2);

		/**
		 * Obtains the kind of access specifier provided for this parent.
		 */
		AccessSpecifier getAccessSpecifier() const {
			return (AccessSpecifier)(this->getAccessSpecifierKind()->getValue());
		}

		/**
		 * Determines whether this parent node is referring to a type via virtual inheritance.
		 */
		bool isVirtual() const {
			return getVirtual().getValue();
		}

		/**
		 * Determines whether this node is modeling a public inheritance.
		 */
		bool isPublic() const {
			return this->getAccessSpecifier() == AS_PUBLIC;
		}

		/**
		 * Determines whether this node is modeling a public inheritance.
		 */
		bool isProtected() const {
			return this->getAccessSpecifier() == AS_PROTECTED;
		}

		/**
		 * Determines whether this node is modeling a public inheritance.
		 */
		bool isPrivate() const {
			return this->getAccessSpecifier() == AS_PRIVATE;
		}

	IR_NODE_END()

	/**
	 * The node type used to represent a link to a parent class within a base type.
	 */
	IR_NODE(Parent, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			if(isVirtual()) { out << "virtual "; }
			return out << *getType();
		}

	  public:

		/**
		 * This static factory method allows to construct a new parent-link instance referencing
		 * the given type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul a flag determining whether the result should be a virtual inheritance link
		 * @param access the access specifier determining the kind of inheritance (public, private or protected)
		 * @param type the type to be referenced as a parent class
		 * @return the requested type instance managed by the given manager
		 */
		static ParentPtr get(NodeManager& manager, const BoolValuePtr& virtul, const UIntValuePtr& access, const TypePtr& type) {
			return manager.get(Parent(virtul, access, type));
		}

		/**
			* This static factory method allows to construct a new parent-link instance referencing
			* the given type.
			*
			* @param manager the manager used for maintaining instances of this class
			* @param virtul a flag determining whether the result should be a virtual inheritance link
			* @param access the access specifier determining the kind of inheritance (public, private or protected)
			* @param type the type to be referenced as a parent class
			* @return the requested type instance managed by the given manager
		*/
		static ParentPtr get(NodeManager& manager,  bool virtul, const UIntValuePtr& access, const TypePtr& type) {
			return get(manager, BoolValue::get(manager, virtul), access, type);
		}

		/**
		 * This static factory method allows to construct a new parent-link instance referencing
		 * the given type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul a flag determining whether the result should be a virtual inheritance link
		 * @param access the access specifier determining the kind of inheritance (public, private or protected)
		 * @param type the type to be referenced as a parent class
		 * @return the requested type instance managed by the given manager
		 */
		static ParentPtr get(NodeManager& manager, const BoolValuePtr& virtul, AccessSpecifier access, const TypePtr& type) {
			return manager.get(Parent(virtul, UIntValue::get(manager, (unsigned)access), type));
		}

		/**
		 * This static factory method allows to construct a new parent-link instance referencing
		 * the given type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul a flag determining whether the result should be a virtual inheritance link
		 * @param access the access specifier determining the kind of inheritance (public, private or protected)
		 * @param type the type to be referenced as a parent class
		 * @return the requested type instance managed by the given manager
		 */
		static ParentPtr get(NodeManager& manager, bool virtul, AccessSpecifier access, const TypePtr& type) {
			return get(manager, BoolValue::get(manager, virtul), UIntValue::get(manager, (unsigned)access), type);
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
		static ParentPtr get(NodeManager& manager, const BoolValuePtr& virtul, const TypePtr& type) {
			return get(manager, virtul, AS_PUBLIC, type);
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
	IR_NODE_END()

	// ------------------------------------ A class representing a list of parent types  ------------------------------

	/**
	 * The accessor associated to a list of parent types.
	 */
	IR_LIST_NODE_ACCESSOR(Parents, Support, Types, Parent)
	IR_NODE_END()

	/**
	 * A node type representing a list of parent types.
	 */
	IR_NODE(Parents, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
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
	IR_NODE_END()


	// ---------------------------------------- Generic Type ------------------------------

	/**
	 * The accessor associated to generic types.
	 */
	IR_NODE_ACCESSOR(GenericType, Type, StringValue, Parents, Types)
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

		/**
		 * Obtains a list of types forming the parameter types of this function type.
		 */
		vector<Ptr<const Type>> getTypeParameterList() const {
			return getTypeParameter()->getElements();
		}

	IR_NODE_END()

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
		virtual std::ostream& printTo(std::ostream & out) const;

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
		 */
		static GenericTypePtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents, const TypesPtr& typeParams) {
			return manager.get(GenericType(name, parents, typeParams));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a generic type pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 */
		static GenericTypePtr get(NodeManager& manager, const StringValuePtr& name, const TypesPtr& typeParams) {
			return get(manager, name, Parents::get(manager), typeParams);
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
		static GenericTypePtr get(NodeManager& manager, const string& name, const ParentList& parentTypes, const TypeList& typeParams = TypeList()) {
			return get(manager, StringValue::get(manager, name), Parents::get(manager, parentTypes), Types::get(manager, typeParams));
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
		static GenericTypePtr get(NodeManager& manager, const string& name, const TypeList& typeParams = TypeList()) {
			return get(manager, StringValue::get(manager, name), Types::get(manager, typeParams));
		}
	IR_NODE_END()


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
	IR_NODE_END()

	/**
	 * A node type representing concrete type variables.
	 */
	IR_NODE(TypeVariable, Type)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
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
	IR_NODE_END()


	// ------------------------------------ A class representing variadic type variables  ------------------------------

	/**
	 * The accessor associated to a variadic type variable. Each variadic type variable has a single child node - a string value representing
	 * the name of the variable.
	 */
	IR_NODE_ACCESSOR(VariadicTypeVariable, Type, StringValue)
		/**
		 * Obtains the name of this variadic type variable.
		 */
		IR_NODE_PROPERTY(StringValue, VarName, 0);
	IR_NODE_END()

	/**
	 * A node type representing concrete type variables.
	 */
	IR_NODE(VariadicTypeVariable, Type)
	protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "'" << *getVarName() << "...";
		}

	public:
		/**
		 * This static factory method allows to construct a variadic type variable based on a string value (its identifier).
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting type variable
		 * @return the requested type instance managed by the given manager
		 */
		static VariadicTypeVariablePtr get(NodeManager& manager, const StringValuePtr& name) {
			return manager.get(VariadicTypeVariable(name));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a variadic type variable pointer pointing toward a variable with the given name maintained by the
		 * given manager.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting type variable
		 * @return the requested type instance managed by the given manager
		 */
		static VariadicTypeVariablePtr get(NodeManager& manager, const string& name) {
			return get(manager, StringValue::get(manager, name));
		}
	IR_NODE_END()


	// ------------------------------------ A class representing generic type variables  ------------------------------

	/**
	 * The accessor associated to generic types variable.
	 */
	IR_NODE_ACCESSOR(GenericTypeVariable, Type, StringValue, Types)
		/**
		 * Obtains the name of this generic type variable.
		 */
		IR_NODE_PROPERTY(StringValue, VarName, 0);

		/**
		 * Obtains the list of variable type parameters of this generic type variable.
		 */
		IR_NODE_PROPERTY(Types, TypeParameter, 1);

		/**
		 * Obtains a reference to the type parameter with the given index.
		 */
		Ptr<const Type> getTypeParameter(std::size_t index) const {
			return getTypeParameter()->getElement(index);
		}

		/**
		 * Obtains a list of types forming the parameter types of this function type.
		 */
		vector<Ptr<const Type>> getTypeParameterList() const {
			return getTypeParameter()->getElements();
		}

	IR_NODE_END()

	/**
	 * This type represents a generic type variable which can be used to represent placeholders for arbitrary user defined
	 * or derived types.
	 */
	IR_NODE(GenericTypeVariable, Type)
	protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	public:

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a generic type variable pointer pointing toward a variable with the given name, and type
		 * parameters which is maintained by the given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 */
		static GenericTypeVariablePtr get(NodeManager& manager, const StringValuePtr& name, const TypesPtr& typeParams) {
			return manager.get(GenericTypeVariable(name, typeParams));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a generic type variable pointer pointing toward a variable with the given properties maintained by the
		 * given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 */
		static GenericTypeVariablePtr get(NodeManager& manager, const string& name, const TypeList& typeParams = TypeList()) {
			return get(manager, StringValue::get(manager, name), Types::get(manager, typeParams));
		}

	IR_NODE_END()


	// ------------------------------------ A class representing variadic generic type variables  ------------------------------

	/**
	* The accessor associated to variadic generic types variable.
	*/
	IR_NODE_ACCESSOR(VariadicGenericTypeVariable, Type, StringValue, Types)
		/**
		* Obtains the name of this generic type variable.
		*/
		IR_NODE_PROPERTY(StringValue, VarName, 0);

		/**
		* Obtains the list of variable type parameters of this generic type variable.
		*/
		IR_NODE_PROPERTY(Types, TypeParameter, 1);

		/**
		* Obtains a reference to the type parameter with the given index.
		*/
		Ptr<const Type> getTypeParameter(std::size_t index) const {
			return getTypeParameter()->getElement(index);
		}

		/**
		* Obtains a list of types forming the parameter types of this function type.
		*/
		vector<Ptr<const Type>> getTypeParameterList() const {
			return getTypeParameter()->getElements();
		}

	IR_NODE_END()

	/**
	 * This type represents a variadic generic type variable.
	 */
	IR_NODE(VariadicGenericTypeVariable, Type)
	protected:
		/**
		* Prints a string representation of this node to the given output stream.
		*/
		virtual std::ostream& printTo(std::ostream & out) const;

	public:

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a variadic generic type variable pointer pointing toward a variable with the given name, and type
		 * parameters which is maintained by the given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 */
		static VariadicGenericTypeVariablePtr get(NodeManager& manager, const StringValuePtr& name, const TypesPtr& typeParams) {
			return manager.get(VariadicGenericTypeVariable(name, typeParams));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a variadic generic type variable pointer pointing toward a variable with the given properties maintained by the
		 * given manager.
		 *
		 * @param manager		the manager to be used for creating the node (memory management)
		 * @param name 			the name of the new type (only the prefix)
		 * @param typeParams	the type parameters of this type, concrete or variable
		 */
		static VariadicGenericTypeVariablePtr get(NodeManager& manager, const string& name, const TypeList& typeParams = TypeList()) {
			return get(manager, StringValue::get(manager, name), Types::get(manager, typeParams));
		}

	IR_NODE_END()

	// ---------------------------------------- A tuple type ------------------------------

	/**
	 * The accessor associated to a tuple type. Each tuple type is consisting of a list
	 * of type pointers.
	 */
	IR_LIST_NODE_ACCESSOR(TupleType, Type, ElementTypes, Type)
	IR_NODE_END()

	/**
	 * A node type representing Tuple Type.
	 */
	IR_NODE(TupleType, Type)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
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
	IR_NODE_END()


	// ---------------------------------------- Function Type ------------------------------

	/**
	 * An enumeration used for distinguishing the various kinds of function types.
	 */
	enum FunctionKind {
		FK_PLAIN = 1,              /* < a plain function produced by a simple lambda */
		FK_CLOSURE,                /* < a closure function produced by a binding */
		FK_CONSTRUCTOR,            /* < a constructor used for creating object instances */
		FK_DESTRUCTOR,             /* < a destructor used for destroying object instances */
		FK_MEMBER_FUNCTION,        /* < a member function being associated to a class */
		FK_VIRTUAL_MEMBER_FUNCTION /* < a member function being associated to a class */
	};

	/**
	 * The accessor associated to a function type. Each function type is composed of 4 sub-nodes.
	 * The first is forming a list of parameters, the second the return type. The third node
	 * determines the kind of function - according to the FunctionKind enumeration, and the last
	 * node optionally provides a list of instantiated or uninstantiated generic argument types.
	 * (For generic type parameters which do not appear in the function type)
	 */
	IR_NODE_ACCESSOR(FunctionType, Type, Types, Type, UIntValue, Types)
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
		 * Obtains the (optional) list of instantiation types.
		 */
		IR_NODE_PROPERTY(Types, InstantiationTypes, 3);

		/**
		 * A utility function allowing to determine directly whether a function
		 * is plain or not.
		 */
		bool isPlain() const {
			return getKind() == FK_PLAIN;
		}

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a closure or not.
		 */
		bool isClosure() const {
			return getKind() == FK_CLOSURE;
		}

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a constructor or not.
		 */
		bool isConstructor() const {
			return getKind() == FK_CONSTRUCTOR;
		}

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a destructor or not.
		 */
		bool isDestructor() const {
			return getKind() == FK_DESTRUCTOR;
		}

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a member function or not.
		 */
		bool isMemberFunction() const {
			return getKind() == FK_MEMBER_FUNCTION;
		}

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a virtual member function or not.
		 */
		bool isVirtualMemberFunction() const {
			return getKind() == FK_VIRTUAL_MEMBER_FUNCTION;
		}

		/**
		 * A utility function allowing to determine directly whether a function
		 * is a constructor, destructor or member function.
		 */
		bool isMember() const {
			return isConstructor() || isDestructor()
			       || isMemberFunction() || isVirtualMemberFunction();
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
			return getParameterTypes()->getElement(index);
		}

		/**
		 * Obtains the kind of function this function type is representing.
		 */
		FunctionKind getKind() const {
			return (FunctionKind)getFunctionKind()->getValue();
		}

		/**
		 * Obtains a list of instantiation types of this function type.
		 */
		vector<Ptr<const Type>> getInstantiationTypeList() const {
			return getInstantiationTypes()->getElements();
		}

		/**
		 * Returns true if the list of instantiation types is non-empty.
		 */
		bool hasInstantiationTypes() const {
			return getInstantiationTypes()->getTypes().size() != 0;
		}

	IR_NODE_END()

	/**
	 * This type corresponds to the type of a function. It specifies the argument types and the
	 * value returned by the members of this type.
	 */
	IR_NODE(FunctionType, Type)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

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
	     * @param instantiationTypes determining the instantiation types of the function
	     * @return a pointer to a instance of the required type maintained by the given manager
	     */
	    static FunctionTypePtr get(NodeManager& manager, const TypesPtr& paramType, const TypePtr& returnType, FunctionKind kind = FK_PLAIN,
	                               TypesPtr instantiationTypes = TypesPtr()) {
		    if(!instantiationTypes) instantiationTypes = Types::get(manager, TypeList());
		    return manager.get(FunctionType(paramType, returnType, UIntValue::get(manager, kind), instantiationTypes));
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
	IR_NODE_END()


	// ------------------------------------ Tag Type Reference  ------------------------------

	/**
	 * The accessor associated to a tag type reference utilized within tag-types to form (optional) recursive
	 * dependencies.
	 */
	IR_NODE_ACCESSOR(TagTypeReference, Type, StringValue)
		/**
		 * Obtains the name of this tag.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

	IR_NODE_END()

	/**
	 * A node type representing concrete tag type reference.
	 */
	IR_NODE(TagTypeReference, Type)

	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "^" << *getName();
		}

	  public:

		/**
		 * This static factory method allows to construct a tag type reference based on a string value (its identifier).
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting tag type reference
		 * @return the requested type instance managed by the given manager
		 */
		static TagTypeReferencePtr get(NodeManager& manager, const StringValuePtr& name) {
			return manager.get(TagTypeReference(name));
		}

		/**
		 * This method provides a static factory method for this type of node. It will return
		 * a tag type reference pointer pointing toward a tag with the given name maintained by the
		 * given manager.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the identifier defining the name of the resulting tag type reference
		 * @return the requested type instance managed by the given manager
		 */
		static TagTypeReferencePtr get(NodeManager& manager, const string& name) {
			return get(manager, StringValue::get(manager, name));
		}
	IR_NODE_END()


	// ---------------------------------------- Tag Type ------------------------------

	/**
	 * The accessor associated to a tag type binding. Each binding maps a tag-type reference to
	 * a record - potentially including a recursive usage of the tag-type reference.
	 */
	IR_NODE_ACCESSOR(TagTypeBinding, Support, TagTypeReference, Record)
		/**
		 * Obtains the tag being bound by this binding.
		 */
		IR_NODE_PROPERTY(TagTypeReference, Tag, 0);

		/**
		 * Obtains a reference to the record being bound to the associated tag.
		 */
		IR_NODE_PROPERTY(Record, Record, 1);
	IR_NODE_END()

	/**
	 * A node type used to represent tag type bindings.
	 */
	IR_NODE(TagTypeBinding, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to construct a new binding between the given
		 * tag and record.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param tag the tag to be bound
		 * @param record the record to be bound to the given tag
		 * @return the requested bindign managed by the given manager
		 */
		static TagTypeBindingPtr get(NodeManager& manager, const TagTypeReferencePtr& tag, const RecordPtr& record) {
			return manager.get(TagTypeBinding(tag, record));
		}
	IR_NODE_END()


	/**
	 * The tag type definition is a a list of tag type bindings enumerating related
	 * bindings in a recursive group (see: tag type).
	 */
	IR_LIST_NODE_ACCESSOR(TagTypeDefinition, Support, Definitions, TagTypeBinding)

		/**
		 * Obtains a specific definition maintained within this node.
		 */
		Ptr<const Record> getDefinitionOf(const TagTypeReferencePtr& tag) const {
			const auto& list = getDefinitions();
			auto pos = std::find_if(list.begin(), list.end(), [&](const TagTypeBindingPtr& cur) { return *cur->getTag() == *tag; });
			return (pos == list.end()) ? Ptr<const Record>() : (*pos)->getRecord();
		}

		/**
		* Obtains a list of addresses pointing to recursive references to any of the tag types defined
		* by this definition.
		*
		* @param reference the tag type reference to be searched for
		* @return the list of recursive references within this definition, rooted by this definition.
		*/
	    const vector<TagTypeReferenceAddress>& getRecursiveReferences() const {
			return this->getNode().getRecursiveReferences();
		}

		/**
		* Obtains a list of addresses pointing to recursive references to the given tag type reference.
		* The targeted reference has to be defined by this definition.
		*
		* @param reference the tag type reference to be searched for
		* @return the list of recursive references within this definition, rooted by this definition.
		*/
		const vector<TagTypeReferenceAddress>& getRecursiveReferencesOf(const TagTypeReferencePtr& reference) const {
			return this->getNode().getRecursiveReferencesOf(reference);
		}

		/**
		 * Peels this definition once for the given variable.
		 *
		 * @param manager the manager to be used for maintaining the resulting type pointer
		 * @param tag the tag defining the definition to be unrolled once
		 * @param times the number of times it shall be peeled
		 * @return the resulting, peeled type
		 */
		TagTypePtr peel(NodeManager& manager, const TagTypeReferencePtr& tag, unsigned times = 1) const;

		/**
		 * Peels the given member out of this definition. The member might be a member function,
		 * a constructor or destructor.
		 *
		 * @param manager the manager to be used for maintaining the resulting type pointer
		 * @param member the member to be peeled out
		 * @return the peeled out member
		 */
		NodePtr peel(NodeManager& manager, const NodePtr& member) const;

	IR_NODE_END()

	/**
	 * A node type used to represent groups of tag type bindings.
	 */
	IR_NODE(TagTypeDefinition, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "{" << join(",", getChildList(), print<deref<NodePtr>>()) << "}";
		}

	  public:
		/**
		 * This static factory method constructing a new tag type definition based
		 * on a given list of tag type bindings.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param bindings the bindings to be included within this definition
		 * @return the requested type instance managed by the given manager
		 */
		static TagTypeDefinitionPtr get(NodeManager& manager, const TagTypeBindingMap& bindings);

		/**
		 * Obtains a list of addresses pointing to recursive references to any of the tag types defined
		 * by this definition.
		 *
		 * @param reference the tag type reference to be searched for
		 * @return the list of recursive references within this definition, rooted by this definition.
		 */
		const vector<TagTypeReferenceAddress>& getRecursiveReferences() const;

		/**
		 * Obtains a list of addresses pointing to recursive references to the given tag type reference.
		 * The targeted reference has to be defined by this definition.
		 *
		 * @param reference the tag type reference to be searched for
		 * @return the list of recursive references within this definition, rooted by this definition.
		 */
		const vector<TagTypeReferenceAddress>& getRecursiveReferencesOf(const TagTypeReferencePtr& reference) const;

		/**
		 * Peels this definition for the given tag for the given number of times.
		 *
		 * @param manager the manager to be used for maintaining the resulting type pointer
		 * @param tag the tag selecting the definition to be peeled once
		 * @param times the number of times to be peeled
		 * @return the resulting, peeled type
		 */
		TagTypePtr peelDefinition(NodeManager& manager, const TagTypeReferencePtr& tag, unsigned times = 1) const;

		/**
		 * Peels the given member out of this definition. The member might be a member function,
		 * a constructor or destructor.
		 *
		 * @param manager the manager to be used for maintaining the resulting type pointer
		 * @param member the member to be peeled out
		 * @return the peeled out member
		 */
		NodePtr peelMember(NodeManager& manager, const NodePtr& member) const;

		/**
	     * Unpeels the given input type.
	     * E.g. When called on BlaType: "(a : struct BlaType {...}) -> bool" turns into "(a : ^BlaType) -> bool"
		 *
		 * @param mgr the manager to be used for maintaining the resulting type pointer
		 * @param input the type to be unpeeled
		 * @return the unpeeled type
	     */
	    TypePtr unpeel(NodeManager& mgr, const TypePtr& input) const;

	IR_NODE_END()


	/**
	 * The accessor associated to a tag type. A tag type (formerly known as recursive type) is simply referencing
	 * a tag binding within a tag type definition.
	 */
	IR_NODE_ACCESSOR(TagType, Type, TagTypeReference, TagTypeDefinition)
		/**
		 * Obtains the tag picked in the group of nested definitions by this tag type.
		 */
		IR_NODE_PROPERTY(TagTypeReference, Tag, 0);

		/**
		 * Obtains a reference the underlying tag type definition.
		 */
		IR_NODE_PROPERTY(TagTypeDefinition, Definition, 1);

		/**
		 * Determines whether the represented type is a struct.
		 */
		bool isStruct() const {
			return getRecord()->getNodeType() == NT_Struct;
		}

		/**
		 * Determines whether the represented type is a union.
		 */
		bool isUnion() const {
			return getRecord()->getNodeType() == NT_Union;
		}

		/**
		 * Obtains the definition of the record defined by this
		 * tag type. It is accessing the internal tag type definition
		 * and obtaining the record associated to the tag associated to this
		 * tag type node.
		 */
		Ptr<const Record> getRecord() const {
			assert_true(getDefinition()->getDefinitionOf(getTag().template as<TagTypeReferencePtr>()));
			return getDefinition()->getDefinitionOf(getTag().template as<TagTypeReferencePtr>());
		}

		/**
		 * Obtains the definition in form of a struct.
		 */
		Ptr<const Struct> getStruct() const {
			assert_true(isStruct());
			return getRecord().template as<Ptr<const Struct>>();
		}

		/**
		 * Obtains the definition in form of a union.
		 */
		Ptr<const Union> getUnion() const {
			assert_true(isUnion());
			return getRecord().template as<Ptr<const Union>>();
		}

		/**
		 * Retrieves the name of the represented record.
		 */
		Ptr<const StringValue> getName() const {
			return getRecord()->getName();
		}

		/**
		 * Retrieves the list of fields of this tag type.
		 */
		Ptr<const Fields> getFields() const {
			return getRecord()->getFields();
		}

		/**
		 * Retrieves the field with the given name within this
		 * record type or a null pointer if there is no such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Field> getField(const string& name) const {
			return getRecord()->getField(name);
		}

		/**
		 * Retrieves the field with the given name within this
		 * record type or a null pointer if there is no such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Field> getField(const StringValuePtr& name) const {
			return getField(name->getValue());
		}

		/**
		 * Retrieves the type of a field of this record or a null pointer if there is no
		 * such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Type> getFieldType(const string& name) const {
			Ptr<const Field> field = getField(name);
			return (field) ? (field->getType()) : Ptr<const Type>();
		}

		/**
		 * Retrieves the type of a field of this record or a null pointer if there is no
		 * such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Type> getFieldType(const StringValuePtr& name) const {
			return getFieldType(name->getValue());
		}

		/**
		 * Peels this tag type.
		 */
		TagTypePtr peel(NodeManager& manager, unsigned times = 1) const {
			return (*getDefinition()).peel(manager, getTag(), times);
		}

		/**
		 * Peels this recursive type once.
		 */
		TagTypePtr peel(unsigned times = 1) const {
			return peel(this->getNode().getNodeManager(), times);
		}

		/**
		 * Peels out the given member.
		 */
		NodePtr peel(NodeManager& manager, const NodePtr& member) const {
			return (*getDefinition()).peel(manager, member);
		}

		/**
		 * Peels out the given member.
		 */
		NodePtr peel(const NodePtr& member) const {
			return peel(this->getNode().getNodeManager(), member);
		}

	    /**
		* Peels out the given member.
		*/
		template<typename T>
		Pointer<const T> peel(const Pointer<const T>& member) const {
			return peel(this->getNode().getNodeManager(), member.template as<NodePtr>()).template as<Pointer<const T>>();
		}

	    /*
	     * Unpeels the given input type.
	     * E.g. When called on BlaType: "(a : struct BlaType {...}) -> bool" turns into "(a : ^BlaType) -> bool"
	     */
	    TypePtr unpeel(NodeManager& mgr, const TypePtr& input) const {
			return (*getDefinition()).unpeel(mgr, input);
	    }

	    /*
	     * Unpeels the given input type.
	     * E.g. When called on BlaType: "(a : struct BlaType {...}) -> bool" turns into "(a : ^BlaType) -> bool"
	     */
	    TypePtr unpeel(const TypePtr& input) const {
			return unpeel(input->getNodeManager(), input);
	    }

		/**
		 * Determines whether the represented tag type is a recursive type.
		 */
		bool isRecursive() const {
			return this->getNode().isRecursive();
		}

	IR_NODE_END()

	/**
	 * This type connector allows the definition of tag types within the IR language. Tag
	 * types are types which may be defined by referencing themselves. For instance, the definition of
	 * a list type may consist of a pair, where the first element is corresponding to a head
	 * element and the second to the remaining list. Such a type can only be defined through
	 * self-referencing.
	 *
	 * This implementation allows to define mutually recursive types, hence, situations
	 * in which the definition of multiple types are interleaved.
	 */
	IR_NODE(TagType, Type)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * A factory method for obtaining a new tag type instance.
		 *
		 * @param manager the manager which should be maintaining the new instance
		 * @param tag the tag of the bindings to be referenced within the given tag type definition group
		 * @param definition the tag type definition group providing the actual definition of of this tag type
		 */
		static TagTypePtr get(NodeManager& manager, const TagTypeReferencePtr& tag, const TagTypeDefinitionPtr& definition) {
			return manager.get(TagType(tag, definition));
		}

		/**
		 * Determines whether this tag type is a recursive type.
		 */
		bool isRecursive() const;

	IR_NODE_END()


	// --------------------------------- Fields ----------------------------

	/**
	 * The accessor associated to a field. A field is linking a name to
	 * a type. Fields are the components records (structs and unions)
	 * are build form.
	 */
	IR_NODE_ACCESSOR(Field, Support, StringValue, Type)
		/**
		 * Obtains the name bound by this binding.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the bound type.
		 */
		IR_NODE_PROPERTY(Type, Type, 1);
	IR_NODE_END()

	/**
	 * A node type used to represent a named type within a struct or an union.
	 */
	IR_NODE(Field, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << *getName() << ":" << *getType();
		}

	  public:
		/**
		 * This static factory method allows to construct a new binding between the given
		 * name and type.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the name of the field
		 * @param type the type of the field
		 * @return the requested field instance managed by the given manager
		 */
		static FieldPtr get(NodeManager& manager, const StringValuePtr& name, const TypePtr& type) {
			return manager.get(Field(name, type));
		}
	IR_NODE_END()

	/**
	 * The accessor associated to a list of fields.
	 */
	IR_LIST_NODE_ACCESSOR(Fields, Support, Fields, Field)
	IR_NODE_END()

	/**
	 * A node type representing a list of fields.
	 */
	IR_NODE(Fields, Support)
	  protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	  public:

		/**
		 * This static factory method allows to construct a field list based
		 * on a given list of fields.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param fields the fields to be included within the resulting field list
		 * @return the requested field list instance managed by the given manager
		 */
		static FieldsPtr get(NodeManager& manager, const FieldList& fields = FieldList()) {
			assert_false(hasDuplicates(fields, [](const FieldPtr& field) { return field->getName()->getValue(); }))  << "field names must be unique";
			return manager.get(Fields(convertList(fields)));
		}

	IR_NODE_END()


	// --------------------------------- Member Function ----------------------------

	/**
	 * The accessor associated to a member function. A member function is linking a name to
	 * a member function expression. Member functions can be associated to records.
	 */
	IR_NODE_ACCESSOR(MemberFunction, Support, StringValue, BoolValue, Expression)

		/**
		 * Obtains the name of this member function.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the flag determining whether it is virtual.
		 */
		IR_NODE_PROPERTY(BoolValue, VirtualFlag, 1);

		/**
		 * Obtains the implementation of this member function.
		 */
		IR_NODE_PROPERTY(Expression, Implementation, 2);

		/**
		 * Obtains the name of this member function as a string.
		 */
		const std::string& getNameAsString() const {
			return getName().getValue();
		}

		/**
		 * Obtains the type of this member function.
		 */
		FunctionTypePtr getType() const {
			return getImplementation()->getType().template as<FunctionTypePtr>();
		}

		/**
		 * Determines whether this member function is marked virtual or not.
		 */
		bool isVirtual() const {
			return getVirtualFlag().getValue();
		}

	IR_NODE_END()

	/**
	 * The node type utilized to represent member functions.
	 */
	IR_NODE(MemberFunction, Support)
	  protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << (isVirtual() ? "virtual " : "") << *getName() << ":" << *getImplementation();
		}

	  public:

		/**
		 * This static factory method allows to construct a new member function
		 * based on the given ingredients.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul the flag determining whether the member is virtual
		 * @param name the name of the member function
		 * @param impl the implementation of the member function
		 * @return the requested member function instance managed by the given manager
		 */
		static MemberFunctionPtr get(NodeManager& manager, const BoolValuePtr& virtul, const StringValuePtr& name, const ExpressionPtr& impl) {
			return manager.get(MemberFunction(name, virtul, impl));
		}

		/**
		 * This static factory method allows to construct a new member function
		 * based on the given ingredients.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param virtul the flag determining whether the member is virtual
		 * @param name the name of the member function
		 * @param impl the implementation of the member function
		 * @return the requested member function instance managed by the given manager
		 */
		static MemberFunctionPtr get(NodeManager& manager, bool virtul, const std::string& name, const ExpressionPtr& impl) {
			return get(manager, BoolValue::get(manager, virtul), StringValue::get(manager, name), impl);
		}

	IR_NODE_END()

	/**
	 * The accessor associated to a list of member functions.
	 */
	IR_LIST_NODE_ACCESSOR(MemberFunctions, Support, Members, MemberFunction)
	IR_NODE_END()

	/**
	 * A node type representing a list of member functions.
	 */
	IR_NODE(MemberFunctions, Support)
	  protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	  public:

		/**
		 * This static factory method allows to construct a member function list based
		 * on a given list of member functions.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param members the members to be included within the resulting member function list
		 * @return the requested member function list instance managed by the given manager
		 */
	    static MemberFunctionsPtr get(NodeManager& manager, const MemberFunctionList& fields = MemberFunctionList()) {
		    MemberFunctionList sorted = fields;
		    std::stable_sort(sorted.begin(), sorted.end(), detail::semanticNodeLessThan);
		    return manager.get(MemberFunctions(convertList(sorted)));
	    }

	IR_NODE_END()


	// ------------------------------ Pure Virtual Member Function ----------------------------

	/**
	 * The accessor associated to pure virtual member functions. A pure virtual member function is linking a name to
	 * function type. Pure virtual member functions can be associated to records.
	 */
	IR_NODE_ACCESSOR(PureVirtualMemberFunction, Support, StringValue, FunctionType)

		/**
		 * Obtains the name of this member function.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the type of this pure virtual member function.
		 */
		IR_NODE_PROPERTY(FunctionType, Type, 1);

		/**
		 * Obtains the name of this member function as a string.
		 */
		const std::string& getNameAsString() const {
			return getName().getValue();
		}

	IR_NODE_END()

	/**
	 * The node type utilized to represent pure virtual member functions.
	 */
	IR_NODE(PureVirtualMemberFunction, Support)
	  protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "pure virtual " << *getName() << ":" << *getType();
		}

	  public:

		/**
		 * This static factory method allows to construct a new member function
		 * based on the given ingredients.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the name of the pure virtual member function
		 * @param type the type of the pure virtual member function
		 * @return the requested pure virtual member function instance managed by the given manager
		 */
		static PureVirtualMemberFunctionPtr get(NodeManager& manager, const StringValuePtr& name, const FunctionTypePtr& type) {
			return manager.get(PureVirtualMemberFunction(name, type));
		}

		/**
		 * This static factory method allows to construct a new member function
		 * based on the given ingredients.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the name of the pure virtual member function
		 * @param type the type of the pure virtual member function
		 * @return the requested pure virtual member function instance managed by the given manager
		 */
		static PureVirtualMemberFunctionPtr get(NodeManager& manager, const std::string& name, const FunctionTypePtr& type) {
			return get(manager, StringValue::get(manager, name), type);
		}

	IR_NODE_END()

	/**
	 * The accessor associated to a list of pure virtual member functions.
	 */
	IR_LIST_NODE_ACCESSOR(PureVirtualMemberFunctions, Support, Members, PureVirtualMemberFunction)
	IR_NODE_END()

	/**
	 * A node type representing a list of pure virtual member functions.
	 */
	IR_NODE(PureVirtualMemberFunctions, Support)
	  protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	  public:

		/**
		 * This static factory method allows to construct a member function list based
		 * on a given list of member functions.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param members the members to be included within the resulting member function list
		 * @return the requested member function list instance managed by the given manager
		 */
		static PureVirtualMemberFunctionsPtr get(NodeManager& manager, const PureVirtualMemberFunctionList& fields = PureVirtualMemberFunctionList()) {
			return manager.get(PureVirtualMemberFunctions(convertList(fields)));
		}

	IR_NODE_END()

	// --------------------------------- Records ----------------------------

	/**
	 * The accessor associated to a record. A record is a union or a struct/class type in C/C++.
	 */
	template <typename LeafType, template <typename T> class Ptr>
	struct Accessor<Record,LeafType,Ptr> : public Accessor<Support,LeafType, Ptr> {

		/**
		 * Obtains the name of this type - might be empty. Names can be used to distinguish
		 * structurally identical types.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains the list of fields contributing to this record type.
		 */
		IR_NODE_PROPERTY(Fields, Fields, 1);

		/**
		 * Obtains the list of all constructors defined for this record type.
		 */
		IR_NODE_PROPERTY(Expressions, Constructors, 2);

		/**
		 * Obtains the destructor defined for this record type if available.
		 */
		IR_NODE_PROPERTY(Expressions, OptionalDestructor, 3);

		/**
		 * Whether or not the destructor is virtual.
		 */
		IR_NODE_PROPERTY(BoolValue, DestructorVirtual, 4);

		/**
		 * Obtains the list of all member functions fined for this record type.
		 */
		IR_NODE_PROPERTY(MemberFunctions, MemberFunctions, 5);

		/**
		 * Obtains the list of all pure virtual member functions fined for this record type.
		 */
		IR_NODE_PROPERTY(PureVirtualMemberFunctions, PureVirtualMemberFunctions, 6);

		/**
		 * Retrieves the field with the given name within this
		 * record type or a null pointer if there is no such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Field> getField(const string& name) const {
			auto list = getFields();
			auto pos = std::find_if(list.begin(), list.end(), [&](const FieldPtr& cur) { return cur->getName()->getValue() == name; });
			return (pos == list.end()) ? Ptr<const Field>() : (*pos);
		}

		/**
		 * Retrieves the field with the given name within this
		 * record type or a null pointer if there is no such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Field> getField(const StringValuePtr& name) const {
			return getField(name->getValue());
		}

		/**
		 * Retrieves the type of a field of this record or a null pointer if there is no
		 * such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Type> getFieldType(const string& name) const {
			Ptr<const Field> field = getField(name);
			return (field) ? (field->getType()) : Ptr<const Type>();
		}

		/**
		 * Retrieves the type of a field of this record or a null pointer if there is no
		 * such field.
		 *
		 * @param name the name to be searching for
		 */
		Ptr<const Type> getFieldType(const StringValuePtr& name) const {
			return getFieldType(name->getValue());
		}

		/**
		 * Determines whether a destructor is defined.
		 */
		bool hasDestructor() const {
			return !getOptionalDestructor()->getExpressions().empty();
		}

		/**
		 * Determines whether the destructor of this record is virtual or not.
		 */
		bool hasVirtualDestructor() const {
			return getDestructorVirtual().getValue();
		}

		/**
		 * Get the destructor, if one is defined.
		 */
		Ptr<const Expression> getDestructor() const {
			assert_true(hasDestructor());
			return getOptionalDestructor()[0];
		}
	};


	/**
	 * A node type used to represent a common base-class for structs and unions.
	 */
	class Record : public Support, public AbstractFixedSizeNodeHelper<Record,StringValue,Fields,Expressions,Expressions,BoolValue,MemberFunctions,PureVirtualMemberFunctions> {
	  protected:
		/**
		 * A constructor creating a new instance of this type based on a given child-node list.
		 *
		 * @param nodeType the actual node-type the resulting node will be representing
		 * @param children the child nodes to be contained
		 */
		template <typename... Nodes>
		Record(const NodeType nodeType, const Pointer<const Nodes>&... children)
			: Support(nodeType, children...), AbstractFixedSizeNodeHelper(getChildNodeList()) {}

		/**
		 * A constructor creating a new instance of this type based on a given child-node list.
		 *
		 * @param nodeType the type of the newly created node
		 * @param children the child nodes to be used to create the new node
		 */
		Record(const NodeType nodeType, const NodeList& children)
			: Support(nodeType, children), AbstractFixedSizeNodeHelper(getChildNodeList()) {}
	};


    #define IR_RECORD_ACCESSOR(NAME, ...)                                                                                                                  \
		IR_NODE_ACCESSOR(NAME, Record, StringValue, Fields, Expressions, Expressions, BoolValue, MemberFunctions, PureVirtualMemberFunctions, ##__VA_ARGS__)


	// --------------------------------- Struct ----------------------------

	/**
	 * The accessor for instances of structs.
	 */
	IR_RECORD_ACCESSOR(Struct, Parents)

		/**
		 * Obtains the list of parent classes associated to this struct.
		 */
		IR_NODE_PROPERTY(Parents, Parents, 7);

	IR_NODE_END();


	/**
	 * The type used to represent struct / records.
	 */
	IR_NODE(Struct, Record)

	  protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:

		/**
		 * A factory method allowing to obtain a pointer to a struct representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param fields the list of fields the resulting struct should consist of
		 * @param ctors the list of constructors for this struct
		 * @param dtorOpt the destructor for this struct or empty
		 * @param mfuns the list of member functions associated to this struct
		 * @param pvmfuns the list of pure virtual member functions of this struct
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents, const FieldsPtr& fields,
		                     const ExpressionsPtr& ctors, const ExpressionsPtr& dtorOpt, const BoolValuePtr& dtorIsVirtual,
		                     const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvfuns) {
			ExpressionList sortedCtors = ctors.getExpressions();
		    std::stable_sort(sortedCtors.begin(), sortedCtors.end(), detail::semanticNodeLessThan);
			return manager.get(Struct(name, fields, Expressions::get(manager, sortedCtors), dtorOpt, dtorIsVirtual, mfuns, pvfuns, parents));
		}

		/**
		 * A factory method allowing to obtain a pointer to a struct representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param fields the list of fields the resulting struct should consist of
		 * @param ctors the list of constructors for this struct
		 * @param dtor the destructor for this struct
		 * @param mfuns the list of member functions associated to this struct
		 * @param pvmfuns the list of pure virtual member functions of this struct
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents, const FieldsPtr& fields,
		                     const ExpressionsPtr& ctors, const ExpressionPtr& dtor, const BoolValuePtr& dtorIsVirtual,
		                     const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvfuns) {
			return get(manager, name, parents, fields, ctors, Expressions::get(manager, dtor ? toVector(dtor) : ExpressionList()), dtorIsVirtual, mfuns, pvfuns);
		}

		/**
		 * A factory method allowing to obtain a pointer to a struct representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param fields the list of fields the resulting struct should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents, const FieldsPtr& fields);

		/**
		 * A factory method allowing to obtain a pointer to a struct representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param fields the list of fields the resulting struct should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const StringValuePtr& name, const ParentsPtr& parents, const vector<FieldPtr>& fields = vector<FieldPtr>()) {
			return get(manager, name, parents, Fields::get(manager, fields));
		}

		/**
		 * A factory method allowing to obtain a pointer to an unnamed struct representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param parents the list of parent types to be referenced by the resulting struct
		 * @param fields the list of fields the new struct should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const ParentsPtr& parents, const vector<FieldPtr>& fields = vector<FieldPtr>()) {
			return get(manager, StringValue::get(manager, ""), parents, fields);
		}

		/**
		 * A factory method allowing to obtain a pointer to a named struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct
		 * @param fields the list of fields the new struct should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const StringValuePtr& name, const vector<FieldPtr>& fields = vector<FieldPtr>()) {
			return get(manager, name, Parents::get(manager), fields);
		}

		/**
		 * A factory method allowing to obtain a pointer to a named struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting struct
		 * @param fields the list of fields the new struct should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const std::string& name, const vector<FieldPtr>& fields = vector<FieldPtr>()) {
			return get(manager, StringValue::get(manager,name), fields);
		}

		/**
		 * A factory method allowing to obtain a pointer to an unnamed struct type representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param fields the list of fields the new struct should consist of
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static StructPtr get(NodeManager& manager, const vector<FieldPtr>& fields = vector<FieldPtr>()) {
			return get(manager, Parents::get(manager), fields);
		}

	IR_NODE_END()


	// --------------------------------- Union ----------------------------

	/**
	 * The accessor for instances of structs.
	 */
	IR_RECORD_ACCESSOR(Union)
		/** nothing special so far */
	IR_NODE_END();

	/**
	 * The type used to represent unions.
	 */
	IR_NODE(Union, Record)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:

		/**
		 * A factory method allowing to obtain a pointer to a union representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting union
		 * @param fields the list of fields the resulting union should consist of
		 * @param ctors the list of constructors for this union
		 * @param dtorOpt the destructor for this union or empty
		 * @param mfuns the list of member functions associated to this union
		 * @param pvmfuns the list of pure virtual member functions of this union
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static UnionPtr get(NodeManager& manager, const StringValuePtr& name, const FieldsPtr& fields,
		                    const ExpressionsPtr& ctors, const ExpressionsPtr& dtorOpt, const BoolValuePtr& dtorIsVirtual,
		                    const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvfuns) {
			ExpressionList sortedCtors = ctors.getExpressions();
		    std::stable_sort(sortedCtors.begin(), sortedCtors.end(), detail::semanticNodeLessThan);
			return manager.get(Union(name, fields, Expressions::get(manager, sortedCtors), dtorOpt, dtorIsVirtual, mfuns, pvfuns));
		}

		/**
		 * A factory method allowing to obtain a pointer to a union representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param name the name of the resulting union
		 * @param fields the list of fields the resulting union should consist of
		 * @param ctors the list of constructors for this union
		 * @param dtor the destructor for this union
		 * @param mfuns the list of member functions associated to this union
		 * @param pvmfuns the list of pure virtual member functions of this union
		 * @return a pointer to a instance of the requested type. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static UnionPtr get(NodeManager& manager, const StringValuePtr& name, const FieldsPtr& fields,
		                    const ExpressionsPtr& ctors, const ExpressionPtr& dtor, const BoolValuePtr& dtorIsVirtual,
		                    const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvfuns) {
			return get(manager, name, fields, ctors, Expressions::get(manager, dtor ? toVector(dtor) : ExpressionList()), dtorIsVirtual, mfuns, pvfuns);
		}

		/**
		 * A factory method allowing to obtain a pointer to a union representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param the name given to the union
		 * @param fields the list of fields the new union should consist of
		 * @return a pointer to a instance of the requested record. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static UnionPtr get(NodeManager& manager, const StringValuePtr& name, const FieldsPtr& fields);

		/**
		 * A factory method allowing to obtain a pointer to a union representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param the name given to the union
		 * @param fields the list of fields the new union should consist of
		 * @return a pointer to a instance of the requested record. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static UnionPtr get(NodeManager& manager, const StringValuePtr& name, const vector<FieldPtr>& fields) {
			return get(manager, name, Fields::get(manager, fields));
		}

		/**
		 * A factory method allowing to obtain a pointer to a union representing
		 * an instance managed by the given manager.
		 *
		 * @param manager the manager which should be responsible for maintaining the new
		 * 				  type instance and all its referenced elements.
		 * @param fields the list of fields the new union should consist of
		 * @return a pointer to a instance of the requested record. Multiple requests using
		 * 		   the same parameters will lead to pointers addressing the same instance.
		 */
		static UnionPtr get(NodeManager& manager, const vector<FieldPtr>& fields = vector<FieldPtr>()) {
			return get(manager, StringValue::get(manager, ""), fields);
		}
	IR_NODE_END()


	// --------------------------------------------- Numeric Type ---------------------------------------------

	/**
	 * The accessor associated to the number type class. A number type has a single element, an expression
	 * representing its value. The expression may either be a variable or literal.
	 */
	IR_NODE_ACCESSOR(NumericType, Type, Expression)

		/**
		 * Obtains the value of this number type.
		 */
		IR_NODE_PROPERTY(Expression, Value, 0);

		/**
		 * Determines whether this number type is a constant, statically fixed value.
		 */
		bool isConstant() const {
			return this->getValue()->getNodeType() == NT_Literal;
		}

		/**
		 * Determines whether this number type is a variable, dynamically determined value.
		 */
		bool isVariable() const {
			return !isConstant();
		}
	IR_NODE_END()


	/**
	 * A node type representing concrete type variables.
	 */
	IR_NODE(NumericType, Type)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to construct a numeric type based on a literal constant.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param value the value to be represented
		 * @return the requested type instance managed by the given manager
		 */
		static NumericTypePtr get(NodeManager& manager, const LiteralPtr& value);

		/**
		 * This static factory method allows to construct a numeric type based on a variable.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param var the variable to be represented
		 * @return the requested type instance managed by the given manager
		 */
		static NumericTypePtr get(NodeManager& manager, const VariablePtr& var);
	IR_NODE_END()


	// --------------------------- late definitions to resolve cyclic dependencies  ---------------------------------------

	template<typename LeafType, template<typename T> class Ptr>
	TagTypePtr Accessor<TagTypeDefinition, LeafType, Ptr>::peel(NodeManager& manager, const TagTypeReferencePtr& tag, unsigned times) const {
		return Accessor<TagTypeDefinition, LeafType, Ptr>::getNode().peelDefinition(manager, tag, times);
	}

	template<typename LeafType, template<typename T> class Ptr>
	NodePtr Accessor<TagTypeDefinition, LeafType, Ptr>::peel(NodeManager& manager, const NodePtr& member) const {
		return Accessor<TagTypeDefinition, LeafType, Ptr>::getNode().peelMember(manager, member);
	}


} // end namespace core
} // end namespace insieme

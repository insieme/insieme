/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include "insieme/core/ir_statements.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/lazy.h"

namespace insieme {
namespace core {


	// ------------------------------------- Literals -----------------------------------

	/**
	 * The accessor associated to the Literals.
	 */
	IR_NODE_ACCESSOR(Literal, Expression, Type, StringValue)
		/**
		 * Obtains a reference to the value of this literal.
		 */
		IR_NODE_PROPERTY(StringValue, Value, 1);

		/**
		 * Obtains the string version of this literal.
		 */
		const string getStringValue() const {
			if(getValue()->getValue() == "type_literal") { return toString(*this->getType()); }
			return getValue()->getValue();
		}

		/**
		 * A function extracting the value of this type by interpreting it
		 * according to the given type parameter.
		 */
		template <class T>
		const T getValueAs() const {
			return utils::numeric_cast<T>(getStringValue());
		}
	IR_NODE_END()

	/**
	 * The entity used to represent literals within the IR.
	 *
	 * Literal:
	 *   - any constant value, e.g. an integer, character or string
	 *   - any external functions, the literal has to be of a function type
	 *   - any external variable, the literal has to be of a ref-type
	 */
	IR_NODE(Literal, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			if(getValue()->getValue() == "type_literal") { return out << *getType(); }
			return out << *getValue();
		}

	  public:
		/**
		 * A comparison operator for variables.
		 */
		bool operator<(const Literal& other) const;

		/**
		 * This static factory method allows to obtain a literal instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested literal
		 * @param value the value of the requested literal
		 * @return the requested type instance managed by the given manager
		 */
		static LiteralPtr get(NodeManager & manager, const TypePtr& type, const StringValuePtr& value) {
			return manager.get(Literal(type, value));
		}

		/**
		 * This static factory method allows to obtain a literal instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested literal
		 * @param value the value of the requested literal
		 * @return the requested type instance managed by the given manager
		 */
		static LiteralPtr get(NodeManager & manager, const TypePtr& type, const string& value) {
			return get(manager, type, StringValue::get(manager, value));
		}
	IR_NODE_END()


	// ------------------------------------- Variables -----------------------------------

	/**
	 * The accessor associated to the Variable.
	 */
	IR_NODE_ACCESSOR(Variable, Expression, Type, UIntValue)
		/**
		 * Obtains a reference to the ID of this variable.
		 */
		IR_NODE_PROPERTY(UIntValue, ID, 1);

		/**
		 * Obtains the ID of this variable as a value.
		 */
		unsigned int getId() const {
			return getID()->getValue();
		}
	IR_NODE_END()

	/**
	 * The entity used to represent variables within the IR.
	 */
	IR_NODE(Variable, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "v" << getId();
		}

	  public:
		/**
		 * A comparison operator for variables.
		 */
		bool operator<(const Variable& other) const;

		/**
		 * This static factory method allows to obtain an instance of a variable
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested variable
		 * @param id the value of the requested variable
		 * @return the requested type instance managed by the given manager
		 */
		static VariablePtr get(NodeManager & manager, const TypePtr& type, const UIntValuePtr& id) {
			return manager.get(Variable(type, id));
		}

		/**
		 * This static factory method allows to obtain an instance of a variable
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested variable
		 * @param id the value of the requested variable
		 * @return the requested type instance managed by the given manager
		 */
		static VariablePtr get(NodeManager & manager, const TypePtr& type, unsigned int id) {
			return get(manager, type, UIntValue::get(manager, id));
		}

		/**
		 * This static factory method allows to obtain an instance of a variable
		 * within the given node manager based on the given parameters. The ID of
		 * the resulting variable will be a fresh one which has not been used within
		 * this node manager so far.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested variable
		 * @param id the value of the requested variable
		 * @return the requested type instance managed by the given manager
		 */
		static VariablePtr get(NodeManager & manager, const TypePtr& type) {
			unsigned id = manager.getFreshID();
			// Variable var(type,UIntValue::get(manager, id));
			while(manager.contains(Variable(type, UIntValue::get(manager, id)))) {
				id = manager.getFreshID();
			}
			return manager.get(Variable(type, UIntValue::get(manager, id)));
		}
	IR_NODE_END()


	// ------------------------------------- Call Expr -----------------------------------


	/**
	 * The accessor associated to the call expression.
	 */
	IR_LIST_NODE_ACCESSOR(CallExpr, Expression, Arguments, Type, Expression, Expression)

		/**
		 * Obtains a reference to the function called by this expression.
		 */
		IR_NODE_PROPERTY(Expression, FunctionExpr, 1);

		/**
		 * Obtains a reference to the requested argument.
		 */
		Ptr<const Expression> getArgument(unsigned index) const {
			return CallExprAccessor<Derived, Ptr>::getElement(index);
		}
		
		/**
		 * Obtains a list of argument expressions for this call.
		 */
		ExpressionList getArgumentList() const {
			return getArguments();
		}
	IR_NODE_END()

	/**
	 * The entity used to represent function calls within the IR.
	 */
	IR_NODE(CallExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << *getFunctionExpr() << "(" << join(", ", getArguments(), print<deref<ExpressionPtr>>()) << ")";
		}

	  public:
		/**
		 * This static factory method allows to obtain an instance of a call expression
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the call expression, hence the return type of the function
		 * @param function the function to be called
		 * @param arguments the arguments to be passed to the function call
		 * @return the requested type instance managed by the given manager
		 */
		static CallExprPtr get(NodeManager & manager, const TypePtr& type, const ExpressionPtr& function, const NodeRange<ExpressionPtr>& arguments) {
			NodeList children;
			children.push_back(type);
			children.push_back(function);
			children.insert(children.end(), arguments.begin(), arguments.end());
			return manager.get(CallExpr(children));
		}

		/**
		 * This static factory method allows to obtain an instance of a call expression
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the call expression, hence the return type of the function
		 * @param function the function to be called
		 * @param arguments the arguments to be passed to the function call
		 * @return the requested type instance managed by the given manager
		 */
		static CallExprPtr get(NodeManager & manager, const TypePtr& type, const ExpressionPtr& function, const ExpressionList& arguments) {
			return get(manager, type, function, NodeRange<ExpressionPtr>(arguments.begin(), arguments.end()));
		}
	IR_NODE_END()


	// ------------------------------------- Cast Expr -----------------------------------


	/**
	 * The accessor associated to the cast expression.
	 */
	IR_NODE_ACCESSOR(CastExpr, Expression, Type, Expression)
		/**
		 * Obtains a reference to the expression being casted.
		 */
		IR_NODE_PROPERTY(Expression, SubExpression, 1);
	IR_NODE_END()

	/**
	 * The entity used to represent casts within the IR.
	 */
	IR_NODE(CastExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "cast<" << *getType() << ">"
			           << "(" << *getSubExpression() << ")";
		}

	  public:
		/**
		 * This static factory method allows to obtain an instance of a cast expression
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type the given value should be casted to
		 * @param value the value to be casted
		 * @return the requested type instance managed by the given manager
		 */
		static CastExprPtr get(NodeManager & manager, const TypePtr& type, const ExpressionPtr& value) {
			return manager.get(CastExpr(type, value));
		}

	IR_NODE_END()


	// ------------------------------------- Parameters -----------------------------------

	/**
	 * The accessor associated to the parameter node.
	 */
	IR_LIST_NODE_ACCESSOR(Parameters, Support, Parameters, Variable)
	IR_NODE_END()

	/**
	 * A node type representing a list of parameter.
	 */
	IR_NODE(Parameters, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	  public:
		/**
		 * This static factory method allows to construct a parameter list based
		 * on the given variables.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param parameter the list of parameters to be included
		 * @return the requested instance managed by the given manager
		 */
		static ParametersPtr get(NodeManager & manager, const VariableList& parameter) {
			return manager.get(Parameters(convertList(parameter)));
		}
	IR_NODE_END()


	// ------------------------------------- Lambda -----------------------------------


	/**
	 * The accessor associated to the lambda node.
	 */
	IR_NODE_ACCESSOR(Lambda, Support, FunctionType, Parameters, CompoundStmt)

		/**
		 * Obtains a reference to the type of this lambda.
		 */
		IR_NODE_PROPERTY(FunctionType, Type, 0);

		/**
		 * Obtains a reference to the parameters accepted by this lambda.
		 */
		IR_NODE_PROPERTY(Parameters, Parameters, 1);

		/**
		 * Obtains a reference to the body of this lambda.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 2);

		/**
		 * Obtains a reference to the internally maintained parameter vector.
		 */
		vector<Ptr<const Variable>> getParameterList() const {
			return getParameters()->getParameters();
		}
	IR_NODE_END()

	/**
	 * The entity used to represent lambda nodes.
	 */
	IR_NODE(Lambda, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			static auto paramPrinter = [](std::ostream& out, const VariablePtr& cur) { out << *cur->getType() << " " << *cur; };
			return out << "fun(" << join(", ", getParameters(), paramPrinter) << ") " << *getBody();
		}

	  public:
		/**
		 * This static factory method allows to obtain an instance of a lambda node
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the lambda expression
		 * @param params the list of parameters accepted by the requested lambda
		 * @param body the body defining the requested lambda
		 * @return the requested type instance managed by the given manager
		 */
		static LambdaPtr get(NodeManager & manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body);

		/**
		 * This static factory method allows to obtain an instance of a lambda node
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the lambda expression
		 * @param params the list of parameters accepted by the requested lambda
		 * @param body the body defining the requested lambda
		 * @return the requested type instance managed by the given manager
		 */
		static LambdaPtr get(NodeManager & manager, const FunctionTypePtr& type, const VariableList& params, const CompoundStmtPtr& body) {
			return get(manager, type, Parameters::get(manager, params), body);
		}
	IR_NODE_END()


	// ------------------------------------- Lambda Expression -----------------------------------

	/**
	 * The accessor associated to a lambda expression.
	 */
	IR_NODE_ACCESSOR(LambdaExpr, Expression, Type, Variable, LambdaDefinition)

		/**
		 * Obtains a reference to the variable identifying the represented lambda within the definition.
		 */
		IR_NODE_PROPERTY(Variable, Variable, 1);

		/**
		 * Obtains a reference to the definition of this lambda expression.
		 */
		IR_NODE_PROPERTY(LambdaDefinition, Definition, 2);

		/**
		 * Obtains the function type of this
		 */
		Ptr<const FunctionType> getFunctionType() const {
			static const typename Ptr<const FunctionType>::StaticCast caster = typename Ptr<const FunctionType>::StaticCast();
			return caster.template operator()<const FunctionType>(ExpressionAccessor<Derived, Ptr>::getType());
		}

		/**
		 * Obtains a reference to the lambda defining this lambda expression.
		 */
		Ptr<const Lambda> getLambda() const {
			return getDefinition()->getDefinitionOf(getVariable());
		}

		/**
		 * Obtains a reference to the parameters of the lambda defining this expression.
		 */
		Ptr<const Parameters> getParameterList() const {
			return getLambda()->getParameters();
		}

		/**
		 * Obtains a reference to the body of the lambda defining this expression.
		 */
		Ptr<const CompoundStmt> getBody() const {
			return getLambda()->getBody();
		}

		/**
		 * Determines whether this function is recursively defined or not.
		 */
		bool isRecursive() const {
			return ExpressionAccessor<Derived, Ptr>::getNode().isRecursiveInternal();
		}

		/**
		 * Peels this lambda the given amount of time. If the lambda is not recursive, the
		 * result will be the same as this lambda.
		 *
		 * @param numTimes the number of times this lambda shell be peeled
		 * @return the resulting, peeled lambda expression
		 */
		LambdaExprPtr peel(unsigned numTimes = 1) const {
			return peel(ExpressionAccessor<Derived, Ptr>::getNodeManager(), numTimes);
		}

		/**
		 * Peels this lambda the given amount of time. If the lambda is not recursive, the
		 * result will be the same as this lambda.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param numTimes the number of times this lambda shell be peeled
		 * @return the resulting, peeled lambda expression
		 */
		LambdaExprPtr peel(NodeManager & manager, unsigned numTimes = 1) const {
			if(!isRecursive()) { return manager.get(ExpressionAccessor<Derived, Ptr>::getNode()); }
			return getDefinition()->peel(manager, getVariable(), numTimes);
		}


		/**
		 * Unrolls this lambda the given amount of time. If the lambda is not recursive, the
		 * result will be the same as this lambda.
		 *
		 * @param numTimes the number of times this lambda shell be unrolled
		 * @return the resulting, unrolled lambda expression
		 */
		LambdaExprPtr unroll(unsigned numTimes) const {
			return unroll(ExpressionAccessor<Derived, Ptr>::getNodeManager(), numTimes);
		}

		/**
		 * Unrolls this lambda the given amount of time. If the lambda is not recursive, the
		 * result will be the same as this lambda.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param numTimes the number of times this lambda shell be unrolled
		 * @return the resulting, unrolled lambda expression
		 */
		LambdaExprPtr unroll(NodeManager & manager, unsigned numTimes) const {
			return ExpressionAccessor<Derived, Ptr>::getNode().unroll(manager, numTimes);
		}
	IR_NODE_END()

	/**
	 * The entity used to represent lambda expressions.
	 */
	IR_NODE(LambdaExpr, Expression)

		/**
		 * A flag indicating whether this lambda is representing a recursive function.
		 */
		mutable utils::Lazy<bool> recursive;

	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to obtain an instance of a lambda expression
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested lambda expression
		 * @param var the variable identifying the lambda to be represented within the given definition
		 * @param lambda the definition defining the lambda to be represented
		 * @return the requested type instance managed by the given manager
		 */
		static LambdaExprPtr get(NodeManager & manager, const FunctionTypePtr& type, const VariablePtr& var, const LambdaDefinitionPtr& definition) {
			return manager.get(LambdaExpr(type, var, definition));
		}

		/**
		 * This static factory method allows to obtain an instance of a lambda expression
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param var the variable identifying the lambda to be represented within the given definition
		 * @param lambda the definition defining the lambda to be represented
		 * @return the requested type instance managed by the given manager
		 */
		static LambdaExprPtr get(NodeManager & manager, const VariablePtr& var, const LambdaDefinitionPtr& definition) {
			assert_true(dynamic_pointer_cast<FunctionTypePtr>(var->getType())) << "Variable type has to be a function type!";
			return get(manager, static_pointer_cast<FunctionTypePtr>(var->getType()), var, definition);
		}

		/**
		 * Obtains a simple, non-recursive Lambda based on the given definition.
		 *
		 * @param manager the manager maintaining the resulting node instance
		 * @param lambda the lambda to be referenced by the resulting expression
		 * @return the requested lambda expression managed by the given manager
		 */
		static LambdaExprPtr get(NodeManager & manager, const LambdaPtr& lambda);

		/**
		 * Obtains a simple, non-recursive Lambda expression exposing the given type, parameters and body.
		 *
		 * @param manager the manager maintaining the resulting node instance
		 * @param type the type of the resulting lambda expression
		 * @param params the parameters accepted by the resulting lambda
		 * @param body the body of the resulting function
		 * @return the requested lambda expression managed by the given manager
		 */
		static LambdaExprPtr get(NodeManager & manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body);

		/**
		 * Obtains a simple, non-recursive Lambda expression exposing the given type, parameters and body.
		 *
		 * @param manager the manager maintaining the resulting node instance
		 * @param type the type of the resulting lambda expression
		 * @param params the parameters accepted by the resulting lambda
		 * @param body the body of the resulting function
		 * @return the requested lambda expression managed by the given manager
		 */
		static LambdaExprPtr get(NodeManager & manager, const FunctionTypePtr& type, const VariableList& params, const CompoundStmtPtr& body) {
			return get(manager, type, Parameters::get(manager, params), body);
		}

		/**
		 * Determines whether this function is recursively defined or not.
		 */
		bool isRecursiveInternal() const;

		/**
		 * Unrolls this lambda expression for the given number of times.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param numTimes the number of times this lambda expression shell be unrolled
		 * @return the resulting, unrolled lambda expression
		 */
		LambdaExprPtr unroll(NodeManager & manager, unsigned numTimes) const;

	IR_NODE_END()


	// ------------------------------------- Lambda Binding -----------------------------------

	/**
	 * The accessor associated to a lambda binding. A lambda binding is defining a variable
	 * for a lambda within a lambda definition.
	 */
	IR_NODE_ACCESSOR(LambdaBinding, Support, Variable, Lambda)

		/**
		 * Obtains a reference to the variable bound within this binding.
		 */
		IR_NODE_PROPERTY(Variable, Variable, 0);

		/**
		 * Obtains a reference to the lambda bound to the variable referenced by this binding.
		 */
		IR_NODE_PROPERTY(Lambda, Lambda, 1);

	IR_NODE_END()

	/**
	 * The entity used to represent lambda nodes.
	 */
	IR_NODE(LambdaBinding, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << *getVariable() << "=" << *getLambda();
		}

	  public:
		/**
		 * This static factory method allows to obtain an instance of a lambda node
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param var the variable to be bound to the given lambda
		 * @param lambda the lambda to be bound to the given variable
		 * @return the requested type instance managed by the given manager
		 */
		static LambdaBindingPtr get(NodeManager & manager, const VariablePtr& var, const LambdaPtr& lambda) {
			return manager.get(LambdaBinding(var, lambda));
		}

	IR_NODE_END()


	// ------------------------------------- Lambda Definition -----------------------------------

	/**
	 * The accessor associated to a lambda definition
	 */
	IR_LIST_NODE_ACCESSOR(LambdaDefinition, Support, Definitions, LambdaBinding)

		/**
		 * Obtains a pointer to the lambda binding associated to the recursive lambda
		 * variable handed in as an argument or null if there is no such binding.
		 *
		 * @param variable the variable used to define the requested recursive function within
		 * 				   this recursive function definition.
		 * @return a pointer to the associated lambda binding or null if there is no such binding
		 */
		Ptr<const LambdaBinding> getBindingOf(const VariablePtr& variable) const {
			const auto& definitions = getDefinitions();
			auto it = std::find_if(definitions.begin(), definitions.end(), [&](const LambdaBindingPtr& cur) { return *cur->getVariable() == *variable; });
			return (it == definitions.end()) ? Ptr<const LambdaBinding>() : *it;
		}

		/**
		 * Obtains a pointer to the lambda defining the recursive function represented
		 * by the given variable within this recursive definition.
		 *
		 * @param variable the variable used to define the requested recursive function within
		 * 				   this recursive function definition.
		 * @return a pointer to the actual function definition.
		 */
		Ptr<const Lambda> getDefinitionOf(const VariablePtr& variable) const {
			auto binding = getBindingOf(variable);
			return (binding) ? binding->getLambda() : Ptr<const Lambda>();
		}

		/**
		 * Determines whether the definition of the function referenced by the given variable within
		 * this lambda definition is recursive - hence, whether it is invoking itself or one of the other
		 * functions within this block.
		 *
		 * @param variable the variable identifying the function to be checked within this definition
		 * @return true if recursive, false otherwise
		 */
		bool isRecursive(const VariablePtr& variable) const {
			return SupportAccessor<Derived, Ptr>::getNode().isRecursivelyDefined(variable);
		}

		/**
		 * Determines whether any lambda defined within this definition is recursive.
		 *
		 * @return true if there is a recursive lambda, false otherwise
		 */
		bool isRecursive() const {
			return any(getDefinitions(), [&](const LambdaBindingPtr& cur) { return this->isRecursive(cur->getVariable()); });
		}

		/**
		 * Obtains a list of addresses pointing to locations referencing the given recursive variable
		 * utilized for triggering recursive calls.
		 *
		 * @param variable the recursive variable to be searched for
		 * @return the list of recursive references within this definition, rooted by this definition.
		 */
		const vector<VariableAddress>& getRecursiveCallsOf(const VariablePtr& variable) const {
			return SupportAccessor<Derived, Ptr>::getNode().getRecursiveCallsOf(variable);
		}

		/**
		 * Peels this definition the given number of times for the given variable.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param variable the variable defining the definition to be peeled
		 * @param numTimes then number of times the lambda should be peeled
		 * @return the resulting, peeled lambda expression
		 */
		LambdaExprPtr peel(NodeManager & manager, const VariablePtr& variable, unsigned numTimes = 1) const {
			return SupportAccessor<Derived, Ptr>::getNode().peel(manager, variable, numTimes);
		}

		/**
		 * Unrolls this definition the given number of times.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param numTimes then number of times the lambda should be unrolled
		 * @return the resulting, unrolled lambda definition
		 */
		LambdaDefinitionPtr unroll(NodeManager & manager, unsigned numTimes) const {
			return SupportAccessor<Derived, Ptr>::getNode().unroll(manager, numTimes);
		}
	IR_NODE_END()

	/**
	 * The entity used to represent lambda definitions.
	 */
	IR_NODE(LambdaDefinition, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "{" << join(", ", getChildList(), print<deref<NodePtr>>()) << "}";
		}

	  public:
		/**
		 * This static factory method constructing a new lambda definition based
		 * on a given list of lambda bindings.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param bindings the bindings to be included within this definition
		 * @return the requested type instance managed by the given manager
		 */
		static LambdaDefinitionPtr get(NodeManager & manager, const vector<LambdaBindingPtr>& bindings) {
			return manager.get(LambdaDefinition(convertList(bindings)));
		}

		/**
		 * Determines whether the definition of the function referenced by the given variable within
		 * this lambda definition is recursive - hence, whether it is invoking itself or one of the other
		 * functions within this block.
		 *
		 * @param variable the variable identifying the function to be checked within this definition
		 * @return true if recursive, false otherwise
		 */
		bool isRecursivelyDefined(const VariablePtr& variable) const;

		/**
		 * Obtains a list of addresses pointing to locations referencing the given recursive variable
		 * utilized for triggering recursive calls.
		 *
		 * @param variable the recursive variable to be searched for
		 * @return the list of recursive references within this definition, rooted by this definition.
		 */
		const vector<VariableAddress>& getRecursiveCallsOf(const VariablePtr& variable) const;

		/**
		 * Peels this definition for the given variable and number of times.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param variable the variable defining the definition to be peeled
		 * @param numTimes the number of times the selected definition shell be peeled
		 * @return the resulting, peeled lambda expression
		 */
		LambdaExprPtr peel(NodeManager & manager, const VariablePtr& variable, unsigned numTimes = 1) const;

		/**
		 * Unrolls the definitions of all the recursive functions within this definition
		 * for the given number of times.
		 *
		 * @param manager the manager to be used for maintaining the resulting nodes
		 * @param numTimes the number of times to unroll this definition; if it is 0 or 1, it will
		 * 				stick to the current version; if it is > 1, actual unrolling will happen
		 * @return the unrolled definition
		 */
		LambdaDefinitionPtr unroll(NodeManager & manager, unsigned numTimes) const;

	IR_NODE_END()


	// ------------------------------------------ Bind ----------------------------------------

	/**
	 * The accessor associated to a bind node. A bind node is introducing a new function
	 * by accepting a list of parameters and carrying out a pre-determined call.
	 */
	IR_NODE_ACCESSOR(BindExpr, Expression, Type, Parameters, CallExpr)

		/**
		 * Obtains a reference to the parameters accepted by this bind.
		 */
		IR_NODE_PROPERTY(Parameters, Parameters, 1);

		/**
		 * Obtains a reference to the call defining this bind expression.
		 */
		IR_NODE_PROPERTY(CallExpr, Call, 2);

		/**
		 * Obtains a list of all expressions which's resulting value is bound by this expression.
		 */
		vector<Ptr<const Expression>> getBoundExpressions() const {
			vector<Ptr<const Expression>> res;

			const auto& parameters = this->getNode().getParameters()->getElements();
			for_each(getCall()->getArguments(), [&](const Ptr<const Expression>& cur) {
				if(cur->getNodeType() == NT_Variable) {
					const VariablePtr& var = cur.template as<VariablePtr>();
					if(contains(parameters, var)) { return; }
				}
				// add to bind expressions
				res.push_back(cur);
			});

			return res;
		}

		/**
		 * Tests whether the given expression is bound by this bind expression.
		 */
		bool isBoundExpression(const Ptr<const Expression>& expr) const {
			// make sure it is not a parameter
			if(expr->getNodeType() == NT_Variable) {
				const auto& parameters = this->getNode().getParameters()->getElements();
				if(contains(parameters, expr.template as<VariablePtr>())) { return false; }
			}

			// search for bound values
			for(const auto& cur : getCall()) {
				if(cur == expr) { return true; }
			}

			// it is not there
			return false;
		}
	IR_NODE_END()

	/**
	 * A bind expression allows to take a function, fix some of its parameters, reordering others and produce
	 * a new function which may than be called or forwarded to another call. Unlike static lambda structures, this
	 * bindings are evaluated during runtime.
	 */
	IR_NODE(BindExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "bind(" << join(",", getParameters(), print<deref<VariablePtr>>()) << "){" << *getCall() << "}";
		}

	  public:
		/**
		 * A factory method for obtaining a bind expression node instance maintained by the given
		 * node manager.
		 *
		 * @param manager the manager which should be maintaining the new instance
		 * @param type the type of the resulting function (hence, of the resulting expression)
		 * @param parameters the parameters of the resulting function
		 * @param call the call including the bound and unbound parameters
		 * @return the requested bind expression managed by the given manager
		 */
		static BindExprPtr get(NodeManager & manager, const FunctionTypePtr& type, const ParametersPtr& parameters, const CallExprPtr& call) {
			return manager.get(BindExpr(type, parameters, call));
		}

		/**
		 * A factory method for obtaining a bind expression node instance maintained by the given
		 * node manager.
		 *
		 * @param manager the manager which should be maintaining the new instance
		 * @param type the type of the resulting function (hence, of the resulting expression)
		 * @param parameters the parameters of the resulting function
		 * @param call the call including the bound and unbound parameters
		 * @return the requested bind expression managed by the given manager
		 */
		static BindExprPtr get(NodeManager & manager, const FunctionTypePtr& type, const VariableList& parameters, const CallExprPtr& call) {
			return get(manager, type, Parameters::get(manager, parameters), call);
		}

	IR_NODE_END()


	// ------------------------------------- Tuple Expression -----------------------------------

	/**
	 * The accessor associated to a tuple expression.
	 */
	IR_NODE_ACCESSOR(TupleExpr, Expression, TupleType, Expressions)
		/**
		 * Obtains a reference to the list of expressions aggregated to a tuple by
		 * the represented node.
		 */
		IR_NODE_PROPERTY(Expressions, Expressions, 1);
	IR_NODE_END()

	/**
	 * The entity used to represent tuple expressions. A tuple expression is composing
	 * a given list of values into a single tuple containing those values.
	 */
	IR_NODE(TupleExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "tuple(" << join(",", getExpressions()->getExpressions(), print<deref<NodePtr>>()) << ")";
		}

	  public:
		/**
		 * This static factory method constructing a new tuple expression based
		 * on the resulting type and a given list of expressions.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the tuple constructed by the resulting expression
		 * @param expressions the expressions to be packed into a tuple
		 * @return the requested type instance managed by the given manager
		 */
		static TupleExprPtr get(NodeManager & manager, const TupleTypePtr& type, const ExpressionsPtr& expressions) {
			return manager.get(TupleExpr(type, expressions));
		}

	IR_NODE_END()


	// ------------------------------------- Named Values -----------------------------------

	/**
	 * The accessor associated to a named value.
	 */
	IR_NODE_ACCESSOR(NamedValue, Support, StringValue, Expression)
		/**
		 * Obtains a reference to the bound name.
		 */
		IR_NODE_PROPERTY(StringValue, Name, 0);

		/**
		 * Obtains a reference to the bound value.
		 */
		IR_NODE_PROPERTY(Expression, Value, 1);
	IR_NODE_END()

	/**
	 * This type of node is used within the struct expression to represent the
	 * connection between a name (the name of a member) and a value.
	 */
	IR_NODE(NamedValue, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << *getName() << "=" << *getValue();
		}

	  public:
		/**
		 * This static factory method constructing a new named value instance based
		 * on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param name the name to be bound
		 * @param value the value to be bound
		 * @return the requested type instance managed by the given manager
		 */
		static NamedValuePtr get(NodeManager & manager, const StringValuePtr& name, const ExpressionPtr& value) {
			return manager.get(NamedValue(name, value));
		}

	IR_NODE_END()


	// ------------------------------------ A class representing a list of named values------------------------------

	/**
	 * The accessor associated to a list of named values.
	 */
	IR_LIST_NODE_ACCESSOR(NamedValues, Support, NamedValues, NamedValue)
	IR_NODE_END()

	/**
	 * A node type representing a list of named values.
	 */
	IR_NODE(NamedValues, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	  public:
		/**
		 * This static factory method allows to construct a named value list based
		 * on the given list.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param values the values to be included within the requested list
		 * @return the requested type instance managed by the given manager
		 */
		static NamedValuesPtr get(NodeManager & manager, const NamedValueList& values) {
			return manager.get(NamedValues(convertList(values)));
		}
	IR_NODE_END()


	// ------------------------------------- Struct Expression -----------------------------------

	/**
	 * The accessor associated to a struct expression.
	 */
	IR_NODE_ACCESSOR(StructExpr, Expression, Type, NamedValues)

		/**
		 * Obtains a reference to the list of named values aggregated to a struct by
		 * the represented node.
		 */
		IR_NODE_PROPERTY(NamedValues, Members, 1);

	IR_NODE_END()

	/**
	 * The entity used to represent struct expressions. A struct expression is composing
	 * a given list of values into a struct containing those values.
	 */
	IR_NODE(StructExpr, Expression)
	  public:
		/**
		 * A type definition defining the type of entries this composed type is consisting of.
		 */
		typedef NamedValueList Members;

	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			static auto entryPrinter = [](std::ostream& out, const NamedValuePtr& cur) { out << *cur->getName() << "=" << *cur->getValue(); };
			return out << "struct{" << join(", ", getMembers()->getElements(), entryPrinter) << "}";
		}

	  public:
		/**
		 * This static factory method constructing a new struct expression based
		 * on the resulting type and a given list of expressions.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the struct constructed by the resulting expression
		 * @param values the values to be packed into a vector
		 * @return the requested type instance managed by the given manager
		 */
		static StructExprPtr get(NodeManager & manager, const TypePtr& type, const NamedValuesPtr& values) {
			return manager.get(StructExpr(type, values));
		}

	IR_NODE_END()


	// ------------------------------------- Union Expression -----------------------------------

	/**
	 * The accessor associated to a union expression.
	 */
	IR_NODE_ACCESSOR(UnionExpr, Expression, Type, StringValue, Expression)

		/**
		 * Obtains a reference to the name of the field initialized by this union
		 */
		IR_NODE_PROPERTY(StringValue, MemberName, 1);

		/**
		 * Obtains a reference to the value associated to the addressed member of the resulting union.
		 */
		IR_NODE_PROPERTY(Expression, Member, 2);

	IR_NODE_END()

	/**
	 * The entity used to represent union expressions. A union expression is composing
	 * a given list of values into a union containing those values.
	 */
	IR_NODE(UnionExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "union{" << *getMemberName() << "=" << *getMember() << "}";
		}

	  public:
		/**
		 * This static factory method constructing a new union expression based
		 * on the resulting type, a member name and a value.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the union constructed by the resulting expression
		 * @param member the name of the member to be initialized
		 * @param value the value to be used for the initialization
		 * @return the requested type instance managed by the given manager
		 */
		static UnionExprPtr get(NodeManager & manager, const TypePtr& type, const StringValuePtr& member, const ExpressionPtr& value) {
			return manager.get(UnionExpr(type, member, value));
		}

	IR_NODE_END()


	// ------------------------------------- Job Expression -----------------------------------

	/**
	 * The accessor associated to a job expression.
	 */
	IR_NODE_ACCESSOR(JobExpr, Expression, GenericType, Expression, Expression)

		/**
		 * Obtains a reference to the expression determining the range for the number of threads.
		 */
		IR_NODE_PROPERTY(Expression, ThreadNumRange, 1);

		/**
		 * Obtains a reference to the expression evaluated in parallel.
		 */
		IR_NODE_PROPERTY(Expression, Body, 2);
	IR_NODE_END()

	/**
	 * The entity used to represent union expressions. A union expression is composing
	 * a given list of values into a union containing those values.
	 */
	IR_NODE(JobExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method constructing a new union expression based
		 * on the resulting type, a member name and a value.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the resulting job expression
		 * @param range the thread number range to be used for the construction
		 * @param def the body expression to be evaluated
		 * @return the requested type instance managed by the given manager
		 */
		static JobExprPtr get(NodeManager & manager, const GenericTypePtr& type, const ExpressionPtr& range, const ExpressionPtr& def) {
			return manager.get(JobExpr(type, range, def));
		}

	IR_NODE_END()


	// ---------------------------------------- Marker Expression ------------------------------

	/**
	 * The accessor associated to the marker expression.
	 */
	IR_NODE_ACCESSOR(MarkerExpr, Expression, Type, UIntValue, Expression)
		/**
		 * Obtains a reference to the ID of this marker.
		 */
		IR_NODE_PROPERTY(UIntValue, ID, 1);

		/**
		 * Obtains a reference to the covered expression.
		 */
		IR_NODE_PROPERTY(Expression, SubExpression, 2);

		/**
		 * Obtains the ID of this marker as a value.
		 */
		unsigned int getId() const {
			return getID()->getValue();
		}

	IR_NODE_END()

	/**
	 * The entity used to represent a marker expression within the IR.
	 */
	IR_NODE(MarkerExpr, Expression)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "<M id=" << *getID() << ">" << *getSubExpression() << "</M>";
		}

	  public:
		/**
		 * This static factory method allows to obtain a marker expression instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param id the id of the new marker
		 * @param subExpr the expression represented by the marker
		 * @return the requested type instance managed by the given manager
		 */
		static MarkerExprPtr get(NodeManager & manager, const UIntValuePtr& id, const ExpressionPtr& subExpr) {
			return manager.get(MarkerExpr(subExpr->getType(), id, subExpr));
		}

		/**
		 * This static factory method allows to obtain a for statement instance
		 * within the given node manager based on the given parameters. For the id
		 * a new, fresh value will be used.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param subExpr the expression represented by the marker
		 * @return the requested type instance managed by the given manager
		 */
		static MarkerExprPtr get(NodeManager & manager, const ExpressionPtr& subExpr) {
			return get(manager, UIntValue::get(manager, manager.getFreshID()), subExpr);
		}

	IR_NODE_END()


} // end namespace core
} // end namespace insieme

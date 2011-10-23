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
#include "insieme/core/ir_statements.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace core {
namespace new_core {


	// ------------------------------------- Expressions -----------------------------------

	/**
	 * The accessor associated to a list of expressions.
	 */
	IR_LIST_NODE_ACCESSOR(Expressions, Support, Expression)
	};

	/**
	 * A node type representing a list of expressions.
	 */
	IR_NODE(Expressions, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	public:

		/**
		 * This static factory method allows to construct a expression list based
		 * on the given expressions.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param expressions the list of expressions to be included
		 * @return the requested instance managed by the given manager
		 */
		static ExpressionsPtr get(NodeManager& manager, const ExpressionList& expressions) {
			return manager.get(Expressions(convertList(expressions)));
		}
	};




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
		string& getStringValue() const { return getValue()->getValue(); }

		/**
		 * A function extracting the value of this type by interpreting it
		 * according to the given type parameter.
		 */
		template <class T>
	    const T getValueAs() const { return utils::numeric_cast<T>(getStringValue()); }
	};

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
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *getValue();
		}

	public:

		/**
		 * This static factory method allows to obtain a literal instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type of the requested literal
		 * @param value the value of the requested literal
		 * @return the requested type instance managed by the given manager
		 */
		static LiteralPtr get(NodeManager& manager, const TypePtr& type, const StringValuePtr& value) {
			return manager.get(Literal(type, value));
		}

	};




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
		unsigned int getId() const { return getID()->getValue(); }
	};

	/**
	 * The entity used to represent variables within the IR.
	 */
	IR_NODE(Variable, Expression)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "var" << getId();
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
		static VariablePtr get(NodeManager& manager, const TypePtr& type, const UIntValuePtr& id) {
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
		static VariablePtr get(NodeManager& manager, const TypePtr& type, unsigned int id) {
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
		static VariablePtr get(NodeManager& manager, const TypePtr& type) {
			return get(manager, type, manager.getFreshID());
		}

	};




	// ------------------------------------- Call Expr -----------------------------------


	/**
	 * The accessor associated to the call expression.
	 */
	IR_NODE_ACCESSOR(CallExpr, Expression, Type, Expression, Expressions)
		/**
		 * Obtains a reference to the function called by this expression.
		 */
		IR_NODE_PROPERTY(Expression, FunctionExpr, 1);

		/**
		 * Obtains the list of argument expressions passed on to this call expression.
		 */
		IR_NODE_PROPERTY(Expressions, Arguments, 2);
	};

	/**
	 * The entity used to represent function calls within the IR.
	 */
	IR_NODE(CallExpr, Expression)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *getFunctionExpr() << "(" << join(",", getArguments()->getElements(), print<deref<ExpressionPtr>>()) << ")";
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
		static CallExprPtr get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& function, const ExpressionsPtr& arguments) {
			return manager.get(CallExpr(type, function, arguments));
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
		static CallExprPtr get(NodeManager& manager, const TypePtr& type, unsigned int id) {
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
		static CallExprPtr get(NodeManager& manager, const TypePtr& type) {
			return get(manager, type, manager.getFreshID());
		}

	};




	// ------------------------------------- Parameter -----------------------------------

	/**
	 * The accessor associated to the parameter node.
	 */
	IR_LIST_NODE_ACCESSOR(Parameters, Support, Variable)
	};

	/**
	 * A node type representing a list of parameter.
	 */
	IR_NODE(Parameters, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
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
		static ParametersPtr get(NodeManager& manager, const VariableList& parameter) {
			return manager.get(Parameters(convertList(parameter)));
		}
	};



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
		IR_NODE_PROPERTY(Parameters, Parameter, 1);

		/**
		 * Obtains a reference to the body of this lambda.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 2);
	};

	/**
	 * The entity used to represent lambda nodes.
	 */
	IR_NODE(Lambda, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "fun(" << join(", ", getParameter()->getElements(), print<deref<NodePtr>>()) << ") " << *getBody();
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
		static LambdaPtr get(NodeManager& manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body) {
			return manager.get(Lambda(type, params, body));
		}

	};




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

	};

	/**
	 * The entity used to represent lambda nodes.
	 */
	IR_NODE(LambdaBinding, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
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
		static LambdaBindingPtr get(NodeManager& manager, const VariablePtr& var, const LambdaPtr& lambda) {
			return manager.get(LambdaBinding(var, lambda));
		}

	};




	// ------------------------------------- Lambda Definition -----------------------------------

	/**
	 * The accessor associated to a lambda definition
	 */
	IR_LIST_NODE_ACCESSOR(LambdaDefinition, Support, LambdaBinding)

		/**
		 * Retrieves the definitions of the recursive functions represented by this instance.
		 */
		const vector<LambdaBindingPtr>& getDefinitions() const {
			return ListNodeAccessor<Derived,LambdaBinding,Ptr>::getElements();
		}

		/**
		 * Obtains a pointer to the function body defining the recursive function represented
		 * by the given variable within this recursive definition.
		 *
		 * @param variable the variable used to define the requested recursive function within
		 * 				   this recursive function definition.
		 * @return a copy of the internally maintained pointer to the actual function definition.
		 */
		const LambdaPtr& getDefinitionOf(const VariablePtr& variable) const;

		/**
		 * Determines whether the definition of the function referenced by the given variable within
		 * this lambda definition is recursive - hence, whether it is invoking itself or one of the other
		 * functions within this block.
		 *
		 * @param variable the variable identifying the function to be checked within this definition
		 * @return true if recursive, false otherwise
		 */
		bool isRecursive(const VariablePtr& variable) const;

		/**
		 * Unrolls this definition once for the given variable.
		 *
		 * @param manager the manager to be used for maintaining the resulting reference
		 * @param variable the variable defining the definition to be unrolled once
		 * @return the resulting, unrolled lambda expression
		 */
		LambdaExprPtr unrollOnce(NodeManager& manager, const VariablePtr& variable) const;
	};

	/**
	 * The entity used to represent lambda definitions.
	 */
	IR_NODE(LambdaDefinition, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", getChildList(), print<deref<NodePtr>>()) << "}";
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
		static LambdaDefinitionPtr get(NodeManager& manager, const vector<LambdaBindingPtr>& bindings) {
			return manager.get(LambdaDefinition(convertList(bindings)));
		}
	};




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

	};

	/**
	 * The entity used to represent lambda expressions.
	 */
	IR_NODE(LambdaExpr, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "rec " << *getVariable() << "." << *getDefinition();
		}

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
		static LambdaExprPtr get(NodeManager& manager, const FunctionTypePtr& type, const VariablePtr& var, const LambdaDefinitionPtr& definition) {
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
		static LambdaExprPtr get(NodeManager& manager, const VariablePtr& var, const LambdaDefinitionPtr& definition) {
			assert(dynamic_pointer_cast<FunctionTypePtr>(var->getType()) && "Variable type has to be a function type!");
			return get(manager, static_pointer_cast<FunctionTypePtr>(var->getType()), var, definition);
		}

		/**
		 * Obtains a simple, non-recursive Lambda expression exposing the given type, parameters and body.
		 *
		 * @param manager the manager maintaining the resulting node instance
		 * @param type the type of the resulting lambda expression
		 * @param params the parameters accepted by the resulting lambda
		 * @param body the body of the resulting function
		 * @return the requested lambda expression managed by the given manager
		 */
		static LambdaExprPtr get(NodeManager& manager, const FunctionTypePtr& type, const ParametersPtr& params, const CompoundStmtPtr& body) {
			VariablePtr var = Variable::get(manager, type);
			LambdaPtr lambda = Lambda::get(manager, type, params, body);
			LambdaBindingPtr binding = LambdaBinding::get(manager, var, lambda);
			LambdaDefinitionPtr def = LambdaDefinition::get(manager, toVector(binding));
			return manager.get(LambdaExpr(var, def));
		}

	};




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


//		/**
//		 * Obtains a list of all expressions which's resulting value is bound by this expression.
//		 */
//		vector<ExpressionPtr> getBoundExpressions() const;
	};

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
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "bind(" << join(",", getParameters()->getElements(), print<deref<VariablePtr>>()) << "){" << *getCall() << "}";
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
		static BindExprPtr get(NodeManager& manager, const FunctionTypePtr& type, const ParametersPtr& parameters, const CallExprPtr& call) {
			return manager.get(BindExpr(type, parameters, call));
		}

	};

//
//class TupleExpr : public Expression {
//	const vector<ExpressionPtr> expressions;
//
//	TupleExpr(const TupleTypePtr& type, const vector<ExpressionPtr>& expressions);
//	virtual TupleExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	virtual std::ostream& printTo(std::ostream& out) const;
//
//	const vector<ExpressionPtr>& getExpressions() const { return expressions; }
//
//	static TupleExprPtr get(NodeManager& manager, const vector<ExpressionPtr>& expressions);
//	static TupleExprPtr get(NodeManager& manager, const TupleTypePtr& type, const vector<ExpressionPtr>& expressions);
//};
//
//
//class VectorExpr : public Expression {
//	const vector<ExpressionPtr> expressions;
//
//	VectorExpr(const VectorTypePtr& type, const vector<ExpressionPtr>& expressions);
//	virtual VectorExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//
//	bool equalsExpr(const Expression& expr) const;
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	virtual std::ostream& printTo(std::ostream& out) const;
//
//	const vector<ExpressionPtr>& getExpressions() const { return expressions; }
//
//	static VectorExprPtr get(NodeManager& manager, const vector<ExpressionPtr>& expressions);
//	static VectorExprPtr get(NodeManager& manager, const VectorTypePtr& type, const vector<ExpressionPtr>& expressions);
//};
//
//class StructExpr : public Expression {
//public:
//	typedef std::pair<IdentifierPtr, ExpressionPtr> Member;
//	typedef std::vector<Member> Members;
//
//private:
//	const Members members;
//
//	StructExpr(const StructTypePtr& type, const Members& members);
//	virtual StructExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	const Members& getMembers() const{ return members; }
//	virtual std::ostream& printTo(std::ostream& out) const;
//	static StructExprPtr get(NodeManager& manager, const Members& members);
//	static StructExprPtr get(NodeManager& manager, const StructTypePtr& type, const Members& members);
//};
//
//class UnionExpr : public Expression {
//
//	const IdentifierPtr memberName;
//	const ExpressionPtr member;
//
//        UnionExpr(const TypePtr& type, const IdentifierPtr& memberName, const ExpressionPtr& member);
//	virtual UnionExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	const IdentifierPtr& getMemberName() const { return memberName; }
//	const ExpressionPtr& getMember() const { return member; }
//	virtual std::ostream& printTo(std::ostream& out) const;
//        static UnionExprPtr get(NodeManager& manager, const TypePtr& type, const IdentifierPtr& memberName, const ExpressionPtr& member);
//};
//
//
//class JobExpr : public Expression {
//public:
//	typedef std::vector<DeclarationStmtPtr> LocalDecls;
//	typedef std::pair<ExpressionPtr, ExpressionPtr> GuardedStmt;
//	typedef std::vector<GuardedStmt> GuardedStmts;
//
//private:
//
//	const ExpressionPtr threadNumRange;
//	const LocalDecls localDecls;
//	const GuardedStmts guardedStmts;
//	const ExpressionPtr defaultStmt;
//
//	JobExpr(const ExpressionPtr& range, const ExpressionPtr& defaultStmt, const GuardedStmts& guardedStmts, const LocalDecls& localDecs);
//
//	virtual JobExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	virtual std::ostream& printTo(std::ostream& out) const;
//
//	const ExpressionPtr getThreadNumRange() const { return threadNumRange; }
//	const LocalDecls& getLocalDecls() const { return localDecls; }
//	const GuardedStmts& getGuardedStmts() const { return guardedStmts; }
//	const ExpressionPtr& getDefaultStmt() const { return defaultStmt; }
//
//	static JobExprPtr get(NodeManager& manager, const ExpressionPtr& defaultStmt,
//			const GuardedStmts& guardedStmts = GuardedStmts(), const LocalDecls& localDecs = LocalDecls());
//
//	static JobExprPtr get(NodeManager& manager, const ExpressionPtr& threadNumRange, const ExpressionPtr& defaultStmt,
//		const GuardedStmts& guardedStmts = GuardedStmts(), const LocalDecls& localDecs = LocalDecls());
//};
//
//class CastExpr : public Expression {
//	const ExpressionPtr subExpression;
//
//	CastExpr(const TypePtr& type, const ExpressionPtr& subExpression);
//	virtual CastExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	virtual std::ostream& printTo(std::ostream& out) const;
//
//	const ExpressionPtr& getSubExpression() const { return subExpression; }
//
//	static CastExprPtr get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& subExpression);
//};
//
//
//class MemberAccessExpr : public Expression {
//	const ExpressionPtr subExpression;
//	const IdentifierPtr member;
//
//	MemberAccessExpr(const ExpressionPtr& subExpression, const IdentifierPtr& member);
//	virtual MemberAccessExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	virtual std::ostream& printTo(std::ostream& out) const;
//
//	const ExpressionPtr& getSubExpression() const { return subExpression; }
//
//	const IdentifierPtr& getMemberName() const { return member; }
//
//	static MemberAccessExprPtr get(NodeManager& manager, const ExpressionPtr& subExpression, const IdentifierPtr& member);
//};
//
//
//class TupleProjectionExpr : public Expression {
//	const ExpressionPtr subExpression;
//	const unsigned index;
//
//	TupleProjectionExpr( const ExpressionPtr& subExpression, const unsigned index);
//	virtual TupleProjectionExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//	virtual std::ostream& printTo(std::ostream& out) const;
//
//	const ExpressionPtr& getSubExpression() const { return subExpression; }
//
//	const unsigned getIndex() const { return index; }
//
//	static TupleProjectionExprPtr get(NodeManager& manager, const ExpressionPtr& subExpression, const unsigned index);
//};
//
//
//class MarkerExpr : public Expression {
//
//	static unsigned int counter;
//
//	const ExpressionPtr subExpression;
//	const unsigned id;
//
//	MarkerExpr(const ExpressionPtr& subExpression, const unsigned id);
//	virtual MarkerExpr* createCopyUsing(NodeMapping& mapper) const;
//
//protected:
//
//	bool equalsExpr(const Expression& expr) const;
//	virtual NodeListOpt getChildNodes() const;
//
//public:
//
//	virtual std::ostream& printTo(std::ostream& out) const;
//	static MarkerExprPtr get(NodeManager& manager, const ExpressionPtr& subExpression);
//	static MarkerExprPtr get(NodeManager& manager, const ExpressionPtr& subExpression, const unsigned id);
//
//
//	const ExpressionPtr& getSubExpression() const { return subExpression; }
//	const unsigned int getID() const { return id; }
//};


} // end namespace new_core
} // end namespace core
} // end namespace insieme


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
#include "insieme/core/ir_types.h"

namespace insieme {
namespace core {


	// ------------------------------------- Statements ---------------------------------


	// ---------------------------------------- Break Statement ------------------------------

	/**
	 * The accessor associated to the break statement.
	 */
	IR_NODE_ACCESSOR(BreakStmt, Statement)
	IR_NODE_END()

	/**
	 * The entity used to represent break statements within the IR.
	 */
	IR_NODE(BreakStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "break";
		}

	  public:
		/**
		 * This static factory method allows to obtain the break statement instance
		 * within the given node manager.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @return the requested type instance managed by the given manager
		 */
		static BreakStmtPtr get(NodeManager & manager) {
			return manager.get(BreakStmt());
		}
	IR_NODE_END()


	// ---------------------------------------- Continue Statement ------------------------------

	/**
	 * The accessor associated to the continue statement.
	 */
	IR_NODE_ACCESSOR(ContinueStmt, Statement)
	IR_NODE_END()

	/**
	 * The entity used to represent continue statements within the IR.
	 */
	IR_NODE(ContinueStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "continue";
		}

	  public:
		/**
		 * This static factory method allows to obtain the continue statement instance
		 * within the given node manager.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @return the requested type instance managed by the given manager
		 */
		static ContinueStmtPtr get(NodeManager & manager) {
			return manager.get(ContinueStmt());
		}
	IR_NODE_END()

	// ---------------------------------------- Goto Statement ------------------------------

	/**
	 * The accessor associated to the goto statement.
	 */
	IR_NODE_ACCESSOR(GotoStmt, Statement, StringValue)
		/**
		 * Obtains a reference to the string value associated to this goto statement.
		 */
		IR_NODE_PROPERTY(StringValue, Label, 0);
	IR_NODE_END()

	/**
	 * The entity used to represent goto statements within the IR.
	 */
	IR_NODE(GotoStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "goto " << *getLabel();
		}

	  public:
		/**
		 * This static factory method allows to obtain a goto statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param label string value
		 * @return the requested type instance managed by the given manager
		 */
		static GotoStmtPtr get(NodeManager & manager, const StringValuePtr& label) {
			return manager.get(GotoStmt(label));
		}
	IR_NODE_END()


	// ---------------------------------------- Label Statement ------------------------------

	/**
	 * The accessor associated to the label statement.
	 */
	IR_NODE_ACCESSOR(LabelStmt, Statement, StringValue)
		/**
		 * Obtains a reference to the string value associated to this label statement.
		 */
		IR_NODE_PROPERTY(StringValue, Label, 0);
	IR_NODE_END()

	/**
	 * The entity used to represent label statements within the IR.
	 */
	IR_NODE(LabelStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << *getLabel() << ": ";
		}

	  public:
		/**
		 * This static factory method allows to obtain a label statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param label string value
		 * @return the requested type instance managed by the given manager
		 */
		static LabelStmtPtr get(NodeManager & manager, const StringValuePtr& label) {
			return manager.get(LabelStmt(label));
		}
	IR_NODE_END()

	// ------------------------------------- Declaration ---------------------------------

	/**
	 * The accessor associated to the declaration support node.
	 */
	IR_NODE_ACCESSOR(Declaration, Support, Type, Expression)

		/**
		 * Obtains the type being declared.
		 */
		IR_NODE_PROPERTY(Type, Type, 0);

		/**
		 * Obtains the initialization value of the declared type.
		 */
		IR_NODE_PROPERTY(Expression, Initialization, 1);

	IR_NODE_END()

	/**
	 * The support node used to represent declarations within the IR.
	 */
	IR_NODE(Declaration, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to obtain a declaration instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param type the type to be declared
		 * @param initExpression the expression initializing the given variable
		 * @return the requested type instance managed by the given manager
		 */
		static DeclarationPtr get(NodeManager & manager, const TypePtr& type, const ExpressionPtr& initExpression) {
			return manager.get(Declaration(type, initExpression));
		}
	IR_NODE_END()


	// ------------------------------------- Declarations -----------------------------------

	/**
	 * The accessor associated to a list of declarations.
	 */
	IR_LIST_NODE_ACCESSOR(Declarations, Support, Declarations, Declaration)
	IR_NODE_END()

	/**
	 * A node type representing a list of expressions.
	 */
	IR_NODE(Declarations, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	  public:
		/**
		 * This static factory method allows to construct a declaration list based
		 * on the given declarations.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param declarations the list of declarations to be included
		 * @return the requested instance managed by the given manager
		 */
		static DeclarationsPtr get(NodeManager & manager, const DeclarationList& declarations) {
			return manager.get(Declarations(convertList(declarations)));
		}
	IR_NODE_END()

	// ---------------------------------------- Declaration Statement ------------------------------

	/**
	 * The accessor associated to the declaration statement.
	 */
	IR_NODE_ACCESSOR(DeclarationStmt, Statement, Declaration, Variable)

		/**
		 * Obtains the declaration support node used by this declaration statement.
		 */
		IR_NODE_PROPERTY(Declaration, Declaration, 0);

		/**
		 * Obtains the variable being bound in this declaration statement.
		 */
		IR_NODE_PROPERTY(Variable, Variable, 1);

		Ptr<const Expression> getInitialization() const {
			return getDeclaration()->getInitialization();
		}

	IR_NODE_END()

	/**
	 * The entity used to represent declaration statements within the IR.
	 */
	IR_NODE(DeclarationStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	  public:
		/**
		 * This static factory method allows to obtain a declaration statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param variable the variable to be declared
		 * @param initExpression the initial value of the new variable
		 * @return the requested type instance managed by the given manager
		 */
		static DeclarationStmtPtr get(NodeManager& manager, const VariablePtr& variable, const ExpressionPtr& initExpression);

		/**
		 * This static factory method allows to obtain a declaration statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param declaration the underlying declaration node
		 * @param variable the variable to be declared
		 * @return the requested type instance managed by the given manager
		 */
		static DeclarationStmtPtr get(NodeManager& manager, const DeclarationPtr& declaration, const VariablePtr& variable);

	IR_NODE_END()


	// ---------------------------------------- Compound Statement ------------------------------

	/**
	 * The accessor associated to the compound statement.
	 */
	IR_LIST_NODE_ACCESSOR(CompoundStmt, Statement, Statements, Statement)

		/**
		 * Obtains a reference to the statement with the given index.
		 */
		Ptr<const Statement> getStatement(std::size_t index) const {
			return this->getElement(index);
		}
	IR_NODE_END()

	/**
	 * The entity used to represent a compound statements within the IR.
	 */
	IR_NODE(CompoundStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "{" << join("; ", getChildList(), print<deref<NodePtr>>()) << ((!getChildList().empty()) ? ";" : "") << "}";
		}

	  public:
		/**
		 * This static factory method allows to obtain a compound statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param stmts the statements to be combined within the resulting compound statement
		 * @return the requested type instance managed by the given manager
		 */
		static CompoundStmtPtr get(NodeManager & manager, const StatementList& stmts = StatementList()) {
			return manager.get(CompoundStmt(convertList(stmts)));
		}

		/**
		 * This static factory method allows to obtain a compound statement instance containing
		 * a single statement instance being managed by the given node manager.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param stmt the statement to be included within the resulting compound statement
		 * @return the requested type instance managed by the given manager
		 */
		static CompoundStmtPtr get(NodeManager & manager, const StatementPtr& stmt) {
			return get(manager, toVector(stmt));
		}
	IR_NODE_END()


	// ---------------------------------------- If Statement ------------------------------

	/**
	 * The accessor associated to the if statement.
	 */
	IR_NODE_ACCESSOR(IfStmt, Statement, Expression, CompoundStmt, CompoundStmt)
		/**
		 * Obtains a reference to the condition evaluated by this if statement.
		 */
		IR_NODE_PROPERTY(Expression, Condition, 0);

		/**
		 * Obtains a reference to the then-statement evaluated in case the condition evaluates to true.
		 */
		IR_NODE_PROPERTY(CompoundStmt, ThenBody, 1);

		/**
		 * Obtains a reference to the else-statement evaluated in case the condition evaluates to false.
		 */
		IR_NODE_PROPERTY(CompoundStmt, ElseBody, 2);
	IR_NODE_END()

	/**
	 * The entity used to represent an if statement within the IR.
	 */
	IR_NODE(IfStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "if(" << *getCondition() << ") " << *getThenBody() << " else " << *getElseBody();
		}

	  public:
		/**
		 * This static factory method allows to obtain a if statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param condition the condition to be evaluated by this if statement
		 * @param thenStmt the statement evaluated if the condition evaluates to true
		 * @param elseStmt the statement evaluated if the condition evaluates to false
		 * @return the requested type instance managed by the given manager
		 */
		static IfStmtPtr get(NodeManager & manager, const ExpressionPtr& condition, const CompoundStmtPtr& thenStmt, const CompoundStmtPtr& elseStmt) {
			return manager.get(IfStmt(condition, thenStmt, elseStmt));
		}

	IR_NODE_END()


	// ---------------------------------------- While Statement ------------------------------

	/**
	 * The accessor associated to the while statement.
	 */
	IR_NODE_ACCESSOR(WhileStmt, Statement, Expression, CompoundStmt)
		/**
		 * Obtains a reference to the condition of the represented while stmt.
		 */
		IR_NODE_PROPERTY(Expression, Condition, 0);

		/**
		 * Obtains a reference to the body of the represented while stmt.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 1);

	IR_NODE_END()

	/**
	 * The entity used to represent a while statement within the IR.
	 */
	IR_NODE(WhileStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "while(" << *getCondition() << ") " << *getBody();
		}

	  public:
		/**
		 * This static factory method allows to obtain a while statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param condition the condition to be checked by the while statement
		 * @param body the body of the while statement
		 * @return the requested type instance managed by the given manager
		 */
		static WhileStmtPtr get(NodeManager & manager, const ExpressionPtr& condition, const CompoundStmtPtr& body) {
			return manager.get(WhileStmt(condition, body));
		}

	IR_NODE_END()


	// ---------------------------------------- For Statement ------------------------------

	/**
	 * The accessor associated to the for statement.
	 *
	 * Note that the IR for loop always assumes the operator "<" for the end comparison.
	 * The IR ForStmt
	 *   for(int<4> v0 = 0 .. 5 : 1) {
	 *     var ref<int<4>> i = v0;
	 *   }
	 * Represents the C for loop
	 *   for(int i = 0; i < 5; ++i) { }
	 *
	 * If the original for loop used > (or >= or <=) as the operator, the start, end, step and variable assignment in the body need to be adjusted accordingly.
	 */
	IR_NODE_ACCESSOR(ForStmt, Statement, DeclarationStmt, Expression, Expression, CompoundStmt)

		/**
		 * Obtains a reference to the variable declaration within this for stmt.
		 */
		IR_NODE_PROPERTY(DeclarationStmt, Declaration, 0);

		/**
		 * Obtains a reference to the expression representing the end value of the iterator variable (exclusive).
		 */
		IR_NODE_PROPERTY(Expression, End, 1);

		/**
		 * Obtains a reference to the expression representing the step-size value of the iterator variable.
		 */
		IR_NODE_PROPERTY(Expression, Step, 2);

		/**
		 * Obtains a reference to the body of the loop.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 3);

		/**
		 * Obtains a reference to the variable used as an iterator for this loop.
		 */
		Ptr<const Variable> getIterator() const {
			return getDeclaration()->getVariable();
		}

		/**
		 * Obtains a reference to the expression representing the start value of the iterator variable (inclusive)
		 */
		Ptr<const Expression> getStart() const {
			return getDeclaration()->getInitialization();
		}

	IR_NODE_END()

	/**
	 * The entity used to represent a for statement within the IR.
	 */
	IR_NODE(ForStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to obtain a for statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param varDecl the declaration of the iterator variable to be included in the resulting for
		 * @param end the end value of the for loop
		 * @param step the step size value of the for loop
		 * @param body the body of the for loop
		 * @return the requested type instance managed by the given manager
		 */
		static ForStmtPtr get(NodeManager & manager, const DeclarationStmtPtr& varDecl, const ExpressionPtr& end, const ExpressionPtr& step,
							  const CompoundStmtPtr& body) {
			return manager.get(ForStmt(varDecl, end, step, body));
		}

		/**
		 * This static factory method allows to obtain a for statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param iterator the iterator to be used for the for loop
		 * @param start the start value of the for loop
		 * @param end the end value of the for loop
		 * @param step the step size value of the for loop
		 * @param body the body of the for loop
		 * @return the requested type instance managed by the given manager
		 */
		static ForStmtPtr get(NodeManager & manager, const VariablePtr& iterator, const ExpressionPtr& start, const ExpressionPtr& end, const ExpressionPtr& step,
							  const CompoundStmtPtr& body);

	IR_NODE_END()


	// ---------------------------------------- Switch Statement ------------------------------

	/**
	 * The accessor associated to a switch case. A switch case is one entry within a switch
	 * statement.
	 */
	IR_NODE_ACCESSOR(SwitchCase, Support, Literal, CompoundStmt)
		/**
		 * Obtains the literal forming the guard of this switch-case.
		 */
		IR_NODE_PROPERTY(Literal, Guard, 0);

		/**
		 * Obtains a reference to the type being bound to the referenced variable.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 1);
	IR_NODE_END()

	/**
	 * A node type used to represent cases within a switch expression.
	 */
	IR_NODE(SwitchCase, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to construct a new switch case.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param guard the guard determining whether this case should be executed
		 * @param body the body to be evaluated
		 * @return the requested type instance managed by the given manager
		 */
		static SwitchCasePtr get(NodeManager & manager, const LiteralPtr& guard, const CompoundStmtPtr& body) {
			return manager.get(SwitchCase(guard, body));
		}

	IR_NODE_END()

	/**
	 * The accessor associated to a list of switch cases.
	 */
	IR_LIST_NODE_ACCESSOR(SwitchCases, Support, Cases, SwitchCase)
	IR_NODE_END()

	/**
	 * A node type used to represent lists of cases within a switch expressions.
	 */
	IR_NODE(SwitchCases, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << join(" | ", getChildList(), print<deref<NodePtr>>());
		}

	  public:
		/**
		 * This static factory method allows to construct a new list of switch cases.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param cases the cases to be contained within the resulting instance
		 * @return the requested type instance managed by the given manager
		 */
		static SwitchCasesPtr get(NodeManager & manager, const vector<SwitchCasePtr>& cases) {
			return manager.get(SwitchCases(convertList(cases)));
		}

	IR_NODE_END()


	/**
	 * The accessor associated to the switch statement.
	 */
	IR_NODE_ACCESSOR(SwitchStmt, Statement, Expression, SwitchCases, CompoundStmt)
		/**
		 * Obtains a reference the expression evaluated for determining the guard.
		 */
		IR_NODE_PROPERTY(Expression, SwitchExpr, 0);

		/**
		 * Obtains a reference to the list of cases within this switch expression.
		 */
		IR_NODE_PROPERTY(SwitchCases, Cases, 1);

		/**
		 * Obtains a reference to the default body evaluated in case non of the cases are valid.
		 */
		IR_NODE_PROPERTY(CompoundStmt, DefaultCase, 2);

	IR_NODE_END()

	/**
	 * The entity used to represent switch statements within the IR.
	 */
	IR_NODE(SwitchStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to obtain a switch statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param expr the expression evaluated to determine which case to take
		 * @param cases the cases to select from
		 * @param def the default case to be used if no case is matching
		 * @return the requested type instance managed by the given manager
		 */
		static SwitchStmtPtr get(NodeManager & manager, const ExpressionPtr& expr, const SwitchCasesPtr& cases, const CompoundStmtPtr& def) {
			return manager.get(SwitchStmt(expr, cases, def));
		}

	IR_NODE_END()


	// ---------------------------------------- Marker Statement ------------------------------

	/**
	 * The accessor associated to the marker statement.
	 */
	IR_NODE_ACCESSOR(MarkerStmt, Statement, UIntValue, Statement)
		/**
		 * Obtains a reference to the ID of this marker.
		 */
		IR_NODE_PROPERTY(UIntValue, ID, 0);

		/**
		 * Obtains a reference to the covered statement.
		 */
		IR_NODE_PROPERTY(Statement, SubStatement, 1);

		/**
		 * Obtains the ID of this marker as a value.
		 */
		unsigned int getId() const {
			return getID()->getValue();
		}

	IR_NODE_END()

	/**
	 * The entity used to represent a marker statement within the IR.
	 */
	IR_NODE(MarkerStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "<M id=" << *getID() << ">" << *getSubStatement() << "</M>";
		}

	  public:
		/**
		 * This static factory method allows to obtain a marker statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param id the id of the new marker
		 * @param subStmt the statement represented by the marker
		 * @return the requested type instance managed by the given manager
		 */
		static MarkerStmtPtr get(NodeManager & manager, const UIntValuePtr& id, const StatementPtr& subStmt) {
			return manager.get(MarkerStmt(id, subStmt));
		}

		/**
		 * This static factory method allows to obtain a for statement instance
		 * within the given node manager based on the given parameters. For the id
		 * a new, fresh value will be used.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param subStmt the statement represented by the marker
		 * @return the requested type instance managed by the given manager
		 */
		static MarkerStmtPtr get(NodeManager & manager, const StatementPtr& subStmt) {
			return get(manager, UIntValue::get(manager, manager.getFreshID()), subStmt);
		}

	IR_NODE_END()


	// ---------------------------------------- Throw Statement ------------------------------

	/**
	 * The accessor associated to the throw statement.
	 */
	IR_NODE_ACCESSOR(ThrowStmt, Statement, Expression)
		/**
		 * Obtains a reference to the throw-expression associated to this return statement.
		 */
		IR_NODE_PROPERTY(Expression, ThrowExpr, 0);
	IR_NODE_END()

	/**
	 * The entity used to represent throw statements within the IR.
	 */
	IR_NODE(ThrowStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			return out << "throw " << *getThrowExpr();
		}

	  public:
		/**
		 * This static factory method allows to obtain a throw statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param expression the expression to be thrown by the resulting statement
		 * @return the requested type instance managed by the given manager
		 */
		static ThrowStmtPtr get(NodeManager & manager, const ExpressionPtr& expression) {
			return manager.get(ThrowStmt(expression));
		}

	IR_NODE_END()


	// ---------------------------------------- Try-Catch Statement ------------------------------

	/**
	 * The accessor associated to a try-catch clause.
	 */
	IR_NODE_ACCESSOR(CatchClause, Support, Variable, CompoundStmt)
		/**
		 * Obtains the variable capturing a potential exception.
		 */
		IR_NODE_PROPERTY(Variable, Variable, 0);

		/**
		 * Obtains a reference to the body of this catch clause.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 1);
	IR_NODE_END()

	/**
	 * A node type used to represent cases within a switch expression.
	 */
	IR_NODE(CatchClause, Support)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to construct a new catch clause.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param var the variable to be utilized for catching an exception
		 * @param body the body of the resulting catch clause
		 * @return the requested type instance managed by the given manager
		 */
		static CatchClausePtr get(NodeManager & manager, const VariablePtr& var, const CompoundStmtPtr& body) {
			return manager.get(CatchClause(var, body));
		}

	IR_NODE_END()

	/**
	 * The accessor associated to a try-catch statement.
	 */
	IR_LIST_NODE_ACCESSOR(TryCatchStmt, Statement, Clauses, CompoundStmt, CatchClause)
		/**
		 * Obtains a reference the compound statement forming the body of this statement.
		 */
		IR_NODE_PROPERTY(CompoundStmt, Body, 0);
	IR_NODE_END()

	/**
	 * The entity used to represent switch statements within the IR.
	 */
	IR_NODE(TryCatchStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const;

	  public:
		/**
		 * This static factory method allows to obtain a try-catch statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param body the body to be covered by the try-catch block
		 * @param catchClauses the list of clauses to be present for handling exceptions
		 * @return the requested statement instance managed by the given manager
		 */
		static TryCatchStmtPtr get(NodeManager & manager, const CompoundStmtPtr& body, const vector<CatchClausePtr>& catchClauses) {
			assert_false(catchClauses.empty());
			NodeList children;
			children.push_back(body);
			children.insert(children.end(), catchClauses.begin(), catchClauses.end());
			return manager.get(TryCatchStmt(children));
		}

	IR_NODE_END()


	// ---------------------------------------- Return Statement ------------------------------

	/**
	 * The accessor associated to the return statement.
	 */
	IR_NODE_ACCESSOR(ReturnStmt, Statement, Declaration)
		/**
		 * Obtains the declaration support node associated to this return statement.
		 */
		IR_NODE_PROPERTY(Declaration, ReturnDeclaration, 0);

		/**
		 * Obtains the expression associated to this return statement.
		 */
		Ptr<const Expression> getReturnExpr() const {
			return getReturnDeclaration()->getInitialization();
		}

		/**
		 * Obtains the return type associated to this return statement.
		 */
		Ptr<const Type> getReturnType() const {
			return getReturnDeclaration()->getType();
		}

	IR_NODE_END()

	/**
	 * The entity used to represent return statements within the IR.
	 */
	IR_NODE(ReturnStmt, Statement)
	  protected:
		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream & out) const {
			// variable number gives no additional information
			return out << "return " << *getReturnExpr();
		}

	  public:

		/**
		 * This static factory method allows to obtain a return statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param declaration the declaration of the return value
		 * @return the requested type instance managed by the given manager
		 */
		static ReturnStmtPtr get(NodeManager & manager, const DeclarationPtr& declaration);

		/**
		 * This static factory method allows to obtain a return statement instance
		 * within the given node manager based on the given parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param returnExpr the expression forming the return value
		 * @return the requested type instance managed by the given manager
		 */
		static ReturnStmtPtr get(NodeManager & manager, const ExpressionPtr& returnExpr);

	IR_NODE_END()

} // end namespace core
} // end namespace insieme

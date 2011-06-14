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

#include <string>
#include <utility>
#include <vector>
#include <cassert>
#include <map>
#include <memory>

#include <boost/utility.hpp>

#include "insieme/utils/pointer.h"

namespace insieme {
namespace backend {
namespace c_ast {

	using std::string;
	using std::vector;
	using std::pair;

	/**
	 * Adds forward declarations for all C AST node types. Further, for each
	 * type a type definition for a corresponding annotated pointer is added.
	 */
	#define NODE(NAME) \
	class NAME; \
	typedef Ptr<NAME> NAME ## Ptr; \
	// take all nodes from within the definition file
	#include "insieme/backend/c_ast/c_nodes.def"
	#undef NODE

	#define CONCRETE(name) NT_ ## name,
	enum NodeType {
		// the necessary information is obtained from the node-definition file
		#include "insieme/backend/c_ast/c_nodes.def"
	};
	#undef CONCRETE



	// -- Utilities ------------------------------

	class CNodeManager;
	typedef std::shared_ptr<CNodeManager> SharedCNodeManager;

	class CNodeManager : private boost::noncopyable {

		vector<NodePtr> nodes;
		std::map<string, IdentifierPtr> identMap;

	public:

		~CNodeManager();

		IdentifierPtr create(const string& name);

		template<typename T, typename ... Args>
		Ptr<T> create(Args ... args) {
			T* res = new T(args...);
			res->setManager(this);
			nodes.push_back(res);
			return res;
		}

		static SharedCNodeManager createShared() {
			return std::make_shared<CNodeManager>();
		}

	};

	// -- Basic ----------------------------------

	class Node {
		const NodeType type;
		CNodeManager* manager;

	public:
		Node(NodeType type) : type(type) {}
		virtual ~Node() {};

		NodeType getType() const { return type; }

		void setManager(CNodeManager* mgr) { manager = mgr; }
		CNodeManager* getManager() const {
			assert(manager && "Manager uninitialized!");
			return manager;
		}
	};

	struct Identifier : public Node {
		const string name;
		Identifier(const string& name) : Node(NT_Identifier), name(name) {}

		bool operator==(Identifier& other) const { return name == other.name; }
		bool operator<(Identifier& other) const { return name < other.name; }
	};

	struct Comment : public Node {
		const string comment;
		Comment(const string& comment) : Node(NT_Comment), comment(comment) {}
	};

	struct OpaqueCode : public Node {
		string code;
		OpaqueCode(const string& code) : Node(NT_OpaqueCode), code(code) {};
		OpaqueCode(const string&& code) : Node(NT_OpaqueCode), code(code) {};
	};

	// -- Types ----------------------------------

	struct Type : public Node {
		Type(NodeType type) : Node(type) {}
	};

	struct PrimitiveType : public Type {
		IdentifierPtr name;
		PrimitiveType(IdentifierPtr name) : Type(NT_PrimitiveType), name(name) {}
	};

	struct PointerType : public Type {
		TypePtr elementType;
		PointerType(TypePtr elementType) : Type(NT_PointerType), elementType(elementType) {}
	};

	struct VectorType : public Type {
		TypePtr elementType;
		ExpressionPtr size;
		VectorType(TypePtr elementType, ExpressionPtr size) : Type(NT_VectorType), elementType(elementType), size(size) {}
	};

	struct NamedCompositeType : public Type {
		IdentifierPtr name;
		vector<pair<IdentifierPtr, TypePtr>> elements;
		NamedCompositeType(NodeType type, const IdentifierPtr name)
			: Type(type), name(name) {}
	};

	struct StructType : public NamedCompositeType {
		StructType(IdentifierPtr name) : NamedCompositeType(NT_StructType, name) {}
	};

	struct UnionType : public NamedCompositeType {
		UnionType(IdentifierPtr name) : NamedCompositeType(NT_UnionType, name) {}
	};

	// -- Statements -----------------------------

	struct Statement : public Node {
		Statement(NodeType type) : Node(type) {}
	};


	struct VarDecl : public Statement {
		VariablePtr var;
		ExpressionPtr init;
		VarDecl(VariablePtr var, ExpressionPtr init)
			: Statement(NT_VarDecl), var(var), init(init) {};
	};

	struct Compound : public Statement {
		const vector<StatementPtr> statements;
		Compound() : Statement(NT_Compound) {};
	};

	struct If : public Statement {
		ExpressionPtr condition;
		StatementPtr thenStmt;
		StatementPtr elseStmt;
		If() : Statement(NT_Compound), condition(0), thenStmt(0), elseStmt(0) {};
		If(ExpressionPtr condition, StatementPtr thenStmt, StatementPtr elseStmt)
			: Statement(NT_Compound), condition(condition), thenStmt(thenStmt), elseStmt(elseStmt) {};
	};

	struct Switch : public Statement {
		ExpressionPtr value;
		vector<pair<ExpressionPtr, StatementPtr>> cases;
		ExpressionPtr defaultBranch;
		Switch(ExpressionPtr value) : Statement(NT_Switch), value(value) {}
	};

	struct For : public Statement {
		ExpressionPtr init;
		ExpressionPtr check;
		ExpressionPtr step;
		StatementPtr body;
		For(ExpressionPtr init, ExpressionPtr check, ExpressionPtr step, StatementPtr body)
			: Statement(NT_For), init(init), check(check), step(step), body(body) {}
	};

	struct While : public Statement {
		ExpressionPtr condition;
		StatementPtr body;
		While(ExpressionPtr condition, StatementPtr body)
			: Statement(NT_While), condition(condition), body(body) {}
	};

	struct Continue : public Statement {
		Continue() : Statement(NT_Continue) {}
	};

	struct Break : public Statement {
		Break() : Statement(NT_Break) {}
	};

	struct Return : public Statement {
		ExpressionPtr value;
		Return(ExpressionPtr value) : Statement(NT_Return), value(value) {}
	};

	// -- Expressions -----------------------------

	struct Expression : public Statement {
		Expression(NodeType type) : Statement(type) {}
	};

	struct Literal : public Expression {
		string value;
		Literal(const string& value) : Expression(NT_Literal), value(value) {}
	};

	struct Variable : public Expression {
		TypePtr type;
		IdentifierPtr name;
		Variable(TypePtr type, IdentifierPtr name) : Expression(NT_Variable), type(type), name(name) {}
	};

	struct Initializer : public Expression {
		TypePtr type;
		vector<ExpressionPtr> values;
		Initializer(TypePtr type) : Expression(NT_Initializer), type(type) {};
	};

	struct UnaryOperation : public Expression {

		enum UnaryOp {
			UnaryPlus,
			UnaryMinus,
			PrefixInc,
			PrefixDec,
			PostFixInc,
			PostFixDec,
			LogicNot,
			BitwiseNot,
			Indirection,
			Reference,
			SizeOf,
		};

		UnaryOp operation;
		NodePtr operand;

		UnaryOperation(UnaryOp operation, NodePtr operand)
			: Expression(NT_UnaryOperation), operation(operation), operand(operand) {}

	};

	struct BinaryOperation : public Expression {

		enum BinaryOp {
			Assignment,
			Additon,
			Subtraction,
			Multiplication,
			Division,
			Modulo,
			Equal,
			NotEqual,
			GreaterThan,
			LessThan,
			GreaterOrEqual,
			LessOrEqual,
			LogicAnd,
			LogicOr,
			BitwiseAnd,
			BitwiseOr,
			BitwiseXOr,
			BitwiseLeftShift,
			BitwiseRightShift,
			AdditionAssign,
			SubtractionAssign,
			MultiplicationAssign,
			DivisionAssign,
			ModuloAssign,
			BitwiseAndAssign,
			BitwiseOrAssign,
			BitwiseXOrAssign,
			BitwiseLeftShiftAssign,
			BitwiseRightShiftAssign,
			MemberAccess,
			Subscript,
			Cast,
		};

		BinaryOp operation;
		NodePtr operandA;
		NodePtr operandB;

		BinaryOperation(BinaryOp operation, NodePtr operandA, NodePtr operandB)
			: Expression(NT_BinaryOperation), operation(operation), operandA(operandA), operandB(operandB) {}

	};

	struct TernaryOperation : public Expression {
		enum TernaryOp {
			TernaryCondition
		};

		TernaryOp operation;
		NodePtr operandA;
		NodePtr operandB;
		NodePtr operandC;

		TernaryOperation(TernaryOp operation, NodePtr operandA, NodePtr operandB, NodePtr operandC)
			: Expression(NT_TernaryOperation), operation(operation),
			  operandA(operandA), operandB(operandB), operandC(operandC) {}
	};

	struct Call : public Expression {
		IdentifierPtr function;
		vector<ExpressionPtr> arguments;

		Call(IdentifierPtr function) : Expression(NT_Call), function(function) {}
	};

	struct Parentheses : public Expression {
		ExpressionPtr expression;
		Parentheses(ExpressionPtr expression) : Expression(NT_Parentheses), expression(expression) {}
	};

	// -- Declarations ----------------------------

	struct Declaration : public Node {
		Declaration(NodeType type) : Node(type) {};
	};

	struct TypePrototype : public Declaration {
		TypePtr type;
		TypePrototype(TypePtr type) : Declaration(NT_TypePrototype), type(type) {}
	};

	struct TypeDef : public Declaration {
		TypePtr type;
		IdentifierPtr name;
		TypeDef(TypePtr type, IdentifierPtr name)
			: Declaration(NT_TypeDef), type(type), name(name) {}
	};

	struct Function : public Declaration {
		TypePtr returnType;
		IdentifierPtr name;
		vector<VariablePtr> parameter;
		StatementPtr body;
		Function(TypePtr returnType, IdentifierPtr name, StatementPtr body)
					: Declaration(NT_Function), returnType(returnType), name(name), body(body) {};
		Function(TypePtr returnType, IdentifierPtr name, const vector<VariablePtr> params, StatementPtr body)
			: Declaration(NT_Function), returnType(returnType), name(name), parameter(params), body(body) {};
	};

	struct FunctionPrototype : public Declaration {
		FunctionPtr function;
		FunctionPrototype(FunctionPtr function) : Declaration(NT_FunctionPrototype), function(function) {}
	};


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

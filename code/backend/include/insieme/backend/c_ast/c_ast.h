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
#include "insieme/utils/id_generator.h"
#include "insieme/utils/container_utils.h"

#include "insieme/backend/c_ast/forward_decls.h"

namespace insieme {
namespace backend {
namespace c_ast {

	using std::string;
	using std::vector;
	using std::pair;


	// -- Basic ----------------------------------

	class Node {
		const NodeType type;
		CNodeManager* manager;

		/**
		 * The type used for representing equality IDs.
		 */
		typedef uint64_t EqualityID;

		/**
		 * A static generator for generating equality class IDs
		 */
		static utils::SimpleIDGenerator<EqualityID> equalityClassIDGenerator;

		/**
		 * The ID of the equality class of this node. This ID is used to significantly
		 * speed up the equality check.
		 */
		mutable EqualityID equalityID;


	public:
		Node(NodeType type) : type(type), equalityID(0) {}
		virtual ~Node() {};

		NodeType getType() const { return type; } // TODO: remove this, expressions have types!
		NodeType getNodeType() const { return type; }

		void setManager(CNodeManager* mgr) { manager = mgr; }
		CNodeManager* getManager() const {
			assert(manager && "Manager uninitialized!");
			return manager;
		}

		bool operator==(const Node& other) const;

	private:

		virtual bool equals(const Node& other) const=0;
	};

	struct Identifier : public Node {
		string name;
		Identifier(const string& name) : Node(NT_Identifier), name(name) {}

		bool operator==(Identifier& other) const { return name == other.name; }
		bool operator<(Identifier& other) const { return name < other.name; }

		virtual bool equals(const Node& other) const;
	};

	struct Comment : public Node {
		const string comment;
		Comment(const string& comment) : Node(NT_Comment), comment(comment) {}
		virtual bool equals(const Node& other) const;
	};

	struct OpaqueCode : public Node {
		string code;
		OpaqueCode(const string& code) : Node(NT_OpaqueCode), code(code) {};
		OpaqueCode(const string&& code) : Node(NT_OpaqueCode), code(code) {};
		virtual bool equals(const Node& other) const;
	};

	// -- Types ----------------------------------

	struct Type : public Node {
		Type(NodeType type) : Node(type) {}
	};

	struct PrimitiveType : public Type {
		enum CType {
			Void, Bool, Char, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Float, Double
		};
		const CType type;
		PrimitiveType(CType type) : Type(NT_PrimitiveType), type(type) {}
		virtual bool equals(const Node& other) const;
	};

	struct ModifiedType : public Type {
		enum Modifier {
			VOLATILE=1, CONST=2
		};
		TypePtr type;
		unsigned mods;
		ModifiedType(const TypePtr& type, unsigned mods) : Type(NT_ModifiedType), type(type), mods(mods) {}
		virtual bool equals(const Node& other) const;
		bool hasMod(Modifier mod) const { return mods & mod; }
	};

	struct NamedType : public Type {
		IdentifierPtr name;
		vector<NodePtr> parameters;
		NamedType(IdentifierPtr name) : Type(NT_NamedType), name(name) {}
		virtual bool equals(const Node& other) const;
	};

	struct PointerType : public Type {
		TypePtr elementType;
		PointerType(TypePtr elementType) : Type(NT_PointerType), elementType(elementType) {}
		virtual bool equals(const Node& other) const;
	};

	struct ReferenceType : public Type {
		bool isConst;
		TypePtr elementType;
		ReferenceType(bool isConst, TypePtr elementType) : Type(NT_ReferenceType), isConst(isConst), elementType(elementType) {}
		virtual bool equals(const Node& other) const;
	};

	struct VectorType : public Type {
		TypePtr elementType;
		ExpressionPtr size;		// could be null to represent T[]; - no size given
		VectorType(TypePtr elementType, ExpressionPtr size = ExpressionPtr()) : Type(NT_VectorType), elementType(elementType), size(size) {}
		virtual bool equals(const Node& node) const;
	};

	struct NamedCompositeType : public Type {
		IdentifierPtr name;
		vector<VariablePtr> elements;
		NamedCompositeType(NodeType type, const IdentifierPtr name)
			: Type(type), name(name) {}
		virtual bool equals(const Node& node) const;
	};

	struct Parent : public Node {
		bool isVirtual;
		TypePtr parent;
		Parent(bool isVirtual, const TypePtr& parent)
			: Node(NT_Parent), isVirtual(isVirtual), parent(parent) {}
		virtual bool equals(const Node& node) const;
	};

	struct StructType : public NamedCompositeType {
		vector<ParentPtr> parents;
		vector<ConstructorPrototypePtr> ctors;
		DestructorPrototypePtr dtor;
		vector<MemberFunctionPrototypePtr> members;
		StructType(IdentifierPtr name) : NamedCompositeType(NT_StructType, name) {}
		virtual bool equals(const Node& node) const;
	};

	struct UnionType : public NamedCompositeType {
		UnionType(IdentifierPtr name) : NamedCompositeType(NT_UnionType, name) {}
	};

	struct FunctionType : public Type {
		TypePtr returnType;
		vector<TypePtr> parameterTypes;
		FunctionType(const TypePtr& returnType)
					: Type(NT_FunctionType), returnType(returnType), parameterTypes() {}
		FunctionType(const TypePtr& returnType, const vector<TypePtr>& parameter)
			: Type(NT_FunctionType), returnType(returnType), parameterTypes(parameter) {}
		virtual bool equals(const Node& node) const;
	};

	struct VarArgsType : public Type {
		VarArgsType() : Type(NT_VarArgsType) {}
		virtual bool equals(const Node& node) const;
	};

	struct AttributedType : public Type {
		string attribute;
		TypePtr type;
		AttributedType(const string& attribute, const TypePtr& type)
			: Type(NT_AttributedType), attribute(attribute), type(type) {}
		virtual bool equals(const Node& node) const;
	};

	// -- Statements -----------------------------

	struct Statement : public Node {
		Statement(NodeType type) : Node(type) {}
	};


	struct VarDecl : public Statement {
		const vector<pair<VariablePtr,ExpressionPtr>> varInit;
		VarDecl(VariablePtr var)
			: Statement(NT_VarDecl), varInit(toVector(std::make_pair(var, ExpressionPtr()))) {};
		VarDecl(VariablePtr var, ExpressionPtr init)
			: Statement(NT_VarDecl), varInit(toVector(std::make_pair(var, init))) {};
		VarDecl(const vector<pair<VariablePtr,ExpressionPtr>>& initList);
		virtual bool equals(const Node& node) const;
	};

	struct Compound : public Statement {
		vector<NodePtr> statements;
		Compound() : Statement(NT_Compound) {};
		Compound(const vector<NodePtr>& stmts) : Statement(NT_Compound), statements(stmts) {};

		template<typename ... E>
		Compound(E ... stmts) : Statement(NT_Compound), statements(toVector<NodePtr>(stmts...)) {};
		virtual bool equals(const Node& node) const;
	};

	struct If : public Statement {
		ExpressionPtr condition;
		StatementPtr thenStmt;
		StatementPtr elseStmt;
		If() : Statement(NT_If), condition(0), thenStmt(0), elseStmt(0) {};
		If(ExpressionPtr condition, StatementPtr thenStmt, StatementPtr elseStmt)
			: Statement(NT_If), condition(condition), thenStmt(thenStmt), elseStmt(elseStmt) {};
		virtual bool equals(const Node& node) const;
	};

	struct Switch : public Statement {
		ExpressionPtr value;
		vector<pair<ExpressionPtr, StatementPtr>> cases;
		StatementPtr defaultBranch;
		Switch(ExpressionPtr value) : Statement(NT_Switch), value(value) {}
		virtual bool equals(const Node& node) const;
	};

	struct For : public Statement {
		StatementPtr init;
		StatementPtr check;
		StatementPtr step;
		StatementPtr body;
		For(StatementPtr init, StatementPtr check, StatementPtr step, StatementPtr body)
			: Statement(NT_For), init(init), check(check), step(step), body(body) {}
		virtual bool equals(const Node& node) const;
	};

	struct While : public Statement {
		ExpressionPtr condition;
		StatementPtr body;
		While(ExpressionPtr condition, StatementPtr body)
			: Statement(NT_While), condition(condition), body(body) {}
		virtual bool equals(const Node& node) const;
	};

	struct TryCatch : public Statement {
		// a catch clause - if the variable is null it will create a catch-all
		struct Clause {
			VariablePtr var;
			StatementPtr body;
			Clause(VariablePtr var, StatementPtr body) : var(var), body(body) {}
			Clause(StatementPtr body) : body(body) {} 			// the variant for the capture-all clause
			bool operator==(const Clause& other) const;
		};
		StatementPtr body;
		vector<Clause> clauses;
		TryCatch(StatementPtr body, const vector<Clause>& clauses)
			: Statement(NT_TryCatch), body(body), clauses(clauses) {};
		virtual bool equals(const Node& node) const;
	};

	struct Continue : public Statement {
		Continue() : Statement(NT_Continue) {}
		virtual bool equals(const Node& node) const { return true; }
	};

	struct Break : public Statement {
		Break() : Statement(NT_Break) {}
		virtual bool equals(const Node& node) const { return true; }
	};

	struct Return : public Statement {
		ExpressionPtr value;
		Return(ExpressionPtr value = ExpressionPtr()) : Statement(NT_Return), value(value) {}
		virtual bool equals(const Node& node) const;
	};

	struct Throw : public Statement {
		ExpressionPtr value;
		Throw(ExpressionPtr value) : Statement(NT_Throw), value(value) {}
		virtual bool equals(const Node& node) const;
	};

	// -- Expressions -----------------------------

	struct Expression : public Statement {
		Expression(NodeType type) : Statement(type) {}
	};

	struct Literal : public Expression {
		string value;
		Literal(const string& value) : Expression(NT_Literal), value(value) {}
		virtual bool equals(const Node& node) const;
	};

	struct Variable : public Expression {
		TypePtr type;
		IdentifierPtr name;
		Variable(TypePtr type, IdentifierPtr name) : Expression(NT_Variable), type(type), name(name) {}
		virtual bool equals(const Node& node) const;
	};

	struct Initializer : public Expression {
		TypePtr type;
		vector<NodePtr> values;
		Initializer(TypePtr type) : Expression(NT_Initializer), type(type) {};
		Initializer(TypePtr type, const vector<NodePtr>& values) : Expression(NT_Initializer), type(type), values(values) {};
		virtual bool equals(const Node& node) const;
	};

	struct DesignatedInitializer : public Expression {
		TypePtr type;
		IdentifierPtr member;
		ExpressionPtr value;
		DesignatedInitializer(TypePtr type, IdentifierPtr member, ExpressionPtr value)
			: Expression(NT_DesignatedInitializer), type(type), member(member), value(value) {};
		virtual bool equals(const Node& node) const;
	};

	struct ArrayInit : public Expression {
		TypePtr type;
		ExpressionPtr size;
		ArrayInit(TypePtr type, ExpressionPtr size)
			: Expression(NT_ArrayInit), type(type), size(size) {};
		virtual bool equals(const Node& node) const;
	};

	struct VectorInit : public Expression {
		vector<NodePtr> values;
		VectorInit() : Expression(NT_VectorInit) {};
		VectorInit(const vector<NodePtr>& values) : Expression(NT_VectorInit), values(values) {};
		virtual bool equals(const Node& node) const;
	};

	struct OCLVectorInit : public Expression {
		TypePtr type;
		vector<NodePtr> values;
		OCLVectorInit() : Expression(NT_OCLVectorInit) {};
		OCLVectorInit(TypePtr type, const vector<NodePtr>& values) : Expression(NT_OCLVectorInit), type(type), values(values) {};
		virtual bool equals(const Node& node) const;
	};

	struct UnaryOperation : public Expression {

		enum UnaryOp {
			UnaryPlus,
			UnaryMinus,
			PrefixInc,
			PrefixDec,
			PostfixInc,
			PostfixDec,
			LogicNot,
			BitwiseNot,
			Indirection,
			Reference,
			SizeOf,
			New,
			Delete,
			DeleteArray,
		};

		UnaryOp operation;
		NodePtr operand;

		UnaryOperation(UnaryOp operation, NodePtr operand)
			: Expression(NT_UnaryOperation), operation(operation), operand(operand) {}

		virtual bool equals(const Node& node) const;
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
			IndirectMemberAccess,
			Subscript,
			Cast,
			Comma,

			// C++ operators
			StaticCast,
			DynamicCast
		};

		BinaryOp operation;
		NodePtr operandA;
		NodePtr operandB;

		BinaryOperation(BinaryOp operation, NodePtr operandA, NodePtr operandB)
			: Expression(NT_BinaryOperation), operation(operation), operandA(operandA), operandB(operandB) {}

		virtual bool equals(const Node& node) const;
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

		virtual bool equals(const Node& node) const;
	};

	struct Call : public Expression {
		NodePtr function;
		vector<NodePtr> arguments;

		Call(NodePtr function) : Expression(NT_Call), function(function) {}

		Call(NodePtr function, const vector<NodePtr>& args)
			: Expression(NT_Call), function(function), arguments(args) {}

		template<typename ... E>
		Call(NodePtr function, E ... args)
			: Expression(NT_Call), function(function), arguments(toVector<NodePtr>(args...)) {}

		virtual bool equals(const Node& node) const;
	};

	struct MemberCall : public Expression {
		NodePtr memberFun;
		NodePtr object;
		vector<NodePtr> arguments;

		MemberCall(NodePtr memberFun, NodePtr object, const vector<NodePtr>& args)
			: Expression(NT_MemberCall), memberFun(memberFun), object(object), arguments(args) {}

		virtual bool equals(const Node& node) const;
	};

	struct ConstructorCall : public Expression {
		TypePtr classType;
		vector<NodePtr> arguments;
		ExpressionPtr location;
		ConstructorCall(TypePtr classType, const vector<NodePtr>& args, ExpressionPtr location = ExpressionPtr())
			: Expression(NT_ConstructorCall), classType(classType), arguments(args), location(location) {}
		virtual bool equals(const Node& node) const;
	};

	struct DestructorCall : public Expression {
		TypePtr classType;
		ExpressionPtr location;
		bool isVirtual;
		DestructorCall(TypePtr classType, ExpressionPtr location, bool isVirtual)
			: Expression(NT_DestructorCall), classType(classType), location(location), isVirtual(isVirtual) {}
		virtual bool equals(const Node& node) const;
	};

	struct Parentheses : public Expression {
		ExpressionPtr expression;
		Parentheses(ExpressionPtr expression) : Expression(NT_Parentheses), expression(expression) {}
		virtual bool equals(const Node& node) const;
	};

	// -- TopLevelElement -------------------------

	struct TopLevelElement : public Node {
		TopLevelElement(NodeType type) : Node(type) {};
	};

	// -- Declarations ----------------------------

	struct Declaration : public TopLevelElement {
		Declaration(NodeType type) : TopLevelElement(type) {};
	};

	struct TypeDeclaration : public Declaration {
		TypePtr type;
		TypeDeclaration(TypePtr type) : Declaration(NT_TypeDeclaration), type(type) {}
		virtual bool equals(const Node& node) const;
	};

	struct FunctionPrototype : public Declaration {
		FunctionPtr function;
		FunctionPrototype(FunctionPtr function) : Declaration(NT_FunctionPrototype), function(function) {}
		virtual bool equals(const Node& node) const;
	};

	struct GlobalVarDecl : public Declaration {
		TypePtr type;
		string name;
		bool external;
		GlobalVarDecl(TypePtr type, const string& name, bool external)
			: Declaration(NT_GlobalVarDecl), type(type), name(name), external(external) {}
		virtual bool equals(const Node& node) const;
	};

	struct ConstructorPrototype : public Node {
		ConstructorPtr ctor;
		ConstructorPrototype(const ConstructorPtr& ctor)
			: Node(NT_ConstructorPrototype), ctor(ctor) {}
		virtual bool equals(const Node& node) const;
	};

	struct DestructorPrototype : public Node {
		DestructorPtr dtor;
		bool isVirtual;
		DestructorPrototype(const DestructorPtr& dtor, bool isVirtual = false)
			: Node(NT_DestructorPrototype), dtor(dtor), isVirtual(isVirtual) {}
		virtual bool equals(const Node& node) const;
	};

	struct MemberFunctionPrototype : public Node {
		bool isVirtual;
		MemberFunctionPtr fun;
		bool pureVirtual;
		MemberFunctionPrototype(const MemberFunctionPtr& fun, bool isVirtual = false, bool isPureVirtual = false)
			: Node(NT_MemberFunctionPrototype), isVirtual(isVirtual), fun(fun), pureVirtual(isPureVirtual) {}
		virtual bool equals(const Node& node) const;
	};


	// -- Definitions ----------------------------

	struct Definition : public TopLevelElement {
		Definition(NodeType type) : TopLevelElement(type) {};
	};

	struct TypeDefinition : public Definition {
		TypePtr type;
		IdentifierPtr name;
		TypeDefinition(TypePtr type)
			: Definition(NT_TypeDefinition), type(type), name(0) {}
		TypeDefinition(TypePtr type, IdentifierPtr name)
			: Definition(NT_TypeDefinition), type(type), name(name) {}
		virtual bool equals(const Node& node) const;
	};

	struct Function : public Definition {
		enum Modifier {
			STATIC 		= 1<<0,
			INLINE 		= 1<<1,
			OCL_KERNEL 	= 1<<2
		};
		unsigned flags;
		TypePtr returnType;
		IdentifierPtr name;
		vector<VariablePtr> parameter;
		StatementPtr body;
		Function() : Definition(NT_Function), flags(0) {};
		Function(TypePtr returnType, IdentifierPtr name, StatementPtr body)
					: Definition(NT_Function), flags(0), returnType(returnType), name(name), body(body) {};
		Function(TypePtr returnType, IdentifierPtr name, const vector<VariablePtr> params, StatementPtr body)
			: Definition(NT_Function), flags(0), returnType(returnType), name(name), parameter(params), body(body) {};
		Function(unsigned flags, TypePtr returnType, IdentifierPtr name, const vector<VariablePtr> params, StatementPtr body)
					: Definition(NT_Function), flags(flags), returnType(returnType), name(name), parameter(params), body(body) {};
		virtual bool equals(const Node& node) const;
	};


	struct Constructor : public Definition {
		typedef pair<IdentifierPtr, vector<NodePtr>> InitializerListEntry;
		typedef vector<InitializerListEntry> InitializationList;
		IdentifierPtr className;
		FunctionPtr function;
		InitializationList initialization;
		Constructor(const IdentifierPtr& className, const FunctionPtr& function, const InitializationList& initializer = InitializationList())
			: Definition(NT_Constructor), className(className), function(function), initialization(initializer) {}
		virtual bool equals(const Node& node) const;
	};

	struct Destructor : public Definition {
		IdentifierPtr className;
		FunctionPtr function;
		Destructor(const IdentifierPtr& className, const FunctionPtr& function)
			: Definition(NT_Destructor), className(className), function(function) {}
		virtual bool equals(const Node& node) const;
	};

	struct MemberFunction : public Definition {
		IdentifierPtr className;
		FunctionPtr function;
		bool isConstant;
		MemberFunction(const IdentifierPtr& className, const FunctionPtr& function, bool isConstant = false)
			: Definition(NT_MemberFunction), className(className), function(function), isConstant(isConstant) {}
		virtual bool equals(const Node& node) const;
	};

	struct Namespace : public TopLevelElement {
		IdentifierPtr name;
		TopLevelElementPtr definition;
		Namespace(const IdentifierPtr& name, const TopLevelElementPtr& definition)
			: TopLevelElement(NT_Namespace), name(name), definition(definition) {}
		virtual bool equals(const Node& node) const;
	};

	struct ExternC : public TopLevelElement {
		vector<TopLevelElementPtr> definitions;
		ExternC(const TopLevelElementPtr& definition)
			: TopLevelElement(NT_ExternC), definitions(toVector(definition)) {}
		ExternC(const vector<TopLevelElementPtr>& definitions = vector<TopLevelElementPtr>())
			: TopLevelElement(NT_ExternC), definitions(definitions) {}
		virtual bool equals(const Node& node) const;
	};


	// -- Utilities ------------------------------

	class CNodeManager;
	typedef std::shared_ptr<CNodeManager> SharedCNodeManager;

	class CNodeManager : private boost::noncopyable {

		vector<NodePtr> nodes;
		std::map<string, IdentifierPtr> identMap;

	public:

		CNodeManager() : nodes(), identMap() {}

		~CNodeManager();

		IdentifierPtr create(const string& name = "");

		PrimitiveTypePtr create(const PrimitiveType::CType type);

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


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

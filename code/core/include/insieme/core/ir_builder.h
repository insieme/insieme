/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/forward_decls.h"

#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_node_traits.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_values.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/analysis/normalize.h"

#include "insieme/core/parser/ir_parser.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/reference.h"

#include "insieme/core/types/subtyping.h"

namespace insieme {
namespace core {


	/**
	 * A base class for IR builder modules covering basic module features
	 * including e.g. the maintenance of a node manager reference.
	 */
	class IRBuilderModule {
	  protected:

		/**
		 * The mode manager references this module is utilizing for
		 * constructing IR nodes.
		 */
		NodeManager& manager;

	  public:

		/**
		 * Creates a new instance of this module based on the given manager.
		 */
		IRBuilderModule(NodeManager& mgr) : manager(mgr) {}

	};

	/**
	 * The
	 */
	template<typename ... Modules>
	class ModularIRBuilder : public Modules... {
	public:
		ModularIRBuilder(NodeManager& mgr) : Modules(mgr)... {}
	};


	/**
	 * An IR builder module covering the construction of basic language
	 * including literals, calls, and struct statements. It also introduces
	 * support for parsing IR code fragments and normalizing IR code.
	 */
	class IRBuilderBaseModule : public IRBuilderModule {

	  public:

		/**
		 * A type used within some signatures mapping variables to values.
		 */
		typedef utils::map::PointerMap<VariablePtr, ExpressionPtr> VarValueMapping;

		/**
		 * Creates a new IR builder working with the given node manager.
		 */
		IRBuilderBaseModule(NodeManager& manager) : IRBuilderModule(manager) {}

		/**
		 * Obtains a reference to the node manager used by this builder.
		 */
		NodeManager& getNodeManager() const {
			return manager;
		}

		/**
		 * Obtains a reference to the basic generator within the node manager.
		 */
		const lang::BasicGenerator& getLangBasic() const;

		template<typename Extension>
		const Extension& getExtension() const {
			return manager.getLangExtension<Extension>();
		}

		template <typename T, typename... Children>
		Pointer<const T> get(Children... child) const {
			return T::get(manager, child...);
		}

		template <typename T>
		Pointer<const T> get(const NodeList& children) const {
			return T::get(manager, children);
		}

		template <NodeType type, typename Node = typename to_node_type<type>::type>
		Pointer<const Node> get(const NodeList& children) const {
			// use factory method of Node implementation
			return Node::get(manager, children);
		}

		NodePtr get(NodeType type, const NodeList& children) const;


		// --- Add parser support ---

		// the type utilized for forwarding literal definitions to the parser functions
		typedef parser::DefinitionMap LazyDefinitionMap;

		// a convenience type for forwarding literal definitions to the parser functions
		typedef std::map<std::string, NodePtr> EagerDefinitionMap;

		/**
		 * Parses any kind of IR fragment encoded within the given code. The given symbol table
		 * allows additional pre-defined let-definitions to be considered.
		 *
		 * NOTE:  this is a bad choice, try to use the other functions.
		 *      THIS FUNCTION PROVIDES NO FEEDBACK
		 *      THIS FUNCTION IS SLOW
		 *
		 * @param code the code to be parsed and returned as a node
		 * @param symbols a set of pre-defined symbols to be used within the code
		 */
		NodePtr parse(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		NodePtr parse(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * The same as the parse member function yet interpreting the given code as a type.
		 */
		TypePtr parseType(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		TypePtr parseType(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * The same as the parse member function yet interpreting the given code as an expression.
		 */
		ExpressionPtr parseExpr(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		ExpressionPtr parseExpr(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * The same as the parse member function yet interpreting the given code as a statement.
		 */
		StatementPtr parseStmt(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		StatementPtr parseStmt(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * The same as the parse member function yet interpreting the given code as a full program.
		 */
		ProgramPtr parseProgram(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		ProgramPtr parseProgram(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		* Allows lists of addresses to be parsed in a type. This parser supports the same grammar + allows constructs to be enclosed
		* within $ .. $ signs. Addresses referencing constructs enclosed like this will be returned. The resulting list is
		* ordered according to the order of node-addresses (lexicographical).
		*/
		vector<NodeAddress> parseAddressesType(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		* The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		*/
		vector<NodeAddress> parseAddressesType(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * Allows lists of addresses to be parsed in an expression. This parser supports the same grammar + allows constructs to be enclosed
		 * within $ .. $ signs. Addresses referencing constructs enclosed like this will be returned. The resulting list is
		 * ordered according to the order of node-addresses (lexicographical).
		 */
		vector<NodeAddress> parseAddressesExpression(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		vector<NodeAddress> parseAddressesExpression(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * Allows lists of addresses to be parsed in a Statement. This parser supports the same grammar + allows constructs to be enclosed
		 * within $ .. $ signs. Addresses referencing constructs enclosed like this will be returned. The resulting list is
		 * ordered according to the order of node-addresses (lexicographical).
		 */
		vector<NodeAddress> parseAddressesStatement(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		vector<NodeAddress> parseAddressesStatement(const string& code, const EagerDefinitionMap& symbols) const;

		/**
		 * Allows lists of addresses to be parsed in an IR program. This parser supports the same grammar + allows constructs to be enclosed
		 * within $ .. $ signs. Addresses referencing constructs enclosed like this will be returned. The resulting list is
		 * ordered according to the order of node-addresses (lexicographical).
		 */
		vector<NodeAddress> parseAddressesProgram(const string& code, const LazyDefinitionMap& symbols = LazyDefinitionMap()) const;

		/**
		 * The same as above, but utilizing a eager definition map (will be internally converted into a lazy map).
		 */
		vector<NodeAddress> parseAddressesProgram(const string& code, const EagerDefinitionMap& symbols) const;


		// --- Imported Standard Factory Methods from Node Types ---

		#include "insieme/core/generated/ir_builder.h"

		// --- Handle value classes ---

		StringValuePtr stringValue(const char* str) const;
		StringValuePtr stringValue(const string& str) const;

		BoolValuePtr boolValue(bool value) const;
		CharValuePtr charValue(char value) const;
		IntValuePtr intValue(int value) const;
		UIntValuePtr uintValue(unsigned value) const;

		// --- Convenience Utilities ---

		bool matchType(const std::string& typeStr, const core::TypePtr& irType) const;

		template <typename T>
		core::Pointer<const T> normalize(const core::Pointer<const T>& root) const {
			return core::analysis::normalize(root);
		}

		GenericTypePtr refType(const TypePtr& elementType, bool _const = false, bool _volatile = false, lang::ReferenceType::Kind kind = lang::ReferenceType::Kind::Plain) const;
		TypePtr ptrType(const TypePtr& elementType, bool _const = false, bool _volatile = false) const;
		GenericTypePtr channelType(const TypePtr& elementType, const ExpressionPtr& size) const;

		GenericTypePtr arrayType(const TypePtr& elementType) const;
		GenericTypePtr arrayType(const TypePtr& elementType, const LiteralPtr& size) const;
		GenericTypePtr arrayType(const TypePtr& elementType, const VariablePtr& size) const;
		GenericTypePtr arrayType(const TypePtr& elementType, size_t size) const;

		FieldPtr field(const string& name, const TypePtr& type) const;

		TagTypePtr structType(const vector<std::pair<StringValuePtr, TypePtr>>& fields) const;
		TagTypePtr structType(const vector<ParentPtr>& parents, const vector<FieldPtr>& fields) const;
		TagTypePtr structType(const vector<TypePtr>& parents, const vector<FieldPtr>& fields) const;
		TagTypePtr structType(const vector<ParentPtr>& parents, const vector<std::pair<StringValuePtr, TypePtr>>& fields) const;
		TagTypePtr structType(const vector<TypePtr>& parents, const vector<std::pair<StringValuePtr, TypePtr>>& fields) const;
		TagTypePtr structType(const vector<FieldPtr>& fields = vector<FieldPtr>()) const;
		TagTypePtr structType(const string& name, const vector<FieldPtr>& fields) const;
		TagTypePtr structType(const StringValuePtr& name, const vector<FieldPtr>& fields) const;
		TagTypePtr structType(const StringValuePtr& name, const vector<ParentPtr>& parents, const vector<FieldPtr>& fields) const;
		TagTypePtr structType(const string& name, const ParentList& parents, const FieldList& fields,
		                      const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
		                      const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) const;
		TagTypePtr structType(const StringValuePtr& name, const ParentsPtr& parents, const FieldsPtr& fields,
		                      const ExpressionsPtr& ctors, const ExpressionPtr& dtor, const BoolValuePtr& dtorIsVirtual,
		                      const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvmfuns) const;
		TagTypePtr structTypeWithDefaults(const TypePtr& thisType, const ParentList& parents, const FieldList& fields,
		                                  const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
		                                  const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) const;

		TagTypePtr unionType(const vector<std::pair<StringValuePtr, TypePtr>>& fields) const;
		TagTypePtr unionType(const StringValuePtr& name, const vector<FieldPtr>& fields) const;
		TagTypePtr unionType(const vector<FieldPtr>& fields) const;
		TagTypePtr unionType(const string& name, const FieldList& fields,
		                     const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
		                     const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) const;
		TagTypePtr unionType(const StringValuePtr& name, const FieldsPtr& fields,
		                     const ExpressionsPtr& ctors, const ExpressionPtr& dtor, const BoolValuePtr& dtorIsVirtual,
		                     const MemberFunctionsPtr& mfuns, const PureVirtualMemberFunctionsPtr& pvmfuns) const;
		TagTypePtr unionTypeWithDefaults(const TypePtr& thisType, const FieldList& fields,
		                                 const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
		                                 const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) const;

		// -- special functions for classes --

		LambdaExprPtr getDefaultConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) const;

		LambdaExprPtr getDefaultCopyConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) const;

		LambdaExprPtr getDefaultMoveConstructor(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) const;

		LambdaExprPtr getDefaultDestructor(const TypePtr& thisType) const;

		MemberFunctionPtr getDefaultCopyAssignOperator(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) const;

		MemberFunctionPtr getDefaultMoveAssignOperator(const TypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields) const;

	  private:
		LiteralPtr getLiteralForMember(const FunctionTypePtr& functionType, const std::string& memberName) const;
	  public:

		LiteralPtr getLiteralForConstructor(const FunctionTypePtr& functionType) const;

		LiteralPtr getLiteralForDestructor(const FunctionTypePtr& functionType) const;

		LiteralPtr getLiteralForMemberFunction(const FunctionTypePtr& functionType, const std::string& memberName) const;

		// -----------------------------------

		TupleExprPtr tupleExpr(const ExpressionList& values = ExpressionList()) const;

		template <typename... T>
		TupleExprPtr tupleExpr(const ExpressionPtr& expr, const T&... rest) const {
			return tupleExpr(toVector<ExpressionPtr>(expr, rest...));
		}

		InitExprPtr initExpr(const ExpressionPtr& memExpr, const ExpressionList& initExprs = ExpressionList()) const {
			assert_true(lang::isReference(memExpr)) << "initExpr needs to be built on Reference type, got " << dumpColor(memExpr->getType());
			return initExpr(memExpr->getType().as<GenericTypePtr>(), memExpr, expressions(initExprs));
		}
		template <typename... T>
		InitExprPtr initExpr(const ExpressionPtr& memExpr, const T&... initExprs) const {
			return initExpr(memExpr, toVector<ExpressionPtr>(initExprs...));
		}

		InitExprPtr initExprTemp(const GenericTypePtr& type, const ExpressionList& initExprs = ExpressionList()) const {
			return initExpr(lang::buildRefTemp(type), initExprs);
		}
		template <typename... T>
		InitExprPtr initExprTemp(const GenericTypePtr& type, const T&... initExprs) const {
			return initExprTemp(type, toVector<ExpressionPtr>(initExprs...));
		}

		// creates a program - empty or based on the given entry points
		ProgramPtr createProgram(const ExpressionList& entryPoints = ExpressionList()) const;

		// Function Types
		FunctionTypePtr toPlainFunctionType(const FunctionTypePtr& funType) const;
		FunctionTypePtr toThickFunctionType(const FunctionTypePtr& funType) const;

		// Unit consume
		ExpressionPtr unitConsume(const ExpressionPtr& toConsume) const;

		// Literals
		LiteralPtr stringLit(const std::string& str, const bool isConst = true) const;
		LiteralPtr intLit(const int val, bool tight = false) const;
		LiteralPtr uintLit(const unsigned int val, bool tight = false) const;
		LiteralPtr integerLit(const int val, bool tight = false) const;
		LiteralPtr boolLit(bool value) const;

		LiteralPtr floatLit(const string& value) const;
		LiteralPtr floatLit(float value) const;
		LiteralPtr doubleLit(const string& value) const;
		LiteralPtr doubleLit(double value) const;

		// Support reverse literal construction
		LiteralPtr literal(const std::string& value, const TypePtr& type) const {
			return literal(type, value);
		}
		LiteralPtr literal(const StringValuePtr& value, const TypePtr& type) const {
			return literal(type, value);
		}

		// Build undefined initializers
		ExpressionPtr undefinedNew(const TypePtr& type) const;

		/**
		 * A factory method for a identifier literal.
		 */
		LiteralPtr getIdentifierLiteral(const string& value) const;
		LiteralPtr getIdentifierLiteral(const StringValuePtr& value) const;

		/**
		 * Build type tokens (metatypes.... one of those => type<'a>  when 'a is a bounded to an specific type)
		 */
		TypePtr getTypeLiteralType(const TypePtr& type) const;

		/**
		 * Build a numeric Type from a number.
		 */
		TypePtr numericType(int64_t value) const;

		/**
		 * A factory method for a type literals.
		 */
		LiteralPtr getTypeLiteral(const TypePtr& type) const;

		/**
		 * A method generating a vector init expression form a scalar.
		 */
		ExpressionPtr scalarToVector(const TypePtr& type, const ExpressionPtr& subExpr) const;

		// Values
		// obtains a zero value - recursively resolved for the given type
		ExpressionPtr getZero(const TypePtr& type) const;

		// Referencing
		ExpressionPtr deref(const ExpressionPtr& subExpr) const;
		CallExprPtr refTemp(const ExpressionPtr& subExpr) const;
		CallExprPtr refNew(const ExpressionPtr& subExpr) const;
		CallExprPtr refDelete(const ExpressionPtr& subExpr) const;
		CallExprPtr assign(const ExpressionPtr& target, const ExpressionPtr& value) const;
		ExpressionPtr tryDeref(const ExpressionPtr& subExpr) const;

		ExpressionPtr refReinterpret(const ExpressionPtr& subExpr, const TypePtr& newElementType) const;

		ExpressionPtr invertSign(const ExpressionPtr& subExpr) const;
		// Returns the negation of the passed subExpr (which must be of boolean type)
		// 	       (<BOOL> expr) -> <BOOL> !expr
		ExpressionPtr negateExpr(const ExpressionPtr& subExpr) const;

		// Compound Statements
		template <typename... Nodes>
		CompoundStmtPtr compoundStmt(const Pointer<const Nodes>&... nodes) const {
			return compoundStmt(toVector<StatementPtr>(nodes...));
		}

		// Declaration Statements
		DeclarationStmtPtr declarationStmt(const ExpressionPtr& value) const;
		DeclarationStmtPtr declarationStmt(const TypePtr& type, const ExpressionPtr& value) const;
		DeclarationStmtPtr declarationStmt(const VariablePtr& value) const;

		// Return Statement
		ReturnStmtPtr returnStmt(const ExpressionPtr& retVal) const;
		ReturnStmtPtr returnStmt() const;

		// Call Expressions
		CallExprPtr callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr) const;

		template <typename First, typename... Rest>
		typename std::enable_if<!std::is_same<First, DeclarationPtr>::value, CallExprPtr>::type
		callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const First& arg1, const Rest&... rest) const {
			return callExpr(resultType, functionExpr, toVector<ExpressionPtr>(arg1, rest...));
		}
		template <typename First, typename... Rest>
		typename std::enable_if<std::is_same<First, DeclarationPtr>::value, CallExprPtr>::type
		callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const First& arg1, const Rest&... rest) const {
			return callExpr(resultType, functionExpr, toVector<DeclarationPtr>(arg1, rest...));
		}

		// For the methods below, the return type is deduced from the functionExpr's function type
		CallExprPtr callExpr(const ExpressionPtr& functionExpr, const ExpressionList& arguments = vector<ExpressionPtr>()) const;
		CallExprPtr callExpr(const ExpressionPtr& functionExpr, const NodeRange<ExpressionPtr>& range) const {
			return callExpr(functionExpr, (ExpressionList)range);
		}
		CallExprPtr callExpr(const ExpressionPtr& functionExpr, const DeclarationList& arguments) const;

		template <typename First, typename... Rest>
		typename std::enable_if<!std::is_same<First, DeclarationPtr>::value, CallExprPtr>::type
		callExpr(const ExpressionPtr& functionExpr, const First& arg1, const Rest&... rest) const {
			return callExpr(functionExpr, toVector<ExpressionPtr>(arg1, rest...));
		}
		template <typename First, typename... Rest>
		typename std::enable_if<std::is_same<First, DeclarationPtr>::value, CallExprPtr>::type
		callExpr(const ExpressionPtr& functionExpr, const First& arg1, const Rest&... rest) const {
			return callExpr(functionExpr, toVector<DeclarationPtr>(arg1, rest...));
		}

		// Lambda Nodes
		LambdaPtr lambda(const FunctionTypePtr& type, const ParametersPtr& params, const StatementPtr& body) const;
		LambdaPtr lambda(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body) const;

		// Lambda Expressions
		LambdaExprPtr lambdaExpr(const TypePtr& returnType, const VariableList& params, const StatementPtr& body, const std::string& name="_") const;
		LambdaExprPtr lambdaExpr(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body, const std::string& name = "_") const;

		BindExprPtr bindExpr(const VariableList& params, const CallExprPtr& call) const;

		template <typename... Vars>
		ParametersPtr parameters(const Vars&... vars) const {
			return Parameters::get(manager, toVector<VariablePtr>(vars...));
		}

		// Create a job expression
		JobExprPtr jobExpr(const ExpressionPtr& threadNumRange, const ExpressionPtr& body) const;
		JobExprPtr jobExpr(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& rangeUpperBound, const ExpressionPtr& body) const;
		JobExprPtr jobExpr(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& rangeUpperBound, const ExpressionPtr& rangeMod,
		                   const ExpressionPtr& body) const;
		JobExprPtr jobExprUnbounded(const ExpressionPtr& rangeLowerBound, const ExpressionPtr& body) const;
		JobExprPtr jobExpr(const StatementPtr& stmt, int numThreads = -1) const;

		// Create a marker expression
		MarkerExprPtr markerExpr(const ExpressionPtr& subExpr, unsigned id) const;
		MarkerExprPtr markerExpr(const ExpressionPtr& subExpr, const UIntValuePtr& id) const;
		MarkerStmtPtr markerStmt(const StatementPtr& subExpr, unsigned id) const;
		MarkerStmtPtr markerStmt(const StatementPtr& subExpr, const UIntValuePtr& id) const;

		// Creation of thread number ranges
		CallExprPtr getThreadNumRange(unsigned min) const;
		CallExprPtr getThreadNumRange(unsigned min, unsigned max) const;
		CallExprPtr getThreadNumRange(const ExpressionPtr& min) const;
		CallExprPtr getThreadNumRange(const ExpressionPtr& min, const ExpressionPtr& max) const;
		CallExprPtr getThreadNumRange(const ExpressionPtr& min, const ExpressionPtr& max, const ExpressionPtr& mod) const;

		// Direct call expression of getThreadGroup
		CallExprPtr getThreadGroup(ExpressionPtr level = ExpressionPtr()) const;
		CallExprPtr getThreadGroupSize(ExpressionPtr level = ExpressionPtr()) const;
		CallExprPtr getThreadId(ExpressionPtr level = ExpressionPtr()) const;
		CallExprPtr getDefaultThreads() const;

		// Direct call expression of barrier
		CallExprPtr barrier(ExpressionPtr threadgroup = ExpressionPtr()) const;

		// Direct call expression of mergeAll
		CallExprPtr mergeAll() const;

		// Direct call expression of pfor
		CallExprPtr pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step = ExpressionPtr()) const;

		// Build a Call expression for a pfor that mimics the effect of the given for statement
		CallExprPtr pfor(const ForStmtPtr& initialFor) const;

		// Builds a job processing the given statement and triggers its parallel execution
		CallExprPtr parallel(const StatementPtr& stmt, int numThreads = -1) const;

		/**
		 * Creates an expression accessing the corresponding member of the given struct.
		 */
		CallExprPtr accessMember(const ExpressionPtr& structExpr, const string& member) const;

		/**
		 * Creates an expression accessing the corresponding member of the given struct.
		 */
		CallExprPtr accessMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const;

		/**
		 * Creates an expression obtaining a reference to a member of a struct.
		 */
		CallExprPtr refMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const;

		/**
		 * Creates an expression obtaining a reference to a member of a struct.
		 */
		CallExprPtr refMember(const ExpressionPtr& structExpr, const string& member) const;

		/**
		 * Creates an expression accessing the given component of the given tuple value.
		 */
		CallExprPtr accessComponent(ExpressionPtr tupleExpr, unsigned component) const;
		CallExprPtr accessComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const;

		/**
		 * Creates an expression accessing the reference to a component of the given tuple value.
		 */
		CallExprPtr refComponent(ExpressionPtr tupleExpr, unsigned component) const;
		CallExprPtr refComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const;

		/**
		 * Creates an expression accessing the reference to a component of the given array value.
		 */
		CallExprPtr arraySubscript(const ExpressionPtr& array, const ExpressionPtr& index) const;
		CallExprPtr arrayRefElem(const ExpressionPtr& array, const ExpressionPtr& index) const;
		CallExprPtr arrayAccess(const ExpressionPtr& array, const ExpressionPtr& index) const;

		// Locks
		CallExprPtr acquireLock(const ExpressionPtr& lock) const;
		CallExprPtr tryAcquireLock(const ExpressionPtr& lock) const;
		CallExprPtr releaseLock(const ExpressionPtr& lock) const;
		CallExprPtr initLock(const ExpressionPtr& lock) const;

		// Atomics
		CallExprPtr atomicOp(const ExpressionPtr& location, const ExpressionPtr& testFunc, const ExpressionPtr& replaceFunc);
		/** Creates an atomic assignment operation (including operations such as atomic addition) */
		CallExprPtr atomicAssignment(const CallExprPtr& assignment);
		/** Creates a conditional atomic operation */
		CallExprPtr atomicConditional(const IfStmtPtr& statement);

		// Variants
		CallExprPtr pickVariant(const ExpressionList& variants) const;
		CallExprPtr pickInRange(const ExpressionPtr& id, const ExpressionPtr& max, const ExpressionPtr& qualLB = ExpressionPtr(),
		                        const ExpressionPtr& qualUB = ExpressionPtr(), const ExpressionPtr& qualS = ExpressionPtr()) const;


		/**
		 * A function obtaining a reference to a NoOp instance.
		 */
		CompoundStmtPtr getNoOp() const;

		/**
		 * Tests whether the given node is a no-op.
		 *
		 * @param node the node to be tested
		 * @return true if it is a no-op, false otherwise
		 */
		bool isNoOp(const NodePtr& node) const;
		bool isNoOp(const CompoundStmtPtr& p) const {
			return p->empty();
		}

		IfStmtPtr ifStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody = StatementPtr()) const;
		WhileStmtPtr whileStmt(const ExpressionPtr& condition, const StatementPtr& body) const;
		ForStmtPtr forStmt(const DeclarationStmtPtr& var, const ExpressionPtr& end, const ExpressionPtr& step, const StatementPtr& body) const;
		ForStmtPtr forStmt(const VariablePtr& var, const ExpressionPtr& start, const ExpressionPtr& end, const ExpressionPtr& step,
		                   const StatementPtr& body) const;

		SwitchCasePtr switchCase(const LiteralPtr& lit, const StatementPtr& stmt) const {
			return switchCase(lit, wrapBody(stmt));
		};

		SwitchStmtPtr switchStmt(const ExpressionPtr& switchStmt, const vector<std::pair<ExpressionPtr, StatementPtr>>& cases,
		                         const StatementPtr& defaultCase = StatementPtr()) const;
		SwitchStmtPtr switchStmt(const ExpressionPtr& switchStmt, const vector<SwitchCasePtr>& cases, const StatementPtr& defaultCase = StatementPtr()) const;


		// ------------------------ Operators ---------------------------

		TypePtr infereExprType(const ExpressionPtr& op, const ExpressionPtr& a) const;
		TypePtr infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b) const;
		TypePtr infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& c) const;

		inline CallExprPtr unaryOp(const ExpressionPtr& op, const ExpressionPtr& a) const {
			return callExpr(infereExprType(op, a), op, a);
		}

		inline CallExprPtr binaryOp(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b) const {
			return callExpr(infereExprType(op, a, b), op, a, b);
		}

		inline CallExprPtr ternaryOp(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& c) const {
			return callExpr(infereExprType(op, a, b, c), op, a, b, c);
		}

		inline ExpressionPtr getOperator(lang::BasicGenerator::Operator op, const TypePtr& a) const {
			return getLangBasic().getOperator(a, op);
		}

		inline ExpressionPtr getOperator(lang::BasicGenerator::Operator op, const TypePtr& a, const TypePtr& b) const {
			// TODO: pick operator based on both operands types!!
			return getLangBasic().getOperator(a, op);
		}

		// unary operators

		inline CallExprPtr bitwiseNeg(const ExpressionPtr& a) const {
			return unaryOp(getOperator(lang::BasicGenerator::Not, a->getType()), a);
		}

		inline CallExprPtr logicNeg(const ExpressionPtr& a) const {
			return unaryOp(getOperator(lang::BasicGenerator::LNot, a->getType()), a);
		}

		inline ExpressionPtr plus(const ExpressionPtr& a) const {
			return a; // this operator can be skipped
		}

		// special (more complex) handling of unary minus)
		LiteralPtr minus(const LiteralPtr& lit) const;
		ExpressionPtr minus(const ExpressionPtr& a) const;

		ExpressionPtr numericCast(const core::ExpressionPtr& expr, const core::TypePtr& targetType) const;

		inline CallExprPtr preInc(const ExpressionPtr& a) const {
			return unaryOp(getExtension<lang::ReferenceExtension>().getGenPreInc(), a);
		}

		inline CallExprPtr postInc(const ExpressionPtr& a) const {
			return unaryOp(getExtension<lang::ReferenceExtension>().getGenPostInc(), a);
		}

		inline CallExprPtr preDec(const ExpressionPtr& a) const {
			return unaryOp(getExtension<lang::ReferenceExtension>().getGenPreDec(), a);
		}

		inline CallExprPtr postDec(const ExpressionPtr& a) const {
			return unaryOp(getExtension<lang::ReferenceExtension>().getGenPostDec(), a);
		}

		// binary operators

		inline CallExprPtr add(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Add, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr sub(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Sub, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr mul(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Mul, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr ceil(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(manager.getLangBasic().getCloogCeil(), a, b);
		}

		inline CallExprPtr floor(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(manager.getLangBasic().getCloogFloor(), a, b);
		}

		inline CallExprPtr div(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Div, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr mod(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Mod, a->getType(), b->getType()), a, b);
		}


		inline CallExprPtr bitwiseAnd(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::And, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr bitwiseOr(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Or, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr bitwiseXor(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Xor, a->getType(), b->getType()), a, b);
		}


		inline CallExprPtr leftShift(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::LShift, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr rightShift(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::RShift, a->getType(), b->getType()), a, b);
		}


		inline CallExprPtr logicAnd(const ExpressionPtr& a, ExpressionPtr b) const {
			if(b->getType()->getNodeType() != NT_FunctionType) { b = wrapLazy(b); }
			return binaryOp(getOperator(lang::BasicGenerator::LAnd, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr logicOr(const ExpressionPtr& a, ExpressionPtr b) const {
			if(b->getType()->getNodeType() != NT_FunctionType) { b = wrapLazy(b); }
			return binaryOp(getOperator(lang::BasicGenerator::LOr, a->getType(), b->getType()), a, b);
		}


		inline CallExprPtr eq(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Eq, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr ne(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Ne, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr lt(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Lt, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr le(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Le, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr gt(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Gt, a->getType(), b->getType()), a, b);
		}

		inline CallExprPtr ge(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return binaryOp(getOperator(lang::BasicGenerator::Ge, a->getType(), b->getType()), a, b);
		}

		// ternary operators

		inline CallExprPtr ite(const ExpressionPtr& cond, const ExpressionPtr& a, const ExpressionPtr& b) const {
			return ternaryOp(getLangBasic().getIfThenElse(), cond, a, b);
		}

		// output operators
		CallExprPtr print(const string& format, const ExpressionList& args) const;
		CallExprPtr print(const ExpressionPtr& format, const ExpressionList& args) const;

		CallExprPtr pack(const ExpressionList& values) const;

		// select operator and derived variants

		CallExprPtr select(const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& op) const;
		CallExprPtr select(const ExpressionPtr& a, const ExpressionPtr& b, lang::BasicGenerator::Operator op) const;

		inline CallExprPtr min(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return select(a, b, lang::BasicGenerator::Lt);
		}

		inline CallExprPtr max(const ExpressionPtr& a, const ExpressionPtr& b) const {
			return select(a, b, lang::BasicGenerator::Gt);
		}

		CallExprPtr id(const ExpressionPtr& a) const {
			return callExpr(a->getType(), getLangBasic().getId(), a);
		}

		/**
		 * Encapsulate the given statement inside a body.
		 */
		CompoundStmtPtr wrapBody(const StatementPtr& stmt) const;

		ExpressionPtr wrapLazy(const ExpressionPtr& expr) const;

		// helper for the pointwise operation
		CallExprPtr pointwise(const ExpressionPtr& callee) const;

		// helper for accuraccy functions
		CallExprPtr accuracyHigh(const ExpressionPtr& callee) const;
		CallExprPtr accuracyBestEffort(const ExpressionPtr& callee) const;
		CallExprPtr accuracyFast(const ExpressionPtr& callee) const;

		// helper for vector permute
		CallExprPtr vectorPermute(const ExpressionPtr& dataVec, const ExpressionPtr& permutationVec) const;

	  private:
		unsigned extractNumberFromExpression(ExpressionPtr& expr) const;
	};


	/**
	 * The default IR builder collecting a standard set of modules.
	 */
	class IRBuilder : public ModularIRBuilder<IRBuilderBaseModule> {
	public:
		IRBuilder(NodeManager& manager) : ModularIRBuilder(manager) {}
	};

	// Utilities

	template <typename Iter, typename T = typename std::remove_const<typename Iter::value_type::element_type>::type,
	          typename boost::enable_if<boost::is_base_of<Expression, T>, int>::type = 0>
	static TypeList extractTypes(const Iter& begin, const Iter& end) {
		TypeList types;
		std::transform(begin, end, std::back_inserter(types), [](const ExpressionPtr& p) { return p->getType(); });
		return types;
	}

	template <typename Container, typename T = typename std::remove_const<typename Container::value_type::element_type>::type,
	          typename boost::enable_if<boost::is_base_of<Expression, T>, int>::type = 0>
	static TypeList extractTypes(const Container& exprs) {
		return extractTypes(exprs.begin(), exprs.end());
	}


} // namespace core
} // namespace insieme

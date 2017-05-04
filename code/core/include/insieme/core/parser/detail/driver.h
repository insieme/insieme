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
 *
 */
#pragma once

#include <string>
#include <sstream>
#include <iostream>
#include <map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/tu/ir_translation_unit.h"

#include "insieme/core/parser/detail/scanner.h"
#include "insieme/core/parser/detail/typed_expression.h"
#include "insieme/core/parser/ir_parser.h"

#include "insieme/core/ir_node_annotation.h"

#include "inspire_parser.hpp"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace insieme {
namespace core {
namespace parser {

	namespace detail {

	class location;
	class InspireParser;

		class ParserIRExtension : public core::lang::Extension {
			/**
			 * Allow the node manager to create instances of this class.
			 */
			friend class core::NodeManager;

			const IRBuilder builder;

			const LiteralPtr memberDummyLambda;

			const LiteralPtr explicitMemberDummyLambda;

			const CompoundStmtPtr defaultedBodyCompound;

			const CompoundStmtPtr deletedBodyCompound;

			const LiteralPtr defaultedBodyMarker;

			/**
			 * Creates a new instance based on the given node manager.
			 */
			ParserIRExtension(core::NodeManager& manager) : core::lang::Extension(manager),
					builder(IRBuilder(manager)),
					memberDummyLambda(builder.literal(builder.genericType("parser_member_dummy_lambda"), "parser_member_dummy_lambda")),
					explicitMemberDummyLambda(builder.literal(builder.genericType("parser_explicit_member_dummy_lambda"), "parser_explicit_member_dummy_lambda")),
					defaultedBodyCompound(builder.compoundStmt(builder.literal(builder.genericType("parser_defaulted_body_compound_marker"), "parser_defaulted_body_compound_marker"))),
					deletedBodyCompound(builder.compoundStmt(builder.literal(builder.genericType("parser_deleted_body_compound_marker"), "parser_deleted_body_compound_marker"))),
					defaultedBodyMarker(builder.literal(builder.genericType("parser_defaulted_body_marker"), "parser_defaulted_body_marker"))
			{}

			LANG_EXT_LITERAL(MemberFunctionAccess, "parser_member_function_access", "('a, identifier) -> unit")

			const LiteralPtr& getMemberDummyLambda() const {
				return memberDummyLambda;
			}

			const LiteralPtr& getExplicitMemberDummyLambda() const {
				return explicitMemberDummyLambda;
			}

			const CompoundStmtPtr& getDefaultedBodyCompound() const {
				return defaultedBodyCompound;
			}

			const CompoundStmtPtr& getDeletedBodyCompound() const {
				return deletedBodyCompound;
			}

			const LiteralPtr& getDefaultedBodyMarker() const {
				return defaultedBodyMarker;
			}
		};

		/**
		 * A struct summarizing an error encountered during parsing.
		 */
		struct ParserError {
			location l;
			std::string msg;
			ParserError(const location& l, const std::string& msg) : l(l), msg(msg) {}
		};

		/**
		 * The driver of the inspire parser providing the context for the parsing process
		 * and recording the result.
		 */
		class InspireDriver {

			// scope management
			struct Scope {
				std::map<TypePtr, TypePtr> aliases;
				DefinitionMap declaredSymbols;
				DefinitionMap declaredTypes;
			};

			mutable std::vector<ParserError> errors;

			std::vector<std::shared_ptr<Scope>> scopes;

		  public:

			InspireDriver(const std::string& f, NodeManager& mgr);

			virtual ~InspireDriver();

			NodeManager& mgr;
			IRBuilder builder;
			std::string file;
			const std::string& str;

			tu::IRTranslationUnit tu;

			NodePtr result;

			location globLoc;

			bool inLambda = true;

		  private:

			std::stringstream ss;
			InspireScanner scanner;
			InspireParser parser;
			mutable bool printedErrors;

			struct RecordStackEntry {
				GenericTypePtr record;
			};

			std::vector<RecordStackEntry> currentRecordStack;

			std::vector<VariablePtr> thisStack;

			std::vector<StringValuePtr> temporaryAnonymousNames;

			const ParserIRExtension& parserIRExtension;

		  public:

			ProgramPtr parseProgram();

			TypePtr parseType();

			StatementPtr parseStmt();

			ExpressionPtr parseExpression();


			// ~~~~~~~~~~~~~~~~~~~~~~~~  tools  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

			/**
			 *  removes potential tuple wrappers from the given expression (if it is a single element)
			 */
			ExpressionPtr getScalar(ExpressionPtr expr);

			/**
			 *  handles appropriate type for expression to be used in an operation
			 */
			ExpressionPtr getOperand(ExpressionPtr expr);

			/**
			 * generates a binary operation given by op between left and right expressions
			 */
			ExpressionPtr genBinaryExpression(const location& l, const std::string& op, ExpressionPtr left, ExpressionPtr right);

			/**
			 * generate a member access in tagtype (struct/union)
			 */
			ExpressionPtr genMemberAccess(const location& l, const ExpressionPtr&, const std::string& memberName);

			/**
			 * generates a tuple access based on index
			 */
			ExpressionPtr genTupleAccess(const location& l, const ExpressionPtr& expr, const std::string& member);

			/**
			 * generates a numeric literal of the correct type, removing any type modifier suffixes
			 */
			LiteralPtr genNumericLiteral(const location& l, const TypePtr& type, const std::string& lit);

			/**
			 * generates a generic type
			 * @param l: the location where this generic type was found
			 * @param name: the name of type
			 * @param parents: list of parent types if any
			 * @param params: list of type parameters
			 * @param IntParamList: list of int type parameters
			 */
			TypePtr genGenericType(const location& l, const std::string& name, const ParentList& parents = ParentList(), const TypeList& params = TypeList());

			/**
			 * generates a numeric type representing the given value
			 * @param l: the location where this generic type was found
			 * @param value: the value to be represented
			 * @return the corresponding type node
			 */
			NumericTypePtr genNumericType(const location& l, const string& value) const;

			/**
			 * generates a negative numeric type from the given positive value
			 * @param l: the location where this generic type was found
			 * @param value: the positive part of the value to be represented
			 * @return the corresponding type node
			 */
			NumericTypePtr genNegativeNumericType(const location& l, const string& value) const;

			/**
			 * generates an unsigned numeric type representing the given value
			 * @param l: the location where this generic type was found
			 * @param value: the value to be represented
			 * @return the corresponding type node
			 */
			NumericTypePtr genUnsignedNumericType(const location& l, const string& value) const;

			/**
			 * generates a numeric type representing the given value
			 * @param l: the location where this generic type was found
			 * @param variable: the variable to be represented
			 * @return the corresponding type node
			 */
			NumericTypePtr genNumericType(const location& l, const ExpressionPtr& variable) const;

			/**
			 *  generates a function type
			 */
			TypePtr genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk = FK_PLAIN);

			/**
			 * generate a record type
			 */
			TypePtr genRecordType(const location& l, const NodeType& type, const string& name, const ParentList& parents, const FieldList& fields, const ExpressionList& ctors,
					const ExpressionPtr& dtor, const bool dtorIsVirtual, const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns);

			/**
			 * generate a simple struct or union consisting only of fields. The decision between sctuct or union will be made based on the given node type.
			 */
			TypePtr genSimpleStructOrUnionType(const location& l, const NodeType& type, const FieldList& fields);

			/**
			 * check whether type alias can be applied to the given type and applies those.
			 */
			TypePtr resolveTypeAliases(const location& l, const TypePtr& type);

			/**
			 * generates a lambda expression
			 */
			LambdaExprPtr genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body, const FunctionKind functionKind = FK_PLAIN);

			/**
			 * generates a closure
			 */
			BindExprPtr genClosure(const location& l, const VariableList& params, StatementPtr body);

			/**
			 * registers the given fields in the current record type
			 */
			void registerFields(const location& l, const FieldList& fields);

			/**
			 * registers the given field for the given record in the global scope
			 */
			void registerField(const location l, const std::string& recordName, const std::string& fieldName, const TypePtr& fieldType);

		  public:

			/**
			 * generates a constructor from the given lambda expression (mostly used in conjunction with genConstructorDetail)
			 */
			ExpressionPtr genConstructor(const location& l, const LambdaExprPtr& ctor);

			/**
			 * generates a constructor for the currently defined record type
			 */
			LambdaExprPtr genConstructorLambda(const location& l, const VariableList& params, const StatementPtr& body);

			/**
			 * generates a destructor for the currently defined record type
			 */
			ExpressionPtr genDestructor(const location& l, const StatementPtr& body);

			/**
			 * generates a member function for the currently defined record type
			 */
			MemberFunctionPtr genMemberFunction(const location& l, bool virtl, bool cnst, bool voltile, const std::string& name, const VariableList& params, const TypePtr& retType, const StatementPtr& body);

			/**
			 * generates a member function for the currently defined record type
			 */
			PureVirtualMemberFunctionPtr genPureVirtualMemberFunction(const location& l, bool cnst, bool voltile, const std::string& name, const FunctionTypePtr& type);

			/**
			 * generates a dummy compound which represents defaulted members
			 */
			CompoundStmtPtr getParserDefaultCompound() const;

			/**
			 * generates a dummy compound which represents deleted members
			 */
			CompoundStmtPtr getParserDeleteCompound() const;

			/**
			 * Tests whether the given node represents a defaulted member (this can either be a literal or a member function)
			 */
			bool isMarkedAsDefaultedMember(const NodePtr& node) const;

			/**
			 * generates a free constructor for the given lambda
			 */
			ExpressionPtr genFreeConstructor(const location& l, const std::string& name, const LambdaExprPtr& ctor);

			/**
			 * generates a function definition
			 */
			ExpressionPtr genFunctionDefinition(const location& l, const std::string name, const LambdaExprPtr& lambda);

			/**
			 * generates an abstract type
			 */
			TypePtr findOrGenAbstractType(const location& l, const std::string& name, const ParentList& parents, const TypeList& typeList);

			/**
			 * Creates a new typed expression from the given arguments
			 */
			ParserTypedExpression genTypedExpression(const location& l, const ExpressionPtr& expression, const TypePtr& type);

			/**
			 * generates a call expression
			 */
			ExpressionPtr genCall(const location& l, const ExpressionPtr& func, ParserTypedExpressionList args);

			/**
			 * generates a constructor call expression
			 */
			ExpressionPtr genConstructorCall(const location& l, const std::string name, ParserTypedExpressionList args);

			/**
			 * generates a destructor call expression
			 */
			ExpressionPtr genDestructorCall(const location& l, const std::string name, const ExpressionPtr& thisArgument);

			/**
			 * Generates a new call expression from the given one with the type of the call expression materialized
			 */
			ExpressionPtr materializeCall(const location& l, const ExpressionPtr& exp);

			/**
			 * constructs an initializer expression according to the given type and expression list
			 */
			ExpressionPtr genInitializerExprTemp(const location& l, const TypePtr& type, const ExpressionList& list);

			/**
			 * constructs an initializer expression according to the given type and expression list
			 */
			ExpressionPtr genInitializerExpr(const location& l, const TypePtr& type, const ExpressionPtr& memExpr, const ExpressionList& list);

			/**
			 * constructs a parameter
			 */
			VariablePtr genParameter(const location& l, const std::string& name, const TypePtr& type);

			/**
			 * registers the given parameters in the current scope using the names attached to them
			 */
			void registerParameters(const location& l, const VariableList& params, bool _const = false, bool _volatile = false);
			void unregisterParameters();

			/**
			 * constructs a job expression with the given range
			 */
			ExpressionPtr genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound, const ExpressionPtr& expr);

			/**
			 * constructs a job expression with a minimum, maximum, and modulo
			 */
			ExpressionPtr genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound,
			                                    const ExpressionPtr& modExpr, const ExpressionPtr& expr);

			/**
			 * constructs a job expression with a minimum but no maximum
			 */
			ExpressionPtr genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& expr);

			/**
			 * constructs a job expression with a range from one to infinite
			 */
			ExpressionPtr genJobExpr(const location& l, const ExpressionPtr& expr);

			/**
			 * constructs a sync expression
			 */
			ExpressionPtr genSync(const location& l, const ExpressionPtr& expr);

			/**
			 * constructs a syncAll expression
			 */
			ExpressionPtr genSyncAll(const location& l);

			/**
			 * constructs a deref expression
			 */
			ExpressionPtr genDerefExpr(const location& l, const ExpressionPtr& expr);

			/**
			 * constructs an as-expression
			 */
			ExpressionPtr genAsExpr(const location& l, const ExpressionPtr& expr, const TypePtr& type);

			/**
			 * constructs a new variable declaration with a given type
			 */
			DeclarationStmtPtr genVariableDefinition(const location& l, const TypePtr& type, const std::string name, const ExpressionPtr& init);

			/**
			 * constructs a new variable declaration with a given type
			 */
			VariablePtr genVariableDeclaration(const location& l, const TypePtr& type, const std::string name);

			/**
			 * constructs a new declaration statement for the variable with name name and the given init expression
			 */
			DeclarationStmtPtr genDeclarationStmt(const location& l, const std::string name, const ExpressionPtr& init);

			/**
			 * constructs a new declaration statement for the variable with an undefined init expression
			 */
			DeclarationStmtPtr genUndefinedDeclarationStmt(const location& l, const TypePtr& type, const std::string name);

			/**
			 * constructs a new for loop
			 */
			ForStmtPtr genForStmt(const location& l, const TypePtr& iteratorType, const std::string iteratorName,
			                      const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound, const ExpressionPtr& stepExpr, const StatementPtr& body);

			/**
			 * declares a new record type and all it's default members
			 */
			void declareRecordType(const location& l, const std::string name);

			/**
			 * generates a new forward declaration for everything except member fields
			 */
			void genDeclaration(const location& l, const std::string name, const TypePtr& type);

			/**
			 * constructs a literal referencing the current object
			 */
			ExpressionPtr genThis(const location& l);

			ExpressionPtr genMemLambdaReference(const location& l, const string& structName, const string& lambdaName);

		  private:
			GenericTypePtr getThisTypeForLambdaAndFunction(const bool cnst, const bool voltile);

			TypeList getParamTypesForLambdaAndFunction(const location& l, const VariableList& params);

			ExpressionPtr genJobInternal(const location& l, const ExpressionPtr& expr, const std::function<ExpressionPtr(const ExpressionPtr&)>& jobGenerator);

		  public:

			/*
			 * Computes the final result of parsing by using the TU to resolve all symbols and applying some post-processing actions.
			 *
			 * Calling this method will set the variable result to the resulting IR.
			 */
			void computeResult(const NodePtr& fragment);

			// ------------- scope management -------------------

			/**
			 * looks up a declared symbol
			 */
			NodePtr lookupDeclared(const std::string& name);

			/**
			 * looks up a declared symbol in the global scope
			 */
			NodePtr lookupDeclaredInGlobalScope(const std::string& name);

			/**
			 * finds an previously defined expression symbol
			 */
			ExpressionPtr findSymbol(const location& l, const std::string& name);

			/**
			 * finds a previously defined type symbol
			 */
			TypePtr findType(const location& l, const std::string& name);

			/**
			 *  Open a nested scope
			 */
			void openScope();

			/**
			 *  Close a nested scope
			 */
			void closeScope();

			/**
			 * returns the current scope
			 */
			std::shared_ptr<InspireDriver::Scope> getCurrentScope();

			/**
			 * Checks the given symbol name for validity
			 */
			bool checkSymbolName(const location& l, const std::string& name);

			/**
			 * add a symbol declaration to the current scope
			 */
			void declareSymbol(const location& l, const std::string& name, const ExpressionPtr& node);
			void declareSymbol(const location& l, const std::string& name, const NodeFactory& factory);

			/**
			 * add a symbol declaration to the global scope
			 */
			void declareSymbolInGlobalScope(const location& l, const std::string& name, const ExpressionPtr& node);

			/**
			 * checks whether the given symbol is declared in the current scope
			 */
			bool isSymbolDeclaredInCurrentScope(const std::string name);

			/**
			 * checks whether the given symbol is declared in the global scope
			 */
			bool isSymbolDeclaredInGlobalScope(const std::string name);

			/**
			 * add a type declaration to the current scope
			 */
			void declareType(const location& l, const std::string& name, const TypePtr& node);

			/**
			 * checks whether the given type is declared in the current scope
			 */
			bool isTypeDeclaredInCurrentScope(const std::string name);

			/**
			 * add a type alias to the current scope
			 */
			void addTypeAlias(const TypePtr& pattern, const TypePtr& substitute);

			/**
			 * Opens a new record definition (implies opening a new scope)
			 */
			void beginRecord(const location& l, const std::string& name);

			/**
			 * Ends a record definition (implies closing the current scope)
			 */
			void endRecord();

			/**
			 * returns whether we currently are within a record type definition
			 */
			bool isInRecordType();

			/**
			 * Obtains the type of a this pointer in the currently defined record.
			 */
			GenericTypePtr getThisType();

			/**
			 * Utility to mark addresses when parsing addresses (expression overload)
			 */
			ExpressionPtr markAddress(const location& l, const ExpressionPtr& expr);

			/**
			 * Utility to mark addresses when parsing addresses (stmt overload)
			 */
			StatementPtr markAddress(const location& l, const StatementPtr& stmt);

			/**
			 *  support for using keyword (allows to include extensions)
			 */
			void importExtension(const location& l, const std::string& extensionName);

			/**
			 * supports the import of all the symbols and aliases of an extension.
			 */
			void importExtension(const lang::Extension& extension);

			/**
			 * supports the import of an extension.
			 */
			template<typename Extension>
			void importExtension() {
				importExtension(mgr.getLangExtension<Extension>());
			}

			/**
			 *  debug: prints location in parsed text
			 */
			void printLocation(const location& l) const;

			// Error handling.
			void error(const location& l, const std::string& m) const;
			void error(const std::string& m) const;
			bool wereErrors() const;
			void printErrors(std::ostream& out = std::cout, bool color = true) const;
		};

		class AddressMark : public core::value_annotation::copy_on_migration {};

	} // namespace detail
} // namespace parser
} // namespace core
} // namespace insieme

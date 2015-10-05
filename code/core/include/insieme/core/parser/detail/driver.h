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

#include <string>
#include <sstream>
#include <iostream>
#include <map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/tu/ir_translation_unit.h"

#include "insieme/core/parser/detail/scanner.h"
#include "insieme/core/parser/ir_parser.h"

#include "insieme/core/ir_node_annotation.h"

#include "inspire_parser.hpp"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace insieme {
namespace core {
namespace parser {
	namespace detail {


		/**
		 *  The declaration context keeps track of all the symbols available in the scope,
		 *  it is useful to keep track of the let bindings
		 */
		struct DeclarationContext {

			struct Scope {
				type_alias_map alias;
				definition_map types;
				definition_map symbols;
			};

			std::vector<Scope> stack;

			// public:

			DeclarationContext() { open_scope(); /* global scope */ }
			DeclarationContext(const DeclarationContext& o) =delete;

			// ~~~~~~~~~~~~~~~~~~~~~ scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

			void open_scope();
			void close_scope();

			void add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute);
			TypePtr resolve(const TypePtr& type) const;

			bool add_type(const std::string& name, const node_factory& factory);
			bool add_type(const std::string& name, const TypePtr& type);

			bool add_symb(const std::string& name, const node_factory& factory);
			bool add_symb(const std::string& name, const ExpressionPtr& expr);

			NodePtr find_type(const std::string& name) const;
			NodePtr find_symb(const std::string& name) const;

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

			mutable std::vector<ParserError> errors;

			DeclarationContext scopes;

		  public:

			InspireDriver(const std::string& f, NodeManager& mgr);

			virtual ~InspireDriver();

			NodeManager& mgr;
			IRBuilder builder;
			std::string file;
			const std::string& str;

			tu::IRTranslationUnit tu;

			NodePtr result;

			location glob_loc;

		  private:

			std::stringstream ss;
			InspireScanner scanner;
			InspireParser parser;

		  public:

			ProgramPtr parseProgram();

			TypePtr parseType();

			StatementPtr parseStmt();

			ExpressionPtr parseExpression();


			// ~~~~~~~~~~~~~~~~~~~~~~~~  tools  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

			/**
			 * finds an expression symbol previously defined in the scope
			 */
			ExpressionPtr findSymbol(const location& l, const std::string& name);

			/**
			 * finds a type symbol previously defined in the scope
			 */
			TypePtr findType(const location& l, const std::string& name);

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
			 * generate a field access in tagtype (struct/union)
			 */
			ExpressionPtr genFieldAccess(const location& l, const ExpressionPtr&, const std::string& fieldname);

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
			TypePtr genNumericType(const location& l, const string& value) const;

			/**
			 * generates a numeric type representing the given value
			 * @param l: the location where this generic type was found
			 * @param variable: the variable to be represented
			 * @return the corresponding type node
			 */
			TypePtr genNumericType(const location& l, const ExpressionPtr& variable) const;

			/**
			 *  generates a function type
			 */
			TypePtr genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk = FK_PLAIN);

			/**
			 * generate a record type
			 */
			TypePtr genRecordType(const location& l, const NodeType& type, const string& name, const ParentList& parents, const FieldList& fields, const LambdaExprList& ctors,
					const LambdaExprPtr& dtor, const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns);

			/**
			 * check whether type alias can be applied to the given type and applies those.
			 */
			TypePtr resolveTypeAliases(const location& l, const TypePtr& type);

			/**
			 * generates a lambda expression
			 */
			LambdaExprPtr genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body,
			                        const FunctionKind& = FK_PLAIN, bool isLambda = true);

			/**
			 * generates a closure
			 */
			BindExprPtr genClosure(const location& l, const VariableList& params, StatementPtr body);

			/**
			 * generates a call expression
			 */
			ExpressionPtr genCall(const location& l, const ExpressionPtr& func, ExpressionList params);

			/**
			 * constructs a struct expression
			 */
			ExpressionPtr genStructExpression(const location& l, const TypePtr& structType, const ExpressionList& list);

			/**
			 * constructs a union expression
			 */
			ExpressionPtr genUnionExpression(const location& l, const TypePtr& type, const std::string field, const ExpressionPtr& expr);

			/**
			 * constructs a parameter
			 */
			VariablePtr genParameter(const location& l, const std::string& name, const TypePtr& type);

			/**
			 * constructs a job expression with the given range
			 */
			ExpressionPtr genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound, const ExpressionPtr& expr);

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
			DeclarationStmtPtr genVariableDeclaration(const location& l, const TypePtr& type, const std::string name, const ExpressionPtr& init);

			/**
			 * constructs a new for loop
			 */
			ForStmtPtr genForStmt(const location& l, const TypePtr& iteratorType, const std::string iteratorName,
			                      const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound, const ExpressionPtr& stepExpr, const StatementPtr& body);


			/**
			 * stores in the current scope the "this" variable with the given type
			 */
			void add_this(const location& l, const TypePtr& classType);

			/**
			 * add a symbol into the scope
			 */
			void add_symb(const location& l, const std::string& name, const node_factory& factory);

			/**
			 * add a symbol into the scope
			 */
			void add_symb(const location& l, const std::string& name, const ExpressionPtr& node);

			/**
			 * add a symbol into the scope (no location, used when setting up the InspireParser)
			 */
			void add_symb(const std::string& name, const node_factory& factory);

			/**
			 * add a symbol into the scope (no location, used when setting up the InspireParser)
			 */
			void add_symb(const std::string& name, const ExpressionPtr& node);

			/**
			 * add a symbol into the scope
 			*/
			void add_type(const location& l, const std::string& name, const node_factory& factory);

			/**
			 * add a symbol into the scope
			 */
			void add_type(const location& l, const std::string& name, const TypePtr& node);

			/**
			 * add a symbol into the scope (no location, used when setting up the InspireParser)
			 */
			void add_type(const std::string& name, const node_factory& factory);

			/**
			 * add a symbol into the scope (no location, used when setting up the InspireParser)
			 */
			void add_type(const std::string& name, const TypePtr& node);

			/**
			 * add a type alias to the current scope
			 */
			void add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute);

			/**
			 *  Open a nested scope.
			 */
			void open_scope();

			/**
			 *  Close a nested scope.
			 */
			void close_scope();

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
			void import_extension(const location& l, const std::string& extension_name);

			/**
			 * supports the import of all the symbols and aliases of an extension.
			 */
			void import_extension(const lang::Extension& extension);

			/**
			 * supports the import of an extension.
			 */
			template<typename Extension>
			void import_extension() {
				import_extension(mgr.getLangExtension<Extension>());
			}

			/**
			 *  debug: prints location in parsed text
			 */
			void print_location(const location& l) const;

			// Error handling.
			void error(const location& l, const std::string& m) const;
			void error(const std::string& m) const;
			bool where_errors() const;
			void print_errors(std::ostream& out = std::cout, bool color = true) const;
		};

		class AddressMark : public core::value_annotation::copy_on_migration {};

	} // namespace detail
} // namespace parser
} // namespace core
} // namespace insieme

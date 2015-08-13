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
#include "insieme/core/parser3/ir_parser.h"
#include "insieme/core/parser3/detail/scanner.h"

#include "inspire_parser.hpp"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace insieme {
namespace core {
namespace parser3 {
	namespace detail {


		/**
		 *  The declaration context keeps track of all the symbols available in the scope,
		 *  it is useful to keep track of the let bindings
		 */
		struct DeclarationContext {

			struct Scope {
				definition_map symbols;
				type_alias_map alias;
			};

			std::vector<Scope> stack;

			// public:

			DeclarationContext() { open_scope(); /* global scope */ }
			DeclarationContext(const DeclarationContext& o) =default;

			// ~~~~~~~~~~~~~~~~~~~~~ scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //

			void open_scope(const std::string& msg = "");
			void close_scope(const std::string& msg = "");

			bool add_symb(const std::string& name, const node_factory& factory);
			NodePtr find(const std::string& name) const;

			void add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute);
			TypePtr resolve(const TypePtr& type) const;
		};


		// Conducting the whole scanning and parsing of Calc++.
		class inspire_driver {
			struct t_error {
				location l;
				std::string msg;
				t_error(const location& l, const std::string& msg) : l(l), msg(msg) {}
			};
			mutable std::vector<t_error> errors;

			DeclarationContext scopes;

		  public:
			inspire_driver(const std::string& f, NodeManager& nk, const DeclarationContext& ctx = DeclarationContext());
			virtual ~inspire_driver();

			NodeManager& mgr;
			IRBuilder builder;
			std::string file;
			const std::string& str;
			NodePtr result;

			location glob_loc;

		  private:
			std::stringstream ss;
			inspire_scanner scanner;
			inspire_parser parser;

		  public:
			unsigned let_count;
			unsigned inhibit_building_count;

		  private:
			struct Lambda_let {
				TypePtr retType;
				VariableList params;
				std::string expression;
				FunctionKind fk;

				Lambda_let(const TypePtr& retType, const VariableList& params, const std::string& expression, const FunctionKind& fk)
				    : retType(retType), params(params.begin(), params.end()), expression(expression), fk(fk) {}
			};
			std::vector<std::string> let_names;
			std::vector<Lambda_let> lambda_lets;
			std::vector<TypePtr> type_lets;
			std::vector<ExpressionPtr> expr_lets;


		  public:
			ProgramPtr parseProgram();
			TypePtr parseType();
			StatementPtr parseStmt();
			ExpressionPtr parseExpression();

			// ~~~~~~~~~~~~~~~~~~~~~~~~  tools  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			/**
			 * finds an expression symbol previously defined in the scoope
			 */
			ExpressionPtr findSymbol(const location& l, const std::string& name);
			/**
			 * finds a type symbol previously defined in the scoope
			 */
			TypePtr findType(const location& l, const std::string& name);

			/**
			 *  handles apropiate type for expression to be used in an operation
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
			 * generates a generic type
			 * @param l: the location where this generic type was found
			 * @param name: the name of type
			 * @param parents: list of parent types if any
			 * @param params: list of type paramenters
			 * @param IntParamList: list of int type paramenters
			 */
			TypePtr genGenericType(const location& l, const std::string& name, const ParentList& parents, const TypeList& params);

			/**
			 * generates a numeric type representing the given value
			 * @param l: the location where this generic type was found
			 * @param value: the value to be represented
			 * @return the corresponding type node
			 */
			TypePtr genNumericType(const location& l, const string& value) const;

			/**
			 *  generates a function type
			 */
			TypePtr genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk = FK_PLAIN);

			/**
			 * check whether type alias can be applied to the given type and applies those.
			 */
			TypePtr resolveTypeAliases(const location& l, const TypePtr& type);

			/**
			 * generates a lambda expression
			 */
			ExpressionPtr genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body,
			                        const FunctionKind& = FK_PLAIN);

			/**
			 * genereates a closure
			 */
			ExpressionPtr genClosure(const location& l, const VariableList& params, StatementPtr body);

			/**
			 * generates a call expression
			 */
			ExpressionPtr genCall(const location& l, const ExpressionPtr& func, ExpressionList params);

			/**
			 * constructs a struct/union expression
			 */
			ExpressionPtr genTagExpression(const location& l, const TypePtr& structType, const ExpressionList& list);


			/* ~~~~~~~~~~~~~~~~~~~~~~~ let bindings management ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

			/**
			 * stores in the current state a name for a let binding being defined
			 */
			void add_let_name(const location& l, const std::string& name);

			/**
			 * stores in the current state the type and code (text) for a lambda being defined
			 */
			void add_let_lambda(const location& l, const location& bodyb, const location& bodye, const TypePtr& retType,
			                    const VariableList& params = VariableList(), const FunctionKind& fk = FK_PLAIN);
			/**
			 * stores in the current state the type for a let type being defined
			 */
			void add_let_type(const location& l, const TypePtr& type);

			/**
			 * stores in the current state the expression for a let expression being defined
			 */
			void add_let_expression(const location& l, const ExpressionPtr& expr);

			/**
			 * stores in the current scope the "this" variable with the given type
			 */
			void add_this(const location& l, const TypePtr& classType);

			/**
			 * finish the let statement, matches names and definitions of types/lambdas/expresions
			 */
			void close_let_statement(const location& l);

			/**
			 * add a symbol into the scope
			 */
			void add_symb(const location& l, const std::string& name, const node_factory& factory);

			/**
			 * add a symbol into the scope
			 */
			void add_symb(const location& l, const std::string& name, const NodePtr& node);

			/**
			 * add a symbol into the scope (no location, used when setting up the inspire_parser)
			 */
			void add_symb(const std::string& name, const node_factory& factory);

			/**
			 * add a symbol into the scope (no location, used when setting up the inspire_parser)
			 */
			void add_symb(const std::string& name, const NodePtr& node);

			/**
			 * add a type alias to the current scope
			 */
			void add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute);

			/**
			 *  Open a frame in the scope manager
			 */
			void open_scope(const location& l, const std::string&);

			/**
			 *  Close a frame in the scope manager
			 */
			void close_scope(const location& l, const std::string&);

			/**
			 * Utility to mark addresses when parsing addresses (expression overload)
			 */
			ExpressionPtr mark_address(const location& l, const ExpressionPtr& expr);

			/**
			 * Utility to mark addresses when parsing addresses (stmt overload)
			 */
			StatementPtr mark_address(const location& l, const StatementPtr& stmt);

			/**
			 * queries whenever the core generation is inhibited
			 *  syntatic parsing, no build (this can be used to jump over large ranges of code to do subscooping
			 */
			bool inhibit_building() const;

			/**
			 *  enambes code generation inhibition
			 */
			void set_inhibit(bool flag = true);

			/**
			 *  support for using keyword (allows to include extensions)
			 */
			void using_scope_handle(const location& l, const std::vector<std::string>& extension_names);

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

		class AddressMark {};

	} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

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

#include "insieme/core/ast_visitor.h"

#include "insieme/simple_backend/backend_convert.h"
#include "insieme/simple_backend/formatting/operator_formatting.h"

namespace insieme {
namespace simple_backend {

	/**
	 * Central simple_backend statement and expression conversion class. It is recursively processing IR DAGs
	 * and generates corresponding C code.
	 */
	class StmtConverter : private core::ASTVisitor<>, private boost::noncopyable {

		/**
		 * A reference to the central container maintaining all the instances of the required manager.
		 */
		const Converter& cc;

		/**
		 * The table handling operator specific formatting rules for operators.
		 */
		const formatting::FormatTable formats;

		/**
		 * A pointer to the code fragment currently produced by this converter.
		 */
		CodeFragmentPtr currentCodeFragment;

	public:

		/**
		 * Creates a new statement and expression converter using the given context and formatting rules.
		 *
		 * @param context the context to be used by the resulting converter
		 * @param formats the format table to be used to generate code for various operators
		 */
		StmtConverter(Converter& context, const formatting::FormatTable& formats)
			: ASTVisitor<>(false), cc(context), formats(formats) { };

		/**
		 * Obtains a reference to the conversion context used by this converter.
		 *
		 * @return the conversion context used by this converter
		 */
		const Converter& getConversionContext() const {
			return cc;
		}

		/**
		 * Obtains a reference to the code fragment currently produced by this converter.
		 *
		 * @return the currently generated code fragment (may be partially completed)
		 */
		const CodeFragmentPtr& getCurrentCodeFragment() const {
			return currentCodeFragment;
		}

		/**
		 * Obtains a list of headers required by this statement converter.
		 *
		 * @return a vector of all headers required by this converter (including the #include)
		 */
		virtual vector<string> getHeaderDefinitions();

		/**
		 * Converts the given node and writes the result into the current code fragment.
		 *
		 * @param node the node to be processes
		 */
		void convert(const core::NodePtr& node);

		/**
		 * Instructs this statement convert to process the given IR node and append
		 * the results to the given code fragment.
		 *
		 * @param node the node to be processes
		 * @param fragment the code fragment the code should be appended to
		 */
		void convert(const core::NodePtr& node, const CodeFragmentPtr& fragment);

		/**
		 * Instructs this statement and expression converter to process the given expression and to
		 * produce a result which can be forwarded to an external function.
		 *
		 * @param expression the expression which's result should be represented in a way it can
		 *     				 be passed to an external function.
		 */
		void convertAsParameterToExternal(const core::ExpressionPtr& expression);

		/**
		 * Instructs this statement and expression converter to process the given expression and to
		 * produce a result which can be forwarded to an external function. The result will be
		 * added to the given code fragment.
		 *
		 * @param expression the expression which's result should be represented in a way it can
		 *     				 be passed to an external function.
		 * @param fragment the code fragment the code should be appended to
		 */
		void convertAsParameterToExternal(const core::ExpressionPtr& expression, const CodeFragmentPtr& fragment);

		/**
		 * Creates the code required to initialize the members of the given structs. The code will consist
		 * of a sequence of assignment statements targeting the various members of the given struct.
		 *
		 * @param target the target of the initialization
		 * @param value the call producing the value the target should be initialized with
		 */
		void initStruct(const core::ExpressionPtr& target, const core::ExpressionPtr& init);

	protected:

		// --------------------------------------------------------------------------------
		// -- node processing -------------------------------------------------------------
		// --------------------------------------------------------------------------------

		void visitNode(const core::NodePtr& node);

		void visitProgram(const core::ProgramPtr&);

		////////////////////////////////////////////////////////////////////////// Statements

		void visitBreakStmt(const core::BreakStmtPtr&);

		void visitCompoundStmt(const core::CompoundStmtPtr& ptr);

		void visitContinueStmt(const core::ContinueStmtPtr&);

		void visitDeclarationStmt(const core::DeclarationStmtPtr& ptr);

		void visitForStmt(const core::ForStmtPtr& ptr);

		void visitIfStmt(const core::IfStmtPtr& ptr);

		void visitWhileStmt(const core::WhileStmtPtr& ptr);

		void visitReturnStmt(const core::ReturnStmtPtr& ptr);

		void visitSwitchStmt(const core::SwitchStmtPtr& ptr);


		////////////////////////////////////////////////////////////////////////// Expressions

		void visitCallExpr(const core::CallExprPtr& ptr);

		void visitBindExpr(const core::BindExprPtr& ptr);

		void visitCastExpr(const core::CastExprPtr& ptr);

		void visitJobExpr(const core::JobExprPtr& ptr);

		void visitLambdaExpr(const core::LambdaExprPtr& ptr);

		void visitLiteral(const core::LiteralPtr& ptr);

		void visitMarkerExpr(const core::MarkerExprPtr& ptr);

		void visitMarkerStmt(const core::MarkerStmtPtr& ptr);

		void visitStructExpr(const core::StructExprPtr& ptr);

		void visitUnionExpr(const core::UnionExprPtr& ptr);

		void visitTupleExpr(const core::TupleExprPtr& ptr) {
			// TODO: replace this with a C99 solution
			const CodeFragmentPtr& code = getCurrentCodeFragment();
			code << "std::make_tuple(";
			auto exps = ptr->getExpressions();
			if(exps.size() > 0) {
				visit(exps.front());
				for_each(exps.cbegin()+1, exps.cend(), [&](const core::ExpressionPtr& cur) {
					code << ", ";
					this->visit(cur);
				});
			}
			code << ")";
		}

		void visitMemberAccessExpr(const core::MemberAccessExprPtr& ptr);

		void visitVariable(const core::VariablePtr& ptr);

		void visitVectorExpr(const core::VectorExprPtr& ptr);

	};

}
}

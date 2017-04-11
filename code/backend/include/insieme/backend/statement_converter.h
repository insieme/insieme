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

#include <memory>
#include <set>
#include <functional>

#include "insieme/backend/converter.h"
#include "insieme/backend/c_ast/c_ast.h"

#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace backend {

	typedef std::function<c_ast::NodePtr(ConversionContext&, const core::NodePtr&)> StmtHandler;
	typedef vector<StmtHandler> StmtHandlerList;


	class StmtConverter : public core::IRVisitor<c_ast::NodePtr, core::Pointer, ConversionContext&> {
		const Converter& converter;

		StmtHandlerList stmtHandler;

	  public:
		StmtConverter(const Converter& converter, const StmtHandlerList& stmtHandler = StmtHandlerList())
		    : core::IRVisitor<c_ast::NodePtr, core::Pointer, ConversionContext&>(true), converter(converter), stmtHandler(stmtHandler) {}

		c_ast::NodePtr convert(ConversionContext& context, const core::NodePtr& node);

		void addStmtHandler(const StmtHandler& handler) {
			stmtHandler.push_back(handler);
		}

		void addStmtHandler(const StmtHandlerList& list) {
			stmtHandler.insert(stmtHandler.end(), list.begin(), list.end());
		}

		////////////////////////////////////////////////////////////////////////// Utilities

		virtual c_ast::NodePtr visit(const core::NodePtr&, ConversionContext& context);

		template <typename T>
		Ptr<T> convert(ConversionContext& context, const core::NodePtr& node) {
			auto res = visit(node, context);
			assert(dynamic_pointer_cast<T>(res) && "Invalid specified type!");
			return static_pointer_cast<T>(res);
		}

		c_ast::TypePtr convertType(ConversionContext& context, const core::TypePtr& type) {
			return convert<c_ast::Type>(context, type);
		}

		c_ast::ExpressionPtr convertExpression(ConversionContext& context, const core::ExpressionPtr& expr) {
			return convert<c_ast::Expression>(context, expr);
		}

		c_ast::StatementPtr convertStmt(ConversionContext& context, const core::StatementPtr& stmt) {
			return convert<c_ast::Statement>(context, stmt);
		}

		c_ast::ExpressionPtr convertInitExpression(ConversionContext& context, const core::TypePtr& targetType, const core::ExpressionPtr& initValue);

	  protected:
		////////////////////////////////////////////////////////////////////////// Basic Elements

		c_ast::NodePtr visitNode(const core::NodePtr& node, ConversionContext& context);

		c_ast::NodePtr visitType(const core::TypePtr& type, ConversionContext& context);

		c_ast::NodePtr visitProgram(const core::ProgramPtr& node, ConversionContext& context);

		////////////////////////////////////////////////////////////////////////// Statements

		c_ast::NodePtr visitBreakStmt(const core::BreakStmtPtr&, ConversionContext& context);

		c_ast::NodePtr visitCompoundStmt(const core::CompoundStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitContinueStmt(const core::ContinueStmtPtr&, ConversionContext& context);

		c_ast::NodePtr visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitForStmt(const core::ForStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitIfStmt(const core::IfStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitWhileStmt(const core::WhileStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitTryCatchStmt(const core::TryCatchStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitReturnStmt(const core::ReturnStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitThrowStmt(const core::ThrowStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitGotoStmt(const core::GotoStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitLabelStmt(const core::LabelStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitSwitchStmt(const core::SwitchStmtPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitMarkerStmt(const core::MarkerStmtPtr& ptr, ConversionContext& context);


		////////////////////////////////////////////////////////////////////////// Expressions

		c_ast::NodePtr visitCallExpr(const core::CallExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitBindExpr(const core::BindExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitCastExpr(const core::CastExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitJobExpr(const core::JobExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitLambdaExpr(const core::LambdaExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitLiteral(const core::LiteralPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitMarkerExpr(const core::MarkerExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitInitExpr(const core::InitExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitTupleExpr(const core::TupleExprPtr& ptr, ConversionContext& context);

		c_ast::NodePtr visitVariable(const core::VariablePtr& ptr, ConversionContext& context);

	};

	// deal with a very specific case: construction of an object in-place as the argument
	//                   or return value of a function (without any copies being created)
	// - in IR, as a non-materializing constructor with a ref_decl as its this parameter
	// - in C++, it's implemented by performing an in-place construction using the {} syntax
	// in case direct construction is required, returns the required C AST, otherwise returns a null pointer
	c_ast::ExpressionPtr checkDirectConstruction(ConversionContext& context, const core::TypePtr& targetT, const core::ExpressionPtr& arg);

} // end namespace backend
} // end namespace insieme

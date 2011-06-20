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

#include <memory>

#include "insieme/backend/converter.h"

#include "insieme/core/ast_visitor.h"

namespace insieme {
namespace backend {

	namespace c_ast {

		class Node;
		typedef Ptr<Node> NodePtr;

		class CNodeManager;
		typedef std::shared_ptr<CNodeManager> SharedCNodeManager;

		class CCode;
		typedef std::shared_ptr<CCode> CCodePtr;

	}

	// a forward declaration - implementation is hidden
	class StmtConversionContext;


	class StmtConverter : public core::ASTVisitor<c_ast::NodePtr, core::Pointer, StmtConversionContext&> {

		const Converter& converter;

	public:

		StmtConverter(const Converter& converter)
			: core::ASTVisitor<c_ast::NodePtr, core::Pointer, StmtConversionContext&>(true), converter(converter) {}

		c_ast::CCodePtr convert(const core::NodePtr& node);

	protected:

		////////////////////////////////////////////////////////////////////////// Basic Elements

		c_ast::NodePtr visitNode(const core::NodePtr& node, StmtConversionContext& context);

		c_ast::NodePtr visitProgram(const core::ProgramPtr& node, StmtConversionContext& context);

		////////////////////////////////////////////////////////////////////////// Statements

		c_ast::NodePtr visitBreakStmt(const core::BreakStmtPtr&, StmtConversionContext& context);

		c_ast::NodePtr visitCompoundStmt(const core::CompoundStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitContinueStmt(const core::ContinueStmtPtr&, StmtConversionContext& context);

		c_ast::NodePtr visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitForStmt(const core::ForStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitIfStmt(const core::IfStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitWhileStmt(const core::WhileStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitReturnStmt(const core::ReturnStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitSwitchStmt(const core::SwitchStmtPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitMarkerStmt(const core::MarkerStmtPtr& ptr, StmtConversionContext& context);


		////////////////////////////////////////////////////////////////////////// Expressions

		c_ast::NodePtr visitCallExpr(const core::CallExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitBindExpr(const core::BindExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitCastExpr(const core::CastExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitJobExpr(const core::JobExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitLambdaExpr(const core::LambdaExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitLiteral(const core::LiteralPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitMarkerExpr(const core::MarkerExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitStructExpr(const core::StructExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitUnionExpr(const core::UnionExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitTupleExpr(const core::TupleExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitMemberAccessExpr(const core::MemberAccessExprPtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitVariable(const core::VariablePtr& ptr, StmtConversionContext& context);

		c_ast::NodePtr visitVectorExpr(const core::VectorExprPtr& ptr, StmtConversionContext& context);
	};


} // end namespace backend
} // end namespace insieme

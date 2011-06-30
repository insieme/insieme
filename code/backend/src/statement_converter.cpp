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

#include "insieme/backend/statement_converter.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {

	// --------------- Conversion Context struct ---------------

	struct StmtConversionContext {
		c_ast::SharedCNodeManager cNodeManager;
		std::set<c_ast::CodeFragmentPtr>& dependencies;
		StmtConversionContext(const c_ast::SharedCNodeManager& manager, std::set<c_ast::CodeFragmentPtr>& dependencies)
			: cNodeManager(manager), dependencies(dependencies) {}
	};


	namespace {

		c_ast::CCodeFragmentPtr toCodeFragment(StmtConversionContext context, c_ast::NodePtr code) {
			c_ast::CCodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(context.cNodeManager, code);
			fragment->addDependencies(context.dependencies);
			return fragment;
		}

	}


	// --------------- conversion operations -------------------

	c_ast::NodePtr StmtConverter::convert(const core::NodePtr& node, std::set<c_ast::CodeFragmentPtr>& dependencies) {
		// create a context for the conversion and conduct procedure
		StmtConversionContext context(converter.getCNodeManager(), dependencies);
		return visit(node, context);
	}

	c_ast::CCodePtr StmtConverter::convert(const core::NodePtr& node) {

		// construct target code
		std::set<c_ast::CodeFragmentPtr> dependencies;
		auto fragment = c_ast::CCodeFragment::createNew(converter.getCNodeManager(), convert(node, dependencies));
		fragment->addDependencies(dependencies);
		return std::make_shared<c_ast::CCode>(node, fragment);
	}

	c_ast::NodePtr StmtConverter::visitNode(const core::NodePtr& node, StmtConversionContext& context) {
		// default handling of unsupported nodes => produce comment
		return context.cNodeManager->create<c_ast::Comment>("Unsupported: " + toString(node));
	}


	c_ast::NodePtr StmtConverter::visitProgram(const core::ProgramPtr& node, StmtConversionContext& context) {

		// get shared C Node Manager reference
		const c_ast::SharedCNodeManager& manager = context.cNodeManager;

		// program is not producing any C code => just dependencies
		for_each(node->getEntryPoints(), [&](const core::ExpressionPtr& entryPoint) {

			// create a new context
			std::set<c_ast::CodeFragmentPtr> dependencies;
			StmtConversionContext entryContext(manager, dependencies);

			// create new fragment
			auto fragment = toCodeFragment(entryContext, this->visit(entryPoint, entryContext));

			// add converted fragment to dependency list
			context.dependencies.insert(fragment);

		});

		// create empty node (program does not represent any code)
		return 0;
	}


	////////////////////////////////////////////////////////////////////////// Expressions

	c_ast::NodePtr StmtConverter::visitCallExpr(const core::CallExprPtr& ptr, StmtConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Literal>("/*UNSUPPORTED*/");
	}

	c_ast::NodePtr StmtConverter::visitBindExpr(const core::BindExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitCastExpr(const core::CastExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitJobExpr(const core::JobExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitLambdaExpr(const core::LambdaExprPtr& ptr, StmtConversionContext& context) {

		// obtain function manager reference
		FunctionManager& funManager = converter.getFunctionManager();

		// create function

//		CodeFragmentPtr& code = currentCodeFragment;
//
//		// obtain name of resulting function type and add cast
//		FunctionTypePtr funType = static_pointer_cast<const FunctionType>(ptr->getType());
//		const TypeManager::FunctionTypeInfo& info = cc.getTypeManager().getFunctionTypeInfo(funType);
//		const string& name = info.closureName;
//		code << "(" << name << "*)" << name << "_ctr(";
//
//		// allocate memory
//		code << "(" << name << "*)alloca(sizeof(" + name + ")),";
//		code << "&" << funManager.getFunctionName(currentCodeFragment, ptr);
//		code << "_wrap)";

		// get reference to C node manager
		const c_ast::SharedCNodeManager& astManager = context.cNodeManager;

		return astManager->create<c_ast::Comment>("I owe you a lambda!");

//		return visitNode(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitLiteral(const core::LiteralPtr& ptr, StmtConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Literal>(ptr->getValue());
	}

	c_ast::NodePtr StmtConverter::visitStructExpr(const core::StructExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitUnionExpr(const core::UnionExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitTupleExpr(const core::TupleExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitMemberAccessExpr(const core::MemberAccessExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitVariable(const core::VariablePtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitVectorExpr(const core::VectorExprPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitMarkerExpr(const core::MarkerExprPtr& ptr, StmtConversionContext& context) {
		// markers are just ignored
		return visit(ptr->getSubExpression(), context);
	}



	////////////////////////////////////////////////////////////////////////// Statements

	c_ast::NodePtr StmtConverter::visitBreakStmt(const core::BreakStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitCompoundStmt(const core::CompoundStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitContinueStmt(const core::ContinueStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitForStmt(const core::ForStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitIfStmt(const core::IfStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitWhileStmt(const core::WhileStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitReturnStmt(const core::ReturnStmtPtr& ptr, StmtConversionContext& context) {
		// wrap sub-expression into return expression
		return context.cNodeManager->create<c_ast::Return>(convertExpression(ptr->getReturnExpr(), context));
	}

	c_ast::NodePtr StmtConverter::visitSwitchStmt(const core::SwitchStmtPtr& ptr, StmtConversionContext& context) { return visitNode(ptr, context); }

	c_ast::NodePtr StmtConverter::visitMarkerStmt(const core::MarkerStmtPtr& ptr, StmtConversionContext& context) {
		// markers are just ignored
		return visit(ptr->getSubStatement(), context);
	}


} // end namespace backend
} // end namespace insieme

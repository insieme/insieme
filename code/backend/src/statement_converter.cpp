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

#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/variable_manager.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace backend {

	// --------------- Conversion Context struct ---------------

	namespace {

		c_ast::CCodeFragmentPtr toCodeFragment(ConversionContext context, c_ast::NodePtr code) {
			c_ast::CCodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(context.getCNodeManager(), code);
			fragment->addDependencies(context.getDependencies());
			return fragment;
		}

	}


	// --------------- conversion operations -------------------

	c_ast::NodePtr StmtConverter::convert(ConversionContext& context, const core::NodePtr& node) {
		// create a context for the conversion and conduct procedure
		return visit(node, context);
	}


	////////////////////////////////////////////////////////////////////////// Basic Nodes

	c_ast::NodePtr StmtConverter::visitNode(const core::NodePtr& node, ConversionContext& context) {
		// default handling of unsupported nodes => produce comment
		return converter.getCNodeManager()->create<c_ast::Comment>("Unsupported: " + toString(node));
	}

	c_ast::NodePtr StmtConverter::visitType(const core::TypePtr& type, ConversionContext& context) {
		// obtain type information, add dependency and return type name
		const TypeInfo& info = converter.getTypeManager().getTypeInfo(type);
		context.getDependencies().insert(info.definition);
		return info.rValueType;
	}


	c_ast::NodePtr StmtConverter::visitProgram(const core::ProgramPtr& node, ConversionContext& context) {

		// get shared C Node Manager reference
		const c_ast::SharedCNodeManager& manager = converter.getCNodeManager();

		// program is not producing any C code => just dependencies
		for_each(node->getEntryPoints(), [&](const core::ExpressionPtr& entryPoint) {

			// create a new context
			VariableManager varManager;
			c_ast::DependencySet dependencies;
			ConversionContext entryContext(converter, dependencies, varManager);

			c_ast::CodeFragmentPtr fragment;
			if (entryPoint->getNodeType() == core::NT_LambdaExpr) {
				// handle function-entry point specially
				core::LambdaExprPtr lambda = static_pointer_cast<const core::LambdaExpr>(entryPoint);
				fragment = converter.getFunctionManager().getInfo(lambda).definition;
			} else {
				// use default conversion
				fragment = toCodeFragment(entryContext, this->visit(entryPoint, entryContext));
			}

			// add converted fragment to dependency list
			context.getDependencies().insert(fragment);

		});

		// create empty node (program does not represent any code)
		return manager->create<c_ast::OpaqueCode>("");
	}


	////////////////////////////////////////////////////////////////////////// Expressions

	c_ast::NodePtr StmtConverter::visitCallExpr(const core::CallExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getCall(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitBindExpr(const core::BindExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getValue(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitCastExpr(const core::CastExprPtr& ptr, ConversionContext& context) {
		return c_ast::cast(converter.getTypeManager().getTypeInfo(ptr->getType()).rValueType, visit(ptr->getSubExpression(), context));
	}

	c_ast::NodePtr StmtConverter::visitJobExpr(const core::JobExprPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Literal>("JOB-UNSUPPORTED");
	}

	c_ast::NodePtr StmtConverter::visitLambdaExpr(const core::LambdaExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getValue(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitLiteral(const core::LiteralPtr& ptr, ConversionContext& context) {
		// Function literals are handled by function manager
		if (ptr->getType()->getNodeType() == core::NT_FunctionType) {
			return converter.getFunctionManager().getValue(ptr, context);
		}

		// the rest is just converted into a ordinary literal
		return converter.getCNodeManager()->create<c_ast::Literal>(ptr->getValue());
	}

	c_ast::NodePtr StmtConverter::visitStructExpr(const core::StructExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

		// get type and create empty init expression
		c_ast::TypePtr type = converter.getTypeManager().getTypeInfo(ptr->getType()).rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// append initialization values
		::transform(ptr->getMembers(), std::back_inserter(init->values),
				[&](const core::StructExpr::Member& cur) {
					return convert(context, cur.second);
		});

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitUnionExpr(const core::UnionExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding union
		//     (<type>){<single member>}

		// get type and create init expression
		c_ast::TypePtr type = converter.getTypeManager().getTypeInfo(ptr->getType()).rValueType;
		return c_ast::init(type, convert(context, ptr->getMember()));
	}

	c_ast::NodePtr StmtConverter::visitTupleExpr(const core::TupleExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

		// get type and create empty init expression
		c_ast::TypePtr type = converter.getTypeManager().getTypeInfo(ptr->getType()).rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// append initialization values
		::transform(ptr->getExpressions(), std::back_inserter(init->values),
				[&](const core::ExpressionPtr& cur) {
					return convert(context, cur);
		});

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitMemberAccessExpr(const core::MemberAccessExprPtr& ptr, ConversionContext& context) {
		// simply return a C-AST structure accessing the requested member
		return c_ast::access(convert(context, ptr->getSubExpression()), ptr->getMemberName()->getName());
	}

	c_ast::NodePtr StmtConverter::visitVariable(const core::VariablePtr& ptr, ConversionContext& context) {
		// just look up variable within variable manager and return variable token ...
		const VariableInfo& info = context.getVariableManager().getInfos(ptr);
		return info.var;
	}

	c_ast::NodePtr StmtConverter::visitVectorExpr(const core::VectorExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct - where one value is a vector
		//     (<type>){(<vector type>){<list of members>}}

		// get type and create empty init expression
		const TypeInfo& info = converter.getTypeManager().getTypeInfo(ptr->getType());
		c_ast::TypePtr type = info.rValueType;
		c_ast::TypePtr vectorType = info.externalType;

		// create inner vector init and append initialization values
		c_ast::InitializerPtr vectorInit = c_ast::init(vectorType);
		::transform(ptr->getExpressions(), std::back_inserter(vectorInit->values),
				[&](const core::ExpressionPtr& cur) {
					return convert(context, cur);
		});

		// create and return out initializer
		return c_ast::init(type, vectorInit);
	}

	c_ast::NodePtr StmtConverter::visitMarkerExpr(const core::MarkerExprPtr& ptr, ConversionContext& context) {
		// markers are just ignored
		return visit(ptr->getSubExpression(), context);
	}



	////////////////////////////////////////////////////////////////////////// Statements

	c_ast::NodePtr StmtConverter::visitBreakStmt(const core::BreakStmtPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Break>();
	}

	c_ast::NodePtr StmtConverter::visitCompoundStmt(const core::CompoundStmtPtr& ptr, ConversionContext& context) {
		c_ast::CompoundPtr res = converter.getCNodeManager()->create<c_ast::Compound>();
		::transform(ptr->getStatements(), std::back_inserter(res->statements),
				[&](const core::StatementPtr& cur) {
					return this->visit(cur, context);
		});
		return res;
	}

	c_ast::NodePtr StmtConverter::visitContinueStmt(const core::ContinueStmtPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Continue>();
	}

	c_ast::NodePtr StmtConverter::visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, ConversionContext& context) {

		// goal: create a variable declaration and register new variable within variable manager

		auto& basic = converter.getNodeManager().getBasicGenerator();
		auto manager = converter.getCNodeManager();

		core::VariablePtr var = ptr->getVariable();
		core::ExpressionPtr init = ptr->getInitialization();

		// decide storage location of variable
		VariableInfo::MemoryLocation location = VariableInfo::NONE;
		if (core::analysis::hasRefType(var)) {
			if (core::analysis::isCallOf(init, basic.getRefVar())) {
				location = VariableInfo::DIRECT;
			} else {
				location = VariableInfo::INDIRECT;
			}
		}

		// register variable information
		const VariableInfo& info = context.getVariableManager().addInfo(converter, var, location);

		// TODO: handle initUndefine and init struct cases

		// create declaration statement
		return manager->create<c_ast::VarDecl>(info.var, convertExpression(context, init));
	}

	c_ast::NodePtr StmtConverter::visitForStmt(const core::ForStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		auto varManager = context.getVariableManager();
		auto var = ptr->getDeclaration()->getVariable();

		// get induction variable info
		const VariableInfo& info = varManager.addInfo(converter, var, VariableInfo::NONE);

		// create init, check, step and body
		c_ast::VarDeclPtr init = manager->create<c_ast::VarDecl>(info.var, convertExpression(context, ptr->getDeclaration()->getInitialization()));
		c_ast::ExpressionPtr check = c_ast::lt(info.var, convertExpression(context, ptr->getEnd()));
		c_ast::ExpressionPtr step = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info.var, convertExpression(context, ptr->getStep()));
		c_ast::StatementPtr body = convertStmt(context, ptr->getBody());

		// remove variable info since no longer in scope
		varManager.remInfo(var);

		// combine all into a for
		return manager->create<c_ast::For>(init, check, step, body);
	}

	c_ast::NodePtr StmtConverter::visitIfStmt(const core::IfStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		// create condition, then and else branch
		c_ast::ExpressionPtr condition = convertExpression(context, ptr->getCondition());
		c_ast::StatementPtr thenBranch = convertStmt(context, ptr->getThenBody());
		c_ast::StatementPtr elseBranch = convertStmt(context, ptr->getElseBody());

		return manager->create<c_ast::If>(condition, thenBranch, elseBranch);
	}

	c_ast::NodePtr StmtConverter::visitWhileStmt(const core::WhileStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		// create condition, then and else branch
		c_ast::ExpressionPtr condition = convertExpression(context, ptr->getCondition());
		c_ast::StatementPtr body = convertStmt(context, ptr->getBody());

		return manager->create<c_ast::While>(condition, body);
	}

	c_ast::NodePtr StmtConverter::visitReturnStmt(const core::ReturnStmtPtr& ptr, ConversionContext& context) {
		// wrap sub-expression into return expression
		return context.getCNodeManager()->create<c_ast::Return>(convertExpression(context, ptr->getReturnExpr()));
	}

	c_ast::NodePtr StmtConverter::visitSwitchStmt(const core::SwitchStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		// create empty switch ...
		c_ast::SwitchPtr res = manager->create<c_ast::Switch>(convertExpression(context, ptr->getSwitchExpr()));

		// add cases ..
		::transform(ptr->getCases(), std::back_inserter(res->cases), [&](const core::SwitchStmt::Case& cur) {
			return std::make_pair(convertExpression(context, cur.first), convertStmt(context, cur.second));
		});

		// add default ..
		res->defaultBranch = convertStmt(context, ptr->getDefaultCase());

		// .. and done
		return res;
	}

	c_ast::NodePtr StmtConverter::visitMarkerStmt(const core::MarkerStmtPtr& ptr, ConversionContext& context) {
		// markers are just ignored
		return visit(ptr->getSubStatement(), context);
	}


} // end namespace backend
} // end namespace insieme

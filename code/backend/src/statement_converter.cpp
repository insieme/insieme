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

#include "insieme/backend/function_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/name_manager.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/variable_manager.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	// --------------- Conversion Context struct ---------------

	namespace {

		c_ast::CCodeFragmentPtr toCodeFragment(const ConversionContext& context, c_ast::NodePtr code) {
			c_ast::CCodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(context.getConverter().getFragmentManager(), code);
			fragment->addDependencies(context.getDependencies());
			return fragment;
		}

	}


	// --------------- conversion operations -------------------

	c_ast::NodePtr StmtConverter::convert(ConversionContext& context, const core::NodePtr& node) {
		// create a context for the conversion and conduct procedure
		return visit(node, context);
	}


	////////////////////////////////////////////////////////////////////////// Core Visitor

	c_ast::NodePtr StmtConverter::visit(const core::NodePtr& node, ConversionContext& context) {
		// first ask the handlers
		if (!stmtHandler.empty()) {
			for(auto it = stmtHandler.begin(); it != stmtHandler.end(); ++it) {
				c_ast::NodePtr res = (*it)(context, node);
				if (res) {
					return res;
				}
			}
		}

		// use default conversion
		return IRVisitor::visit(node, context);
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

			// fix name of main entry
			if (node->getEntryPoints().size() == static_cast<std::size_t>(1)) {
				context.getConverter().getNameManager().setName(entryPoint, "main");
			}

			// create a new context
			ConversionContext entryContext(converter);

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

		// convert literal
		c_ast::ExpressionPtr res = converter.getCNodeManager()->create<c_ast::Literal>(ptr->getStringValue());

		// special handling for the global struct
		if (!ptr->getStringValue().compare(0, IRExtensions::GLOBAL_ID.size(), IRExtensions::GLOBAL_ID)) {
			if (ptr->getType()->getNodeType() == core::NT_RefType) {
				res = c_ast::ref(res);
			}

			// add code dependency to global struct
			auto fragment = converter.getFragmentManager()->getFragment(IRExtensions::GLOBAL_ID);
			assert(fragment && "Global Fragment not yet initialized!");
			context.getDependencies().insert(fragment);

			return res;
		}

		// special handling for type literals (fall-back solution)
		if (core::analysis::isTypeLiteralType(ptr->getType())) {
			const TypeInfo& info = converter.getTypeManager().getTypeInfo(ptr->getType());
			context.addDependency(info.declaration);
			return c_ast::lit(info.rValueType, "type_token");
		}

		// handle null pointer
		if (converter.getNodeManager().getLangBasic().isNull(ptr)) {
			return converter.getCNodeManager()->create<c_ast::Literal>("0");
		}

		// handle all literals defined as extern  
		if (core::analysis::isRefType(ptr->getType())) {
			res = c_ast::ref(res);
		}

		// done
		return res;
	}

	c_ast::NodePtr StmtConverter::visitStructExpr(const core::StructExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

        auto typeInfo = converter.getTypeManager().getTypeInfo(ptr->getType());
        context.addDependency(typeInfo.definition);

        // get type and create init expression
        c_ast::TypePtr type = typeInfo.rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// obtain some helper
		auto& basic = converter.getNodeManager().getLangBasic();

		// append initialization values
		::transform(ptr->getMembers()->getElements(), std::back_inserter(init->values),
				[&](const core::NamedValuePtr& cur) {
					core::ExpressionPtr arg = cur->getValue();
					// skip ref.var if present
					if (core::analysis::isCallOf(cur->getValue(), basic.getRefVar())) {
						arg = static_pointer_cast<const core::CallExpr>(cur->getValue())->getArgument(0);
						if (core::analysis::isCallOf(arg, basic.getRefDeref())) {
							arg = static_pointer_cast<const core::CallExpr>(arg)->getArgument(0);
						}
					}
					return convert(context, arg);
		});

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitUnionExpr(const core::UnionExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding union
		//     (<type>){<single member>}

        auto typeInfo = converter.getTypeManager().getTypeInfo(ptr->getType());
        context.addDependency(typeInfo.definition);

        // get type and create init expression
        c_ast::TypePtr type = typeInfo.rValueType;
		return c_ast::init(type, convert(context, ptr->getMember()));
	}

	c_ast::NodePtr StmtConverter::visitTupleExpr(const core::TupleExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

        auto typeInfo = converter.getTypeManager().getTypeInfo(ptr->getType());
        context.addDependency(typeInfo.definition);

        // get type and create init expression
        c_ast::TypePtr type = typeInfo.rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// append initialization values
		::transform(ptr->getExpressions()->getElements(), std::back_inserter(init->values),
				[&](const core::ExpressionPtr& cur) {
					return convert(context, cur);
		});

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitVariable(const core::VariablePtr& ptr, ConversionContext& context) {
		// just look up variable within variable manager and return variable token ...
		const VariableInfo& info = context.getVariableManager().getInfo(ptr);
		return (info.location == VariableInfo::DIRECT && !core::analysis::isRefOf(ptr->getType(), core::NT_ArrayType))?c_ast::ref(info.var):info.var;
	}

	c_ast::NodePtr StmtConverter::visitVectorExpr(const core::VectorExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct - where one value is a vector
		//     (<type>){{<list of members>}}

		// get type and create empty init expression
		const TypeInfo& info = converter.getTypeManager().getTypeInfo(ptr->getType());
		c_ast::TypePtr type = info.rValueType;
		context.getDependencies().insert(info.definition);

		// create inner vector init and append initialization values
		c_ast::VectorInitPtr vectorInit = context.getConverter().getCNodeManager()->create<c_ast::VectorInit>();
		::transform(ptr->getExpressions()->getElements(), std::back_inserter(vectorInit->values),
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
		for_each(ptr->getStatements(), [&](const core::StatementPtr& cur) {
			c_ast::NodePtr stmt = this->visit(cur,context);
			if (stmt) { res->statements.push_back(stmt); }
		});
		return res;
	}

	c_ast::NodePtr StmtConverter::visitContinueStmt(const core::ContinueStmtPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Continue>();
	}

	c_ast::NodePtr StmtConverter::visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, ConversionContext& context) {

		// goal: create a variable declaration and register new variable within variable manager

		auto& basic = converter.getNodeManager().getLangBasic();
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

		// add code dependency
		context.getDependencies().insert(info.typeInfo->definition);

		// create declaration statement
		c_ast::ExpressionPtr initValue = convertInitExpression(context, init);
		return manager->create<c_ast::VarDecl>(info.var, initValue);
	}

	c_ast::ExpressionPtr StmtConverter::convertInitExpression(ConversionContext& context, const core::ExpressionPtr& init) {
		auto& basic = converter.getNodeManager().getLangBasic();
		auto manager = converter.getCNodeManager();

		// test whether initialization is required ...
		if (core::analysis::isCallOf(init, basic.getRefVar())) {
			core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(init);
			if (core::analysis::isCallOf(call->getArgument(0), basic.getUndefined())) {
				// => undefined initialization, hence no initialization!
				return c_ast::ExpressionPtr();
			}
		}

		// TODO: handle initUndefine and init struct cases

		// drop ref.var ...
		core::ExpressionPtr initValue = init;
		if (core::analysis::isCallOf(initValue, basic.getRefVar())) {
			initValue = core::analysis::getArgument(initValue, 0);
		}

		return convertExpression(context, initValue);
	}

	namespace {
		bool isSimple(const core::ExpressionPtr& exp) {
			try {
				core::arithmetic::Formula form = core::arithmetic::toFormula(exp);
				if(form.isConstant() || form.isValue()) return true;
			} catch(core::arithmetic::NotAFormulaException e) { }
			return false;
		}
	}


	c_ast::NodePtr StmtConverter::visitForStmt(const core::ForStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		VariableManager& varManager = context.getVariableManager();
		core::IRBuilder builder(ptr->getNodeManager());
		auto var_iter = ptr->getIterator();

		// get induction variable info
		const VariableInfo& info_iter = varManager.addInfo(converter, var_iter, VariableInfo::NONE);
		// add dependency to iterator type definition
		context.getDependencies().insert(info_iter.typeInfo->definition);

		auto initVector = toVector(std::make_pair(info_iter.var, convertExpression(context, ptr->getStart())));
		
		// Process:
		// For both "end" and "step" expressions:
		// - if simple: use directly
		// - if not: build variables to store results
		
		// handle step
		core::ExpressionPtr step = ptr->getStep();
		c_ast::ExpressionPtr cStep = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info_iter.var, convertExpression(context, step));
		core::VariablePtr var_step;
		if(!isSimple(step)) {
			// create variable storing step
			var_step = builder.variable(ptr->getIterator()->getType());
			const VariableInfo& info_step = varManager.addInfo(converter, var_step, VariableInfo::NONE);
			initVector.push_back(std::make_pair(info_step.var, convertExpression(context, step)));
			cStep = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info_iter.var, info_step.var);
		} 
		else { // use pre(inc/dec) if abs(step) == 1
			core::arithmetic::Formula form = core::arithmetic::toFormula(step);
			if(form.isConstant()) {
				if(form.isOne()) cStep = c_ast::preInc(info_iter.var);
				if((-form).isOne()) cStep = c_ast::preDec(info_iter.var);
			}
		}

		// handle end
		core::ExpressionPtr end = ptr->getEnd();
		c_ast::ExpressionPtr cCheck = c_ast::lt(info_iter.var, convertExpression(context, end));
		core::VariablePtr var_end;
		if(!isSimple(end)) {
			// create variable storing end
			var_end = builder.variable(ptr->getIterator()->getType());
			const VariableInfo& info_end  = varManager.addInfo(converter, var_end, VariableInfo::NONE);
			initVector.push_back(std::make_pair(info_end.var, convertExpression(context, end)));
			cCheck = c_ast::lt(info_iter.var, info_end.var);
		}

		// create init and body
		c_ast::VarDeclPtr cInit = manager->create<c_ast::VarDecl>(initVector);
		c_ast::StatementPtr cBody = convertStmt(context, ptr->getBody());

		// remove variable info since no longer in scope
		varManager.remInfo(var_iter);
		if(var_step) varManager.remInfo(var_step);
		if(var_end) varManager.remInfo(var_end);

		// combine all into a for
		return manager->create<c_ast::For>(cInit, cCheck, cStep, cBody);
	}

	c_ast::NodePtr StmtConverter::visitIfStmt(const core::IfStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		// create condition, then and else branch
		c_ast::ExpressionPtr condition = convertExpression(context, ptr->getCondition());
		c_ast::StatementPtr thenBranch = convertStmt(context, ptr->getThenBody());
		c_ast::StatementPtr elseBranch = (core::analysis::isNoOp(ptr->getElseBody()))
				?c_ast::StatementPtr():convertStmt(context, ptr->getElseBody());

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
		if (context.getConverter().getNodeManager().getLangBasic().isUnit(ptr->getReturnExpr()->getType())) {
			// special handling for unit-return
			return converter.getCNodeManager()->create<c_ast::Return>();
		}
		return converter.getCNodeManager()->create<c_ast::Return>(convertExpression(context, ptr->getReturnExpr()));
	}

	c_ast::NodePtr StmtConverter::visitSwitchStmt(const core::SwitchStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		// create empty switch ...
		c_ast::SwitchPtr res = manager->create<c_ast::Switch>(convertExpression(context, ptr->getSwitchExpr()));

		// add cases ..
		::transform(ptr->getCases()->getElements(), std::back_inserter(res->cases), [&](const core::SwitchCasePtr& cur) {
			return std::make_pair(convertExpression(context, cur->getGuard()), convertStmt(context, cur->getBody()));
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

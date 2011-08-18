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

#include "insieme/backend/runtime/runtime_preprocessing.h"

#include "insieme/core/expressions.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/encoder/encoder.h"

#include "insieme/backend/runtime/runtime_extensions.h"

namespace insieme {
namespace backend {
namespace runtime {


	namespace {


		core::StatementPtr registerEntryPoint(core::NodeManager& manager, const WorkItemImpl& entry) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.getBasicGenerator();
			auto& extensions = manager.getLangExtension<Extensions>();

			// create register call
			return builder.callExpr(basic.getUnit(), extensions.registerWorkItemImpl, core::encoder::toIR(manager, entry));
		}

		WorkItemImpl wrapEntryPoint(core::NodeManager& manager, const core::ExpressionPtr& entry) {
			core::ASTBuilder builder(manager);
			const core::lang::BasicGenerator& basic = manager.getBasicGenerator();
			const Extensions& extensions = manager.getLangExtension<Extensions>();

			// create new lambda expression wrapping the entry point
			assert(entry->getType()->getNodeType() == core::NT_FunctionType && "Only functions can be entry points!");
			core::FunctionTypePtr entryType = static_pointer_cast<const core::FunctionType>(entry->getType());
			assert(entryType->isPlain() && "Only plain functions can be entry points!");


			// define parameter of resulting lambda
			core::VariablePtr workItem = builder.variable(builder.refType(extensions.workItemType));
			core::TypePtr tupleType = DataItem::toLWDataItemType(builder.tupleType(entryType->getParameterTypes()));
			core::ExpressionPtr paramTypes = core::encoder::toIR(manager, tupleType);

			vector<core::ExpressionPtr> argList;
			unsigned counter = 0;
			transform(entryType->getParameterTypes(), std::back_inserter(argList), [&](const core::TypePtr& type) {
				return builder.callExpr(type, extensions.getWorkItemArgument,
						toVector<core::ExpressionPtr>(workItem, core::encoder::toIR(manager, counter++), paramTypes, basic.getTypeLiteral(type)));
			});

			// produce replacement
			core::TypePtr unit = basic.getUnit();
			core::ExpressionPtr call = builder.callExpr(entryType->getReturnType(), entry, argList);
			core::ExpressionPtr exit = builder.callExpr(unit, extensions.exitWorkItem, workItem);
			WorkItemVariant variant(builder.lambdaExpr(unit, builder.compoundStmt(call, exit), toVector(workItem)));
			return WorkItemImpl(toVector(variant));
		}


		core::ProgramPtr replaceMain(core::NodeManager& manager, const core::ProgramPtr& program) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.getBasicGenerator();
			auto& extensions = manager.getLangExtension<Extensions>();

			core::TypePtr unit = basic.getUnit();
			core::TypePtr intType = basic.getUInt4();

			// build up list of statements for the body
			vector<core::StatementPtr> stmts;

			// -------------------- assemble parameters ------------------------------

			core::VariablePtr argc = builder.variable(basic.getInt4());
			core::VariablePtr argv = builder.variable(builder.refType(builder.arrayType(builder.refType(builder.arrayType(basic.getChar())))));
			vector<core::VariablePtr> params = toVector(argc,argv);

			// ------------------- Add list of entry points --------------------------

			for_each(program->getEntryPoints(), [&](const core::ExpressionPtr& entry) {
				stmts.push_back(registerEntryPoint(manager, wrapEntryPoint(manager, entry)));
			});

			// ------------------- Start standalone runtime  -------------------------

			// construct light-weight data item tuple
			core::ExpressionPtr expr = builder.tupleExpr(toVector<core::ExpressionPtr>(argc, argv));
			core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(expr->getType());
			expr = builder.callExpr(DataItem::toLWDataItemType(tupleType), extensions.wrapLWData, toVector(expr));

			// create call to standalone runtime
			stmts.push_back(builder.callExpr(unit, extensions.runStandalone, expr));

			// ------------------- Add return   -------------------------

			stmts.push_back(builder.returnStmt(builder.intLit(0)));

			// ------------------- Creation of new main function -------------------------


			core::FunctionTypePtr mainType = builder.functionType(toVector(argc->getType(), argv->getType()), basic.getInt4());

			// create new main function
			core::StatementPtr body = builder.compoundStmt(stmts);
			core::ExpressionPtr main = builder.lambdaExpr(mainType, params, body);

			// return resulting program
			return core::Program::create(manager, toVector(main), true);
		}

	}


	core::NodePtr WorkItemExtractor::process(core::NodeManager& manager, const core::NodePtr& node) {

		// TODO:
		//    - convert entry points to work items
		// 	  - create alternative main conducting a runtime call (+ initContext())
		//	  - identification and creation of work items

		auto nodeType = node->getNodeType();

		// handle programs specially
		if (nodeType == core::NT_Program) {
			return replaceMain(manager, static_pointer_cast<const core::Program>(node));
		}

		// if it is a expression, wrap it within a program and resolve equally
		if (core::ExpressionPtr expr = dynamic_pointer_cast<const core::Expression>(node)) {
			return replaceMain(manager, core::Program::create(manager, toVector(expr)));
		}

		// nothing to do otherwise
		return node;
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

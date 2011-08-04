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

#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"

namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		const core::LiteralPtr getInitRuntime(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// the init-context function is simple a ()->unit function
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(), basic.getUnit());

			return builder.literal(type, "initRuntime");
		}

		const core::TypePtr getContextType(core::NodeManager& manager) {
			return core::GenericType::get(manager, "Context");
		}

		core::LiteralPtr getInitContext(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// create type
			core::TypePtr contextType = builder.refType(getContextType(manager));
			core::TypePtr funType = builder.functionType(toVector(contextType), basic.getUnit());
			return builder.literal(funType, "insieme_init_context");
		}

		core::LiteralPtr getCleanupContext(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// create type
			core::TypePtr contextType = builder.refType(getContextType(manager));
			core::TypePtr funType = builder.functionType(toVector(contextType), basic.getUnit());
			return builder.literal(funType, "insieme_cleanup_context");
		}

		const core::TypePtr getWorkItemType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item type as a generic type
			return builder.genericType("WorkItem");
		}

		const core::TypePtr getWorkItemImplType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// this type is a function type (for now, at some point it will be updated to a list of functions)
			return builder.functionType(toVector<core::TypePtr>(), basic.getUnit());
		}

		const core::TypePtr getDataItemType(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);

			// create the work item type as a generic type
			return builder.genericType("DataItem", toVector<core::TypePtr>(builder.typeVariable("a")));
		}

		const core::LiteralPtr getRegisterWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// parameter list:
			// 	- the lambda implementing this work item (TODO: extend to list of lambdas - or a vector<lambda,#n>)
			// future stuff (not yet implemented):
			//  - the work-item parameter type
			//  - the data item requirements
			//  - the resource requirements

			core::TypePtr unit = basic.getUnit();
			core::TypePtr fun = builder.functionType(core::TypeList(), unit);
			core::TypePtr resType = builder.functionType(toVector(fun), unit);

			return builder.literal(resType, "registerWorkItem");
		}

		const core::LiteralPtr getCreateWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// parameter list:
			//	- start/end/step-size of range
			//  - work item entry
			//  - parameter data item

			core::TypePtr intType = basic.getUIntGen();
			core::TypePtr workItemImplType = getWorkItemImplType(manager);
			core::TypePtr dataItemType = getDataItemType(manager);
			core::TypePtr workItemType = getWorkItemType(manager);

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector<core::TypePtr>(intType, intType, intType, workItemImplType, dataItemType), workItemType);

			return builder.literal(type, "createWorkItem");
		}

		const core::LiteralPtr getSubmitWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// simply (WorkItem)->unit
			core::TypePtr unit = basic.getUnit();
			core::TypePtr workItemType = getWorkItemType(manager);

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector(workItemType), unit);

			return builder.literal(type, "submitWorkItem");
		}

		const core::LiteralPtr getJoinWorkItem(core::NodeManager& manager) {
			core::ASTBuilder builder(manager);
			auto& basic = manager.basic;

			// simply (WorkItem)->unit
			core::TypePtr unit = basic.getUnit();
			core::TypePtr workItemType = getWorkItemType(manager);

			// the create-work-item function
			core::TypePtr type = builder.functionType(toVector(workItemType), unit);

			return builder.literal(type, "joinWorkItem");
		}


	}

	Extensions::Extensions(core::NodeManager& manager)
		: initRuntime(getInitRuntime(manager)), initContext(getInitContext(manager)), cleanupContext(getCleanupContext(manager)),
		  workItemType(getWorkItemType(manager)), dataItemType(getDataItemType(manager)),
		  registerWorkItem(getRegisterWorkItem(manager)), createWorkItem(getCreateWorkItem(manager)),
		  submitWorkItem(getSubmitWorkItem(manager)), joinWorkItem(getJoinWorkItem(manager)) {}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

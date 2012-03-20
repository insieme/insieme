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

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/ocl_kernel/kernel_extensions.h"
#include "insieme/backend/ocl_kernel/kernel_stmt_handler.h"
#include "insieme/backend/ocl_kernel/kernel_type_handler.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/utils/logging.h"


namespace insieme {
namespace backend {
namespace ocl_kernel {

	namespace {

		c_ast::NodePtr handleStmts(ConversionContext& context, const core::NodePtr& node) {

			// only interested in declaration statements
			c_ast::NodePtr res;
			if (node->getNodeType() != core::NT_DeclarationStmt) {
				return res; // let anybody else try ...
			}

			// get decl statement
			core::DeclarationStmtPtr decl = static_pointer_cast<const core::DeclarationStmt>(node);

			// check for a OCL extended type ...
			const Extensions& extensions = context.getConverter().getNodeManager().getLangExtension<Extensions>();
			if (!extensions.isWrapperType(decl->getVariable()->getType())) {
				return res;
			}

			// build declaration replacement
			const Converter& converter = context.getConverter();
			auto manager = converter.getCNodeManager();

			core::VariablePtr var = decl->getVariable();
			core::ExpressionPtr init = decl->getInitialization();

			// register new variable within context
			const VariableInfo& info = context.getVariableManager().addInfo(converter, var, VariableInfo::DIRECT);

			// add code dependency
			context.getDependencies().insert(info.typeInfo->definition);

			// get first argument of init statement
			init = core::analysis::getArgument(init, 0);
			auto initExpr = context.getConverter().getStmtConverter().convertInitExpression(context, init);
			return manager->create<c_ast::VarDecl>(info.var, initExpr);
		}

	}

	StmtHandler OclKernelStmtHandler = &handleStmts;

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

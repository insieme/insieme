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
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/pattern/ir_generator.h"

#include "insieme/utils/logging.h"

using namespace insieme::transform::pattern;
namespace irg = insieme::transform::pattern::generator::irg;

namespace insieme {
namespace backend {
namespace ocl_kernel {

	namespace {

    using insieme::transform::pattern::any;
    using insieme::transform::pattern::anyList;

		c_ast::NodePtr handleStmts(ConversionContext& context, const core::NodePtr& node) {

            const core::NodeManager& man = node->getNodeManager();

            // only interested in declaration statements
			c_ast::NodePtr res;
			if (node->getNodeType() != core::NT_DeclarationStmt) {
				return res; // let anybody else try ...
			}

			// get decl statement
			core::DeclarationStmtPtr decl = static_pointer_cast<const core::DeclarationStmt>(node);

            core::VariablePtr var = decl->getVariable();
            core::ExpressionPtr init = decl->getInitialization();

            // build declaration replacement
            const Converter& converter = context.getConverter();
            auto manager = converter.getCNodeManager();

            core::TypePtr varTy = var->getType();
            if(core::RefTypePtr refTy = dynamic_pointer_cast<const core::RefType>(varTy))
                varTy = refTy->getElementType();

            if (varTy->getNodeType() == core::NT_VectorType) {
                TreePatternPtr vecExpr = aT(irp::vectorExpr(*any));
                auto&& match = vecExpr->matchPointer(init);
                if (match) { // if it's a vector expression
                    std::string str = oclRefTypeToString(man.getLangBasic(), var->getType());
                    if(!str.empty()){
                        // register new variable within context
                        const VariableInfo& info = context.getVariableManager().addInfo(converter, var, VariableInfo::DIRECT);

                        // add code dependency
                        context.getDependencies().insert(info.typeInfo->definition);


                        // convert init expression into (arg1,arg2,...,argN)
                        init = core::analysis::getArgument(init, 0);

                        vector<c_ast::NodePtr> valueList;
                        for_each(init.as<core::VectorExprPtr>()->getExpressions()->getElements(),
                            [&](const core::ExpressionPtr& cur) {
                                valueList.push_back(context.getConverter().getStmtConverter().convertInitExpression(context, cur));
                        });

                        c_ast::TypePtr type = context.getConverter().getTypeManager().getTypeInfo(init->getType()).rValueType;
                        auto initExpr = c_ast::initOCLVector(type, valueList);
                        return manager->create<c_ast::VarDecl>(info.var, initExpr);
                    }
                }
            }

            // only interested in OCL extended type ...
			const Extensions& extensions = context.getConverter().getNodeManager().getLangExtension<Extensions>();
			if (!extensions.isWrapperType(decl->getVariable()->getType())) {
				return res;
			}

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

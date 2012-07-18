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

#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/ocl_host/host_operator.h"
#include "insieme/backend/ocl_host/host_extensions.h"
#include "insieme/backend/ocl_host/host_code_fragments.h"

#include "insieme/backend/ocl_kernel/kernel_backend.h"
#include "insieme/backend/ocl_kernel/kernel_extensions.h"

#include "insieme/backend/runtime/runtime_extensions.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace backend {
namespace ocl_host {

	OperatorConverterTable& addOpenCLHostSpecificOps(core::NodeManager& manager, OperatorConverterTable& table) {

		const Extensions& ext = manager.getLangExtension<Extensions>();
		auto& kernelExt = manager.getLangExtension<ocl_kernel::Extensions>();
//		auto& runtimeExt = manager.getLangExtension<runtime::Extensions>();

		#include "insieme/backend/operator_converter_begin.inc"

		table[kernelExt.wrapGlobal] = OP_CONVERTER({
			return CONVERT_ARG(0);
			//return c_ast::call(C_NODE_MANAGER->create("moveToGPU"), CONVERT_ARG(0));
		});

		table[kernelExt.wrapLocal] = OP_CONVERTER({
			return CONVERT_ARG(0);
			//return c_ast::call(C_NODE_MANAGER->create("moveToGPU"), CONVERT_ARG(0));
		});

		table[kernelExt.wrapConst] = OP_CONVERTER({
			return CONVERT_ARG(0);
			//return c_ast::call(C_NODE_MANAGER->create("moveToGPU"), CONVERT_ARG(0));
		});

		table[ext.callKernel] = OP_CONVERTER({
			/*
			irt_ocl_rt_run_kernel(0,	1, &szGlobalWorkSize, &szLocalWorkSize,
										3,
										(size_t)0, buf_input,
										(size_t)0, buf_output,
										sizeof(cl_long), &len_input);
			*/

			const Converter& converter = context.getConverter();
			StmtConverter& stmtConverter = converter.getStmtConverter();

			// register kernel
			KernelCodeTablePtr table = KernelCodeTable::get(converter);
			context.addDependency(table);

			unsigned kernelID = table->registerKernel(ARG(0));

			const ocl_kernel::Extensions& ext = converter.getNodeManager().getLangExtension<ocl_kernel::Extensions>();

			c_ast::TypePtr sizeType = C_NODE_MANAGER->create<c_ast::NamedType>(C_NODE_MANAGER->create("size_t"));
			c_ast::ExpressionPtr zero = c_ast::lit(sizeType, "0");

			core::VectorTypePtr vecType = static_pointer_cast<const core::VectorType>(ARG(3)->getType());

			core::ExpressionPtr values = static_pointer_cast<const core::CallExpr>(ARG(4))->getArgument(0);
			const vector<core::ExpressionPtr>& kernelArgs = static_pointer_cast<const core::TupleExpr>(values)->getExpressions()->getElements();

			vector<c_ast::ExpressionPtr> args;
			args.push_back(c_ast::lit(sizeType, utils::numeric_cast<string>(kernelID)));
			args.push_back(c_ast::lit(sizeType, toString(*vecType->getSize())));


			args.push_back(c_ast::cast(c_ast::ptr(sizeType), CONVERT_ARG(1))); // offset
			args.push_back(c_ast::cast(c_ast::ptr(sizeType), c_ast::access(CONVERT_ARG(2), "data")));
			args.push_back(c_ast::cast(c_ast::ptr(sizeType), c_ast::access(CONVERT_ARG(3), "data")));

			args.push_back(c_ast::lit(sizeType, utils::numeric_cast<string>(kernelArgs.size())));

			for_each(kernelArgs, [&](const core::ExpressionPtr& cur) {
				c_ast::ExpressionPtr arg = stmtConverter.convertExpression(context, cur);
				if (ext.isWrapperType(cur->getType())) {
					args.push_back(c_ast::cast(sizeType, zero));
					args.push_back(arg);
                } else if(cur->getNodeType() == core::NT_Variable && cur->getType()->getNodeType() == core::NT_RefType) {
					args.push_back(c_ast::sizeOf(stmtConverter.convertType(context, cur->getType())));
					args.push_back(c_ast::ref(arg));
                } else if(cur->getType()->getNodeType() == core::NT_StructType) {
                    args.push_back(c_ast::sizeOf(stmtConverter.convertType(context, cur->getType())));
                    args.push_back(c_ast::ref(arg));
				} else {
					args.push_back(c_ast::sizeOf(stmtConverter.convertType(context, cur->getType())));
					args.push_back(c_ast::ref(c_ast::init(CONVERT_TYPE(cur->getType()), arg)));
				}

			});

			c_ast::ExpressionPtr fun = C_NODE_MANAGER->create<c_ast::Literal>("irt_ocl_rt_run_kernel");
			return c_ast::call(fun, args);

			//return converter.getCNodeManager()->create<c_ast::Literal>(format(codeTemplate, kernelID, numArgs, argList.str().c_str()));

			//return CONVERT_ARG(0);
		});

		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme

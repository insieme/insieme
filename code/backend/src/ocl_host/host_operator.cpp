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

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/string_utils.h"

namespace insieme {
namespace backend {
namespace ocl_host {

	OperatorConverterTable& addOpenCLHostSpecificOps(core::NodeManager& manager, OperatorConverterTable& table) {

		//auto& ext = manager.getLangExtension<Extensions>();
		auto& kernelExt = manager.getLangExtension<ocl_kernel::Extensions>();

		#include "insieme/backend/operator_converter_begin.inc"



		table[kernelExt.kernelWrapper] 	= OP_CONVERTER({

			KernelCodeTablePtr table = KernelCodeTable::get(context.getConverter());
			context.addDependency(table);

			const ocl_kernel::Extensions& ext = NODE_MANAGER.getLangExtension<ocl_kernel::Extensions>();

			static const char* codeTemplate = "\n\n"
					"irt_ocl_kernel* kernel = &irt_context_get_current()->kernel_binary_table[0][%1$d];\n"
					"irt_ocl_set_kernel_ndrange(kernel, 1, irt_ocl_get_global_size(%1$d), irt_ocl_get_global_size(%1$d));\n"
					"irt_ocl_run_kernel(kernel, %2$d %3$s);\n\n";

			// derive some infos
			unsigned kernelID = table->registerKernel(call);
			unsigned numArgs = call->getArguments().size();

			std::stringstream argList;
			for_each(call->getArguments(), [&](const core::ExpressionPtr& cur) {
				c_ast::ExpressionPtr arg = CONVERT_EXPR(cur);
				if (ext.isWrapperType(cur->getType())) {
					argList << ", (size_t)0";
				} else {
					argList << ", " << c_ast::toC(c_ast::sizeOf(CONVERT_TYPE(cur->getType())));
				}
				argList << ", " << c_ast::toC(arg);
			});

			return C_NODE_MANAGER->create<c_ast::Literal>(format(codeTemplate, kernelID, numArgs, argList.str().c_str()));

			/*
		irt_ocl_device* dev = irt_ocl_get_device(0);
		irt_ocl_kernel* kernel = &irt_context_get_current()->kernel_binary_table[0][0];

		 irt_ocl_buffer* buf_input = irt_ocl_create_buffer(dev, CL_MEM_READ_ONLY, mem_size_input);

		irt_ocl_write_buffer(buf_input, CL_FALSE, mem_size_input, &input[wi->range.begin]);


	irt_ocl_set_kernel_ndrange(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize);

	irt_ocl_run_kernel(kernel, 3,   (size_t)0, (void *)buf_input,
								 (size_t)0, (void *)buf_output,
								 sizeof(cl_long), (void *)&len_input);

	 irt_ocl_read_buffer(buf_output, CL_TRUE, mem_size_output, &output[wi->range.begin]);


	irt_ocl_release_buffer(buf_input);
			 */


			//return C_NODE_MANAGER->create<c_ast::Literal>("");
		});

		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme

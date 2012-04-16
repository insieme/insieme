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

#include "insieme/backend/ocl_host/host_code_fragments.h"

#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/trim.hpp>

#include "insieme/annotations/c/naming.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/backend/function_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/ocl_kernel/kernel_backend.h"

#include "insieme/backend/runtime/runtime_code_fragments.h"

namespace insieme {
namespace backend {
namespace ocl_host {

	// definition of some names within the generated code
	#define KERNEL_TABLE_NAME "g_insieme_kernel_code_table"


	// -- Kernel Code Table --------------------------------------------------------------

	KernelCodeTablePtr KernelCodeTable::get(const Converter& converter) {
		static string ENTRY_NAME = "KernelCodeTable";

		// look up the entry within the fragment manager
		auto manager = converter.getFragmentManager();
		auto res = manager->getFragment(ENTRY_NAME);
		if (!res) {
			// create new instance
			KernelCodeTablePtr table = manager->create<KernelCodeTable>(boost::ref(converter));
			manager->bindFragment(ENTRY_NAME, table);
			res = table;

			// add initialization and cleanup lines
			runtime::ContextHandlingFragmentPtr fragments = runtime::ContextHandlingFragment::get(converter);
			fragments->addInitExpression(
					"    #ifdef USE_OPENCL\n"
					"    irt_ocl_rt_create_all_kernels(%s, " KERNEL_TABLE_NAME ", g_kernel_code_table_size);\n"
					"    #endif\n"
			);
			fragments->addCleanupExpression(
					"    #ifdef USE_OPENCL\n"
					"    irt_ocl_rt_release_all_kernels(%s, g_kernel_code_table_size);\n"
					"    #endif\n"
			);
		}
		return static_pointer_cast<KernelCodeTable>(res);
	}

	unsigned KernelCodeTable::registerKernel(const core::ExpressionPtr& kernel) {

		// lookup kernel in map
		auto pos = kernelMap.find(kernel);
		if (pos != kernelMap.end()) {
			return pos->second;
		}

		// resolve kernel to code
		unsigned id = codes.size();
		kernelMap.insert(std::make_pair(kernel, id));

		assert(kernel->getNodeType() == core::NT_CallExpr);
		core::ExpressionPtr kernelLambda = core::analysis::getArgument(kernel, 0);

		// fix name of kernel
		std::string name;
		if (kernelLambda->hasAnnotation(annotations::c::CNameAnnotation::KEY)) {
			name = kernelLambda->getAnnotation(annotations::c::CNameAnnotation::KEY)->getName();
		} else {
			name = "main_kernel";
			kernelLambda->addAnnotation(std::make_shared<annotations::c::CNameAnnotation>(name));
		}

		// create a kernel code entry
		KernelCode code(name, ocl_kernel::OCLKernelBackend::getDefault()->convert(kernel));
		codes.push_back(code);

		return id;
	}



	const c_ast::ExpressionPtr KernelCodeTable::getTableToken() {
		return c_ast::ref(converter.getCNodeManager()->create(KERNEL_TABLE_NAME));
	}

	std::ostream& KernelCodeTable::printTo(std::ostream& out) const {

		out <<	"// --- the kernel code table --- \n"
				"unsigned g_kernel_code_table_size = " << codes.size() << ";\n"
				"irt_ocl_kernel_code " KERNEL_TABLE_NAME "[] = {\n";

		int counter = 0;
		for_each(codes, [&](const KernelCode& cur) {
			out << 	"    { // kernel " << counter++ << "\n"
				"    \"" << cur.name << "\",\n    \""
				"#pragma OPENCL EXTENSION cl_khr_fp64: enable\\n\"\n    \"";

			// the most pure version:
			// escape(out) << *cur.code;

			// the nice one
			string original = toString(*cur.code);
			boost::trim(original);
			std::stringstream buffer;
			escape(buffer) << original;
			string code = buffer.str();
			boost::replace_all(code, "\\n", "\\n\"\n    \"");
			out << code;
			out << "\"\n    },\n";
		});

		return out << "};\n\n";
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

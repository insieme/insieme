/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/backend/opencl/opencl_postprocessor.h"
#include "insieme/backend/opencl/opencl_transform.h"
#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

namespace insieme {
namespace backend {
namespace opencl {

	OffloadSupportPost::OffloadSupportPost(const transform::StepContext& sc) :
		PostProcessor(), sc(sc)
	{ }

	c_ast::NodePtr OffloadSupportPost::process(const Converter& converter, const c_ast::NodePtr& node) {
		switch(node->getNodeType()) {
		case c_ast::NT_Comment:
			// strip them, comments are not useful in kernels anyway
			return converter.getCNodeManager()->create<c_ast::OpaqueExpr>("");
		case c_ast::NT_Function:
			{
				// if the name of the function is __insieme_fun_0 we tag it as kernel
				auto fun = node.as<c_ast::FunctionPtr>();
				if (fun->name->name == sc.getKernelName()) fun->flags |= c_ast::Function::OCL_KERNEL;
				return node;
			}
		case c_ast::NT_UnaryOperation:
				// this is introduced as the kernel backend is called with a lambdaExpr!
				// the last statement in the tree is therefore &...
				return converter.getCNodeManager()->create<c_ast::OpaqueExpr>("");
		default:
				// nothing is changed or stripped .. just pass through
				return node;
		}
	}

	void OffloadSupportPost::generateCompat(const transform::StepContext& sc, std::stringstream& ss) {
		// put together the required pragma extensions
		for (auto extension : sc.getExtensions()) {
			std::string name;
			switch (extension) {
			case transform::StepContext::KhrExtension::Fp64:
					name = "cl_khr_fp64"; break;
					break;
			case transform::StepContext::KhrExtension::ByteAddressableStore:
					name = "cl_khr_byte_addressable_store";
					break;
			case transform::StepContext::KhrExtension::All:
					name = "all";
					break;
			}
			ss << ::format("#pragma OPENCL EXTENSION %s : enable\n", name);
		}
		// write out compat typedefs to map IR primitives to OCL primitives
		ss << "typedef char int8_t;\n";
		ss << "typedef unsigned char uint8_t;\n";
		ss << "typedef short int16_t;\n";
		ss << "typedef unsigned short uint16_t;\n";
		ss << "typedef int int32_t;\n";
		ss << "typedef unsigned int uint32_t;\n";
		ss << "typedef long int64_t;\n";
		ss << "typedef unsigned long uint64_t;\n";
	}
} // end namespace opencl
} // end namespace backend
} // end namespace insieme

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
#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/ocl/ocl_host_replace_kernel.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {

}

KernelReplacer::KernelReplacer(core::NodePtr prog) : prog(prog){
	findKernelNames();
	collectArguments();
}

void KernelReplacer::findKernelNames() {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	NodeAddress pA(prog);

	TreePatternPtr nameLiteral = pattern::var("kernel_name", pattern::any);
	TreePatternPtr mayEncapsulatedNameLiteral = irp::callExpr(pattern::any, pattern::any, *pattern::any << nameLiteral << *pattern::any) | nameLiteral;
	TreePatternPtr clCreateKernel = irp::callExpr(pattern::any, irp::literal("clCreateKernel"),	pattern::any <<
			mayEncapsulatedNameLiteral << pattern::var("err", pattern::any) );
	TreePatternPtr createKernelPattern = pattern::var("clCreateKernel", irp::callExpr(pattern::any, irp::atom(mgr.getLangBasic().getRefAssign()),
			pattern::var("kernel", pattern::any) << clCreateKernel));

//std::cout << printer::PrettyPrinter(pA) << "matching\n";
	irp::matchAllPairs(createKernelPattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createKernel) {
//std::cout << "kernel: " << *createKernel["clCreateKernel"].getValue() << std::endl;

		std::string kernelName = createKernel["kernel_name"].getValue().as<LiteralPtr>()->getStringValue();
		ExpressionAddress kernelExpr = matchAddress >> createKernel["kernel"].getValue().as<ExpressionAddress>();
		ExpressionAddress kernelVar = extractVariable(kernelExpr).as<ExpressionAddress>();

		kernelVar = getRootVariable(kernelVar).as<ExpressionAddress>();

//std::cout << "varAddr: " << kernelVar << " - " << *kernelVar << std::endl;
		kernelNames[kernelName] = kernelVar;

		// remove the clCreateKernel call including the assignment and its lhs
		prog = transform::replaceAll(mgr, prog, NodePtr(createKernel["clCreateKernel"].getValue()), builder.getNoOp(), false);
	});
}

void KernelReplacer::collectArguments() {
//	NodeManager& mgr = prog->getNodeManager();
	NodeAddress pA(prog);

	TreePatternPtr clCreateKernel = irp::callExpr(pattern::any, irp::literal("clSetKernelArg"),
			pattern::var("kernel", pattern::any) << pattern::var("arg_index", pattern::any) << pattern::any << pattern::var("arg_value", pattern::any) );


	irp::matchAllPairs(clCreateKernel, pA, [&](const NodeAddress& matchAddress, const AddressMatch& setArg) {
//		std::cout << "Kernel: " << *setArg["kernel"].getValue() << "\n\tidx: " << *setArg["arg_index"].getValue() << " val "
//				<< *setArg["arg_value"].getValue() << std::endl;
	});
}

} //namespace ocl
} //namespace frontend
} //namespace insieme


/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/frontend/extensions/ocl_host_extension.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/ocl/ocl_host_replace_buffers.h"
#include "insieme/frontend/ocl/ocl_host_replace_kernel.h"
#include "insieme/frontend/ocl/ocl_type_fixer.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"
#include "insieme/frontend/ocl/ocl_host_handler.h"

namespace fe = insieme::frontend;

using namespace insieme::frontend;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace extensions {

using namespace insieme::core;
using namespace insieme::frontend::ocl;

FrontendExtension::flagHandler OclHostExtension::registerFlag(boost::program_options::options_description& options) {
    //register omp flag
    options.add_options()("fopencl", boost::program_options::value<bool>(&flagActivated)->implicit_value(true), "OpenCL support");
    //create lambda
    auto lambda = [&](const ConversionJob& job) {
        if(!flagActivated)
            return false;
        std::cerr << "Warning: OpenCL frontend support still experimental, consider switching to ICL frontend.\n";
        oclJobIncludeDirs = job.getIncludeDirectories();
        return true;
    };
    return lambda;
}


core::ProgramPtr OclHostExtension::IRVisit(insieme::core::ProgramPtr& prog) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	ocl::BufferReplacer br(prog->getElement(0));
	core::NodePtr root = br.getTransformedProgram();

	ocl::KernelReplacer kr(root, oclJobIncludeDirs);
	root = kr.getTransformedProgram();

	ocl::OclSimpleFunHandler osfh;
	root = osfh.mapElement(0, root);

	std::vector<TypePtr> typesToFix = {builder.genericType("_cl_kernel"), builder.genericType("_cl_mem")};
	ocl::TypeFixer otf(root, typesToFix);
	root = otf.getTransformedProg();

	core::ExpressionList list;
	list.push_back(root.as<core::ExpressionPtr>());

	prog = builder.program(list);

//std::cout << printer::PrettyPrinter(root) << std::endl;

	return prog;
}

FrontendExtension::flagHandler IclHostExtension::registerFlag(boost::program_options::options_description& options) {
    //register omp flag
    options.add_options()("flib-icl", boost::program_options::value<bool>(&flagActivated)->implicit_value(true), "Lib ICL support");
    //create lambda
    auto lambda = [&](const ConversionJob& job) {
        if(!flagActivated)
            return false;
        iclJobIncludeDirs = job.getIncludeDirectories();
        return true;
    };
    return lambda;
}

ExpressionPtr IclHostExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
                                               insieme::frontend::conversion::Converter& convFact) {
	NodeManager& mgr = irExpr->getNodeManager();
	IRBuilder builder(mgr);
	const core::lang::BasicGenerator& gen = builder.getLangBasic();
		iclRunKernel = irp::callExpr(pattern::any, irp::literal("icl_run_kernel"),
				var("derefKernel", irp::callExpr(pattern::any, pattern::atom(gen.getRefDeref()), pattern::single(var("kernel", pattern::any)))) <<
				*pattern::any << irp::callExpr(pattern::any, pattern::atom(gen.getVarlistPack()),
				pattern::single(irp::tupleExpr(pattern::any << irp::expressions(*var("args", pattern::any))))));

	core::pattern::TreePattern derefOfIclBuffer = irp::callExpr(pattern::atom(builder.refType(
																					builder.arrayType(builder.genericType("_icl_buffer")))),
																					pattern::atom(gen.getRefDeref()),
																					pattern::single(var("buffer", pattern::any)));


	NodeMap replacements;
	irp::matchAllPairs(iclRunKernel, irExpr, [&](const NodePtr& matchPtr, const NodeMatch& runKernel) {

		// remove deref from buffers
		for(NodePtr arg : runKernel["args"].getFlattened()) {
			MatchOpt match = derefOfIclBuffer.matchPointer(arg);
			if(match) {
				replacements[match.get().getRoot()] = match.get()["buffer"].getValue();
			}
		}
	});
	if(!replacements.empty())
		return transform::replaceAll(mgr, irExpr, replacements).as<ExpressionPtr>();

	return irExpr;
}

core::ProgramPtr IclHostExtension::IRVisit(insieme::core::ProgramPtr& prog) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	ocl::IclBufferReplacer br(prog->getElement(0));
	core::NodePtr root = br.getTransformedProgram();

	ocl::IclKernelReplacer kr(root, iclJobIncludeDirs);
	root = kr.getTransformedProgram();

	ocl::OclSimpleFunHandler osfh;
	root = osfh.mapElement(0, root);

	TypePtr iclKernelTy = builder.structType(builder.stringValue("_icl_kernel"), toVector(
			builder.namedType("kernel", builder.refType(builder.arrayType(builder.genericType("_cl_kernel")))),
			builder.namedType("dev", builder.refType(builder.arrayType(br.getIclDeviceType())))));


	std::vector<TypePtr> typesToFix = {iclKernelTy, br.getIclBufferType()};
	ocl::TypeFixer otf(root, typesToFix);
	root = otf.getTransformedProg();

	core::ExpressionList list;
	list.push_back(root.as<core::ExpressionPtr>());

//std::cout << printer::PrettyPrinter(root) << std::endl;
	prog = builder.program(list);

	return prog;
}

} //namespace extensions
} //namespace frontend
} //namespace insieme

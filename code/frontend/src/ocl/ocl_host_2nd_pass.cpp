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

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/frontend/ocl/ocl_host_utils.h"
#include "insieme/frontend/ocl/ocl_host_2nd_pass.h"

namespace ba = boost::algorithm;

namespace insieme {
namespace frontend {
namespace ocl {
using namespace insieme::core;

void Host2ndPass::mapNamesToLambdas(const vector<ExpressionPtr>& kernelEntries)
{
//	std::cout << "kernelNames:\n" << kernelNames << std::endl;
	std::map<string, int> checkDuplicates;
	for_each(kernelEntries, [&](ExpressionPtr entryPoint) {
			if(const LambdaExprPtr lambdaEx = dynamic_pointer_cast<const LambdaExpr>(entryPoint)) {
                std::string cname = insieme::core::annotations::getAttachedName(lambdaEx->getLambda());
                if(!cname.empty()) {
    //std::cout << "Cname: " << cname->getName() << std::endl;
                    assert(checkDuplicates[cname] == 0 && "Multiple kernels with the same name not supported");
                    checkDuplicates[cname] = 1;
    //std::cout << "found " << kernelNames;
                    if(ExpressionPtr clKernel = kernelNames[cname]) {
                        kernelLambdas[clKernel] = lambdaEx;
                    }
                }
			}
		});
}

ClmemTable& Host2ndPass::getCleanedStructures() {
	for_each(cl_mems, [&](std::pair<const VariablePtr, VariablePtr>& var) {
//std::cout << "clmem " << var.first << " -> " << var.second << std::endl;
			if(StructTypePtr type = dynamic_pointer_cast<const StructType>(getNonRefType(var.second))) {
				// delete all unnecessary cl_* fields form the struct
				StructType::Entries entries = type->getEntries(); // actual fields of the struct
				StructType::Entries newEntries;

				for_each(entries, [&](const NamedTypePtr& entry) {
					// todo removing kernel for irt_ version is untested
						if((entry->getType()->toString().find("_cl_") == string::npos && entry->getType()->toString().find("irt_ocl") == string::npos)
								|| (entry->getType()->toString().find("struct<kernel:ref<array<_cl_kernel,1>>") == string::npos
								&& entry->getType()->toString().find("_cl_kernel") != string::npos)) {
							newEntries.push_back(entry);
//std::cout << "\nKeeping " << entry << " in " << var.second << std::endl;
						} //else
//std::cout << "\nRemoving " << entry << " from " << var.second << std::endl;
					});

				// update struct in replacement map
				NodePtr replacement = builder.variable(builder.refType(builder.structType(newEntries)));
				copyAnnotations(var.second, replacement);
				var.second = static_pointer_cast<const Variable>(replacement);
			}
		});

	return cl_mems;
}

void Host2ndPass::updateKernelArgs(KernelArgs& kernelArgs, NodeMap& replacements) {
	for_each(kernelArgs, [&](std::pair<ExpressionPtr, std::vector<ExpressionPtr> > args) {
		for_each(args.second, [&](ExpressionPtr& arg) {
			NodePtr replaced = transform::replaceAll(arg->getNodeManager(), arg, replacements, true); // needed for USE_HOST_PTR flag
			arg = core::transform::replaceVarsRecursiveGen(arg->getNodeManager(), replaced, cl_mems, false).as<ExpressionPtr>();
			std::cout << "arg: " << arg->getType() << " - " << arg << std::endl;
		});
		// uptdate map
		kernelArgs[args.first] = args.second;
	});

}

} //namespace ocl
} //namespace frontend
} //namespace insieme

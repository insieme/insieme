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

#pragma once

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/utils/map_utils.h"
#include "insieme/frontend/frontend.h"

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
/*
 * Collects the kernel function names, specified using clCreateKernel and stores them in a map
 *
class FindKernelNames: public core::transform::CachedNodeMapping {
	std::map<core::ExpressionPtr, std::string> kernelNames;
	core::pattern::TreePatternPtr clCreateKernel;

	const core::NodePtr resolveElement(const core::CallExprPtr& ptr);

public:
	FindKernelNames();
};
*/
}

/*
 * Collects cl_kernel expressions, identifies all the arguments for the corresponding kernel functions and replaces it with a tuple, holding the arguments
 * Loads kernel source codes from files and adds them to the program
 * Replaces nd_range calls with calls to the actual function
 */
class KernelReplacer {
public:
	KernelReplacer(core::ProgramPtr& prog);
private:
	core::ProgramPtr& prog;

public:
	core::NodePtr getTransformedProgram() {return prog;}
};
} //namespace ocl
} //namespace frontend
} //namespace insieme

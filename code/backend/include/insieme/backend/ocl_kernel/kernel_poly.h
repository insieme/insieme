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

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/annotations/data_annotations.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

	class KernelPoly {
		core::NodePtr program;

    	std::vector<core::ExpressionPtr> kernels;
    	std::vector<annotations::Range> ranges;
    	std::vector<core::StatementPtr> loopNests;

    	/*
    	 * transforms a kernel into a loop nest which is analyzable by the polyhedral model
    	 * @param
    	 * kernel The kernel to be transformed
    	 * @return
    	 * the transformed kernel
    	 */
    	core::StatementPtr transformKernelToLoopnest(core::ExpressionAddress kernel);

    	/*
    	 * tries to find kernel functions
    	 * @param
    	 * lambda The node to be checked
    	 * @return
    	 * true if the lambda is a kernel-function-call, false otherwise
    	 */
    	core::ExpressionPtr isKernelFct(const core::CallExprPtr& call);

    	/*
    	 * transforms a kernel to use the get_*_id functions directly where ever possible
    	 * @param
    	 * kernel The kernel to be transformed
    	 * @return
    	 * the transformed kernel
    	 */
    	core::ExpressionPtr insertInductionVariables(core::ExpressionPtr kernel);

    	/*
    	 * generates the Work Item - Data Item relation function for all kernels inside program
    	 */
    	void genWiDiRelation();

	public:
    	KernelPoly(core::NodePtr& program): program(program) {
    		this->genWiDiRelation();
    	}

    	std::vector<core::ExpressionPtr>& getKernels() { return kernels; }
    	std::vector<core::StatementPtr>& getLoopNests() { return loopNests; }
    	std::vector<annotations::Range>& getRanges() { return ranges; }

	};


} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme


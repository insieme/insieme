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

#include "insieme/frontend/ocl/ocl_host_passes.h"

namespace insieme {
namespace frontend {
namespace ocl {

/*
 * Second pass when translating a program OpenCL to IR
 * Responsible for:
 * - connecting the names of kernel functions with the IR entry points (= LambdaExpr)
 */
class Host2ndPass {
	KernelNames& kernelNames;
	ClmemTable& cl_mems;
	const core::IRBuilder& builder;
//	EquivalenceMap& eqMap;
	KernelLambdas kernelLambdas;

public:
	Host2ndPass(KernelNames& oclKernelNames, ClmemTable& clMemTable, EquivalenceMap& equivalenceMap, const core::ProgramPtr& program, core::IRBuilder& build) :
		kernelNames(oclKernelNames), cl_mems(clMemTable), builder(build), /*eqMap(equivalenceMap),*/ kernelLambdas(
				boost::unordered_map<core::ExpressionPtr, std::vector<core::ExpressionPtr>, hash_target<core::ExpressionPtr>, equal_variables>::size_type(),
				hash_target_specialized(build, equivalenceMap), equal_variables(build, program)) {}

	void mapNamesToLambdas(const vector<core::ExpressionPtr>& kernelEntries);

	ClmemTable& getCleanedStructures();

	void updateKernelArgs(KernelArgs& kernelArgs, core::NodeMap& replacements);

	KernelNames& getKernelNames() {
		return kernelNames;
	}
	KernelLambdas& getKernelLambdas() {
		return kernelLambdas;
	}
};

} //namespace ocl
} //namespace frontend
} //namespace insieme

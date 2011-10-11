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
 * First pass when translating a program OpenCL to IR
 * Responsible for:
 * - replace the OpenCL cl_mem vars/structs containing cl_mem vars with IR variables (array<...>)
 * - remove instances of unused variables (cl_program, cl_kernel, ...)
 * - replace calls to cl_enqueueNDRangeKernls with function calls to the correct LambdaExpr with the appropriate arguments
 */
class HostMapper3rdPass: public core::transform::CachedNodeMapping {
	const core::ASTBuilder& builder;
	ClmemTable& cl_mems;
	KernelArgs& kernelArgs;
	LocalMemDecls& localMemDecls;
	KernelNames& kernelNames;
	KernelLambdas& kernelLambdas;
	EquivalenceMap& eqMap;
	insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr>& replacements;

	// Generates a function which, taking the kernel name as a string as argument, returns the corresponding lambda
	const core::ExpressionPtr genGetKernelLambda();

	// get the zero element of the corresponding type
	const core::ExpressionPtr getZeroElem(const core::TypePtr& type);

	// gets the innermost type out of an array/ref nest
	const core::TypePtr& getInnermostType(const core::TypePtr& type);

	// takes the expression size which describes the work size for the clEnqueueNDRange and embed it in an IR function which returns a
	// vector<uint<4>, 3>, always awaited by the kernel function. The elements with index greater or equal to workDim will always be set
	// to 1, regardless of the argument size
	const core::ExpressionPtr anythingToVec3(core::ExpressionPtr workDim,
			core::ExpressionPtr size);

	// Takes a function which argument's may have changed and which return value depends on the argument to create a new function
	// with an appropriate return value
	bool updateReturnVal(const core::CallExprPtr& oldCall, core::NodePtr& newCall);

	// do all the replacements needed to create a normal function call out of an clEnqueueNDRangeKernel/irt_ocl_run_kernel call
	// callExpr is the original call to the NDRangeKernle function, newCall is the substituted one
	const core::NodePtr handleNDRangeKernel(const core::CallExprPtr& callExpr, const core::CallExprPtr&  newCall, const size_t offset);

public:
	HostMapper3rdPass(const core::ASTBuilder build, ClmemTable& clMemTable, KernelArgs& oclKernelArgs, LocalMemDecls& oclLocalMemDecls,
			KernelNames& oclKernelNames, KernelLambdas& oclKernelLambdas, EquivalenceMap& equivalenceMap,
			insieme::utils::map::PointerMap<core::NodePtr, core::NodePtr>& oclReplacements) :
		builder(build), cl_mems(clMemTable), kernelArgs(oclKernelArgs),	localMemDecls(oclLocalMemDecls), kernelNames(oclKernelNames),
			kernelLambdas(oclKernelLambdas), eqMap(equivalenceMap), replacements(oclReplacements) { }

	const core::NodePtr resolveElement(const core::NodePtr& element);

};

} //namespace ocl
} //namespace frontend
} //namespace insieme

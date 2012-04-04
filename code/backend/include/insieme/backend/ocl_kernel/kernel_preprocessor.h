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
#include "insieme/backend/ocl_kernel/kernel_extensions.h"

#include "insieme/backend/preprocessor.h"

namespace insieme {
namespace backend {
namespace ocl_kernel {

/**
 * Tests whether the given lambda is marked to converted into an OpenCL kernel.
 */
bool isKernel(const core::LambdaExprPtr& lambda);

core::JobExprPtr getGlobalJob(const core::LambdaExprPtr& kernel);

typedef utils::map::PointerMap<core::VariablePtr, AddressSpace> AddressSpaceMap;

/**
 * Determines for each of the parameters of the given kernel whether it is referencing
 * a memory location within the local or global memory space.
 */
AddressSpaceMap getAddressSpaces(const core::LambdaExprPtr& kernel);

/**
 * Maps variables to their origins. The origin of a variable is either another variable
 * within an outer scope or a initialization value.
 */
typedef utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> VariableMap;

/**
 * Combines the given variable maps by computing the concatenation of res and other.
 * Hence, if A was mapped to B in res and B was mapped to C in other, the resulting
 * map will map A to C. The resulting map will be the res map - which is modified
 * during the execution.
 */
VariableMap& compose(VariableMap& res, const VariableMap& other) ;

VariableMap& mapBodyVars(VariableMap& res, const core::CallExprPtr& call);

VariableMap& mapBodyVars(VariableMap& res, const core::JobExprPtr& job);

VariableMap mapBodyVars(const core::LambdaExprPtr& kernel);

core::StatementPtr getKernelCore(const core::BindExprPtr& bind);

core::StatementPtr getKernelCore(const core::LambdaExprPtr& lambda);

bool isGetIDHelper(const core::ExpressionPtr& expr, std::size_t length);
bool isGetLocalID(const core::ExpressionPtr& expr);
bool isGetGlobalID(const core::ExpressionPtr& expr);
bool isGetGroupID(const core::ExpressionPtr& expr);

	class KernelPreprocessor : public PreProcessor {
		const std::string outFilePath;
	public:
		KernelPreprocessor() : outFilePath(std::string()) {}
		KernelPreprocessor(const std::string outFilePath) : outFilePath(outFilePath) {}

		virtual core::NodePtr process(core::NodeManager& manager, const core::NodePtr& code);
	};

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

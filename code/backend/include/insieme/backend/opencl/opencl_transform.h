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

#pragma once

#include "insieme/core/forward_decls.h"

#include "insieme/annotations/opencl/opencl_annotations.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/preprocessor.h"
#include "insieme/backend/opencl/opencl_entities.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace transform {
	/**
	 * transform::outline may produces e.g. float ** (due to ref<ref<ptr<real<4>>>>) which is not
	 * suitable for OpenCL kernel node arguments. This step flattens e.g. float ** to float *
	 */
	class FlattenVariableIndirectionStep : public PreProcessor {
		core::NodeManager& manager;
	public:
		FlattenVariableIndirectionStep(core::NodeManager& manager) : manager(manager) {}
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * tries to inline c/cpp_style_assignments produced by the frontend
	 */
	class InlineAssignmentsStep : public PreProcessor {
		core::NodeManager& manager;
	public:
		InlineAssignmentsStep(core::NodeManager& manager) : manager(manager) {}
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	core::CallExprPtr outline(core::NodeManager& manager, const core::StatementPtr& stmt);
	
	core::CallExprPtr buildRegisterKernel(core::NodeManager& manager, unsigned int& id, const core::LambdaExprPtr& oclExpr);
	
	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const core::ExpressionPtr& ndrange,
										 const core::ExpressionList& requirements, const core::ExpressionList& optionals);
	
	core::LambdaExprPtr toIR(core::NodeManager& manager, const NDRangePtr& ndrange);
	
	core::LambdaExprPtr toIR(core::NodeManager& manager, const annotations::opencl::VariableRequirementPtr& var);
	
	// note: callExpr shall be obtained by invoking outline() of above
	core::LambdaExprPtr toOcl(const Converter& converter, core::NodeManager& manager, unsigned int& id, const core::CallExprPtr& callExpr);
}
}
}
}

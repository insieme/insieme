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
#include "insieme/backend/opencl/opencl_extension.h"

#include "insieme/utils/graph_utils.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace transform {

	using namespace insieme::annotations::opencl;

	class CallContext {
		// ndrange associated with this call
		NDRangePtr ndrange;
		// concrete requirements -- if not empty, size must be the same as defaultRq!
		// a processing step can use it to adjust the requirements for its needs
		core::ExpressionList overrideRq;
		// a list of optionals which shall be passed to irt_opencl_execute.
		// right now the type must fit into an uint64 (limitation of IRT)
		core::ExpressionList optionals;
	public:
		const NDRangePtr& getNDRange() const { return ndrange; }
		void setNDRange(const NDRangePtr& ptr) { ndrange = ptr; }
		const core::ExpressionList& getOverrideRequirements() const { return overrideRq; }
		void setOverrideRequirements(const core::ExpressionList& lst) { overrideRq = lst; }
		const core::ExpressionList& getOptionals() const { return optionals; }
		void setOptionals(const core::ExpressionList& lst) { optionals = lst; }
	};
	typedef std::shared_ptr<CallContext> CallContextPtr;

	class StepContext {
	public:
		// represents the valid opencl extensions
		enum class KhrExtension { All, Fp64, ByteAddressableStore };
	private:
		// represents the initial requirements -- a call context can overwrite it
		VariableRequirementList defaultRq;
		// each call context will lead to an invokation of irt_opencl_execute
		utils::graph::Graph<CallContextPtr> graph;
		// represents what extensions the kernel needs to function properly
		std::set<KhrExtension> extensions;
		// name of the kernel
		std::string kernelName;
	public:
		const VariableRequirementList& getDefaultRequirements() const { return defaultRq; }
		void setDefaultRequirements(const VariableRequirementList& lst) { defaultRq = lst; }
		utils::graph::Graph<CallContextPtr>& getCallGraph() { return graph; }
		const utils::graph::Graph<CallContextPtr>& getCallGraph() const { return graph; }
		std::set<KhrExtension>& getExtensions() { return extensions; }
		const std::set<KhrExtension>& getExtensions() const { return extensions; }
		void setKernelName(const std::string& kernelName) { this->kernelName = kernelName; }
		const std::string& getKernelName() const { return kernelName; }
	};

	class Step : public PreProcessor {
	protected:
		core::NodeManager& manager;
		StepContext& context;
		const OpenCLExtension& oclExt;
	public:
		Step(core::NodeManager& manager, StepContext& context) :
			PreProcessor(), manager(manager), context(context), oclExt(manager.getLangExtension<OpenCLExtension>())
		{}
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override { return code; }
	};

	/**
	 * transform::outline may produces e.g. float ** (due to ref<ref<ptr<real<4>>>>) which is not
	 * suitable for OpenCL kernel node arguments. This step flattens e.g. float ** to float *
	 */
	class FlattenVariableIndirectionStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * tries to inline c/cpp_style_assignments produced by the frontend
	 */
	class InlineAssignmentsStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * introduces the NDRange concept
	 */
	class CallIntroducerStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * introduces KernelType and applies 'local' transformations -- this shall be the last step in toOcl!
	 */
	class KernelTypeStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * checks the integrity of the IR
	 */
	class IntegrityCheckStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	core::CallExprPtr outline(core::NodeManager& manager, const core::StatementPtr& stmt);

	core::CallExprPtr buildGetWorkDim(core::NodeManager& manager);
	core::CallExprPtr buildGetGlobalId(core::NodeManager& manager, const core::ExpressionPtr& dim);
	core::CallExprPtr buildGetGlobalSize(core::NodeManager& manager, const core::ExpressionPtr& dim);
	core::CallExprPtr buildGetLocalId(core::NodeManager& manager, const core::ExpressionPtr& dim);
	core::CallExprPtr buildGetLocalSize(core::NodeManager& manager, const core::ExpressionPtr& dim);
	core::CallExprPtr buildGetNumGroups(core::NodeManager& manager, const core::ExpressionPtr& dim);
	core::CallExprPtr buildGetGroupId(core::NodeManager& manager, const core::ExpressionPtr& dim);

	core::CallExprPtr buildRegisterKernel(core::NodeManager& manager, unsigned int& id,
										  const StepContext& sc, const core::LambdaExprPtr& oclExpr);

	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const core::ExpressionPtr& ndrange,
										 const core::ExpressionList& requirements, const core::ExpressionList& optionals);


	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const StepContext& sc, const CallContext& cc);

	core::StatementList buildExecuteGraph(core::NodeManager& manager, unsigned int id, const StepContext& sc);

	core::LambdaExprPtr toIR(core::NodeManager& manager, const NDRangePtr& ndrange);

	core::LambdaExprPtr toIR(core::NodeManager& manager, const annotations::opencl::VariableRequirementPtr& var);

	// note: callExpr shall be obtained by invoking outline() of above
	core::LambdaExprPtr toOcl(const Converter& converter, core::NodeManager& manager, unsigned int& id, const core::CallExprPtr& callExpr);
}
}
}
}

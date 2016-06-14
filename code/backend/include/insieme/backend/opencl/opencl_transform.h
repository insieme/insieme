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

	/**
	 * Represents a single kernel execution within a CallGraph which consists of:
	 * 1. An NDRange which models {workDim, [globalWorkOffset...], [globalWorkSize...], [localWorkSize...]}
	 * 2. A possible list of override requirements used by this call
	 * 3. A possible list of optionals which will be passed along the captured environment
	 *
	 * Note: the optionals specified in 3. must fit into an uint64 due to limitations in the runtime
	 */
	class CallContext {
		NDRangePtr ndrange;
		core::ExpressionList overrideRq;
		OptionalList optionals;
	public:
		const NDRangePtr& getNDRange() const { return ndrange; }
		void setNDRange(const NDRangePtr& ptr) { ndrange = ptr; }
		const core::ExpressionList& getOverrideRequirements() const { return overrideRq; }
		void setOverrideRequirements(const core::ExpressionList& lst) { overrideRq = lst; }
		const OptionalList& getOptionals() const { return optionals; }
		void setOptionals(const OptionalList& lst) { optionals = lst; }
	};
	typedef std::shared_ptr<CallContext> CallContextPtr;

	/**
	 * This context is shared by all Steps during OpenCL transformation and stores all important
	 * information required to generate code
	 */
	class StepContext {
	public:
		typedef utils::graph::Graph<CallContextPtr> CallGraph;
		/**
		 * Models all KhrXxxExtensions which are supported by the backend
		 */
		enum class KhrExtension { All, Fp64, ByteAddressableStore };
	private:
		/**
		 * Holds the original lambda which was generated by outline()
		 */
		core::LambdaExprPtr defaultLe;
		/**
		 * Holds the corresponding LWDataItemType which is associated with @defaultLe
		 */
		core::TypePtr defaultTy;
		/**
		 * Holds the default requirements which were determined by the analysis module
		 */
		VariableRequirementList defaultRq;
		/**
		 * Represents an execution graph where each vertex is a distinct call and each
		 * edge models a dependency which restricts static parallelism
		 */
		CallGraph graph;
		/**
		 * Represents a set of extension which are required by the generated kernel to operate
		 */
		std::set<KhrExtension> extensions;
		/**
		 * Name of the generated kernel which will be used by the runtime system to enqueue the execution
		 */
		std::string kernelName;
	public:
		void setDefaultLambdaExpr(const core::LambdaExprPtr& lambdaExpr) { defaultLe = lambdaExpr; }
		const core::LambdaExprPtr& getDefaultLambdaExpr() const { return defaultLe; }
		void setDefaultLWDataItemType(const core::TypePtr& type) { defaultTy = type; }
		const core::TypePtr& getDefaultLWDataItemType() const { return defaultTy; }
		const VariableRequirementList& getDefaultRequirements() const { return defaultRq; }
		void setDefaultRequirements(const VariableRequirementList& lst) { defaultRq = lst; }
		CallGraph& getCallGraph() { return graph; }
		const CallGraph& getCallGraph() const { return graph; }
		std::set<KhrExtension>& getExtensions() { return extensions; }
		const std::set<KhrExtension>& getExtensions() const { return extensions; }
		void setKernelName(const std::string& kernelName) { this->kernelName = kernelName; }
		const std::string& getKernelName() const { return kernelName; }
	};

	/**
	 * Represents an abstract processing step applied to an outlined lambdaExpr
	 */
	class Step : public PreProcessor {
	protected:
		core::NodeManager& manager;
		core::IRBuilder builder;
		StepContext& context;
		const OpenCLExtension& oclExt;
	public:
		Step(core::NodeManager& manager, StepContext& context) :
			PreProcessor(), manager(manager), builder(manager),
			context(context), oclExt(manager.getLangExtension<OpenCLExtension>())
		{}
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override { return code; }
	};

	/**
	 * Strips all arguments which are not solely devoted to the generated kernel.
     * Such additional arguments may be introduced during the process of outline
	 * if a data requirement uses additionally captured information.
	 */
	class FixParametersStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * Tries to simplify the given code by e.g. inlining or using the core's functionality
	 */
	class StmtOptimizerStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * Transform independent loops into OpenCL aware code and generate a corresponding call vertex
	 */
	class LoopOptimizerStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * May be used to modify kernel types, e.g. add the addition of __local prefetching
	 */
	class TypeOptimizerStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * May be used to optimize a given CallGraph
	 */
	class CallOptimizerStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
	 * Checks the integrity of the produced IR before it is handed off to the Sub-Backend
	 */
	class IntegrityCheckStep : public Step {
	public:
		using Step::Step;
		core::NodePtr process(const Converter& converter, const core::NodePtr& code) override;
	};

	/**
     * Outlines the given statement and captures any expressions indirectly required by @requirements
     */
	core::CallExprPtr outline(core::NodeManager& manager, const core::StatementPtr& stmt,
							  VariableRequirementList& requirements);

	/**
	 * Build the IR-Equivalent of get_work_dim()
	 */
	core::CallExprPtr buildGetWorkDim(core::NodeManager& manager);

	/**
	 * Build the IR-Equivalent of get_global_id()
	 */
	core::CallExprPtr buildGetGlobalId(core::NodeManager& manager, const core::ExpressionPtr& dim);

	/**
	 * Build the IR-Equivalent of get_global_size()
	 */
	core::CallExprPtr buildGetGlobalSize(core::NodeManager& manager, const core::ExpressionPtr& dim);

	/**
	 * Build the IR-Equivalent of get_local_id()
	 */
	core::CallExprPtr buildGetLocalId(core::NodeManager& manager, const core::ExpressionPtr& dim);

	/**
	 * Build the IR-Equivalent of get_local_size()
	 */
	core::CallExprPtr buildGetLocalSize(core::NodeManager& manager, const core::ExpressionPtr& dim);

	/**
	 * Build the IR-Equivalent of get_num_groups()
	 */
	core::CallExprPtr buildGetNumGroups(core::NodeManager& manager, const core::ExpressionPtr& dim);

	/**
	 * Build the IR-Equivalent of get_group_id()
	 */
	core::CallExprPtr buildGetGroupId(core::NodeManager& manager, const core::ExpressionPtr& dim);

	/**
	 * Build a callExpr which will register the given kernel within the runtime system
	 */
	core::CallExprPtr buildRegisterKernel(core::NodeManager& manager, unsigned int& id,
										  const StepContext& sc, const core::LambdaExprPtr& oclExpr);

	/**
	 * Build a callExpr which will execute the given kernel synchronously
	 */
	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const core::ExpressionPtr& ndrange,
										 const core::ExpressionList& requirements, const OptionalList& optionals);

	/**
	 * Build a callExpr which will execute the given kernel synchronously with information taken from the CallContext
	 */
	core::CallExprPtr buildExecuteKernel(core::NodeManager& manager, unsigned int id, const StepContext& sc, const CallContext& cc);

	/**
	 * Builds an execution graph for the given kernel, furthermore the impl tries to parallelize independent calls
	 */
	core::StatementList buildExecuteGraph(core::NodeManager& manager, unsigned int id, const StepContext& sc);

	/**
	 * Build an optional kernel argument in the proper IR construct
	 */
	OptionalPtr buildOptional(core::NodeManager& manager, const core::ExpressionPtr& expr, Optional::Modifier modifier);

	/**
	 * Convert a given NDRange into a runtime callable function
	 */
	core::LambdaExprPtr toIR(core::NodeManager& manager, const StepContext& sc, const NDRangePtr& ndrange);

	/**
	 * Convert the given VariableRequirement into a DataRequirement with an associated runtime callable function
	 */
	core::LambdaExprPtr toIR(core::NodeManager& manager, const StepContext& sc, const VariableRequirementPtr& var);

	/**
	 * Convert the given code, which must have been previously outlined via opencl::transform::outline(), into a set
	 * of lambdaExprs which represent pickable WI-variants
	 */
	std::vector<core::LambdaExprPtr> toOcl(const Converter& converter, core::NodeManager& manager, const core::NodePtr& code,
										   const core::CallExprPtr& callExpr, const VariableRequirementList& requirements,
										   const DeviceAnnotationPtr& deviceInfo);
}
}
}
}
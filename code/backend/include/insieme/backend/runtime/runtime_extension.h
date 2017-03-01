/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace backend {
namespace runtime {

	/**
	 * This class offers a list of IR extensions required to model concepts within the
	 * Insieme Runtime. The extensions include literals and types to model work items,
	 * data items and additional runtime functionality.
	 */
	class RuntimeExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		RuntimeExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		// import reference extension to utilize aliases
		IMPORT_MODULE(core::lang::ReferenceExtension);

		// -- Runtime entities ------
	    LANG_EXT_TYPE(ContextType, "irt_context")

		LANG_EXT_TYPE(WorkItemType, "irt_wi")
		LANG_EXT_TYPE(WorkItemImplType, "irt_wi_implementation_id")
		LANG_EXT_TYPE(WorkItemVariantType, "irt_wi_variant")
		LANG_EXT_TYPE(WorkItemVariantImplType, "(ref<irt_wi>)->unit")

		LANG_EXT_LITERAL(WorkItemVariantCtr, "WorkItemVariant", "((ref<irt_wi>)->unit)->irt_wi_variant")
		LANG_EXT_LITERAL(WorkItemImplCtr, "WorkItemImpl", "(list<irt_wi_variant>)->irt_wi_implementation_id")
		LANG_EXT_LITERAL(UnknownEffort, "unknownEffort", "(int<8>,int<8>)->uint<8>")


		/**
		 * Used to force the registration of work item implementations within the implementation table.
		 * The given work item implementation will not be executed
		 */
		LANG_EXT_LITERAL(RegisterWorkItemImpl, "registerImpl", "(irt_wi_implementation_id)->unit")
		LANG_EXT_LITERAL(JoinWorkItem, "irt_wi_join", "(ref<irt_wi>)->unit")
		LANG_EXT_LITERAL(ExitWorkItem, "irt_wi_end", "(ref<irt_wi>)->unit")

		// -- Data Item types and literals ------
		LANG_EXT_TYPE(TypeID, "irt_type_id")
		LANG_EXT_LITERAL(WrapLWData, "wrap", "('a)->irt_lwdi<'a>")
		LANG_EXT_LITERAL(UnwrapLWData, "unwrap", "(irt_lwdi<'a>)->'a")


		// -- IR interface ---------
		LANG_EXT_TYPE(JobType, "irt_parallel_job")

		LANG_EXT_LITERAL(CreateJob, "createJob", "(uint<8>,uint<8>,uint<8>,irt_wi_implementation_id,irt_lwdi<'a>)->irt_parallel_job")
		LANG_EXT_LITERAL(Parallel, "irt_parallel", "(irt_parallel_job)->ref<irt_wi>")
		LANG_EXT_LITERAL(Task, "irt_task", "(irt_parallel_job)->ref<irt_wi>")
		LANG_EXT_LITERAL(Region, "irt_region", "(irt_parallel_job)->ref<irt_wi>")

		LANG_EXT_LITERAL(Pfor, "irt_pfor", "(threadgroup,int<8>,int<8>,int<8>,irt_wi_implementation_id,irt_lwdi<'a>)->ref<irt_wi>")
		LANG_EXT_LITERAL(Merge, "irt_merge", "(ref<irt_wi>)->unit")


		/*
		 * An attribute marking an expression as being a region. For now,
		 * this attribute is only supported when being applied to a pfor-call.
		 */
		LANG_EXT_LITERAL(RegionAttribute, "region", "(uint<8>)->attribute")


		// -- Standalone runtime Handling ------
		LANG_EXT_LITERAL(RunStandalone, "runStandalone", "(irt_wi_implementation_id, irt_lwdi<'a>)->unit")

		// -- Code Utilities ------
		LANG_EXT_TYPE_WITH_NAME(WorkItemRange, "irt_work_item_range", "struct { begin : int<4>; end : int<4>; step : int<4>; }")

		LANG_EXT_LITERAL(GetWorkItemRange, "getRange", "(ref<irt_wi>)->struct { begin : int<4>; end : int<4>; step : int<4>; }")
		LANG_EXT_LITERAL(GetWorkItemArgument, "getArg", "(ref<irt_wi>, uint<4>, type<irt_lwdi<'p>>, type<'a>)->'a")



	};

} // end namespace encoder
} // end namespace core
} // end namespace insieme

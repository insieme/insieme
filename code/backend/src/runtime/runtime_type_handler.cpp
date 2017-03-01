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
#include <insieme/backend/runtime/runtime_extension.h>
#include "insieme/backend/runtime/runtime_type_handler.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/runtime/runtime_entities.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/core/lang/parallel.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		const TypeInfo* getLWDataItemStruct(ConversionContext& context, const core::TypePtr& type) {
			const Converter& converter = context.getConverter();

			// make sure it is only invoked using LW data items
			assert_true(DataItem::isLWDataItemType(type)) << "Only works on LW Data Items!";

			// get underlying tuple type
			core::TupleTypePtr tupleType =
			    static_pointer_cast<const core::TupleType>(static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0]);

			// convert to data item to tuple used within runtime
			core::TupleTypePtr fullTuple = DataItem::getUnfoldedLWDataItemType(tupleType);

			// obtain type information from base struct => use the same type
			return &converter.getTypeManager().getTypeInfo(context, fullTuple);
		}


		const TypeInfo* handleType(ConversionContext& context, const core::TypePtr& type) {
			const Converter& converter = context.getConverter();

			auto& mgr = converter.getNodeManager();
			auto& extension = mgr.getLangExtension<RuntimeExtension>();
			auto& parExt = mgr.getLangExtension<core::lang::ParallelExtension>();

			if(extension.isContextType(type)) {
				// use runtime definition of the context
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_context", "irt_all_impls.h");
			}

			if(extension.isWorkItemType(type)) {
				// use runtime definition of the work item type
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_work_item", "irt_all_impls.h");
			}

			if(extension.isTypeID(type)) {
				// use runtime definition of the id
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_type_id", "irt_all_impls.h");
			}

			if(extension.isWorkItemRange(type)) {
				// use runtime definition of the work item range type
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_work_item_range", "irt_all_impls.h");
			}

			if(DataItem::isLWDataItemType(type)) {
				// create a substitution struct - the same struct + the type id
				return getLWDataItemStruct(context, type);
			}

			// handle jobs
			const core::lang::BasicGenerator& basic = converter.getNodeManager().getLangBasic();

			// check for job types ...
			if(basic.isJob(type)) { return type_info_utils::createInfo(converter.getFragmentManager(), "irt_parallel_job", "ir_interface.h"); }

			if(basic.isThreadGroup(type)) { return type_info_utils::createInfo(converter.getFragmentManager(), "irt_work_group*", "ir_interface.h"); }

			if(parExt.isLock(type)) { return type_info_utils::createInfo(converter.getFragmentManager(), "irt_lock", "irt_lock.h"); }

			// it is not a special runtime type => let somebody else try
			return 0;
		}
	}

	TypeHandler RuntimeTypeHandler = &handleType;

} // end namespace ocl_standalone
} // end namespace backend
} // end namespace insieme

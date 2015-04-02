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

#include "insieme/backend/runtime/runtime_type_handler.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/runtime/runtime_entities.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		const TypeInfo* getLWDataItemStruct(const Converter& converter, const core::TypePtr& type) {
			// make sure it is only invoked using LW data items
			assert(DataItem::isLWDataItemType(type) && "Only works on LW Data Items!");

			// get underlying tuple type
			core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(
					static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0]);

			// convert to data item to tuple used within runtime
			core::TupleTypePtr fullTuple = DataItem::getUnfoldedLWDataItemType(tupleType);

			// obtain type information from base struct => use the same type
			return &converter.getTypeManager().getTypeInfo(fullTuple);
		}


		const TypeInfo* handleType(const Converter& converter, const core::TypePtr& type) {

			const Extensions& extensions = converter.getNodeManager().getLangExtension<Extensions>();

			if (*extensions.contextType == *type) {
				// use runtime definition of the context
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_context", "irt_all_impls.h");
			}

			if (*extensions.workItemType == *type) {
				// use runtime definition of the work item type
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_work_item", "irt_all_impls.h");
			}

			if (*extensions.typeID == *type) {
				// use runtime definition of the id
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_type_id", "irt_all_impls.h");
			}

			if (DataItem::isLWDataItemType(type)) {
				// create a substitution struct - the same struct + the type id
				return getLWDataItemStruct(converter, type);
			}

			// handle jobs
			const core::lang::BasicGenerator& basic = converter.getNodeManager().getLangBasic();

			// check for job types ...
			if(basic.isJob(type)) {
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_parallel_job", "ir_interface.h");
			}

			if(basic.isThreadGroup(type)) {
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_work_group", "ir_interface.h");
			}

			if(basic.isLock(type)) {
				return type_info_utils::createInfo(converter.getFragmentManager(), "irt_lock", "irt_lock.h");
			}

			// it is not a special runtime type => let somebody else try
			return 0;
		}

	}

	TypeHandler RuntimeTypeHandler = &handleType;

} // end namespace ocl_standalone
} // end namespace backend
} // end namespace insieme

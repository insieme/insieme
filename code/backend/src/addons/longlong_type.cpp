/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/backend/addons/longlong_type.h"

#include "insieme/core/lang/basic.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"

namespace insieme {
namespace backend {
namespace addons {

	namespace {

		const TypeInfo* LongLongTypeHandler(ConversionContext& context, const core::TypePtr& type) {
			const TypeInfo* skip = nullptr;

			// intercept 128-bit types and convert them to the long-long type
			const auto& base = type->getNodeManager().getLangBasic();

			// get the c-node manager
			c_ast::CNodeManager& manager = *context.getConverter().getCNodeManager();

			// check for the two special types
			if(base.isInt16(type)) { return type_info_utils::createInfo(manager, c_ast::PrimitiveType::LongLong); }
			if(base.isUInt16(type)) { return type_info_utils::createInfo(manager, c_ast::PrimitiveType::ULongLong); }

			// otherwise use default handling
			return skip;
		}
	}

	void LongLongType::installOn(Converter& converter) const {
		// registers type handler
		converter.getTypeManager().addTypeHandler(LongLongTypeHandler);
	}

} // end namespace addons
} // end namespace backend
} // end namespace insieme

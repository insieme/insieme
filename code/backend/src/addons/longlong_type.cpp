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

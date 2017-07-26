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
 */

#pragma once

#include "insieme/backend/c_ast/forward_decls.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace backend {

	class Converter;
	class ConversionContext;
	struct TypeInfo;

	struct VariableInfo {
		enum MemoryLocation {
			NONE,    /* < the variable is not a reference type */
			DIRECT,  /* < the variable represents the corresponding memory cell directly (e.g. a local variable) */
			INDIRECT /* < the variable is a pointer to the actually represented memory cell */
		};

		const TypeInfo* typeInfo;
		c_ast::VariablePtr var;
		MemoryLocation location;
	};

	class VariableManager {
		utils::map::PointerMap<core::VariablePtr, VariableInfo> infos;

	  public:
		VariableManager() : infos(){};

		const VariableInfo& getInfo(const core::VariablePtr& var) const;

		const VariableInfo& addInfo(ConversionContext& context, const core::VariablePtr& var, VariableInfo::MemoryLocation location);

		const VariableInfo& addInfo(ConversionContext& context, const core::VariablePtr& var, VariableInfo::MemoryLocation location, const TypeInfo& typeInfo);

		void remInfo(const core::VariablePtr& var);
	};

} // end namespace backend
} // end namespace insieme

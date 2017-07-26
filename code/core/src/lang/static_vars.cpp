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

#include "insieme/core/lang/static_vars.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {


	bool StaticVariableExtension::isStaticType(const TypePtr& type) const {
		auto tagType = type.isa<TagTypePtr>();
		if (!tagType || !tagType->isStruct()) return false;
		StructPtr structType = tagType->getRecord().as<StructPtr>();
		if(structType->getName()->getValue() != "__static_var") { return false; }
		auto fields = structType->getFields();
		if(fields.size() != 2) { return false; }
		if(fields[0]->getName()->getValue() != "initialized") { return false; }
		if(fields[1]->getName()->getValue() != "value") { return false; }
		if(!type.getNodeManager().getLangBasic().isBool(fields[0]->getType())) { return false; }
		return true;
	}

	TypePtr StaticVariableExtension::wrapStaticType(const TypePtr& type) const {
		IRBuilder builder(type.getNodeManager());
		return builder.structType(builder.stringValue("__static_var"), builder.parents(),
		                          toVector(builder.field("initialized", builder.getLangBasic().getBool()), builder.field("value", type)));
	}

	TypePtr StaticVariableExtension::unwrapStaticType(const TypePtr& type) const {
		assert_true(isStaticType(type)) << "Unable to unwrap non-static type.";
		return type.as<TagTypePtr>()->getRecord()->getFields()[1]->getType();
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme

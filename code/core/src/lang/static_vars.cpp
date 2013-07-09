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

#include "insieme/core/lang/static_vars.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {


	bool StaticVariableExtension::isStaticType(const TypePtr& type) const {
		if (!type || type->getNodeType() != NT_StructType) return false;
		StructTypePtr structType = type.as<StructTypePtr>();
		if (structType->getName()->getValue() != "__static_var") return false;
		if (structType.size() != 2) return false;
		if (structType[0]->getName()->getValue() != "initialized") return false;
		if (structType[1]->getName()->getValue() != "value") return false;
		if (!type.getNodeManager().getLangBasic().isBool(structType[0]->getType())) return false;
		return true;
	}

	TypePtr StaticVariableExtension::wrapStaticType(const TypePtr& type) const {
		IRBuilder builder(type.getNodeManager());
		return builder.structType(
				builder.stringValue("__static_var"),
				builder.parents(),
				toVector(
						builder.namedType("initialized", builder.getLangBasic().getBool()),
						builder.namedType("value", type)
				)
		);
	}

	TypePtr StaticVariableExtension::unwrapStaticType(const TypePtr& type) const {
		assert(isStaticType(type) && "Unable to unwrap non-static type.");
		return type.as<StructTypePtr>()[1]->getType();
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme

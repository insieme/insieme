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

#include "insieme/core/types/variable_sized_struct_utils.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace core {
namespace types {

// -------------------------------------------------------------------------------------------------------------------------
//                                                    Variable Sized Structs
// -------------------------------------------------------------------------------------------------------------------------


bool isVariableSized(const TypePtr& cur) {
	NodeType type = cur->getNodeType();
	switch(type) {
	case NT_ArrayType: return true;
	case NT_StructType: {
		StructTypePtr structType = cur.as<StructTypePtr>();
		return !structType.empty() && isVariableSized(structType.back()->getType());
	}
	case NT_TupleType: {
		TupleTypePtr tupleType = cur.as<TupleTypePtr>();
		return !tupleType.empty() && isVariableSized(tupleType.back());
	}
	case NT_UnionType: {
		UnionTypePtr unionType = cur.as<UnionTypePtr>();
		return any(unionType, [](const NamedTypePtr& cur) { return isVariableSized(cur->getType()); });
	}
	default: return false;
	}
}

TypePtr getRepeatedType(const TypePtr& cur) {
	assert(isVariableSized(cur) && "Only variable sized types contain repeated types.");

	NodeType type = cur->getNodeType();
	switch(type) {
	case NT_ArrayType: return cur.as<ArrayTypePtr>()->getElementType();
	case NT_StructType: {
		StructTypePtr structType = cur.as<StructTypePtr>();
		return getRepeatedType(structType.back()->getType());
	}
	case NT_TupleType: {
		TupleTypePtr tupleType = cur.as<TupleTypePtr>();
		return getRepeatedType(tupleType.back());
	}
	case NT_UnionType: {
		UnionTypePtr unionType = cur.as<UnionTypePtr>();
		for(auto cur : unionType) {
			if (isVariableSized(cur->getType())) return getRepeatedType(cur->getType());
		}
		break;
	}
	default: break;
	}

	assert(false && "Invalid classification as a variable sized type!");
	return TypePtr();
}

} // end namespace types
} // end namespace core
} // end namespace insieme

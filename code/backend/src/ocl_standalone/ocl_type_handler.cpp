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

#include "insieme/backend/ocl_standalone/ocl_type_handler.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/ocl_standalone/ocl_standalone_extensions.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

namespace insieme {
namespace backend {
namespace ocl_standalone {

	namespace {

		TypeInfo* handleType(const Converter& converter, const core::TypePtr& type) {

			Extensions extensions(converter.getNodeManager());

			// determine kind of address space
			char const * attribute;
			if (extensions.isGlobalType(type)) {
				attribute = "__global";
			} else if (extensions.isLocalType(type)) {
				attribute = "__local";
			} else if (extensions.isConstType(type)) {
				attribute = "__const";
			} else {
				// it is not an OpenCL type ..
				return 0;
			}

			// resolve sub-type info
			core::TypePtr subType = static_pointer_cast<const core::GenericType>(type)->getTypeParameter()[0];
			const TypeInfo& subInfo = converter.getTypeManager().getTypeInfo(subType);

			// copy info of actual type
			TypeInfo* res = new TypeInfo();
			*res = subInfo;

			// annotate the sub-type type to get the actual type
			res->rValueType = c_ast::attribute(std::string(attribute), res->rValueType);
			res->lValueType = c_ast::attribute(std::string(attribute), res->lValueType);
			return res;
		}

	}

	TypeHandler OpenCLTypeHandler = &handleType;

} // end namespace ocl_standalone
} // end namespace backend
} // end namespace insieme

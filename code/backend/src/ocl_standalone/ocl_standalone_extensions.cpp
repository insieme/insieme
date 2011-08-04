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

#include "insieme/backend/ocl_standalone/ocl_standalone_extensions.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"

namespace insieme {
namespace backend {
namespace ocl_standalone {

	namespace {

		const core::TypePtr getWrapperType(core::NodeManager& manager, const string& name) {
			core::ASTBuilder builder(manager);

			core::TypePtr alpha = builder.genericType("a");
			return builder.genericType("_ocl_wrap_" + name, toVector(alpha));
		}

		const core::LiteralPtr getUnwrapLiteral(core::NodeManager& manager, const string& name) {
			core::ASTBuilder builder(manager);

			core::TypePtr alpha = builder.genericType("a");
			core::TypePtr funType = builder.functionType(toVector(alpha), alpha, true);
			return builder.literal(funType, "_ocl_unwrap_" + name);
		}

	}


	Extensions::Extensions(core::NodeManager& manager)
		  : constWrapper(getWrapperType(manager, "const")),
			unwrapConst(getUnwrapLiteral(manager, "const")),
			globalWrapper(getWrapperType(manager, "global")),
			unwrapGlobal(getUnwrapLiteral(manager, "global")),
			localWrapper(getWrapperType(manager, "local")),
			unwrapLocal(getUnwrapLiteral(manager, "local")),
			privateWrapper(getWrapperType(manager, "private")),
			unwrapPrivate(getUnwrapLiteral(manager, "private")) {}


} // end namespace ocl_standalone
} // end namespace backend
} // end namespace insieme

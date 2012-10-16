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

#pragma once

#include "insieme/frontend/pragma/handler.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace frontend {
namespace cilk {


	template<typename T>
	struct CilkPragma : public pragma::Pragma, public pragma::AutomaticAttachable {

		CilkPragma(const clang::SourceLocation& 	startLoc,
					  const clang::SourceLocation& 	endLoc,
					  const std::string& 			name,
					  const pragma::MatchMap& 		mmap):
				Pragma(startLoc, endLoc, name, mmap) { }

		virtual core::NodePtr attachTo(const core::NodePtr& node, conversion::ConversionFactory& fact) const {
			core::IRBuilder builder(node->getNodeManager());
			core::NodePtr res;
			if (node->getNodeCategory() == core::NC_Statement) {
				res = builder.markerStmt(node.as<core::StatementPtr>());
			} else if (node->getNodeCategory() == core::NC_Expression) {
				res = builder.markerExpr(node.as<core::ExpressionPtr>());
			} else {
				assert(false && "...");
			}

			res->attachValue<T>();
			return res;
		}

	};


	/**
	 * Registers the handlers for Cilk pragmas
	 */
	void registerPragmaHandlers(clang::Preprocessor& pp);


} // end namespace cilk
} // end namespace frontend
} // end namespace insieme

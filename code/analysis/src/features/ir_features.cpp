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

#include "insieme/analysis/features/ir_features.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_statements.h"

#include "insieme/core/lang/basic.h"

namespace insieme {
namespace analysis {
namespace features {


	Value NumStatements::evaluateFor(const core::NodePtr& code) const {
		int sum = 0;
		core::visitDepthFirst(code, [&](const core::StatementPtr& cur) { sum++; });
		return sum;
	}

	Value NumArithmeticOps::evaluateFor(const core::NodePtr& code) const {
		const auto& basic = code->getNodeManager().getLangBasic();
		int sum = 0;
		core::visitDepthFirst(code, [&](const core::CallExprPtr& cur) {
			if (basic.isArithOp(cur->getFunctionExpr())) {
				sum++;
			}
		});
		return sum;
	}

	Value IsLeaf::evaluateFor(const core::NodePtr& code) const {
		// NOTE: this implementation is incomplete
		// search for a call to a non-literal function => leaf
		return !core::visitDepthFirstInterruptible(code, [&](const core::CallExprPtr& cur) {
			return cur->getFunctionExpr()->getNodeType() != core::NT_Literal;
		});
	}


} //end namespace features
} //end namespace analysis
} //end namespace insieme

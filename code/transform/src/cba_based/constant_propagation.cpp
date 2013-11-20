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

#include "insieme/transform/cba_based/constant_propagation.h"

#include <map>

#include "insieme/analysis/cba/analysis.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace transform {
namespace cba_based {

	using std::map;


	core::NodeAddress propagateConstants(const core::NodeAddress& target) {

		auto& mgr = target->getNodeManager();
		core::IRBuilder builder(mgr);

		auto trueLit = builder.boolLit(true);
		auto falseLit = builder.boolLit(false);

		// find a list of constants
		map<core::NodeAddress, core::NodePtr> constants;
		visitDepthFirstPrunable(target, [&](const core::ExpressionAddress& expr)->bool {
			// skip literals
			if (expr.isa<core::LiteralPtr>()) return true;

			// check boolean literals
			if (analysis::cba::isTrue(expr)) {
				constants[expr] = trueLit;
				return true;
			} else if (analysis::cba::isFalse(expr)) {
				constants[expr] = falseLit;
				return true;
			} else if (auto lit = analysis::cba::isIntegerConstant(expr)) {
				// check whether it is a literal
				constants[expr] = lit;
				return true;
			}

			// decent
			return false;
		});

		// replace all variable expressions by constants
		return target.switchRoot(core::transform::replaceAll(mgr, constants));
	}



} // end namespace cba_based
} // end namespace transform
} // end namespace insieme

/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/analysis/region/fun_call_selector.h"

#include <string.h>

#include <boost/regex.hpp>

#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"

namespace insieme {
namespace core {
namespace analysis {
namespace region {

	RegionList FunctionCallSelector::getRegions(const core::NodeAddress& code) const {
		RegionList res;

		core::visitDepthFirst(code, [&](const core::StatementAddress& cur) -> bool {
			core::StatementPtr stmt = cur.getAddressedNode();

			if(stmt->getNodeType() != core::NT_CallExpr) { return false; }

			const auto funExpr = stmt.as<core::CallExprPtr>()->getFunctionExpr();

			std::string name;

			if(const core::LiteralPtr literalPtr = funExpr.isa<core::LiteralPtr>()) {
				name = literalPtr->getStringValue();
			} else {
				if(core::annotations::hasAttachedName(funExpr)) {
					name = core::annotations::getAttachedName(funExpr);
				} else {
					// no name found, hence cannot match
					return false;
				}
			}

			if(boost::regex_search(name, boost::regex(nameSubString, boost::regex_constants::ECMAScript))) {
				res.push_back(Region(cur));
				return true;
			}

			return false;
		}, false);

		return res;
	}

} // end namespace region
} // end namespace analysis
} // end namespace core
} // end namespace insieme

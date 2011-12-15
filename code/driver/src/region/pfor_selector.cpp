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

#include "insieme/driver/region/pfor_selector.h"

#include "insieme/transform/pattern/ir_pattern.h"

namespace insieme {
namespace driver {
namespace region {

	namespace p = transform::pattern;
	namespace irp = transform::pattern::irp;

	RegionList PForBodySelector::getRegions(const core::NodePtr& node) const {
		// use pattern to identify sections

		// search for a pfor call any get body of lambda ..
		p::TreePatternPtr pattern = irp::callExpr(
					irp::literal("pfor"),
					p::any << p::any << p::any << p::any << p::aT(p::var("x",irp::compoundStmt(p::anyList))
				)
		);

		// now get all outermost instances of those
		pattern = p::outermost(pattern);

		auto match = pattern->matchAddress(core::NodeAddress(node));

		assert(match && "Pattern should always match!");

		// extract region list
		RegionList res;

		// test whether there are any matches
		if (!match->isVarBound("x")) {
			return res;
		}

		// collect regions
		auto list = match->getVarBinding("x").getList();
		for_each(list, [&](const core::NodeAddress& cur) {
			// convert matches
			res.push_back(core::static_address_cast<core::CompoundStmtAddress>(cur));
		});

		return res;
	}

} // end namespace region
} // end namespace driver
} // end namespace insieme


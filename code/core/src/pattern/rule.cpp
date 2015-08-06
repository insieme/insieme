/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#include "insieme/core/pattern/rule.h"

#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/generator.h"

namespace insieme {
namespace core {
namespace pattern {

core::NodePtr Rule::applyTo(const core::NodePtr& tree) const {
	auto match = pattern.matchPointer(tree);
	if(!match) {
		return core::NodePtr();
	}
	return generator.generate(*match);
}

core::NodePtr Rule::fixpoint(const core::NodePtr& tree) const {
	auto res = tree;
	
	// while applicable ..
	while(auto next = applyTo(res)) {
		// if nothing has changed => done
		if(res == next) {
			return res;
		}
		
		// try next iteration
		res = next;
	}
	
	// done
	return res;
}

Rule Rule::getNestedRule() const {
	return Rule(
	           aT(var("%root%", pattern)),
	           generator::substitute(generator::root, generator::var("%root%"), generator)
	       );
}

TreePtr Rule::applyTo(const TreePtr& tree) const {
	auto match = pattern.matchTree(tree);
	if(!match) {
		return TreePtr();
	}
	return generator.generate(*match);
}

} // end namespace pattern
} // end namespace core
} // end namespace insieme

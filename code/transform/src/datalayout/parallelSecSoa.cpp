/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/transform/datalayout/parallelSecSoa.h"
#include "insieme/transform/datalayout/datalayout_utils.h"


namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;

void ParSecSoa::transform() {
	NodeManager& m = mgr;
	IRBuilder builder(m);
	const NodeAddress tta(toTransform);
	std::vector<std::pair<ExprAddressSet, RefTypePtr>> toReplaceLists = createCandidateLists(tta);

	pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(m.getLangBasic().getArrayCreate1D(),
			pattern::any << var("nElems", pattern::any))));
	VariableMap<std::map<LiteralPtr, ExpressionPtr>> map;

	for(std::pair<ExprAddressSet, RefTypePtr> toReplaceList : toReplaceLists) {
		ExprAddressMap varReplacements;
		ExpressionMap nElems;

		for(ExpressionAddress oldVar : toReplaceList.first) {
			TypePtr newType = core::transform::replaceAll(m, oldVar->getType(), oldStructType,
					newStructType).as<TypePtr>();
std::cout << "NT: " << newStructType << " var " << *oldVar << std::endl;

			// check if local or global variable
			LiteralPtr globalVar = oldVar.isa<LiteralPtr>();

			// create new variables, local or global
			varReplacements[oldVar] = globalVar ?
					builder.literal(globalVar->getStringValue() + "_soa", newType).as<ExpressionPtr>() :
					builder.variable(newType).as<ExpressionPtr>();
		}

		// replacing the declarations of the old variables with new ones
		addNewDecls(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);

		const std::vector<core::StatementAddress> begin, end;
		//replace array accesses
		replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements);

		// assignments to the entire struct should be ported to the new struct members
//TODO	replaceAssignments(varReplacements, newStructType, oldStructType, tta, pattern::TreePattern(), nElems, replacements);

//		for(std::pair<core::NodeAddress, core::NodePtr> r : replacements) {
//			std::cout << "\nfrom ";
//			dumpPretty(r.first);
//			std::cout << "to ";
//			dumpPretty(r.second);
//		}

		//replace arguments
		for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
//			std::cout << "from " << *vr.first << " to " << *vr.second << std::endl;

			if(vr.first.isa<VariableAddress>())
				visitDepthFirst(vr.first.getParentAddress(2), [&](const VariableAddress& var) {
					if(compareVariables(vr.first, var)) {
						replacements[var] = vr.second;
					}
				});
		}

	}
}


} // datalayout
} // transform
} // insieme

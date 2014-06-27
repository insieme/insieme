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

//#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/transform/datalayout/aos_to_soa.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;

AosToSoa::AosToSoa(core::NodePtr toTransform) {
	std::set<VariablePtr> structs;

	pattern::TreePatternPtr structVar = pattern::irp::variable(pattern::aT(pattern::irp::refType(pattern::irp::arrayType(
			pattern::irp::structType(*pattern::any)))));

	pattern::irp::matchAllPairs(structVar, toTransform, [&](const NodePtr& match, pattern::NodeMatch nm) {
		structs.insert(match.as<VariablePtr>());

	});

	std::map<VariablePtr, StructTypePtr> newStructTypes;
	for(VariablePtr struct_ : structs) {


		StructTypePtr oldType = struct_->getType();
		for(TypePtr memberType) {
			std::cout << *struct_->getType() << std::endl;
		}
	}
}


} // datalayout
} // transform
} // insieme

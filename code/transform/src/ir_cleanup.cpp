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

#include "insieme/transform/ir_cleanup.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/transform/connectors.h"
#include "insieme/transform/pattern/ir_pattern.h"

#include "insieme/transform/dfabased/const_prop.h"

#include "insieme/utils/cmd_line_utils.h"

namespace insieme { namespace transform {

using namespace insieme::core;

namespace {

// TODO: split elimination of refs and arrays ... one after another

bool isArrayType(const TypePtr& cur) {

	// check for null pointer
	if (!cur) {
		return false;
	}

	// accept pure array type
	if (cur->getNodeType() == NT_ArrayType) {
		return true;
	}

	// also accept ref/array combination
	if ((cur->getNodeType() == NT_RefType && 
		 static_pointer_cast<const RefType>(cur)->getElementType()->getNodeType() == NT_ArrayType)) 
	{
		return true;
	}
	return false;
}


core::NodePtr removePseudoArraysInStructs(const core::NodePtr& node) {

	// Step 1) search list of structs containing arrays as elements
	utils::set::PointerSet<StructTypePtr> structs;

	// search for the property
	visitDepthFirstOnce(node, makeLambdaVisitor([&](const NodePtr& cur){
		if (cur->getNodeType() != NT_StructType) {
			return;
		}

		StructTypePtr type = static_pointer_cast<const StructType>(cur);
		if (any(type->getEntries(), [](const core::NamedTypePtr& cur)->bool { return isArrayType(cur->getType()); })) {
			structs.insert(type);
			return;
		}

	}, true));

	// print list of structs
	for_each(structs, [](const StructTypePtr& cur) {
		std::cout << "Current struct: " << *cur << std::endl;
	});

	return node;
}

} // end anonymous namespace


core::NodePtr cleanup(const core::NodePtr& node) {

	// start by doing nothing ..
	core::NodePtr res = node;

	// remove unnecessary array indirections
//	res = removePseudoArraysInStructs(res);
//	res = normalizeLoops(res);
//	res = removeUnecessaryDerefs(res);

	insieme::analysis::polyhedral::scop::mark(res);

	res = deadBranchElimination(res);

	//res = polyhedralSemplification(res);

	res = eliminateRedundantAssignments(res);

	res = eliminatePseudoArrays(res);

	if(CommandLineOptions::ConstantPropagation) {
		LOG(INFO) << "Performing Constant Propagation on input program";
		res = doConstProp(res->getNodeManager(), res);
	}
	// done
	return res;
}

} // end of namespace transform
} // end of namespace insieme

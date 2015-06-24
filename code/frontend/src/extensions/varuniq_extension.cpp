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

#include <algorithm>
#include <iomanip>
#include <ostream>
#include <string>

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_accessor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/frontend/extensions/varuniq_extension.h"
#include "insieme/utils/logging.h"

using namespace insieme::analysis;
using namespace insieme::core;
using namespace insieme::frontend::extensions;

VarUniqExtension::VarUniqExtension(NodeAddress frag): frag(frag), dep(frag) {
}

/// Do the actual variable ID replacement in the complete IR "tree" for the variable "from" to a variable with ID "to".
/// Checks need to be done beforehand, especially to ensure that the target ID "to" is not used by another
/// variable with the same parameters (type, etc) in the same scope. To do these checks, you can for example
/// utilize DataDependence::getDefs(unsigned int).
NodeAddress VarUniqExtension::renameVar(NodeAddress tree, VariableAddress from, unsigned int to) {
	// set up the node manager
	NodeManager& mgr=tree->getNodeManager();

	// convert from node addresses to node ptrs
	NodePtr treeptr=tree.getAddressedNode(),
	        fromptr=from.getAddressedNode(),
	        toptr=NodeAddress(Variable::get(mgr, from->getType(), to)),
	        newtreeptr=transform::replaceAll(mgr, treeptr, fromptr, toptr, false);

	// return the newly generated IR tree
	return NodeAddress(newtreeptr);
}

/// findPerfectID assigns each variable in consecutive (execution) order an increasing, unique ID.
std::map<VariableAddress, unsigned int> VarUniqExtension::findPerfectID(std::vector<VariableAddress> vars) {
	std::map<VariableAddress, unsigned int> ret;
	unsigned int ctr=0;
	for (auto var: vars)
		ret.emplace(var, ctr++);
	return ret;
}

/// Return the updated code with unique variable identifiers.
NodeAddress VarUniqExtension::IR(bool renumberUnused) {
	NodeAddress ret=frag;

	// convert boolean parameter to functional for node selection
	std::function<bool(VariableAddress)>
		pred=[this](const VariableAddress &def){ return this->dep.getUse(def).size(); };
	if (renumberUnused)
		pred=[]    (const VariableAddress &   ){ return true; };

	// do the actual variable replacement with a multi-step iteration until a fixed-point is reached
	// first, calculate the goal
	std::map<VariableAddress, unsigned int> goalID=findPerfectID(dep.getDefs(pred));
	DataDependence newdep=dep;

	// second: iterate over the goal until empty
	while (!goalID.empty()) {
		// third: retrieve and remove the first target from the map
		std::pair<VariableAddress, unsigned int> p=*(goalID.begin());
		goalID.erase(goalID.begin());

		// fourth, check if the target variable ID is already unused
		// four A: there is still a definition for the target variable ID
		if (newdep.getDefs(p.second).size()) {
			// get unused ID
			unsigned int unusedID=findUnusedID(newdep.getDefs()); // all IDs are considered here: no ID clash!
			// rename the variable to the unused ID
			ret=renameVar(ret, p.first, unusedID);
			// append the previously removed element at the end of the map
			FIXME;

		// four B: for the target variable ID no definition could be found
		} else {
			// rename the variable directly to the target ID
			ret=renameVar(ret, p.first, p.second);
		}
	}

	// step 6: done
	return ret;
}

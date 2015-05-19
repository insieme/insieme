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

#include <iomanip>
#include <ostream>
#include <string>

#include "insieme/core/ir_node.h"
#include "insieme/frontend/extensions/varuniq_extension.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::frontend::extensions;

VarUniqExtension::VarUniqExtension(NodeAddress frag): frag(frag), def(0) {
	visit(frag);
}

/// Debug routine to print the given node and its immediate children. The node address is the only mandatory
/// argument; the other arguments are a textual description, and a start index and a node count. The start index
/// tells the subroutine where to start printing (0 means the given node itself, 1 means the first child node,
/// n means the n-th child node. The count gives the number of child nodes which should be printed;
/// 0 means print only the given node, 1 means print only one child node, etc.
void VarUniqExtension::printNode(const NodeAddress &node, std::string descr, unsigned int start, int count) {
	// determine total number of child nodes, and adjust count accordingly
	auto children=node.getChildAddresses();
	if (count<0) count=children.size();

	// if requested (start=0), print the node itself with some debug information
	if (start==0) {
		if (!descr.empty()) descr=" ("+descr+")";
		std::cout << std::endl << node.getNodeType() << descr << ": " << node << std::endl;
		start++;
	}

	// iterate over the requested children to complete the output
	for (unsigned int n=start-1; n<start-1+count; n++)
		std::cout << "\t-" << n << "\t" << children[n].getNodeType() << ": " << *children[n] << std::endl;
}

/// Generic visitor (used for every non-implemented node type) to visit all children of the current node.
void VarUniqExtension::visitNode(const NodeAddress &node) {
	for (auto child: node->getChildList()) visit(child);
}

void VarUniqExtension::visitVariable(const VariableAddress &var) {
	// get variable definition point and increase counter for given variable
	VariableAddress defpt=getVarDefinition(var);
	if (defpt==var) {
		def++;
		unsigned int id=nextID();
		use  [defpt]=0;
		varid[defpt]=id;
		idstaken[id]=defpt;
	} else {
		use[defpt]++;
		//std::cout << var << " (" << *var << ") defined at " << def << " (" << *def << ")" << std::endl;
	}

	// visit children
	for (auto child: var->getChildList()) visit(child);
}

/// Return the updated code with unique variable identifiers.
NodeAddress VarUniqExtension::IR() {
	// print some status information for debugging
	for (auto dups: varid)
		std::cout << "var " << dups.first << " (" << dups.second << ") found " << use[dups.first] << "×" << std::endl;
	std::cout << "Found " << def << " variable defs, " << allUses() << " uses." << std::endl;

	// return the newly generated code
	return frag;
}

/// Find the address where the given variable has been defined.
VariableAddress VarUniqExtension::getVarDefinition(const VariableAddress& var) {
	// first, save the pointer of the given variable so that we have something to compare with
	VariablePtr varptr=var.getAddressedNode();
	NodeAddress cur=var;

	while (!cur.isRoot()) {
		NodeAddress var=cur.getParentAddress();

		switch (var->getNodeType()) {
		case NT_BindExpr: {
			for (auto n: var.as<BindExprAddress>()->getParameters())
				if (n.as<VariablePtr>()==varptr) return n;
			break; }
		case NT_CompoundStmt: {
			auto compound=var.as<CompoundStmtAddress>();
			for (int i=cur.getIndex(); i>=0; i--)
				if (DeclarationStmtAddress decl=compound[i].isa<DeclarationStmtAddress>()) {
					auto n=decl->getVariable();
					if (n.as<VariablePtr>()==varptr) return n;
				}
			break; }
		case NT_Parameters: {   // FIXME: this variable is a parameter; trace var to outer scope
			ParametersAddress par=var.as<ParametersAddress>();
			for (VariableAddress n: par->getParameters())
				if (n.as<VariablePtr>()==varptr) return n;
			break; }
		case NT_Lambda: {
			for (auto n: var.as<LambdaAddress>()->getParameters())
				if (n.as<VariablePtr>()==varptr) return n;
			break; }
		case NT_LambdaBinding: {
			auto n=var.as<LambdaBindingAddress>()->getVariable();
			if (n.as<VariablePtr>()==varptr) return n;
			break; }
		case NT_LambdaDefinition: {
			if (auto n=var.as<LambdaDefinitionAddress>()->getBindingOf(varptr))
				return n->getVariable();
			break; }
		case NT_ForStmt: {
			auto n=var.as<ForStmtAddress>()->getIterator();
			if (n.as<VariablePtr>()==varptr) return n;
			break; }
		default: break;
		}
		cur=var;
	}

	// we have reached the root, and never found a binding for the variable; return the variable address itself
	return var;
}

/// Return the next available ID for a variable renaming.
unsigned int VarUniqExtension::nextID() {
	unsigned int ret=0;
	while (idstaken.find(ret)!=idstaken.end()) ++ret;
	return ret;
}

/// Return the total of all variable uses.
unsigned int VarUniqExtension::allUses() {
	unsigned int ret=0;
	for (auto pair: use) ret+=pair.second;
	return ret;
}

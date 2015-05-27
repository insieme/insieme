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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_accessor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
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

	// do not change anything inside derived constructs
	if (lang::isDerived(defpt.getParentNode())) return;

	// we did not find any earlier occurrence of the given variable
	if (defpt==var) {
		//std::cout << *(var.getParentAddress().getParentAddress());
		//printNode(var.getParentAddress(), "variable definition "+std::to_string(def)); std::cout << std::endl;
		if (lang::isDerived(var.getParentNode()))
			printNode(var.getParentAddress(), "is derived");

		def++;
		unsigned int id=nextID();
		use  [defpt]=0;
		varid[defpt]=id;
		idstaken[id]=defpt;

	// the variable is not defined but used here, increase use counter
	} else {
		use[defpt]++;
	}

	// visit children
	for (auto child: var->getChildList()) visit(child);
}

/// Return the updated code with unique variable identifiers.
NodeAddress VarUniqExtension::IR() {
	// print some status information for debugging
	//for (auto dups: varid)
	//	std::cout << "var " << dups.first << " (" << dups.second << ") found " << use[dups.first] << "×" << std::endl;
	std::cout << "Found " << def << " variable defs, " << allUses() << " uses." << std::endl;

	// now that everything is in place, do the actual replacement
	NodeManager& mgr=frag->getNodeManager();
	NodePtr newfrag=frag.getAddressedNode();
	for (auto id: idstaken) {
		if (false && id.first<=5) {
			printNode(id.second);
			VariablePtr var3=Variable::get(mgr, id.second->getType(), id.first);
			printNode(NodeAddress(var3));
		}
		NodePtr oldvar=id.second.getAddressedNode(),
		        newvar=NodeAddress(Variable::get(mgr, id.second->getType(), id.first)),
		        oldfrag=newfrag;
		newfrag=transform::replaceAll(mgr, oldfrag, oldvar, newvar, false);
		//std::cout << "node count: " << countNodes(NodeAddress(oldfrag)) << " " << countNodes(NodeAddress(newfrag)) << std::endl;
	}

	// return the newly generated code
	return NodeAddress(newfrag);
}

/// Find the address where the given variable has been defined.
VariableAddress VarUniqExtension::getVarDefinition(const VariableAddress& givenaddr) {
	// first, save the pointer of the given variable so that we have something to compare with
	VariablePtr givenptr=givenaddr.getAddressedNode();
	NodeAddress current=givenaddr;

	while (!current.isRoot()) {
		NodeAddress oneup=current.getParentAddress();

		switch (oneup->getNodeType()) {
		case NT_BindExpr: {
			for (auto n: oneup.as<BindExprAddress>()->getParameters())
				if (n.as<VariablePtr>()==givenptr) return n;
			break; }
		case NT_CompoundStmt: {
			auto compound=oneup.as<CompoundStmtAddress>();
			for (int i=current.getIndex(); i>=0; i--)
				if (DeclarationStmtAddress decl=compound[i].isa<DeclarationStmtAddress>()) {
					auto n=decl->getVariable();
					if (n.as<VariablePtr>()==givenptr) return n;
				}
			break; }
		case NT_Parameters: {   // FIXME: this variable is a parameter; trace var to outer scope
			ParametersAddress par=oneup.as<ParametersAddress>();
			for (VariableAddress n: par->getParameters())
				if (n.as<VariablePtr>()==givenptr) return n;
			break; }
		case NT_Lambda: {
			for (auto n: oneup.as<LambdaAddress>()->getParameters())
				if (n.as<VariablePtr>()==givenptr) return n;
			break; }
		case NT_LambdaBinding: {
			auto n=oneup.as<LambdaBindingAddress>()->getVariable();
			if (n.as<VariablePtr>()==givenptr) return n;
			break; }
		case NT_LambdaDefinition: {
			if (auto n=oneup.as<LambdaDefinitionAddress>()->getBindingOf(givenptr))
				return n->getVariable();
			break; }
		case NT_ForStmt: {
			auto n=oneup.as<ForStmtAddress>()->getIterator();
			if (n.as<VariablePtr>()==givenptr) return n;
			break; }
		default: break;
		}
		current=oneup; // we did not find a binding yet, try to find it one level up
	}

	// we have reached the root, and never found a binding for the variable; return the given variable address
	return givenaddr;
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

unsigned int VarUniqExtension::countNodes(const NodeAddress &node) {
	unsigned int count=0;
	core::visitDepthFirst(node, [&](const core::NodeAddress &n) { ++count; });
	return count;
}

int VarUniqExtension::nodeDelta(const NodeAddress &n1, const NodeAddress &n2) {
	return countNodes(n1)-countNodes(n2);
}

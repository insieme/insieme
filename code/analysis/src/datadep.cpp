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

#include "insieme/analysis/datadep.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_accessor.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::analysis;

/// DataDependence constructor: Collect data about the given code fragment by kicking off the visiting process.
DataDependence::DataDependence(NodeAddress frag): frag(frag), count(0) {
	visit(frag);
}

/// Generic visitor (used for every non-implemented node type) to visit all children of the current node.
void DataDependence::visitNode(const NodeAddress &node) {
	// if not a derived construct, visit the children
	if (!lang::isDerived(node))
		for (auto child: node->getChildList()) visit(child);
}

void DataDependence::visitVariable(const VariableAddress &var) {
	bool dbg_vars=false, dbg_class=true, dbg_misclassified=true, misclassified=false;
	boost::optional<VariableAddress> comp;
	if (dbg_vars)
		std::cout << "visit variable: " << var;

	count++;

	// get variable definition point and increase counter for given variable
	VariableAddress defpt=getDef(var);

	if (defpt==var) recordDef(defpt);   // the given variable is not defined somewhere else but here
	else recordUse(defpt, var);         // the variable is not defined but used here

	if (dbg_vars) {
		std::string defuse=(defpt==var? "def:": "use:");
		std::cout << " (" << defuse << *var << ")";
	}
	if (dbg_class) {
		if (var.getDepth()>=3) {
			std::vector<int> diff(var.getDepth());
			diff.pop_back(); diff.pop_back(); diff[diff.size()-1]--;
			comp=DataDependence::otherVar(var, diff);
			if (comp && comp->getAddressedNode().as<VariablePtr>() == var.getAddressedNode().as<VariablePtr>())
				misclassified=true;
		}
	}
	if (dbg_vars) {
		if (misclassified) std::cout << " misclassified";
		std::cout << std::endl;
	}
	if (misclassified && dbg_misclassified) {
		auto n=commonAncestor(var, *comp);
		if (n) {
			std::vector<int> diff(n->getDepth());
			diff[diff.size()-1]++;
			diff.push_back(0);
			printNode(*otherNode(*n, diff));
		}
	}

	// visit children
	visitNode(var);
}

/// Records a variable definition by creating appropriate data structures in our protected "uses" class member.
void DataDependence::recordDef(const VariableAddress &def) {
	auto ret=uses.insert(std::pair<VariableAddress, std::vector<VariableAddress> >(def, std::vector<VariableAddress>()));
	if (!ret.second && ret.first->first!=def) {   // the definition point does not match the recorded definition point
		LOG(ERROR) << "Cannot insert a single variable with two distinct definition points!" << std::endl;
	}
}

/// Records a variable use by linking its definition point to the use address in the "uses" class member.
void DataDependence::recordUse(const VariableAddress &def, const VariableAddress &use) {
	// first, determine whether some other uses have already been recorded, and preserve these
	std::vector<VariableAddress> uselist;
	auto it=uses.find(def);
	bool isnew=0;
	if (it==uses.end()) isnew=1;
	else uselist=it->second;

	// second, push the new use address to the vector of uses
	uselist.push_back(use);

	// third, update the definition point with the correspending use vector
	if (isnew) {
		std::pair<VariableAddress, std::vector<VariableAddress> > value={def, uselist};
		uses.insert(value);
	} else
		it->second=uselist;
}

/// Returns the indices of the path of the given address as a vector of integers.
std::vector<unsigned int> DataDependence::addressVector(const NodeAddress &a) {
	std::vector<unsigned int> ret(a.getDepth());
	NodeAddress t=a;
	for (int i=a.getDepth()-1; i>=0; --i) {
		ret[i]=t.getIndex();
		if (!t.isRoot()) t=t.getParentAddress();
	}
	return ret;
}

/// Eventually return the other node that can be found relative (with offset addressDiff) to the given node.
boost::optional<NodeAddress> DataDependence::otherNode(const NodeAddress &a, std::vector<int> addressDiff) {
	// set up the base
	boost::optional<NodeAddress> nothing;
	NodeAddress reta=a.getRootAddress();
	std::vector<unsigned int> base=addressVector(a);
	if (!addressDiff.size() || addressDiff[0]) return nothing;

	// extend the root node according to the values calculated from the base address and the address diff
	for (unsigned int i=1; i<addressDiff.size(); ++i) {
		unsigned int idx=addressDiff[i];   // we know that addressDiff[i] is defined for every i (if negative: invalid)
		if (i<base.size()) idx+=base[i];   // add base[i] only after checking that base is long enough
		std::vector<NodeAddress> children=reta.getChildAddresses();
		if (idx>=children.size()) return nothing;
		else reta=children[idx];
	}
	return boost::optional<NodeAddress>(reta);
}

/// Eventually return the other variable address that can be found relative (with offset addressDiff) to the
/// given node address.
boost::optional<VariableAddress> DataDependence::otherVar(const NodeAddress &a, std::vector<int> addressDiff) {
	boost::optional<NodeAddress> optnode=otherNode(a, addressDiff);
	if (optnode && optnode->getNodeType()==NT_Variable)
		return boost::optional<VariableAddress>(optnode->as<VariableAddress>());
	// else
		return boost::optional<VariableAddress>();
}

/// Find the address where the given variable has been defined.
VariableAddress DataDependence::getDef(const VariableAddress& givenaddr) {
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
		case NT_LambdaExpr: {
			//break;
			auto n=oneup.as<LambdaExprAddress>()->getVariable();
			if (n.as<VariablePtr>()==givenptr) return n;
			break;
		}
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

/// Get all variable definitions in the given code fragment that satisfy the given predicate. If not explicitly set,
/// the predicate is true for all definitions, yielding any variable definition in the given code fragment.
std::vector<VariableAddress> DataDependence::getDefs(std::function<bool(const VariableAddress&)> predicate) {
	std::vector<VariableAddress> defs;
	for (auto def: uses)
		if (predicate(def.first)) defs.push_back(def.first);
	return defs;
}

/// Get the definition of all variables that have the given ID as their internal variable number. As this ID is not
/// unique, several variable nodes can be returned.
std::vector<VariableAddress> DataDependence::getDefs(unsigned int id) {
	std::function<bool(const VariableAddress &def)> pred=[this, id](const VariableAddress &def) {
		return getVarID(def)==id;
	};
	return getDefs(pred);
}

/// Return an array with all of the uses of the given variable definition point. If the variable definition point is
/// not yet known, call getDef first.
std::vector<VariableAddress> DataDependence::getUse(const VariableAddress& def) {
	std::vector<VariableAddress> ret;     // return an empty array in case we cannot find the variable definition
	auto it=uses.find(def);
	if (it!=uses.end()) ret=it->second;
	return ret;
}

/// Return the total number of variables found in this code block.
unsigned int DataDependence::size() {
	return count;
}

/// Get the internal variable ID for the given variable address. This method is private, as the
/// variable ID is non-portable.
unsigned int DataDependence::getVarID(const VariableAddress &var) {
	return var.getAddressOfChild(1).as<UIntValueAddress>()->getValue();
}

/// Return the address of the element that is common to both addresses.
boost::optional<NodeAddress> DataDependence::commonAncestor(const NodeAddress &n1, const NodeAddress &n2) {
	boost::optional<NodeAddress> ret;
	unsigned int max=n1.getDepth(), shorter=0;
	if (n2.getDepth()<max) { max=n2.getDepth(); shorter=1; }
	unsigned int matching=max;

	std::vector<NodeAddress> up={n1.getParentAddress(n1.getDepth()-max), n2.getParentAddress(n2.getDepth()-max)};
	for (unsigned int i=max; i>0; --i) {
		if (up[0].getIndex()!=up[1].getIndex()) matching=i-1;
		if (i>0) for (unsigned int j=0; j<2; ++j) up[j]=up[j].getParentAddress();
	}

	if (matching>0) {
		if (shorter==0) ret=boost::optional<NodeAddress>(n1.getParentAddress(max-matching));
		else            ret=boost::optional<NodeAddress>(n2.getParentAddress(max-matching));
	}
	return ret;
}

/// Debug routine to print the given node and its immediate children. The node address is the only mandatory
/// argument; the other arguments are a textual description, and a start index and a node count. The start index
/// tells the subroutine where to start printing (0 means the given node itself, 1 means the first child node,
/// n means the n-th child node. The count gives the number of child nodes which should be printed;
/// 0 means print only the given node, 1 means print only one child node, etc.
void DataDependence::printNode(const NodeAddress &node, std::string descr, unsigned int start, int count) {
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

/// Print the chain of types that the given node is contained in. Count says how many nodes are considered at most.
void DataDependence::printTypeChain(const NodeAddress &node, std::string descr, int count) {
	if (!count) return;
	if (!descr.empty()) std::cout << "(" << descr << "):";
	NodeAddress addr=node;
	while (count--) {
		std::cout << " " << addr.getNodeType();
		//if (lang::isDerived(addr)) std::cout << "(derived)";
		if (!addr.isRoot()) addr=addr.getParentAddress();
		else count=0;
	}
	std::cout << std::endl;
}

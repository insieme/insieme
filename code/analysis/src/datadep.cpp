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
#include <sstream>
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

/// Return the total number of variables found in this code block.
unsigned int DataDependence::size() {
	return count;
}

/// Generic visitor (used for every non-implemented node type) to visit all children of the current node.
void DataDependence::visitNode(const NodeAddress &node) {
	// if not a derived construct, visit the children
	if (!lang::isDerived(node))
		for (auto child: node->getChildList()) visit(child);
}

/// We found a variable: Count it, record it, and visit its children (if available?!)
void DataDependence::visitVariable(const VariableAddress &var) {
	// first, turn towards the variable itself and record its occurrence
	recordVar(var);

	// then, do some bookkeeping
	count++;
	visitNode(var);
}

/// Record the given variable so that in the end all variables are collected in a map.
void DataDependence::recordVar(const VariableAddress &var) {
	VariablePtr vptr=var.getAddressedNode();
	auto it_ok=uses.insert(std::pair<VariablePtr, std::vector<VariableAddress> >(vptr, std::vector<VariableAddress>{var}));
	if (!it_ok.second)
		it_ok.first->second.push_back(var);
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
boost::optional<VariableAddress> DataDependence::getDef(const VariableAddress& givenaddr) {
	boost::optional<VariableAddress> ret;   // if variable is not found in code fragment, return nothing
	auto found=uses.find(givenaddr.getAddressedNode());
	if (found!=uses.end())
		ret=boost::optional<VariableAddress>(found->second.front());
	return ret;
}

/// Get all variable definitions in the given code fragment that satisfy the given predicate. If not explicitly set,
/// the predicate is true for all definitions, yielding any variable definition in the given code fragment.
std::vector<VariableAddress> DataDependence::getDefs(std::function<bool(const VariableAddress&)> predicate) {
	std::vector<VariableAddress> defs;
	for (auto def: uses)
		if (predicate(def.second.front())) defs.push_back(def.second.front());
	std::sort(defs.begin(), defs.end());
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
/// not known, an empty result is provided.
std::vector<VariableAddress> DataDependence::getUse(const VariableAddress& def) {
	std::vector<VariableAddress> ret;           // return an empty array in case we cannot find the variable definition
	auto it=uses.find(def.getAddressedNode());
	if (it!=uses.end()) ret=it->second;
	ret.erase(ret.begin());    // remove the first element of the array as this is the definition point of the variable
	return ret;
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
std::string DataDependence::printNode(const NodeAddress &node, std::string descr, unsigned int start, int count) {
	std::stringstream strbuf;

	// determine total number of child nodes, and adjust count accordingly
	auto children=node.getChildAddresses();
	if (count<0) count=children.size();

	// if requested (start=0), print the node itself with some debug information
	if (start==0) {
		if (!descr.empty()) descr=" ("+descr+")";
		strbuf << std::endl << node.getNodeType() << descr << ": " << node << std::endl;
		start++;
	}

	// iterate over the requested children and complete the output
	for (unsigned int n=start-1; n<start-1+count; n++)
		strbuf << "\t-" << n << "\t" << children[n].getNodeType() << ": " << *children[n] << std::endl;
	return strbuf.str();
}

/// Return a printed representation of the nodes in between n1 and n2.
std::string DataDependence::printRange(const NodeAddress &n1, const NodeAddress &n2, std::string descr) {
	std::stringstream strbuf;

	// derive the common ancestor of n1 and n2, and the vector representation of the corresponding paths
	boost::optional<NodeAddress> a=commonAncestor(n1, n2);
	if (!a) return "No common ancestor for "+toString(n1)+" and "+toString(n2)+".\n";
	std::vector<unsigned int>
	        n1vec=addressVector(n1),
	        n2vec=addressVector(n2);

	// remove prefixes of paths n1, n2 that correspond to the common ancestor
	n1vec.erase(n1vec.begin(), n1vec.begin()+a->getDepth());
	n2vec.erase(n2vec.begin(), n2vec.begin()+a->getDepth());

	// show trace from a to n1
	std::vector<NodeAddress> trace{*a};
	for (int d: n1vec) trace.push_back(trace.back().getChildAddresses()[d]);
	strbuf << printTrace(trace, true, descr);

	// show trace from a to n2
	trace.clear();
	trace.push_back(*a);
	for (int d: n2vec) trace.push_back(trace.back().getChildAddresses()[d]);
	strbuf << printTrace(trace, false) << std::endl;

	return strbuf.str();
}

/// Return a printed representation of the trace (list of node addresses that are related) given.
std::string DataDependence::printTrace(std::vector<NodeAddress> trace, bool printHead, std::string descr) {
	std::stringstream strbuf;

	NodeAddress headnode=trace.front();
	if (printHead)
		strbuf << headnode << (!descr.empty()? " ("+descr+")": "") << std::endl;
	strbuf << "\t\t" << headnode.getNodeType();
	if (headnode.getNodeType()!=NT_LambdaBinding)
		strbuf << ": " << printer::PrettyPrinter(headnode, printer::PrettyPrinter::PRINT_SINGLE_LINE, 1) << std::endl;
	for (auto it=trace.begin()+1; it!=trace.end(); ++it) {
		strbuf << "\t-" << it->getIndex() << "\t" << it->getNodeType();
		if (it->getNodeType()!=NT_LambdaBinding)
			strbuf << ": " << printer::PrettyPrinter(*it, printer::PrettyPrinter::PRINT_SINGLE_LINE, 1);
		strbuf << std::endl;
	}

	// compact string (just a quick hack to make functions less verbose)
	std::string full=strbuf.str();
	for (size_t p=full.find("  ", 0); p!=std::string::npos; p=full.find("  ", p)) full.replace(p, 2, " ");
	for (size_t p=full.find("... ...", 0); p!=std::string::npos; p=full.find("... ...", p)) full.replace(p, 7, "...");
	for (size_t p=full.find("( ", 0); p!=std::string::npos; p=full.find("( ", p)) full.replace(p, 2, "(");
	for (size_t p=full.find(" )", 0); p!=std::string::npos; p=full.find(" )", p)) full.replace(p, 2, ")");
	for (size_t p=full.find(" ,", 0); p!=std::string::npos; p=full.find(" ,", p)) full.replace(p, 2, ",");
	for (size_t p=full.find("...", 0); p!=std::string::npos; p=full.find("...", p)) full.replace(p, 3, "…");

	return full;
}

/// Print the chain of types that the given node is contained in. Count says how many nodes are considered at most.
std::string DataDependence::printTypeChain(const NodeAddress &node, std::string descr, int count) {
	std::stringstream strbuf;
	if (!count) return strbuf.str();
	if (!descr.empty()) strbuf << "(" << descr << "):";
	NodeAddress addr=node;
	while (count--) {
		strbuf << " " << addr.getNodeType();
		//if (lang::isDerived(addr)) strbuf << "(derived)";
		if (!addr.isRoot()) addr=addr.getParentAddress();
		else count=0;
	}
	strbuf << std::endl;
	return strbuf.str();
}

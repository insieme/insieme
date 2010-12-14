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

#pragma once

#include "insieme/core/statements.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>

namespace insieme {
namespace analysis {
class CFG;
} // end analysis namespace
} // end insieme namespace

namespace std {
std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg);
} // end std namespace

namespace insieme {
namespace analysis {
namespace cfg {

struct CFGElement : public core::StatementPtr {
	CFGElement() : core::StatementPtr() { }
	CFGElement(const core::StatementPtr& stmt) : core::StatementPtr(stmt) { }
};

/**
 * Terminator: The terminator represents the type of control-flow that occurs
 * at the end of the basic block.  The terminator is a StatementPtr referring
 * to an AST node that has control-flow: if-statements, breaks, loops, etc.
 * If the control-flow is conditional, the condition expression will appear
 * within the set of statements in the block (usually the last statement).
 */
struct Terminator : public CFGElement {
	Terminator() : CFGElement() { }
	Terminator(const core::StatementPtr& stmt) : CFGElement(stmt) { }
};

/**
 * CFGBlock: A Control Flow Block represents a sequence of statements with
 * a single entry/single exit control point.
 */
struct CFGBlock {
	typedef std::vector<CFGElement> StatementList;

	CFGBlock() { }
	CFGBlock(const CFGBlock& other) : stmtList(other.stmtList) { }
	~CFGBlock() { }

	// Appends a statement to an existing CFGBlock
	void appendStmt(const core::StatementPtr& stmt) { stmtList.push_back(stmt); }

	// merges two CFGBlocks by appending statements of other into this
	void merge(const CFGBlock& other) {
		std::copy(other.stmt_begin(), other.stmt_end(), std::back_inserter(stmtList));
	}

	// Returns an iterator through the statements contained in this block
	StatementList::const_iterator stmt_begin() const { return stmtList.begin(); }
	StatementList::const_iterator stmt_end() const { return stmtList.end(); }

private:
	StatementList 	stmtList;
	Terminator		term;
};

struct CFGEdge {

};

} // end cfg namespace

class CFG {

	struct NodeProperty {
		cfg::CFGElement node;
	};

	struct EdgeProperty {
		cfg::CFGEdge edge;
	};

	template <class NodeTy>
	class LabelWriter {
	public:
		LabelWriter(NodeTy& node) : prop(node) { }

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			out << "[label=\"" << *prop[v] << "\"]";
		}
	private:
		NodeTy& prop;
	};

	friend std::ostream& std::operator<<(std::ostream& out, const CFG& cfg);
public:
	typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, NodeProperty, EdgeProperty> NodeDepGraph;

	typedef typename boost::graph_traits<NodeDepGraph>::vertex_descriptor 	VertexTy;
	typedef typename boost::graph_traits<NodeDepGraph>::edge_descriptor 	EdgeTy;

	CFG() { }

	VertexTy addCFGNode(const cfg::CFGElement& block) {
		VertexTy&& v = boost::add_vertex(graph);
		typename boost::property_map<CFG::NodeDepGraph, cfg::CFGElement CFG::NodeProperty::*>::type&& node = get(&NodeProperty::node, graph);
		boost::put(node, v, block);
		return v;
	}

	EdgeTy addCFGEdge(VertexTy src, VertexTy dest) { }

	static CFG buildCFG(const core::StatementPtr& rootNode);
private:
	NodeDepGraph	graph;
};

} // end analysis namespace
} // end insieme namespace


namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg) {
	using namespace insieme::analysis;

	typename boost::property_map<CFG::NodeDepGraph, cfg::CFGElement CFG::NodeProperty::*>::const_type&& node = get(&CFG::NodeProperty::node, cfg.graph);
	boost::write_graphviz(out, cfg.graph, CFG::LabelWriter<typename boost::property_map<CFG::NodeDepGraph, cfg::CFGElement CFG::NodeProperty::*>::const_type>(node));
	return out;
}

} // end std namespace

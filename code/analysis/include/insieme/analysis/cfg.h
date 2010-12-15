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
namespace cfg {
class Block;
class Terminator;
}
} // end analysis namespace
} // end insieme namespace

namespace std {
std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg);
std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Block& block);
std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Terminator& term);
} // end std namespace

namespace insieme {
namespace analysis {
namespace cfg {

/**
 * CFGElement - Represents a top-level expression in a basic block.
 */
struct Element : core::StatementPtr {
	Element(const core::StatementPtr& stmt = core::StatementPtr()) : core::StatementPtr(stmt) { }
};

/**
 * Terminator: The terminator represents the type of control-flow that occurs
 * at the end of the basic block.  The terminator is a StatementPtr referring
 * to an AST node that has control-flow: if-statements, breaks, loops, etc.
 * If the control-flow is conditional, the condition expression will appear
 * within the set of statements in the block (usually the last statement).
 */
struct Terminator : public Element {
	Terminator(const core::StatementPtr& stmt = core::StatementPtr()) : Element(stmt) { }
};

/**
 * Block - Represents a single basic block in a source-level CFG.
 *  It consists of:
 *
 *  (1) A set of statements/expressions (which may contain subexpressions).
 *  (2) A "terminator" statement (not in the set of statements).
 */
struct Block {
	typedef std::vector<Element> StatementList;
	Block() { }

	// Appends a statement to an existing CFGBlock
	void appendStmt(const core::StatementPtr& stmt) { stmtList.push_back(stmt); }

	void setTerminal(const core::StatementPtr& stmt) { term = stmt; }
	const Terminator& getTerminator() const { return term; }

	size_t getSize() const { return stmtList.size(); }

	/// Returns an iterator through the statements contained in this block
	StatementList::const_reverse_iterator stmt_begin() const { return stmtList.rbegin(); }
	StatementList::const_reverse_iterator stmt_end() const { return stmtList.rend(); }

private:
	StatementList 	stmtList;
	Terminator		term;
};

struct Edge {

};

} // end cfg namespace

class CFG {

	struct NodeProperty {
		cfg::Block block;

		NodeProperty() { }
		NodeProperty(const cfg::Block& block) : block(block) { }
	};

	struct EdgeProperty {
		cfg::Edge edge;
	};

	template <class NodeTy>
	class LabelWriter {
	public:
		LabelWriter(NodeTy& node) : prop(node) { }

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			const cfg::Block& node = prop[v];
			out << node;
		}
	private:
		NodeTy& prop;
	};

	friend std::ostream& std::operator<<(std::ostream& out, const CFG& cfg);
public:
	// The CFG will be internally represented by an adjacency list (we cannot use an adjacency matrix because the number
	// of nodes in the graph is not known before hand), the CFG is a directed graph node and edge property classes
	// are used to represent control flow blocks and edges properties
	typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::bidirectionalS, NodeProperty, EdgeProperty> 		ControlFlowGraph;

	typedef typename boost::property_map<CFG::ControlFlowGraph, cfg::Block CFG::NodeProperty::*>::type 			NodePropertyMapTy;
	typedef typename boost::property_map<CFG::ControlFlowGraph, cfg::Block CFG::NodeProperty::*>::const_type 	ConstNodePropertyMapTy;

	typedef typename boost::property_map<CFG::ControlFlowGraph, cfg::Edge CFG::EdgeProperty::*>::type 			EdgePropertyMapTy;
	typedef typename boost::property_map<CFG::ControlFlowGraph, cfg::Edge CFG::EdgeProperty::*>::const_type 	ConstEdgePropertyMapTy;

	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_descriptor 	VertexTy;
	typedef typename boost::graph_traits<ControlFlowGraph>::edge_descriptor 	EdgeTy;

	typedef typename boost::graph_traits<ControlFlowGraph>::out_edge_iterator 	OutEdgeIterator;
	typedef typename boost::graph_traits<ControlFlowGraph>::in_edge_iterator 	InEdgeIterator;

	CFG() { }

	/**
	 * Adds a CFG element (or node) to the Control flow graph.
	 *
	 * @param block
	 * @return
	 */
	VertexTy addNode(const cfg::Block& block) {
		return boost::add_vertex(NodeProperty(block), graph);
	}

	VertexTy addNode() {
		return boost::add_vertex(graph);
	}

	/**
	 * Returns a cfg element of the graph given its vertex id.
	 *
	 * @param vertexId
	 * @return
	 */
	cfg::Block& getNode(const VertexTy& vertexId) {
		NodePropertyMapTy&& node = get(&NodeProperty::block, graph);
		return node[vertexId];
	}

	const cfg::Element& getNode(const VertexTy& vertexId) const { return getNode(vertexId); }


	EdgeTy addEdge(VertexTy src, VertexTy dest, const cfg::Edge& edge) {

	}

	EdgeTy addEdge(VertexTy src, VertexTy dest) {
		return boost::add_edge(src, dest, graph).first;
	}

	ControlFlowGraph& getGraph() { return graph; }

	/**
	 * Builds a control flow graph starting from the rootNode
	 *
	 * @param rootNode
	 * @return
	 */
	static CFG buildCFG(const core::ProgramPtr& rootNode);
private:
	ControlFlowGraph	graph;
};

} // end analysis namespace
} // end insieme namespace


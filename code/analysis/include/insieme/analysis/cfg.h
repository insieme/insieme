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
} // end cfg namespace
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
 * Element - Represents a top-level expression in a basic block.
 * A type is included to distinguish expression from terminal nodes.
 */
struct Element : public core::StatementPtr {
	enum Type { None, CtrlCond, LoopInit, LoopIncrement };

	Element(const core::StatementPtr& stmt = core::StatementPtr(), const Type& type = None) : core::StatementPtr (stmt), type(type) { }
	const Type& getType() const { return type; }
private:
	Type type;
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
 * Edge: An edge interconnects two CFG Blocks. An edge can contain an expression which
 * determines the condition under which the path is taken.
 */
struct Edge {
	std::string label; // fixme: replace the string label with an expression
	Edge(const std::string& label = std::string()) : label(label) { }
};

} // end cfg namespace

class CFG;
typedef std::shared_ptr<CFG> CFGPtr;

/**
 * CFG: represents the graph built from IR. Boost.Graph is used as internal representation.
 */
class CFG {

	// Each node of the boost::graph is mapped to a CFGBlock.
	struct NodeProperty {
		const cfg::Block* block;
		NodeProperty(const cfg::Block* block=NULL) : block(block) { }
	};

	struct EdgeProperty {
		cfg::Edge edge;
		EdgeProperty() { }
		EdgeProperty(const cfg::Edge& edge) : edge(edge) { }
	};

	/**
	 * Utility class for the production of a DOT graph from the boost::graph.
	 *
	 * This class takes care of printing the content of CFG Blocks to the output stream.
	 */
	template <class NodeTy>
	class BlockLabelWriter {
	public:
		BlockLabelWriter(NodeTy& node) : prop(node) { }

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			const cfg::Block* node = prop[v];
			assert(node);
			out << *node;
		}
	private:
		NodeTy& prop;
	};

	/**
	 * Utility class for the production of a DOT graph from the boost::graph.
	 *
	 * This class takes care of printing the content of CFG Edges to the output stream.
	 */
	template <class EdgeTy>
	class EdgeLabelWriter {
	public:
		EdgeLabelWriter(EdgeTy& edge) : edge(edge) { }

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			const cfg::Edge& e = edge[v];
			out << "[label=\"" << e.label << "\"]";
		}
	private:
		EdgeTy& edge;
	};

	friend std::ostream& std::operator<<(std::ostream& out, const CFG& cfg);
public:
	// The CFG will be internally represented by an adjacency list (we cannot use an adjacency matrix because the number
	// of nodes in the graph is not known before hand), the CFG is a directed graph node and edge property classes
	// are used to represent control flow blocks and edges properties
	typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::bidirectionalS, NodeProperty, EdgeProperty> 		ControlFlowGraph;

	typedef typename boost::property_map<CFG::ControlFlowGraph, const cfg::Block* CFG::NodeProperty::*>::type 		NodePropertyMapTy;
	typedef typename boost::property_map<CFG::ControlFlowGraph, const cfg::Block* CFG::NodeProperty::*>::const_type ConstNodePropertyMapTy;

	typedef typename boost::property_map<CFG::ControlFlowGraph, cfg::Edge CFG::EdgeProperty::*>::type 				EdgePropertyMapTy;
	typedef typename boost::property_map<CFG::ControlFlowGraph, cfg::Edge CFG::EdgeProperty::*>::const_type 		ConstEdgePropertyMapTy;

	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_descriptor 		VertexTy;
	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_iterator     	VertexIterator;

	typedef typename boost::graph_traits<ControlFlowGraph>::edge_descriptor 		EdgeTy;
	typedef typename boost::graph_traits<ControlFlowGraph>::out_edge_iterator 		OutEdgeIterator;
	typedef typename boost::graph_traits<ControlFlowGraph>::in_edge_iterator 		InEdgeIterator;

	typedef typename boost::graph_traits<ControlFlowGraph>::adjacency_iterator  								AdjacencyIterator;
	typedef typename boost::inv_adjacency_iterator_generator<ControlFlowGraph, VertexTy, InEdgeIterator>::type 	InvAdjacencyIterator;


	CFG() { }
	~CFG();

	/**
	 * Adds a CFG block (or node) to the Control flow graph.
	 *
	 * @param cfg block containing a list of cfg elements.
	 * @return the internal vertex id which identifies the block within the boost::graph
	 */
	VertexTy addBlock(cfg::Block* block);

	/**
	 * Returns a CFG element of the graph given its vertex id.
	 *
	 * @param vertexId The id of the CFG Block
	 * @return A reference to the CFG Block associated to this vertex
	 */
	const cfg::Block& getBlock(const VertexTy& vertexId) const {
		ConstNodePropertyMapTy&& node = get(&NodeProperty::block, graph);
		return *node[vertexId];
	}

	EdgeTy addEdge(VertexTy src, VertexTy dest, const cfg::Edge& edge) {
		return boost::add_edge(src, dest, CFG::EdgeProperty(edge), graph).first;
	}

	/**
	 * Insert an edge connecting the source to the destination.
	 *
	 * @param src CFG Block representing the source of the edge
	 * @param dest CFG Block representing the sink of the edge
	 * @return The identifier of the newly created edge
	 */
	EdgeTy addEdge(VertexTy src, VertexTy dest) {
		return boost::add_edge(src, dest, graph).first;
	}

	/**
	 * Returns the internal representation of this CFG.
	 */
	ControlFlowGraph& getGraph() { return graph; }

	/**
	 * Returns the number of CFG Blocks in this graph.
	 */
	size_t getSize() { return num_vertices(graph); }

	/**
	 * Returns the entry block of the CFG.
	 */
	VertexTy getEntry() const { return *(boost::vertices(graph).first); }

	std::pair<AdjacencyIterator, AdjacencyIterator> getSuccessors(const VertexTy& v) const {
		return adjacent_vertices(v, graph);
	}

	std::pair<InvAdjacencyIterator, InvAdjacencyIterator> getPrevious(const VertexTy& v) const {
		return inv_adjacent_vertices(v, graph);
	}

	/**
	 * Builds a control flow graph starting from the rootNode
	 *
	 * @param rootNode
	 */
	static CFGPtr buildCFG(const core::NodePtr& rootNode);

private:
	ControlFlowGraph	graph;
};

namespace cfg {
/**
 * Block - Represents a single basic block in a source-level CFG.
 *  It consists of:
 *
 *  (1) A set of statements/expressions (which may contain subexpressions).
 *  (2) A "terminator" statement (not in the set of statements).
 */
struct Block {
	typedef std::vector<Element> StatementList;

	typedef StatementList::const_reverse_iterator const_iterator;
	typedef StatementList::const_iterator const_reverse_iterator;
	Block() { }
	Block(const CFG::VertexTy& id) : id(id) { }

	/// Appends a statement to an existing CFGBlock
	void appendElement(const cfg::Element& elem) { stmtList.push_back(elem); }

	/// Set a terminal element for this block
	void setTerminal(const core::StatementPtr& stmt) { term = stmt; }
	const Terminator& getTerminator() const { return term; }
	bool hasTerminator() const { return !!term; }

	/// Returns the number of elements inside this block
	size_t size() const { return stmtList.size(); }
	/// Returns true of the block is empty
	bool empty() const { return stmtList.empty() && !term; }

	void setBlockID(const CFG::VertexTy& vid) { id = vid; }
	const CFG::VertexTy& getBlockID() const { return id; }

	/// Returns an iterator through the statements contained in this block
	const_iterator stmt_begin() const { return stmtList.rbegin(); }
	const_iterator stmt_end() const { return stmtList.rend(); }

	/// Returns an iterator through the statements contained in this block
	const_reverse_iterator stmt_rbegin() const { return stmtList.begin(); }
	const_reverse_iterator stmt_rend() const { return stmtList.end(); }

private:
	CFG::VertexTy	id;
	StatementList 	stmtList;
	Terminator		term;
};
} // end cfg namespace
} // end analysis namespace
} // end insieme namespace


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
#include <boost/property_map/property_map.hpp>

#include <iterator>
#include "insieme/utils/map_utils.h"

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
 * Element - Represents a top-level expression in a basic block. A type is included to distinguish expression from
 * terminal nodes.
 */
struct Element : public core::StatementPtr {
	enum Type { None, CtrlCond, LoopInit, LoopIncrement };

	Element(const core::StatementPtr& stmt = core::StatementPtr(), const Type& type = None) :
		core::StatementPtr(stmt), type(type) { }

	void operator=(const Element& other) { core::StatementPtr::operator=(other); type = other.type; }

	const Type& getType() const { return type; }
private:
	Type type;
};

/**
 * Terminator - The terminator represents the type of control-flow that occurs at the end of the basic block.  The
 * terminator is a StatementPtr referring to an AST node that has control-flow: if-statements, breaks, loops, etc.
 * If the control-flow is conditional, the condition expression will appear within the set of statements in the block
 * (usually the last statement).
 */
struct Terminator : public Element {
	Terminator(const core::StatementPtr& stmt = core::StatementPtr()) : Element(stmt) { }
};

/**
 * Edge: An edge interconnects two CFG Blocks. An edge can contain an expression which determines the condition under
 * which the path is taken.
 */
struct Edge {
	std::string label; // fixme: replace the string label with an expression
	Edge(const std::string& label = std::string()) : label(label) { }
	void operator=(const Edge& other) { label = other.label; }
};

} // end cfg namespace

class CFG;
typedef std::shared_ptr<CFG> CFGPtr;

enum CreationPolicy { OneStmtPerBasicBlock, MultiStmtPerBasicBlock };

/**
 * CFG: represents the graph built from IR. Boost.Graph is used as internal representation.
 */
class CFG {
public:
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
private:
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
			const cfg::Block* node = get(prop, v);
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

	typedef boost::property<boost::vertex_index_t, size_t, NodeProperty> vertex_prop;
	typedef boost::property<boost::edge_index_t, size_t, EdgeProperty> edge_prop;

public:

	// The CFG will be internally represented by an adjacency list (we cannot use an adjacency matrix because the number
	// of nodes in the graph is not known before hand), the CFG is a directed graph node and edge property classes
	// are used to represent control flow blocks and edges properties
	typedef boost::adjacency_list<boost::listS,
								  boost::listS,
								  boost::bidirectionalS,
								  vertex_prop,
								  edge_prop>  ControlFlowGraph;

	typedef typename boost::property_map< CFG::ControlFlowGraph, const cfg::Block* CFG::NodeProperty::* >::type
			NodePropertyMapTy;

	typedef typename boost::property_map< CFG::ControlFlowGraph, const cfg::Block* CFG::NodeProperty::* >::const_type
			ConstNodePropertyMapTy;

	typedef typename boost::property_map< CFG::ControlFlowGraph, cfg::Edge CFG::EdgeProperty::* >::type
			EdgePropertyMapTy;

	typedef typename boost::property_map< CFG::ControlFlowGraph, cfg::Edge CFG::EdgeProperty::* >::const_type
			ConstEdgePropertyMapTy;

	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_descriptor 	VertexTy;
	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_iterator   	VertexIterator;

	typedef typename boost::graph_traits<ControlFlowGraph>::edge_descriptor 	EdgeTy;
	typedef typename boost::graph_traits<ControlFlowGraph>::out_edge_iterator 	OutEdgeIterator;
	typedef typename boost::graph_traits<ControlFlowGraph>::in_edge_iterator 	InEdgeIterator;

	typedef typename boost::graph_traits<ControlFlowGraph>::adjacency_iterator 	AdjacencyIterator;

	typedef typename boost::inv_adjacency_iterator_generator<ControlFlowGraph,
															 VertexTy, InEdgeIterator>::type 	InvAdjacencyIterator;

	typedef std::pair<CFG::VertexTy, CFG::VertexTy> GraphBounds;
	typedef insieme::utils::map::PointerMap<core::NodePtr, GraphBounds> SubGraphMap;

	/**
	 * This iterator transforms iterators returned by boost::Graph iterating through vertex indexes into an iterator
	 * iterating through cfg::Blocks. Makes traversing of the CFG easier.
	 */
	template <class IterT>
	class CFGBlockIterator: public std::iterator<std::forward_iterator_tag, const cfg::Block> {
		// reference to the CFG the iterator belongs to
		const CFG* cfg;
		IterT iter, end;
	public:
		CFGBlockIterator(const CFG* cfg, const IterT& start, const IterT& end) : cfg(cfg), iter(start), end(end) { }
		CFGBlockIterator(const IterT& end) : cfg(NULL), iter(end), end(end) { }

		// increment this iterator only if we are not at the end
		void operator++() {
			assert(iter != end && "Incrementing an invalid iterator");
			++iter;
		}
		// checks whether 2 iterators are equal
		bool operator==(const CFGBlockIterator<IterT>& other) const { return iter == other.iter; }
		bool operator!=(const CFGBlockIterator<IterT>& other) const { return !(*this == other); }
		// Returns a reference to the block referenced by this iterator
		const cfg::Block& operator*() const {
			assert(iter != end && cfg && "Iterator out of scope!");
			return cfg->getBlock(*iter);
		}
	};

	typedef CFGBlockIterator<AdjacencyIterator> SuccessorsIterator;
	typedef CFGBlockIterator<InvAdjacencyIterator> PredecessorsIterator;

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
		ConstNodePropertyMapTy&& nodeMap = get(&NodeProperty::block, graph);
		return *nodeMap[vertexId];
	}

	EdgeTy addEdge(const VertexTy& src, const VertexTy& dest, const cfg::Edge& edge) {
		EdgeTy&& edgeId = boost::add_edge(src, dest, graph).first;
		EdgePropertyMapTy&& edgeMap = get(&EdgeProperty::edge, graph);
		put(edgeMap, edgeId, edge);
		return edgeId;
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
	ControlFlowGraph& getRawGraph() { return graph; }

	/**
	 * Returns the number of CFG Blocks in this graph.
	 */
	size_t getSize() const  { return num_vertices(graph); }

	/**
	 * Returns the entry block of the CFG.
	 */
	VertexTy getEntry() const { return entry; }

	void setEntry(const CFG::VertexTy& v) { entry = v; }

	/**
	 * Returns the exit block of the CFG.
	 */
	VertexTy getExit() const { return exit; }

	void setExit(const CFG::VertexTy& v) { exit = v; }

	SuccessorsIterator successors_begin(const VertexTy& v) const {
		std::pair<AdjacencyIterator, AdjacencyIterator>&& adjIt = adjacent_vertices(v, graph);
		return SuccessorsIterator( this, adjIt.first, adjIt.second );
	}

	SuccessorsIterator successors_end(const VertexTy& v) const {
		return SuccessorsIterator( adjacent_vertices(v, graph).second );
	}

	PredecessorsIterator predecessors_begin(const VertexTy& v) const {
		std::pair<InvAdjacencyIterator, InvAdjacencyIterator>&& adjIt = inv_adjacent_vertices(v, graph);
		return PredecessorsIterator( this, adjIt.first, adjIt.second );
	}

	PredecessorsIterator predecessors_end(const VertexTy& v) const {
		return PredecessorsIterator( inv_adjacent_vertices(v, graph).second );
	}

	/**
	 * Builds a control flow graph starting from the rootNode
	 *
	 * @param rootNode
	 */
	template <CreationPolicy CP = OneStmtPerBasicBlock>
	static CFGPtr buildCFG(const core::NodePtr& rootNode, CFGPtr cfg = std::make_shared<CFG>());

	GraphBounds addSubGraph(const core::NodePtr& root);

	/**
	 * Check whether a graph for the root node has been already created
	 * @param root
	 * @return
	 */
	bool hasSubGraph(const core::NodePtr& root) const {
		return subGraphs.find(root) != subGraphs.end();
	}

	GraphBounds getNodeBounds(const core::NodePtr& root) {
		auto fit = subGraphs.find(root);
		assert( fit != subGraphs.end() );
		return fit->second;
	}

	void printStats(std::ostream& out);

private:
	ControlFlowGraph	graph;
	SubGraphMap			subGraphs;
	size_t				currId;
	VertexTy			entry, exit;
};

namespace cfg {
/**
 * Block - Represents a single basic block in a source-level CFG. It consists of:
 *
 *  (1) A set of statements/expressions (which may contain subexpressions).
 *  (2) A "terminator" statement (not in the set of statements).
 */
struct Block {
	typedef std::vector<Element> StatementList;

	typedef StatementList::const_reverse_iterator const_iterator;
	typedef StatementList::const_iterator const_reverse_iterator;

	enum Type { DEFAULT, ENTRY, EXIT, CALL, RET };

	Block(const Type& blockType = DEFAULT) : blockType(blockType) { }
	Block(const CFG::VertexTy& id, const Type& blockType) : blockType(blockType), id(id) { }

	/// Appends a statement to an existing CFGBlock
	void appendElement(const cfg::Element& elem) { stmtList.push_back(elem); }

	/// Setters and getters for the terminator element
	const Terminator& terminator() const { return term; }
	Terminator& terminator() { return term; }

	bool hasTerminator() const { return !!term; }

	/// Returns the number of elements inside this block
	size_t size() const { return stmtList.size(); }
	/// Returns true of the block is empty
	bool empty() const { return stmtList.empty() && !term; }

	// return the block type
	const Type& type() const { return blockType; }

	// Setters/Getters for the block ID
	const CFG::VertexTy& blockId() const { return id; }
	CFG::VertexTy& blockId() { return id; }

	bool operator==(const Block& other) const {	return id == other.id; }

	Element& operator[](size_t idx) {
		assert(idx < size() && "Out of bound array access");
		return stmtList[size()-1-idx];
	}

	const Element& operator[](size_t idx) const {
		assert(idx < size() && "Out of bound array access");
		return stmtList[size()-1-idx];
	}

	/// Returns an iterator through the statements contained in this block
	const_iterator stmt_begin() const { return stmtList.rbegin(); }
	const_iterator stmt_end() const { return stmtList.rend(); }

	/// Returns an iterator through the statements contained in this block
	const_reverse_iterator stmt_rbegin() const { return stmtList.begin(); }
	const_reverse_iterator stmt_rend() const { return stmtList.end(); }

	virtual ~Block() { }
private:
	const Type		blockType;
	CFG::VertexTy	id;
	StatementList 	stmtList;
	Terminator		term;
};

struct RetBlock;

struct CallBlock: public Block {
	CallBlock(): Block(CALL), ret(NULL) { }

	const RetBlock* returnBlock() const { return ret; }
	RetBlock*& returnBlock() { return ret; }
private:
	RetBlock* ret;
};

struct RetBlock: public Block {
	RetBlock(): Block(RET), call(NULL) { }

	const CallBlock* callBlock() const { return call; }
	CallBlock*& callBlock() { return call; }

private:
	CallBlock* call;
};

} // end cfg namespace
} // end analysis namespace
} // end insieme namespace


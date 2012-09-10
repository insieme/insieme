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

#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_address.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/operators.hpp>
#include <boost/graph/breadth_first_search.hpp>
#include <boost/graph/depth_first_search.hpp>

#include <iterator>
#include <tuple>

#include "insieme/utils/map_utils.h"

#include "insieme/analysis/tmp_var_map.h"

namespace insieme {
namespace analysis {

class CFG;
typedef std::shared_ptr<CFG> CFGPtr;

namespace cfg {

/*/////////////////////////////////////////////////////////////////////////////////////////////////
 * Element - Represents a top-level expression in a basic block. A type is included to distinguish
 * expression from terminal nodes.
 *
 * Because the generation of the CFG introduces temporaries, the node store 2 pointers to IR. 
 *
 * viewPtr: This is a pointer to the analysis view of the IR where tempoaries are used as a
 * placeholder for DF values coming from the call of subexpressions
 *
 * baseAddr: Is the address of the original statement to which this CFG Element is point to. This
 * information is important in order to retrieve addresses of IR nodes. For example to give some
 * meaningfull information as result of the DF analysis 
 */////////////////////////////////////////////////////////////////////////////////////////////////
struct Element : public utils::Printable {

	enum Type { 
		NONE, 
		CTRL_COND, 
		LOOP_INIT, 
		LOOP_INCREMENT 
	};

	Element(const core::StatementAddress& baseAddr, const Type& type = NONE) : 
		viewPtr( baseAddr.getAddressedNode() ), 
		baseAddr( baseAddr ), 
		diffAddr( core::StatementAddress(baseAddr.getAddressedNode()) ),
		type( type ) { }

	Element(const core::StatementPtr& viewPtr, 
			const core::StatementAddress& baseAddr, 
			const Type& type = NONE) 
	: viewPtr(viewPtr), 
	  baseAddr(baseAddr), 
	  diffAddr( viewPtr ? core::StatementAddress(viewPtr) : core::StatementAddress() ),
	  type(type) { }

	Element(const core::StatementPtr& viewPtr, 
			const core::StatementAddress& baseAddr, 
			const core::StatementAddress& diffAddr,
			const Type& type = NONE) 
	: viewPtr(viewPtr), 
	  baseAddr(baseAddr), 
	  diffAddr(diffAddr),
	  type(type) 
	{
		assert( (!diffAddr || (diffAddr && diffAddr.getRootNode() == viewPtr)) && 
				"Diff must always be relative to analysis stmt"
			  ); 
	}

	inline const Type& getType() const { return type; }
	
	inline const core::StatementPtr& getAnalysisStatement() const { return viewPtr; }

	inline const core::StatementAddress& getStatementAddress() const { return baseAddr; }

	inline const core::StatementAddress& getDifference() const { return diffAddr; }

	inline std::ostream& printTo(std::ostream& out) const { 
		return out << *viewPtr; 
	}

private:

	/** 
	 * Stores the analysis statement utilized for analysis 
	 */
	core::StatementPtr 		viewPtr;

	/** 
	 * Stores the address of the original IR statement from which the analyisis statement has been
	 * generated
	 */
	core::StatementAddress 	baseAddr;

	/** 
	 * This is an addresses which allows to easily find the position of the original statement
	 * within the analysis stmt generated for analsyis purposes 
	 */
	core::StatementAddress 	diffAddr;

	Type type;
};

inline bool operator==(const Element& lhs, const core::StatementPtr& rhs) {
	return *lhs.getStatementAddress().getAddressedNode() == *rhs;
}

inline bool operator==(const core::StatementPtr& lhs, const Element& rhs) {
	return *rhs.getStatementAddress().getAddressedNode() == *lhs;
}


/*/////////////////////////////////////////////////////////////////////////////////////////////////
 * Terminator - The terminator represents the type of control-flow that occurs at the end of the
 * basic block.  The terminator is a StatementPtr referring to an AST node that has control-flow:
 * if-statements, breaks, loops, etc.  If the control-flow is conditional, the condition expression
 * will appear within the set of statements in the block (usually the last statement).
 */////////////////////////////////////////////////////////////////////////////////////////////////
struct Terminator : public Element {

	//Terminator() : 
	//	Element(core::StatementPtr(), core::StatementAddress()) { }

	Terminator(const core::StatementAddress& stmt) : Element(stmt) { }

	std::ostream& printTo(std::ostream& out) const;
};


/*/////////////////////////////////////////////////////////////////////////////////////////////////
 * Edge: An edge interconnects two CFG Blocks. An edge can contain an expression which determines
 * the condition under which the path is taken.
 */////////////////////////////////////////////////////////////////////////////////////////////////
struct Edge {
	
	Edge(const core::ExpressionPtr& expr = core::ExpressionPtr()) : 
		expr(expr) { }

	inline Edge& operator=(const Edge& other) { 
		expr = other.expr; 
		return *this; 
	}

	inline core::ExpressionPtr getEdgeExpr() const { return expr; }
private:
	core::ExpressionPtr expr;	
};


class Block;
typedef std::shared_ptr<Block> BlockPtr;


/*////////////////////////////////////////////////////////////////////////////////////////////
 * A CFGAddress stores the unique address of an IR entity based on the CFG representation. 
 *
 * An entity is represented by a triplette, represented by che CFG block, the statement index
 * (relative to the block) and the address relative to the statement. 
 *////////////////////////////////////////////////////////////////////////////////////////////
struct Address : public utils::Printable {
	
	cfg::BlockPtr		block;
	unsigned			stmt_idx;
	core::NodeAddress	addr;

	/**
	 * Create a CFG address starting from a block pointer, an index addressing the element inside
	 * the block and an address relative to the addressed analysis statement 
	 */
	Address(const cfg::BlockPtr& block, unsigned stmt_idx, const core::NodeAddress& addr) :
		block(block), stmt_idx(stmt_idx), addr(addr) { }
	
	/**
	 * Copy constructores with and without move semantics 
	 */
	Address(const cfg::Address& other) :
		block(other.block), 
		stmt_idx(other.stmt_idx),
		addr(other.addr) { }

	Address(cfg::Address&& other) : 
		block(std::move(other.block)), 
		stmt_idx(other.stmt_idx), 
		addr(std::move(other.addr)) { }


	inline operator bool() const { return static_cast<bool>(block); }


	/** 
	 * Retrieve the CFG block to which this address referes to 
	 */
	inline const cfg::Block& getBlock() const {
		assert(block && "Trying to access an invalid cfg address");
		return *block; 
	}

	inline const cfg::BlockPtr& getBlockPtr() const { 
		return block; 
	}

	inline unsigned getStmtIdx() const { return stmt_idx; }

	/**
	 * Retrieve the node addressed by this address (the name guarantee consistency with ir addresses 
	 */
	inline core::NodePtr getAddressedNode() const { return addr.getAddressedNode(); }

	/**
	 * Return the absolute address if the addressed entity exists outside the CFG 
	 */
	core::NodeAddress toAbsoluteAddress(const CFG& cfg) const;

	std::ostream& printTo(std::ostream& out) const;
};



} // end cfg namespace

enum CreationPolicy { 
	OneStmtPerBasicBlock, 
	MultiStmtPerBasicBlock 
};



/**
 * CFG: represents the graph built from IR. Boost.Graph is used as internal representation.
 */
class CFG : public utils::Printable {

	typedef boost::property<
		boost::vertex_index_t, 
		size_t, 
		cfg::BlockPtr
	> vertex_prop;

	typedef boost::property<
		boost::edge_index_t,   
		size_t, 
		cfg::Edge
	> edge_prop;

public:

	// The CFG will be internally represented by an adjacency list (we cannot use an adjacency
	// matrix because the number of nodes in the graph is not known before hand), the CFG is a
	// directed graph node and edge property classes are used to represent control flow blocks and
	// edges properties
	typedef boost::adjacency_list<
		boost::listS,
		boost::listS,
	 	boost::bidirectionalS,
		vertex_prop,
		edge_prop
	>  ControlFlowGraph;

	typedef typename 
		boost::property_map<
			CFG::ControlFlowGraph, boost::vertex_index_t>::type 				BlockIDPropertyMapTy;

	typedef typename 
		boost::property_map<
			CFG::ControlFlowGraph, boost::vertex_index_t>::const_type 			ConstBlockIDPropertyMapTy;

	// Vertex related types 
	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_descriptor 	VertexTy;

	typedef typename boost::graph_traits<ControlFlowGraph>::vertex_iterator   	VertexIterator;

	// Edge related types
	typedef typename boost::graph_traits<ControlFlowGraph>::edge_descriptor 	EdgeTy;

	// Iterator through the Outgoing edges of a vertex 
	typedef typename boost::graph_traits<ControlFlowGraph>::out_edge_iterator 	OutEdgeIterator;

	// Iterator through the incoming edges of a vertex
	typedef typename boost::graph_traits<ControlFlowGraph>::in_edge_iterator 	InEdgeIterator;

	// Iterator through precedent vertices of a vertex: all i: (i -> v)
	typedef typename boost::graph_traits<ControlFlowGraph>::adjacency_iterator 	AdjacencyIterator;

	// Iterator through successive vertices of a vertex: all i: (v -> i)
	typedef typename 
		boost::inv_adjacency_iterator_generator<
			ControlFlowGraph,
			VertexTy, 
			InEdgeIterator
		>::type 																InvAdjacencyIterator;

	// Keeps the reference to the entry and exit node for subgraphs
	typedef std::tuple<core::VariablePtr, CFG::VertexTy, CFG::VertexTy> 		GraphBounds;

	// Maps IR root nodes (i.e. LambdaExpr and Program) to the respective bounds
	typedef insieme::utils::map::PointerMap<core::NodePtr, GraphBounds> 		SubGraphMap;

	/**
	 * This iterator transforms iterators returned by boost::Graph iterating through vertex indexes into an iterator
	 * iterating through cfg::Blocks. Makes traversing of the CFG easier.
	 */
	template <class IterT>
	class BlockIterator : 
		public std::iterator<std::forward_iterator_tag, const cfg::BlockPtr>,
		public boost::equality_comparable<BlockIterator<IterT>>
	{
		// reference to the CFG the iterator belongs to
		const CFG* cfg;
		IterT iter, end;

	public:
		BlockIterator(const CFG* cfg, const IterT& start, const IterT& end) :
		   cfg(cfg), iter(start), end(end) { }

		BlockIterator(const CFG* cfg, const IterT& end) :
		   cfg(cfg), iter(end), end(end) { }

		// increment this iterator only if we are not at the end
		inline BlockIterator<IterT> operator++() {
			assert(iter != end && "Incrementing an invalid iterator"); 
			++iter;
			return *this;
		}

		// increment this iterator only if we are not at the end
		inline BlockIterator<IterT> operator+=(unsigned id) {
			assert(iter != end && "Incrementing an invalid iterator"); 
			for(size_t i=0; i<id; i++) ++iter;
			return *this;
		}

		// checks whether 2 iterators are equal
		inline bool operator==(const BlockIterator<IterT>& other) const { 
			return iter == other.iter; 
		}

		// Returns a reference to the block referenced by this iterator
		inline const cfg::BlockPtr& operator*() const {
			assert(iter != end && "Iterator out of scope");
			return cfg->getBlockPtr(*iter);
		}

		// Returns a reference to the block referenced by this iterator
		inline const cfg::Block* operator->() const {
			assert(iter != end && "Iterator out of scope");
			return &cfg->getBlock(*iter);
		}
	};

	typedef BlockIterator<AdjacencyIterator> 	SuccessorsIterator;
	typedef BlockIterator<InvAdjacencyIterator> PredecessorsIterator;


	/** 
	 * Generic visitor utility class
	 */
	struct BlockVisitor : public boost::base_visitor<BlockVisitor> {
		typedef boost::on_discover_vertex event_filter;
		
		typedef std::function<void (const cfg::BlockPtr&)> FunctorType;

		BlockVisitor(const FunctorType& func) : func(func) { }

		inline void operator()(const CFG::VertexTy& v, const CFG::ControlFlowGraph& g) { 
			func(g[v]); 
		}

	private:
		FunctorType func;
	};


	CFG() { }

	/**
	 * Adds a CFG block (or node) to the Control flow graph.
	 *
	 * @param cfg block containing a list of cfg elements.
	 * @return the internal vertex id which identifies the block within the boost::graph
	 */
	VertexTy addBlock(cfg::Block* block);

	// Removes a CFG Block from the CFG
	void removeBlock(const VertexTy& v);

	/**
	 * Returns a CFG element of the graph given its vertex id.
	 *
	 * @param vertexId The id of the CFG Block
	 * @return A reference to the CFG Block associated to this vertex
	 */
	inline const cfg::Block& getBlock(const VertexTy& vertexId) const { 
		return *graph[vertexId]; 
	}

	inline const cfg::BlockPtr& getBlockPtr(const VertexTy& vertexId) const { 
		return graph[vertexId]; 
	}

	inline const size_t& getBlockID(const VertexTy& vertexId) const {
		ConstBlockIDPropertyMapTy&& idMap = get(boost::vertex_index, graph);
		return get(idMap, vertexId);
	}

	EdgeTy addEdge(const VertexTy& src, const VertexTy& dest, const cfg::Edge& edge = cfg::Edge());

	// Returns the Edge object associated to a graph edge connecting src and dest vertices 
	const cfg::Edge& getEdge(const VertexTy& src, const VertexTy& dest) const; 

	// Returns the internal representation of this CFG.
	inline ControlFlowGraph& getRawGraph() { return graph; }
	inline const ControlFlowGraph& getRawGraph() const { return graph; }

	void replaceNode(const VertexTy& oldNode, const VertexTy& newNode);

	/// Returns the number of CFG Blocks in this graph.
	inline size_t size() const  { return num_vertices(graph); }

	// getter/setter for the entry block of the CFG.
	inline const VertexTy& entry() const { return entry_block; }
	inline VertexTy& entry() { return entry_block; }

	bool isEntry(const cfg::BlockPtr& block) const;

	bool isExit(const cfg::BlockPtr& block) const;

	//  getter/setter for the exit block of the CFG.
	inline const VertexTy& exit() const { return exit_block; }
	inline VertexTy& exit() { return exit_block; }

	// Returns an iterator which iterates through the successor blocks of a given cfg::Block
	inline SuccessorsIterator successors_begin(const VertexTy& v) const {
		auto&& adjIt = adjacent_vertices(v, graph);
		return SuccessorsIterator( this, adjIt.first, adjIt.second );
	}
	inline SuccessorsIterator successors_end(const VertexTy& v) const {
		return SuccessorsIterator( this, adjacent_vertices(v, graph).second );
	}

	// Returns an iterator which iterates through the predecessors blocks of a given cfg::Block
	inline PredecessorsIterator predecessors_begin(const VertexTy& v) const {
		auto&& adjIt = inv_adjacent_vertices(v, graph);
		return PredecessorsIterator( this, adjIt.first, adjIt.second );
	}
	inline PredecessorsIterator predecessors_end(const VertexTy& v) const {
		return PredecessorsIterator( this, inv_adjacent_vertices(v, graph).second );
	}

	/**
	 * Search for a particular node which is contained inside any of the blocks and return 
	 * the containing block and statement ID relative to the block 
	 */
	cfg::Address find(const core::NodeAddress& node) const;

	/**
	 * Builds a control flow graph starting from the rootNode
	 *
	 * @param rootNode
	 */
	template <CreationPolicy CP = OneStmtPerBasicBlock>
	static CFGPtr buildCFG(const core::NodePtr& rootNode, CFGPtr cfg = std::make_shared<CFG>());

	core::NodePtr getRootNode() const;

	GraphBounds addSubGraph(const core::NodePtr& root);

	// Check whether a graph for the root node has been already created
	inline bool hasSubGraph(const core::NodePtr& root) const {
		return subGraphs.find(root) != subGraphs.end();
	}

	inline GraphBounds getNodeBounds(const core::NodePtr& root) {
		auto fit = subGraphs.find(root);
		assert( fit != subGraphs.end() );
		return fit->second;
	}

	std::ostream& printTo(std::ostream& out) const;

	void printStats(std::ostream& out);

	int getStrongComponents();

	// Visitor interface 
	void visitDFS(const std::function<void (const cfg::BlockPtr& block)>& lambda) const {
	
		typedef std::map<VertexTy, boost::default_color_type> color_type;
		color_type color;
		boost::associative_property_map<color_type> color_map(color);

		boost::depth_first_visit( graph, 
				entry_block, 
				boost::make_dfs_visitor( BlockVisitor(lambda) ),
				color_map
			);
	}

	inline const TmpVarMap& getTmpVarMap() const { return tmpVarMap; }
	inline TmpVarMap& getTmpVarMap() { return tmpVarMap; }

private:
	ControlFlowGraph	graph;

	// Map LambdaExpr graphs to their respective entry/exit block 
	SubGraphMap			subGraphs;

	size_t				currId;
	VertexTy			entry_block, exit_block;


	TmpVarMap 			tmpVarMap;
};

namespace cfg {

/**
 * Block - Represents a single basic block in a source-level CFG. It consists of:
 *
 *  (1) A set of statements/expressions (which may contain subexpressions).
 *  (2) A "terminator" statement (not in the set of statements).
 */
struct Block : 
	public utils::Printable,
	public boost::noncopyable,
	public boost::equality_comparable<Block> {
	
	typedef std::vector<Element> StatementList;

	typedef StatementList::const_reverse_iterator const_iterator;
	typedef StatementList::const_iterator const_reverse_iterator;

	typedef CFG::PredecessorsIterator predecessors_iterator;
	typedef CFG::SuccessorsIterator   successors_iterator;

	enum Type { 
		DEFAULT, // Default block 
		ENTRY, 	 // This is an entry block 
		EXIT, 	 // This is an exit block 
		CALL,  	 // This is call block 
		RET 	 // This is return block 
	};

	Block(const CFG& parentCFG, const Type& blockType = DEFAULT) : 
		parentCFG(parentCFG), blockType(blockType) { }

	Block(const CFG& parentCFG, const CFG::VertexTy& id, const Type& blockType) : 
		parentCFG(parentCFG), blockType(blockType), vertex_id(id) { }

	/// Appends a statement to an existing CFGBlock
	inline void appendElement(const cfg::Element& elem) { 
		stmtList.push_back(elem); 
	}

	/// Setters and getters for the terminator element
	inline const Terminator& terminator() const { 
		assert(hasTerminator() && "trying to access invalid terminator");
		return *term; 
	}
	inline void setTerminator(const Terminator& t) {
		term = t;
	}

	inline bool hasTerminator() const { return static_cast<bool>(term); }

	/// Returns the number of elements inside this block
	inline size_t size() const { return stmtList.size(); }
	/// Returns true of the block is empty
	inline bool empty() const { 
		return stmtList.empty() && !hasTerminator(); 
	}

	// return the block type
	inline const Type& type() const { return blockType; }

	// Setters/Getters for the vertex ID
	inline const CFG::VertexTy& getVertexID() const { return vertex_id; }
	inline void setVertexID(const CFG::VertexTy& vid) { vertex_id = vid; }

	// Implements an implicit cast of a block to its vertex id used within the boost graph
	inline operator CFG::VertexTy() const { return vertex_id; }

	// Getter for the block ID
	inline const size_t& getBlockID() const {
		return parentCFG.getBlockID(vertex_id);
	}

	inline bool operator==(const Block& other) const {
		return vertex_id == other.vertex_id && 
			   &parentCFG == &other.parentCFG; 
	}

	inline Element& operator[](size_t idx) {
		assert(idx < size() && "Out of bound array access");
		return stmtList[size()-1-idx];
	}

	inline const Element& operator[](size_t idx) const {
		assert(idx < size() && "Out of bound array access");
		return stmtList[size()-1-idx];
	}

	/// Returns an iterator through the statements contained in this block
	inline const_iterator stmt_begin() const { return stmtList.rbegin(); }
	inline const_iterator stmt_end() const { return stmtList.rend(); }

	/// Returns an iterator through the statements contained in this block
	inline const_reverse_iterator stmt_rbegin() const { return stmtList.begin(); }
	inline const_reverse_iterator stmt_rend() const { return stmtList.end(); }


	// Retrieves an iterator through the predecessors of this block
	inline predecessors_iterator predecessors_begin() const { 
		return parentCFG.predecessors_begin( vertex_id ); 
	}
	inline predecessors_iterator predecessors_end() const { 
		return parentCFG.predecessors_end( vertex_id ); 
	}

	inline size_t predecessors_count() const {
		return boost::in_degree(vertex_id, parentCFG.getRawGraph());
	}

	inline const cfg::BlockPtr& predecessor(unsigned id) const {
		return *(parentCFG.predecessors_begin( vertex_id )+=id);
	}

	// Retrieves an iterator through the successors of this Block 
	inline successors_iterator successors_begin() const { 
		return parentCFG.successors_begin( vertex_id ); 
	}
	inline successors_iterator successors_end() const { 
		return parentCFG.successors_end( vertex_id ); 
	}

	inline size_t successors_count() const {
		return boost::out_degree(vertex_id, parentCFG.getRawGraph());
	}

	inline const cfg::BlockPtr& successor(unsigned id) const {
		return *(parentCFG.successors_begin( vertex_id )+=id);
	}

	std::ostream& printTo(std::ostream& out) const;

	virtual ~Block() { }

private:
	// private getter for CFG parent graph
	inline const CFG& getParentCFG() const { return parentCFG; }

	const CFG&		parentCFG;

	const Type		blockType;
	CFG::VertexTy	vertex_id;

	StatementList 				stmtList;
	boost::optional<Terminator>	term;
};

struct RetBlock;

struct CallBlock: public Block {

	CallBlock(const CFG& cfg): Block(cfg, CALL), ret(NULL) { }

	inline const RetBlock& getReturnBlock() const { 
		assert(ret && "Return block for this CALL block not set."); 
		return *ret; 
	}
	inline void setReturnBlock(RetBlock& ret) { this->ret = &ret; }

	virtual ~CallBlock() { }
private:
	RetBlock* ret;
};

struct RetBlock: public Block {

	RetBlock(const CFG& cfg): Block(cfg, RET), call(NULL) { }

	inline const CallBlock& getCallBlock() const { 
		assert(call && "Call block for this RET block not set.");
		return *call; 
	}
	inline void setCallBlock(CallBlock& call) { this->call = &call; }

	virtual ~RetBlock() { }
private:
	CallBlock* call;
};

} // end cfg namespace
} // end analysis namespace
} // end insieme namespace

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::BlockPtr& blockPtr) {
		return out << "&{B" << blockPtr->getBlockID() << "}";
	}

} // end std namespace 


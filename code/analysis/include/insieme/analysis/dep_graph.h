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

#include "insieme/utils/printable.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/analysis/polyhedral/constraint.h"
#include "insieme/analysis/polyhedral/backend.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/optional.hpp>

namespace insieme { 
	
namespace core { namespace arithmetic {
class Formula;
} } // end core::arithmetic namespace

namespace analysis { 
	
namespace polyhedral {
class Scop;
} // end polyhderal namespace

namespace dep {

// forward decl
class Stmt;
typedef std::shared_ptr<const Stmt> StmtPtr;

class Dependence;
typedef std::shared_ptr<const Dependence> DependencePtr;

/**
 * Defines the possible type of dependencies which could appear in the code
 */
enum DependenceType { RAW=0x1, TRUE=0x1,   // Read-After-Write dependence (or true-dependence)
					  WAR=0x2, ANTI=0x2,   // Write-After-Read dependence (or anti-dependence)
					  WAW=0x4, OUTPUT=0x4, // Write-After-Write dependence (or output-dependence)
					  RAR=0x8, INPUT=0x8,   // Read-After-Read dependence (or input-dependence)
					  ALL=0xF, 			   // All dependencies
					  WRITE= RAW | WAR | WAW // All non-RAR dependencies
					};

/**
 * Provide a toStr for the dependencies types defined above
 */
std::string depTypeToStr(const dep::DependenceType& dep);

/**
 * Define a list of formulas which is kept to store the distance vector of a particular dependence
 */
typedef std::vector<core::arithmetic::Formula> FormulaList;

/**
 * The distance vector is represented by an array of distances (for each iterator in the iteration vector
 * and a constraint which determines the domain for which the dependence exists 
 */
typedef std::pair<FormulaList, utils::CombinerPtr<core::arithmetic::Formula>> DistanceVector;


// Define a pair to hold the result of a strong connected component where the first 
// element of the pair is the root node and the second is the set of statements belonging 
// to such component 
typedef std::pair<StmtPtr, std::set<StmtPtr>> Component;

// List of the components contained in this dependence graph
typedef std::vector<Component> ComponentList;

typedef std::tuple<
	DependenceType, 		// dep type
	core::StatementAddress,	// src
	core::StatementAddress,	// sink
	DistanceVector
> DependenceInstance;

typedef std::vector<DependenceInstance> DependenceList;


/**
 * Data structure utilized to store the dependencies within a SCoP region. The internal
 * representation is based on boost.graph representation. Once the dependence graph of a SCoP is
 * build it is kept as read-only object. In order to modify the dependencies code transfromations
 * should be applied to the original code and the new dependence graph recreated on demand.
 */
struct DependenceGraph : public utils::Printable {

	typedef boost::adjacency_list<
		boost::multisetS, 
		boost::vecS, 
		boost::bidirectionalS, 
		std::shared_ptr<dep::Stmt>, 
		std::shared_ptr<dep::Dependence>
	> Graph;
	
	typedef typename boost::graph_traits<Graph>::vertex_descriptor 	VertexTy;
	typedef typename boost::graph_traits<Graph>::edge_descriptor   	EdgeTy;

	typedef typename boost::graph_traits<Graph>::vertex_iterator 	VertexIterator;
	typedef typename boost::graph_traits<Graph>::edge_iterator		EdgeIterator;	

	// Iterator through the Outgoing edges of a vertex 
	typedef typename boost::graph_traits<Graph>::out_edge_iterator 	OutEdgeIterator;

	// Iterator through the incoming edges of a vertex
	typedef typename boost::graph_traits<Graph>::in_edge_iterator 	InEdgeIterator;

	template <class IterT>
	class DependenceIteratorImpl : 
		public std::iterator<std::forward_iterator_tag, const Dependence>,
		public boost::equality_comparable<DependenceIteratorImpl<IterT>>
	{
		// reference to the CFG the iterator belongs to
		const Graph& graph;
		IterT iter;

	public:
		DependenceIteratorImpl(const Graph& graph, const IterT& it) :
		   graph(graph), iter(it) { }

		// increment this iterator only if we are not at the end
		inline DependenceIteratorImpl<IterT>& operator++() { ++iter; return *this; }

		// checks whether 2 iterators are equal
		inline bool operator==(const DependenceIteratorImpl<IterT>& other) const { 
			return iter == other.iter;
		}

		// Returns a reference to the block referenced by this iterator
		inline const Dependence& operator*() const { return *graph[*iter]; }

		// Returns a reference to the block referenced by this iterator
		inline const Dependence* operator->() const { return &*graph[*iter]; }
	};

	typedef DependenceIteratorImpl<OutEdgeIterator> DependenceIterator;

	DependenceGraph(insieme::core::NodeManager& mgr, 
					const polyhedral::Scop& scop, 
					const unsigned& depType, 
					bool transitive_closure = false); 

	const Graph& getBoostGraph() const { return graph; }

	Graph& getBoostGraph() { return graph; }
	
	EdgeTy addDependence(const VertexTy& src, 
						 const VertexTy& sink, 
						 const DependenceType& type, 
						 const DistanceVector& dist = DistanceVector());
	
	inline const Stmt& getStatement(const VertexTy& v) const { return *graph[v]; }

	const core::StatementAddress& getStatementAddress(const VertexTy& v) const;

	// Given a statement address the method returns the vertex utilized to store the statement 
	// in this graph. If the statement is not in this dependence graph then false will be returned
	// as second element of the pair
	boost::optional<VertexTy> getStatementID(const core::StatementAddress& addr) const;
	
	// Retrieve the dependence connecting two statements in the dependence graph 
	DependenceIterator deps_begin(const VertexTy& src, const VertexTy& trg) const {
		return DependenceIterator(graph, boost::edge_range(src, trg, graph).first);
	}

	// Retrieve the dependence connecting two statements in the dependence graph 
	DependenceIterator deps_end(const VertexTy& src, const VertexTy& trg) const {
		return DependenceIterator(graph, boost::edge_range(src, trg, graph).second);
	}

	inline size_t size() const { return boost::num_vertices(graph); }
	inline size_t getNumDependencies() const { return boost::num_edges(graph); }

	DependenceList getDependencies() const;

	ComponentList strongComponents() const;

	/**
	 * Tests whether there is a dependency between the given source and sink of the given type.
	 */
	bool containsDependency(const core::StatementAddress& source, const core::StatementAddress& sink, DependenceType type = ALL, int level = -1);

	/**
	 * Produces a printable representation of this dependence graph by listing the dependencesies 
	 */
	virtual std::ostream& printTo(std::ostream& out) const;
	
	/**
	 * Produces a DOT dump of the dependence graph 
	 */
	void dumpDOT(std::ostream& out) const;

private:
	Graph graph;

};

/**
 * Represents a vertex of the dependence graph. It contains the ID of this statement inside the 
 * containing SCoP and the address of the IR statement to which it refers to
 */
class Stmt : public boost::equality_comparable<Stmt>, public utils::Printable {
	const DependenceGraph&  	m_graph;
	DependenceGraph::VertexTy 	m_id;
	core::StatementAddress 		m_addr;
	
	friend class DependenceGraph;
public:

	typedef DependenceGraph::DependenceIteratorImpl<DependenceGraph::OutEdgeIterator> OutgoingDependenceIterator;
	typedef DependenceGraph::DependenceIteratorImpl<DependenceGraph::InEdgeIterator>  IncomingDependenceIterator;

	Stmt(const DependenceGraph& graph, const DependenceGraph::VertexTy& id);
	
	inline const DependenceGraph::VertexTy& id() const { return m_id; }

	inline const core::StatementAddress& addr() const { return m_addr; }

	// Returns an iterator which iterates through the dependencies where vertex v is a sink
	inline IncomingDependenceIterator incoming_begin() const {
		const DependenceGraph::Graph& bg = m_graph.getBoostGraph();
		return IncomingDependenceIterator(bg, boost::in_edges(m_id, bg).first);
	}
	inline IncomingDependenceIterator incoming_end() const {
		const DependenceGraph::Graph& bg = m_graph.getBoostGraph();
		return IncomingDependenceIterator(bg, boost::in_edges(m_id, bg).second);
	}

	inline size_t in_degree() const { return boost::in_degree(m_id, m_graph.getBoostGraph()); }

	// Returns an iterator which iterates through the dependencies where vertex v is a source
	inline OutgoingDependenceIterator outgoing_begin() const {
		const DependenceGraph::Graph& bg = m_graph.getBoostGraph();
		return OutgoingDependenceIterator(bg, boost::out_edges(m_id, bg).first);
	}
	inline OutgoingDependenceIterator outgoing_end() const {
		const DependenceGraph::Graph& bg = m_graph.getBoostGraph();
		return OutgoingDependenceIterator(bg, boost::out_edges(m_id, bg).second);
	}

	inline size_t out_degree() const { return boost::out_degree(m_id, m_graph.getBoostGraph()); }

	// Two stmt are equals only if they are indeed the same object 
	bool operator==(const Stmt& other) const { 
		return this == &other || (&m_graph == &other.m_graph && m_id == other.m_id); 
	}

	virtual std::ostream& printTo(std::ostream& out) const;

	// Implement an automatic convertion of this object to the identifier used within the 
	// boost graph to identify this node 
	operator DependenceGraph::VertexTy() const { return m_id; }
};

inline const core::StatementAddress& DependenceGraph::getStatementAddress(const VertexTy& v) const {
	assert( v < size() && "Vertex id is not part of the graph" );
	return graph[v]->m_addr;
}

class Dependence : public utils::Printable {
	const DependenceGraph&  m_graph;
	DependenceGraph::EdgeTy m_id;
	DependenceType 			m_type;
	DistanceVector 			m_dist;

	friend class DependenceGraph;
public:
	Dependence(const DependenceGraph& graph, const DependenceGraph::EdgeTy& id);

	const DependenceType& type() const { return m_type; }
	const DistanceVector& distance() const { return m_dist; }

	/**
	 * Obtains the level of the dependency (position of first element in distance vector being not 0
	 * starting with 1 for the first component). If the distance vector is all zero it is not a loop
	 * carried dependency and the result will be 0.
	 */
	unsigned getLevel() const;

	inline const Stmt& source() const { 
		return m_graph.getStatement(boost::source(m_id, m_graph.getBoostGraph()));
	}

	inline const Stmt& sink() const { 
		return m_graph.getStatement(boost::target(m_id, m_graph.getBoostGraph()));
	}

	virtual std::ostream& printTo(std::ostream& out) const;
	
	// Implements an automatic convertion of this Dependence to the identifier utilized 
	// within the boost graph to identify this edge
	operator DependenceGraph::EdgeTy() const { return m_id; }

};

DependenceGraph 
extractDependenceGraph(const core::NodePtr& root, 
					   const unsigned& type = RAW | WAR | WAW | RAR, 
					   bool transitive_closure = false);

DependenceGraph 
extractDependenceGraph(core::NodeManager& mgr,
					   const polyhedral::Scop& scop, 
					   const unsigned& type = RAW | WAR | WAW | RAR, 
					   bool transitive_closure = false);

DistanceVector extractDistanceVector(const std::vector<core::VariablePtr>& skel, 
									 core::NodeManager& mgr,
									 const polyhedral::IterationVector& iterVec, 
									 const polyhedral::AffineConstraintPtr& cons);

} // end dep namespace
} // end analysis namespace
} // end insieme namespace 

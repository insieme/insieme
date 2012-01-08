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

namespace insieme {

namespace core {
namespace arithmetic {

class Formula;

} // end arithmetic namespace
} // end core namespace 

namespace analysis {

namespace poly {
class Scop;
}

namespace dep {

// forward decl
class DependenceGraph;

class Stmt {
	unsigned 			m_id;
	core::NodeAddress 	m_addr;

	friend class DependenceGraph;
public:

	Stmt() { }
	Stmt(const core::NodeAddress& addr);
	
	inline const unsigned& id() const { return m_id; }

	const core::NodeAddress& addr() const { return m_addr; }
};

enum DependenceType { RAW=0x1, TRUE=0x1,   // Read-After-Write dependence (or true-dependence)
					  WAR=0x2, ANTI=0x2,   // Write-After-Read dependence (or anti-dependence)
					  WAW=0x4, OUTPUT=0x4, // Write-After-Write dependence (or output-dependence)
					  RAR=0x8, INPUT=0x8,   // Read-After-Read dependence (or input-dependence)
					  ALL=0xF			   // All dependencies
					};

std::string depTypeToStr(const dep::DependenceType& dep);

typedef std::vector<core::arithmetic::Formula> FormulaList;

/**
 * The distance vector is represented by an array of distances (for each iterator in the iteration vector
 * and a constraint which determines the domain for which the dependence exists 
 */
typedef std::pair<
	FormulaList, 
	utils::ConstraintCombinerPtr<core::arithmetic::Formula>
> DistanceVector;


class Dependence {
	
	DependenceType m_type;
	DistanceVector m_dist;

	friend class DependenceGraph;
public:
	Dependence();
	Dependence(const DependenceType& type);

	const DependenceType& type() const { return m_type; }
	const DistanceVector& distance() const { return m_dist; }
};


struct DependenceGraph : public utils::Printable {

	typedef boost::adjacency_list<
		boost::vecS, 
		boost::vecS, 
		boost::directedS, 
		Stmt, 
		Dependence
	> Graph;

	
	typedef typename boost::graph_traits<Graph>::vertex_descriptor 	VertexTy;
	typedef typename boost::graph_traits<Graph>::edge_descriptor   	EdgeTy;

	// Iterator through the Outgoing edges of a vertex 
	typedef typename 
		boost::graph_traits<Graph>::out_edge_iterator 				OutEdgeIterator;

	// Iterator through the incoming edges of a vertex
	typedef typename 
		boost::graph_traits<Graph>::in_edge_iterator 				InEdgeIterator;
	// Iterator through precedent vertices of a vertex: all i: (i -> v)
	typedef typename 
		boost::graph_traits<Graph>::adjacency_iterator 				AdjacencyIterator;

	// Iterator through successive vertices of a vertex: all i: (v -> i)
	typedef typename 
		boost::inv_adjacency_iterator_generator<
			Graph,
			VertexTy, 
			InEdgeIterator
		>::type 													InvAdjacencyIterator;

	DependenceGraph(insieme::core::NodeManager& mgr, 
					size_t stmt_size, 
					const poly::CtxPtr<>& ctx, 
					const poly::MapPtr<>& deps);

	DependenceGraph(insieme::core::NodeManager& mgr, const poly::Scop& scop, const unsigned& depType); 

	const Graph& getBoostGraph() const { return graph; }
	Graph& getBoostGraph() { return graph; }
	
	EdgeTy addDependence(const VertexTy& src, 
						 const VertexTy& sink, 
						 const DependenceType& type, 
						 const DistanceVector& dist = DistanceVector());
	
	std::ostream& printTo(std::ostream& out) const;

private:
	Graph graph;

};

DependenceGraph 
extractDependenceGraph(const core::NodePtr& root, 
					   const unsigned& type = RAW | WAR | WAW | RAR );


DistanceVector extractDistanceVector(core::NodeManager& mgr,
									 const poly::IterationVector& iterVec, 
									 const poly::AffineConstraintPtr& cons);

} // end dep namespace
} // end analysis namespace
} // end insieme namespace 

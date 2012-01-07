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

#include"insieme/analysis/dep_graph.h"


#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/core/ir_node.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include <boost/graph/graphviz.hpp>

#include <fstream>

using namespace insieme::analysis;
using namespace insieme::analysis::poly;

namespace {

typedef std::tuple<
	IslCtx&,
	insieme::core::NodeManager&,
	dep::DependenceGraph&, 
	const dep::DependenceType&
> UserData;

int addDependence(isl_map *map, void *user) {
	
	auto getID = [&] ( const std::string& tuple_name ) -> unsigned { 
		return insieme::utils::numeric_cast<unsigned>( tuple_name.substr(1) );
	};

	UserData& data= *reinterpret_cast<UserData*>(user);
	IslCtx& ctx = std::get<0>(data);
	insieme::core::NodeManager& mgr = std::get<1>(data);
	dep::DependenceGraph& graph = std::get<2>(data);

	dep::DependenceGraph::VertexTy src = getID(isl_map_get_tuple_name(map, isl_dim_in));
	dep::DependenceGraph::VertexTy sink = getID(isl_map_get_tuple_name(map, isl_dim_out));
	
	std::cout << "DELTAS" << std::endl;
 	isl_set* deltas = isl_map_deltas( isl_map_copy( map ) );
	IslSet set( ctx, isl_union_set_from_set(deltas) );

	IterationVector iv;
	AffineConstraintPtr c = set.toConstraint(mgr, iv);

	auto&& distVec = dep::extractDistanceVector(mgr, iv, c);
	
	/*dep::DependenceGraph::EdgeTy&& edge = */graph.addDependence(src, sink, std::get<3>(data), distVec);

	isl_map_free(map);
	return 0;
}

void getDep(isl_union_map* 					umap, 
			IslCtx& 	 					ctx, 
			insieme::core::NodeManager&		mgr,
			dep::DependenceGraph& 			graph, 
			const dep::DependenceType& 		type) 
{

	UserData data(ctx, mgr, graph, type);
	isl_union_map_foreach_map(umap, &addDependence, &data);

}

} // end anonymous namespace 


namespace insieme {
namespace analysis {
namespace dep {

std::string depTypeToStr(const dep::DependenceType& dep) { 

	switch (dep) {
	case RAW: return "RAW";
	case WAR: return "WAR";
	case WAW: return "WAW";
	case RAR: return "RAR";
	default : assert( false && "Dependence type not supported");
	}

}

Stmt::Stmt(const core::NodeAddress& addr) : m_addr(addr) { }

DependenceGraph::DependenceGraph(insieme::core::NodeManager& mgr, 
				size_t num_stmts, 
				const poly::CtxPtr<>& ctx, 
				const poly::MapPtr<>& deps) : graph(num_stmts) 
{
	graph[0].m_id = 0;
	graph[0].m_addr = core::NodeAddress();

	getDep(deps->getAsIslMap(), *ctx, mgr, *this, dep::RAW);
}

DependenceGraph::DependenceGraph(core::NodeManager& mgr, const Scop& scop, const unsigned& depType) : 
	graph( scop.size() ) 
{ 
	// Assign the ID and relative stmts to each node of the graph
	typename boost::graph_traits<Graph>::vertex_iterator vi, vi_end;
	for (tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
		assert(*vi == scop[*vi].getId() && 
				"Assigned ID in the dependence graph doesn't correspond to" 
				" statement ID assigned inside this SCoP"
			  );
		graph[*vi].m_id = *vi;
		graph[*vi].m_addr = scop[*vi].getAddr();
	}
	
	// create a ISL context
	auto&& ctx = makeCtx();

	auto addDepType = [&] (const DependenceType& dep) {
		auto&& depPoly = scop.computeDeps(ctx, dep);
		getDep(depPoly->getAsIslMap(), *ctx, mgr, *this, dep);
	};
	// for each kind of dependence we extract them
	if ((depType & dep::RAW) == dep::RAW) { addDepType(dep::RAW); }
	if ((depType & dep::WAR) == dep::WAR) { addDepType(dep::WAR); }
	if ((depType & dep::WAW) == dep::WAW) { addDepType(dep::WAW); }
	if ((depType & dep::RAR) == dep::RAR) { addDepType(dep::RAR); }
} 

DependenceGraph::EdgeTy DependenceGraph::addDependence(
		const DependenceGraph::VertexTy& src, 
		const DependenceGraph::VertexTy& sink, 
		const DependenceType& type,
		const DistanceVector& distVec) 
{
	auto&& edge = add_edge(src, sink, graph);
	graph[edge.first].m_type = type;
	graph[edge.first].m_dist = distVec;
	return edge.first;
}

DependenceGraph extractDependenceGraph( const core::NodePtr& root, const unsigned& type ) {
	
	assert(root->hasAnnotation(scop::ScopRegion::KEY) && "IR statement must be a SCoP");
	Scop& scop = root->getAnnotation(scop::ScopRegion::KEY)->getScop();

	DependenceGraph ret(root->getNodeManager(), scop, type);
	std::ofstream of("/home/motonacciu/graph.dot", std::ios::out | std::ios::trunc);
 	of << ret;
	of.close();
	return ret;
}

namespace {

// Check wether there are any equalities and extract it while keeping all the disequalities as 
// domain of existence of this distance vector 
//
struct DistanceVectorExtractor : public utils::RecConstraintVisitor<AffineFunction> {

	AffineConstraintPtr 	newCC;
	IterationVector		 	iterVec;
	core::NodeManager&		mgr;
	core::ExpressionList 	distVec;

	DistanceVectorExtractor(const poly::IterationVector& iterVec, core::NodeManager& mgr) : 
		iterVec(iterVec), mgr(mgr), distVec(iterVec.getIteratorNum()) { }

	void visit(const RawAffineConstraint& rcc) { 
		const AffineConstraint& c = rcc.getConstraint();
	
		if (c.getType() == utils::ConstraintType::EQ) {
			// this will go into the distance vector, we make a copy
			poly::AffineFunction af = c.getFunction();
			// we expect only 1 of the iterators to have a coefficient != 0
			bool found = false;
			for_each(iterVec.iter_begin(), iterVec.iter_end(), 
				[&](const Iterator& cur) {
					int coeff = af.getCoeff(cur);
					if (!coeff) { return; }
					
					assert(coeff == 1 && "The coefficient value is not unitary as expected");
					assert(!found && "More than 1 iterator have unary coefficient");
					found = true;
					assert(!distVec[iterVec.getIdx(cur)] && "Expected a NULL pointer");
					// reset the value of the coefficient 
					af.setCoeff(cur, 0);

					distVec[iterVec.getIdx(cur)] = toIR(mgr, af);
				});
			return;
		} 

		newCC = newCC ? (newCC and c) : makeCombiner(c);
	}

	virtual void visit(const NegatedAffineConstraint& ucc) {
		// Because the constraint containing a distance vector is a basic_set, 
		// there should not be any negations in this constraint 
		assert(false);
	}

	virtual void visit(const BinaryAffineConstraint& bcc) {
		assert(bcc.getType() == BinaryAffineConstraint::AND && "This is not possible");
		bcc.getLHS()->accept(*this);
		AffineConstraintPtr lhs = newCC;

		bcc.getRHS()->accept(*this);
		AffineConstraintPtr rhs = newCC;

		newCC = lhs and rhs;
	}
};

} // end anonymous namespace 

DistanceVector extractDistanceVector(core::NodeManager& mgr, 
									 const IterationVector& iterVec, 
									 const poly::AffineConstraintPtr& cons) 
{
	DistanceVectorExtractor dve(iterVec, mgr);
	cons->accept(dve);

	assert(all(dve.distVec, [](const core::ExpressionPtr& cur) { return static_cast<bool>(cur); }));
	return std::make_pair(dve.distVec, dve.newCC);
}

std::ostream& DependenceGraph::printTo(std::ostream& out) const {

	auto&& node_printer =[&] (std::ostream& out, const DependenceGraph::VertexTy& v) { 
			out << " [label=\"S" << v << "\"]";
		};

	auto&& edge_printer = [&] (std::ostream& out, const DependenceGraph::EdgeTy& e) { 
		std::cout << "HELLO" << std::endl;
		const dep::Dependence& dep = graph[e];
		out << "[label=\"" << depTypeToStr(dep.type()) << " DIST: <" << 
			join(", ", dep.distance().first, 
					[&](std::ostream& jout, const core::ExpressionPtr& cur) { jout << *cur; });
		out << "> ";

		//if (dep.distance().second) {
		//	out << "DOM: " << *dep.distance().second;
		//}

		out << "\", ";

		switch(dep.type()) {
			case dep::RAW: out << "color=red";
						   break;
			case dep::WAR: out << "color=orange";
						   break;
			case dep::WAW: out << "color=brown";
						   break;
			case dep::RAR: out << "color=green";
						   break;
			default: assert(false);
		}
		out << "]";
	};

	boost::write_graphviz(out, graph, node_printer, edge_printer);
	return out;
}

} // end dep namespace
} // end analysis namespace
} // end insieme namespace 

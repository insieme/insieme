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

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include <boost/graph/graphviz.hpp>

#include <fstream>

using namespace insieme::analysis;
using namespace insieme::analysis::poly;

typedef typename BackendTraits<POLY_BACKEND>::ctx_type PolyCtx;

namespace {

typedef std::tuple<
	PolyCtx&,
	dep::DependenceGraph&, 
	const dep::DependenceType&
> UserData;

int addDependence(isl_map *map, void *user) {
	
	auto getID = [&] ( const std::string& tuple_name ) -> unsigned { 
		return insieme::utils::numeric_cast<unsigned>( tuple_name.substr(1) );
	};

	UserData& data= *reinterpret_cast<UserData*>(user);

	dep::DependenceGraph::VertexTy src = getID(isl_map_get_tuple_name(map, isl_dim_in));
	dep::DependenceGraph::VertexTy sink = getID(isl_map_get_tuple_name(map, isl_dim_out));
	
	dep::DependenceGraph::EdgeTy&& edge = std::get<1>(data).addDependence(src, sink, std::get<2>(data));
	// printIslMap(std::cout, std::get<0>(data).getRawContext(), isl_union_map_from_map( isl_map_copy(map)));

 	isl_set* deltas = isl_map_deltas( isl_map_copy( map ) );
	isl_union_set* uset = isl_union_set_from_set(deltas);

//	if (deltas) {
		// printIslSet(std::cout, std::get<0>(data).getRawContext(), uset); 
//	} else 
//		LOG(INFO) << "EMPTY";

	isl_union_set_free(uset);
	isl_map_free(map);
	return 0;
}

void getDep(isl_union_map* 					umap, 
			PolyCtx& 	 					ctx, 
			dep::DependenceGraph& 			graph, 
			const dep::DependenceType& 		type) 
{

	UserData data(ctx, graph, type);
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

DependenceGraph::DependenceGraph(const Scop& scop, const unsigned& depType) : 
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
	BackendTraits<POLY_BACKEND>::ctx_type ctx;

	auto addDepType = [&] (const DependenceType& dep) {
		auto&& depPoly = scop.computeDeps(ctx, dep);
		getDep(depPoly->getAsIslMap(), ctx, *this, dep);
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
		const DependenceType& type) 
{
	auto&& edge = add_edge(src, sink, graph);
	graph[edge.first].m_type = type;
	return edge.first;
}

DependenceGraph extractDependenceGraph( const core::NodePtr& root, const unsigned& type ) {
	
	assert(root->hasAnnotation(scop::ScopRegion::KEY) && "IR statement must be a SCoP");
	Scop& scop = root->getAnnotation(scop::ScopRegion::KEY)->getScop();

	DependenceGraph ret(scop, type);
	std::ofstream of("/home/motonacciu/graph.dot", std::ios::out | std::ios::trunc);
 	of << ret;
	of.close();
	return ret;
}

std::ostream& DependenceGraph::printTo(std::ostream& out) const {

	auto&& node_printer =[&] (std::ostream& out, const DependenceGraph::VertexTy& v) { 
			out << " [label=\"S" << v << "\"]";
		};

	auto&& edge_printer = [&] (std::ostream& out, const DependenceGraph::EdgeTy& e) { 
			out << "[label=\"" << depTypeToStr(graph[e].type()) << "\", ";
			switch(graph[e].type()) {
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

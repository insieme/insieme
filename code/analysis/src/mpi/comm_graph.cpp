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

#include <functional>

#include "insieme/analysis/mpi/comm_graph.h"
#include "insieme/analysis/cfg.h"

#include "insieme/annotations/mpi/mpi_annotations.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/timer.h"

#include <boost/bind.hpp>
#include <boost/config.hpp>

using namespace insieme;
using namespace insieme::core;

using namespace insieme::analysis::mpi;
using namespace insieme::annotations::mpi;

typedef std::vector<CallExprAddress> CallExprList;

namespace insieme {
namespace analysis {
namespace mpi {

CommGraph extractCommGraph( const core::NodePtr& program ) {
	
	utils::Timer timer("Insieme.MPI.Communication.Graph");

	CallExprList mpiCalls;
	
	auto&& filter = [&] (const CallExprAddress& callExpr) -> bool { 
		static core::LiteralAddress lit;
		return (lit = dynamic_address_cast<const Literal>(callExpr->getFunctionExpr()) ) && 
			    lit->getStringValue().compare(0,4,"MPI_") == 0;
	};

	typedef void (CallExprList::*PushBackPtr)(const CallExprAddress&);

	PushBackPtr push_back = &CallExprList::push_back;
	visitDepthFirst( NodeAddress(program), makeLambdaVisitor( filter, fun(mpiCalls, push_back) ) );

	LOG(DEBUG) << "Found " << mpiCalls.size() << " MPI calls";

	// Check wether MPI calls are correctly annotated
	CallExprList toAnnotate;
	auto&& twin = filterIterator(mpiCalls.begin(), mpiCalls.end(), 
		[&] (const CallExprAddress& curr) { 
			assert (curr.getParentNode()->getNodeType() == core::NT_MarkerExpr);
			return curr.getParentNode()->hasAnnotation(CallID::KEY); 
		} );
	
	LOG(INFO) << "Non annotated calls: " << std::distance(twin.first, twin.second);
	assert(std::distance(twin.first, twin.second) == 0);
	
	typedef boost::graph_traits<CommGraph>::vertex_descriptor VertexTy;

	CommGraph graph(mpiCalls.size());

	std::map<size_t, VertexTy> id_map;
	VertexTy vid=0;
	for_each(mpiCalls, [&](const CallExprAddress& call) { 
			size_t id = call.getParentNode().getAnnotation(CallID::KEY)->id();
			id_map[id] = vid;

			graph[vid].callID = id;
			graph[vid].call = call;
			vid++;
		});

	// Build the graph 
	for_each(mpiCalls, [&](const CallExprAddress& cur) {
		const CallID& call = *cur.getParentNode()->getAnnotation(CallID::KEY);
		for_each(call.deps(), [&] (const size_t& cur) {
				assert( id_map.find(call.id()) != id_map.end() && id_map.find(cur) != id_map.end() );
				boost::add_edge( id_map[call.id()], id_map[cur], graph );
			});
	});

	timer.stop();
	LOG(INFO) << timer;
	
	LOG(DEBUG) << graph;

	return graph;
}

void merge(CFGPtr& cfg, const CommGraph& commGraph) {
	// The idea of this function is to iterator over each vertex of the communication graph. For
	// every vertex containing outgoing edges we find in cfg the cfg block containing that
	// communication statement and inesert a corresponding communication edge
	typedef boost::graph_traits<CommGraph>::vertex_descriptor VertexTy;
	typedef boost::graph_traits<CommGraph>::vertex_iterator   VertexIter;
	typedef boost::graph_traits<CommGraph>::out_edge_iterator OutEdgeIter;

	VertexIter vi, vi_end;
	for( boost::tie(vi, vi_end) = vertices(commGraph); vi != vi_end; ++vi ) {
		OutEdgeIter ei, ei_end;
		boost::tie(ei, ei_end) = out_edges(*vi, commGraph);
		if (std::distance(ei, ei_end) != 0) {
			// Add the edges 
			std::pair<cfg::BlockPtr,size_t> srcBlock = cfg->find( commGraph[*vi].call );
			assert( srcBlock.first );

			for(; ei!=ei_end; ++ei) {
				std::pair<cfg::BlockPtr,size_t> trgBlock = cfg->find( commGraph[ boost::target(*ei, commGraph) ].call );
				assert(trgBlock.first);

				cfg->addEdge(*srcBlock.first, *trgBlock.first);

			}
		}
	}
}

} // end mpi namespace
} // end analysis namespace
} // end insieme namespace 

namespace std {

std::ostream& operator<<(std::ostream& out, const CommGraph& graph) {
	
	typedef boost::graph_traits<CommGraph>::vertex_descriptor VertexTy;

	auto&& node_printer =[&] (std::ostream& out, const VertexTy& v) { 
			out << " [label=\"ID" << graph[v].callID << " "
				<< static_address_cast<const Literal>(graph[v].call->getFunctionExpr())->getStringValue() 
				<< "\"]";
		};

	boost::write_graphviz(out, graph, node_printer);

	return out;
}

} // end std namespace 

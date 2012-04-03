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

#include <boost/graph/depth_first_search.hpp>

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include <boost/graph/graphviz.hpp>
#include <boost/graph/strong_components.hpp>

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

namespace {

typedef std::tuple<
	const Scop&,
	IslCtx&,
	NodeManager&,
	dep::DependenceGraph&, 
	const dep::DependenceType&
> UserData;


int addDependence(isl_basic_map *bmap, void *user) {
	
	auto getID = [&] ( const std::string& tuple_name ) -> unsigned {
		return insieme::utils::numeric_cast<unsigned>( tuple_name.substr(1) );
	};

	UserData& data= *reinterpret_cast<UserData*>(user);
	const Scop& scop = std::get<0>(data);
	IslCtx& ctx = std::get<1>(data);
	NodeManager& mgr = std::get<2>(data);
	dep::DependenceGraph& graph = std::get<3>(data);

	IslMap mmap(ctx, isl_union_map_from_map(isl_map_from_basic_map(isl_basic_map_copy(bmap))));

	dep::DependenceGraph::VertexTy src = getID(isl_basic_map_get_tuple_name(bmap, isl_dim_in));
	dep::DependenceGraph::VertexTy sink = getID(isl_basic_map_get_tuple_name(bmap, isl_dim_out));
	
	// we get the loop nests for the two statements 
	std::vector<VariablePtr>&& srcNest = scop[src].loopNest();
	std::vector<VariablePtr>&& sinkNest = scop[sink].loopNest();
	// cound the dimensions which are equal
	size_t idx=0;
	for(; idx<std::min(srcNest.size(), sinkNest.size()) && srcNest[idx] == sinkNest[idx]; ++idx)  ;
	
	// the first idx dimensions are the same, therefore this is the size of our distance vector
	std::vector<VariablePtr> distVecSkel(srcNest.begin(), srcNest.begin()+idx);

	bmap = isl_basic_map_set_tuple_name(bmap, isl_dim_in, NULL);
	bmap = isl_basic_map_set_tuple_name(bmap, isl_dim_out, NULL);

 	isl_set* deltas = isl_map_deltas( isl_map_from_basic_map( isl_basic_map_copy( bmap ) ) );

	assert(deltas && isl_set_n_basic_set(deltas) == 1);

	IslSet set( ctx, isl_union_set_from_set(deltas) );
	set.simplify();

	IterationVector iv;
	AffineConstraintPtr c = set.toConstraint(mgr, iv);

	auto&& distVec = dep::extractDistanceVector(distVecSkel, mgr, iv, c);
	graph.addDependence(src, sink, std::get<4>(data), distVec);

	isl_basic_map_free(bmap);
	return 0;
}

int visit_isl_map(isl_map* map, void* user) {
	int ret = isl_map_foreach_basic_map(map, &addDependence, user);
	isl_map_free(map);
	return ret;
}

void getDep(isl_union_map* 				umap, 
			const Scop&					scop,
			IslCtx& 	 				ctx, 
			NodeManager&				mgr,
			dep::DependenceGraph& 		graph, 
			const dep::DependenceType& 	type) 
{
	UserData data(scop, ctx, mgr, graph, type);
	isl_union_map_foreach_map(umap, &visit_isl_map, &data);
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

//==== Stmt =====================================================================================
Stmt::Stmt(const DependenceGraph& graph, const DependenceGraph::VertexTy& id) : m_graph(graph), m_id(id) { }

std::ostream& Stmt::printTo(std::ostream& out) const {
	out << "STMT: " << m_id << " [" << *m_addr << "]" << std::endl;
	out << "@ Source of dependence: " << std::endl;
	for_each(incoming_begin(), incoming_end(), [&](const Dependence& cur) {
				out << "  " << cur << std::endl;
			});
	out << "@ Sink of dependence: " << std::endl;
	for_each(outgoing_begin(), outgoing_end(), [&](const Dependence& cur) {
				out << "  " << cur << std::endl;
			});
	return out;
}

//==== Dependence =================================================================================
Dependence::Dependence(const DependenceGraph& graph, const DependenceGraph::EdgeTy& id) : 
	m_graph(graph), m_id(id) { }

unsigned Dependence::getLevel() const {
	const FormulaList& list = m_dist.first;
	// search for first non-zero distance
	for(unsigned i=0; i<list.size(); i++) {
		if (!list[i].isZero()) {
			return i+1;
		}
	}
	// it is not a loop-carried dependency
	return 0;
}

std::ostream& Dependence::printTo(std::ostream& out) const {
	out << depTypeToStr(m_type) << " (" << source().id() << " -> " << sink().id() << ")";
	if (m_dist.first.empty())
		return out;
	out << " " << toString(m_dist.first);
	if (m_dist.second) {
		out << " if: " << *m_dist.second;
	}
	return out;
}

//==== DependenceGraph ============================================================================
DependenceGraph::DependenceGraph(core::NodeManager& mgr, 
								 const Scop& scop, 
								 const unsigned& depType,
								 bool transitive_closure) : 
	graph( scop.size() ) 
{ 
	// Assign the ID and relative stmts to each node of the graph
	typename boost::graph_traits<Graph>::vertex_iterator vi, vi_end;
	for (tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
		assert(*vi == scop[*vi].getId() && 
				"Assigned ID in the dependence graph doesn't correspond to" 
				" statement ID assigned inside this SCoP"
			  );
		graph[*vi] = std::make_shared<Stmt>( *this, *vi ); 
		graph[*vi]->m_addr = scop[*vi].getAddr();
	}
	
	// create a ISL context
	auto&& ctx = makeCtx();

	auto addDepType = [&] (const DependenceType& dep) {
		auto&& depPoly = scop.computeDeps(ctx, dep);
		isl_union_map* map = depPoly->getIslObj();
		if (transitive_closure) {
			int exact;
			map = isl_union_map_transitive_closure( map, &exact );
			if (!exact) {
				LOG(WARNING) << "Computation of transitive closure resulted in a overapproximation";
			}
		}
		getDep(map, scop, *ctx, mgr, *this, dep);
		isl_union_map_free(map);
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
	graph[edge.first] = std::make_shared<Dependence>( *this, edge.first );
	graph[edge.first]->m_type = type;
	graph[edge.first]->m_dist = distVec;
	return edge.first;
}

DependenceGraph extractDependenceGraph( const core::NodePtr& root, 
										const unsigned& type,
										bool transitive_closure) 
{
	polyhedral::scop::mark(root);	// add scop annotation if necessary
	assert(root->hasAnnotation(scop::ScopRegion::KEY) && "IR statement must be a SCoP");
	Scop& scop = root->getAnnotation(scop::ScopRegion::KEY)->getScop();
	
	return extractDependenceGraph(root->getNodeManager(), scop, type, transitive_closure);
}

DependenceGraph 
extractDependenceGraph(core::NodeManager& mgr,
					   const polyhedral::Scop& scop, 
					   const unsigned& type,
					   bool transitive_closure) 
{
	DependenceGraph ret(mgr, scop, type, transitive_closure);
	return ret;
}

ComponentList DependenceGraph::strongComponents() const {
	// Compute strong connected components 
	size_t num_vertices = size();
	std::vector<int> component(num_vertices);
	std::vector<boost::default_color_type> color(num_vertices);
	std::vector<VertexTy> root(num_vertices);

	int num = strong_components(graph, &component.front(), 
                              	       boost::root_map(&root.front()));

	ComponentList comps(num);

	for(size_t v=0; v<num_vertices; ++v) {
		comps[ component[v] ].first = graph[root[v]];
		comps[ component[v] ].second.insert( graph[v] );
	}

	return comps;
}


DependenceList DependenceGraph::getDependencies() const {
	DependenceList ret;

	VertexIterator vi, vi_end;
	for(tie(vi, vi_end) = boost::vertices(graph); vi != vi_end; ++vi) {
		for_each(getStatement(*vi).outgoing_begin(), getStatement(*vi).outgoing_end(), 
			[&](const Dependence& dep) { 
				ret.push_back( 
					DependenceInstance(
						dep.type(), 
						dep.source().addr(), 
						dep.sink().addr(), 
						dep.distance()
					) );
			});
	}
	return ret;
}

namespace {

// Check wether there are any equalities and extract it while keeping all the disequalities as 
// domain of existence of this distance vector 
//
struct DistanceVectorExtractor : public utils::RecConstraintVisitor<AffineFunction, AffineConstraintPtr> {

	IterationVector		 			iterVec;
	core::NodeManager&				mgr;
	const std::vector<VariablePtr>& skel;
	FormulaList						distVec;

	DistanceVectorExtractor(const polyhedral::IterationVector& iterVec, 
							core::NodeManager& mgr, 
							const std::vector<VariablePtr>& skel) : 
		iterVec(iterVec), mgr(mgr), 
		skel(skel), 
		distVec(skel.size()) { }

	AffineConstraintPtr visitRawConstraint(const RawAffineConstraint& rcc) { 
		const AffineConstraint& c = rcc.getConstraint();
	
		if (c.getType() == utils::ConstraintType::EQ) {
			// this will go into the distance vector, we make a copy
			polyhedral::AffineFunction af = c.getFunction();

			// we expect only 1 of the iterators to have a coefficient != 0
			bool found = false;
			for_each(iterVec.iter_begin(), iterVec.iter_end(), 
				[&](const Iterator& cur) {
					VariablePtr var = static_pointer_cast<const VariablePtr>(cur.getExpr());
					
					auto&& fit = find(skel.begin(), skel.end(), var);
					if(fit == skel.end()) { return; }
					
					int coeff = af.getCoeff(cur);
					if (!coeff) { return; }
					
					assert(coeff == 1 && "The coefficient value is not unitary as expected");
					assert(!found && "More than 1 iterator have unary coefficient");
					found = true;
		 			// assert(!distVec[iterVec.getIdx(cur)] && "Expected a NULL pointer");
					// reset the value of the coefficient 
					af.setCoeff(cur, 0);

					distVec[ std::distance(skel.begin(), fit)] = core::arithmetic::Formula()-af;
				});

			return AffineConstraintPtr();
		} 

		return makeCombiner(c);
	}

	AffineConstraintPtr visitNegConstraint(const NegAffineConstraint& ucc) {
		// Because the constraint containing a distance vector is a basic_set, 
		// there should not be any negations in this constraint 
		assert(false);
	}

	AffineConstraintPtr visitBinConstraint(const BinAffineConstraint& bcc) {
		assert(bcc.isConjunction() && "This is not possible");

		AffineConstraintPtr lhs = visit(bcc.getLHS());
		AffineConstraintPtr rhs = visit(bcc.getRHS());

		if (lhs && !rhs) { return lhs; }
		if (!lhs && rhs) { return rhs; }
		if (rhs && lhs) { return lhs and rhs; }
		return AffineConstraintPtr();
	}
};

} // end anonymous namespace 

DistanceVector extractDistanceVector(const std::vector<VariablePtr>& skel,
									 core::NodeManager& mgr, 
									 const IterationVector& iterVec, 
									 const polyhedral::AffineConstraintPtr& cons) 
{
	DistanceVectorExtractor dve(iterVec, mgr, skel);
	AffineConstraintPtr&& res = dve.visit(cons);

	// assert(all(dve.distVec, [](const core::Formula& cur) { return static_cast<bool>(cur); }));
	utils::CombinerPtr<core::arithmetic::Formula> cf;
	if (res) {
		cf = utils::castTo<AffineFunction, core::arithmetic::Formula>(res);
	}
	return std::make_pair(dve.distVec, cf);
}

bool DependenceGraph::containsDependency(
		const core::StatementAddress& source, 
		const core::StatementAddress& sink, 
		DependenceType type, 
		int level) 
{
	// start by determining IDs of source and sink
	auto sourceID = getStatementID(source);
	auto sinkID = getStatementID(sink);

	// if statements are unknown => no dependency
	if (!sourceID || !sinkID) { return false; }

	// search for dependency by iterating over all of them
	return any(
			deps_begin(*sourceID, *sinkID), deps_end(*sourceID, *sinkID),
			[&](const Dependence& cur) { 
				return (cur.m_type & type) && (level < 0 || cur.getLevel() == static_cast<unsigned>(level)); 
			}
	);
}

boost::optional<DependenceGraph::VertexTy> 
DependenceGraph::getStatementID(const core::StatementAddress& addr) const {
	
	VertexIterator vi, vi_end;
	for(tie(vi, vi_end) = boost::vertices(graph); vi != vi_end; ++vi) {
		if (graph[*vi]->m_addr == addr) {
			return boost::optional<VertexTy>(*vi);
		}
	}
	return boost::optional<VertexTy>();
}

std::ostream& DependenceGraph::printTo(std::ostream& out) const {
	out << "@~~~> List of statements within Dependency Graph <~~~@" << std::endl;
	size_t num=0;
	VertexIterator vi, vi_end;
	for(tie(vi, vi_end) = boost::vertices(graph); vi != vi_end; ++vi) {
		out << num++ << ": " << *(graph[*vi]->m_addr) << std::endl;
	}

	out << "@~~~> List of dependencies within this SCoP <~~~@" << std::endl;
	num = 0;
	EdgeIterator ei, ei_end;
	for(tie(ei, ei_end) = boost::edges(graph); ei != ei_end; ++ei) {
		out << num++ << ": " << *graph[*ei] << std::endl;
	}
	out << "Total dependencies: " << num << std::endl;
	auto&& scc = strongComponents();
	out << "Total number of strongly connected components: " << scc.size() << std::endl;
	for(size_t c=0; c<scc.size(); ++c) {
		out << "  "<< c << " -> {" << 
			join(",", scc[c].second, [](std::ostream& jout, const StmtPtr& cur) { jout << cur->id(); }) 
			<< "}" << std::endl;
	}
	out << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	// we list the dependencies involving each statement 
	return out;
}

void DependenceGraph::dumpDOT(std::ostream& out) const {

	auto&& node_printer =[&] (std::ostream& out, const DependenceGraph::VertexTy& v) { 
			out << " [label=\"S" << v << "\"]";
		};

	auto&& edge_printer = [&] (std::ostream& out, const DependenceGraph::EdgeTy& e) { 
		const dep::Dependence& dep = *graph[e];
		out << "[label=\"" << depTypeToStr(dep.type()) << " <" << 
			join(", ", dep.distance().first, 
					[&](std::ostream& jout, const core::arithmetic::Formula& cur) { jout << cur; });
		out << "> ";

		if (dep.distance().second) {
			out << "if: " << *dep.distance().second;
		}

		out << "\", ";

		switch(dep.type()) {
			case dep::RAW: out << "color=red";     break;
			case dep::WAR: out << "color=orange";  break;
			case dep::WAW: out << "color=brown";   break;
			case dep::RAR: out << "color=green";   break;
			default: assert(false);
		}
		out << "]";
	};

	boost::write_graphviz(out, graph, node_printer, edge_printer);
}

} // end dep namespace
} // end analysis namespace
} // end insieme namespace 

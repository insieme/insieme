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

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/strong_components.hpp>

namespace clang {
class FunctionDecl;
class Type;
} // end clang namespace

namespace std {
std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl);
} // end std namespace

namespace insieme {
namespace frontend {
namespace utils {

/**
 * Utility class used to build the dependencies between types and functions.
 * Uses boost::graph to compute the strong connected components.
 */
template <class T>
class DependencyGraph {

	struct NodeProperty {
		T node;
	};

	template <class NodeTy>
	class label_writer {
	public:
		label_writer(NodeTy& node) : node(node) { }

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			out << "[label=\"" << node[v] << "\"]";
		}
	private:
		NodeTy& node;
	};

public:
	typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, NodeProperty> NodeDepGraph;

	typedef typename boost::graph_traits<NodeDepGraph>::vertex_descriptor 	VertexTy;
	typedef typename boost::graph_traits<NodeDepGraph>::edge_descriptor 	EdgeTy;

	DependencyGraph(): dirtyFlag(true), numComponents(0) { }

	/**
	 * Finds the node inside the graph and returns the vertex id used to identify this node
	 */
	std::pair<bool,VertexTy> find(const T& type) const {
		typename boost::property_map<NodeDepGraph, T NodeProperty::*>::const_type node = get(&NodeProperty::node, graph);

		typename boost::graph_traits<NodeDepGraph>::vertex_iterator vertCurrIt, vertEndIt;
		boost::tie(vertCurrIt, vertEndIt) = boost::vertices(graph);
		for(;vertCurrIt != vertEndIt; ++vertCurrIt) {
			if(node[*vertCurrIt] == type)
				return std::make_pair(true, *vertCurrIt);
		}
		return std::make_pair(false, 0);
	}

	VertexTy addNode(const T& type, const VertexTy* parent = NULL ) {
		auto&& fit = find(type);
		if(fit.first) {
			// node was found
			if(parent)
				boost::add_edge(*parent, fit.second, graph);
			return fit.second;
		}

		// this node is not inside the graph, we have to add it
		VertexTy&& v = boost::add_vertex(graph);
		if(parent) {
			// we have to add an edge between this node and the parent
			boost::add_edge(*parent, v, graph);
		}
		dirtyFlag = true;
		typename boost::property_map<NodeDepGraph, T NodeProperty::*>::type&& node = get(&NodeProperty::node, graph);
		boost::put(node, v, type);
		Handle(type, v);

		return v;
	}

	std::set<T> getStronglyConnectedComponents(const T& t) {
		auto&& fit = find(t);
		assert(fit.first && "Type node is not in the graph");

		VertexTy v = fit.second;

		// We update the information of strongly connected components in the case the graph has been changed
		updateStrongComponents();

		// We return a set of Types which are connected with the input type t
		std::set<T> ret;
		typename boost::property_map<NodeDepGraph, T NodeProperty::*>::type node = get(&NodeProperty::node, graph);
		for (std::vector<size_t>::size_type i = 0, e = strongComponents.size(); i != e; ++i)
			// we have check if the two nodes are in the same component
			if((i != v && strongComponents[i] == strongComponents[v]) || (i == v && boost::edge(v, v, graph).second)) {
				// in the case we are comparing the same node, we add it to the result only if there is an explicit
				// link from the node to itself
				ret.insert( node[i] );
			}

		return ret;
	}

	std::set<T> getSubComponents(const T& t) {
		// update the strong components
		getStronglyConnectedComponents(t);
		
		// before resolving this component we have to resolve all the
		// components with an ID with is smaller than the current. For 
		// each of these components, we return the root node 
		auto&& fit = find(t);
		assert(fit.first && "Type node is not in the graph");
		VertexTy v = fit.second;

		std::set<T> ret;
		size_t compID = strongComponents[v];

		typename boost::property_map<NodeDepGraph, T NodeProperty::*>::type node = get(&NodeProperty::node, graph);
		for(size_t idx=0; idx<compID; ++idx) {
			auto fidx = std::find(strongComponents.begin(), strongComponents.end(), idx);
			assert(fidx != strongComponents.end());
			ret.insert( node[rootMap[std::distance(strongComponents.begin(), fidx)]] );
		}

		return ret;
	}

	void print(std::ostream& out) const {
		typename boost::property_map<NodeDepGraph, T NodeProperty::*>::const_type&& node = get(&NodeProperty::node, graph);
		boost::write_graphviz(out, graph, label_writer<typename boost::property_map<NodeDepGraph, T NodeProperty::*>::const_type>(node));
	}

	~DependencyGraph() {
	   graph.clear();
	}

private:

	/**
	 * Calls the boost::strong_component() algorithm on the graph, this should be done only if the graph has been changed (thus
	 * the dirtyFlag is true).
	 */
	void updateStrongComponents() {
		if ( !dirtyFlag ) 
			return ;

		strongComponents.resize( num_vertices(graph) );
		rootMap.resize( num_vertices(graph) );
		numComponents = boost::strong_components(graph, &strongComponents[0], boost::root_map(&rootMap[0]));

		dirtyFlag = false;
	}

	void Handle(T type, const VertexTy& v);

	NodeDepGraph graph;
	bool dirtyFlag;
	size_t numComponents;
	std::vector<size_t> strongComponents;
	std::vector<VertexTy> rootMap;

};

} // end utils namespace
} // end frontend namespace
} // end insieme namespace

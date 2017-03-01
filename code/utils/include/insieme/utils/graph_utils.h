/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <iterator>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/strong_components.hpp>
#include <boost/graph/topological_sort.hpp>

#include <boost/unordered_map.hpp>
#include <boost/optional.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace utils {
namespace graph {

	/**
	 * A simple, empty struct used as a label for unlabeled graphs.
	 */
	struct unlabeled : public Printable {
		std::ostream& printTo(std::ostream& out) const {
			return out;
		}
	};

	/**
	 * A type wrapper allowing unordered maps to be used to maintain vertices within a graph.
	 */
	template <class V, class D>
	class SimpleUnorderedMap : public boost::unordered_map<V, D> {};

	/**
	 * A functor realizing no label.
	 */
	struct no_label {
		template <class T>
		void operator()(std::ostream& out, const T& t) const {}
	};

	/**
	 * A functor realizing a default to-string label.
	 */
	struct default_label {
		template <class T>
		void operator()(std::ostream& out, const T& t) const {
			out << t;
		}
	};

	/**
	 * A functor realizing no decoration.
	 */
	struct default_deco {
		template <class T>
		void operator()(std::ostream& out, const T& t) const {}
	};

	/**
	 * A functor class capable of printing labels within dot files for nodes and edges.
	 */
	template <class Graph, class Printer = default_label, class Decorator = default_deco>
	class label_printer {
		Graph graph;
		const Printer& printer;
		const Decorator& decorator;

	  public:
		label_printer(Graph _graph, const Printer& printer, const Decorator& decorator) : graph(_graph), printer(printer), decorator(decorator) {}

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			out << "[label=\"";
			printer(out, graph[v]);
			out << "\" ";
			decorator(out, graph[v]);
			out << "]";
		}
	};

	/**
	 * A template for a boost graph wrapper supporting an arbitrary vertex nodes and edge labels. The resulting graph will
	 * not support parallel edges (hence, multiple edges from the same source to the same sink). Unlike the pure boost graph,
	 * this template class can be used similar to a container.
	 *
	 * @tparam Vertex the type of vertex to be maintained within the graph
	 * @tparam EdgeLabel the type of label to be used on the edges (unlabeled by default)
	 * @tparam DirectedS allows to determine the structure of the wrapped graph
	 * @tparam VertexMap the kind of map to be used for maintaining vertices / vertices_descripter connections within this graph
	 */
	template <class Vertex, class EdgeLabel = unlabeled, class DirectedS = boost::bidirectionalS,
	          template <class V, class D> class VertexMap = SimpleUnorderedMap>
	class Graph : public Printable {
	  public:
		// some member type definitions
		typedef Vertex VertexType;
		typedef EdgeLabel EdgeLabelType;

		// the type of underlying boost graph
		typedef typename boost::adjacency_list<boost::hash_setS, boost::vecS, DirectedS, Vertex, EdgeLabel> GraphType;

		// also some derived types
		typedef typename boost::graph_traits<GraphType>::vertex_descriptor vertex_descriptor;

	  private:
		// some private type definitions
		typedef VertexMap<Vertex, vertex_descriptor> VertexMapType;

		/**
		 * This boost adjacency_list represents the internally maintained graph data structure.
		 */
		GraphType graph;

		/**
		 * In addition to the graph data structure, a register for the involved vertices is added.
		 */
		VertexMapType vertexMap;

	  public:
		/**
		 * Obtains a reference to the internally maintained boost graph instance.
		 *
		 * @return a constant reference to the internally maintained boost graph; the graph may be used for
		 * 			applying boost graph algorithms on it.
		 */
		const GraphType& asBoostGraph() const {
			return graph;
		}

		/**
		 * An implicit converter of this builder into the resulting graph.
		 */
		operator const GraphType&() const {
			return graph;
		}

		/**
		 * Adds a new vertex to the represented graph.
		 *
		 * @param vertex the new vertex
		 * @return true if this is a new vertex, false if the same vertex was already present
		 */
		bool addVertex(const Vertex& vertex) {
			// use internal implementation for adding a vertex
			return addVertexInternal(vertex).second;
		}

		/**
		 * Obtains a reference to the internal copy of the given vertex. If the vertex
		 * is not present, a copy will be added.
		 *
		 * @param vertex the vertex to be obtained
		 * @return a reference to the internal copy of the given vertex
		 */
		Vertex& getVertex(const Vertex& vertex) {
			return graph[addVertexInternal(vertex).first];
		}

		/**
		 * Obtains the descriptor utilized within the underlying boost graph to represent
		 * the given vertex value.
		 */
		vertex_descriptor getVertexDescriptor(const Vertex& vertex) const {
			assert_true(vertexMap.find(vertex) != vertexMap.end());
			return vertexMap.find(vertex)->second;
		}

		/**
		 * Looks up the vertex-value associated with the given descriptor.
		 */
		const Vertex& getVertexFromDescriptor(const vertex_descriptor& desc) const {
			return graph[desc];
		}

		/**
		 * Tests whether the given vertex is part of this graph.
		 *
		 * @param vertex the vertex to be tested
		 * @return true if so, false otherwise
		 */
		bool containsVertex(const Vertex& vertex) const {
			return vertexMap.find(vertex) != vertexMap.end();
		}

		/**
		 * Obtains the total number of vertices within this graph.
		 *
		 * @return the total number of vertices.
		 */
		std::size_t getNumVertices() const {
			return vertexMap.size();
		}

		/**
		 * Adds a new edge to this graph by connecting the given source and sink. If the source / sink vertex is not
		 * present, it will be added. Further, a label can optionally be specified for this new link.
		 *
		 * @param source the source of the new edge
		 * @param sink the sink of the new edge
		 * @param label the label of the new edge
		 * @return true if a new edge has been added, false otherwise. If the same edge has already been present, the old label will remain untouched.
		 */
		bool addEdge(const Vertex& source, const Vertex& sink, const EdgeLabel& label = EdgeLabel()) {
			// obtain the descriptors for the source / sink vertices
			auto src = addVertexInternal(source).first;
			auto trg = addVertexInternal(sink).first;
			return boost::add_edge(src, trg, label, graph).second;
		}

		/**
		 * Tests whether there is an edge connecting the given source and sink vertex.
		 *
		 * @param source the source of the edge to be searched
		 * @param sink the sink of the edge to be searched
		 * @return true if present, false otherwise
		 */
		bool containsEdge(const Vertex& source, const Vertex& sink) {
			// check presents of source / sink vertex an finally the edge
			return containsVertex(source) && containsVertex(sink) && boost::edge(getVertexDescriptorInternal(source), getVertexDescriptorInternal(sink), graph);
		}

		/**
		 * Obtains the total number of edges within this graph.
		 *
		 * @return the total number of edges
		 */
		std::size_t getNumEdges() const {
			return num_edges(graph);
		}

		/**
		 * Obtains the label associated to an edge between the given vertices.
		 *
		 * @param source the source-node of the requested edge
		 * @param sink the target-node of the requested edge
		 * @return an optional containing the label if present; if there is no such edge
		 * 		the result will be uninitialized
		 */
		boost::optional<EdgeLabel> getLabel(const Vertex& source, const Vertex& sink) {
			auto source_desc = getVertexDescriptorInternal(source);
			auto sink_desc = getVertexDescriptorInternal(sink);

			// look up edge
			if(!source_desc || !sink_desc) {
				// => no such edge
				return boost::optional<EdgeLabel>();
			}

			// obtain edge label
			auto edge = boost::edge(*source_desc, *sink_desc, graph);
			if(edge.second) { return boost::optional<EdgeLabel>(graph[edge.first]); }

			// no such edge
			return boost::optional<EdgeLabel>();
		}

		/**
		 * Prints a string-representation of this container to the given output stream.
		 *
		 * @see #printGraphViz(..) for a nicer presentation
		 */
		std::ostream& printTo(std::ostream& out) const {
			// define some utility types
			typedef typename VertexMapType::value_type VertexMapEntry;
			typedef typename std::iterator_traits<typename boost::graph_traits<GraphType>::edge_iterator>::value_type Edge;

			// just print a pair of node and edge sets
			out << "({";
			out << join(",", vertexMap, [](std::ostream& out, const VertexMapEntry& cur) -> std::ostream& { return out << cur.first; });
			out << "},{";
			out << join(",", boost::edges(graph), [this](std::ostream& out, const Edge& cur) -> std::ostream& {
				return out << "(" << this->graph[source(cur, this->graph)] << "," << this->graph[target(cur, this->graph)] << ")";
			});
			out << "})";
			return out;
		}

		/**
		 * A generic printer allowing to represent the given graph in the graphViz dot format.
		 *
		 * @tparam VertexPrinter a printer converting the vertex type to a string (default uses << operator)
		 * @tparam EdgePrinter a printer converting the edge labels to a strings (default uses << operator)
		 * @param out the stream to be printed to
		 * @param vertexPrinter the printer to be used for formating vertices
		 * @param vertexPrinter the printer to be used for formating edge labels
		 * @return the reference to the handed in output stream
		 */
		template <class VertexPrinter = default_label, class EdgePrinter = default_label, class VertexDecorator = default_deco,
		          class EdgeDecorator = default_deco>
		std::ostream& printGraphViz(std::ostream& out, const VertexPrinter& vertexPrinter = VertexPrinter(), const EdgePrinter& edgePrinter = EdgePrinter(),
		                            const VertexDecorator& vertexDeco = VertexDecorator(), const EdgeDecorator& edgeDeco = EdgeDecorator()) const {
			boost::write_graphviz(out, graph, label_printer<GraphType, VertexPrinter, VertexDecorator>(graph, vertexPrinter, vertexDeco),
			                      label_printer<GraphType, EdgePrinter, EdgeDecorator>(graph, edgePrinter, edgeDeco));
			return out;
		}


		// -- vertex iterator ------------------------------------------------------

		/**
		 * A vertex iterator implementation
		 */
		struct const_iterator : public std::iterator<std::forward_iterator_tag, Vertex> {
			typedef typename boost::graph_traits<GraphType>::vertex_iterator vertex_iter;

			const_iterator() : graph(nullptr), iter() {}
			const_iterator(const GraphType& graph, const vertex_iter& iter) : graph(&graph), iter(iter) {}

			bool operator==(const const_iterator& other) const {
				return graph == other.graph && iter == other.iter;
			}

			bool operator!=(const const_iterator& other) const {
				return !(*this == other);
			}

			const Vertex& operator*() const {
				return (*graph)[*iter];
			}

			const Vertex* operator->() const {
				return &(*graph)[*iter];
			}

			const_iterator& operator++() {
				++iter;
				return *this;
			}

		  private:
			const GraphType* graph;
			vertex_iter iter;
		};

		/**
		 * Obtains a const iterator referencing the first vertex.
		 */
		const_iterator vertexBegin() const {
			return const_iterator(graph, boost::vertices(graph).first);
		}

		/**
		 * Obtains a const iterator referencing the position after the last vertex.
		 */
		const_iterator vertexEnd() const {
			return const_iterator(graph, boost::vertices(graph).second);
		}


	  private:
		/**
		 * A method used internally for adding new vertices.
		 *
		 * @param vertex the vertex to be added
		 * @return a pair of the vertex_descriptor used inside the boost graph to represent this vertex and a boolean
		 * 			flag indicating whether this operation has altered the graph (hence, whether a new node has been added)
		 */
		std::pair<vertex_descriptor, bool> addVertexInternal(const Vertex& vertex) {
			// check whether vertex is already present
			auto pos = vertexMap.find(vertex);
			if(pos != vertexMap.end()) { return std::make_pair(pos->second, false); }

			// create and add new vertex
			vertex_descriptor&& v = boost::add_vertex(graph);

			// link vertex with descriptor (via vertex map)
			vertexMap.insert(std::make_pair(vertex, v));

			// link descriptor with vertex (via node property)
			graph[v] = vertex;

			// done
			return std::make_pair(v, true);
		}

		/**
		 * Obtains the boost-graph vertex descriptor used for representing the given vertex within
		 * the internally maintained boost graph.
		 *
		 * @param vertex the vertex looking for
		 * @return an optional containing the requested descriptor whenever present
		 */
		boost::optional<vertex_descriptor> getVertexDescriptorInternal(const Vertex& vertex) const {
			auto pos = vertexMap.find(vertex);
			if(pos != vertexMap.end()) { return boost::optional<vertex_descriptor>(pos->second); }

			// there is no such descriptor
			return boost::optional<vertex_descriptor>();
		}
	};


	/**
	 * A special graph version capable of handling pointer as vertices. Those pointers dereferenced before being compared. Hence,
	 * two pointers referencing two equivalent values within different memory locations will be considered identical.
	 *
	 * @tparam Vertex the type of vertex to be maintained within the graph - has to be a pointer type
	 * @tparam EdgeLabel the type of label to be used on the edges (unlabeled by default)
	 * @tparam DirectedS allows to determine the structure of the wrapped graph
	 */
	template <class Vertex, class EdgeLabel = unlabeled, class DirectedS = boost::directedS>
	class PointerGraph : public Graph<Vertex, EdgeLabel, DirectedS, map::PointerMap> {};


	/**
	 * A generic printer allowing to print a adjacency_list graph in the graphViz dot format. All the generic
	 * parameters should be automatically deduced from the arguments.
	 *
	 * @param out the stream to be printed to
	 * @param graph the graph to be printed
	 * @param vertexPrinter the printer to be used for formating vertices
	 * @param vertexPrinter the printer to be used for formating edge labels
	 * @return the reference to the handed in output stream
	 */
	template <class OutEdgeListS, class VertexListS, class DirectedS, class VertexProperty, class EdgeProperty, class GraphProperty, class EdgeListS,
	          class VertexPrinter = default_label, class EdgePrinter = default_label, class VertexDecorator = default_deco, class EdgeDecorator = default_deco>
	inline std::ostream&
	printGraphViz(std::ostream& out, boost::adjacency_list<OutEdgeListS, VertexListS, DirectedS, VertexProperty, EdgeProperty, GraphProperty, EdgeListS> graph,
	              const VertexPrinter& vertexPrinter = VertexPrinter(), const EdgePrinter& edgePrinter = EdgePrinter(),
	              const VertexDecorator& vertexDeco = VertexDecorator(), const EdgeDecorator& edgeDeco = EdgeDecorator()) {
		typedef boost::adjacency_list<OutEdgeListS, VertexListS, DirectedS, VertexProperty, EdgeProperty, GraphProperty, EdgeListS> GraphType;
		boost::write_graphviz(out, graph, label_printer<GraphType, VertexPrinter, VertexDecorator>(graph, vertexPrinter, vertexDeco),
		                      label_printer<GraphType, EdgePrinter, EdgeDecorator>(graph, edgePrinter, edgeDeco));
		return out;
	}

	namespace detail {

		/**
		 * A utility visitor for detecting cycles.
		 */
		template <typename VertexDecriptorType>
		struct cycle_detector : public boost::dfs_visitor<> {
			std::vector<VertexDecriptorType>& cycle;
			std::map<VertexDecriptorType, VertexDecriptorType> predecessors;

			cycle_detector(std::vector<VertexDecriptorType>& cycle) : cycle(cycle) {}

			template <typename Edge, typename Graph>
			void tree_edge(const Edge& e, const Graph& g) {
				predecessors[boost::target(e, g)] = boost::source(e, g); // record predecessor relation
			}

			template <typename Edge, typename Graph>
			void back_edge(const Edge& e, const Graph& g) {
				// check whether a cycle has already been found
				if(!cycle.empty()) { return; }

				// we have a back-edge => compute cycle
				auto start = boost::target(e, g);

				// restore cycle
				auto cur = boost::source(e, g);
				cycle.push_back(cur);
				while(cur != start) {
					cur = predecessors[cur];
					cycle.push_back(cur);
				}
			}
		};
	}

	/**
	 * Searches for a cycle in the given graph.
	 */
	template <typename GraphType, typename VertexType = typename boost::vertex_bundle_type<GraphType>::type,
	          typename VertexDecriptorType = typename boost::graph_traits<GraphType>::vertex_descriptor>
	std::vector<VertexType> detectCycle(const GraphType& graph) {
		std::vector<VertexDecriptorType> cycle;
		detail::cycle_detector<VertexDecriptorType> detector(cycle);

		boost::depth_first_search(graph, boost::visitor(detector));

		// extract result
		std::vector<VertexType> res;
		for(const auto& cur : cycle) {
			res.push_back(graph[cur]);
		}

		return res;
	}


	template <typename GraphType, typename VertexType = typename boost::vertex_bundle_type<GraphType>::type,
	          typename VertexDecriptorType = typename boost::graph_traits<GraphType>::vertex_descriptor>
	Graph<std::set<VertexType>> computeSCCGraph(const GraphType& graph) {
		auto numVertices = boost::num_vertices(graph);

		// no vertices?! just return empty graph
		if (!numVertices) {
			return Graph<std::set<VertexType>>();
		}

		// start by computing identifying the components using boost ...
		std::vector<typename GraphType::vertices_size_type> componentMap(numVertices);
		auto numComponents = boost::strong_components(graph, make_iterator_property_map(componentMap.begin(), get(boost::vertex_index, graph), componentMap[0]));

		// create sets forming equivalence classes (nodes of the resulting graph)
		std::vector<std::set<VertexType>> sets(numComponents);

		for(std::size_t i = 0; i < numVertices; i++) {
			// add type to corresponding set
			sets[componentMap[i]].insert(graph[i]);
		}
		
		// create resulting graph
		Graph<std::set<VertexType>> res;
		for(std::size_t i = 0; i < numComponents; i++) {
			res.addVertex(sets[i]);
		}

		// add edges between components
		// Therefore: iterate through all original edges and connect corresponding SCCs
		auto edges = boost::edges(graph);
		for(auto it = edges.first; it != edges.second; ++it) {
			auto cur = *it;

			// get components this edge is connection
			#pragma GCC diagnostic push
			#pragma GCC diagnostic ignored "-Wuninitialized"
			#if(__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7)
			#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
			#endif
			auto src = componentMap[source(cur, graph)];
			auto trg = componentMap[target(cur, graph)];
			#pragma GCC diagnostic pop

			// if edge is crossing component boarders ...
			if(src != trg) {
				// ... add an edge to the result
				res.addEdge(sets[src], sets[trg]);
			}
		}

		return res;
	}

	/**
	 * Computes a topological order of the nodes stored within the given boost graph.
	 */
	template <typename GraphType>
	std::vector<typename GraphType::vertex_descriptor> getTopologicalOrder(const GraphType& graph) {
		vector<typename GraphType::vertex_descriptor> order;
		try {
			// compute (reverse) topological order
			boost::topological_sort(graph, std::back_inserter(order));
		} catch(boost::not_a_dag e) { assert_fail() << "There should not be any cycles!"; }

		// reverse order and return result
		return reverse(order);
	}

	/**
	 * Computes a topological order of the vertices organized within the given graph.
	 */
	template <class Vertex, class EdgeLabel, template <class V, class D> class VertexMap>
	std::vector<Vertex> getTopologicalOrder(const Graph<Vertex, EdgeLabel, boost::bidirectionalS, VertexMap>& graph) {
		auto order = getTopologicalOrder(graph.asBoostGraph());
		std::vector<Vertex> res;
		for(const auto& cur : order) {
			res.push_back(graph.getVertexFromDescriptor(cur));
		}
		return res;
	}

} // end namespace graph
} // end namespace utils
} // end namespace insieme

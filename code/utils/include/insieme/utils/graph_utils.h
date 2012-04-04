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
#include <boost/graph/graph_traits.hpp>
#include <boost/graph/graphviz.hpp>

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
		std::ostream& printTo(std::ostream& out) const { return out; }
	};

	/**
	 * A type wrapper allowing unordered maps to be used to maintain vertices within a graph.
	 */
	template<class V, class D>
	class SimpleUnorderedMap : public boost::unordered_map<V, D> {};

	/**
	 * A functor class capable of printing labels within dot files for nodes and edges.
	 */
	template <class Graph, class Printer>
	class label_printer {
		Graph graph;
		const Printer& printer;
	public:
		label_printer(Graph _graph, const Printer& printer)
			: graph(_graph), printer(printer) {}

		template <class VertexOrEdge>
		void operator()(std::ostream& out, const VertexOrEdge& v) const {
			out << "[label=\"";
			printer(out, graph[v]);
			out << "\"]";
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
	template<
		class Vertex,
		class EdgeLabel = unlabeled,
		class DirectedS = boost::bidirectionalS,
		template<class V, class D> class VertexMap = SimpleUnorderedMap
	>
	class Graph : public Printable {

	public:

		// some member type definitions
		typedef Vertex VertexType;
		typedef EdgeLabel EdgeLabelType;

		// the type of underlying boost graph
		typedef typename boost::adjacency_list<boost::hash_setS, boost::vecS, DirectedS, Vertex, EdgeLabel> GraphType;

	private:

		// some private type definitions
		typedef typename boost::graph_traits<GraphType>::vertex_descriptor vertex_descriptor;
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
			return containsVertex(source) && containsVertex(sink) && boost::edge(getVertexDescriptor(source), getVertexDescriptor(sink), graph);
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

			auto source_desc = getVertexDescriptor(source);
			auto sink_desc = getVertexDescriptor(sink);

			// look up edge
			if (!source_desc || !sink_desc) {
				// => no such edge
				return boost::optional<EdgeLabel>();
			}

			// obtain edge label
			auto edge = boost::edge(*source_desc, *sink_desc, graph);
			if (edge.second) {
				return boost::optional<EdgeLabel>(graph[edge.first]);
			}

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
			out << join(",", vertexMap, [](std::ostream& out, const VertexMapEntry& cur)->std::ostream& { return out << cur.first; } );
			out << "},{";
			out << join(",", boost::edges(graph), [this](std::ostream& out, const Edge& cur)->std::ostream& {
 				return out << "(" << this->graph[source(cur,this->graph)] << "," << this->graph[target(cur, this->graph)] << ")";
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
		template<class VertexPrinter = print<id<Vertex>>, class EdgePrinter = print<id<EdgeLabel>>>
		std::ostream& printGraphViz(std::ostream& out, const VertexPrinter& vertexPrinter = VertexPrinter(), const EdgePrinter& edgePrinter = EdgePrinter()) const {
			boost::write_graphviz(out, graph,
					label_printer<GraphType, VertexPrinter>(graph, vertexPrinter),
					label_printer<GraphType, EdgePrinter>(graph, edgePrinter)
			);
			return out;
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
			if (pos != vertexMap.end()) {
				return std::make_pair(pos->second, false);
			}

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
		boost::optional<vertex_descriptor> getVertexDescriptor(const Vertex& vertex) {
			auto pos = vertexMap.find(vertex);
			if (pos != vertexMap.end()) {
				return boost::optional<vertex_descriptor>(pos->second);
			}

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
	template<class Vertex, class EdgeLabel = unlabeled, class DirectedS = boost::directedS>
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
	template<class OutEdgeListS,
			class VertexListS,
			class DirectedS,
			class VertexProperty,
			class EdgeProperty,
			class GraphProperty,
			class EdgeListS,
			class VertexPrinter = print<id<VertexProperty>>,
			class EdgePrinter = print<id<EdgeProperty>>>

	inline std::ostream& printGraphViz(std::ostream& out,
			boost::adjacency_list<OutEdgeListS, VertexListS, DirectedS, VertexProperty, EdgeProperty, GraphProperty, EdgeListS> graph,
			const VertexPrinter& vertexPrinter = VertexPrinter(), const EdgePrinter& edgePrinter = EdgePrinter()) {

		typedef boost::adjacency_list<OutEdgeListS, VertexListS, DirectedS, VertexProperty, EdgeProperty, GraphProperty, EdgeListS> GraphType;
		boost::write_graphviz(out, graph,
				label_printer<GraphType, VertexPrinter>(graph, vertexPrinter),
				label_printer<GraphType, EdgePrinter>(graph, edgePrinter)
		);
		return out;
	}

} // end namespace graph
} // end namespace utils
} // end namespace insieme

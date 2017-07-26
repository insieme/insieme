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
 */

#pragma once

#include <boost/graph/breadth_first_search.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_node.h"
#include "insieme/annotations/opencl/opencl_annotations.h"
#include "insieme/utils/graph_utils.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace analysis {

	using namespace annotations::opencl;
	/**
	 * Represents a dependencygraph within the context of opencl
	 */
	typedef utils::graph::PointerGraph<core::NodePtr, utils::graph::unlabeled, boost::directedS> DependencyGraph;
	/**
     * Returns true, iff the given node represents a primitive type
	 */
	bool isPrimitive(const core::NodePtr& node);
	bool isPrimitive(const core::TypePtr& type);
	/**
	 * Shortcut for eg. core::lang::PointerType(node).getElementType() & friends
	 */
	core::TypePtr getElementType(const core::TypePtr& type);
	/**
     * Returns the underlying type of a given node:
	 * eg: ref<ptr<array<int<4>>> yields int<4>
	 */
	core::TypePtr getUnderlyingType(const core::TypePtr& type);
	/**
	 * Removes any nested refs and ptrs until a non-indirect type is encountered.
	 */
	core::TypePtr getReferencedType(const core::NodePtr& node);
	core::TypePtr getReferencedType(const core::TypePtr& type);
	/**
	 * Tests whether the given variable is readOnly or not
	 */
	bool isReadOnly(const core::StatementPtr& stmt, const core::VariablePtr& var);
	/**
	 * Tests whether the given node is outline-able or not.
	 */
	bool isOffloadAble(core::NodeManager& manager, const core::NodePtr& node);
	/**
	 * Obtain all variable requirements for callExpr which must have been built
	 * using opencl::transform::outline.
	 */
	VariableRequirementList getVariableRequirements(core::NodeManager& manager, const core::NodePtr& code,
																		 const core::StatementPtr& stmt);
	/**
	 * Determine if the given requirements belong to the lambdaExpr and are fully defined by the latter
	 * @param lambdaExpr lambda to check the requirements against
	 * @param requirements list of requirements to check
	 */
	bool isVariableRequirementOf(const core::LambdaExprPtr& lambdaExpr, const VariableRequirementList& requirements);
	/**
	 * Determines all free variables within stmt by taking into account the given requirements
	 */
	core::VariableList getFreeVariables(core::NodeManager& manager, const core::StatementPtr& stmt,
										const VariableRequirementList& requirements);
	/**
	 * Determine if a given for statement is independent and thus suitable for parallelization
	 */
	bool isIndependentStmt(const core::StatementPtr& stmt);
	/**
	 * Determine all offloadable statements within a given node
	 */
	core::NodeList getOffloadAbleStmts(const core::NodePtr& node);
	/**
	 * Determine the LWDataItemType for a given lambda
	 */
	core::TypePtr getLWDataItemType(const core::LambdaExprPtr& lambdaExpr);
	/**
	 * Determine static device information for the given statement
	 */
	DeviceAnnotationPtr getDeviceInfo(const core::StatementPtr& stmt);

	DependencyGraph& getDependencyGraph(const core::StatementPtr& stmt, const core::VariableSet& vars, DependencyGraph& base);

	DependencyGraph& getDependencyGraph(const core::CallExprPtr& callExpr, core::NodeMap& mapping, DependencyGraph& base);

	namespace {
		template<typename Lambda>
		struct DependencyGraphVisitor : public boost::default_dfs_visitor {
			Lambda lambda;
			const DependencyGraph& graph;
		public:
			DependencyGraphVisitor(const DependencyGraph& graph, Lambda& lambda) :
				lambda(lambda), graph(graph)
			{ }

			template <typename Vertex, typename Graph>
			void initialize_vertex(const Vertex& v, const Graph& g) { }

			template <typename Vertex, typename Graph>
			void start_vertex(const Vertex& v, const Graph& g) { }

			template <typename Vertex, typename Graph>
			void discover_vertex(const Vertex& v, const Graph& g) {
				// at this point we can invoke the lambda
				lambda(graph.getVertexFromDescriptor(v));
			}

			template <typename Edge, typename Graph>
			void examine_edge(const Edge& e, const Graph& g) { }

			template <typename Edge, typename Graph>
			void tree_edge(const Edge& e, const Graph& g) { }

			template <typename Edge, typename Graph>
			void back_edge(const Edge& e, const Graph& g) { }

			template <typename Edge, typename Graph>
			void forward_or_cross_edge(const Edge& e, const Graph& g) { }

			template <typename Edge, typename Graph>
			void finish_edge(const Edge& e, const Graph& g) { }

			template <typename Vertex, typename Graph>
			void finish_vertex(const Vertex& v, const Graph& g) { }
		};
	}

	template<typename Lambda, typename Enable = typename std::enable_if<!std::is_polymorphic<Lambda>::value, void>::type>
	void visitDepthFirstOnce(const DependencyGraph& graph, Lambda lambda) {
		DependencyGraphVisitor<Lambda> visitor(graph, lambda);
		boost::depth_first_search(graph.asBoostGraph(), boost::visitor(visitor));
	}

	template <typename GraphType, typename VertexType = typename boost::vertex_bundle_type<GraphType>::type>
	std::vector<std::set<VertexType>> getTopologicalSets(const GraphType& graph) {
		auto N = boost::num_vertices(graph);
		typename boost::graph_traits<GraphType>::vertices_size_type d[N];
		std::fill(d, d+N, 0);

		// in case the ordering returns an empty set we are done already
		auto ordered = utils::graph::getTopologicalOrder(graph);
		if (ordered.empty())
			return {};

		// let boost compute the distances for each vertex for us
		boost::breadth_first_search(graph, ordered.front(), boost::visitor(
			boost::make_bfs_visitor(boost::record_distances(d, boost::on_tree_edge{}))));
		// allocate enough storage for all required sets
		std::vector<std::set<VertexType>> result(*std::max_element(d, d+N)+1);
		for (unsigned i = 0; i < result.capacity(); ++i)
			result[i] = std::set<VertexType>();
		// finally put everything into sets
		for (unsigned i = 0; i < N; ++i)
			result[d[i]].insert(graph[i]);
		return result;
	}
}
}
}
}

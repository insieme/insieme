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

#include "insieme/core/types/subtype_constraints.h"

#include <boost/graph/strong_components.hpp>
#include <boost/graph/topological_sort.hpp>

#include "insieme/core/ir_types.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/graph_utils.h"

namespace insieme {
namespace core {
namespace types {

	namespace {

		typedef utils::graph::PointerGraph<TypePtr> SubTypeGraph;

		typedef utils::graph::Graph<vector<TypePtr>> SubTypeEqualityGraph;


		/**
		 * Converts the given sub-type constraint graph into an equivalent graph where
		 * equivalent types are grouped together to equality classes. The nodes of
		 * the resulting type therefore contain sets of types.
		 */
		inline SubTypeEqualityGraph computeEqualityGraph(const SubTypeGraph& graph) {
			auto numTypes = graph.getNumVertices();

			// start by computing identifying the components using boost ...
			SubTypeGraph::GraphType::vertices_size_type componentMap[numTypes];
			auto numComponents = boost::strong_components(graph.asBoostGraph(), componentMap);

			// create type sets forming equivalence groups
			vector<vector<TypePtr>> sets(numComponents);

			const SubTypeGraph::GraphType& boostGraph = graph.asBoostGraph();
			for(std::size_t i = 0; i < numTypes; i++) {
				// add type to corresponding set
				sets[componentMap[i]].push_back(boostGraph[i]);
			}

			// create resulting graph
			SubTypeEqualityGraph res;
			for(std::size_t i = 0; i < numComponents; i++) {
				res.addVertex(sets[i]);
			}

			// add edges between components
			// Therefore: iterate through all original edges and connect corresponding SCCs
			auto edges = boost::edges(boostGraph);
			for(auto it = edges.first; it != edges.second; ++it) {
				auto cur = *it;

				// get components this edge is connection
				#pragma GCC diagnostic push
				#pragma GCC diagnostic ignored "-Wuninitialized"
				#if(__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7)
				#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
				#endif
				auto src = componentMap[source(cur, boostGraph)];
				auto trg = componentMap[target(cur, boostGraph)];
				#pragma GCC diagnostic pop

				// if edge is crossing component boarders ...
				if(src != trg) {
					// ... add an edge to the result
					res.addEdge(sets[src], sets[trg]);
				}
			}

			return res;
		}


		inline void reduceByUnifying(NodeManager& manager, vector<TypePtr>& cur, Substitution& res) {
			std::size_t size = cur.size();
			if(size <= 1) {
				// nothing to do left
				return;
			}

			// test whether all entries can be unified ...
			if(auto unifier = unifyAll(manager, cur)) {
				// => drop list and exchange with unified version
				TypePtr first = cur[0];
				cur.clear();
				cur.push_back(unifier->applyTo(first));

				// => add unifier to result
				res = Substitution::compose(manager, res, *unifier);
				return;
			}

			// conduct a pair-wise unification attempt
			bool changed = true;
			while(changed) {
				changed = false;

				// update size
				size = cur.size();
				if(size <= 1) { return; }

				// check each pair of entries
				for(std::size_t i = 0; !changed && i < size; i++) {
					for(std::size_t j = i + 1; !changed && j < size; j++) {
						if(auto unifier = unify(manager, cur[i], cur[j])) {
							// unify-able => reduce
							TypePtr replacement = unifier->applyTo(cur[i]);

							cur.erase(cur.begin() + j);
							cur.erase(cur.begin() + i);

							// add replacement
							cur.push_back(replacement);

							// apply unification to remaining elements
							for(std::size_t k = 0; k < size - 1; k++) {
								cur[i] = unifier->applyTo(cur[i]);
							}

							// combine with resulting substitution
							res = Substitution::compose(manager, res, *unifier);

							// mark as changed (exit for loops => restart pairwise search)
							changed = true;
						}
					}
				}
			}
		}


		inline vector<SubTypeEqualityGraph::GraphType::vertex_descriptor> getTopologicalOrder(const SubTypeEqualityGraph::GraphType& graph) {
			vector<SubTypeEqualityGraph::GraphType::vertex_descriptor> order;
			try {
				// compute (reverse) topological order
				boost::topological_sort(graph, std::back_inserter(order));
			} catch(const boost::not_a_dag& e) { assert_fail() << "There should not be any cycles!"; }

			// reverse order and return result
			return reverse(order);
		}

		inline void applyUnifierToAllNodes(SubTypeEqualityGraph::GraphType& graph, SubstitutionOpt unifier) {
			if(!unifier) { return; }

			typedef SubTypeEqualityGraph::GraphType::vertex_iterator vertex_iterator;
			vertex_iterator begin;
			vertex_iterator end;
			boost::tie(begin, end) = boost::vertices(graph);
			for(auto it = begin; it != end; ++it) {
				// obtain reference to set
				vector<TypePtr>& cur = graph[*it];

				std::size_t size = cur.size();
				for(std::size_t i = 0; i < size; i++) {
					cur[i] = unifier->applyTo(cur[i]);
				}
			}
		}

		SubstitutionOpt isUnsignedToSignedConversion(const TypePtr& argType, const TypePtr& paramType) {
			SubstitutionOpt fail;

			// convert to generic types
			GenericTypePtr arg = argType.isa<GenericTypePtr>();
			GenericTypePtr param = paramType.isa<GenericTypePtr>();

			// both must be generic types
			if (!arg || !param) return fail;

			// only for unsigned to signed integer conversion
			if (arg->getFamilyName() != "uint" || param->getFamilyName() != "int") return fail;

			// the parameter must be variadic
			auto var = param.getTypeParameter(0).isa<TypeVariablePtr>();
			if (!var) return fail;

			// fix the size one size larger than the input
			auto& basic = arg.getNodeManager().getLangBasic();
			if (basic.isUInt1(arg)) return Substitution(var,basic.getInt2().as<GenericTypePtr>().getTypeParameter(0));
			if (basic.isUInt2(arg)) return Substitution(var,basic.getInt4().as<GenericTypePtr>().getTypeParameter(0));
			if (basic.isUInt4(arg)) return Substitution(var,basic.getInt8().as<GenericTypePtr>().getTypeParameter(0));
			if (basic.isUInt8(arg)) return Substitution(var,basic.getIntInf().as<GenericTypePtr>().getTypeParameter(0));
			if (basic.isUIntInf(arg)) return Substitution(var,basic.getIntInf().as<GenericTypePtr>().getTypeParameter(0));

			// otherwise => problem
			return fail;
		}

	} // end namespace

	SubstitutionOpt SubTypeConstraints::solve() const {
		if(!isSatisfiable()) { return boost::none; }

		if(!constraints.empty()) { return solve(constraints.begin()->first->getNodeManager()); }

		// otherwise, return an empty substitution map
		return Substitution();
	}


	SubstitutionOpt SubTypeConstraints::solve(NodeManager& manager) const {
		const bool debug = false;

		if(debug) { std::cout << std::endl << "--------------- Start -------------------" << std::endl; }
		if(debug) { std::cout << "Constraints: " << *this << std::endl; }

		// quick check to exclude unsatisfiability
		if(!isSatisfiable()) { return boost::none; }

		// create result
		Substitution res;

		// quick-solution for an empty constraint set
		if(constraints.empty()) { return res; }

		// --------------------------------- The constraint solving algorithm --------------------
		//
		//    Step 1) all the constraints are converted into a sub-type graph
		//			  			- each node within the graph is a type
		//						- an edge from A -> B demands that A is a subtype of B
		//
		//    Step 2) Compute equality classes within sub-type graph
		//						- every SCC (strongly connected component) is such a class
		//						- result: a DAG where each node is a set of types, edges are sub-type constraints
		//
		//    Step 3) Compute represent for each equaltiy class
		//						- by unifying the entries (pairwise) => Substitutions are part of result
		//						- if unifyable => use unified type as represent
		//						- if not, reduce as far as possible (smallest set of non-unifyable types)
		//							 - check whether types are pairwise equivalent ( => if not, fail)
		//							 - if so, pick on as a represent
		//						=> result: a DAG where every node is only one type
		//
		//    Step 4) Resolve constraints within graph in topological order (starting at leafs, working upwards)
		//						- collect all sub-types and compute smallest common super-type (first unify, then SCST computation)
		//						- try unify with type of current node
		//								if successful, add unifier to result
		//								else, check sub-type constraint. On fail => constraints unsatisfiable
		//
		//			  Extension for non-inhabitated types:
		//						- if sub-types are empty and current type is a variable:
		//							- compute all super-types and compute biggest common sub-type of thoes (unify first, than BCST)
		//							- try unify with type of current node
		//								if successful, add unifier to result
		//								else, check sub-type constraint. On fail => constraints unsatisfiable
		//
		// -------------------------------------------------------------------------------------

		// step 1) form a sub-type graph
		//		- types & variables are nodes
		//		- edges indicate sub-type constraints (A -> B ... A has to be a sub-type of B)


		// add edges - one edge for each sub-type constraint
		SubTypeGraph subTypeGraph;
		for_each(constraints, [&](const Constraint& cur) { subTypeGraph.addEdge(cur.first, cur.second); });

		// print some debugging information
		if(debug) { std::cout << "Subtype graph: " << std::endl; }
		if(debug) { subTypeGraph.printGraphViz(std::cout, print<deref<TypePtr>>()); }


		// step 2) compute SCCs within subtype graph
		SubTypeEqualityGraph sccGraph = computeEqualityGraph(subTypeGraph);

		if(debug) { std::cout << "Subtype Equality Graph: " << std::endl; }
		if(debug) { sccGraph.printGraphViz(std::cout); }


		// step 3) unify sets attached to nodes within sccGraph => pick one represent
		typedef SubTypeEqualityGraph::GraphType GraphType;
		typedef GraphType::vertex_descriptor vertex_descriptor;
		typedef GraphType::vertex_iterator vertex_iterator;

		SubTypeEqualityGraph::GraphType graph = sccGraph.asBoostGraph();

		vertex_iterator begin;
		vertex_iterator end;
		boost::tie(begin, end) = boost::vertices(graph);
		for(auto it = begin; it != end; ++it) {
			// obtain reference to set
			vector<TypePtr>& cur = graph[*it];

			// apply current unification step
			for(std::size_t i = 0; i < cur.size(); i++) {
				cur[i] = res.applyTo(cur[i]);
			}

			if(debug) { std::cout << "    Before reduction: " << cur << std::endl; }

			// try reducing set to 1 element using unification
			reduceByUnifying(manager, cur, res);

			if(debug) { std::cout << "    After reduction: " << cur << std::endl; }

			// if there are more than 2 elements ...
			std::size_t size = cur.size();
			if(size == 1) { continue; }

			// those have to be sub-types of each other
			for(std::size_t i = 0; i < size; i++) {
				for(std::size_t j = i + 1; j < size; j++) {
					if(!isSubTypeOf(cur[i], cur[j]) || !isSubTypeOf(cur[j], cur[i])) {
						// equality constraint violated => no solution
						if(debug) { std::cout << "WARNING: equality constraint violated within " << cur << std::endl; }
						return boost::none;
					}
				}
			}

			// OK, all are equivalent => pick only one
			TypePtr represent = cur[0];
			cur.clear();
			cur.push_back(represent);
		}

		if(debug) { std::cout << "Represent Graph: " << std::endl; }
		if(debug) { printGraphViz(std::cout, graph); }


		// step 4) resolve sub-type constraints - bottom up in topological order
		vector<vertex_descriptor>&& order = getTopologicalOrder(graph);

		// resolve constraints per node
		std::size_t size = order.size();
		for(std::size_t i = 0; i < size; i++) {
			// get types of current node
			vertex_descriptor vertex = order[i];
			vector<TypePtr>& curList = graph[vertex];
			assert_eq(curList.size(), 1) << "Graph-Vertices are not supposed to contain more than 1 type!";

			TypePtr cur = curList[0];

			if(debug) { std::cout << "    Processing " << cur << std::endl; }
			if(debug) { std::cout << "      Current res: " << res << std::endl; }

			// obtain super-types
			vector<TypePtr> subTypes;

			GraphType::in_edge_iterator begin;
			GraphType::in_edge_iterator end;
			boost::tie(begin, end) = boost::in_edges(vertex, graph);
			for(auto it = begin; it != end; ++it) {
				vector<TypePtr>& sub = graph[source(*it, graph)];
				assert_eq(sub.size(), 1) << "Graph-Vertices are not supposed to contain more than 1 type!";
				subTypes.push_back(sub[0]);
			}

			if(debug) { std::cout << "       Sub-Types: " << subTypes << std::endl; }

			// skip rest if there are no super types
			if(!subTypes.empty()) {
				// reduce sub-types using unification
				reduceByUnifying(manager, subTypes, res);

				// obtain limit => by compute smallest common super type
				TypePtr limit = getSmallestCommonSuperType(subTypes);
				if(debug) { std::cout << "            Limit: " << limit << std::endl; }

				if(!limit) {
					if(debug) { std::cout << "No common super type for " << subTypes << std::endl; }
					return boost::none;
				}

				// super types could be unified => try the same with current
				if(auto unifier = unify(manager, cur, limit)) {
					// fine => use unified version
					applyUnifierToAllNodes(graph, unifier);

					// add to resulting unifier
					res = Substitution::compose(manager, res, *unifier);
					continue;

				// handle special case of unsigned-to-signed integer conversion
				} else if (auto unifier = isUnsignedToSignedConversion(limit,cur)) {
					// fine => use unified version
					applyUnifierToAllNodes(graph, unifier);

					// add to resulting unifier
					res = Substitution::compose(manager, res, *unifier);
					continue;

				} else {
					// check whether current type is a sub-type of the limit
					if(!isSubTypeOf(limit, cur)) {
						// => no solution
						if(debug) { std::cout << "The type " << cur << " is definitely not a sub-type of " << limit << std::endl; }
						return boost::none;
					}
				}

			} else if(cur->getNodeType() == NT_TypeVariable) {
				// this extension is only necessary to support also non-inhabitated types

				// so, there are no sub-types => loop for super-types
				vector<TypePtr> superTypes;

				GraphType::out_edge_iterator begin;
				GraphType::out_edge_iterator end;
				boost::tie(begin, end) = boost::out_edges(vertex, graph);
				for(auto it = begin; it != end; ++it) {
					vector<TypePtr>& super = graph[target(*it, graph)];
					assert_eq(super.size(), 1) << "Graph-Vertices are not supposed to contain more than 1 type!";
					superTypes.push_back(super[0]);
				}

				if(debug) { std::cout << "     Super-Types: " << superTypes << std::endl; }

				if(!superTypes.empty()) {
					// reduce super-types using unification
					reduceByUnifying(manager, superTypes, res);

					// obtain limit => by compute smallest common super type
					TypePtr limit = getBiggestCommonSubType(superTypes);
					if(debug) { std::cout << "            Limit: " << limit << std::endl; }

					if(!limit) {
						if(debug) { std::cout << "No common sub type for " << superTypes << std::endl; }
						return boost::none;
					}

					// super types could be unified => try the same with current
					if(auto unifier = unify(manager, cur, limit)) {
						// fine => use unified version
						applyUnifierToAllNodes(graph, unifier);

						// add to resulting unifier
						res = Substitution::compose(manager, res, *unifier);
						continue;
					} else {
						// check whether current type is a sub-type of the limit
						if(!isSubTypeOf(cur, limit)) {
							// => no solution
							if(debug) { std::cout << "The type " << limit << " is not a sub-type of " << cur << std::endl; }
							return boost::none;
						}
					}
				}
			}
		}

		if(debug) { std::cout << "Substitution so far: " << res << std::endl; }
		if(debug) { std::cout << std::endl; }
		return res;
	}


	std::ostream& SubTypeConstraints::printTo(std::ostream& out) const {
		auto constraintList = join(",", constraints, [](std::ostream& out, const Constraint& cur) { out << *(cur.first) << "<:" << *(cur.second); });

		if(unsatisfiable) { return out << "[unsatisfiable / " << constraintList << "]"; }
		return out << "[" << constraintList << "]";
	}

} // end namespace types
} // end namespace core
} // end namespace insieme

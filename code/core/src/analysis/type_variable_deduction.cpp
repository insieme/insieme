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

#include "insieme/core/analysis/type_variable_deduction.h"

#include <iterator>

#include <boost/graph/strong_components.hpp>
#include <boost/graph/topological_sort.hpp>

#include "insieme/core/analysis/type_variable_renamer.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/annotation.h"
#include "insieme/core/expressions.h"

#include "insieme/utils/graph_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace analysis {


	TypePtr TypeVariableConstraints::VariableConstraint::solve() const {
		TypePtr res;

		// try infer type from sub-type constraints
		if (!subTypes.empty()) {
			// compute biggest common sub-type
			res = getBiggestCommonSubType(subTypes);

			// check against super-type constraints
			bool valid = res;
			for_each(superTypes, [&](const TypePtr& cur) {
				// check super-type constraint
				valid = valid && isSubTypeOf(cur, res);
			});

			// if all constraints are satisfied ...
			if (valid) {
				return res;
			}

			// => unsatisfiable
			return 0;
		}

		// infer type from super-type constraints
		if (!superTypes.empty()) {
			// compute smallest common super type
			res = getSmallestCommonSuperType(superTypes);
		}

		// done
		return res;

	}


	std::ostream& TypeVariableConstraints::VariableConstraint::printTo(std::ostream& out) const {
		return out << "[ >= " << superTypes << " && <= " << subTypes << " ]";
	}


	namespace {

		typedef utils::graph::PointerGraph<TypePtr> SubTypeGraph;

		typedef utils::graph::Graph<vector<TypePtr>> SubTypeEqualityGraph;


		/**
		 * Converts the given sub-type constraint graph into an equivalent graph where
		 * equivalent types are grouped together to equality classes. The nodes of
		 * the resulting type therefore contain sets of types.
		 */
		inline SubTypeEqualityGraph computeSCCGraph(const SubTypeGraph& graph) {

			auto numTypes = graph.getNumVertices();

			// start by computing identifying the components using boost ...
			SubTypeGraph::GraphType::vertices_size_type componentMap[numTypes];
			auto numComponents = boost::strong_components(graph.asBoostGraph(), componentMap);

			// create type sets forming equivalence groups
			vector<TypePtr> sets[numComponents];

			const SubTypeGraph::GraphType& boostGraph = graph.asBoostGraph();
			for (std::size_t i=0; i<numTypes; i++) {
				// add type to corresponding set
				sets[componentMap[i]].push_back(boostGraph[i]);
			}

			// create resulting graph
			SubTypeEqualityGraph res;
			for (std::size_t i=0; i<numComponents; i++) {
				res.addVertex(sets[i]);
			}

			// add edges between components
			// Therefore: iterate through all original edges and connect corresponding SCCs
			auto edges = boost::edges(boostGraph);
			for(auto it = edges.first; it != edges.second; ++it) {
				auto cur = *it;

				// get components this edge is connection
				auto src = componentMap[source(cur, boostGraph)];
				auto trg = componentMap[target(cur, boostGraph)];

				// if edge is crossing component boarders ...
				if (src != trg) {
					// ... add an edge to the result
					res.addEdge(sets[src], sets[trg]);
				}
			}

			return res;
		}


		inline void reduceByUnifying(NodeManager& manager, vector<TypePtr>& cur, Substitution& res) {

			std::size_t size = cur.size();
			if (size <= 1) {
				// nothing to do left
				return;
			}

			// test whether all entries can be unified ...
			if (auto unifier = unifyAll(manager, cur)) {

//				std::cout << " Was capable of directly unify " << cur << " using " << *unifier << std::endl;

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
			while (changed) {
				changed = false;

				// update size
				size = cur.size();
				if (size <= 1) {
					return;
				}

				// check each pair of entries
				for (std::size_t i=0; !changed && i<size; i++) {
					for (std::size_t j=i+1; !changed && j<size; j++) {
						if (auto unifier = unify(manager, cur[i], cur[j])) {

							// unify-able => reduce
							TypePtr replacement = unifier->applyTo(cur[i]);

							// remove cur[i] and cur[j] (j first, since j > i)

//							std::cout << "Erasing " << *(cur.begin() + j) << std::endl;
							cur.erase(cur.begin() + j);
//							std::cout << "Erasing " << *(cur.begin() + i) << std::endl;
							cur.erase(cur.begin() + i);

							// add replacement
							cur.push_back(replacement);

							// apply unification to remaining elements
							for (std::size_t k=0; k<size-1; k++) {
								cur[i] = unifier->applyTo(cur[i]);
							}

							// combine with resulting substitution
							res = Substitution::compose(manager, res, *unifier);

							// mark as changed (exit for loops => restart pairwise search)
							changed=true;
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
			} catch(boost::not_a_dag e) {
				assert(0 && "There should not be any cycles!");
			}

			// reverse order and return result
			return reverse(order);
		}

		inline void applyUnifierToAllNodes(SubTypeEqualityGraph::GraphType& graph, SubstitutionOpt unifier) {
			if(!unifier) {
				return;
			}

			typedef SubTypeEqualityGraph::GraphType::vertex_iterator vertex_iterator;
			vertex_iterator begin;
			vertex_iterator end;
			boost::tie(begin, end) = boost::vertices(graph);
			for (auto it = begin; it!=end; ++it) {
				// obtain reference to set
				vector<TypePtr>& cur = graph[*it];

				std::size_t size = cur.size();
				for (std::size_t i=0; i<size; i++) {
					cur[i] = unifier->applyTo(cur[i]);
				}
			}
		}

	}

	SubstitutionOpt TypeVariableConstraints::solve() const {
		if (!isSatisfiable()) {
			return 0;
		}

		if (!constraints.empty()) {
			return solve(constraints.begin()->first->getNodeManager());
		}

		Substitution res;
		for_each(intTypeParameter, [&](const std::pair<VariableIntTypeParamPtr, IntTypeParamPtr>& cur) {
			res.addMapping(cur.first, cur.second);
		});
		return res;
	}


	SubstitutionOpt TypeVariableConstraints::solve(NodeManager& manager) const {

		const bool debug = false;

		if (debug) std::cout << std::endl << "--------------- Start -------------------" << std::endl;
		if (debug) std::cout << "Constraints: " << *this << std::endl;

		// quick check to exclude unsatisfiability
		if (!isSatisfiable()) {
			return 0;
		}

		// create result
		Substitution res;

		// initialize substitutions
		for_each(intTypeParameter, [&](const std::pair<VariableIntTypeParamPtr, IntTypeParamPtr>& cur) {
			res.addMapping(cur.first, cur.second);
		});

		// quick-solution for an empty constraint set
		if (constraints.empty()) {
			return res;
		}

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


		// add edges
		SubTypeGraph subTypeGraph;
		for(auto it = constraints.begin(); it != constraints.end(); ++it) {

			const TypeVariablePtr& var = it->first;
			const TypeVariableConstraints::VariableConstraint& cur = it->second;

			// add sub-type constraints
			for_each(cur.subTypes, [&](const TypePtr& cur) {
				subTypeGraph.addEdge(var, res.applyTo(cur));
			});

			// add super-type constraints
			for_each(cur.superTypes, [&](const TypePtr& cur) {
				subTypeGraph.addEdge(res.applyTo(cur), var);
			});
		}

		// print some debugging information
		if (debug) std::cout << "Subtype graph: " << std::endl;
		if (debug) subTypeGraph.printGraphViz(std::cout, print<deref<TypePtr>>());


		// step 2) compute SCCs within subtype graph
		SubTypeEqualityGraph sccGraph = computeSCCGraph(subTypeGraph);

		if (debug) std::cout << "Subtype Equality Graph: " << std::endl;
		if (debug) sccGraph.printGraphViz(std::cout);


		// step 3) unify sets attached to nodes within sccGraph => pick one represent
		typedef SubTypeEqualityGraph::GraphType GraphType;
		typedef GraphType::vertex_descriptor vertex_descriptor;
		typedef GraphType::vertex_iterator vertex_iterator;

		SubTypeEqualityGraph::GraphType graph = sccGraph.asBoostGraph();

		vertex_iterator begin;
		vertex_iterator end;
		boost::tie(begin, end) = boost::vertices(graph);
		for (auto it = begin; it!=end; ++it) {
			// obtain reference to set
			vector<TypePtr>& cur = graph[*it];

			// apply current unification step
			for (std::size_t i=0; i<cur.size(); i++) {
				cur[i] = res.applyTo(cur[i]);
			}

			if (debug) std::cout << "    Before reduction: " << cur << std::endl;

			// try reducing set to 1 element using unification
			reduceByUnifying(manager, cur, res);

			if (debug) std::cout << "    After reduction: " << cur << std::endl;

			// if there are more than 2 elements ...
			std::size_t size = cur.size();
			if (size == 1) {
				continue;
			}

			// those have to be sub-types of each other
			for (std::size_t i=0; i<size; i++) {
				for (std::size_t j=i+1; j<size; j++) {
					if (!isSubTypeOf(cur[i], cur[j]) || !isSubTypeOf(cur[j], cur[i])) {
						// equality constraint violated => no solution
						if (debug) std::cout << "WARNING: equality constraint violated within " << cur << std::endl;
						return 0;
					}
				}
			}

			// OK, all are equivalent => pick only one
			TypePtr represent = cur[0];
			cur.clear();
			cur.push_back(represent);
		}

		if (debug) std::cout << "Represent Graph: " << std::endl;
		if (debug) printGraphViz(std::cout, graph);


		// step 4) resolve sub-type constraints - bottom up in topological order
		vector<vertex_descriptor>&& order = getTopologicalOrder(graph);

		// resolve constraints per node
		std::size_t size = order.size();
		for (std::size_t i=0; i<size; i++) {

			// get types of current node
			vertex_descriptor vertex = order[i];
			vector<TypePtr>& curList = graph[vertex];
			assert(curList.size() == 1 && "Graph-Vertices are not supposed to contain more than 1 type!");

			TypePtr cur = curList[0];

			if (debug) std::cout << "    Processing " << cur << std::endl;
			if (debug) std::cout << "      Current res: " << res << std::endl;

			// obtain super-types
			vector<TypePtr> subTypes;

			GraphType::in_edge_iterator begin;
			GraphType::in_edge_iterator end;
			boost::tie(begin, end) = boost::in_edges(vertex, graph);
			for (auto it = begin; it!=end; ++it) {
				vector<TypePtr>& sub = graph[source(*it, graph)];
				assert(sub.size() == 1 && "Graph-Vertices are not supposed to contain more than 1 type!");
				subTypes.push_back(sub[0]);
			}

			if (debug) std::cout << "       Sub-Types: " << subTypes << std::endl;

			// skip rest if there are no super types
			if (!subTypes.empty()) {

				// reduce sub-types using unification
				reduceByUnifying(manager, subTypes, res);

				// obtain limit => by compute smallest common super type
				TypePtr limit = getSmallestCommonSuperType(subTypes);
				if (debug) std::cout << "            Limit: " << limit << std::endl;

				if (!limit) {
					if (debug) std::cout << "No common super type for " << subTypes << std::endl;
					return 0;
				}

				// super types could be unified => try the same with current
				if (auto unifier = unify(manager, cur, limit)) {

					// fine => use unified version
					applyUnifierToAllNodes(graph, unifier);

					// add to resulting unifier
					res = Substitution::compose(manager, res, *unifier);
					continue;
				} else {

					// check whether current type is a sub-type of the limit
					if (!isSubTypeOf(limit, cur)) {
						// => no solution
						if (debug) std::cout << "The type " << cur << " is not a sub-type of " << limit << std::endl;
						return 0;
					}

				}

			} else if (cur->getNodeType() == NT_TypeVariable) {
				// this extension is only necessary to support also non-inhabitated types

				// so, there are no sub-types => loop for super-types
				vector<TypePtr> superTypes;

				GraphType::out_edge_iterator begin;
				GraphType::out_edge_iterator end;
				boost::tie(begin, end) = boost::out_edges(vertex, graph);
				for (auto it = begin; it!=end; ++it) {
					vector<TypePtr>& super = graph[target(*it, graph)];
					assert(super.size() == 1 && "Graph-Vertices are not supposed to contain more than 1 type!");
					superTypes.push_back(super[0]);
				}

				if (debug) std::cout << "     Super-Types: " << superTypes << std::endl;

				if (!superTypes.empty()) {

					// reduce super-types using unification
					reduceByUnifying(manager, superTypes, res);

					// obtain limit => by compute smallest common super type
					TypePtr limit = getBiggestCommonSubType(superTypes);
					if (debug) std::cout << "            Limit: " << limit << std::endl;

					if (!limit) {
						if (debug) std::cout << "No common sub type for " << superTypes << std::endl;
						return 0;
					}

					// super types could be unified => try the same with current
					if (auto unifier = unify(manager, cur, limit)) {

						// fine => use unified version
						applyUnifierToAllNodes(graph, unifier);

						// add to resulting unifier
						res = Substitution::compose(manager, res, *unifier);
						continue;
					} else {

						// check whether current type is a sub-type of the limit
						if (!isSubTypeOf(cur, limit)) {
							// => no solution
							if (debug) std::cout << "The type " << limit << " is not a sub-type of " << cur << std::endl;
							return 0;
						}

					}
				}
			}
		}

		if (debug) std::cout << "Substitution so far: " << res << std::endl;
		if (debug) std::cout << std::endl;
		return res;
	}


	std::ostream& TypeVariableConstraints::printTo(std::ostream& out) const {
		if (unsatisfiable) {
			return out << "[unsatisfiable / " << constraints << "/" << intTypeParameter << "]";
		}
		return out << "[" << constraints << "/" << intTypeParameter << "]";
	}


	namespace {

		// An enumeration of the constraint directions
		enum Direction {
			SUB_TYPE = 0, SUPER_TYPE = 1
		};

		/**
		 * A utility function to inverse constrain types. This function will turn a sub-type constrain
		 * into a super-type constraint and vica verce.
		 */
		inline static Direction inverse(Direction type) {
			return (Direction)(1 - type);
		}

		/**
		 *  Adds the necessary constraints on the instantiation of the type variables used within the given type parameter.
		 *
		 *  @param constraints the set of constraints to be extendeded
		 *  @param paramType the type on the parameter side (function side)
		 *  @param argType the type on the argument side (argument passed by call expression)
		 */
		void addEqualityConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType);

		/**
		 * Adds additional constraints to the given constraint collection such that the type variables used within the
		 * parameter type are limited to values representing super- / sub-types of the given argument type.
		 *
		 *  @param constraints the set of constraints to be extended
		 *  @param paramType the type on the parameter side (function side)
		 *  @param argType the type on the argument side (argument passed by call expression)
		 *  @param direction the direction to be ensured - sub- or supertype
		 */
		void addTypeConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType, Direction direction);


		// -------------------------------------------------------- Implementation ----------------------------------------------


		void addEqualityConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType) {

			// check constraint status
			if (!constraints.isSatisfiable()) {
				return;
			}

			// extract node types
			NodeType nodeTypeA = paramType->getNodeType();
			NodeType nodeTypeB = argType->getNodeType();

			// handle variables
			if(nodeTypeA == NT_TypeVariable) {
				// the substitution for the variable has to be equivalent to the argument type
				constraints.addEqualsConstraint(static_pointer_cast<const TypeVariable>(paramType), argType);
				return;
			}

			// for all other types => the node type has to be the same
			if (nodeTypeA != nodeTypeB) {
				// not satisfiable in any way
				constraints.makeUnsatisfiable();
				return;
			}


			// so, the type is the same => distinguish the node type

			// check node types
			switch(nodeTypeA) {

				// first handle those types equipped with int type parameters
				case NT_GenericType:
				case NT_RefType:
				case NT_ArrayType:
				case NT_VectorType:
				case NT_ChannelType:
				{
					// check name of generic type ... if not matching => wrong
					auto genParamType = static_pointer_cast<const GenericType>(paramType);
					auto genArgType = static_pointer_cast<const GenericType>(argType);
					if (genParamType->getFamilyName() != genArgType->getFamilyName()) {
						// those types cannot be equivalent if they are part of a different family
						constraints.makeUnsatisfiable();
						return;
					}

					// check the int-type parameter ...
					auto param = genParamType->getIntTypeParameter();
					auto args = genArgType->getIntTypeParameter();

					// quick-check on size
					if (param.size() != args.size()) {
						constraints.makeUnsatisfiable();
						return;
					}

					// match int-type parameters individually
					for (auto it = make_paired_iterator(param.begin(), args.begin()); it != make_paired_iterator(param.end(), args.end()); ++it) {
						// check constraints state
						if (!constraints.isSatisfiable()) {
							return;
						}
						auto paramA = (*it).first;
						auto paramB = (*it).second;
						if (paramA->getNodeType() == NT_VariableIntTypeParam) {
							// obtain variable
							VariableIntTypeParamPtr var = static_pointer_cast<const VariableIntTypeParam>(paramA);

							// check if the parameter has already been set
							if (IntTypeParamPtr value = constraints.getIntTypeParamValue(var)) {
								if (*value == *paramB) {
									// everything is fine
									continue;
								} else {
									// int type parameter needs to be instantiated twice, in different ways => unsatisfiable
									constraints.makeUnsatisfiable();
									return;
								}
							}

							// fix a new value for the parameter
							constraints.fixIntTypeParameter(var, paramB);
						} else {
							// the two parameters have to be the same!
							if (*paramA != *paramB) {
								// unable to satisfy constraints
								constraints.makeUnsatisfiable();
								return;
							}
						}
					}

					// ... and fall-through to check the child types
				}

				case NT_TupleType:
				case NT_FunctionType:
				{
					// the number of sub-types must match
					auto paramChildren = paramType->getChildList();
					auto argChildren = argType->getChildList();

					// check number of sub-types
					if (paramChildren.size() != argChildren.size()) {
						constraints.makeUnsatisfiable();
						return;
					}

					// check all child nodes
					for (auto it = make_paired_iterator(paramChildren.begin(), argChildren.begin());
							it != make_paired_iterator(paramChildren.end(), argChildren.end()); ++it) {

						// filter int-type parameter
						if ((*it).first->getNodeCategory() == NC_Type) {
							// add equality constraints recursively
							addEqualityConstraints(constraints,
									static_pointer_cast<const Type>((*it).first),
									static_pointer_cast<const Type>((*it).second)
							);
						}
					}

					break;
				}

				case NT_StructType:
				case NT_UnionType:
				{
					// names and sub-types have to be checked
					auto entriesA = static_pointer_cast<const NamedCompositeType>(paramType)->getEntries();
					auto entriesB = static_pointer_cast<const NamedCompositeType>(argType)->getEntries();

					// check equality of names and types
					// check number of entries
					if (entriesA.size() != entriesB.size()) {
						constraints.makeUnsatisfiable();
						return;
					}

					// check all child nodes
					for (auto it = make_paired_iterator(entriesA.begin(), entriesB.begin());
							it != make_paired_iterator(entriesA.end(), entriesB.end()); ++it) {

						// ensure identifiers are identical (those cannot be variable)
						auto entryA = (*it).first;
						auto entryB = (*it).second;
						if (*entryA.first != *entryB.first) {
							// unsatisfiable
							constraints.makeUnsatisfiable();
							return;
						}

						// add equality constraints recursively
						addEqualityConstraints(constraints, entryA.second, entryB.second);
					}


					break;
				}

				case NT_RecType:
				{
					// TODO: implement RecType pattern matching
					if (*paramType != *argType) {
						LOG(WARNING) << "Yet unhandled recursive type encountered while resolving subtype constraints:"
									 << " Parameter Type: " << paramType << std::endl
									 << "  Argument Type: " << argType << std::endl
									 << " => the argument will be considered equal!!!";
//						assert(false && "Sorry - not implemented!");
						constraints.makeUnsatisfiable();
					}
					break;
				}
				default:
					assert(false && "Missed a kind of type!");
			}
		}

		void addTypeConstraints(TypeVariableConstraints& constraints, const TypePtr& paramType, const TypePtr& argType, Direction direction) {

			// check constraint status
			if (!constraints.isSatisfiable()) {
				return;
			}

			// check whether relation is already satisfied
			if ((direction == Direction::SUB_TYPE && isSubTypeOf(paramType, argType)) ||
					(direction == Direction::SUPER_TYPE && isSubTypeOf(argType, paramType))) {
				return;
			}

			// extract node types
			NodeType nodeTypeA = paramType->getNodeType();
			NodeType nodeTypeB = argType->getNodeType();

			// handle variables
			if(nodeTypeA == NT_TypeVariable) {
				// add corresponding constraint to this variable
				if (direction == Direction::SUB_TYPE) {
					constraints.addSubtypeConstraint(static_pointer_cast<const TypeVariable>(paramType), argType);
				} else {
					constraints.addSupertypeConstraint(static_pointer_cast<const TypeVariable>(paramType), argType);
				}
				return;
			}


			// ---------------------------------- Arrays / Vectors Type ---------------------------------------------

			// check direction and array/vector relationship
			if ((direction == Direction::SUB_TYPE && nodeTypeA == NT_VectorType && nodeTypeB == NT_ArrayType) ||
					(direction == Direction::SUPER_TYPE && nodeTypeA == NT_ArrayType && nodeTypeB == NT_VectorType)) {

				bool isSubType = (direction == Direction::SUB_TYPE);
				const VectorTypePtr& vector = static_pointer_cast<const VectorType>((isSubType)?paramType:argType);
				const ArrayTypePtr& array = static_pointer_cast<const ArrayType>((isSubType)?argType:paramType);

				// make sure the dimension of the array is 1
				auto dim = array->getDimension();
				ConcreteIntTypeParamPtr one = ConcreteIntTypeParam::get(array->getNodeManager(), 1);
				switch(dim->getNodeType()) {
					case NT_VariableIntTypeParam:
						// this is fine ... no restrictions required
						if (!isSubType) constraints.fixIntTypeParameter(static_pointer_cast<const VariableIntTypeParam>(dim), one);
						break;
					case NT_ConcreteIntTypeParam:
					case NT_InfiniteIntTypeParam:
						if (*dim != *one) {
							// only dimension 1 is allowed when passing a vector
							constraints.makeUnsatisfiable();
							return;
						}
					default:
						assert(false && "Unknown int-type parameter encountered!");
				}

				// also make sure element types are equivalent
				if (isSubType) {
					addEqualityConstraints(constraints, array->getElementType(), vector->getElementType());
				} else {
					addEqualityConstraints(constraints, vector->getElementType(), array->getElementType());
				}
				return;

			}

			// --------------------------------------------- Function Type ---------------------------------------------

			// check function type
			if (nodeTypeA == NT_FunctionType) {
				// argument has to be a function as well
				if (nodeTypeB != NT_FunctionType) {
					// => no match possible
					constraints.makeUnsatisfiable();
					return;
				}

				auto funParamType = static_pointer_cast<const FunctionType>(paramType);
				auto funArgType = static_pointer_cast<const FunctionType>(argType);

				// check number of arguments
				auto paramArgs = funParamType->getArgumentTypes();
				auto argArgs = funArgType->getArgumentTypes();
				if (paramArgs.size() != argArgs.size()) {
					// different number of arguments => unsatisfiable
					constraints.makeUnsatisfiable();
					return;
				}

				// add constraints on arguments
				auto begin = make_paired_iterator(paramArgs.begin(), argArgs.begin());
				auto end = make_paired_iterator(paramArgs.end(), argArgs.end());
				for (auto it = begin; constraints.isSatisfiable() && it != end; ++it) {
					addTypeConstraints(constraints, it->first, it->second, inverse(direction));
				}

				// ... and the return type
				addTypeConstraints(constraints, funParamType->getReturnType(), funArgType->getReturnType(), direction);
				return;
			}

			// check rest => has to be equivalent (e.g. ref, channels, ...)
			addEqualityConstraints(constraints, paramType, argType);
		}



		inline TypeMapping substituteFreeVariablesWithConstants(NodeManager& manager, const TypeList& arguments) {

			TypeMapping argumentMapping;

			// realized using a recursive lambda visitor
			ASTVisitor<>* rec;
			auto collector = makeLambdaPtrVisitor([&](const NodePtr& cur){
				NodeType kind = cur->getNodeType();
				switch(kind) {
				case NT_TypeVariable: {
					// check whether already encountered
					const TypeVariablePtr& var = static_pointer_cast<const TypeVariable>(cur);
					if (argumentMapping.containsMappingFor(var)) {
						break;
					}
					// add a new constant
					const TypePtr substitute = GenericType::get(manager, "_const_" + var->getVarName());
					argumentMapping.addMapping(var, substitute);
					break;
				}
				case NT_VariableIntTypeParam: {
					// check whether already encountered
					const VariableIntTypeParamPtr& var = static_pointer_cast<const VariableIntTypeParam>(cur);
					if (argumentMapping.containsMappingFor(var)) {
						break;
					}
					// add a new constant
					// NOTE: the generation of constants is not safe in all cases - it is just assumed
					// that no constants > 1.000.000.000 are used
					auto substitute = ConcreteIntTypeParam::get(manager, 1000000000 + var->getSymbol());
					argumentMapping.addMapping(var, substitute);
					break;
				}
				case NT_RecType:
				case NT_FunctionType: {
					// do not consider function types and recursive types (variables inside are bound)
					break;
				}
				default: {
					// decent recursively
					for_each(cur->getChildList(), [&](const NodePtr& cur) {
						rec->visit(cur);
					});
				}
				}
			}, true);
			rec = &collector;

			// finally, collect and substitute variables
			collector.visit(TupleType::get(manager, arguments));

			return argumentMapping;
		}


	}


	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypeList& parameter, const TypeList& arguments) {

		const bool debug = false;

		// check length of parameter and arguments
		if (parameter.size() != arguments.size()) {
			return 0;
		}

		// use private node manager for computing results
		NodeManager internalManager;

		// ---------------------------------- Variable Renaming -----------------------------------------
		//
		// The variables have to be renamed to avoid collisions within type variables used within the
		// function type and variables used within the arguments. Further, variables within function
		// types of the arguments need to be renamed independently (to avoid illegal capturing)
		//
		// ----------------------------------------------------------------------------------------------


		// Vor the renaming a variable renamer is used
		VariableRenamer renamer;

		// 1) convert parameters (consistently, all at once)
		TypeMapping parameterMapping = renamer.mapVariables(TupleType::get(internalManager, parameter));
		TypeList renamedParameter = parameterMapping.applyForward(internalManager, parameter);

		if (debug) std::cout << " Parameter: " << parameter << std::endl;
		if (debug) std::cout << "   Renamed: " << renamedParameter << std::endl;

		// 2) convert arguments (individually)
		//		- fix free variables consistently => replace with constants
		//		- rename unbounded variables - individually per parameter

		// collects the mapping of free variables to constant replacements (to protect them
		// from being substituted during some unification process)
		TypeMapping&& argumentMapping = substituteFreeVariablesWithConstants(internalManager, arguments);

		if (debug) std::cout << " Arguments: " << arguments << std::endl;
		if (debug) std::cout << "   Mapping: " << argumentMapping << std::endl;

		// apply renaming to arguments
		TypeList renamedArguments = arguments;
		for (std::size_t i = 0; i < renamedArguments.size(); ++i) {
			TypePtr& cur = renamedArguments[i];

			// first: apply bound variable substitution
			cur = argumentMapping.applyForward(internalManager, cur);

			// second: apply variable renameing
			cur = renamer.rename(internalManager, cur);
		}

		if (debug) std::cout << " Renamed Arguments: " << renamedArguments << std::endl;



		// ---------------------------------- Assembling Constraints -----------------------------------------

		// collect constraints on the type variables used within the parameter types
		TypeVariableConstraints constraints;

		// collect constraints
		auto begin = make_paired_iterator(renamedParameter.begin(), renamedArguments.begin());
		auto end = make_paired_iterator(renamedParameter.end(), renamedArguments.end());
		for (auto it = begin; constraints.isSatisfiable() && it!=end; ++it) {
			// add constraints to ensure current parameter is a super-type of the arguments
			addTypeConstraints(constraints, it->first, it->second, Direction::SUPER_TYPE);
		}


		// ---------------------------------- Solve Constraints -----------------------------------------

		// solve constraints to obtain results
		SubstitutionOpt&& res = constraints.solve(internalManager);
		if (!res) {
			// if unsolvable => return this information
			if (debug) std::cout << " Terminated with no solution!" << std::endl << std::endl;
			return res;
		}

		// ----------------------------------- Revert Renaming ------------------------------------------
		// (and produce a result within the correct node manager - internal manager will be thrown away)

		// check for empty solution (to avoid unnecessary operations
		if (res->empty()) {
			if (debug) std::cout << " Terminated with: " << *res << std::endl << std::endl;
			return res;
		}

		// reverse variables from previously selected constant replacements (and bring back to correct node manager)
		Substitution restored;
		for (auto it = res->getMapping().begin(); it != res->getMapping().end(); ++it) {
			TypeVariablePtr var = static_pointer_cast<const TypeVariable>(parameterMapping.applyBackward(manager, it->first));
			TypePtr substitute = argumentMapping.applyBackward(manager, it->second);
			restored.addMapping(manager.get(var), manager.get(substitute));
		}
		for (auto it = res->getIntTypeParamMapping().begin(); it != res->getIntTypeParamMapping().end(); ++it) {
			VariableIntTypeParamPtr var = static_pointer_cast<const VariableIntTypeParam>(parameterMapping.applyBackward(manager, it->first));
			IntTypeParamPtr substitute = argumentMapping.applyBackward(manager, it->second);
			restored.addMapping(manager.get(var), manager.get(substitute));
		}
		if (debug) std::cout << " Terminated with: " << restored << std::endl << std::endl;
		return restored;
	}



	namespace {

		// The kind of annotations attached to call nodes to cache type variable substitutions
		class TypeVariableInstantionInfo : public Annotation {

		public:

			// The key used to attack instantiation results to call nodes
			static StringKey<TypeVariableInstantionInfo> KEY;

		private:

			// the represented value
			SubstitutionOpt substitution;

		public :

			TypeVariableInstantionInfo(const SubstitutionOpt& substitution) : substitution(substitution) {}

			virtual const AnnotationKey* getKey() const {
				return &KEY;
			}

			virtual const std::string getAnnotationName() const {
				return "TypeVariableInstantionInfo";
			}

			const SubstitutionOpt& getSubstitution() const {
				return substitution;
			}

		};

		StringKey<TypeVariableInstantionInfo> TypeVariableInstantionInfo::KEY = StringKey<TypeVariableInstantionInfo>("TYPE_VARIABLE_INSTANTIATION_INFO");
	}

	SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const CallExprPtr& call) {

		// check for null
		if (!call) {
			return 0;
		}

		// check annotations
		if (auto data = call->getAnnotation(TypeVariableInstantionInfo::KEY)) {
			return data->getSubstitution();
		}

		// derive substitution

		// get argument types
		TypeList argTypes;
		::transform(call->getArguments(), back_inserter(argTypes), [](const ExpressionPtr& cur) {
			return cur->getType();
		});

		// get function parameter types
		const TypePtr& funType = call->getFunctionExpr()->getType();
		if (funType->getNodeType() != NT_FunctionType) {
			return 0;
		}
		const TypeList& paramTypes = static_pointer_cast<const FunctionType>(funType)->getArgumentTypes();

		// compute type variable instantiation
		SubstitutionOpt res = getTypeVariableInstantiation(manager, paramTypes, argTypes);

		// attack substitution
		call->addAnnotation(std::make_shared<TypeVariableInstantionInfo>(res));

		// done
		return res;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

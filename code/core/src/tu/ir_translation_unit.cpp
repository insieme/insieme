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
#include "insieme/core/tu/ir_translation_unit.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/graph_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/frontend_ir_builder.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statistic.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/utils/graph_utils.h"

namespace insieme {
namespace core {
namespace tu {

	void IRTranslationUnit::addGlobal(const Global& newGlobal) {
		assert_true(newGlobal.first && core::analysis::isRefType(newGlobal.first->getType()));

		auto git = std::find_if(globals.begin(), globals.end(), [&](const Global& cur) -> bool { return *newGlobal.first == *cur.first; });
		if(git == globals.end()) {
			// global is new >> insert into globalsList
			globals.push_back(Global(mgr->get(newGlobal.first), mgr->get(newGlobal.second)));
		} else {
			// global is already in globalsList, if the "new one" has a initValue update the init
			if(newGlobal.second) { git->second = newGlobal.second; }
		}
	}

	std::ostream& IRTranslationUnit::printTo(std::ostream& out) const {
		static auto print = [](const core::NodePtr& node) { return toString(node); };
		return out << "TU(\n\tTypes:\n\t\t"
			       << join("\n\t\t", types,
			               [&](std::ostream& out, const std::pair<core::GenericTypePtr, core::TypePtr>& cur) { out << *cur.first << " => " << *cur.second; })
			       << ",\n\tGlobals:\n\t\t" << join("\n\t\t", globals,
			                                        [&](std::ostream& out, const std::pair<core::LiteralPtr, core::ExpressionPtr>& cur) {
			                                            out << *cur.first << ":" << *cur.first->getType() << " => ";
			                                            if(cur.second) {
				                                            out << print(cur.second);
			                                            } else {
				                                            out << "<uninitalized>";
			                                            }
				                                    })
			       << ",\n\tInitializer:\n\t\t" << join("\n\t\t", initializer, [&](std::ostream& out, const core::StatementPtr& cur) { out << print(cur); })
			       << ",\n\tFunctions:\n\t\t" << join("\n\t\t", functions,
			                                          [&](std::ostream& out, const std::pair<core::LiteralPtr, core::ExpressionPtr>& cur) {
				                                          out << *cur.first << " : " << *cur.first->getType() << " => " << print(cur.second);
				                                      })
			       << ",\n\tEntry Points:\n\t\t{" << join(", ", entryPoints, [&](std::ostream& out, const core::LiteralPtr& cur) { out << *cur; }) << "}"
			       << ",\n\tis C++:\n\t\t" << isCXX() << "\n)";
	}

	IRTranslationUnit IRTranslationUnit::toManager(core::NodeManager& manager) const {
		IRTranslationUnit res(manager);

		// copy types
		for(auto cur : getTypes()) {
			res.addType(cur.first, cur.second);
		}

		// copy functions
		for(auto cur : getFunctions()) {
			res.addFunction(cur.first, cur.second);
		}

		// copy globals
		for(auto cur : getGlobals()) {
			res.addGlobal(cur);
		}

		// copy initalizer
		for(auto cur : getInitializer()) {
			res.addInitializer(cur);
		}

		// entry points
		for(auto cur : getEntryPoints()) {
			res.addEntryPoints(cur);
		}

		// done
		return res;
	}

	IRTranslationUnit merge(core::NodeManager& mgr, const IRTranslationUnit& a, const IRTranslationUnit& b) {
		IRTranslationUnit res = a.toManager(mgr);

		// copy types
		for(auto cur : b.getTypes()) {
			if(!res[cur.first]) { res.addType(cur.first, cur.second); }
		}

		// copy functions
		for(auto cur : b.getFunctions()) {
			if(!res[cur.first]) { res.addFunction(cur.first, cur.second); }
		}

		// copy globals
		for(auto cur : b.getGlobals()) {
			res.addGlobal(cur);
		}

		// copy initializer
		for(auto cur : b.getInitializer()) {
			res.addInitializer(cur);
		}

		// entry points
		for(auto cur : b.getEntryPoints()) {
			res.addEntryPoints(cur);
		}

		// done
		return res;
	}

	IRTranslationUnit merge(core::NodeManager& mgr, const vector<IRTranslationUnit>& units) {
		IRTranslationUnit res(mgr);
		for(const auto& cur : units) {
			res = merge(mgr, res, cur);
		}
		res.setCXX(any(units, [](const IRTranslationUnit& cur) { return cur.isCXX(); }));
		return res;
	}


	// ------------------------------- to-program conversion ----------------------------


	namespace {

		using namespace core;

		/**
		 * The class converting a IR-translation-unit into an actual IR program by
		 * realizing recursive definitions.
		 */
		class Resolver : private core::SimpleNodeMapping {
			typedef utils::graph::PointerGraph<NodePtr> SymbolDependencyGraph;
			typedef utils::graph::Graph<std::set<NodePtr>> RecComponentGraph;

			NodeManager& mgr;

			IRBuilder builder;

			NodeMap symbolMap;

		  public:
			Resolver(NodeManager& mgr, const IRTranslationUnit& unit) : mgr(mgr), builder(mgr), symbolMap() {

				VLOG(3) << "#####################################################\n" << unit << "########################################\n";

				// copy type symbols into symbol table
				for(auto cur : unit.getTypes()) {
					symbolMap[mgr.get(cur.first)] = mgr.get(cur.second);
				}

				// copy function symbols into symbol table
				for(auto cur : unit.getFunctions()) {
					symbolMap[mgr.get(cur.first)] = mgr.get(cur.second);
				}
			}

			template <typename T>
			Pointer<T> apply(const Pointer<T>& ptr) {
				return apply(NodePtr(ptr)).as<Pointer<T>>();
			}

			NodePtr apply(const NodePtr& node) {
				if (!node) return node;

				// 1. get set of contained symbols
				const NodeSet& init = getContainedSymbols(node);

				// 2. build dependency graph
				auto depGraph = getDependencyGraph(init);

				// 3. compute SCCs graph
				auto components = computeSCCGraph(depGraph);

				// 4. resolve SCCs components bottom up
				resolveComponents(components);

				// 5. resolve input
				return map(node);
			}

		  private:
			// --- Utilities ---

			bool isSymbol(const NodePtr& node) const {
				return symbolMap.find(node) != symbolMap.end();
			}

			NodePtr getDefinition(const NodePtr& symbol) const {
				assert_true(isSymbol(symbol));
				return symbolMap.find(symbol)->second;
			}

			// --- Step 1: Symbol extraction ---

			mutable utils::map::PointerMap<NodePtr, bool> containsSymbolsCache;

			bool containsSymbols(const NodePtr& node) const {
				// handle null-pointers
				if (!node) return false;

				// check cache
				auto pos = containsSymbolsCache.find(node);
				if(pos != containsSymbolsCache.end()) { return pos->second; }

				// compute result recursively
				bool res = isSymbol(node) || any(node->getChildList(), [&](const NodePtr& cur) { return containsSymbols(cur); });

				// cache and return result
				return containsSymbolsCache[node] = res;
			}

			mutable utils::map::PointerMap<NodePtr, NodeSet> containedSymbols;

			const NodeSet& getContainedSymbols(const NodePtr& node) const {
				// if the node is known to not contain symbols => done
				static const NodeSet empty;
				if(!containsSymbols(node)) { return empty; }

				// first check cache
				auto pos = containedSymbols.find(node);
				if(pos != containedSymbols.end()) { return pos->second; }

				// collect contained symbols
				NodeSet res;
				core::visitDepthFirstOncePrunable(node, [&](const NodePtr& cur) {
					if(isSymbol(cur)) { res.insert(cur); }
					return !containsSymbols(cur);
				}, true);

				// add result to cache and return it
				return containedSymbols[node] = res;
			}

			// --- Step 2: Dependency Graph ---

			SymbolDependencyGraph getDependencyGraph(const NodeSet& seed) const {
				SymbolDependencyGraph res;

				NodeList open;
				for(const auto& cur : seed) {
					if(!isResolved(cur)) { open.push_back(cur); }
				}

				NodeSet processed;
				while(!open.empty()) {
					// get current element
					NodePtr cur = open.back();
					open.pop_back();

					// skip elements already processed
					if(processed.contains(cur)) { continue; }
					processed.insert(cur);

					// skip resolved symbols
					if(isResolved(cur)) { continue; }

					// add vertex (even if there is no edge)
					res.addVertex(cur);

					// add dependencies to graph
					for(const auto& other : getContainedSymbols(getDefinition(cur))) {
						// skip already resolved nodes
						if(isResolved(other)) { continue; }

						// add dependency to other
						res.addEdge(cur, other);

						// add other to list of open nodes
						open.push_back(other);
					}
				}

				// done
				return res;
			}

			// --- Step 3: SCC computation ---

			RecComponentGraph computeSCCGraph(const SymbolDependencyGraph& graph) {
				// there is a utility for that
				return utils::graph::computeSCCGraph(graph.asBoostGraph());
			}

			// --- Step 4: Component resolution ---

			std::map<NodePtr, NodePtr> resolutionCache;
			bool cachingEnabled = true;

			bool isResolved(const NodePtr& symbol) const {
				assert_true(isSymbol(symbol)) << "Should only be queried for symbols!";
				return resolutionCache.find(symbol) != resolutionCache.end();
			}

			NodePtr getRecVar(const NodePtr& symbol) {
				assert_true(isSymbol(symbol));

				// create a fresh recursive variable
				if(const GenericTypePtr& type = symbol.isa<GenericTypePtr>()) {
					return builder.tagTypeReference(type->getFamilyName());
				} else if(const LiteralPtr& fun = symbol.isa<LiteralPtr>()) {
					return builder.lambdaReference(map(fun->getType().as<FunctionTypePtr>()), fun->getValue());
				}

				// otherwise fail!
				assert_fail() << "Unsupported symbol encountered!";
				return symbol;
			}

			bool isDirectRecursive(const NodePtr& symbol) {
				return getContainedSymbols(getDefinition(symbol)).contains(symbol);
			}

			void resolveComponents(const RecComponentGraph& graph) {
				// compute topological order -- there is a utility for this too
				auto components = utils::graph::getTopologicalOrder(graph);

				// reverse (since dependencies have to be resolved bottom up)
				components = reverse(components);

				// process in reverse order
				for(const auto& cur : components) {

					// sort variables
					vector<NodePtr> vars(cur.begin(), cur.end());

					// get first element
					auto first = vars.front();

					// skip this component if it has already been resolved as a side effect of another operation
					if(isResolved(first)) { continue; }

					// split recursive variables in expressions and types
					vector<NodePtr> typeVars;
					vector<NodePtr> exprVars;
					for(const auto& cur : vars) {
						if (auto type = cur.isa<GenericTypePtr>()) {
							typeVars.push_back(type);
						} else {
							exprVars.push_back(cur.as<LiteralPtr>());
						}
					}

					// add variables for current components to resolution Cache
					cachingEnabled = false;					// no caching for this part
					for(const auto& s : typeVars) {			// first type variables
						resolutionCache[s] = getRecVar(s);
					}
					for(const auto& s : exprVars) {			// then expression variables
						resolutionCache[s] = getRecVar(s);
					}
					cachingEnabled = true;


					assert_true(isResolved(first));

					// process expr variables first, then type variables
					for(const auto& vars : { exprVars, typeVars } ) {

						// skip empty parts
						if (vars.empty()) continue;

						// get first element
						auto first = vars.front();

						// resolve definitions (without caching temporal values)
						cachingEnabled = false;

						std::map<NodePtr, NodePtr> resolved;
						for(const auto& s : vars) {
							resolved[s] = map(getDefinition(s));
						}

						// re-enable caching again
						cachingEnabled = true;

						// remember the tag-type definition for post-processing
						TagTypeDefinitionPtr tagTypeDefinition;

						// close recursion if necessary
						if(vars.size() > 1 || isDirectRecursive(first)) {

							// close recursive types
							if(first.isa<GenericTypePtr>()) {
								// build recursive type definition
								TagTypeBindingMap bindings;
								for(const auto& cur : vars) {
									bindings.insert({ resolutionCache[cur].as<TagTypeReferencePtr>(), resolved[cur].as<TagTypePtr>()->getRecord() });
								}

								// build recursive type definition
								auto def = builder.tagTypeDefinition(bindings);

								// de-compose type definition
								auto types = analysis::minimizeRecursiveGroup(def);

								// simply construct a recursive type
								for (auto& cur : resolved) {
									auto newType = types[resolutionCache[cur.first].as<TagTypeReferencePtr>()];
									core::transform::utils::migrateAnnotations(cur.second, newType);
									cur.second = newType;
								}

								// remember definition
								tagTypeDefinition = def;

							} else if(first.isa<LiteralPtr>()) {
								// build recursive lambda definition
								LambdaBindingMap bindings;
								for(const auto& cur : vars) {
									bindings.insert({ resolutionCache[cur].as<LambdaReferencePtr>(), resolved[cur].as<LambdaExprPtr>()->getLambda() });
								}

								// build recursive type definition
								auto def = builder.lambdaDefinition(bindings);

								// de-compose lambda definition
								auto funs = analysis::minimizeRecursiveGroup(def);

								// simply construct a recursive function
								for(auto& cur : resolved) {
									auto newFun = funs[resolutionCache[cur.first].as<LambdaReferencePtr>()];
									core::transform::utils::migrateAnnotations(cur.second, newFun);
									cur.second = newFun;
								}

							} else {
								assert_fail() << "Unsupported Symbol encountered: " << first << " - " << first.getNodeType() << "\n";
							}
						}

						// replace bindings with resolved definitions
						for(const auto& s : vars) {
							// migrate annotations
							core::transform::utils::migrateAnnotations(s, resolved[s]);

							// and add to cache
							resolutionCache[s] = resolved[s];
						}

						// peel expressions out of their types if necessary
						if (tagTypeDefinition) {

							// peel all the members so that they can be used in the global context
							for(const auto& cur : exprVars) {
								resolutionCache[cur] = tagTypeDefinition->peel(mgr, resolutionCache[cur].as<ExpressionPtr>());
							}

						}

					}
				}
			}

			virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
				// check cache first
				auto pos = resolutionCache.find(ptr);
				if(pos != resolutionCache.end()) { return pos->second; }

				// compute resolved type recursively
				auto res = ptr->substitute(mgr, *this);

				// Cleanups:
				if(res != ptr) {
					// special service: get rid of unnecessary casts (which might be introduced due to opaque generic types)
					if(const CastExprPtr& cast = res.isa<CastExprPtr>()) {
						// check whether cast can be skipped
						if(types::isSubTypeOf(cast->getSubExpression()->getType(), cast->getType())) { res = cast->getSubExpression(); }
					}

					// also fix type literals
					if(core::analysis::isTypeLiteral(res)) { res = builder.getTypeLiteral(core::analysis::getRepresentedType(res.as<ExpressionPtr>())); }
				}

				// if nothing has changed ..
				if(ptr == res) {
					// .. the result can always be cached (even if caching is disabled)
					return resolutionCache[ptr] = res;
				}

				// migrate annotations
				core::transform::utils::migrateAnnotations(ptr, res);

				// do not save result in cache if caching is disabled
				if(!cachingEnabled) {
					return res; // this is for temporaries
				}

				// done
				return resolutionCache[ptr] = res;
			}
		};


		core::LambdaExprPtr addGlobalsInitialization(const IRTranslationUnit& unit, const core::LambdaExprPtr& mainFunc, Resolver& resolver) {
			core::LambdaExprPtr internalMainFunc = mainFunc;

			// we only want to init what we use, so we check it
			core::NodeSet usedLiterals;
			core::visitDepthFirstOnce(internalMainFunc, [&](const core::LiteralPtr& literal) { usedLiterals.insert(literal); });

			// we check if the global var is used as initializer for a global var inserted in the previous step
			core::NodeSet prevAddedLiterals = usedLiterals;
			core::NodeSet currAddedLiterals;
			while(!prevAddedLiterals.empty()) {
				for(auto cur : unit.getGlobals()) {
					if(contains(prevAddedLiterals, cur.first)) {
						core::visitDepthFirstOnce(cur.second, [&](const core::LiteralPtr& literal) {
							if(core::analysis::isRefType(literal->getType())) {
								if(contains(usedLiterals, literal)) {
									return;
								}
								currAddedLiterals.insert(literal);
								usedLiterals.insert(literal);
							}
						});
					}
				}
				prevAddedLiterals = currAddedLiterals;
				currAddedLiterals.clear();
			};

			core::FrontendIRBuilder builder(internalMainFunc->getNodeManager());
			core::StatementList inits;

			// ~~~~~~~~~~~~~~~~~~ INITIALIZE GLOBALS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			core::NodeMap replacements;
			for(auto cur : unit.getGlobals()) {
				// only consider having an initialization value
				if(!cur.second) { continue; }

				core::LiteralPtr newLit = resolver.apply(cur.first);
				if(!contains(usedLiterals, newLit)) { continue; }

				// check if the initialization of any literal specifies the array type more accurately than the literal (e.g. inf -> fixed size)
				// if so, replace the literal type
				auto globalRefT = core::analysis::getReferencedType(newLit);
				auto lit = newLit;
				auto initT = cur.second->getType();
				// if we init an array using an initExpr the type is wrapped in a reference and we need to unwrap it
				if(core::analysis::isRefType(cur.second) && core::lang::isArray(core::analysis::getReferencedType(cur.second))) {
					initT = core::analysis::getReferencedType(cur.second);
				}
				if(core::lang::isArray(globalRefT) && core::lang::isArray(initT)) {
					auto litArrT = core::lang::ArrayType(globalRefT);
					auto initArrT = core::lang::ArrayType(initT);
					if(litArrT.isUnknownSize() && !initArrT.isUnknownSize()) {
						// get the literal
						auto rT = core::lang::ReferenceType(newLit);
						auto replacement =
							resolver.apply(builder.literal(newLit->getStringValue(), core::lang::ReferenceType::create((GenericTypePtr)initArrT, rT.isConst(),
							                                                                                           rT.isVolatile(), rT.getKind())));
						// add to replacement list
						replacements.insert({newLit, replacement});
						lit = replacement;
					}
				}

				// translate init exprs and constructor calls directly, generate init calls for scalars
				// also correctly handle dereffed values and calls to basic.getZero
				auto initExp = resolver.apply(cur.second);
				if(core::analysis::isCallOf(initExp, initExp.getNodeManager().getLangBasic().getZero())) {
					initExp = builder.getZero(core::analysis::getRepresentedType(core::analysis::getArgument(initExp, 0)));
				}
				if(initExp.getNodeManager().getLangExtension<lang::ReferenceExtension>().isCallOfRefDeref(initExp)) {
					initExp = core::analysis::getArgument(initExp, 0);
				}
				if(core::analysis::isConstructorCall(initExp) || initExp.isa<core::InitExprPtr>()) {
					inits.push_back(initExp);
				} else { // else build init expr
					inits.push_back(builder.initExpr(lit, initExp));
				}
			}

			// ~~~~~~~~~~~~~~~~~~ PREPARE STATICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			const lang::StaticVariableExtension& ext = internalMainFunc->getNodeManager().getLangExtension<lang::StaticVariableExtension>();
			for(auto cur : usedLiterals) {
				auto lit = cur.as<LiteralPtr>();
				// only consider static variables
				auto type = lit->getType();
				if(!core::analysis::isRefType(type) || !ext.isStaticType(core::analysis::getReferencedType(type))) { continue; }
				// add creation statement
				inits.push_back(builder.createStaticVariable(lit));
			}

			// build resulting lambda
			internalMainFunc = core::transform::insert(internalMainFunc->getNodeManager(), core::LambdaExprAddress(internalMainFunc)->getBody(), inits, 0)
			    .as<core::LambdaExprPtr>();
			internalMainFunc = transform::replaceAllGen(internalMainFunc->getNodeManager(), internalMainFunc, replacements, core::transform::globalReplacement);

			return internalMainFunc;
		}

		core::LambdaExprPtr addInitializer(const IRTranslationUnit& unit, const core::LambdaExprPtr& mainFunc) {
			// check whether there are any initializer expressions
			if(unit.getInitializer().empty()) { return mainFunc; }

			// insert init statements
			auto initStmts = ::transform(unit.getInitializer(), [](const ExpressionPtr& cur) -> StatementPtr { return cur; });
			return core::transform::insert(mainFunc->getNodeManager(), core::LambdaExprAddress(mainFunc)->getBody(), initStmts, 0).as<core::LambdaExprPtr>();
		}

	} // end anonymous namespace


	core::NodePtr IRTranslationUnit::resolve(const core::NodePtr& node) const {
		return Resolver(getNodeManager(), *this).apply(node);
	}


	core::ProgramPtr toProgram(core::NodeManager& mgr, const IRTranslationUnit& a, const string& entryPoint) {
		// search for entry point
		Resolver resolver(mgr, a);
		core::IRBuilder builder(mgr);
		for(auto cur : a.getFunctions()) {
			if(cur.first->getStringValue() == entryPoint || insieme::utils::demangle(cur.first->getStringValue()) == entryPoint) {
				// get the symbol
				core::NodePtr symbol = cur.first;
				//				std::cout << "Starting resolving symbol " << symbol << " ...\n";

				// extract lambda expression
				core::LambdaExprPtr lambda = resolver.apply(symbol).as<core::LambdaExprPtr>();

				// add initializer
				//				std::cout << "Adding initializers ...\n";
				lambda = addInitializer(a, lambda);

				// add global initializers
				//				std::cout << "Adding globals ...\n";
				lambda = addGlobalsInitialization(a, lambda, resolver);

				// wrap into program
				return builder.program(toVector<core::ExpressionPtr>(lambda));
			}
		}

		assert_fail() << "No such entry point!\n"
		              << "Searching for: " << entryPoint << "\n";
		return core::ProgramPtr();
	}


	core::ProgramPtr resolveEntryPoints(core::NodeManager& mgr, const IRTranslationUnit& a) {
		// convert entry points stored within TU int a program
		core::ExpressionList entryPoints;
		Resolver resolver(mgr, a);
		for(auto cur : a.getEntryPoints()) {
			entryPoints.push_back(resolver.apply(cur.as<core::ExpressionPtr>()));
		}

		// built complete program
		return core::IRBuilder(mgr).program(entryPoints);
	}

} // end namespace tu
} // end namespace core
} // end namespace insieme

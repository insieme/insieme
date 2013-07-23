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

#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/utils/assert.h"

#include "insieme/core/ir.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/extern.h"

namespace insieme {
namespace frontend {
namespace tu {

	void IRTranslationUnit::addGlobal(const Global& global) {
		assert(global.first && global.first->getType().isa<core::RefTypePtr>());
		assert(!global.second || core::types::isSubTypeOf(global.second->getType(), global.first->getType().as<core::RefTypePtr>()->getElementType()));
		assert(!any(globals, [&](const Global& cur)->bool { return *global.first == *cur.first; }));
		globals.push_back(global);
	}

	std::ostream& IRTranslationUnit::printTo(std::ostream& out) const {
		static auto print = [](const core::NodePtr& node) { return core::printer::printInOneLine(node); };
		return out << "TU(\n\tTypes:\n\t\t"
				<< join("\n\t\t", types, [&](std::ostream& out, const std::pair<core::GenericTypePtr, core::TypePtr>& cur) { out << *cur.first << " => " << *cur.second; })
				<< ",\n\tGlobals:\n\t\t"
				<< join("\n\t\t", globals, [&](std::ostream& out, const std::pair<core::LiteralPtr, core::ExpressionPtr>& cur) { out << *cur.first << ":" << *cur.first->getType() << " => "; if (cur.second) out << print(cur.second); else out << "<uninitalized>"; })
				<< ",\n\tInitializer:\n\t\t"
				<< join("\n\t\t", initializer, [&](std::ostream& out, const core::StatementPtr& cur) { out << print(cur); })
				<< ",\n\tFunctions:\n\t\t"
				<< join("\n\t\t", functions, [&](std::ostream& out, const std::pair<core::LiteralPtr, core::ExpressionPtr>& cur) { out << *cur.first << " => " << print(cur.second); })
				<< ",\n\tEntry Points:\t{"
				<< join(", ", entryPoints, [&](std::ostream& out, const core::LiteralPtr& cur) { out << *cur; })
				<< "}\n)";
	}

	IRTranslationUnit merge(const IRTranslationUnit& a, const IRTranslationUnit& b) {

		IRTranslationUnit res = a;

		// copy types
		for(auto cur : b.getTypes()) {
			if(!res[cur.first]) res.addType(cur.first, cur.second);
		}

		// copy functions
		for(auto cur : b.getFunctions()) {
			if(!res[cur.first]) res.addFunction(cur.first, cur.second);
		}

		// copy globals
		for(auto cur : b.getGlobals()) {
			res.addGlobal(cur);
		}

		// copy initalizer
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

	IRTranslationUnit merge(const vector<IRTranslationUnit>& units) {
		IRTranslationUnit res;
		for(const auto& cur : units) {
			res = merge(res, cur);
		}
		return res;
	}


	// ------------------------------- to-program conversion ----------------------------


	namespace {

		using namespace core;

		/**
		 * The class converting a IR-translation-unit into an actual IR program by
		 * realizing recursive definitions.
		 */
		class Resolver : public core::NodeMapping {

			NodeManager& mgr;
			IRBuilder builder;

			NodeMap cache;
			NodeMap symbolMap;
			NodeMap recVarMap;
			NodeMap recVarResolutions;
			NodeSet recVars;

		public:

			Resolver(NodeManager& mgr, const IRTranslationUnit& unit)
				: mgr(mgr), builder(mgr) {

				// copy type symbols into symbol table
				for(auto cur : unit.getTypes()) {
					symbolMap[cur.first] = cur.second;
				}

				// copy function symbols into symbol table
				for(auto cur : unit.getFunctions()) {
					symbolMap[cur.first] = cur.second;
				}

			}

			virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {

				// check whether value is already cached
				{
					auto pos = cache.find(ptr);
					if (pos != cache.end()) {
						return pos->second;
					}
				}

				// init result
				NodePtr res = ptr;

				// check whether the current node is in the symbol table
				NodePtr recVar;
				auto pos = symbolMap.find(ptr);
				if (pos != symbolMap.end()) {

					// result will be the substitution
					res = pos->second;

					// enter a recursive substitute into the cache
					recVar = recVarMap[ptr];
					if (recVar) {
						// check whether the recursive variable has already been completely resolved
						auto pos = recVarResolutions.find(recVar);
						if (pos != recVarResolutions.end()) {
							return pos->second;
						}

					} else {

						// create a fresh recursive variable
						if (const GenericTypePtr& symbol = ptr.isa<GenericTypePtr>()) {
							recVar = builder.typeVariable(symbol->getFamilyName());
						} else if (const LiteralPtr& symbol = ptr.isa<LiteralPtr>()) {
							recVar = builder.variable(map(symbol->getType()));
						} else {
							assert(false && "Unsupported symbol encountered!");
						}
						recVarMap[ptr] = recVar;
						recVars.insert(recVar);
					}

					// update cache for current element
					cache[ptr] = recVar;
				}

				// resolve result recursively
				res = res->substitute(mgr, *this);


				// fix recursions
				if (recVar) {

					// check whether nobody has been messing with the cache!
					assert_eq(cache[ptr], recVar) << "Don't touch this!";

					// remove recursive variable from cache
					cache.erase(ptr);

					if (TypePtr type = res.isa<TypePtr>()) {

						// fix type recursion
						res = fixRecursion(type, recVar.as<core::TypeVariablePtr>());

						// migrate annotations
						core::transform::utils::migrateAnnotations(pos->second, res);

						if (!hasFreeTypeVariables(res)) {
							cache[ptr] = res;

							// also, register results in recursive variable resolution map
							if (const auto& recType = res.isa<RecTypePtr>()) {
								auto definition = recType->getDefinition();
								if (definition.size() > 1) {
									for(auto cur : definition) {
										recVarResolutions[cur->getVariable()] = builder.recType(cur->getVariable(), definition);
									}
								}
							}
						}

					} else if (LambdaExprPtr lambda = res.isa<LambdaExprPtr>()) {

						// re-build current lambda with correct recursive variable
						assert_eq(1u, lambda->getDefinition().size());
						auto var = recVar.as<VariablePtr>();
						auto binding = builder.lambdaBinding(var, lambda->getLambda());
						lambda = builder.lambdaExpr(var, builder.lambdaDefinition(toVector(binding)));

						// fix recursions
						res = fixRecursion(lambda);

						// migrate annotations
						core::transform::utils::migrateAnnotations(pos->second, res);

						// add final results to cache
						if (!analysis::hasFreeVariables(res)) {
							cache[ptr] = res;

							// also, register results in recursive variable resolution map
							auto definition = res.as<LambdaExprPtr>()->getDefinition();
							if (definition.size() > 1) {
								for(auto cur : definition) {
									recVarResolutions[cur->getVariable()] = builder.lambdaExpr(cur->getVariable(), definition);
								}
							}
						}

					} else {
						std::cout << res->getNodeType();
						assert(false && "Unsupported recursive structure encountered!");
					}

				}

				// special service: get rid of unnecessary casts (which might be introduced due to opaque generic types)
				if (const CastExprPtr& cast = res.isa<CastExprPtr>()) {
					// check whether cast can be skipped
					if (types::isSubTypeOf(cast->getSubExpression()->getType(), cast->getType())) {
						res = cast->getSubExpression();
					}
				}

				// migrate annotations
				if (ptr != res) {
					core::transform::utils::migrateAnnotations(ptr, res);
				}

				// add result to cache if it does not contain recursive parts
				if (*ptr == *res) {
					cache[ptr] = res;
				}

				// done
				return res;
			}

		private:

			bool hasFreeTypeVariables(const NodePtr& node) {
				return analysis::hasFreeTypeVariables(node.isa<TypePtr>());
			}

			bool hasFreeTypeVariables(const TypePtr& type) {
				return analysis::hasFreeTypeVariables(type);
			}

			TypePtr fixRecursion(const TypePtr type, const TypeVariablePtr var) {
				// if it is a direct recursion, be done
				NodeManager& mgr = type.getNodeManager();
				IRBuilder builder(mgr);

				// make sure it is handling a struct or union type
				assert(type.isa<StructTypePtr>() || type.isa<UnionTypePtr>());

				// see whether there is any free type variable
				if (!hasFreeTypeVariables(type)) return type;

				// 1) check nested recursive types - those include this type

				// check whether there is nested recursive type specification that equals the current type
				std::vector<RecTypePtr> recTypes;
				visitDepthFirstOnce(type, [&](const RecTypePtr& cur) {
					if (cur->getDefinition()->getDefinitionOf(var)) recTypes.push_back(cur);
				}, true, true);

				// see whether one of these is matching
				for(auto cur : recTypes) {
					// TODO: here it should actually be checked whether the inner one is structurally identical
					//		 at the moment we relay on the fact that it has the same name
					return builder.recType(var, cur->getDefinition());
				}


				// 2) normalize recursive type

				// collect all struct types within the given type
				TypeList structs;
				visitDepthFirstOncePrunable(type, [&](const TypePtr& cur) {
					//if (containsVarFree(cur)) ;
					if (cur.isa<RecTypePtr>()) return !hasFreeTypeVariables(cur);
					if (cur.isa<NamedCompositeTypePtr>() && hasFreeTypeVariables(cur)) {
						structs.push_back(cur.as<TypePtr>());
					}
					return false;
				}, true);

				// check whether there is a recursion at all
				if (structs.empty()) return type;

				// create de-normalized recursive bindings
				vector<RecTypeBindingPtr> bindings;
				for(auto cur : structs) {
					bindings.push_back(builder.recTypeBinding(builder.typeVariable(annotations::c::getCName(cur)), cur));
				}

				// sort according to variable names
				std::sort(bindings.begin(), bindings.end(), [](const RecTypeBindingPtr& a, const RecTypeBindingPtr& b) {
					return a->getVariable()->getVarName()->getValue() < b->getVariable()->getVarName()->getValue();
				});

				// create definitions
				RecTypeDefinitionPtr def = builder.recTypeDefinition(bindings);

				// test whether this is actually a closed type ..
				if(hasFreeTypeVariables(def.as<NodePtr>())) return type;

				// normalize recursive representation
				RecTypeDefinitionPtr old;
				while(old != def) {
					old = def;

					// set up current variable -> struct definition replacement map
					NodeMap replacements;
					for (auto cur : def) {
						replacements[cur->getType()] = cur->getVariable();
					}

					// wrap into node mapper
					auto mapper = makeLambdaMapper([&](int, const NodePtr& cur) {
						return transform::replaceAllGen(mgr, cur, replacements);
					});

					// apply mapper to defintions
					vector<RecTypeBindingPtr> newBindings;
					for (RecTypeBindingPtr& cur : bindings) {
						auto newBinding = builder.recTypeBinding(cur->getVariable(), cur->getType()->substitute(mgr, mapper));
						if (!contains(newBindings, newBinding)) newBindings.push_back(newBinding);
					}
					bindings = newBindings;

					// update definitions
					def = builder.recTypeDefinition(bindings);
				}

				// convert structs into list of definitions

				// build up new recursive type (only if it is closed)
				auto res = builder.recType(var, def);
				return hasFreeTypeVariables(res.as<TypePtr>())?type:res;
			}


			/**
			 * The conversion of recursive function is conducted lazily - first recursive
			 * functions are build in an unrolled way before they are closed (by combining
			 * multiple recursive definitions into a single one) by this function.
			 *
			 * @param lambda the unrolled recursive definition to be collapsed into a proper format
			 * @return the proper format
			 */
			LambdaExprPtr fixRecursion(const LambdaExprPtr& lambda) {

				// check whether this is the last free variable to be defined
				VariablePtr recVar = lambda->getVariable();
				auto freeVars = analysis::getFreeVariables(lambda->getLambda());
				if (freeVars != toVector(recVar)) {
					// it is not, delay closing recursion
					return lambda;
				}

				// search all directly nested lambdas
				vector<LambdaExprAddress> inner;
				visitDepthFirstOncePrunable(NodeAddress(lambda), [&](const LambdaExprAddress& cur) {
					if (cur.isRoot()) return false;
					if (!analysis::hasFreeVariables(cur)) return true;
					if (!recVars.contains(cur.as<LambdaExprPtr>()->getVariable())) return false;
					inner.push_back(cur);
					return false;
				});

				// if there is no inner lambda with free variables it is a simple recursion
				if (inner.empty()) return lambda;		// => done

				// ---------- build new recursive function ------------

				auto& mgr = lambda.getNodeManager();
				IRBuilder builder(mgr);

				// build up resulting lambda
				vector<LambdaBindingPtr> bindings;
				bindings.push_back(builder.lambdaBinding(recVar, lambda->getLambda()));
				for(auto cur : inner) {
					assert(cur->getDefinition().size() == 1u);
					auto def = cur->getDefinition()[0];

					// only add every variable once
					if (!any(bindings, [&](const LambdaBindingPtr& binding)->bool { return binding->getVariable() == def.getAddressedNode()->getVariable(); })) {
						bindings.push_back(def);
					}
				}

				LambdaExprPtr res = builder.lambdaExpr(recVar, builder.lambdaDefinition(bindings));

				// collapse recursive definitions
				while (true) {
					// search for reductions (lambda => rec_variable)
					std::map<NodeAddress, NodePtr> replacements;
					visitDepthFirstPrunable(NodeAddress(res), [&](const LambdaExprAddress& cur) {
						if (cur.isRoot()) return false;
						if (!analysis::hasFreeVariables(cur)) return true;

						// only focus on inner lambdas referencing recursive variables
						auto var = cur.as<LambdaExprPtr>()->getVariable();
						if (!res->getDefinition()->getBindingOf(var)) return false;
						replacements[cur] = var;
						return false;
					});

					// check whether the job is done
					if (replacements.empty()) break;

					// apply reductions
					res = transform::replaceAll(mgr, replacements).as<LambdaExprPtr>();
				}

				// that's it
				return res;
			}

		};


		core::LambdaExprPtr addGlobalsInitialization(const IRTranslationUnit& unit, const core::LambdaExprPtr& mainFunc){

			// we only want to init what we use, so we check it
			core::NodeSet usedLiterals;
			core::visitDepthFirstOnce (mainFunc, [&] (const core::LiteralPtr& literal){
				usedLiterals.insert(literal);
			});

			core::IRBuilder builder(mainFunc->getNodeManager());
			core::StatementList inits;

			// ~~~~~~~~~~~~~~~~~~ INITIALIZE GLOBALS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			for (auto cur : unit.getGlobals()) {
				// only consider having an initialization value
				if (!cur.second) continue;
				if (!contains(usedLiterals, cur.first)) continue;
				inits.push_back(builder.assign(cur.first, cur.second));
			}

			// ~~~~~~~~~~~~~~~~~~ PREPARE STATICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
			const lang::StaticVariableExtension& ext = mainFunc->getNodeManager().getLangExtension<lang::StaticVariableExtension>();
			for (auto cur : unit.getGlobals()) {
				// only consider static variables
				auto type = cur.first->getType();
				if (!type.isa<RefTypePtr>() || !ext.isStaticType(type.as<RefTypePtr>()->getElementType())) continue;
				// skip unused variables
				if (!contains(usedLiterals, cur.first)) continue;
				// add creation statement
				inits.push_back(builder.createStaticVariable(cur.first));
			}

			// fix the external markings
			for(auto cur : usedLiterals) {
				auto type = cur.as<LiteralPtr>()->getType();
				annotations::c::markExtern(cur.as<LiteralPtr>(),
						type.isa<RefTypePtr>() &&
						!cur.as<LiteralPtr>()->getStringValue()[0]=='\"' &&
						!ext.isStaticType(type.as<RefTypePtr>()->getElementType()) &&
						!any(unit.getGlobals(), [&](const IRTranslationUnit::Global& global) { return *global.first == *cur; })
				);
			}

			// build resulting lambda
			if (inits.empty()) return mainFunc;

			return core::transform::insert ( mainFunc->getNodeManager(), core::LambdaExprAddress(mainFunc)->getBody(), inits, 0).as<core::LambdaExprPtr>();
		}

		core::LambdaExprPtr addInitializer(const IRTranslationUnit& unit, const core::LambdaExprPtr& mainFunc) {

			// check whether there are any initializer expressions
			if (unit.getInitializer().empty()) return mainFunc;

			// insert init statements
			auto initStmts = ::transform(unit.getInitializer(), [](const ExpressionPtr& cur)->StatementPtr { return cur; });
			return core::transform::insert( mainFunc->getNodeManager(), core::LambdaExprAddress(mainFunc)->getBody(), initStmts, 0).as<core::LambdaExprPtr>();
		}

	} // end anonymous namespace


	core::NodePtr IRTranslationUnit::resolve(const core::NodePtr& node) const {
		return Resolver(getNodeManager(), *this).map(node);
	}

	core::ProgramPtr toProgram(core::NodeManager& mgr, const IRTranslationUnit& a, const string& entryPoint) {

		// search for entry point
		core::IRBuilder builder(mgr);
		for (auto cur : a.getFunctions()) {
			if (cur.first->getStringValue() == entryPoint) {

				// get the symbol
				core::NodePtr symbol = cur.first;

				// extract lambda expression
				core::LambdaExprPtr lambda = Resolver(mgr, a).map(symbol).as<core::LambdaExprPtr>();

				// add initializer
				lambda = addInitializer(a, lambda);

				// add global initializers
				lambda = addGlobalsInitialization(a, lambda);

				// wrap into program
				return builder.program(toVector<core::ExpressionPtr>(lambda));
			}
		}

		assert(false && "No such entry point!");
		return core::ProgramPtr();
	}


	core::ProgramPtr resolveEntryPoints(core::NodeManager& mgr, const IRTranslationUnit& a) {
		// convert entry points stored within TU int a program
		core::ExpressionList entryPoints;
		Resolver creator(mgr, a);
		for(auto cur : a.getEntryPoints()) {
			entryPoints.push_back(creator.map(cur.as<core::ExpressionPtr>()));
		}
		return core::IRBuilder(mgr).program(entryPoints);
	}


} // end namespace tu
} // end namespace frontend
} // end namespace insieme

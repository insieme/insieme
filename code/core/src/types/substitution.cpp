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

#include "insieme/core/types/substitution.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"


namespace insieme {
namespace core {
namespace types {

	namespace {

		const std::vector<TypeAddress>& getFreeTypeVariables(const NodePtr& node) {

			struct Cache : public core::value_annotation::drop_on_clone {
				std::vector<TypeAddress> vars;
				bool operator==(const Cache& other) const {
					return vars == other.vars;
				}
			};

			if (auto cache = node->hasAttachedValue<Cache>()) {
				return cache->vars;
			}

			// create a new list of variables
			auto& vars = node->attachValue<Cache>().vars;
			visitDepthFirstPrunable(NodeAddress(node),[&](const NodeAddress& cur)->Action {

				switch(cur->getNodeType()) {

					// collect addresses of type variables
					case NT_TypeVariable:
					case NT_GenericTypeVariable:
					case NT_VariadicTypeVariable:
					case NT_VariadicGenericTypeVariable:
						vars.push_back(cur.as<TypeAddress>());
						return Action::Prune;

					// prune scope
					case NT_LambdaExpr:

						// we do not need to descent further (lambdas create their own scope)
						return Action::Prune;

					case NT_Literal:
						// also literals of function types introduce new type variable scopes
						return (cur.as<LiteralPtr>()->getType().isa<FunctionTypePtr>())
								? Action::Prune
								: Action::Descent;

					default: {
						// nothing
					}
				}

				// for all the rest, continue searching
				return Action::Descent;
			},true);

			return vars;

		}

	} // end anonymous namespace


	Substitution::Substitution(const TypeVariablePtr& var, const TypePtr& type) {
		addMapping(var, type);
	};

	Substitution::Substitution(const GenericTypeVariablePtr& var, const TypePtr& type) {
		addMapping(var, type);
	};

	bool isMatchingStructure(const TypePtr& value, const GenericTypeVariablePtr& pattern) {
		
		// value needs to be a generic type or generic type variable
		auto type = value.isa<GenericTypePtr>();
		auto var = value.isa<GenericTypeVariablePtr>();
		if (!type && !var) return false;

		const auto& valueParams = (type) ? type->getTypeParameter() : var->getTypeParameter();
		const auto& patternParams = pattern->getTypeParameter();

		// check length of list
		if (valueParams.size() != patternParams.size()) return false;

		// check structure of individual parameters
		for (const auto& cur : make_paired_range(valueParams, patternParams)) {
			if (cur.first.isa<TypeVariablePtr>() || cur.second.isa<TypeVariablePtr>()) {
				// all fine
			} else if (auto var = cur.second.isa<GenericTypeVariablePtr>()) {
				if (!isMatchingStructure(cur.first, var)) return false;
			} else {
				assert_fail() << "Invalid pattern element type: " << cur.second->getNodeType();
			}
		}

		// all fine
		return true;
	}

	TypePtr Substitution::operator[](const GenericTypeVariablePtr& var) const {
		// looking up matching entry
		for (const auto& cur : genericTypeVarMapping) {
			if (*cur.first->getVarName() == *var->getVarName() && isMatchingStructure(var, cur.first)) {
				return cur.second;
			}
		}
		return TypePtr();
	}

	const GenericTypeVariableList* Substitution::operator[](const VariadicGenericTypeVariablePtr& var) const {
		auto pos = variadicGenericTypeVarMapping.find(var->getVarName());
		return (pos == variadicGenericTypeVarMapping.end()) ? nullptr : &pos->second;
	}

	NodePtr Substitution::applyTo(NodeManager& manager, const NodePtr& node) const {

		// shortcut for empty substitutions
		if (empty()) return node;

		// when being applied to functions or bind expressions, apply to defined scope
		if (node.isa<LiteralPtr>() || node.isa<LambdaExprPtr>() || node.isa<BindExprPtr>()) {
			auto mapper = makeLambdaMapper([&](unsigned,const NodePtr& node){
				return applyTo(manager,node);
			});
			return node->substitute(manager, mapper);

		}

		// step 1: expand variadic type variables
		NodePtr res = node;

		// get list of parents of type variadic type variables
		std::set<NodeAddress> parents;
		for(const auto& cur : getFreeTypeVariables(res)) {

			// only interested in variadic variables mapped in this substitution
			if (!cur.isa<VariadicTypeVariablePtr>() && !cur.isa<VariadicGenericTypeVariablePtr>()) continue;

			// if it is the root, also not interested (con't do anything)
			if (cur.isRoot()) continue;

			// record parent
			parents.insert(cur.getParentAddress());
		}

		// expand all parents, in reverse order (bottom up)
		for(auto it = parents.rbegin(); it != parents.rend(); ++it) {
			auto parent = it->switchRoot(res);

			// a utility to expand lists of types
			auto expandList = [&](const TypeList& list)->TypeList {
				TypeList res;
				for(const auto& cur : list) {

					// expand variadic variables
					if (auto var = cur.isa<VariadicTypeVariablePtr>()) {

						// expand variadic type variable in list of type variables
						auto expanded = operator[](var);
						if (expanded) {
							for(const auto& cur : *expanded) {
								res.push_back(cur);
							}
						} else {
							res.push_back(cur);
						}

					} else if (auto var = cur.isa<VariadicGenericTypeVariablePtr>()) {

						// expand variadic generic type variable into list of generic type variables
						auto expanded = operator[](var);
						if (expanded) {
							for(const auto& cur : *expanded) {
								res.push_back(GenericTypeVariable::get(manager, cur->getVarName(), var->getTypeParameter()));
							}
						} else {
							res.push_back(cur);
						}

					} else {

						// all others are just copied
						res.push_back(cur);
					}
				}
				return res;
			};

			// a free variable may only be within type lists or tuple types
			if (auto types = parent.isa<TypesPtr>()) {

				// expand list of types and create new types node
				auto newList = Types::get(manager,expandList(types->getTypes()));
				if (*types != *newList) res = transform::replaceNode(manager,parent,newList);

			} else if (auto tupleType = parent.isa<TupleTypePtr>()) {

				// expand list of types and create new tuple node
				auto newTuple = TupleType::get(manager,expandList(tupleType->getElementTypes()));
				if (*tupleType != *newTuple) res = transform::replaceNode(manager,parent,newTuple);

			} else {
				assert_not_implemented() << "No support for expanding variadic type variables in " << parent->getNodeType() << " nodes.";
			}

		}


		// step 2: substitute remaining type variables

		// create substitution map
		std::map<NodeAddress,NodePtr> replacements;
		for(const auto& cur : getFreeTypeVariables(res)) {

			// ordinary type variable substitution ..
			if(TypeVariablePtr var = cur.isa<TypeVariablePtr>()) {
				if (auto replacement = operator[](var)) {
					replacements[cur] = replacement;
				}
			}

			// and generic type variable substitution
			if(GenericTypeVariablePtr var = cur.isa<GenericTypeVariablePtr>()) {
				// lookup current variable within the substitution mapping
				if (auto replacement = operator[](var)) {
					// found! => replace, but only type family name
					if (auto genType = replacement.isa<GenericTypePtr>()) {
						replacements[cur] = applyTo(manager,GenericType::get(manager, genType->getName(), var->getTypeParameter()).as<TypePtr>());
					} else if (auto gvar = replacement.isa<GenericTypeVariablePtr>()) {
						replacements[cur] = applyTo(manager,GenericTypeVariable::get(manager, gvar->getVarName(), var->getTypeParameter()).as<TypePtr>());
					} else {
						assert_fail() << "Invalid mapping of " << *var << ": " << *replacement << " of type " << replacement->getNodeType();
					}
				}
			}
		}

		if (!replacements.empty()) {
			res = transform::replaceAll(manager,replacements);
		}

		// done
		return res;
	}

	void Substitution::addMapping(const TypeVariablePtr& var, const TypePtr& type) {
		typeVarMapping[var] = type;
	}

	void Substitution::addMapping(const VariadicTypeVariablePtr& var, const TypeVariableList& expanded) {
		variadicTypeVarMapping[var->getVarName()] = expanded;
	}

	void Substitution::addMapping(const GenericTypeVariablePtr& var, const TypePtr& type) {
		genericTypeVarMapping[analysis::normalize(var)] = type;
	}

	void Substitution::addMapping(const VariadicGenericTypeVariablePtr& var, const GenericTypeVariableList& expanded) {
		variadicGenericTypeVarMapping[var->getVarName()] = expanded;
	}

	void Substitution::remMappingOf(const TypeVariablePtr& var) {
		typeVarMapping.erase(var);
	}

	void Substitution::remMappingOf(const VariadicTypeVariablePtr& var) {
		auto* list = operator[](var);
		if (list) for (const auto& cur : *list) {
			remMappingOf(cur);
		}
		variadicTypeVarMapping.erase(var->getVarName());
	}

	void Substitution::remMappingOf(const GenericTypeVariablePtr& var) {
		genericTypeVarMapping.erase(analysis::normalize(var));
	}

	void Substitution::remMappingOf(const VariadicGenericTypeVariablePtr& var) {
		auto* list = operator[](var);
		if (list) for (const auto& cur : *list) {
			remMappingOf(cur);
		}
		variadicGenericTypeVarMapping.erase(var->getVarName());
	}

	void Substitution::removeMappings(const TypeVariableSet& variables) {
		for (const auto& cur : variables) {
			remMappingOf(cur);
		}
	}

	void Substitution::removeMappings(const VariadicTypeVariableSet& variables) {
		for (const auto& cur : variables) {
			remMappingOf(cur);
		}
	}
	
	void Substitution::removeMappings(const GenericTypeVariableSet& variables) {
		for (const auto& cur : variables) {
			remMappingOf(cur);
		}
	}
	
	void Substitution::removeMappings(const VariadicGenericTypeVariableSet& variables) {
		for (const auto& cur : variables) {
			remMappingOf(cur);
		}
	}

	std::ostream& Substitution::printTo(std::ostream& out) const {
		out << "{";
		
		// print variable -> type mappings sorted
		std::vector<std::string> mappings;
		for(const auto& cur : typeVarMapping) {
			mappings.push_back(format("%s->%s",*cur.first,*cur.second));
		}
		std::sort(mappings.begin(),mappings.end());
		out << join(",",mappings);
		
		if (!typeVarMapping.empty() && !genericTypeVarMapping.empty()) out << ",";
		
		out << join(",", genericTypeVarMapping, [](std::ostream& out, const GenericTypeVariableMapping::value_type& cur) { 
			out << *cur.first << "->";
			if (auto genType = cur.second.isa<GenericTypePtr>()) {
				out << genType->getFamilyName();
			} else if(auto var = cur.second.isa<GenericTypeVariablePtr>()) {
				out << "'" << *var->getVarName();
			} else {
				out << "?";
			}
		});

		// end here if there are no variadic entries
		if (variadicTypeVarMapping.empty() && variadicGenericTypeVarMapping.empty()) return out << "}";

		out << "|";

		out << join(",", variadicTypeVarMapping, [](std::ostream& out, const VariadicTypeVariableMapping::value_type& cur) {
			out << *cur.first << "->[" << join(",", cur.second, print<deref<TypeVariablePtr>>()) << "]";
		});
		
		if (!variadicTypeVarMapping.empty() && !variadicGenericTypeVarMapping.empty()) out << ",";

		out << join(",", variadicGenericTypeVarMapping, [](std::ostream& out, const VariadicGenericTypeVariableMapping::value_type& cur) {
			out << *cur.first << "->[" << join(",", cur.second, print<deref<GenericTypeVariablePtr>>()) << "]";
		});

		return out << "}";
	}

	Substitution Substitution::compose(NodeManager& manager, const Substitution& a, const Substitution& b) {

		// copy substitution a
		Substitution res(a);

		// -- type variables --

		// apply substitution b to all mappings in a
		for_each(res.typeVarMapping, [&manager, &b](typename TypeVariableMapping::value_type& cur) { cur.second = b.applyTo(manager, cur.second); });

		// add remaining mappings of b
		Substitution::TypeVariableMapping& resMapping = res.typeVarMapping;
		for_each(b.typeVarMapping, [&resMapping](const typename TypeVariableMapping::value_type& cur) {
			if(resMapping.find(cur.first) == resMapping.end()) { resMapping.insert(cur); }
		});

		
		// -- generic type variables --

		// apply substitution b to all mappings in a
		for_each(res.genericTypeVarMapping, [&manager, &b](typename GenericTypeVariableMapping::value_type& cur) { cur.second = b.applyTo(manager, cur.second); });

		// add remaining mappings of b
		Substitution::GenericTypeVariableMapping& resGenMapping = res.genericTypeVarMapping;
		for_each(b.genericTypeVarMapping, [&resGenMapping](const typename GenericTypeVariableMapping::value_type& cur) {
			if (resGenMapping.find(cur.first) == resGenMapping.end()) { resGenMapping.insert(cur); }
		});

		// done
		return res;
	}

	Substitution Substitution::copyTo(NodeManager& manager) const {

		// copy the substitution
		Substitution res = *this;

		// copy variable mapping
		for (auto& cur : res.typeVarMapping) {
			const_cast<TypeVariablePtr&>(cur.first) = manager.get(cur.first);
			cur.second = manager.get(cur.second);
		}

		// copy variadic variable mapping
		for (auto& cur : res.variadicTypeVarMapping) {
			const_cast<StringValuePtr&>(cur.first) = manager.get(cur.first);
			for (auto& var : cur.second) {
				var = manager.get(var);
			}
		}

		// copy generic variable mapping
		for (auto& cur : res.genericTypeVarMapping) {
			const_cast<GenericTypeVariablePtr&>(cur.first) = manager.get(cur.first);
			cur.second = manager.get(cur.second);
		}

		// copy variadic variable mapping
		for (auto& cur : res.variadicGenericTypeVarMapping) {
			const_cast<StringValuePtr&>(cur.first) = manager.get(cur.first);
			for (auto& var : cur.second) {
				var = manager.get(var);
			}
		}

		// done
		return res;
	}

} // end namespace types
} // end namespace core
} // end namespace insieme

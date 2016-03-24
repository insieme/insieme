/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/types/substitution.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"


namespace insieme {
namespace core {
namespace types {

	namespace {

		/**
		 * This class provides a wrapper for a substitution to be applied to some type. This
		 * wrapper is based on a node mapping, which allows this class to exploit the general node mapping
		 * mechanism to perform
		 */
		class SubstitutionMapper : public transform::CachedNodeMapping {

			/**
			 * The node manager to be used for creating new type nodes.
			 */
			NodeManager& manager;

			/**
			 * The substitution to be applied.
			 */
			const Substitution& substitution;

			/**
			 * The root node of the substitution. This one will always be effected, however,
			 * nested scopes will be skipped.
			 */
			NodePtr root;

		  public:
			/**
			 * Creates a new instance of this class wrapping the given substitution.
			 *
			 * @param manager the node manager to be used for creating new node instances if necessary
			 * @param substitution the substitution to be wrapped by the resulting instance.
			 */
			SubstitutionMapper(NodeManager& manager, const Substitution& substitution, const NodePtr& root)
			    : manager(manager), substitution(substitution), root(root) {};

			/**
			 * The procedure mapping a node to its substitution.
			 *
			 * @param element the node to be resolved
			 */
			const NodePtr resolveElement(const NodePtr& element) override {

				// prune area of influence
				if (element != root && (
						element.isa<LambdaExprPtr>() ||
						element.isa<BindExprPtr>() ||
						(element.isa<LiteralPtr>() && element.as<LiteralPtr>()->getType().isa<FunctionTypePtr>())
					)) {

					// lambdas define new scopes for variables => filter
					auto funType = element.as<ExpressionPtr>()->getType().as<FunctionTypePtr>();

					// compute reduced type variable substitution
					Substitution sub = substitution;
					sub.removeMappings(analysis::getTypeVariablesBoundBy(funType));
					sub.removeMappings(analysis::getVariadicTypeVariablesBoundBy(funType));
					
					// run substitution recursively on reduced set
					return sub(element);
				}

				// make sure current limitations are not exceeded
				assert_true(!element.isa<LambdaExprPtr>() ||
						!element.as<LambdaExprPtr>()->isRecursive() ||
						!analysis::isGeneric(element.as<ExpressionPtr>()->getType()) ||
						element.as<LambdaExprPtr>()->getDefinition().size() == 1)
						<< "Instantiation of generic recursive functions not supported yet!"
						<< "Function:\n" << dumpColor(element);

				// handle tuple types
				if (TupleTypePtr tuple = element.isa<TupleTypePtr>()) {
					// map tuple types to type lists, convert those, and move back
					NodeManager& mgr = element->getNodeManager();
					auto types = Types::get(mgr, tuple->getElementTypes());
					auto newTypes = resolveElement(types).as<TypesPtr>();
					return TupleType::get(mgr, newTypes->getTypes());
				}

				// check for variadic type variables
				if (TypesPtr types = element.isa<TypesPtr>()) {
					if (!types.empty()) {
						if (auto vvar = types.back().isa<VariadicTypeVariablePtr>()) {
							if (auto expanded = substitution[vvar]) {
								TypeList list = types.getTypes();
								list.pop_back();
								for (const auto& cur : *expanded) {
									list.push_back(cur);
								}
								return resolveElement(Types::get(element->getNodeManager(), list));
							}
						} else if (auto vvar = types.back().isa<VariadicGenericTypeVariablePtr>()) {
							if (auto expanded = substitution[vvar]) {
								TypeList list = types.getTypes();
								list.pop_back();
								for (const auto& cur : *expanded) {
									list.push_back(GenericTypeVariable::get(manager, cur->getVarName(), vvar->getTypeParameter()));
								}
								return resolveElement(Types::get(element->getNodeManager(), list));
							}
						}
					}
				}

				// from here on, only variables are substituted
				if (auto var = element.isa<TypeVariablePtr>()) {
					// lookup current variable within the mapping
					if (TypePtr replacement = substitution[var]) {
						// found! => replace
						return replacement;
					}
				}

				// and generic type variable substitution
				if (auto var = element.isa<GenericTypeVariablePtr>()) {
					// lookup current variable within the mapping
					if (TypePtr replacement = substitution[var]) {
						// found! => replace, but only type family name
						if (auto genType = replacement.isa<GenericTypePtr>()) {
							return resolveElement(GenericType::get(manager, genType->getName(), var->getTypeParameter()));
						} else if (auto gvar = replacement.isa<GenericTypeVariablePtr>()) {
							return resolveElement(GenericTypeVariable::get(manager, gvar->getVarName(), var->getTypeParameter()));
						}
						assert_fail() << "Invalid mapping of " << *var << ": " << *replacement << " of type " << replacement->getNodeType();
					}
				}


				// -- everything else --

				// replace base
				auto res = element->substitute(manager, *this);

				// move annotations
				transform::utils::migrateAnnotations(element, res);

				// done
				return res;
			}
		};

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
		// perform substitution
		SubstitutionMapper mapper(manager, *this, node);
		return mapper.map(0, node);
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
		
		out << join(",", typeVarMapping, [](std::ostream& out, const TypeVariableMapping::value_type& cur) { 
			out << *cur.first << "->" << *cur.second; 
		});
		
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

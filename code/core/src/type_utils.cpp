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

#include <cassert>

#include <boost/unordered_map.hpp>

#include "insieme/core/type_utils.h"
#include "insieme/core/ast_visitor.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/map_utils.h"


namespace insieme {
namespace core {

namespace {

class SubstitutionMapper : public NodeMapping {

	NodeManager& manager;
	const Substitution::Mapping& mapping;
	const Substitution::IntTypeParamMapping& paramMapping;

public:

	SubstitutionMapper(NodeManager& manager, const Substitution& substitution)
		: NodeMapping(!substitution.getIntTypeParamMapping().empty()), manager(manager),
		  mapping(substitution.getMapping()), paramMapping(substitution.getIntTypeParamMapping()) {};

	const NodePtr mapElement(unsigned, const NodePtr& element) {
		// quick check - only variables are substituted
		if (element->getNodeType() != NT_TypeVariable) {
			return element->substitute(manager, *this);
		}

		// lookup current variable within the mapping
		auto pos = mapping.find(static_pointer_cast<const TypeVariable>(element));
		if (pos != mapping.end()) {
			// found! => replace
			return (*pos).second;
		}

		// not found => return current node
		// (since nothing within a variable node may be substituted)
		return element;
	}

	virtual IntTypeParam mapParam(const IntTypeParam& param) {
		if (param.getType() != IntTypeParam::VARIABLE) {
			return param;
		}
		auto pos = paramMapping.find(param);
		if (pos != paramMapping.end()) {
			return (*pos).second;
		}
		return param;
	}
};


}

Substitution::Substitution(const TypeVariablePtr& var, const TypePtr& type) {
	mapping.insert(std::make_pair(var, type));
};

Substitution::Substitution(const IntTypeParam& var, const IntTypeParam& type) {
	assert( var.getType()==IntTypeParam::VARIABLE && "Cannot add mapping for non-variable parameter!");
	paramMapping.insert(std::make_pair(var,type));
}

TypePtr Substitution::applyTo(NodeManager& manager, const TypePtr& type) const {
	// perform substitution
	SubstitutionMapper mapper(manager, *this);
	return mapper.map(0, type);
}

IntTypeParam Substitution::applyTo(const IntTypeParam& param) const {
	if (param.getType() != IntTypeParam::VARIABLE) {
		return param;
	}
	auto pos = paramMapping.find(param);
	if (pos == paramMapping.end()) {
		return param;
	}
	return (*pos).second;
}


void Substitution::addMapping(const TypeVariablePtr& var, const TypePtr& type) {
	auto element = std::make_pair(var,type);
	auto res = mapping.insert(element);
	if (!res.second) {
		mapping.erase(var);
		res = mapping.insert(element);
		assert( res.second && "Insert was not successful!" );
	}
}

void Substitution::addMapping(const IntTypeParam& var, const IntTypeParam& value) {

	assert( var.getType()==IntTypeParam::VARIABLE && "Cannot add mapping for non-variable parameter!");

	auto element = std::make_pair(var,value);
	auto res = paramMapping.insert(element);
	if (!res.second) {
		paramMapping.erase(var);
		res = paramMapping.insert(element);
		assert( res.second && "Insert was not successful!" );
	}
}

void Substitution::remMappingOf(const TypeVariablePtr& var) {
	if (var->getNodeType() != NT_TypeVariable) {
		return;
	}
	mapping.erase(var);
}

void Substitution::remMappingOf(const IntTypeParam& var) {
	if (var.getType() != IntTypeParam::VARIABLE) {
		return;
	}
	paramMapping.erase(var);
}


Substitution Substitution::compose(NodeManager& manager, const Substitution& a, const Substitution& b) {

	typedef Substitution::Mapping::value_type Entry;
	typedef Substitution::IntTypeParamMapping::value_type ParamEntry;

	// copy substitution b
	Substitution res(b);

	// --- normal types ---

	// apply substitution a to all mappings in b
	for_each(res.mapping, [&manager, &a](Entry& cur) {
		cur.second = a.applyTo(manager, cur.second);
	});

	// add remaining mappings of a
	Substitution::Mapping& resMapping = res.mapping;
	for_each(a.mapping, [&resMapping](const Entry& cur) {
		if (resMapping.find(cur.first) == resMapping.end()) {
			resMapping.insert(cur);
		}
	});

	// --- int type parameter ---

	// apply substitution a to all mappings in b
	for_each(res.paramMapping, [&manager, &a](ParamEntry& cur) {
		cur.second = a.applyTo(cur.second);
	});

	// add remaining mappings of a
	Substitution::IntTypeParamMapping& resParamMapping = res.paramMapping;
	for_each(a.paramMapping, [&resParamMapping](const ParamEntry& cur) {
		if (resParamMapping.find(cur.first) == resParamMapping.end()) {
			resParamMapping.insert(cur);
		}
	});

	return res;
}


bool occurs(const NodePtr& x, const NodePtr& term) {
	// it occurs if it is the current node or one of its child contains the node
	return *x==*term || any(term->getChildList(), [&x](const NodePtr& cur) {
		return occurs(x, cur);
	});
}

namespace {

	inline void preprocessTypes(NodeManager& manager, TypePtr& typeA, TypePtr& typeB) {

		// check node types
		NodeType a = typeA->getNodeType();
		NodeType b = typeB->getNodeType();

		if (a==b) {
			return;
		}


		// if only one is a vector, replace the vector with an array ...
		if (a == NT_ArrayType && b == NT_VectorType) {
			preprocessTypes(manager, typeB, typeA);
			return;
		}
		if (a == NT_VectorType && b == NT_ArrayType) {
			TypePtr elementType = static_pointer_cast<const VectorType>(typeA)->getElementType();
			typeA = ArrayType::get(manager, elementType, IntTypeParam::getConcreteIntParam(1));
			return;
		}
	}

}

boost::optional<Substitution> unifyAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list) {
	typedef std::pair<TypePtr, TypePtr> Pair;
	typedef std::list<Pair> List;

	// create result
	Substitution res;

	while(!list.empty()) {

		// get current element
		Pair cur = list.front();
		list.pop_front();

		TypePtr a = cur.first;
		TypePtr b = cur.second;

		// preprocess types (e.g. vector - array relation)
		preprocessTypes(manager, a, b);

		const NodeType typeOfA = a->getNodeType();
		const NodeType typeOfB = b->getNodeType();

		// 1) if (a == b) ignore pair ..
		if (*a==*b) {
			continue;
		}

		// 2) swap if b is a variable but a is not ..
		if (typeOfA != NT_TypeVariable && typeOfB == NT_TypeVariable) {
			// add swapped pair to front of list
			list.push_front(std::make_pair(b, a));
			continue;
		}

		// 3) handle variables on left hand side ...
		if (typeOfA == NT_TypeVariable) {
			if (occurs(a, b)) {
				// not unifiable (e.g. X = type<X> cannot be unified)
				return boost::optional<Substitution>();
			}

			// convert current pair into substitution
			Substitution mapping(
					static_pointer_cast<const TypeVariable>(a),
					static_pointer_cast<const Type>(b)
			);

			// apply substitution to remaining pairs
			for_each(list, [&mapping, &manager](Pair& cur) {
				cur.first = mapping.applyTo(manager, cur.first);
				cur.second = mapping.applyTo(manager, cur.second);
			});

			// compose current mapping with overall result
			res = Substitution::compose(manager, res, mapping);
			continue;
		}

		// 4) function types / generic types / tuples / structs / unions / recursive types
		if (typeOfA != typeOfB) {
			// => not unifiable
			return boost::optional<Substitution>();
		}

		if (typeOfA == NT_RecType) {
			// TODO: implement
			assert ("RECURSIVE TYPE SUPPORT NOT IMPLEMENTED!");
		}

		// => check family of generic type
		if (typeOfA == NT_GenericType) {
			const GenericTypePtr& genericTypeA = static_pointer_cast<const GenericType>(a);
			const GenericTypePtr& genericTypeB = static_pointer_cast<const GenericType>(b);

			if (genericTypeA->getFamilyName() != genericTypeB->getFamilyName()) {
				return boost::optional<Substitution>();
			}

			// ---- unify int type parameter ---

			// get lists
			auto paramsA = genericTypeA->getIntTypeParameter();
			auto paramsB = genericTypeB->getIntTypeParameter();

			// check number of arguments ...
			if (paramsA.size() != paramsB.size()) {
				// => not unifyable
				return boost::optional<Substitution>();
			}

			for(std::size_t i=0; i<paramsA.size(); i++) {
				IntTypeParam paramA = paramsA[i];
				IntTypeParam paramB = paramsB[i];

				// equivalent pairs can be ignored ...
				if (paramA == paramB) {
					continue;
				}

				// check for variables
				if (paramA.getType() != IntTypeParam::VARIABLE && paramB.getType() != IntTypeParam::VARIABLE) {
					// different constants => not unifyable!
					return boost::optional<Substitution>();
				}

				// move variable to first place
				if (paramA.getType() != IntTypeParam::VARIABLE && paramB.getType() ==IntTypeParam::VARIABLE) {
					IntTypeParam tmp = paramA;
					paramA = paramB;
					paramB = tmp;
				}

				// add mapping
				Substitution mapping(paramA,paramB);

				// apply substitution to remaining pairs
				for_each(list, [&mapping, &manager](Pair& cur) {
					cur.first = mapping.applyTo(manager, cur.first);
					cur.second = mapping.applyTo(manager, cur.second);
				});

				// compose current mapping with overall result
				res = Substitution::compose(manager, res, mapping);
			}
		}

		// => check all child nodes
		auto childrenA = a->getChildList();
		auto childrenB = b->getChildList();
		if (childrenA.size() != childrenB.size()) {
			// => not unifyable
			return boost::optional<Substitution>();
		}

		std::for_each(
				make_paired_iterator(childrenA.begin(), childrenB.begin()),
				make_paired_iterator(childrenA.end(), childrenB.end()),

				[&list](const std::pair<NodePtr, NodePtr>& cur) {
					list.push_front(std::make_pair(
							static_pointer_cast<const Type>(cur.first),
							static_pointer_cast<const Type>(cur.second)
					));
		});
	}

	// done
	return boost::optional<Substitution>(res);
}


boost::optional<Substitution> unify(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB) {
	return unifyAll(manager, toVector<TypePtr>(typeA), toVector<TypePtr>(typeB));
}

bool isUnifyable(const TypePtr& typeA, const TypePtr& typeB) {
	NodeManager tmp; // requires only temporary manager
	return typeA==typeB || unify(tmp, typeA, typeB);
}


namespace {
	using std::pair;

	// ------------- Utilities to support the replacement of type variables with fresh variables ------------------------

	pair<vector<TypeVariablePtr>, vector<IntTypeParam>> getVariables(const TypePtr& typeA) {

		pair<vector<TypeVariablePtr>, vector<IntTypeParam>> res;

		visitAllNodesOnce(typeA, [&res](const NodePtr& node){
			switch(node->getNodeType()) {
			case NT_TypeVariable:
				// collect type variables
				res.first.push_back(static_pointer_cast<const TypeVariable>(node));
				break;
			case NT_GenericType: {
					// collect int-type param variables
					GenericTypePtr genType = static_pointer_cast<const GenericType>(node);
					for_each(genType->getIntTypeParameter(), [&res](const IntTypeParam& cur) {
						if (cur.getType() == IntTypeParam::VARIABLE) {
							res.second.push_back(cur);
						}
					});
					break;
				}
			default:
				// nothing to do for the rest
				break;
			}
		});

		return res;
	}

	class TypeVariableReplacer : public NodeMapping {

		unsigned varCounter;

		unsigned paramCounter;

		NodeManager& manager;

		boost::unordered_map<TypeVariablePtr, TypeVariablePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> varMap;

		boost::unordered_map<IntTypeParam, IntTypeParam, boost::hash<IntTypeParam>> paramMap;

	public:

		TypeVariableReplacer(NodeManager& manager) : NodeMapping(true), varCounter((unsigned)0), paramCounter((unsigned)0), manager(manager) { }

		virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
			// only handle type variables
			if (ptr->getNodeType() != NT_TypeVariable) {
				return ptr;
			}

			// cast type variable
			TypeVariablePtr cur = static_pointer_cast<const TypeVariable>(ptr);

			// search for parameter ...
			auto pos = varMap.find(cur);
			if (pos != varMap.end()) {
				// found => return result
				return pos->second;
			}

			// create new variable substitution
			varMap.insert(std::make_pair(cur, TypeVariable::get(manager, "FV" + toString(++varCounter))));
			return mapElement(0, cur);
		}

		virtual IntTypeParam mapParam(const IntTypeParam& param) {
			// search for parameter ...
			auto pos = paramMap.find(param);
			if (pos != paramMap.end()) {
				// found => return result
				return pos->second;
			}

			// create new variable substitution
			paramMap.insert(std::make_pair(param, IntTypeParam::getVariableIntParam('a' + paramCounter)));
			paramCounter++;
			return mapParam(param);
		}

		void startNewType() {
			varMap.clear();
			paramMap.clear();
		}

	};

}


std::pair<TypePtr, TypePtr> makeTypeVariablesUnique(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB) {
	std::vector<TypePtr>&& list = makeTypeVariablesUnique(manager, toVector(typeA, typeB));
	return std::make_pair(list[0],list[1]);
}

std::vector<TypePtr> makeTypeVariablesUnique(NodeManager& manager, const vector<TypePtr>& types) {
	// check whether a check is actually necessary ..
	if (types.size() < 2) {
		// => no collisions possible - done!
		return types;
	}

	// replace type variables and variable int-type parameter

	// The replacer is handling the actual replacement by mapping variables to new values within types
	TypeVariableReplacer replacer(manager);

	// apply transformation
	std::vector<TypePtr> res;
	transform(types, std::back_inserter(res), [&](const TypePtr& type)->TypePtr {
		replacer.startNewType();
		return static_pointer_cast<const Type>(type->substitute(manager, replacer));
	});
	return res;
}




} // end namespace core
} // end namespace insieme


std::ostream& operator<<(std::ostream& out, const insieme::core::Substitution& substitution) {
	out << "{";
	out << join(",", substitution.getMapping(), [](std::ostream& out, const insieme::core::Substitution::Mapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	});
	out << "/";
	out << join(",", substitution.getIntTypeParamMapping(), [](std::ostream& out, const insieme::core::Substitution::IntTypeParamMapping::value_type& cur)->std::ostream& {
		return out << cur.first.toString() << "->" << cur.second.toString();
	});
	out << "}";
	return out;
}


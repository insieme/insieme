/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/types/match.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {
namespace types {

	namespace {

		/**
		 * Determines whether the structure of the value type matches the structure of the pattern type.
		 * The pattern type may only be a combination of type variables.
		 *
		 * @param value the value to be tested
		 * @param pattern the pattern to fit
		 */
		bool hasMatchingStructure(const TypePtr& value, const TypePtr& pattern) {

			// the simple cases first
			if (pattern.isa<TypeVariablePtr>()) return true;		// everything matches this

			// the other case is a generic type variable
			if (auto pattern_var = pattern.isa<GenericTypeVariablePtr>()) {

				// the value needs to be a type or a matching variable
				auto type = value.isa<GenericTypePtr>();
				auto var = value.isa<GenericTypeVariablePtr>();
				if (!type && !var) return false;

				// compare parameters
				TypeList paramsA = (type) ? type->getTypeParameterList() : var->getTypeParameterList();
				TypeList paramsB = pattern_var->getTypeParameterList();

				// -- handle variadic types --

				// remove those from the value
				if (!paramsA.empty() && (paramsA.back().isa<VariadicTypeVariablePtr>() || paramsA.back().isa<VariadicGenericTypeVariablePtr>())) {
					paramsA.pop_back();
				}

				// and expand them in the pattern
				if (!paramsB.empty()) {
					if (paramsB.back().isa<VariadicTypeVariablePtr>()) {
						paramsB.pop_back();
						TypeVariablePtr var = TypeVariable::get(value->getNodeManager(), "_");
						while (paramsB.size() < paramsA.size()) paramsB.push_back(var);
					} else if (auto vvar = paramsB.back().isa<VariadicGenericTypeVariablePtr>()) {
						paramsB.pop_back();
						GenericTypeVariablePtr var = GenericTypeVariable::get(value->getNodeManager(), "_", vvar->getTypeParameter());
						while (paramsB.size() < paramsA.size()) paramsB.push_back(var);
					}
				}

				// check equivalent structure
				return paramsA.size() == paramsB.size() && all(make_paired_range(paramsA, paramsB), [](const std::pair<TypePtr,TypePtr>& cur) {
					return hasMatchingStructure(cur.first, cur.second);
				});
			}

			// for concrete types, check the structure
			if (auto genPattern = pattern.isa<GenericTypePtr>()) {
				if (auto genValue = value.isa<GenericTypePtr>()) {
					return
						genValue->getName() == genPattern->getName() &&
						genValue->getTypeParameter().size() == genPattern->getTypeParameter().size() &&
						all(make_paired_range(genValue->getTypeParameter(), genPattern->getTypeParameter()), [](const std::pair<TypePtr,TypePtr>& cur) {
							return hasMatchingStructure(cur.first, cur.second);
						});
				}
			}

			// everything else is not a valid pattern, and thus not fitting
			return false;
		}


		bool matchTypes(NodeManager& manager, const TypePtr& type, const TypePtr& pattern, Substitution& cur);

		bool matchParameters(NodeManager& manager, const TypePtr& a, const TypePtr& b, Substitution& solution);

		bool matchTypes(NodeManager& manager, const TypesPtr& a, const TypesPtr& b, Substitution& solution);

		bool matchTypes(NodeManager& manager, const TypeList& a, const TypeList& b, Substitution& solution);


		bool matchParameters(NodeManager& manager, const TypePtr& a, const TypePtr& b, Substitution& solution) {
			// => check all child nodes
			return matchTypes(manager, analysis::getElementTypes(a), analysis::getElementTypes(b), solution);
		}

		bool matchTypes(NodeManager& manager, const TypesPtr& a, const TypesPtr& b, Substitution& solution) {
			return matchTypes(manager, a->getTypes(), b->getTypes(), solution);
		}

		bool matchTypes(NodeManager& manager, const TypeList& a, const TypeList& b, Substitution& solution) {
			bool OK = true;
			bool unmatchable = false;

			// get a mutable copy
			TypeList typeParamsA = a;
			TypeList typeParamsB = b;

			// -- variadic type variables --

			// remove tailing variadic type variable pointer from target parameters
			if (!typeParamsA.empty() && typeParamsA.back().isa<VariadicTypeVariablePtr>()) {
				typeParamsA.pop_back();
			}

			// adapt pattern parameter lists when facing variadic type variables
			if (!typeParamsB.empty() && typeParamsB.back().isa<VariadicTypeVariablePtr>()) {
				// drop variadic variable
				auto var = typeParamsB.back().as<VariadicTypeVariablePtr>();
				typeParamsB.pop_back();

				// check whether variadic variable has been resolved before
				TypeVariableList newExpansion;
				const TypeVariableList* expansion = solution[var];
				if (!expansion) {
					// create a new list
					int freshTypeVariableCounter = 0;

					// replace with fresh variables until size fits
					while (typeParamsA.size() > typeParamsB.size()) {
						// add fresh type variables at the end
						auto element = TypeVariable::get(manager, format("%s#%d", var->getVarName(), ++freshTypeVariableCounter));
						typeParamsB.push_back(element);
						newExpansion.push_back(element);
					}

					// remember expansion
					solution.addMapping(var, newExpansion);

				}
				else {

					// use previous list
					for (const auto& cur : *expansion) {
						typeParamsB.push_back(cur);
					}

				}

			}

			// -----------------------------

			// -- variadic generic type variables --

			// remove tailing variadic type variable pointer from target parameters
			if (!typeParamsA.empty() && typeParamsA.back().isa<VariadicGenericTypeVariablePtr>()) {
				typeParamsA.pop_back();
			}

			// adapt pattern parameter lists when facing variadic type variables
			if (!typeParamsB.empty() && typeParamsB.back().isa<VariadicGenericTypeVariablePtr>()) {
				// drop variadic variable
				auto var = typeParamsB.back().as<VariadicGenericTypeVariablePtr>();
				typeParamsB.pop_back();

				// check whether variadic variable has been resolved before
				GenericTypeVariableList newExpansion;
				const GenericTypeVariableList* expansion = solution[var];
				if (!expansion) {
					// create a new list
					int freshTypeVariableCounter = 0;

					// replace with fresh variables until size fits
					while (typeParamsA.size() > typeParamsB.size()) {
						// add fresh type variables at the end
						auto element = GenericTypeVariable::get(manager, format("%s#%d", var->getVarName(), ++freshTypeVariableCounter), var->getTypeParameter());
						typeParamsB.push_back(element);
						newExpansion.push_back(element);
					}

					// remember expansion
					solution.addMapping(var, newExpansion);

				}
				else {

					// use previous list
					for (const auto& cur : *expansion) {
						typeParamsB.push_back(cur);
					}

				}

			}

			// -----------------------------


			// check fitting parameter length
			if (typeParamsA.size() != typeParamsB.size()) {
				// => not matchable
				return unmatchable;
			}

			// match all parameters
			for (const auto& cur : make_paired_range(typeParamsA, typeParamsB)) {
				// of some step fails => matching fails
				if (!matchTypes(manager, cur.first, cur.second, solution)) return unmatchable;
			}

			// processing was successful
			return OK;
		}

		bool matchStructure(NodeManager& manager, const TypePtr& type, const TypePtr& pattern, const Substitution& res) {

			TypePtr a = type;
			TypePtr b = pattern;

			if (b.isa<TypeVariablePtr>()) b = res(b);

			if (auto genPattern = b.isa<GenericTypeVariablePtr>()) {
				b = res(b);
				if (auto genType = b.isa<GenericTypePtr>()) {
					b = GenericType::get(manager, genType->getName(), genPattern->getTypeParameter());
				}
			}

			return hasMatchingStructure(a, b);
		}


		bool matchTypes(NodeManager& manager, const TypePtr& type, const TypePtr& pattern, Substitution& res) {
			static const bool DEBUG = false;

			bool OK = true;
			bool unmatchable = false;

			if (DEBUG) std::cout << "\nMatching " << *type << " with " << *pattern << ", current substitution: " << res << "\n";

			TypePtr a = type;  // the value
			TypePtr b = res.applyTo(pattern); // the pattern

			if (DEBUG) std::cout << "Matching " << *a << " with " << *b << "\n";


			const NodeType typeOfA = a->getNodeType();
			const NodeType typeOfB = b->getNodeType();

			// 1) if (a == b) ignore pair ..
			if (*a == *b) { return OK; }

			// 2) test whether on the B side there is a variable
			if (typeOfB == NT_TypeVariable) {
				// matching: map variable to value
				auto var = b.as<TypeVariablePtr>();

				// convert current pair into substitution
				Substitution mapping(var, a);

				// combine current mapping with overall result
				res = Substitution::compose(manager, res, mapping);
				return OK;
			}
			if (typeOfB == NT_GenericTypeVariable) {

				// matching: map variable to value
				auto asType = a.isa<GenericTypePtr>();
				auto asVar = a.isa<GenericTypeVariablePtr>();
				if (!asType && !asVar) return unmatchable;

				// check whether variable should be skipped
				auto var = b.as<GenericTypeVariablePtr>();

				// convert current pair into substitution
				Substitution mapping(var, a);

				// combine current mapping with overall result
				res = Substitution::compose(manager, res, mapping);

				// process parameters
				return matchParameters(manager, a, b, res);
			}

			// 3) handle variables on left hand side ...
			if (typeOfA == NT_TypeVariable || typeOfA == NT_GenericTypeVariable) {
				// generic part in value => not matchable
				return unmatchable;
			}

			// 4) unlimited variadic variables
			if (typeOfA == NT_VariadicTypeVariable || typeOfB == NT_VariadicTypeVariable ||
				typeOfA == NT_VariadicGenericTypeVariable || typeOfB == NT_VariadicGenericTypeVariable) {
				// unbounded type variables => not matchable
				return unmatchable;
			}

			// 5) function types / generic types / tuples / structs / unions / recursive types
			if (typeOfA != typeOfB) {
				// => not matchable
				return unmatchable;
			}

			// handle recursive types (special treatment)
			if (typeOfA == NT_TagType) {

				// peel recursive types until no longer recursive
				TagTypePtr ta = a.as<TagTypePtr>();
				TagTypePtr tb = b.as<TagTypePtr>();

				if (ta.isRecursive()) {
					return matchTypes(manager, ta.peel(), b, res);
				}

				if (tb.isRecursive()) {
					return matchTypes(manager, a, tb.peel(), res);
				}

				// check for same kind of record
				if (ta->getRecord()->getNodeType() != tb->getRecord()->getNodeType()) {
					// e.g. structs and unions can not be matched
					return unmatchable;
				}

				// add pairs of field types to matched
				FieldsPtr fa = ta->getFields();
				FieldsPtr fb = tb->getFields();

				// check number of fields
				if (fa.size() != fb.size()) {
					return unmatchable;
				}

				// for all pairs of fields ...
				for (const auto& cur : make_paired_range(fa, fb)) {
					// check name equivalence
					if (*cur.first->getName() != *cur.second->getName()) {
						return unmatchable;
					}
					// match field types
					if (!matchTypes(manager, cur.first->getType(), cur.second->getType(), res)) return unmatchable;
				}

				// also consider parent types
				if (ta->isStruct()) {
					ParentsPtr pa = ta->getStruct()->getParents();
					ParentsPtr pb = tb->getStruct()->getParents();

					if (pa.size() != pb.size()) return unmatchable;

					// for all pairs of parents ...
					for (const auto& cur : make_paired_range(pa, pb)) {
						// check type of inheritance
						if (cur.first->isVirtual() != cur.second->isVirtual()) return unmatchable;
						if (cur.first->getAccessSpecifier() != cur.second->getAccessSpecifier()) return unmatchable;
						// match parent types
						if (!matchTypes(manager, cur.first->getType(), cur.second->getType(), res)) return unmatchable;
					}
				}

				// that's it
				return OK;
			}

			// => check family of generic type
			if (typeOfA == NT_GenericType) {
				const GenericTypePtr& genericTypeA = static_pointer_cast<const GenericType>(a);
				const GenericTypePtr& genericTypeB = static_pointer_cast<const GenericType>(b);

				// check family names
				if (*genericTypeA->getName() != *genericTypeB->getName()) { return unmatchable; }
			}

			// => check function kind and instantiation types
			if (typeOfA == NT_FunctionType) {
				const FunctionTypePtr& funTypeA = a.as<FunctionTypePtr>();
				const FunctionTypePtr& funTypeB = b.as<FunctionTypePtr>();

				// check function kind
				if (funTypeA->getKind() != funTypeB->getKind()) {
					if(!(funTypeA->getKind() == FK_PLAIN && funTypeB->getKind() == FK_CLOSURE)) return unmatchable;
				}

				// process parameter first
				if (!matchTypes(manager, funTypeA->getParameterTypes(), funTypeB->getParameterTypes(), res)) return unmatchable;

				// then return type
				if (!matchTypes(manager, funTypeA->getReturnType(), funTypeB->getReturnType(), res)) return unmatchable;

				// handle instantiation types
				TypeList listA = funTypeA->getInstantiationTypeList();
				TypeList listB = funTypeB->getInstantiationTypeList();

				// remove variadic type variables those from the value side
				if (!listA.empty() && (listA.back().isa<VariadicTypeVariablePtr>() || listA.back().isa<VariadicGenericTypeVariablePtr>())) {
					listA.pop_back();
				}

				// and expand them in the pattern
				if (!listB.empty()) {
					if (auto vvar = listB.back().isa<VariadicTypeVariablePtr>()) {
						listB.pop_back();
						if (auto expanded = res[vvar]) {
							for (const auto& cur : *expanded) {
								listB.push_back(cur);
							}
						} else {
							TypeVariablePtr var = TypeVariable::get(manager, "_");
							while (listB.size() < listA.size()) listB.push_back(var);
						}
					} else if (auto vvar = listB.back().isa<VariadicGenericTypeVariablePtr>()) {
						listB.pop_back();
						GenericTypeVariablePtr var = GenericTypeVariable::get(manager, "_", vvar->getTypeParameter());
						if (auto expanded = res[vvar]) {
							for (const auto& cur : *expanded) {
								listB.push_back(cur);
							}
						} else {
							while (listB.size() < listA.size()) listB.push_back(var);
						}
					}
				}

				// check whether the initializer lists are matching
				return listA.size() == listB.size() && all(make_paired_range(listA, listB), [&](const std::pair<TypePtr, TypePtr>& cur) {
					return matchStructure(manager, cur.first, cur.second, res);
				});
			}

			// for the rest: process type parameters
			return matchParameters(manager, a, b, res);
		}


	} // end namespace



	SubstitutionOpt match(NodeManager& manager, const TypePtr& type, const TypePtr& pattern) {
		Substitution solution;
		auto match = matchTypes(manager, type, pattern, solution);
		return match ? boost::optional<Substitution>(solution) : boost::optional<Substitution>();
	}

	bool isMatchable(const TypePtr& type, const TypePtr& pattern) {
		return match(type.getNodeManager(), type, pattern);
	}

} // end namespace types
} // end namespace core
} // end namespace insieme

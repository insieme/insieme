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

#include "insieme/core/types/match.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {
namespace types {

	namespace {

		/**
		 * This method implements the actual algorithm for matching types.
		 *
		 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
		 * @param list the list of pairs of types to be matched. The list will be altered during the computation.
		 * @return the resulting matching or an uninitialized value if the types couldn't be matched.
		 */
		SubstitutionOpt computeMapping(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list) {
			typedef std::pair<TypePtr, TypePtr> Pair;

			// create result
			Substitution res;
			boost::optional<Substitution> unmatchable;

			while(!list.empty()) {
				// get current element
				Pair cur = list.front();
				list.pop_front();

				TypePtr a = cur.first;  // the value
				TypePtr b = cur.second; // the pattern

				const NodeType typeOfA = a->getNodeType();
				const NodeType typeOfB = b->getNodeType();

				// 1) if (a == b) ignore pair ..
				if(*a == *b) { continue; }

				// 2) test whether on the B side there is a variable
				if(typeOfB == NT_TypeVariable) {
					// matching: map variable to value

					// convert current pair into substitution
					Substitution mapping(b.as<TypeVariablePtr>(), a);

					// combine current mapping with overall result
					res = Substitution::compose(manager, res, mapping);
					continue;
				}

				// 3) handle variables on left hand side ...
				if(typeOfA == NT_TypeVariable) {
					// generic part in value => not matchable
					return unmatchable;
				}

				// 4) function types / generic types / tuples / structs / unions / recursive types
				if(typeOfA != typeOfB) {
					// => not matchable
					return unmatchable;
				}

				// handle recursive types (special treatment)
				if(typeOfA == NT_TagType) {

					// peel recursive types until no longer recursive
					TagTypePtr ta = a.as<TagTypePtr>();
					TagTypePtr tb = b.as<TagTypePtr>();

					if (ta.isRecursive()) {
						list.push_back({ta.peel(), b});
						continue;
					}

					if (tb.isRecursive()) {
						list.push_back({a, tb.peel()});
						continue;
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
					for(const auto& cur : make_paired_range(fa,fb)) {
						// check name equivalence
						if (*cur.first->getName() != *cur.second->getName()) {
							return unmatchable;
						}
						// unify field types
						list.push_back({cur.first->getType(), cur.second->getType()});
					}

					// also consider parent types
					if (ta->isStruct()) {
						ParentsPtr pa = ta->getStruct()->getParents();
						ParentsPtr pb = tb->getStruct()->getParents();

						if (pa.size() != pb.size()) return unmatchable;

						// for all pairs of parents ...
						for(const auto& cur : make_paired_range(pa,pb)) {
							// check type of inheritance
							if (cur.first->isVirtual() != cur.second->isVirtual()) return unmatchable;
							if (cur.first->getAccessSpecifier() != cur.second->getAccessSpecifier()) return unmatchable;
							// unify parent types
							list.push_back({cur.first->getType(), cur.second->getType()});
						}
					}

					// that's it
					continue;
				}

				// => check family of generic type
				if(typeOfA == NT_GenericType) {
					const GenericTypePtr& genericTypeA = static_pointer_cast<const GenericType>(a);
					const GenericTypePtr& genericTypeB = static_pointer_cast<const GenericType>(b);

					// check family names
					if(*genericTypeA->getName() != *genericTypeB->getName()) { return unmatchable; }

					// check same number of type parameters
					if(genericTypeA->getTypeParameter().size() != genericTypeB->getTypeParameter().size()) { return unmatchable; }
				}

				// => check all child nodes
				auto typeParamsA = analysis::getElementTypes(a);
				auto typeParamsB = analysis::getElementTypes(b);
				if(typeParamsA.size() != typeParamsB.size()) {
					// => not matchable
					return unmatchable;
				}

				// add pairs of children to list to be processed
				list.insert(list.end(), make_paired_iterator(typeParamsA.begin(), typeParamsB.begin()),
				            make_paired_iterator(typeParamsA.end(), typeParamsB.end()));
			}

			// done
			return boost::optional<Substitution>(res);
		}

	} // end namespace


	SubstitutionOpt match(NodeManager& manager, const TypePtr& type, const TypePtr& pattern) {
		std::list<std::pair<TypePtr, TypePtr>> list;
		list.push_back(std::make_pair(type, pattern));
		return computeMapping(manager, list);
	}

	bool isMatchable(const TypePtr& type, const TypePtr& pattern) {
		return match(type.getNodeManager(), type, pattern);
	}

} // end namespace types
} // end namespace core
} // end namespace insieme

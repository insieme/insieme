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
			 * The type variable mapping constituting the substitution to be wrapped by this instance.
			 */
			const Substitution::Mapping& mapping;

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
			    : manager(manager), mapping(substitution.getMapping()), root(root) {};

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
					auto typeVars = analysis::getTypeVariablesBoundBy(element.as<ExpressionPtr>()->getType().as<FunctionTypePtr>());

					// compute reduced type variable substitution
					Substitution sub;
					for(const auto& cur : mapping) {
						if (!typeVars.contains(cur.first)) {
							sub.addMapping(cur.first, cur.second);
						}
					}

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

				// quick check - only variables are substituted
				auto currentType = element->getNodeType();
				if(currentType != NT_TypeVariable) {

					// replace base
					auto res = element->substitute(manager, *this);

					// move annotations
					transform::utils::migrateAnnotations(element, res);

					// done
					return res;
				}

				// check type
				assert_eq(NT_TypeVariable, currentType);

				// lookup current variable within the mapping
				auto pos = mapping.find(static_pointer_cast<const TypeVariable>(element));
				if(pos != mapping.end()) {
					// found! => replace
					return (*pos).second;
				}

				// not found => return current node
				// (since nothing within a variable node may be substituted)
				return element;
			}
		};

	} // end anonymous namespace


	Substitution::Substitution(const TypeVariablePtr& var, const TypePtr& type) {
		mapping.insert(std::make_pair(var, type));
	};

	NodePtr Substitution::applyTo(NodeManager& manager, const NodePtr& node) const {
		// shortcut for empty substitutions
		if (empty()) return node;
		// perform substitution
		SubstitutionMapper mapper(manager, *this, node);
		return mapper.map(0, node);
	}

	void Substitution::addMapping(const TypeVariablePtr& var, const TypePtr& type) {
		auto element = std::make_pair(var, type);
		auto res = mapping.insert(element);
		if(!res.second) {
			mapping.erase(var);
			res = mapping.insert(element);
			assert_true(res.second) << "Insert was not successful!";
		}
	}

	bool Substitution::containsMappingFor(const TypeVariablePtr& var) const {
		return mapping.find(var) != mapping.end();
	}

	void Substitution::remMappingOf(const TypeVariablePtr& var) {
		mapping.erase(var);
	}

	std::ostream& Substitution::printTo(std::ostream& out) const {
		out << "{";
		out << join(",", mapping, [](std::ostream& out, const Mapping::value_type& cur) { out << *cur.first << "->" << *cur.second; });
		out << "}";
		return out;
	}

	Substitution Substitution::compose(NodeManager& manager, const Substitution& a, const Substitution& b) {
		typedef Substitution::Mapping::value_type Entry;

		// copy substitution a
		Substitution res(a);

		// apply substitution b to all mappings in a
		for_each(res.mapping, [&manager, &b](Entry& cur) { cur.second = b.applyTo(manager, cur.second); });

		// add remaining mappings of b
		Substitution::Mapping& resMapping = res.mapping;
		for_each(b.mapping, [&resMapping](const Entry& cur) {
			if(resMapping.find(cur.first) == resMapping.end()) { resMapping.insert(cur); }
		});

		return res;
	}


} // end namespace types
} // end namespace core
} // end namespace insieme

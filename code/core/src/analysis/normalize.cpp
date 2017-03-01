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
#include "insieme/core/analysis/normalize.h"

#include <set>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/utils/debug/backtrace.h"

namespace insieme {
namespace core {
namespace analysis {


	namespace {

		/**
		 * An annotation attached to all normalized nodes referencing the normalized version.
		 * This annotation is used to cache results and to prevent normalizing code fragments
		 * multiple times.
		 */
		class NormalizeAnnotation : public NodeAnnotation {
		  public:
			/**
			 * The name of this annotation.
			 */
			const static string NAME;

			/**
			 * The KEY used to register this annotation.
			 */
			const static utils::StringKey<NormalizeAnnotation> KEY;

		  private:
			/**
			 * A reference to the normalized version of the annotated node.
			 */
			NodePtr normalized;

		  public:
			/**
			 * A constructor for this annotation accepting the normalized version of
			 * the node this annotation is attached to.
			 */
			NormalizeAnnotation(const NodePtr& normalized) : normalized(normalized) {}

			/**
			 * Obtains a pointer to the Key associated to this annotation class.
			 */
			virtual const utils::AnnotationKeyPtr getKey() const {
				return &KEY;
			}

			/**
			 * Obtains a reference to the name of this annotation.
			 */
			virtual const std::string& getAnnotationName() const {
				return NAME;
			}

			/**
			 * Obtains a reference to the associated normalized value.
			 */
			NodePtr getNormalized() const {
				return normalized;
			}

			/**
			 * Checks whether this annotation is equivalent to the given annotation. The default
			 * implementation returns true if and only if the given object is the same instance
			 * (object identity).
			 *
			 * @param other the annotation to be compared with
			 * @return true if equivalent, false otherwise.
			 */
			virtual bool operator==(const Annotation& other) const {
				// check for object identity
				if(this == &other) { return true; }

				// checke type of other annotation
				const NormalizeAnnotation* annotation = dynamic_cast<const NormalizeAnnotation*>(&other);
				if(!annotation) { return false; }

				// compare normalized representation
				return *normalized == *annotation->normalized;
			}

			/**
			 * Enables migrating this annotation to the given target node.
			 */
			virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
				if(*before != *after || before == after) {
					return false; // no migration
				}

				// it is migrated but not altered => fine
				NodePtr newNormalized = (before == normalized) ? after : after->getNodeManager().get(normalized);
				after.addAnnotation(std::make_shared<NormalizeAnnotation>(newNormalized));
				return true;
			};

			/**
			 * Migrates this annotation to a cloned version of the annotated node.
			 */
			virtual void clone(const NodeAnnotationPtr& ptr, const NodePtr& copy) const {
				// simply copy annotation
				copy.addAnnotation(std::make_shared<NormalizeAnnotation>(copy.getNodeManager().get(normalized)));
			}
		};


		const string NormalizeAnnotation::NAME = "NormalizeAnnotation";
		const utils::StringKey<NormalizeAnnotation> NormalizeAnnotation::KEY(NAME);

		/**
		 * The NodeMapping used to conduct the replacement operation transforming
		 * a given node into a normalized instance.
		 */
		class Normalizer : public transform::CachedNodeMapping {
			/**
			 * The map mapping variables to their normalized counterpart.
			 */
			VariableMap vars;

		  public:
			/**
			 * Creates a new instance of this Normalizer executing the given
			 * variable normalization operation.
			 *
			 * @param vars the map mapping every variable within a scope to their normalized counterpart.
			 */
			Normalizer(const VariableMap& varMap) : vars(varMap) { };

			/**
			 * Conducts the actual replacement.
			 */
			const NodePtr resolveElement(const NodePtr& cur) {
				// fix variables
				NodeType type = cur->getNodeType();
				if(type == NT_Variable) {
					// replace with normalized variables
					auto pos = vars.find(cur.as<VariablePtr>());
					if(pos != vars.end()) { return pos->second->substitute(cur.getNodeManager(), *this); }
					// it is a free variable, no replacement!
					return cur->substitute(cur.getNodeManager(), *this);
				}

				// invoke normalization recursively on lambda expressions
				if(type == NT_LambdaExpr) {
					auto freeVarsEmpty = getFreeVariables(cur).empty();
					if(freeVarsEmpty) return normalize(cur); // entering new scope
				}

				// invoke this normalizer on anything else
				return cur->substitute(cur.getNodeManager(), *this);
			}
		};


		/**
		 * The internal implementation of the normalization not considering any
		 * normalization-annotations.
		 *
		 * The normalizer is "normalizing" the variables within individually
		 * regions not exhibiting any free variables. Those regions are identified
		 * on the fly during the normalization process.
		 *
		 * @param node the node to be normalized
		 * @return the normalized code fragment
		 */
		NodePtr normalizeInternal(const NodePtr& node) {
			NodeManager& manager = node.getNodeManager();
			IRBuilder builder(manager);
			
			// get set of free variables
			VariableList freeVarList = getFreeVariables(node);

			// provide a mechanism to generate variable substitutions
			int index = 0;
			auto getNextVariable = [&](const TypePtr& type) -> VariablePtr {
				VariablePtr res;
				do {
					res = builder.variable(type, index++);
				} while(contains(freeVarList, res, [](const VariablePtr& a, const VariablePtr& b) {
					return a->getId() == b->getId();
				})); // avoid id-collisions with free variables
				return res;
			};

			// collect all free variables in the top-level scope
			VariableMap varMap;
			visitDepthFirstOncePrunable(node, [&](const NodePtr& cur) {
				NodeType type = cur->getNodeType();
				switch(type) {
				case NT_Variable: {
					// check whether variable is free
					VariablePtr var = cur.as<VariablePtr>();
					if(contains(freeVarList, var)) {
						break; // do not touch free variables
					}

					// register variable to be replaced
					varMap[var] = getNextVariable(var->getType());
					// this might not be the best idea, as we could get unwanted names on nodes (e.g. all int v0 have the same name)
					core::transform::utils::migrateAnnotations(var, varMap[var]);
					break;
				}
				case NT_LambdaExpr: {
					return cur != node && getFreeVariables(cur).empty(); // prune here
				}
				default: break;
				}
				// descend further
				return false;
			}, true);
			
			// special handling for LambdaExpression (to break recursive loop)
			Normalizer normalizer(varMap);
			if(node->getNodeType() == NT_LambdaExpr) { return node->substitute(manager, normalizer); }

			// use recursive normalizer and be done
			auto result = normalizer.map(node);
			return result;
		}
	}


	NodePtr normalize(const NodePtr& node) {		
		// handle null pointer
		if(!node) { return node; }

		// short cut for constructs not including any variables
		if(node->getNodeType() == NT_Literal) { return node; }

		// check whether it has already been normalized
		if(auto annotation = node.getAnnotation(NormalizeAnnotation::KEY)) { return annotation->getNormalized(); }

		// normalize node using the internal implementation
		NodePtr res = normalizeInternal(node);

		// add annotations
		auto annotation = std::make_shared<NormalizeAnnotation>(res);
		node.addAnnotation(annotation);
		res.addAnnotation(annotation);

		// done - return result
		return res;
	}

	NodeAddress normalize(const NodeAddress& node) {
		return node.switchRoot(normalize(node.getRootNode()));
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

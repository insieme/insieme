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

#include "insieme/frontend/cilk/cilk_sema.h"

#include "insieme/frontend/cilk/cilk_annotation.h"
#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace frontend {
namespace cilk {

	namespace {

		class Cilkifyer : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;

		public:

			Cilkifyer(core::NodeManager& manager) : manager(manager) {}


			virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				// skip all types
				if (ptr->getNodeCategory() == core::NC_Type) {
					return manager.get(ptr);
				}

				// process child-nodes first
				core::NodePtr res = ptr->substitute(manager, *this);

				// only interested in compound statements
				if (res->getNodeType() != core::NT_CompoundStmt) return res;

				// check statements one by one
				bool changed = false;
				core::StatementList statements = res.as<core::CompoundStmtPtr>()->getStatements();
				core::StatementList newStmts;
				for(core::StatementPtr cur : statements) {

					// spawn => parallel(job(...))
					if (cur->hasAttachedValue<CilkSpawnMarker>()) {
						changed = true;

						// remove marker if present
						cur = stripMarker(cur);

						// handle declarations
						if (cur->getNodeType() == core::NT_DeclarationStmt) {
							auto decl = cur.as<core::DeclarationStmtPtr>();

							core::TypePtr type = decl->getVariable()->getType();
							assert(type->getNodeType() == core::NT_RefType && "can only handle reference types!");

							core::IRBuilder builder(manager);

							// initialize the variable using an undefined
							newStmts.push_back(builder.declarationStmt(decl.getVariable(), builder.refVar(builder.undefined(type.as<core::RefTypePtr>()->getElementType()))));

							// assign the value
							core::ExpressionPtr init = decl->getInitialization();
							assert(core::analysis::isCallOf(init, manager.getLangBasic().getRefVar()));
							init = init.as<core::CallExprPtr>()->getArgument(0);
							newStmts.push_back(builder.parallel(builder.assign(decl.getVariable(), init),1));

							// done
							continue;
						}

						// all the rest
						cur = core::IRBuilder(manager).parallel(stripMarker(cur), 1);
					}

					// sync => sneak in an extra merge all
					if (cur->hasAttachedValue<CilkSyncMarker>()) {
						changed = true;
						newStmts.push_back(core::IRBuilder(manager).mergeAll());
					}

					// take statement as it is
					newStmts.push_back(cur);
				}

				// return result
				if (!changed) return res;
				return core::CompoundStmt::get(manager, newStmts);
			}

		public:

			core::StatementPtr stripMarker(const core::StatementPtr& node) const {
				if (node->getNodeType() == core::NT_MarkerExpr) {
					return stripMarker(node.as<core::MarkerExprPtr>()->getSubExpression());
				}
				if (node->getNodeType() == core::NT_MarkerStmt) {
					return stripMarker(node.as<core::MarkerStmtPtr>()->getSubStatement());
				}
				return node;
			}

		};

	}

	tu::IRTranslationUnit applySema(const tu::IRTranslationUnit& unit, core::NodeManager& mgr) {

		// copy translation unit
		tu::IRTranslationUnit res = unit;

		// and update functions - that's all
		Cilkifyer cilkifyer(mgr);
		for(auto& cur : res.getFunctions()) {
			cur.second = cilkifyer.map(cur.second);
		}

		// done
		return res;
	}

} // namespace cilk
} // namespace frontend
} // namespace insieme

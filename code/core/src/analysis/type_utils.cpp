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

#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace core {
namespace analysis {

	bool hasFreeTypeVariables(const TypePtr& type) {

		// if it is not a type, there are no type variables
		if (!type) return false;

		struct HasFreeTypeVariableVisitor : public IRVisitor<bool, Pointer, NodeSet&> {

			HasFreeTypeVariableVisitor() : IRVisitor<bool, Pointer, NodeSet&>(true) {}

			bool visitTypeVariable(const TypeVariablePtr& cur, NodeSet& knownVariables) {
				return !contains(knownVariables, cur);
			}

			bool visitRecTypeDefinition(const RecTypeDefinitionPtr& def, NodeSet& knownVariables) {
				NodeSet local = knownVariables;
				for(const RecTypeBindingPtr& binding : def) {
					local.insert(binding->getVariable());
				}
				return visitNode(def, local);
			}

			bool visitFunctionType(const FunctionTypePtr& cur, NodeSet& knownVariables) {
				return false;	// function types are binding their free type variables
			}

			bool visitRecType(const RecTypePtr& cur, NodeSet& knownVariables) {
				return visit(cur->getDefinition(), knownVariables);
			}

			bool visitNode(const NodePtr& cur, NodeSet& knownVariables) {
				return any(cur.getChildList(), [&](const NodePtr& cur)->bool { return this->visit(cur, knownVariables); });
			}

		};

		NodeSet tmp;
		return HasFreeTypeVariableVisitor().visit(type, tmp);

	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

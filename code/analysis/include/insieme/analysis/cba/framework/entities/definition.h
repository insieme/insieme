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

#pragma once

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/hash_utils.h"

#include "insieme/analysis/cba/framework/entities/data_path.h"

namespace insieme {
namespace analysis {
namespace cba {

	// an entity representing a value assigned to a location (variable in the standard DFA world)
	template<typename Context>
	class Definition
		: public utils::Printable,
		  public utils::HashableImmutableData<Definition<Context>>,
		  public boost::equality_comparable<Definition<Context>>,
		  public boost::partially_ordered<Definition<Context>> {

		/**
		 * The expression conducting the assignment (realizing the definition)
		 */
		core::CallExprInstance definitionPoint;

		/**
		 * The context when triggering the assignment operation.
		 */
		Context definitionContext;

	public:

		Definition()
			: utils::HashableImmutableData<Definition<Context>>(combineHashes(core::CallExprInstance(), Context())) {}

		Definition(const core::CallExprInstance& expr, const Context& ctxt = Context())
			: utils::HashableImmutableData<Definition<Context>>(combineHashes(expr, ctxt)),
			  definitionPoint(expr),
			  definitionContext(ctxt) {}

		const core::CallExprInstance& getDefinitionPoint() const {
			return definitionPoint;
		}

		const Context& getContext() const {
			return definitionContext;
		}

		bool operator==(const Definition<Context>& other) const {
			if (this == &other) return true;
			if (this->hash() != other.hash()) return false;
			return definitionPoint == other.definitionPoint && definitionContext == other.definitionContext;
		}

		bool operator<(const Definition<Context>& other) const {
			return definitionPoint < other.definitionPoint ||
					(definitionPoint == other.definitionPoint && definitionContext < other.definitionContext);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "(" << definitionPoint << "," << definitionContext << ")";
		}
	};

	inline bool isDefinition(const core::StatementInstance& address) {
		core::StatementPtr stmt = address;

		// only ref.assign calls are definitions
		return core::analysis::isCallOf(stmt, stmt->getNodeManager().getLangBasic().getRefAssign());
	}

	template<typename Context>
	Definition<Context> getDefinition(const core::StatementInstance& def, const Context& ctxt) {

		// make sure it is a valid definition target
		assert(isDefinition(def));

		// create the definition instance
		return Definition<Context>(def.as<core::CallExprInstance>(), ctxt);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

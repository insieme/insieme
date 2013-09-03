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

#include <type_traits>

#include "insieme/analysis/cba/framework/forward_decl.h"
#include "insieme/analysis/cba/framework/context.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/set_constraint/solver2.h"

namespace insieme {
namespace analysis {
namespace cba {

	// -------------------- Constraint Generator ---------------------------

	// the type used for lists of constraints
	typedef utils::set_constraint_2::Constraints Constraints;

	// declaration of utility function
	template<typename Context> bool isValidContext(CBA& cba, const Context& context);

	/**
	 * A base class for classes capable of lazily resolving set constraints while processing constraint
	 * based analysis.
	 *
	 * Essentially, a constraint resolver is an IR visitor generating for a given node (addressed by its
	 * address) and a call / thread context constraints. In general, every resolver is only supposed to
	 * generate in-constraints for the requested target set specified by the address and context parameter.
	 */
	template<typename Context>
	class ConstraintGenerator : public core::IRVisitor<void, core::Address, const Context&, Constraints&> {

		// a short-cut for the base class
		typedef core::IRVisitor<void, core::Address, const Context&, Constraints&> super;

		/**
		 * The base-implementation is preventing the same arguments to be processed multiple times.
		 */
		typedef std::tuple<core::NodeAddress, Context> Item;
		std::set<Item> processed;

	protected:

		/**
		 * The analysis context this resolver is working for. Every instance may only be utilized by
		 * a single CBA instance.
		 */
		CBA& cba;

	public:

		ConstraintGenerator(CBA& cba)
			: processed(), cba(cba) {}

		/**
		 * The main entry function for resolving constraints for the given node and context. Constraints
		 * will be added to the given list.
		 *
		 * @param node the node for which constraints shell be generated - details regarding the set covered
		 * 			by this resolver are implementation specific.
		 * @param ctxt the call context to be considered for the constraint generation
		 */
		void addConstraints(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints) {
			// just forward call to visit-process
			visit(node, ctxt, constraints);
		}

		/**
		 * Overrides the standard visit function of the super type and realizes the guard avoiding the
		 * repeated evaluation of identical argument types.
		 */
		virtual void visit(const core::NodeAddress& node, const Context& ctxt, Constraints& constraints) {

			// do not resolve the same nodes multiple times
			if (!processed.insert(Item(node,ctxt)).second) return;

			// filter out invalid contexts
			if (!isValidContext(cba, ctxt)) return;

			// for valid content => std procedure
			super::visit(node, ctxt, constraints);
		}

	protected:

		/**
		 * Provides access to the context
		 */
		CBA& getCBA() {
			return cba;
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

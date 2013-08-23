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

#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/entitiey.h"
//#include "insieme/analysis/cba/framework/basic_data_flow_constraint_resolver.h"
#include "insieme/analysis/cba/utils/cba_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- references ---------------

	template<typename Context> class ReferenceConstraintResolver;

	template<typename Context>
	const TypedSetType<Location<Context>,ReferenceConstraintResolver>& R() {
		static const TypedSetType<Location<Context>,ReferenceConstraintResolver> instance("R");
		return instance;
	}

	template<typename Context>
	const TypedSetType<Location<Context>,ReferenceConstraintResolver>& r() {
		static const TypedSetType<Location<Context>,ReferenceConstraintResolver> instance("r");
		return instance;
	}


	template<typename Context> class ReferenceConstraintResolver;


//	template<typename Context>
//	class ReferenceConstraintResolver : public BasicDataFlowConstraintResolver<Location<Context>,Context> {
//
//		typedef BasicDataFlowConstraintResolver<Location<Context>,Context> super;
//
//	public:
//
//		ReferenceConstraintResolver(CBA& context) : super(context, R, r) { };
//
//		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {
//
//			// and default handling
//			super::visitLiteral(literal, ctxt, constraints);
//
//			// only interested in memory location constructors
//			if (!isMemoryConstructor(literal)) return;
//
//			// add constraint literal \in R(lit)
//			auto value = context.getLocation(literal);
//			auto l_lit = context.getLabel(literal);
//
//			auto R_lit = context.getSet(R, l_lit, ctxt);
//			constraints.add(elem(value, R_lit));
//
//		}
//
//		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
//
//			// and default handling
//			super::visitCallExpr(call, ctxt, constraints);
//
//			// introduce memory location in some cases
//			if (!isMemoryConstructor(call)) return;
//
//			// add constraint location \in R(call)
//			auto value = context.getLocation(call);
//			auto l_lit = context.getLabel(call);
//
//			auto R_lit = context.getSet(R, l_lit, ctxt);
//			constraints.add(elem(value, R_lit));
//		}
//
//	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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

#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- simple constants ---------------

	template<typename Context> class ConstantConstraintGenerator;

	typedef DataAnalysisType<core::ExpressionPtr,ConstantConstraintGenerator> SimpleConstantSetType;
	extern const SimpleConstantSetType D;
	extern const SimpleConstantSetType d;

	template<typename Context>
	class ConstantConstraintGenerator : public BasicDataFlowConstraintGenerator<core::ExpressionPtr,SimpleConstantSetType,Context> {

		typedef BasicDataFlowConstraintGenerator<core::ExpressionPtr,SimpleConstantSetType,Context> super;

		CBA& cba;

	public:

		ConstantConstraintGenerator(CBA& cba)
			: super(cba, D, d), cba(cba) { };

		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLiteral(literal, ctxt, constraints);

			// not interested in functions
			if (literal->getType().isa<FunctionTypePtr>()) return;

			// add constraint literal \in C(lit)
			auto value = literal.as<ExpressionPtr>();
			auto l_lit = cba.getLabel(literal);

			auto D_lit = cba.getSet(D, l_lit, ctxt);
			constraints.add(elem(value, D_lit));

		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
			auto& base = call->getNodeManager().getLangBasic();

			// conduct std-procedure
			super::visitCallExpr(call, ctxt, constraints);

			// some special cases
			if (base.isIntArithOp(call->getFunctionExpr())) {

				// mark result as being unknown
				auto D_call = cba.getSet(D, cba.getLabel(call), ctxt);
				auto unknown = ExpressionPtr();
				constraints.add(elem(unknown, D_call));

			}

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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
#include "insieme/analysis/cba/framework/entities/callable.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	template<typename A, typename B, typename C> class DataFlowConstraintGenerator;

	// ----------------- inter-procedural control flow ------------------

	template<typename C> class ControlFlowConstraintGenerator;

	struct control_flow_analysis_data : public dependent_data_analysis<Callable, ControlFlowConstraintGenerator> {};
	struct control_flow_analysis_var  : public dependent_data_analysis<Callable, ControlFlowConstraintGenerator> {};

	extern const control_flow_analysis_data C;
	extern const control_flow_analysis_var  c;


	template<typename Context>
	class ControlFlowConstraintGenerator : public DataFlowConstraintGenerator<control_flow_analysis_data, control_flow_analysis_var, Context> {

		typedef DataFlowConstraintGenerator<control_flow_analysis_data, control_flow_analysis_var, Context> super;

		CBA& cba;

	public:

		ControlFlowConstraintGenerator(CBA& cba)
			: super(cba, C, c), cba(cba) { };

		using super::elem;

		void visitLiteral(const LiteralInstance& literal, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLiteral(literal, ctxt, constraints);

			// only interested in functions ...
			if (!literal->getType().isa<FunctionTypePtr>()) return;

			// add constraint: literal \in C(lit)
			auto value = Callable<Context>(literal);
			auto l_lit = cba.getLabel(literal);

			auto C_lit = cba.getSet(C, l_lit, ctxt);
			constraints.add(elem(value, C_lit));

		}

		void visitLambdaExpr(const LambdaExprInstance& lambda, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLambdaExpr(lambda, ctxt, constraints);

			// add constraint: lambda \in C(lambda)
			auto value = Callable<Context>(lambda);
			auto label = cba.getLabel(lambda);

			constraints.add(elem(value, cba.getSet(C, label, ctxt)));

			// TODO: handle recursions

		}

		void visitBindExpr(const BindExprInstance& bind, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitBindExpr(bind, ctxt, constraints);

			// add constraint: bind \in C(bind)
			auto value = Callable<Context>(bind, ctxt);
			auto label = cba.getLabel(bind);

			auto C_bind = cba.getSet(C, label, ctxt);
			constraints.add(elem(value, C_bind));

		}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

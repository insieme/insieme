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

#include "insieme/core/lang/basic.h"

#include "insieme/analysis/cba/analysis/analysis.h"
#include "insieme/analysis/cba/framework/basic_program_point_constraint_resolver.h"

namespace insieme {
namespace analysis {
namespace cba {


	template<typename Context, typename ElementSetType>
	class ImperativeInStateConstraintCollector;

	template<typename Context, typename ElementSetType>
	class ImperativeOutStateConstraintCollector;

	template<typename Context, typename ElementSetType>
	class ImperativeInStateConstraintCollector : public BasicInConstraintResolver<StateSetType, StateSetType,ImperativeInStateConstraintCollector<Context, ElementSetType>,Context> {

		typedef BasicInConstraintResolver<StateSetType, StateSetType,ImperativeInStateConstraintCollector<Context, ElementSetType>,Context> super;

		const ElementSetType& dataSet;

		// the one location this instance is working for
		Location<Context> location;

		CBA& cba;

	public:

		ImperativeInStateConstraintCollector(CBA& cba, const ElementSetType& dataSet, const Location<Context>& location)
			: super(cba, Sin, Sout, *this), dataSet(dataSet), location(location), cba(cba) {}

		void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			constraints.add(subset(s_in, s_out));

		}

		template<typename E>
		void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			if (ac == bc) {
				constraints.add(subsetIf(value, set, s_in, s_out));
			} else {
				auto pre = cba.getSet(pred, bc.callContext.back());
				constraints.add(subsetIf(ac.callContext.back(), pre, value, set, s_in, s_out));
			}
		}
	};


	template<typename Context, typename ElementSetType>
	class ImperativeOutStateConstraintCollector : public BasicOutConstraintResolver<StateSetType, StateSetType,ImperativeOutStateConstraintCollector<Context, ElementSetType>,Context> {

		typedef BasicOutConstraintResolver<StateSetType, StateSetType,ImperativeOutStateConstraintCollector<Context, ElementSetType>,Context> super;

		const ElementSetType& dataSet;

		// the one location this instance is working for
		Location<Context> location;

		CBA& cba;

	public:

		ImperativeOutStateConstraintCollector(CBA& cba, const ElementSetType& dataSet, const Location<Context>& location)
			: super(cba, Sin, Sout, *this), dataSet(dataSet), location(location), cba(cba) {
		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {
			const auto& base = call->getNodeManager().getLangBasic();

			// one special case: assignments
			auto fun = call.as<CallExprPtr>()->getFunctionExpr();
			if (base.isRefAssign(fun)) {

				// get some labels
				auto l_call = cba.getLabel(call);
				auto l_rhs = cba.getLabel(call[0]);
				auto l_lhs = cba.getLabel(call[1]);

				// ---- S_out of args => S_tmp of call (only if other location is possible)

				auto R_rhs = cba.getSet(R<Context>(), l_rhs, ctxt);
				auto S_out_rhs = cba.getSet(Sout, l_rhs, ctxt, location, dataSet);
				auto S_out_lhs = cba.getSet(Sout, l_lhs, ctxt, location, dataSet);
				auto S_tmp = cba.getSet(Stmp, l_call, ctxt, location, dataSet);
				constraints.add(subsetIfReducedBigger(R_rhs, location, 0, S_out_rhs, S_tmp));
				constraints.add(subsetIfReducedBigger(R_rhs, location, 0, S_out_lhs, S_tmp));

				// ---- combine S_tmp to S_out ...

				// add rule: loc \in R[rhs] => A[lhs] \sub Sout[call]
				auto A_value = cba.getSet(dataSet, l_lhs, ctxt);
				auto S_out = cba.getSet(Sout, l_call, ctxt, location, dataSet);
				constraints.add(subsetIf(location, R_rhs, A_value, S_out));

				// add rule: |R[rhs]\{loc}| > 0 => Stmp[call] \sub Sout[call]
				constraints.add(subsetIfReducedBigger(R_rhs, location, 0, S_tmp, S_out));

				// done
				return;
			}

			// everything else is treated using the default procedure
			super::visitCallExpr(call, ctxt, constraints);
		}


		void connectStateSets(const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			constraints.add(subset(s_in, s_out));

		}

		template<typename E>
		void connectStateSetsIf(const E& value, const TypedSetID<E>& set, const StateSetType& a, Label al, const Context& ac, const StateSetType& b, Label bl, const Context& bc, Constraints& constraints) const {

			// general handling - Sin = Sout

			// get Sin set		TODO: add context to locations
			auto s_in = cba.getSet(a, al, ac, location, dataSet);
			auto s_out = cba.getSet(b, bl, bc, location, dataSet);

			// state information entering the set is also leaving it
			constraints.add(subsetIf(value, set, s_in, s_out));
		}
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

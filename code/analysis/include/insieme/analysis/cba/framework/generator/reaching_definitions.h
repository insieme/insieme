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
#include "insieme/analysis/cba/framework/entities/definition.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- jobs ---------------

	template<typename C> class ReachingDefsInConstraintGenerator;
	template<typename C> class ReachingDefsOutConstraintGenerator;

	template<typename Context>
	const SetBasedAnalysisType<Definition<Context>,ReachingDefsInConstraintGenerator>& ReachingDefIn() {
		static const SetBasedAnalysisType<Definition<Context>,ReachingDefsInConstraintGenerator> instance("ReachingDefIn");
		return instance;
	}

	template<typename Context>
	const SetBasedAnalysisType<Definition<Context>,ReachingDefsOutConstraintGenerator>& ReachingDefOut() {
		static const SetBasedAnalysisType<Definition<Context>,ReachingDefsOutConstraintGenerator> instance("ReachingDefOut");
		return instance;
	}

	template<typename Context>
	class ReachingDefsInConstraintGenerator
			: public BasicInConstraintGenerator<
						SetBasedAnalysisType<Definition<Context>,ReachingDefsInConstraintGenerator>,
						SetBasedAnalysisType<Definition<Context>,ReachingDefsOutConstraintGenerator>,
						ReachingDefsInConstraintGenerator<Context>,
						Context
					> {

		typedef BasicInConstraintGenerator<
			SetBasedAnalysisType<Definition<Context>,ReachingDefsInConstraintGenerator>,
			SetBasedAnalysisType<Definition<Context>,ReachingDefsOutConstraintGenerator>,
			ReachingDefsInConstraintGenerator<Context>, Context
		> super;

		// the location this constraint generator is operating on
		Location<Context> loc;

		CBA& cba;

	public:

		ReachingDefsInConstraintGenerator(CBA& cba, const Location<Context>& loc)
			: super(cba, ReachingDefIn<Context>(), ReachingDefOut<Context>(), *this), cba(cba), loc(loc) { }

		virtual ~ReachingDefsInConstraintGenerator() {};

		template<typename SetTypeA, typename SetTypeB>
		void connectStateSets (
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					Constraints& constraints
				) const {

			auto A = cba.getSet(a, al, ac);
			auto B = cba.getSet(b, bl, bc);
			constraints.add(subset(A, B));
		}

		template<typename E, typename L, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIf (
					const E& value, const TypedValueID<L>& set,
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					Constraints& constraints
				) const {

			if (ac != bc) {
				auto pre = cba.getSet(pred, bc.callContext.back());
				auto A = cba.getSet(a, al, ac);
				auto B = cba.getSet(b, bl, bc);
				constraints.add(subsetIf(ac.callContext.back(), pre, value, set, A, B));
			} else {
				auto A = cba.getSet(a, al, ac);
				auto B = cba.getSet(b, bl, bc);
				constraints.add(subsetIf(value, set, A, B));
			}
		}

	};

	template<typename Context>
	class ReachingDefsOutConstraintGenerator
			: public BasicOutConstraintGenerator<
			  	  	  	SetBasedAnalysisType<Definition<Context>,ReachingDefsInConstraintGenerator>,
						SetBasedAnalysisType<Definition<Context>,ReachingDefsOutConstraintGenerator>,
						ReachingDefsOutConstraintGenerator<Context>,
			  	  	  	Context
			  	     > {

		typedef BasicOutConstraintGenerator<
			SetBasedAnalysisType<Definition<Context>,ReachingDefsInConstraintGenerator>,
			SetBasedAnalysisType<Definition<Context>,ReachingDefsOutConstraintGenerator>,
			ReachingDefsOutConstraintGenerator<Context>,
			Context
		> super;

		// the location this constraint generator is operating on
		Location<Context> loc;

		CBA& cba;

	public:

		ReachingDefsOutConstraintGenerator(CBA& cba, const Location<Context>& loc)
			: super(cba, ReachingDefIn<Context>(), ReachingDefOut<Context>(), *this), cba(cba), loc(loc) { }

		virtual ~ReachingDefsOutConstraintGenerator() {};

		template<typename SetTypeA, typename SetTypeB>
		void connectStateSets (
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					Constraints& constraints
				) const {

			auto A = cba.getSet(a, al, ac);
			auto B = cba.getSet(b, bl, bc);
			constraints.add(subset(A, B));
		}

		template<typename E, typename L, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIf (
					const E& value, const TypedValueID<L>& set,
					const SetTypeA& a, Label al, const Context& ac,
					const SetTypeB& b, Label bl, const Context& bc,
					Constraints& constraints
				) const {

			auto A = cba.getSet(a, al, ac);
			auto B = cba.getSet(b, bl, bc);
			constraints.add(subsetIf(value, set, A, B));
		}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

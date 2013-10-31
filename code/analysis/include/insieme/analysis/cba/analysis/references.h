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
#include "insieme/analysis/cba/framework/location.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/analysis/data_paths.h"

#include "insieme/analysis/cba/utils/cba_utils.h"
#include "insieme/analysis/cba/utils/constraint_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	// forward declarations
	template<typename Context> class DataPathConstraintGenerator;
	typedef DataAnalysisType<DataPath,DataPathConstraintGenerator> DataPathAnalysisType;
	extern const DataPathAnalysisType DP;
	extern const DataPathAnalysisType dp;

	// ----------------- references ---------------

	template<typename Context> class ReferenceConstraintGenerator;

	template<typename Context>
	const DataAnalysisType<Reference<Context>,ReferenceConstraintGenerator>& R() {
		static const DataAnalysisType<Reference<Context>,ReferenceConstraintGenerator> instance("R");
		return instance;
	}

	template<typename Context>
	const DataAnalysisType<Reference<Context>,ReferenceConstraintGenerator>& r() {
		static const DataAnalysisType<Reference<Context>,ReferenceConstraintGenerator> instance("r");
		return instance;
	}



	template<typename Context>
	class ReferenceConstraintGenerator : public BasicDataFlowConstraintGenerator<Reference<Context>,DataAnalysisType<Reference<Context>,ReferenceConstraintGenerator>, Context> {

		typedef BasicDataFlowConstraintGenerator<Reference<Context>,DataAnalysisType<Reference<Context>,ReferenceConstraintGenerator>, Context> super;

		CBA& cba;

		const core::lang::BasicGenerator& base;

	public:

		ReferenceConstraintGenerator(CBA& cba) : super(cba, R<Context>(), r<Context>()), cba(cba), base(cba.getRoot().getNodeManager().getLangBasic()) { };

		using super::elem;

		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLiteral(literal, ctxt, constraints);

			// only interested in memory location constructors
			if (!isMemoryConstructor(literal)) return;

			// add constraint literal \in R(lit)
			auto value = cba.getLocation<Context>(literal, ctxt);
			auto l_lit = cba.getLabel(literal);

			auto R_lit = cba.getSet(R<Context>(), l_lit, ctxt);
			constraints.add(elem(value, R_lit));

		}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitCallExpr(call, ctxt, constraints);

			// introduce memory location in some cases
			if (isMemoryConstructor(call)) {

				// add constraint location \in R(call)
				auto value = cba.getLocation<Context>(call, ctxt);
				auto l_lit = cba.getLabel(call);

				auto R_lit = cba.getSet(R<Context>(), l_lit, ctxt);
				constraints.add(elem(value, R_lit));

				// done
				return;
			}

			// check whether the operation is a narrow or expand (data path operation)
			const auto& fun = call->getFunctionExpr();
			if (base.isRefNarrow(fun)) {

				// obtain involved sets
				auto R_in  = cba.getSet(R<Context>(), call[0], ctxt);	// the input reference
				auto DP_in = cba.getSet(DP, call[1], ctxt);				// the data path values
				auto R_out = cba.getSet(R<Context>(), call, ctxt);		// the resulting context

				// add constraint linking in and out values
				constraints.add(combine(this->getValueManager(), R_in, DP_in, R_out,
						[](const Reference<Context>& ref, const DataPath& path)->Reference<Context> {
							return Reference<Context>(ref.getLocation(), ref.getDataPath() << path);
						}
				));

			}
		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

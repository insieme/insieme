/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/cba/framework/entities/data_path.h"
#include "insieme/analysis/cba/framework/entities/data_value.h"
#include "insieme/analysis/cba/framework/entities/formula.h"

#include "insieme/analysis/cba/analysis/simple_constant.h"
#include "insieme/analysis/cba/analysis/arithmetic.h"

#include "insieme/analysis/cba/utils/cba_utils.h"
#include "insieme/analysis/cba/utils/constraint_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- references ---------------

	struct simple_constant_analysis_data;
	struct simple_constant_analysis_var;

	extern const simple_constant_analysis_data D;
	extern const simple_constant_analysis_var  d;

	template<typename C> class DataPathConstraintGenerator;

	struct data_path_analysis_data : public data_analysis<DataPath, DataPathConstraintGenerator> {};
	struct data_path_analysis_var  : public data_analysis<DataPath, DataPathConstraintGenerator> {};

	extern const data_path_analysis_data DP;
	extern const data_path_analysis_var  dp;


	template<typename Context>
	class DataPathConstraintGenerator : public DataFlowConstraintGenerator<data_path_analysis_data, data_path_analysis_var, Context> {

		typedef DataFlowConstraintGenerator<data_path_analysis_data, data_path_analysis_var, Context> super;

		CBA& cba;

		const core::lang::BasicGenerator& base;

	public:

		DataPathConstraintGenerator(CBA& cba) : super(cba, DP, dp), cba(cba), base(cba.getRoot()->getNodeManager().getLangBasic()) { };

		using super::elem;

		void visitLiteral(const LiteralInstance& literal, const Context& ctxt, Constraints& constraints) {

			// check whether it is the data-path root element
			if (!base.isDataPathRoot(literal)) return;

			// in this case, seed the analysis
			auto value = DataPath();
			constraints.add(elem(value, cba.getVar(DP, literal, ctxt)));

		}

		void visitCallExpr(const CallExprInstance& call, const Context& ctxt, Constraints& constraints) {

			// add default handling
			super::visitCallExpr(call, ctxt, constraints);

			// special handling in case it is a data-path constructor
			ExpressionPtr fun = call->getFunctionExpr();
			if (!base.isDataPathPrimitive(fun) || call.size() != 2u) return;

			// get source and target sets
			auto DP_src = cba.getVar(DP, call[0], ctxt);
			auto DP_trg = cba.getVar(DP, call, ctxt);

			// check out the type of data path constructor
			if (base.isDataPathMember(fun)) {

				// get set containing value of identifier
				auto D_field = cba.getVar(D, call[1], ctxt);	// we use the simple-constant analyses to get the identifier

				constraints.add(combine(this->getValueManager(), DP_src, D_field, DP_trg,
						[](const DataPath& head, const ExpressionPtr& field)->DataPath {
							assert_true(field.isa<LiteralPtr>());
							return head << FieldIndex(field.as<LiteralPtr>()->getValue());
						})
				);

			} else if (base.isDataPathElement(fun)) {

				// get set containing value of identifier
				auto A_index = cba.getVar(A, call[1], ctxt);	// we use the arithmetic analyses to obtain the index

				constraints.add(combine(this->getValueManager(), DP_src, A_index, DP_trg,
						[](const DataPath& head, const Formula& index)->DataPath {
							if (!index) return head << ElementIndex();
							return head << ElementIndex(*index.formula);
						})
				);

			} else if (base.isDataPathComponent(fun)) {

				// get set containing value of identifier
				auto A_index = cba.getVar(A, call[1], ctxt);	// we use the arithmetic analyses to obtain the index

				constraints.add(combine(this->getValueManager(), DP_src, A_index, DP_trg,
						[](const DataPath& head, const Formula& index)->DataPath {
							if (!index) return head << ElementIndex();
							return head << ElementIndex(*index.formula);
						})
				);

			} else {
				// unknown constructur encountered
				assert_fail() << "Unsupported data path constructor encountered: " << *fun;
			}

//			LITERAL(DataPathRoot,		"dp.root", 				"datapath")
//			LITERAL(DataPathMember,		"dp.member", 			"(datapath, identifier) -> datapath")
//			LITERAL(DataPathElement,	"dp.element", 			"(datapath, uint<8>) -> datapath")
//			LITERAL(DataPathComponent,	"dp.component", 		"(datapath, uint<8>) -> datapath")
//			LITERAL(DataPathParent,     "dp.parent",            "(datapath, type<'a>) -> datapath")

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

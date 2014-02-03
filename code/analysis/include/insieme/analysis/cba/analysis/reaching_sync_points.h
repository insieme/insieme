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
#include "insieme/analysis/cba/framework/entities/program_point.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/analysis/cba/utils/cba_utils.h"
#include "insieme/analysis/cba/utils/constraint_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * This class implements an analysis collecting all synchronization points that are processed before
	 * a given program point without any other synchronization point in between. This analysis is mainly
	 * utilized for computing thread regions.
	 */

	// ----------------- reaching sync points ---------------

	template<typename Context> class ReachingSyncPointsInConstraintGenerator;
	template<typename Context> class ReachingSyncPointsTmpConstraintGenerator;
	template<typename Context> class ReachingSyncPointsOutConstraintGenerator;

	struct reaching_sync_points_in_analysis  : public dependent_set_analysis<ProgramPoint,  ReachingSyncPointsInConstraintGenerator> {};
	struct reaching_sync_points_tmp_analysis : public dependent_set_analysis<ProgramPoint, ReachingSyncPointsTmpConstraintGenerator> {};
	struct reaching_sync_points_out_analysis : public dependent_set_analysis<ProgramPoint, ReachingSyncPointsOutConstraintGenerator> {};

	extern const reaching_sync_points_in_analysis  RSPin;
	extern const reaching_sync_points_tmp_analysis RSPtmp;
	extern const reaching_sync_points_out_analysis RSPout;


	template<typename Context>
	class ReachingSyncPointsInConstraintGenerator : public BasicInConstraintGenerator<reaching_sync_points_in_analysis, reaching_sync_points_tmp_analysis, reaching_sync_points_out_analysis, ReachingSyncPointsInConstraintGenerator<Context>, Context> {

		typedef BasicInConstraintGenerator<reaching_sync_points_in_analysis, reaching_sync_points_tmp_analysis, reaching_sync_points_out_analysis, ReachingSyncPointsInConstraintGenerator<Context>, Context> super;

		StatementAddress root;

		CBA& cba;

	public:

		ReachingSyncPointsInConstraintGenerator(CBA& cba)
			: super(cba, RSPin, RSPtmp, RSPout), root(cba.getRoot()), cba(cba) { }

		void visit(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {

			// the entry point of every thread is a sync point
			if (auto stmt = node.isa<StatementAddress>()) {
				if (isThreadBody(stmt, ctxt)) {
					auto l = cba.getLabel(stmt);
					auto R = cba.getSet(RSPin, l, ctxt);
					constraints.add(elem(ProgramPoint<Context>(ProgramPoint<Context>::In, node.as<StatementAddress>(), ctxt), R));
					return;
				}
			}

			// otherwise treat it as usual
			super::visit(node, ctxt, constraints);
		}

	};

	template<typename Context>
	class ReachingSyncPointsOutConstraintGenerator : public BasicOutConstraintGenerator<reaching_sync_points_in_analysis, reaching_sync_points_tmp_analysis, reaching_sync_points_out_analysis, ReachingSyncPointsOutConstraintGenerator<Context>,Context> {

		typedef BasicOutConstraintGenerator<reaching_sync_points_in_analysis, reaching_sync_points_tmp_analysis, reaching_sync_points_out_analysis, ReachingSyncPointsOutConstraintGenerator<Context>,Context> super;

		CBA& cba;

	public:

		ReachingSyncPointsOutConstraintGenerator(CBA& cba)
			: super(cba, RSPin, RSPtmp, RSPout), cba(cba) { }

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// check whether it is a sync-operation call
			auto fun = call->getFunctionExpr();
			if (isSynchronizingFunction(fun)) {
				auto l = cba.getLabel(call);
				auto R = cba.getSet(RSPout, l, ctxt);
				constraints.add(elem(ProgramPoint<Context>(ProgramPoint<Context>::Tmp, call, ctxt), R));
				return;
			}

			// otherwise treat it as usual
			super::visitCallExpr(call, ctxt, constraints);
		}
	};

	template<typename Context>
	class ReachingSyncPointsTmpConstraintGenerator : public BasicTmpConstraintGenerator<reaching_sync_points_in_analysis, reaching_sync_points_tmp_analysis, reaching_sync_points_out_analysis, Context> {

		typedef BasicTmpConstraintGenerator<reaching_sync_points_in_analysis, reaching_sync_points_tmp_analysis, reaching_sync_points_out_analysis, Context> super;

	public:

		ReachingSyncPointsTmpConstraintGenerator(CBA& cba) : super(cba, RSPin, RSPtmp, RSPout) {}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

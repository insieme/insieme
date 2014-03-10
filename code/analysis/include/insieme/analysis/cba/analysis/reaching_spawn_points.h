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
	 * This class implements an analysis collecting all spawn points that are processed before
	 * a given program point without any other merge-all point in between. This analysis is mainly
	 * utilized for computing the list of threads merged by a merge-all call.
	 */

	// ----------------- reaching spawn points ---------------

	template<typename Context> class ReachingSpawnPointsInConstraintGenerator;
	template<typename Context> class ReachingSpawnPointsTmpConstraintGenerator;
	template<typename Context> class ReachingSpawnPointsOutConstraintGenerator;

	struct reaching_spawn_points_in_analysis  : public dependent_set_analysis<ProgramPoint,  ReachingSpawnPointsInConstraintGenerator> {};
	struct reaching_spawn_points_tmp_analysis : public dependent_set_analysis<ProgramPoint, ReachingSpawnPointsTmpConstraintGenerator> {};
	struct reaching_spawn_points_out_analysis : public dependent_set_analysis<ProgramPoint, ReachingSpawnPointsOutConstraintGenerator> {};

	extern const reaching_spawn_points_in_analysis  ReachingSpawnPointsIn;
	extern const reaching_spawn_points_tmp_analysis ReachingSpawnPointsTmp;
	extern const reaching_spawn_points_out_analysis ReachingSpawnPointsOut;

	namespace detail {

		bool isSpawnPointFree(const ExpressionPtr& expr);

	}

	template<typename Context>
	class ReachingSpawnPointsInConstraintGenerator : public BasicInConstraintGenerator<reaching_spawn_points_in_analysis, reaching_spawn_points_tmp_analysis, reaching_spawn_points_out_analysis, ReachingSpawnPointsInConstraintGenerator<Context>, Context> {

		typedef BasicInConstraintGenerator<reaching_spawn_points_in_analysis, reaching_spawn_points_tmp_analysis, reaching_spawn_points_out_analysis, ReachingSpawnPointsInConstraintGenerator<Context>, Context> super;

	public:

		ReachingSpawnPointsInConstraintGenerator(CBA& cba)
			: super(cba, ReachingSpawnPointsIn, ReachingSpawnPointsTmp, ReachingSpawnPointsOut) { }

		// nothing to do here

	};

	template<typename Context>
	class ReachingSpawnPointsOutConstraintGenerator : public BasicOutConstraintGenerator<reaching_spawn_points_in_analysis, reaching_spawn_points_tmp_analysis, reaching_spawn_points_out_analysis, ReachingSpawnPointsOutConstraintGenerator<Context>,Context> {

		typedef BasicOutConstraintGenerator<reaching_spawn_points_in_analysis, reaching_spawn_points_tmp_analysis, reaching_spawn_points_out_analysis, ReachingSpawnPointsOutConstraintGenerator<Context>,Context> super;

		CBA& cba;

	public:

		ReachingSpawnPointsOutConstraintGenerator(CBA& cba)
			: super(cba, ReachingSpawnPointsIn, ReachingSpawnPointsTmp, ReachingSpawnPointsOut), cba(cba) { }

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, Constraints& constraints) {

			// check whether it is a spawn-operation call
			auto fun = call->getFunctionExpr();
			ProgramPoint<Context> point(ProgramPoint<Context>::Tmp, call, ctxt);
			if (point.isSpawn() && point.getStatement().getParentAddress().template isa<CompoundStmtAddress>()) {		// only interested in unattached spawns

				auto l = cba.getLabel(call);
				auto R = cba.getSet(ReachingSpawnPointsOut, l, ctxt);
				constraints.add(elem(point, R));
				// and the default handling (fall-through)

			} else if (point.isMergeAll()) {
				// stop here - nothing included!
				return;
			}

			// test whether sub-expressions may contain spawn points
			if (detail::isSpawnPointFree(call)) {
				// if so in = out
				auto In  = cba.getSet(this->Ain, call, ctxt);
				auto Out = cba.getSet(this->Aout, call, ctxt);

				constraints.add(subset(In,Out));
				return;
			}

			// the rest treat it as usual
			super::visitCallExpr(call, ctxt, constraints);
		}

	};

	template<typename Context>
	class ReachingSpawnPointsTmpConstraintGenerator : public BasicTmpConstraintGenerator<reaching_spawn_points_in_analysis, reaching_spawn_points_tmp_analysis, reaching_spawn_points_out_analysis, Context> {

		typedef BasicTmpConstraintGenerator<reaching_spawn_points_in_analysis, reaching_spawn_points_tmp_analysis, reaching_spawn_points_out_analysis, Context> super;

	public:

		ReachingSpawnPointsTmpConstraintGenerator(CBA& cba) : super(cba, ReachingSpawnPointsIn, ReachingSpawnPointsTmp, ReachingSpawnPointsOut) {}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

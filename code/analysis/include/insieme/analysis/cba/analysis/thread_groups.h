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
#include "insieme/analysis/cba/framework/entities/thread_group.h"
#include "insieme/analysis/cba/framework/generator/basic_data_flow.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- groups ---------------

	template<typename Context> class ThreadGroupConstraintGenerator;

	template<typename Context>
	const DataAnalysisType<ThreadGroup<Context>,ThreadGroupConstraintGenerator>& ThreadGroups() {
		static const DataAnalysisType<ThreadGroup<Context>,ThreadGroupConstraintGenerator> instance("ThreadGroups");
		return instance;
	}

	template<typename Context>
	const DataAnalysisType<ThreadGroup<Context>,ThreadGroupConstraintGenerator>& threadGroups() {
		static const DataAnalysisType<ThreadGroup<Context>,ThreadGroupConstraintGenerator> instance("threadGroups");
		return instance;
	}


	template<typename Context>
	class ThreadGroupConstraintGenerator : public BasicDataFlowConstraintGenerator<ThreadGroup<Context>,DataAnalysisType<ThreadGroup<Context>,ThreadGroupConstraintGenerator>, Context> {

		typedef BasicDataFlowConstraintGenerator<ThreadGroup<Context>,DataAnalysisType<ThreadGroup<Context>,ThreadGroupConstraintGenerator>, Context> super;

		CBA& cba;

		const core::lang::BasicGenerator& base;

	public:

		ThreadGroupConstraintGenerator(CBA& cba) : super(cba, ThreadGroups<Context>(), threadGroups<Context>()), cba(cba), base(cba.getRoot().getNodeManager().getLangBasic()) { };

		using super::elem;

		void visitCallExpr(const CallExprAddress& expr, const Context& ctxt, Constraints& constraints) {

			// default handling
			super::visitCallExpr(expr, ctxt, constraints);

			// handle creation of thread groups if necessary
			if (!isThreadGroupConstructor(expr)) return;

			// this expression is creating a job
			auto value = getThreadGroupFromConstructor(expr, ctxt);
			auto G_res = cba.getSet(ThreadGroups<Context>(), expr, ctxt);

			// add constraint fixing this job
			constraints.add(elem(value, G_res));

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

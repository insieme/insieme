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
#include "insieme/analysis/cba/framework/generator/basic_program_point.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- jobs ---------------

	template<typename Context> class ReachingDefInConstraintGenerator;
	template<typename Context> class ReachingDefOutConstraintGenerator;

	struct reaching_def_in_analysis  : public location_based_set_analysis<Definition,  ReachingDefInConstraintGenerator> {};
	struct reaching_def_out_analysis : public location_based_set_analysis<Definition, ReachingDefOutConstraintGenerator> {};

	extern const reaching_def_in_analysis  RDin;
	extern const reaching_def_out_analysis RDout;

	template<typename Context>
	class ReachingDefInConstraintGenerator
		: public BasicInConstraintGenerator<
		  	  reaching_def_in_analysis,
		  	  reaching_def_out_analysis,
		  	  ReachingDefInConstraintGenerator<Context>,
		  	  Context,
		  	  Location<Context>
		  > {

		typedef BasicInConstraintGenerator<
			  	  reaching_def_in_analysis,
			  	  reaching_def_out_analysis,
			  	  ReachingDefInConstraintGenerator<Context>,
			  	  Context,
			  	  Location<Context>
			 > super;

		CBA& cba;

	public:

		ReachingDefInConstraintGenerator(CBA& cba) : super(cba, RDin, RDout), cba(cba) {}

	};

	template<typename Context>
	class ReachingDefOutConstraintGenerator
		: public BasicOutConstraintGenerator<
		  	  reaching_def_in_analysis,
		  	  reaching_def_out_analysis,
		  	  ReachingDefOutConstraintGenerator<Context>,
		  	  Context,
		  	  Location<Context>
		  > {

		typedef BasicOutConstraintGenerator<
				  reaching_def_in_analysis,
				  reaching_def_out_analysis,
				  ReachingDefOutConstraintGenerator<Context>,
				  Context,
				  Location<Context>
			 > super;


		CBA& cba;

	public:

		ReachingDefOutConstraintGenerator(CBA& cba) : super(cba, RDin, RDout), cba(cba) {}

		void visitCallExpr(const CallExprAddress& call, const Context& ctxt, const Location<Context>& loc, Constraints& constraints) {
			const auto& base = call->getNodeManager().getLangBasic();

			// one special case: assignments
			auto fun = call.as<CallExprPtr>()->getFunctionExpr();
			if (!base.isRefAssign(fun)) {

				// use default treatment
				super::visitCallExpr(call, ctxt, loc, constraints);

				// done
				return;
			}

			// TODO: check referenced location
			//		- if not referenced => out = in
			//		- if only reference => in definitions are not forwarded
			//		- if on of many => in definition + local definition


			auto value = Definition<Context>(call, ctxt);
			auto RD_out = cba.getSet(RDout, call, ctxt, loc);

			constraints.add(elem(value, RD_out));

		}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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
#include "insieme/analysis/cba/framework/generator/basic_program_point.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ------------------- reachable code ------------------

	struct Reachable : public utils::Printable {
		bool operator<(const Reachable& other) const { return false; }
		bool operator==(const Reachable& other) const { return true; }
		std::ostream& printTo(std::ostream& out) const { return out << "reachable"; };
	};


	template<typename Context> class ReachableInConstraintGenerator;
	template<typename Context> class ReachableTmpConstraintGenerator;
	template<typename Context> class ReachableOutConstraintGenerator;

	struct reachable_in_analysis  : public set_analysis<Reachable,  ReachableInConstraintGenerator> {};
	struct reachable_tmp_analysis : public set_analysis<Reachable, ReachableTmpConstraintGenerator> {};
	struct reachable_out_analysis : public set_analysis<Reachable, ReachableOutConstraintGenerator> {};

	extern const reachable_in_analysis Rin;
	extern const reachable_tmp_analysis Rtmp;
	extern const reachable_out_analysis Rout;


	template<typename Context>
	class ReachableInConstraintGenerator : public BasicInConstraintGenerator<reachable_in_analysis, reachable_tmp_analysis, reachable_out_analysis, ReachableInConstraintGenerator<Context>, Context> {

		typedef BasicInConstraintGenerator<reachable_in_analysis, reachable_tmp_analysis, reachable_out_analysis, ReachableInConstraintGenerator<Context>, Context> super;

		StatementAddress root;

		bool initSet;

		CBA& cba;

	public:

		ReachableInConstraintGenerator(CBA& cba)
			: super(cba, Rin, Rtmp, Rout), root(cba.getRoot()), initSet(false), cba(cba) { }

		void visit(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {

			// make sure root is reachable
			if (!initSet && node == root && ctxt == Context()) {
				auto l = cba.getLabel(root);
				auto R = cba.getSet(Rin, l, ctxt);
				constraints.add(elem(Reachable(), R));
				initSet = true;
			}

			// and all the other constraints
			super::visit(node, ctxt, constraints);
		}

	};

	template<typename Context>
	class ReachableOutConstraintGenerator : public BasicOutConstraintGenerator<reachable_in_analysis, reachable_tmp_analysis, reachable_out_analysis, ReachableOutConstraintGenerator<Context>,Context> {

		typedef BasicOutConstraintGenerator<reachable_in_analysis, reachable_tmp_analysis, reachable_out_analysis, ReachableOutConstraintGenerator<Context>,Context> super;

		CBA& cba;

	public:

		ReachableOutConstraintGenerator(CBA& cba)
			: super(cba, Rin, Rtmp, Rout), cba(cba) { }

		/**
		 * Here we need a special handling (not considering reachability) since it is what we try to compute.
		 */
		template<typename E, typename L, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIfImpl (
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

	template<typename Context>
	class ReachableTmpConstraintGenerator : public BasicTmpConstraintGenerator<reachable_tmp_analysis, reachable_out_analysis, Context> {

		typedef BasicTmpConstraintGenerator<reachable_in_analysis, reachable_tmp_analysis, reachable_out_analysis, ReachableTmpConstraintGenerator<Context>, Context> super;

	public:

		ReachableTmpConstraintGenerator(CBA& cba) : super(cba, Rtmp, Rout) {}

	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

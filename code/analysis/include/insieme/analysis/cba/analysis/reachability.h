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

#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/basic_program_point_constraint_resolver.h"

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

	template<typename C> class ReachableInConstraintResolver;
	typedef TypedSetType<Reachable,ReachableInConstraintResolver> ReachableInSetType;
	extern const ReachableInSetType Rin;

	template<typename C> class ReachableOutConstraintResolver;
	typedef TypedSetType<Reachable,ReachableOutConstraintResolver> ReachableOutSetType;
	extern const ReachableOutSetType Rout;


	template<typename Context>
	class ReachableInConstraintResolver : public BasicInConstraintResolver<ReachableInSetType, ReachableOutSetType, ReachableInConstraintResolver<Context>, Context> {

		typedef BasicInConstraintResolver<ReachableInSetType, ReachableOutSetType, ReachableInConstraintResolver<Context>, Context> super;

		StatementAddress root;

		bool initSet;

		CBA& cba;

	public:

		ReachableInConstraintResolver(CBA& cba)
			: super(cba, Rin, Rout, *this), root(cba.getRoot()), initSet(false), cba(cba) { }

		virtual void visit(const NodeAddress& node, const Context& ctxt, Constraints& constraints) {

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

		template<typename E, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIf (
					const E& value, const TypedSetID<E>& set,
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
	class ReachableOutConstraintResolver : public BasicOutConstraintResolver<ReachableInSetType, ReachableOutSetType, ReachableOutConstraintResolver<Context>,Context> {

		typedef BasicOutConstraintResolver<ReachableInSetType, ReachableOutSetType, ReachableOutConstraintResolver<Context>,Context> super;

		CBA& cba;

	public:

		ReachableOutConstraintResolver(CBA& cba)
			: super(cba, Rin, Rout, *this), cba(cba) { }

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

		template<typename E, typename SetTypeA, typename SetTypeB>
		void connectStateSetsIf (
					const E& value, const TypedSetID<E>& set,
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

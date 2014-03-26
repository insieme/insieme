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

#include "insieme/analysis/cba/framework/generator/basic_program_point.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace detail {

		bool le(const std::set<Formula>& sa, const std::set<Formula>& sb) {
			// if there is any undefined or symbolic value => assume the worst
			for(const auto& a : sa) if (!a) return true;
			for(const auto& b : sb) if (!b) return true;

			// check whether there is some value in a that is less than b
			for(const auto& a : sa) {
				const auto& af = *a.formula;

				for(const auto& b : sb) {
					const auto& bf = *b.formula;

					// check whether value A is less than B
					//   A < B iff B-A > 0  (cancels out variables and symbols)

					auto diff = bf - af;

					// if it is not a constant => worst case
					if (!diff.isConstant()) return true;

					// check sign of difference
					if (diff.getConstantValue().getNumerator() >= 0) return true;
				}
			}
			return false;
		}

		bool eq(const std::set<Formula>& sa, const std::set<Formula>& sb) {
			// they are not equal if they are not singletons
			if (sa.size() != 1 || sb.size() != 1) return false;

			// extract the formulas
			const Formula& a = *sa.begin();
			const Formula& b = *sb.begin();

			// if any of those is undefined => not equal
			if (!a || !b) return false;

			// compare the inner formulas
			return a == b;
		}

		// create a filter determining whether a loop is entered
		bool loop_entered(const std::set<Formula>& l, const std::set<Formula>& u, const std::set<Formula>& s) {

			// check direction of iteration
			bool isInc = any(s, [](const Formula& cur) { return !cur || !cur.formula->isConstant() || cur.formula->getConstantValue().getNumerator() >= 0; });
			bool isDec = any(s, [](const Formula& cur) { return !cur || !cur.formula->isConstant() || cur.formula->getConstantValue().getNumerator() <= 0; });

			// if direction is not clear => assume worst
			if (isInc && isDec) return true;

			// if it is neither increasing nor decreasing => it is not evaluated yet
			if (!isInc && !isDec) return false;

			// if the formulas are equal the loop is entered
			if (eq(l,u)) return true;

			// otherwise check based on the iteration direction
			if (isInc) return le(l, u);
			if (isDec) return le(u, l);

			// should not be reached
			assert_fail() << "Should not be reached! " << isInc << "/" << isDec << "\n";
			return true;
		}

		// create a filter determining whether a loop is not entered
		bool loop_not_entered(const std::set<Formula>& l, const std::set<Formula>& u, const std::set<Formula>& s) {

			// check direction of iteration
			bool isInc = any(s, [](const Formula& cur) { return !cur || !cur.formula->isConstant() || cur.formula->getConstantValue().getNumerator() >= 0; });
			bool isDec = any(s, [](const Formula& cur) { return !cur || !cur.formula->isConstant() || cur.formula->getConstantValue().getNumerator() <= 0; });

			// if direction is not clear => assume worst
			if (isInc && isDec) return true;

			// if it is neither increasing nor decreasing => it is not evaluated yet
			if (!isInc && !isDec) return false;

			// if the formulas are equal the loop is entered
			if (eq(l,u)) return false;

			// otherwise check based on the iteration direction
			if (isInc) return le(u, l);		// this is the reverse check of the loop_entered case
			if (isDec) return le(l, u);		// this one too

			// should not be reached
			assert_fail() << "Should not be reached! " << isInc << "/" << isDec << "\n";
			return true;
		}

	} // end namespace detail

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

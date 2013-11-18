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

#include "insieme/analysis/cba/analysis.h"

#include "insieme/analysis/cba/cba.h"
#include "insieme/analysis/cba/analysis/boolean.h"
#include "insieme/analysis/cba/analysis/references.h"
#include "insieme/analysis/cba/analysis/arithmetic.h"

#include "insieme/core/ir.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/assert.h"

namespace insieme {
namespace analysis {
namespace cba {

	namespace {

		bool isBooleanValue(const core::ExpressionPtr& e) {
			auto& basic = e->getNodeManager().getLangBasic();
			return basic.isBool(e->getType());
		}

	}

	// -- Boolean --

	bool isTrue(const core::ExpressionAddress& a) {
		if (!isBooleanValue(a)) return false;
		std::set<bool> res = getValues(a, B);
		return res.size() == 1 && res.find(true) != res.end();
	}

	bool mayBeTrue(const core::ExpressionAddress& a) {
		if (!isBooleanValue(a)) return false;
		std::set<bool> res = getValues(a, B);
		return res.find(true) != res.end();
	}

	bool isFalse(const core::ExpressionAddress& a) {
		if (!isBooleanValue(a)) return false;
		std::set<bool> res = getValues(a, B);
		return res.size() == 1 && res.find(false) != res.end();
	}

	bool mayBeFalse(const core::ExpressionAddress& a) {
		if (!isBooleanValue(a)) return false;
		std::set<bool> res = getValues(a, B);
		return res.find(false) != res.end();
	}


	// -- Arithmetic --

	core::LiteralPtr isIntegerConstant(const core::ExpressionAddress& a) {
		const static core::LiteralPtr fail;

		const auto& base = a->getNodeManager().getLangBasic();
		if (!base.isInt(a->getType())) return fail;

		// get all values
		std::set<Formula> res = getValues(a, A);

		// check whether result is fixed
		if (res.size() != 1) return fail;

		// see whether value is known
		Formula value = *res.begin();
		if (!value) return fail;

		// check whether value is a integer constant
		if (!value.formula->isInteger()) return fail;

		// convert result into literal
		return core::arithmetic::toIR(a->getNodeManager(), (*value.formula)).as<core::LiteralPtr>();
	}


	// -- Functions --


	// -- Other --


	// *************************************************************************************
	//										 Aliases
	// *************************************************************************************

	bool notAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		return !mayAlias(a,b);
	}

	bool mayAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		assert_eq(a.getRootNode(), b.getRootNode());
		assert_true(a->getType().isa<core::RefTypePtr>()) << "Expected reference value - got value of type: " << *a->getType() << "\n";
		assert_true(b->getType().isa<core::RefTypePtr>()) << "Expected reference value - got value of type: " << *b->getType() << "\n";

		// shortcut for the simple stuff
		if (a == b) return true;

		typedef Reference<DefaultContext> Reference;

		// compute references set
		std::set<Reference> refA = getValues(a, R<DefaultContext>());
		std::set<Reference> refB = getValues(b, R<DefaultContext>());

		// check whether there are overlapping references
		for (const auto& a : refA) {
			for (const auto& b : refB) {
				if (a.isAlias(b)) return true;
			}
		}
		return false;
	}

	bool isAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		assert_eq(a.getRootNode(), b.getRootNode());
		assert_true(a->getType().isa<core::RefTypePtr>()) << "Expected reference value - got value of type: " << *a->getType() << "\n";
		assert_true(b->getType().isa<core::RefTypePtr>()) << "Expected reference value - got value of type: " << *b->getType() << "\n";

		// shortcut for the simple stuff
		if (a == b) return true;

		typedef Reference<DefaultContext> Reference;

		// compute references set
		std::set<Reference> refA = getValues(a, R<DefaultContext>());
		std::set<Reference> refB = getValues(b, R<DefaultContext>());

		// check whether all references are overlapping
		for (const auto& a : refA) {
			for (const auto& b : refB) {
				if (!a.isAlias(b)) return false;
			}
		}
		return true;
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

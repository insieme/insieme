/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/haskell/dataflow.h"

#include "insieme/analysis/haskell/adapter.h"

using namespace insieme::core;

namespace insieme {
namespace analysis {
namespace haskell {

	VariableAddress getDefinitionPoint(const VariableAddress& var) {
		auto& env = Environment::getInstance();

		auto ir = env.passIR(var.getRootNode());
		auto var_addr = env.passAddress(var, ir);

		auto target = env.findDecl(var_addr);
		if (!target) {
			return {};
		}

		return target->toNodeAddress(var.getRootNode()).as<VariableAddress>();
	}

	bool isTrue(const core::ExpressionAddress& expr) {
		auto& env = Environment::getInstance();
		auto ir = env.passIR(expr.getRootNode());
		auto expr_addr = env.passAddress(expr, ir);
		return env.checkBoolean(expr_addr) == BooleanAnalysisResult_AlwaysTrue;
	}

	bool isFalse(const core::ExpressionAddress& expr) {
		auto& env = Environment::getInstance();
		auto ir = env.passIR(expr.getRootNode());
		auto expr_addr = env.passAddress(expr, ir);
		return env.checkBoolean(expr_addr) == BooleanAnalysisResult_AlwaysFalse;
	}

	bool mayBeTrue(const core::ExpressionAddress& expr) {
		auto& env = Environment::getInstance();
		auto ir = env.passIR(expr.getRootNode());
		auto expr_addr = env.passAddress(expr, ir);
		auto res = env.checkBoolean(expr_addr);
		return res == BooleanAnalysisResult_AlwaysTrue || res == BooleanAnalysisResult_Both;
	}

	bool mayBeFalse(const core::ExpressionAddress& expr) {
		auto& env = Environment::getInstance();
		auto ir = env.passIR(expr.getRootNode());
		auto expr_addr = env.passAddress(expr, ir);
		auto res = env.checkBoolean(expr_addr);
		return res == BooleanAnalysisResult_AlwaysFalse || res == BooleanAnalysisResult_Both;
	}

	ArithmeticSet getArithmeticValue(const core::ExpressionAddress& expr) {
		auto& env = Environment::getInstance();
		env.setRoot(expr.getRootNode());
		auto ir = env.passIR(expr.getRootNode());
		auto expr_addr = env.passAddress(expr, ir);
		auto res_ptr = env.arithmeticValue(expr_addr);

		ArithmeticSet res(std::move(*res_ptr));
		delete res_ptr;
		return res;
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

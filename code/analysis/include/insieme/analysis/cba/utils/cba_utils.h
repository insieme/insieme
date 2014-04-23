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

#include "insieme/core/ir.h"
#include "insieme/core/ir_instance.h"

namespace insieme {
namespace analysis {
namespace cba {

	class CBA;

	CBA& getCBA(const core::NodeAddress& node);

	core::NodeInstance getSurroundingFreeFunction(const core::NodeInstance& cur);

	core::LambdaInstance getSurroundingRecursiveFunction(const core::NodeInstance& cur);

	vector<core::ExpressionInstance> getAllFreeFunctions(const core::NodeInstance& root);

	// allows to check whether a given statement is a memory location constructor (including globals)
	bool isMemoryConstructor(const core::StatementInstance& stmt);

	core::VariableInstance getDefinitionPoint(const core::VariableInstance& varAddress);

	core::ExpressionInstance getLocationDefinitionPoint(const core::StatementInstance& stmt);

	core::StatementInstance getAnalysisRoot(const core::NodeInstance& node);

	bool isRecursiveCall(const core::CallExprInstance& call);

	/**
	 * Checks whether the given address is referencing a value captured by a bind expression.
	 */
	bool isCapturedValue(const core::ExpressionInstance& value);

	/**
	 * Checks whether the given function has a synchronizing effect on threads.
	 */
	bool isSynchronizingFunction(const core::ExpressionPtr& expr);


	namespace detail {
		/**
		 * Checks whether the given statement is a potential entry point for a thread.
		 */
		bool isThreadBody(const core::StatementInstance& stmt);
	}

	/**
	 * Checks whether the given stmt / context combination is a potential body of a thread.
	 */
	template<typename Context>
	bool isThreadBody(const core::StatementInstance& stmt, const Context& ctxt) {
		typedef typename Context::call_context call_context_type;

		// check context
		static const call_context_type empty;
		if (ctxt.callContext != empty) return false;

		// check statement
		return detail::isThreadBody(stmt);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

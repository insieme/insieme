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

#include <set>
#include "insieme/core/ir.h"

#include "insieme/analysis/cba/framework/cba.h"
#include "insieme/analysis/cba/utils/cba_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	// forward declarations of the result-set type token
	template<typename E, template<typename C> class G>
	struct TypedSetType;

	/**
	 * The main facade function for utilizing the constraint-based analysis framework (CBA).
	 *
	 * Analysis are capable of deducing values of expressions within a given code fragment. The fragment
	 * is determined by the root node of the given expr-address, while the expression itself is addressing
	 * the targeted expression. The kind of information to be obtained can be determined by the type
	 * parameter and potential parameters is determined by the type parameter. A catalog of those is
	 * provided by the header files located within the cba/analysis directory. Additional analysis
	 * may be defined for specific tasks.
	 *
	 * For Example usages see the
	 *
	 * 						ut_analysis_cba_facade.cc
	 *
	 * test case.
	 *
	 *
	 * @param expr the expressions which's values should be determined by the analysis
	 * @param type the the type analysis result to be obtained
	 * @param ctxt the optional context the given input expression should be considered in
	 * @return a reference to a set of values representing the result of the analysis
	 */
	template<typename T, template<typename C> class G, typename Context = DefaultContext>
	const std::set<T>& getValues(const core::ExpressionAddress& expr, const TypedSetType<T,G>& type, const Context& ctxt = Context()) {

		typedef std::shared_ptr<CBA> CBA_Ptr;

		// obtain CBA context from root node
		core::StatementPtr root = getRootStmt(expr);
		if (!root->hasAttachedValue<CBA_Ptr>()) {
			root->attachValue<CBA_Ptr>(std::make_shared<CBA>(core::StatementAddress(root)));
		}

		// run analysis
		CBA& cba = *root->getAttachedValue<CBA_Ptr>();
		return cba.getValuesOf(expr, type, ctxt);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

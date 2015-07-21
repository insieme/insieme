/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/checks/full_check.h"

#include "insieme/core/checks/imperative_checks.h"
#include "insieme/core/checks/type_checks.h"
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/checks/literal_checks.h"


namespace insieme {
namespace core {
namespace checks {

	namespace {

		CheckPtr buildFullCheck() {

			std::vector<CheckPtr> checks;
			checks.push_back(make_check<KeywordCheck>());
			checks.push_back(make_check<FunctionKindCheck>());
			checks.push_back(make_check<ParentCheck>());
			checks.push_back(make_check<ClassInfoCheck>());
			checks.push_back(make_check<CallExprTypeCheck>());
			checks.push_back(make_check<FunctionTypeCheck>());
			checks.push_back(make_check<BindExprTypeCheck>());
			checks.push_back(make_check<ExternalFunctionTypeCheck>());
			checks.push_back(make_check<ReturnTypeCheck>());
			checks.push_back(make_check<LambdaTypeCheck>());
			checks.push_back(make_check<DeclarationStmtTypeCheck>());
			checks.push_back(make_check<IfConditionTypeCheck>());
			checks.push_back(make_check<ForStmtTypeCheck>());
			checks.push_back(make_check<WhileConditionTypeCheck>());
			checks.push_back(make_check<SwitchExpressionTypeCheck>());
			checks.push_back(make_check<StructExprTypeCheck>());
			checks.push_back(make_check<MemberAccessElementTypeCheck>());
			checks.push_back(make_check<ComponentAccessTypeCheck>());
			checks.push_back(make_check<BuiltInLiteralCheck>());
			checks.push_back(make_check<RefCastCheck>());
			checks.push_back(make_check<CastCheck>());
			checks.push_back(make_check<GenericZeroCheck>());
			checks.push_back(make_check<ArrayTypeCheck>());
			checks.push_back(make_check<GenericOpsCheck>());

			checks.push_back(make_check<UndeclaredVariableCheck>());

			checks.push_back(make_check<ScalarArrayIndexRangeCheck>());
			//checks.push_back(make_check<UndefinedCheck>());
			checks.push_back(make_check<FreeBreakInsideForLoopCheck>());
			checks.push_back(make_check<MissingReturnStmtCheck>());

			checks.push_back(make_check<NarrowCheck>());
			checks.push_back(make_check<ExpandCheck>());

			checks.push_back(make_check<LiteralFormatCheck>());

			// assemble the IR check list
			CheckPtr recursive = makeVisitOnce(combine(checks));

			return combine(
					toVector<CheckPtr>(
						recursive
					)
			);
		}

	}

	CheckPtr getFullCheck() {
		// share common check-instance (initialization is thread save in C++11)
		static const CheckPtr fullChecks = buildFullCheck();
		return fullChecks;
	}

} // end namespace check
} // end namespace core
} // end namespace insieme

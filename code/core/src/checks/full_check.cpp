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

#include "insieme/core/checks/full_check.h"

#include "insieme/common/env_vars.h"

#include "insieme/core/checks/imperative_checks.h"
#include "insieme/core/checks/type_checks.h"
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/checks/literal_checks.h"


namespace insieme {
namespace core {
namespace checks {

	namespace {

		CheckPtr buildFullCheck() {
			std::vector<CheckPtr> context_free_checks;
			context_free_checks.push_back(make_check<KeywordCheck>());
			context_free_checks.push_back(make_check<FunctionKindCheck>());
			context_free_checks.push_back(make_check<ParentCheck>());
			context_free_checks.push_back(make_check<CallExprTypeCheck>());
			context_free_checks.push_back(make_check<BindExprTypeCheck>());
			context_free_checks.push_back(make_check<ExternalFunctionTypeCheck>());
			context_free_checks.push_back(make_check<ReturnTypeCheck>());
			context_free_checks.push_back(make_check<LambdaTypeCheck>());
			context_free_checks.push_back(make_check<DeclarationTypeCheck>());
			context_free_checks.push_back(make_check<DeclarationStmtTypeCheck>());
			context_free_checks.push_back(make_check<RefDeclTypeCheck>());
			context_free_checks.push_back(make_check<IfConditionTypeCheck>());
			context_free_checks.push_back(make_check<ForStmtTypeCheck>());
			context_free_checks.push_back(make_check<WhileConditionTypeCheck>());
			context_free_checks.push_back(make_check<SwitchExpressionTypeCheck>());
			context_free_checks.push_back(make_check<InitExprTypeCheck>());
			context_free_checks.push_back(make_check<TagTypeFieldsCheck>());
			context_free_checks.push_back(make_check<EnumTypeCheck>());
			context_free_checks.push_back(make_check<MemberAccessElementTypeCheck>());
			context_free_checks.push_back(make_check<MemberAccessElementTypeInTagTypeCheck>());
			context_free_checks.push_back(make_check<ComponentAccessTypeCheck>());
			context_free_checks.push_back(make_check<BuiltInLiteralCheck>());
			context_free_checks.push_back(make_check<RefCastCheck>());
			context_free_checks.push_back(make_check<IllegalNumCastCheck>());
			context_free_checks.push_back(make_check<IllegalNumTypeToIntCheck>());
			context_free_checks.push_back(make_check<IllegalTypeInstantiationCheck>());
			context_free_checks.push_back(make_check<CastCheck>());
			context_free_checks.push_back(make_check<GenericZeroCheck>());
			context_free_checks.push_back(make_check<ArrayTypeCheck>());
			context_free_checks.push_back(make_check<GenericOpsCheck>());

			context_free_checks.push_back(make_check<ConstructorTypeCheck>());
			context_free_checks.push_back(make_check<DuplicateConstructorTypeCheck>());
			context_free_checks.push_back(make_check<DestructorTypeCheck>());
			context_free_checks.push_back(make_check<MemberFunctionTypeCheck>());
			context_free_checks.push_back(make_check<DuplicateMemberFunctionCheck>());
			context_free_checks.push_back(make_check<DuplicateMemberFieldCheck>());

			context_free_checks.push_back(make_check<UndeclaredVariableCheck>());

			// context_free_checks.push_back(make_check<UndefinedCheck>());
			context_free_checks.push_back(make_check<FreeBreakInsideForLoopCheck>());
			context_free_checks.push_back(make_check<MissingReturnStmtCheck>());
			context_free_checks.push_back(make_check<ValidInitExprMemLocationCheck>());

			context_free_checks.push_back(make_check<LiteralFormatCheck>());

			// assemble the IR check list
			return combine(toVector<CheckPtr>(
				makeVisitOnce(combine(context_free_checks)),
				make_check<FreeTagTypeReferencesCheck>()
			), true);
		}
	}

	CheckPtr getFullCheck() {
		// don't run the checks if the user requested this with the environment variable
		if(getenv(INSIEME_NO_SEMA)) {
			return combine({});
		}
		// share common check-instance (initialization is thread save in C++11)
		static const CheckPtr fullChecks = buildFullCheck();
		return fullChecks;
	}

} // end namespace check
} // end namespace core
} // end namespace insieme

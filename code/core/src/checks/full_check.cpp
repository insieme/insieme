/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <fstream>

#include "insieme/core/checks/full_check.h"

#include "insieme/common/env_vars.h"

#include "insieme/core/checks/imperative_checks.h"
#include "insieme/core/checks/literal_checks.h"
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/core/checks/type_checks.h"

namespace insieme {
namespace core {
namespace checks {

	namespace {

		class FullCheck : public IRCheck {

			/**
			 * The checks to be conducted.
			 */
			CheckPtr checks;

		public:

			FullCheck(const CheckPtr& checks)
				: IRCheck(checks->isVisitingTypes()), checks(checks) {}

		protected:

			OptionalMessageList visit(const NodeAddress& node) {
				struct FullyCorrectTag {};

				// check whether it has been considered error-free earlier
				if(node->hasAttachedValue<FullyCorrectTag>()) { return OptionalMessageList(); }

				// run all the nested checks
				OptionalMessageList res = checks->visit(node);

				// check whether the check was error free
				if (node.isRoot() && (!res || res->empty())) {
					// attach the flag
					node->attachValue<FullyCorrectTag>();
				}

				// done
				return res;
			}

		};


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
			context_free_checks.push_back(make_check<DefaultedDeletedPreTUMarkerCheck>());
			context_free_checks.push_back(make_check<ForLoopSemanticsCheck>());
			context_free_checks.push_back(make_check<MissingReturnStmtCheck>());
			context_free_checks.push_back(make_check<ValidInitExprMemLocationCheck>());
			context_free_checks.push_back(make_check<ValidDeclarationCheck>());

			context_free_checks.push_back(make_check<LiteralFormatCheck>());

			// assemble the IR check list
			return make_check<FullCheck>(combine(toVector<CheckPtr>(
				makeVisitOnce(combine(context_free_checks)),
				make_check<FreeTagTypeReferencesCheck>()
			)));
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


	MessageList check(const NodePtr& node) {
		return check(node, getFullCheck());
	}

} // end namespace checks
} // end namespace core
} // end namespace insieme


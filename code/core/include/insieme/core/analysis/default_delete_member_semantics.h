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

#pragma once

#include <vector>

#include <boost/optional.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/default_members.h"


namespace insieme {
namespace core {
namespace analysis {

	/**
	 * A struct representing a member (ctor, dtor, member function) of a C++ record.
	 * The literal can be used to identify the function.
	 * The lambda contains the implementation.
	 * The member function is only set for member functions and it's implementation will be the literal.
	 */
	struct MemberProperties {
		LiteralPtr literal;
		LambdaExprPtr lambda;
		MemberFunctionPtr memberFunction;

		bool operator==(const MemberProperties& rhs) const;
	};

	/**
	 * A struct representing the input as well as the result of applying the c++ default/delete semantics.
	 */
	struct CppDefaultDeleteMembers {
		std::vector<MemberProperties> constructors;
		boost::optional<MemberProperties> destructor;
		std::vector<MemberProperties> memberFunctions;

		/**
		 * Creates an empty CppDefaultDeleteMembers object.
		 */
		CppDefaultDeleteMembers() : constructors(), destructor(), memberFunctions() {};

		/**
		 * Creates a new CppDefaultDeleteMembers object from the passed members lists.
		 */
		CppDefaultDeleteMembers(NodeManager& mgr, const ExpressionList& constructors, const ExpressionPtr& destructor, const MemberFunctionList& memberFunctions);

		/**
		 * Returns the list of literals representing the constructors which can be used to build a record type.
		 */
		ExpressionList getConstructorLiteralList();

		/**
		 * Returns the destructor literal or a nullptr.
		 */
		ExpressionPtr getDestructorLiteral();

		/**
		 * Returns the list of member functions which can be used to build a record type.
		 */
		MemberFunctionList getMemberFunctionList();
	};

	/**
	 * Applies the C++ semantics for defaulted and deleted members on the given members.
	 * This function returns an object containing the resulting members which can be used to build record types.
	 *
	 * Note that this function will only require the input member object to be filled with literals for all types of members, as well as
	 * MemberFunction objects for member functions, which will be used to get the name of these. The LambdaExprs need not be set.
	 * These literals may be marked as defaulted or deleted by use of core::annotations::markedDefaultedPreTU and core::annotations::markedDeletedPreTU.
	 * The resulting object will contain the remaining members. All new members will have their literals set correctly, as well as contain a
	 * lambda with the implementation. Note that the generated member functions will have their implementation set to the literal, not the lambda.
	 */
	CppDefaultDeleteMembers applyCppDefaultDeleteSemantics(const GenericTypePtr& thisType, const ParentList& parents, const FieldList& fields,
	                                                       const FieldInitMap& fieldInits,
	                                                       const CppDefaultDeleteMembers& inputMembers);

} // end namespace analysis
} // end namespace core
} // end namespace insieme

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

#include "insieme/core/ir.h"


namespace insieme {
namespace utils {
namespace map {
	template <class KeyPtr, class ValueType>
	struct PointerMap;
}
}

namespace core {
namespace analysis {

	using FieldInitMap = utils::map::PointerMap<FieldPtr, ExpressionPtr>;

	// ---------------------------- Getters for the 6 default members and their type --------------------------------------

	FunctionTypePtr buildDefaultDefaultConstructorType(const TypePtr& thisType);
	LambdaExprPtr   buildDefaultDefaultConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields, const FieldInitMap& fieldInits = {});

	FunctionTypePtr buildDefaultCopyConstructorType(const TypePtr& thisType);
	LambdaExprPtr   buildDefaultCopyConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields);

	FunctionTypePtr buildDefaultMoveConstructorType(const TypePtr& thisType);
	LambdaExprPtr   buildDefaultMoveConstructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields);

	FunctionTypePtr buildDefaultDestructorType(const TypePtr& thisType);
	LambdaExprPtr   buildDefaultDestructor(const TypePtr& thisType, const ParentList& parents, const FieldList& fields);

	FunctionTypePtr   buildDefaultCopyAssignOperatorType(const TypePtr& thisType);
	MemberFunctionPtr buildDefaultCopyAssignOperator(const TypePtr& thisType, const ParentList& parents, const FieldList& fields);

	FunctionTypePtr   buildDefaultMoveAssignOperatorType(const TypePtr& thisType);
	MemberFunctionPtr buildDefaultMoveAssignOperator(const TypePtr& thisType, const ParentList& parents, const FieldList& fields);


	// ---------------------------- Functions to check for default members --------------------------------------

	/**
	 * Determines whether the given tag type has a default constructor.
	 */
	bool hasDefaultConstructor(const TagTypePtr&);

	/**
	 * Returns the default constructor of the given type. Asserts if there is none.
	 */
	LambdaExprPtr getDefaultConstructor(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a copy constructor.
	 */
	bool hasCopyConstructor(const TagTypePtr&);

	/**
	 * Returns the copy constructor of the given type. Asserts if there is none.
	 */
	LambdaExprPtr getCopyConstructor(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a move constructor.
	 */
	bool hasMoveConstructor(const TagTypePtr&);

	/**
	 * Returns the move constructor of the given type. Asserts if there is none.
	 */
	LambdaExprPtr getMoveConstructor(const TagTypePtr&);

	/**
	 * Determines whether the given TagType has a destructor.
	 */
	bool hasDestructor(const TagTypePtr&);

	/**
	 * Returns the destructor of the given type. Asserts if there is none.
	 */
	LambdaExprPtr getDestructor(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a copy assignment operator.
	 */
	bool hasCopyAssignment(const TagTypePtr&);

	/**
	 * Returns the copy assignment operator implementation of the given type. Asserts if there is none.
	 */
	LambdaExprPtr getCopyAssignment(const TagTypePtr&);

	/**
	 * Determines whether the given tag type has a move assignment operator.
	 */
	bool hasMoveAssignment(const TagTypePtr&);

	/**
	 * Returns the move assignment operator implementation of the given type. Asserts if there is none.
	 */
	LambdaExprPtr getMoveAssignment(const TagTypePtr&);

	/**
	 * Determines whether the given constructor is one of the default generated ones for the given type.
	 */
	bool isaDefaultConstructor(const ExpressionPtr& ctor);

	/*
	 * Determines whether the given expression is a destructor and if it is a default destructor
	 */
	bool isaDefaultDestructor(const ExpressionPtr& dtor);

	/**
	 * Determines whether the given member function is one of the default generated assignment operators for the given type.
	 */
	bool isaDefaultAssignment(const MemberFunctionPtr& memberFunction);

	/**
	 * Checks whether the given node is a lambda or member function which is marked as a default member
	 */
	bool isaDefaultMember(const NodePtr& node);

} // end namespace analysis
} // end namespace core
} // end namespace insieme

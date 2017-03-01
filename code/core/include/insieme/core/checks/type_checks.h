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
 *
 */
#pragma once

#include "insieme/core/checks/ir_checks.h"

namespace insieme {
namespace core {
namespace checks {

// defines macros for generating CHECK declarations
#include "insieme/core/checks/check_macros.inc"

	SIMPLE_CHECK(Keyword, GenericType, true);
	SIMPLE_CHECK(FunctionKind, FunctionType, true);

	SIMPLE_CHECK(Parent, Parent, true);
	SIMPLE_CHECK(FreeTagTypeReferences, Node, true);
	SIMPLE_CHECK(TagTypeFields, TagType, true);
	SIMPLE_CHECK(EnumType, GenericType, true);

	SIMPLE_CHECK(ConstructorType, TagTypeBinding, true);
	SIMPLE_CHECK(DuplicateConstructorType, TagTypeBinding, true);
	SIMPLE_CHECK(DestructorType, TagTypeBinding, true);
	SIMPLE_CHECK(MemberFunctionType, TagTypeBinding, true);
	SIMPLE_CHECK(DuplicateMemberFunction, TagTypeBinding, true);
	SIMPLE_CHECK(DuplicateMemberField, Fields, true);
	SIMPLE_CHECK(MemberAccessElementTypeInTagType, TagTypeDefinition, true);

	SIMPLE_CHECK(CallExprType, CallExpr, false);
	SIMPLE_CHECK(BindExprType, BindExpr, false);
	SIMPLE_CHECK(ExternalFunctionType, Literal, false);
	SIMPLE_CHECK(ReturnType, Lambda, false);
	SIMPLE_CHECK(LambdaType, LambdaExpr, false);
	SIMPLE_CHECK(ArrayType, Node, true);
	SIMPLE_CHECK(GenericOps, CallExpr, false);

	SIMPLE_CHECK(DeclarationType, Declaration, false);
	SIMPLE_CHECK(DeclarationStmtType, DeclarationStmt, false);
	SIMPLE_CHECK(IfConditionType, IfStmt, false);
	SIMPLE_CHECK(ForStmtType, ForStmt, false);
	SIMPLE_CHECK(WhileConditionType, WhileStmt, false);
	SIMPLE_CHECK(SwitchExpressionType, SwitchStmt, false);
	SIMPLE_CHECK(RefDeclType, CallExpr, false);

	SIMPLE_CHECK(InitExprType, InitExpr, false);
	SIMPLE_CHECK(MemberAccessElementType, CallExpr, false);
	SIMPLE_CHECK(ComponentAccessType, CallExpr, false);

	SIMPLE_CHECK(BuiltInLiteral, Literal, false);

	SIMPLE_CHECK(RefCast, CastExpr, false);
	SIMPLE_CHECK(IllegalNumCast, CallExpr, false);
	SIMPLE_CHECK(IllegalNumTypeToInt, CallExpr, false);
	SIMPLE_CHECK(RefOfFunCast, CallExpr, false);
	SIMPLE_CHECK(IllegalTypeInstantiation, CallExpr, false);

	SIMPLE_CHECK(Cast, CastExpr, false);

	SIMPLE_CHECK(GenericZero, CallExpr, false);


	// TODO:
	//	- check that only concrete types are used for variables

	#undef SIMPLE_CHECK

} // end namespace check
} // end namespace core
} // end namespace insieme

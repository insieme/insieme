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


#include "insieme/core/forward_decls.h"
#include "insieme/core/analysis/default_delete_member_semantics.h"
#include "insieme/core/lang/reference.h"
#include "insieme/frontend/clang.h"

namespace insieme {
namespace core {
namespace analysis {
	struct MemberProperties;
}
}

namespace frontend {

namespace conversion {
	class Converter;
}

namespace utils {

	/// Replace RefTemps in initialization with memLoc
	///
	core::ExpressionPtr fixTempMemoryInInitExpression(const core::ExpressionPtr& memLoc, const core::ExpressionPtr& initExp);

	/// Replace RefTemps in the inits of the passed InitExpr with ref_decls
	///
	core::ExpressionPtr fixTempMemoryInInitExpressionInits(const core::ExpressionPtr& exprIn);

	/// Build a Cxx method call from its components
	///
	core::CallExprPtr buildCxxMethodCall(conversion::Converter& converter, const core::TypePtr& retType, const core::ExpressionPtr& callee,
		                                 const core::ExpressionPtr& thisArgument, clang::CallExpr::arg_const_range argumentRange);

	/// Creates an expression corresponding to a clang EnumConstantDecl
	///
	core::ExpressionPtr buildEnumConstantExpression(conversion::Converter& converter, const clang::EnumConstantDecl* decl);

	/// Convert a CXXConstructExpr to construct the object at a given ir memory location
	///
	core::ExpressionPtr convertConstructExpr(conversion::Converter& converter, const clang::CXXConstructExpr* constructExpr, const core::ExpressionPtr& memLoc);

	/// Materialize "retIr" if possible
	///
	core::ExpressionPtr convertMaterializingExpr(conversion::Converter& converter, const core::ExpressionPtr& retIr);

	/// Surrounds the given init with a ref cast to the initializedType if necessary according to materialization semantics
	///
	core::ExpressionPtr castInitializationIfNotMaterializing(const core::TypePtr& initializedType, const core::ExpressionPtr& init);

	/// Prepare the given 'this' argument if necessary
	///
	core::ExpressionPtr prepareThisExpr(conversion::Converter& converter, core::ExpressionPtr thisArg);

	/// Modify the body of a for loop to contain the given update expression before every possible exit point
	///
	core::StatementPtr addIncrementExprBeforeAllExitPoints(const core::StatementPtr& body, const core::StatementPtr& incrementExpression);

	/// Generate a correctly qualified "this" type based on irThisType and a clang object indicating the required qualifiers
	///
	template<class ClangQualifierIndicator>
	core::TypePtr getThisType(const ClangQualifierIndicator* funProto, const core::TypePtr irThisType) {
		auto refKind = core::lang::ReferenceType::Kind::Plain;
		switch(funProto->getRefQualifier()) {
		case clang::RefQualifierKind::RQ_LValue: refKind = core::lang::ReferenceType::Kind::CppReference; break;
		case clang::RefQualifierKind::RQ_RValue: refKind = core::lang::ReferenceType::Kind::CppRValueReference; break;
		case clang::RefQualifierKind::RQ_None: break; // stop warnings
		}
		return core::lang::buildRefType(irThisType, funProto->isConst(), funProto->isVolatile(), refKind);
	}

	core::TypePtr getThisType(conversion::Converter& converter, const clang::CXXMethodDecl* methDecl);

	inline const std::string getDummyAutoDeducedTypeName() {
		return "INSIEME_AUTO_TYPE";
	}

	/// Returns whether the passed CXXMethodDecl is one of the six default constructs of a class
	///
	bool isDefaultClassMember(const clang::CXXMethodDecl* methDecl);

	/// Creates a default constructor without parameters which calls the passed one with the default values correctly set
	///
	core::analysis::MemberProperties createDefaultCtorFromDefaultCtorWithDefaultParams(conversion::Converter& converter,
	                                                                                   const clang::CXXConstructorDecl* ctorDecl,
	                                                                                   const core::analysis::MemberProperties& defaultCtorWithParams);

	/// Creates a new struct or union (depending on the passed flag) with the given recordName, parents and fields.
	/// Also the constructors, destructor and member functions passed in recordMembers will be added.
	/// This function will correctly honour defaulted and deleted members as well as create all the missing default members according to C++ semantics.
	/// This function will also take care of registering all ctors/dtor/member functions in the IrTU (not the returned type itself though)
	///
	core::TagTypePtr createStructOrUnion(conversion::Converter& converter, bool isStruct,
	                                     const core::GenericTypePtr& recordName, const core::ParentList& parents, const core::FieldList& fields,
	                                     const core::analysis::FieldInitMap& fieldInits,
	                                     core::analysis::CppDefaultDeleteMembers recordMembersIn,
	                                     bool destructorVirtual, const core::PureVirtualMemberFunctionList& pvMembers,
										 const core::StaticMemberFunctionList& sFuns = core::StaticMemberFunctionList());
	core::TagTypePtr createStructOrUnion(conversion::Converter& converter, bool isStruct,
	                                     const core::GenericTypePtr& recordName, const core::FieldList& fields);

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

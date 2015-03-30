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

#include "insieme/core/frontend_ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/static_vars.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {

ExpressionPtr FrontendIRBuilder::getPureVirtual(const FunctionTypePtr& type) const {
	assert_true(type->isMemberFunction());
	const auto& ext = getNodeManager().getLangExtension<lang::IRppExtensions>();
	return callExpr(type, ext.getPureVirtual(), getTypeLiteral(type));
}

ExpressionPtr FrontendIRBuilder::toCppRef(const ExpressionPtr& ref) const {
	assert(ref && ref->getType()->getNodeType() == NT_RefType);
	const auto& ext = getNodeManager().getLangExtension<lang::IRppExtensions>();

	// avoid multiple nesting of wrapping / unwrapping
	if (core::analysis::isCallOf(ref, ext.getRefCppToIR())) {
		return ref.as<CallExprPtr>()[0];	// strip of previous call
	}

	// use converter function all
	return callExpr(core::analysis::getCppRef(ref->getType().as<RefTypePtr>()->getElementType()), ext.getRefIRToCpp(), ref);
}

ExpressionPtr FrontendIRBuilder::toConstCppRef(const ExpressionPtr& ref) const {
	assert(ref && ref->getType()->getNodeType() == NT_RefType);
	const auto& ext = getNodeManager().getLangExtension<lang::IRppExtensions>();

	// avoid multiple nesting of wrapping / unwrapping
	if (core::analysis::isCallOf(ref, ext.getRefConstCppToIR())) {
		return ref.as<CallExprPtr>()[0];	// strip of previous call
	}

	return callExpr(core::analysis::getConstCppRef(ref->getType().as<RefTypePtr>()->getElementType()), ext.getRefIRToConstCpp(), ref);
}

ExpressionPtr FrontendIRBuilder::toConstRValCppRef(const ExpressionPtr& ref) const {
	assert(ref && ref->getType()->getNodeType() == NT_RefType);
	const auto& ext = getNodeManager().getLangExtension<lang::IRppExtensions>();

	// avoid multiple nesting of wrapping / unwrapping
	if (core::analysis::isCallOf(ref, ext.getRefConstRValCppToIR())) {
		return ref.as<CallExprPtr>()[0];	// strip of previous call
	}

	return callExpr(core::analysis::getConstRValCppRef(ref->getType().as<RefTypePtr>()->getElementType()), ext.getRefIRToConstRValCpp(), ref);
}

ExpressionPtr FrontendIRBuilder::toIRRef(const ExpressionPtr& ref) const {
	const auto& ext = getNodeManager().getLangExtension<lang::IRppExtensions>();
	assert(ref && (analysis::isAnyCppRef(ref->getType())));

	// see whether this is a value which has just been wrapped
	if (analysis::isCallOf(ref, ext.getRefIRToCpp()) || analysis::isCallOf(ref, ext.getRefIRToConstCpp())) {
		return ref.as<CallExprPtr>()[0];		// strip of nested wrapper
	}

	// check whether it is a non-const reference
	if (analysis::isCppRef(ref->getType())) {
		return callExpr(refType(analysis::getCppRefElementType(ref->getType())), ext.getRefCppToIR(), ref);
	}
	// check whether it is a const reference
	if (analysis::isConstCppRef(ref->getType())) {
		return callExpr(refType(analysis::getCppRefElementType(ref->getType()), RK_SOURCE), ext.getRefConstCppToIR(), ref);
	}
	// check whether it is a rval reference
	if (analysis::isRValCppRef(ref->getType())) {
		return callExpr(refType(analysis::getCppRefElementType(ref->getType())), ext.getRefRValCppToIR(), ref);
	}
	// check whether it is a rval reference
	if (analysis::isConstRValCppRef(ref->getType())) {
		return callExpr(refType(analysis::getCppRefElementType(ref->getType()), RK_SOURCE), ext.getRefConstRValCppToIR(), ref);
	}
	assert_fail() << "failed to convert C++ reference to IR reference";
    return ref;
}



ExpressionPtr FrontendIRBuilder::initStaticVariable(const LiteralPtr& staticVariable, const ExpressionPtr& initValue, bool constant) const{
	const lang::StaticVariableExtension& ext = getNodeManager().getLangExtension<lang::StaticVariableExtension>();

	assert(staticVariable.getType().isa<RefTypePtr>());
	assert(ext.isStaticType(staticVariable->getType().as<core::RefTypePtr>().getElementType()));

	if (constant){
		return callExpr(refType(initValue->getType()), ext.getInitStaticConst(), staticVariable, initValue);
	}
	else{
		return callExpr(refType(initValue->getType()), ext.getInitStaticLazy(), staticVariable, wrapLazy(initValue));
	}
}

StatementPtr FrontendIRBuilder::createStaticVariable(const LiteralPtr& staticVariable) const {
	const lang::StaticVariableExtension& ext = getNodeManager().getLangExtension<lang::StaticVariableExtension>();

	return callExpr(ext.getCreateStatic(), staticVariable);
}


}   //namespace core
}   //namespace insieme

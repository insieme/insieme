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


#include "insieme/utils/logging.h"
#include "insieme/utils/unused.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/core/types/subtyping.h"
#include "insieme/frontend/utils/ir_utils.h"
#include "insieme/frontend/utils/debug.h"

using namespace insieme;


namespace insieme{
namespace frontend {
namespace utils {

////////////////////////////////////////////////////////////////////////////////////////////////////
// C++
////////////////////////////////////////////////////////////////////////////////////////////////////

// unwraps cppRef/constCppRef
core::ExpressionPtr unwrapCppRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr) {
	
	core::NodeManager& mgr = builder.getNodeManager();	
	core::TypePtr irType = expr->getType();
	if (core::analysis::isCppRef(irType)) {
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToIR(), expr);
	}
	else if (core::analysis::isConstCppRef(irType)) {
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefConstCppToIR(), expr);
	}
	return expr;
}



////////////////////////////////////////////////////////////////////////////////////////////////////
core::ExpressionPtr createSafeAssigment(core::ExpressionPtr& left, core::ExpressionPtr& right){	
	core::IRBuilder builder( left->getNodeManager() );
	assert(left.getType().isa<core::RefTypePtr>() && "we can not assign to a non ref type");
	// parameter is some kind of cpp ref, but we want to use the value, unwrap it
	if (!IS_CPP_REF_TYPE(left.getType().as<core::RefTypePtr>()->getElementType()) &&
		IS_CPP_REF_EXPR(right)){
		right = builder.deref( utils::unwrapCppRef(builder,right));
	}
	else{
		right = builder.tryDeref(right);
	}

	return builder.assign( left, right);
}
} // end utils namespace
} // end frontend namespace 
} // end insieme namespace 


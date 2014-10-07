/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_types.h"


/**
 * The cast tool is ment to be used to conduct all casts between IR types,
 * the idea is to have a centralized hub where to introduce new acurate type conversions,
 * this module should substitute the CAST ir node,
 */

namespace insieme {
namespace core {
namespace types {


	/**
	 * 	Hub entry point where the compatibility between types is checked and the more precise
	 * 	techique can be used
	 * 	 @param type, the target type, 
	 * 	 @param expr, the source expression we want to addapt
	 * 	 @return a casted/modified expression with the rightfull type
	 */
	core::ExpressionPtr smartCast (const core::TypePtr& type, const core::ExpressionPtr& expr);
	core::ExpressionPtr smartCast (const core::ExpressionPtr& expr, const core::TypePtr& type );


	/**
	 *
	 */
	core::ExpressionPtr castScalar(const core::TypePtr& trgTy, core::ExpressionPtr expr);

	core::ExpressionPtr castToBool (const core::ExpressionPtr& expr);

	ExpressionPtr convertExprToType(const IRBuilder& builder, const TypePtr& trgTy, ExpressionPtr expr);

	core::ExpressionPtr refScalarToRefArray(const core::ExpressionPtr& expr);
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//  some other tools
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	std::size_t getPrecission(const core::TypePtr& type, const core::lang::BasicGenerator& gen);

	bool isRefRef(const core::TypePtr& type);

	bool isArray(const core::TypePtr& type);
	bool isRefArray(const core::TypePtr& type);

	bool isVector(const core::TypePtr& type);
	bool isRefVector(const core::TypePtr& type);

	bool isNullPtrExpression(const core::ExpressionPtr& expr);


}
}
}

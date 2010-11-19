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

#pragma once

#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"

#include "insieme/simple_backend/code_management.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/name_generator.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace simple_backend {

using namespace insieme::core;

class ConversionContext;

/** Manages C function generation and lookup for named lambda expressions.
 ** */
class OldFunctionManager {
	ConversionContext& cc;

public:
	typedef std::unordered_map<LambdaExprPtr, CodePtr, hash_target<LambdaExprPtr>, equal_target<LambdaExprPtr>> FunctionMap;

private:
	FunctionMap functionMap;

public:
	OldFunctionManager(ConversionContext& cc) : cc(cc) { }

	CodePtr getFunction(const core::LambdaPtr& lambda);
	CodePtr getFunction(const core::LambdaExprPtr& lambda, const CodePtr& surrounding);
	CodePtr getFunctionLiteral(const LiteralPtr& literal);
	void writeFunctionCall(const Identifier& funId, const LambdaExprPtr& ptr);
};


class FunctionManager {

	/**
	 * The conversion context this manager is part of.
	 */
	ConversionContext& cc;

public:

	/**
	 * The information stored for each lambda expression within the program.
	 */
	struct LambdaCode {
		CodePtr closure;
		CodePtr function;
		TypeManager::FunctionTypeEntry* typeInfo;

		LambdaCode() : closure(), function(), typeInfo(NULL) { }
		LambdaCode(const CodePtr& closure, const CodePtr& function, TypeManager::FunctionTypeEntry* typeInfo)
			: closure(closure), function(function), typeInfo(typeInfo) { }
	};

private:

	/**
	 * A map linking Lambda expressions (recursive or non-recursive) to closure definitions and
	 * functions.
	 */
	utils::map::PointerMap<core::LambdaPtr, LambdaCode> functionDefinitions;

	/**
	 * A map l
	 */
	utils::map::PointerMap<core::LiteralPtr, CodePtr> externalFunctions;

public:

	FunctionManager(ConversionContext& conversionContext) : cc(conversionContext) { }

	/**
	 * Appends the name of the external function to the given context.
	 *
	 * @param context the code fragment the given external function call should be appended to.
	 * @param external the literal representing the external function.
	 */
	void createCallable(const CodePtr& context, const core::LiteralPtr& external);


	/**
	 * Appends the code required for handling the given lambda to the given context.
	 *
	 * @param context the code fragment the given lambda (including the variable capturing) should be appended to.
	 * @param lambda the lambda (including the capture initialization) to be handled
	 */
	void createCallable(const CodePtr& context, const core::CaptureInitExprPtr& lambda);

	void createCallable(const CodePtr& context, const core::LambdaExprPtr& lambda);

private:

	const LambdaCode& resolve(const LambdaPtr& lambda);

	CodePtr resolve(const LiteralPtr& literal);
};


}
}

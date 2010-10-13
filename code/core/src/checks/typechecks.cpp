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

#include "checks/typechecks.h"



namespace insieme {
namespace core {
namespace checks {

#define CAST(TargetType, value) \
	static_pointer_cast<const TargetType>(value)



MessageList NumArgumentCheck::visitCallExpr(const CallExprAddress& address) {

	MessageList res;

	// check type of function
	TypePtr funType = address->getFunctionExpr()->getType();
	assert( address->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Illegal function expression!");
	TypePtr arguments = CAST(FunctionType, funType)->getArgumentType();


	// compute the number of required arguments
	int requiredArguments = 1;
	if (arguments->getNodeType() == NT_TupleType) {
		requiredArguments = CAST(TupleType, arguments)->getElementTypes().size();
	}

	// check number of arguments
	int actualArguments = address->getArguments().size();
	if (requiredArguments != actualArguments) {
		res.push_back(Message(address,
				format("Incorrect number of arguments - expected: %d, actual: %d", requiredArguments, actualArguments),
				Message::ERROR));
	}
	return res;
}

MessageList ReturnTypeCheck::visitCallExpr(const CallExprAddress& address) {

	// TODO: extend with unification result!

	MessageList res;

	// check type of function
	TypePtr funType = address->getFunctionExpr()->getType();
	assert( address->getFunctionExpr()->getType()->getNodeType() == NT_FunctionType && "Illegal function expression!");
	TypePtr returnType = CAST(FunctionType, funType)->getReturnType();

	// check return type
	TypePtr actualType = address->getType();
	if (*returnType != *actualType) {
		res.push_back(Message(address,
				format("Incorrect return type of call expression - expected: %s, actual: %s",
						toString(*returnType).c_str(),
						toString(*actualType).c_str()),
				Message::ERROR));
	}
	return res;
}


#undef CAST

} // end namespace check
} // end namespace core
} // end namespace insieme

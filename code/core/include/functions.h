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

/*
 * functions.h
 *
 *  Defines the class used to represent and manage functions within the IR.
 *
 *  Created on: Aug 5, 2010
 *      Author: Herbert Jordan
 */
#pragma once

#include <stdexcept>
#include <string>
#include <vector>

#include "statements.h"
#include "stringutils.h"
#include "types.h"

using std::string;
using std::vector;


class Function {

	const FunctionTypePtr type;
	const vector<string> parameterNames;

	/**
	 * A flag determining whether
	 */
	const bool external;

	/**
	 * The statement forming the body of the represented function.
	 */
	const StmtPtr body;

	/**
	 * A private constructor for this type requesting values for all member fields.
	 *
	 * @param type	the type of the new function
	 * @param parameterNames	the names of the parameters. Those will be available within
	 * 			the body as variables of the corresponding type. It has to be ensured that
	 * 			the number of parameter names and input type parameters is equivalent.
	 * @param external	should be set to true if this function is defined externally
	 */
	Function(FunctionTypePtr type, vector<string> parameterNames, bool external = true, StmtPtr body = NoOpStmt::getInstance()) :
		type(type), parameterNames(parameterNames), external(external), body(body) {


		// TODO: check whether parameter types are fitting

		// TODO: check
	}

public:
};

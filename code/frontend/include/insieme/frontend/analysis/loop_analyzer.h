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

#include <iostream>
#include <set>
#include <exception>
//#include <stdexcept>

#include "insieme/core/ir_expressions.h"

namespace clang {

class VarDecl;
class ForStmt;

} // end clang namespace

namespace insieme {
namespace frontend {
namespace conversion {

class Converter;

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

namespace insieme {
namespace frontend {
namespace analysis {

class LoopNormalizationError: public std::exception {
	std::string cause;
public:
	LoopNormalizationError(): std::exception() { }
	LoopNormalizationError(const std::string& cause): std::exception(), cause(cause) { }
	const char* what() const throw() { return cause.c_str(); }
	~LoopNormalizationError() throw() { }
};

class InductionVariableNotFoundException: public LoopNormalizationError {
public:
	InductionVariableNotFoundException(): LoopNormalizationError("failed to determine loop induction variable") { }
};

class InductionVariableNotReadOnly: public LoopNormalizationError {
public:
	InductionVariableNotReadOnly(): LoopNormalizationError("it seems that the induction variable is written inside of the loop") { }
};

using insieme::frontend::conversion::Converter;

/**
 * Implements the checks to determine loop properties, such as induction variable, increment step and condition
 */
class LoopAnalyzer {

private:

	enum loopDir{ 
		LOOP_UP,
		LOOP_DOWN,
	};
	Converter& 	convFact;

	insieme::core::VariableList		conditionVars;			// variables used in conditions

	insieme::core::ExpressionPtr	originalInductionExpr;  // old induction expressio
	insieme::core::VariablePtr		inductionVar;  			// New read only induction var

	insieme::core::ExpressionPtr	normalizedInductionExpr; // Expression to use as iterator (normalized)
	insieme::core::ExpressionPtr	normalizedIterations; 	// normalized number of iterations

	insieme::core::ExpressionPtr 	incrExpr;				// normalized increment ( +1 )
	insieme::core::ExpressionPtr    stepExpr; 				// step of each iteration

	insieme::core::ExpressionPtr    initValue;				// lowe bonduary for real iteration
	insieme::core::ExpressionPtr    endValue;				// upper bonduary

	insieme::core::StatementList	preStmts;  				// statements that need to be reproduced BEFORE the loop
	insieme::core::StatementList	postStmts;  			// statements that need to be reproduced AFTER the loop
	insieme::core::StatementList	firstStmts;				// statements that need to be reproduced at the BEGINNING of loop body
	insieme::core::StatementList	lastStmts;				// statements that need to be reproduced at the END of loop body

	loopDir direction;			// loop up or loop down, not used
	bool    loopToBounduary;    // whenever to loop until value is equal, or until value is less than
	bool    restoreValue;       // if induction variable was defined outside of scope, we need to give it a final value


	void findConditionVariables(const clang::ForStmt* forStmt);
	void findInductionVariable(const clang::ForStmt* forStmt);
	void handleIncrExpr(const clang::ForStmt* forStmt);
	void handleCondExpr(const clang::ForStmt* forStmt);

public:

	LoopAnalyzer(const clang::ForStmt* forStmt, Converter& convFact);

	const insieme::core::ExpressionPtr 	getOriginalInductionExpr()  const { return originalInductionExpr; }
	const insieme::core::ExpressionPtr 	getInductionExpr() const { return normalizedInductionExpr; }

	const insieme::core::StatementList& getPreStmts() const { return preStmts;}
	const insieme::core::StatementList& getPostStmts() const { return postStmts;}

	/**
	 * creates a loop which is normalized. but is up to the caller to generate the right body
	 */
	insieme::core::StatementPtr  getNormalizedLoop(const insieme::core::StatementPtr& body) const; 
};

} // End analysis namespace
} // End froentend namespace
} // End insieme namespace

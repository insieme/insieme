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

#ifndef SCOPVISITOR_H
#define SCOPVISITOR_H

#include <stack>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/transform/polyhedral/scop.h"
#include "insieme/transform/polyhedral/scoplist.h"

namespace insieme { namespace transform { namespace polyhedral { namespace novel {

class SCoPVisitor: public insieme::core::IRVisitor<void, insieme::core::Address>{

	std::stack<SCoP> scopstack;
	unsigned int lvl;

public:

	/// The public variable scoplist holds the result from visiting all nodes in a program. It should be
	/// used/passed/copied, then the SCoPVisitor can be destructed.
	insieme::transform::polyhedral::novel::SCoPList scoplist;

	SCoPVisitor(const insieme::core::ProgramAddress &node);
	void printNode    (const insieme::core::NodeAddress &node, std::string id="", unsigned int start=0, int count=-1);
	void visitNode    (const insieme::core::NodeAddress &node);
	void visitChildren(const insieme::core::NodeAddress &node);

	void visitLambdaExpr     (const insieme::core::LambdaExprAddress      &expr);
	void visitForStmt        (const insieme::core::ForStmtAddress         &stmt);
	void visitVariable       (const insieme::core::VariableAddress        &expr);
	void visitDeclarationStmt(const insieme::core::DeclarationStmtAddress &node);
};

}}}}

#endif // SCOPVISITOR_H

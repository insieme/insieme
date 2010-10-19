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

#include <vector>
#include <string>

#include "program.h"
#include "statements.h"
#include "expressions.h"
#include "types.h"
#include "lang_basic.h"

namespace insieme {
namespace core {
	
using std::vector;
using std::string;

class ASTBuilder {

	SharedNodeManager sharedManager;
	NodeManager& manager;

public:
	ASTBuilder(const SharedNodeManager& manager = SharedNodeManager(new NodeManager())) : sharedManager(manager), manager(*sharedManager) { }

	typedef vector<VariablePtr> ParamList;
	typedef std::pair<Identifier, TypePtr> Entry;
	typedef vector<Entry> Entries;
	typedef vector<TypePtr> ElementTypeList;

	typedef std::pair<Identifier, ExpressionPtr> Member;
	typedef std::vector<Member> Members;

	typedef std::vector<DeclarationStmtPtr> LocalDecls;
	typedef std::pair<ExpressionPtr, StatementPtr> GuardedStmt;
	typedef std::vector<GuardedStmt> GuardedStmts;

	typedef std::unordered_map<TypeVariablePtr, TypePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> RecTypeDefs;
	typedef std::unordered_map<VariablePtr, LambdaExprPtr, hash_target<VariablePtr>, equal_target<VariablePtr>> RecFunDefs;


	SharedNodeManager getNodeManager() const {
		return sharedManager;
	}

	ProgramPtr createProgram(const Program::EntryPointSet& entryPoints = Program::EntryPointSet());


	// ---------------------------- Create Derived Types ----------------------------

	lang::UnitTypePtr getUnitType() const;
	lang::BoolTypePtr getBoolType() const;
	lang::IntTypePtr  getIntType (unsigned short size) const;
	lang::UIntTypePtr getUIntType(unsigned short size) const;
	lang::RealTypePtr getRealType(unsigned short size) const;


#include "ast_builder.inl"

};

} // namespace core
} // namespace insieme

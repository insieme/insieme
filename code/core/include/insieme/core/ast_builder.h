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

#include "insieme/core/program.h"
#include "insieme/core/statements.h"
#include "insieme/core/expressions.h"
#include "insieme/core/types.h"
#include "insieme/core/lang/basic.h"

namespace insieme {
namespace core {

using std::vector;
using std::string;

class ASTBuilder {

	/**
	 * This manager is only used in case the no manager reference is based
	 * within the constructor.
	 */
	std::shared_ptr<NodeManager> internalManager;

	/**
	 * The manager used by this builder to create new nodes.
	 */
	NodeManager& manager;

	lang::BasicGenerator typeGen;

public:
	ASTBuilder() : internalManager(std::make_shared<NodeManager>()), manager(*internalManager), typeGen(manager) { }
	ASTBuilder(NodeManager& manager) : manager(manager), typeGen(manager) { }


	typedef std::pair<Identifier, TypePtr> Entry;
	typedef vector<Entry> Entries;
	typedef vector<TypePtr> ElementTypeList;

	typedef std::pair<Identifier, ExpressionPtr> Member;
	typedef std::vector<Member> Members;

	typedef std::vector<DeclarationStmtPtr> LocalDecls;
	typedef std::pair<LambdaExprPtr, LambdaExprPtr> GuardedStmt;
	typedef std::vector<GuardedStmt> GuardedStmts;

	typedef std::unordered_map<TypeVariablePtr, TypePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> RecTypeDefs;
	typedef std::unordered_map<VariablePtr, LambdaExprPtr, hash_target<VariablePtr>, equal_target<VariablePtr>> RecFunDefs;

	typedef Lambda::CaptureList CaptureList;
	typedef Lambda::ParamList ParamList;

	typedef LambdaDefinition::Definitions Definitions;

	typedef CaptureInitExpr::Initializations CaptureInits;

	/**
	 * Obtains a reference to the node manager used by this builder.
	 */
	NodeManager& getNodeManager() const {
		return manager;
	}

	ProgramPtr createProgram(const Program::EntryPointSet& entryPoints = Program::EntryPointSet(), bool main = false);

#include "ast_builder.inl"

	LiteralPtr stringVal(const char* str) const;

	// Referencing
	CallExprPtr deref(const ExpressionPtr& subExpr) const;

	// Call Expressions
	CallExprPtr callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const;
	CallExprPtr callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const;
	CallExprPtr callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const;
	// For the methods below, the return type is deduced from the functionExpr's function type
	CallExprPtr callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments = vector<ExpressionPtr>()) const;
	CallExprPtr callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const;
	CallExprPtr callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const;
	CallExprPtr callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const;

	// Lambda Expressions
	LambdaExprPtr lambdaExpr(const StatementPtr& body, const ParamList& params = ParamList()) const;
	LambdaExprPtr lambdaExpr(const StatementPtr& body, const CaptureList& captures, const ParamList& params = ParamList()) const;
	LambdaExprPtr lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const ParamList& params = ParamList()) const;
	LambdaExprPtr lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureList& captures = CaptureList(), const ParamList& params = ParamList()) const;
	// Direct lambda with capture initialization // TODO
	LambdaExprPtr lambdaExpr(const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params = ParamList()) const;
	LambdaExprPtr lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params = ParamList()) const;


	// Utilities
private:
	static ElementTypeList extractParamTypes(const ParamList& params);
};

} // namespace core
} // namespace insieme

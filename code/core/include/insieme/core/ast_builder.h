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

#include "insieme/core/ast_node.h"
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

public:
	ASTBuilder() : internalManager(std::make_shared<NodeManager>()), manager(*internalManager) { }
	ASTBuilder(NodeManager& manager) : manager(manager) { }


	typedef vector<TypePtr> TypeList;

	typedef NamedCompositeType::Entry Entry;
	typedef NamedCompositeType::Entries Entries;

	typedef StructExpr::Member Member;
	typedef StructExpr::Members Members;

	typedef JobExpr::LocalDecls LocalDecls;
	typedef JobExpr::GuardedStmt GuardedStmt;
	typedef JobExpr::GuardedStmts GuardedStmts;

	typedef RecTypeDefinition::RecTypeDefs RecTypeDefs;

	typedef Lambda::CaptureList CaptureList;
	typedef Lambda::ParamList ParamList;

	typedef LambdaDefinition::Definitions Definitions;

	typedef CaptureInitExpr::Values Values;
	typedef utils::map::PointerMap<VariablePtr, ExpressionPtr> CaptureInits;

	/**
	 * Obtains a reference to the node manager used by this builder.
	 */
	NodeManager& getNodeManager() const {
		return manager;
	}

	/**
	 * Obtains a reference to the basic generator within the node manager.
	 */
	const lang::BasicGenerator& getBasicGenerator() const {
		return manager.basic;
	}

	ProgramPtr createProgram(const Program::EntryPointSet& entryPoints = Program::EntryPointSet(), bool main = false);

#include "ast_builder.inl"

	// Literals
	LiteralPtr stringLit(const char* str) const;
    LiteralPtr intLit(const int val) const;
    LiteralPtr uintLit(const unsigned int val) const;

	// Referencing
	CallExprPtr deref(const ExpressionPtr& subExpr) const;
	CallExprPtr refVar(const ExpressionPtr& subExpr) const;
	CallExprPtr refNew(const ExpressionPtr& subExpr) const;

	ExpressionPtr invertSign(const ExpressionPtr& subExpr) const;

	// Vectors
	CallExprPtr vectorSubscript(const ExpressionPtr& vec, const ExpressionPtr& index) const;
	//CallExprPtr vectorSubscript(const ExpressionPtr& vec, unsigned index) const;

	// Compound Statements
	CompoundStmtPtr compoundStmt(const StatementPtr& s1, const StatementPtr& s2) const;
	CompoundStmtPtr compoundStmt(const StatementPtr& s1, const StatementPtr& s2, const StatementPtr& s3) const;

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

	// Direct lambda with capture initialization
	CaptureInitExprPtr lambdaExpr(const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params = ParamList()) const;
	CaptureInitExprPtr lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params = ParamList()) const;

	// Creation of thread number ranges
	CallExprPtr getThreadNumRange() const;
	CallExprPtr getThreadNumRange(unsigned min) const;
	CallExprPtr getThreadNumRange(unsigned min, unsigned max) const;

	// Direct call expression of getThreadGroup
	CallExprPtr getThreadGroup(ExpressionPtr level = ExpressionPtr()) const;
	CallExprPtr getThreadId(ExpressionPtr level = ExpressionPtr()) const;

	// Direct call expression of barrier
	CallExprPtr barrier(ExpressionPtr threadgroup = ExpressionPtr()) const;

	// Direct call expression of pfor
	CallExprPtr pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step = ExpressionPtr()) const;

	// Build a Call expression for a pfor that mimics the effect of the given for statement
	CallExprPtr pfor(const ForStmtPtr& initialFor) const;

	// Utilities
private:
	static TypeList extractParamTypes(const ParamList& params);
};

} // namespace core
} // namespace insieme

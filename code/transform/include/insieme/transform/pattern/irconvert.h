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

#include <string>
#include <memory>
#include <ostream>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/transform/pattern/structure.h"
#include "insieme/transform/pattern/pattern.h"

namespace insieme {
using namespace core;

namespace transform {
namespace pattern {

/** Represents a complex IR node (with children).
 * Children are converted lazily on demand.
 */
class IRTree : public Tree {
public:
	typedef std::function<void(IRTree& self)> EvalFunctor;
protected:
	const core::NodeAddress data;
	bool evaluated;
	const EvalFunctor evalFunctor;
public:
	IRTree(const core::NodeAddress& data, const EvalFunctor& evalFunctor = [](IRTree& self){}) 
		: Tree(data->getNodeType()), data(data), evaluated(false), evalFunctor(evalFunctor) {}
	
	virtual const core::NodeAddress& getData() const { return data; }
	virtual std::ostream& printTo(std::ostream& out) const;
	virtual bool operator==(Tree& other);
	void evaluate();
	virtual std::vector<TreePtr>& getSubTrees();
};

/** Represents a simple IR node (no children, but an address).
*/
class IRLeaf : public IRTree {
	static std::vector<TreePtr> emptyTreeVec;

public:
	IRLeaf(const core::NodeAddress& data) : IRTree(data) {}
		
	virtual std::ostream& printTo(std::ostream& out) const;
	virtual bool operator==(Tree& other);
};

/** Represents a non-node IR structure (no children, no address).
*/
class IRBlob : public Tree {
	const string blob;
public:
	IRBlob(const string& blob) : Tree(100000), blob(blob) {}
		
	virtual std::ostream& printTo(std::ostream& out) const;
	virtual bool operator==(Tree& other);
};

/** Visitor that converts any IR address to the tree structure used by the pattern matcher.
*/
class ConversionVisitor : public core::ASTVisitor<TreePtr, Address> {

	const IRTree::EvalFunctor& getDefaultChildEvaluator();

public:
	ConversionVisitor() : core::ASTVisitor<TreePtr, Address>(true) { }

	// TYPES

	TreePtr visitTypeVariable(const TypeVariableAddress& addr);
	TreePtr visitFunctionType(const FunctionTypeAddress& addr);
	TreePtr visitTupleType(const TupleTypeAddress& addr);
	TreePtr visitRecType(const RecTypeAddress& addr);

	TreePtr visitGenericType(const GenericTypeAddress& addr);

	TreePtr visitStructType(const StructTypeAddress& addr);
	TreePtr visitUnionType(const UnionTypeAddress& addr);

	// STATEMENTS

	TreePtr visitBreakStmt(const BreakStmtAddress& addr);
	TreePtr visitContinueStmt(const ContinueStmtAddress& addr);
	TreePtr visitReturnStmt(const ReturnStmtAddress& addr);
	TreePtr visitDeclarationStmt(const DeclarationStmtAddress& addr);
	TreePtr visitCompoundStmt(const CompoundStmtAddress& addr);
	TreePtr visitWhileStmt(const WhileStmtAddress& addr);
	TreePtr visitForStmt(const ForStmtAddress& addr);
	TreePtr visitIfStmt(const IfStmtAddress& addr);
	TreePtr visitSwitchStmt(const SwitchStmtAddress& addr);

	// EXPRESSIONS

	TreePtr visitVariable(const VariableAddress& addr);
	TreePtr visitLambdaExpr(const LambdaExprAddress& addr);
	TreePtr visitBindExpr(const BindExprAddress& addr);
	TreePtr visitLiteral(const LiteralAddress& addr);
	TreePtr visitCallExpr(const CallExprAddress& addr);
	TreePtr visitCastExpr(const CastExprAddress& addr);
	TreePtr visitJobExpr(const JobExprAddress& addr);
	TreePtr visitTupleExpr(const TupleExprAddress& addr);
	TreePtr visitVectorExpr(const VectorExprAddress& addr);
	TreePtr visitStructExpr(const StructExprAddress& addr);
	TreePtr visitUnionExpr(const UnionExprAddress& addr);
	TreePtr visitMemberAccessExpr(const MemberAccessExprAddress& addr);
	TreePtr visitTupleProjectionExpr(const TupleProjectionExprAddress& addr);

	// PROGRAM

	TreePtr visitProgram(const ProgramAddress& addr);

	// SUPPORTING

	TreePtr visitIdentifier(const IdentifierAddress& addr);

	TreePtr visitVariableIntTypeParam(const VariableIntTypeParamAddress& addr);
	TreePtr visitConcreteIntTypeParam(const ConcreteIntTypeParamAddress& addr);
	TreePtr visitInfiniteIntTypeParam(const InfiniteIntTypeParamAddress& addr);

	TreePtr visitLambda(const LambdaAddress& addr);
	TreePtr visitLambdaDefinition(const LambdaDefinitionAddress& addr);

	TreePtr visitRecTypeDefinition(const RecTypeDefinitionAddress& addr);

	// MARKER

	TreePtr visitMarkerExpr(const MarkerExprAddress& addr);
	TreePtr visitMarkerStmt(const MarkerStmtAddress& addr);

};

inline TreePtr convertIR(const NodeAddress& addr) {
	ConversionVisitor c;
	return c.visit(addr);
}
inline TreePtr convertIR(const NodePtr& node) {
	return convertIR(NodeAddress(node));
}

} // end namespace pattern
} // end namespace transform
} // end namespace insieme

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

#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/ir_checks.h"

#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/string_utils.h"

#include <ostream>
#include <sstream>

using namespace insieme::core;
using namespace insieme::core::checks;

namespace insieme {
namespace driver {

template <class NodeTy, class NodeIdTy, class ProperyIdTy, class PropertyValueTy>
struct GraphBuilder {

	typedef std::map<const ProperyIdTy, PropertyValueTy> Decorator;

	class Node: public Decorator {
		NodeIdTy id;
	public:
		Node(const NodeIdTy& id): id(id) { }
		const NodeIdTy& getId() const { return id; }
	};

	class Link: public Decorator {
		NodeIdTy src;
		NodeIdTy dest;
	public:
		Link(const NodeIdTy& src, const NodeIdTy& dest) : src(src), dest(dest) { }

		const NodeIdTy& getSrc() const { return src; }
		const NodeIdTy& getDest() const { return dest; }
	};

	template <class GraphNodeTy>
	void addNode(const NodeTy& node) { addNode(GraphNodeTy( getNodeId(node) )); }
	virtual void addNode(const Node& node) = 0;

	virtual void addLink(const Link& link) = 0;
	virtual NodeIdTy getNodeId(const NodeTy& fromNode) = 0;
};

enum NodeProperty{ LABEL, SHAPE, STYLE, DIRECTION, HEIGHT, WIDTH, COLOR };

class DOTGraphBuilder: public GraphBuilder<core::NodePtr, size_t, NodeProperty, std::string> {
	std::ostream& 	out;
public:
	typedef GraphBuilder<core::NodePtr, size_t, NodeProperty, std::string>::Node 		Node;
	typedef GraphBuilder<core::NodePtr, size_t, NodeProperty, std::string>::Link 		Link;
	typedef GraphBuilder<core::NodePtr, size_t, NodeProperty, std::string>::Decorator	Properties;

	DOTGraphBuilder(std::ostream& out): out(out) { }

	virtual void addNode(const Node& node);
	virtual void addLink(const Link& link);
	virtual size_t getNodeId(const core::NodePtr& fromNode);
};

class ASTPrinter: public insieme::core::IRVisitor<> {
public:
	typedef GraphBuilder<core::NodePtr, size_t, NodeProperty, std::string> IRBuilder;
	typedef std::shared_ptr<IRBuilder> IRBuilderPtr;

	ASTPrinter(const IRBuilderPtr& builder, const MessageList& errors);
	// Types
	void visitTypeVariable(const TypeVariablePtr& typeVar);
	void visitGenericType(const GenericTypePtr& genTy);
	void visitIntTypeParam(const IntTypeParamPtr& intTyParm);
	void visitFunctionType(const FunctionTypePtr& funcType);
	void visitTupleType(const TupleTypePtr& tupleTy);
	void visitNamedCompositeType(const NamedCompositeTypePtr& compTy);
	void visitRecType(const RecTypePtr& recTy);
	void visitRecTypeDefinition(const RecTypeDefinitionPtr& recTy);

	// Statements
	void visitCompoundStmt(const CompoundStmtPtr& comp);
	void visitForStmt(const ForStmtPtr& forStmt);
	void visitIfStmt(const IfStmtPtr& ifStmt);
	void visitWhileStmt(const WhileStmtPtr& whileStmt);
	void visitDeclarationStmt(const DeclarationStmtPtr& declStmt);
	void visitReturnStmt(const ReturnStmtPtr& retStmt);

	// Expressions
	void visitLambdaExpr(const LambdaExprPtr& lambdaExpr);
	void visitLambda(const LambdaPtr& lambda);
	void visitLambdaDefinition(const LambdaDefinitionPtr& lambda);
	void visitVariable(const VariablePtr& var);
	void visitCallExpr(const CallExprPtr& callExpr);
	void visitCastExpr(const CastExprPtr& castExpr);
	void visitLiteral(const LiteralPtr& lit);
	void visitVectorExpr(const VectorExprPtr& init);

	void visitStatement(const insieme::core::StatementPtr& stmt);
	void visitNode(const insieme::core::NodePtr& node);
	void visitProgram(const core::ProgramPtr& prog);
private:
	size_t dummyNodeID;
	IRBuilderPtr builder;
	const MessageList& errors;
};

ASTPrinter makeDotPrinter(std::ostream& out, const MessageList& errors);

void printDotGraph(const insieme::core::NodePtr& root, const MessageList& errors, std::ostream& out);

} // end namespace driver
} // end namespace insieme

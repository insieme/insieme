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

#include "dot_printer.h"

using namespace insieme;

namespace {


class DotGraphBuilder: public insieme::driver::GraphBuilder<insieme::core::NodePtr, size_t> {
	std::ostream& 	out;

public:
	enum DotAttributes{ LABEL, SHAPE, STYLE, DIRECTION, HEIGHT, WIDTH, COLOR};

	typedef GraphBuilder::Decorator<DotAttributes, std::string> DotProperties;

	static std::string attributeIdToString(const DotAttributes& attr) {
		switch(attr) {
		case LABEL: return "label";
		case SHAPE: return "shape";
		case STYLE: return "style";
		case DIRECTION: return "dir";
		case HEIGHT: return "height";
		case WIDTH: return "width";
		case COLOR: return "color";
		default:	assert(false);
		}
	}

	void dumpProperties(const DotProperties& dec, std::ostream& out) const {
		out << "[" << join(", ", dec,
			[ ](std::ostream& out, const DotProperties::value_type& cur) {
				out << attributeIdToString(cur.first) << "=" << cur.second;
			}) <<
		"];" << std::endl;
	}

	struct DotNode: public DotGraphBuilder::Node, public DotProperties {
		DotNode(size_t id): Node(id) { }

		DotNode(size_t id, const std::string& label) : Node(id) {
			if(!label.empty())
				(*this)[LABEL] = label;
		}
	};

	struct DotLink: public DotGraphBuilder::Link, public DotProperties {
		DotLink(const size_t& src, const size_t& dest, const std::string& label): Link(src, dest) {
			if(!label.empty())
				(*this)[LABEL] = label;
		}
	};

	struct TypeNode: public DotNode {
		TypeNode(size_t id, const std::string& label): DotNode(id, label) {
			(*this)[SHAPE] = "ellipse";
		}
	};

	struct StmtNode: public DotNode {
		StmtNode(size_t id, const std::string& label): DotNode(id, label) {
			(*this)[SHAPE] = "box";
			(*this)[STYLE] = "filled";
		}
	};

	struct JunctionNode: public DotNode {
		JunctionNode(size_t id): DotNode(id, "\"\"") {
			(*this)[SHAPE] = "circle";
			(*this)[STYLE] = "filled";
			(*this)[HEIGHT] = ".15";
			(*this)[WIDTH] = ".15";
		}
	};

	struct AnnotationNode: public DotNode {
		AnnotationNode(size_t id, const std::string& label): DotNode(id, label) {
			(*this)[SHAPE] = "hexagon";
			(*this)[STYLE] = "dashed";
		}
	};

	struct LiteralNode: public DotNode {
		LiteralNode(size_t id, const std::string& label): DotNode(id, label) {
			(*this)[SHAPE] = "diamond";
		}
	};

	DotGraphBuilder(std::ostream& out): out(out) { }

	void addNode(const GraphBuilder<insieme::core::NodePtr, size_t>::Node& node) {
		out << node.getId()  << "\t";
		dumpProperties( static_cast<const DotNode&>(node), out );
	}

	void addLink(const GraphBuilder<insieme::core::NodePtr, size_t>::Link& link) {
		out << link.getSrc() << " -> " << link.getDest() << "\t";
		dumpProperties( static_cast<const DotLink&>(link), out );
	}

	size_t getNodeId(const core::NodePtr& fromNode) { return (size_t) &*fromNode; }
};

template <class NodeId, class NodeIdTy>
void visitAnnotationList(insieme::driver::GraphBuilder<NodeId,NodeIdTy>& builder, size_t parentId, const core::AnnotationMap& map) {
	for(core::AnnotationMap::const_iterator it = map.begin(), end = map.end(); it != end; ++it) {
		size_t annotationId = (size_t)&*it->second;
		std::string label = it->second->getAnnotationName();
		if(!it->second->toString().empty())
			label = "\"" + label + " \\n[" + it->second->toString() + "]\"";
		builder.addNode(DotGraphBuilder::AnnotationNode(annotationId,label));
		DotGraphBuilder::DotLink l1(parentId, annotationId, "");
		l1[DotGraphBuilder::DIRECTION] = "none";
		l1[DotGraphBuilder::STYLE] = "dashed";
		builder.addLink( l1 );
	}
}


template <class NodeId, class NodeIdTy, class ElemTy>
void visitChildList(insieme::driver::GraphBuilder<NodeId,NodeIdTy>& builder,
		const std::vector<ElemTy>& children, const core::NodePtr& parent, const std::string& labelPrefix)
{
	unsigned elemCount = 0;
	std::for_each(children.begin(), children.end(), [ & ](const ElemTy& curr) {
		std::string label = labelPrefix + (children.size() > 1 ? ("_" + utils::numeric_cast<std::string>(elemCount++)) : "");
		if(curr.getAnnotations().empty()) {
			builder.addLink( DotGraphBuilder::DotLink(builder.getNodeId(parent), builder.getNodeId(curr), label) );
		} else {
			// introducing an intermediate node to attach annotations to the pointer
			size_t interId = builder.getNodeId(parent) ^ builder.getNodeId(curr);

			DotGraphBuilder::JunctionNode node(interId);
			builder.addNode(node);

			DotGraphBuilder::DotLink l1(builder.getNodeId(parent), interId, label);
			l1[DotGraphBuilder::DIRECTION] = "none";
			builder.addLink( l1 );
			builder.addLink( DotGraphBuilder::DotLink(interId, builder.getNodeId(curr), "") );

			visitAnnotationList(builder, interId, curr.getAnnotations());
		}
	});
}

void checkSemanticErrors(const MessageList& errors, DotGraphBuilder::DotNode& currNode, const core::NodePtr& node) {
	std::for_each(errors.begin(), errors.end(), [&currNode, node](const Message& cur) {
		if(*node == *cur.getAddress().getAddressedNode()) {
			currNode[DotGraphBuilder::COLOR] = "red";
			currNode[DotGraphBuilder::LABEL] =
					"\"" + currNode[DotGraphBuilder::LABEL] + "\\n{ ERR_CODE: " + utils::numeric_cast<std::string>(cur.getErrorCode()) + "}\"";
		}
	});
}

}

namespace insieme {
namespace driver {

#define NODE_ID(node)	builder.getNodeId(node)

ASTPrinter::ASTPrinter(const IRBuilderPtr& builder, const MessageList& errors): builder(builder), errors(errors) { }

void ASTPrinter::visitGenericType(const core::GenericTypePtr& genTy) {
	std::ostringstream ss("");
	// special handling for integer type parameters
	if(!genTy->getIntTypeParameter().empty()) {
		ss << "<" << join(", ", genTy->getIntTypeParameter(), [ ](std::ostream& out, const IntTypeParam& cur) {
			out << (cur.isConcrete() ? insieme::utils::numeric_cast<std::string>(cur.getValue()) : ""+cur.getSymbol());
		}) << ">";
	}
	DotGraphBuilder::TypeNode genNode(builder->getNodeId(genTy), "\"" + genTy->getFamilyName().getName() + " " + ss.str() + "\"");
	checkSemanticErrors(errors, genNode, genTy);
	builder->addNode(genNode);

	visitAnnotationList(*builder, builder->getNodeId(genTy), genTy->getAnnotations());
	visitChildList(*builder, genTy->getTypeParameter(), genTy, "typeVar");
}

void ASTPrinter::visitFunctionType(const FunctionTypePtr& funcType) {
	DotGraphBuilder::TypeNode funcNode( builder->getNodeId(funcType), "func");
	checkSemanticErrors(errors, funcNode, funcType);
	builder->addNode(funcNode);

	visitAnnotationList(*builder, builder->getNodeId(funcType), funcType->getAnnotations());
	visitChildList(*builder, toVector(funcType->getReturnType()), funcType, "retTy");
	visitChildList(*builder, toVector(funcType->getArgumentType()), funcType, "argTy");
}

void ASTPrinter::visitTupleType(const TupleTypePtr& tupleTy) {
	DotGraphBuilder::TypeNode tupleNode( builder->getNodeId(tupleTy), "tuple");
	checkSemanticErrors(errors, tupleNode, tupleTy);
	builder->addNode(tupleNode);

	visitAnnotationList(*builder, builder->getNodeId(tupleTy), tupleTy->getAnnotations());
	visitChildList(*builder, tupleTy->getElementTypes(), tupleTy, "elemTy");
}

void ASTPrinter::visitNamedCompositeType(const NamedCompositeTypePtr& compTy) {
	std::string name;
	if(dynamic_pointer_cast<const StructType>(compTy))
		name = "structType";
	else
		name = "unionType";

//		out << "\t" << (size_t)&*compTy << "\t[label=\"" << name << "\", shape=ellipse]" << std::endl;
	// TODO
}

void ASTPrinter::visitCompoundStmt(const CompoundStmtPtr& comp) {
	DotGraphBuilder::StmtNode compNode( builder->getNodeId(comp), "compound");
	checkSemanticErrors(errors, compNode, comp);
	builder->addNode(compNode);

	visitAnnotationList(*builder, builder->getNodeId(comp), comp->getAnnotations());
	visitChildList(*builder, comp->getChildList(), comp, "stmt");
}

void ASTPrinter::visitForStmt(const ForStmtPtr& forStmt) {
	DotGraphBuilder::StmtNode forNode( NODE_ID(forStmt), "for");
	checkSemanticErrors(errors, forNode, forStmt);
	builder->addNode(forNode);

	visitAnnotationList(*builder, NODE_ID(forStmt), forStmt->getAnnotations());

	visitChildList(*builder, toVector(forStmt->getDeclaration()), forStmt, "decl");
	visitChildList(*builder, toVector(forStmt->getEnd()), forStmt, "cond");
	visitChildList(*builder, toVector(forStmt->getStep()), forStmt, "step");
	visitChildList(*builder, toVector(forStmt->getBody()), forStmt, "body");
}

void ASTPrinter::visitIfStmt(const IfStmtPtr& ifStmt) {
	DotGraphBuilder::StmtNode ifNode( NODE_ID(ifStmt), "if");
	checkSemanticErrors(errors, ifNode, ifStmt);
	builder->addNode(ifNode);

	visitAnnotationList(*builder, builder->getNodeId(ifStmt), ifStmt->getAnnotations());

	visitChildList(*builder, toVector(ifStmt->getCondition()), ifStmt, "cond");
	visitChildList(*builder, toVector(ifStmt->getThenBody()), ifStmt, "then");
	visitChildList(*builder, toVector(ifStmt->getElseBody()), ifStmt, "else");
}

void ASTPrinter::visitWhileStmt(const WhileStmtPtr& whileStmt) {
	DotGraphBuilder::StmtNode whileNode( NODE_ID(whileStmt), "while");
	checkSemanticErrors(errors, whileNode, whileStmt);
	builder->addNode(whileNode);

	visitAnnotationList(*builder, NODE_ID(whileStmt), whileStmt->getAnnotations());

	visitChildList(*builder, toVector(whileStmt->getCondition()), whileStmt, "cond");
	visitChildList(*builder, toVector(whileStmt->getBody()), whileStmt, "body");
}

void ASTPrinter::visitDeclarationStmt(const DeclarationStmtPtr& declStmt) {
	DotGraphBuilder::StmtNode declNode( NODE_ID(declStmt), "decl");
	checkSemanticErrors(errors, declNode, declStmt);
	builder->addNode(declNode);

	visitAnnotationList(*builder, NODE_ID(declStmt), declStmt->getAnnotations());

	visitChildList(*builder, toVector(declStmt->getVariable()), declStmt, "var");
	visitChildList(*builder, toVector(declStmt->getInitialization()), declStmt, "init");
}

void ASTPrinter::visitReturnStmt(const ReturnStmtPtr& retStmt) {
	DotGraphBuilder::StmtNode retNode( NODE_ID(retStmt), "return");
	checkSemanticErrors(errors, retNode, retStmt);
	builder->addNode(retNode);

	visitAnnotationList(*builder, NODE_ID(retStmt), retStmt->getAnnotations());

	visitChildList(*builder, toVector(retStmt->getReturnExpr()), retStmt, "expr");
}

void ASTPrinter::visitLambdaExpr(const LambdaExprPtr& lambdaExpr) {
	DotGraphBuilder::StmtNode lambdaNode( NODE_ID(lambdaExpr), "lambda");
	checkSemanticErrors(errors, lambdaNode, lambdaExpr);
	builder->addNode(lambdaNode);

	visitAnnotationList(*builder, NODE_ID(lambdaExpr), lambdaExpr->getAnnotations());

	visitChildList(*builder, toVector(lambdaExpr->getType()), lambdaExpr, "type");
	visitChildList(*builder, lambdaExpr->getParams(), lambdaExpr, "param");
	visitChildList(*builder, toVector(lambdaExpr->getBody()), lambdaExpr, "body");
}

void ASTPrinter::visitVariable(const VariablePtr& var) {
	std::string label = "\"var\\n(" + var->toString() + ")\"";
	DotGraphBuilder::StmtNode varNode( NODE_ID(var), label);
	checkSemanticErrors(errors, varNode, var);
	builder->addNode(varNode);

	visitAnnotationList(*builder, NODE_ID(var), var->getAnnotations());
	visitChildList(*builder, toVector(var->getType()), var, "type");
}

void ASTPrinter::visitCallExpr(const CallExprPtr& callExpr) {
	DotGraphBuilder::StmtNode currNode( NODE_ID(callExpr), "call");
	checkSemanticErrors(errors, currNode, callExpr);
	builder->addNode(currNode);

	visitAnnotationList(*builder, NODE_ID(callExpr), callExpr->getAnnotations());

	visitChildList(*builder, toVector(callExpr->getType()), callExpr, "type");
	visitChildList(*builder, toVector(callExpr->getFunctionExpr()), callExpr, "func_expr");

	visitChildList(*builder, callExpr->getArguments(), callExpr, "arguments");
}

void ASTPrinter::visitCastExpr(const CastExprPtr& castExpr) {
	DotGraphBuilder::StmtNode castNode( NODE_ID(castExpr), "cast");
	checkSemanticErrors(errors, castNode, castExpr);
	builder->addNode(castNode);

	visitAnnotationList(*builder, NODE_ID(castExpr), castExpr->getAnnotations());

	visitChildList(*builder, toVector(castExpr->getType()), castExpr, "type");
	visitChildList(*builder, toVector(castExpr->getSubExpression()), castExpr, "sub_expr");
}

void ASTPrinter::visitLiteral(const LiteralPtr& lit) {
	std::string label = lit->getValue();
	if(label.find('\"') == std::string::npos) {
		label = "\"" + label +"\"";
	}
	DotGraphBuilder::LiteralNode litNode( NODE_ID(lit), label);
	checkSemanticErrors(errors, litNode, lit);
	builder->addNode(litNode);

	visitAnnotationList(*builder, NODE_ID(lit), lit->getAnnotations());
	visitChildList(*builder, toVector(lit->getType()), lit, "type");
}

void ASTPrinter::visitStatement(const insieme::core::StatementPtr& stmt) {
	DotGraphBuilder::StmtNode stmtNode( NODE_ID(stmt), "STMT");
	checkSemanticErrors(errors, stmtNode, stmt);
	builder->addNode(stmtNode);

	visitAnnotationList(*builder, NODE_ID(stmt), stmt->getAnnotations());
	visitChildList(*builder, stmt->getChildList(), stmt, "child");
}

void ASTPrinter::visitNode(const insieme::core::NodePtr& node) {
	builder->addNode(DotGraphBuilder::DotNode( builder->getNodeId(node), utils::numeric_cast<std::string>(node->getNodeCategory()) ));

	visitAnnotationList(*builder, builder->getNodeId(node), node->getAnnotations());
	visitChildList(*builder, node->getChildList(), node, "child");
}

void ASTPrinter::visitProgram(const core::ProgramPtr& prog) {
	DotGraphBuilder::DotNode root( builder->getNodeId(prog), "\"\"" );
	root[DotGraphBuilder::SHAPE] = "doublecircle";
	root[DotGraphBuilder::STYLE] = "filled";
	root[DotGraphBuilder::WIDTH] = ".4";
	builder->addNode( root );

	visitAnnotationList(*builder, builder->getNodeId(prog), prog->getAnnotations());
	visitChildList(*builder, prog->getChildList(), prog, "entry_point");
}

std::shared_ptr<ASTPrinter> makeDotPrinter(std::ostream& out, const MessageList& errors) {
	return std::make_shared<ASTPrinter>( std::make_shared<DotGraphBuilder>(out), errors );
}

void printDotGraph(const insieme::core::NodePtr& root, const MessageList& errors, std::ostream& out) {
	insieme::core::visitAllOnce(root, *makeDotPrinter(out, errors));
}
}
}


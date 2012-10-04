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

#include "insieme/driver/dot_printer.h"

#include "insieme/utils/logging.h"

using namespace insieme;
using namespace driver;

using namespace insieme::core::checks;

std::string attributeIdToString(const NodeProperty& prop) {
	switch(prop) {
	case NodeProperty::LABEL: return "label";
	case NodeProperty::SHAPE: return "shape";
	case NodeProperty::STYLE: return "style";
	case NodeProperty::DIRECTION: return "dir";
	case NodeProperty::HEIGHT: return "height";
	case NodeProperty::WIDTH: return "width";
	case NodeProperty::COLOR: return "color";
	default:	assert(false);
	}
	return "-unknown-";
}

void dumpProperties(const DOTGraphBuilder::Properties& dec, std::ostream& out) {
	out << "[" << join(", ", dec,
		[ ](std::ostream& out, const DOTGraphBuilder::Properties::value_type& cur) {
			out << attributeIdToString(cur.first) << "=" << cur.second;
		}) <<
	"];" << std::endl;
}

struct DotNode: public DOTGraphBuilder::Node {
	DotNode(size_t id): Node(id) { }

	DotNode(size_t id, const std::string& label) : Node(id) {
		if(!label.empty())
			(*this)[LABEL] = label;
	}
};

struct DotLink: public DOTGraphBuilder::Link {
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

struct RootNode: public DotNode {
	RootNode(size_t id): DotNode(id, "\"\"") {
		(*this)[NodeProperty::SHAPE] = "doublecircle";
		(*this)[NodeProperty::STYLE] = "filled";
		(*this)[NodeProperty::WIDTH] = ".4";
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

#define NODE_ID(node)	this->builder->getNodeId(node)

namespace insieme {
namespace driver {

void DOTGraphBuilder::addNode(const DOTGraphBuilder::Node& node) {
	out << node.getId()  << "\t";
	dumpProperties( static_cast<const DotNode&>(node), out );
}

void DOTGraphBuilder::addLink(const DOTGraphBuilder::Link& link) {
	out << link.getSrc() << " -> " << link.getDest() << "\t";
	dumpProperties( static_cast<const DotLink&>(link), out );
}

size_t DOTGraphBuilder::getNodeId(const core::NodePtr& fromNode) { return (size_t) &*fromNode; }

template <class BuilderTy>
void visitAnnotationList(BuilderTy& builder, size_t parentId, const core::Node::annotation_container::annotation_map_type& map) {
	for(core::Node::annotation_map_type::const_iterator it = map.begin(), end = map.end(); it != end; ++it) {
		size_t annotationId = (size_t)&*it->second;
		
		std::ostringstream ss;
		ss << "\"" << it->second->getAnnotationName() << "\\n[" << *(it->second) << "]\"";
		builder.addNode( AnnotationNode(annotationId,ss.str()) );

		DotLink l1(parentId, annotationId, "");
		l1[NodeProperty::DIRECTION] = "none";
		l1[NodeProperty::STYLE] = "dashed";
		builder.addLink( l1 );
	}
}

template <class BuilderTy, class Container, class ElemTy = typename Container::value_type>
void visitChildList(BuilderTy& builder,
		const Container& children, const core::NodePtr& parent, const std::string& labelPrefix)
{
	unsigned elemCount = 0;
	std::for_each(children.begin(), children.end(), [ & ](const ElemTy& curr) {
		std::string label = labelPrefix + (children.size() > 1 ? ("_" + utils::numeric_cast<std::string>(elemCount++)) : "");
		builder.addLink( DotLink(builder.getNodeId(parent), builder.getNodeId(curr), label) );
	});
}

void checkSemanticErrors(const MessageList& list, DotNode& currNode, const core::NodePtr& node) {
	auto errors = list.getAll();
	std::sort(errors.begin(), errors.end());
	std::for_each(errors.begin(), errors.end(), [&currNode, node](const Message& cur) {
		if(*node == *cur.getAddress().getAddressedNode()) {
			if(cur.getType() == core::checks::Message::ERROR)
				currNode[NodeProperty::COLOR] = "red";
			else
				currNode[NodeProperty::COLOR] = "yellow";
			currNode[NodeProperty::STYLE] = "filled";
			std::string label = currNode[NodeProperty::LABEL];
			if(label.at(0) == '\"')
				label = label.substr(1,label.size()-2);
			currNode[NodeProperty::LABEL] =
					"\"" + label + "\\n{ CODE: " + utils::numeric_cast<std::string>(cur.getErrorCode()) + "}\"";
		}
	});
}

ASTPrinter::ASTPrinter(const IRBuilderPtr& builder, const MessageList& errors): 
	insieme::core::IRVisitor<>(true), dummyNodeID(1), builder(builder), errors(errors) { }

void ASTPrinter::visitTypeVariable(const TypeVariablePtr& typeVar) {
	TypeNode varNode( NODE_ID(typeVar), "\"var\\n{" + typeVar->toString() + "}\"");
	checkSemanticErrors(errors, varNode, typeVar);
	builder->addNode(varNode);

	visitAnnotationList(*builder, NODE_ID(typeVar), typeVar->getAnnotations());
}

void ASTPrinter::visitIntTypeParam(const IntTypeParamPtr& intTyParm) { }

void ASTPrinter::visitGenericType(const core::GenericTypePtr& genTy) {
	std::ostringstream ss("");
	// special handling for integer type parameters
	if(!genTy->getIntTypeParameter().empty()) {
		ss << "<" << join(", ", genTy->getIntTypeParameter(), [ ](std::ostream& out, const IntTypeParamPtr& cur) {
			out << toString(*cur);
		}) << ">";
	}
	TypeNode genNode(NODE_ID(genTy), "\"" + genTy->getFamilyName() + " " + ss.str() + "\"");
	checkSemanticErrors(errors, genNode, genTy);
	builder->addNode(genNode);

	visitAnnotationList(*builder, builder->getNodeId(genTy), genTy->getAnnotations());
	visitChildList(*builder, genTy->getTypeParameter()->getElements(), genTy, "typeVar");
}

void ASTPrinter::visitFunctionType(const FunctionTypePtr& funcType) {
	TypeNode funcNode( NODE_ID(funcType), "func");
	checkSemanticErrors(errors, funcNode, funcType);
	builder->addNode(funcNode);

	visitAnnotationList(*builder, builder->getNodeId(funcType), funcType->getAnnotations());
	visitChildList(*builder, toVector(funcType->getReturnType()), funcType, "retTy");
	visitChildList(*builder, funcType->getParameterTypes()->getElements(), funcType, "argTy");
}

void ASTPrinter::visitTupleType(const TupleTypePtr& tupleTy) {
	TypeNode tupleNode( NODE_ID(tupleTy), "tuple");
	checkSemanticErrors(errors, tupleNode, tupleTy);
	builder->addNode(tupleNode);

	visitAnnotationList(*builder, NODE_ID(tupleTy), tupleTy->getAnnotations());
	visitChildList(*builder, tupleTy->getElementTypes(), tupleTy, "elemTy");
}

void ASTPrinter::visitNamedCompositeType(const NamedCompositeTypePtr& compTy) {
	std::string name;
	if(dynamic_pointer_cast<const StructType>(compTy))
		name = "struct";
	else
		name = "union";

	TypeNode structNode( NODE_ID(compTy), name);
	checkSemanticErrors(errors, structNode, compTy);
	builder->addNode(structNode);

	std::for_each(compTy->getEntries().begin(), compTy->getEntries().end(),
		[ this, compTy ](const NamedTypePtr& cur){
			this->builder->addLink( DotLink(NODE_ID(compTy), NODE_ID(cur->getType()), cur->getName()->getValue()) );
			// TODO: VISIT ANNOTATIONS IN THE POINTER
	});
	visitAnnotationList(*builder, NODE_ID(compTy), compTy->getAnnotations());
}

void ASTPrinter::visitRecType(const RecTypePtr& recTy) {
	TypeNode recNode( NODE_ID(recTy), "rec");
	checkSemanticErrors(errors, recNode, recTy);
	builder->addNode(recNode);

	visitAnnotationList(*builder, NODE_ID(recTy), recTy->getAnnotations());
	visitChildList(*builder, toVector(recTy->getTypeVariable()), recTy, "var");
	visitChildList(*builder, toVector(recTy->getDefinition()), recTy, "def");
}

void ASTPrinter::visitRecTypeDefinition(const RecTypeDefinitionPtr& recTy) {
	TypeNode recNode( NODE_ID(recTy), "rec_def");
	checkSemanticErrors(errors, recNode, recTy);
	builder->addNode(recNode);

	unsigned num = 0;
	std::for_each(recTy->getDefinitions().begin(), recTy->getDefinitions().end(),
		[ this, recTy, &num ](const RecTypeBindingPtr& cur){
			size_t interId = dummyNodeID++;

			JunctionNode node(interId);
			node[NodeProperty::SHAPE] = "diamond";
			node[NodeProperty::HEIGHT] = ".25";
			node[NodeProperty::WIDTH] = ".25";
			this->builder->addNode(node);
			std::string label = "def";
			if(recTy->getDefinitions().size() > 1)
				label += "_" + utils::numeric_cast<std::string>(num++);

			DotLink link(NODE_ID(recTy), interId, label);
			this->builder->addLink( link );
			this->builder->addLink( DotLink(interId, NODE_ID(cur->getVariable()), "") );
			this->builder->addLink( DotLink(interId, NODE_ID(cur->getType()), "") );
			// TODO: VISIT ANNOTATIONS IN THE POINTER
	});

	visitAnnotationList(*builder, NODE_ID(recTy), recTy->getAnnotations());
}

void ASTPrinter::visitCompoundStmt(const CompoundStmtPtr& comp) {
	StmtNode compNode( NODE_ID(comp), "compound");
	checkSemanticErrors(errors, compNode, comp);
	builder->addNode(compNode);

	visitAnnotationList(*builder, builder->getNodeId(comp), comp->getAnnotations());
	visitChildList(*builder, comp->getChildList(), comp, "stmt");
}

void ASTPrinter::visitForStmt(const ForStmtPtr& forStmt) {
	StmtNode forNode( NODE_ID(forStmt), "for");
	checkSemanticErrors(errors, forNode, forStmt);
	builder->addNode(forNode);

	visitAnnotationList(*builder, NODE_ID(forStmt), forStmt->getAnnotations());

	visitChildList(*builder, toVector(forStmt->getIterator()), forStmt, "iterator");
	visitChildList(*builder, toVector(forStmt->getStart()), forStmt, "start");
	visitChildList(*builder, toVector(forStmt->getEnd()), forStmt, "end");
	visitChildList(*builder, toVector(forStmt->getStep()), forStmt, "step");
	visitChildList(*builder, toVector(forStmt->getBody()), forStmt, "body");
}

void ASTPrinter::visitIfStmt(const IfStmtPtr& ifStmt) {
	StmtNode ifNode( NODE_ID(ifStmt), "if");
	checkSemanticErrors(errors, ifNode, ifStmt);
	builder->addNode(ifNode);

	visitAnnotationList(*builder, builder->getNodeId(ifStmt), ifStmt->getAnnotations());

	visitChildList(*builder, toVector(ifStmt->getCondition()), ifStmt, "cond");
	visitChildList(*builder, toVector(ifStmt->getThenBody()), ifStmt, "then");
	visitChildList(*builder, toVector(ifStmt->getElseBody()), ifStmt, "else");
}

void ASTPrinter::visitWhileStmt(const WhileStmtPtr& whileStmt) {
	StmtNode whileNode( NODE_ID(whileStmt), "while");
	checkSemanticErrors(errors, whileNode, whileStmt);
	builder->addNode(whileNode);

	visitAnnotationList(*builder, NODE_ID(whileStmt), whileStmt->getAnnotations());

	visitChildList(*builder, toVector(whileStmt->getCondition()), whileStmt, "cond");
	visitChildList(*builder, toVector(whileStmt->getBody()), whileStmt, "body");
}

void ASTPrinter::visitDeclarationStmt(const DeclarationStmtPtr& declStmt) {
	StmtNode declNode( NODE_ID(declStmt), "decl");
	checkSemanticErrors(errors, declNode, declStmt);
	builder->addNode(declNode);

	visitAnnotationList(*builder, NODE_ID(declStmt), declStmt->getAnnotations());

	visitChildList(*builder, toVector(declStmt->getVariable()), declStmt, "var");
	visitChildList(*builder, toVector(declStmt->getInitialization()), declStmt, "init");
}

void ASTPrinter::visitReturnStmt(const ReturnStmtPtr& retStmt) {
	StmtNode retNode( NODE_ID(retStmt), "return");
	checkSemanticErrors(errors, retNode, retStmt);
	builder->addNode(retNode);

	visitAnnotationList(*builder, NODE_ID(retStmt), retStmt->getAnnotations());

	visitChildList(*builder, toVector(retStmt->getReturnExpr()), retStmt, "expr");
}

void ASTPrinter::visitLambdaExpr(const LambdaExprPtr& lambdaExpr) {
	StmtNode lambdaNode( NODE_ID(lambdaExpr), "lambdaExpr");
	checkSemanticErrors(errors, lambdaNode, lambdaExpr);
	builder->addNode(lambdaNode);

	visitAnnotationList(*builder, NODE_ID(lambdaExpr), lambdaExpr->getAnnotations());

	visitChildList(*builder, toVector(lambdaExpr->getType()), lambdaExpr, "type");
	visitChildList(*builder, toVector(lambdaExpr->getVariable()), lambdaExpr, "var");
	visitChildList(*builder, toVector(lambdaExpr->getDefinition()), lambdaExpr, "definition");
	visitChildList(*builder, toVector(lambdaExpr->getLambda()), lambdaExpr, "lambda");
}

void ASTPrinter::visitLambda(const LambdaPtr& lambda) {
	StmtNode lambdaNode( NODE_ID(lambda), "lambda");
	checkSemanticErrors(errors, lambdaNode, lambda);
	builder->addNode(lambdaNode);

	visitAnnotationList(*builder, NODE_ID(lambda), lambda->getAnnotations());

	visitChildList(*builder, toVector(lambda->getType()), lambda, "type");
	visitChildList(*builder, lambda->getParameterList(), lambda, "param");
	visitChildList(*builder, toVector(lambda->getBody()), lambda, "body");
}

void ASTPrinter::visitLambdaDefinition(const LambdaDefinitionPtr& lambdaDef) {
	StmtNode lambdaNode( NODE_ID(lambdaDef), "lambdaDef");
	checkSemanticErrors(errors, lambdaNode, lambdaDef);
	builder->addNode(lambdaNode);

	visitAnnotationList(*builder, NODE_ID(lambdaDef), lambdaDef->getAnnotations());

	size_t num = 1;
	for_each(lambdaDef->getDefinitions().begin(), lambdaDef->getDefinitions().end(),
		[&num, &lambdaDef, this](const LambdaBindingPtr& curr){
			// this->builder->addLink( DotLink(NODE_ID(recTy), NODE_ID(cur.second), "\"\"") );
			size_t interId =dummyNodeID++; 

			JunctionNode node(interId);
			node[NodeProperty::SHAPE] = "diamond";
			node[NodeProperty::HEIGHT] = ".25";
			node[NodeProperty::WIDTH] = ".25";
			this->builder->addNode(node);
			std::string label = "def";
			if(lambdaDef->getDefinitions().size() > 1)
				label += "_" + utils::numeric_cast<std::string>(num++);

			DotLink link(NODE_ID(lambdaDef), interId, label);
			this->builder->addLink( link );
			this->builder->addLink( DotLink(interId, NODE_ID(curr->getVariable()), "var") );
			this->builder->addLink( DotLink(interId, NODE_ID(curr->getLambda()), "lambda") );
		}
	);
}

void ASTPrinter::visitVariable(const VariablePtr& var) {
	std::string label = "\"var\\n(" + var->toString() + ")\"";
	StmtNode varNode( NODE_ID(var), label);
	checkSemanticErrors(errors, varNode, var);
	builder->addNode(varNode);

	visitAnnotationList(*builder, NODE_ID(var), var->getAnnotations());
	visitChildList(*builder, toVector(var->getType()), var, "type");
}

void ASTPrinter::visitCallExpr(const CallExprPtr& callExpr) {
	StmtNode currNode( NODE_ID(callExpr), "call");
	checkSemanticErrors(errors, currNode, callExpr);
	builder->addNode(currNode);

	visitAnnotationList(*builder, NODE_ID(callExpr), callExpr->getAnnotations());

	visitChildList(*builder, toVector(callExpr->getType()), callExpr, "type");
	visitChildList(*builder, toVector(callExpr->getFunctionExpr()), callExpr, "func_expr");

	visitChildList(*builder, callExpr->getArguments(), callExpr, "argument");
}

void ASTPrinter::visitCastExpr(const CastExprPtr& castExpr) {
	StmtNode castNode( NODE_ID(castExpr), "cast");
	checkSemanticErrors(errors, castNode, castExpr);
	builder->addNode(castNode);

	visitAnnotationList(*builder, NODE_ID(castExpr), castExpr->getAnnotations());

	visitChildList(*builder, toVector(castExpr->getType()), castExpr, "type");
	visitChildList(*builder, toVector(castExpr->getSubExpression()), castExpr, "sub_expr");
}

void ASTPrinter::visitLiteral(const LiteralPtr& lit) {
	std::string label = lit->getStringValue();
	if(label.find('\"') == std::string::npos) {
		label = "\"" + label +"\"";
	}
	LiteralNode litNode( NODE_ID(lit), label);
	checkSemanticErrors(errors, litNode, lit);
	builder->addNode(litNode);

	visitAnnotationList(*builder, NODE_ID(lit), lit->getAnnotations());
	visitChildList(*builder, toVector(lit->getType()), lit, "type");
}

void ASTPrinter::visitVectorExpr(const VectorExprPtr& vexp) {
	StmtNode vectNode( NODE_ID(vexp), "vect_expr");
	checkSemanticErrors(errors, vectNode, vexp);
	builder->addNode(vectNode);

	visitAnnotationList(*builder, NODE_ID(vexp), vexp->getAnnotations());
	visitChildList(*builder, vexp->getChildList(), vexp, "expr");
}

void ASTPrinter::visitStatement(const insieme::core::StatementPtr& stmt) {
	StmtNode stmtNode( NODE_ID(stmt), "stmt");
	checkSemanticErrors(errors, stmtNode, stmt);
	builder->addNode(stmtNode);

	visitAnnotationList(*builder, NODE_ID(stmt), stmt->getAnnotations());
	visitChildList(*builder, stmt->getChildList(), stmt, "child");
}

void ASTPrinter::visitNode(const insieme::core::NodePtr& node) {
	LOG(WARNING) << "Using generic visitor for IR node: " << *node;
	builder->addNode( DotNode( NODE_ID(node), utils::numeric_cast<std::string>(node->getNodeCategory()) ));

	visitAnnotationList(*builder, NODE_ID(node), node->getAnnotations());
	visitChildList(*builder, node->getChildList(), node, "child");
}

void ASTPrinter::visitProgram(const core::ProgramPtr& prog) {
	builder->addNode( RootNode( NODE_ID(prog)) );

	visitAnnotationList(*builder, builder->getNodeId(prog), prog->getAnnotations());
	visitChildList(*builder, prog->getChildList(), prog, "entry_point");
}

ASTPrinter makeDotPrinter(std::ostream& out, const MessageList& errors) {
	return ASTPrinter( std::make_shared<DOTGraphBuilder>(out), errors );
}

void printDotGraph(const insieme::core::NodePtr& root, const MessageList& errors, std::ostream& out) {
	out << "digraph inspire {" << std::endl;
	insieme::core::visitDepthFirstOnce(root, makeDotPrinter(out, errors));
	out << "}" << std::endl;
}
}
}


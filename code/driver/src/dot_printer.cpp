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

class DotGraphBuilder: public GraphBuilder<size_t> {
	std::ostream& 	out;

public:
	enum DotAttributes{ LABEL, SHAPE, STYLE, DIRECTION, HEIGHT, WIDTH };

	static std::string attributeIdToString(const DotAttributes& attr) {
		switch(attr) {
		case LABEL: return "label";
		case SHAPE: return "shape";
		case STYLE: return "style";
		case DIRECTION: return "dir";
		case HEIGHT: return "height";
		case WIDTH: return "width";
		default:	assert(false);
		}
	}

	void dumpProperties(const Decoration<DotAttributes, std::string>& dec, std::ostream& out) const {
		out << "[" << join(", ", dec,
			[ ](std::ostream& out, const Decoration<DotAttributes, std::string>::value_type& cur) {
				out << attributeIdToString(cur.first) << "=" << cur.second;
			}) <<
		"];" << std::endl;
	}

	struct DotNode: public DotGraphBuilder::Node, public DotGraphBuilder::Decoration<DotAttributes, std::string> {
		DotNode(size_t id, const std::string& label) : Node(id) {
			if(!label.empty())
				(*this)[LABEL] = label;
		}
	};

	struct DotLink: public DotGraphBuilder::Link, public DotGraphBuilder::Decoration<DotAttributes, std::string> {
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

	const void addNode(const GraphBuilder<size_t>::Node& node) {
		out << node.getId()  << "\t";
		dumpProperties( static_cast<const DotNode&>(node), out );
	}

	void addLink(const GraphBuilder<size_t>::Link& link) {
		out << link.getSrc() << " -> " << link.getDest() << "\t";
		dumpProperties( static_cast<const DotLink&>(link), out );
	}
};


void visitAnnotationList(GraphBuilder<size_t>& builder, size_t parentId, const core::AnnotationMap& map) {
	for(core::AnnotationMap::const_iterator it = map.begin(), end = map.end(); it != end; ++it) {
		size_t annotationId = (size_t)&*it->second;

		builder.addNode(DotGraphBuilder::AnnotationNode(annotationId, it->second->getAnnotationName()));
		DotGraphBuilder::DotLink l1(parentId, annotationId, "");
		l1[DotGraphBuilder::DIRECTION] = "none";
		l1[DotGraphBuilder::STYLE] = "dashed";
		builder.addLink( l1 );
	}
}


template <class ElemTy>
void visitChildList(GraphBuilder<size_t>& builder, const std::vector<ElemTy>& children,
		const core::NodePtr& parent, const std::string& labelPrefix) {

	unsigned elemCount = 0;

	std::for_each(children.begin(), children.end(), [ & ](const ElemTy& curr) {
		std::string label = labelPrefix + (children.size() > 1 ? ("_" + utils::numeric_cast<std::string>(elemCount++)) : "");
		if(curr.getAnnotations().empty()) {
			builder.addLink( DotGraphBuilder::DotLink(GraphPrinter<size_t>::getNodeId(parent), GraphPrinter<size_t>::getNodeId(curr), label) );
		} else {
			// introducing an intermediate node to attach annotations to the pointer
			size_t interId = GraphPrinter<size_t>::getNodeId(parent) ^ GraphPrinter<size_t>::getNodeId(curr);

			DotGraphBuilder::JunctionNode node(interId);
			builder.addNode(node);

			DotGraphBuilder::DotLink l1(GraphPrinter<size_t>::getNodeId(parent), interId, label);
			l1[DotGraphBuilder::DIRECTION] = "none";
			builder.addLink( l1 );
			builder.addLink( DotGraphBuilder::DotLink(interId, GraphPrinter<size_t>::getNodeId(curr), "") );

			visitAnnotationList(builder, interId, curr.getAnnotations());
		}
	});
}

}
namespace insieme {

template <class size_t>
size_t GraphPrinter<size_t>::getNodeId(const NodePtr& node) {
	return (size_t)&*node;
}

template <class T>
void GraphPrinter<T>::visitGenericType(const core::GenericTypePtr& genTy) {
	std::ostringstream ss("");
	// special handling for integer type parameters
	if(!genTy->getIntTypeParameter().empty()) {
		ss << "<" << join(", ", genTy->getIntTypeParameter(), [ ](std::ostream& out, const IntTypeParam& cur) {
			out << (cur.isConcrete() ? insieme::utils::numeric_cast<std::string>(cur.getValue()) : ""+cur.getSymbol());
		}) << ">";
	}
	builder->addNode(DotGraphBuilder::TypeNode(getNodeId(genTy), "\"" + genTy->getFamilyName().getName() + " " + ss.str() + "\""));

	visitAnnotationList(*builder, getNodeId(genTy), genTy->getAnnotations());
	visitChildList(*builder, genTy->getTypeParameter(), genTy, "typeVar");
}

template <class T>
void GraphPrinter<T>::visitFunctionType(const FunctionTypePtr& funcType) {
	builder->addNode(DotGraphBuilder::TypeNode( getNodeId(funcType), "func"));

	visitAnnotationList(*builder, getNodeId(funcType), funcType->getAnnotations());
	visitChildList(*builder, toVector(funcType->getReturnType()), funcType, "retTy");
	visitChildList(*builder, toVector(funcType->getArgumentType()), funcType, "argTy");
}

template <class T>
void GraphPrinter<T>::visitTupleType(const TupleTypePtr& tupleTy) {
	builder->addNode(DotGraphBuilder::TypeNode( getNodeId(tupleTy), "tuple") );

	visitAnnotationList(*builder, getNodeId(tupleTy), tupleTy->getAnnotations());
	visitChildList(*builder, tupleTy->getElementTypes(), tupleTy, "elemTy");
}

template <class T>
void GraphPrinter<T>::visitNamedCompositeType(const NamedCompositeTypePtr& compTy) {
	std::string name;
	if(dynamic_pointer_cast<const StructType>(compTy))
		name = "structType";
	else
		name = "unionType";

	out << "\t" << (size_t)&*compTy << "\t[label=\"" << name << "\", shape=ellipse]" << std::endl;
	// TODO
}

template <class T>
void GraphPrinter<T>::visitCompoundStmt(const CompoundStmtPtr& comp) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(comp), "compound"));

	visitAnnotationList(*builder, getNodeId(comp), comp->getAnnotations());
	visitChildList(*builder, comp->getChildList(), comp, "stmt");
}

template <class T>
void GraphPrinter<T>::visitForStmt(const ForStmtPtr& forStmt) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(forStmt), "for"));

	visitAnnotationList(*builder, getNodeId(forStmt), forStmt->getAnnotations());

	visitChildList(*builder, toVector(forStmt->getDeclaration()), forStmt, "decl");
	visitChildList(*builder, toVector(forStmt->getEnd()), forStmt, "cond");
	visitChildList(*builder, toVector(forStmt->getStep()), forStmt, "step");
	visitChildList(*builder, toVector(forStmt->getBody()), forStmt, "body");
}

template <class T>
void GraphPrinter<T>::visitIfStmt(const IfStmtPtr& ifStmt) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(ifStmt), "if"));

	visitAnnotationList(*builder, getNodeId(ifStmt), ifStmt->getAnnotations());

	visitChildList(*builder, toVector(ifStmt->getCondition()), ifStmt, "cond");
	visitChildList(*builder, toVector(ifStmt->getThenBody()), ifStmt, "then");
	visitChildList(*builder, toVector(ifStmt->getElseBody()), ifStmt, "else");
}

template <class T>
void GraphPrinter<T>::visitWhileStmt(const WhileStmtPtr& whileStmt) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(whileStmt), "while"));

	visitAnnotationList(*builder, getNodeId(whileStmt), whileStmt->getAnnotations());

	visitChildList(*builder, toVector(whileStmt->getCondition()), whileStmt, "cond");
	visitChildList(*builder, toVector(whileStmt->getBody()), whileStmt, "body");
}

template <class T>
void GraphPrinter<T>::visitDeclarationStmt(const DeclarationStmtPtr& declStmt) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(declStmt), "decl"));

	visitAnnotationList(*builder, getNodeId(declStmt), declStmt->getAnnotations());

	visitChildList(*builder, toVector(declStmt->getVariable()), declStmt, "var");
	visitChildList(*builder, toVector(declStmt->getInitialization()), declStmt, "init");
}

template <class T>
void GraphPrinter<T>::visitReturnStmt(const ReturnStmtPtr& retStmt) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(retStmt), "return"));

	visitAnnotationList(*builder, getNodeId(retStmt), retStmt->getAnnotations());

	visitChildList(*builder, toVector(retStmt->getReturnExpr()), retStmt, "expr");
}

template <class T>
void GraphPrinter<T>::visitLambdaExpr(const LambdaExprPtr& lambdaExpr) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(lambdaExpr), "lambda"));

	visitAnnotationList(*builder, getNodeId(lambdaExpr), lambdaExpr->getAnnotations());

	visitChildList(*builder, toVector(lambdaExpr->getType()), lambdaExpr, "type");
	visitChildList(*builder, lambdaExpr->getParams(), lambdaExpr, "param");
	visitChildList(*builder, toVector(lambdaExpr->getBody()), lambdaExpr, "body");
}


template <class T>
void GraphPrinter<T>::visitVariable(const VariablePtr& var) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(var), "var"));

	visitAnnotationList(*builder, getNodeId(var), var->getAnnotations());
	visitChildList(*builder, toVector(var->getType()), var, "type");
}

template <class T>
void GraphPrinter<T>::visitCallExpr(const CallExprPtr& callExpr) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(callExpr), "call"));

	visitAnnotationList(*builder, getNodeId(callExpr), callExpr->getAnnotations());

	visitChildList(*builder, toVector(callExpr->getType()), callExpr, "type");
	visitChildList(*builder, toVector(callExpr->getFunctionExpr()), callExpr, "func_expr");

	visitChildList(*builder, callExpr->getArguments(), callExpr, "arguments");
}

template <class T>
void GraphPrinter<T>::visitCastExpr(const CastExprPtr& castExpr) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(castExpr), "cast"));

	visitAnnotationList(*builder, getNodeId(castExpr), castExpr->getAnnotations());

	visitChildList(*builder, toVector(castExpr->getType()), castExpr, "type");
	visitChildList(*builder, toVector(castExpr->getSubExpression()), castExpr, "sub_expr");
}

template <class T>
void GraphPrinter<T>::visitLiteral(const LiteralPtr& lit) {
	std::string label = lit->getValue();
	if(label.find('\"') == std::string::npos) {
		label = "\"" + label +"\"";
	}
	builder->addNode(DotGraphBuilder::LiteralNode( getNodeId(lit), label));

	visitAnnotationList(*builder, getNodeId(lit), lit->getAnnotations());
	visitChildList(*builder, toVector(lit->getType()), lit, "type");
}

template <class T>
void GraphPrinter<T>::visitStatement(const insieme::core::StatementPtr& stmt) {
	builder->addNode(DotGraphBuilder::StmtNode( getNodeId(stmt), "STMT"));

	visitAnnotationList(*builder, getNodeId(stmt), stmt->getAnnotations());
	visitChildList(*builder, stmt->getChildList(), stmt, "child");
}

template <class T>
void GraphPrinter<T>::visitNode(const insieme::core::NodePtr& node) {
	builder->addNode(DotGraphBuilder::DotNode( getNodeId(node), utils::numeric_cast<std::string>(node->getNodeCategory()) ));

	visitAnnotationList(*builder, getNodeId(node), node->getAnnotations());
	visitChildList(*builder, node->getChildList(), node, "child");
}

template <class T>
void GraphPrinter<T>::visitProgram(const core::ProgramPtr& prog) {
	DotGraphBuilder::DotNode root( getNodeId(prog), "\"\"" );
	root[DotGraphBuilder::SHAPE] = "doublecircle";
	root[DotGraphBuilder::STYLE] = "filled";
	root[DotGraphBuilder::WIDTH] = ".4";
	builder->addNode( root );

	visitAnnotationList(*builder, getNodeId(prog), prog->getAnnotations());
	visitChildList(*builder, prog->getChildList(), prog, "entry_point");
}

template <class T>
GraphPrinter<T>::GraphPrinter(std::ostream& out) : out(out), builder( new DotGraphBuilder(out) ) { }

void printDotGraph(const insieme::core::NodePtr& root, std::ostream& out) {
	GraphPrinter<size_t> dv(out);
	out << "digraph inspire {" << std::endl;
	insieme::core::visitAllOnce(root, dv);
	out << "}" << std::endl;
}
}

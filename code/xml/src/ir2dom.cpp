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

#include <xercesc/dom/DOM.hpp>

#include "insieme/xml/xml_utils.h"
#include "insieme/xml/xsd_config.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::xml;
using namespace std;

XERCES_CPP_NAMESPACE_USE

#define GET_ID(nodePtr)		numeric_cast<string>((size_t)(&*(nodePtr)))

namespace {
// ------------------------------------ XmlVisitor ----------------------------

typedef typename Node::annotation_container::annotation_map_type AnnotationMap;

class XmlVisitor : public IRVisitor<void> {
	DOMDocument* doc;
	XmlElement rootElem;

	template <class T>
	void append(XmlElement& xmlNode, const T& expr, const std::string& name) {
		XmlElement expressionPtr(name, doc);
		xmlNode << (expressionPtr << XmlElement::Attribute("ref", GET_ID(expr)));
	}
	
	template <typename Container>
	void appendList(XmlElement& xmlParentNode, const Container& list, const std::string& groupName, const std::string& elemName) {
		typedef typename Container::value_type ElemTyp;
		XmlElement subNode(groupName, this->doc);
		xmlParentNode << subNode;
		std::for_each(list.begin(), list.end(),
			[this, &subNode, elemName](const ElemTyp& curr) {
				this->append(subNode, curr, elemName);
			}
		);
	}

public:
	XmlVisitor(DOMDocument* udoc): IRVisitor<void>(true), doc(udoc), rootElem(doc->getDocumentElement()) { }

	void visitAnnotations(const AnnotationMap& map, XmlElement& node) {
		if (map.empty())
			return;

		XmlElement annotations("annotations", doc);
		node << annotations;

		XmlConverter& xmlConverter = XmlConverter::get();
		for(AnnotationMap::const_iterator iter = map.begin(); iter != map.end(); ++iter) {
			annotations << xmlConverter.irToDomAnnotation (*(iter->second), doc);
		}
	}
	
	void visitGenericType(const GenericTypePtr& cur) {
		XmlElement genType("genType", doc);
		genType << XmlElement::Attribute("id", GET_ID(cur))
				<< XmlElement::Attribute("familyName", cur->getFamilyName());
		rootElem << genType;

		typedef vector<TypePtr> ParamList;
		const ParamList& param = cur->getTypeParameter()->getElements();
		if (!param.empty()) {
			XmlElement typeParams("typeParams", doc);
			genType << typeParams;
			for_each(param.begin(), param.end(), [ this, &typeParams ](const TypePtr& curr) { this->append(typeParams, curr, "typePtr"); });
		}

		typedef vector<IntTypeParamPtr> IntParamList;
		const IntParamList& intParam = cur->getIntTypeParameter()->getElements();
		if (!intParam.empty()){
			XmlElement intTypeParams("intTypeParams", doc);
			genType << intTypeParams;
			for_each(intParam.begin(), intParam.end(), [ this, &intTypeParams ](const IntTypeParamPtr& curr) { this->append(intTypeParams, curr, "intTypeParamPtr"); });
		}
		visitAnnotations(cur->getAnnotations(), genType);
	}
	
	void visitSingleElementType(const SingleElementTypePtr& cur) {
		XmlElement genType("genType", doc);
		genType << XmlElement::Attribute("id", GET_ID(cur));
		switch(cur->getNodeType()) {
			case NT_ArrayType: genType << XmlElement::Attribute("familyName", "array"); break;
			case NT_VectorType: genType << XmlElement::Attribute("familyName", "vector"); break;
			case NT_ChannelType: genType << XmlElement::Attribute("familyName", "channel"); break;
			default: assert(false && "Unsupported type encountered!");
		}
		rootElem << genType;

		XmlElement typeParams("typeParams", doc);
		genType << typeParams;
		append(typeParams, cur->getElementType(), "typePtr");

		XmlElement intTypeParams("intTypeParams", doc);
		genType << intTypeParams;
		append(intTypeParams, cur->getIntTypeParameter(), "intTypeParamPtr");

		visitAnnotations(cur->getAnnotations(), genType);
	}

	void visitRefType(const RefTypePtr& cur) {
		XmlElement genType("genType", doc);
		genType << XmlElement::Attribute("id", GET_ID(cur))
				<< XmlElement::Attribute("familyName", "ref");
		rootElem << genType;

		XmlElement typeParams("typeParams", doc);
		genType << typeParams;
		append(typeParams, cur->getElementType(), "typePtr");

		visitAnnotations(cur->getAnnotations(), genType);
	}


	void visitConcreteIntTypeParam(const ConcreteIntTypeParamPtr& cur) {
		XmlElement concreteIntTypeParam("concreteIntTypeParam", doc);
		concreteIntTypeParam << XmlElement::Attribute("id", GET_ID(cur));
		rootElem << concreteIntTypeParam;
		
		XmlElement concrete("concrete", doc);
		concrete << XmlElement::Attribute("value", numeric_cast<std::string>(cur->getValue()));
		concreteIntTypeParam << concrete;
		visitAnnotations(cur->getAnnotations(), concreteIntTypeParam);
	}
	
	void visitVariableIntTypeParam(const VariableIntTypeParamPtr& cur) {
		XmlElement variableIntTypeParam("variableIntTypeParam", doc);
		variableIntTypeParam << XmlElement::Attribute("id", GET_ID(cur));
		rootElem << variableIntTypeParam;
		
		XmlElement variable("variable", doc);
		variable << XmlElement::Attribute("value", numeric_cast<std::string>(cur->getSymbol()->getValue()));
		variableIntTypeParam << variable;
		visitAnnotations(cur->getAnnotations(), variableIntTypeParam);
	}
	
	void visitInfiniteIntTypeParam(const InfiniteIntTypeParamPtr& cur) {
		XmlElement infiniteIntTypeParam("infiniteIntTypeParam", doc);
		infiniteIntTypeParam << XmlElement::Attribute("id", GET_ID(cur));
		rootElem << infiniteIntTypeParam;
		
		XmlElement infinite("infinite", doc);
		infiniteIntTypeParam << infinite;
		visitAnnotations(cur->getAnnotations(), infiniteIntTypeParam);
	}

	void visitFunctionType(const FunctionTypePtr& cur) {
		XmlElement functionType("functionType", doc);
		rootElem << (functionType << XmlElement::Attribute("id", GET_ID(cur))
								  << XmlElement::Attribute("kind", toString(numeric_cast<int>(cur->getKind()))));
		
		appendList(functionType, cur->getParameterTypes()->getElements(), "parameterTypeList", "typePtr");

		XmlElement returnType("returnType", doc);
		append(returnType, cur->getReturnType(), "typePtr");
		functionType << returnType;

		visitAnnotations(cur->getAnnotations(), functionType);
	}

	void visitStructType(const StructTypePtr& cur) {
		XmlElement structType("structType",doc);
		visitNamedCompositeType_(structType, cur);
	}

	void visitUnionType(const UnionTypePtr& cur) {
		XmlElement unionType("unionType", doc);
		visitNamedCompositeType_(unionType, cur);
	}

	void visitNamedCompositeType_(XmlElement& el, const NamedCompositeTypePtr& cur) {
		rootElem << (el << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement entries("entries",doc);
		el << entries;

		const NamedCompositeType::Entries& entriesVec = cur->getEntries();
		std::for_each(entriesVec.begin(), entriesVec.end(),
			[this, &entries](const NamedTypePtr& curr) {
				XmlElement entry("entry", this->doc);
				this->append(entry, curr->getName(), "identifierPtr");
				this->append(entry, curr->getType(), "typePtr");
				entries << entry;
			}
		);
		visitAnnotations(cur->getAnnotations(), el);
	}

	void visitTupleType(const TupleTypePtr& cur) {
		XmlElement tupleType("tupleType", doc);
		rootElem << (tupleType << XmlElement::Attribute("id", GET_ID(cur)));
		appendList(tupleType, cur->getElementTypes(), "elementTypeList", "typePtr");

		visitAnnotations(cur->getAnnotations(), tupleType);
	}

	void visitTypeVariable(const TypeVariablePtr& cur) {
		XmlElement typeVariable("typeVariable", doc);
		typeVariable << XmlElement::Attribute("id", GET_ID(cur))
					<< XmlElement::Attribute("name", cur->getVarName()->getValue());
		rootElem << typeVariable;

		visitAnnotations(cur->getAnnotations(), typeVariable);
	}

	void visitRecType(const RecTypePtr& cur) {
		XmlElement recType("recType", doc);
		rootElem << (recType << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement definition("definition", doc);
		append(definition, cur->getDefinition(), "recTypeDefinitionPtr");
		recType << definition;

		XmlElement typeVariable("typeVariable", doc);
		append(typeVariable, cur->getTypeVariable(), "typeVariablePtr");
		recType << typeVariable;

		visitAnnotations(cur->getAnnotations(), recType);
	}

	void visitRecTypeDefinition(const RecTypeDefinitionPtr& cur) {
		XmlElement recTypeDefinition("recTypeDefinition", doc);
		rootElem << (recTypeDefinition << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement definitions("definitions", doc);
		recTypeDefinition << definitions;

		const vector<RecTypeBindingPtr> defs = cur->getDefinitions();
		for(auto iter = defs.begin(), end = defs.end(); iter != end; ++iter) {
			XmlElement definition("definition", doc);
			append(definition, (*iter)->getVariable(), "typeVariablePtr");
			append(definition, (*iter)->getType(), "typePtr");
			definitions << definition;
		}
		visitAnnotations(cur->getAnnotations(), recTypeDefinition);
	}

	void visitLiteral(const LiteralPtr& cur) {
		XmlElement literal("literal", doc);
		literal << XmlElement::Attribute("id", GET_ID(cur))
				<< XmlElement::Attribute("value", cur->getStringValue());
		rootElem << literal;

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		literal << type;

		visitAnnotations(cur->getAnnotations(), literal);
	}

	void visitReturnStmt(const ReturnStmtPtr& cur) {
		XmlElement returnStmt("returnStmt", doc);
		rootElem << (returnStmt << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement returnExpression("returnExpression", doc);
		append(returnExpression, cur->getReturnExpr(), "expressionPtr");
		returnStmt << returnExpression;

		visitAnnotations(cur->getAnnotations(), returnStmt);
	}

	void visitForStmt(const ForStmtPtr& cur) {
		XmlElement forStmt("forStmt", doc);
		rootElem << (forStmt << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement declaration("declaration", doc);
		DeclarationStmtPtr decl = DeclarationStmt::get(cur->getNodeManager(), cur->getIterator(), cur->getStart());
		append(declaration, decl, "declarationStmtPtr");
		forStmt << declaration;

		visit(decl); // it is not in the IR Tree => visit explicitly

		XmlElement body("body", doc);
		append(body, cur->getBody(), "statementPtr");
		forStmt << body;

		XmlElement end("end", doc);
		append(end, cur->getEnd(), "expressionPtr");
		forStmt << end;

		XmlElement step("step", doc);
		append(step, cur->getStep(), "expressionPtr");
		forStmt << step;

		visitAnnotations(cur->getAnnotations(), forStmt);
	}

	void visitIfStmt(const IfStmtPtr& cur) {
		XmlElement ifStmt("ifStmt", doc);
		rootElem << (ifStmt << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement condition("condition", doc);
		append(condition, cur->getCondition(), "expressionPtr");
		ifStmt << condition;

		XmlElement thenBody("thenBody", doc);
		append(thenBody, cur->getThenBody(), "statementPtr");
		ifStmt << thenBody;

		XmlElement elseBody("elseBody", doc);
		append(elseBody, cur->getElseBody(), "statementPtr");
		ifStmt << elseBody;

		visitAnnotations(cur->getAnnotations(), ifStmt);
	}

	void visitSwitchStmt(const SwitchStmtPtr& cur) {
		XmlElement switchStmt("switchStmt", doc);
		rootElem << (switchStmt << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement expression("expression", doc);
		append(expression, cur->getSwitchExpr(), "expressionPtr");
		switchStmt << expression;

		XmlElement cases("cases", doc);
		switchStmt << cases;

		const vector<SwitchCasePtr>& cas = cur->getCases()->getElements();
		std::for_each(cas.begin(), cas.end(),
			[this, &cases](const SwitchCasePtr& curr) {
				XmlElement caseEl("case", this->doc);
				append(caseEl, curr->getGuard(), "expressionPtr");
				append(caseEl, curr->getBody(), "statementPtr");
				cases << caseEl;
			}
		);

		XmlElement defaultCase("defaultCase", doc);
		append(defaultCase, cur->getDefaultCase(), "statementPtr");
		switchStmt << defaultCase;

		visitAnnotations(cur->getAnnotations(), switchStmt);
	}

	void visitWhileStmt(const WhileStmtPtr& cur) {
		XmlElement whileStmt("whileStmt", doc);
		rootElem << (whileStmt << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement condition("condition", doc);
		append(condition, cur->getCondition(), "expressionPtr");
		whileStmt << condition;

		XmlElement body("body", doc);
		append(body,  cur->getBody(), "statementPtr");
		whileStmt << body;

		visitAnnotations(cur->getAnnotations(), whileStmt);
	}

	void visitBreakStmt(const BreakStmtPtr& cur) {
		XmlElement breakStmt("breakStmt", doc);
		rootElem << (breakStmt << XmlElement::Attribute("id", GET_ID(cur)));
		visitAnnotations(cur->getAnnotations(), breakStmt);
	}

	void visitContinueStmt(const ContinueStmtPtr& cur) {
		XmlElement continueStmt("continueStmt", doc);
		rootElem << (continueStmt << XmlElement::Attribute("id", GET_ID(cur)));
		visitAnnotations(cur->getAnnotations(), continueStmt);
	}

	void visitCompoundStmt(const CompoundStmtPtr& cur) {
		XmlElement compoundStmt("compoundStmt", doc);
		rootElem << (compoundStmt << XmlElement::Attribute("id", GET_ID(cur)));
		appendList(compoundStmt, cur->getStatements(), "statements", "statementPtr");

		visitAnnotations(cur->getAnnotations(), compoundStmt);
	}

	void visitStructExpr(const StructExprPtr& cur) {
		XmlElement structExpr("structExpr",doc);
		rootElem << (structExpr << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		structExpr << type;

		XmlElement members("members",doc);
		const vector<NamedValuePtr>& membersVec = cur->getMembers()->getElements();
		std::for_each(membersVec.begin(), membersVec.end(),
			[this, &members](const NamedValuePtr& curr) {
				XmlElement member("member", this->doc);
				members << member;
				this->append(member, curr->getName(), "identifierPtr");
				this->append(member, curr->getValue(), "expressionPtr");
			}
		);
		structExpr << members;

		visitAnnotations(cur->getAnnotations(), structExpr);
	}

	void visitUnionExpr(const UnionExprPtr& cur) {
		XmlElement unionExpr("unionExpr", doc);
		rootElem << (unionExpr << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		unionExpr << type;
		
		XmlElement memberName("memberName", doc);
		append(memberName, cur->getMemberName(), "identifierPtr");
		unionExpr << memberName;
		
		XmlElement member("member", doc);
		append(member, cur->getMember(), "expressionPtr");
		unionExpr << member;

		visitAnnotations(cur->getAnnotations(), unionExpr);	
	}

	void visitVectorExpr(const VectorExprPtr& cur) {
		XmlElement vectorExpr("vectorExpr", doc);
		rootElem << (vectorExpr << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		vectorExpr << type;
		
		appendList(vectorExpr, cur->getExpressions()->getElements(), "expressions", "expressionPtr");

		visitAnnotations(cur->getAnnotations(), vectorExpr);
	}

	void visitTupleExpr(const TupleExprPtr& cur) {
		XmlElement tupleExpr("tupleExpr", doc);
		rootElem << (tupleExpr << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		tupleExpr << type;

		appendList(tupleExpr, cur->getExpressions()->getElements(), "expressions", "expressionPtr");

		visitAnnotations(cur->getAnnotations(), tupleExpr);
	}

	void visitCastExpr(const CastExprPtr& cur) {
		XmlElement castExpr("castExpr", doc);
		rootElem << (castExpr << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		castExpr << type;

		XmlElement subExpression("subExpression", doc);
		append(subExpression, cur->getSubExpression(), "expressionPtr");
		castExpr << subExpression;

		visitAnnotations(cur->getAnnotations(), castExpr);
	}

	void visitCallExpr(const CallExprPtr& cur) {
		XmlElement callExpr("callExpr", doc);
		rootElem << (callExpr << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		callExpr << type;

		XmlElement function("function", doc);
		append(function, cur->getFunctionExpr(), "expressionPtr");
		callExpr << function;

		appendList(callExpr, cur->getArguments(), "arguments", "expressionPtr");

		visitAnnotations(cur->getAnnotations(), callExpr);
	}

	void visitVariable(const VariablePtr& cur) {
		XmlElement variable("variable", doc);
		variable << XmlElement::Attribute("id", GET_ID(cur))
				 << XmlElement::Attribute("varId", numeric_cast<std::string>(cur->getId()));
		rootElem << variable;

		XmlElement type("type", doc);
		append(type, cur->getType(), "typePtr");
		variable << type;

		visitAnnotations(cur->getAnnotations(), variable);
	}

	void visitDeclarationStmt(const DeclarationStmtPtr& cur) {
		XmlElement declarationStmt("declarationStmt", doc);
		rootElem << (declarationStmt << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement variable("variable", doc);
		append(variable, cur->getVariable(), "variablePtr");
		declarationStmt << variable;

		XmlElement expression("expression", doc);
		append(expression, cur->getInitialization(), "expressionPtr");
		declarationStmt << expression;

		visitAnnotations(cur->getAnnotations(), declarationStmt);
	}

	void visitJobExpr(const JobExprPtr& cur) {
		XmlElement jobExpr("jobExpr", doc);
		rootElem << (jobExpr << XmlElement::Attribute("id", GET_ID(cur)));
		
		appendList(jobExpr, cur->getLocalDecls()->getElements(), "declarations", "declarationStmtPtr");

		XmlElement guardedStatements("guardedStatements", doc);
		jobExpr << guardedStatements;

		const vector<GuardedExprPtr>& stmtsVec = cur->getGuardedExprs()->getElements();
		std::for_each(stmtsVec.begin(), stmtsVec.end(),
			[ this, &guardedStatements ](const GuardedExprPtr& curr) {
				XmlElement guardedStatement("guardedStatement", this->doc);
				this->append(guardedStatement, curr->getGuard(), "expressionPtr");
				this->append(guardedStatement, curr->getExpression(), "expressionPtr");
				guardedStatements << guardedStatement;
			}
		);

		XmlElement defaultStatement("defaultStatement", doc);
		append(defaultStatement, cur->getDefaultExpr(), "expressionPtr");
		jobExpr << defaultStatement;
		
		XmlElement threadNumRange("threadNumRange", doc);
		append(threadNumRange, cur->getThreadNumRange(), "expressionPtr");
		jobExpr << threadNumRange;

		visitAnnotations(cur->getAnnotations(), jobExpr);
	}

	void visitLambdaExpr(const LambdaExprPtr& cur) {
		XmlElement lambdaExpr("lambdaExpr", doc);
		rootElem << (lambdaExpr << XmlElement::Attribute("id", GET_ID(cur)));
		
		XmlElement variable("variable", doc);
		append(variable, cur->getVariable(), "variablePtr");
		lambdaExpr << variable;
		
		XmlElement definition("definition", doc);
		append(definition, cur->getDefinition(), "lambdaDefinitionPtr");
		lambdaExpr << definition;

		visitAnnotations(cur->getAnnotations(), lambdaExpr);
	}

	void visitProgram(const ProgramPtr& cur) {
		XmlElement program("program", doc);
		program << XmlElement::Attribute("id", GET_ID(cur))
				<< XmlElement::Attribute("main", "0");
		rootElem << program;
		
		appendList(program,
			std::vector<ExpressionPtr>(cur->getEntryPoints().begin(), cur->getEntryPoints().end()) ,
			"expressions", "expressionPtr");
		visitAnnotations(cur->getAnnotations(), program);
	}

	void visitLambdaDefinition(const LambdaDefinitionPtr& cur) {
		XmlElement lambdaDefinition("lambdaDefinition", doc);
		rootElem << (lambdaDefinition << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement definitions("definitions", doc);
		lambdaDefinition << definitions;

		const vector<LambdaBindingPtr>& defs = cur->getDefinitions();
		std::for_each(defs.begin(), defs.end(),
			[this, &definitions ](const LambdaBindingPtr& cur) {
				XmlElement definition("definition", doc);
				append(definition, cur->getVariable(), "variablePtr");
				append(definition, cur->getLambda(), "lambdaPtr");
				definitions << definition;
			}
		);

		visitAnnotations(cur->getAnnotations(), lambdaDefinition);
	}
	
	void visitLambda(const LambdaPtr& cur) {
		XmlElement lambda("lambda", doc);
		rootElem << (lambda << XmlElement::Attribute("id", GET_ID(cur)));

		XmlElement type("type", doc);
		append(type, cur->getType(), "functionTypePtr");
		lambda << type;

		appendList(lambda, cur->getParameterList(), "paramList", "variablePtr");

		XmlElement body("body", doc);
		append(body, cur->getBody(), "statementPtr");
		lambda << body;

		visitAnnotations(cur->getAnnotations(), lambda);
	}
	
	void visitMarkerStmt(const MarkerStmtPtr& cur) {
		XmlElement markerStmt("markerStmt", doc);
		markerStmt << XmlElement::Attribute("id", GET_ID(cur))
				 << XmlElement::Attribute("markId", numeric_cast<std::string>(cur->getId()));
		rootElem << markerStmt;

		XmlElement subStatement("subStatement", doc);
		append(subStatement, cur->getSubStatement(), "statementPtr");
		markerStmt << subStatement;

		visitAnnotations(cur->getAnnotations(), markerStmt);
	}
	
	void visitMarkerExpr(const MarkerExprPtr& cur) {
		XmlElement markerExpr("markerExpr", doc);
		markerExpr << XmlElement::Attribute("id", GET_ID(cur))
				 << XmlElement::Attribute("markId", numeric_cast<std::string>(cur->getId()));
		rootElem << markerExpr;

		XmlElement subExpression("subExpression", doc);
		append(subExpression, cur->getSubExpression(), "expressionPtr");
		markerExpr << subExpression;

		visitAnnotations(cur->getAnnotations(), markerExpr);
	}
	
	void visitStringValue(const StringValuePtr& cur) {
		XmlElement identifier("identifier", doc);
		identifier << XmlElement::Attribute("id", GET_ID(cur))
				   << XmlElement::Attribute("name", cur->getValue());
		rootElem << identifier;
		visitAnnotations(cur->getAnnotations(), identifier);
	}
	
	void visitBindExpr (const BindExprPtr& cur){
		XmlElement bindExpr("bindExpr", doc);
		rootElem << (bindExpr << XmlElement::Attribute("id", GET_ID(cur)));

		appendList(bindExpr, cur->getParameters()->getElements(), "paramList", "variablePtr");

		XmlElement call("call", doc);
		append(call, cur->getCall(), "callExprPtr");
		bindExpr << call;

		visitAnnotations(cur->getAnnotations(), bindExpr);
	}

	void visitNode(const NodePtr& cur) {
		if (cur->getNodeCategory() != NC_Support && cur->getNodeCategory() != NC_Value) {
			std::cerr << "Unsupported node type: " << cur->getNodeType() << "\n";
			assert(false && "Encountered unsupported node!");
		}
	}
};

}

namespace insieme {
namespace xml {

void XmlUtil::convertIrToDom(const NodePtr& node) {
	if (doc) {
		doc->release();
		doc = NULL;
	}
	doc = impl->createDocument(0, toUnicode("inspire"),0);
	XmlElement rootNode("rootNode", doc);
	doc->getDocumentElement()->appendChild(rootNode.getBase());

	XmlElement nodePtr ("nodePtr", doc);
	nodePtr << XmlElement::Attribute("ref", GET_ID(node));
	rootNode << nodePtr;

	XmlVisitor visitor(doc);
	visitDepthFirstOnce(node, visitor);
}

XmlElementPtr XmlConverter::irToDomAnnotation (const Annotation& ann, xercesc::DOMDocument* doc) const {
	const string& type = ann.getAnnotationName();
	IrToDomConvertMapType::const_iterator fit = IrToDomConvertMap.find(type);
	if(fit != IrToDomConvertMap.end()) {
		return (fit->second)(ann, doc);
	}
	LOG(log::WARNING) << "Annotation \"" << type << "\" is not registred for Xml_write!";
	return shared_ptr<XmlElement>();
}

} // End xml namespace
} // End insieme namespace

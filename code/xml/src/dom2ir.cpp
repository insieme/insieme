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

#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::xml;
using namespace std;

XERCES_CPP_NAMESPACE_USE

namespace { // begin namespace

void buildAnnotations(const XmlElement& type, const Node& baseType){
	XmlElementPtr annotations = type.getFirstChildByName("annotations");
	if(!annotations) return;

	XmlConverter& xmlConverter = XmlConverter::get();
	XmlElementList&& ann = annotations->getChildrenByName("annotation");
	for(auto iter = ann.begin(), end = ann.end(); iter != end; ++iter) {
		baseType.getAnnotationContainer().addAnnotation(xmlConverter.domToIrAnnotation(*iter));
	}
}

class NodeBuilder {
	NodeManager& mgr;
	IRBuilder builder;

	typedef std::pair<const XmlElement*, NodePtr> MapElement;
	typedef std::map<unsigned long, MapElement> ElemMapType;

	ElemMapType elemMap;

	const MapElement& getElementId(const std::string& id) const {
		assert(!id.empty());
		ElemMapType::const_iterator fit = elemMap.find(numeric_cast<unsigned long>(id));
		assert(fit != elemMap.end() && "Odd thing happened, element not in the map!");
		return fit->second;
	}

	void checkRef(const XmlElement& elem) {
		std::string&& id = elem.getAttr("ref");
		if( !id.empty() ) {
			buildNode( *(getElementId(id).first) );
		}
		XmlElementList&& children = elem.getChildren();
		std::for_each(children.begin(), children.end(), [this](const XmlElement& curr) { this->checkRef(curr); });
	}

	template <class T>
	void updateMap(const XmlElement& elem, const T& node) {
		string&& id = elem.getAttr("id");
		assert(!id.empty());
		auto fit = elemMap.find(numeric_cast<unsigned long>(id));
		assert(fit != elemMap.end());
		assert(fit->second.first == &elem && "The XmlElement associated to this node has changed!");
		assert(!fit->second.second && "Value have been resolved before!");
		fit->second.second = node;
	}

	template <class T>
	Pointer<const T> createNode(const XmlElement& elem) {
		Pointer<const T>&& pointer = dynamic_pointer_cast<const T>(getElementId(elem.getAttr("ref")).second);
		assert(pointer && "Element not of the expected type.");
		return pointer;
	}

	template <class T>
	Pointer<const T> createNode(const XmlElement& elem, const string& first, const string& second) {
		return createNode<T>(*elem.getFirstChildByName(first)->getFirstChildByName(second));
	}

	template <class T>
	Pointer<const T> createNode(const XmlElement& elem, const string& first) {
		return createNode<T>(*elem.getFirstChildByName(first));
	}

	template <class NodeTy, class ... Args>
	Pointer<const NodeTy> createIrNode(const XmlElement& elem, Args ... args) {
		// test whether element has been resolved before
		if (Pointer<const NodeTy> res = dynamic_pointer_cast<const NodeTy>(getElementId(elem.getAttr("id")).second)) {
			return res;
		}

		// needs to be constructed
		Pointer<const NodeTy> irNode = NodeTy::get(mgr, args...);
		buildAnnotations(elem, *irNode);
		updateMap(elem, irNode);
		return irNode;
	}

public:
	NodeBuilder(NodeManager& mgr): mgr(mgr), builder(mgr) { }

	NodePtr handle_genType(const XmlElement& elem) {
		TypePtr baseType;
		XmlElementPtr&& base = elem.getFirstChildByName("baseType");
		if (base) {
			baseType = createNode<Type>(*base, "typePtr");
		}

		vector<TypePtr> typeParams;
		XmlElementPtr&& param = elem.getFirstChildByName("typeParams");
		if (param){
			XmlElementList&& types = param->getChildrenByName("typePtr");
			for(auto iter = types.begin(), end = types.end(); iter != end; ++iter) {
				typeParams.push_back( createNode<Type>(*iter) );
			}
		}

		vector<IntTypeParamPtr> intTypeParams;
		XmlElementPtr&& intParam = elem.getFirstChildByName("intTypeParams");
		if (intParam){
			XmlElementList&& types = intParam->getChildrenByName("intTypeParamPtr");
			for(auto iter = types.begin(), end = types.end(); iter != end; ++iter) {
				intTypeParams.push_back( createNode<IntTypeParam>(*iter) );
			}
		}
		string&& name = elem.getAttr("familyName");
		if (name == "vector")  	return createIrNode<VectorType>(elem, typeParams[0], intTypeParams[0]);
		if (name == "array")   	return createIrNode<ArrayType>(elem, typeParams[0], intTypeParams[0]);
		if (name == "channel") 	return createIrNode<ChannelType>(elem, typeParams[0], intTypeParams[0]);
		if (name == "ref")		return createIrNode<RefType>(elem, typeParams[0]);
		return createIrNode<GenericType>(elem, builder.stringValue(name), builder.types(typeParams), builder.intTypeParams(intTypeParams));
	}
	
	NodePtr handle_concreteIntTypeParam(const XmlElement& elem) {
		const unsigned value = numeric_cast<unsigned>(elem.getFirstChildByName("concrete")->getAttr("value"));
		return createIrNode<ConcreteIntTypeParam>(elem, value);
	}
	
	NodePtr handle_variableIntTypeParam(const XmlElement& elem) {
		const char value = elem.getFirstChildByName("variable")->getAttr("value")[0];
		return createIrNode<VariableIntTypeParam>(elem, value);
	}
	
	NodePtr handle_infiniteIntTypeParam(const XmlElement& elem) {
		return createIrNode<InfiniteIntTypeParam>(elem);
	}

	NodePtr handle_functionType(const XmlElement& elem) {
		XmlElementList&& params = elem.getFirstChildByName("parameterTypeList")->getChildrenByName("typePtr");
		TypeList paramTypes;
		for(auto iter = params.begin(), end = params.end(); iter != end; ++iter) {
			paramTypes.push_back(createNode<Type>(*iter));
		}
		TypePtr&& retType = createNode<Type>(elem, "returnType","typePtr");
		FunctionKind kind = (FunctionKind)numeric_cast<int>(elem.getAttr("kind"));
		return createIrNode<FunctionType>(elem, paramTypes, retType, kind);
	}

private:
	NamedCompositeType::Entries visit_namedCompositeType(const XmlElement& elem) {
		NamedCompositeType::Entries entryVec;
		XmlElementList&& entries = elem.getFirstChildByName("entries")->getChildrenByName("entry");
		for(auto iter = entries.begin(), end = entries.end(); iter != end; ++iter) {
			StringValuePtr&& ident = createNode<StringValue>(*iter, "identifierPtr");
			TypePtr&& el = createNode<Type>(*iter, "typePtr");
			entryVec.push_back( builder.namedType(ident, el) );
		}
		return entryVec;
	}
public:
	NodePtr handle_unionType(const XmlElement& elem) {
		return createIrNode<UnionType>(elem, visit_namedCompositeType(elem));
	}

	NodePtr handle_structType(const XmlElement& elem) {
		return createIrNode<StructType>(elem, visit_namedCompositeType(elem));
	}

	NodePtr handle_tupleType(const XmlElement& elem) {
		XmlElementList&& types = elem.getFirstChildByName("elementTypeList")->getChildrenByName("typePtr");
		TypeList elementList;
		for(auto iter = types.begin(), end = types.end(); iter != end; ++iter) {
			elementList.push_back(createNode<Type>(*iter));
		}
		return createIrNode<TupleType>(elem, elementList);
	}

	NodePtr handle_typeVariable(const XmlElement& elem) {
		return createIrNode<TypeVariable>(elem, elem.getAttr("name"));
	}

	NodePtr handle_recType(const XmlElement& elem) {
		RecTypeDefinitionPtr&& definition = createNode<RecTypeDefinition>(elem, "definition","recTypeDefinitionPtr");
		TypeVariablePtr&& var = createNode<TypeVariable>(elem, "typeVariable","typeVariablePtr");
		return createIrNode<RecType>(elem, var, definition);
	}

	NodePtr handle_recTypeDefinition(const XmlElement& elem) {
		XmlElementList&& defs = elem.getFirstChildByName("definitions")->getChildrenByName("definition");
		vector<RecTypeBindingPtr> defVec;
		for(auto iter = defs.begin(), end = defs.end(); iter != end; ++iter) {
			TypeVariablePtr&& var = createNode<TypeVariable>(*iter, "typeVariablePtr");
			TypePtr&& ty = createNode<Type>(*iter, "typePtr");
			defVec.push_back( builder.recTypeBinding(var, ty) );
		}
		return createIrNode<RecTypeDefinition>(elem, defVec);
	}

	NodePtr handle_literal(const XmlElement& elem) {
		TypePtr&& typeT = createNode<Type>(elem, "type", "typePtr");
		return createIrNode<Literal>(elem, typeT, elem.getAttr("value"));
	}

	NodePtr handle_returnStmt(const XmlElement& elem) {
		ExpressionPtr&& expression = createNode<Expression>(elem, "returnExpression", "expressionPtr");
		return createIrNode<ReturnStmt>(elem, expression);
	}

	NodePtr handle_forStmt(const XmlElement& elem) {
		DeclarationStmtPtr&& decl = createNode<DeclarationStmt>(elem, "declaration", "declarationStmtPtr");
		CompoundStmtPtr&& body = createNode<CompoundStmt>(elem, "body", "statementPtr");
		ExpressionPtr&& end = createNode<Expression>(elem, "end", "expressionPtr");
		ExpressionPtr&& step = createNode<Expression>(elem, "step", "expressionPtr");
		return createIrNode<ForStmt>(elem, decl->getVariable(), decl->getInitialization(), end, step, body);
	}

	NodePtr handle_ifStmt(const XmlElement& elem) {
		ExpressionPtr&& expr = createNode<Expression>(elem, "condition", "expressionPtr");
		CompoundStmtPtr&& thenStmt = createNode<CompoundStmt>(elem, "thenBody", "statementPtr");
		CompoundStmtPtr&& elseStmt = createNode<CompoundStmt>(elem, "elseBody", "statementPtr");
		return createIrNode<IfStmt>(elem, expr, thenStmt, elseStmt);
	}

	NodePtr handle_switchStmt(const XmlElement& elem) {
		ExpressionPtr&& expr = createNode<Expression>(elem, "expression", "expressionPtr");
		XmlElementList&& cases = elem.getFirstChildByName("cases")->getChildrenByName("case");

		vector<SwitchCasePtr> caseVector;
		for(auto iter = cases.begin(), end = cases.end(); iter != end; ++iter) {
			LiteralPtr&& expr = createNode<Literal>(*iter, "expressionPtr");
			CompoundStmtPtr&& stat = createNode<CompoundStmt>(*iter, "statementPtr");
			caseVector.push_back( builder.switchCase(static_pointer_cast<LiteralPtr>(expr), stat) );
		}
		CompoundStmtPtr&& defaultCase = createNode<CompoundStmt>(elem, "defaultCase", "statementPtr");

		return createIrNode<SwitchStmt>(elem, expr, builder.switchCases(caseVector), defaultCase);
	}

	NodePtr handle_whileStmt(const XmlElement& elem) {
		ExpressionPtr&& cond = createNode<Expression>(elem, "condition", "expressionPtr");
		CompoundStmtPtr&& body = createNode<CompoundStmt>(elem, "body", "statementPtr");
		return createIrNode<WhileStmt>(elem, cond, body);
	}

	NodePtr handle_breakStmt(const XmlElement& elem) {
		return createIrNode<BreakStmt>(elem);
	}

	NodePtr handle_continueStmt(const XmlElement& elem) {
		return createIrNode<ContinueStmt>(elem);
	}

	NodePtr handle_compoundStmt(const XmlElement& elem) {
		XmlElementList&& stats = elem.getFirstChildByName("statements")->getChildrenByName("statementPtr");
		vector<StatementPtr> stmtVec;
		for(auto iter = stats.begin(), end = stats.end(); iter != end; ++iter) {
			stmtVec.push_back( createNode<Statement>(*iter));
		}
		return createIrNode<CompoundStmt>(elem, stmtVec);
	}

	NodePtr handle_declarationStmt(const XmlElement& elem) {
		VariablePtr&& var = createNode<Variable>(elem, "variable", "variablePtr");
		ExpressionPtr&& expr = createNode<Expression>(elem, "expression", "expressionPtr");
		return createIrNode<DeclarationStmt>(elem, var, expr);
	}

	NodePtr handle_structExpr(const XmlElement& elem) {
		StructTypePtr&& typeT = createNode<StructType>(elem, "type", "typePtr");
		XmlElementList&& membs = elem.getFirstChildByName("members")->getChildrenByName("member");

		StructExpr::Members membVec;
		for(auto iter = membs.begin(), end = membs.end(); iter != end; ++iter) {
			StringValuePtr&& ident = createNode<StringValue>(*iter, "identifierPtr");
			ExpressionPtr&& expr = createNode<Expression>(*iter, "expressionPtr");
			membVec.push_back( builder.namedValue(ident, expr) );
		}
		return createIrNode<StructExpr>(elem, typeT, builder.namedValues(membVec));
	}

	NodePtr handle_unionExpr(const XmlElement& elem) {
		UnionTypePtr&& typeT = createNode<UnionType>(elem, "type", "typePtr");
		StringValuePtr&& ident = createNode<StringValue>(elem, "memberName", "identifierPtr");
		ExpressionPtr&& expr = createNode<Expression>(elem, "member", "expressionPtr");

		return createIrNode<UnionExpr>(elem, typeT, ident, expr);
	}

	NodePtr handle_vectorExpr(const XmlElement& elem) {
		VectorTypePtr&& typeT = createNode<VectorType>(elem, "type", "typePtr");
		XmlElementList&& exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expressionPtr");
		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(), end = exprs.end(); iter != end; ++iter) {
			exprVec.push_back( createNode<Expression>(*iter));
		}
		return createIrNode<VectorExpr>(elem, typeT, builder.expressions(exprVec));
	}

	NodePtr handle_tupleExpr(const XmlElement& elem) {
		TupleTypePtr&& typeT = createNode<TupleType>(elem, "type", "typePtr");
		XmlElementList&& exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expressionPtr");

		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(), end = exprs.end(); iter != end; ++iter) {
			exprVec.push_back( createNode<Expression>(*iter));
		}
		return createIrNode<TupleExpr>(elem, typeT, builder.expressions(exprVec));
	}

	NodePtr handle_castExpr(const XmlElement& elem) {
		TypePtr&& typeT = createNode<Type>(elem, "type", "typePtr");
		ExpressionPtr&& expr = createNode<Expression>(elem, "subExpression", "expressionPtr");
		return createIrNode<CastExpr>(elem, typeT, expr);
	}

	NodePtr handle_callExpr(const XmlElement& elem) {
		TypePtr&& typeT = createNode<Type>(elem, "type", "typePtr");
		ExpressionPtr&& expr = createNode<Expression>(elem, "function", "expressionPtr");
		XmlElementList&& args = elem.getFirstChildByName("arguments")->getChildrenByName("expressionPtr");

		vector<ExpressionPtr> argVec;
		for(auto iter = args.begin(), end = args.end(); iter != end; ++iter) {
			argVec.push_back( createNode<Expression>(*iter));
		}
		return createIrNode<CallExpr>(elem, typeT, expr, argVec);
	}

	NodePtr handle_variable(const XmlElement& elem) {
		TypePtr&& typeT = createNode<Type>(elem, "type", "typePtr");
		return createIrNode<Variable>(elem, typeT, numeric_cast<int>(elem.getAttr("varId")));
	}

	NodePtr handle_jobExpr(const XmlElement& elem) {
		XmlElementList&& decls = elem.getFirstChildByName("declarations")->getChildrenByName("declarationStmtPtr");
		vector<DeclarationStmtPtr> localDecls;
		for(auto iter = decls.begin(), end = decls.end(); iter != end; ++iter) {
			localDecls.push_back(createNode<DeclarationStmt>(*iter));
		}
		
		XmlElementList&& stmts = elem.getFirstChildByName("guardedStatements")->getChildrenByName("guardedStatement");
		vector<GuardedExprPtr> guardedStmts;
		for(auto iter = stmts.begin(), end = stmts.end(); iter != end; ++iter) {
			XmlElementList&& types = iter->getChildrenByName("expressionPtr");
			guardedStmts.push_back( builder.guardedExpr(createNode<LambdaExpr>(types[0]), createNode<Expression>(types[1])) );
		}
		
		ExpressionPtr&& defaultStmt = createNode<Expression>(elem, "defaultStatement", "expressionPtr");
		
		ExpressionPtr&& threadNum = createNode<Expression>(elem, "threadNumRange", "expressionPtr");
		
		GenericTypePtr type = static_pointer_cast<GenericTypePtr>(builder.getLangBasic().getJob());

		return createIrNode<JobExpr>(elem, type, threadNum, builder.declarationStmts(localDecls), builder.guardedExprs(guardedStmts), defaultStmt);
	}

	NodePtr handle_lambdaDefinition(const XmlElement& elem) {
		XmlElementList&& defs = elem.getFirstChildByName("definitions")->getChildrenByName("definition");

		vector<LambdaBindingPtr> definitions;
		for(auto iter = defs.begin(), end = defs.end(); iter != end; ++iter) {
			VariablePtr&& var = createNode<Variable>(*iter, "variablePtr");
			LambdaPtr&& lambda = createNode<Lambda>(*iter, "lambdaPtr");
			definitions.push_back( builder.lambdaBinding(var, lambda) );
		}

		return createIrNode<LambdaDefinition>(elem, definitions);
	}

	NodePtr handle_lambdaExpr(const XmlElement& elem) {
		VariablePtr&& variable = createNode<Variable>(elem, "variable", "variablePtr");
		LambdaDefinitionPtr&& definition = createNode<LambdaDefinition>(elem, "definition", "lambdaDefinitionPtr");
		
		return createIrNode<LambdaExpr>(elem, variable, definition);
	}
	
	NodePtr handle_lambda(const XmlElement& elem) {
		FunctionTypePtr&& typeT = createNode<FunctionType>(elem, "type", "functionTypePtr");
		
		XmlElementList&& pars = elem.getFirstChildByName("paramList")->getChildrenByName("variablePtr");
		vector<VariablePtr> paramList;
		for(auto iter = pars.begin(), end = pars.end(); iter != end; ++iter) {
			paramList.push_back(createNode<Variable>(*iter));
		}
		
		CompoundStmtPtr&& body = createNode<CompoundStmt>(elem, "body", "statementPtr");
		
		return createIrNode<Lambda>(elem, typeT, builder.parameters(paramList), body);
	}
	
	NodePtr handle_bindExpr(const XmlElement& elem) {
		XmlElementList&& pars = elem.getFirstChildByName("paramList")->getChildrenByName("variablePtr");
		vector<VariablePtr> paramList;
		for(auto iter = pars.begin(), end = pars.end(); iter != end; ++iter) {
			paramList.push_back(createNode<Variable>(*iter));
		}

		CallExprPtr&& call = createNode<CallExpr>(elem, "call", "callExprPtr");
		FunctionTypePtr type = static_pointer_cast<FunctionTypePtr>(builder.bindExpr(paramList, call)->getType());

		return createIrNode<BindExpr>(elem, type, builder.parameters(paramList), call);
	}
	
	NodePtr handle_markerStmt(const XmlElement& elem) {
		StatementPtr&& subStatement = createNode<Statement>(elem, "subStatement", "statementPtr");
		return createIrNode<MarkerStmt>(elem, builder.uintValue(numeric_cast<unsigned>(elem.getAttr("markId"))), subStatement);
	}
	
	NodePtr handle_markerExpr(const XmlElement& elem) {
		ExpressionPtr&& subExpression = createNode<Expression>(elem, "subExpression", "expressionPtr");
		return createIrNode<MarkerExpr>(elem, builder.uintValue(numeric_cast<unsigned>(elem.getAttr("markId"))), subExpression);
	}

	NodePtr handle_identifier(const XmlElement& elem) {
		return createIrNode<StringValue>(elem, elem.getAttr("name"));
	}
	
	NodePtr handle_program(const XmlElement& elem) {
		XmlElementList&& exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expressionPtr");

		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(), end = exprs.end(); iter != end; ++iter) {
			exprVec.push_back( createNode<Expression>(*iter));
		}
		
		ProgramPtr&& program = Program::get(mgr, exprVec);

		buildAnnotations(elem, *program);
		updateMap(elem, program);
		return program;
	}

	NodePtr handle_rootNode(const XmlElement& elem) {
		XmlElementPtr&& type = elem.getFirstChildByName("nodePtr");
		string&& ref = type->getAttr("ref");
		assert(!ref.empty());
		auto fit = elemMap.find(numeric_cast<unsigned long>(ref));
		assert(fit != elemMap.end() && "Element not in the map");

		return fit->second.second;
	}

	#define DISPATCH(TYPE) if(nodeName == #TYPE) { return handle_ ## TYPE(elem); }

	NodePtr buildNode(const XmlElement& elem) {

		// check whether node has been resolved before
		string&& id = elem.getAttr("id");
		if( !id.empty() ) {
			// => take from cache
			NodePtr res = getElementId(id).second;
			if (res) return res;
		}

		// ensure all referenced nodes are covered
		checkRef(elem);

		// different types of nodes
		string&& nodeName = elem.getName();
		DISPATCH(genType)			DISPATCH(functionType)		DISPATCH(unionType)			DISPATCH(structType)			DISPATCH(concreteIntTypeParam)
		DISPATCH(typeVariable)		DISPATCH(recType)			DISPATCH(recTypeDefinition)	DISPATCH(literal)				DISPATCH(variableIntTypeParam)
		DISPATCH(forStmt)			DISPATCH(ifStmt)			DISPATCH(switchStmt)		DISPATCH(whileStmt)				DISPATCH(infiniteIntTypeParam)
		DISPATCH(continueStmt)		DISPATCH(compoundStmt)		DISPATCH(declarationStmt)	DISPATCH(structExpr)			DISPATCH(unionExpr)
		DISPATCH(vectorExpr)		DISPATCH(tupleExpr)			DISPATCH(castExpr) 			DISPATCH(callExpr)				DISPATCH(variable)
		DISPATCH(jobExpr)			DISPATCH(lambdaExpr)		DISPATCH(program)			DISPATCH(lambdaDefinition)		DISPATCH(lambda)
		DISPATCH(rootNode)			DISPATCH(bindExpr)			DISPATCH(markerStmt)		DISPATCH(returnStmt)			DISPATCH(breakStmt)
		DISPATCH(markerExpr)		DISPATCH(tupleType)			DISPATCH(identifier)
		assert(false && "XML node not handled!");
		return 0;
	}

	NodePtr buildAST(const XmlElement& inspire) {
		XmlElementPtr root = inspire.getFirstChildByName("rootNode");
		assert ( root && "RootNode is not present in DOM" );

		XmlElementList&& elemVec = inspire.getChildren();
		for(auto iter = elemVec.begin(), end = elemVec.end(); iter != end; ++iter) {
			string&& id = iter->getAttr("id");
			if(!id.empty())
				elemMap[numeric_cast<unsigned long>(id)].first = &(*iter);
		}

		return buildNode(*root);
	}
};

} // end namespace

namespace insieme {
namespace xml {

NodeAnnotationPtr XmlConverter::domToIrAnnotation(const XmlElement& el) const {
	string&& type = el.getAttr("type");
	DomToIrConvertMapType::const_iterator fit = DomToIrConvertMap.find(type);
	if(fit != DomToIrConvertMap.end()) {
		return (fit->second)(el);
	}
	LOG(log::WARNING) << "Annotation \"" << type << "\" is not registered for Xml_Read!";
	return NodeAnnotationPtr();
}

NodePtr XmlUtil::convertDomToIr(NodeManager& manager) {

	XmlElement inspire(doc->getDocumentElement(), doc);
	NodeBuilder nb(manager);
	return nb.buildAST(inspire);

}

} // end xml namespace
} // end insieme namespace

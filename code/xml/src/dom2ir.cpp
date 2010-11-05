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

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLUni.hpp>

#include "ast_builder.h"
#include "xml_utils.h"
#include "xsd_config.h"
#include "logging.h"

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::xml;
using namespace std;

XERCES_CPP_NAMESPACE_USE

namespace { // begin namespace
typedef map<string, pair <const XmlElement*, NodePtr>> ElemMapType;

void buildAnnotations(const XmlElementPtr, const NodePtr, const bool);
void buildNode(NodeManager&, const XmlElement&, ElemMapType&);
void checkRef(NodeManager&, const XmlElement&, ElemMapType&);

void buildAnnotations(const XmlElement& type, const NodePtr baseType, const bool value){
	XmlElementPtr annotations = type.getFirstChildByName("annotations");
	if (annotations){
		XmlConverter& xmlConverter = XmlConverter::get();
		vector<XmlElement> ann = annotations->getChildrenByName("annotation");
		for(vector<XmlElement>::const_iterator iter = ann.begin(); iter != ann.end(); ++iter) {
			if (value)
				baseType.addAnnotation(xmlConverter.domToIrAnnotation(*iter));
			else
				baseType->addAnnotation(xmlConverter.domToIrAnnotation(*iter));
		}
	}
}

template <class T>
void updateMap(const XmlElement& elem, ElemMapType& elemMap, const T& node){
	// string id = elem.getAttr("id");
	elemMap[elem.getAttr("id")].second = node;
	// pair<const XmlElement*, NodePtr>&& oldPair = ;
	// elemMap[id] = make_pair(oldPair.first, node);
}

template <class T>
AnnotatedPtr<const T> createNode(const XmlElement& elem, const ElemMapType& elemMap) {
	ElemMapType::const_iterator fit = elemMap.find(elem.getAttr("ref"));
	assert(fit != elemMap.end() && "Odd thing happened, element not in the map!");
	AnnotatedPtr<const T>&& pointer = dynamic_pointer_cast<const T>(fit->second.second);
	assert(pointer && "Element not of the expected type.");
	buildAnnotations(elem, pointer, true);
	return pointer;
}

template <class T>
AnnotatedPtr<const T> createNode(const XmlElement& elem, const ElemMapType& elemMap, const string& first, const string& second){
	return createNode<T>(*elem.getFirstChildByName(first)->getFirstChildByName(second), elemMap);
}

template <class T>
AnnotatedPtr<const T> createNode(const XmlElement& elem, const ElemMapType& elemMap, const string& first){
	return createNode<T>(*elem.getFirstChildByName(first), elemMap);
}

void buildNode(NodeManager& manager, const XmlElement& elem, ElemMapType& elemMap) {
	checkRef(manager, elem, elemMap);

	// different types of nodes
	const string& nodeName = elem.getName();

	if (nodeName == "genType") {
		TypePtr baseType;
		const XmlElementPtr& base = elem.getFirstChildByName("baseType");
		if (base) {
			const XmlElementPtr& type = base->getFirstChildByName("typePtr");
			baseType = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, baseType, true);
		}

		vector<TypePtr> typeParams;
		const XmlElementPtr& param = elem.getFirstChildByName("typeParams");
		if (param){
			const vector<XmlElement>& types = param->getChildrenByName("typePtr");
			for(auto iter = types.begin(); iter != types.end(); ++iter) {
				typeParams.push_back( createNode<Type>(*iter, elemMap) );
			}
		}

		vector<IntTypeParam> intTypeParams;
		const XmlElementPtr& intParam = elem.getFirstChildByName("intTypeParams");
		if (intParam){
			const vector<XmlElement>& intPar = intParam->getChildrenByName("intTypeParam");
			for(vector<XmlElement>::const_iterator iter = intPar.begin(); iter != intPar.end(); ++iter) {
				if (iter->getChildrenByName("variable").size() != 0){
					intTypeParams.push_back(IntTypeParam::getVariableIntParam((iter->getFirstChildByName("variable"))->getAttr("value")[0]));
				}
				else if (iter->getChildrenByName("concrete").size() != 0){
					intTypeParams.push_back(IntTypeParam::getConcreteIntParam(numeric_cast<int>((iter->getFirstChildByName("concrete"))->getAttr("value"))));
				}
				else {
					intTypeParams.push_back(IntTypeParam::getInfiniteIntParam());
				}
			}
		}
		
		const string& name = elem.getAttr("familyName");
		if (name == "vector"){
			VectorTypePtr&& vec = VectorType::get(manager, typeParams[0], intTypeParams[0]);
			buildAnnotations(elem, vec, false);
			updateMap(elem, elemMap, vec);
		}
		else if (name == "array"){
			ArrayTypePtr&& arr = ArrayType::get(manager, typeParams[0], intTypeParams[0]);
			buildAnnotations(elem, arr, false);
			updateMap(elem, elemMap, arr);
		}
		else if (name == "channel"){
			ChannelTypePtr&& chan = ChannelType::get(manager, typeParams[0], intTypeParams[0]);
			buildAnnotations(elem, chan, false);
			updateMap(elem, elemMap, chan);
		}
		else if (name == "ref"){
			RefTypePtr&& ref = RefType::get(manager, typeParams[0]);
			buildAnnotations(elem, ref, false);
			updateMap(elem, elemMap, ref);
		}
		else {
			GenericTypePtr&& gen = GenericType::get(manager, name, typeParams, intTypeParams, baseType);
			buildAnnotations(elem, gen, false);
			updateMap(elem, elemMap, gen);
		}
	}

	else if (nodeName == "functionType") {
		TupleTypePtr&& argType = createNode<TupleType>(elem, elemMap, "argumentType","tupleTypePtr");
		TypePtr&& retType = createNode<Type>(elem, elemMap, "returnType","typePtr");
		
		FunctionTypePtr&& fun = FunctionType::get(manager, argType, retType);
		buildAnnotations(elem, fun, false);
		updateMap(elem, elemMap, fun);
	}

	else if (nodeName == "unionType" || nodeName == "structType") {
		const vector<XmlElement>& entries = elem.getFirstChildByName("entries")->getChildrenByName("entry");
		vector<NamedCompositeType::Entry> entryVec;
		for(auto iter = entries.begin(); iter != entries.end(); ++iter) {
			Identifier ident = iter->getFirstChildByName("id")->getText();
			TypePtr el = createNode<Type>(*iter, elemMap, "typePtr");
			entryVec.push_back(NamedCompositeType::Entry(ident, el));
		}
		if (nodeName == "structType") {
			StructTypePtr structT = StructType::get(manager, entryVec);
			buildAnnotations(elem, structT, false);
			updateMap(elem, elemMap, structT);
		}
		else {
			UnionTypePtr unionT = UnionType::get(manager, entryVec);
			buildAnnotations(elem, unionT, false);
			updateMap(elem, elemMap, unionT);
		}
	}

	else if (nodeName == "tupleType") {
		vector<XmlElement> types = elem.getFirstChildByName("elementTypeList")->getChildrenByName("elementType");
		vector<TypePtr> elementList;
		for(auto iter = types.begin(); iter != types.end(); ++iter) {
			TypePtr el = createNode<Type>(*iter, elemMap, "typePtr");
			elementList.push_back(el);
		}

		TupleTypePtr tuple = TupleType::get(manager, elementList);
		buildAnnotations(elem, tuple, false);
		updateMap(elem, elemMap, tuple);
	}

	else if (nodeName == "typeVariable") {
		TypeVariablePtr var = TypeVariable::get(manager, elem.getAttr("name"));
		buildAnnotations(elem, var, false);
		updateMap(elem, elemMap, var);
	}

	else if (nodeName == "recType") {
		RecTypeDefinitionPtr definition = createNode<RecTypeDefinition>(elem, elemMap, "definition","recTypeDefinitionPtr");
		TypeVariablePtr var = createNode<TypeVariable>(elem, elemMap, "typeVariable","typeVariablePtr");

		RecTypePtr recType = RecType::get(manager, var, definition);
		buildAnnotations(elem, recType, false);
		updateMap(elem, elemMap, recType);
	}

	else if (nodeName == "recTypeDefinition") {
		vector<XmlElement> defs = elem.getFirstChildByName("definitions")->getChildrenByName("definition");
		RecTypeDefinition::RecTypeDefs defVec;
		for(auto iter = defs.begin(); iter != defs.end(); ++iter) {
			TypeVariablePtr var = createNode<TypeVariable>(*iter, elemMap, "typeVariablePtr");
			TypePtr ty = createNode<Type>(*iter, elemMap, "typePtr");
			defVec.insert(std::make_pair(var, ty));
		}
		
		RecTypeDefinitionPtr rec = RecTypeDefinition::get(manager, defVec);
		buildAnnotations(elem, rec, false);
		updateMap(elem, elemMap, rec);
	}

	else if (nodeName == "literal") {
		TypePtr typeT = createNode<Type>(elem, elemMap, "type", "typePtr");
		
		LiteralPtr lit = Literal::get(manager, typeT, elem.getAttr("value"));
		buildAnnotations(elem, lit, false);
		updateMap(elem, elemMap, lit);
	}

	else if (nodeName == "returnStmt") {
		ExpressionPtr expression = createNode<Expression>(elem, elemMap, "returnExpression", "expressionPtr");
		
		ReturnStmtPtr ret = ReturnStmt::get(manager, expression);
		buildAnnotations(elem, ret, false);
		updateMap(elem, elemMap, ret);
	}

	else if (nodeName == "forStmt") {
		DeclarationStmtPtr decl = createNode<DeclarationStmt>(elem, elemMap, "declaration", "declarationStmtPtr");
		StatementPtr body = createNode<Statement>(elem, elemMap, "body", "statementPtr");
		ExpressionPtr end = createNode<Expression>(elem, elemMap, "end", "expressionPtr");
		ExpressionPtr step = createNode<Expression>(elem, elemMap, "step", "expressionPtr");
		
		ForStmtPtr fstmt = ForStmt::get(manager, decl, body, end, step);
		buildAnnotations(elem, fstmt, false);
		updateMap(elem, elemMap, fstmt);
	}

	else if (nodeName == "ifStmt") {
		ExpressionPtr expr = createNode<Expression>(elem, elemMap, "condition", "expressionPtr");
		StatementPtr thenStmt = createNode<Statement>(elem, elemMap, "thenBody", "statementPtr");
		StatementPtr elseStmt = createNode<Statement>(elem, elemMap, "elseBody", "statementPtr");

		IfStmtPtr stmt = IfStmt::get(manager, expr, thenStmt, elseStmt);
		buildAnnotations(elem, stmt, false);
		updateMap(elem, elemMap, stmt);
	}

	else if (nodeName == "switchStmt") {
		ExpressionPtr expr = createNode<Expression>(elem, elemMap, "expression", "expressionPtr");

		vector<XmlElement> cases = elem.getFirstChildByName("cases")->getChildrenByName("case");
		vector<SwitchStmt::Case> caseVector;
		for(auto iter = cases.begin(); iter != cases.end(); ++iter) {
			ExpressionPtr expr = createNode<Expression>(*iter, elemMap, "expressionPtr");
			StatementPtr stat = createNode<Statement>(*iter, elemMap, "statementPtr");
			caseVector.push_back(std::make_pair(expr, stat));
		}
		StatementPtr defaultCase = createNode<Statement>(elem, elemMap, "defaultCase", "statementPtr");
		
		SwitchStmtPtr stmt = SwitchStmt::get(manager, expr, caseVector, defaultCase);
		buildAnnotations(elem, stmt, false);
		updateMap(elem, elemMap, stmt);
	}

	else if (nodeName == "whileStmt") {
		ExpressionPtr cond = createNode<Expression>(elem, elemMap, "condition", "expressionPtr");
		StatementPtr body = createNode<Statement>(elem, elemMap, "body", "statementPtr");

		WhileStmtPtr stmt = WhileStmt::get(manager, cond, body);
		buildAnnotations(elem, stmt, false);
		updateMap(elem, elemMap, stmt);
	}

	else if (nodeName == "breakStmt") {
		BreakStmtPtr stmt = BreakStmt::get(manager);
		buildAnnotations(elem, stmt, false);
		updateMap(elem, elemMap, stmt);
	}

	else if (nodeName == "continueStmt") {
		ContinueStmtPtr stmt = ContinueStmt::get(manager);
		buildAnnotations(elem, stmt, false);
		updateMap(elem, elemMap, stmt);
	}

	else if (nodeName == "compoundStmt") {
		vector<XmlElement> stats = elem.getFirstChildByName("statements")->getChildrenByName("statement");
		vector<StatementPtr> stmtVec;
		for(auto iter = stats.begin(); iter != stats.end(); ++iter) {
			stmtVec.push_back(createNode<Statement>(*iter, elemMap, "statementPtr"));
		}

		CompoundStmtPtr comp = CompoundStmt::get(manager, stmtVec);
		buildAnnotations(elem, comp, false);
		updateMap(elem, elemMap, comp);
	}

	else if (nodeName == "declarationStmt") {
		VariablePtr var = createNode<Variable>(elem, elemMap, "variable", "variablePtr");		
		ExpressionPtr expr = createNode<Expression>(elem, elemMap, "expression", "expressionPtr");

		DeclarationStmtPtr dstmt = DeclarationStmt::get(manager, var, expr);
		buildAnnotations(elem, dstmt, false);
		updateMap(elem, elemMap, dstmt);
	}

	else if (nodeName == "structExpr") {
		StructTypePtr typeT = createNode<StructType>(elem, elemMap, "type", "typePtr");
		
		vector<XmlElement> membs = elem.getFirstChildByName("members")->getChildrenByName("member");
		StructExpr::Members membVec;
		for(auto iter = membs.begin(); iter != membs.end(); ++iter) {
			Identifier ident(iter->getFirstChildByName("id")->getText());
			ExpressionPtr expr = createNode<Expression>(*iter, elemMap, "expressionPtr");
			membVec.push_back(StructExpr::Member(ident, expr));
		}

		StructExprPtr structT = StructExpr::get(manager, typeT, membVec);
		buildAnnotations(elem, structT, false);
		updateMap(elem, elemMap, structT);
	}
	
	else if(nodeName == "unionExpr"){
		UnionTypePtr typeT = createNode<UnionType>(elem, elemMap, "type", "typePtr");		
		Identifier ident(elem.getFirstChildByName("member")->getFirstChildByName("id")->getText());
		ExpressionPtr expr = createNode<Expression>(elem, elemMap, "member", "expressionPtr");

		UnionExprPtr unionT = UnionExpr::get(manager, typeT, ident, expr);
		buildAnnotations(elem, unionT, false);
		updateMap(elem, elemMap, unionT);	
	}

	else if (nodeName == "vectorExpr") {
		VectorTypePtr typeT = createNode<VectorType>(elem, elemMap, "type", "typePtr");
		
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			ExpressionPtr expr = createNode<Expression>(*iter, elemMap,"expressionPtr");
			exprVec.push_back(expr);
		}

		VectorExprPtr vecT = VectorExpr::get(manager, typeT, exprVec);
		buildAnnotations(elem, vecT, false);
		updateMap(elem, elemMap, vecT);
	}

	else if (nodeName == "tupleExpr") {
		TupleTypePtr typeT = createNode<TupleType>(elem, elemMap, "type", "typePtr");
		
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			ExpressionPtr expr = createNode<Expression>(*iter, elemMap,"expressionPtr");
			exprVec.push_back(expr);
		}

		TupleExprPtr tuple = TupleExpr::get(manager, typeT, exprVec);
		buildAnnotations(elem, tuple, false);
		updateMap(elem, elemMap, tuple);
	}

	else if (nodeName == "castExpr") {
		TypePtr typeT = createNode<Type>(elem, elemMap, "type", "typePtr");
		ExpressionPtr expr = createNode<Expression>(elem, elemMap, "subExpression", "expressionPtr");

		CastExprPtr cast = CastExpr::get(manager, typeT, expr);
		buildAnnotations(elem, cast, false);
		updateMap(elem, elemMap, cast);
	}

	else if (nodeName == "callExpr") {
		TypePtr typeT = createNode<Type>(elem, elemMap, "type", "typePtr");
		ExpressionPtr expr = createNode<Expression>(elem, elemMap, "function", "expressionPtr");

		vector<XmlElement> args = elem.getFirstChildByName("arguments")->getChildrenByName("argument");
		vector<ExpressionPtr> argVec;
		for(auto iter = args.begin(); iter != args.end(); ++iter) {
			ExpressionPtr expr = createNode<Expression>(*iter, elemMap,"expressionPtr");
			argVec.push_back(expr);
		}

		CallExprPtr call = CallExpr::get(manager, typeT, expr, argVec);
		buildAnnotations(elem, call, false);
		updateMap(elem, elemMap, call);
	}

	else if (nodeName == "variable") {
		TypePtr typeT = createNode<Type>(elem, elemMap, "type", "typePtr");

		VariablePtr var = Variable::get(manager, typeT, numeric_cast<int>(elem.getAttr("identifier")));
		buildAnnotations(elem, var, false);
		updateMap(elem, elemMap, var);
	}

	else if (nodeName == "jobExpr") {
		vector<XmlElement> decls = elem.getFirstChildByName("declarations")->getChildrenByName("declaration");
		vector<DeclarationStmtPtr> declVec;
		for(auto iter = decls.begin(); iter != decls.end(); ++iter) {
			DeclarationStmtPtr decl = createNode<DeclarationStmt>(*iter, elemMap,"declarationStmtPtr");
			declVec.push_back(decl);
		}
		vector<XmlElement> stmts = elem.getFirstChildByName("guardedStatements")->getChildrenByName("guardedStatement");
		JobExpr::GuardedStmts stmtVec;
		for(auto iter = stmts.begin(); iter != stmts.end(); ++iter) {
			vector<XmlElement> types = iter->getChildrenByName("lambdaExprPtr"); // Attention !!
			
			LambdaExprPtr guard = dynamic_pointer_cast<const LambdaExpr>(elemMap[types[0].getAttr("ref")].second);
			buildAnnotations(types[0], guard, true);

			LambdaExprPtr stmt = dynamic_pointer_cast<const LambdaExpr>(elemMap[types[1].getAttr("ref")].second);
			buildAnnotations(types[1], stmt, true);

			stmtVec.push_back(make_pair(guard, stmt));
		}

		XmlElementPtr type = elem.getFirstChildByName("defaultStatement")->getFirstChildByName("lambdaExprPtr");

		LambdaExprPtr default1 = dynamic_pointer_cast<const LambdaExpr>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, default1, true);

		JobExprPtr job = JobExpr::get(manager, default1, stmtVec, declVec);
		buildAnnotations(elem, job, false);
		updateMap(elem, elemMap, job);
	}

	else if (nodeName == "lambdaExpr") {
		TypePtr typeT = createNode<Type>(elem, elemMap, "type", "typePtr");

		vector<XmlElement> decls = elem.getFirstChildByName("captureList")->getChildrenByName("declarationStmtPtr");
		vector<DeclarationStmtPtr> declVec;
		for(auto iter = decls.begin(); iter != decls.end(); ++iter) {
			DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(elemMap[iter->getAttr("ref")].second);
			buildAnnotations(*iter, decl, true);
			declVec.push_back(decl);
		}

		vector<XmlElement> params = elem.getFirstChildByName("params")->getChildrenByName("param");
		vector<VariablePtr> parVec;
		for(auto iter = params.begin(); iter != params.end(); ++iter) {
			VariablePtr par = createNode<Variable>(*iter, elemMap,"variablePtr");
			parVec.push_back(par);
		}
		StatementPtr body = createNode<Statement>(elem, elemMap, "body", "statementPtr");

		LambdaExprPtr lExpr = LambdaExpr::get(manager, typeT, declVec, parVec, body);
		buildAnnotations(elem, lExpr, false);
		updateMap(elem, elemMap, lExpr);
	}

	else if (nodeName == "program") {
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		Program::EntryPointSet exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			ExpressionPtr expr = createNode<Expression>(*iter, elemMap,"expressionPtr");
			exprVec.insert(expr);
		}
		ProgramPtr program = Program::create(manager);
		
		program = Program::addEntryPoints(manager, program, exprVec);
		buildAnnotations(elem, program, false);
		updateMap(elem, elemMap, program);
	}

	else if (nodeName == "recLambdaDefinition") {
		vector<XmlElement> funs = elem.getFirstChildByName("definitions")->getChildrenByName("definition");
		RecLambdaDefinition::RecFunDefs funVec;
		for(auto iter = funs.begin(); iter != funs.end(); ++iter) {
			VariablePtr var = createNode<Variable>(*iter, elemMap,"variablePtr");
			LambdaExprPtr lExpr = createNode<LambdaExpr>(*iter, elemMap,"lambdaExprPtr");
			funVec.insert(std::make_pair(var, lExpr));
		}

		RecLambdaDefinitionPtr definition = RecLambdaDefinition::get(manager, funVec);
		buildAnnotations(elem, definition, false);
		updateMap(elem, elemMap, definition);
	}

	else if (nodeName == "recLambdaExpr") {
		VariablePtr var = createNode<Variable>(elem, elemMap, "variable", "variablePtr");
		RecLambdaDefinitionPtr def = createNode<RecLambdaDefinition>(elem, elemMap, "definition", "recLambdaDefinitionPtr");

		RecLambdaExprPtr recExpr = RecLambdaExpr::get(manager, var, def);
		buildAnnotations(elem, recExpr, false);
		updateMap(elem, elemMap, recExpr);
	}

	else if (nodeName == "rootNode") {
		XmlElementPtr type = elem.getFirstChildByName("nodePtr");
		buildAnnotations(*type, elemMap[type->getAttr("ref")].second, true);
	}
}

void checkRef(NodeManager& manager, const XmlElement& elem, ElemMapType& elemMap) {
	string id = elem.getAttr("ref");
	if (id.size() != 0){
		buildNode(manager, *(elemMap[id].first), elemMap);
	}

	vector<XmlElement> children = elem.getChildren();
	for(vector<XmlElement>::const_iterator iter = children.begin(); iter != children.end(); ++iter) {
		checkRef(manager, *iter, elemMap);
	}
}

} // end namespace

namespace insieme {
namespace xml {

shared_ptr<Annotation> XmlConverter::domToIrAnnotation (const XmlElement& el) const {
	string type = el.getAttr("type");
	DomToIrConvertMapType::const_iterator fit = DomToIrConvertMap.find(type);
	if(fit != DomToIrConvertMap.end()) {
		return (fit->second)(el);
	}
	DLOG(WARNING) << "Annotation \"" << type << "\" is not registred for Xml_Read!";
	return shared_ptr<Annotation>();
}

NodePtr XmlUtil::convertDomToIr(NodeManager& manager){
	ElemMapType elemMap;

	XmlElement inspire(doc->getDocumentElement(), doc);

	vector<XmlElement> elemVec = inspire.getChildren();
	for(vector<XmlElement>::const_iterator iter = elemVec.begin(); iter != elemVec.end(); ++iter) {
		elemMap[iter->getAttr("id")] = make_pair(&(*iter), NodePtr());
	}

	XmlElementPtr root = inspire.getFirstChildByName("rootNode");

	assert ( root && "RootNode is not present in DOM");

	buildNode(manager, *root, elemMap);

	return elemMap[(*root->getFirstChildByName("nodePtr")).getAttr("ref")].second;
}

} // end xml namespace
} // end insieme namespace

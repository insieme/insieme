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
typedef map<string, pair <const XmlElement*, NodePtr>> elemMapType;

void buildAnnotations(const XmlElementPtr, const NodePtr, const bool);
void buildNode(NodeManager&, const XmlElement&, elemMapType&);
void checkRef(NodeManager&, const XmlElement&, elemMapType&);

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

void buildNode(NodeManager& manager, const XmlElement& elem, elemMapType& elemMap) {
	checkRef(manager, elem, elemMap);

	// different types of nodes
	const string& nodeName = elem.getName();

	if (nodeName == "genType") {
		TypePtr baseType = NULL;
		XmlElementPtr base = elem.getFirstChildByName("baseType");
		if (base){
			XmlElementPtr type = base->getFirstChildByName("typePtr");
			baseType = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);

			buildAnnotations(*type, baseType, true);
		}

		vector<TypePtr> typeParams;
		XmlElementPtr param = elem.getFirstChildByName("typeParams");
		if (param){
			vector<XmlElement> types = param->getChildrenByName("typePtr");
			for(vector<XmlElement>::const_iterator iter = types.begin(); iter != types.end(); ++iter) {
				TypePtr typeParam = dynamic_pointer_cast<const Type>(elemMap[iter->getAttr("ref")].second);
				buildAnnotations(*iter, typeParam, true);
				typeParams.push_back(typeParam);
			}
		}

		vector<IntTypeParam> intTypeParams;
		XmlElementPtr intParam = elem.getFirstChildByName("intTypeParams");
		if (intParam){
			vector<XmlElement> intPar = intParam->getChildrenByName("intTypeParam");
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

		GenericTypePtr gen = GenericType::get(manager, elem.getAttr("familyName"), typeParams, intTypeParams, baseType);
		buildAnnotations(elem, gen, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, gen);
	}

	else if (nodeName == "functionType") {
		XmlElementPtr type = elem.getFirstChildByName("argumentType")->getFirstChildByName("tupleTypePtr");
		TupleTypePtr argType = dynamic_pointer_cast<const TupleType>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, argType, true);

		type = elem.getFirstChildByName("returnType")->getFirstChildByName("typePtr");
		TypePtr retType = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, retType, true);

		FunctionTypePtr fun = FunctionType::get(manager, argType, retType);
		buildAnnotations(elem, fun, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, fun);
	}

	else if (nodeName == "unionType" || nodeName == "structType") {
		vector<XmlElement> entries = elem.getFirstChildByName("entries")->getChildrenByName("entry");
		vector<NamedCompositeType::Entry> entryVec;
		for(auto iter = entries.begin(); iter != entries.end(); ++iter) {
			Identifier ident = iter->getFirstChildByName("id")->getText();

			XmlElementPtr type = iter->getFirstChildByName("typePtr");
			TypePtr el = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, el, true);
			entryVec.push_back(NamedCompositeType::Entry(ident, el));
		}
		if (nodeName == "structType") {
			StructTypePtr structT = StructType::get(manager, entryVec);
			buildAnnotations(elem, structT, false);

			string id = elem.getAttr("id");
			pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
			elemMap[id] = make_pair(oldPair.first, structT);
		}
		else {
			UnionTypePtr unionT = UnionType::get(manager, entryVec);
			buildAnnotations(elem, unionT, false);

			string id = elem.getAttr("id");
			pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
			elemMap[id] = make_pair(oldPair.first, unionT);
		}
	}

	else if (nodeName == "tupleType") {
		vector<XmlElement> types = elem.getFirstChildByName("elementTypeList")->getChildrenByName("elementType");
		vector<TypePtr> elementList;
		for(auto iter = types.begin(); iter != types.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("typePtr");
			TypePtr elType = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, elType, true);
			elementList.push_back(elType);
		}

		TupleTypePtr tuple = TupleType::get(manager, elementList);
		buildAnnotations(elem, tuple, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, tuple);
	}

	else if (nodeName == "typeVariable") {
		TypeVariablePtr var = TypeVariable::get(manager, elem.getAttr("name"));
		buildAnnotations(elem, var, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, var);
	}

	else if (nodeName == "recType") {
		XmlElementPtr type = elem.getFirstChildByName("definition")->getFirstChildByName("recTypeDefinitionPtr");
		RecTypeDefinitionPtr definition = dynamic_pointer_cast<const RecTypeDefinition>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, definition, true);

		type = elem.getFirstChildByName("typeVariable")->getFirstChildByName("typeVariablePtr");
		TypeVariablePtr var = dynamic_pointer_cast<const TypeVariable>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, var, true);

		RecTypePtr recType = RecType::get(manager, var, definition);
		buildAnnotations(elem, recType, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, recType);
	}

	else if (nodeName == "recTypeDefinition") {
		vector<XmlElement> defs = elem.getFirstChildByName("definitions")->getChildrenByName("definition");
		RecTypeDefinition::RecTypeDefs defVec;
		for(auto iter = defs.begin(); iter != defs.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("typeVariablePtr");
			TypeVariablePtr var = dynamic_pointer_cast<const TypeVariable>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, var, true);

			type = iter->getFirstChildByName("typePtr");
			TypePtr ty = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, ty, true);

			defVec.insert(std::make_pair(var, ty));
		}

		RecTypeDefinitionPtr rec = RecTypeDefinition::get(manager, defVec);
		buildAnnotations(elem, rec, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, rec);
	}

	else if (nodeName == "literal") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TypePtr typeT = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);

		LiteralPtr lit = Literal::get(manager, typeT, elem.getAttr("value"));
		buildAnnotations(elem, lit, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, lit);
	}

	else if (nodeName == "returnStmt") {
		XmlElementPtr type = elem.getFirstChildByName("returnExpression")->getFirstChildByName("expressionPtr");
		ExpressionPtr expression = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expression, true);

		ReturnStmtPtr ret = ReturnStmt::get(manager, expression);
		buildAnnotations(elem, ret, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, ret);
	}

	else if (nodeName == "forStmt") {
		XmlElementPtr type = elem.getFirstChildByName("declaration")->getFirstChildByName("declarationStmtPtr");
		DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, decl, true);

		type = elem.getFirstChildByName("body")->getFirstChildByName("statementPtr");
		StatementPtr body = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, body, true);

		type = elem.getFirstChildByName("end")->getFirstChildByName("expressionPtr");
		ExpressionPtr end = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, end, true);

		type = elem.getFirstChildByName("step")->getFirstChildByName("expressionPtr");
		ExpressionPtr step = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, step, true);

		ForStmtPtr fstmt = ForStmt::get(manager, decl, body, end, step);
		buildAnnotations(elem, fstmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, fstmt);
	}

	else if (nodeName == "ifStmt") {
		XmlElementPtr type = elem.getFirstChildByName("condition")->getFirstChildByName("expressionPtr");
		ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expr, true);

		type = elem.getFirstChildByName("thenBody")->getFirstChildByName("statementPtr");
		StatementPtr thenStmt = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, thenStmt, true);

		type = elem.getFirstChildByName("elseBody")->getFirstChildByName("statementPtr");
		StatementPtr elseStmt = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, elseStmt, true);

		IfStmtPtr stmt = IfStmt::get(manager, expr, thenStmt, elseStmt);
		buildAnnotations(elem, stmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, stmt);
	}

	else if (nodeName == "switchStmt") {
		XmlElementPtr type = elem.getFirstChildByName("expression")->getFirstChildByName("expressionPtr");
		ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expr, true);

		vector<XmlElement> cases = elem.getFirstChildByName("cases")->getChildrenByName("case");
		vector<SwitchStmt::Case> caseVector;
		for(auto iter = cases.begin(); iter != cases.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			type = iter->getFirstChildByName("statementPtr");
			StatementPtr stat = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, stat, true);

			caseVector.push_back(std::make_pair(expr, stat));
		}

		type = elem.getFirstChildByName("defaultCase")->getFirstChildByName("statementPtr");
		StatementPtr defaultCase = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, defaultCase, true);

		SwitchStmtPtr stmt = SwitchStmt::get(manager, expr, caseVector, defaultCase);
		buildAnnotations(elem, stmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, stmt);
	}

	else if (nodeName == "whileStmt") {
		XmlElementPtr type = elem.getFirstChildByName("condition")->getFirstChildByName("expressionPtr");
		ExpressionPtr cond = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, cond, true);

		type = elem.getFirstChildByName("body")->getFirstChildByName("statementPtr");
		StatementPtr body = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, body, true);

		WhileStmtPtr stmt = WhileStmt::get(manager, cond, body);
		buildAnnotations(elem, stmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, stmt);
	}

	else if (nodeName == "breakStmt") {
		BreakStmtPtr stmt = BreakStmt::get(manager);
		buildAnnotations(elem, stmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, stmt);
	}

	else if (nodeName == "continueStmt") {
		ContinueStmtPtr stmt = ContinueStmt::get(manager);
		buildAnnotations(elem, stmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, stmt);
	}

	else if (nodeName == "compoundStmt") {
		vector<XmlElement> stats = elem.getFirstChildByName("statements")->getChildrenByName("statement");
		vector<StatementPtr> stmtVec;
		for(auto iter = stats.begin(); iter != stats.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("statementPtr");
			const StatementPtr stat = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, stat, true);

			stmtVec.push_back(stat);
		}

		CompoundStmtPtr comp = CompoundStmt::get(manager, stmtVec);
		buildAnnotations(elem, comp, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, comp);
	}

	else if (nodeName == "declarationStmt") {
		XmlElementPtr type = elem.getFirstChildByName("variable")->getFirstChildByName("variablePtr");
		VariablePtr var = dynamic_pointer_cast<const Variable>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, var, true);

		type = elem.getFirstChildByName("expression")->getFirstChildByName("expressionPtr");
		ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expr, true);

		DeclarationStmtPtr dstmt = DeclarationStmt::get(manager, var, expr);
		buildAnnotations(elem, dstmt, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, dstmt);
	}

	else if (nodeName == "structExpr") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		StructTypePtr typeT = dynamic_pointer_cast<const StructType>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);
		
		vector<XmlElement> membs = elem.getFirstChildByName("members")->getChildrenByName("member");
		StructExpr::Members membVec;
		for(auto iter = membs.begin(); iter != membs.end(); ++iter) {
			Identifier ident(iter->getFirstChildByName("id")->getText());

			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			membVec.push_back(StructExpr::Member(ident, expr));
		}

		StructExprPtr structT = StructExpr::get(manager, typeT, membVec);
		buildAnnotations(elem, structT, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, structT);
	}
	
	else if(nodeName == "unionExpr"){
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		UnionTypePtr typeT = dynamic_pointer_cast<const UnionType>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);
		
		Identifier ident(elem.getFirstChildByName("member")->getFirstChildByName("id")->getText());
		
		type = elem.getFirstChildByName("member")->getFirstChildByName("expressionPtr");
		ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expr, true);

		UnionExprPtr unionT = UnionExpr::get(manager, typeT, ident, expr);
		buildAnnotations(elem, unionT, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, unionT);		
	}

	else if (nodeName == "vectorExpr") {
		/*XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		VectorTypePtr typeT = dynamic_pointer_cast<const VectorType>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);*/
		
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			exprVec.push_back(expr);
		}

		VectorExprPtr vecT = VectorExpr::get(manager, exprVec);
		buildAnnotations(elem, vecT, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, vecT);
	}

	else if (nodeName == "tupleExpr") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TupleTypePtr typeT = dynamic_pointer_cast<const TupleType>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);
		
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			exprVec.push_back(expr);
		}

		TupleExprPtr tuple = TupleExpr::get(manager, typeT, exprVec);
		buildAnnotations(elem, tuple, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, tuple);
	}

	else if (nodeName == "castExpr") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TypePtr typeT = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);

		type = elem.getFirstChildByName("subExpression")->getFirstChildByName("expressionPtr");
		ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expr, true);

		CastExprPtr cast = CastExpr::get(manager, typeT, expr);
		buildAnnotations(elem, cast, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, cast);
	}

	else if (nodeName == "callExpr") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TypePtr typeT = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);

		type = elem.getFirstChildByName("function")->getFirstChildByName("expressionPtr");
		ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, expr, true);

		vector<XmlElement> args = elem.getFirstChildByName("arguments")->getChildrenByName("argument");
		vector<ExpressionPtr> argVec;
		for(auto iter = args.begin(); iter != args.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			argVec.push_back(expr);
		}

		CallExprPtr call = CallExpr::get(manager, typeT, expr, argVec);
		buildAnnotations(elem, call, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, call);
	}

	else if (nodeName == "variable") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TypePtr typeT = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);

		VariablePtr var = Variable::get(manager, typeT, numeric_cast<int>(elem.getAttr("identifier")));
		buildAnnotations(elem, var, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, var);
	}

	else if (nodeName == "jobExpr") {
		vector<XmlElement> decls = elem.getFirstChildByName("declarations")->getChildrenByName("declaration");
		vector<DeclarationStmtPtr> declVec;
		for(auto iter = decls.begin(); iter != decls.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("declarationStmtPtr");
			DeclarationStmtPtr decl = dynamic_pointer_cast<const DeclarationStmt>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, decl, true);

			declVec.push_back(decl);
		}

		vector<XmlElement> stmts = elem.getFirstChildByName("guardedStatements")->getChildrenByName("guardedStatement");
		JobExpr::GuardedStmts stmtVec;
		for(auto iter = stmts.begin(); iter != stmts.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			type = iter->getFirstChildByName("statementPtr");
			StatementPtr stmt = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, stmt, true);

			stmtVec.push_back(make_pair(expr, stmt));
		}

		XmlElementPtr type = elem.getFirstChildByName("defaultStatement")->getFirstChildByName("statementPtr");
		StatementPtr default1 = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, default1, true);

		JobExprPtr job = JobExpr::get(manager, default1, stmtVec, declVec);
		buildAnnotations(elem, job, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, job);
	}

	else if (nodeName == "lambdaExpr") {
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TypePtr typeT = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);

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
			XmlElementPtr type = iter->getFirstChildByName("variablePtr");
			VariablePtr par = dynamic_pointer_cast<const Variable>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, par, true);

			parVec.push_back(par);
		}

		type = elem.getFirstChildByName("body")->getFirstChildByName("statementPtr");
		StatementPtr body = dynamic_pointer_cast<const Statement>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, body, true);

		LambdaExprPtr lExpr = LambdaExpr::get(manager, typeT, declVec, parVec, body);
		buildAnnotations(elem, lExpr, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, lExpr);
	}

	else if (nodeName == "program") {
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		Program::EntryPointSet exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			exprVec.insert(expr);
		}

		ProgramPtr program = Program::create(manager);
		program = Program::addEntryPoints(manager, program, exprVec);
		buildAnnotations(elem, program, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, program);
	}

	else if (nodeName == "recLambdaDefinition") {
		vector<XmlElement> funs = elem.getFirstChildByName("definitions")->getChildrenByName("definition");
		RecLambdaDefinition::RecFunDefs funVec;
		for(auto iter = funs.begin(); iter != funs.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("variablePtr");
			VariablePtr var = dynamic_pointer_cast<const Variable>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, var, true);

			type = iter->getFirstChildByName("lambdaExprPtr");
			LambdaExprPtr lExpr = dynamic_pointer_cast<const LambdaExpr>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, lExpr, true);

			funVec.insert(std::make_pair(var, lExpr));
		}

		RecLambdaDefinitionPtr definition = RecLambdaDefinition::get(manager, funVec);
		buildAnnotations(elem, definition, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, definition);
	}

	else if (nodeName == "recLambdaExpr") {
		XmlElementPtr type = elem.getFirstChildByName("variable")->getFirstChildByName("variablePtr");
		VariablePtr var = dynamic_pointer_cast<const Variable>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, var, true);

		type = elem.getFirstChildByName("definition")->getFirstChildByName("recLambdaDefinitionPtr");
		RecLambdaDefinitionPtr definition = dynamic_pointer_cast<const RecLambdaDefinition>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, definition, true);

		RecLambdaExprPtr recExpr = RecLambdaExpr::get(manager, var, definition);
		buildAnnotations(elem, recExpr, false);

		string id = elem.getAttr("id");
		pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
		elemMap[id] = make_pair(oldPair.first, recExpr);
	}

	else if (nodeName == "rootNode") {
		XmlElementPtr type = elem.getFirstChildByName("nodePtr");
		buildAnnotations(*type, elemMap[type->getAttr("ref")].second, true);
	}
}

void checkRef(NodeManager& manager, const XmlElement& elem, elemMapType& elemMap) {
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
	elemMapType elemMap;

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

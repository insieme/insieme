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

namespace {
// FIXME!
// ------------------------------------ XStr ----------------------------
#define toUnicode(str) XStr(str).unicodeForm()

class XStr {
	::XMLCh* fUnicodeForm;
public:
	XStr(const string& toTranscode) { fUnicodeForm = XMLString::transcode(toTranscode.c_str()); }

	~XStr() { XMLString::release(&fUnicodeForm); }

	const ::XMLCh* unicodeForm() { return fUnicodeForm; }
};

class error_handler: public DOMErrorHandler {
	bool failed_;

public:
	error_handler () : failed_ (false) {}

	bool failed () const { return failed_; }

	virtual bool handleError (const xercesc::DOMError& e){
		bool warn (e.getSeverity() == DOMError::DOM_SEVERITY_WARNING);
		if (!warn) failed_ = true;
	
		DOMLocator* loc (e.getLocation ());
	
		char* uri (XMLString::transcode (loc->getURI ()));
		char* msg (XMLString::transcode (e.getMessage ()));
	
		cerr << uri << ":" 
			<< loc->getLineNumber () << ":" << loc->getColumnNumber () << " "
			<< (warn ? "warning: " : "error: ") << msg << endl;

		XMLString::release (&uri);
		XMLString::release (&msg);
		return true;
	}
};
} // end anonymoous namespace

namespace insieme {
namespace xml {

// const ::XMLCh* toUnicode(const std::string& str) { return XStr(str).unicodeForm(); }

// ------------------------------------ XmlElement ----------------------------

XmlElement::XmlElement(DOMElement* elem) : doc(NULL), base(elem) { }
XmlElement::XmlElement(string name, DOMDocument* doc): doc(doc), base(doc->createElement(toUnicode(name))) { }
XmlElement::XmlElement(DOMElement* elem, DOMDocument* doc) : doc(doc), base(elem) { }

DOMElement* XmlElement::getBase() const {
	return base;
}

DOMDocument* XmlElement::getDoc() const {
	return doc;
}

const XmlElement& XmlElement::operator<<(const XmlElement& childNode) {
	base->appendChild(childNode.base);
	return childNode;
}

XmlElement& XmlElement::operator<<(const XmlElementPtr& childNode) {
	if (childNode) {
		base->appendChild(childNode->base);
		return *childNode;
	}
	return *this; // if XmlElement is NULL (annotation without registration) return the left element
}


// XmlElement& XmlElement::setAttr(const string& id, const string& value) {
XmlElement& XmlElement::operator<<(const XmlElement::Attribute& attr) {
	base->setAttribute(toUnicode(attr.first), toUnicode(attr.second));
	return *this;
}

XmlElement& XmlElement::setText(const string& text) {
	assert(doc != NULL && "Attempt to create text on a root node");
	DOMNode* first = base->getFirstChild();
	bool found = false;
	while(first && !found){
		if (first->getNodeType() == 3) {
			first->setTextContent(toUnicode(text));
			found = true;
		}
		first = first->getPreviousSibling();
	}
	if (!found){
		DOMText* textNode = doc->createTextNode(toUnicode(text));
		base->appendChild(textNode);
	}
	return *this;
}

string XmlElement::getAttr(const string& id) const { // return the empty string if there is not attribute
	char* ctype (XMLString::transcode (base->getAttribute(toUnicode(id))));
	string type(ctype);
	XMLString::release(&ctype);	
	return type;
}

string XmlElement::getText() const { // return the empty string if there is no text
	DOMNode* first = base->getFirstChild();
	while(first){
		if (first->getNodeType() == 3) {
			char* ctext (XMLString::transcode(first->getNodeValue()));
			string text(ctext);
			XMLString::release(&ctext);
			return text;
		}
		first = first->getPreviousSibling();
	}
	return "";
}

string XmlElement::getName() const {
	char* cname (XMLString::transcode (base->getTagName()));
	string name(cname);
	XMLString::release(&cname);	
	return name;
}

const vector<XmlElement> XmlElement::getChildrenByName(const string& name) const {
	vector<XmlElement> vec2;
	vector<XmlElement> vec = XmlElement::getChildren();
	for (auto iter = vec.begin(); iter != vec.end(); ++iter ){
		if ((*iter).getName() == name){
			vec2.push_back(*iter);
		}
	}
	return vec2;
}

const XmlElementPtr XmlElement::getFirstChildByName(const string& name) const {
	vector<XmlElement> vec = XmlElement::getChildren();
	for (auto iter = vec.begin(); iter != vec.end(); ++iter ){
		if ((*iter).getName() == name){
			return make_shared<XmlElement>(*iter);
		}
	}
	return shared_ptr<XmlElement>();
}

const vector<XmlElement> XmlElement::getChildren() const {
	vector<XmlElement> vec;
	DOMElement* first = base->getFirstElementChild();
	while(first) {
		vec.push_back(XmlElement(first, doc));
		first = first->getNextElementSibling();
	}
	return vec;
}

// ------------------------------------ XmlConverter ----------------------------

XmlConverter& XmlConverter::get(){
	static XmlConverter instance;
	return instance;
}

shared_ptr<Annotation> XmlConverter::domToIrAnnotation (const XmlElement& el) const {
	string type = el.getAttr("type");
	DomToIrConvertMapType::const_iterator fit = DomToIrConvertMap.find(type);
	if(fit != DomToIrConvertMap.end()) {
		return (fit->second)(el);
	}
	DLOG(WARNING) << "Annotation \"" << type << "\" is not registred for Xml_Read!";
	return shared_ptr<Annotation>();
}

void* XmlConverter::registerAnnotation(const string& name, const XmlConverter::IrToDomConvertType& toXml, const XmlConverter::DomToIrConvertType& fromXml) {
	IrToDomConvertMap.insert( std::make_pair(name, toXml) );
	DomToIrConvertMap.insert( std::make_pair(name, fromXml) );
	return NULL;
}

// ------------------------------------ XmlUtil ----------------------------

XmlUtil::XmlUtil(): doc(NULL), rootElem(NULL), parser(NULL) {
		XMLPlatformUtils::Initialize();
		impl = DOMImplementationRegistry::getDOMImplementation(toUnicode("Core"));
}

XmlUtil::~XmlUtil(){
	if (parser) parser->release();
	if (doc) doc->release();
	XMLPlatformUtils::Terminate();
}

void XmlUtil::convertXmlToDom(const string fileName, const bool validate){
	((DOMImplementationLS*)impl)->createLSSerializer();
	parser = ((DOMImplementationLS*)impl)->createLSParser (DOMImplementationLS::MODE_SYNCHRONOUS, 0);
	
	// remove the old DOM
	if (doc) {
		doc->release();
		doc = NULL;
	}
	
	if (parser) {
		DOMConfiguration* conf (parser->getDomConfig ());

		conf->setParameter (XMLUni::fgDOMComments, false);
		conf->setParameter (XMLUni::fgDOMDatatypeNormalization, true);
		conf->setParameter (XMLUni::fgDOMEntities, false);
		conf->setParameter (XMLUni::fgDOMNamespaces, true);
		conf->setParameter (XMLUni::fgDOMElementContentWhitespace, false);

		// Enable validation.
		conf->setParameter (XMLUni::fgDOMValidate, validate);
		conf->setParameter (XMLUni::fgXercesSchema, validate);
		conf->setParameter (XMLUni::fgXercesSchemaFullChecking, false);

		// Use the loaded grammar during parsing.
		conf->setParameter (XMLUni::fgXercesUseCachedGrammarInParse, true);

		// Don't load schemas from any other source
		conf->setParameter (XMLUni::fgXercesLoadSchema, false);

		// We will release the DOM document ourselves.
		conf->setParameter (XMLUni::fgXercesUserAdoptsDOMDocument, true);

		error_handler eh;
		parser->getDomConfig ()->setParameter (XMLUni::fgDOMErrorHandler, &eh);
		
		if (validate) {
			if (!parser->loadGrammar ((XML_SCHEMA_DIR + "schema.xsd").c_str(), Grammar::SchemaGrammarType, true)) {
				LOG(ERROR) << "ERROR: Unable to load schema.xsd";
				return;
			}
			if (eh.failed ()) {
				return;
			}
		}
		if (doc) doc->release();
		doc = parser->parseURI(fileName.c_str());
		if (eh.failed ()) {
			doc->release();
			doc = NULL;
			LOG(ERROR) << "problem during parsing of XML file" << endl;
			return;
		}
	}
}

void XmlUtil::convertDomToXml(const string outputFile){
	DOMImplementationLS* implLS = dynamic_cast<DOMImplementationLS*>(impl);
	assert(implLS);
	DOMLSSerializer*	theSerializer = implLS->createLSSerializer();
	DOMLSOutput*		theOutputDesc = implLS->createLSOutput();
	DOMConfiguration* 	serializerConfig = theSerializer->getDomConfig();
	
	if (serializerConfig->canSetParameter(XMLUni::fgDOMWRTFormatPrettyPrint, true))
		serializerConfig->setParameter(XMLUni::fgDOMWRTFormatPrettyPrint, true);
		
	XMLFormatTarget* myFormTarget = NULL;
	if (!outputFile.empty()){
		myFormTarget = new LocalFileFormatTarget(outputFile.c_str());
	} else {
		myFormTarget = new StdOutFormatTarget();
	}

	theOutputDesc->setByteStream(myFormTarget);
	theSerializer->write(doc, theOutputDesc);

	theOutputDesc->release();
	theSerializer->release();
	delete myFormTarget;
}

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
	
	else if (nodeName == "structExpr" || nodeName == "unionExpr") {
		// FIXME
		XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");

		vector<XmlElement> membs = elem.getFirstChildByName("members")->getChildrenByName("member");
		StructExpr::Members membVec;
		for(auto iter = membs.begin(); iter != membs.end(); ++iter) {
			Identifier ident(iter->getFirstChildByName("id")->getText());

			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			membVec.push_back(StructExpr::Member(ident, expr));
		}
		
		if (nodeName == "structExpr") {
			StructExprPtr structT = StructExpr::get(manager, membVec);
			buildAnnotations(elem, structT, false);

			string id = elem.getAttr("id");
			pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
			elemMap[id] = make_pair(oldPair.first, structT);
		}
		else {
			// FIXME
			UnionTypePtr typeT = dynamic_pointer_cast<const UnionType>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, typeT, true);
			assert(typeT);

			UnionExprPtr unionT = UnionExpr::get(manager, typeT, Identifier("FxME"), ExpressionPtr()); // FIXME
			buildAnnotations(elem, unionT, false);

			string id = elem.getAttr("id");
			pair <const XmlElement*, NodePtr> oldPair = elemMap[id];
			elemMap[id] = make_pair(oldPair.first, unionT);
		}
	}

	else if (nodeName == "vectorExpr") {
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
		vector<XmlElement> exprs = elem.getFirstChildByName("expressions")->getChildrenByName("expression");
		vector<ExpressionPtr> exprVec;
		for(auto iter = exprs.begin(); iter != exprs.end(); ++iter) {
			XmlElementPtr type = iter->getFirstChildByName("expressionPtr");
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(elemMap[type->getAttr("ref")].second);
			buildAnnotations(*type, expr, true);

			exprVec.push_back(expr);
		}
		
		TupleExprPtr tuple = TupleExpr::get(manager, exprVec);
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
		/*XmlElementPtr type = elem.getFirstChildByName("type")->getFirstChildByName("typePtr");
		TypePtr typeT = dynamic_pointer_cast<const Type>(elemMap[type->getAttr("ref")].second);
		buildAnnotations(*type, typeT, true);*/
		
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

string XmlUtil::convertDomToString(){
	if (doc){
		DOMImplementationLS* implLS = dynamic_cast<DOMImplementationLS*>(impl);
		DOMLSSerializer*	theSerializer = implLS->createLSSerializer();
		string stringTemp = XMLString::transcode (theSerializer->writeToString(doc));
		theSerializer->release();
		
		string stringDump = "";
		for (string::iterator it = stringTemp.begin() ; it < stringTemp.end(); ++it){
	    	if (!isspace (*it))
	      		stringDump += *it;
		}
		return stringDump;
	}
	throw "DOM is empty";
}



// -------------------------Xml Write - Read - Validate----------------------

void xmlWrite(const NodePtr& node, const string fileName) {
	XmlUtil xml;
	xml.convertIrToDom(node);
	xml.convertDomToXml(fileName);
};

void xmlRead(NodeManager& manager, const string fileName) {
	XmlUtil xml;
	xml.convertXmlToDom(fileName, true);
	xml.convertDomToIr(manager);
};

void xmlValidate(const string fileName) {
	XmlUtil xml;
	xml.convertXmlToDom(fileName, true);
};

} // end xml namespace
} // end insieme namespace

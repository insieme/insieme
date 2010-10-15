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
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>
#include <xercesc/framework/LocalFileFormatTarget.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLUni.hpp>

#include "xml_utils.h"
#include "xsd_config.h"

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::xml;
using namespace std;

XERCES_CPP_NAMESPACE_USE
	
// ------------------------------------ XmlVisitor ----------------------------	

class XmlVisitor : public ASTVisitor<void> {
	DOMDocument* doc;
	XmlElement rootElem;

public:
	XmlVisitor(DOMDocument* udoc): doc(udoc), rootElem(doc->getDocumentElement()) { }
	
	~XmlVisitor() { }
	
	void visitAnnotations(const AnnotationMap& map, XmlElement& node){
		if (!map.empty()){
			XmlElement annotations("annotations", doc);
			node << annotations;

			XmlConverter& xmlConverter = XmlConverter::get();
			for(AnnotationMap::const_iterator iter = map.begin(); iter != map.end(); ++iter) {
				annotations << xmlConverter.irToDomAnnotation (*(iter->second), doc);
			}
		}
	}

	void visitGenericType(const GenericTypePtr& cur) {
		XmlElement genType("genType", doc);
		genType.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		genType.setAttr("familyName", cur->getFamilyName().getName());
		rootElem << genType;

		if (const TypePtr& base = cur->getBaseType()) {
			XmlElement baseType("baseType", doc);
			genType << baseType;

			XmlElement typePtr("typePtr", doc);
			typePtr.setAttr("ref", numeric_cast<string>((size_t)(&*base)));		
			baseType << typePtr;
			
			visitAnnotations(base.getAnnotations(), typePtr);
		}

		const vector<TypePtr>& param = cur->getTypeParameter();
		if (!param.empty()){
			XmlElement typeParams("typeParams", doc);
			genType << typeParams;
			for(vector<TypePtr>::const_iterator iter = param.begin(); iter != param.end(); ++iter) {
				XmlElement typePtr("typePtr", doc);
				typePtr.setAttr("ref", numeric_cast<string>((size_t)&*(*iter)));			
				typeParams << typePtr;
				
				visitAnnotations((*iter).getAnnotations(), typePtr);
			}
		}

		const vector<IntTypeParam>& intParam = cur->getIntTypeParameter();
		if (!intParam.empty()){
			XmlElement intTypeParams("intTypeParams", doc);
			genType << intTypeParams;

			const vector<IntTypeParam>& intParam = cur->getIntTypeParameter();
			for(vector<IntTypeParam>::const_iterator iter = intParam.begin(); iter != intParam.end(); ++iter) {

				XmlElement intTypeParam("intTypeParam", doc);
				intTypeParams << intTypeParam;
				switch (iter->getType()) {
				case IntTypeParam::VARIABLE:
					intTypeParam.setAttr("type", "variable");
					intTypeParam.setAttr("value", numeric_cast<string>(iter->getSymbol()));
					break;
				case IntTypeParam::CONCRETE:
					intTypeParam.setAttr("type", "concrete");
					intTypeParam.setAttr("value", numeric_cast<string>(iter->getSymbol()));
					break;
				case IntTypeParam::INFINITE:
					intTypeParam.setAttr("type", "infinite");
					break;
				default:
					intTypeParam.setAttr("type", "Invalid Parameter");
					break;
				}
			}	
		}
		
		visitAnnotations(cur->getAnnotations(), genType);
	}
	
	void visitFunctionType(const FunctionTypePtr& cur){
		XmlElement functionType("functionType", doc);
		functionType.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		rootElem << functionType;
		
		if (const TypePtr& argument = cur->getArgumentType()) {
			XmlElement argumentType("argumentType", doc);
			functionType << argumentType;

			XmlElement typePtr("typePtr", doc);
			typePtr.setAttr("ref", numeric_cast<string>((size_t)(&*argument)));			
			argumentType << typePtr;

			visitAnnotations(argument.getAnnotations(), typePtr);
		}
		
		if (const TypePtr& returnT = cur->getReturnType()) {
			XmlElement returnType("returnType", doc);
			functionType << returnType;

			XmlElement typePtr("typePtr", doc);
			typePtr.setAttr("ref", numeric_cast<string>((size_t)(&*returnT)));			
			returnType << typePtr;

			visitAnnotations(returnT.getAnnotations(), typePtr);
		}
		
		visitAnnotations(cur->getAnnotations(), functionType);
	}
	
	void visitStructType(const StructTypePtr& cur) {
		XmlElement structType("structType",doc);
		structType.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		rootElem << structType;
		XmlElement entries("entries", doc);
		structType << entries;
		
		const vector<NamedCompositeType::Entry>& entriesVec = cur->getEntries ();
		for(vector<NamedCompositeType::Entry>::const_iterator iter = entriesVec.begin(); iter != entriesVec.end(); ++iter) {
			XmlElement entry("entry", doc);
			entries << entry;
			
			XmlElement id("id", doc);			
			id.setText((iter->first).getName());
			entry << id;
			
			XmlElement typePtr("typePtr", doc);
			typePtr.setAttr("ref", numeric_cast<string>((size_t)(&*iter->second)));			
			entry << typePtr;
			
			visitAnnotations((*iter->second).getAnnotations(), typePtr);
		}
		
		visitAnnotations(cur->getAnnotations(), structType);
	}
	
	void visitUnionType(const UnionTypePtr& cur) {
		XmlElement unionType("unionType", doc);
		unionType.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		rootElem << unionType;
		
		XmlElement entries("entries",doc);
		unionType << entries;
		
		const vector<NamedCompositeType::Entry>& entriesVec = cur->getEntries ();
		for(vector<NamedCompositeType::Entry>::const_iterator iter = entriesVec.begin(); iter != entriesVec.end(); ++iter) {
			XmlElement entry("entry", doc);
			entries << entry;
			
			XmlElement id("id", doc);
			id.setText((iter->first).getName());		
			entry << id;

			XmlElement typePtr("typePtr", doc);
			typePtr.setAttr("ref", numeric_cast<string>((size_t)(&*iter->second)));			
			entry << typePtr;
			
			visitAnnotations((*iter->second).getAnnotations(), typePtr);
		}
		
		visitAnnotations(cur->getAnnotations(), unionType);
	}

	void visitTupleType(const TupleTypePtr& cur) {
		XmlElement tupleType("tupleType", doc);
		tupleType.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		rootElem << tupleType;

		XmlElement elementTypeList("elementTypeList",doc);
		tupleType << elementTypeList;

		const vector<TypePtr>& elementList = cur->getElementTypes();
		for(vector<TypePtr>::const_iterator iter = elementList.begin(); iter != elementList.end(); ++iter) {
			XmlElement elementType("elementType", doc);
			elementTypeList << elementType;
			
			XmlElement typePtr("typePtr", doc);
			typePtr.setAttr("ref", numeric_cast<string>((size_t)&*(*iter)));			
			elementType << typePtr;
			
			// annotations
		}
		
		// annotations
	}

	void visitTypeVariable(const TypeVariablePtr& cur) {
		XmlElement typeVariable("typeVariable", doc);
		typeVariable.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		typeVariable.setAttr("name", cur->getVarName());
		rootElem << typeVariable;
		
		// annotations
	}
	
	void visitRecType(const RecTypePtr& cur) {
		XmlElement recType("recType", doc);
		recType.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		rootElem << recType;
		
		if (const RecTypeDefinitionPtr& definitionT = cur->getDefinition()) {
			XmlElement definition("definition", doc);
			recType << definition;

			XmlElement recTypeDefinitionPtr("recTypeDefinitionPtr", doc);
			recTypeDefinitionPtr.setAttr("ref", numeric_cast<string>((size_t)(&*definitionT)));			
			definition << recTypeDefinitionPtr;

			// annotation
		}
		
		// annotations
	}
	
	void visitRecTypeDefinition(const RecTypeDefinitionPtr& cur) {
		XmlElement recTypeDefinition("recTypeDefinition", doc);
		recTypeDefinition.setAttr("id", numeric_cast<string>((size_t)(&*cur)));
		rootElem << recTypeDefinition;
		
		const RecTypeDefinition::RecTypeDefs& defs = cur->getDefinitions();
		if (!defs.empty()){
			XmlElement definitions("definitions", doc);
			recTypeDefinition << definitions;
			
			for(RecTypeDefinition::RecTypeDefs::const_iterator iter = defs.begin(); iter != defs.end(); ++iter) {
				XmlElement definition("definition", doc);
				definitions << definition;
				
				XmlElement typeVariablePtr("typeVariablePtr", doc);
				typeVariablePtr.setAttr("ref", numeric_cast<string>((size_t)&(*iter->first)));
				definition << typeVariablePtr;
				
				XmlElement typePtr("typePtr", doc);
				typePtr.setAttr("ref", numeric_cast<string>((size_t)&(*iter->second)));
				definition << typePtr;
				
				// annotations
			}
		}
		
		// annotations
	}

	void visitArrayType(const ArrayTypePtr& cur) {
	
	}

	void visitRefType(const RefTypePtr& cur) {
	
	}
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



// ------------------------------------ XmlElement ----------------------------

XmlElement::XmlElement(DOMElement* elem) : doc(NULL), base(elem) { }
XmlElement::XmlElement(string name, DOMDocument* doc): doc(doc), base(doc->createElement(toUnicode(name))) { }

DOMElement* XmlElement::getBase() {
	return base;
}

DOMDocument* XmlElement::getDoc(){
	return doc;
}

XmlElement& XmlElement::operator<<(XmlElement& childNode) {
	base->appendChild(childNode.base);
	return childNode;
}

XmlElement& XmlElement::setAttr(const string& id, const string& value) {
	base->setAttribute(toUnicode(id), toUnicode(value));
	return *this;
}

string XmlElement::getAttr(const string& id) const {
	char* ctype (XMLString::transcode (base->getAttribute(toUnicode(id))));
	string type(ctype);
	XMLString::release(&ctype);	
	return type;
}

XmlElement& XmlElement::setText(const string& text) {
	assert(doc != NULL && "Attempt to create text on a root node");
	DOMText* textNode = doc->createTextNode(toUnicode(text));
	base->appendChild(textNode);
	return *this;
}

// ------------------------------------ XStr ----------------------------

XStr::XStr(const string& toTranscode) { fUnicodeForm = XMLString::transcode(toTranscode.c_str()); }

XStr::~XStr() { XMLString::release(&fUnicodeForm); }

const XMLCh* XStr::unicodeForm() { return fUnicodeForm; }


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
	} else {
		assert(false && "Type not found");
	}
}

XmlElement& XmlConverter::irToDomAnnotation (const Annotation& ann, DOMDocument* doc) const {
	const string& type = ann.getAnnotationName();
	IrToDomConvertMapType::const_iterator fit = IrToDomConvertMap.find(type);
	if(fit != IrToDomConvertMap.end()) {
		return (fit->second)(ann, doc);
	} else {
		assert(false && "Type not found");
	}
}

void* XmlConverter::registerAnnotation(string name, 
		function<XmlElement& (const Annotation&, xercesc::DOMDocument*)> toXml, 
		function<shared_ptr<Annotation> (const XmlElement&)> fromXml) {
	IrToDomConvertMap[name] = toXml;
	DomToIrConvertMap[name] = fromXml;
	return NULL;
}

// ------------------------------------ XmlUtil ----------------------------

XmlUtil::XmlUtil(){
	try {
		XMLPlatformUtils::Initialize();
		impl = DOMImplementationRegistry::getDOMImplementation(toUnicode("Core"));
	}
	catch(const XMLException& toCatch)
	{
		char* pMsg = XMLString::transcode(toCatch.getMessage());
		XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n" << "  Exception message:" << pMsg;
		XMLString::release(&pMsg);
	}
	doc = NULL;
	rootElem = NULL;
	parser = NULL;
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
				cerr << "ERROR: Unable to load schema.xsd" << endl;
				return;
			}

			if (eh.failed ()){
				return;
			}
		}
		if (doc) doc->release();
		doc = parser->parseURI(fileName.c_str());
		if (eh.failed ()){
			doc->release();
			doc = NULL;
			cerr << "ERROR: found problem during parsing" << endl;
			return;
		}
	}
}

void XmlUtil::convertDomToXml(const string outputFile){
	DOMLSSerializer   *theSerializer = ((DOMImplementationLS*)impl)->createLSSerializer();
	DOMLSOutput       *theOutputDesc = ((DOMImplementationLS*)impl)->createLSOutput();
	DOMConfiguration* serializerConfig = theSerializer->getDomConfig();
	
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

void XmlUtil::convertDomToIr(const SharedNodeManager& manager){
	
}

void XmlUtil::convertIrToDom(const NodePtr& node){
	if (doc) {
		doc->release();
		doc = NULL;
	}
	doc = impl->createDocument(0, toUnicode("inspire"),0);
	XmlElement rootNode("rootNode", doc);
	(doc->getDocumentElement())->appendChild(rootNode.getBase());
	
	XmlElement nodePtr ("nodePtr", doc);
	nodePtr.setAttr("ref", numeric_cast<string>((size_t)(&*node)));
	rootNode << nodePtr;

	AnnotationMap map = node.getAnnotations();
	if (!map.empty()){
		XmlElement annotations("annotations", doc);
		nodePtr << annotations;
		
		XmlConverter& xmlConverter = XmlConverter::get();
		for(AnnotationMap::const_iterator iter = map.begin(); iter != map.end(); ++iter) {
			annotations << xmlConverter.irToDomAnnotation (*(iter->second), doc);
		}
	}
	
	XmlVisitor visitor(doc);
	visitAllOnce(node, visitor);
}

string XmlUtil::convertDomToString(){
	if (doc){
		DOMLSSerializer   *theSerializer = ((DOMImplementationLS*)impl)->createLSSerializer();
		string stringTemp = XMLString::transcode (theSerializer->writeToString(doc));
		theSerializer->release();
		
		string stringDump = "";
		for (string::iterator it = stringTemp.begin() ; it < stringTemp.end(); ++it){
	    	if (!isspace (*it))
	      		stringDump += *it;
		}
		return stringDump;
	}
	else {
		return "DOM is empty";
	}
}



// -------------------------Xml Write - Read - Validate----------------------

void insieme::xml::xmlWrite(const NodePtr& node, const string fileName) {
	XmlUtil xml;
	xml.convertIrToDom(node);
	xml.convertDomToXml(fileName);
};

void insieme::xml::xmlRead(const SharedNodeManager& manager, const string fileName, const bool validate) {
	XmlUtil xml;
	xml.convertXmlToDom(fileName, validate);
	xml.convertDomToIr(manager);
};

void insieme::xml::xmlValidate(const string fileName) {
	XmlUtil xml;
	xml.convertXmlToDom(fileName, true);
};

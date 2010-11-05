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

vector<XmlElement> XmlElement::getChildrenByName(const string& name) const {
	vector<XmlElement> vec2;
	vector<XmlElement>&& vec = XmlElement::getChildren();
	for (auto iter = vec.begin(); iter != vec.end(); ++iter ){
		if ((*iter).getName() == name){
			vec2.push_back(*iter);
		}
	}
	return vec2;
}

XmlElementPtr XmlElement::getFirstChildByName(const string& name) const {
	vector<XmlElement>&& vec = XmlElement::getChildren();
	for (auto iter = vec.begin(); iter != vec.end(); ++iter ){
		if ((*iter).getName() == name){
			return make_shared<XmlElement>(*iter);
		}
	}
	return shared_ptr<XmlElement>();
}

vector<XmlElement> XmlElement::getChildren() const {
	vector<XmlElement> vec;
	DOMElement* curr = base->getFirstElementChild();
	while(curr) {
		if(!(curr->getNodeType() == DOMNode::TEXT_NODE || curr->getNodeType() == DOMNode::COMMENT_NODE)) //skip text nodes and comments
			vec.push_back(XmlElement(curr, doc));
		curr = curr->getNextElementSibling();
	}
	return vec;
}

// ------------------------------------ XmlConverter ----------------------------

XmlConverter& XmlConverter::get() {
	static XmlConverter instance;
	return instance;
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

XmlUtil::~XmlUtil() {
	if (parser) parser->release();
	if (doc) doc->release();
	XMLPlatformUtils::Terminate();
}

void XmlUtil::convertXmlToDom(const string fileName, const bool validate) {
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

void XmlUtil::convertDomToXml(const string outputFile) {
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

string XmlUtil::convertDomToString() {
	if (doc){
		DOMImplementationLS* implLS = dynamic_cast<DOMImplementationLS*>(impl);
		DOMLSSerializer*	theSerializer = implLS->createLSSerializer();
		string stringTemp = XMLString::transcode (theSerializer->writeToString(doc));
		theSerializer->release();
		
		string stringDump = "";
		for (string::iterator it = stringTemp.begin() ; it < stringTemp.end(); ++it) {
	    	if (!isspace (*it))
	      		stringDump += *it;
		}
		return stringDump;
	}
	throw "DOM is empty"; // FIXME add an exception type for this
}

// -------------------------Xml Write - Read - Validate----------------------

void xmlWrite(const NodePtr& node, const string fileName) {
	XmlUtil xml;
	xml.convertIrToDom(node);
	xml.convertDomToXml(fileName);
};

NodePtr xmlRead(NodeManager& manager, const string fileName) {
	XmlUtil xml;
	xml.convertXmlToDom(fileName, true);
	return xml.convertDomToIr(manager);
};

void xmlValidate(const string fileName) {
	XmlUtil xml;
	xml.convertXmlToDom(fileName, true);
};

} // end xml namespace
} // end insieme namespace

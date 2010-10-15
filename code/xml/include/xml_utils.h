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

#include "ast_visitor.h"
#include <xercesc/util/XercesDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN
class DOMElement;
class DOMImplementation;
class DOMDocument;
class DOMLSParser;
XERCES_CPP_NAMESPACE_END

using namespace insieme::core;
using namespace std;

namespace insieme {
namespace xml{

// ------------------------------------ XmlUtil ----------------------------

class XmlUtil {
public:
	xercesc::DOMImplementation* impl;
	xercesc::DOMDocument* doc;
	xercesc::DOMElement* rootElem;
	xercesc::DOMLSParser* parser;

public:
	XmlUtil();
	
	~XmlUtil();
	
	void convertXmlToDom(const string fileName, const bool validate);
	
	void convertDomToXml(const string fileName);
	
	void convertDomToIr(const SharedNodeManager& manager);
	
	void convertIrToDom(const NodePtr& node);
	
	string convertDomToString();
	
};


// ------------------------------------ XmlElement ----------------------------

class XmlElement {
	xercesc::DOMDocument* doc;
	xercesc::DOMElement* base;
	
public:
	XmlElement(xercesc::DOMElement* elem);
	XmlElement(string name, xercesc::DOMDocument* doc);
	XmlElement(xercesc::DOMElement* base, xercesc::DOMDocument* doc);
	
	xercesc::DOMElement* getBase();
	
	xercesc::DOMDocument* getDoc();
	
	XmlElement& operator<<(XmlElement& childNode);
	
	const vector<XmlElement> operator[](const string&) const;
	
	XmlElement& setAttr(const string& id, const string& value);

	XmlElement& setText(const string& text);
	
	string getAttr(const string& id) const;
	
	string getText() const;
	
	string getName() const;
	
	const vector<XmlElement> getChildren() const;
};



// ------------------------------------ XStr ----------------------------

#define toUnicode(str) XStr(str).unicodeForm()

class XStr {
	XMLCh* fUnicodeForm;
public:
	XStr(const string& toTranscode);

	~XStr();

	const XMLCh* unicodeForm();
};


// ------------------------------------ XmlConverter ----------------------------

class XmlConverter: public boost::noncopyable {
	XmlConverter() { }
public:
	typedef map<const string, function<XmlElement& (const Annotation&, xercesc::DOMDocument*)>> IrToDomConvertMapType;
	typedef map<const string, function<shared_ptr<Annotation> (const XmlElement&)>> DomToIrConvertMapType;

	static XmlConverter& get();
	
	shared_ptr<Annotation> domToIrAnnotation (const XmlElement& el) const;
	
	XmlElement& irToDomAnnotation (const Annotation& ann, xercesc::DOMDocument* doc) const;
	
	void* registerAnnotation(string name, 
							function<XmlElement& (const Annotation&, xercesc::DOMDocument*)> toXml, 
							function<shared_ptr<Annotation> (const XmlElement&)> fromXml);
private:
	IrToDomConvertMapType IrToDomConvertMap;
	DomToIrConvertMapType DomToIrConvertMap;
};


#define XML_CONVERTER(className_, toXML_, fromXML_) \
	insieme::xml::XmlElement& convert ## className_ ## ToXML(const Annotation& ann, xercesc::DOMDocument* doc) { \
	const className_& annotation = dynamic_cast<const className_&>(ann); \
	insieme::xml::XmlElement* node = new insieme::xml::XmlElement("annotation", doc); \
	node->setAttr("type", #className_); \
	toXML_(annotation, *node); \
	return *node; } \
	shared_ptr<Annotation> convert ## className_ ## FromXML(const XmlElement& node) { \
	return fromXML_(node); } \
	void* hack ## className_ ## hack = insieme::xml::XmlConverter::get().registerAnnotation(#className_, \
					& convert ## className_ ## ToXML, & convert ## className_ ## FromXML);


// -------------------------Xml Write - Read - Validate----------------------

void xmlWrite(const NodePtr& root, const std::string fileName = std::string());

void xmlRead(const SharedNodeManager& manager, const std::string fileName, const bool validate);

void xmlValidate(const std::string fileName);

} // end namespace xml
} // end namespace insieme

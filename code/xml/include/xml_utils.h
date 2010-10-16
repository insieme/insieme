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
// #include <xercesc/util/XercesDefs.hpp>

namespace xercesc_3_1 {
class DOMElement;
class DOMImplementation;
class DOMDocument;
class DOMLSParser;
}

using namespace insieme::core;

namespace insieme {
namespace xml{

using namespace xercesc_3_1;

// ------------------------------------ XmlUtil ----------------------------

class XmlUtil {
public:
	DOMImplementation* impl;
	DOMDocument* doc;
	DOMElement* rootElem;
	DOMLSParser* parser;

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

	DOMDocument* doc;
	DOMElement* base;
	
public:
	XmlElement(DOMElement* elem);
	XmlElement(std::string name, DOMDocument* doc);
	XmlElement(DOMElement* base, DOMDocument* doc);
	
	DOMElement* getBase();
	
	DOMDocument* getDoc();
	
	XmlElement& operator<<(XmlElement& childNode);
	
	XmlElement& operator<<(std::shared_ptr<XmlElement> childNode);
	
	const vector<XmlElement> operator[](const std::string&) const;
	
	XmlElement& setAttr(const std::string& id, const std::string& value);

	XmlElement& setText(const std::string& text);
	
	std::string getAttr(const std::string& id) const;
	
	std::string getText() const;
	
	std::string getName() const;
	
	const vector<XmlElement> getChildren() const;
};


// ------------------------------------ XmlConverter ----------------------------

class XmlConverter: public boost::noncopyable {
	XmlConverter() { }
public:
	typedef std::function<std::shared_ptr<XmlElement> (const Annotation&, DOMDocument*)> IrToDomConvertType;
	typedef map<const std::string, IrToDomConvertType> IrToDomConvertMapType;

	typedef std::function<std::shared_ptr<Annotation> (const XmlElement&)> DomToIrConvertType;
	typedef map<const std::string, DomToIrConvertType> DomToIrConvertMapType;

	static XmlConverter& get();
	
	std::shared_ptr<Annotation> domToIrAnnotation (const XmlElement& el) const;
	
	std::shared_ptr<XmlElement> irToDomAnnotation (const Annotation& ann, DOMDocument* doc) const;
	
	void* registerAnnotation(std::string name, const IrToDomConvertType& toXml, const DomToIrConvertType& fromXml);
private:
	IrToDomConvertMapType IrToDomConvertMap;
	DomToIrConvertMapType DomToIrConvertMap;
};


#define XML_CONVERTER(className_, toXML_, fromXML_) \
	shared_ptr<XmlElement> convert ## className_ ## ToXML(const Annotation& ann, xercesc::DOMDocument* doc) { \
	const className_& annotation = dynamic_cast<const className_&>(ann); \
	shared_ptr<XmlElement> node (new insieme::xml::XmlElement("annotation", doc)); \
	node->setAttr("type", #className_); \
	toXML_(annotation, *node); \
	return node; } \
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

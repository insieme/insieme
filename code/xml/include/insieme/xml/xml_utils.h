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

#include "insieme/core/ast_visitor.h"

namespace xercesc_3_1 {
class DOMElement;
class DOMImplementation;
class DOMDocument;
class DOMLSParser;
}

using namespace insieme::core;

namespace insieme {
namespace xml {

using namespace xercesc_3_1;

// const uint16_t* toUnicode(const std::string& str);

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

	NodePtr convertDomToIr(NodeManager& manager);

	void convertIrToDom(const NodePtr& node);

	string convertDomToString();

};

// ------------------------------------ XmlElement ----------------------------

class XmlElement {

	DOMDocument* doc;
	DOMElement* base;
	
public:

	typedef std::pair<std::string, std::string> Attribute;

	XmlElement(DOMElement* elem);
	XmlElement(std::string name, DOMDocument* doc);
	XmlElement(DOMElement* base, DOMDocument* doc);
	
	DOMElement* getBase() const;
	
	DOMDocument* getDoc() const;
	
	const XmlElement& operator<<(const XmlElement& childNode);
	
	XmlElement& operator<<(const std::shared_ptr<XmlElement>& childNode);
	
	vector<XmlElement> operator[](const std::string&) const;
	
	XmlElement& operator<<(const Attribute& attr);

	XmlElement& setText(const std::string& text);
	
	std::string getAttr(const std::string& id) const;
	
	std::string getText() const;
	
	std::string getName() const;
	
	vector<XmlElement> getChildren() const;
	
	vector<XmlElement> getChildrenByName(const string& name) const;
	
	std::shared_ptr<XmlElement> getFirstChildByName(const string& name) const;
};

typedef std::shared_ptr<XmlElement> XmlElementPtr;

// ------------------------------------ XmlConverter ----------------------------

class XmlConverter: public boost::noncopyable {
	XmlConverter() { }
public:
	typedef std::function<XmlElementPtr (const Annotation&, xercesc_3_1::DOMDocument*)> IrToDomConvertType;
	typedef map<const std::string, IrToDomConvertType> IrToDomConvertMapType;

	typedef std::function<std::shared_ptr<Annotation> (const XmlElement&)> DomToIrConvertType;
	typedef map<const std::string, DomToIrConvertType> DomToIrConvertMapType;

	static XmlConverter& get();
	
	AnnotationPtr domToIrAnnotation (const XmlElement& el) const;
	XmlElementPtr irToDomAnnotation (const Annotation& ann, xercesc_3_1::DOMDocument* doc) const;
	
	void* registerAnnotation(const std::string& name, const IrToDomConvertType& toXml, const DomToIrConvertType& fromXml);
private:
	IrToDomConvertMapType IrToDomConvertMap;
	DomToIrConvertMapType DomToIrConvertMap;
};

template <class AnnotationTy>
XmlElementPtr convertToXML(const std::string& mapName, std::function<XmlElement& (const AnnotationTy&, XmlElement&)> toXml,
		const Annotation& ann, xercesc_3_1::DOMDocument* doc)
{
	insieme::xml::XmlElementPtr node( new XmlElement("annotation", doc) );
	*node << XmlElement::Attribute("type", mapName);
	toXml(dynamic_cast<const AnnotationTy&>(ann), *node);
	return node;
}

template <class AnnotationTy>
AnnotationPtr convertFromXML(std::function<std::shared_ptr<AnnotationTy> (const XmlElement&)> fromXml, const XmlElement& node) {
	return fromXml(node);
}

#define XML_CONVERTER(CLASS_NAME, KEY, TO_XML, FROM_XML)	\
	void* hack ## CLASS_NAME ## hack = \
		insieme::xml::XmlConverter::get().registerAnnotation(KEY, \
			std::bind(insieme::xml::convertToXML<CLASS_NAME>, KEY, TO_XML, std::placeholders::_1, std::placeholders::_2), \
			std::bind(insieme::xml::convertFromXML<CLASS_NAME>, FROM_XML, std::placeholders::_1) );


// -------------------------Xml Write - Read - Validate----------------------

void xmlWrite(const NodePtr& root, const std::string fileName = std::string());

NodePtr xmlRead(NodeManager& manager, const std::string fileName);

void xmlValidate(const std::string fileName);

} // end namespace xml
} // end namespace insieme

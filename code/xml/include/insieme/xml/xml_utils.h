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

#include <insieme/utils/annotation.h>
#include <insieme/core/ir_node.h>

#include <map>

namespace xercesc_3_1 {
class DOMElement;
class DOMImplementation;
class DOMDocument;
class DOMLSParser;
} // end xercesc_3_1 namespace

namespace insieme {
namespace xml {

class XStr {
	uint16_t* fUnicodeForm;
public:
	XStr(const std::string&);
	~XStr();
	const uint16_t* unicodeForm();
};

#define toUnicode(str) insieme::xml::XStr(str).unicodeForm()

// ------------------------------------ XmlUtil ----------------------------
class XmlUtil {
public:
	xercesc_3_1::DOMImplementation* impl;
	xercesc_3_1::DOMDocument* doc;
	xercesc_3_1::DOMElement* rootElem;
	xercesc_3_1::DOMLSParser* parser;

	XmlUtil();
public:

	void convertXmlToDom(const std::string& fileName, const bool validate);
	void convertDomToXml(const std::string& fileName);

	insieme::core::NodePtr convertDomToIr(insieme::core::NodeManager& manager);
	void convertIrToDom(const insieme::core::NodePtr& node);

	void convertStringToDom(const std::string& stringName, const bool validate);	
	string convertDomToString();

	static void write(const insieme::core::NodePtr& node, const std::string& fileName) {
		XmlUtil xml;
		xml.convertIrToDom(node);
		xml.convertDomToXml(fileName);
	}

	static insieme::core::NodePtr read(insieme::core::NodeManager& manager, const std::string& fileName) {
		XmlUtil xml;
		xml.convertXmlToDom(fileName, true);
		return xml.convertDomToIr(manager);
	}

	static void validate(const std::string& fileName) {
		XmlUtil xml;
		xml.convertXmlToDom(fileName, true);
	}

	~XmlUtil();
};

// ------------------------------------ XmlElement ----------------------------

class XmlElement;
typedef std::shared_ptr<XmlElement> XmlElementPtr;
typedef vector<XmlElement> XmlElementList;

class XmlElement {
	xercesc_3_1::DOMDocument* doc;
	xercesc_3_1::DOMElement* base;
public:
	typedef std::pair<std::string, std::string> Attribute;

	XmlElement(xercesc_3_1::DOMElement* elem);
	XmlElement(const std::string& name, xercesc_3_1::DOMDocument* doc);
	XmlElement(xercesc_3_1::DOMElement* base, xercesc_3_1::DOMDocument* doc);
	
	xercesc_3_1::DOMElement* getBase() const;
	xercesc_3_1::DOMDocument* getDoc() const;
	
	const XmlElement& operator<<(const XmlElement& childNode);
	XmlElement& operator<<(const XmlElementPtr& childNode);
	XmlElement& operator<<(const Attribute& attr);
	
	XmlElementList operator[](const std::string&) const;

	void setText(const std::string& text);
	std::string getText() const;
	
	std::string getAttr(const std::string& id) const;

	std::string getName() const;
	
	XmlElementList getChildren() const;
	XmlElementList getChildrenByName(const string& name) const;
	XmlElementPtr getFirstChildByName(const string& name) const;
};

// ------------------------------------ XmlConverter ----------------------------

class XmlConverter: public boost::noncopyable {
	XmlConverter() { }
public:
	typedef std::function<XmlElementPtr (const insieme::utils::Annotation&, xercesc_3_1::DOMDocument*)> IrToDomConvertType;
	typedef std::map<const std::string, IrToDomConvertType> IrToDomConvertMapType;

	typedef std::function<insieme::core::NodeAnnotationPtr (const XmlElement&)> DomToIrConvertType;
	typedef std::map<const std::string, DomToIrConvertType> DomToIrConvertMapType;

	static XmlConverter& get();
	
	insieme::core::NodeAnnotationPtr domToIrAnnotation (const XmlElement& el) const;
	XmlElementPtr irToDomAnnotation (const insieme::utils::Annotation& ann, xercesc_3_1::DOMDocument* doc) const;
	
	void* registerAnnotation(const std::string& name, const IrToDomConvertType& toXml, const DomToIrConvertType& fromXml);
private:
	IrToDomConvertMapType IrToDomConvertMap;
	DomToIrConvertMapType DomToIrConvertMap;
};

template <class AnnotationTy>
XmlElementPtr convertToXML(const std::string& mapName, std::function<XmlElement& (const AnnotationTy&, XmlElement&)> toXml,
		const insieme::utils::Annotation& ann, xercesc_3_1::DOMDocument* doc) {
	insieme::xml::XmlElementPtr node( new XmlElement("annotation", doc) );
	*node << XmlElement::Attribute("type", mapName);
	toXml(dynamic_cast<const AnnotationTy&>(ann), *node);
	return node;
}

template <class AnnotationTy>
insieme::core::NodeAnnotationPtr convertFromXML(std::function<std::shared_ptr<AnnotationTy> (const XmlElement&)> fromXml, const XmlElement& node) {
	return fromXml(node);
}

#define XML_REGISTER_ANNOTATION(CLASS_NAME, KEY, TO_XML, FROM_XML)	\
	void* hack ## CLASS_NAME ## hack = \
		insieme::xml::XmlConverter::get().registerAnnotation(KEY, \
			std::bind(insieme::xml::convertToXML<CLASS_NAME>, KEY, TO_XML, std::placeholders::_1, std::placeholders::_2), \
			std::bind(insieme::xml::convertFromXML<CLASS_NAME>, FROM_XML, std::placeholders::_1) );


} // end namespace xml
} // end namespace insieme

/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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

#pragma once
#include <insieme/utils/annotation.h>
#include <insieme/core/ir_node.h>

#include "insieme/xml/xsd_config.h"

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

	void convertXmlToDom(const std::string& fileName, const std::string& schemaFile, const bool validate);
	void convertStringToDom(const std::string& stringName, const std::string& schemaFile, const bool validate);	
	string convertDomToString();

	static insieme::core::NodePtr readIWIR(insieme::core::NodeManager& manager, const std::string& fileName);

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
	
	bool hasAttr(const std::string& id) const;
	std::string getAttr(const std::string& id) const;

	std::string getName() const;
	
	XmlElementList getChildren() const;
	XmlElementList getChildrenByName(const string& name) const;
	XmlElementPtr getFirstChildByName(const string& name) const;
};

} // end namespace xml
} // end namespace insieme

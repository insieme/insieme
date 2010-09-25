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

// DELETE
#include <xercesc/util/XMLUni.hpp>
//



#include "xml_dump.h"

using namespace insieme::core;
using namespace insieme::utils;
using namespace std;

XERCES_CPP_NAMESPACE_USE

namespace {

#define toUnicode(str) XStr(str).unicodeForm()

class XStr {
	XMLCh* fUnicodeForm;
public:
	XStr(const string& toTranscode) {
		fUnicodeForm = XMLString::transcode(toTranscode.c_str());
	}

	~XStr() {
		XMLString::release(&fUnicodeForm);
	}

	const XMLCh* unicodeForm() const {
		return fUnicodeForm;
	}
};

}



// ------------------------------------ XmlWriter ---------------------------

XmlWriter::XmlWriter(const string fileName) : outputFile (fileName) {
	try {
		XMLPlatformUtils::Initialize();
	}
	catch(const XMLException& toCatch)
	{
		char *pMsg = XMLString::transcode(toCatch.getMessage());
		XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n" << "  Exception message:" << pMsg;
		XMLString::release(&pMsg);
	}

	impl =  DOMImplementationRegistry::getDOMImplementation(toUnicode("Core"));
	doc = impl->createDocument(0, toUnicode("Inspire"),0);
	rootElem = doc->getDocumentElement();
}

XmlWriter::~XmlWriter() {
	DOMLSSerializer   *theSerializer = ((DOMImplementationLS*)impl)->createLSSerializer();
	DOMLSOutput       *theOutputDesc = ((DOMImplementationLS*)impl)->createLSOutput();
	DOMConfiguration* serializerConfig = theSerializer->getDomConfig();
	
	theOutputDesc->setEncoding(toUnicode("ISO-8859-1"));
	
	if (serializerConfig->canSetParameter(XMLUni::fgDOMWRTFormatPrettyPrint, true))
		serializerConfig->setParameter(XMLUni::fgDOMWRTFormatPrettyPrint, true);
		
	XMLFormatTarget* myFormTarget = NULL;
	if (!outputFile.empty()){
		myFormTarget=new LocalFileFormatTarget(outputFile.c_str());
	} else {
		myFormTarget=new StdOutFormatTarget();
	}

	theOutputDesc->setByteStream(myFormTarget);

	theSerializer->write(doc, theOutputDesc);
	
	theOutputDesc->release();
	theSerializer->release();
	delete myFormTarget;
	
	doc->release();
	XMLPlatformUtils::Terminate();
}

void XmlWriter::visitGenericType(const GenericTypePtr& cur) {
	DOMElement*	genType = doc->createElement(toUnicode("genType"));
	genType->setAttribute(toUnicode("id"), toUnicode(numeric_cast<string>((size_t)(&*cur))));
	genType->setAttribute(toUnicode("familyName"), toUnicode(cur->getFamilyName().getName()));
	rootElem->appendChild(genType);

	if (const TypePtr base = cur->getBaseType()) {
		DOMElement*	baseType = doc->createElement(toUnicode("baseType"));
		genType->appendChild(baseType);
	
		DOMElement*	typePtr = doc->createElement(toUnicode("typePtr"));
		typePtr->setAttribute(toUnicode("ref"), toUnicode(numeric_cast<string>((size_t)(&*base))));			
		baseType->appendChild(typePtr);
	
		// all the edge annotations
	}

	DOMElement*	typeParams = doc->createElement(toUnicode("typeParams"));
	genType->appendChild(typeParams);

	const vector< TypePtr >& param = cur->getTypeParameter();
	for(vector< TypePtr >::const_iterator iter = param.begin(); iter != param.end(); ++iter) {
		DOMElement*	typePtr = doc->createElement(toUnicode("typePtr"));
		typePtr->setAttribute(toUnicode("ref"), toUnicode(numeric_cast<string>((size_t)&*(*iter))));			
		typeParams->appendChild(typePtr);
	
		// all the annotations
	}

	DOMElement*	intTypeParams = doc->createElement(toUnicode("intTypeParams"));
	genType->appendChild(intTypeParams);

	const vector<IntTypeParam>& intParam = cur->getIntTypeParameter();
	for(vector <IntTypeParam>::const_iterator iter = intParam.begin(); iter != intParam.end(); ++iter) {
	
		DOMElement*	intTypeParam = doc->createElement(toUnicode("intTypeParam"));
		intTypeParams->appendChild(intTypeParam);
		switch (iter->getType()) {
		case IntTypeParam::VARIABLE:
			intTypeParam->setAttribute(toUnicode("type"), toUnicode("variable"));
			intTypeParam->setAttribute(toUnicode("value"), toUnicode(numeric_cast<string>(iter->getSymbol())));
			break;
		case IntTypeParam::CONCRETE:
			intTypeParam->setAttribute(toUnicode("type"), toUnicode("concrete"));
			intTypeParam->setAttribute(toUnicode("value"), toUnicode(numeric_cast<string>(iter->getSymbol())));
			break;
		case IntTypeParam::INFINITE:
			intTypeParam->setAttribute(toUnicode("type"), toUnicode("infinite"));
			break;
		default:
			intTypeParam->setAttribute(toUnicode("type"), toUnicode("Invalid Parameter"));
			break;
		}
	}
}

void XmlWriter::visitExpression(const ExpressionPtr& cur) {
}

void XmlWriter::visitArrayType(const ArrayTypePtr& cur) {
}

void XmlWriter::visitRefType(const RefTypePtr& cur) {
}


// ------------------------------------ XmlReader ---------------------------

XmlReader::XmlReader(const std::string fileName, const bool validate){
	XMLPlatformUtils::Initialize();
	bool error = false;
	XercesDOMParser *parser = new XercesDOMParser;
	if (parser) {
		parser->setValidationScheme(XercesDOMParser::Val_Auto);
		parser->setDoNamespaces(false);
		parser->setDoSchema(false);
		
		parser->setCreateEntityReferenceNodes(false);
		try	{
			parser->parse(fileName.c_str());
			error = parser->getErrorCount() != 0;
			if (error) {
				std::cerr << "Parsing " << fileName.c_str();
				std::cerr << " error count: " << error << std::endl;
			}
		}
		catch (const DOMException& e) {
			std::cerr << "DOM Exception parsing ";
			std::cerr << " reports: ";
			if (e.msg) {
				char *strMsg = XMLString::transcode(e.msg);
				std::cerr << strMsg << std::endl;
				XMLString::release(&strMsg);
			}
	  		else {
				std::cerr << e.code << std::endl;
			}
			error = true;
		}
		catch (const XMLException& e) {
			std::cerr << "XML Exception parsing ";
			std::cerr << fileName.c_str();
			std::cerr << " reports: ";
			std::cerr << e.getMessage() << std::endl;
			error = true;
		}
		catch (const SAXException& e) {
			std::cerr << "SAX Exception parsing ";
			std::cerr << fileName.c_str();
			std::cerr << " reports: ";
			std::cerr << e.getMessage() << std::endl;
			error = true;
		}
		catch (...) {
			std::cerr << "An exception parsing ";
			std::cerr << fileName.c_str() << std::endl;
		 	error = true;
		}
		if (!error) {
			DOMNode* doc = parser->getDocument();
			XMLCh tempStr[3] = {chLatin_L, chLatin_S, chNull};
			DOMImplementation *impl          = DOMImplementationRegistry::getDOMImplementation(tempStr);
			DOMLSSerializer   *theSerializer = ((DOMImplementationLS*)impl)->createLSSerializer();
			DOMLSOutput       *theOutputDesc = ((DOMImplementationLS*)impl)->createLSOutput();

			XMLFormatTarget* myFormTarget = new StdOutFormatTarget();
			theOutputDesc->setByteStream(myFormTarget);
	
			theSerializer->write(doc, theOutputDesc);
			theOutputDesc->release();
			theSerializer->release();
		}
	}
	XMLPlatformUtils::Terminate();
}

XmlReader::~XmlReader(){}




// -------------------------Xml Write - Read - Validate----------------------

void insieme::core::xmlWrite(const NodePtr& root, const std::string fileName){
	XmlWriter visitor(fileName);
	visitAllOnce(root, visitor);
};

void insieme::core::xmlRead(const std::string fileName, const bool validate){
	XmlReader reader(fileName, validate);
};

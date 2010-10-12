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

#include <gtest/gtest.h>
#include <xml_utils.h>

using namespace insieme::core;
using namespace insieme::xml;

TEST(XmlTest, GenericTypeTest) {
	NodeManager manager;

	GenericTypePtr type1 = GenericType::get(manager, "int");
	GenericTypePtr type2 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type1), toVector(IntTypeParam::getVariableIntParam('p')), type1);
	
	NodePtr root = type1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, FunctionTypeTest) {
	NodeManager manager;
	
	GenericTypePtr type1 = GenericType::get(manager, "int");
	GenericTypePtr type2 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type1), toVector(IntTypeParam::getVariableIntParam('p')), type1);
	
	FunctionTypePtr funType1 = FunctionType::get(manager, type1, type2);
	
	NodePtr root = funType1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, StructTypeTest) {
	NodeManager manager;

	Identifier identA("a");
	Identifier identB("b");

	StructType::Entries entriesA;
	entriesA.push_back(StructType::Entry(identA, GenericType::get(manager, "A")));
	entriesA.push_back(StructType::Entry(identB, GenericType::get(manager, "B")));

	StructTypePtr structA = StructType::get(manager, entriesA);
	
	NodePtr root = structA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, UnionTypeTest) {
	NodeManager manager;

	Identifier identA("a");
	Identifier identB("b");

	UnionType::Entries entriesA;
	entriesA.push_back(UnionType::Entry(identA, GenericType::get(manager, "A")));
	entriesA.push_back(UnionType::Entry(identB, GenericType::get(manager, "B")));

	UnionTypePtr unionA = UnionType::get(manager, entriesA);
	
	NodePtr root = unionA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}


// ------------------- DummyAnnotation ---------------------------------
class DummyAnnotation : public Annotation {
public:
	static StringKey<DummyAnnotation> DummyKey;
	string value;
	DummyAnnotation(string value) : value(value) { };

	virtual AnnotationKey* getKey() const {
		return &DummyKey;
	}
	
	const std::string getAnnotationName() const {
		 return "DummyAnnotation"; 
	}
};

// initalization of the dummy key
StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");

XmlElement DummyAnnotationToXML(DummyAnnotation ann, XmlElement el, xercesc::DOMDocument* doc){
	XmlElement intNode("int", doc);
	intNode.setText(ann.value);
	el << intNode;
	return el;
}

shared_ptr<Annotation> DummyAnnotationFromXML(XmlElement el){
	return shared_ptr<Annotation> (new DummyAnnotation("1"));
}

XML_CONVERTER(DummyAnnotation, DummyAnnotationToXML, DummyAnnotationFromXML)


TEST(XmlTest, AnnotationTest) {
	typedef shared_ptr<DummyAnnotation> DummyAnnotationPtr;
	DummyAnnotationPtr dummyA(new DummyAnnotation("A"));
	//DummyAnnotationPtr dummyB(new DummyAnnotation("B"));
	
	NodeManager manager;
	GenericTypePtr type1 = GenericType::get(manager, "int");
	GenericTypePtr type2 = GenericType::get(manager, "val");
	GenericTypePtr type3 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type1), toVector(IntTypeParam::getVariableIntParam('p')), type2);
	//type1->addAnnotation(dummyB);
	type2.addAnnotation(dummyA);
	NodePtr root = type3;
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	std::cout << s1;
}



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
#include <xercesc/util/XercesDefs.hpp>

#include "insieme/core/ast_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/xml/xml_utils.h"

using namespace std;
using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::xml;

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
	
	bool operator==(const Annotation& other) const {
		if(typeid(other) != typeid(*this)) return false;
		const DummyAnnotation& dummy = dynamic_cast<const DummyAnnotation&>(other); 
		return (value.compare(dummy.value) == 0);
	}
	
	bool operator!=(const Annotation& other) const {
		return !operator==(other);
	}
	
};

// initalization of the dummy key
StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");

typedef shared_ptr<DummyAnnotation> DummyAnnotationPtr;

XmlElement& DummyAnnotationToXML(const DummyAnnotation& ann, XmlElement& el){
	XmlElement intNode("int", el.getDoc());
	intNode.setText(ann.value);
	el << intNode;
	return el;
}

DummyAnnotationPtr DummyAnnotationFromXML(const XmlElement& el){
	return std::make_shared<DummyAnnotation>(el.getChildrenByName("int")[0].getText());
}

XML_REGISTER_ANNOTATION(DummyAnnotation, "DummyAnnotation", DummyAnnotationToXML, DummyAnnotationFromXML);

// ------------------- VectorAnnotation ---------------------------------
class VectorAnnotation : public Annotation {
public:
	static StringKey<VectorAnnotation> VectorKey;
	vector<string> values;
	VectorAnnotation(vector<string> values) : values(values) { };

	virtual AnnotationKey* getKey() const {
		return &VectorKey;
	}
	
	const std::string getAnnotationName() const {
		 return "VectorAnnotation"; 
	}
	
	bool operator==(const Annotation& other) const {
		if(typeid(other) != typeid(*this)) return false;
		const VectorAnnotation& vec = dynamic_cast<const VectorAnnotation&>(other);
		return (equals(values, vec.values));
	}
	
	bool operator!=(const Annotation& other) const {
		return !operator==(other);
	}
};

// initalization of the vector key
StringKey<VectorAnnotation> VectorAnnotation::VectorKey("VectorKey");

typedef shared_ptr<VectorAnnotation> VectorAnnotationPtr;

XmlElement& VectorAnnotationToXML(const VectorAnnotation& ann, XmlElement& el){
	XmlElement entries("entries", el.getDoc());
	el << entries;
	for (vector<string>::const_iterator iter = ann.values.begin(); iter != ann.values.end(); ++iter){
		XmlElement entry("entry", el.getDoc());
		entry.setText(*iter);
		entries << entry;
	}
	
	return el;
}

VectorAnnotationPtr VectorAnnotationFromXML(const XmlElement& el){
	vector <string> vec;
	XmlElement entries = el.getChildrenByName("entries")[0];
	vector<XmlElement> entryVec = entries.getChildrenByName("entry");
	for (vector<XmlElement>::const_iterator iter = entryVec.begin(); iter != entryVec.end(); ++iter){
		vec.push_back(iter->getText());
	}
	return std::make_shared<VectorAnnotation>(vec);
}

XML_REGISTER_ANNOTATION(VectorAnnotation, "VectorAnnotation", VectorAnnotationToXML, VectorAnnotationFromXML)

TEST(XmlTest, GenericTypeTest) {
	vector <string> vec;
	vec.push_back("genTy e1");
	vec.push_back("genTy e2");
	
	VectorAnnotationPtr vector_gte(new VectorAnnotation(vec));
	DummyAnnotationPtr dummy_gtn(new DummyAnnotation("genTy n"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("base n"));
	DummyAnnotationPtr dummy_tp1n(new DummyAnnotation("typePar1 n"));
	DummyAnnotationPtr dummy_tp2n(new DummyAnnotation("typePar2 n"));
	
	NodeManager manager;
	GenericTypePtr type1 = GenericType::get(manager, "type1");
	type1->addAnnotation(dummy_tp1n);
	GenericTypePtr type2 = GenericType::get(manager, "type2");
	type2->addAnnotation(dummy_tp2n);
	GenericTypePtr type3 = GenericType::get(manager, "type3");
	type3->addAnnotation(dummy_bn);
	GenericTypePtr type4 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type2), toVector(IntTypeParam::getVariableIntParam('p')), type3);
	type4->addAnnotation(dummy_gtn);
	
	NodePtr root = type4;
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, FunctionTypeTest) {
	NodeManager manager;

	GenericTypePtr type1 = GenericType::get(manager, "val");
	GenericTypePtr type2 = GenericType::get(manager, "int");
	GenericTypePtr type3 = GenericType::get(manager, "var");

	DummyAnnotationPtr dummy_fn(new DummyAnnotation("fun n"));
	DummyAnnotationPtr dummy_rn(new DummyAnnotation("ret n"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("arg n"));

	type1->addAnnotation(dummy_an);
	type2->addAnnotation(dummy_rn);

	FunctionTypePtr funType1 = FunctionType::get(manager, TypeList(), toVector<TypePtr>(type1, type3), type2);

	funType1->addAnnotation(dummy_fn);

	NodePtr root = funType1;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);

	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, StructTypeTest) {
	NodeManager manager;

	Identifier identA("a");
	Identifier identB("b");

	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	StructType::Entries entriesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");

	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");

	typeB->addAnnotation(dummy_bn);
	
	entriesA.push_back(StructType::Entry(identA, typeA));
	entriesA.push_back(StructType::Entry(identB, typeB));

	StructTypePtr structA = StructType::get(manager, entriesA);
	
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("struct n"));
	

	structA->addAnnotation(dummy_sn);
	
	NodePtr root = structA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, UnionTypeTest) {
	NodeManager manager;

	Identifier identA("a");
	Identifier identB("b");

	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	UnionType::Entries entriesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");

	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");

	typeB->addAnnotation(dummy_bn);
	
	entriesA.push_back(UnionType::Entry(identA, typeA));
	entriesA.push_back(UnionType::Entry(identB, typeB));

	UnionTypePtr UnionA = UnionType::get(manager, entriesA);
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("Union n"));
	

	UnionA->addAnnotation(dummy_sn);
	
	NodePtr root = UnionA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, TupleTypeTest) {
	NodeManager manager;

	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	vector<TypePtr> subTypesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");

	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");
	typeB->addAnnotation(dummy_bn);
	
	subTypesA.push_back(typeA);
	subTypesA.push_back(typeB);

	TupleTypePtr tupleA = TupleType::get(manager, subTypesA);
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("Tuple n"));
	
	tupleA->addAnnotation(dummy_sn);
	
	NodePtr root = tupleA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, TypeVariableTest) {
	NodeManager manager;
	TypeVariablePtr varTypeA = TypeVariable::get(manager, "alpha");
	DummyAnnotationPtr dummy_An(new DummyAnnotation("typeA n"));
	varTypeA->addAnnotation(dummy_An);
		
	TypeVariablePtr varTypeB = TypeVariable::get(manager, "beta");
	
	TypeVariablePtr varTypeG = TypeVariable::get(manager, "gamma");
	DummyAnnotationPtr dummy_Gn(new DummyAnnotation("typeG n"));
	varTypeG->addAnnotation(dummy_Gn);
	
	GenericTypePtr type1 = GenericType::get(manager, "int", toVector<TypePtr>(varTypeA, varTypeB), 
								toVector(IntTypeParam::getVariableIntParam('p')), varTypeG);
	
	NodePtr root = type1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, RecTypeTest) {
	NodeManager manager;
	TypeVariablePtr varTypeA = TypeVariable::get(manager, "alpha");
	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	varTypeA->addAnnotation(dummy_an);
	
	DummyAnnotationPtr dummy_gn(new DummyAnnotation("typeG n"));

	GenericTypePtr typeG = GenericType::get(manager, "G");
	typeG->addAnnotation(dummy_gn);
	
	RecTypeDefinition::RecTypeDefs definitions;
	definitions.insert(std::make_pair(varTypeA, typeG));
	
	RecTypeDefinitionPtr definition = RecTypeDefinition::get(manager, definitions);
	DummyAnnotationPtr dummy_rtd_n(new DummyAnnotation("RecTypeDefs n"));
	definition->addAnnotation(dummy_rtd_n);
 
	RecTypePtr type = RecType::get(manager, varTypeA, definition);
	DummyAnnotationPtr dummy_rt_n(new DummyAnnotation("RecType n"));
	type->addAnnotation(dummy_rt_n);
	
	NodePtr root = type;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, LiteralTest) {
	NodeManager manager;
	LiteralPtr lit1 = Literal::get(manager, manager.basic.getInt8(), "10");
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lit1 n"));
	lit1->addAnnotation(dummy_ln);
	
	NodePtr root = lit1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ReturnStmtTest) {
	NodeManager manager;
	
	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "12");
	ReturnStmtPtr rstmt = ReturnStmt::get(manager, literal);
	DummyAnnotationPtr dummy_rn(new DummyAnnotation("return n"));
	rstmt->addAnnotation(dummy_rn);

	NodePtr root = rstmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ForStmtTest) {
	NodeManager manager;

	LiteralPtr start = Literal::get(manager, manager.basic.getInt4(), "1");
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("lit_start n"));
	start->addAnnotation(dummy_sn);
	
	LiteralPtr end   = Literal::get(manager, manager.basic.getInt4(), "9");
	DummyAnnotationPtr dummy_en(new DummyAnnotation("end n"));
	end->addAnnotation(dummy_en);
	
	LiteralPtr step  = Literal::get(manager, manager.basic.getInt4(), "2");
	DummyAnnotationPtr dummy_tn(new DummyAnnotation("step n"));
	step->addAnnotation(dummy_tn);

	DeclarationStmtPtr decl = DeclarationStmt::get(manager, Variable::get(manager, manager.basic.getInt4(), 1), start);
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("decl n"));
	decl->addAnnotation(dummy_dn);
	
	StatementPtr body = manager.get(manager.basic.getNoOp());
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body->addAnnotation(dummy_bn);

	ForStmtPtr fstmt = ForStmt::get(manager, decl, body, end, step);
	DummyAnnotationPtr dummy_fn(new DummyAnnotation("for n"));
	fstmt->addAnnotation(dummy_fn);

	NodePtr root = fstmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, IfStmtTest) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, manager.basic.getBool(), 1);
	
	LiteralPtr thenStmt  = Literal::get(manager, manager.basic.getInt4(), "7");
	DummyAnnotationPtr dummy_tn(new DummyAnnotation("then n"));
	thenStmt->addAnnotation(dummy_tn);
	
	StatementPtr elseStmt = manager.get(manager.basic.getNoOp());
	DummyAnnotationPtr dummy_en(new DummyAnnotation("else n"));
	elseStmt->addAnnotation(dummy_en);

	IfStmtPtr stmt = IfStmt::get(manager, var, thenStmt, elseStmt);
	DummyAnnotationPtr dummy_in(new DummyAnnotation("if n"));
	stmt->addAnnotation(dummy_in);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, SwitchStmtTest) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, manager.basic.getInt4());
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var n"));
	var->addAnnotation(dummy_vn);

	LiteralPtr literalA = Literal::get(manager, manager.basic.getInt4(), "1");
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, manager.basic.getInt4(), "2");
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB->addAnnotation(dummy_lBn);
	
	StatementPtr caseA = manager.get(manager.basic.getNoOp());
	DummyAnnotationPtr dummy_can(new DummyAnnotation("caseA n"));
	caseA->addAnnotation(dummy_can);
	
	StatementPtr caseB = ContinueStmt::get(manager);
	DummyAnnotationPtr dummy_cbn(new DummyAnnotation("caseB n"));
	caseB->addAnnotation(dummy_cbn);

	std::vector<SwitchStmt::Case> cases;
	cases.push_back(SwitchStmt::Case(literalA, caseA));
	cases.push_back(SwitchStmt::Case(literalB, caseB));
	StatementPtr other = BreakStmt::get(manager);

	SwitchStmtPtr stmt = SwitchStmt::get(manager, var, cases, other);
	DummyAnnotationPtr dummy_in(new DummyAnnotation("switch n"));
	stmt->addAnnotation(dummy_in);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}


TEST(XmlTest, WhileStmtTest) {
	NodeManager manager;

	LiteralPtr condition = Literal::get(manager, manager.get(manager.basic.getBool()), "true");
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("cond n"));
	condition->addAnnotation(dummy_cn);
	
	StatementPtr body = manager.get(manager.basic.getNoOp());
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body->addAnnotation(dummy_bn);

	WhileStmtPtr stmt = WhileStmt::get(manager, condition, body);
	DummyAnnotationPtr dummy_wn(new DummyAnnotation("while n"));
	stmt->addAnnotation(dummy_wn);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, BreakStmtTest) {
	NodeManager manager;

	BreakStmtPtr stmt = BreakStmt::get(manager);
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("break n"));
	stmt->addAnnotation(dummy_bn);
	
	NodePtr root = stmt;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ContinueStmtTest) {
	NodeManager manager;

	ContinueStmtPtr stmt = ContinueStmt::get(manager);
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("continue n"));
	stmt->addAnnotation(dummy_cn);
	
	NodePtr root = stmt;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, CompoundStmtTest) {
	NodeManager manager;
	
	BreakStmtPtr bS = BreakStmt::get(manager);
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("break n"));
	bS->addAnnotation(dummy_bn);
		
	ContinueStmtPtr cS = ContinueStmt::get(manager);
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("continue n"));
	cS->addAnnotation(dummy_cn);
	
	vector<StatementPtr> stmtVec;
	stmtVec.push_back(bS);
	stmtVec.push_back(cS);
	CompoundStmtPtr compS = CompoundStmt::get(manager, stmtVec);
	DummyAnnotationPtr dummy_cmn(new DummyAnnotation("compound n"));
	compS->addAnnotation(dummy_cmn);
	
	NodePtr root = compS;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, DeclarationStmtTest) {
	NodeManager manager;

	VariablePtr var1 = Variable::get(manager, manager.basic.getBool(), 1);
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var1 n"));
	var1->addAnnotation(dummy_vn);

	LiteralPtr literalA = Literal::get(manager, manager.basic.getInt4(), "1");
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA->addAnnotation(dummy_lAn);

	DeclarationStmtPtr stmt = DeclarationStmt::get(manager, var1, literalA);
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("decl n"));
	stmt->addAnnotation(dummy_dn);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, StructExprTest) {
	NodeManager manager;

	Identifier ident1("j");
	Identifier ident2("k");
	
	vector<StructExpr::Member> vecA;
	
	LiteralPtr literal1 = Literal::get(manager, manager.basic.getInt4(), "111");
	DummyAnnotationPtr dummy_l1n(new DummyAnnotation("lit1 n"));
	literal1->addAnnotation(dummy_l1n);
	
	LiteralPtr literal2 = Literal::get(manager, manager.basic.getInt4(), "222");
	DummyAnnotationPtr dummy_l2n(new DummyAnnotation("lit2 n"));
	literal2->addAnnotation(dummy_l2n);
	
	vecA.push_back(StructExpr::Member(ident1, literal1));
	vecA.push_back(StructExpr::Member(ident2, literal2));

	StructExprPtr structA = StructExpr::get(manager, vecA);
	
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("struct n"));
	
	structA->addAnnotation(dummy_sn);
	
	NodePtr root = structA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, UnionExprTest) {
	NodeManager manager;
	
	Identifier identC("c");
	LiteralPtr literalC = Literal::get(manager, manager.basic.getInt4(), "10");
	DummyAnnotationPtr dummy_lCn(new DummyAnnotation("litC n"));
	literalC->addAnnotation(dummy_lCn);
	
	UnionTypePtr unionType = UnionType::get(manager, toVector(make_pair(identC, literalC->getType())));
	UnionExprPtr unionA = UnionExpr::get(manager, unionType, identC, literalC);

	DummyAnnotationPtr dummy_sn(new DummyAnnotation("union n"));

	unionA->addAnnotation(dummy_sn);

	NodePtr root = unionA;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);

	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, VectorExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalE = Literal::get(manager, manager.basic.getInt4(), "11");
	DummyAnnotationPtr dummy_lEn(new DummyAnnotation("litE n"));
	literalE->addAnnotation(dummy_lEn);
	
	LiteralPtr literalF = Literal::get(manager, manager.basic.getInt4(), "21");
	DummyAnnotationPtr dummy_lFn(new DummyAnnotation("litF n"));
	literalF->addAnnotation(dummy_lFn);
	
	vecA.push_back(literalE);
	vecA.push_back(literalF);
	
	VectorExprPtr vec = VectorExpr::get(manager, vecA);
	DummyAnnotationPtr dummy_Vn(new DummyAnnotation("vecExpr n"));
	vec->addAnnotation(dummy_Vn);
	
	NodePtr root = vec;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, TupleExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalG = Literal::get(manager, manager.basic.getInt4(), "12");
	DummyAnnotationPtr dummy_lGn(new DummyAnnotation("litG n"));
	literalG->addAnnotation(dummy_lGn);
	
	LiteralPtr literalH = Literal::get(manager, manager.basic.getInt4(), "22");
	DummyAnnotationPtr dummy_lHn(new DummyAnnotation("litH n"));
	literalH->addAnnotation(dummy_lHn);
	
	vecA.push_back(literalG);
	vecA.push_back(literalH);
	
	TupleExprPtr tuple = TupleExpr::get(manager, vecA);
	DummyAnnotationPtr dummy_Tn(new DummyAnnotation("tupleExpr n"));
	tuple->addAnnotation(dummy_Tn);
	
	NodePtr root = tuple;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, CastExprTest) {
	NodeManager manager;
	
	LiteralPtr literal = Literal::get(manager, manager.basic.getInt4(), "16");
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lit n"));
	literal->addAnnotation(dummy_ln);
	
	CastExprPtr cast = CastExpr::get(manager, manager.basic.getInt8(), literal);
	DummyAnnotationPtr dummy_Cn(new DummyAnnotation("castExpr n"));
	cast->addAnnotation(dummy_Cn);
	
	NodePtr root = cast;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, CallExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literal_A = Literal::get(manager, manager.basic.getInt4(), "1");
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("lit A n"));
	literal_A->addAnnotation(dummy_lAn);
	
	LiteralPtr literal_B = Literal::get(manager, manager.basic.getInt4(), "2");
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("lit B n"));
	literal_B->addAnnotation(dummy_lBn);
	
	LiteralPtr literal_C = Literal::get(manager, manager.basic.getInt4(), "3");
	DummyAnnotationPtr dummy_lCn(new DummyAnnotation("lit C n"));
	literal_C->addAnnotation(dummy_lCn);
	
	vecA.push_back(literal_A);
	vecA.push_back(literal_B);
	
	CallExprPtr call = CallExpr::get(manager, manager.basic.getBool(), literal_C, vecA);
	DummyAnnotationPtr dummy_Cn(new DummyAnnotation("callExpr n"));
	call->addAnnotation(dummy_Cn);
	
	NodePtr root = call;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, VariableTest) {
	NodeManager manager;
	VariablePtr var1 = Variable::get(manager, manager.basic.getInt8());
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var1 n"));
	var1->addAnnotation(dummy_vn);
	
	NodePtr root = var1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, JobExprTest) {
    NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr intType = manager.basic.getUIntGen();
	FunctionTypePtr funType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(), manager.basic.getUnit());
	FunctionTypePtr guardType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(intType, intType), manager.basic.getBool());

	ExpressionPtr handlerA = Variable::get(manager, funType);
	ExpressionPtr handlerB = Variable::get(manager, funType);
	ExpressionPtr handlerC = Variable::get(manager, funType);

	ExpressionPtr guardA = Variable::get(manager, guardType);
	ExpressionPtr guardB = Variable::get(manager, guardType);
	ExpressionPtr guardC = Variable::get(manager, guardType);
	ExpressionPtr defaultHandler = Variable::get(manager, funType);

	JobExpr::GuardedStmts stmts;
	stmts.push_back(JobExpr::GuardedStmt(guardA, handlerA));
	stmts.push_back(JobExpr::GuardedStmt(guardB, handlerB));
	stmts.push_back(JobExpr::GuardedStmt(guardC, handlerC));

	vector<DeclarationStmtPtr> localDeclarations;
	localDeclarations.push_back(DeclarationStmt::get(manager, Variable::get(manager, intType), Literal::get(manager, intType, "1")));
	localDeclarations.push_back(DeclarationStmt::get(manager, Variable::get(manager, intType), Literal::get(manager, intType, "2")));

	ExpressionPtr range = builder.getThreadNumRange(1,40);
	JobExprPtr job = JobExpr::get(manager, range, defaultHandler, stmts, localDeclarations);
	
	//JobExprPtr job = JobExpr::get(manager, defaultHandler, stmts, localDeclarations);
	DummyAnnotationPtr dummy_jn(new DummyAnnotation("job n"));
	job->addAnnotation(dummy_jn);
	
	NodePtr root = job;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, LambdaTest) {
	NodeManager manager;
	
	GenericTypePtr type1 = GenericType::get(manager, "val");
	GenericTypePtr type2 = GenericType::get(manager, "int");
	GenericTypePtr type3 = GenericType::get(manager, "var");
	
	FunctionTypePtr funType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(type1, type3), type2);
	
	Lambda::ParamList paramList;
	paramList.push_back(Variable::get(manager, manager.basic.getBool(), 1));
	paramList.push_back(Variable::get(manager, manager.basic.getBool(), 2));	
	
	Lambda::CaptureList captureList;
	captureList.push_back(Variable::get(manager, manager.basic.getBool(), 3));
	captureList.push_back(Variable::get(manager, manager.basic.getBool(), 4));

	StatementPtr body = ReturnStmt::get(manager, manager.basic.getTrue());
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body->addAnnotation(dummy_bn);
	
	LambdaPtr expr = Lambda::get(manager, funType,captureList, paramList, body);
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lambda n"));
	expr->addAnnotation(dummy_ln);

	NodePtr root = expr;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ProgramTest) {
	NodeManager manager;
	
	ProgramPtr program = Program::create(manager);

	ExpressionPtr entryA = Variable::get(manager, manager.basic.getBool(), 1);
	DummyAnnotationPtr dummy_an(new DummyAnnotation("entrya n"));
	entryA->addAnnotation(dummy_an);

	Program::EntryPointList entrySet;
	entrySet.push_back(entryA);

	program = Program::addEntryPoints(manager, program, entrySet, true);
	DummyAnnotationPtr dummy_pn(new DummyAnnotation("program n"));
	program->addAnnotation(dummy_pn);

	NodePtr root = program;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, MemberAccessExprTest) {
	NodeManager manager;

	Identifier ident1("a");
	
	vector<StructExpr::Member> vecA;
	
	LiteralPtr literal1 = Literal::get(manager, manager.basic.getInt4(), "222");
	DummyAnnotationPtr dummy_l1n(new DummyAnnotation("literal1 n"));
	literal1->addAnnotation(dummy_l1n);
	
	vecA.push_back(StructExpr::Member(ident1, literal1));

	StructExprPtr structA = StructExpr::get(manager, vecA);
	
	MemberAccessExprPtr expr = MemberAccessExpr::get(manager, structA, ident1);
	
	NodePtr root = expr;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, TupleProjectionExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literal1 = Literal::get(manager, manager.basic.getInt4(), "2");
	DummyAnnotationPtr dummy_l1n(new DummyAnnotation("literal1 n"));
	literal1->addAnnotation(dummy_l1n);
	
	vecA.push_back(literal1);

	TupleExprPtr tuple = TupleExpr::get(manager, vecA);
	
	TupleProjectionExprPtr expr = TupleProjectionExpr::get(manager, tuple, 0);
	
	NodePtr root = expr;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, CaptureInitExpr) {
	NodeManager manager;

	TypePtr res = GenericType::get(manager,"A");
	FunctionTypePtr funType2 = FunctionType::get(manager, toVector(res,res), TypeList(), res);
	VariablePtr captureVar = Variable::get(manager, res);
	
	LiteralPtr initValue = Literal::get(manager, res, "X");
	LiteralPtr initValue2 = Literal::get(manager, res, "Y");
	
	LambdaExprPtr lambda2 = LambdaExpr::get(manager, funType2, toVector<VariablePtr>(captureVar), Lambda::ParamList(), ReturnStmt::get(manager, Literal::get(manager, res, "A")));
	
	CaptureInitExprPtr expr = CaptureInitExpr::get(manager, lambda2, toVector<ExpressionPtr>(initValue, initValue2));
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("capinit n"));
	expr->addAnnotation(dummy_cn);
	
	NodePtr root = expr;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, MarkerStmtTest) {
	NodeManager manager;

	TypePtr type = GenericType::get(manager, "A");
	LiteralPtr literal = Literal::get(manager, type, "1");

	MarkerStmtPtr markerA = MarkerStmt::get(manager, literal);
	DummyAnnotationPtr dummy_mn(new DummyAnnotation("marker n"));
	markerA->addAnnotation(dummy_mn);

	NodePtr root = markerA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, MarkerExprTest) {
	NodeManager manager;

	TypePtr type = GenericType::get(manager, "A");
	LiteralPtr literal = Literal::get(manager, type, "1");

	MarkerExprPtr markerA = MarkerExpr::get(manager, literal);
	DummyAnnotationPtr dummy_mn(new DummyAnnotation("marker n"));
	markerA->addAnnotation(dummy_mn);

	NodePtr root = markerA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

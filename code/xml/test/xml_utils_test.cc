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
#include "xml_utils.h"
#include "lang_basic.h"
#include <xercesc/util/XercesDefs.hpp>

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
};

// initalization of the dummy key
StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");

XmlElement DummyAnnotationToXML(DummyAnnotation ann, XmlElement el){
	XmlElement intNode("int", el.getDoc());
	intNode.setText(ann.value);
	el << intNode;
	return el;
}

shared_ptr<Annotation> DummyAnnotationFromXML(XmlElement el){
	return std::make_shared<DummyAnnotation>("1");
}

XML_CONVERTER(DummyAnnotation, DummyAnnotationToXML, DummyAnnotationFromXML)
/*
typedef shared_ptr<DummyAnnotation> DummyAnnotationPtr;

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
};

// initalization of the vector key
StringKey<VectorAnnotation> VectorAnnotation::VectorKey("VectorKey");

XmlElement VectorAnnotationToXML(VectorAnnotation ann, XmlElement el){
	XmlElement entries("entries", el.getDoc());
	el << entries;
	for (vector<string>::const_iterator iter = ann.values.begin(); iter != ann.values.end(); ++iter){
		XmlElement entry("entry", el.getDoc());
		entry.setText(*iter);
		entries << entry;
	}
*/	
	/*vector<XmlElement> prova = entries.getChildren();
	for (vector<XmlElement>::const_iterator iter = prova.begin(); iter != prova.end(); ++iter){
		std::cout << iter->getName() << std::endl;
		std::cout << iter->getText() << std::endl;
	}*/
	
/*	return el;
}

shared_ptr<Annotation> VectorAnnotationFromXML(XmlElement el){
	vector <string> vec;
	vec.push_back("test1");
	vec.push_back("test2");
	return std::make_shared<VectorAnnotation>( vec );
}

XML_CONVERTER(VectorAnnotation, VectorAnnotationToXML, VectorAnnotationFromXML)

typedef shared_ptr<VectorAnnotation> VectorAnnotationPtr;

TEST(XmlTest, GenericTypeTest) {
	vector <string> vec;
	vec.push_back("genTy e1");
	vec.push_back("genTy e2");
	
	VectorAnnotationPtr vector_gte(new VectorAnnotation(vec));
	DummyAnnotationPtr dummy_gtn(new DummyAnnotation("genTy n"));
	DummyAnnotationPtr dummy_be(new DummyAnnotation("base e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("base n"));
	DummyAnnotationPtr dummy_tp1e(new DummyAnnotation("typePar1 e"));
	DummyAnnotationPtr dummy_tp1n(new DummyAnnotation("typePar1 n"));
	DummyAnnotationPtr dummy_tp2e(new DummyAnnotation("typePar2 e"));
	DummyAnnotationPtr dummy_tp2n(new DummyAnnotation("typePar2 n"));
	
	NodeManager manager;
	GenericTypePtr type1 = GenericType::get(manager, "type1");
	type1.addAnnotation(dummy_tp1e);
	type1->addAnnotation(dummy_tp1n);
	GenericTypePtr type2 = GenericType::get(manager, "type2");
	type2.addAnnotation(dummy_tp2e);
	type2->addAnnotation(dummy_tp2n);
	GenericTypePtr type3 = GenericType::get(manager, "type3");
	type3.addAnnotation(dummy_be);
	type3->addAnnotation(dummy_bn);
	GenericTypePtr type4 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type2), toVector(IntTypeParam::getVariableIntParam('p')), type3);
	type4.addAnnotation(vector_gte);
	type4->addAnnotation(dummy_gtn);
	
	NodePtr root = type4;
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
	//
	//xml.convertDomtoIr();
}

TEST(XmlTest, FunctionTypeTest) {
	NodeManager manager;
	
	GenericTypePtr type1 = GenericType::get(manager, "val");
	GenericTypePtr type2 = GenericType::get(manager, "int");

	DummyAnnotationPtr dummy_fe(new DummyAnnotation("fun e"));
	DummyAnnotationPtr dummy_fn(new DummyAnnotation("fun n"));
	DummyAnnotationPtr dummy_re(new DummyAnnotation("ret e"));
	DummyAnnotationPtr dummy_ae(new DummyAnnotation("arg e"));
	DummyAnnotationPtr dummy_rn(new DummyAnnotation("ret n"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("arg n"));
	
	type1.addAnnotation(dummy_ae);
	type1->addAnnotation(dummy_an);
	
	type2.addAnnotation(dummy_re);
	type2->addAnnotation(dummy_rn);
	
	FunctionTypePtr funType1 = FunctionType::get(manager, TupleType::get(manager, toVector<TypePtr>(type1)), type2);
	
	funType1.addAnnotation(dummy_fe);
	funType1->addAnnotation(dummy_fn);
	
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

	DummyAnnotationPtr dummy_ae(new DummyAnnotation("typeA e"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	
	DummyAnnotationPtr dummy_be(new DummyAnnotation("typeB e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	StructType::Entries entriesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");
	typeA.addAnnotation(dummy_ae);
	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");
	typeB.addAnnotation(dummy_be);
	typeB->addAnnotation(dummy_bn);
	
	entriesA.push_back(StructType::Entry(identA, typeA));
	entriesA.push_back(StructType::Entry(identB, typeB));

	StructTypePtr structA = StructType::get(manager, entriesA);
	
	DummyAnnotationPtr dummy_se(new DummyAnnotation("struct e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("struct n"));
	
	structA.addAnnotation(dummy_se);
	structA->addAnnotation(dummy_sn);
	
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

	DummyAnnotationPtr dummy_ae(new DummyAnnotation("typeA e"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	
	DummyAnnotationPtr dummy_be(new DummyAnnotation("typeB e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	UnionType::Entries entriesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");
	typeA.addAnnotation(dummy_ae);
	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");
	typeB.addAnnotation(dummy_be);
	typeB->addAnnotation(dummy_bn);
	
	entriesA.push_back(UnionType::Entry(identA, typeA));
	entriesA.push_back(UnionType::Entry(identB, typeB));

	UnionTypePtr UnionA = UnionType::get(manager, entriesA);
	
	DummyAnnotationPtr dummy_se(new DummyAnnotation("Union e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("Union n"));
	
	UnionA.addAnnotation(dummy_se);
	UnionA->addAnnotation(dummy_sn);
	
	NodePtr root = UnionA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);	
}

TEST(XmlTest, TupleTypeTest) {
	NodeManager manager;

	DummyAnnotationPtr dummy_ae(new DummyAnnotation("typeA e"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	
	DummyAnnotationPtr dummy_be(new DummyAnnotation("typeB e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	vector<TypePtr> subTypesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");
	typeA.addAnnotation(dummy_ae);
	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");
	typeB.addAnnotation(dummy_be);
	typeB->addAnnotation(dummy_bn);
	
	subTypesA.push_back(typeA);
	subTypesA.push_back(typeB);

	TupleTypePtr tupleA = TupleType::get(manager, subTypesA);

	DummyAnnotationPtr dummy_se(new DummyAnnotation("Tuple e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("Tuple n"));
	
	tupleA.addAnnotation(dummy_se);
	tupleA->addAnnotation(dummy_sn);
	
	NodePtr root = tupleA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, TypeVariableTest) {
	NodeManager manager;
	TypeVariablePtr varTypeA = TypeVariable::get(manager, "alpha");
	DummyAnnotationPtr dummy_Ae(new DummyAnnotation("typeA e"));
	DummyAnnotationPtr dummy_An(new DummyAnnotation("typeA n"));
	varTypeA.addAnnotation(dummy_Ae);
	varTypeA->addAnnotation(dummy_An);
		
	TypeVariablePtr varTypeB = TypeVariable::get(manager, "beta");
	
	TypeVariablePtr varTypeG = TypeVariable::get(manager, "gamma");
	DummyAnnotationPtr dummy_Ge(new DummyAnnotation("typeG e"));
	DummyAnnotationPtr dummy_Gn(new DummyAnnotation("typeG n"));
	varTypeG.addAnnotation(dummy_Ge);
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
}

TEST(XmlTest, RecTypeTest) {
	NodeManager manager;
	TypeVariablePtr varTypeA = TypeVariable::get(manager, "alpha");
	DummyAnnotationPtr dummy_ae(new DummyAnnotation("typeA e"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	varTypeA.addAnnotation(dummy_ae);
	varTypeA->addAnnotation(dummy_an);
	
	DummyAnnotationPtr dummy_ge(new DummyAnnotation("typeG e"));
	DummyAnnotationPtr dummy_gn(new DummyAnnotation("typeG n"));

	GenericTypePtr typeG = GenericType::get(manager, "G");
	typeG.addAnnotation(dummy_ge);
	typeG->addAnnotation(dummy_gn);
	
	RecTypeDefinition::RecTypeDefs definitions;
	definitions.insert(std::make_pair(varTypeA, typeG));
	
	RecTypeDefinitionPtr definition = RecTypeDefinition::get(manager, definitions);
	DummyAnnotationPtr dummy_rtd_e(new DummyAnnotation("RecTypeDefs e"));
	DummyAnnotationPtr dummy_rtd_n(new DummyAnnotation("RecTypeDefs n"));
	definition.addAnnotation(dummy_rtd_e);
	definition->addAnnotation(dummy_rtd_n);
 
	RecTypePtr type = RecType::get(manager, varTypeA, definition);
	DummyAnnotationPtr dummy_rt_e(new DummyAnnotation("RecType e"));
	DummyAnnotationPtr dummy_rt_n(new DummyAnnotation("RecType n"));
	type.addAnnotation(dummy_rt_e);
	type->addAnnotation(dummy_rt_n);
	
	NodePtr root = type;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, LiteralTest) {
	NodeManager manager;
	LiteralPtr lit1 = Literal::get(manager, "10", TYPE_INT_8_PTR);
	DummyAnnotationPtr dummy_le(new DummyAnnotation("lit1 e"));
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lit1 n"));
	lit1.addAnnotation(dummy_le);
	lit1->addAnnotation(dummy_ln);
	
	NodePtr root = lit1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, ReturnStmtTest) {
	NodeManager manager;
	
	LiteralPtr literal = Literal::get(manager, "12", lang::TYPE_INT_4_PTR);
	ReturnStmtPtr rstmt = ReturnStmt::get(manager, literal);
	DummyAnnotationPtr dummy_re(new DummyAnnotation("return e"));
	DummyAnnotationPtr dummy_rn(new DummyAnnotation("return n"));
	rstmt.addAnnotation(dummy_re);
	rstmt->addAnnotation(dummy_rn);

	NodePtr root = rstmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, ForStmtTest) {
	NodeManager manager;

	LiteralPtr start = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_se(new DummyAnnotation("lit_start e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("lit_start n"));
	start.addAnnotation(dummy_se);
	start->addAnnotation(dummy_sn);
	
	LiteralPtr end   = Literal::get(manager, "9", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_ee(new DummyAnnotation("end e"));
	DummyAnnotationPtr dummy_en(new DummyAnnotation("end n"));
	end.addAnnotation(dummy_ee);
	end->addAnnotation(dummy_en);
	
	LiteralPtr step  = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_te(new DummyAnnotation("step e"));
	DummyAnnotationPtr dummy_tn(new DummyAnnotation("step n"));
	step.addAnnotation(dummy_te);
	step->addAnnotation(dummy_tn);

	DeclarationStmtPtr decl = DeclarationStmt::get(manager, Variable::get(manager, lang::TYPE_INT_4_PTR, 1), start);
	DummyAnnotationPtr dummy_de(new DummyAnnotation("decl e"));
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("decl n"));
	decl.addAnnotation(dummy_de);
	decl->addAnnotation(dummy_dn);
	
	StatementPtr body = manager.get(lang::STMT_NO_OP_PTR);
	DummyAnnotationPtr dummy_be(new DummyAnnotation("body e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body.addAnnotation(dummy_be);
	body->addAnnotation(dummy_bn);

	ForStmtPtr fstmt = ForStmt::get(manager, decl, body, end, step);
	DummyAnnotationPtr dummy_fe(new DummyAnnotation("for e"));
	DummyAnnotationPtr dummy_fn(new DummyAnnotation("for n"));
	fstmt.addAnnotation(dummy_fe);
	fstmt->addAnnotation(dummy_fn);

	NodePtr root = fstmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, IfStmtTest) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, lang::TYPE_BOOL_PTR, 1);
	
	StatementPtr thenStmt = manager.get(lang::STMT_NO_OP_PTR);
	DummyAnnotationPtr dummy_te(new DummyAnnotation("then e"));
	DummyAnnotationPtr dummy_tn(new DummyAnnotation("then n"));
	thenStmt.addAnnotation(dummy_te);
	thenStmt->addAnnotation(dummy_tn);
	
	StatementPtr elseStmt = manager.get(lang::STMT_NO_OP_PTR);
	DummyAnnotationPtr dummy_ee(new DummyAnnotation("else e"));
	DummyAnnotationPtr dummy_en(new DummyAnnotation("else n"));
	elseStmt.addAnnotation(dummy_ee);
	elseStmt->addAnnotation(dummy_en);

	IfStmtPtr stmt = IfStmt::get(manager, var, thenStmt, elseStmt);
	DummyAnnotationPtr dummy_ie(new DummyAnnotation("if e"));
	DummyAnnotationPtr dummy_in(new DummyAnnotation("if n"));
	stmt.addAnnotation(dummy_ie);
	stmt->addAnnotation(dummy_in);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, SwitchStmtTest) {
	NodeManager manager;

	VariablePtr var = Variable::get(manager, lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_ve(new DummyAnnotation("var e"));
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var n"));
	var.addAnnotation(dummy_ve);
	var->addAnnotation(dummy_vn);

	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lBe(new DummyAnnotation("litB e"));
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB.addAnnotation(dummy_lBe);
	literalB->addAnnotation(dummy_lBn);
	
	StatementPtr caseA = manager.get(lang::STMT_NO_OP_PTR);
	DummyAnnotationPtr dummy_cae(new DummyAnnotation("caseA e"));
	DummyAnnotationPtr dummy_can(new DummyAnnotation("caseA n"));
	caseA.addAnnotation(dummy_cae);
	caseA->addAnnotation(dummy_can);
	
	StatementPtr caseB = ContinueStmt::get(manager);
	DummyAnnotationPtr dummy_cbe(new DummyAnnotation("caseB e"));
	DummyAnnotationPtr dummy_cbn(new DummyAnnotation("caseB n"));
	caseB.addAnnotation(dummy_cbe);
	caseB->addAnnotation(dummy_cbn);

	std::vector<SwitchStmt::Case> cases;
	cases.push_back(SwitchStmt::Case(literalA, caseA));
	cases.push_back(SwitchStmt::Case(literalB, caseB));
	StatementPtr other = BreakStmt::get(manager);

	SwitchStmtPtr stmt = SwitchStmt::get(manager, var, cases, other);
	DummyAnnotationPtr dummy_ie(new DummyAnnotation("switch e"));
	DummyAnnotationPtr dummy_in(new DummyAnnotation("switch n"));
	stmt.addAnnotation(dummy_ie);
	stmt->addAnnotation(dummy_in);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}


TEST(XmlTest, WhileStmtTest) {
	NodeManager manager;

	LiteralPtr condition = Literal::get(manager, "true", manager.get(lang::TYPE_BOOL_PTR));
	DummyAnnotationPtr dummy_ce(new DummyAnnotation("cond e"));
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("cond n"));
	condition.addAnnotation(dummy_ce);
	condition->addAnnotation(dummy_cn);
	
	StatementPtr body = manager.get(lang::STMT_NO_OP_PTR);
	DummyAnnotationPtr dummy_be(new DummyAnnotation("body e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body.addAnnotation(dummy_be);
	body->addAnnotation(dummy_bn);

	WhileStmtPtr stmt = WhileStmt::get(manager, condition, body);
	DummyAnnotationPtr dummy_we(new DummyAnnotation("while e"));
	DummyAnnotationPtr dummy_wn(new DummyAnnotation("while n"));
	stmt.addAnnotation(dummy_we);
	stmt->addAnnotation(dummy_wn);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, BreakStmtTest) {
	NodeManager manager;

	BreakStmtPtr stmt = BreakStmt::get(manager);
	DummyAnnotationPtr dummy_be(new DummyAnnotation("break e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("break n"));
	stmt.addAnnotation(dummy_be);
	stmt->addAnnotation(dummy_bn);
	
	NodePtr root = stmt;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, ContinueStmtTest) {
	NodeManager manager;

	ContinueStmtPtr stmt = ContinueStmt::get(manager);
	DummyAnnotationPtr dummy_ce(new DummyAnnotation("continue e"));
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("continue n"));
	stmt.addAnnotation(dummy_ce);
	stmt->addAnnotation(dummy_cn);
	
	NodePtr root = stmt;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, CompoundStmtTest) {
	NodeManager manager;
	
	BreakStmtPtr bS = BreakStmt::get(manager);
	DummyAnnotationPtr dummy_be(new DummyAnnotation("break e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("break n"));
	bS.addAnnotation(dummy_be);
	bS->addAnnotation(dummy_bn);
		
	ContinueStmtPtr cS = ContinueStmt::get(manager);
	DummyAnnotationPtr dummy_ce(new DummyAnnotation("continue e"));
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("continue n"));
	cS.addAnnotation(dummy_ce);
	cS->addAnnotation(dummy_cn);
	
	vector<StatementPtr> stmtVec;
	stmtVec.push_back(bS);
	stmtVec.push_back(cS);
	CompoundStmtPtr compS = CompoundStmt::get(manager, stmtVec);
	DummyAnnotationPtr dummy_cme(new DummyAnnotation("compound e"));
	DummyAnnotationPtr dummy_cmn(new DummyAnnotation("compound n"));
	compS.addAnnotation(dummy_cme);
	compS->addAnnotation(dummy_cmn);
	
	NodePtr root = compS;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);	
}

TEST(XmlTest, DeclarationStmtTest) {
	NodeManager manager;

	VariablePtr var1 = Variable::get(manager, lang::TYPE_BOOL_PTR, 1);
	DummyAnnotationPtr dummy_ve(new DummyAnnotation("var1 e"));
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var1 n"));
	var1.addAnnotation(dummy_ve);
	var1->addAnnotation(dummy_vn);

	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);

	DeclarationStmtPtr stmt = DeclarationStmt::get(manager, var1, literalA);
	DummyAnnotationPtr dummy_de(new DummyAnnotation("decl e"));
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("decl n"));
	stmt.addAnnotation(dummy_de);
	stmt->addAnnotation(dummy_dn);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, StructExprTest) {
	NodeManager manager;

	Identifier identA("a");
	Identifier identB("b");
	
	vector<NamedCompositeExpr::Member> vecA;
	
	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lBe(new DummyAnnotation("litB e"));
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB.addAnnotation(dummy_lBe);
	literalB->addAnnotation(dummy_lBn);
	
	vecA.push_back(NamedCompositeExpr::Member(identA, literalA));
	vecA.push_back(NamedCompositeExpr::Member(identB, literalB));

	StructExprPtr structA = StructExpr::get(manager, vecA);
	
	DummyAnnotationPtr dummy_se(new DummyAnnotation("struct e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("struct n"));
	
	structA.addAnnotation(dummy_se);
	structA->addAnnotation(dummy_sn);
	
	NodePtr root = structA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);	
}

TEST(XmlTest, UnionExprTest) {
	NodeManager manager;

	Identifier identA("a");
	Identifier identB("b");
	
	vector<NamedCompositeExpr::Member> vecA;
	
	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lBe(new DummyAnnotation("litB e"));
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB.addAnnotation(dummy_lBe);
	literalB->addAnnotation(dummy_lBn);
	
	vecA.push_back(NamedCompositeExpr::Member(identA, literalA));
	vecA.push_back(NamedCompositeExpr::Member(identB, literalB));

	UnionExprPtr unionA = UnionExpr::get(manager, vecA);
	
	DummyAnnotationPtr dummy_se(new DummyAnnotation("union e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("union n"));
	
	unionA.addAnnotation(dummy_se);
	unionA->addAnnotation(dummy_sn);
	
	NodePtr root = unionA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);	
}

TEST(XmlTest, VectorExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lBe(new DummyAnnotation("litB e"));
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB.addAnnotation(dummy_lBe);
	literalB->addAnnotation(dummy_lBn);
	
	vecA.push_back(literalA);
	vecA.push_back(literalB);
	
	VectorExprPtr vec = VectorExpr::get(manager, vecA);
	DummyAnnotationPtr dummy_Ve(new DummyAnnotation("vecExpr e"));
	DummyAnnotationPtr dummy_Vn(new DummyAnnotation("vecExpr n"));
	vec.addAnnotation(dummy_Ve);
	vec->addAnnotation(dummy_Vn);
	
	NodePtr root = vec;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, TupleExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lBe(new DummyAnnotation("litB e"));
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB.addAnnotation(dummy_lBe);
	literalB->addAnnotation(dummy_lBn);
	
	vecA.push_back(literalA);
	vecA.push_back(literalB);
	
	TupleExprPtr tuple = TupleExpr::get(manager, vecA);
	DummyAnnotationPtr dummy_Te(new DummyAnnotation("tupleExpr e"));
	DummyAnnotationPtr dummy_Tn(new DummyAnnotation("tupleExpr n"));
	tuple.addAnnotation(dummy_Te);
	tuple->addAnnotation(dummy_Tn);
	
	NodePtr root = tuple;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, CastExprTest) {
	NodeManager manager;
	
	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	CastExprPtr cast = CastExpr::get(manager, lang::TYPE_INT_8_PTR, literalA);
	DummyAnnotationPtr dummy_Ce(new DummyAnnotation("castExpr e"));
	DummyAnnotationPtr dummy_Cn(new DummyAnnotation("castExpr n"));
	cast.addAnnotation(dummy_Ce);
	cast->addAnnotation(dummy_Cn);
	
	NodePtr root = cast;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, CallExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalA = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lAe(new DummyAnnotation("litA e"));
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA.addAnnotation(dummy_lAe);
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lBe(new DummyAnnotation("litB e"));
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB.addAnnotation(dummy_lBe);
	literalB->addAnnotation(dummy_lBn);
	
	LiteralPtr literalC = Literal::get(manager, "3", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_lCe(new DummyAnnotation("litC e"));
	DummyAnnotationPtr dummy_lCn(new DummyAnnotation("litC n"));
	literalC.addAnnotation(dummy_lCe);
	literalC->addAnnotation(dummy_lCn);
	
	vecA.push_back(literalA);
	vecA.push_back(literalB);
	
	CallExprPtr call = CallExpr::get(manager, lang::TYPE_BOOL_PTR, literalC, vecA);
	DummyAnnotationPtr dummy_Ce(new DummyAnnotation("callExpr e"));
	DummyAnnotationPtr dummy_Cn(new DummyAnnotation("callExpr n"));
	call.addAnnotation(dummy_Ce);
	call->addAnnotation(dummy_Cn);
	
	NodePtr root = call;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, VariableTest) {
	NodeManager manager;
	VariablePtr var1 = Variable::get(manager, TYPE_INT_8_PTR);
	DummyAnnotationPtr dummy_ve(new DummyAnnotation("var1 e"));
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var1 n"));
	var1.addAnnotation(dummy_ve);
	var1->addAnnotation(dummy_vn);
	
	NodePtr root = var1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, JobExprTest) {
	NodeManager manager;
	
	JobExpr::GuardedStmts guarded;
	
	LiteralPtr default1 = Literal::get(manager, "1", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_de(new DummyAnnotation("default1 e"));
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("default1 n"));
	default1.addAnnotation(dummy_de);
	default1->addAnnotation(dummy_dn);
	
	LiteralPtr expr1 = Literal::get(manager, "2", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_ee(new DummyAnnotation("expr1 e"));
	DummyAnnotationPtr dummy_en(new DummyAnnotation("expr1 n"));
	expr1.addAnnotation(dummy_ee);
	expr1->addAnnotation(dummy_en);
	
	LiteralPtr stat1 = Literal::get(manager, "3", lang::TYPE_INT_4_PTR);
	DummyAnnotationPtr dummy_se(new DummyAnnotation("stat e"));
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("stat n"));
	stat1.addAnnotation(dummy_se);
	stat1->addAnnotation(dummy_sn);
	
	guarded.push_back(make_pair(expr1,stat1));
	
	VariablePtr var1 = Variable::get(manager, lang::TYPE_BOOL_PTR, 1);
	LiteralPtr literalA = Literal::get(manager, "4", lang::TYPE_INT_4_PTR);
	DeclarationStmtPtr decl = DeclarationStmt::get(manager, var1, literalA);
	DummyAnnotationPtr dummy_dece(new DummyAnnotation("decl e"));
	DummyAnnotationPtr dummy_decn(new DummyAnnotation("decl n"));
	decl.addAnnotation(dummy_dece);
	decl->addAnnotation(dummy_decn);
	
	JobExpr::LocalDecls decls;
	
	decls.push_back(decl);
	
	JobExprPtr job = JobExpr::get(manager, default1, guarded, decls);
	
	NodePtr root = job;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}

TEST(XmlTest, LambdaExprTest) {
	NodeManager manager;
	
	LambdaExpr::ParamList list;
	list.push_back(Variable::get(manager, TYPE_BOOL_PTR, 1));
	list.push_back(Variable::get(manager, TYPE_BOOL_PTR, 2));

	StatementPtr body = ReturnStmt::get(manager, CONST_BOOL_TRUE_PTR);
	DummyAnnotationPtr dummy_be(new DummyAnnotation("body e"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body.addAnnotation(dummy_be);
	body->addAnnotation(dummy_bn);
	
	LambdaExprPtr expr = LambdaExpr::get(manager, TYPE_BINARY_BOOL_OP_PTR, list, body);
	DummyAnnotationPtr dummy_le(new DummyAnnotation("lambda e"));
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lambda n"));
	expr.addAnnotation(dummy_le);
	expr->addAnnotation(dummy_ln);

	NodePtr root = expr;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertDomToXml("dump1.xml");
	xml.convertXmlToDom("dump1.xml", true);
	string s2 = xml.convertDomToString();
	EXPECT_EQ (s1, s2);
}*/
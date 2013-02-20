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

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/xml/xml_utils.h"

using namespace std;
using namespace insieme::utils;
using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::xml;

namespace insieme {
namespace xml {

// ------------------- DummyAnnotation ---------------------------------
class DummyAnnotation : public NodeAnnotation {
public:
	static const string NAME;
	static const StringKey<DummyAnnotation> DummyKey;
	string value;
	DummyAnnotation(string value) : value(value) { };
	
	virtual const AnnotationKeyPtr getKey() const {
		return &DummyKey;
	}
	
	const std::string& getAnnotationName() const {
		 return NAME;
	}
	
	bool operator==(const Annotation& other) const {
		if(typeid(other) != typeid(*this)) return false;
		const DummyAnnotation& dummy = dynamic_cast<const DummyAnnotation&>(other); 
		return (value.compare(dummy.value) == 0);
	}
	
	bool operator!=(const Annotation& other) const {
		return !operator==(other);
	}
	
	virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
		// just ignore
		return false;
	}
};

// initalization of the dummy key
const string DummyAnnotation::NAME = "DummyAnnotation";
const StringKey<DummyAnnotation> DummyAnnotation::DummyKey("DummyKey");

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
class VectorAnnotation : public NodeAnnotation {
public:
	static const string NAME;
	static const StringKey<VectorAnnotation> VectorKey;
	vector<string> values;
	VectorAnnotation(vector<string> values) : values(values) { };

	virtual const AnnotationKeyPtr getKey() const {
		return &VectorKey;
	}
	
	const std::string& getAnnotationName() const {
		 return NAME;
	}
	
	bool operator==(const Annotation& other) const {
		if(typeid(other) != typeid(*this)) return false;
		const VectorAnnotation& vec = dynamic_cast<const VectorAnnotation&>(other);
		return (equals(values, vec.values));
	}
	
	bool operator!=(const Annotation& other) const {
		return !operator==(other);
	}

	virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
		// just ignore
		return false;
	}
};

// initalization of the vector name and key
const string VectorAnnotation::NAME = "VectorAnnotation";
const StringKey<VectorAnnotation> VectorAnnotation::VectorKey("VectorKey");

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
	GenericTypePtr type4 = GenericType::get(manager, "int", toVector<TypePtr>(type1, type2), toVector<IntTypeParamPtr>(VariableIntTypeParam::get(manager, 'p')));
	type4->addAnnotation(dummy_gtn);
	
	NodePtr root = type4;
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, FunctionTypeTest) {
	NodeManager manager;

	GenericTypePtr type1 = GenericType::get(manager, "A");
	GenericTypePtr type2 = GenericType::get(manager, "B");
	GenericTypePtr type3 = GenericType::get(manager, "R");

	DummyAnnotationPtr dummy_fn(new DummyAnnotation("fun n"));
	DummyAnnotationPtr dummy_rn(new DummyAnnotation("ret n"));
	DummyAnnotationPtr dummy_an(new DummyAnnotation("arg n"));

	type1->addAnnotation(dummy_an);
	type2->addAnnotation(dummy_rn);

	TypeList list;
	list.push_back(type1);
	list.push_back(type2);

	FunctionTypePtr funType1 = FunctionType::get(manager, list, type3, FK_PLAIN);
	FunctionTypePtr funType2 = FunctionType::get(manager, list, type3, FK_CLOSURE);

	funType1->addAnnotation(dummy_fn);
	funType2->addAnnotation(dummy_fn);

	NodePtr root = funType1;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);

	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));

	// -- now for the closur function type ---

	root = funType2;

	xml.convertIrToDom(root);
	s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

	root2 = xml.convertDomToIr(manager2);

	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, StructTypeTest) {
	NodeManager manager;

	StringValuePtr identA = StringValue::get(manager, "a");
	StringValuePtr identB = StringValue::get(manager, "b");

	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	StructType::Entries entriesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");

	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");

	typeB->addAnnotation(dummy_bn);
	
	entriesA.push_back(NamedType::get(manager, identA, typeA));
	entriesA.push_back(NamedType::get(manager, identB, typeB));

	StructTypePtr structA = StructType::get(manager, entriesA);
	
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("struct n"));
	

	structA->addAnnotation(dummy_sn);
	
	NodePtr root = structA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, UnionTypeTest) {
	NodeManager manager;

	StringValuePtr identA = StringValue::get(manager, "a");
	StringValuePtr identB = StringValue::get(manager, "b");

	DummyAnnotationPtr dummy_an(new DummyAnnotation("typeA n"));
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("typeB n"));
	
	UnionType::Entries entriesA;
	GenericTypePtr typeA = GenericType::get(manager, "A");

	typeA->addAnnotation(dummy_an);
	
	GenericTypePtr typeB = GenericType::get(manager, "B");

	typeB->addAnnotation(dummy_bn);
	
	entriesA.push_back(NamedType::get(manager, identA, typeA));
	entriesA.push_back(NamedType::get(manager, identB, typeB));

	UnionTypePtr UnionA = UnionType::get(manager, entriesA);
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("Union n"));
	

	UnionA->addAnnotation(dummy_sn);
	
	NodePtr root = UnionA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

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
	xml.convertStringToDom(s1, true);
	
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

	GenericTypePtr type1 = GenericType::get(manager, "int", toVector<TypePtr>(varTypeA, varTypeB), 
								toVector<IntTypeParamPtr>(VariableIntTypeParam::get(manager, 'p')));
	
	NodePtr root = type1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
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
	
	vector<RecTypeBindingPtr> definitions;
	definitions.push_back(RecTypeBinding::get(manager, varTypeA, typeG));
	
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
	xml.convertStringToDom(s1, true);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, LiteralTest) {
	NodeManager manager;
	LiteralPtr lit1 = Literal::get(manager, manager.getLangBasic().getInt8(), "10");
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lit1 n"));
	lit1->addAnnotation(dummy_ln);
	
	NodePtr root = lit1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ReturnStmtTest) {
	NodeManager manager;
	
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
	ReturnStmtPtr rstmt = ReturnStmt::get(manager, literal);
	DummyAnnotationPtr dummy_rn(new DummyAnnotation("return n"));
	rstmt->addAnnotation(dummy_rn);

	NodePtr root = rstmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ForStmtTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	VariablePtr iter = Variable::get(manager, manager.getLangBasic().getInt4(), 1);
	DummyAnnotationPtr dummy_it(new DummyAnnotation("iterator"));
	iter->addAnnotation(dummy_it);

	LiteralPtr start = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("lit_start n"));
	start->addAnnotation(dummy_sn);
	
	LiteralPtr end   = Literal::get(manager, manager.getLangBasic().getInt4(), "9");
	DummyAnnotationPtr dummy_en(new DummyAnnotation("end n"));
	end->addAnnotation(dummy_en);
	
	LiteralPtr step  = Literal::get(manager, manager.getLangBasic().getInt4(), "2");
	DummyAnnotationPtr dummy_tn(new DummyAnnotation("step n"));
	step->addAnnotation(dummy_tn);

	DeclarationStmtPtr decl = DeclarationStmt::get(manager, iter, start);
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("decl n"));
	decl->addAnnotation(dummy_dn);
	
	CompoundStmtPtr body = builder.getNoOp();
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body->addAnnotation(dummy_bn);

	ForStmtPtr fstmt = ForStmt::get(manager, iter, start, end, step, body);
	DummyAnnotationPtr dummy_fn(new DummyAnnotation("for n"));
	fstmt->addAnnotation(dummy_fn);

	NodePtr root = fstmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, IfStmtTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	VariablePtr var = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	
	LiteralPtr thenStmt  = Literal::get(manager, manager.getLangBasic().getInt4(), "7");
	DummyAnnotationPtr dummy_tn(new DummyAnnotation("then n"));
	thenStmt->addAnnotation(dummy_tn);
	
	CompoundStmtPtr elseStmt = builder.getNoOp();
	DummyAnnotationPtr dummy_en(new DummyAnnotation("else n"));
	elseStmt->addAnnotation(dummy_en);

	IfStmtPtr stmt = builder.ifStmt(var, thenStmt, elseStmt);
	DummyAnnotationPtr dummy_in(new DummyAnnotation("if n"));
	stmt->addAnnotation(dummy_in);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, SwitchStmtTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	VariablePtr var = Variable::get(manager, manager.getLangBasic().getInt4());
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var n"));
	var->addAnnotation(dummy_vn);

	LiteralPtr literalA = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA->addAnnotation(dummy_lAn);
	
	LiteralPtr literalB = Literal::get(manager, manager.getLangBasic().getInt4(), "2");
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("litB n"));
	literalB->addAnnotation(dummy_lBn);
	
	CompoundStmtPtr caseA = builder.getNoOp();
	DummyAnnotationPtr dummy_can(new DummyAnnotation("caseA n"));
	caseA->addAnnotation(dummy_can);
	
	CompoundStmtPtr caseB = builder.wrapBody(ContinueStmt::get(manager));
	DummyAnnotationPtr dummy_cbn(new DummyAnnotation("caseB n"));
	caseB->addAnnotation(dummy_cbn);

	std::vector<SwitchCasePtr> cases;
	cases.push_back(builder.switchCase(literalA, caseA));
	cases.push_back(builder.switchCase(literalB, caseB));
	CompoundStmtPtr other = builder.wrapBody(BreakStmt::get(manager));

	SwitchStmtPtr stmt = SwitchStmt::get(manager, var, builder.switchCases(cases), other);
	DummyAnnotationPtr dummy_in(new DummyAnnotation("switch n"));
	stmt->addAnnotation(dummy_in);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}


TEST(XmlTest, WhileStmtTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	LiteralPtr condition = Literal::get(manager, manager.get(manager.getLangBasic().getBool()), "true");
	DummyAnnotationPtr dummy_cn(new DummyAnnotation("cond n"));
	condition->addAnnotation(dummy_cn);
	
	CompoundStmtPtr body = builder.getNoOp();
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body->addAnnotation(dummy_bn);

	WhileStmtPtr stmt = WhileStmt::get(manager, condition, body);
	DummyAnnotationPtr dummy_wn(new DummyAnnotation("while n"));
	stmt->addAnnotation(dummy_wn);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
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
	xml.convertStringToDom(s1, true);
	
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
	xml.convertStringToDom(s1, true);
	
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
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, DeclarationStmtTest) {
	NodeManager manager;

	VariablePtr var1 = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var1 n"));
	var1->addAnnotation(dummy_vn);

	LiteralPtr literalA = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("litA n"));
	literalA->addAnnotation(dummy_lAn);

	DeclarationStmtPtr stmt = DeclarationStmt::get(manager, var1, literalA);
	DummyAnnotationPtr dummy_dn(new DummyAnnotation("decl n"));
	stmt->addAnnotation(dummy_dn);
	
	NodePtr root = stmt;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, StructExprTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	StringValuePtr ident1 = StringValue::get(manager, "j");
	StringValuePtr ident2 = StringValue::get(manager, "k");
	
	vector<NamedValuePtr> vecA;
	
	LiteralPtr literal1 = Literal::get(manager, manager.getLangBasic().getInt4(), "111");
	DummyAnnotationPtr dummy_l1n(new DummyAnnotation("lit1 n"));
	literal1->addAnnotation(dummy_l1n);
	
	LiteralPtr literal2 = Literal::get(manager, manager.getLangBasic().getInt4(), "222");
	DummyAnnotationPtr dummy_l2n(new DummyAnnotation("lit2 n"));
	literal2->addAnnotation(dummy_l2n);
	
	vecA.push_back(builder.namedValue(ident1, literal1));
	vecA.push_back(builder.namedValue(ident2, literal2));

	StructExprPtr structA = builder.structExpr(vecA);
	
	DummyAnnotationPtr dummy_sn(new DummyAnnotation("struct n"));
	
	structA->addAnnotation(dummy_sn);
	
	NodePtr root = structA;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, UnionExprTest) {
	NodeManager manager;
	
	StringValuePtr identC = StringValue::get(manager, "c");
	LiteralPtr literalC = Literal::get(manager, manager.getLangBasic().getInt4(), "10");
	DummyAnnotationPtr dummy_lCn(new DummyAnnotation("litC n"));
	literalC->addAnnotation(dummy_lCn);
	
	UnionTypePtr unionType = UnionType::get(manager, toVector(NamedType::get(manager, identC, literalC->getType())));
	UnionExprPtr unionA = UnionExpr::get(manager, unionType, identC, literalC);

	DummyAnnotationPtr dummy_sn(new DummyAnnotation("union n"));

	unionA->addAnnotation(dummy_sn);

	NodePtr root = unionA;

	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);

	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, VectorExprTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalE = Literal::get(manager, manager.getLangBasic().getInt4(), "11");
	DummyAnnotationPtr dummy_lEn(new DummyAnnotation("litE n"));
	literalE->addAnnotation(dummy_lEn);
	
	LiteralPtr literalF = Literal::get(manager, manager.getLangBasic().getInt4(), "21");
	DummyAnnotationPtr dummy_lFn(new DummyAnnotation("litF n"));
	literalF->addAnnotation(dummy_lFn);
	
	vecA.push_back(literalE);
	vecA.push_back(literalF);
	
	VectorExprPtr vec = builder.vectorExpr(vecA);
	DummyAnnotationPtr dummy_Vn(new DummyAnnotation("vecExpr n"));
	vec->addAnnotation(dummy_Vn);
	
	NodePtr root = vec;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, TupleExprTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literalG = Literal::get(manager, manager.getLangBasic().getInt4(), "12");
	DummyAnnotationPtr dummy_lGn(new DummyAnnotation("litG n"));
	literalG->addAnnotation(dummy_lGn);
	
	LiteralPtr literalH = Literal::get(manager, manager.getLangBasic().getInt4(), "22");
	DummyAnnotationPtr dummy_lHn(new DummyAnnotation("litH n"));
	literalH->addAnnotation(dummy_lHn);
	
	vecA.push_back(literalG);
	vecA.push_back(literalH);
	
	TupleExprPtr tuple = builder.tupleExpr(vecA);
	DummyAnnotationPtr dummy_Tn(new DummyAnnotation("tupleExpr n"));
	tuple->addAnnotation(dummy_Tn);
	
	NodePtr root = tuple;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, CastExprTest) {
	NodeManager manager;
	
	LiteralPtr literal = Literal::get(manager, manager.getLangBasic().getInt4(), "16");
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lit n"));
	literal->addAnnotation(dummy_ln);
	
	CastExprPtr cast = CastExpr::get(manager, manager.getLangBasic().getInt8(), literal);
	DummyAnnotationPtr dummy_Cn(new DummyAnnotation("castExpr n"));
	cast->addAnnotation(dummy_Cn);
	
	NodePtr root = cast;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, CallExprTest) {
	NodeManager manager;
	
	vector<ExpressionPtr> vecA;
	
	LiteralPtr literal_A = Literal::get(manager, manager.getLangBasic().getInt4(), "1");
	DummyAnnotationPtr dummy_lAn(new DummyAnnotation("lit A n"));
	literal_A->addAnnotation(dummy_lAn);
	
	LiteralPtr literal_B = Literal::get(manager, manager.getLangBasic().getInt4(), "2");
	DummyAnnotationPtr dummy_lBn(new DummyAnnotation("lit B n"));
	literal_B->addAnnotation(dummy_lBn);
	
	LiteralPtr literal_C = Literal::get(manager, manager.getLangBasic().getInt4(), "3");
	DummyAnnotationPtr dummy_lCn(new DummyAnnotation("lit C n"));
	literal_C->addAnnotation(dummy_lCn);
	
	vecA.push_back(literal_A);
	vecA.push_back(literal_B);
	
	CallExprPtr call = CallExpr::get(manager, manager.getLangBasic().getBool(), literal_C, vecA);
	DummyAnnotationPtr dummy_Cn(new DummyAnnotation("callExpr n"));
	call->addAnnotation(dummy_Cn);
	
	NodePtr root = call;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, BindExprTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr typeA = GenericType::get(manager, "A");
	TypePtr typeRes = GenericType::get(manager, "R");
	
	FunctionTypePtr funTypeC = FunctionType::get(manager, toVector(typeA, typeA), typeRes);
	LiteralPtr funC = Literal::get(manager, funTypeC, "h");

	VariablePtr param1 = Variable::get(manager, typeA, 1);
	VariablePtr param2 = Variable::get(manager, typeA, 2);

	CallExprPtr callC4 = CallExpr::get(manager, manager.getLangBasic().getBool(), funC, toVector<ExpressionPtr>(param2, param1));
	BindExprPtr bind = builder.bindExpr(toVector(param1, param2), callC4);
		
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("bind n"));
	bind->addAnnotation(dummy_bn);
	
	NodePtr root = bind;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
	
}

TEST(XmlTest, VariableTest) {
	NodeManager manager;
	VariablePtr var1 = Variable::get(manager, manager.getLangBasic().getInt8());
	DummyAnnotationPtr dummy_vn(new DummyAnnotation("var1 n"));
	var1->addAnnotation(dummy_vn);
	
	NodePtr root = var1;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, JobExprTest) {
	NodeManager manager;
	IRBuilder builder(manager);

	TypePtr intType = manager.getLangBasic().getUIntGen();
	FunctionTypePtr funType = FunctionType::get(manager, toVector<TypePtr>(), manager.getLangBasic().getUnit());
	FunctionTypePtr guardType = FunctionType::get(manager, toVector<TypePtr>(intType, intType), manager.getLangBasic().getBool());

	ExpressionPtr handlerA = Variable::get(manager, funType);
	ExpressionPtr handlerB = Variable::get(manager, funType);
	ExpressionPtr handlerC = Variable::get(manager, funType);

	VariablePtr p1 = Variable::get(manager, intType, 10);
	VariablePtr p2 = Variable::get(manager, intType, 20);
	vector<VariablePtr> params = toVector(p1,p2);

	LambdaExprPtr guardA = builder.lambdaExpr(guardType, params, builder.returnStmt(builder.boolLit(false)));
	LambdaExprPtr guardB = builder.lambdaExpr(guardType, params, builder.returnStmt(builder.boolLit(true)));
	LambdaExprPtr guardC = builder.lambdaExpr(guardType, params, builder.returnStmt(builder.eq(p1,p2)));
	ExpressionPtr defaultHandler = Variable::get(manager, funType);

	vector<GuardedExprPtr> stmts;
	stmts.push_back(builder.guardedExpr(guardA, handlerA));
	stmts.push_back(builder.guardedExpr(guardB, handlerB));
	stmts.push_back(builder.guardedExpr(guardC, handlerC));

	vector<DeclarationStmtPtr> localDeclarations;
	localDeclarations.push_back(DeclarationStmt::get(manager, Variable::get(manager, intType), Literal::get(manager, intType, "1")));
	localDeclarations.push_back(DeclarationStmt::get(manager, Variable::get(manager, intType), Literal::get(manager, intType, "2")));

	ExpressionPtr range = builder.getThreadNumRange(1,40);
	JobExprPtr job = builder.jobExpr(range, localDeclarations, stmts, defaultHandler);
	
	DummyAnnotationPtr dummy_jn(new DummyAnnotation("job n"));
	job->addAnnotation(dummy_jn);
	
	NodePtr root = job;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, LambdaTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	GenericTypePtr type1 = GenericType::get(manager, "val");
	GenericTypePtr type2 = GenericType::get(manager, "int");
	GenericTypePtr type3 = GenericType::get(manager, "var");
	
	TypeList list;
	list.push_back(type1);
	list.push_back(type2);
	
	FunctionTypePtr funType = FunctionType::get(manager, list, type3);
	
	VariableList paramList;
	paramList.push_back(Variable::get(manager, manager.getLangBasic().getBool(), 1));
	paramList.push_back(Variable::get(manager, manager.getLangBasic().getBool(), 2));	
	
	StatementPtr body = ReturnStmt::get(manager, manager.getLangBasic().getTrue());
	DummyAnnotationPtr dummy_bn(new DummyAnnotation("body n"));
	body->addAnnotation(dummy_bn);
	
	LambdaPtr expr = builder.lambda(funType, paramList, body);
	DummyAnnotationPtr dummy_ln(new DummyAnnotation("lambda n"));
	expr->addAnnotation(dummy_ln);

	NodePtr root = expr;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ProgramTest) {
	NodeManager manager;
	
	ProgramPtr program = Program::get(manager);

	ExpressionPtr entryA = Variable::get(manager, manager.getLangBasic().getBool(), 1);
	DummyAnnotationPtr dummy_an(new DummyAnnotation("entrya n"));
	entryA->addAnnotation(dummy_an);

	ExpressionList entrySet;
	entrySet.push_back(entryA);

	program = Program::addEntryPoints(manager, program, entrySet);
	DummyAnnotationPtr dummy_pn(new DummyAnnotation("program n"));
	program->addAnnotation(dummy_pn);

	NodePtr root = program;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
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
	xml.convertStringToDom(s1, true);
	
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
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, ConcreteIntTypeParamTest) {
	NodeManager manager;

	ConcreteIntTypeParamPtr cType = ConcreteIntTypeParam::get(manager, 12);
	
	DummyAnnotationPtr dummy_ct(new DummyAnnotation("ctype n"));
	cType->addAnnotation(dummy_ct);

	NodePtr root = cType;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, InfiniteIntTypeParamTest) {
	NodeManager manager;

	InfiniteIntTypeParamPtr iType = InfiniteIntTypeParam::get(manager);
	
	DummyAnnotationPtr dummy_it(new DummyAnnotation("itype n"));
	iType->addAnnotation(dummy_it);

	NodePtr root = iType;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

TEST(XmlTest, VariableIntTypeParamTest) {
	NodeManager manager;

	VariableIntTypeParamPtr vType = VariableIntTypeParam::get(manager, 'p');
	
	DummyAnnotationPtr dummy_vt(new DummyAnnotation("vtype n"));
	vType->addAnnotation(dummy_vt);

	NodePtr root = vType;
	
	XmlUtil xml;
	xml.convertIrToDom(root);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);
	
	NodeManager manager2;
	NodePtr root2 = xml.convertDomToIr(manager2);
	
	EXPECT_EQ(*root, *root2);
	EXPECT_NE(root, root2);
	EXPECT_TRUE(equalsWithAnnotations(root, root2));
}

bool check(NodePtr node) {

	XmlUtil xml;
	xml.convertIrToDom(node);
	string s1 = xml.convertDomToString();
	xml.convertStringToDom(s1, true);

	NodeManager manager2;
	NodePtr restored = xml.convertDomToIr(manager2);

	EXPECT_TRUE(*node == *restored)
		<< "Orig: " << *node << "\n"
		<< "Rest: " << *restored << "\n";

	return node!=restored && *node == *restored;
}

TEST(XmlTest, FunctionKindTest) {
	NodeManager mgr;

	IRBuilder builder(mgr);

	TypePtr A = builder.refType(builder.genericType("A"));
	TypePtr B = builder.genericType("B");

	FunctionTypePtr funA = builder.functionType(toVector(A), B, FK_PLAIN);
	FunctionTypePtr funB = builder.functionType(toVector(A), B, FK_CLOSURE);
	FunctionTypePtr funC = builder.functionType(toVector(A), B, FK_CONSTRUCTOR);
	FunctionTypePtr funD = builder.functionType(toVector(A), B, FK_DESTRUCTOR);
	FunctionTypePtr funE = builder.functionType(toVector(A), B, FK_MEMBER_FUNCTION);

	EXPECT_TRUE(check(funA)) << *funA;
	EXPECT_TRUE(check(funB)) << *funB;
	EXPECT_TRUE(check(funC)) << *funC;
	EXPECT_TRUE(check(funD)) << *funD;
	EXPECT_TRUE(check(funE)) << *funE;
}

} // end namespace xml
} // end namespace insieme

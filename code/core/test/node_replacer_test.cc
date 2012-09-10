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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/utils/test/test_utils.h"

#include "ir_dummy_annotations.inc"


#include "insieme/utils/logging.h"
#include "insieme/core/transform/manipulation_utils.h"

namespace insieme {
namespace core {

using namespace checks;

TEST(NodeReplacer, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a simple AST construct
	TypePtr typeA = builder.genericType("A");
	TypePtr typeB = builder.genericType("B");
	TypePtr typeC = builder.genericType("C", toVector(typeA, typeB, typeA));
	TypePtr typeD = builder.genericType("D");

	EXPECT_EQ("C<A,B,A>", toString(*typeC));

	NodePtr mod;

	mod = transform::replaceAll(manager, typeC, typeA, typeD);
	EXPECT_EQ("C<D,B,D>", toString(*mod));

	mod = transform::replaceAll(manager, typeC, typeB, typeD);
	EXPECT_EQ("C<A,D,A>", toString(*mod));

	mod = transform::replaceAll(manager, typeC, typeC, typeD);
	EXPECT_EQ("D", toString(*mod));

}


TEST(NodeReplacer, AnnotationPreservation) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a simple AST construct
	GenericTypePtr typeA = builder.genericType("A");
	GenericTypePtr typeB = builder.genericType("B");
	GenericTypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeA, typeB, typeA));
	GenericTypePtr typeD = builder.genericType("D");

	typeD->addAnnotation(std::make_shared<DummyAnnotation>(12));
	typeC->getTypeParameter(0)->addAnnotation(std::make_shared<DummyAnnotation>(16));
	typeC->addAnnotation(std::make_shared<DummyAnnotation>(10));

	EXPECT_EQ("C<A,B,A>", toString(*typeC));
	EXPECT_TRUE(typeC->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(typeD->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(typeC->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));

	GenericTypePtr mod;

	mod = static_pointer_cast<GenericTypePtr>(transform::replaceAll(manager, typeC, typeA, typeD));
	EXPECT_EQ("C<D,B,D>", toString(*mod));
	EXPECT_FALSE(mod->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));

	// check preservation of replacement annotations
	typeD->addAnnotation(std::make_shared<DummyAnnotation>(14));
	mod = static_pointer_cast<GenericTypePtr>(transform::replaceAll(manager, typeC, typeB, typeD));
	EXPECT_EQ("C<A,D,A>", toString(*mod));
	EXPECT_FALSE(mod->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(1)->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));

	// ---- TEST preservation of annotations to modified nodes ----
	// add both annotations
	typeC->addAnnotation(std::make_shared<DummyAnnotation2>(20));
	EXPECT_TRUE(typeC->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(typeC->hasAnnotation(DummyAnnotation2::DummyKey));

	mod = static_pointer_cast<GenericTypePtr>(transform::replaceAll(manager, typeC, typeB, typeD));
	EXPECT_EQ("C<A,D,A>", toString(*mod));

	// only one should have been preserved
	EXPECT_FALSE(mod->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->hasAnnotation(DummyAnnotation2::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(1)->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
}

TEST(NodeReplacer, ReplaceByAddress) {
	NodeManager nm;
	IRBuilder builder(nm);

	// OK ... create a simple AST construct
	GenericTypePtr typeA = builder.genericType("A");
	GenericTypePtr typeB = builder.genericType("B");
	GenericTypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeA, typeB, typeA));
	GenericTypePtr typeD = builder.genericType("D", toVector<TypePtr>(typeC));
	GenericTypePtr typeX = builder.genericType("X");

	EXPECT_EQ("D<C<A,B,A>>", toString(*typeD));

	typeD->getTypeParameter(0)->addAnnotation(std::make_shared<DummyAnnotation>(12));
	typeD->getTypeParameter(0)->addAnnotation(std::make_shared<DummyAnnotation2>(14));
	EXPECT_TRUE(typeD->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(typeD->getTypeParameter(0)->hasAnnotation(DummyAnnotation2::DummyKey));

	GenericTypeAddress addrD(typeD);
	GenericTypeAddress addrC = static_address_cast<GenericTypeAddress>(addrD->getTypeParameter(0));
	GenericTypeAddress addrA1 = static_address_cast<GenericTypeAddress>(addrC->getTypeParameter(0));
	GenericTypeAddress addrB = static_address_cast<GenericTypeAddress>(addrC->getTypeParameter(1));
	GenericTypeAddress addrA2 = static_address_cast<GenericTypeAddress>(addrC->getTypeParameter(2));

	GenericTypePtr mod;
	NodeManager manager;
	mod = static_pointer_cast<GenericTypePtr>(transform::replaceNode(manager, addrA1, typeX));
	EXPECT_EQ("D<C<X,B,A>>", toString(*mod));
	EXPECT_FALSE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation2::DummyKey));

	mod = static_pointer_cast<GenericTypePtr>(transform::replaceNode(manager, addrA2, typeX));
	EXPECT_EQ("D<C<A,B,X>>", toString(*mod));
	EXPECT_FALSE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation::DummyKey));
	EXPECT_TRUE(mod->getTypeParameter(0)->hasAnnotation(DummyAnnotation2::DummyKey));

	mod = static_pointer_cast<GenericTypePtr>(transform::replaceNode(manager, addrD, typeX));
	EXPECT_EQ("X", toString(*mod));
}

TEST(NodeReplacer, ReplaceAllByAddress) {
	NodeManager nm;
	IRBuilder builder(nm);

	// OK ... create a simple AST construct
	GenericTypePtr typeA = builder.genericType("A");
	GenericTypePtr typeB = builder.genericType("B");
	GenericTypePtr typeC = builder.genericType("C", toVector<TypePtr>(typeA, typeB, typeA));
	GenericTypePtr typeD = builder.genericType("D", toVector<TypePtr>(typeC));

	GenericTypePtr typeX = builder.genericType("X");
	GenericTypePtr typeY = builder.genericType("Y");
	GenericTypePtr typeZ = builder.genericType("Z");

	EXPECT_EQ("D<C<A,B,A>>", toString(*typeD));

	GenericTypeAddress typeCAdr = static_address_cast<GenericTypeAddress>(GenericTypeAddress(typeD)->getTypeParameter(0));

	std::map<NodeAddress, NodePtr> replacements;
	replacements.insert(std::make_pair(typeCAdr->getTypeParameter(0), typeX));
	EXPECT_EQ("D<C<X,B,A>>", toString(*transform::replaceAll(nm, replacements)));

	replacements.clear();
	replacements.insert(std::make_pair(typeCAdr->getTypeParameter(0), typeX));
	replacements.insert(std::make_pair(typeCAdr->getTypeParameter(1), typeY));
	EXPECT_EQ("D<C<X,Y,A>>", toString(*transform::replaceAll(nm, replacements)));

	replacements.clear();
	replacements.insert(std::make_pair(typeCAdr->getTypeParameter(0), typeX));
	replacements.insert(std::make_pair(typeCAdr->getTypeParameter(1), typeY));
	replacements.insert(std::make_pair(typeCAdr->getTypeParameter(2), typeZ));
	EXPECT_EQ("D<C<X,Y,Z>>", toString(*transform::replaceAll(nm, replacements)));
}

TEST(NodeReplacer, ReplaceVariable) {

	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	TypePtr uint4 = basic.getUInt4();
	TypePtr boolType  = basic.getBool();

	LiteralPtr zero = builder.literal(uint4, "2");

	VariablePtr varA = builder.variable(builder.refType(uint4), 1);
	VariablePtr varB = builder.variable(builder.refType(boolType), 2);

	VariablePtr param = builder.variable(uint4, 3);
	FunctionTypePtr funType = builder.functionType(toVector(uint4), uint4);
	LambdaExprPtr lambda = builder.lambdaExpr(funType, toVector(param), builder.returnStmt(param));


	StatementPtr stmt = builder.compoundStmt(toVector<StatementPtr>(
			builder.declarationStmt(varA, builder.refVar(zero)),
			builder.callExpr(basic.getRefAssign(), varA, builder.callExpr(uint4, lambda, builder.deref(varA)))
	));
	//EXPECT_EQ("", toString(printer::PrettyPrinter(stmt)));

	CheckPtr all = core::checks::getFullCheck();

	EXPECT_EQ("[]", toString(check(stmt, all)));

	// apply recursive variable replacer
	utils::map::PointerMap<VariablePtr, VariablePtr> map;
	map[varA] = varB;
	NodePtr stmt2 = transform::replaceVarsRecursiveGen(manager, stmt, map);

	// fix initalization
	stmt2 = transform::replaceAll(manager, stmt2, builder.refVar(zero), builder.refVar(builder.literal(boolType, "false")));

	//EXPECT_EQ("", toString(printer::PrettyPrinter(stmt2)));
	

	EXPECT_EQ("[]", toString(check(stmt2, all)));
	EXPECT_PRED2(containsSubString, toString(printer::PrettyPrinter(stmt2)), "decl ref<bool> v2 =  var(false)");
	EXPECT_PRED2(containsSubString, toString(printer::PrettyPrinter(stmt2)), "fun(bool");

}

TEST(NodeReplacer, RecVarsReplacement) {
	NodeManager manager;
	IRBuilder builder(manager);
	const lang::BasicGenerator& basic = builder.getLangBasic();

	TypePtr kernelType = builder.refType(builder.arrayType(builder.genericType("_cl_kernel")));
	VariablePtr kernel = builder.variable(builder.refType(builder.structType(toVector(std::make_pair<StringValuePtr, TypePtr>(
			builder.stringValue("kernel"), builder.vectorType(kernelType, builder.concreteIntTypeParam(2)))))));
	TypePtr clMemType = builder.refType(builder.arrayType(builder.genericType("_cl_mem")));
	VariablePtr arg = builder.variable(builder.refType(clMemType));


	FunctionTypePtr fTy = builder.functionType(toVector(kernelType, clMemType), basic.getInt4());
	VariablePtr tuple = builder.variable(kernelType);

	VariablePtr src = builder.variable(clMemType);
	VariableList params = toVector(tuple, src);
	StatementList body;
	body.push_back(builder.callExpr(basic.getUnit(), basic.getRefAssign(), builder.callExpr(basic.getTupleRefElem(), tuple,
			builder.literal(basic.getUInt8(), "0"),
			builder.getTypeLiteral(src->getType())), src));
	body.push_back(builder.returnStmt(builder.intLit(0)));
	LambdaExprPtr function = builder.lambdaExpr(fTy, params, builder.compoundStmt(body));

	ExpressionPtr accessStruct = builder.callExpr(basic.getVectorRefElem(),
			builder.callExpr( basic.getCompositeRefElem(),
			kernel, builder.getIdentifierLiteral(builder.stringValue("kernel")), builder.getTypeLiteral(
					builder.vectorType(kernelType, builder.concreteIntTypeParam(2)))), builder.literal(basic.getUInt8(), "0"));

	StatementList stmts;
	ExpressionPtr kernelInit = builder.callExpr(basic.getRefVar(), builder.callExpr(basic.getUndefined(),
			builder.getTypeLiteral(builder.structType(toVector(std::make_pair<StringValuePtr, TypePtr>(builder.stringValue("kernel"),
				builder.vectorType(kernelType, builder.concreteIntTypeParam(2))))))));
	ExpressionPtr argInit = builder.callExpr(basic.getRefVar(), builder.callExpr(basic.getUndefined(),
			builder.getTypeLiteral(clMemType)));
	stmts.push_back(builder.declarationStmt(kernel, kernelInit));
	stmts.push_back(builder.declarationStmt(arg, argInit));
	stmts.push_back(builder.callExpr(basic.getInt4(), function,
			builder.callExpr( basic.getRefDeref(),accessStruct),
			builder.callExpr(clMemType, basic.getRefDeref(), arg)));

	StatementPtr stmt = builder.compoundStmt(stmts);

//	std::cout << printer::PrettyPrinter(stmt) << std::endl;;

	CheckPtr all = core::checks::getFullCheck();

	EXPECT_EQ(1u, check(stmt,all).size()); // there is one error

	// Set up replacement map
	utils::map::PointerMap<VariablePtr, VariablePtr> map;
	RefTypePtr kernelReplacementTy = static_pointer_cast<const RefType>(transform::replaceAll(manager, kernel->getType(), kernelType, builder.refType(
			builder.tupleType(toVector<TypePtr>(builder.refType(builder.arrayType(basic.getReal4())))))));

	map[kernel] = builder.variable(kernelReplacementTy);

	RefTypePtr clMemReplacementTy = static_pointer_cast<const RefType>(transform::replaceAll(manager, arg->getType(), builder.genericType("_cl_mem"),
			basic.getReal4()));

	map[arg] = builder.variable(clMemReplacementTy);

	// apply recursive variable replacer
//	std::cout << "Replacements " << map << std::endl;
	NodePtr stmt2 = builder.normalize(transform::replaceVarsRecursiveGen(manager, stmt, map, false,
			transform::defaultTypeRecovery, transform::getVarInitUpdater(manager)));
//	std::cout << stmt2 << std::endl;
	// fix initalization
/* fixed by functor passed to replaceVarsRecutsiveGen
	NodePtr kernelInitReplacement = builder.callExpr(basic.getRefVar(), builder.callExpr(basic.getUndefined(),
			basic.getTypeLiteral(kernelReplacementTy->getElementType())));
	stmt2 = transform::replaceAll(manager, stmt2, kernelInit, kernelInitReplacement, false);
	NodePtr clMemInitReplacement = builder.callExpr(basic.getRefVar(), builder.callExpr(basic.getUndefined(),
			basic.getTypeLiteral(clMemReplacementTy->getElementType())));
	stmt2 = transform::replaceAll(manager, stmt2, argInit, clMemInitReplacement, false);

	std::cout << ":(" << std::endl << printer::PrettyPrinter(stmt2) << std::endl;;
	std::cout << "(N)" << std::endl << stmt2 << std::endl;*/
	//EXPECT_EQ("", toString(printer::PrettyPrinter(stmt2)));


	EXPECT_EQ("[]", toString(check(stmt2, all)));
	EXPECT_PRED2(containsSubString, toString(printer::PrettyPrinter(stmt2, printer::PrettyPrinter::NO_LET_BINDINGS)), "decl ref<struct<kernel:vector<ref<(ref<array<real<4>,1>>)>,2>>> v0 =\
  var(undefined(type<struct<kernel:vector<ref<(ref<array<real<4>,1>>)>,2>>>));");
	EXPECT_PRED2(containsSubString, toString(printer::PrettyPrinter(stmt2, printer::PrettyPrinter::NO_LET_BINDINGS)), "decl ref<ref<array<real<4>,1>>> v1 =  var(undefined(type<ref<array<real<4>,1>>>))");
	EXPECT_PRED2(containsSubString, toString(printer::PrettyPrinter(stmt2, printer::PrettyPrinter::NO_LET_BINDINGS)), "fun(ref<(ref<array<real<4>,1>>)> v1, ref<array<real<4>,1>> v2)");

}

} // end namespace core
} // end namespace insieme


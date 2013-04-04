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

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/analysis/polyhedral/constraint.h"
#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic.h"

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

using insieme::utils::ConstraintType;

#define CREATE_ITER_VECTOR \
	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); \
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); \
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); \
	\
	IterationVector iterVec; \
	\
	iterVec.add( Iterator(iter1) ); \
	EXPECT_EQ(2u, iterVec.size()); \
	iterVec.add( Parameter(param) ); \
	EXPECT_EQ(3u, iterVec.size()); \
	iterVec.add( Iterator(iter2) ); \
	EXPECT_EQ(4u, iterVec.size()); \

TEST(IterationVector, Creation) {
	
	NodeManager mgr;
	CREATE_ITER_VECTOR;
	EXPECT_EQ(4u, iterVec.size());
	EXPECT_EQ("(v1,v2|v3|1)", toString(iterVec));

	EXPECT_TRUE( iterVec[0] == Iterator(iter1) );
	EXPECT_FALSE( iterVec[0] == Parameter(iter1) );

	IterationVector iterVec2;

	iterVec2.add( Parameter(param) );
	iterVec2.add( Iterator(iter1) ); 
	iterVec2.add( Iterator(iter2) );

	for (size_t it = 0; it < iterVec.size(); ++it ) {
		EXPECT_TRUE( iterVec[it] == iterVec2[it]);
	}

	EXPECT_EQ("(v1,v2|v3|1)", toString(iterVec2));
}

TEST(IterationVector, Iterator) {
	
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	IterationVector::iterator it = iterVec.begin();
	EXPECT_EQ(*(it+1), Iterator(iter2));
	EXPECT_EQ(*(it+2), Parameter(param));
	EXPECT_EQ(*(it+3), Constant());

}

TEST(IterationVector, MergeEmpty) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1( { iter1, iter2 } ) ; 
//	iterVec1.add( Parameter(param) ); 

	IterationVector iterVec2; 

	IterationVector itv = merge(iterVec1, iterVec2);
	
	IterationVector::iterator it = itv.begin();
	EXPECT_EQ(Iterator(iter1), *(it++));
	EXPECT_EQ(Iterator(iter2), *(it++));
//	EXPECT_EQ(Parameter(param), *(it++));
	EXPECT_EQ(Constant(), *it);

	IterationVector itv2 = merge(iterVec2, itv);

	IterationVector::iterator it2 = itv2.begin();
	EXPECT_EQ(Iterator(iter1), *(it2++));
	EXPECT_EQ(Iterator(iter2), *(it2++));
	EXPECT_EQ(Constant(), *it2);

}

TEST(IterationVector, MergeNotEmpty) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1; 
	iterVec1.add( Iterator(iter1) ); 
	iterVec1.add( Parameter(param) ); 

	IterationVector iterVec2; 
	iterVec2.add( Parameter(param) ); 
	iterVec2.add( Iterator(iter2) ); 

	IterationVector itv = merge(iterVec1, iterVec2);
	IterationVector::iterator it = itv.begin();
	EXPECT_EQ(Iterator(iter1), *(it++));
	EXPECT_EQ(Iterator(iter2), *(it++));
	EXPECT_EQ(Parameter(param), *(it++));
	EXPECT_EQ(Constant(), *it);
}

TEST(IterVec, Transform) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1( { iter1 }, { param } ); 
	iterVec1.add( Iterator(iter1) ); 
	iterVec1.add( Parameter(param) ); 
	// std::cout << iterVec1 << std::endl;
	
	VariablePtr iter3 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 4); 
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 5); 
	
	IterationVector iterVec2( { iter3, iter1, iter2 }, { param2, param } ); 
	
	const IndexTransMap&& transMap = transform(iterVec2, iterVec1);
	EXPECT_EQ(transMap, IndexTransMap({1,4,5}));
	
}

//==== AffineFunction =========================================================

TEST(AffineFunction, Creation) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec);
	af.setCoeff(Iterator(iter1), 0);
	af.setCoeff(Parameter(param),2);
	af.setCoeff(Iterator(iter2), 1);
	af.setCoeff(Constant(), 10);
	EXPECT_EQ("v2 + 2*v3 + 10", toString(af));

	EXPECT_EQ(0, af.getCoeff(iter1));
	EXPECT_EQ(2, af.getCoeff(param));
	EXPECT_EQ(1, af.getCoeff(iter2));
	EXPECT_EQ(10, af.getCoeff(Constant()));

	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 4); 	
	iterVec.add(Parameter(param2));

	EXPECT_EQ(0, af.getCoeff(param2));
	EXPECT_EQ(0, af.getCoeff(iter1));
	EXPECT_EQ(2, af.getCoeff(param));
	EXPECT_EQ(1, af.getCoeff(iter2));
	EXPECT_EQ(10, af.getCoeff(Constant()));
	EXPECT_EQ("v2 + 2*v3 + 10", toString(af));

	// convertion to IR 
	ExpressionPtr expr = toIR(mgr, af);
	EXPECT_EQ("int.add(int.add(v2, int.mul(2, v3)), 10)", toString(*expr));
}

TEST(AffineFunction, CreationFromExpr) {
	NodeManager mgr;
   
   	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1);
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2);
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3);

	CallExprPtr sum = CallExpr::get(mgr, mgr.getLangBasic().getInt4(), mgr.getLangBasic().getSignedIntAdd(), 
			toVector<ExpressionPtr>(iter1, param) 
		);
			
	IterationVector iterVec; 
	iterVec.add( Iterator(iter1) );

	AffineFunction af(iterVec, static_pointer_cast<const Expression>(sum));

	EXPECT_EQ(1, af.getCoeff(iter1));
	EXPECT_EQ(1, af.getCoeff(param));
	EXPECT_EQ(0, af.getCoeff(Constant()));
	EXPECT_EQ("v1 + v3", toString(af));

	iterVec.add( Iterator(iter2) );
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 4); 
	iterVec.add(Parameter(param2));

	EXPECT_EQ(1, af.getCoeff(iter1));
	EXPECT_EQ(0, af.getCoeff(iter2));
	EXPECT_EQ(1, af.getCoeff(param));
	EXPECT_EQ(0, af.getCoeff(param2));
	EXPECT_EQ(0, af.getCoeff(Constant()));
	EXPECT_EQ("v1 + v3", toString(af));
}

TEST(AffineFunction, ToExpr) {
	NodeManager mgr;
   
   	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1);
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2);
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3);

	CallExprPtr sum = CallExpr::get(mgr, mgr.getLangBasic().getInt4(), mgr.getLangBasic().getSignedIntAdd(), 
			toVector<ExpressionPtr>(iter1, param) 
		);
			
	IterationVector iterVec; 
	iterVec.add( Iterator(iter1) );
	AffineFunction af(iterVec, static_pointer_cast<const Expression>(sum));

	ExpressionPtr expr = toIR(mgr, af);

	EXPECT_EQ("int.add(v1, v3)", toString(*expr));

	AffineFunction af2(iterVec, expr);
	EXPECT_EQ(*expr, *toIR(mgr,af2));
}

TEST(AffineFunction, Equality) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1; 
	iterVec1.add( Iterator(iter1) ); 
	iterVec1.add( Parameter(iter2) ); 
	
	AffineFunction af1(iterVec1, {1,1,0} );
	
	IterationVector iterVec2; 
	iterVec2.add( Iterator(iter2) ); 
	iterVec2.add( Parameter(iter1) ); 
		
	AffineFunction af2(iterVec2, {1,1,0} );
	
	EXPECT_NE(af1, af2);
}

TEST(AffineFunction, AddIter) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1( { iter1 }, { param } ); 
	
	AffineFunction af1(iterVec1, {1,1,0} );
	iterVec1.add( Iterator(iter2) );
	
	af1.setCoeff(iter2, 2);
	
	EXPECT_EQ( af1.getCoeff(iter2), 2 );
}

TEST(AffineFunction, AddParam) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr param1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1( { iter1 }, { param1 }); 
	
	AffineFunction af1(iterVec1, {1,1,0} );
	iterVec1.add( Parameter(param2) );
	
	af1.setCoeff(param2, 2);
	
	EXPECT_EQ( af1.getCoeff(param2), 2 );
}


TEST(AffineFunction, AFChangeBase) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1( { iter2, iter1 }, { param }); 
	
	AffineFunction af1(iterVec1, { 0,1,1,9 } );
	
	VariablePtr iter3 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 4); 
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 5); 
	
	IterationVector iterVec2( { iter3, iter1, iter2 }, { param2, param } ) ; 
	
	const IndexTransMap&& transMap = transform(iterVec2, iterVec1);
	EXPECT_EQ(transMap, IndexTransMap( { 2,1,4,5 } ));
	
	AffineFunction aft = af1.toBase(iterVec2, transMap);
	EXPECT_EQ(af1, aft);
	
	iterVec1.add( Iterator(iter3) );
	AffineFunction af2(iterVec1, {0,1,0,1,9} );
	EXPECT_EQ(af2, aft);
	
	af2.setCoeff(Iterator(iter3), 3);
	EXPECT_NE(af2, aft);
}


TEST(AffineFunction, ToFormula) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 1); 
	VariablePtr param1 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 2); 
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3); 
	
	IterationVector iterVec1( { iter1 }, { param1 }); 
	
	AffineFunction af1(iterVec1, {1,1,0} );
	iterVec1.add( Parameter(param2) );
	af1.setCoeff(param2, 2);
	
	arithmetic::Formula f = af1;

	EXPECT_EQ( 1, static_cast<int64_t>(f[iter1]) );
	f = f - iter1;
	EXPECT_EQ( 1, static_cast<int64_t>(f[param1]) );
	f = f - param1;
	EXPECT_EQ( 2, static_cast<int64_t>(f[param2]));
	f = f - (arithmetic::Formula(2)*param2);
	EXPECT_TRUE(f.isZero());
}


// Constraints ======================================================================================

TEST(Constraint, Creation) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec, {0,1,2,10} );
	AffineConstraint c(af, ConstraintType::EQ);
	EXPECT_EQ("v2 + 2*v3 + 10 == 0", toString(c));
}

TEST(Constraint, Normalization) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec, {0,1,2,10});
	AffineConstraint c(af, ConstraintType::LT);
	EXPECT_EQ("v2 + 2*v3 + 10 < 0", toString(c));

	AffineConstraintPtr&& nc = normalize(c);
	EXPECT_EQ("(-v2 + -2*v3 + -11 >= 0)", toString(*nc));
}

TEST(Constraint, Combiner) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec, {0,1,2,10}); //FIXE LE
	AffineConstraint c1(af, ConstraintType::EQ);
	EXPECT_EQ("int.le(int.add(int.add(v2, int.mul(2, v3)), 10), 0)", toString(*toIR(mgr,c1)));

	AffineFunction af2(iterVec, {2,3,0,10});
	AffineConstraint c2(af2, ConstraintType::LT);
	EXPECT_EQ( "int.le(int.add(int.add(int.mul(2, v1), int.mul(3, v2)), 10), 0)", toString(*toIR(mgr,c2)) );

	AffineConstraintPtr&& ptr = c1 or not_(c2);

	ExpressionPtr expr = toIR(mgr, ptr);
	EXPECT_EQ("bool.or(int.le(int.add(int.add(v2, int.mul(2, v3)), 10), 0), "
			  "bind(){rec v0.{v0=fun(int<4> v4, int<4> v5) {"
			  	"return bool.not(int.le(int.add(int.add(int.mul(2, v4), int.mul(3, v5)), 10), 0));"
			  "}}(v1, v2)})", toString(*expr));
	
}

TEST(Constraint, ToFormula) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec, {0,1,2,10});
	AffineConstraint c1(af, ConstraintType::EQ);

	auto fc = insieme::utils::castTo<AffineFunction, arithmetic::Formula>( makeCombiner(c1) );
	EXPECT_EQ("(v2+2*v3+10 == 0)", toString(*fc));

	AffineFunction af2(iterVec, {2,3,0,10});
	AffineConstraint c2(af2, ConstraintType::LT);
	AffineConstraintPtr&& ptr = c1 or not_(c2);

	auto fc2 = insieme::utils::castTo<AffineFunction, arithmetic::Formula>( ptr );
	EXPECT_EQ("((v2+2*v3+10 == 0) v !(2*v1+3*v2+10 < 0))", toString(*fc2));

}	

// === Iteration Domain =======================================================
TEST(IterationDomain, Creation) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec,  { 0, 1, 2, 10 });
	AffineFunction af2(iterVec, { 1, 1, 0,  7 });
	AffineFunction af3(iterVec, { 1, 0, 1,  0 });

	AffineConstraintPtr&& cl = 
		AffineConstraint(af, 	ConstraintType::LT) and 
		AffineConstraint(af2, ConstraintType::LT) and 
		AffineConstraint(af3, ConstraintType::NE);

	EXPECT_EQ("(v1,v2|v3|1)", toString(iterVec));
	EXPECT_EQ("(((v2 + 2*v3 + 10 < 0) ^ (v1 + v2 + 7 < 0)) ^ (v1 + v3 != 0))", toString(*cl));

	IterationDomain it(cl);
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 4); 
	iterVec.add(Parameter(param2));
	EXPECT_EQ(5u, iterVec.size());
	EXPECT_EQ("(v1,v2|v3,v4|1)", toString(iterVec));

	// check weather these 2 affine functions are the same... even thought the
	// underlying iteration vector has been changed
	// EXPECT_EQ(af, (*it.begin()).getAffineFunction());
}

TEST(IterationDomain, SimpleStrided) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	VariablePtr stride = Variable::get(mgr, mgr.getLangBasic().getInt4(), 8);
	iterVec.add( Iterator(stride, true) );

	AffineFunction af(iterVec,  { 0, 1, 2, 0, 10 });
	AffineFunction af2(iterVec, { 1, 1, 0, 4,  7 });
	AffineFunction af3(iterVec, { 1, 0, 1, 0,  0 });

	AffineConstraintPtr&& cl = 
		AffineConstraint(af, 	ConstraintType::LT) and 
		AffineConstraint(af2, ConstraintType::EQ) and 
		AffineConstraint(af3, ConstraintType::NE);

	EXPECT_EQ("(v1,v2,v8|v3|1)", toString(iterVec));
	EXPECT_EQ("(((v2 + 2*v8 + 10 < 0) ^ (v1 + v2 + 4*v3 + 7 == 0)) ^ (v1 + v8 != 0))", toString(*cl));
}

TEST(AffineFunction, ChangeBase) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	AffineFunction af(iterVec, { 0, 1, 2, 10 } );
	EXPECT_EQ("v2 + 2*v3 + 10", toString(af));

	IterationVector iterVec1( { iter1, param, iter2 } ); 
	// std::cout << iterVec1 << std::endl;

	const IndexTransMap&& map = transform(iterVec1, iterVec);
	EXPECT_EQ(map, IndexTransMap( {0,2,1,3} ));

	AffineFunction&& converted = af.toBase(iterVec1, map);
	EXPECT_EQ("2*v3 + v2 + 10", toString(converted));

	AffineFunction&& converted2 = af.toBase(iterVec1);
	EXPECT_EQ(converted, converted2);

}

//==== IterationDomain ============================================================================

TEST(IterationDomain, Construction) {

	IterationVector iv;
	IterationDomain dom1(iv, true);
	EXPECT_FALSE(dom1.universe());
}

TEST(IterationDomain, range) {
	NodeManager mgr;

	VariablePtr param = Variable::get(mgr, mgr.getLangBasic().getInt4(), 3);
	IterationVector iterVec;
	iterVec.add( Parameter(param) ); 

	IterationDomain dom = makeVarRange(iterVec, param, IRBuilder(mgr).intLit(10));
	EXPECT_EQ("(v3 + -10 == 0)", toString(dom));
	
	VariablePtr param2 = Variable::get(mgr, mgr.getLangBasic().getInt4(), 4);
	iterVec.add( Parameter(param2) );

	IterationDomain dom1 = makeVarRange(iterVec, param, IRBuilder(mgr).intLit(10), param2);
	EXPECT_EQ("((v3 + -10 >= 0) ^ (v3 + -v4 < 0))", toString(dom1));
}

TEST(IterationDomain, FromVariable) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<iint<4>,1>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$for(int<4> i = 10 .. 50 : 1) { "
		"	v[$i+b$]; "
		"}$", symbols
	);

	EXPECT_EQ(2u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	auto iAddr = addresses[1].as<ExpressionAddress>();
	auto dom = getVariableDomain(iAddr);

	EXPECT_TRUE(dom.second);
	EXPECT_EQ("((-v3 + 49 >= 0) ^ (v3 + -10 >= 0))", toString(*dom.second));
}


TEST(IterationDomain, FromVariable2) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$for(int<4> i = 10 .. 50 : 1) { "
		"	if ( $i$<20 ) "
		"		v[$i+b$]; "
		"}$", symbols);

	EXPECT_EQ(3u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	{ 
		// get the iterator i used in the if condition 
		auto iAddr =  addresses[1].as<ExpressionAddress>();
		auto dom = getVariableDomain(iAddr);

		EXPECT_TRUE(dom.second);
		EXPECT_EQ("((-v3 + 49 >= 0) ^ (v3 + -10 >= 0))", toString(*dom.second));
	}

	{ 
		// Get the iterator i inside the if stmt
		auto iAddr =  addresses[2].as<ExpressionAddress>();
		auto dom = getVariableDomain(iAddr);

		EXPECT_TRUE(dom.second);
		EXPECT_EQ("(((-v3 + 19 >= 0) ^ (-v3 + 49 >= 0)) ^ (v3 + -10 >= 0))", toString(*dom.second));
	}
}

TEST(IterationDomain, NotAScop) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,100>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$for(int<4> i = 10 .. a*b : 1) { "
		"	v[$i+b$]; "
		"}$", symbols
	);

	EXPECT_EQ(2u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_FALSE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	auto dom = getVariableDomain(addresses[1].as<ExpressionAddress>());

	EXPECT_FALSE(dom.second);
}

TEST(IterationDomain, FromVariable3) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$for(int<4> i = 10 .. 50 : 1) { "
		"	if ( $i$<b ) "
		"		v[$i+b$]; "
		"}$", symbols);

	EXPECT_EQ(3u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	{ 
		// get the iterator i used in the if condition 
		auto dom = getVariableDomain(addresses[1].as<ExpressionAddress>());
		EXPECT_TRUE(dom.second);
		EXPECT_EQ("((-v3 + 49 >= 0) ^ (v3 + -10 >= 0))", toString(*dom.second));
	}
	{ 
		// Get the iterator i inside the if stmt
		auto dom = getVariableDomain(addresses[2].as<ExpressionAddress>());
		EXPECT_TRUE(dom.second);
		EXPECT_EQ("(((-v3 + 49 >= 0) ^ (-v3 + v2 + -1 >= 0)) ^ (v3 + -10 >= 0))", toString(*dom.second));
	}
}

TEST(IterationDomain, FromVariable4) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$if ( a > 20 ) { "
		"	for(int<4> i = b .. a : 1) { "
		"		if ( b < $i$ ) "
		"			v[$i+b$]; "
		"	} "
		"}$", symbols);

	EXPECT_EQ(3u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	{ 
		// get the iterator i used in the if condition 
		auto dom = getVariableDomain(addresses[1].as<ExpressionAddress>());

		EXPECT_TRUE(dom.second);
		EXPECT_EQ("((-v4 + v2 + -1 >= 0) ^ (v4 + -v3 >= 0))", toString(*dom.second));
		//auto pw = cardinality(mgr,*dom);
		//EXPECT_EQ("v1-v3 -> if (v1-v3-1 >= 0)", toString(pw));
	}
	{ 
		// Get the iterator i inside the if stmt
		auto dom = getVariableDomain(addresses[2].as<ExpressionAddress>());

		EXPECT_TRUE(dom.second);
		EXPECT_EQ("((((-v4 + v2 + -1 >= 0) ^ (v2 + -21 >= 0)) ^ (v4 + -v3 + -1 >= 0)) ^ (v4 + -v3 >= 0))", 
				  toString(*dom.second));
		//auto pw = cardinality(mgr,*dom);
		//EXPECT_EQ("v1-v3-1 -> if ((v1-v3-2 >= 0) ^ (v1-21 >= 0))", toString(pw));
	}
}



TEST(IterationDomain, FromVariableStrided) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$if ( a > 20 ) { "
		"	for(int<4> i = b .. a : 2) { "
		"		if ( b<$i$ ) "
		"			v[$i+b$]; "
		"	} "
		"}$ ", symbols);

	EXPECT_EQ(3u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	{ 
		// get the iterator i used in the if condition 
		auto dom = getVariableDomain(addresses[1].as<ExpressionAddress>());
		EXPECT_TRUE(dom.second);
		EXPECT_EQ("(((-v4 + v2 + -1 >= 0) ^ (v4 + -2*v8 + -v3 == 0)) ^ (v4 + -v3 >= 0))", toString(*dom.second));
		//auto pw = cardinality(mgr,*dom);
		//EXPECT_EQ("v1-v3 -> if (v1-v3-1 >= 0)", toString(pw));
	}

	{ 
		// Get the iterator i inside the if stmt
		auto dom = getVariableDomain(addresses[2].as<ExpressionAddress>());
		EXPECT_TRUE(dom.second);
		EXPECT_EQ("(((((-v4 + v2 + -1 >= 0) ^ (v2 + -21 >= 0)) ^ (v4 + -2*v8 + -v3 == 0)) ^ (v4 + -v3 + -1 >= 0)) ^ (v4 + -v3 >= 0))", toString(*dom.second));
		//auto pw = cardinality(mgr,*dom);
		//EXPECT_EQ("v1-v3-1 -> if ((v1-v3-2 >= 0) ^ (v1-21 >= 0))", toString(pw));
	}
}



TEST(IterationDomain, FromVariable5) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$if ( 1==1 ) { "
		"	for(int<4> i = a .. a+10 : 2) { "
		"		if ( i==a+2 ) "
		"			v[$i+b$]; "
		"	} "
		"}$ ", symbols);

	EXPECT_EQ(2u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	// Get the iterator i inside the if stmt
	auto dom = getVariableDomain(addresses[1].as<ExpressionAddress>());
	EXPECT_TRUE(dom.second);

	EXPECT_EQ("((((-v4 + v2 + 9 >= 0) ^ (v4 + -2*v7 + -v2 == 0)) ^ (v4 + -v2 + -2 == 0)) ^ (v4 + -v2 >= 0))", 
			  toString(*dom.second));
	//auto pw = cardinality(mgr,*dom);
	//EXPECT_EQ("v1-v3-1 -> if ((v1-v3-2 >= 0) ^ (v1-21 >= 0))", toString(pw));
}


TEST(IterationDomain, FromVariable6) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses(
		"$if ( a>4 ) { "
		"	for(int<4> i = a .. a+10 : 2) { "
		"		if ( i==a+1 ) "
		"			v[$i+b$]; "
		"	} "
		"}$ ", symbols);

	EXPECT_EQ(2u, addresses.size());
	scop::mark(addresses[0].getAddressedNode());

	EXPECT_TRUE( addresses[0]->hasAnnotation(scop::ScopRegion::KEY) );

	{ 
		// Get the iterator i inside the if stmt
		auto dom = getVariableDomain(addresses[1].as<ExpressionAddress>());
		EXPECT_TRUE(dom.second);
		EXPECT_EQ("(((((-v4 + v2 + 9 >= 0) ^ (v2 + -5 >= 0)) ^ (v4 + -2*v7 + -v2 == 0)) ^ (v4 + -v2 + -1 == 0)) ^ (v4 + -v2 >= 0))", toString(*dom.second));
		//auto pw = cardinality(mgr,*dom);
		//EXPECT_EQ("v1-v3-1 -> if ((v1-v3-2 >= 0) ^ (v1-21 >= 0))", toString(pw));
	}
}


//==== Scop =======================================================================================
TEST(Scop, BuildScop) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	VariablePtr var = Variable::get(mgr, mgr.getLangBasic().getInt4());
	ExpressionPtr arrAcc = builder.callExpr( 
		mgr.getLangBasic().getArrayRefElem1D(), 
		builder.callExpr( 
			mgr.getLangBasic().getArrayRefElem1D(), 
			builder.variable( builder.arrayType(builder.arrayType(mgr.getLangBasic().getInt4())) ),
			iter1
		),
		iter2
	);

	StatementPtr stmt = builder.callExpr( mgr.getLangBasic().getRefAssign(), var, arrAcc );
	
	// std::cout << "STMT: " << *stmt << std::endl;

	// Build the Iteration Vector
	IterationVector iterVec( { iter1, iter2 } );  // (i,j,1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { {  1, 0,   0 },     	// v1 >= 0
		  							   { -1, 0, 100 }, 		// -v1 + 100 >= 0
  							   	       {  0, 1,   0 },		// v2 >= 0
									   {  0,-1, 100 } } );	// -v2 + 100 >= 0

	// std::cout << "DOM: " << domain << std::endl;
	AffineSystem sched(iterVec, { {1, 0, 0}, 
							      {0, 1, 0} } );

	Scop scop(iterVec);
	scop.push_back( Stmt( 0, StatementAddress(stmt), domain, sched ) );

	std::vector<insieme::core::VariablePtr> nest = scop[0].loopNest();
	EXPECT_EQ( 2u, nest.size() );
	EXPECT_EQ( iter1, nest[0] );
	EXPECT_EQ( iter2, nest[1] );
}


//==== Transformations ============================================================================

TEST(Transformations, Interchange) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	VariablePtr var = Variable::get(mgr, mgr.getLangBasic().getInt4());
	ExpressionPtr arrAcc = builder.callExpr( 
		mgr.getLangBasic().getArrayRefElem1D(), 
		builder.callExpr( 
			mgr.getLangBasic().getArrayRefElem1D(), 
			builder.variable( builder.arrayType(builder.arrayType(mgr.getLangBasic().getInt4())) ),
			iter1
		),
		iter2
	);

	StatementPtr stmt = builder.callExpr( mgr.getLangBasic().getRefAssign(), var, arrAcc );
	
	// std::cout << "STMT: " << *stmt << std::endl;

	// Build the Iteration Vector
	IterationVector iterVec( { iter1, iter2 } );  // (i,j,1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { {  1, 0,   0 },     	// v1 >= 0
		  							   { -1, 0, 100 }, 		// -v1 + 100 >= 0
  									   {  0, 1,   0 },		// v2 >= 0
									   {  0,-1, 100 } } );	// -v2 + 100 >= 0

	// std::cout << "DOM: " << domain << std::endl;
	AffineSystem sched(iterVec, { {1, 0, 0}, 
							      {0, 1, 0} } );

	Scop scop(iterVec);
	scop.push_back( Stmt( 0, StatementAddress(stmt), domain, sched ) );

	NodePtr ir = scop.toIR(mgr);
	
	EXPECT_EQ( "for(int<4> v5 = 0 .. int.add(100, 1) : 1) {"
				 "for(int<4> v6 = 0 .. int.add(100, 1) : 1) {"
				 	"ref.assign(v3, array.ref.elem.1D(array.ref.elem.1D(v4, v5), v6));"
				  "};"
			   "}", toString(*ir));

	// perform interchange 
	AffineSystem& schedule = scop[0].getSchedule();
	schedule.set( { { 0, 1, 0}, 
					{ 1, 0, 0} } );

	ir = scop.toIR(mgr);
	EXPECT_EQ( "for(int<4> v7 = 0 .. int.add(100, 1) : 1) {"
					"for(int<4> v8 = 0 .. int.add(100, 1) : 1) {"
						"ref.assign(v3, array.ref.elem.1D(array.ref.elem.1D(v4, v8), v7));"
					"};"
				"}", toString(*ir));
}

TEST(Transformations, Tiling) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 

	VariablePtr iterTile = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	VariablePtr var = Variable::get(mgr, mgr.getLangBasic().getInt4());
	ExpressionPtr arrAcc = builder.callExpr( 
		mgr.getLangBasic().getArrayRefElem1D(), 
		builder.callExpr( 
			mgr.getLangBasic().getArrayRefElem1D(), 
			builder.variable( builder.arrayType(builder.arrayType(mgr.getLangBasic().getInt4())) ),
			iter1
		),
		iter2
	);

	StatementPtr stmt = builder.callExpr( 
					mgr.getLangBasic().getRefAssign(), 
					var, 
					arrAcc
				);
	
	IterationVector iterVec( { iter1, iter2 } );  // (i,j,1)

	// DOMAIN
	// v1 >= 0 && v1 <= 100
	// v2 >= 0 && v2 <= 100
	IterationDomain domain( iterVec, { { 1, 0,   0 }, 
		 						       {-1, 0, 100 }, 
									   { 0, 1,   0 }, 
									   { 0,-1, 100 } } );

	AffineSystem sched( iterVec, { { 1,0,0 }, { 0,1,0} } );

	Scop scop(iterVec);
	scop.push_back( Stmt( 0, StatementAddress(stmt), domain, sched ) );

	NodePtr ir = scop.toIR(mgr);
	EXPECT_EQ( "for(int<4> v6 = 0 .. int.add(100, 1) : 1) {"
					"for(int<4> v7 = 0 .. int.add(100, 1) : 1) {"
						"ref.assign(v4, array.ref.elem.1D(array.ref.elem.1D(v5, v6), v7));"
					"};"
				"}", toString(*ir));

	// perform interchange 
	AffineSystem& schedule = scop[0].getSchedule();

	// Add an outer loop 
	scop.getIterationVector().add( Iterator(iterTile) );

	VariablePtr&& existenceVar = builder.variable( mgr.getLangBasic().getInt4() );
	scop.getIterationVector().add( Iterator( existenceVar, true ) );

	// update the domain
	// v3 >= 0 && v3 <= 100
	// v1 >= v3 && v1 <= v3+T
	scop[0].getDomain() &= IterationDomain(scop.getIterationVector(),
			{ { 0, 0,  1, 0,   0 }, 
		      { 0, 0, -1, 0, 100 }, 
		      { 1, 0, -1, 0,   0 },
			  {-1, 0,  1, 0,  25 } } );
	
	// exist e0: e0*T == v3
	scop[0].getDomain() &= IterationDomain( 
		AffineConstraint( 
			AffineFunction( scop.getIterationVector(), { 0, 0,  1, -25, 0 } ), 
			ConstraintType::EQ 
		)  
	);

	// std::cout << "DOM: " << scop[0].getDomain() << std::endl;

	// add a new row to the scheduling matrix 
	schedule.append( { 0, 1, 0, 0, 0 } );

	// change the scheduling function by scheduling this loop as first 
	schedule[0].setCoeff(iterTile, 1);
	schedule[0].setCoeff(iter1, 0);
	schedule[1].setCoeff(iter1, 1);
	schedule[1].setCoeff(iter2, 0);

	ir = scop.toIR(mgr);
	EXPECT_EQ( "for(int<4> v9 = 0 .. int.add(100, 1) : 25) {"
					"for(int<4> v10 = v9 .. int.add(select(int.add(cast<int<4>>(v9), cast<int<4>>(25)), 100, int.lt), 1) : 1) {"
						"for(int<4> v11 = 0 .. int.add(100, 1) : 1) {"
							"ref.assign(v4, array.ref.elem.1D(array.ref.elem.1D(v5, v10), v11));"
						"};"
					"};"
				"}", toString(*ir));

}

TEST(Transformations, Fusion) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	VariablePtr iter1 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	VariablePtr iter2 = Variable::get(mgr, mgr.getLangBasic().getInt4()); 
	
	VariablePtr fusedIter = Variable::get(mgr, mgr.getLangBasic().getInt4()); 

	VariablePtr var = Variable::get(mgr, mgr.getLangBasic().getInt4());
	ExpressionPtr arrAcc1 = builder.callExpr( 
		mgr.getLangBasic().getArrayRefElem1D(), 
		builder.callExpr( 
			mgr.getLangBasic().getArrayRefElem1D(), 
			builder.variable( builder.arrayType(builder.arrayType(mgr.getLangBasic().getInt4())) ),
			iter1
		),
		iter2
	);

	ExpressionPtr arrAcc2 = builder.callExpr( 
		mgr.getLangBasic().getArrayRefElem1D(), 
		builder.callExpr( 
			mgr.getLangBasic().getArrayRefElem1D(), 
			builder.variable( builder.arrayType(builder.arrayType(mgr.getLangBasic().getInt4())) ),
			iter2
		),
		iter1
	);

	StatementPtr stmt1 = builder.callExpr( 
					mgr.getLangBasic().getRefAssign(), 
					var, 
					arrAcc1
				);

	StatementPtr stmt2 = builder.callExpr( 
					mgr.getLangBasic().getRefAssign(), 
					var,
					builder.callExpr( mgr.getLangBasic().getSignedIntAdd(), var, arrAcc2)
				);

	
	// std::cout << "STMT: " << *stmt << std::endl;

	IterationVector iterVec;  // (i,j,1)
	iterVec.add( Iterator(iter1) ); 
	iterVec.add( Iterator(iter2) ); 

	// STMT 1
	// DOMAIN
	// v1 >= 0 && v1 <= 100
	IterationDomain domain1( iterVec, { {  1, 0,  0 },
		 							    { -1, 0, 90 } } );
	
	domain1 &= IterationDomain( 
		AffineConstraint( 
			AffineFunction( iterVec, std::vector<int>({ 0, 1,  0 }) ), 
			ConstraintType::EQ 
		)  
	);

	AffineSystem sched1(iterVec, { {0, 0, 0},
		 						   {1, 0, 0}, 
								   {0, 0, 0} } );

	Scop scop(iterVec);
	scop.push_back( Stmt( 0, StatementAddress(stmt1), domain1, sched1 ) );

	// STMT2
	AffineSystem sched2(iterVec, { {0, 0, 1}, 
								   {0, 1, 0}, 
								   {0, 0, 0} } );


	IterationDomain domain2( iterVec, { {  0, 1,   0 },
			 						    {  0,-1, 100 } } );

	domain2 &= IterationDomain( 
		AffineConstraint( 
			AffineFunction( iterVec, std::vector<int>({ 1, 0,  0 }) ), 
			ConstraintType::EQ 
		)  
	);

	scop.push_back( Stmt( 1, StatementAddress(stmt2), domain2, sched2 ) );

	NodePtr ir = scop.toIR(mgr);
	EXPECT_EQ("{"
				"for(int<4> v7 = 0 .. int.add(90, 1) : 1) {"
					"ref.assign(v4, array.ref.elem.1D(array.ref.elem.1D(v5, v7), 0));"
			   "}; "
			   "for(int<4> v8 = 0 .. int.add(100, 1) : 1) {"
					"ref.assign(v4, int.add(v4, array.ref.elem.1D(array.ref.elem.1D(v6, v8), 0)));"
				"};"
			  "}", toString(*ir));


	// Add an outer loop 
	// scop.getIterationVector().add( Iterator(fusedIter) );

	//scop[0].getDomain() &= IterationDomain( 
	//	makeCombiner( 
	//		AffineConstraint( 
	//			AffineFunction( scop.getIterationVector(), CoeffVect({ 1, 0, -1, 0 }) ), 
	//			ConstraintType::EQ 
	//		) 
	//	) 
	//);

	//scop[1].getDomain() &= IterationDomain( 
	//	makeCombiner( 
	//		AffineConstraint( 
	//			AffineFunction( scop.getIterationVector(), CoeffVect({ 0, 1, -1, 0 }) ), 
	//			ConstraintType::EQ 
	//		) 
	//	) 
	//);

	// perform interchange 
	scop[0].getSchedule().set( { {0,0,0},
								 {1,0,0}, 
		 						 {0,0,0} } );

	scop[1].getSchedule().set( { {0,0,0}, 
								 {0,1,0},
							 	 {0,0,1} } );

	ir = scop.toIR(mgr);

	EXPECT_EQ("{"
				"for(int<4> v9 = 0 .. int.add(90, 1) : 1) {"
					"ref.assign(v4, array.ref.elem.1D(array.ref.elem.1D(v5, v9), 0)); "
					"ref.assign(v4, int.add(v4, array.ref.elem.1D(array.ref.elem.1D(v6, v9), 0)));"
				"}; "
				"for(int<4> v10 = 91 .. int.add(100, 1) : 1) {"
					"ref.assign(v4, int.add(v4, array.ref.elem.1D(array.ref.elem.1D(v6, v10), 0)));"
				"};"
			  "}", toString(*ir));
}
 

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

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/statements.h"

using namespace insieme::core;
using namespace insieme::analysis;

#define CREATE_ITER_VECTOR \
	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); \
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2); \
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); \
	\
	poly::IterationVector iterVec; \
	\
	iterVec.add( poly::Iterator(iter1) ); \
	EXPECT_EQ(static_cast<size_t>(2), iterVec.size()); \
	iterVec.add( poly::Parameter(param) ); \
	EXPECT_EQ(static_cast<size_t>(3), iterVec.size()); \
	iterVec.add( poly::Iterator(iter2) ); \
	EXPECT_EQ(static_cast<size_t>(4), iterVec.size()); \

TEST(IterationVector, Creation) {
	
	NodeManager mgr;
	CREATE_ITER_VECTOR;
	EXPECT_EQ(static_cast<size_t>(4), iterVec.size());
	{
		std::ostringstream ss;
		iterVec.printTo(ss);
		EXPECT_EQ("(v1,v2|v3|1)", ss.str());
	}
	EXPECT_TRUE( iterVec[0] == poly::Iterator(iter1) );
	EXPECT_FALSE( iterVec[0] == poly::Parameter(iter1) );

	poly::IterationVector iterVec2;

	iterVec2.add( poly::Parameter(param) );
	iterVec2.add( poly::Iterator(iter1) ); 
	iterVec2.add( poly::Iterator(iter2) );

	for (size_t it = 0; it < iterVec.size(); ++it ) {
		EXPECT_TRUE( iterVec[it] == iterVec2[it]);
	}
	{
		std::ostringstream ss;
		ss << iterVec2;
		EXPECT_EQ("(v1,v2|v3|1)", ss.str());
	}
}

TEST(IterationVector, Iterator) {
	
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::IterationVector::iterator it = iterVec.begin();
	EXPECT_EQ(*(it+1), poly::Iterator(iter2));
	EXPECT_EQ(*(it+2), poly::Parameter(param));
	EXPECT_EQ(*(it+3), poly::Constant());

}

TEST(IterationVector, MergeEmpty) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); 
	
	poly::IterationVector iterVec1; 
	iterVec1.add( poly::Iterator(iter1) ); 
	iterVec1.add( poly::Iterator(iter2) );
//	iterVec1.add( poly::Parameter(param) ); 

	poly::IterationVector iterVec2; 

	poly::IterationVector itv = poly::merge(iterVec1, iterVec2);
	
	poly::IterationVector::iterator it = itv.begin();
	EXPECT_EQ(poly::Iterator(iter1), *(it++));
	EXPECT_EQ(poly::Iterator(iter2), *(it++));
//	EXPECT_EQ(poly::Parameter(param), *(it++));
	EXPECT_EQ(poly::Constant(), *it);

	poly::IterationVector itv2 = poly::merge(iterVec2, itv);
	// std::cout << itv;
	poly::IterationVector::iterator it2 = itv2.begin();
	EXPECT_EQ(poly::Iterator(iter1), *(it2++));
	EXPECT_EQ(poly::Iterator(iter2), *(it2++));
// 	EXPECT_EQ(poly::Parameter(param), *(it2++));
	EXPECT_EQ(poly::Constant(), *it2);

}

TEST(IterationVector, MergeNotEmpty) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); 
	
	poly::IterationVector iterVec1; 
	iterVec1.add( poly::Iterator(iter1) ); 
	iterVec1.add( poly::Parameter(param) ); 
	std::cout << iterVec1 << std::endl;

	poly::IterationVector iterVec2; 
	iterVec2.add( poly::Parameter(param) ); 
	iterVec2.add( poly::Iterator(iter2) ); 
	std::cout << iterVec2 << std::endl; 

	poly::IterationVector itv = poly::merge(iterVec1, iterVec2);
	std::cout << itv << std::endl;
	poly::IterationVector::iterator it = itv.begin();
	EXPECT_EQ(poly::Iterator(iter1), *(it++));
	EXPECT_EQ(poly::Iterator(iter2), *(it++));
	EXPECT_EQ(poly::Parameter(param), *(it++));
	EXPECT_EQ(poly::Constant(), *it);
}

TEST(IterVec, Transform) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); 
	
	poly::IterationVector iterVec1; 
	iterVec1.add( poly::Iterator(iter1) ); 
	iterVec1.add( poly::Parameter(param) ); 
	// std::cout << iterVec1 << std::endl;
	
	VariablePtr iter3 = Variable::get(mgr, mgr.basic.getInt4(), 4); 
	VariablePtr param2 = Variable::get(mgr, mgr.basic.getInt4(), 5); 
	
	poly::IterationVector iterVec2; 
	iterVec2.add( poly::Iterator(iter3) ); 
	iterVec2.add( poly::Iterator(iter1) ); 
	iterVec2.add( poly::Iterator(iter2) ); 
	iterVec2.add( poly::Parameter(param2) ); 
	iterVec2.add( poly::Parameter(param) ); 
	// std::cout << iterVec2 << std::endl;
	
	const poly::IndexTransMap&& transMap = poly::transform(iterVec2, iterVec1);
	EXPECT_EQ(transMap, poly::IndexTransMap({1,4,5}));
	
}

//==== AffineFunction =========================================================

TEST(AffineFunction, Creation) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec);
	af.setCoeff(poly::Iterator(iter1), 0);
	af.setCoeff(poly::Parameter(param),2);
	af.setCoeff(poly::Iterator(iter2), 1);
	af.setCoeff(poly::Constant(), 10);

	{
		std::ostringstream ss;
		af.printTo(ss);
		EXPECT_EQ("1*v2 + 2*v3 + 10*1", ss.str());
	}

	EXPECT_EQ(0, af.getCoeff(iter1));
	EXPECT_EQ(2, af.getCoeff(param));
	EXPECT_EQ(1, af.getCoeff(iter2));
	EXPECT_EQ(10, af.getCoeff(poly::Constant()));

	VariablePtr param2 = Variable::get(mgr, mgr.basic.getInt4(), 4); 	
	iterVec.add(poly::Parameter(param2));

	EXPECT_EQ(0, af.getCoeff(param2));
	EXPECT_EQ(0, af.getCoeff(iter1));
	EXPECT_EQ(2, af.getCoeff(param));
	EXPECT_EQ(1, af.getCoeff(iter2));
	EXPECT_EQ(10, af.getCoeff(poly::Constant()));

	{
		std::ostringstream ss;
		af.printTo(ss);
		EXPECT_EQ("1*v2 + 2*v3 + 10*1", ss.str());
	}

	// convertion to IR 
	ExpressionPtr expr = poly::toIR(mgr, af);
	EXPECT_EQ(expr->toString(), "int.add(int.add(v2, int.mul(2, v3)), 10)");
}

TEST(AffineFunction, CreationFromExpr) {
	NodeManager mgr;
   
   	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1);
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2);
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3);

	CallExprPtr sum = CallExpr::get(mgr, mgr.basic.getInt4(), mgr.basic.getSignedIntAdd(), 
			toVector<ExpressionPtr>(iter1, param) 
		);
			
	poly::IterationVector iterVec; 
	iterVec.add( poly::Iterator(iter1) );

	poly::AffineFunction af(iterVec, sum);

	EXPECT_EQ(1, af.getCoeff(iter1));
	EXPECT_EQ(1, af.getCoeff(param));
	EXPECT_EQ(0, af.getCoeff(poly::Constant()));

	{
		std::ostringstream ss;
		af.printTo(ss);
		EXPECT_EQ("1*v1 + 1*v3", ss.str());
	}

	iterVec.add( poly::Iterator(iter2) );
	VariablePtr param2 = Variable::get(mgr, mgr.basic.getInt4(), 4); 
	iterVec.add(poly::Parameter(param2));

	EXPECT_EQ(1, af.getCoeff(iter1));
	EXPECT_EQ(0, af.getCoeff(iter2));
	EXPECT_EQ(1, af.getCoeff(param));
	EXPECT_EQ(0, af.getCoeff(param2));
	EXPECT_EQ(0, af.getCoeff(poly::Constant()));

	{
		std::ostringstream ss;
		af.printTo(ss);
		EXPECT_EQ("1*v1 + 1*v3", ss.str());
	}
}

TEST(AffineFunction, ToExpr) {
	NodeManager mgr;
   
   	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1);
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2);
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3);

	CallExprPtr sum = CallExpr::get(mgr, mgr.basic.getInt4(), mgr.basic.getSignedIntAdd(), 
			toVector<ExpressionPtr>(iter1, param) 
		);
			
	poly::IterationVector iterVec; 
	iterVec.add( poly::Iterator(iter1) );
	poly::AffineFunction af(iterVec, sum);

	ExpressionPtr expr = poly::toIR(mgr, af);

	EXPECT_EQ(expr->toString(), "int.add(v1, v3)");

	poly::AffineFunction af2(iterVec, expr);
	EXPECT_EQ(*expr, *toIR(mgr,af2));
}

TEST(AffineFunction, Equality) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); 
	
	poly::IterationVector iterVec1; 
	iterVec1.add( poly::Iterator(iter1) ); 
	iterVec1.add( poly::Parameter(iter2) ); 
	
	poly::AffineFunction af1(iterVec1, {1,1,0});
	
	poly::IterationVector iterVec2; 
	iterVec2.add( poly::Iterator(iter2) ); 
	iterVec2.add( poly::Parameter(iter1) ); 
		
	poly::AffineFunction af2(iterVec2, {1,1,0});
	
	EXPECT_NE(af1, af2);
}

TEST(AffineFunction, AFChangeBase) {
	NodeManager mgr;

	VariablePtr iter1 = Variable::get(mgr, mgr.basic.getInt4(), 1); 
	VariablePtr iter2 = Variable::get(mgr, mgr.basic.getInt4(), 2); 
	VariablePtr param = Variable::get(mgr, mgr.basic.getInt4(), 3); 
	
	poly::IterationVector iterVec1; 
	iterVec1.add( poly::Iterator(iter2) ); 
	iterVec1.add( poly::Iterator(iter1) ); 
	iterVec1.add( poly::Parameter(param) ); 
	// std::cout << iterVec1 << std::endl;
	
	poly::AffineFunction af1(iterVec1, {0,1,1,9});
	
	VariablePtr iter3 = Variable::get(mgr, mgr.basic.getInt4(), 4); 
	VariablePtr param2 = Variable::get(mgr, mgr.basic.getInt4(), 5); 
	
	poly::IterationVector iterVec2; 
	iterVec2.add( poly::Iterator(iter3) ); 
	iterVec2.add( poly::Iterator(iter1) ); 
	iterVec2.add( poly::Iterator(iter2) ); 
	iterVec2.add( poly::Parameter(param2) ); 
	iterVec2.add( poly::Parameter(param) ); 
	// std::cout << iterVec2 << std::endl;
	
	const poly::IndexTransMap&& transMap = poly::transform(iterVec2, iterVec1);
	EXPECT_EQ(transMap, poly::IndexTransMap({2,1,4,5}));
	
	poly::AffineFunction aft = af1.toBase(iterVec2, transMap);
	EXPECT_EQ(af1, aft);
	
	iterVec1.add( poly::Iterator(iter3) );
	poly::AffineFunction af2(iterVec1, {0,1,0,1,9});
	EXPECT_EQ(af2, aft);
	
	af2.setCoeff(poly::Iterator(iter3), 3);
	EXPECT_NE(af2, aft);
}

TEST(Constraint, Creation) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,1,2,10});
	poly::Constraint c(af, poly::Constraint::EQ);
	{
		std::ostringstream ss;
		c.printTo(ss);
		EXPECT_EQ("1*v2 + 2*v3 + 10*1 == 0", ss.str());
	}
}

TEST(Constraint, Normalization) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,1,2,10});
	poly::Constraint c(af, poly::Constraint::LT);
	{
		std::ostringstream ss;
		c.printTo(ss);
		EXPECT_EQ("1*v2 + 2*v3 + 10*1 < 0", ss.str());
	}
	poly::ConstraintCombinerPtr nc = normalize(c);
	{
		std::ostringstream ss;
		nc->printTo(ss);
		EXPECT_EQ("(-1*v2 + -2*v3 + -11*1 >= 0)", ss.str());
	}
}

TEST(Constraint, Combiner) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,1,2,10});
	poly::Constraint c1(af, poly::Constraint::EQ);
	EXPECT_EQ(toIR(mgr,c1)->toString(), 
			"int.le(int.add(int.add(v2, int.mul(2, v3)), 10), 0)"
		);

	poly::AffineFunction af2(iterVec, {2,3,0,10});
	poly::Constraint c2(af2, poly::Constraint::LT);
	EXPECT_EQ(toIR(mgr,c2)->toString(), 
			"int.le(int.add(int.add(int.mul(2, v1), int.mul(3, v2)), 10), 0)"
		);

	poly::ConstraintCombinerPtr ptr = c1 or not_(c2);

	ExpressionPtr expr = toIR(mgr, ptr);
	EXPECT_EQ(expr->toString(), 
		"bool.or(int.le(int.add(int.add(v2, int.mul(2, v3)), 10), 0), bind(){rec v3.{v3=fun(int<4> v1, int<4> v2) {return bool.not(int.le(int.add(int.add(int.mul(2, v2), int.mul(3, v1)), 10), 0));}}(v2, v1)})");
	
}

TEST(IterationDomain, Creation) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,1,2,10});
	poly::AffineFunction af2(iterVec, {1,1,0,7});
	poly::AffineFunction af3(iterVec, {1,0,1,0});

	poly::ConstraintCombinerPtr cl = 
		poly::Constraint(af, poly::Constraint::LT) and 
		poly::Constraint(af2, poly::Constraint::LT) and 
		poly::Constraint(af3, poly::Constraint::NE);

	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1,v2|v3|1)", ss.str());
	}

	poly::IterationDomain it(cl);
	VariablePtr param2 = Variable::get(mgr, mgr.basic.getInt4(), 4); 
	iterVec.add(poly::Parameter(param2));
	EXPECT_EQ(static_cast<size_t>(5), iterVec.size());

	{
		std::ostringstream ss;
		ss << iterVec;
		EXPECT_EQ("(v1,v2|v3,v4|1)", ss.str());
	}

	// check weather these 2 affine functions are the same... even thought the
	// underlying iteration vector has been changed
	// EXPECT_EQ(af, (*it.begin()).getAffineFunction());
}


TEST(AffineFunction, ChangeBase) {
	NodeManager mgr;
	CREATE_ITER_VECTOR;

	poly::AffineFunction af(iterVec, {0,1,2,10});
	{
		std::ostringstream ss;
		af.printTo(ss);
		EXPECT_EQ("1*v2 + 2*v3 + 10*1", ss.str());
	}

	poly::IterationVector iterVec1; 
	iterVec1.add( poly::Iterator(iter1) ); 
	iterVec1.add( poly::Iterator(param) ); 
	iterVec1.add( poly::Iterator(iter2) ); 
	// std::cout << iterVec1 << std::endl;

	const poly::IndexTransMap&& map = poly::transform(iterVec1, iterVec);
	EXPECT_EQ(map, poly::IndexTransMap({0,2,1,3}));

	poly::AffineFunction&& converted = af.toBase(iterVec1, map);
	{
		std::ostringstream ss;
		converted.printTo(ss);
		EXPECT_EQ("2*v3 + 1*v2 + 10*1", ss.str());
	}

	poly::AffineFunction&& converted2 = af.toBase(iterVec1);
	EXPECT_EQ(converted, converted2);

}

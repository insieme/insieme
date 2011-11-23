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

#include "insieme/analysis/polyhedral/constraint.h"

#include "insieme/utils/logging.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"

namespace insieme {
namespace analysis {
namespace poly {

//===== Constraint ================================================================================
typedef Constraint<AffineFunction> AffineConstraint;

ConstraintCombinerPtr<AffineFunction> normalize(const AffineConstraint& c) {
	const AffineConstraint::Type& type = c.getType();
	if ( type == AffineConstraint::EQ || type == AffineConstraint::GE ) { return makeCombiner(c); }

	if ( type == AffineConstraint::NE ) {
		// if the contraint is !=, then we convert it into a negation
		return not_( AffineConstraint(c.getFunction(), AffineConstraint::EQ) );
	}

	AffineFunction newAf( c.getFunction() );
	// we have to invert the sign of the coefficients 
	if(type == AffineConstraint::LT || type == AffineConstraint::LE) {
		for(AffineFunction::iterator it=c.getFunction().begin(), 
								     end=c.getFunction().end(); it!=end; ++it) 
		{
			newAf.setCoeff((*it).first, -(*it).second);
		}
	}
	if (type == AffineConstraint::LT || type == AffineConstraint::GT) {
		// we have to subtract -1 to the constant part
		newAf.setCoeff(Constant(), newAf.getCoeff(Constant())-1);
	}
	return makeCombiner( AffineConstraint(newAf, AffineConstraint::GE) );
}

Constraint<AffineFunction> toBase(const Constraint<AffineFunction>& c, const IterationVector& iterVec, const IndexTransMap& idxMap) {
	return Constraint<AffineFunction>( c.getFunction().toBase(iterVec, idxMap), c.getType() );
}

namespace {

//===== ConstraintCloner ==========================================================================
// because Constraints are represented on the basis of an iteration vector which is shared among the
// constraints componing a constraint combiner, when a combiner is stored, the iteration vector has
// to be changed. 
struct ConstraintCloner : public RecConstraintVisitor<AffineFunction> {
	ConstraintCombinerPtr<AffineFunction> newCC;
	const IterationVector& trg;
	const IterationVector* src;
	IndexTransMap transMap;

	ConstraintCloner(const IterationVector& trg) : trg(trg), src(NULL) { }

	void visit(const RawConstraintCombiner<AffineFunction>& rcc) { 
		const AffineConstraint& c = rcc.getConstraint();
		
		// we are really switching iteration vectors
		if (transMap.empty() ) {
			src = &c.getFunction().getIterationVector();
			transMap = transform( trg, *src );
		}

		assert(c.getFunction().getIterationVector() == *src);
		newCC = makeCombiner( toBase(c, trg, transMap) ); 
	}

	virtual void visit(const NegatedConstraintCombiner<AffineFunction>& ucc) {
		RecConstraintVisitor<AffineFunction>::visit(ucc);
		newCC = not_(newCC);
	}

	virtual void visit(const BinaryConstraintCombiner<AffineFunction>& bcc) {
		bcc.getLHS()->accept(*this);
		ConstraintCombinerPtr<AffineFunction> lhs = newCC;

		bcc.getRHS()->accept(*this);
		ConstraintCombinerPtr<AffineFunction> rhs = newCC;

		newCC = ConstraintCombinerPtr<AffineFunction>( 
				std::make_shared<BinaryConstraintCombiner<AffineFunction>>( bcc.getType(), lhs, rhs ) 
			);
	}
};

struct IterVecExtractor : public RecConstraintVisitor<AffineFunction> {
	
	const IterationVector* iterVec; 

	IterVecExtractor() : iterVec(NULL) { }

	void visit(const RawConstraintCombiner<AffineFunction>& rcc) { 
		const IterationVector& thisIterVec = rcc.getConstraint().getFunction().getIterationVector();
		if (iterVec == NULL) {
			iterVec = &thisIterVec;
		} 
		assert(*iterVec == thisIterVec); // FIXME use exceptions for this
	}
};

struct ConstraintConverter : public RecConstraintVisitor<AffineFunction> {
	
	core::NodeManager& mgr;
	core::IRBuilder   builder;
	core::ExpressionPtr ret;

	ConstraintConverter(core::NodeManager& mgr) : mgr(mgr), builder(mgr) { }

	void visit(const RawConstraintCombiner<AffineFunction>& rcc) { 
		const AffineConstraint& c = rcc.getConstraint();
		ret = toIR(mgr, c.getFunction());

		core::lang::BasicGenerator::Operator op;
		switch(c.getType()) {
			case AffineConstraint::GT: op = core::lang::BasicGenerator::Operator::Gt;
			case AffineConstraint::LT: op = core::lang::BasicGenerator::Operator::Lt;
			case AffineConstraint::EQ: op = core::lang::BasicGenerator::Operator::Eq;
			case AffineConstraint::NE: op = core::lang::BasicGenerator::Operator::Ne;
			case AffineConstraint::GE: op = core::lang::BasicGenerator::Operator::Ge;
			case AffineConstraint::LE: op = core::lang::BasicGenerator::Operator::Le;
		}
	
		ret = builder.callExpr( mgr.getLangBasic().getOperator(mgr.getLangBasic().getInt4(), op), ret, builder.intLit(0));

		assert( mgr.getLangBasic().isBool(ret->getType()) && "Type of a constraint must be of boolean type" );
	}

	virtual void visit(const NegatedConstraintCombiner<AffineFunction>& ucc) {
		ucc.getSubConstraint()->accept(*this);
		assert(ret && "Conversion of sub constraint went wrong");
		ret = builder.callExpr( mgr.getLangBasic().getBoolLNot(), ret);
	}

	virtual void visit(const BinaryConstraintCombiner<AffineFunction>& bcc) {
		bcc.getLHS()->accept(*this);
		assert(ret && "Conversion of sub constraint went wrong");
		core::ExpressionPtr lhs = ret;

		bcc.getRHS()->accept(*this);
		assert(ret && "Conversion of sub constraint went wrong");
		core::ExpressionPtr rhs = ret;

		core::lang::BasicGenerator::Operator op;
		switch (bcc.getType()) {
			case BinaryConstraintCombiner<AffineFunction>::AND: op = core::lang::BasicGenerator::Operator::LAnd;
			case BinaryConstraintCombiner<AffineFunction>::OR:  op = core::lang::BasicGenerator::Operator::LOr;
		}

		ret = builder.callExpr( mgr.getLangBasic().getOperator(mgr.getLangBasic().getBool(), op), 
				lhs, builder.createCallExprFromBody(builder.returnStmt(rhs), mgr.getLangBasic().getBool(), true) 
			);
		assert( mgr.getLangBasic().isBool(ret->getType()) && "Type of a constraint must be of boolean type" );
	}

};

struct CopyFromVisitor : public RecConstraintVisitor<AffineFunction> {
	
	ConstraintCombinerPtr<AffineFunction> curr;
	const poly::Element& src;
	const poly::Element& dest;

	CopyFromVisitor(const poly::Element& src, const poly::Element& dest) : 
		src(src), dest(dest) { }

	void visit(const RawConstraintCombiner<AffineFunction>& rcc) { 

		AffineFunction func = rcc.getConstraint().getFunction();
		int coeff = func.getCoeff(src);
		assert ( coeff != 0 );
	
		func.setCoeff(dest, coeff);
		func.setCoeff(src, 0);
		
		Constraint<AffineFunction> copy(func, rcc.getConstraint().getType());
		curr = makeCombiner( copy );
	}

	virtual void visit(const NegatedConstraintCombiner<AffineFunction>& ucc) {

		ucc.getSubConstraint()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		curr = not_(curr);
	}

	virtual void visit(const BinaryConstraintCombiner<AffineFunction>& bcc) {

		bcc.getLHS()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		ConstraintCombinerPtr<AffineFunction> lhs = curr;

		bcc.getRHS()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		ConstraintCombinerPtr<AffineFunction> rhs = curr;

		curr = bcc.getType() == BinaryConstraintCombiner<AffineFunction>::OR ? lhs or rhs : lhs and rhs; 
	}

};

} // end anonymous namespace 

ConstraintCombinerPtr<AffineFunction> 
cloneConstraint(const IterationVector& trgVec, const ConstraintCombinerPtr<AffineFunction>& old) {
	if (!old) { return ConstraintCombinerPtr<AffineFunction>(); }

	ConstraintCloner cc(trgVec);
	old->accept(cc);
	return cc.newCC;
}

const IterationVector& extractIterationVector(const ConstraintCombinerPtr<AffineFunction>& constraint) {
	assert( constraint && "Passing an empty constraint" );

	IterVecExtractor ive;
	constraint->accept(ive);

	assert(ive.iterVec != NULL);
	return *ive.iterVec;
}

insieme::core::ExpressionPtr toIR(core::NodeManager& mgr, const ConstraintCombinerPtr<AffineFunction>& c) {
	ConstraintConverter cconv(mgr);
	c->accept( cconv );
	assert ( cconv.ret && "Conversion of constraint failed" );
	return cconv.ret;
}

ConstraintCombinerPtr<AffineFunction> 
copyFromConstraint(const ConstraintCombinerPtr<AffineFunction>& cc, const poly::Element& src, const poly::Element& dest) {
	CopyFromVisitor cconv(src, dest);
	cc->accept( cconv );
	assert ( cconv.curr && "Conversion of constraint failed" );

	return cconv.curr;
}


} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 

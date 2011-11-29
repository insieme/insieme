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
AffineConstraintPtr normalize(const AffineConstraint& c) {
	const ConstraintType& type = c.getType();
	if ( type == ConstraintType::EQ || type == ConstraintType::GE ) { return makeCombiner(c); }

	if ( type == ConstraintType::NE ) {
		// if the contraint is !=, then we convert it into a negation
		return not_( AffineConstraint(c.getFunction(), ConstraintType::EQ) );
	}

	AffineFunction newAf( c.getFunction() );
	// we have to invert the sign of the coefficients 
	if(type == ConstraintType::LT || type == ConstraintType::LE) {
		for(AffineFunction::iterator it=c.getFunction().begin(), 
								     end=c.getFunction().end(); it!=end; ++it) 
		{
			newAf.setCoeff((*it).first, -(*it).second);
		}
	}
	if (type == ConstraintType::LT || type == ConstraintType::GT) {
		// we have to subtract -1 to the constant part
		newAf.setCoeff(Constant(), newAf.getCoeff(Constant())-1);
	}
	return makeCombiner( AffineConstraint(newAf, ConstraintType::GE) );
}

AffineConstraint toBase(const AffineConstraint& c, const IterationVector& iterVec, const IndexTransMap& idxMap) {
	return AffineConstraint( c.getFunction().toBase(iterVec, idxMap), c.getType() );
}

namespace {

//===== ConstraintCloner ==========================================================================
// because Constraints are represented on the basis of an iteration vector which is shared among the
// constraints componing a constraint combiner, when a combiner is stored, the iteration vector has
// to be changed. 
struct ConstraintCloner : public utils::RecConstraintVisitor<AffineFunction> {
	AffineConstraintPtr newCC;
	const IterationVector& trg;
	const IterationVector* src;
	IndexTransMap transMap;

	ConstraintCloner(const IterationVector& trg) : trg(trg), src(NULL) { }

	void visit(const utils::RawConstraintCombiner<AffineFunction>& rcc) { 
		const AffineConstraint& c = rcc.getConstraint();
		
		// we are really switching iteration vectors
		if (transMap.empty() ) {
			src = &c.getFunction().getIterationVector();
			transMap = transform( trg, *src );
		}

		assert(c.getFunction().getIterationVector() == *src);
		newCC = makeCombiner( toBase(c, trg, transMap) ); 
	}

	virtual void visit(const utils::NegatedConstraintCombiner<AffineFunction>& ucc) {
		RecConstraintVisitor<AffineFunction>::visit(ucc);
		newCC = not_(newCC);
	}

	virtual void visit(const BinaryAffineConstraint& bcc) {
		bcc.getLHS()->accept(*this);
		AffineConstraintPtr lhs = newCC;

		bcc.getRHS()->accept(*this);
		AffineConstraintPtr rhs = newCC;

		newCC = AffineConstraintPtr( std::make_shared<BinaryAffineConstraint>( bcc.getType(), lhs, rhs ) );
	}
};

struct IterVecExtractor : public utils::RecConstraintVisitor<AffineFunction> {
	
	const IterationVector* iterVec; 

	IterVecExtractor() : iterVec(NULL) { }

	void visit(const utils::RawConstraintCombiner<AffineFunction>& rcc) { 
		const IterationVector& thisIterVec = rcc.getConstraint().getFunction().getIterationVector();
		if (iterVec == NULL) {
			iterVec = &thisIterVec;
		} 
		assert(*iterVec == thisIterVec); // FIXME use exceptions for this
	}
};

struct ConstraintConverter : public utils::RecConstraintVisitor<AffineFunction> {
	
	core::NodeManager& mgr;
	core::IRBuilder   builder;
	core::ExpressionPtr ret;

	ConstraintConverter(core::NodeManager& mgr) : mgr(mgr), builder(mgr) { }

	void visit(const RawAffineConstraint& rcc) { 
		const AffineConstraint& c = rcc.getConstraint();
		ret = toIR(mgr, c.getFunction());

		core::lang::BasicGenerator::Operator op;
		switch(c.getType()) {
			case ConstraintType::GT: op = core::lang::BasicGenerator::Operator::Gt;
			case ConstraintType::LT: op = core::lang::BasicGenerator::Operator::Lt;
			case ConstraintType::EQ: op = core::lang::BasicGenerator::Operator::Eq;
			case ConstraintType::NE: op = core::lang::BasicGenerator::Operator::Ne;
			case ConstraintType::GE: op = core::lang::BasicGenerator::Operator::Ge;
			case ConstraintType::LE: op = core::lang::BasicGenerator::Operator::Le;
		}
	
		ret = builder.callExpr( mgr.getLangBasic().getOperator(mgr.getLangBasic().getInt4(), op), ret, builder.intLit(0));

		assert( mgr.getLangBasic().isBool(ret->getType()) && "Type of a constraint must be of boolean type" );
	}

	virtual void visit(const NegatedAffineConstraint& ucc) {
		ucc.getSubConstraint()->accept(*this);
		assert(ret && "Conversion of sub constraint went wrong");
		ret = builder.callExpr( mgr.getLangBasic().getBoolLNot(), ret);
	}

	virtual void visit(const BinaryAffineConstraint& bcc) {
		bcc.getLHS()->accept(*this);
		assert(ret && "Conversion of sub constraint went wrong");
		core::ExpressionPtr lhs = ret;

		bcc.getRHS()->accept(*this);
		assert(ret && "Conversion of sub constraint went wrong");
		core::ExpressionPtr rhs = ret;

		core::lang::BasicGenerator::Operator op;
		switch (bcc.getType()) {
			case BinaryAffineConstraint::AND: op = core::lang::BasicGenerator::Operator::LAnd;
			case BinaryAffineConstraint::OR:  op = core::lang::BasicGenerator::Operator::LOr;
		}

		ret = builder.callExpr( mgr.getLangBasic().getOperator(mgr.getLangBasic().getBool(), op), 
				lhs, builder.createCallExprFromBody(builder.returnStmt(rhs), mgr.getLangBasic().getBool(), true) 
			);
		assert( mgr.getLangBasic().isBool(ret->getType()) && "Type of a constraint must be of boolean type" );
	}

};

struct CopyFromVisitor : public utils::RecConstraintVisitor<AffineFunction> {
	
	AffineConstraintPtr curr;
	const poly::Element& src;
	const poly::Element& dest;

	CopyFromVisitor(const poly::Element& src, const poly::Element& dest) : 
		src(src), dest(dest) { }

	void visit(const RawAffineConstraint& rcc) { 

		AffineFunction func = rcc.getConstraint().getFunction();
		int coeff = func.getCoeff(src);
		assert ( coeff != 0 );
	
		func.setCoeff(dest, coeff);
		func.setCoeff(src, 0);
		
		AffineConstraint copy(func, rcc.getConstraint().getType());
		curr = makeCombiner( copy );
	}

	virtual void visit(const NegatedAffineConstraint& ucc) {

		ucc.getSubConstraint()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		curr = not_(curr);
	}

	virtual void visit(const BinaryAffineConstraint& bcc) {

		bcc.getLHS()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		AffineConstraintPtr lhs = curr;

		bcc.getRHS()->accept(*this);
		assert(curr && "Conversion of sub constraint went wrong");
		AffineConstraintPtr rhs = curr;

		curr = bcc.getType() == BinaryAffineConstraint::OR ? lhs or rhs : lhs and rhs; 
	}

};

} // end anonymous namespace 

AffineConstraintPtr cloneConstraint(const IterationVector& trgVec, const AffineConstraintPtr& old) {
	if (!old) { return AffineConstraintPtr(); }

	ConstraintCloner cc(trgVec);
	old->accept(cc);
	return cc.newCC;
}

const IterationVector& extractIterationVector(const AffineConstraintPtr& constraint) {
	assert( constraint && "Passing an empty constraint" );

	IterVecExtractor ive;
	constraint->accept(ive);

	assert(ive.iterVec != NULL);
	return *ive.iterVec;
}

insieme::core::ExpressionPtr toIR(core::NodeManager& mgr, const AffineConstraintPtr& c) {
	ConstraintConverter cconv(mgr);
	c->accept( cconv );
	assert ( cconv.ret && "Conversion of constraint failed" );
	return cconv.ret;
}

AffineConstraintPtr
copyFromConstraint(const AffineConstraintPtr& cc, const poly::Element& src, const poly::Element& dest) {
	CopyFromVisitor cconv(src, dest);
	cc->accept( cconv );
	assert ( cconv.curr && "Conversion of constraint failed" );

	return cconv.curr;
}


} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 

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

namespace insieme {
namespace analysis {
namespace poly {

//===== Constraint ================================================================================
std::ostream& Constraint::printTo(std::ostream& out) const { 
	out << af << " ";
	switch(type) {
	case EQ: out << "==";  break;	case NE: out << "!=";  break; 	case GT: out << ">";   break;
	case LT: out << "<";   break;	case GE: out << ">=";  break;	case LE: out << "<=";  break;
	}
	return out << " 0";
}

bool Constraint::operator<(const Constraint& other) const {
	if (af.size() == other.af.size()) {	return type < other.type; }
	return af.size() < other.af.size(); 
}

Constraint Constraint::toBase(const IterationVector& iterVec, const IndexTransMap& idxMap) const {
	return Constraint( af.toBase(iterVec, idxMap), type );
}

ConstraintCombinerPtr normalize(const Constraint& c) {
	const Constraint::Type& type = c.getType();
	if ( type == Constraint::EQ || type == Constraint::GE ) { return makeCombiner(c); }

	if ( type == Constraint::NE ) {
		// if the contraint is !=, then we convert it into a negation
		return not_( Constraint(c.getAffineFunction(), Constraint::EQ) );
	}

	AffineFunction newAf( c.getAffineFunction() );
	// we have to invert the sign of the coefficients 
	if(type == Constraint::LT || type == Constraint::LE) {
		for(AffineFunction::iterator it=c.getAffineFunction().begin(), 
								     end=c.getAffineFunction().end(); it!=end; ++it) 
		{
			newAf.setCoeff((*it).first, -(*it).second);
		}
	}
	if (type == Constraint::LT || type == Constraint::GT) {
		// we have to subtract -1 to the constant part
		newAf.setCoeff(Constant(), newAf.getCoeff(Constant())-1);
	}
	return makeCombiner( Constraint(newAf, Constraint::GE) );
}

//===== ConstraintCombiner ========================================================================

void RawConstraintCombiner::accept(ConstraintVisitor& v) const { v.visit(*this); }
void NegatedConstraintCombiner::accept(ConstraintVisitor& v) const { v.visit(*this); }
void BinaryConstraintCombiner::accept(ConstraintVisitor& v) const { v.visit(*this); }

namespace {

//===== ConstraintPrinter =========================================================================
// Visits the constraints and prints the expression to a provided output stream
struct ConstraintPrinter : public ConstraintVisitor {
	
	std::ostream& out;

	ConstraintPrinter(std::ostream& out) : out(out) { }

	void visit(const RawConstraintCombiner& rcc) { out << "(" << rcc.getConstraint() << ")"; }

	virtual void visit(const NegatedConstraintCombiner& ucc) {
		out << "NOT"; ConstraintVisitor::visit(ucc);
	}

	virtual void visit(const BinaryConstraintCombiner& bcc) {
		out << "(";
		bcc.getLHS()->accept(*this);
		out << (bcc.isConjunction() ? " AND " : " OR ");
		bcc.getRHS()->accept(*this);
		out << ")";
	}

};

} // end anonymous namespace

std::ostream& ConstraintCombiner::printTo(std::ostream& out) const {
	ConstraintPrinter vis(out);
	accept( vis );
	return out;
} 

ConstraintCombinerPtr makeCombiner(const Constraint& constr) {
	return std::make_shared<RawConstraintCombiner>(constr);
}

ConstraintCombinerPtr makeCombiner(const ConstraintCombinerPtr& cc) { return cc; }

namespace {

//===== ConstraintCloner ==========================================================================
// because Constraints are represented on the basis of an iteration vector which is shared among the
// constraints componing a constraint combiner, when a combiner is stored, the iteration vector has
// to be changed. 
struct ConstraintCloner : public ConstraintVisitor {
	ConstraintCombinerPtr newCC;
	const IterationVector& trg;
	const IterationVector* src;
	IndexTransMap transMap;

	ConstraintCloner(const IterationVector& trg) : trg(trg), src(NULL) { }

	void visit(const RawConstraintCombiner& rcc) { 
		const Constraint& c = rcc.getConstraint();
		
		// we are really switching iteration vectors
		if (transMap.empty() ) {
			src = &c.getAffineFunction().getIterationVector();
			transMap = transform( trg, *src );
		}

		assert(c.getAffineFunction().getIterationVector() == *src);
		newCC = std::make_shared<RawConstraintCombiner>( c.toBase(trg, transMap) ); 
	}

	virtual void visit(const NegatedConstraintCombiner& ucc) {
		ConstraintVisitor::visit(ucc);
		newCC = not_(newCC);
	}

	virtual void visit(const BinaryConstraintCombiner& bcc) {
		bcc.getLHS()->accept(*this);
		ConstraintCombinerPtr lhs = newCC;

		bcc.getRHS()->accept(*this);
		ConstraintCombinerPtr rhs = newCC;

		newCC = std::make_shared<BinaryConstraintCombiner>( bcc.getType(), lhs, rhs );
	}
};

} // end anonymous namespace 

ConstraintCombinerPtr cloneConstraint(const IterationVector& trgVec, const ConstraintCombinerPtr& old) {
	if (!old) { return ConstraintCombinerPtr(); }

	ConstraintCloner cc(trgVec);
	old->accept(cc);
	return cc.newCC;
}

} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 

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

#include "insieme/analysis/polyhedral.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include <iomanip>

namespace insieme {
namespace analysis {
namespace poly {
//====== Exceptions ==========================================================
NotAffineExpr::NotAffineExpr(const core::ExpressionPtr& expr): 
		std::logic_error("Expression is not linear and affine"), expr(expr) { }

VariableNotFound::VariableNotFound(const core::VariablePtr& var) : 
		std::logic_error("Variable not found in the iteration vector."), var(var) { }


//====== Element =============================================================

bool Element::operator==(const Element& other) const {
	if (this == &other) { return true; }

	if (type == other.type) {
		if(type == ITER || type == PARAM) 
			return *static_cast<const Variable&>(*this).getVariable() == 
				   *static_cast<const Variable&>(other).getVariable();
		else 
			return true;
	}
	return false;
}

std::ostream& Iterator::printTo(std::ostream& out) const { return out << *getVariable(); }

std::ostream& Parameter::printTo(std::ostream& out) const { return out << *getVariable(); }


//====== IterationVector ======================================================

int IterationVector::getIdx(const Element& elem) const {
	if (const Iterator* iter = dynamic_cast<const Iterator*>(&elem)) {
		return getIdxFrom(*iter, iters);
	}
	if (const Parameter* param = dynamic_cast<const Parameter*>(&elem)) {
		int idx = getIdxFrom(*param, params);
		return (idx==-1?-1:idx+iters.size());	
	}
	assert( dynamic_cast<const Constant*>(&elem) != NULL && "Element not valid." ); 
	return size()-1;
}

const Element& IterationVector::operator[](size_t idx) const { 
	assert(idx >= 0 && idx < size() && "Index out of range");
	if (idx<getIteratorNum()) {
		return iters[idx];
	} 
	if (idx<size()-1) {
		return params[idx-iters.size()];
	}
	return constant;
}

bool IterationVector::operator==(const IterationVector& other) const {
	if (this == &other) {
		return true;
	}
	// check weather the two iterators contain the same elements in the same
	// order
	return std::equal(begin(), end(), other.begin());
}

std::ostream& IterationVector::printTo(std::ostream& out) const {
	return out << join(",", begin(), end(), [&](std::ostream& jout, const Element& cur){ jout << cur; } );
}

// Merges two iteration vectors (a and b) to create a new iteration vector which contains
// both the elements of a and b. 
IterationVector merge(const IterationVector& a, const IterationVector& b) {
	IterationVector::iter_iterator aIt = a.iter_begin(), aEnd = a.iter_end(), bIt = b.iter_begin(), bEnd = b.iter_end();
	IterationVector ret;

	// because the two iteration vectors are built bottom-up, the iterators in a will not be b and viceversa
	// having the same iterators would mean the same variable has been used as loop iterator index in 1  statement
	// as a parameter in another, therefore we can safely remove the iterators and merge the set of parameters. 
		
	ret.add( *aIt );
		
}

//====== IterationVector::iterator =============================================

void IterationVector::iterator::inc(size_t n) {
	if (!valid || n==0) return;

	IterVec::const_iterator&& iterEnd = iterVec.iters.end();
	size_t dist = std::distance(iterIt, iterEnd);
	if (iterIt != iterEnd && dist>=n) {
		iterIt+=n;
		return;
	}
	iterIt = iterEnd;
	n-=dist;

	ParamVec::const_iterator&& paramEnd = iterVec.params.end();
	dist = std::distance(paramIt, paramEnd);
	if (paramIt != paramEnd && dist>=n) {
		paramIt+=n;
		return;
	}
	paramIt = paramEnd;
	n-=dist;

	if (constant) { 
		constant = false;
		valid = false;
	}
}

const Element& IterationVector::iterator::operator*() const {  
	if (!valid) 
		throw IteratorNotValid();

	if (iterIt != iterVec.iters.end())
		return *iterIt;
	else if (paramIt != iterVec.params.end())
		return *paramIt;
	assert(constant && "Iteration vector has no constant part");
	return iterVec.constant;
}

//====== AffineFunction ========================================================

AffineFunction::AffineFunction(IterationVector& iterVec, const insieme::core::ExpressionPtr& expr) : 
	iterVec(iterVec), sep(iterVec.getIteratorNum())
{
	using namespace insieme::core::arithmetic;
	// extract the Formula object 
	Formula&& formula = toFormula(expr);
	
	if ( !(formula.isLinear() || formula.isOne()) ) 
		throw NotAffineExpr(expr);

	if ( formula.isOne() ) {
		coeffs.resize(iterVec.size()); // by default the values are initialized to 0
		coeffs.back() = 1;	
		return;
	}

	// this is a linear function
	assert( formula.isLinear() && "Expression is not an affine linear function.");
	
	const std::vector<Formula::Term>& terms = formula.getTerms();
	// we have to updated the iteration vector by adding eventual parameters
	// which are being used by this function. Because by looking to an
	// expression we cannot determine if a variable is an iterator or a
	// parameter we assume that variables in this expression which do not appear
	// in the iteration domain are parameters.
	for_each( terms.begin(), terms.end(), [&](const Formula::Term& cur){ 
		const Product& prod = cur.first;
		assert(prod.getFactors().size() <= 1 && "Not a linear expression");

		if ( !prod.isOne() ) {
			const core::VariablePtr& var = prod.getFactors().front().first;
			// We make sure the variable is not already among the iterators
			if ( iterVec.getIdx( Iterator(var) ) == -1 ) {
				iterVec.add( Parameter(var) );
			}
		}
	});

	// now the iteration vector is inlined with the Formula object extracted
	// from the expression, the size of the coefficient vector can be set.
	coeffs.resize(iterVec.size());
	for_each( terms.begin(), terms.end(), [&](const Formula::Term& cur){ 
		const Product& prod = cur.first;
		assert(prod.getFactors().size() <= 1 && "Not a linear expression");

		if ( prod.isOne() ) {
			coeffs.back() = cur.second;
		} else {
			int idx = iterVec.getIdx( prod.getFactors().front().first );
			assert (idx != -1);
			coeffs[idx] = cur.second;
		}
	});
}

AffineFunction::AffineFunction(const IterationVector& newIterVec, const AffineFunction& other) : 
	iterVec(newIterVec)
{
	// check weather the 2 iteration vectors are compatible. 
	if(iterVec != other.iterVec) 
		throw "Operation not allowed, iteration vectors are different";

	// FIXME: allow the copy of iteration vector which are not perfectly the
	// same but have the same structure 
	
	coeffs = other.coeffs;
	sep = other.sep;
}

int AffineFunction::idxConv(size_t idx) const {

	if(idx<sep)	{ return idx; }
	if(idx == iterVec.size()-1) { return coeffs.size()-1; }

	if(idx>=sep && idx<iterVec.getIteratorNum()) { return -1; }

	idx -= iterVec.getIteratorNum();
	if(idx<coeffs.size()-sep-1)
		return sep+idx;
	
	return -1;
}

std::ostream& AffineFunction::printTo(std::ostream& out) const { 
	return out << join(" + ", begin(), end(), [&](std::ostream& jout, const Term& cur){ jout << cur; } );
}

int AffineFunction::getCoeff(const core::VariablePtr& var) const {
	int idx = iterVec.getIdx(var);
	// In the case the variable is not in the iteration vector, throw an
	// exception
	if (idx == -1) {
		throw VariableNotFound(var); 
	}
	idx = idxConv(idx);
	return idx==-1?0:coeffs[idx];
}

bool AffineFunction::operator==(const AffineFunction& other) const {
	// in the case the iteration vector is the same, then we look at the
	// coefficients and the separator value to determine if the two functions
	// are the same.
	if(iterVec == other.iterVec)
		return sep == other.sep && std::equal(coeffs.begin(), coeffs.end(), other.coeffs.begin());

	// if the two iteration vector are not the same we need to determine if at
	// least the position for which a coefficient is specified is the same 	
	iterator thisIt = begin(), otherIt = other.begin(); 
	while( thisIt!=end() ) {
		if( ((*thisIt).first.getType() == Element::PARAM && (*otherIt).first.getType() == Element::ITER) || 
			((*thisIt).first.getType() == Element::CONST && (*otherIt).first.getType() != Element::CONST) ) {
			if ((*otherIt).second != 0) { return false; }
			++otherIt;
		} else if( ((*thisIt).first.getType() == Element::ITER && (*otherIt).first.getType() == Element::PARAM) || 
			((*thisIt).first.getType() != Element::CONST && (*otherIt).first.getType() == Element::CONST) )	{
			if ((*thisIt).second != 0) { return false; }
			++thisIt;
		} else if(*thisIt==*otherIt) {
			// iterators aligned 
			++thisIt;
			++otherIt;
		} else {
			return false;
		}
	}	
	return true;
}

//====== AffineFunction::iterator =================================================

AffineFunction::iterator& AffineFunction::iterator::operator++() { 
	if ( iterPos == iterVec.size() )
		throw "Iterator not valid.";

	iterPos++;
	return *this;
}

AffineFunction::Term AffineFunction::iterator::operator*() const {  
	if ( iterPos == iterVec.size() )
		throw "Iterator not valid.";

	int idx = af.idxConv(iterPos);
	return Term(iterVec[iterPos], idx==-1?0:af.coeffs[idx]);
}

//===== Constraint ==============================================================
std::ostream& Constraint::printTo(std::ostream& out) const { 
	out << af << " ";
	switch(type) {
	case EQ: out << "==";  break;	case NE: out << "!=";  break; 	case GT: out << ">";   break;
	case LT: out << "<";   break;	case GE: out << ">=";  break;	case LE: out << "<=";  break;
	}
	return out << " 0";
}

}
}
}

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::poly::AffineFunction::Term& c) {
	return out << c.second << "*" << c.first;
}

}


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

#include "insieme/analysis/polyhedral/affine_func.h"

#include <set>

#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"

namespace {

using namespace insieme;

// Remove expression which are used in the IR for semantics checks (like derefs and refs)
core::ExpressionPtr removeSugar(core::ExpressionPtr expr) {
	const core::NodeManager& mgr = expr->getNodeManager();
	
	while (expr->getNodeType() == core::NT_CallExpr &&
		   core::analysis::isCallOf(core::static_pointer_cast<const core::CallExpr>(expr), mgr.getLangBasic().getRefDeref())) {

			expr = core::static_pointer_cast<const core::CallExpr>(expr)->getArgument(0);	
	}

	return expr;
}

} // end anonymous namespace

namespace insieme { namespace analysis { namespace polyhedral {

//====== Exceptions ===============================================================================
VariableNotFound::VariableNotFound(const core::VariablePtr& var) : 
                std::logic_error("Variable not found in the iteration vector."), var(var) { }

void AffineFunction::buildFromFormula(IterationVector& iterVec, const insieme::core::arithmetic::Formula& formula) {
	using namespace insieme::core::arithmetic;

	if ( formula.isOne() ) {
		coeffs.resize(iterVec.size()); // by default the values are initialized to 0
		coeffs.back() = 1;	
		return;
	}

		
	const std::vector<Formula::Term>& terms = formula.getTerms();
	// we have to updated the iteration vector by adding eventual parameters which are being used by
	// this function. Because by looking to an expression we cannot determine if a variable is an
	// iterator or a parameter we assume that variables in this expression which do not appear in
	// the iteration domain are parameters.
	for_each( terms.begin(), terms.end(), [&](const Formula::Term& cur){ 
		const Product& prod = cur.first;
		assert(prod.getFactors().size() <= 1 && "Not a linear expression");

		if ( !prod.isOne() ) {
			core::ExpressionPtr&& var = removeSugar(prod.getFactors().front().first);
			// we get rid of eventual deref operations occurring 
						// We make sure the variable is not already among the iterators
			if ( var->getNodeType() != core::NT_Variable || 
					iterVec.getIdx( Iterator(core::static_pointer_cast<const core::Variable>(var)) ) == -1 ) 
			{
				iterVec.add( Parameter(var) );
			}
		}
	});

	// now the iteration vector is inlined with the Formula object extracted from the expression,
	// the size of the coefficient vector can be set.
	coeffs.resize(iterVec.size());
	for_each( terms.begin(), terms.end(), [&](const Formula::Term& cur){ 
		const Product& prod = cur.first;
		assert(prod.getFactors().size() <= 1 && "Not a linear expression");

		assert (cur.second.isInteger());
		if ( prod.isOne() ) {
			coeffs.back() = cur.second.getNumerator();
		} else {
			int idx = iterVec.getIdx( removeSugar(prod.getFactors().front().first));
			assert (idx != -1);
			coeffs[idx] = cur.second.getNumerator();
		}
	});
}

//====== AffineFunction ===========================================================================

AffineFunction::AffineFunction(IterationVector& iterVec, const insieme::core::ExpressionPtr& expr) : 
	iterVec(iterVec), sep(iterVec.getIteratorNum())
{
	core::arithmetic::Formula&& formula = core::arithmetic::toFormula(expr);

	if ( !(formula.isLinear() || formula.isOne()) ) 
		throw NotAffineExpr(expr);

	buildFromFormula( iterVec, formula );
}

AffineFunction::AffineFunction(IterationVector& iterVec, const insieme::core::arithmetic::Formula& formula) : 
	iterVec(iterVec), sep(iterVec.getIteratorNum())
{
	// this is a linear function
	if( !formula.isLinear() ) throw NotAffineExpr( core::ExpressionPtr() );

	buildFromFormula( iterVec, formula);
}

int AffineFunction::idxConv(size_t idx) const {

	if (idx<sep)	{ return idx; }
	if (idx == iterVec.size()-1) { return coeffs.size()-1; }

	if (idx>=sep && idx<iterVec.getIteratorNum()) { return -1; }

	idx -= iterVec.getIteratorNum();
	if (idx<coeffs.size()-sep-1) {
		return sep+idx;
	}
	
	return -1;
}

bool AffineFunction::operator<(const AffineFunction& other) const {
	if (getIterationVector() == other.getIterationVector()) {
		
		auto thisIt = begin(), thisEnd = end();
		auto otherIt = other.begin(), otherEnd = other.end();

		assert((std::distance(thisIt, thisEnd) == std::distance(otherIt, otherEnd)) && 
				"size of 2 iterators differs");

		while(thisIt != thisEnd) {
			assert((*thisIt).first == (*otherIt).first);
			if ((*thisIt).second > (*otherIt).second)
				return false;
			if ((*thisIt).second < (*otherIt).second) 
				return true;
			
			assert((*thisIt).second == (*otherIt).second);
			++thisIt; ++otherIt;
		}
		// If we end up here it means the 2 functions have same coefficients 
		return false;
	}

	return getIterationVector() < other.getIterationVector();
}

// Converts an AffineFunction to an IR expression
insieme::core::ExpressionPtr toIR(insieme::core::NodeManager& mgr, const AffineFunction& af) {

	auto&& filtered = filterIterator<
					AffineFunction::iterator, 
					AffineFunction::Term, 
					AffineFunction::Term*, 
					AffineFunction::Term
		>(af.begin(), af.end(), [](const AffineFunction::Term& cur) -> bool { return cur.second == 0; }
	);

	insieme::core::IRBuilder builder(mgr);

	core::ExpressionPtr ret;
	for_each(filtered.first, filtered.second, [&] (const AffineFunction::Term& t) {
		core::ExpressionPtr currExpr;
		assert(t.second != 0 && "0 coefficient not filtered out correctly");

		if (t.first.getType() != Element::CONST) {
			core::ExpressionPtr expr = static_cast<const Expr&>(t.first).getExpr();
			// Check whether the expression is of Ref Type
			if (expr->getType()->getNodeType() == core::NT_RefType) {
				expr = builder.deref( expr );
			}
			assert(expr->getType()->getNodeType() != core::NT_RefType && "Operand cannot be of Ref Type");

			// Check whether the expression is of type signed int
			if ( !mgr.getLangBasic().isInt4( expr->getType() ) ) {
				expr = builder.castExpr( mgr.getLangBasic().getInt4(), expr );
			}
			currExpr = t.second == 1 ? expr : 
						builder.callExpr( mgr.getLangBasic().getSignedIntMul(), builder.intLit(t.second), expr );
		} else {
			// This is the constant part, therefore there are no variables to consider, just the
			// integer value 
			currExpr = builder.intLit(t.second);
		}

		if (!ret) {
			ret = currExpr;
			return;
		}

		ret = builder.callExpr( mgr.getLangBasic().getSignedIntAdd(), ret, currExpr );
	});
	
	if (!ret) {
		// it means there where no positive coefficients in this affine expression, 
		// therefore the value is 0
		return builder.intLit(0);
	}
	return ret;
}


namespace {
/**
 * Printer: prints affine functions using different styles which can be selected by policies which can are 
 * specified by the user.
 */
struct Printer : public utils::Printable {

	Printer(const AffineFunction& af, unsigned policy) : af(af), policy(policy) { }

	bool doPrintZeros() const { return policy & AffineFunction::PRINT_ZEROS; }

	bool doPrintVars() const { return policy & AffineFunction::PRINT_VARS; }

	template <class IterT>
	void print(std::ostream& out, const IterT& begin, const IterT& end) const {
		bool isEmpty = true;
		out << join((doPrintVars() ? " + " : " "), begin, end, 
				[&](std::ostream& jout, const AffineFunction::Term& cur) { 
					if ( doPrintVars() ) jout << cur;
					else jout << cur.second; 
					isEmpty = false;
				} 
			);
		if(isEmpty) {
			// If the we didn't produce any output it means the affine constraint is all zeros,
			// print the constant part to visualize the real value
			assert(af.getCoeff(Constant()) == 0);
			out << 0;
		}
	}

	std::ostream& printTo(std::ostream& out) const {

		if (!doPrintZeros()) { 
			auto&& filtered = filterIterator<
					AffineFunction::iterator, 
					AffineFunction::Term, 
					AffineFunction::Term*, 
					AffineFunction::Term
				>(af.begin(), af.end(), [](const AffineFunction::Term& cur) -> bool { return cur.second == 0; }
			);
			print(out, filtered.first, filtered.second);
		} else {
			print(out, af.begin(), af.end());
		}

		return out;

	}
		
private:
	const AffineFunction& af;
	const unsigned policy;
};

} // end anonymous namespace


std::string AffineFunction::toStr(unsigned policy) const {
	std::ostringstream ss;
	ss << Printer(*this, policy);
	return ss.str();
}

std::ostream& AffineFunction::printTo(std::ostream& out) const { 
	return out << toStr( AffineFunction::PRINT_VARS );
}

int AffineFunction::getCoeff(size_t idx) const {
	int index = idxConv(idx);
	return (index==-1)?0:coeffs[index];
}

void AffineFunction::setCoeff(size_t idx, int coeff) { 
	int new_idx = idxConv(idx);

	// We are trying to set an index for an element of the interation vector which was inserted
	// after this affine function was constructed 
	if (new_idx == -1) {
		std::vector<int> new_coeffs;
		std::copy( coeffs.begin(), coeffs.begin()+sep, std::back_inserter(new_coeffs) );

		if( idx < iterVec.getIteratorNum() ) {
			// if this is an iterator coeff, add elements to the coeffs vector before the sep
			for(size_t i=sep; i<idx; i++) {
				new_coeffs.push_back(0);
			}
			new_coeffs.push_back(coeff);
		}
		std::copy( coeffs.begin()+sep, coeffs.end()-1, std::back_inserter(new_coeffs) );

		if( idx >= iterVec.getIteratorNum() ) {
			// this is a parameter coeff
			for(size_t i=coeffs.size()-1; i<idx; i++) {
				new_coeffs.push_back(0);
			}
			new_coeffs.push_back(coeff);
		}
		// Add the constant part
		new_coeffs.push_back(coeffs.back()); 

		// Perform checks and update internal representation
		if( idx < iterVec.getIteratorNum() ) {
			assert(new_coeffs.size() == coeffs.size()+(idx-(sep-1)));
			sep = idx+1;
		} else {				
			assert(new_coeffs.size() == coeffs.size()+(idx-(coeffs.size()-2)));
		}

		coeffs = new_coeffs;
		return;
	}
	coeffs[new_idx] = coeff; 
}


void AffineFunction::setCoeff(const core::VariablePtr& var, int coeff) { 
	int idx = iterVec.getIdx(var);
	// In the case the variable is not in the iteration vector, throw an exception
	if (idx == -1) { throw VariableNotFound(var); }
	setCoeff( idx, coeff );
}

int AffineFunction::getCoeff(const Element& elem) const {
	int idx = iterVec.getIdx(elem);
	// In the case the variable is not in the iteration vector, throw an exception
	assert (idx != -1 && "Element not in iteration vector");
	return getCoeff( idx );
}

int AffineFunction::getCoeff(const core::VariablePtr& var) const { 
	int idx = iterVec.getIdx(var);
	// In the case the variable is not in the iteration vector, throw an exception
	if (idx == -1) { throw VariableNotFound(var); }
	return getCoeff( idx );
}

bool AffineFunction::operator==(const AffineFunction& other) const {
	// in the case the iteration vector is the same, then we look at the coefficients and the
	// separator value to determine if the two functions are the same.
	if(iterVec == other.iterVec)
		return sep == other.sep && std::equal(coeffs.begin(), coeffs.end(), other.coeffs.begin());

	// if the two iteration vector are not the same we need to determine if at least the position
	// for which a coefficient is specified is the same 
	auto&& this_filt = 
		filterIterator<
			AffineFunction::iterator, 
			AffineFunction::Term, 
			AffineFunction::Term*, 
			AffineFunction::Term
		>(begin(), end(), [](const AffineFunction::Term& cur) -> bool { 
				return cur.second == 0; 
		});	
	auto&& other_filt = 
		filterIterator<
			AffineFunction::iterator, 
			AffineFunction::Term, 
			AffineFunction::Term*, 
			AffineFunction::Term
		>(other.begin(), other.end(), [](const AffineFunction::Term& cur) -> bool { 
				return cur.second == 0; 
		});	
	
	std::set<AffineFunction::Term> diff;
	std::set_difference(
		this_filt.first, this_filt.second, other_filt.first, other_filt.second, 
		std::inserter(diff, diff.begin())
	);
	
	return diff.empty();
}

AffineFunction AffineFunction::toBase(const IterationVector& iterVec, const IndexTransMap& idxMap) const {
	assert(iterVec.size() >= this->iterVec.size());
	
	IndexTransMap idxMapCpy = idxMap;
	if ( idxMap.empty() ) {
		idxMapCpy = transform(iterVec, this->iterVec);
	}
	AffineFunction ret(iterVec);
	std::fill(ret.coeffs.begin(), ret.coeffs.end(), 0);

	for(size_t it=0; it<this->iterVec.size(); ++it) { 
		ret.coeffs[idxMapCpy[it]] = getCoeff(it);
	}

	return ret;
}


AffineFunction::operator core::arithmetic::Formula() const {
	using insieme::core::arithmetic::Formula;
	using insieme::core::arithmetic::Value;

	Formula res;

	auto&& filtered = filterIterator<
				AffineFunction::iterator, 
				AffineFunction::Term, 
				AffineFunction::Term*, 
				AffineFunction::Term
	>(begin(), end(), [](const AffineFunction::Term& cur) -> bool { return cur.second == 0; });

	for_each(filtered.first, filtered.second, [&](const AffineFunction::Term& cur) { 
		if(cur.first.getType() == Element::ITER || cur.first.getType() == Element::PARAM) {
			core::ExpressionPtr expr = static_cast<const Expr&>(cur.first).getExpr();
			if (expr->getType()->getNodeType() == core::NT_RefType) {
				// Refs cannot appear in a formula, therefore we need to deref them
				expr = core::IRBuilder(expr->getNodeManager()).deref(expr);
			}
			res = res + (Value(expr) * cur.second);
			return;
		}
		assert(cur.first.getType() == Element::CONST);
		res = res + Formula(cur.second);
	});

	return res;
}

//====== AffineFunction::iterator =================================================================

AffineFunction::iterator& AffineFunction::iterator::operator++() { 
	assert ( iterPos < iterVec.size() && "Iterator not valid!"); 
	
	iterPos++;
	return *this;
}

AffineFunction::Term AffineFunction::iterator::operator*() const {  
	assert ( iterPos < iterVec.size() && "Iterator not valid!"); 

	return Term(iterVec[iterPos], af.getCoeff(iterPos));	
}

} } } // end insieme::analysis::polyhedral namespace

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::polyhedral::AffineFunction::Term& c) {
	// Avoid to print the coefficient when it is 1 or -1
	if (abs(c.second) != 1) { out << c.second << "*"; }
	if (c.second == -1) { out << "-"; }

	return out << c.first;
}

}


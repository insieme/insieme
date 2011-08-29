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

#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/analysis/ir_utils.h"

namespace {

using namespace insieme;

// Remove expression which are used in the IR for semantics checks (like derefs and refs)
core::ExpressionPtr removeSugar(core::ExpressionPtr expr) {
	const core::NodeManager& mgr = expr->getNodeManager();
	
	while (expr->getNodeType() == core::NT_CallExpr &&
		   core::analysis::isCallOf(core::static_pointer_cast<const core::CallExpr>(expr), mgr.basic.getRefDeref())) {

			expr = core::static_pointer_cast<const core::CallExpr>(expr)->getArgument(0);	
	}

	return expr;
}

} // end anonymous namespace

namespace insieme {
namespace analysis {
namespace poly {

//====== Exceptions ===============================================================================
VariableNotFound::VariableNotFound(const core::VariablePtr& var) : 
                std::logic_error("Variable not found in the iteration vector."), var(var) { }

//====== AffineFunction ===========================================================================

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

		if ( prod.isOne() ) {
			coeffs.back() = cur.second;
		} else {
			int idx = iterVec.getIdx( removeSugar(prod.getFactors().front().first));
			assert (idx != -1);
			coeffs[idx] = cur.second;
		}
	});
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

} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::poly::AffineFunction::Term& c) {
	return out << c.second << "*" << c.first;
}

}


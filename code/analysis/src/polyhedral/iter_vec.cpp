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

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/utils/logging.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme { namespace analysis { namespace polyhedral {

//====== Element ==================================================================================

bool Element::operator==(const Element& other) const {
	if (this == &other) { return true; }

	if (type == other.type) {
		if(type == ITER || type == PARAM) {
			return *static_cast<const Expr&>(*this).getExpr() == 
				   *static_cast<const Expr&>(other).getExpr();
		} 
		return true;
	}
	return false;
}

bool Element::operator<(const Element& other) const {
	// order based on the type
	if (type != other.type) {
		return type < other.type;
	}
	
	if (type == Element::ITER || type == Element::PARAM) {
		return  static_cast<const Expr&>(*this).getExpr() < 
				static_cast<const Expr&>(other).getExpr();
	}
	
	return false;
}
	
std::ostream& Iterator::printTo(std::ostream& out) const { 
	return out << *getVariable();
}

namespace {

std::ostream& prettyPrintExpr( std::ostream& out, const core::ExpressionPtr& expr ) {
	using namespace insieme;

	core::NodeManager& mgr = expr->getNodeManager();
	if ( expr->getNodeType() == core::NT_CallExpr ) {
		const core::CallExprPtr& callExpr = core::static_pointer_cast<const core::CallExpr>(expr);

		if( core::analysis::isCallOf(callExpr, mgr.getLangBasic().getArraySubscript1D()) ||
			core::analysis::isCallOf(callExpr, mgr.getLangBasic().getArrayRefElem1D()) || 
			core::analysis::isCallOf(callExpr, mgr.getLangBasic().getVectorRefElem()) || 
			core::analysis::isCallOf(callExpr, mgr.getLangBasic().getVectorSubscript()) ) 
		{
			prettyPrintExpr( out, callExpr->getArgument(0) );
			out << "[";
			prettyPrintExpr( out, callExpr->getArgument(1) );
			return out << "]";
		}

		if (core::analysis::isCallOf(callExpr, mgr.getLangBasic().getCompositeMemberAccess()) || 
			core::analysis::isCallOf(callExpr, mgr.getLangBasic().getCompositeRefElem() )) 
		{
			prettyPrintExpr( out, callExpr->getArgument(0) );
			out << "."; 
			return prettyPrintExpr( out, callExpr->getArgument(1) );
		}
	}

	if ( expr->getNodeType() == core::NT_CastExpr )
		return prettyPrintExpr( out, core::static_pointer_cast<const core::CastExpr>(expr)->getSubExpression() );

	return out << *expr;
}

} // end anonymous namespace 

std::ostream& Parameter::printTo(std::ostream& out) const { 
	return prettyPrintExpr(out, getExpr());
}

//====== IterationVector ==========================================================================

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
	// check weather the two iterators contain the same elements in the same order
	return std::equal(begin(), end(), other.begin());
}

// An iteration vector is represented by three main components, the iterators, the parameters and
// the constant part. The vector is printed displaying the comma separated list of iterators and
// parameters divided by the '|' separator. 
std::ostream& IterationVector::printTo(std::ostream& out) const {
	out << "(" << join(",", iter_begin(), iter_end(), 
			[&](std::ostream& jout, const Element& cur){ jout << cur; } 
		);
	out << "|";
	out << join(",", param_begin(), param_end(), 
			[&](std::ostream& jout, const Element& cur){ jout << cur; } 
		);
	out << "|1)";
	return out;
}

bool IterationVector::contains(const core::ExpressionPtr& expr) const {  
	return getIdx(expr)!=-1; 
}

bool IterationVector::operator<(const IterationVector& other) const {
	if (size() > other.size()) return false;
	if (size() < other.size()) return true;

	// same size
	auto thisIt = begin(), thisEnd = end();
	auto otherIt = other.begin();

	while(thisIt != thisEnd) {
		if (*thisIt >= *otherIt) return false;
		++thisIt; ++otherIt;
	}
	return true;
}

namespace {
template <class T>
void add_if(IterationVector& dest, 
		typename std::vector<T>::const_iterator aBegin, 
		typename std::vector<T>::const_iterator aEnd ) 
{
	std::for_each(aBegin, aEnd, [&dest] (const T& cur) { 
		if (dest.getIdx( cur.getExpr() ) == -1 ) { 
			dest.add(cur); 
		}
	} );
}
		
template <class T>
void merge_add(IterationVector& dest, 
		typename std::vector<T>::const_iterator aBegin, 
		typename std::vector<T>::const_iterator aEnd, 
		typename std::vector<T>::const_iterator bBegin, 
		typename std::vector<T>::const_iterator bEnd )
{
	add_if<T>(dest, aBegin, aEnd);
	add_if<T>(dest, bBegin, bEnd);
}

} // end anonymous namespace 

// Merges two iteration vectors (a and b) to create a new iteration vector which contains
// both the elements of a and b. 
IterationVector merge(const IterationVector& a, const IterationVector& b) {
	IterationVector ret;

	// because the two iteration vectors are built bottom-up, the iterators in a will not be b and
	// viceversa having the same iterators would mean the same variable has been used as loop
	// iterator index in 1  statement as a parameter in another, therefore we can safely remove the
	// iterators and merge the set of parameters. 
	merge_add<Iterator>(ret, a.iter_begin(), a.iter_end(), b.iter_begin(), b.iter_end());	
	merge_add<Parameter>(ret, a.param_begin(), a.param_end(), b.param_begin(), b.param_end());	
	return ret;
}

const IndexTransMap transform(const IterationVector& trg, const IterationVector& src) {
	assert(trg.size() >= src.size()); //TODO: convert into an exception

	IndexTransMap transMap;
	std::for_each(src.begin(), src.end(), [&](const Element& cur) {
			int idx = 0;
			if (cur.getType() == Element::ITER || cur.getType() == Element::PARAM) {
				idx = trg.getIdx( static_cast<const Expr&>(cur).getExpr() ); 
			} else {
				idx = trg.getIdx(cur);
			}
			assert( idx != -1 && static_cast<size_t>(idx) < trg.size() );
			transMap.push_back( idx ); 
		});
	assert(transMap.size() == src.size());
	return transMap;
}

//====== IterationVector::iterator ================================================================

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
	assert (valid && "Iterator not valid"); 

	if (iterIt != iterVec.iters.end())
		return *iterIt;
	else if (paramIt != iterVec.params.end())
		return *paramIt;
	assert(constant && "Iteration vector has no constant part");
	return iterVec.constant;
}

IterationVector removeExistQualified(const IterationVector& iterVec) {
	IterationVector ret;
	for_each(iterVec.begin(), iterVec.end(), [&] ( const Element& cur) {
			if (cur.getType() == Element::CONST) { return; }
			if (cur.getType() == Element::ITER) {
				const Iterator& iter = static_cast<const Iterator&>(cur);
				if (iter.isExistential()) { return; }
			}
			ret.add(cur);
		});
	return ret;

}

} } } // end insieme::analysis::polyhedral namespace


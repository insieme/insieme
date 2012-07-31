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

#pragma once 

#include "insieme/utils/constraint.h"

#include "insieme/analysis/polyhedral/constraint.h"

namespace insieme { namespace analysis { namespace polyhedral {

/**************************************************************************************************
 * IterationDomain: the iteration domain represent the domain on which a statement is valid.
 * Therefore it is a represented by a set of constraints (ConstraintCombiner). However, the
 * iteration domain also allows the creation of empty and universe sets which are used to represent
 * statement which are not bound by any constraint
 **************************************************************************************************/
class IterationDomain : public utils::Printable {

	const IterationVector& iterVec;
	AffineConstraintPtr  constraint;
	bool is_empty;

public:
	IterationDomain(const IterationVector& iterVec, bool is_empty=false) : 
		iterVec(iterVec), is_empty(is_empty) { }

	/**
	 * Conctructs an iteration domain from a combined constraint
	 */
	explicit IterationDomain( const AffineConstraintPtr& constraint ) : 
		iterVec( extractIterationVector(constraint) ), 
		constraint(constraint), 
		is_empty(!static_cast<bool>(constraint)) { }

	/**
	 * Constructs an iteration domain from a simple constraint
	 */
	explicit IterationDomain( const AffineConstraint& constraint ) : 
		iterVec( constraint.getFunction().getIterationVector() ), 
		constraint( makeCombiner(constraint) ), 
		is_empty(false) { }

	/**
	 * Constructs an IterationDomain by copy updating to iterVec iteration vector 
	 */
	IterationDomain( const IterationVector& iv, const IterationDomain& otherDom ) : 
		iterVec(iv), 
		// update the iteration vector of the important constraint to iv
		constraint( cloneConstraint(iv, otherDom.constraint ) ), 
		is_empty(otherDom.is_empty) { }
	
	/**
	 * Builds an iteration domain starting from an iteration vector and a coefficient matrix
	 */
	template <template <class> class Cont=std::initializer_list>
	IterationDomain( const IterationVector& iv, const Cont<int>& coeffs ) : 
		iterVec(iv), 
		is_empty(coeffs.size() == 0) 
	{
		if ( coeffs.size() == 0 ) { return;	}

		constraint = makeCombiner(AffineConstraint(AffineFunction(iterVec, *coeffs.begin())));
		for_each( coeffs.begin()+1, coeffs.end(), [&] (const typename Cont<int>::value_type& cur) { 
			constraint = constraint and AffineConstraint( AffineFunction(iterVec, cur) );
		} );

		assert(constraint);
	}

	/**
	 * Builds an iteration domain starting from an iteration vector and a coefficient matrix
	 */
	template <template <class> class Cont=std::initializer_list>
	IterationDomain( const IterationVector& iv, const Cont<Cont<int>>& coeffs ) : 
		iterVec(iv), 
		is_empty(coeffs.size() == 0) 
	{
		if ( coeffs.size() == 0 ) { return;	}

		constraint = makeCombiner(AffineConstraint(AffineFunction(iterVec, *coeffs.begin())));
		for_each( coeffs.begin()+1, coeffs.end(), [&] (const typename Cont<Cont<int>>::value_type& cur) { 
			constraint = constraint and AffineConstraint( AffineFunction(iterVec, cur) );
		} );

		assert(constraint);
	}

	inline const IterationVector& getIterationVector() const { return iterVec; }
	inline const AffineConstraintPtr& getConstraint() const { return constraint;}

	inline bool universe() const { 
		return (!is_empty && !constraint) ||
			   (!is_empty && constraint && constraint->isEvaluable() && constraint->isTrue()); 
	}

	inline bool empty() const { 
		return is_empty || (!is_empty && constraint && constraint->isEvaluable() && !constraint->isTrue()); 
	}

	// Intersect two iteration domains and return assign the result to this iteration domain
	inline IterationDomain& operator&=(const IterationDomain& other) {
		assert(iterVec == other.iterVec && "Cannot intersect two iteration domains with non-compatible base");
		constraint = constraint and other.constraint;
		return *this;
	}

	// Intersect two iteration domains and return assign the result to this iteration domain
	inline IterationDomain& operator|=(const IterationDomain& other) {
		assert(iterVec == other.iterVec && "Cannot union two iteration domains with non-compatible base");
		constraint = constraint or other.constraint;
		return *this;
	}

	std::ostream& printTo(std::ostream& out) const;
};


IterationDomain operator&&(const IterationDomain& lhs, const IterationDomain& rhs);

IterationDomain operator||(const IterationDomain& lhs, const IterationDomain& rhs);

IterationDomain operator!(const IterationDomain& other);


// Utility which computes the cardinality of a domain
utils::Piecewise<insieme::core::arithmetic::Formula>  cardinality(core::NodeManager& mgr, const IterationDomain& dom);


// Utility function which is utilized to define a simple domain where the variable is defined between 
// an upper bound and lower bound LB <= VAR < UB. If the LB and UB are the same than this translates 
// into the equality VAR == LB == UB. 
IterationDomain makeVarRange( IterationVector& vec, 
					 		  const core::ExpressionPtr& var, 
							  const core::ExpressionPtr& lb, 
							  const core::ExpressionPtr& ub = core::ExpressionPtr());



IterationDomain extractFromCondition(IterationVector& iv, const core::ExpressionPtr& cond);

/**
 * Given a variable, returns the domain on which the variable is defined.
 *
 * If the variable is not inside a SCoP the returned object will be empty which means the domain on
 * which the variable is defined is the universe 
 */
boost::optional<IterationDomain> getVariableDomain(const core::ExpressionAddress& addr);

} } } // end namespace insieme::analysis::polyhedtal

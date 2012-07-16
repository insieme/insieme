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

#include "insieme/analysis/polyhedral/iter_vec.h"
#include "insieme/analysis/polyhedral/affine_func.h"
#include "insieme/core/ir_node.h"

#include "boost/optional.hpp"

#include <set>

namespace insieme { namespace analysis { namespace polyhedral {

using insieme::utils::ConstraintType;

typedef utils::Constraint<AffineFunction> 			AffineConstraint;	
typedef utils::CombinerPtr<AffineFunction>  		AffineConstraintPtr;
typedef utils::BinConstraint<AffineFunction> 	 	BinAffineConstraint;
typedef utils::RawConstraint<AffineFunction>		RawAffineConstraint;
typedef utils::NegConstraint<AffineFunction>		NegAffineConstraint;


AffineConstraint toBase(const AffineConstraint& c, const IterationVector& iterVec, const IndexTransMap& idxMap);


// Makes a copy of the constraint cc changing the base vector to the iteration vector trgVec. 
AffineConstraintPtr cloneConstraint(const IterationVector& trgVec, const AffineConstraintPtr& cc);


// We normalize the constraint, usually required for libraries. 
// Equality constraints remains the same while inequalities must be rewritten to be GE (>=)
AffineConstraintPtr normalize(const AffineConstraint& c);


const IterationVector& extractIterationVector(const AffineConstraintPtr& constraint);

std::set<Iterator> getIterators(const AffineConstraintPtr& constraint);


// Converts a constraint, or a combination of constraints into an IR expression which can be 
// used in the code 
core::ExpressionPtr toIR(core::NodeManager& mgr, const AffineConstraintPtr& c);


inline core::ExpressionPtr toIR(core::NodeManager& mgr, const AffineConstraint& c) {
	return toIR(mgr, makeCombiner(c));
}

AffineConstraintPtr 
copyFromConstraint(const AffineConstraintPtr& cc, const Element& src, const Element& dest);


typedef std::vector<AffineConstraintPtr> 	ConjunctionList;
typedef std::vector<ConjunctionList> 		DisjunctionList;

/** 
 * Given a constraint in disjunction form 	it detect all the constraints which represents lower and
 * upper bounds for the given iterator iter
 */
std::pair<DisjunctionList, DisjunctionList> 
getDomainBounds(const core::VariablePtr iter, const DisjunctionList& disjunctions );

} } // end analysis::polyhedral namespace


namespace utils {

template <>
inline int asConstant(const insieme::analysis::polyhedral::AffineFunction& func) {
	return func.getCoeff( insieme::analysis::polyhedral::Constant() );
}

} } // end insieme::utils namespace


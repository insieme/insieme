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

#include "insieme/utils/printable.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme {
namespace analysis {
namespace poly {

/** 
 * Stores eventual context information to keep the state of the underlying library implementation 
 */
struct Context { 
	Context() { }
	virtual ~Context() { }
};

/**
 * Generic implementation of a the concept of a set which is natively supported by polyhedral
 * libraries. The class presents a set of operations which are possible on sets (i.e. intersect,
 * union, difference, etc...)
 */
struct Set : public utils::Printable {

	// Creates an empty Set based on the dimensionality of the given iteration vector. 
	// Once creates, the iteration vector on which the set is based cannot been changed. 
	Set(Context& ctx, const IterationVector& iterVec) : ctx(ctx), iterVec(iterVec) { } 
	
	// Adds a new constraint to this set. 
	//
	// The iteration vector on which c is expressed must be compatibile with the iterVec
	virtual void addConstraint(const Constraint& c) = 0; 

	virtual void addConstraint(const ConstraintCombinerPtr& c) = 0;

	virtual std::ostream& printTo(std::ostream& out) const = 0; 

	virtual ~Set() { }
protected:
	Context& ctx;
	const IterationVector& iterVec; 
};

template <class SetTy>
SetTy set_union(const SetTy& lhs, const SetTy& rhs);

template <class SetTy>
SetTy set_intersect(const SetTy& lhs, const SetTy& rhs);

template <class SetTy>
SetTy set_negate(const SetTy& lhs);


//===== Conversion Utilities ======================================================================
// Utilities to convert data structures represented as IR annotations
template <class SetTy>
SetTy convertIterationDomain(Context& ctx, const ConstraintCombinerPtr& constraints);


} // end poly namespace
} // end analysis namespace 
} // end insieme namespace 

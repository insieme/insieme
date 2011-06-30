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

#include <vector>

#include "insieme/core/ast_node.h"
#include "insieme/analysis/polyhedral.h"

#include "insieme/analysis/defuse_collect.h"

namespace insieme {
namespace analysis {
namespace scop {

/**
 * Stores the information related to a SCoP (Static Control Part) region of a
 * program. 
 */
class ScopRegion: public core::NodeAnnotation {
public:
	static const string NAME;
	static const utils::StringKey<ScopRegion> KEY;

	typedef std::vector<core::StatementPtr> StmtList;

	// Set of array accesses which appears strictly within this SCoP, array
	// access in sub SCoPs will be directly referred from sub SCoPs. 
	typedef std::set<RefPtr> AccessRefSet; 
	
	ScopRegion(const poly::IterationVector& iv, 
			const poly::ConstraintCombinerPtr& comb = poly::ConstraintCombinerPtr(),
			const StmtList& subScops = StmtList()) : 
		core::NodeAnnotation(), iterVec(iv), 
		constraints(poly::cloneConstraint(iterVec, comb)),
		subScops(subScops) { } 

	const std::string& getAnnotationName() const { return NAME; }

	const std::string toString() const;

	const utils::AnnotationKey* getKey() const { return &KEY; }

	bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, 
			const core::NodePtr& after) const 
	{ 
		return false; 
	}
	
	/**
	 * Return the iteration vector which is spawned by this region, and on
	 * which the associated constraints are based on.
	 */
	const poly::IterationVector& getIterationVector() const { return iterVec; }
	
	/** 
	 * Retrieves the constraint combiner associated to this ScopRegion.
	 */
	const poly::ConstraintCombinerPtr getConstraints() const { return constraints; }

private:
	// Iteration Vector on which constraints are defined 
	poly::IterationVector iterVec;

	// List of constraints which this SCoP defines 
	poly::ConstraintCombinerPtr constraints;

	// Ordered list of sub SCoPs accessible from this SCoP, the SCoPs are
	// ordered in terms of their relative position inside the current SCoP
	// 
	// In the case there are no sub SCoPs for the current SCoP, the list of 
	// sub sub SCoPs is empty
	StmtList subScops;

	// Access informations 
	AccessRefSet accesses;
};

/**
 * AccessFunction : this annotation is used to annotate array subscript
 * expressions with the equality constraint resulting from the access function. 
 *
 * for example the subscript operation A[i+j-N] will generate an equality constraint
 * of the type i+j-N==0. Constraint which is used to annotate the expression.
 */
class AccessFunction: public core::NodeAnnotation {
	poly::IterationVector iterVec;
	poly::EqualityConstraint  eqCons;
public:
	static const string NAME;
	static const utils::StringKey<AccessFunction> KEY;

	AccessFunction(const poly::IterationVector& iv, const poly::EqualityConstraint& eqCons) : 
		core::NodeAnnotation(), iterVec(iv), 
		eqCons( poly::AffineFunction(iterVec, eqCons.getAffineFunction()) ) { }

	const std::string& getAnnotationName() const { return NAME; }

	const utils::AnnotationKey* getKey() const { return &KEY; }

	const std::string toString() const; 
	
	bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, 
			const core::NodePtr& after) const 
	{ 
		return false; 
	}
};

typedef std::vector< std::pair<core::NodePtr, poly::IterationVector> > ScopList;

/**
 * Finds and marks the SCoPs contained in the root subtree and returns a list of
 * found SCoPs (an empty list in the case no SCoP was found). 
 */ 
ScopList mark(const core::NodePtr& root);

} // end namespace scop
} // end namespace analysis
} // end namespace insieme


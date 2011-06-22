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

namespace insieme {
namespace analysis {
namespace scop {

/**
 * Stores the information related to a SCoP (Static Control Part) region of a
 * program. 
 */
class ScopRegion: public core::NodeAnnotation {
	poly::IterationVector iterVec;
	poly::ConstraintCombinerPtr constraints;
public:
	static const string NAME;
	static const utils::StringKey<ScopRegion> KEY;

	ScopRegion(const poly::IterationVector& iv, const poly::ConstraintCombinerPtr& comb = poly::ConstraintCombinerPtr() ): 
		core::NodeAnnotation(), iterVec(iv), constraints(poly::cloneConstraint(iterVec, comb)) { } 

	const std::string& getAnnotationName() const { return NAME; }

	const std::string toString() const;

	const utils::AnnotationKey* getKey() const { return &KEY; }

	bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const { 
		return false; 
	}

	const poly::IterationVector& getIterationVector() const { return iterVec; }
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
		core::NodeAnnotation(), iterVec(iv), eqCons( poly::AffineFunction(iterVec, eqCons.getAffineFunction()) ) { }

	const std::string& getAnnotationName() const { return NAME; }

	const utils::AnnotationKey* getKey() const { return &KEY; }

	const std::string toString() const; 
	
	bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const { 
		return false; 
	}

};

typedef std::vector<std::shared_ptr<ScopRegion>> ScopList;

/**
 * Finds and marks the SCoPs contained in the root subtree and returns a list of
 * found SCoPs (an empty list in the case no SCoP was found). 
 */ 
ScopList mark(const core::NodePtr& root, bool interproc=false);

} // end namespace scop
} // end namespace analysis
} // end namespace insieme


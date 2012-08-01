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

#include "insieme/core/forward_decls.h"

#include "insieme/utils/matrix.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme { namespace transform { namespace polyhedral {

using utils::Matrix;

using analysis::polyhedral::AffineSystem;
using analysis::polyhedral::IterationVector;
using analysis::polyhedral::IterationDomain;
using analysis::polyhedral::Scop;
using analysis::polyhedral::Iterator;
using analysis::polyhedral::Parameter;
using analysis::polyhedral::StmtPtr;
using analysis::polyhedral::Stmt;

// Because most of the transformation in the polyhedral model are in the Z domain, we define
// IntMatrix to represent a Matrix of integer coefficients 
typedef Matrix<int> IntMatrix;

/**
 * Unimodular transformations: a transformation is represented by a matrix
 */
class UnimodularMatrix : public IntMatrix {
public:
	UnimodularMatrix( size_t size ) : IntMatrix(size, size) { 
		assert(!empty() && "Creation of empty Unimodular matrix is not allowed"); 
	}

	UnimodularMatrix( const IntMatrix& mat ) : IntMatrix(mat) { 
		assert(!mat.empty() && mat.cols() == mat.rows() && 
			"Unimodular matrix must be squared and not empty");
	}
};

// Creates a matrix for loop interchange 
UnimodularMatrix makeInterchangeMatrix(const IterationVector&  	iterVec, 
				 					   const core::VariablePtr& src, 
									   const core::VariablePtr& dest );

/**************************************************************************************************
 * Building blocks for polyhedral transformations 
 *************************************************************************************************/
/**
 * Adds a new iterator or parameter to the iteration vector of this scop, the new iterator will be 
 * free (=> not bounded) in the polytope. Use the setConstraint or setZero function to limit the 
 * domain of the iterator
 */
template <class Elem>
const Elem& addTo(Scop& scop, const Elem& iter) {
	IterationVector& iterVec = scop.getIterationVector();
	if ( iterVec.getIdx(iter) != -1 ) { throw "Element already present in iteration vector"; }
	iterVec.add( iter );
	return iter;
}

std::vector<std::reference_wrapper<Stmt>> getLoopSubStatements(Scop& scop, const Iterator& iter);

void scheduleLoopBefore(Scop& scop, const Iterator& iter, const Iterator& newIter);

void scheduleLoopAfter(Scop& scop, const Iterator& iter, const Iterator& newIter);


/**
 * Adds a constraint to the 'iter' iterator. The constraint will be added only for the statements
 * which are within the loop spawned by iterator 'iter'
 */
void addConstraint(Scop& scop, const Iterator& iter, const IterationDomain& dom);


/**
 * Adds a constraint to a parameter of the SCoP. This constraint will be added to all the statements
 * composing this SCoP.
 */
void addConstraint(Scop& scop, const Parameter& param, const IterationDomain& dom);


/**
 * Set an iterator domain to zero for all the statements which are not scheduled under this iterator
 */
void setZeroOtherwise(Scop& scop, const Iterator& iter);

enum TransMode { SCHED_ONLY, ACCESS_ONLY, BOTH };

template <TransMode Mode = BOTH>
void applyUnimodularTransformation(Scop& scop, const UnimodularMatrix& trans);


/**
 * Check if in the transformed SCoP the dependencies exsiting in the original schedule have not been 
 * inverted. 
 */
bool checkTransformedSchedule(const Scop& orgin, const Scop& trans);


/**
 * Strip mine a domain in the provided SCoP. This transformation will strip the loop spawned by 
 * the variable iter by the specific size tile_size. 
 */
core::VariablePtr doStripMine(core::NodeManager& 		mgr,
							  Scop& 					scop, 
							  const core::VariablePtr 	iter, 
							  const IterationDomain& 	dom, 
						  	  unsigned 					tile_size);


/**
 * Fuses together the domains spawned by the iterators iters
 */
void doFuse(Scop& scop, const core::VariableList& iters);

void doSplit(Scop& scop, const core::VariablePtr& iter, const std::vector<unsigned>& stmtIdxs);


/**
 * Duplicates a statement and schedules the generated statements immediately after the statement it
 * was originating from
 */
void dupStmt(Scop& scop, const unsigned& stmtId, const analysis::polyhedral::AffineConstraintPtr& cons);


std::pair<analysis::polyhedral::AffineConstraintPtr, core::ExpressionPtr> 
stampFor(core::NodeManager& mgr, Scop& scop, const core::VariablePtr& iter, const core::arithmetic::Formula& dom, unsigned tileSize);

} // end polyhedral namespace
} // end transform namespace 
} // end insieme namespace 


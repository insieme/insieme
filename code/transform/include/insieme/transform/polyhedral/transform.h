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
#include "insieme/utils/printable.h"

namespace insieme {

// Forward declarations 
namespace analysis {
namespace poly {

class AffineSystem;
class Iterator;
class Parameter;
class IterationVector;
class IterationDomain;
class Scop;

} // end poly namespace
} // end analysis namespace 

namespace transform {
namespace poly {

using utils::Matrix;

using analysis::poly::AffineSystem;
using analysis::poly::IterationVector;
using analysis::poly::IterationDomain;
using analysis::poly::Scop;
using analysis::poly::Iterator;
using analysis::poly::Parameter;

// Because most of the transformation in the polyhedral model are in the Z domain, we define
// IntMatrix to represent a Matrix of integer coefficients 
typedef Matrix<int> IntMatrix;

/** 
 * Extract the coefficient matrix from an AffineSystem. 
 *
 * Because the representation of AffineSystems is compact (with repect of the iteration vector) for
 * semplicity we extract the coefficient matrix in order to simplify operations on the polytope. 
 */
IntMatrix extractFrom(const AffineSystem& sys);


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
UnimodularMatrix makeInterchangeMatrix(size_t size, size_t src, size_t dest);

UnimodularMatrix 
makeInterchangeMatrix(const IterationVector& 	iterVec, 
					  const core::VariablePtr& 	src, 
					  const core::VariablePtr& 	dest );

/**************************************************************************************************
 * Building blocks for polyhedral transformations 
 *************************************************************************************************/
/**
 * Adds a new iterator to the iteration vector, the new iterator will be free (=> not bounded) in
 * the polytope. Use the setConstraint or setZero function to limit the domain of the iterator
 */
void addIterator(Scop& scop, const Iterator& iter);

/**
 * Adds a new parameter to this scop
 */
void addParameter(Scop& scop, const Parameter& param);

/**
 * Adds a constraint to the 'iter' iterator. The constraint will be added only for the statements
 * which are within the loop spawned by iterator 'iter'
 */
void addConstrainit(Scop& scop, const Iterator& iter, const IterationDomain& dom);

/**
 * Adds a constraint to a parameter of the SCoP. This constraint will be added to all the statements
 * composing this SCoP.
 */
void addConstraint(Scop& scop, const Parameter& param, const IterationDomain& dom);

/**
 * Set an iterator domain to zero if not already constrainted
 */
void setZero(Scop& scop, const Iterator& iter);

enum TransMode { SCHED_ONLY, ACCESS_ONLY, BOTH };

template <TransMode Mode = BOTH>
void applyUnimodularTransformation(Scop& scop, const UnimodularMatrix& trans);


} // end poly namespace 
} // end transform namespace 
} // end insieme namespac

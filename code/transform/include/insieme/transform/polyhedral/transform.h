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
class IterationVector;

} // end poly namespace
} // end analysis namespace 

namespace transform {
namespace poly {

using utils::Matrix;

using analysis::poly::AffineSystem;
using analysis::poly::IterationVector;

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
	UnimodularMatrix( size_t rows, size_t cols ) : IntMatrix(rows, cols) { 
		assert(!empty() && "Creation of empty Unimodular matrix is not allowed"); 
	}

	UnimodularMatrix( const IntMatrix& mat ) : IntMatrix(mat) { }

};



// Creates a matrix for loop interchange 
UnimodularMatrix makeInterchangeMatrix(size_t size, size_t src, size_t dest);

UnimodularMatrix 
makeInterchangeMatrix(const IterationVector& 	iterVec, 
					  const core::VariablePtr& 	src, 
					  const core::VariablePtr& 	dest );

} // end poly namespace 
} // end transform namespace 
} // end insieme namespac
